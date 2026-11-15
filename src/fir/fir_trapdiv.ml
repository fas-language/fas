(* Fir_trapdiv: the `trap` flag on udiv/sdiv/urem/srem (SPEC §6, survey A6).

   A trapping division traps on a zero divisor, and for sdiv/srem on
   MIN / -1, exactly where it stands. This pass turns every one the target
   does not trap on by itself into ordinary Fir, in place:

       %c = icmp eq b, 0               (any lane, for a vector)
       %c = or %c, (a == MIN) & (b == -1)    (sdiv, srem)
       br %c, ^trap, ^rest
     ^trap: trap; unreachable
     ^rest: the division without the flag, then the rest of the block

   The checked division then has nothing left to trap on, so dropping its
   flag loses nothing.

   [~hw:true] is the native x86-64 backend. Its isel divides with the
   64-bit `div`/`idiv`, which raise #DE for a zero divisor at any width up
   to 64 bits, and for MIN / -1 only at 64 bits (narrower signed operands
   are sign-extended first, so i32 MIN / -1 does not overflow there). Those
   divisions keep their flag and get no check; nothing after this pass in
   the native pipeline moves or deletes an instruction. [~hw:false] (LLVM,
   whose division by zero is UB) checks every trapping division.

   A divisor that is a constant other than 0 (and other than -1, for the
   signed ones) needs no check; the flag is just dropped. *)

open Fir

let is_div = function Udiv | Sdiv | Urem | Srem -> true | _ -> false
let signed = function Sdiv | Srem -> true | _ -> false

(* the target traps on this division by itself *)
let hardware ~hw k ty =
  hw && match ty with Int w when w <= 64 -> w = 64 || not (signed k) | _ -> false

let needs_rewrite ~hw f (ir : inst_rec) =
  match ir.op with
  | Ibin (k, fl, _, _) when is_div k && has fl f_trap ->
      not (hardware ~hw k (ty_of f ir.results.(0)))
  | _ -> false

(* an integer constant of [ty]'s lane type, splatted for a vector *)
let lane_const f b ty limbs =
  let lane = match ty with Vec (_, t) -> t | t -> t in
  let c = iconst_limbs f lane limbs in
  match ty with Vec _ -> emit1 f b (Splat c) ty | _ -> c

let limbs w = Array.make ((w + 63) / 64) 0L

let minimum w =
  let l = limbs w in
  let top = (w - 1) / 64 in
  l.(top) <- Int64.shift_left 1L ((w - 1) mod 64);
  l

let minus_one w = Array.map (fun _ -> -1L) (limbs w)

(* the divisor is a scalar constant that cannot trap *)
let safe_divisor f k b =
  match const_of f b with
  | Some (Cint (Int w, l)) ->
      Array.exists (fun x -> x <> 0L) l
      && not
           (signed k
           && Array.for_all2 (fun x y -> x = y) l (normalize_limbs w (minus_one w)))
  | _ -> false

(* Rewrites [ir] at the end of block [b] (its later instructions are
   already detached); returns the block the rest of the code goes to *)
let rewrite f b (ir : inst_rec) =
  match ir.op with
  | Ibin (k, fl, x, y) ->
      let loc = ir.loc in
      let ty = ty_of f ir.results.(0) in
      let fl = fl land lnot f_trap in
      if safe_divisor f k y then begin
        let r = emit1 ~loc f b (Ibin (k, fl, x, y)) ty in
        replace_all_uses f ir.results.(0) r;
        b
      end
      else begin
        let w = int_width ty in
        let cty = with_elem ty i1 in
        let eq v limbs = emit1 ~loc f b (Icmp (Eq, v, lane_const f b ty limbs)) cty in
        let bad = eq y (limbs w) in
        let bad =
          if signed k then
            let is_min = eq x (minimum w) in
            let is_minus_one = eq y (minus_one w) in
            let overflow = emit1 ~loc f b (Ibin (And, 0, is_min, is_minus_one)) cty in
            emit1 ~loc f b (Ibin (Or, 0, bad, overflow)) cty
          else bad
        in
        let bad =
          match ty with Vec _ -> emit1 ~loc f b (Reduce (Red_or, bad)) i1 | _ -> bad
        in
        let trap = add_block f and rest = add_block f in
        set_term ~loc f b (Br (bad, target trap, target rest, 0));
        emit0 ~loc f trap Trap;
        set_term ~loc f trap Unreachable;
        let r = emit1 ~loc f rest (Ibin (k, fl, x, y)) ty in
        replace_all_uses f ir.results.(0) r;
        rest
      end
  | _ -> b

let run_func ~hw (f : func) =
  if f.defined then begin
    (* new blocks are appended, so the code moved into them is visited
       too; block !b is scanned again after a rewrite *)
    let b = ref 0 in
    while !b < num_blocks f do
      let br = block f !b in
      let insts = Vec.to_array br.insts in
      match Array.find_index (fun i -> needs_rewrite ~hw f (inst f i)) insts with
      | None -> incr b
      | Some j ->
          let term = br.term and term_loc = br.term_loc in
          br.insts <- Vec.of_list 0 (Array.to_list (Array.sub insts 0 j));
          let cur = rewrite f !b (inst f insts.(j)) in
          let rest = Array.sub insts (j + 1) (Array.length insts - j - 1) in
          Array.iter (fun i -> ignore (Vec.push (block f cur).insts i)) rest;
          set_term ~loc:term_loc f cur term
    done
  end

let run ~hw (m : modul) = Vec.iter (run_func ~hw) m.funcs
