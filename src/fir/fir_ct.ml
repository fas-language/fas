(* Fir_ct: the constant-time taint check on Fir (SPEC §8).

   Taint starts at `secret` marks and ends at `declassify`. It flows through
   every value computation and through block parameters (a parameter is
   secret when any edge passes it a secret); a load's result is public (the
   promise tracks secrecy in values and typed accesses, and the front end
   marks each secret read), and so is a call's.

   Run on the native path after legalization (Fir_trapdiv, Fir_scalarize,
   Fir_wide), just before instruction selection, so it also checks that no
   legalization branched on a secret or used one as an address. The Fas
   front end has already refused these at the source (Sema_secret), so a
   violation here is an internal error.

   Refused on a secret: a branch, switch or indirect-branch operand; any
   address (load, store, ptradd index, memory builtins, atomics, masked and
   va forms, alloca size, indirect callee); division and remainder; float
   operations and conversions to or from floats; `ctz clz popcnt bitrev`;
   lane extraction or insertion at a secret index; reductions, compress,
   expand, permute and masked forms; syscalls; inline assembly. *)

open Fir

exception Failure of string

let check_func (f : func) =
  let nv = num_values f in
  let taint = Array.make nv false in
  let tainted v =
    let v = resolve f v in
    v >= 0 && v < nv && taint.(v)
  in
  let set v =
    let v = resolve f v in
    if v >= 0 && v < nv && not taint.(v) then begin
      taint.(v) <- true;
      true
    end
    else false
  in
  (* does the op's result carry its operands' taint? *)
  let result_taint (op : op) =
    match op with
    | Mark (Secret, _) -> true
    | Mark (Declassify, _) -> false
    | Load _ | Call _ | Masked_load _ | Va_arg _ | Va_arg_agg _ | Alloca _ | Stack_save
    | Syscall _ | Asm _ | Rmw _ | Cmpxchg _ | Cmpxchg2 _ ->
        false
    | _ ->
        let any = ref false in
        iter_op_uses (fun v -> if tainted v then any := true) op;
        !any
  in
  let changed = ref true in
  while !changed do
    changed := false;
    Vec.iter
      (fun (br : block_rec) ->
        Vec.iter
          (fun i ->
            let ir = inst f i in
            if result_taint ir.op then
              Array.iter (fun r -> if set r then changed := true) ir.results)
          br.insts;
        let pass (t : target) =
          let params = (block f t.dest).params in
          Array.iteri
            (fun k a ->
              if tainted a && k < Array.length params && set params.(k) then
                changed := true)
            t.args
        in
        match br.term with
        | Jump t -> pass t
        | Br (_, t, e, _) ->
            pass t;
            pass e
        | Switch (_, d, cases, _) ->
            pass d;
            Array.iter (fun (_, t) -> pass t) cases
        | Indirect_br _ | Asm_goto _ | Ret _ | Unreachable -> ())
      f.blocks
  done;
  let problems = ref [] in
  let fail b what =
    problems := Printf.sprintf "@%s ^%d: a secret %s" f.name b what :: !problems
  in
  Vec.iteri
    (fun b (br : block_rec) ->
      Vec.iter
        (fun i ->
          let ir = inst f i in
          let bad what v = if tainted v then fail b what in
          let any what = iter_op_uses (bad what) ir.op in
          match ir.op with
          | Nop | Freeze _ | Mark _ | Dbg_value _ | Trap | Fence _ | Stack_save -> ()
          | Ibin ((Udiv | Sdiv | Urem | Srem), _, _, _) -> any "operand of a division"
          | Ibin _ | Fsh _ | Ovf _ | Icmp _ | Select _ | Splat _ | Shuffle _ -> ()
          | Iun ((Ctz | Clz | Popcnt | Bitrev), _, _) ->
              any "operand of ctz, clz, popcnt or bitrev"
          | Iun ((Bswap | Abs), _, _) -> ()
          | Fbin _ | Fun _ | Fma _ | Fcmp _ -> any "float operand"
          | Conv ((Trunc | Zext | Sext | Bitcast), _) -> ()
          | Conv ((Ptrtoint | Inttoptr), a) -> bad "address" a
          | Conv (_, a) -> bad "float conversion operand" a
          | Alloca (size, _) -> bad "alloca size" size
          | Load (_, p) -> bad "address" p
          | Store (m, v, p) ->
              bad "address" p;
              if m.order <> Plain then bad "atomic store value" v
          | Ptradd (_, p, idx, _, _) ->
              bad "address" p;
              bad "index forming an address" idx
          | Memcpy _ | Memmove _ | Memset _ -> any "memory builtin operand"
          | Rmw _ | Cmpxchg _ | Cmpxchg2 _ -> any "atomic operand"
          | Extract (_, idx) -> bad "lane index" idx
          | Insert (_, _, idx) -> bad "lane index" idx
          | Reduce _ | Compress _ | Expand _ | Permute _ | Masked_load _
          | Masked_store _ ->
              any "operand of a vector form with no constant-time lowering"
          | Call { callee = Indirect c; _ } -> bad "indirect callee" c
          | Call _ -> ()
          | Syscall _ -> any "syscall argument"
          | Va_start p | Va_arg p | Va_arg_agg (_, p) -> bad "va_list address" p
          | Stack_restore p -> bad "stack pointer" p
          | Lifetime (_, _, p) -> bad "address" p
          | Asm _ -> any "inline assembly operand")
        br.insts;
      let bad what v = if tainted v then fail b what in
      match br.term with
      | Br (c, _, _, _) -> bad "branch condition" c
      | Switch (v, _, _, _) -> bad "switch value" v
      | Indirect_br (v, _) -> bad "indirect branch target" v
      | Asm_goto { aargs; _ } -> Array.iter (bad "inline assembly operand") aargs
      | Jump _ | Ret _ | Unreachable -> ())
    f.blocks;
  List.rev !problems

(* every defined function; the module is unchanged *)
let check ?iter_funcs (m : modul) =
  let iter = Option.value iter_funcs ~default:(fun visit -> Vec.iter visit m.funcs) in
  let problems = ref [] in
  iter (fun (f : func) -> if f.defined then problems := !problems @ check_func f);
  match !problems with
  | [] -> ()
  | ps -> raise (Failure ("constant-time violation:\n" ^ String.concat "\n" ps))
