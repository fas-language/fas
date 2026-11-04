(* Linear-scan register allocation over [Ir] SSA, milestone 3.

   Positions: every block has a start position (where its phis are defined),
   one position per instruction, and one for its terminator. A phi operand is
   a use at the end of the predecessor it comes from. Liveness is computed by
   the usual backward fixpoint, and each value gets one conservative interval
   from its first to its last live position (no holes).

   Registers: the callee-saved rbx and r12-r15 are free for any value; r8-r11,
   rsi and rdi only for values whose interval holds no call or syscall
   (counting a use as an argument), because argument set-up writes them, and
   rsi and rdi not across block copies or intrinsics either, which use them as
   scratch. Code generation keeps rax, rcx and rdx as scratch. When too many values are
   live, the one with the lowest spill weight (uses scaled by loop depth, per
   position of its interval; ties go to the interval ending last) stays in its
   stack slot.

   The caller passes a view of the code: [skip] names values that get no
   register (addresses rebuilt where they are used) and [operands] lists the
   values an instruction really reads once such addresses are folded in.

   A value whose interval ends at an instruction frees its register for that
   instruction's result (code generation reads operands first), and a phi
   prefers the register of its incoming values, so loop counters need no
   copies on the back edge. *)

open Ir
module Hashtbl = Itbl
module Ptbl = Stdlib.Hashtbl

(* Allocation keys: a value id (>= 0) or an interned parameter name (< 0) *)
type key = int

let param_keys : (string, int) Ptbl.t = Ptbl.create 16
let param_names : (int, string) Ptbl.t = Ptbl.create 16

let pkey name =
  match Ptbl.find_opt param_keys name with
  | Some k -> k
  | None ->
      let k = -1 - Ptbl.length param_keys in
      Ptbl.replace param_keys name k;
      Ptbl.replace param_names k name;
      k

let key_name k = if k >= 0 then Printf.sprintf "v%d" k else Ptbl.find param_names k
let callee_saved = [ X64.Rbx; X64.R12; X64.R13; X64.R14; X64.R15 ]
let caller_saved = [ X64.R8; X64.R9; X64.R10; X64.R11; X64.Rsi; X64.Rdi ]

(* Registers that block copies and some intrinsics use as scratch *)
let string_regs = [ X64.Rsi; X64.Rdi ]

let scalar = function
  | I1 | I8 | I16 | I32 | I64 | F32 | F64 | Pointer _ -> true
  | _ -> false

let key_of_skip skip = function
  | Local (id, ty) when scalar ty && not (skip id) -> Some id
  | Param (name, ty) when scalar ty -> Some (pkey name)
  | _ -> None

let default_operands ins =
  let acc = ref [] in
  ignore
    (Ir_fold.map_instr
       (fun v ->
         acc := v :: !acc;
         v)
       ins);
  List.rev !acc

let iter_operands f ins =
  ignore
    (Ir_fold.map_instr
       (fun v ->
         f v;
         v)
       ins)

let iter_term_operands f t =
  ignore
    (Ir_fold.map_term
       (fun v ->
         f v;
         v)
       t)

let clobbers = function
  | Call (_, _, _, name, _) -> not (String.starts_with ~prefix:"llvm." name)
  | Call_indirect _ | Syscall _ -> true
  | _ -> false

let uses_string_regs = function
  | Call (_, _, _, name, _) -> String.starts_with ~prefix:"llvm." name
  | Load (_, (Struct _ | Array _), _, _)
  | Load_volatile (_, (Struct _ | Array _), _, _)
  | Store ((Struct _ | Array _), _, _, _)
  | Store_volatile ((Struct _ | Array _), _, _, _) ->
      true
  | _ -> false

(* [skip] holds the ids of instructions whose code is folded into their
   block's terminator; they define nothing that needs a home. *)
let allocate ?(skip = fun _ -> false) ?(operands = default_operands) ?slot_of (f : func)
    =
  let key_of = key_of_skip skip in
  let iter_operands g ins = List.iter g (operands ins) in
  let start_pos = Hashtbl.create 16 and end_pos = Hashtbl.create 16 in
  let block_starts = Hashtbl.create 16 in
  let n = ref 1 in
  let calls = ref [] and string_ops = ref [] in
  List.iter
    (fun (b : block) ->
      Hashtbl.replace start_pos b.id !n;
      Hashtbl.replace block_starts !n ();
      incr n;
      List.iter
        (fun ins ->
          if clobbers ins then calls := !n :: !calls;
          if uses_string_regs ins then string_ops := !n :: !string_ops;
          incr n)
        b.instrs;
      Hashtbl.replace end_pos b.id !n;
      incr n)
    f.blocks;
  let calls = Array.of_list (List.rev !calls) in
  let string_ops = Array.of_list (List.rev !string_ops) in
  (* Local uses and defs, and phi uses per edge *)
  let blocks = Hashtbl.create 16 in
  List.iter (fun (b : block) -> Hashtbl.replace blocks b.id b) f.blocks;
  let phi_uses pred succ =
    match Hashtbl.find_opt blocks succ with
    | None -> []
    | Some (s : block) ->
        List.filter_map
          (function
            | Phi (_, _, incoming) ->
                List.find_map
                  (fun (v, p) -> if p = pred then key_of v else None)
                  incoming
            | _ -> None)
          s.instrs
  in
  let phi_defs (b : block) =
    List.filter_map
      (function
        | Phi (id, ty, _) when scalar ty && not (skip id) -> Some id | _ -> None)
      b.instrs
  in
  let gen = Hashtbl.create 16 and kill = Hashtbl.create 16 in
  List.iter
    (fun (b : block) ->
      let defs = Hashtbl.create 16 and uses = Hashtbl.create 16 in
      List.iter (fun k -> Hashtbl.replace defs k ()) (phi_defs b);
      let use v =
        match key_of v with
        | Some k when not (Hashtbl.mem defs k) -> Hashtbl.replace uses k ()
        | _ -> ()
      in
      List.iter
        (fun ins ->
          (match ins with Phi _ -> () | _ -> iter_operands use ins);
          match instruction_result ins with
          | Some (id, ty) when scalar ty && not (skip id) -> Hashtbl.replace defs id ()
          | _ -> ())
        b.instrs;
      iter_term_operands use b.terminator;
      Hashtbl.replace gen b.id uses;
      Hashtbl.replace kill b.id defs)
    f.blocks;
  let live_in = Hashtbl.create 16 and live_out = Hashtbl.create 16 in
  List.iter
    (fun (b : block) ->
      Hashtbl.replace live_in b.id (Hashtbl.create 8);
      Hashtbl.replace live_out b.id (Hashtbl.create 8))
    f.blocks;
  let changed = ref true in
  let rev_blocks = List.rev f.blocks in
  while !changed do
    changed := false;
    List.iter
      (fun (b : block) ->
        let out = Hashtbl.find live_out b.id in
        let add tbl k =
          if not (Hashtbl.mem tbl k) then begin
            Hashtbl.replace tbl k ();
            changed := true
          end
        in
        List.iter
          (fun s ->
            List.iter (add out) (phi_uses b.id s);
            match (Hashtbl.find_opt live_in s, Hashtbl.find_opt blocks s) with
            | Some ins, Some sb ->
                let sdefs = phi_defs sb in
                Hashtbl.iter
                  (fun k () -> if not (List.exists (Int.equal k) sdefs) then add out k)
                  ins
            | _ -> ())
          (terminator_successors b.terminator);
        let inn = Hashtbl.find live_in b.id in
        Hashtbl.iter (fun k () -> add inn k) (Hashtbl.find gen b.id);
        let defs = Hashtbl.find kill b.id in
        Hashtbl.iter (fun k () -> if not (Hashtbl.mem defs k) then add inn k) out)
      rev_blocks
  done;
  (* Live ranges: per block, from the definition (or the block start when
     live in) to the last use (or the block end when live out). A value's
     ranges, merged, form an interval with holes. *)
  let first = Hashtbl.create 64 and last = Hashtbl.create 64 in
  let ranges = Hashtbl.create 64 in
  (* Spill weights: each use counts 10^depth, where a block's loop depth is
     the number of back edges (to an earlier block in layout order) whose
     span covers it. *)
  let index = Hashtbl.create 16 in
  List.iteri (fun i (b : block) -> Hashtbl.replace index b.id i) f.blocks;
  let back_edges =
    List.concat_map
      (fun (b : block) ->
        let i = Hashtbl.find index b.id in
        List.filter_map
          (fun s ->
            match Hashtbl.find_opt index s with
            | Some j when j <= i -> Some (j, i)
            | _ -> None)
          (terminator_successors b.terminator))
      f.blocks
  in
  let block_weight (b : block) =
    let i = Hashtbl.find index b.id in
    let depth = List.length (List.filter (fun (j, k) -> j <= i && i <= k) back_edges) in
    let rec pow w d = if d = 0 then w else pow (w *. 10.) (d - 1) in
    pow 1. (min depth 6)
  in
  let weights = Hashtbl.create 64 in
  let weigh w k =
    Hashtbl.replace weights k
      (w +. Option.value ~default:0. (Hashtbl.find_opt weights k))
  in
  let entry_id = match f.blocks with b :: _ -> b.id | [] -> -1 in
  List.iter
    (fun (b : block) ->
      Hashtbl.reset first;
      Hashtbl.reset last;
      let s = Hashtbl.find start_pos b.id and e = Hashtbl.find end_pos b.id in
      let w = block_weight b in
      let touch k p =
        weigh w k;
        if not (Hashtbl.mem first k) then Hashtbl.replace first k p;
        Hashtbl.replace last k p
      in
      List.iter (fun k -> touch k s) (phi_defs b);
      List.iteri
        (fun i ins ->
          let p = s + 1 + i in
          (match ins with
          | Phi _ -> ()
          | _ ->
              iter_operands (fun v -> Option.iter (fun k -> touch k p) (key_of v)) ins);
          match instruction_result ins with
          | Some (id, ty) when scalar ty && not (skip id) -> touch id p
          | _ -> ())
        b.instrs;
      iter_term_operands
        (fun v -> Option.iter (fun k -> touch k e) (key_of v))
        b.terminator;
      List.iter
        (fun succ -> List.iter (fun k -> touch k e) (phi_uses b.id succ))
        (terminator_successors b.terminator);
      let inn = Hashtbl.find live_in b.id and out = Hashtbl.find live_out b.id in
      let keys = Hashtbl.create 16 in
      Hashtbl.iter (fun k _ -> Hashtbl.replace keys k ()) first;
      Hashtbl.iter (fun k () -> Hashtbl.replace keys k ()) inn;
      Hashtbl.iter (fun k () -> Hashtbl.replace keys k ()) out;
      Hashtbl.iter
        (fun k () ->
          let lo =
            if Hashtbl.mem inn k then if b.id = entry_id then 0 else s
            else Option.value ~default:s (Hashtbl.find_opt first k)
          in
          let hi =
            if Hashtbl.mem out k then e
            else Option.value ~default:lo (Hashtbl.find_opt last k)
          in
          Hashtbl.replace ranges k
            ((lo, hi) :: Option.value ~default:[] (Hashtbl.find_opt ranges k)))
        keys)
    f.blocks;
  List.iter
    (fun (p : param) ->
      if scalar p.ty && not (Hashtbl.mem ranges (pkey p.name)) then
        Hashtbl.replace ranges (pkey p.name) [ (0, 0) ])
    f.params;
  let merge rs =
    let rec go = function
      | (a, b) :: (c, d) :: rest when c <= b + 1 -> go ((a, max b d) :: rest)
      | r :: rest -> r :: go rest
      | [] -> []
    in
    go (List.sort compare rs)
  in
  let intervals =
    Hashtbl.fold
      (fun k rs acc ->
        let rs = merge rs in
        (fst (List.hd rs), k, rs) :: acc)
      ranges []
    |> List.sort compare
  in
  let crosses_at points rs =
    (* a call at position c clobbers values live across it or used by it *)
    List.exists (fun (l, h) -> Array.exists (fun c -> l < c && c <= h) points) rs
  in
  let crosses_call = crosses_at calls in
  (* Phi webs: a phi and its incoming values like to share a register. *)
  let related = Hashtbl.create 32 in
  let relate a b =
    Hashtbl.replace related a
      (b :: Option.value ~default:[] (Hashtbl.find_opt related a))
  in
  List.iter
    (fun (b : block) ->
      List.iter
        (function
          | Phi (id, ty, incoming) when scalar ty && not (skip id) ->
              List.iter
                (fun (v, _) ->
                  match key_of v with
                  | Some k ->
                      relate id k;
                      relate k id
                  | None -> ())
                incoming
          | _ -> ())
        b.instrs)
    f.blocks;
  (* Two ranges conflict when they share a position, except that a value
     dying at an instruction leaves its register to that instruction's
     result (code generation reads operands first). Not at block starts,
     where several phis are written together, nor at 0, the parameters. *)
  let single_point_ok p = p > 0 && not (Hashtbl.mem block_starts p) in
  let ranges_conflict (a1, b1) (a2, b2) =
    let lo = max a1 a2 and hi = min b1 b2 in
    lo <= hi
    && not
         (lo = hi && single_point_ok lo
         && ((b1 = lo && a2 = lo) || (b2 = lo && a1 = lo)))
  in
  let conflict r1 r2 = List.exists (fun x -> List.exists (ranges_conflict x) r2) r1 in
  let last_end rs = List.fold_left (fun acc (_, b) -> max acc b) 0 rs in
  (* spill cost per position: a short interval with a use in a loop is
     costly to spill, a long one used once is cheap *)
  let span rs = List.fold_left (fun acc (a, b) -> acc + b - a + 1) 0 rs in
  let weight k rs =
    Option.value ~default:0. (Hashtbl.find_opt weights k) /. float_of_int (span rs)
  in
  let debug = Sys.getenv_opt "FAS_RA_DEBUG" = Some "1" in
  let assignment = Hashtbl.create 64 in
  (* per register: the intervals it holds that may still conflict *)
  let held = Ptbl.create 8 in
  let all = callee_saved @ caller_saved in
  List.iter (fun r -> Ptbl.replace held r []) all;
  List.iter
    (fun (start, k, rs) ->
      (* forget intervals that ended before this start: later intervals start
         no earlier *)
      List.iter
        (fun r ->
          Ptbl.replace held r
            (List.filter (fun (_, _, rs') -> last_end rs' >= start) (Ptbl.find held r)))
        all;
      let crossing = crosses_call rs in
      let string_crossing = crosses_at string_ops rs in
      let usable r =
        ((not crossing) || List.mem r callee_saved)
        && not (string_crossing && List.mem r string_regs)
      in
      let fits r =
        usable r
        && not (List.exists (fun (_, _, rs') -> conflict rs rs') (Ptbl.find held r))
      in
      (* Prefer caller-saved registers for short, call-free intervals so the
         callee-saved ones stay for values that need them. *)
      let order = if crossing then callee_saved else caller_saved @ callee_saved in
      let hinted =
        List.filter_map
          (fun k' -> Hashtbl.find_opt assignment k')
          (Option.value ~default:[] (Hashtbl.find_opt related k))
      in
      let take r =
        Hashtbl.replace assignment k r;
        Ptbl.replace held r ((start, k, rs) :: Ptbl.find held r)
      in
      match List.find_opt fits hinted with
      | Some r -> take r
      | None -> (
          match List.find_opt fits order with
          | Some r -> take r
          | None -> (
              (* Evict the cheapest single conflicting interval if it is
                 cheaper than this one, or as cheap and ending later. *)
              let candidates =
                List.filter_map
                  (fun r ->
                    if not (usable r) then None
                    else
                      match
                        List.filter
                          (fun (_, _, rs') -> conflict rs rs')
                          (Ptbl.find held r)
                      with
                      | [ ((_, vk, vrs) as victim) ] ->
                          Some ((weight vk vrs, -last_end vrs), r, vk, victim)
                      | _ -> None)
                  order
              in
              let mine = (weight k rs, -last_end rs) in
              match
                List.sort (fun (a, _, _, _) (b, _, _, _) -> compare a b) candidates
              with
              | (cost, r, vk, victim) :: _ when cost < mine ->
                  Hashtbl.remove assignment vk;
                  Ptbl.replace held r
                    (List.filter (fun x -> x != victim) (Ptbl.find held r));
                  take r
              | _ -> ())))
    intervals;
  (* Spilled phis share a stack slot with their spilled incoming values when
     the intervals allow it (the same rule as for registers), so their edge
     copies vanish. [slot_of] receives member -> representative. *)
  (match slot_of with
  | None -> ()
  | Some out ->
      let rs_of = Hashtbl.create 64 in
      List.iter (fun (_, k, rs) -> Hashtbl.replace rs_of k rs) intervals;
      let group = Hashtbl.create 16 in
      (* representative -> ranges of all members *)
      let rec find k =
        match Hashtbl.find_opt out k with Some r when r <> k -> find r | _ -> k
      in
      let ranges_of r =
        match Hashtbl.find_opt group r with
        | Some rs -> rs
        | None -> Option.value ~default:[] (Hashtbl.find_opt rs_of r)
      in
      let spilled k = k >= 0 && Hashtbl.mem rs_of k && not (Hashtbl.mem assignment k) in
      List.iter
        (fun (b : block) ->
          List.iter
            (function
              | Phi (id, ty, incoming) when scalar ty && spilled id ->
                  List.iter
                    (fun (v, _) ->
                      match key_of v with
                      | Some k when spilled k ->
                          let a = find id and b = find k in
                          if a <> b then begin
                            let ra = ranges_of a and rb = ranges_of b in
                            if not (conflict ra rb) then begin
                              Hashtbl.replace out b a;
                              Hashtbl.replace group a (ra @ rb)
                            end
                          end
                      | _ -> ())
                    incoming
              | _ -> ())
            b.instrs)
        f.blocks;
      Hashtbl.iter (fun k _ -> Hashtbl.replace out k (find k)) out);
  if debug then
    List.iter
      (fun (_, k, rs) ->
        Printf.eprintf "%s %s %s -> %s\n" f.name (key_name k)
          (String.concat " " (List.map (fun (a, b) -> Printf.sprintf "[%d,%d]" a b) rs))
          (match Hashtbl.find_opt assignment k with
          | Some r -> X64.reg_name r
          | None -> "mem"))
      intervals;
  assignment
