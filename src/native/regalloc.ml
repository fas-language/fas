(* Linear-scan register allocation over [Ir] SSA, milestone 3.

   Positions: every block has a start position (where its phis are defined),
   one position per instruction, and one for its terminator. A phi operand is
   a use at the end of the predecessor it comes from. Liveness is computed by
   the usual backward fixpoint, and each value gets one conservative interval
   from its first to its last live position (no holes).

   Registers: the callee-saved rbx and r12-r15 are free for any value; r8, r9
   and r10 only for values whose interval holds no call or syscall (counting a
   use as an argument), because argument set-up writes them. Code generation
   keeps rax, rcx, rdx, rsi, rdi and r11 as scratch. When too many values are
   live, the one whose interval ends last stays in its stack slot. *)

open Ir

type key = V of int | P of string

let callee_saved = [ X64.Rbx; X64.R12; X64.R13; X64.R14; X64.R15 ]
let caller_saved = [ X64.R8; X64.R9; X64.R10 ]
let scalar = function I1 | I8 | I16 | I32 | I64 | Pointer _ -> true | _ -> false

let key_of = function
  | Local (id, ty) when scalar ty -> Some (V id)
  | Param (name, ty) when scalar ty -> Some (P name)
  | _ -> None

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

(* [skip] holds the ids of instructions whose code is folded into their
   block's terminator; they define nothing that needs a home. *)
let allocate (f : func) =
  let start_pos = Hashtbl.create 16 and end_pos = Hashtbl.create 16 in
  let n = ref 1 in
  let calls = ref [] in
  List.iter
    (fun (b : block) ->
      Hashtbl.replace start_pos b.id !n;
      incr n;
      List.iter
        (fun ins ->
          if clobbers ins then calls := !n :: !calls;
          incr n)
        b.instrs;
      Hashtbl.replace end_pos b.id !n;
      incr n)
    f.blocks;
  let calls = Array.of_list (List.rev !calls) in
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
      (function Phi (id, ty, _) when scalar ty -> Some (V id) | _ -> None)
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
          | Some (id, ty) when scalar ty -> Hashtbl.replace defs (V id) ()
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
                Hashtbl.iter (fun k () -> if not (List.mem k sdefs) then add out k) ins
            | _ -> ())
          (terminator_successors b.terminator);
        let inn = Hashtbl.find live_in b.id in
        Hashtbl.iter (fun k () -> add inn k) (Hashtbl.find gen b.id);
        let defs = Hashtbl.find kill b.id in
        Hashtbl.iter (fun k () -> if not (Hashtbl.mem defs k) then add inn k) out)
      rev_blocks
  done;
  (* Intervals *)
  let lo = Hashtbl.create 64 and hi = Hashtbl.create 64 in
  let touch k p =
    (match Hashtbl.find_opt lo k with
    | Some l when l <= p -> ()
    | _ -> Hashtbl.replace lo k p);
    match Hashtbl.find_opt hi k with
    | Some h when h >= p -> ()
    | _ -> Hashtbl.replace hi k p
  in
  List.iter (fun (p : param) -> if scalar p.ty then touch (P p.name) 0) f.params;
  List.iter
    (fun (b : block) ->
      let s = Hashtbl.find start_pos b.id and e = Hashtbl.find end_pos b.id in
      Hashtbl.iter (fun k () -> touch k s) (Hashtbl.find live_in b.id);
      Hashtbl.iter (fun k () -> touch k e) (Hashtbl.find live_out b.id);
      List.iter (fun k -> touch k s) (phi_defs b);
      List.iteri
        (fun i ins ->
          let p = s + 1 + i in
          (match ins with
          | Phi _ -> ()
          | _ ->
              iter_operands (fun v -> Option.iter (fun k -> touch k p) (key_of v)) ins);
          match instruction_result ins with
          | Some (id, ty) when scalar ty -> touch (V id) p
          | _ -> ())
        b.instrs;
      iter_term_operands
        (fun v -> Option.iter (fun k -> touch k e) (key_of v))
        b.terminator;
      List.iter
        (fun succ -> List.iter (fun k -> touch k e) (phi_uses b.id succ))
        (terminator_successors b.terminator))
    f.blocks;
  let intervals =
    Hashtbl.fold (fun k l acc -> (l, Hashtbl.find hi k, k) :: acc) lo []
    |> List.sort compare
  in
  let crosses_call l h =
    (* a call at position c clobbers values live across it or used by it *)
    Array.exists (fun c -> l < c && c <= h) calls
  in
  let assignment = Hashtbl.create 64 in
  let active =
    ref []
    (* (hi, key, reg) *)
  in
  let free = ref (callee_saved @ caller_saved) in
  List.iter
    (fun (l, h, k) ->
      let expired, still = List.partition (fun (eh, _, _) -> eh < l) !active in
      List.iter (fun (_, _, r) -> free := r :: !free) expired;
      active := still;
      let crossing = crosses_call l h in
      let usable r = (not crossing) || List.mem r callee_saved in
      (* Prefer caller-saved registers for short, call-free intervals so the
         callee-saved ones stay for values that need them. *)
      let pick =
        let order = if crossing then callee_saved else caller_saved @ callee_saved in
        List.find_opt (fun r -> List.mem r !free) order
      in
      match pick with
      | Some r ->
          free := List.filter (( <> ) r) !free;
          Hashtbl.replace assignment k r;
          active := (h, k, r) :: !active
      | None -> (
          (* Spill whichever usable active interval ends last, if it ends
             after this one. *)
          let candidates = List.filter (fun (_, _, r) -> usable r) !active in
          match List.sort (fun (a, _, _) (b, _, _) -> compare b a) candidates with
          | (vh, vk, r) :: _ when vh > h ->
              Hashtbl.remove assignment vk;
              active := (h, k, r) :: List.filter (fun (_, k', _) -> k' <> vk) !active;
              Hashtbl.replace assignment k r
          | _ -> ()))
    intervals;
  assignment
