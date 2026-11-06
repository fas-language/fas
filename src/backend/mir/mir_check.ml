(* Mir_check: an independent check of register allocation (roadmap v0.4).

   It does not trust Mir_ra. It runs two abstract machines side by side over
   the allocated program:

   - the "actual" machine tracks what every physical register and every
     vreg spill slot holds after allocation: the current value of vreg v
     ([Vt v]), a value some instruction wrote to a physical register
     ([Pt id]), or [Unknown];
   - the "virtual" machine follows the program as selected, before
     allocation, but only for physical registers: a [Pt id] where the
     selector itself wrote a fixed register (call arguments, rax around a
     division...).

   For each original instruction it checks that the allocated form is the
   original with virtual registers substituted (consistently, by a register
   of the right class, and leaving physical registers alone), that each
   virtual register read is in a register holding exactly that vreg's
   current value, and that each physical register read holds in the actual
   machine what the selected program put there. Reloads must read a slot
   holding the vreg; spills must store a register holding it. Writing a vreg
   makes every older copy of it stale. Block entry states are the meet of
   the predecessors' exit states, iterated to a fixpoint.

   A failure means the allocator (or this checker) is wrong, never the
   input program, so it is reported as an internal error with the
   instruction. *)

open Mir

type tag = Vt of int | Pt of int | Unknown

exception Failure of string

let nphys = 32

let phys_index = function
  | G r -> X64.reg_num r
  | X i -> 16 + i
  | V _ -> invalid_arg "Mir_check.phys_index"

(* [regs]: what each physical register holds (0..15 general, 16..31 xmm).
   A vreg's slot can only ever hold that vreg's current value or garbage,
   so the slots are one bit per vreg ("v's slot holds v"), not a tag: the
   per-block state stays small for functions with millions of vregs. *)
type state = { regs : tag array; slots : Bytes.t; virt : tag array (* physical registers only *) }

let copy_state s = { regs = Array.copy s.regs; slots = Bytes.copy s.slots; virt = Array.copy s.virt }

let slot_get b v = Char.code (Bytes.unsafe_get b (v lsr 3)) land (1 lsl (v land 7)) <> 0

let slot_set b v on =
  let i = v lsr 3 and m = 1 lsl (v land 7) in
  let c = Char.code (Bytes.get b i) in
  Bytes.set b i (Char.unsafe_chr (if on then c lor m else c land lnot m))

let meet a b =
  let m x y = if x = y then x else Unknown in
  let slots = Bytes.copy a.slots in
  for i = 0 to Bytes.length slots - 1 do
    Bytes.unsafe_set slots i
      (Char.unsafe_chr (Char.code (Bytes.unsafe_get slots i) land Char.code (Bytes.unsafe_get b.slots i)))
  done;
  { regs = Array.map2 m a.regs b.regs; slots; virt = Array.map2 m a.virt b.virt }

let run (f : func) (ra : Mir_ra.result) =
  let nv = Dynarray.length f.vcls in
  let nb = Array.length ra.items in
  let fail bi ins fmt =
    Printf.ksprintf
      (fun s -> raise (Failure (Printf.sprintf "%s: block %d: %s: %s" f.name bi (ins_s ins) s)))
      fmt
  in
  let ids = ref 0 in
  let fresh () =
    incr ids;
    Pt !ids
  in
  (* the entry: every physical register holds its incoming value *)
  let init =
    let regs = Array.make nphys Unknown and virt = Array.make nphys Unknown in
    for p = 0 to nphys - 1 do
      let t = fresh () in
      regs.(p) <- t;
      virt.(p) <- t
    done;
    { regs; slots = Bytes.make ((nv + 7) / 8) '\000'; virt }
  in
  let entry_state = Array.make nb None in
  entry_state.(0) <- Some init;
  (* identities of physical writes must be the same on every pass, so they
     are keyed by (block, item, register) *)
  let pt_ids = Hashtbl.create 256 in
  let pt key =
    match Hashtbl.find_opt pt_ids key with
    | Some t -> t
    | None ->
        let t = fresh () in
        Hashtbl.add pt_ids key t;
        t
  in
  let step bi s =
    let st = copy_state s in
    let a = st.regs and sl = st.slots and vt = st.virt in
    let set_vreg v loc =
      (* a new value of v: every older copy is stale *)
      (* a copy of v can only be in a register or in v's own slot *)
      for i = 0 to nphys - 1 do
        if a.(i) = Vt v then a.(i) <- Unknown
      done;
      slot_set sl v false;
      a.(loc) <- Vt v
    in
    List.iteri
      (fun k item ->
        match item with
        | Mir_ra.Reload (v, r) ->
            let s = ra.vslot.(v) in
            if s < 0 then fail bi (Copy (r, V v)) "reload of a vreg without a slot";
            if not (slot_get sl v) then fail bi (Copy (r, V v)) "reload: the slot does not hold %%%d" v;
            if class_of_phys r <> Dynarray.get f.vcls v then fail bi (Copy (r, V v)) "reload into the wrong class";
            a.(phys_index r) <- Vt v
        | Spill (v, r) ->
            if a.(phys_index r) <> Vt v then fail bi (Copy (V v, r)) "spill: the register does not hold %%%d" v;
            slot_set sl v true
        | Orig (before, after) ->
            (* the substitution *)
            let sub = Hashtbl.create 4 in
            let rb = all_regs before and rf = all_regs after in
            if List.length rb <> List.length rf then fail bi before "operand count changed";
            List.iter2
              (fun x y ->
                match x with
                | V v -> (
                    if not (Mir_ra.is_phys y) then fail bi before "a vreg is left after allocation";
                    if class_of_phys y <> Dynarray.get f.vcls v then fail bi before "%%%d in the wrong class" v;
                    match Hashtbl.find_opt sub v with
                    | Some y' when y' <> y -> fail bi before "%%%d gets two registers" v
                    | _ -> Hashtbl.replace sub v y)
                | p -> if p <> y then fail bi before "physical operand %s changed" (reg_s p))
              rb rf;
            let g = function V v -> Hashtbl.find sub v | p -> p in
            if map_regs g before <> after then fail bi before "allocated form differs: %s" (ins_s after);
            let u, d = uses_defs before in
            (* reads *)
            List.iter
              (function
                | V v ->
                    let r = g (V v) in
                    if a.(phys_index r) <> Vt v then
                      fail bi before "%s does not hold %%%d" (reg_s r) v
                | p ->
                    let i = phys_index p in
                    if vt.(i) = Unknown then fail bi before "reads %s before the program writes it" (reg_s p);
                    if a.(i) <> vt.(i) then fail bi before "%s was overwritten before this read" (reg_s p))
              u;
            (* writes, then clobbers *)
            List.iter
              (function
                | V v -> set_vreg v (phys_index (g (V v)))
                | p ->
                    let t = pt (bi, k, phys_index p) in
                    a.(phys_index p) <- t;
                    vt.(phys_index p) <- t)
              d;
            List.iter
              (fun p ->
                a.(phys_index p) <- Unknown;
                vt.(phys_index p) <- Unknown)
              (clobbers before))
      ra.items.(bi);
    (* the terminator *)
    List.iter
      (fun p ->
        let i = phys_index p in
        if vt.(i) = Unknown || a.(i) <> vt.(i) then
          raise (Failure (Printf.sprintf "%s: block %d: %s does not hold the returned value" f.name bi (reg_s p))))
      (term_uses ra.terms.(bi));
    st
  in
  (* fixpoint over the blocks in order *)
  let changed = ref true in
  while !changed do
    changed := false;
    for bi = 0 to nb - 1 do
      match entry_state.(bi) with
      | None -> ()
      | Some s ->
          let out = step bi s in
          List.iter
            (fun succ ->
              let ns = match entry_state.(succ) with None -> out | Some old -> meet old out in
              let ns = if succ = 0 then meet ns init else ns in
              if entry_state.(succ) <> Some ns then begin
                entry_state.(succ) <- Some ns;
                changed := true
              end)
            (successors ra.terms.(bi))
    done
  done
