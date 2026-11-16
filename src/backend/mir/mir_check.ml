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

(* Sets of vregs as little-endian Patricia trees (Okasaki and Gill). A
   tree is canonical, so structural equality is set equality, and [inter]
   returns a physically shared subtree wherever its arguments share one:
   the meet of two states that came from one ancestor and differ in a few
   vregs builds only the paths to those vregs. Depth is bounded by the
   bits of an int. *)
module Iset = struct
  type t = Empty | Leaf of int | Branch of int * int * t * t (* prefix, branching bit, bit clear, bit set *)

  let empty = Empty
  let zero k m = k land m = 0
  let matches k p m = k land (m - 1) = p

  let join p0 t0 p1 t1 =
    let x = p0 lxor p1 in
    let m = x land -x in
    let p = p0 land (m - 1) in
    if zero p0 m then Branch (p, m, t0, t1) else Branch (p, m, t1, t0)

  let branch p m t0 t1 = match (t0, t1) with Empty, t | t, Empty -> t | _ -> Branch (p, m, t0, t1)

  let rec mem k = function
    | Empty -> false
    | Leaf j -> j = k
    | Branch (p, m, t0, t1) -> matches k p m && mem k (if zero k m then t0 else t1)

  let rec add k t =
    match t with
    | Empty -> Leaf k
    | Leaf j -> if j = k then t else join k (Leaf k) j t
    | Branch (p, m, t0, t1) ->
        if matches k p m then
          if zero k m then
            let t0' = add k t0 in
            if t0' == t0 then t else Branch (p, m, t0', t1)
          else
            let t1' = add k t1 in
            if t1' == t1 then t else Branch (p, m, t0, t1')
        else join k (Leaf k) p t

  let rec remove k t =
    match t with
    | Empty -> t
    | Leaf j -> if j = k then Empty else t
    | Branch (p, m, t0, t1) ->
        if not (matches k p m) then t
        else if zero k m then
          let t0' = remove k t0 in
          if t0' == t0 then t else branch p m t0' t1
        else
          let t1' = remove k t1 in
          if t1' == t1 then t else branch p m t0 t1'

  let rec inter s t =
    if s == t then s
    else
      match (s, t) with
      | Empty, _ | _, Empty -> Empty
      | Leaf k, _ -> if mem k t then s else Empty
      | _, Leaf k -> if mem k s then t else Empty
      | Branch (p, m, s0, s1), Branch (q, n, t0, t1) ->
          if m = n && p = q then
            let r0 = inter s0 t0 and r1 = inter s1 t1 in
            if r0 == s0 && r1 == s1 then s else branch p m r0 r1
          else if m < n && matches q p m then inter (if zero q m then s0 else s1) t
          else if n < m && matches p q n then inter s (if zero p n then t0 else t1)
          else Empty

  let rec equal s t =
    s == t
    ||
    match (s, t) with
    | Leaf a, Leaf b -> a = b
    | Branch (p, m, s0, s1), Branch (q, n, t0, t1) -> p = q && m = n && equal s0 t0 && equal s1 t1
    | _ -> false
end

(* [regs]: what each physical register holds (0..15 general, 16..31 xmm).
   A vreg's slot can only ever hold that vreg's current value or garbage,
   so the slots are a set of vregs ("v's slot holds v"), not a tag. The set
   is persistent: it holds only spilled vregs and shares its structure
   between blocks, so a function with 100k blocks and a million vregs does
   not keep a dense bitmap per block (that was O(blocks x vregs) memory). *)
type state = { regs : tag array; slots : Iset.t; virt : tag array (* physical registers only *) }

let copy_state s = { regs = Array.copy s.regs; slots = s.slots; virt = Array.copy s.virt }

let meet a b =
  let m x y = if x = y then x else Unknown in
  let slots = Iset.inter a.slots b.slots in
  { regs = Array.map2 m a.regs b.regs; slots; virt = Array.map2 m a.virt b.virt }

let state_equal a b = a.regs = b.regs && a.virt = b.virt && Iset.equal a.slots b.slots

let run (f : func) (ra : Mir_ra.result) =
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
    { regs; slots = Iset.empty; virt }
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
    let a = st.regs and sl = ref st.slots and vt = st.virt in
    let set_vreg v loc =
      (* a new value of v: every older copy is stale *)
      (* a copy of v can only be in a register or in v's own slot *)
      for i = 0 to nphys - 1 do
        if a.(i) = Vt v then a.(i) <- Unknown
      done;
      sl := Iset.remove v !sl;
      a.(loc) <- Vt v
    in
    List.iteri
      (fun k item ->
        match item with
        | Mir_ra.Reload (v, r) ->
            let s = ra.vslot.(v) in
            if s < 0 then fail bi (Copy (r, V v)) "reload of a vreg without a slot";
            if not (Iset.mem v !sl) then fail bi (Copy (r, V v)) "reload: the slot does not hold %%%d" v;
            if class_of_phys r <> Dynarray.get f.vcls v then fail bi (Copy (r, V v)) "reload into the wrong class";
            a.(phys_index r) <- Vt v
        | Spill (v, r) ->
            if a.(phys_index r) <> Vt v then fail bi (Copy (V v, r)) "spill: the register does not hold %%%d" v;
            sl := Iset.add v !sl
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
              (clobbers before)
        | Move _ | Ld _ | St _ | Remat _ | Elided _ -> fail bi Ud2 "an imported allocation's item (Mir_icheck checks those)")
      ra.items.(bi);
    (* the terminator *)
    List.iter
      (fun p ->
        let i = phys_index p in
        if vt.(i) = Unknown || a.(i) <> vt.(i) then
          raise (Failure (Printf.sprintf "%s: block %d: %s does not hold the returned value" f.name bi (reg_s p))))
      (term_uses ra.terms.(bi));
    { st with slots = !sl }
  in
  (* fixpoint over the blocks in reverse postorder, visiting only blocks
     whose entry state changed. Block indices are not in flow order (split
     edges come last), and a sweep in index order moved a state one hop per
     round: O(blocks^2) steps on a long chain. In reverse postorder a sweep
     covers every forward edge, so the sweeps are bounded by the loop
     nesting. The order is computed without recursion (100k+ blocks). *)
  let rpo =
    let seen = Array.make nb false and po = Dynarray.create () in
    let stack = Stack.create () in
    if nb > 0 then begin
      seen.(0) <- true;
      Stack.push (0, successors ra.terms.(0)) stack
    end;
    while not (Stack.is_empty stack) do
      let b, rest = Stack.pop stack in
      match rest with
      | [] -> Dynarray.add_last po b
      | s :: rest ->
          Stack.push (b, rest) stack;
          if not seen.(s) then begin
            seen.(s) <- true;
            Stack.push (s, successors ra.terms.(s)) stack
          end
    done;
    Array.of_list (List.rev (Dynarray.to_list po))
  in
  let pending = Array.make nb false in
  if nb > 0 then pending.(0) <- true;
  let changed = ref true in
  while !changed do
    changed := false;
    Array.iter
      (fun bi ->
        if pending.(bi) then begin
          pending.(bi) <- false;
          match entry_state.(bi) with
          | None -> ()
          | Some s ->
              let out = step bi s in
              List.iter
                (fun succ ->
                  let ns = match entry_state.(succ) with None -> out | Some old -> meet old out in
                  let ns = if succ = 0 then meet ns init else ns in
                  let same = match entry_state.(succ) with Some old -> state_equal old ns | None -> false in
                  if not same then begin
                    entry_state.(succ) <- Some ns;
                    pending.(succ) <- true;
                    changed := true
                  end)
                (successors ra.terms.(bi))
        end)
      rpo
  done
