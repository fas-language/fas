(* Mir_ra: register allocation, spill-everywhere (roadmap v0.4).

   Every virtual register owns an 8-byte frame slot. Each definition is
   written to its register and immediately stored to the slot (a "spill");
   each use reads the value from a register that still holds it, or loads it
   from the slot first (a "reload"). The only state carried between
   instructions is a block-local cache of which register currently holds a
   copy of which virtual register, so a value defined and used close
   together costs no reload. Nothing is carried across blocks: at a block
   entry every value is in its slot.

   This is deliberately the simplest allocator that is obviously correct;
   [Mir_check] verifies its output independently, so a better allocator can
   replace it later without trusting it.

   Constraints at one instruction:
   - a physical register the instruction names (a fixed operand, an
     implicit one such as rcx for a shift, or one it clobbers) is never
     assigned to a virtual register there;
   - neither is a physical register live across the instruction (holding a
     call argument being set up, say);
   - virtual registers the instruction reads get distinct registers; a
     virtual register it only writes gets one distinct from all of them
     (simple, and safe for instructions that write before their last read);
     one it reads and writes keeps the same register. *)

open Mir

type item =
  | Orig of ins * ins  (** the instruction before and after allocation *)
  | Reload of int * reg  (** vreg n's slot -> register *)
  | Spill of int * reg  (** register -> vreg n's slot *)

type result = {
  items : item list array;  (** per block, program order *)
  terms : term array;
  vslot : int array;  (** each vreg's frame slot (-1: never used) *)
  saved : X64.reg list;  (** callee-saved registers the function writes *)
}

exception Failure of string

let gpr_pool = X64.[ R10; R11; Rax; Rcx; Rdx; Rsi; Rdi; R8; R9; Rbx; R12; R13; R14; R15 ]
let xmm_pool = List.init 8 (fun i -> i + 8) @ List.init 8 (fun i -> i)
let is_phys = function V _ -> false | _ -> true

module RS = Set.Make (struct
  type t = reg

  let compare = compare
end)

let phys_set l = RS.of_list (List.filter is_phys l)

(* the physical registers live before each instruction of a block, and
   after its last one; nothing physical is live across a block boundary
   except what the terminator reads *)
let phys_liveness (b : block) =
  let code = Array.of_list b.code in
  let n = Array.length code in
  let live = Array.make (n + 1) RS.empty in
  live.(n) <- phys_set (term_uses b.term);
  for i = n - 1 downto 0 do
    let u, d = uses_defs code.(i) in
    let killed = RS.union (phys_set d) (phys_set (clobbers code.(i))) in
    live.(i) <- RS.union (RS.diff live.(i + 1) killed) (phys_set u)
  done;
  (code, live)

let run (f : func) =
  let nv = Dynarray.length f.vcls in
  let vslot = Array.make nv (-1) in
  let slot_of v =
    if vslot.(v) < 0 then vslot.(v) <- new_slot f ~size:8 ~align:8;
    vslot.(v)
  in
  let saved = ref [] in
  let note_saved = function
    | G r when List.mem r callee_saved && not (List.mem r !saved) -> saved := r :: !saved
    | _ -> ()
  in
  let nb = Dynarray.length f.blocks in
  let items = Array.make nb [] and terms = Array.make nb Halt in
  for bi = 0 to nb - 1 do
    let b = Dynarray.get f.blocks bi in
    let code, live = phys_liveness b in
    (* the block-local cache, both directions *)
    let holder : (reg, int) Hashtbl.t = Hashtbl.create 32 in
    let where : (int, reg) Hashtbl.t = Hashtbl.create 32 in
    let evict r =
      match Hashtbl.find_opt holder r with
      | Some v ->
          Hashtbl.remove holder r;
          Hashtbl.remove where v
      | None -> ()
    in
    let forget v = match Hashtbl.find_opt where v with Some r -> evict r | None -> () in
    let cache v r =
      forget v;
      evict r;
      Hashtbl.replace holder r v;
      Hashtbl.replace where v r
    in
    let out = ref [] in
    let push x = out := x :: !out in
    Array.iteri
      (fun i ins ->
        let u, d = uses_defs ins in
        let mentioned = phys_set (u @ d @ clobbers ins) in
        let forbidden = RS.union mentioned (RS.union live.(i) live.(i + 1)) in
        let assigned : (int, reg) Hashtbl.t = Hashtbl.create 4 in
        let taken () = Hashtbl.fold (fun _ r s -> RS.add r s) assigned RS.empty in
        let pick v =
          let cls = Dynarray.get f.vcls v in
          let pool = match cls with Gpr -> List.map (fun r -> G r) gpr_pool | Xmm -> List.map (fun i -> X i) xmm_pool in
          let bad = RS.union forbidden (taken ()) in
          let ok = List.filter (fun r -> not (RS.mem r bad)) pool in
          (* prefer a register whose cached value nobody will miss *)
          match List.find_opt (fun r -> not (Hashtbl.mem holder r)) ok with
          | Some r -> r
          | None -> (
              match ok with
              | r :: _ -> r
              | [] -> raise (Failure (Printf.sprintf "%s: no register for %%%d at %s" f.name v (ins_s ins))))
        in
        let uvs = List.sort_uniq compare (List.filter_map (function V v -> Some v | _ -> None) u) in
        let dvs = List.sort_uniq compare (List.filter_map (function V v -> Some v | _ -> None) d) in
        (* uses: a cached copy when it is in an allowed register, else a reload *)
        List.iter
          (fun v ->
            let r =
              match Hashtbl.find_opt where v with
              | Some r when not (RS.mem r forbidden || RS.mem r (taken ())) -> r
              | _ ->
                  let r = pick v in
                  push (Reload (v, r));
                  cache v r;
                  r
            in
            Hashtbl.replace assigned v r)
          uvs;
        (* defs: the use's register when also read, else a fresh one *)
        List.iter (fun v -> if not (Hashtbl.mem assigned v) then Hashtbl.replace assigned v (pick v)) dvs;
        let g = function V v -> Hashtbl.find assigned v | r -> r in
        let after = map_regs g ins in
        push (Orig (ins, after));
        Hashtbl.iter (fun _ r -> note_saved r) assigned;
        (* physical writes destroy cached copies *)
        RS.iter evict (RS.union (phys_set d) (phys_set (clobbers ins)));
        List.iter
          (fun v ->
            let r = Hashtbl.find assigned v in
            ignore (slot_of v);
            cache v r;
            push (Spill (v, r)))
          dvs;
        List.iter (fun v -> ignore (slot_of v)) uvs)
      code;
    List.iter (function V _ -> raise (Failure "virtual register in a terminator") | _ -> ()) (term_uses b.term);
    items.(bi) <- List.rev !out;
    terms.(bi) <- b.term
  done;
  { items; terms; vslot; saved = List.rev !saved }

let item_s = function
  | Orig (_, a) -> "  " ^ ins_s a
  | Reload (v, r) -> Printf.sprintf "  %s = reload %%%d" (reg_s r) v
  | Spill (v, r) -> Printf.sprintf "  spill %%%d, %s" v (reg_s r)

let to_string (f : func) (r : result) =
  let b = Buffer.create 1024 in
  Printf.bprintf b "func %s (allocated):\n" f.name;
  Array.iteri
    (fun i its ->
      Printf.bprintf b "b%d:\n" i;
      List.iter (fun it -> Printf.bprintf b "%s\n" (item_s it)) its;
      Printf.bprintf b "  %s\n" (term_s r.terms.(i)))
    r.items;
  Buffer.contents b
