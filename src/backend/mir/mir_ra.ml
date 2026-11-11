(* Mir_ra: register allocation, fixed-interval linear scan (roadmap v0.4).

   Deliberately simple; [Mir_check] verifies its output independently, so
   it never has to be trusted, and a better allocator can replace it.

   1. Positions. The blocks are laid out in order and every instruction
      gets a number, plus one for each block's terminator.

   2. Intervals. Each virtual register gets ONE interval [lo, hi]: the hull
      of every position where it is mentioned, the start of every block
      where it is live in and the terminator of every block where it is live
      out. The hull contains every point where the value is live, whatever
      the control flow, so two virtual registers whose hulls do not overlap
      can share a register. No splitting, no holes, no coalescing.
      Liveness is computed per virtual register, by walking backwards from
      the blocks that use it before defining it (memory stays linear in the
      program, even for functions with millions of virtual registers).

      Overlap is decided per instruction, inclusively: a register read for
      the last time by an instruction and one it writes never share, which
      keeps instructions that write before their last read safe.

   3. Physical constraints. A physical register is "blocked" at an
      instruction when the instruction names it (a fixed operand, an
      implicit one such as rcx for a shift, or one it clobbers, a call's
      caller-saved registers among them) or when it is live across it
      (holding a call argument being set up, say). A virtual register can
      only get a physical register blocked nowhere in its interval; so one
      live across a call gets a callee-saved register or the stack.

   4. Linear scan (Poletto and Sarkar). Intervals by start; each takes the
      first free register of its class (caller-saved first). When none is
      free, the interval that ends last (this one or one already holding a
      register it could use) goes to the stack for its whole life.

   5. Spilled virtual registers live in an 8-byte frame slot, one per
      virtual register and never shared. A call marked returns_twice
      (setjmp) depends on that: after a longjmp, a value from before the
      call is read again from its slot, or from a callee-saved register,
      which longjmp restores, even when code that ran in between reused
      the register. A slot shared by intervals that do not overlap in the
      layout would break it, because the second return is an edge the
      hull does not see. At each
      instruction that mentions one, it is loaded into a scratch register
      before ("reload") and stored back after a write ("spill"). The scratch
      registers are kept out of the linear scan whenever something spills:
      r13, r14, r15 and xmm14, xmm15, which instruction selection names
      only for inline asm operands and clobbers, so they are free at almost
      every instruction (an instruction has at most three virtual GPR
      operands and two xmm ones). Where one is named by the instruction or
      live across it (an asm input being set up, say), another register
      free there takes its place: one not blocked at the instruction and
      not holding a virtual register live across it. If nothing spills,
      the scan runs once with every register.

   FAS_MIR_REGS=n (debugging) gives the scan only the first n registers of
   each class, to exercise spilling on small programs. *)

open Mir

type item =
  | Orig of ins * ins  (** the instruction before and after allocation *)
  | Reload of int * reg  (** vreg n's slot -> register *)
  | Spill of int * reg  (** register -> vreg n's slot *)

type result = {
  items : item list array;  (** per block, program order *)
  terms : term array;
  vslot : int array;  (** each spilled vreg's frame slot (-1: in a register, or never used) *)
  saved : X64.reg list;  (** callee-saved registers the function writes *)
}

exception Failure of string

let gpr_pool = X64.[ R10; R11; Rax; Rcx; Rdx; Rsi; Rdi; R8; R9; Rbx; R12; R13; R14; R15 ]
let xmm_pool = List.init 8 (fun i -> i + 8) @ List.init 8 (fun i -> i)
let gpr_scratch = X64.[ R13; R14; R15 ]
let xmm_scratch = [ 14; 15 ]
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

(* physical registers as 0..31 (0..15 general, 16..31 xmm) *)
let nphys = 32

let pidx = function
  | G r -> X64.reg_num r
  | X i -> 16 + i
  | V _ -> invalid_arg "Mir_ra.pidx"

let vregs l = List.sort_uniq compare (List.filter_map (function V v -> Some v | _ -> None) l)

let debug_limit =
  match Sys.getenv_opt "FAS_MIR_REGS" with
  | Some s -> ( match int_of_string_opt s with Some n when n > 0 -> Some n | _ -> None)
  | None -> None

let run (f : func) =
  let nv = Dynarray.length f.vcls in
  let nb = Dynarray.length f.blocks in
  let codes = Array.init nb (fun bi -> Array.of_list (Dynarray.get f.blocks bi).code) in
  (* 1. positions *)
  let bstart = Array.make nb 0 in
  (let p = ref 0 in
   for bi = 0 to nb - 1 do
     bstart.(bi) <- !p;
     p := !p + Array.length codes.(bi) + 1
   done);
  let bterm bi = bstart.(bi) + Array.length codes.(bi) in
  (* 3. blocked positions of each physical register, in increasing order *)
  let blocked = Array.init nphys (fun _ -> Dynarray.create ()) in
  for bi = 0 to nb - 1 do
    let code, live = phys_liveness (Dynarray.get f.blocks bi) in
    Array.iteri
      (fun i ins ->
        let u, d = uses_defs ins in
        let s = RS.union (phys_set (u @ d @ clobbers ins)) (RS.union live.(i) live.(i + 1)) in
        RS.iter (fun r -> Dynarray.add_last blocked.(pidx r) (bstart.(bi) + i)) s)
      code;
    RS.iter (fun r -> Dynarray.add_last blocked.(pidx r) (bterm bi)) live.(Array.length code);
    List.iter (function V _ -> raise (Failure "virtual register in a terminator") | _ -> ()) (term_uses (Dynarray.get f.blocks bi).term)
  done;
  (* is r blocked somewhere in [lo, hi]? *)
  let is_blocked p lo hi =
    let a = blocked.(p) in
    let rec search l h = (* first index with a.(i) >= lo *)
      if l >= h then l else
        let m = (l + h) / 2 in
        if Dynarray.get a m < lo then search (m + 1) h else search l m
    in
    let i = search 0 (Dynarray.length a) in
    i < Dynarray.length a && Dynarray.get a i <= hi
  in
  (* 2. intervals: mentions, then liveness across blocks *)
  let lo = Array.make nv max_int and hi = Array.make nv (-1) in
  let extend v p =
    if p < lo.(v) then lo.(v) <- p;
    if p > hi.(v) then hi.(v) <- p
  in
  let ue = Array.make nv [] and defb = Array.make nv [] in
  let dstamp = Array.make nv (-1) and ustamp = Array.make nv (-1) in
  for bi = 0 to nb - 1 do
    Array.iteri
      (fun i ins ->
        let u, d = uses_defs ins in
        List.iter
          (fun v ->
            extend v (bstart.(bi) + i);
            if dstamp.(v) <> bi && ustamp.(v) <> bi then begin
              ustamp.(v) <- bi;
              ue.(v) <- bi :: ue.(v)
            end)
          (vregs u);
        List.iter
          (fun v ->
            extend v (bstart.(bi) + i);
            if dstamp.(v) <> bi then begin
              dstamp.(v) <- bi;
              defb.(v) <- bi :: defb.(v)
            end)
          (vregs d))
      codes.(bi)
  done;
  let preds = Array.make nb [] in
  for bi = 0 to nb - 1 do
    List.iter (fun s -> preds.(s) <- bi :: preds.(s)) (successors (Dynarray.get f.blocks bi).term)
  done;
  let defmark = Array.make nb (-1) and livein = Array.make nb (-1) in
  for v = 0 to nv - 1 do
    if ue.(v) <> [] then begin
      List.iter (fun b -> defmark.(b) <- v) defb.(v);
      let stack = ref ue.(v) in
      List.iter (fun b -> livein.(b) <- v) ue.(v);
      while !stack <> [] do
        let b = List.hd !stack in
        stack := List.tl !stack;
        extend v bstart.(b);
        List.iter
          (fun p ->
            extend v (bterm p);
            if defmark.(p) <> v && livein.(p) <> v then begin
              livein.(p) <- v;
              stack := p :: !stack
            end)
          preds.(b)
      done
    end
  done;
  (* 4. linear scan *)
  let order =
    let l = ref [] in
    for v = nv - 1 downto 0 do
      if hi.(v) >= 0 then l := v :: !l
    done;
    List.stable_sort (fun a b -> compare lo.(a) lo.(b)) !l
  in
  let scan ~reserve =
    let take n l = match debug_limit with Some k -> List.filteri (fun i _ -> i < min k n) l | None -> l in
    let gp = List.filter (fun r -> not (reserve && List.mem r gpr_scratch)) gpr_pool in
    let xp = List.filter (fun i -> not (reserve && List.mem i xmm_scratch)) xmm_pool in
    let gp = List.map (fun r -> G r) (take (List.length gp) gp)
    and xp = List.map (fun i -> X i) (take (List.length xp) xp) in
    let assign = Array.make nv None in
    let hold_end = Array.make nphys (-1) and hold_v = Array.make nphys (-1) in
    let spilled = ref false in
    List.iter
      (fun v ->
        let s = lo.(v) and e = hi.(v) in
        let pool = match Dynarray.get f.vcls v with Gpr -> gp | Xmm -> xp in
        let usable r = not (is_blocked (pidx r) s e) in
        match List.find_opt (fun r -> hold_end.(pidx r) < s && usable r) pool with
        | Some r ->
            assign.(v) <- Some r;
            hold_end.(pidx r) <- e;
            hold_v.(pidx r) <- v
        | None -> (
            (* the occupied register whose interval ends last, if after this one *)
            let best =
              List.fold_left
                (fun acc r ->
                  let p = pidx r in
                  if hold_end.(p) > e && usable r then
                    match acc with Some r' when hold_end.(pidx r') >= hold_end.(p) -> acc | _ -> Some r
                  else acc)
                None pool
            in
            spilled := true;
            match best with
            | Some r ->
                let p = pidx r in
                assign.(hold_v.(p)) <- None;
                assign.(v) <- Some r;
                hold_end.(p) <- e;
                hold_v.(p) <- v
            | None -> ()))
      order;
    (assign, !spilled)
  in
  let assign, reserved =
    match scan ~reserve:false with
    | a, false when debug_limit = None -> (a, false)
    | _ -> (fst (scan ~reserve:true), true)
  in
  (* 5. rewrite *)
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
  let items = Array.make nb [] and terms = Array.make nb Halt in
  (* a register of class [cls] free at position p, other than [avoid]: not
     blocked there and not holding a virtual register whose interval covers
     p (the scratch fallback; rare, so a plain search) *)
  let free_at cls p avoid =
    let pool = match cls with Gpr -> List.map (fun r -> G r) gpr_pool | Xmm -> List.map (fun i -> X i) xmm_pool in
    List.find_opt
      (fun r ->
        (not (RS.mem r avoid))
        && (not (is_blocked (pidx r) p p))
        &&
        let held = ref false in
        Array.iteri (fun v a -> if a = Some r && lo.(v) <= p && p <= hi.(v) then held := true) assign;
        not !held)
      pool
  in
  for bi = 0 to nb - 1 do
    let b = Dynarray.get f.blocks bi in
    let out = ref [] in
    let push x = out := x :: !out in
    let _, plive = phys_liveness b in
    Array.iteri
      (fun i ins ->
        let u, d = uses_defs ins in
        let p = bstart.(bi) + i in
        let scratch = Hashtbl.create 4 in
        (* physical registers this instruction names or that live across it *)
        let taken = ref (RS.union (phys_set (u @ d @ clobbers ins)) (RS.union plive.(i) plive.(i + 1))) in
        let ok r = not (RS.mem r !taken) in
        let gs = ref (List.filter ok (List.map (fun r -> G r) gpr_scratch))
        and xs = ref (List.filter ok (List.map (fun i -> X i) xmm_scratch)) in
        let g = function
          | V v -> (
              match assign.(v) with
              | Some r -> r
              | None -> (
                  match Hashtbl.find_opt scratch v with
                  | Some r -> r
                  | None ->
                      if not reserved then raise (Failure (Printf.sprintf "%s: %%%d spilled without scratch" f.name v));
                      let cls = Dynarray.get f.vcls v in
                      let pool = match cls with Gpr -> gs | Xmm -> xs in
                      (match !pool with
                      | r :: rest ->
                          pool := rest;
                          Hashtbl.add scratch v r
                      | [] -> (
                          match free_at cls p !taken with
                          | Some r -> Hashtbl.add scratch v r
                          | None ->
                              raise (Failure (Printf.sprintf "%s: out of scratch registers at %s" f.name (ins_s ins)))));
                      taken := RS.add (Hashtbl.find scratch v) !taken;
                      Hashtbl.find scratch v))
          | r -> r
        in
        let uvs = vregs u and dvs = vregs d in
        List.iter (fun v -> if assign.(v) = None then push (Reload (v, g (V v)))) uvs;
        let after = map_regs g ins in
        List.iter note_saved (all_regs after);
        List.iter note_saved (clobbers after);
        push (Orig (ins, after));
        List.iter (fun v -> if assign.(v) = None then push (Spill (v, g (V v)))) dvs;
        List.iter (fun v -> if assign.(v) = None then ignore (slot_of v)) (uvs @ dvs))
      codes.(bi);
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
