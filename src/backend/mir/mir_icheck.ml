(* Mir_icheck: an independent check of an imported allocation
   (FAS_RA_IMPORT, Mir_import).

   It trusts neither the exporter's SSA nor the importer; it reads only the
   selected Mir and the allocated items. An imported allocation may keep a
   value in several places, move it between registers and frame slots,
   recompute constants and drop Mir's register copies, so the check works
   on values rather than on virtual registers (Mir_check's approach):

   1. Structure. Every reachable Mir block keeps its code exactly (each
      instruction as [Orig] or, for a copy, [Elided], in order) and its
      terminator, except that a successor may become a new block (an edge
      split) holding only edits and a jump to it. Edges out of indirect
      jumps and asm goto labels stay.

   2. Values. The variables are the virtual registers and the 32 physical
      ones. A value id is one of: the incoming value of a physical
      register; the result of one definition (block, item, operand); a
      merge m(b, x), the value of variable x on entry to block b where b
      has several predecessors (pessimistic SSA: every variable live into a
      merge block is a new value there; at a single predecessor the values
      carry over). An elided copy makes its destination the source's value;
      a clobber leaves the register without one (undefined). An undefined
      variable may be read (an uninitialized local): any contents do.

   3. Locations: the 32 registers and the allocation's slots, each holding
      a SET of value ids (a value may be in several places and several
      variables may share a value). Running the allocated block:
      - an [Orig] must be the Mir instruction with each virtual register
        replaced by a register of its class, the same one wherever it is
        read and wherever it is written ([Mir.map_regs_ud]), physical
        registers unchanged; each register it reads must hold the value of
        the variable the Mir reads there; each register it writes then
        holds the new value alone, and the value leaves every other
        location (an older run of the same definition, in a loop, is
        stale); clobbered registers hold nothing;
      - [Move], [Ld], [St] copy the set; a [St] of an xmm register into an
        8-byte slot keeps the low half of each value, which serves every
        reader but those of all 16 bytes ([Xstore], a 128-bit asm
        operand); into a 16-byte slot it keeps all of it;
      - [Remat] must recompute a constant (an immediate, the address of a
        slot or symbol, a GOT or TLS load) the Mir defines at (block,
        index); its register then holds that definition's value, which
        every run produces alike, so other copies stay.
      Across an edge p -> t, a location holding the value of variable x at
      the end of p holds x's value at the start of t (the same id, or the
      merge m(t, x)); a variable undefined at the end of p imposes nothing.
      Block entry states are the intersection over the predecessors,
      iterated to a fixpoint. The function's entry acts as one more
      predecessor of block 0.

   4. Also: a callee-saved register written anywhere is in [saved]; rsp
      and rbp are never allocated; moves stay within a class.

   5. returns_twice (setjmp): the second return comes back after the call
      with the callee-saved registers as at the call and the slots as they
      are then. Conservatively: a slot holding anything at such a call is
      never stored to anywhere reachable from that call.

   Liveness (of the variables, over the allocated CFG) only keeps the
   per-block states small. A failure means the allocation (or this
   checker) is wrong, never the program. *)

open Mir

exception Failure of string

module IS = Set.Make (Int)

let nphys = 32
let pidx = function G r -> X64.reg_num r | X i -> 16 + i | V _ -> invalid_arg "Mir_icheck.pidx"

let is_edit = function Mir_ra.Move _ | Ld _ | St _ | Remat _ -> true | Orig _ | Elided _ | Reload _ | Spill _ -> false

(* the instructions whose result is the same on every run: a constant, an
   address fixed for the whole call, a thread's TLS block *)
let remat_ok = function
  | Movi (_, _) | Tlsoff _ | Got _ | Tp _ -> true
  | Lea (_, { base = Bslot _ | Bsym _ | Bnone | Bin; index = None; _ }) -> true
  | _ -> false

let run (f : func) (ra : Mir_ra.result) ~(slots : int array) =
  let nv = Dynarray.length f.vcls in
  let nvar = nv + nphys in
  let nb0 = Dynarray.length f.blocks in
  let nb = Array.length ra.items in
  let nloc = nphys + Array.length slots in
  let fail fmt = Printf.ksprintf (fun s -> raise (Failure (Printf.sprintf "%s: %s" f.name s))) fmt in
  let failb b fmt = Printf.ksprintf (fun s -> raise (Failure (Printf.sprintf "%s: block %d: %s" f.name b s))) fmt in
  if nb < nb0 || Array.length ra.terms <> nb then fail "%d blocks for %d Mir blocks" nb nb0;
  let slot_loc = Hashtbl.create 16 in
  Array.iteri (fun i s -> Hashtbl.replace slot_loc s (nphys + i)) slots;
  let sloc b s = match Hashtbl.find_opt slot_loc s with Some l -> l | None -> failb b "slot %d is not the allocation's" s in
  let var_of = function V n -> n | r -> nv + pidx r in
  let vcls v = Dynarray.get f.vcls v in
  let succs b = List.sort_uniq compare (successors ra.terms.(b)) in
  (* reachability and predecessors in the allocated CFG *)
  let reach = Array.make nb false in
  let po = Dynarray.create () in
  (let st = Stack.create () in
   reach.(0) <- true;
   Stack.push (0, ref (succs 0)) st;
   while not (Stack.is_empty st) do
     let b, rest = Stack.top st in
     match !rest with
     | s :: tl ->
         rest := tl;
         if s < 0 || s >= nb then failb b "jumps to block %d" s;
         if not reach.(s) then begin
           reach.(s) <- true;
           Stack.push (s, ref (succs s)) st
         end
     | [] ->
         ignore (Stack.pop st);
         Dynarray.add_last po b
   done);
  let rpo = Array.of_list (List.rev (Dynarray.to_list po)) in
  let preds = Array.make nb [] in
  Array.iter (fun b -> List.iter (fun s -> preds.(s) <- b :: preds.(s)) (succs b)) rpo;
  (* 1. structure *)
  let edge_ok t s = s = t || (s >= nb0 && ra.terms.(s) = Jmp t && List.for_all is_edit ra.items.(s)) in
  Array.iter
    (fun b ->
      if b < nb0 then begin
        let mb = Dynarray.get f.blocks b in
        let orig =
          List.filter_map
            (function
              | Mir_ra.Orig (o, _) -> Some o
              | Elided (Copy _ as o) -> Some o
              | Elided o -> failb b "%s elided (only copies may be)" (ins_s o)
              | Reload _ | Spill _ -> failb b "a Mir_ra reload or spill"
              | Move _ | Ld _ | St _ | Remat _ -> None)
            ra.items.(b)
        in
        if orig <> mb.code then failb b "the instructions differ from the Mir's";
        let ok =
          match (mb.term, ra.terms.(b)) with
          | Jmp t, Jmp s -> edge_ok t s
          | Jcc (c, t, e), Jcc (c', t', e') -> c = c' && edge_ok t t' && edge_ok e e'
          | Asmgoto (x, l), Asmgoto (x', l') -> l = l' && edge_ok x x'
          | o, n -> o = n
        in
        if not ok then failb b "the terminator differs from the Mir's"
      end
      else begin
        if not (List.for_all is_edit ra.items.(b)) then failb b "an edge block with instructions";
        match ra.terms.(b) with Jmp t when t < nb0 -> () | _ -> failb b "an edge block that is not a jump"
      end)
    rpo;
  (* 2. liveness of the variables *)
  let defb = Array.make nvar [] and ueb = Array.make nvar [] in
  let item_regs = function
    | Mir_ra.Orig (o, _) ->
        let u, d = uses_defs o in
        (u, d @ clobbers o)
    | Elided (Copy (d, s)) -> ([ s ], [ d ])
    | _ -> ([], [])
  in
  Array.iter
    (fun b ->
      let defined = Hashtbl.create 16 in
      let use r =
        let x = var_of r in
        if (not (Hashtbl.mem defined x)) && match ueb.(x) with b' :: _ -> b' <> b | [] -> true then ueb.(x) <- b :: ueb.(x)
      in
      let def r =
        let x = var_of r in
        Hashtbl.replace defined x ();
        match defb.(x) with b' :: _ when b' = b -> () | _ -> defb.(x) <- b :: defb.(x)
      in
      List.iter
        (fun it ->
          let u, d = item_regs it in
          List.iter use u;
          List.iter def d)
        ra.items.(b);
      List.iter use (term_uses ra.terms.(b)))
    rpo;
  let livein = Array.make nb [] in
  let defmark = Array.make nb (-1) and inmark = Array.make nb (-1) in
  for x = 0 to nvar - 1 do
    List.iter (fun b -> defmark.(b) <- x) defb.(x);
    let work = ref [] in
    List.iter
      (fun b ->
        if inmark.(b) <> x then begin
          inmark.(b) <- x;
          livein.(b) <- x :: livein.(b);
          work := b :: !work
        end)
      ueb.(x);
    while !work <> [] do
      let b = List.hd !work in
      work := List.tl !work;
      List.iter
        (fun p ->
          if defmark.(p) <> x && inmark.(p) <> x then begin
            inmark.(p) <- x;
            livein.(p) <- x :: livein.(p);
            work := p :: !work
          end)
        preds.(b)
    done
  done;
  let livein = Array.map (fun l -> Array.of_list (List.rev l)) livein in
  (* value ids: 0..31 the incoming registers, then definitions and merges,
     numbered on first sight (the same on every pass) *)
  let next = ref nphys in
  let ids = Hashtbl.create 256 in
  let id key =
    match Hashtbl.find_opt ids key with
    | Some i -> i
    | None ->
        let i = !next in
        incr next;
        Hashtbl.add ids key i;
        i
  in
  let def_id b k j = id (`Def (b, k, j)) and merge_id b x = id (`Merge (b, x)) in
  (* an xmm register stored to an 8-byte slot keeps only the low half of
     its value: [low i] (negative) stands for that, good for every reader
     but those of all 16 bytes *)
  let low i = -1 - i in
  (* each reachable block's items by Mir code index (for Remat) *)
  let item_of_code = Array.make nb0 [||] in
  Array.iter
    (fun b ->
      if b < nb0 then begin
        let m = Array.make (List.length (Dynarray.get f.blocks b).code) (-1) in
        let c = ref 0 in
        List.iteri
          (fun k it ->
            match it with
            | Mir_ra.Orig _ | Elided _ ->
                m.(!c) <- k;
                incr c
            | _ -> ())
          ra.items.(b);
        item_of_code.(b) <- m
      end)
    rpo;
  (* the virtual machine: each variable's value along a block *)
  let vstep b (virt : (int, int) Hashtbl.t) on_item =
    List.iteri
      (fun k it ->
        on_item k it virt;
        match it with
        | Mir_ra.Orig (o, _) ->
            let _, d = uses_defs o in
            List.iteri (fun j r -> Hashtbl.replace virt (var_of r) (def_id b k j)) d;
            List.iter (fun r -> Hashtbl.remove virt (var_of r)) (clobbers o)
        | Elided (Copy (d, s)) -> (
            match Hashtbl.find_opt virt (var_of s) with
            | Some i -> Hashtbl.replace virt (var_of d) i
            | None -> Hashtbl.remove virt (var_of d))
        | _ -> ())
      ra.items.(b)
  in
  let entry_value x = if x >= nv then Some (x - nv) else None in
  let ventry = Array.make nb [||] and vexit = Array.make nb (Hashtbl.create 1) in
  Array.iter
    (fun b ->
      let single = if b = 0 then preds.(0) = [] else List.length preds.(b) = 1 in
      ventry.(b) <-
        Array.map
          (fun x ->
            ( x,
              if not single then Some (merge_id b x)
              else if b = 0 then entry_value x
              else Hashtbl.find_opt vexit.(List.hd preds.(b)) x ))
          livein.(b);
      let virt = Hashtbl.create 64 in
      Array.iter (fun (x, v) -> Option.iter (Hashtbl.replace virt x) v) ventry.(b);
      vstep b virt (fun _ _ _ -> ());
      vexit.(b) <- virt)
    rpo;
  (* 3. the actual machine *)
  let init = Array.init nloc (fun l -> if l < nphys then IS.singleton l else IS.empty) in
  (* the state at the start of t, from a state at the end of p (or of the
     function's entry) *)
  let relabel (st : IS.t array) exitv t =
    let at = Hashtbl.create 64 in
    Array.iteri (fun l s -> IS.iter (fun i -> Hashtbl.add at i l) s) st;
    let out = Array.make nloc IS.empty in
    let add l i = out.(l) <- IS.add i out.(l) in
    Array.iter
      (fun (x, vt) ->
        match vt with
        | None -> ()
        | Some it -> (
            match exitv x with
            | None -> Array.iteri (fun l s -> out.(l) <- IS.add it s) out
            | Some ip ->
                List.iter (fun l -> add l it) (Hashtbl.find_all at ip);
                List.iter (fun l -> add l (low it)) (Hashtbl.find_all at (low ip))))
      ventry.(t);
    out
  in
  let written = Hashtbl.create 8 in
  let reserved = ref IS.empty and rtwice_calls = ref [] in
  let st_items = ref [] in
  let has_rtwice =
    Array.exists (List.exists (function Mir_ra.Orig (Call { rtwice = true; _ }, _) -> true | _ -> false)) ra.items
  in
  let loc_of b r =
    match r with
    | G (X64.Rsp | X64.Rbp) -> failb b "rsp or rbp allocated"
    | V _ -> failb b "a virtual register after allocation"
    | r -> pidx r
  in
  let step b (entry : IS.t array) =
    let st = Array.copy entry in
    let virt = Hashtbl.create 64 in
    Array.iter (fun (x, v) -> Option.iter (Hashtbl.replace virt x) v) ventry.(b);
    (* where each id may be (a superset: entries go stale, not missing),
       so a definition need not scan every location *)
    let holders = Hashtbl.create 64 in
    let hold l s = IS.iter (fun i -> Hashtbl.add holders i l) s in
    Array.iteri hold st;
    let write l s =
      if l < 16 then Hashtbl.replace written l ();
      hold l s;
      st.(l) <- s
    in
    let def l i =
      List.iter
        (fun j ->
          List.iter (fun m -> st.(m) <- IS.remove j st.(m)) (Hashtbl.find_all holders j);
          while Hashtbl.mem holders j do Hashtbl.remove holders j done)
        [ i; low i ];
      write l (IS.singleton i)
    in
    (* [full]: the reader uses all 16 bytes of an xmm register *)
    let reads ?(full = false) ins r r' =
      let l = loc_of b r' in
      match Hashtbl.find_opt virt (var_of r) with
      | Some i when not (IS.mem i st.(l) || ((not full) && IS.mem (low i) st.(l))) ->
          failb b "%s: %s does not hold the value of %s" (ins_s ins) (reg_s r') (reg_s r)
      | _ -> ()
    in
    vstep b virt (fun k it virt ->
        ignore virt;
        match it with
        | Mir_ra.Orig (o, n) ->
            let us, ds = uses_defs o and us', ds' = uses_defs n in
            if List.length us <> List.length us' || List.length ds <> List.length ds' then
              failb b "%s: allocated as %s" (ins_s o) (ins_s n);
            let umap = Hashtbl.create 4 and dmap = Hashtbl.create 4 in
            let bind tbl r r' =
              match r with
              | V v -> (
                  ignore (loc_of b r');
                  if class_of_phys r' <> vcls v then failb b "%s: %s in the wrong class" (ins_s o) (reg_s r);
                  match Hashtbl.find_opt tbl v with
                  | Some y when y <> r' -> failb b "%s: %s gets two registers" (ins_s o) (reg_s r)
                  | _ -> Hashtbl.replace tbl v r')
              | p -> if p <> r' then failb b "%s: physical operand %s changed" (ins_s o) (reg_s p)
            in
            List.iter2 (bind umap) us us';
            List.iter2 (bind dmap) ds ds';
            let look tbl = function V v -> Hashtbl.find tbl v | p -> p in
            let differs () = failb b "%s: allocated form differs: %s" (ins_s o) (ins_s n) in
            (match map_regs_ud ~u:(look umap) ~d:(look dmap) o with
            | Some n' when n' = n -> ()
            | _ -> differs ()
            | exception Not_found -> differs ());
            if clobbers o <> clobbers n then failb b "%s: clobbers changed" (ins_s o);
            let full = match o with
              | Xstore (_, s) -> [ s ]
              | Asm a -> Array.to_list (Array.map (function Areg ((X _ as r), w) when w > 64 -> [ r ] | _ -> []) a.opnds) |> List.concat
              | _ -> []
            in
            List.iter2 (fun r r' -> reads ~full:(List.mem r full) o r r') us us';
            (match o with
            | Call { rtwice = true; _ } ->
                for l = nphys to nloc - 1 do
                  if not (IS.is_empty st.(l)) then reserved := IS.add l !reserved
                done;
                if not (List.mem (b, k) !rtwice_calls) then rtwice_calls := (b, k) :: !rtwice_calls
            | _ -> ());
            List.iteri
              (fun j r' ->
                let i = def_id b k j in
                def (loc_of b r') i)
              ds';
            List.iter (fun r -> write (loc_of b r) IS.empty) (clobbers n)
        | Elided _ -> ()
        | Move (d, s) ->
            if class_of_phys d <> class_of_phys s then failb b "a move between classes";
            write (loc_of b d) st.(loc_of b s)
        | Ld (s, r) -> write (loc_of b r) st.(sloc b s)
        | St (s, r) ->
            let l = sloc b s in
            let v = st.(loc_of b r) in
            if has_rtwice && not (List.mem (b, k) !st_items) then st_items := (b, k) :: !st_items;
            (* an xmm into an 8-byte slot keeps only its low half; a
               16-byte slot (movdqu) keeps all of it *)
            let wide = fst (Dynarray.get f.slots s) >= 16 in
            write l (match r with X _ when not wide -> IS.map (fun i -> if i >= 0 then low i else i) v | _ -> v)
        | Remat ((ob, ok), i) ->
            let o =
              if ob < 0 || ob >= nb0 || not reach.(ob) then failb b "a remat of block %d" ob
              else match List.nth_opt (Dynarray.get f.blocks ob).code ok with Some o -> o | None -> failb b "a remat of %d.%d" ob ok
            in
            if not (remat_ok o) then failb b "remat of %s, which is not a constant" (ins_s o);
            let r = match uses_defs i with [], [ r ] -> r | _ -> failb b "remat: %s" (ins_s i) in
            let cls = match uses_defs o with _, [ (V v) ] -> vcls v | _, [ p ] -> class_of_phys p | _ -> failb b "remat" in
            if class_of_phys r <> cls || with_def o r <> Some i then failb b "remat of %s as %s" (ins_s o) (ins_s i);
            let k' = item_of_code.(ob).(ok) in
            write (loc_of b r) (IS.singleton (def_id ob k' 0))
        | Reload _ | Spill _ -> failb b "a Mir_ra reload or spill");
    List.iter
      (fun r ->
        match Hashtbl.find_opt virt (var_of r) with
        | Some i when not (IS.mem i st.(loc_of b r) || IS.mem (low i) st.(loc_of b r)) -> failb b "the terminator reads %s, which does not hold its value" (reg_s r)
        | _ -> ())
      (term_uses ra.terms.(b));
    (st, virt)
  in
  let entry_state = Array.make nb None in
  entry_state.(0) <- Some (relabel init entry_value 0);
  let changed = ref true in
  while !changed do
    changed := false;
    Array.iter
      (fun b ->
        match entry_state.(b) with
        | None -> ()
        | Some s ->
            let out, virt = step b s in
            List.iter
              (fun t ->
                let ns = relabel out (Hashtbl.find_opt virt) t in
                let ns = match entry_state.(t) with None -> ns | Some old -> Array.map2 IS.inter old ns in
                if match entry_state.(t) with None -> true | Some old -> not (Array.for_all2 IS.equal old ns) then begin
                  entry_state.(t) <- Some ns;
                  changed := true
                end)
              (succs b))
      rpo
  done;
  (* 4. *)
  Hashtbl.iter
    (fun l () ->
      let r = List.find (fun r -> X64.reg_num r = l) X64.[ Rax; Rcx; Rdx; Rbx; Rsp; Rbp; Rsi; Rdi; R8; R9; R10; R11; R12; R13; R14; R15 ] in
      if List.mem r callee_saved && not (List.mem r ra.saved) then fail "writes %s without saving it" (X64.reg_name r))
    written;
  (* 5. what each returns_twice call reaches *)
  if !rtwice_calls <> [] && not (IS.is_empty !reserved) then begin
    let seen = Array.make nb false in
    let rec visit b =
      if not seen.(b) then begin
        seen.(b) <- true;
        List.iter visit (succs b)
      end
    in
    List.iter (fun (b, _) -> List.iter visit (succs b)) !rtwice_calls;
    List.iter
      (fun (b, k) ->
        let after_call = List.exists (fun (cb, ck) -> cb = b && ck < k) !rtwice_calls in
        if seen.(b) || after_call then
          match List.nth ra.items.(b) k with
          | Mir_ra.St (s, _) when IS.mem (sloc b s) !reserved ->
              failb b "a store into slot s%d, which holds a value at a returns_twice call that reaches it"
                (sloc b s - nphys)
          | _ -> ())
      !st_items
  end
