(* Mir_import: FAS_RA_IMPORT, an allocation computed outside the compiler
   (the register allocator lab, optimizer-lab/regalloc/FORMAT.md) applied
   to a function in place of Mir_ra.

   The allocation answers the problem Mir_export printed for the same
   function, so the importer runs the exporter again to recover how its
   printed instructions and values map onto the Mir (the hash in the file
   must match, or the function changed since the export). It then rebuilds
   each block:

   - an exported instruction becomes [Orig] with every register the
     allocation gave its operands; a register read and a register written
     may differ (SSA), see [Mir.map_regs_ud];
   - a Mir register copy becomes [Elided]: the exporter propagated it, the
     allocation's edits move the value instead;
   - the edits become [Move], [Ld], [St] (frame slots the allocation
     numbers s0..sN-1, new 8-byte slots of the function, 16-byte for those
     its [wide] lines name) and [Remat];
   - the edges the exporter split become new blocks after the Mir's
     (holding their edits and a jump), the unreachable blocks [Halt].

   Nothing here is trusted: Mir_icheck verifies the result independently of
   the exporter and of this module. An allocation the importer cannot
   apply is an error ([Failure]), never a silent fallback. *)

open Mir

exception Failure of string

let fail fmt = Printf.ksprintf (fun s -> raise (Failure s)) fmt

type loc = Preg of reg | Slot of int

type edit = Emove of loc * loc  (** from, to *) | Eremat of int * reg  (** printed value, register *)

type alloc = {
  name : string;
  insts : int;
  hash : int64;
  nslots : int;
  wide : (int, unit) Hashtbl.t;  (** slots of 16 bytes (for SIMD vectors), from [wide] lines *)
  allocs : (int, loc array) Hashtbl.t;  (** printed instruction -> each operand's location *)
  edits : (int * bool, edit list) Hashtbl.t;  (** (instruction, after?) -> edits in file order *)
}

let gprs = X64.[ Rax; Rcx; Rdx; Rbx; Rsp; Rbp; Rsi; Rdi; R8; R9; R10; R11; R12; R13; R14; R15 ]

let preg_of_name s =
  match List.find_opt (fun r -> Mir_export.gpr_names.(X64.reg_num r) = s) gprs with
  | Some r -> Some (G r)
  | None ->
      if String.length s > 3 && String.sub s 0 3 = "xmm" then
        match int_of_string_opt (String.sub s 3 (String.length s - 3)) with
        | Some i when i >= 0 && i < 16 -> Some (X i)
        | _ -> None
      else None

(* ---------- the file ---------- *)

let parse_file path =
  let ic = try open_in_bin path with Sys_error e -> fail "FAS_RA_IMPORT: %s" e in
  let funcs = Hashtbl.create 64 in
  let lineno = ref 0 in
  let bad fmt = Printf.ksprintf (fun s -> fail "FAS_RA_IMPORT %s:%d: %s" path !lineno s) fmt in
  let int s = match int_of_string_opt s with Some n when n >= 0 -> n | _ -> bad "expected a number, got %S" s in
  let cur = ref None in
  let loc nslots s =
    if String.length s > 1 && s.[0] = 's' then
      match int_of_string_opt (String.sub s 1 (String.length s - 1)) with
      | Some n when n >= 0 && n < nslots -> Slot n
      | _ -> bad "bad slot %S (the function has %d)" s nslots
    else
      match preg_of_name s with
      | Some (G (X64.Rsp | X64.Rbp)) -> bad "%s is reserved" s
      | Some r -> Preg r
      | None -> bad "unknown register %S" s
  in
  let words l =
    let l = match String.index_opt l '#' with Some i -> String.sub l 0 i | None -> l in
    List.filter (( <> ) "") (String.split_on_char ' ' (String.map (function '\t' | '\r' -> ' ' | c -> c) l))
  in
  (try
     let first = ref true in
     while true do
       let l = input_line ic in
       incr lineno;
       match words l with
       | [] -> ()
       | w when !first ->
           first := false;
           if w <> [ "fasra-alloc"; "1" ] then bad "not a fasra-alloc 1 file"
       | [ "func"; name; "insts"; k; "hash"; h; "slots"; s ] ->
           if !cur <> None then bad "func inside a func";
           let hash = match Int64.of_string_opt ("0x" ^ h) with Some x -> x | None -> bad "bad hash %S" h in
           cur :=
             Some
               { name; insts = int k; hash; nslots = int s; wide = Hashtbl.create 4; allocs = Hashtbl.create 64; edits = Hashtbl.create 64 }
       | [ "end" ] -> (
           match !cur with
           | Some a ->
               Hashtbl.add funcs a.name a;
               cur := None
           | None -> bad "end outside a func")
       | ("edit" | "remat" | "wide") :: _ when !cur = None -> bad "edit outside a func"
       | "wide" :: ss ->
           let a = Option.get !cur in
           List.iter
             (fun s -> match loc a.nslots s with Slot n -> Hashtbl.replace a.wide n () | Preg _ -> bad "wide %s: not a slot" s)
             ss
       | [ "edit"; i; w; from; to_ ] ->
           let a = Option.get !cur in
           let after = match w with "before" -> false | "after" -> true | _ -> bad "before or after, not %S" w in
           let e = Emove (loc a.nslots from, loc a.nslots to_) in
           (match e with Emove (Slot _, Slot _) -> bad "a slot-to-slot move" | _ -> ());
           let key = (int i, after) in
           Hashtbl.replace a.edits key ((try Hashtbl.find a.edits key with Not_found -> []) @ [ e ])
       | [ "remat"; i; w; v; to_ ] ->
           let a = Option.get !cur in
           let after = match w with "before" -> false | "after" -> true | _ -> bad "before or after, not %S" w in
           let v =
             if String.length v > 1 && v.[0] = '%' then int (String.sub v 1 (String.length v - 1))
             else bad "expected a value, got %S" v
           in
           let r = match loc a.nslots to_ with Preg r -> r | Slot _ -> bad "remat into a slot" in
           let key = (int i, after) in
           Hashtbl.replace a.edits key ((try Hashtbl.find a.edits key with Not_found -> []) @ [ Eremat (v, r) ])
       | i :: locs when int_of_string_opt i <> None -> (
           match !cur with
           | Some a ->
               let i = int i in
               if Hashtbl.mem a.allocs i then bad "instruction %d allocated twice" i;
               Hashtbl.add a.allocs i (Array.of_list (List.map (loc a.nslots) locs))
           | None -> bad "allocation outside a func")
       | w :: _ -> bad "unexpected %S" w
     done
   with End_of_file -> ());
  close_in ic;
  if !cur <> None then fail "FAS_RA_IMPORT %s: the last func has no end" path;
  funcs

let file =
  lazy
    (match Sys.getenv_opt "FAS_RA_IMPORT" with
    | None | Some "" -> None
    | Some p -> Some (parse_file p))

(* the allocations the file has for a function of this name (several when
   modules share a name; the hash picks one) *)
let find (f : func) = match Lazy.force file with None -> [] | Some t -> Hashtbl.find_all t f.name

(* ---------- applying one ---------- *)

let run (f : func) (cands : alloc list) =
  let p =
    try Mir_export.problem f
    with Mir_export.Skip why -> fail "%s: FAS_RA_IMPORT: the exporter cannot express it (%s)" f.name why
  in
  let a =
    match List.find_opt (fun a -> a.hash = p.hash && a.insts = p.ninsts) cands with
    | Some a -> a
    | None -> fail "%s: FAS_RA_IMPORT: no allocation for this problem (hash %016Lx, %d instructions)" f.name p.hash p.ninsts
  in
  let fail fmt = Printf.ksprintf (fun s -> fail "%s: FAS_RA_IMPORT: %s" f.name s) fmt in
  let nb = Array.length p.reach and nb0 = p.nb0 in
  let slots =
    Array.init a.nslots (fun n ->
        if Hashtbl.mem a.wide n then new_slot f ~size:16 ~align:16 else new_slot f ~size:8 ~align:8)
  in
  (* printed instruction -> what it is *)
  let what = Array.make p.ninsts `None in
  Array.iteri
    (fun b es ->
      List.iter (fun (e : Mir_export.entry) -> what.(e.ix) <- `Ins (b, e)) es;
      if p.term_ix.(b) >= 0 then what.(p.term_ix.(b)) <- `Term b)
    p.entries;
  (* printed value -> its defining instruction (for remat) *)
  let defs = Hashtbl.create 64 in
  Array.iteri
    (fun b es ->
      List.iter
        (fun (e : Mir_export.entry) ->
          Array.iteri (fun k (o : Mir_export.opnd) -> if not o.use then Hashtbl.replace defs e.nums.(k) (b, e)) e.x.opnds)
        es)
    p.entries;
  Hashtbl.iter
    (fun i _ -> if i >= p.ninsts || what.(i) = `None then fail "an allocation for instruction %d, which does not exist" i)
    a.allocs;
  Hashtbl.iter
    (fun (i, after) _ ->
      if i >= p.ninsts then fail "an edit at instruction %d, which does not exist" i;
      match what.(i) with `Term _ when after -> fail "an edit after the terminator %d" i | _ -> ())
    a.edits;
  (* the Mir code of each block without its Locs, and each instruction's
     index with them *)
  let raw = Array.init nb0 (fun b -> Array.of_list (Dynarray.get f.blocks b).code) in
  let noloc_raw =
    Array.map
      (fun code ->
        let l = ref [] in
        Array.iteri (fun k i -> match i with Loc _ -> () | _ -> l := k :: !l) code;
        Array.of_list (List.rev !l))
      raw
  in
  let edit_items i after =
    List.map
      (function
        | Emove (Preg s, Preg d) ->
            if class_of_phys s <> class_of_phys d then fail "a move between classes at %d" i;
            Mir_ra.Move (d, s)
        | Emove (Slot s, Preg d) -> Ld (slots.(s), d)
        | Emove (Preg s, Slot d) -> St (slots.(d), s)
        | Emove (Slot _, Slot _) -> fail "a slot-to-slot move at %d" i
        | Eremat (v, r) -> (
            match Hashtbl.find_opt defs v with
            | Some (b, e) when e.x.src >= 0 && List.exists (fun s -> String.starts_with ~prefix:"remat=" s) e.x.attrs -> (
                let k = noloc_raw.(b).(e.x.src) in
                match with_def raw.(b).(k) r with
                | Some ins -> Remat ((b, k), ins)
                | None -> fail "remat of %%%d: its definition has no separate destination" v)
            | _ -> fail "remat of %%%d, which is not defined by a remat instruction" v))
      (try Hashtbl.find a.edits (i, after) with Not_found -> [])
  in
  (* one exported instruction, allocated *)
  let alloc_ins (e : Mir_export.entry) (o : ins) =
    let n = Array.length e.x.opnds in
    let locs =
      match Hashtbl.find_opt a.allocs e.ix with
      | Some l -> l
      | None -> if n = 0 then [||] else fail "instruction %d has no allocation" e.ix
    in
    if Array.length locs <> n then fail "instruction %d: %d locations for %d operands" e.ix (Array.length locs) n;
    let regs =
      Array.mapi
        (fun k l ->
          match l with
          | Preg r -> r
          | Slot _ -> fail "instruction %d: operand %d in a slot (memory operands are not supported in v1)" e.ix k)
        locs
    in
    let us, ds = uses_defs o in
    if List.length us + List.length ds <> n then fail "instruction %d: operand count differs from the Mir" e.ix;
    let umap = Hashtbl.create 4 and dmap = Hashtbl.create 4 in
    let bind tbl k r =
      let y = regs.(k) in
      match r with
      | V v -> (
          match Hashtbl.find_opt tbl v with
          | Some y' when y' <> y -> fail "instruction %d: one register read twice, from %s and %s" e.ix (reg_s y') (reg_s y)
          | _ -> Hashtbl.replace tbl v y)
      | p -> if p <> y then fail "instruction %d: operand %d is fixed to %s, allocated %s" e.ix k (reg_s p) (reg_s y)
    in
    List.iteri (fun k r -> bind umap k r) us;
    List.iteri (fun k r -> bind dmap (List.length us + k) r) ds;
    let look tbl = function V v -> Hashtbl.find tbl v | p -> p in
    match map_regs_ud ~u:(look umap) ~d:(look dmap) o with
    | Some n -> n
    | None -> fail "instruction %d: a register both read and written gets two locations" e.ix
    | exception Not_found -> fail "instruction %d: an operand without a location" e.ix
  in
  let items = Array.make nb [] in
  for b = 0 to nb - 1 do
    if b < nb0 && p.reach.(b) then begin
      let out = ref [] in
      let add l = out := List.rev_append l !out in
      let code = raw.(b) in
      let pos = ref 0 in
      (* the Mir instructions before the [s]th non-Loc one: Locs, and copies *)
      let flush_to s =
        let stop = if s < Array.length noloc_raw.(b) then noloc_raw.(b).(s) else Array.length code in
        while !pos < stop do
          (match code.(!pos) with
          | Loc _ as i -> add [ Mir_ra.Orig (i, i) ]
          | Copy _ as i -> add [ Elided i ]
          | i -> fail "block %d: %s was not exported" b (ins_s i));
          incr pos
        done
      in
      List.iter
        (fun (e : Mir_export.entry) ->
          if e.x.src < 0 then begin
            (* a pseudo (args, undef): no code, its edits where it stands *)
            let n = Array.length e.x.opnds in
            (match Hashtbl.find_opt a.allocs e.ix with
            | Some l when Array.length l <> n -> fail "instruction %d: %d locations for %d operands" e.ix (Array.length l) n
            | Some l ->
                Array.iteri
                  (fun k (o : Mir_export.opnd) ->
                    match (o.con, l.(k)) with
                    | Cfixed q, Preg r when Mir_export.pidx r <> q -> fail "instruction %d: operand %d is fixed" e.ix k
                    | _, Slot _ -> fail "instruction %d: a definition in a slot" e.ix
                    | _ -> ())
                  e.x.opnds
            | None -> if n > 0 then fail "instruction %d has no allocation" e.ix);
            add (edit_items e.ix false);
            add (edit_items e.ix true)
          end
          else begin
            flush_to e.x.src;
            let o = code.(!pos) in
            incr pos;
            add (edit_items e.ix false);
            add [ Orig (o, alloc_ins e o) ];
            add (edit_items e.ix true)
          end)
        p.entries.(b);
      flush_to max_int;
      add (edit_items p.term_ix.(b) false);
      items.(b) <- List.rev !out
    end
    else if b >= nb0 && p.reach.(b) && p.term_ix.(b) >= 0 then items.(b) <- edit_items p.term_ix.(b) false
  done;
  (* the terminators read physical registers only; an allocation printed
     for them must say so *)
  Array.iteri
    (fun b ix ->
      if ix >= 0 then
        match Hashtbl.find_opt a.allocs ix with
        | None -> ()
        | Some l ->
            let rs = term_uses p.terms.(b) in
            if Array.length l <> List.length rs || not (List.for_all2 (fun r l -> l = Preg r) rs (Array.to_list l)) then
              fail "terminator %d: its operands are fixed" ix)
    p.term_ix;
  let terms = Array.mapi (fun b t -> if p.reach.(b) then t else Halt) p.terms in
  (* the callee-saved registers the code mentions *)
  let mentioned = Hashtbl.create 8 in
  let note r = Hashtbl.replace mentioned r () in
  Array.iter
    (List.iter (function
      | Mir_ra.Orig (_, i) | Remat (_, i) ->
          List.iter note (all_regs i);
          List.iter note (clobbers i)
      | Move (d, s) -> note d; note s
      | Ld (_, r) | St (_, r) -> note r
      | _ -> ()))
    items;
  let saved = List.filter (fun r -> Hashtbl.mem mentioned (G r)) callee_saved in
  let nv = Dynarray.length f.vcls in
  ( {
      Mir_ra.items;
      terms;
      vslot = Array.make nv (-1);
      saved;
      assign = Array.make nv None;
      lo = Array.make nv (-1);
      hi = Array.make nv (-1);
    },
    slots )
