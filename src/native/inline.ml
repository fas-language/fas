(* Inlining for the native backend. Callees are functions marked `@inline`
   (Ir.module_.inline_functions) and small functions that call nothing.
   A call is replaced by a renumbered copy of the callee: the calling block
   ends with a branch to the copy's entry, every `ret` becomes a branch to a
   new continuation block that holds the rest of the calling block, and the
   result is the returned value (a phi when there are several returns). The
   copy's allocas move to the caller's entry block. Arguments replace
   parameters directly, which is correct because the IR given to this pass
   is in SSA form after promotion: parameters are never stored to. *)

open Ir
module Hashtbl = Itbl
module Ptbl = Stdlib.Hashtbl

let def = function
  | Bin (i, _, _, _, _)
  | Fneg (i, _, _)
  | Cmp (i, _, _, _, _)
  | Alloca (i, _, _)
  | Load (i, _, _, _)
  | Load_volatile (i, _, _, _)
  | Gep (i, _, _, _)
  | Cast (i, _, _, _, _)
  | Phi (i, _, _)
  | Select (i, _, _, _)
  | Extract (i, _, _, _)
  | Insert (i, _, _, _, _)
  | Shuffle_zero (i, _, _)
  | Shufflevector (i, _, _, _, _)
  | String_ptr (i, _, _)
  | Global_ptr (i, _, _)
  | Syscall (i, _) ->
      Some i
  | Call (d, _, _, _, _) | Call_indirect (d, _, _, _, _) -> d
  | Store _ | Store_volatile _ | Trap | Location _ -> None

let shift_def k = function
  | Bin (i, op, t, a, b) -> Bin (i + k, op, t, a, b)
  | Fneg (i, t, a) -> Fneg (i + k, t, a)
  | Cmp (i, c, t, a, b) -> Cmp (i + k, c, t, a, b)
  | Alloca (i, t, al) -> Alloca (i + k, t, al)
  | Load (i, t, p, al) -> Load (i + k, t, p, al)
  | Load_volatile (i, t, p, al) -> Load_volatile (i + k, t, p, al)
  | Gep (i, t, p, idx) -> Gep (i + k, t, p, idx)
  | Cast (i, s, ft, v, tt) -> Cast (i + k, s, ft, v, tt)
  | Phi (i, t, ins) -> Phi (i + k, t, ins)
  | Select (i, c, a, b) -> Select (i + k, c, a, b)
  | Extract (i, t, v, l) -> Extract (i + k, t, v, l)
  | Insert (i, t, v, l, x) -> Insert (i + k, t, v, l, x)
  | Shuffle_zero (i, t, v) -> Shuffle_zero (i + k, t, v)
  | Shufflevector (i, t, a, b, m) -> Shufflevector (i + k, t, a, b, m)
  | String_ptr (i, a, b) -> String_ptr (i + k, a, b)
  | Global_ptr (i, n, t) -> Global_ptr (i + k, n, t)
  | Syscall (i, args) -> Syscall (i + k, args)
  | Call (d, e, t, n, args) -> Call (Option.map (( + ) k) d, e, t, n, args)
  | Call_indirect (d, e, t, c, args) ->
      Call_indirect (Option.map (( + ) k) d, e, t, c, args)
  | (Store _ | Store_volatile _ | Trap | Location _) as x -> x

let shift_phi_blocks kb = function
  | Phi (i, t, ins) -> Phi (i, t, List.map (fun (v, b) -> (v, b + kb)) ins)
  | x -> x

let targets = function
  | Br b -> [ b ]
  | CondBr (_, a, b) -> [ a; b ]
  | Switch (_, _, cases, d) -> d :: List.map snd cases
  | Ret _ | Unreachable -> []

let shift_term kb = function
  | Br b -> Br (b + kb)
  | CondBr (c, a, b) -> CondBr (c, a + kb, b + kb)
  | Switch (t, v, cases, d) ->
      Switch (t, v, List.map (fun (c, b) -> (c, b + kb)) cases, d + kb)
  | (Ret _ | Unreachable) as x -> x

let max_ids (f : func) =
  List.fold_left
    (fun (mv, mb) (b : block) ->
      ( List.fold_left
          (fun mv ins -> match def ins with Some i -> max mv i | None -> mv)
          mv b.instrs,
        max mb b.id ))
    (0, 0) f.blocks

let size (f : func) =
  List.fold_left
    (fun n (b : block) ->
      n
      + List.length (List.filter (function Location _ -> false | _ -> true) b.instrs))
    0 f.blocks

let is_aggregate = function Struct _ | Array _ -> true | _ -> false

let calls_self (f : func) =
  List.exists
    (fun (b : block) ->
      List.exists (function Call (_, _, _, n, _) -> n = f.name | _ -> false) b.instrs)
    f.blocks

(* Functions whose calls this pass replaces: those marked `@inline`, and
   small internal ones. A function that calls only other candidates counts as
   small when its size plus theirs is, so leaf helpers inline through their
   small callers. [sizes] holds sizes measured before vector scalarization,
   so a short vector function is judged by its vector instructions rather
   than by its lanes. *)
let candidates ?(only = fun _ -> true) sizes (m : module_) =
  let small = 32 in
  let tbl = Ptbl.create 16 in
  let eligible (f : func) =
    f.blocks <> [] && only f.name && (not f.variadic)
    && (not (calls_self f))
    && f.name <> "main"
    && (not (List.mem f.name m.no_inline_functions))
    && (not (is_aggregate f.ret))
    && not (List.exists (fun (p : param) -> is_aggregate p.ty) f.params)
  in
  let own_size (f : func) =
    match Ptbl.find_opt sizes f.name with Some n -> n | None -> size f
  in
  (* total size with the candidate callees, None if it calls anything else *)
  let total (f : func) =
    List.fold_left
      (fun acc (b : block) ->
        List.fold_left
          (fun acc ins ->
            match (acc, ins) with
            | None, _ -> None
            | Some n, Call (_, _, _, name, _)
              when String.starts_with ~prefix:"llvm." name ->
                Some n
            | Some n, Call (_, _, _, name, _) -> (
                match Ptbl.find_opt tbl name with
                | Some (_, k) -> Some (n + k)
                | None -> None)
            | Some _, Call_indirect _ -> None
            | acc, _ -> acc)
          acc b.instrs)
      (Some (own_size f))
      f.blocks
  in
  List.iter
    (fun (f : func) ->
      if eligible f && List.mem f.name m.inline_functions then
        Ptbl.replace tbl f.name (f, own_size f))
    m.funcs;
  let rec rounds k =
    let added = ref false in
    List.iter
      (fun (f : func) ->
        if f.linkage = Internal && (not (Ptbl.mem tbl f.name)) && eligible f then
          match total f with
          | Some n when n <= small ->
              Ptbl.replace tbl f.name (f, n);
              added := true
          | _ -> ())
      m.funcs;
    if !added && k > 1 then rounds (k - 1)
  in
  rounds 4;
  let out = Ptbl.create (Ptbl.length tbl) in
  Ptbl.iter (fun name (f, _) -> Ptbl.replace out name f) tbl;
  out

(* Replace the call at position [pos] of block [bid] in [f] by [callee]. *)
let inline_at (f : func) bid pos (callee : func) =
  let mv, mb = max_ids f in
  let kv = mv + 1 and kb = mb + 1 in
  let cont = kb + (snd (max_ids callee) + 1) in
  let blk = List.find (fun (b : block) -> b.id = bid) f.blocks in
  let before = List.filteri (fun i _ -> i < pos) blk.instrs in
  let call = List.nth blk.instrs pos in
  let after = List.filteri (fun i _ -> i > pos) blk.instrs in
  let dst, ret_ty, args =
    match call with
    | Call (d, _, t, _, args) -> (d, t, args)
    | _ -> invalid_arg "inline_at"
  in
  let env = Ptbl.create 8 in
  List.iter2 (fun (p : param) (_, _, v) -> Ptbl.replace env p.name v) callee.params args;
  let value = function
    | Local (i, t) -> Local (i + kv, t)
    | Param (n, t) -> (
        match Ptbl.find_opt env n with Some v -> v | None -> Param (n, t))
    | v -> v
  in
  let allocas = ref [] in
  let returns = ref [] in
  let copied =
    List.map
      (fun (b : block) ->
        let id = b.id + kb in
        let instrs =
          List.filter_map
            (fun ins ->
              let ins =
                shift_phi_blocks kb (shift_def kv (Ir_fold.map_instr value ins))
              in
              match ins with
              | Alloca _ ->
                  allocas := ins :: !allocas;
                  None
              | ins -> Some ins)
            b.instrs
        in
        let terminator =
          match b.terminator with
          | Ret r ->
              (match r with
              | Some (_, v) -> returns := (value v, id) :: !returns
              | None -> ());
              Br cont
          | t -> shift_term kb (Ir_fold.map_term value t)
        in
        { id; label = callee.name ^ "." ^ b.label; instrs; terminator })
      callee.blocks
  in
  let entry = match copied with b :: _ -> b.id | [] -> invalid_arg "inline_at" in
  (* The call's result: the single returned value, or a phi *)
  let subst, phi =
    match (dst, !returns) with
    | None, _ -> (None, [])
    | Some d, [ (v, _) ] -> (Some (d, v), [])
    | Some d, [] -> (Some (d, Undef ret_ty), [])
    | Some d, rs -> (None, [ Phi (d, ret_ty, List.rev rs) ])
  in
  let continuation =
    {
      id = cont;
      label = blk.label ^ ".cont";
      instrs = phi @ after;
      terminator = blk.terminator;
    }
  in
  let succs = targets blk.terminator in
  let first = match f.blocks with b :: _ -> b.id | [] -> bid in
  (* The successors of [blk] are now reached from [cont] *)
  let from_cont instrs =
    List.map
      (function
        | Phi (i, t, ins) ->
            Phi (i, t, List.map (fun (v, p) -> (v, if p = bid then cont else p)) ins)
        | x -> x)
      instrs
  in
  let blocks =
    List.concat_map
      (fun (b : block) ->
        let instrs = if b.id = bid then before else b.instrs in
        let instrs = if List.mem b.id succs then from_cont instrs else instrs in
        let instrs = if b.id = first then List.rev !allocas @ instrs else instrs in
        if b.id = bid then
          ({ b with instrs; terminator = Br entry } :: copied) @ [ continuation ]
        else [ { b with instrs } ])
      f.blocks
  in
  let blocks =
    match subst with
    | None -> blocks
    | Some (d, v) ->
        let rec resolve = function Local (i, _) when i = d -> resolve v | x -> x in
        List.map
          (fun (b : block) ->
            {
              b with
              instrs = List.map (Ir_fold.map_instr resolve) b.instrs;
              terminator = Ir_fold.map_term resolve b.terminator;
            })
          blocks
  in
  { f with blocks }

let budget_instrs = 20000
let budget_calls = 256

let func cands (f : func) =
  let rec go f n =
    if n >= budget_calls || size f > budget_instrs then f
    else
      let site =
        List.find_map
          (fun (b : block) ->
            List.find_mapi
              (fun i ins ->
                match ins with
                | Call (_, _, _, name, args) when name <> f.name -> (
                    match Ptbl.find_opt cands name with
                    | Some (c : func)
                      when List.length c.params = List.length args
                           && List.for_all2
                                (fun (p : param) (t, _, _) -> p.ty = t)
                                c.params args ->
                        Some (b.id, i, c)
                    | _ -> None)
                | _ -> None)
              b.instrs)
          f.blocks
      in
      match site with
      | None -> f
      | Some (bid, pos, c) -> go (inline_at f bid pos c) (n + 1)
  in
  go f 0

let module_ ?only ?(sizes = Ptbl.create 1) (m : module_) =
  let cands = candidates ?only sizes m in
  if Ptbl.length cands = 0 then (m, false)
  else
    let changed = ref false in
    let funcs =
      List.map
        (fun (f : func) ->
          let g = func cands f in
          if g != f then changed := true;
          g)
        m.funcs
    in
    ({ m with funcs }, !changed)
