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
let sole_site_limit = 300
let slot_arg_limit = 160
let few_sites_growth = 96

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
  (* Larger internal functions inline at some sites only: their only call
     site (the code does not grow), and calls passing the caller's stack
     slots, which inlining lets the caller promote to registers. *)
  let sites = Ptbl.create 64 in
  List.iter
    (fun (f : func) ->
      List.iter
        (fun (b : block) ->
          List.iter
            (function
              | Call (_, _, _, name, _) ->
                  Ptbl.replace sites name
                    (1 + Option.value ~default:0 (Ptbl.find_opt sites name))
              | _ -> ())
            b.instrs)
        f.blocks)
    m.funcs;
  let extra = Ptbl.create 16 in
  List.iter
    (fun (f : func) ->
      if f.linkage = Internal && (not (Ptbl.mem out f.name)) && eligible f then
        let n = own_size f in
        let k = Option.value ~default:0 (Ptbl.find_opt sites f.name) in
        (* inlined everywhere: one site, or a few whose copies stay small *)
        let once = k = 1 || (k <= 3 && n * (k - 1) <= few_sites_growth) in
        if (once && n <= sole_site_limit) || n <= slot_arg_limit then
          Ptbl.replace extra f.name (f, n, once))
    m.funcs;
  (out, extra)

let allocas (f : func) =
  let t = Hashtbl.create 16 in
  List.iter
    (fun (b : block) ->
      List.iter
        (function
          | Alloca (id, _, _) -> Hashtbl.replace t id ()
          (* addresses derived from a slot count as the slot *)
          | (Cast (id, "bitcast", _, Local (j, _), _) | Gep (id, _, Local (j, _), _))
            when Hashtbl.mem t j ->
              Hashtbl.replace t id ()
          | _ -> ())
        b.instrs)
    f.blocks;
  t

let budget_instrs = 20000
let budget_calls = 256

(* Inline the chosen call sites of [f] in one pass: a block splits at each
   inlined call into its head, the copied callee and a continuation, and the
   copies are scanned in turn for further sites. The call results and the
   predecessor renames for the phis are substituted once at the end. *)
let func (cands, extra) (f : func) =
  let mv, mb = max_ids f in
  let next_v = ref (mv + 1) and next_b = ref (mb + 1) in
  let total = ref (size f) and count = ref 0 in
  let slots = allocas f in
  let hoisted = ref [] in
  let subst = Hashtbl.create 16 in
  (* the block that now ends with the terminator of a split block *)
  let moved = Hashtbl.create 16 in
  let rec resolve = function
    | Local (i, _) as v -> (
        match Hashtbl.find_opt subst i with Some w -> resolve w | None -> v)
    | v -> v
  in
  let rec final b =
    match Hashtbl.find_opt moved b with Some c -> final c | None -> b
  in
  let ids = Ptbl.create 8 in
  let callee_ids (c : func) =
    match Ptbl.find_opt ids c.name with
    | Some x -> x
    | None ->
        let mv, mb = max_ids c in
        let x = (mv, mb, size c) in
        Ptbl.replace ids c.name x;
        x
  in
  let chosen name args =
    let c =
      match Ptbl.find_opt cands name with
      | Some c -> Some c
      | None -> (
          match Ptbl.find_opt extra name with
          | Some (c, _, true) -> Some c
          | Some (c, _, false)
            when List.exists
                   (fun (_, _, v) ->
                     match resolve v with
                     | Local (id, _) -> Hashtbl.mem slots id
                     | _ -> false)
                   args ->
              Some c
          | _ -> None)
    in
    match c with
    | Some (c : func)
      when name <> f.name && !count < budget_calls && !total <= budget_instrs
           && List.length c.params = List.length args
           && List.for_all2 (fun (p : param) (t, _, _) -> p.ty = t) c.params args ->
        Some c
    | _ -> None
  in
  let out = ref [] in
  let rec emit (blk : block) =
    let rec split before = function
      | [] -> out := blk :: !out
      | (Call (dst, _, ret_ty, name, args) as ins) :: after -> (
          match chosen name args with
          | None -> split (ins :: before) after
          | Some callee ->
              if Sys.getenv_opt "FAS_INLINE_DEBUG" = Some "1" then
                Printf.eprintf "inline %s <- %s %d (%s)\n" f.name callee.name
                  (size callee)
                  (if Ptbl.mem cands callee.name then "small"
                   else
                     match Ptbl.find_opt extra callee.name with
                     | Some (_, _, true) -> "sole"
                     | _ -> "slots");
              incr count;
              let cmv, cmb, csize = callee_ids callee in
              total := !total + csize;
              let kv = !next_v and kb = !next_b in
              next_v := kv + cmv + 1;
              let cont = kb + cmb + 1 in
              next_b := cont + 1;
              let env = Ptbl.create 8 in
              List.iter2
                (fun (p : param) (_, _, v) -> Ptbl.replace env p.name v)
                callee.params args;
              let value = function
                | Local (i, t) -> Local (i + kv, t)
                | Param (n, t) -> (
                    match Ptbl.find_opt env n with Some v -> v | None -> Param (n, t))
                | v -> v
              in
              let returns = ref [] in
              let copied =
                List.map
                  (fun (b : block) ->
                    let id = b.id + kb in
                    let instrs =
                      List.filter_map
                        (fun ins ->
                          match
                            shift_phi_blocks kb
                              (shift_def kv (Ir_fold.map_instr value ins))
                          with
                          | Alloca (i, _, _) as ins ->
                              Hashtbl.replace slots i ();
                              hoisted := ins :: !hoisted;
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
              let entry =
                match copied with b :: _ -> b.id | [] -> invalid_arg "Inline.func"
              in
              let phi =
                match (dst, !returns) with
                | None, _ -> []
                | Some d, [ (v, _) ] ->
                    Hashtbl.replace subst d v;
                    []
                | Some d, [] ->
                    Hashtbl.replace subst d (Undef ret_ty);
                    []
                | Some d, rs -> [ Phi (d, ret_ty, List.rev rs) ]
              in
              Hashtbl.replace moved blk.id cont;
              out :=
                { blk with instrs = List.rev before; terminator = Br entry } :: !out;
              List.iter emit copied;
              emit
                {
                  id = cont;
                  label = blk.label ^ ".cont";
                  instrs = phi @ after;
                  terminator = blk.terminator;
                })
      | ins :: after -> split (ins :: before) after
    in
    split [] blk.instrs
  in
  List.iter emit f.blocks;
  if !count = 0 then f
  else
    let fix_phi = function
      | Phi (i, t, ins) -> Phi (i, t, List.map (fun (v, p) -> (resolve v, final p)) ins)
      | ins -> Ir_fold.map_instr resolve ins
    in
    let blocks =
      List.mapi
        (fun k (b : block) ->
          let instrs = List.map fix_phi b.instrs in
          {
            b with
            instrs = (if k = 0 then List.rev !hoisted @ instrs else instrs);
            terminator = Ir_fold.map_term resolve b.terminator;
          })
        (List.rev !out)
    in
    { f with blocks }

(* The functions reachable from the exported ones and from global data;
   internal functions whose every call was inlined are not. *)
let reachable (m : module_) =
  let by_name = Ptbl.create 64 in
  List.iter (fun (f : func) -> Ptbl.replace by_name f.name f) m.funcs;
  let live = Ptbl.create 64 in
  let rec mark name =
    if not (Ptbl.mem live name) then begin
      Ptbl.replace live name ();
      match Ptbl.find_opt by_name name with
      | None -> ()
      | Some (f : func) ->
          let value = function Global (n, _) -> mark n | _ -> () in
          List.iter
            (fun (b : block) ->
              List.iter
                (fun ins ->
                  (match ins with
                  | Call (_, _, _, n, _) | Global_ptr (_, n, _) -> mark n
                  | _ -> ());
                  ignore
                    (Ir_fold.map_instr
                       (fun v ->
                         value v;
                         v)
                       ins))
                b.instrs;
              ignore
                (Ir_fold.map_term
                   (fun v ->
                     value v;
                     v)
                   b.terminator))
            f.blocks
    end
  in
  List.iter
    (function
      | Storage_global { pointers; _ } -> List.iter (fun (_, n, _) -> mark n) pointers
      | _ -> ())
    m.globals;
  List.iter (fun (f : func) -> if f.linkage <> Internal then mark f.name) m.funcs;
  live

(* A function inlined at its only call site is copied before its own calls
   are inlined, so its callees are inlined (and scanned) once, in the copy;
   one still reachable afterwards, for lack of budget, is done after. *)
let module_ ?only ?(sizes = Ptbl.create 1) (m : module_) =
  let ((c, e) as cands) = candidates ?only sizes m in
  if Ptbl.length c = 0 && Ptbl.length e = 0 then (m, false)
  else
    let changed = ref false in
    let sole (f : func) =
      match Ptbl.find_opt e f.name with Some (_, _, once) -> once | None -> false
    in
    let run pick funcs =
      List.map
        (fun (f : func) ->
          if not (pick f) then f
          else
            let g = func cands f in
            if g != f then changed := true;
            g)
        funcs
    in
    let funcs = run (fun f -> not (sole f)) m.funcs in
    let live = reachable { m with funcs } in
    let funcs = run (fun f -> sole f && Ptbl.mem live f.name) funcs in
    ({ m with funcs }, !changed)
