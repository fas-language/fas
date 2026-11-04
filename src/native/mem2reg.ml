(* Promote scalar stack slots to SSA values, for the native backend.

   Lowering gives every Fas local an `alloca` with loads and stores. A slot
   whose address is only ever used as the pointer of a plain load or store of
   its own scalar type is promoted: phis are placed on the iterated dominance
   frontier of its stores (Cytron et al.), loads are replaced by the value
   reaching them along the dominator tree, and phis that merge one value are
   removed again. Volatile accesses and slots whose address escapes stay in
   memory. *)

open Ir

let scalar = Regalloc.scalar

module Imap = Map.Make (Int)

(* Jumps into an empty block (debug locations aside) that only jumps on go
   straight to its target,
   when the target has no phis to keep apart by predecessor. *)
let thread (f : func) =
  match f.blocks with
  | [] -> f
  | entry :: _ ->
      let by_id = Hashtbl.create 32 in
      List.iter (fun (b : block) -> Hashtbl.replace by_id b.id b) f.blocks;
      let has_phis id =
        match Hashtbl.find_opt by_id id with
        | Some b -> List.exists (function Phi _ -> true | _ -> false) b.instrs
        | None -> true
      in
      let rec final seen id =
        match Hashtbl.find_opt by_id id with
        | Some { instrs; terminator = Br t; _ }
          when id <> entry.id
               && List.for_all (function Location _ -> true | _ -> false) instrs
               && (not (List.mem id seen))
               && not (has_phis t) ->
            final (id :: seen) t
        | _ -> id
      in
      let retarget = function
        | Br t -> Br (final [] t)
        | CondBr (c, y, n) -> CondBr (c, final [] y, final [] n)
        | Switch (ty, v, cases, d) ->
            Switch (ty, v, List.map (fun (k, t) -> (k, final [] t)) cases, final [] d)
        | t -> t
      in
      (* Only rewrite edges into blocks without phis: a phi's incoming labels
         must keep naming real predecessors. *)
      {
        f with
        blocks =
          List.map
            (fun (b : block) -> { b with terminator = retarget b.terminator })
            f.blocks;
      }

let promote (f : func) =
  let f = thread f in
  match f.blocks with
  | [] -> f
  | entry :: _ ->
      let by_id = Hashtbl.create 32 in
      List.iter (fun (b : block) -> Hashtbl.replace by_id b.id b) f.blocks;
      (* Reverse postorder of the reachable blocks *)
      let visited = Hashtbl.create 32 in
      let post = ref [] in
      let rec dfs id =
        if not (Hashtbl.mem visited id) then begin
          Hashtbl.replace visited id ();
          (match Hashtbl.find_opt by_id id with
          | Some b -> List.iter dfs (List.rev (terminator_successors b.terminator))
          | None -> ());
          post := id :: !post
        end
      in
      dfs entry.id;
      let rpo = Array.of_list !post in
      let order = Hashtbl.create 32 in
      Array.iteri (fun i id -> Hashtbl.replace order id i) rpo;
      (* Reverse postorder is also the code layout: loop bodies follow their
         header and values get short intervals. *)
      let blocks = Array.to_list (Array.map (Hashtbl.find by_id) rpo) in
      (* Predecessor edges, with multiplicity *)
      let preds = Hashtbl.create 32 in
      List.iter
        (fun (b : block) ->
          List.iter
            (fun s ->
              Hashtbl.replace preds s
                (b.id :: Option.value ~default:[] (Hashtbl.find_opt preds s)))
            (terminator_successors b.terminator))
        blocks;
      let preds_of id = Option.value ~default:[] (Hashtbl.find_opt preds id) in
      (* Dominators: Cooper, Harvey and Kennedy *)
      let n = Array.length rpo in
      let idom = Array.make n (-1) in
      idom.(0) <- 0;
      let changed = ref true in
      let rec intersect a b =
        if a = b then a
        else if a > b then intersect idom.(a) b
        else intersect a idom.(b)
      in
      while !changed do
        changed := false;
        for i = 1 to n - 1 do
          let ps =
            List.filter_map
              (fun p ->
                match Hashtbl.find_opt order p with
                | Some j when idom.(j) >= 0 -> Some j
                | _ -> None)
              (preds_of rpo.(i))
          in
          match ps with
          | [] -> ()
          | first :: rest ->
              let d = List.fold_left intersect first rest in
              if idom.(i) <> d then begin
                idom.(i) <- d;
                changed := true
              end
        done
      done;
      (* Dominance frontiers, in rpo indices *)
      let df = Array.make n [] in
      for i = 0 to n - 1 do
        let ps =
          List.sort_uniq compare
            (List.filter_map (Hashtbl.find_opt order) (preds_of rpo.(i)))
        in
        if List.length ps >= 2 then
          List.iter
            (fun p ->
              let r = ref p in
              while !r <> idom.(i) do
                if not (List.mem i df.(!r)) then df.(!r) <- i :: df.(!r);
                r := idom.(!r)
              done)
            ps
      done;
      let children = Array.make n [] in
      for i = n - 1 downto 1 do
        children.(idom.(i)) <- i :: children.(idom.(i))
      done;
      (* Promotable slots *)
      let slots = Hashtbl.create 16 in
      List.iter
        (fun (b : block) ->
          List.iter
            (function
              | Alloca (id, ty, _) when scalar ty -> Hashtbl.replace slots id ty
              | _ -> ())
            b.instrs)
        blocks;
      let reject = function Local (id, _) -> Hashtbl.remove slots id | _ -> () in
      List.iter
        (fun (b : block) ->
          List.iter
            (function
              | Load (_, ty, Local (p, _), _) when Hashtbl.find_opt slots p = Some ty ->
                  ()
              | Store (ty, v, Local (p, _), _) when Hashtbl.find_opt slots p = Some ty
                ->
                  reject v
              | ins -> Regalloc.iter_operands reject ins)
            b.instrs;
          Regalloc.iter_term_operands reject b.terminator)
        blocks;
      if Hashtbl.length slots = 0 then { f with blocks }
      else begin
        let next =
          ref
            (1
            + List.fold_left
                (fun acc (b : block) ->
                  List.fold_left
                    (fun acc ins ->
                      match instruction_result ins with
                      | Some (id, _) -> max acc id
                      | None -> acc)
                    acc b.instrs)
                0 blocks)
        in
        (* Phi placement: (block index, slot) -> phi id *)
        let phis = Hashtbl.create 32 in
        let block_phis = Array.make n [] in
        Hashtbl.iter
          (fun slot ty ->
            let work = ref [] in
            List.iter
              (fun (b : block) ->
                if
                  List.exists
                    (function Store (_, _, Local (p, _), _) -> p = slot | _ -> false)
                    b.instrs
                then work := Hashtbl.find order b.id :: !work)
              blocks;
            let placed = Hashtbl.create 8 in
            while !work <> [] do
              let i = List.hd !work in
              work := List.tl !work;
              List.iter
                (fun d ->
                  if not (Hashtbl.mem placed d) then begin
                    Hashtbl.replace placed d ();
                    let id = !next in
                    incr next;
                    Hashtbl.replace phis (d, slot) id;
                    block_phis.(d) <- (slot, ty, id) :: block_phis.(d);
                    work := d :: !work
                  end)
                df.(i)
            done)
          slots;
        (* Renaming along the dominator tree *)
        let subst = Hashtbl.create 64 in
        let incoming =
          Hashtbl.create 32
          (* phi id -> (value, pred) list, reversed *)
        in
        let new_instrs = Array.make n [] in
        let rec rename i env =
          let b = Hashtbl.find by_id rpo.(i) in
          let env =
            List.fold_left
              (fun env (slot, ty, id) -> Imap.add slot (Local (id, ty)) env)
              env block_phis.(i)
          in
          let env = ref env in
          let kept =
            List.filter
              (fun ins ->
                match ins with
                | Alloca (id, _, _) when Hashtbl.mem slots id -> false
                | Load (id, _, Local (p, _), _) when Hashtbl.mem slots p ->
                    Hashtbl.replace subst id (Imap.find p !env);
                    false
                | Store (_, v, Local (p, _), _) when Hashtbl.mem slots p ->
                    env := Imap.add p v !env;
                    false
                | _ -> true)
              b.instrs
          in
          new_instrs.(i) <- kept;
          List.iter
            (fun s ->
              match Hashtbl.find_opt order s with
              | None -> ()
              | Some si ->
                  List.iter
                    (fun (slot, _, id) ->
                      let prev =
                        Option.value ~default:[] (Hashtbl.find_opt incoming id)
                      in
                      Hashtbl.replace incoming id ((Imap.find slot !env, b.id) :: prev))
                    block_phis.(si))
            (terminator_successors b.terminator);
          List.iter (fun c -> rename c !env) children.(i)
        in
        let initial =
          Hashtbl.fold
            (fun slot ty env -> Imap.add slot (Undef ty) env)
            slots Imap.empty
        in
        rename 0 initial;
        (* Remove phis that merge a single value *)
        let rec resolve = function
          | Local (id, _) as v -> (
              match Hashtbl.find_opt subst id with Some w -> resolve w | None -> v)
          | v -> v
        in
        let live_phis = Hashtbl.create 32 in
        Array.iter
          (List.iter (fun (_, ty, id) -> Hashtbl.replace live_phis id ty))
          block_phis;
        let progress = ref true in
        while !progress do
          progress := false;
          Hashtbl.iter
            (fun id _ ->
              if not (Hashtbl.mem subst id) then
                let values =
                  List.filter_map
                    (fun (v, _) ->
                      match resolve v with
                      | Local (j, _) when j = id -> None
                      | v -> Some v)
                    (Option.value ~default:[] (Hashtbl.find_opt incoming id))
                  |> List.sort_uniq compare
                in
                match values with
                | [ v ] ->
                    Hashtbl.replace subst id v;
                    progress := true
                | _ -> ())
            live_phis
        done;
        let blocks =
          List.map
            (fun (b : block) ->
              let i = Hashtbl.find order b.id in
              let phi_instrs =
                List.filter_map
                  (fun (_, ty, id) ->
                    if Hashtbl.mem subst id then None
                    else
                      let ins =
                        List.rev_map
                          (fun (v, p) -> (resolve v, p))
                          (Option.value ~default:[] (Hashtbl.find_opt incoming id))
                      in
                      Some (Phi (id, ty, ins)))
                  (List.rev block_phis.(i))
              in
              let instrs = List.map (Ir_fold.map_instr resolve) new_instrs.(i) in
              (* Existing phis stay first *)
              let old_phis, rest =
                List.partition (function Phi _ -> true | _ -> false) instrs
              in
              {
                b with
                instrs = old_phis @ phi_instrs @ rest;
                terminator = Ir_fold.map_term resolve b.terminator;
              })
            blocks
        in
        { f with blocks }
      end

(* Mark-and-sweep removal of pure instructions nothing needs; it also drops
   the phi cycles promotion leaves behind for variables that are dead after a
   loop. Division stays (it may trap), and so do memory operations. *)
let pure = function
  | Phi _ | Cmp _ | Gep _ | Cast _ | Select _ | Global_ptr _ | String_ptr _ -> true
  | Bin (_, (Udiv | Sdiv | Urem | Srem), _, _, _) -> false
  | Bin _ -> true
  | _ -> false

let dce (f : func) =
  let def = Hashtbl.create 64 in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          match instruction_result ins with
          | Some (id, _) -> Hashtbl.replace def id ins
          | None -> ())
        b.instrs)
    f.blocks;
  let live = Hashtbl.create 64 in
  let work = ref [] in
  let mark = function
    | Local (id, _) when not (Hashtbl.mem live id) ->
        Hashtbl.replace live id ();
        work := id :: !work
    | _ -> ()
  in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins -> if not (pure ins) then Regalloc.iter_operands mark ins)
        b.instrs;
      List.iter
        (fun ins ->
          match instruction_result ins with
          | Some (id, _) when not (pure ins) -> mark (Local (id, Void))
          | _ -> ())
        b.instrs;
      Regalloc.iter_term_operands mark b.terminator)
    f.blocks;
  while !work <> [] do
    let id = List.hd !work in
    work := List.tl !work;
    match Hashtbl.find_opt def id with
    | Some ins -> Regalloc.iter_operands mark ins
    | None -> ()
  done;
  let keep ins =
    (not (pure ins))
    ||
    match instruction_result ins with
    | Some (id, _) -> Hashtbl.mem live id
    | None -> true
  in
  {
    f with
    blocks =
      List.map
        (fun (b : block) -> { b with instrs = List.filter keep b.instrs })
        f.blocks;
  }

(* Drop the blocks no longer reachable from the entry, keeping the order. *)
let prune (f : func) =
  match f.blocks with
  | [] -> f
  | entry :: _ ->
      let by_id = Hashtbl.create 32 in
      List.iter (fun (b : block) -> Hashtbl.replace by_id b.id b) f.blocks;
      let seen = Hashtbl.create 32 in
      let rec visit id =
        if not (Hashtbl.mem seen id) then begin
          Hashtbl.replace seen id ();
          match Hashtbl.find_opt by_id id with
          | Some b -> List.iter visit (terminator_successors b.terminator)
          | None -> ()
        end
      in
      visit entry.id;
      {
        f with
        blocks = List.filter (fun (b : block) -> Hashtbl.mem seen b.id) f.blocks;
      }

(* A block that only jumps on to a block with no other predecessor and no
   phis absorbs it. `for const` bodies arrive as one block per iteration. *)
let merge (f : func) =
  match f.blocks with
  | [] -> f
  | entry :: _ ->
      let preds = Hashtbl.create 32 in
      List.iter
        (fun (b : block) ->
          List.iter
            (fun s ->
              Hashtbl.replace preds s
                (1 + Option.value ~default:0 (Hashtbl.find_opt preds s)))
            (terminator_successors b.terminator))
        f.blocks;
      let by_id = Hashtbl.create 32 in
      List.iter (fun (b : block) -> Hashtbl.replace by_id b.id b) f.blocks;
      let into = Hashtbl.create 16 in
      let rec absorb (b : block) =
        match b.terminator with
        | Br c
          when c <> b.id && c <> entry.id
               && Hashtbl.find_opt preds c = Some 1
               && not (Hashtbl.mem into c) -> (
            match Hashtbl.find_opt by_id c with
            | Some cb
              when not (List.exists (function Phi _ -> true | _ -> false) cb.instrs) ->
                Hashtbl.replace into c b.id;
                absorb
                  { b with instrs = b.instrs @ cb.instrs; terminator = cb.terminator }
            | _ -> b)
        | _ -> b
      in
      let merged =
        List.filter_map
          (fun (b : block) ->
            if Hashtbl.mem into b.id then None
            else begin
              let b' = absorb (Hashtbl.find by_id b.id) in
              Hashtbl.replace by_id b.id b';
              Some b.id
            end)
          f.blocks
      in
      (* Blocks absorbed before their absorber was reached are gone too *)
      let merged = List.filter (fun id -> not (Hashtbl.mem into id)) merged in
      let rec owner id =
        match Hashtbl.find_opt into id with Some o -> owner o | None -> id
      in
      let rename = function
        | Phi (i, t, ins) -> Phi (i, t, List.map (fun (v, p) -> (v, owner p)) ins)
        | ins -> ins
      in
      {
        f with
        blocks =
          List.map
            (fun id ->
              let b = Hashtbl.find by_id id in
              { b with instrs = List.map rename b.instrs })
            merged;
      }

(* (x op c1) op c2 becomes x op (c1 op c2) for associative integer ops, so a
   chain of constant ORs into one word ends as one OR. *)
let reassociate (m : module_) =
  let one (f : func) =
    let defs = Hashtbl.create 64 in
    let combine op w a b =
      let v =
        match op with
        | Add -> Some (Int64.add a b)
        | Mul -> Some (Int64.mul a b)
        | And -> Some (Int64.logand a b)
        | Or -> Some (Int64.logor a b)
        | Xor -> Some (Int64.logxor a b)
        | _ -> None
      in
      Option.map (Ir_fold.norm w) v
    in
    let rewrite ins =
      match ins with
      | Bin (i, ((Add | Mul | And | Or | Xor) as op), ty, a, b) -> (
          let a, b = match (a, b) with Const _, Local _ -> (b, a) | _ -> (a, b) in
          let ins = Bin (i, op, ty, a, b) in
          match (Ir_fold.width ty, a, b) with
          | Some w, Local (x, _), Const (_, c2) -> (
              match Hashtbl.find_opt defs x with
              | Some (Bin (_, op', ty', a', Const (_, c1))) when op' = op && ty' = ty
                -> (
                  match combine op w c1 c2 with
                  | Some c -> Bin (i, op, ty, a', Const (ty, c))
                  | None -> ins)
              | _ -> ins)
          | _ -> ins)
      | ins -> ins
    in
    {
      f with
      blocks =
        List.map
          (fun (b : block) ->
            {
              b with
              instrs =
                List.map
                  (fun ins ->
                    let ins = rewrite ins in
                    (match ins with
                    | Bin (i, _, _, _, _) -> Hashtbl.replace defs i ins
                    | _ -> ());
                    ins)
                  b.instrs;
            })
          f.blocks;
    }
  in
  { m with funcs = List.map one m.funcs }

(* `addr + n` lowers to inttoptr (add (ptrtoint p) n); as `gep i8 p, n` it
   folds into memory operands like any other address. *)
let addresses (f : func) =
  let defs = Hashtbl.create 64 in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          match instruction_result ins with
          | Some (i, _) -> Hashtbl.replace defs i ins
          | None -> ())
        b.instrs)
    f.blocks;
  let pointer_of = function
    | Local (i, _) -> (
        match Hashtbl.find_opt defs i with
        | Some (Cast (_, "ptrtoint", Pointer _, p, I64)) -> Some p
        | _ -> None)
    | _ -> None
  in
  let rewrite = function
    | Cast (id, "inttoptr", I64, Local (s, _), (Pointer _ as pty)) as ins -> (
        match Hashtbl.find_opt defs s with
        | Some (Bin (_, Add, I64, x, y)) -> (
            match (pointer_of x, pointer_of y) with
            | Some p, _ when value_ty p = pty -> Gep (id, I8, p, [ Index y ])
            | _, Some p when value_ty p = pty -> Gep (id, I8, p, [ Index x ])
            | _ -> ins)
        | _ -> ins)
    | ins -> ins
  in
  {
    f with
    blocks =
      List.map
        (fun (b : block) -> { b with instrs = List.map rewrite b.instrs })
        f.blocks;
  }

(* Promotion and DCE empty more blocks, so thread again afterwards. *)
let module_ (m : module_) =
  {
    m with
    funcs = List.map (fun f -> prune (thread (dce (addresses (promote f))))) m.funcs;
  }

(* After folding: constant branches leave dead code and empty blocks. *)
let cleanup (m : module_) =
  { m with funcs = List.map (fun f -> prune (thread (dce f))) m.funcs }
