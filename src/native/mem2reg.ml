(* Promote scalar stack slots to SSA values, for the native backend.

   Lowering gives every Fas local an `alloca` with loads and stores. A slot
   whose address is only ever used as the pointer of a plain load or store of
   its own scalar type is promoted: phis are placed on the iterated dominance
   frontier of its stores (Cytron et al.), loads are replaced by the value
   reaching them along the dominator tree, and phis that merge one value are
   removed again. Volatile accesses and slots whose address escapes stay in
   memory. *)

open Ir
module Hashtbl = Itbl
module Ptbl = Stdlib.Hashtbl

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
              | Alloca (id, ty, _) when scalar ty || Regalloc.v128 ty ->
                  Hashtbl.replace slots id ty
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
        let block_phis = Array.make n [] in
        (* Blocks storing to each slot, in one pass *)
        let def_blocks = Hashtbl.create 32 in
        List.iter
          (fun (b : block) ->
            let i = Hashtbl.find order b.id in
            List.iter
              (function
                | Store (_, _, Local (p, _), _) when Hashtbl.mem slots p -> (
                    match Hashtbl.find_opt def_blocks p with
                    | Some (j :: _) when j = i -> ()
                    | prev ->
                        Hashtbl.replace def_blocks p (i :: Option.value ~default:[] prev)
                    )
                | _ -> ())
              b.instrs)
          blocks;
        Hashtbl.iter
          (fun slot ty ->
            let work =
              ref (Option.value ~default:[] (Hashtbl.find_opt def_blocks slot))
            in
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
(* Intrinsics with no effect besides their result *)
let pure_call name =
  String.starts_with ~prefix:"llvm." name
  && not
       (String.starts_with ~prefix:"llvm.lifetime." name
       || String.starts_with ~prefix:"llvm.masked." name
       || String.starts_with ~prefix:"llvm.fas.store_if" name
       || String.starts_with ~prefix:"llvm.fas.load_if" name
       || String.starts_with ~prefix:"llvm.trap" name
       || String.starts_with ~prefix:"llvm.experimental." name)

let pure = function
  | Phi _ | Cmp _ | Gep _ | Cast _ | Select _ | Global_ptr _ | String_ptr _ | Extract _
  | Insert _ | Shuffle_zero _ | Shufflevector _ ->
      true
  | Call (Some _, _, _, name, _) -> pure_call name
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

let is_pow2 c = Int64.compare c 0L > 0 && Int64.logand c (Int64.pred c) = 0L

let log2 c =
  let rec go k c = if c = 1L then k else go (k + 1) (Int64.shift_right_logical c 1) in
  go 0 c

(* (x op c1) op c2 becomes x op (c1 op c2) for associative integer ops, so a
   chain of constant ORs into one word ends as one OR. Unsigned division and remainder by a power of two
   become a shift and a mask. *)
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
      (* Unsigned division and remainder by a power of two *)
      | Bin (i, Udiv, ty, a, Const (_, c)) when is_pow2 c ->
          Bin (i, Lshr, ty, a, Const (ty, Int64.of_int (log2 c)))
      | Bin (i, Urem, ty, a, Const (_, c)) when is_pow2 c ->
          Bin (i, And, ty, a, Const (ty, Int64.pred c))
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
                    (* x * 2^k as x << k, after combining constant chains *)
                    match ins with
                    | Bin (i, Mul, ty, a, Const (_, c))
                      when is_pow2 c && Int64.compare c 1L > 0 ->
                        Bin (i, Shl, ty, a, Const (ty, Int64.of_int (log2 c)))
                    | ins -> ins)
                  b.instrs;
            })
          f.blocks;
    }
  in
  { m with funcs = List.map one m.funcs }

(* Demanded bits: below `and x, 2^k-1` only the low k bits of [x] matter,
   and through single-use add, sub, mul, bitwise operations and constant
   left shifts only the low bits of their operands, so an operand
   `and y, m'` whose mask keeps those bits reads [y] instead. *)
let demanded_bits (f : func) =
  let defs = Hashtbl.create 64 and uses = Hashtbl.create 64 in
  let count v =
    (match v with
    | Local (i, _) ->
        Hashtbl.replace uses i (1 + Option.value ~default:0 (Hashtbl.find_opt uses i))
    | _ -> ());
    v
  in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          (match instruction_result ins with
          | Some (i, _) -> Hashtbl.replace defs i ins
          | None -> ());
          ignore (Ir_fold.map_instr count ins))
        b.instrs;
      ignore (Ir_fold.map_term count b.terminator))
    f.blocks;
  let low_mask m = Int64.compare m 0L > 0 && Int64.logand m (Int64.succ m) = 0L in
  let changed = Hashtbl.create 8 in
  let rec strip m v =
    match v with
    | Local (i, _) -> (
        match Hashtbl.find_opt defs i with
        | Some (Bin (_, And, _, z, Const (_, m'))) when Int64.logand m m' = m ->
            strip m z
        | _ ->
            narrow m i;
            v)
    | _ -> v
  and narrow m i =
    if Hashtbl.find_opt uses i = Some 1 && not (Hashtbl.mem changed i) then
      match Hashtbl.find_opt defs i with
      | Some (Bin (d, ((Add | Sub | Mul | And | Or | Xor) as op), ty, a, b)) ->
          let a' = strip m a and b' = strip m b in
          if a' != a || b' != b then begin
            let ins = Bin (d, op, ty, a', b') in
            Hashtbl.replace defs i ins;
            Hashtbl.replace changed i ins
          end
      | Some (Bin (d, Shl, ty, a, (Const (_, c) as k)))
        when Int64.compare c 0L > 0 && Int64.compare c 64L < 0 ->
          let m' = Int64.shift_right_logical m (Int64.to_int c) in
          if Int64.compare m' 0L > 0 then begin
            let a' = strip m' a in
            if a' != a then begin
              let ins = Bin (d, Shl, ty, a', k) in
              Hashtbl.replace defs i ins;
              Hashtbl.replace changed i ins
            end
          end
      | _ -> ()
  in
  List.iter
    (fun (b : block) ->
      List.iter
        (function
          | Bin (_, And, _, Local (x, _), Const (_, m)) when low_mask m -> narrow m x
          | _ -> ())
        b.instrs)
    f.blocks;
  if Hashtbl.length changed = 0 then f
  else
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
                    match instruction_result ins with
                    | Some (i, _) -> (
                        match Hashtbl.find_opt changed i with
                        | Some n -> n
                        | None -> ins)
                    | None -> ins)
                  b.instrs;
            })
          f.blocks;
    }

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

(* Scalar replacement of local arrays: an `arr[N, T]` local with a scalar `T`
   whose every use is a constant-index element address (`gep [N x T], a, 0, k`)
   loaded or stored as a `T`, or a zero fill, becomes N scalar slots, which
   [promote] then turns into registers. This is what `for const` filling a
   local array buys (plan, rule 1). *)
let sroa (f : func) =
  let candidates = Hashtbl.create 8 in
  List.iter
    (fun (b : block) ->
      List.iter
        (function
          | Alloca (id, (Array (n, elt) as ty), al) when scalar elt && n > 0 && n <= 64
            ->
              Hashtbl.replace candidates id (ty, n, elt, al)
          | _ -> ())
        b.instrs)
    f.blocks;
  if Hashtbl.length candidates = 0 then f
  else begin
    let const_index = function
      | Index (Const (_, k)) -> Some (Int64.to_int k)
      | Zero -> Some 0
      | Index _ -> None
    in
    (* gep id -> (array, element) *)
    let elements = Hashtbl.create 16 in
    List.iter
      (fun (b : block) ->
        List.iter
          (function
            | Gep (g, gty, Local (a, _), [ i0; ik ]) when Hashtbl.mem candidates a -> (
                let ty, n, _, _ = Hashtbl.find candidates a in
                match (const_index i0, const_index ik) with
                | Some 0, Some k when gty = ty && k >= 0 && k < n ->
                    Hashtbl.replace elements g (a, k)
                | _ -> Hashtbl.remove candidates a)
            | _ -> ())
          b.instrs)
      f.blocks;
    let reject_value = function
      | Local (v, _) -> (
          Hashtbl.remove candidates v;
          match Hashtbl.find_opt elements v with
          | Some (a, _) -> Hashtbl.remove candidates a
          | None -> ())
      | _ -> ()
    in
    let element_ok g ty =
      match Hashtbl.find_opt elements g with
      | Some (a, _) -> (
          match Hashtbl.find_opt candidates a with
          | Some (_, _, elt, _) -> elt = ty
          | None -> false)
      | None -> false
    in
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match ins with
            | Gep (g, _, Local (a, _), _) when Hashtbl.mem elements g ->
                ignore a (* the base use is accounted for by [elements] *)
            | Load (_, ty, Local (g, _), _) when Hashtbl.mem elements g ->
                if not (element_ok g ty) then reject_value (Local (g, ty))
            | Store (ty, v, Local (g, _), _) when Hashtbl.mem elements g ->
                if not (element_ok g ty) then reject_value (Local (g, ty));
                reject_value v
            | Store (ty, Zero _, Local (a, _), _) when Hashtbl.mem candidates a -> (
                match Hashtbl.find_opt candidates a with
                | Some (aty, _, _, _) when aty = ty -> ()
                | _ -> Hashtbl.remove candidates a)
            | ins -> Regalloc.iter_operands reject_value ins)
          b.instrs;
        Regalloc.iter_term_operands reject_value b.terminator)
      f.blocks;
    if Hashtbl.length candidates = 0 then f
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
              0 f.blocks)
      in
      (* array id -> element slot ids *)
      let slots = Hashtbl.create 8 in
      Hashtbl.iter
        (fun a (_, n, _, _) ->
          Hashtbl.replace slots a
            (Array.init n (fun _ ->
                 let id = !next in
                 incr next;
                 id)))
        candidates;
      let zero elt = match elt with Pointer _ -> Null elt | t -> Const (t, 0L) in
      let resolve = function
        | Local (g, t) as v -> (
            match Hashtbl.find_opt elements g with
            | Some (a, k) when Hashtbl.mem slots a ->
                Local ((Hashtbl.find slots a).(k), t)
            | _ -> v)
        | v -> v
      in
      let rewrite ins =
        match ins with
        | Alloca (a, _, al) when Hashtbl.mem slots a ->
            let _, _, elt, _ = Hashtbl.find candidates a in
            Array.to_list
              (Array.map (fun s -> Alloca (s, elt, al)) (Hashtbl.find slots a))
        | Gep (g, _, _, _)
          when match Hashtbl.find_opt elements g with
               | Some (a, _) -> Hashtbl.mem slots a
               | None -> false ->
            []
        | Store (_, Zero _, (Local (a, _) as p), al) when Hashtbl.mem slots a ->
            let _, _, elt, _ = Hashtbl.find candidates a in
            Array.to_list
              (Array.map
                 (fun s -> Store (elt, zero elt, Local (s, value_ty p), al))
                 (Hashtbl.find slots a))
        | ins -> [ Ir_fold.map_instr resolve ins ]
      in
      {
        f with
        blocks =
          List.map
            (fun (b : block) ->
              {
                b with
                instrs = List.concat_map rewrite b.instrs;
                terminator = Ir_fold.map_term resolve b.terminator;
              })
            f.blocks;
      }
    end
  end

(* `p[u8, b + o1]`, `p[u8, b + o2]`, ... in one block: x86 addressing has one
   base and one index, so `p + (b + ok)` costs an add per access. When one
   addend of the index repeats across byte geps on the same base in a block,
   compute `q = p + b` once and address `[q + ok]`. *)
let split_addresses (m : module_) =
  let one (f : func) =
    let defs = Hashtbl.create 64 in
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match ins with
            | Bin (id, Add, I64, (Local _ as x), (Local _ as y)) ->
                Hashtbl.replace defs id (x, y)
            | _ -> ())
          b.instrs)
      f.blocks;
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
            0 f.blocks)
    in
    let split_index = function
      | Gep (_, I8, ((Local _ | Param _) as base), [ Index (Local (s, _)) ]) -> (
          match Hashtbl.find_opt defs s with
          | Some (x, y) -> Some (base, x, y)
          | None -> None)
      | _ -> None
    in
    let block (b : block) =
      let counts = Ptbl.create 8 in
      let count k =
        Ptbl.replace counts k (1 + Option.value ~default:0 (Ptbl.find_opt counts k))
      in
      List.iter
        (fun ins ->
          match split_index ins with
          | Some (base, x, y) ->
              count (base, x);
              if y <> x then count (base, y)
          | None -> ())
        b.instrs;
      let shared = Ptbl.create 8 in
      let instrs =
        List.concat_map
          (fun ins ->
            match (ins, split_index ins) with
            | Gep (g, _, _, _), Some (base, x, y) -> (
                let n k = Option.value ~default:0 (Ptbl.find_opt counts k) in
                let common, other =
                  if n (base, x) >= n (base, y) then (x, y) else (y, x)
                in
                if n (base, common) < 2 then [ ins ]
                else
                  let pty = value_ty base in
                  match Ptbl.find_opt shared (base, common) with
                  | Some q -> [ Gep (g, I8, Local (q, pty), [ Index other ]) ]
                  | None ->
                      let q = !next in
                      incr next;
                      Ptbl.replace shared (base, common) q;
                      [
                        Gep (q, I8, base, [ Index common ]);
                        Gep (g, I8, Local (q, pty), [ Index other ]);
                      ])
            | _ -> [ ins ])
          b.instrs
      in
      { b with instrs }
    in
    { f with blocks = List.map block f.blocks }
  in
  { m with funcs = List.map one m.funcs }

let tm_acc = Ptbl.create 8
let tm_on = Sys.getenv_opt "FAS_NATIVE_TIME" = Some "1"

let tm name f x =
  if not tm_on then f x
  else
    let t0 = Unix.gettimeofday () in
    let r = f x in
    let d = Unix.gettimeofday () -. t0 in
    Ptbl.replace tm_acc name (d +. Option.value ~default:0. (Ptbl.find_opt tm_acc name));
    r

let () =
  if tm_on then
    at_exit (fun () ->
        Ptbl.iter (fun k v -> Printf.eprintf "  %-12s %8.2f ms\n" k (v *. 1000.)) tm_acc)

(* Promotion and DCE empty more blocks, so thread again afterwards. *)
let func f =
  let f = tm "sroa" sroa f in
  let f = tm "promote" promote f in
  let f = tm "addresses" addresses f in
  let f = tm "dce" dce f in
  prune (tm "thread" thread f)

let module_ (m : module_) = { m with funcs = List.map func m.funcs }

(* Block-local common subexpressions: a second identical arithmetic or cast
   instruction in a block reuses the first one's result. Comparisons and
   geps stay, since isel fuses each one into its single user (a branch, a
   memory operand); division stays because it may trap. *)
let cse (f : func) =
  let subst = Hashtbl.create 16 in
  let rec resolve = function
    | Local (id, _) as v -> (
        match Hashtbl.find_opt subst id with Some w -> resolve w | None -> v)
    | v -> v
  in
  let blocks =
    List.map
      (fun (b : block) ->
        let seen = Ptbl.create 16 in
        let instrs =
          List.filter_map
            (fun ins ->
              let ins = Ir_fold.map_instr resolve ins in
              let key, id, ty =
                match ins with
                | Bin (_, (Udiv | Sdiv | Urem | Srem), _, _, _) -> (None, 0, Void)
                | Bin (i, op, ty, a, b) -> (Some (Bin (0, op, ty, a, b)), i, ty)
                | Cast (i, k, t1, v, t2) -> (Some (Cast (0, k, t1, v, t2)), i, t2)
                | _ -> (None, 0, Void)
              in
              match key with
              | None -> Some ins
              | Some k -> (
                  match Ptbl.find_opt seen k with
                  | Some first ->
                      Hashtbl.replace subst id (Local (first, ty));
                      None
                  | None ->
                      Ptbl.replace seen k id;
                      Some ins))
            b.instrs
        in
        { b with instrs })
      f.blocks
  in
  if Hashtbl.length subst = 0 then f
  else
    {
      f with
      blocks =
        List.map
          (fun (b : block) ->
            {
              b with
              instrs = List.map (Ir_fold.map_instr resolve) b.instrs;
              terminator = Ir_fold.map_term resolve b.terminator;
            })
          blocks;
    }

(* After folding: constant branches leave dead code and empty blocks. *)

(* If-conversion: a branch around at most a few cheap pure instructions (a
   triangle, or a diamond with two such arms) whose join merges only those
   paths becomes straight-line code with selects, so data-dependent
   conditions cost a cmov instead of a mispredicted jump. The join's phis
   turn into selects in place and [merge] later joins the blocks. *)
let ifconv_on = Sys.getenv_opt "FAS_NATIVE_NOIFCONV" <> Some "1"

let ifconvert (f : func) =
  match f.blocks with
  | [] | [ _ ] -> f
  | _ when not ifconv_on -> f
  | _ ->
      let preds = Hashtbl.create 32 in
      List.iter
        (fun (b : block) ->
          List.iter
            (fun s ->
              Hashtbl.replace preds s
                (b.id :: Option.value ~default:[] (Hashtbl.find_opt preds s)))
            (List.sort_uniq compare (terminator_successors b.terminator)))
        f.blocks;
      let by_id = Hashtbl.create 32 in
      List.iter (fun (b : block) -> Hashtbl.replace by_id b.id b) f.blocks;
      let cheap ins = match ins with Phi _ | Call _ -> false | _ -> pure ins in
      let selectable = function
        | I1 | I8 | I16 | I32 | I64 | Pointer _ -> true
        | _ -> false
      in
      (* an arm: a block of [a] alone that jumps to [j] after cheap work *)
      let arm a id j =
        match Hashtbl.find_opt by_id id with
        | Some ({ terminator = Br j'; _ } as b)
          when j' = j
               && Hashtbl.find_opt preds id = Some [ a ]
               && List.length b.instrs <= 4
               && List.for_all cheap b.instrs ->
            Some b
        | _ -> None
      in
      let changed = ref false in
      List.iter
        (fun (b0 : block) ->
          let a = Hashtbl.find by_id b0.id in
          match a.terminator with
          | CondBr ((Local _ as c), t, e) when t <> e && t <> a.id && e <> a.id -> (
              (* arms on the true and false side, and the join *)
              let shape =
                match (arm a.id t e, arm a.id e t) with
                | Some bt, _ -> Some (Some bt, None, e)
                | None, Some be -> Some (None, Some be, t)
                | None, None -> (
                    match Hashtbl.find_opt by_id t with
                    | Some { terminator = Br j; _ } when j <> a.id -> (
                        match (arm a.id t j, arm a.id e j) with
                        | Some bt, Some be -> Some (Some bt, Some be, j)
                        | _ -> None)
                    | _ -> None)
              in
              match shape with
              | None -> ()
              | Some (bt, be, j) -> (
                  let from_t = match bt with Some b -> b.id | None -> a.id in
                  let from_e = match be with Some b -> b.id | None -> a.id in
                  let jp = List.sort_uniq compare [ from_t; from_e ] in
                  match Hashtbl.find_opt by_id j with
                  | Some jb
                    when j <> a.id
                         && List.sort_uniq compare
                              (Option.value ~default:[] (Hashtbl.find_opt preds j))
                            = jp ->
                      let phis =
                        List.filter_map
                          (function Phi (i, ty, ins) -> Some (i, ty, ins) | _ -> None)
                          jb.instrs
                      in
                      let ok =
                        phis <> []
                        && List.for_all
                             (fun (_, ty, ins) ->
                               selectable ty
                               && List.mem_assoc from_t
                                    (List.map (fun (v, p) -> (p, v)) ins)
                               && List.mem_assoc from_e
                                    (List.map (fun (v, p) -> (p, v)) ins))
                             phis
                      in
                      if ok then begin
                        changed := true;
                        let incoming ins p =
                          fst (List.find (fun (_, q) -> q = p) ins)
                        in
                        let instrs =
                          List.map
                            (function
                              | Phi (i, _, ins) ->
                                  Select (i, c, incoming ins from_t, incoming ins from_e)
                              | ins -> ins)
                            jb.instrs
                        in
                        let hoist = function
                          | Some (b : block) -> b.instrs
                          | None -> []
                        in
                        Hashtbl.replace by_id a.id
                          {
                            a with
                            instrs = a.instrs @ hoist bt @ hoist be;
                            terminator = Br j;
                          };
                        Hashtbl.replace by_id j { jb with instrs };
                        (* the join now has the one predecessor [a] *)
                        Hashtbl.replace preds j [ a.id ]
                      end
                  | _ -> ()))
          | _ -> ())
        f.blocks;
      if not !changed then f
      else
        prune
          {
            f with
            blocks = List.map (fun (b : block) -> Hashtbl.find by_id b.id) f.blocks;
          }

let cleanup_func f = prune (thread (dce (cse (ifconvert f))))
let cleanup (m : module_) = { m with funcs = List.map cleanup_func m.funcs }

(* A compare used once, by a select or the branch of its own block, moves
   down next to that user, so code generation fuses the two into cmp + cmov
   or cmp + jcc instead of materializing the flag with setcc. *)
let sink_compares (f : func) =
  let uses = Hashtbl.create 64 in
  let count = function
    | Local (id, _) ->
        Hashtbl.replace uses id (1 + Option.value ~default:0 (Hashtbl.find_opt uses id))
    | _ -> ()
  in
  List.iter
    (fun (b : block) ->
      List.iter (Regalloc.iter_operands count) b.instrs;
      Regalloc.iter_term_operands count b.terminator)
    f.blocks;
  let once id = Hashtbl.find_opt uses id = Some 1 in
  let block (b : block) =
    let pending = Hashtbl.create 4 in
    let branch_cmp =
      match b.terminator with CondBr (Local (c, _), _, _) -> c | _ -> -1
    in
    let selected = Hashtbl.create 8 in
    List.iter
      (function
        | Select (_, Local (s, _), _, _) -> Hashtbl.replace selected s () | _ -> ())
      b.instrs;
    let rec go acc = function
      | [] -> List.rev acc
      | (Cmp (id, _, _, _, _) as c) :: rest
        when once id && (id = branch_cmp || Hashtbl.mem selected id) ->
          Hashtbl.replace pending id c;
          go acc rest
      | (Select (_, Local (s, _), _, _) as sel) :: rest when Hashtbl.mem pending s ->
          let c = Hashtbl.find pending s in
          Hashtbl.remove pending s;
          go (sel :: c :: acc) rest
      | ins :: rest -> go (ins :: acc) rest
    in
    let instrs = go [] b.instrs in
    match Hashtbl.find_opt pending branch_cmp with
    | Some c -> { b with instrs = instrs @ [ c ] }
    | None -> { b with instrs }
  in
  if
    List.exists
      (fun (b : block) -> List.exists (function Cmp _ -> true | _ -> false) b.instrs)
      f.blocks
  then { f with blocks = List.map block f.blocks }
  else f

let sink_compares_module (m : module_) =
  { m with funcs = List.map sink_compares m.funcs }
