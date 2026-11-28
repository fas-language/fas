(* Constant folding over Fir, run on every Fas build (it replaces Ir_fold,
   roadmap v0.6; same scope, not an optimizer).

   Lowering emits const-generic and `for const` arithmetic as instructions on
   constants (`udiv trap i64 -1, 3`, `and i8 0, 7`, `zext i64 0`); every
   division carries the `trap` flag. This pass folds scalar integer
   arithmetic (up to 64 bits), comparisons, integer casts, selects, and block
   parameters that receive one value, then turns branches and switches on
   constants into jumps and empties the blocks that become unreachable. It
   never folds an operation whose result would be a trap or poison (division
   by zero, MIN / -1, out-of-range shift counts): those stay, and trap when
   they run. A folded instruction becomes an alias of its
   value (Fir.replace_all_uses) and is removed. *)

open Fir

let mask w x = if w >= 64 then x else Int64.logand x (Int64.pred (Int64.shift_left 1L w))
let sext w x = if w >= 64 then x else Int64.shift_right (Int64.shift_left x (64 - w)) (64 - w)

(* A scalar integer constant of at most 64 bits: (width, zero-extended bits) *)
let int_const f v =
  match ty_of f (resolve f v) with
  | Int w when w <= 64 -> (
      match const_of f v with
      | Some (Cint (_, l)) -> Some (w, mask w l.(0))
      | Some (Czero _) -> Some (w, 0L)
      | _ -> None)
  | _ -> None

let ibin f k a b =
  match (int_const f a, int_const f b) with
  | Some (w, x), Some (_, y) -> (
      let in_range = Int64.compare y 0L >= 0 && Int64.compare y (Int64.of_int w) < 0 in
      let minimum = sext w (Int64.shift_left 1L (w - 1)) in
      let signed_ok = y <> 0L && not (sext w x = minimum && sext w y = -1L) in
      let c v = Some (iconst f (Int w) (sext w (mask w v))) in
      match k with
      | Add -> c (Int64.add x y)
      | Sub -> c (Int64.sub x y)
      | Mul -> c (Int64.mul x y)
      | And -> c (Int64.logand x y)
      | Or -> c (Int64.logor x y)
      | Xor -> c (Int64.logxor x y)
      | Shl when in_range -> c (Int64.shift_left x (Int64.to_int y))
      | Lshr when in_range -> c (Int64.shift_right_logical x (Int64.to_int y))
      | Ashr when in_range -> c (Int64.shift_right (sext w x) (Int64.to_int y))
      | Udiv when y <> 0L -> c (Int64.unsigned_div x y)
      | Urem when y <> 0L -> c (Int64.unsigned_rem x y)
      | Sdiv when signed_ok -> c (Int64.div (sext w x) (sext w y))
      | Srem when signed_ok -> c (Int64.rem (sext w x) (sext w y))
      | _ -> None)
  | _ -> None

(* `x + 0`, `x * 1`, `x & -1`, `x << 0` ... are x; `x * 0` and `x & 0` are 0 *)
let identity f k a b =
  match ty_of f (resolve f a) with
  | Int w when w <= 64 -> (
      let is n v = match int_const f v with Some (_, x) -> x = mask w n | None -> false in
      match k with
      | (Add | Or | Xor) when is 0L b -> Some a
      | (Add | Or | Xor) when is 0L a -> Some b
      | (Sub | Shl | Lshr | Ashr) when is 0L b -> Some a
      | Mul when is 1L b -> Some a
      | Mul when is 1L a -> Some b
      | (Mul | And) when is 0L b || is 0L a -> Some (iconst f (Int w) 0L)
      | And when is (-1L) b -> Some a
      | And when is (-1L) a -> Some b
      | (Udiv | Sdiv) when is 1L b -> Some a
      | _ -> None)
  | _ -> None

let icmp f k a b =
  match (int_const f a, int_const f b) with
  | Some (w, x), Some (_, y) ->
      let s = compare (sext w x) (sext w y) and u = Int64.unsigned_compare x y in
      let r =
        match k with
        | Eq -> x = y | Ne -> x <> y
        | Slt -> s < 0 | Sle -> s <= 0 | Sgt -> s > 0 | Sge -> s >= 0
        | Ult -> u < 0 | Ule -> u <= 0 | Ugt -> u > 0 | Uge -> u >= 0
      in
      Some (iconst f i1 (if r then 1L else 0L))
  | _ -> None

(* The value an instruction is equal to, if it can be known now *)
let evaluate f (ir : inst_rec) =
  if Array.length ir.results <> 1 then None
  else
    let rt = ty_of f ir.results.(0) in
    match ir.op with
    | Ibin (k, _, a, b) -> ( match ibin f k a b with Some v -> Some v | None -> identity f k a b)
    | Icmp ((Eq | Ne as k), a, b)
      when is_ptr (ty_of f a) && (match const_of f a, const_of f b with
          | Some (Cnull _ | Czero (Ptr _)), Some (Cnull _ | Czero (Ptr _)) -> true
          | _ -> false) -> Some (iconst f i1 (if k = Eq then 1L else 0L))
    | Icmp (k, a, b) -> icmp f k a b
    | Conv ((Zext | Trunc), a) -> (
        match (int_const f a, rt) with Some (_, x), Int tw when tw <= 64 -> Some (iconst f rt (sext tw (mask tw x))) | _ -> None)
    | Conv (Sext, a) -> (
        match (int_const f a, rt) with
        | Some (w, x), Int tw when tw <= 64 -> Some (iconst f rt (sext tw (mask tw (sext w x))))
        | _ -> None)
    | Conv (Inttoptr, a) when int_value f a = Some 0L ->
        Some (const f (Cnull rt) rt)
    | Conv (Ptrtoint, a)
      when (match const_of f a with Some (Cnull _ | Czero (Ptr _)) -> true | _ -> false) ->
        Some (iconst f rt 0L)
    (* a bitcast to its own type is a copy *)
    | Conv (Bitcast, a) when ty_of f (resolve f a) = rt -> Some a
    (* a byte address at offset zero is its base *)
    | Ptradd (_, p, idx, scale, 0) when (scale = 0 || int_value f idx = Some 0L) && ty_of f (resolve f p) = rt -> Some p
    | Select (c, a, b) -> ( match int_const f c with Some (_, x) -> Some (if x <> 0L then a else b) | None -> None)
    | _ -> None

let targets = function
  | Jump t -> [ t ]
  | Br (_, t, e, _) -> [ t; e ]
  | Switch (_, d, cases, _) -> d :: Array.to_list (Array.map snd cases)
  | Indirect_br _ | Asm_goto _ | Ret _ | Unreachable -> []

let fold_term f = function
  | Br (c, t, e, _) as term -> (
      match int_const f c with Some (_, x) -> Jump (if x <> 0L then t else e) | None -> term)
  | Switch (v, d, cases, _) as term -> (
      match int_const f v with
      | Some (w, x) -> (
          match Array.find_opt (fun ((k : int64 array), _) -> mask w k.(0) = x) cases with
          | Some (_, t) -> Jump t
          | None -> Jump d)
      | None -> term)
  | term -> term

let reachable f =
  let seen = Array.make (num_blocks f) false in
  let rec visit b =
    if not seen.(b) then begin
      seen.(b) <- true;
      List.iter visit (successors (block f b).term)
    end
  in
  visit entry;
  seen

(* Removes the block parameters that folding replaced (their definition is
   now an alias), and the matching arguments of every branch to them *)
let drop_params f =
  let keep =
    Array.init (num_blocks f) (fun b ->
        Array.map (fun p -> match def_of f p with Param _ -> true | _ -> false) (block f b).params)
  in
  if Array.exists (Array.exists not) keep then begin
    Vec.iteri
      (fun b (br : block_rec) ->
        if Array.exists not keep.(b) then begin
          let kept = List.filteri (fun k _ -> keep.(b).(k)) (Array.to_list br.params) in
          br.params <- Array.of_list kept;
          Array.iteri (fun k p -> Vec.set f.defs p (Param (b, k))) br.params
        end)
      f.blocks;
    let target (t : target) =
      if Array.exists not keep.(t.dest) then
        { t with args = Array.of_list (List.filteri (fun k _ -> keep.(t.dest).(k)) (Array.to_list t.args)) }
      else t
    in
    Vec.iter
      (fun (br : block_rec) ->
        br.term <-
          (match br.term with
          | Jump t -> Jump (target t)
          | Br (c, t, e, w) -> Br (c, target t, target e, w)
          | Switch (x, d, cases, w) -> Switch (x, target d, Array.map (fun (k, t) -> (k, target t)) cases, w)
          | t -> t))
      f.blocks
  end

(* Track ordinary nonescaped scalar slots along reachable control-flow
   edges. A load may be replaced only when all incoming writes agree. A
   memset of the whole slot to zero (-ftrivial-auto-var-init=zero on a local
   declared without an initializer) is a known zero store: loads it reaches
   become zero of their type. *)
let forward_constants f live =
  let slots = Hashtbl.create 16 in
  Vec.iter (fun i ->
    let ir = inst f i in
    match ir.op with
    | Alloca (n, align) ->
        (match int_value f n with
        | Some bytes when bytes > 0L ->
            Hashtbl.add slots ir.results.(0) (bytes, align, ref [], ref false)
        | _ -> ())
    | _ -> ()) (block f entry).insts;
  let invalidate v = Hashtbl.remove slots (resolve f v) in
  let access b align p ty store =
    match Hashtbl.find_opt slots p with
    | Some (bytes, natural, events, _) when align <= natural && Int64.of_int (size_of ty) <= bytes ->
        events := (b, store) :: !events
    | _ -> invalidate p
  in
  Vec.iteri (fun b (br : block_rec) ->
    if live.(b) then begin
      Vec.iter (fun i ->
        let ir = inst f i in
        match ir.op with
        | Store ({ volatile = false; order = Plain; align; _ }, v, p) ->
            let p = resolve f p and v = resolve f v in
            invalidate v;
            let known = int_const f v <> None ||
              (match const_of f v with Some (Cnull _ | Czero (Ptr _)) -> true | _ -> false)
            in
            if known then Option.iter (fun (_, _, _, found) -> found := true) (Hashtbl.find_opt slots p);
            access b align p (ty_of f v) (`Store (if known then Some (`Value v) else None))
        | Memset (false, p, byte, size)
          when (match Hashtbl.find_opt slots (resolve f p) with
                | Some (bytes, _, _, _) -> int_const f byte = Some (8, 0L) && int_value f size = Some bytes
                | None -> false) ->
            let _, _, events, found = Hashtbl.find slots (resolve f p) in
            found := true;
            events := (b, `Store (Some `Zero)) :: !events
        | Load ({ volatile = false; order = Plain; align; _ }, p) ->
            access b align (resolve f p) (ty_of f ir.results.(0)) (`Load ir)
        | op -> iter_op_uses invalidate op) br.insts;
      iter_term_uses invalidate br.term
    end) f.blocks;
  let changed = ref false in
  let n = num_blocks f in
  let edge_loads = Hashtbl.create 8 in
  let predecessors = lazy (
    let picks = Array.init n (fun b ->
      match (block f b).term with Br _ -> b <> entry && Vec.length (block f b).insts <= 24 | _ -> false)
    in
    let preds = Array.make n [] in
    Vec.iteri (fun b (br : block_rec) ->
      if live.(b) then
        List.iter (fun (t : target) ->
          if picks.(t.dest) && t.dest <> b then preds.(t.dest) <- b :: preds.(t.dest)) (targets br.term))
      f.blocks;
    preds)
  in
  let as_constant ty = function
    | Some (`Value v) when ty_of f v = ty -> Some v
    | Some `Zero when (match ty with Int w -> w <= 64 | Ptr _ -> true | _ -> false) -> Some (zero f ty)
    | _ -> None
  in
  Hashtbl.iter (fun _ (_, _, events, found) ->
    if !found then begin
      let stores = Hashtbl.create 8 in
      List.iter (function
        | b, `Store v when not (Hashtbl.mem stores b) -> Hashtbl.add stores b v
        | _ -> ()) !events;
      let incoming = Array.make n None in
      let outgoing b =
        match Hashtbl.find_opt stores b with
        | Some last -> last
        | None -> Option.get incoming.(b)
      in
      let pending = Queue.create () in
      incoming.(entry) <- Some None;
      Queue.add entry pending;
      while not (Queue.is_empty pending) do
        let b = Queue.take pending in
        let outgoing = outgoing b in
        List.iter (fun dest ->
          let combined =
            match incoming.(dest) with
            | None -> outgoing
            | Some old when old = outgoing -> old
            | Some _ -> None
          in
          if incoming.(dest) <> Some combined then begin
            incoming.(dest) <- Some combined;
            Queue.add dest pending
          end) (successors (block f b).term)
      done;
      let state = ref None and active = ref (-1) in
      List.iter (fun (b, event) ->
        if b <> !active then begin
          active := b;
          state := Option.get incoming.(b)
        end;
        match event with
        | `Store v -> state := v
        | `Load ir ->
            let v = ir.results.(0) in
            let ty = ty_of f v in
            (match as_constant ty !state with
            | Some c ->
                replace_all_uses f v c;
                ir.op <- Nop;
                changed := true
            | None when not (Hashtbl.mem stores b) ->
                List.iter (fun pred ->
                  Option.iter (fun c -> Hashtbl.add edge_loads (pred, b) (v, c))
                    (as_constant ty (outgoing pred))) (Lazy.force predecessors).(b)
            | _ -> ())) (List.rev !events)
    end) slots;
  !changed, edge_loads

(* A private load can have a different constant on each incoming edge.
   Bypass a short conditional block on edges where we can evaluate all its
   instructions and resolve the branch. Values defined there must either
   stay in the block or leave it as explicit successor arguments. *)
let thread_stack_branches f live edge_loads =
  if Hashtbl.length edge_loads = 0 then false else
  let n = num_blocks f in
  let owners = Array.make (num_values f) (-1) in
  let define b v = if resolve f v = v then owners.(v) <- b in
  Vec.iteri (fun b (br : block_rec) ->
    Array.iter (define b) br.params;
    Vec.iter (fun i -> Array.iter (define b) (inst f i).results) br.insts) f.blocks;
  let used_outside = Array.make n false in
  Vec.iteri (fun b (br : block_rec) ->
    if live.(b) then begin
      let use v =
        let owner = owners.(resolve f v) in
        if owner >= 0 && owner <> b then used_outside.(owner) <- true
      in
      Vec.iter (fun i -> iter_op_uses use (inst f i).op) br.insts;
      iter_term_uses use br.term
    end) f.blocks;
  let thread pred t =
    let b = t.dest in
    let br = block f b in
    match br.term, Hashtbl.find_all edge_loads (pred, b) with
    | Br (c, yes, no, _), (_ :: _ as facts)
      when not used_outside.(b) && Array.length br.params = Array.length t.args ->
        let values = Hashtbl.create 8 in
        let get v =
          let v = resolve f v in
          match Hashtbl.find_opt values v with
          | Some v -> v
          | None when owners.(v) <> b -> v
          | None -> raise Exit
        in
        Array.iteri (fun k p -> if resolve f p = p then Hashtbl.add values p (resolve f t.args.(k))) br.params;
        (try
           Vec.iter (fun i ->
             let ir = inst f i in
             match ir.op with
             | Nop -> ()
             | _ ->
                 let value =
                   match ir.op with
                   | Load _ -> List.assoc_opt ir.results.(0) facts
                   | op -> evaluate f { ir with op = map_op_uses get op }
                 in
                 match value with
                 | Some v -> Hashtbl.add values ir.results.(0) v
                 | None -> raise Exit) br.insts;
           match int_const f (get c) with
           | Some (_, bits) ->
               let dst = if bits = 0L then no else yes in
               if dst.dest = b then t else { dst with args = Array.map get dst.args }
           | None -> t
         with Exit -> t)
    | _ -> t
  in
  let edits = ref [] in
  Vec.iteri (fun b (br : block_rec) ->
    if live.(b) then begin
      let edge = thread b in
      let term =
        match br.term with
        | Jump t -> Jump (edge t)
        | Br (c, yes, no, w) -> Br (c, edge yes, edge no, w)
        | Switch (v, d, cases, w) ->
            Switch (v, edge d, Array.map (fun (k, t) -> k, edge t) cases, w)
        | term -> term
      in
      if term <> br.term then edits := (br, term) :: !edits
    end) f.blocks;
  List.iter (fun (br, term) -> br.term <- term) !edits;
  !edits <> []

(* Narrow a switch on a private stack field using an earlier equality
   guard on that field. Stores to disjoint fields preserve the fact. A
   pointer that escapes, or is indexed at an unknown offset, makes the
   whole allocation ineligible. *)
let narrow_stack_switches f live =
  let switches = ref [] in
  Vec.iteri (fun b (br : block_rec) ->
    match br.term with
    | Switch (v, _, _, _) when live.(b) && int_const f v = None ->
        switches := (b, v) :: !switches
    | _ -> ()) f.blocks;
  if !switches = [] then false else
  let allocas = Hashtbl.create 16 and addresses = Hashtbl.create 16 in
  Vec.iter (fun i ->
    let ir = inst f i in
    match ir.op with
    | Alloca (n, _) ->
        (match int_const f n with
        | Some (_, n) when n > 0L ->
            let v = ir.results.(0) in
            Hashtbl.add allocas v n;
            Hashtbl.add addresses v (Some (v, 0L))
        | _ -> ())
    | _ -> ()) (block f entry).insts;
  let rec address v =
    let v = resolve f v in
    if not (is_ptr (ty_of f v)) then None else
    match Hashtbl.find_opt addresses v with
    | Some p -> p
    | None ->
        let p =
          match def_of f v with
          | Result (i, _) ->
              (match (inst f i).op with
              | Ptradd (_, base, idx, scale, disp) ->
                  (match address base, int_const f idx with
                  | Some (a, off), Some (w, x) ->
                      Some (a, Int64.(add off (add (mul (sext w x) (of_int scale)) (of_int disp))))
                  | _ -> None)
              | _ -> None)
          | _ -> None
        in
        Hashtbl.add addresses v p;
        p
  in
  let exposed = Hashtbl.create 8 in
  let escape v = Option.iter (fun (a, _) -> Hashtbl.replace exposed a ()) (address v) in
  let region ptr size =
    match address ptr with
    | Some (a, off) ->
        let bytes = Hashtbl.find allocas a in
        if off >= 0L && off <= bytes && size >= 0L && size <= Int64.sub bytes off then
          Some (a, off, size)
        else (escape ptr; None)
    | None -> None
  in
  let loads = Hashtbl.create 16 and writes = Array.make (num_blocks f) [] in
  let write b at ptr size v =
    match region ptr size with
    | Some loc when size > 0L -> writes.(b) <- (at, loc, v) :: writes.(b)
    | _ -> ()
  in
  Vec.iteri (fun b (br : block_rec) ->
    if live.(b) then begin
      Vec.iteri (fun at i ->
        let ir = inst f i in
        match ir.op with
        | Ptradd (_, p, idx, _, _) ->
            if int_const f idx = None then escape p;
            escape idx
        | Load ({ volatile = false; order = Plain; _ }, p) ->
            let v = ir.results.(0) in
            (match ty_of f v with
            | Int w when w <= 64 ->
                let n = Int64.of_int (size_of (ty_of f v)) in
                Option.iter (fun loc -> Hashtbl.add loads v (loc, w, b, at)) (region p n)
            | _ -> ())
        | Store ({ volatile = false; order = Plain; _ }, v, p) ->
            escape v;
            write b at p (Int64.of_int (size_of (ty_of f v)))
              (Option.map (fun v -> `Scalar v) (int_const f v))
        | Memset (false, p, byte, size) ->
            (match int_const f byte, int_const f size with
            | Some (8, 0L), Some (_, n) -> write b at p n (Some `Zero)
            | _ -> escape p)
        | op -> iter_op_uses escape op) br.insts;
      iter_term_uses escape br.term
    end) f.blocks;
  let overlap (a, off, n) (b, start, size) =
    a = b && off < Int64.add start size && start < Int64.add off n
  in
  (* The load must still describe the field at the edge being refined. *)
  let read b v =
    match Hashtbl.find_opt loads (resolve f v) with
    | Some (((a, _, _) as loc), w, from, at) when b = from && not (Hashtbl.mem exposed a) &&
        not (List.exists (fun (pos, dst, _) -> pos > at && overlap loc dst) writes.(b)) ->
        Some (loc, w)
    | _ -> None
  in
  let rec comparison b v =
    match def_of f (resolve f v) with
    | Result (i, _) ->
        (match (inst f i).op with
        | Conv (Zext, x) when ty_of f x = i1 -> comparison b x
        | Icmp ((Eq | Ne as pred), x, y) ->
            let check x y =
              match int_const f y with
              | Some (w, k) ->
                  (match read b x with
                  | Some (loc, width) when w = width -> Some (loc, w, k, pred = Eq)
                  | _ when k = 0L ->
                      Option.map (fun (loc, w, k, equal) ->
                        (loc, w, k, if pred = Eq then not equal else equal)) (comparison b x)
                  | _ -> None)
              | _ -> None
            in
            (match check x y with Some _ as c -> c | None -> check y x)
        | _ -> None)
    | _ -> None
  in
  let guards = Array.init (num_blocks f) (fun b ->
    match (block f b).term with
    | Br (test, _, _, _) when live.(b) -> comparison b test
    | _ -> None)
  in
  let cache = Hashtbl.create 8 in
  let solve loc width key =
    (* Bit 0 means equality is possible, bit 1 means inequality is possible.
       At joins, all incoming possibilities are retained. *)
    let state = Array.make (num_blocks f) 0 and exit = Array.make (num_blocks f) 0 in
    let transfer = Array.map (fun ws ->
      match List.find_opt (fun (_, dst, _) -> overlap loc dst) ws with
      | Some (_, (_, start, size), Some `Zero)
        when (let _, off, n = loc in start <= off && Int64.add off n <= Int64.add start size) ->
          Some (if key = 0L then 1 else 2)
      | Some (_, dst, Some (`Scalar (w, k))) when dst = loc && w = width ->
          Some (if k = key then 1 else 2)
      | Some _ -> Some 3
      | None -> None) writes
    in
    let pending = Queue.create () in
    let send dst possible =
      let total = state.(dst) lor possible in
      if total <> state.(dst) then begin
        state.(dst) <- total;
        Queue.add dst pending
      end
    in
    send entry 3;
    while not (Queue.is_empty pending) do
      let b = Queue.take pending in
      let out = Option.value transfer.(b) ~default:state.(b) in
      exit.(b) <- out;
      match guards.(b), (block f b).term with
      | Some (cell, w, k, equal), Br (_, yes, no, _) when cell = loc && w = width && k = key ->
          let on_true, on_false = if equal then 1, 2 else 2, 1 in
          send yes.dest (out land on_true);
          send no.dest (out land on_false)
      | _ -> List.iter (fun dst -> send dst out) (successors (block f b).term)
    done;
    exit
  in
  let possible b loc w k =
    let fact = (loc, w, k) in
    let states =
      match Hashtbl.find_opt cache fact with
      | Some result -> result
      | None ->
          let result =
            if Array.exists (function Some (cell, width, key, _) ->
              cell = loc && width = w && key = k | None -> false) guards
            then Some (solve loc w k)
            else None
          in
          Hashtbl.add cache fact result;
          result
    in
    match states with Some state -> state.(b) | None -> 3
  in
  let changed = ref false in
  List.iter (fun (b, v) ->
    match read b v, (block f b).term with
    | Some (loc, w), Switch (_, fallback, cases, weights) ->
        let test (bits, _) = possible b loc w (mask w bits.(0)) in
        let new_term =
          match Array.find_opt (fun case -> test case = 1) cases with
          | Some (_, dst) -> Jump dst
          | None ->
              let kept = Array.of_list (List.filter (fun case -> test case <> 2) (Array.to_list cases)) in
              if Array.length kept = Array.length cases then Switch (v, fallback, cases, weights)
              else if Array.length kept = 0 then Jump fallback
              else Switch (v, fallback, kept, [||])
        in
        if new_term <> (block f b).term then begin
          (block f b).term <- new_term;
          changed := true
        end
    | _ -> ()) !switches;
  !changed

let fold_func ?(forward = false) f =
  let changed = ref true and rounds = ref 0 in
  while !changed && !rounds < 16 do
    changed := false;
    incr rounds;
    let live = reachable f in
    if forward then begin
      let forwarded, edge_loads = forward_constants f live in
      if thread_stack_branches f live edge_loads || forwarded then changed := true
    end;
    (* instructions *)
    Vec.iter
      (fun (br : block_rec) ->
        Vec.iter
          (fun i ->
            let ir = inst f i in
            match evaluate f ir with
            | Some v when resolve f v <> ir.results.(0) ->
                replace_all_uses f ir.results.(0) v;
                ir.op <- Nop;
                changed := true
            | _ -> ())
          br.insts)
      f.blocks;
    (* block parameters that receive one constant, or have one incoming edge *)
    let incoming = Array.make (num_blocks f) [] in
    for b = 0 to num_blocks f - 1 do
      if live.(b) then List.iter (fun (t : target) -> incoming.(t.dest) <- t.args :: incoming.(t.dest)) (targets (block f b).term);
      match (block f b).term with
      | Indirect_br (_, bs) -> if live.(b) then Array.iter (fun d -> incoming.(d) <- [||] :: incoming.(d)) bs
      | Asm_goto { fall; labels; _ } ->
          if live.(b) then Array.iter (fun d -> incoming.(d) <- [||] :: incoming.(d)) (Array.append [| fall |] labels)
      | _ -> ()
    done;
    for b = 0 to num_blocks f - 1 do
      if b <> entry && live.(b) then
        Array.iteri
          (fun k p ->
            if resolve f p = p then
              let args = List.map (fun a -> if k < Array.length a then Some (resolve f a.(k)) else None) incoming.(b) in
              let replacement =
                match args with
                | [ Some v ] -> Some v
                | Some v :: rest when const_of f v <> None && List.for_all (( = ) (Some v)) rest -> Some v
                | _ -> None
              in
              match replacement with
              | Some v when v <> p ->
                  replace_all_uses f p v;
                  changed := true
              | _ -> ())
          (block f b).params
    done;
    if forward && narrow_stack_switches f live then changed := true;
    (* terminators, then the blocks that became unreachable *)
    Vec.iter
      (fun (br : block_rec) ->
        let t = fold_term f (map_term_uses (resolve f) br.term) in
        if t <> br.term then begin
          (match (br.term, t) with Jump _, Jump _ -> () | _ -> changed := true);
          br.term <- t
        end)
      f.blocks;
    let live = reachable f in
    Vec.iteri
      (fun b (br : block_rec) ->
        if (not live.(b)) && (Vec.length br.insts > 0 || br.term <> Unreachable) then begin
          br.insts <- Vec.create 0;
          br.term <- Unreachable;
          changed := true
        end)
      f.blocks
  done;
  resolve_all f;
  drop_params f;
  drop_nops f

(* Folding empties the blocks it disconnects; they are then removed, since
   SPEC §4 requires every block to be reachable. Fas never takes block
   addresses, but a function whose blocks are pinned by a [blockaddr] is left
   alone all the same. *)
let run (m : modul) =
  Vec.iter
    (fun f ->
      if f.defined && not (blocks_pinned m f) then begin
        fold_func f;
        ignore (remove_unreachable_blocks f)
      end)
    m.funcs
