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
  Hashtbl.iter (fun _ (_, _, events, found) ->
    if !found then begin
      let stores = Hashtbl.create 8 in
      List.iter (function
        | b, `Store v when not (Hashtbl.mem stores b) -> Hashtbl.add stores b v
        | _ -> ()) !events;
      let incoming = Array.make n None in
      let pending = Queue.create () in
      incoming.(entry) <- Some None;
      Queue.add entry pending;
      while not (Queue.is_empty pending) do
        let b = Queue.take pending in
        let outgoing =
          match Hashtbl.find_opt stores b with
          | Some last -> last
          | None -> Option.get incoming.(b)
        in
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
            let ty = ty_of f ir.results.(0) in
            (match !state with
            | Some (`Value v) when ty_of f v = ty ->
                replace_all_uses f ir.results.(0) v;
                ir.op <- Nop;
                changed := true
            | Some `Zero when (match ty with Int w -> w <= 64 | Ptr _ -> true | _ -> false) ->
                replace_all_uses f ir.results.(0) (zero f ty);
                ir.op <- Nop;
                changed := true
            | _ -> ())) (List.rev !events)
    end) slots;
  !changed

let fold_func ?(forward = false) f =
  let changed = ref true and rounds = ref 0 in
  while !changed && !rounds < 16 do
    changed := false;
    incr rounds;
    let live = reachable f in
    if forward && forward_constants f live then changed := true;
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
