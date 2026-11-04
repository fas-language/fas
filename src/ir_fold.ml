(* Constant folding over lowered IR, run on every build.

   Lowering emits const-generic and `for const` arithmetic as instructions on
   constants (`udiv i64 -1, 3`, `and i8 0, 7`, `zext i8 0 to i64`), with a trap
   check on every constant divisor. This pass folds integer arithmetic,
   comparisons, casts, selects and uniform phis whose operands are constants,
   then folds branches and switches on constants and drops the blocks that
   become unreachable. It never folds an operation whose result would be a trap
   or poison (division by zero, `MIN / -1`, out-of-range shift counts); those
   stay for the runtime check that guards them. *)

open Ir

let width = function
  | I1 -> Some 1
  | I8 -> Some 8
  | I16 -> Some 16
  | I32 -> Some 32
  | I64 -> Some 64
  | _ -> None

let norm w v =
  if w >= 64 then v else Int64.logand v (Int64.pred (Int64.shift_left 1L w))

let sext w v =
  if w >= 64 then v else Int64.shift_right (Int64.shift_left v (64 - w)) (64 - w)

let const ty v =
  match width ty with Some w -> Some (Const (ty, norm w v)) | None -> None

let int_const = function
  | Const (ty, v) -> (
      match width ty with Some w -> Some (w, norm w v) | None -> None)
  | _ -> None

let fold_bin op ty a b =
  match (width ty, int_const a, int_const b) with
  | Some w, Some (_, x), Some (_, y) -> (
      let in_range = Int64.compare y 0L >= 0 && Int64.compare y (Int64.of_int w) < 0 in
      let minimum = sext w (Int64.shift_left 1L (w - 1)) in
      let signed_ok = y <> 0L && not (sext w x = minimum && sext w y = -1L) in
      match op with
      | Add -> const ty (Int64.add x y)
      | Sub -> const ty (Int64.sub x y)
      | Mul -> const ty (Int64.mul x y)
      | And -> const ty (Int64.logand x y)
      | Or -> const ty (Int64.logor x y)
      | Xor -> const ty (Int64.logxor x y)
      | Shl when in_range -> const ty (Int64.shift_left x (Int64.to_int y))
      | Lshr when in_range -> const ty (Int64.shift_right_logical x (Int64.to_int y))
      | Ashr when in_range -> const ty (Int64.shift_right (sext w x) (Int64.to_int y))
      | Udiv when y <> 0L -> const ty (Int64.unsigned_div x y)
      | Urem when y <> 0L -> const ty (Int64.unsigned_rem x y)
      | Sdiv when signed_ok -> const ty (Int64.div (sext w x) (sext w y))
      | Srem when signed_ok -> const ty (Int64.rem (sext w x) (sext w y))
      | _ -> None)
  | _ -> None

let fold_cmp cmp a b =
  match (int_const a, int_const b) with
  | Some (w, x), Some (_, y) ->
      let s = compare (sext w x) (sext w y) and u = Int64.unsigned_compare x y in
      let r =
        match cmp with
        | Eq -> Some (x = y)
        | Ne -> Some (x <> y)
        | Slt -> Some (s < 0)
        | Sle -> Some (s <= 0)
        | Sgt -> Some (s > 0)
        | Sge -> Some (s >= 0)
        | Ult -> Some (u < 0)
        | Ule -> Some (u <= 0)
        | Ugt -> Some (u > 0)
        | Uge -> Some (u >= 0)
        | _ -> None
      in
      Option.map (fun r -> Const (I1, if r then 1L else 0L)) r
  | _ -> None

let fold_cast kind from v target =
  match (int_const v, width from, width target) with
  | Some (_, x), Some fw, Some tw -> (
      match kind with
      | "zext" | "trunc" -> const target x
      | "sext" -> const target (sext fw x)
      | "bitcast" when fw = tw -> const target x
      | _ -> None)
  | _ -> None

let map_instr f = function
  | Bin (i, op, t, a, b) -> Bin (i, op, t, f a, f b)
  | Fneg (i, t, a) -> Fneg (i, t, f a)
  | Cmp (i, c, t, a, b) -> Cmp (i, c, t, f a, f b)
  | Alloca _ as x -> x
  | Load (i, t, p, a) -> Load (i, t, f p, a)
  | Load_volatile (i, t, p, a) -> Load_volatile (i, t, f p, a)
  | Store (t, v, p, a) -> Store (t, f v, f p, a)
  | Store_volatile (t, v, p, a) -> Store_volatile (t, f v, f p, a)
  | Gep (i, t, p, idx) ->
      Gep (i, t, f p, List.map (function Zero -> Zero | Index v -> Index (f v)) idx)
  | Cast (i, k, ft, v, tt) -> Cast (i, k, ft, f v, tt)
  | Call (i, e, t, n, args) ->
      Call (i, e, t, n, List.map (fun (t, e, v) -> (t, e, f v)) args)
  | Call_indirect (i, e, t, c, args) ->
      Call_indirect (i, e, t, f c, List.map (fun (t, e, v) -> (t, e, f v)) args)
  | Phi (i, t, ins) -> Phi (i, t, List.map (fun (v, b) -> (f v, b)) ins)
  | Select (i, c, a, b) -> Select (i, f c, f a, f b)
  | Extract (i, t, v, l) -> Extract (i, t, f v, f l)
  | Insert (i, t, v, l, x) -> Insert (i, t, f v, f l, f x)
  | Shuffle_zero (i, t, v) -> Shuffle_zero (i, t, f v)
  | Shufflevector (i, t, a, b, m) -> Shufflevector (i, t, f a, f b, f m)
  | Syscall (i, args) -> Syscall (i, List.map f args)
  | (String_ptr _ | Global_ptr _ | Trap | Location _) as x -> x

let map_term f = function
  | Ret (Some (t, v)) -> Ret (Some (t, f v))
  | CondBr (c, a, b) -> CondBr (f c, a, b)
  | Switch (t, v, cases, d) -> Switch (t, f v, cases, d)
  | (Ret None | Br _ | Unreachable) as x -> x

(* Algebraic identities with one constant operand: `x + 0`, `x * 1`,
   `x & -1`, `x << 0` and so on are `x`; `x * 0` and `x & 0` are `0`. *)
let identity op ty a b =
  match width ty with
  | None -> None
  | Some w -> (
      let ones = norm w (-1L) in
      let is k v =
        match int_const v with Some (_, x) -> x = norm w k | None -> false
      in
      let all_ones v =
        match int_const v with Some (_, x) -> x = ones | None -> false
      in
      match op with
      | (Add | Or | Xor) when is 0L b -> Some a
      | (Add | Or | Xor) when is 0L a -> Some b
      | (Sub | Shl | Lshr | Ashr) when is 0L b -> Some a
      | Mul when is 1L b -> Some a
      | Mul when is 1L a -> Some b
      | (Mul | And) when is 0L b || is 0L a -> const ty 0L
      | And when all_ones b -> Some a
      | And when all_ones a -> Some b
      | (Udiv | Sdiv) when is 1L b -> Some a
      | _ -> None)

(* The folded value of an instruction, if all it needs is constants. *)
let evaluate = function
  | Bin (i, op, t, a, b) -> (
      match fold_bin op t a b with
      | Some v -> Some (i, v)
      | None -> Option.map (fun v -> (i, v)) (identity op t a b))
  | Cmp (i, c, _, a, b) -> Option.map (fun v -> (i, v)) (fold_cmp c a b)
  | Cast (i, k, ft, v, tt) -> Option.map (fun v -> (i, v)) (fold_cast k ft v tt)
  | Select (i, Const (I1, c), a, b) -> Some (i, if c <> 0L then a else b)
  | Phi (i, _, ((Const _ as v), _) :: rest) when List.for_all (fun (w, _) -> w = v) rest
    ->
      Some (i, v)
  (* A single predecessor's value dominates the phi's block. *)
  | Phi (i, _, [ (v, _) ]) -> Some (i, v)
  | _ -> None

let fold_term = function
  | CondBr (Const (I1, c), a, b) -> Br (if c <> 0L then a else b)
  | Switch (t, v, cases, default) as term -> (
      match int_const v with
      | Some (w, x) -> (
          ignore t;
          match List.find_opt (fun (k, _) -> norm w k = x) cases with
          | Some (_, target) -> Br target
          | None -> Br default)
      | None -> term)
  | term -> term

let fold_function (func : func) =
  let env = Hashtbl.create 64 in
  let rec resolve = function
    | Local (i, _) as v -> (
        match Hashtbl.find_opt env i with Some w -> resolve w | None -> v)
    | v -> v
  in
  let blocks = ref func.blocks in
  let changed = ref true in
  let rounds = ref 0 in
  while !changed && !rounds < 16 do
    changed := false;
    incr rounds;
    (* Values *)
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match evaluate (map_instr resolve ins) with
            | Some (i, v) when not (Hashtbl.mem env i) ->
                Hashtbl.replace env i v;
                changed := true
            | _ -> ())
          b.instrs)
      !blocks;
    (* Rewrite operands and drop folded definitions *)
    blocks :=
      List.map
        (fun (b : block) ->
          let instrs =
            List.filter_map
              (fun ins ->
                match instruction_result ins with
                | Some (i, _) when Hashtbl.mem env i -> None
                | _ -> Some (map_instr resolve ins))
              b.instrs
          in
          let terminator = fold_term (map_term resolve b.terminator) in
          if terminator <> b.terminator then changed := true;
          { b with instrs; terminator })
        !blocks;
    (* Drop unreachable blocks and the phi entries for edges that are gone *)
    match !blocks with
    | [] -> ()
    | entry :: _ ->
        let by_id = Hashtbl.create 64 in
        List.iter (fun (b : block) -> Hashtbl.replace by_id b.id b) !blocks;
        let reachable = Hashtbl.create 64 in
        let rec visit id =
          if not (Hashtbl.mem reachable id) then (
            Hashtbl.replace reachable id ();
            match Hashtbl.find_opt by_id id with
            | Some b -> List.iter visit (terminator_successors b.terminator)
            | None -> ())
        in
        visit entry.id;
        let live =
          List.filter (fun (b : block) -> Hashtbl.mem reachable b.id) !blocks
        in
        if List.length live <> List.length !blocks then changed := true;
        (* Edge multiplicity pred -> succ among live blocks *)
        let edges = Hashtbl.create 64 in
        List.iter
          (fun (b : block) ->
            List.iter
              (fun s ->
                let k = (b.id, s) in
                Hashtbl.replace edges k
                  (1 + Option.value ~default:0 (Hashtbl.find_opt edges k)))
              (terminator_successors b.terminator))
          live;
        blocks :=
          List.map
            (fun (b : block) ->
              let instrs =
                List.map
                  (function
                    | Phi (i, t, ins) ->
                        let seen = Hashtbl.create 4 in
                        let ins' =
                          List.filter
                            (fun (_, p) ->
                              let allowed =
                                Option.value ~default:0
                                  (Hashtbl.find_opt edges (p, b.id))
                              in
                              let used =
                                Option.value ~default:0 (Hashtbl.find_opt seen p)
                              in
                              if used < allowed then (
                                Hashtbl.replace seen p (used + 1);
                                true)
                              else false)
                            ins
                        in
                        if List.length ins' <> List.length ins then changed := true;
                        Phi (i, t, ins')
                    | ins -> ins)
                  b.instrs
              in
              { b with instrs })
            live
  done;
  { func with blocks = !blocks }

let module_ (m : module_) = { m with funcs = List.map fold_function m.funcs }
