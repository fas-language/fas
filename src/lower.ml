type block_state = {
  id : int;
  instrs : Ir.instr Queue.t;
  term : Ir.terminator option ref;
}

type loop = {
  label : string option;
  break_to : int;
  continue_to : int;
  keep_scopes : int;
}

type local_slot = { pointer : Ir.value; size : int }

type state = {
  literal_globals : Hir.global list ref;
  mutable next_value : int;
  mutable next_block : int;
  blocks : block_state Queue.t;
  entry : block_state;
  mutable current : block_state;
  env : (int, Ir.value * int) Hashtbl.t;
  mutable env_scopes : (int * (Ir.value * int) option) list list;
  ret : Ir.ty;
  structs : Hir.struct_def list;
  strings : string list;
  functions : Hir.func list;
  addressed_functions : (string, unit) Hashtbl.t;
  mutable defer_scopes : Hir.stmt list list list;
  mutable scope_slots : local_slot list list;
  address_taken : (int, unit) Hashtbl.t;
  sanitize_address : bool;
  intrinsic_masked_loads : bool;
  mutable loops : loop list;
  file : string option;
  mutable location : (int * int) option;
}

let error span msg = Error [ Diag.error span msg ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let layout_ok structs t =
  match Hir.layout structs t with
  | Ok _ -> Ok ()
  | Error m ->
      error Span.synthetic
        (Printf.sprintf "internal error: type `%s` has no layout: %s" (Hir.ty_name t) m)

let fresh s =
  let n = s.next_value in
  s.next_value <- n + 1;
  n

let fresh_block s =
  let n = s.next_block in
  s.next_block <- n + 1;
  let b = { id = n; instrs = Queue.create (); term = ref None } in
  Queue.add b s.blocks;
  b

let emit s i = Queue.add i s.current.instrs
let emit_entry s i = Queue.add i s.entry.instrs

let mark s (span : Span.t) =
  if (not (Span.is_synthetic span)) && Some span.Span.file = s.file then (
    s.location <- Some (span.Span.line, span.Span.column);
    emit s (Ir.Location (span.Span.line, span.Span.column)))

let enter s block =
  s.current <- block;
  Option.iter
    (fun (line, column) -> Queue.add (Ir.Location (line, column)) block.instrs)
    s.location

let open_block s = !(s.current.term) = None

let value_ty = function
  | Ir.Const (t, _)
  | Const_vector (t, _)
  | Null t
  | Undef t
  | Zero t
  | Local (_, t)
  | Param (_, t)
  | Global (_, t) ->
      t

let rec ty = function
  | Hir.Bool -> Ir.I1
  | Hir.Float Hir.F32 -> Ir.F32
  | Hir.Float Hir.F64 -> Ir.F64
  | Hir.Int Hir.U8 | Hir.Int Hir.I8 -> I8
  | Hir.Int Hir.U16 | Hir.Int Hir.I16 -> I16
  | Hir.Int Hir.U32 | Hir.Int Hir.I32 -> I32
  | Hir.Int Hir.U64 | Hir.Int Hir.I64 -> I64
  | Hir.Int Hir.Usize | Hir.Int Hir.Isize -> (
      match Target_layout.pointer_integer_bits (Target_layout.current ()) with
      | Ok 32 -> I32
      | Ok 64 -> I64
      | Ok bits -> invalid_arg (Printf.sprintf "unsupported pointer width: %d" bits)
      | Error message -> invalid_arg message)
  | Hir.Addr | Hir.Handle _ -> Ir.Pointer Ir.I8
  | Hir.Vec (n, t) -> Ir.Vector (n, ty t)
  | Hir.Array (n, t) -> Ir.Array (n, storage_ty t)
  | Hir.Struct n -> Ir.Struct n
  | Hir.Opaque n -> Ir.Struct n
  | Hir.Void -> Ir.Void

and storage_ty = function
  | Hir.Bool -> Ir.I8
  | Hir.Array (n, t) -> Ir.Array (n, storage_ty t)
  | Hir.Struct n -> Ir.Struct n
  | Hir.Opaque n -> Ir.Struct n
  | t -> ty t

let ir_extension = function
  | Target_layout.C_no_extension -> Ir.No_extension
  | Target_layout.C_sign_extension -> Ir.Sign_extension
  | Target_layout.C_zero_extension -> Ir.Zero_extension

let c_extension = function
  | Hir.Bool -> ir_extension (Target_layout.current ()).c_abi_bool_extension
  | Hir.Int kind ->
      Target_layout.c_integer_extension (Target_layout.current ())
        ~signed:
          (match kind with Hir.I8 | I16 | I32 | I64 | Isize -> true | _ -> false)
        (Hir.int_bytes kind * 8)
      |> ir_extension
  | _ -> Ir.No_extension

let has_c_abi (f : Hir.func) = f.linkage = Hir.External_c

let align s t =
  match Hir.layout s.structs t with
  | Ok (_, a) -> Ok a
  | Error m ->
      error Span.synthetic
        (Printf.sprintf "internal error: type `%s` has no layout: %s" (Hir.ty_name t) m)

let offset_alignment alignment offset =
  let rec reduce alignment =
    if alignment <= 1 || offset mod alignment = 0 then alignment
    else reduce (alignment / 2)
  in
  reduce alignment

let load_value s value_ty pointer alignment =
  match value_ty with
  | Hir.Bool ->
      let byte_pointer =
        match Ir.value_ty pointer with
        | Ir.Pointer Ir.I8 -> pointer
        | pointer_ty ->
            let id = fresh s in
            emit s (Ir.Cast (id, "bitcast", pointer_ty, pointer, Ir.Pointer Ir.I8));
            Ir.Local (id, Ir.Pointer Ir.I8)
      in
      let loaded = fresh s in
      emit s (Ir.Load (loaded, Ir.I8, byte_pointer, alignment));
      let normalized = fresh s in
      emit s
        (Ir.Cmp
           (normalized, Ir.Ne, Ir.I8, Ir.Local (loaded, Ir.I8), Ir.Const (Ir.I8, 0L)));
      Ok (Ir.Local (normalized, Ir.I1))
  | _ ->
      let loaded = fresh s in
      emit s (Ir.Load (loaded, ty value_ty, pointer, alignment));
      Ok (Ir.Local (loaded, ty value_ty))

let rec place_alignment s expression =
  match expression with
  | Hir.Local (local, span) -> (
      match Hashtbl.find_opt s.env local.id with
      | Some (_, alignment) -> Ok alignment
      | None -> error span ("unknown local `" ^ local.name ^ "`"))
  | Hir.Raw_select _ -> Ok 1
  | Hir.Field (base, _, field_ty, offset, _) ->
      let* base_alignment = place_alignment s base in
      let* field_alignment = align s field_ty in
      Ok (min (offset_alignment base_alignment offset) field_alignment)
  | Hir.Index (base, _, element_ty, _) ->
      let* base_alignment = place_alignment s base in
      let* element_alignment = align s element_ty in
      Ok (min base_alignment element_alignment)
  | Hir.Global (_, global_ty, _) | Hir.Const_array (_, global_ty, _) ->
      align s global_ty
  | _ -> align s (Hir.expr_ty expression)

let target_alignment s target target_ty =
  match target with
  | Hir.ALocal local -> (
      match Hashtbl.find_opt s.env local.id with
      | Some (_, alignment) -> Ok alignment
      | None -> error Span.synthetic ("unknown local `" ^ local.name ^ "`"))
  | Hir.AGlobal (_, global_ty) -> align s global_ty
  | Hir.ARaw _ -> Ok 1
  | Hir.AIndex (base, _) -> (
      match Hir.expr_ty base with
      | Hir.Array _ ->
          let* base_alignment = place_alignment s base in
          let* element_alignment = align s target_ty in
          Ok (min base_alignment element_alignment)
      | _ -> align s target_ty)
  | Hir.AField (base, _, offset) ->
      let* base_alignment = place_alignment s base in
      let* field_alignment = align s target_ty in
      Ok (min (offset_alignment base_alignment offset) field_alignment)

let zero t = Ir.Const (t, 0L)

let ones span = function
  | Ir.I1 -> Ok 1L
  | I8 -> Ok 0xffL
  | I16 -> Ok 0xffffL
  | I32 -> Ok 0xffff_ffffL
  | I64 -> Ok Int64.minus_one
  | _ -> error span "internal error: bitwise complement has a non-integer type"

let unsigned = function
  | Hir.Int (Hir.U8 | U16 | U32 | U64 | Usize) -> true
  | Hir.Vec (_, Hir.Int (Hir.U8 | U16 | U32 | U64 | Usize)) -> true
  | _ -> false

let is_float_type = function
  | Hir.Float _ | Hir.Vec (_, Hir.Float _) -> true
  | _ -> false

let cmp_for span t = function
  | Ast.Eq when is_float_type t -> Ok Ir.Foeq
  | Ne when is_float_type t -> Ok Ir.Fune
  | Lt when is_float_type t -> Ok Ir.Folt
  | Le when is_float_type t -> Ok Ir.Fole
  | Gt when is_float_type t -> Ok Ir.Fogt
  | Ge when is_float_type t -> Ok Ir.Foge
  | Ast.Eq -> Ok Ir.Eq
  | Ne -> Ok Ne
  | Lt -> Ok (if unsigned t then Ult else Slt)
  | Le -> Ok (if unsigned t then Ule else Sle)
  | Gt -> Ok (if unsigned t then Ugt else Sgt)
  | Ge -> Ok (if unsigned t then Uge else Sge)
  | _ ->
      error span "internal error: non-comparison operator reached comparison lowering"

let bin_for span t = function
  | Ast.Add when is_float_type t -> Ok Ir.Fadd
  | Sub when is_float_type t -> Ok Ir.Fsub
  | Mul when is_float_type t -> Ok Ir.Fmul
  | Div when is_float_type t -> Ok Ir.Fdiv
  | Ast.Add -> Ok Ir.Add
  | Sub -> Ok Sub
  | Mul -> Ok Mul
  | Div -> Ok (if unsigned t then Udiv else Sdiv)
  | Rem -> Ok (if unsigned t then Urem else Srem)
  | Bit_and -> Ok And
  | Bit_or -> Ok Or
  | Bit_xor -> Ok Xor
  | _ -> error span "internal error: non-arithmetic operator reached binary lowering"

let width span = function
  | Ir.I1 -> Ok 1
  | I8 -> Ok 8
  | I16 -> Ok 16
  | I32 -> Ok 32
  | I64 -> Ok 64
  | _ -> error span "internal error: integer width requested for a non-integer type"

let coerce s span v target =
  let from = value_ty v in
  if from = target then Ok v
  else
    let* from_width = width span from in
    let* target_width = width span target in
    let id = fresh s in
    let k = if from_width < target_width then "zext" else "trunc" in
    emit s (Ir.Cast (id, k, from, v, target));
    Ok (Ir.Local (id, target))

let splat_scalar s span vector_ty elem_ty value =
  let* value = coerce s span value elem_ty in
  let inserted = fresh s in
  emit s
    (Ir.Insert (inserted, vector_ty, Ir.Zero vector_ty, Ir.Const (Ir.I32, 0L), value));
  let shuffled = fresh s in
  emit s (Ir.Shuffle_zero (shuffled, vector_ty, Ir.Local (inserted, vector_ty)));
  Ok (Ir.Local (shuffled, vector_ty))

let shift_amount s span target value =
  let elem_ty = match target with Ir.Vector (_, t) -> t | t -> t in
  match (target, value_ty value) with
  | (Ir.Vector (lanes, _) as target_ty), Ir.Vector (count_lanes, count_elem)
    when count_lanes = lanes ->
      let* bits = width span elem_ty in
      let mask = Int64.of_int (bits - 1) in
      let* count =
        if value_ty value = target_ty then Ok value
        else
          let from = value_ty value in
          let* from_width = width span count_elem in
          let* target_width = width span elem_ty in
          let id = fresh s in
          let k = if from_width < target_width then "zext" else "trunc" in
          emit s (Ir.Cast (id, k, from, value, target_ty));
          Ok (Ir.Local (id, target_ty))
      in
      let* mask_vec =
        splat_scalar s span target_ty elem_ty (Ir.Const (elem_ty, mask))
      in
      let id = fresh s in
      emit s (Ir.Bin (id, Ir.And, target_ty, count, mask_vec));
      Ok (Ir.Local (id, target_ty))
  | _ -> (
      let* value = coerce s span value elem_ty in
      let* bits = width span elem_ty in
      let mask = Int64.of_int (bits - 1) in
      let id = fresh s in
      emit s (Ir.Bin (id, Ir.And, elem_ty, value, Ir.Const (elem_ty, mask)));
      let value = Ir.Local (id, elem_ty) in
      match target with
      | Ir.Vector _ -> splat_scalar s span target elem_ty value
      | _ -> Ok value)

let rec intrinsic_suffix span = function
  | Ir.I8 -> Ok "i8"
  | Ir.I16 -> Ok "i16"
  | Ir.I32 -> Ok "i32"
  | Ir.I64 -> Ok "i64"
  | Ir.Vector (lanes, elem) ->
      let* elem_suffix = intrinsic_suffix span elem in
      Ok (Printf.sprintf "v%d%s" lanes elem_suffix)
  | _ -> error span "internal error: integer intrinsic has a non-integer type"

let rec float_suffix span = function
  | Ir.F32 -> Ok "f32"
  | Ir.F64 -> Ok "f64"
  | Ir.Vector (lanes, elem) ->
      let* suffix = float_suffix span elem in
      Ok (Printf.sprintf "v%d%s" lanes suffix)
  | _ -> error span "internal error: float intrinsic has a non-float type"

let value_const s t value =
  match t with
  | Ir.Vector (_, elem) ->
      let inserted = fresh s in
      emit s
        (Ir.Insert
           (inserted, t, Ir.Zero t, Ir.Const (Ir.I32, 0L), Ir.Const (elem, value)));
      let shuffled = fresh s in
      emit s (Ir.Shuffle_zero (shuffled, t, Ir.Local (inserted, t)));
      Ir.Local (shuffled, t)
  | _ -> Ir.Const (t, value)

let compare s cmp t lhs rhs =
  let id = fresh s in
  emit s (Ir.Cmp (id, cmp, t, lhs, rhs));
  match t with
  | Ir.Vector (lanes, _) -> Ir.Local (id, Ir.Vector (lanes, Ir.I1))
  | _ -> Ir.Local (id, Ir.I1)

let reduce_any s span value =
  match value_ty value with
  | Ir.I1 -> Ok value
  | Ir.Vector (lanes, Ir.I1) as t ->
      let id = fresh s in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             Ir.I1,
             Printf.sprintf "llvm.vector.reduce.or.v%di1" lanes,
             [ (t, Ir.No_extension, value) ] ));
      Ok (Ir.Local (id, Ir.I1))
  | _ -> error span "internal error: boolean reduction has a non-boolean type"

let condition_ty value =
  match value_ty value with
  | Ir.I1 as t -> t
  | Ir.Vector (_, Ir.I1) as t -> t
  | _ -> Ir.I1

let combine_conditions s lhs rhs =
  let t = condition_ty lhs in
  let id = fresh s in
  emit s (Ir.Bin (id, Ir.And, t, lhs, rhs));
  Ir.Local (id, t)

let either_condition s lhs rhs =
  let t = condition_ty lhs in
  let id = fresh s in
  emit s (Ir.Bin (id, Ir.Or, t, lhs, rhs));
  Ir.Local (id, t)

let emit_trap s =
  emit s Ir.Trap;
  s.current.term := Some Ir.Unreachable

let guard_condition s condition =
  let trap = fresh_block s and valid = fresh_block s in
  s.current.term := Some (Ir.CondBr (condition, trap.id, valid.id));
  enter s trap;
  emit_trap s;
  enter s valid

let signed_type = function
  | Hir.Int (Hir.I8 | I16 | I32 | I64 | Isize)
  | Hir.Vec (_, Hir.Int (Hir.I8 | I16 | I32 | I64 | Isize)) ->
      true
  | _ -> false

let lane_guard s index_expr iv lanes =
  let ity = value_ty iv in
  let limit = value_const s ity (Int64.of_int lanes) in
  let condition =
    if signed_type (Hir.expr_ty index_expr) then
      let below = compare s Ir.Slt ity iv (value_const s ity 0L) in
      let above = compare s Ir.Sge ity iv limit in
      either_condition s below above
    else compare s Ir.Uge ity iv limit
  in
  guard_condition s condition

let division_guard s span source_ty binop ir_ty lhs rhs =
  let zero_condition = compare s Ir.Eq ir_ty rhs (value_const s ir_ty 0L) in
  let* condition =
    if binop = Ir.Sdiv && signed_type source_ty then
      let* bits = width span (match ir_ty with Ir.Vector (_, t) -> t | t -> t) in
      let minimum = Int64.shift_left 1L (bits - 1) in
      let lhs_minimum = compare s Ir.Eq ir_ty lhs (value_const s ir_ty minimum) in
      let rhs_minus_one =
        compare s Ir.Eq ir_ty rhs (value_const s ir_ty Int64.minus_one)
      in
      Ok
        (either_condition s zero_condition
           (combine_conditions s lhs_minimum rhs_minus_one))
    else Ok zero_condition
  in
  match value_ty condition with
  | Ir.I1 ->
      guard_condition s condition;
      Ok ()
  | Ir.Vector (lanes, Ir.I1) ->
      let* any_lane = reduce_any s span condition in
      let ordered = fresh_block s and proceed = fresh_block s in
      s.current.term := Some (Ir.CondBr (any_lane, ordered.id, proceed.id));
      enter s ordered;
      let rec guard_lane lane =
        if lane >= lanes then Ok ()
        else
          let id = fresh s in
          emit s
            (Ir.Extract
               (id, value_ty condition, condition, Ir.Const (Ir.I64, Int64.of_int lane)));
          let trap = fresh_block s and next = fresh_block s in
          s.current.term := Some (Ir.CondBr (Ir.Local (id, Ir.I1), trap.id, next.id));
          enter s trap;
          emit_trap s;
          enter s next;
          guard_lane (lane + 1)
      in
      let* () = guard_lane 0 in
      emit_trap s;
      enter s proceed;
      Ok ()
  | _ -> error span "internal error: division guard has a non-boolean condition"

let emit_binary s span source_ty op ir_ty lhs rhs =
  if op = Ast.Shl || op = Ast.Shr then (
    let* count = shift_amount s span ir_ty rhs in
    let binop =
      match op with
      | Ast.Shr -> if unsigned source_ty then Ir.Lshr else Ir.Ashr
      | _ -> Ir.Shl
    in
    let result = fresh s in
    emit s (Ir.Bin (result, binop, ir_ty, lhs, count));
    Ok (Ir.Local (result, ir_ty)))
  else
    let* binop = bin_for span source_ty op in
    let division =
      match binop with Ir.Sdiv | Srem | Udiv | Urem -> true | _ -> false
    in
    let* () =
      if division then division_guard s span source_ty binop ir_ty lhs rhs else Ok ()
    in
    if binop = Ir.Srem then (
      let is_minus_one =
        compare s Ir.Eq ir_ty rhs (value_const s ir_ty Int64.minus_one)
      in
      let safe_rhs_id = fresh s in
      let safe_rhs = value_const s ir_ty 1L in
      emit s (Ir.Select (safe_rhs_id, is_minus_one, safe_rhs, rhs));
      let raw_id = fresh s in
      emit s (Ir.Bin (raw_id, binop, ir_ty, lhs, Ir.Local (safe_rhs_id, ir_ty)));
      let result_id = fresh s in
      emit s
        (Ir.Select
           (result_id, is_minus_one, value_const s ir_ty 0L, Ir.Local (raw_id, ir_ty)));
      Ok (Ir.Local (result_id, ir_ty)))
    else
      let result = fresh s in
      emit s (Ir.Bin (result, binop, ir_ty, lhs, rhs));
      Ok (Ir.Local (result, ir_ty))

let require_bool span v =
  match value_ty v with
  | Ir.I1 -> Ok v
  | _ -> error span "internal error: condition lowering received a non-bool value"

let bind_local s binding value alignment =
  let old = Hashtbl.find_opt s.env binding.Hir.id in
  (match s.env_scopes with
  | scope :: rest -> s.env_scopes <- ((binding.id, old) :: scope) :: rest
  | [] -> ());
  Hashtbl.replace s.env binding.id (value, alignment)

let push_scope s =
  s.env_scopes <- [] :: s.env_scopes;
  s.defer_scopes <- [] :: s.defer_scopes;
  s.scope_slots <- [] :: s.scope_slots

let pop_scope s =
  match (s.env_scopes, s.defer_scopes, s.scope_slots) with
  | scope :: er, _ :: dr, _ :: lr ->
      List.iter
        (fun (n, old) ->
          match old with
          | None -> Hashtbl.remove s.env n
          | Some v -> Hashtbl.replace s.env n v)
        scope;
      s.env_scopes <- er;
      s.defer_scopes <- dr;
      s.scope_slots <- lr
  | _ -> ()

let find_struct s n = List.find_opt (fun (d : Hir.struct_def) -> d.name = n) s.structs

let normalize_index_type s index_ty span value =
  let bits = function
    | Hir.Int (Hir.U8 | Hir.I8) -> Ok 8
    | Hir.Int (Hir.U16 | Hir.I16) -> Ok 16
    | Hir.Int (Hir.U32 | Hir.I32) -> Ok 32
    | Hir.Int (Hir.U64 | Hir.I64) -> Ok 64
    | Hir.Int (Hir.Usize | Hir.Isize) -> Ok ((Target_layout.current ()).pointer_size * 8)
    | _ -> error span "internal error: index lowering received a non-integer value"
  in
  let t = match index_ty with Hir.Vec (_, element) -> element | _ -> index_ty in
  let* source_bits = bits t in
  let target_bits = (Target_layout.current ()).pointer_size * 8 in
  if source_bits = target_bits then Ok value
  else
    let id = fresh s in
    let kind =
      if source_bits > target_bits then "trunc"
      else if unsigned t then "zext"
      else "sext"
    in
    let pointer_int = ty (Hir.Int Hir.Usize) in
    emit s (Ir.Cast (id, kind, value_ty value, value, pointer_int));
    Ok (Ir.Local (id, pointer_int))

let normalize_index s expression value =
  normalize_index_type s (Hir.expr_ty expression) (Hir.expr_span expression) value

let rec static_address structs = function
  | Hir.Global (name, _, _) | Hir.Const_array (name, _, _) -> Some (name, 0)
  | Hir.Field (base, _, _, offset, _) ->
      Option.map
        (fun (name, previous) -> (name, previous + offset))
        (static_address structs base)
  | Hir.Index (base, Hir.EInt (index, _, _), element, _) -> (
      match Hir.layout structs element with
      | Ok (size, _) ->
          Option.map
            (fun (name, offset) -> (name, offset + (Int64.to_int index * size)))
            (static_address structs base)
      | Error _ -> None)
  | _ -> None

let rec constant_construction structs = function
  | Hir.Init_zero (zero, _, _) -> Some (Hir.Global_zero zero)
  | Hir.Init_omitted (zero, _) -> Some (Hir.Global_zero zero)
  | Hir.Init_value (Hir.EInt (value, _, _)) -> Some (Hir.Global_int value)
  | Hir.Init_value (Hir.EFloat (value, _, _)) -> Some (Hir.Global_float value)
  | Hir.Init_value (Hir.EBool (value, _)) -> Some (Hir.Global_bool value)
  | Hir.Init_value (Hir.Null _) -> Some Hir.Global_null
  | Hir.Init_value (Hir.EString (id, _, _)) ->
      Some (Hir.Global_address (".str." ^ string_of_int id, 0))
  | Hir.Init_value (Hir.Function_address (name, _)) ->
      Some (Hir.Global_address (name, 0))
  | Hir.Init_value (Hir.Address (place, _, _)) ->
      Option.map
        (fun (name, offset) -> Hir.Global_address (name, offset))
        (static_address structs place)
  | Hir.Init_value (Hir.Call (Hir.Builtin (Hir.Handle_from_addr _), [ value ], _, _)) ->
      constant_construction structs (Hir.Init_value value)
  | Hir.Init_value (Hir.EVector (values, _, _)) -> Some (Hir.Global_vector values)
  | Hir.Init_aggregate (aggregate_ty, _, entries, _) ->
      let entries = List.map (constant_construction structs) entries in
      if List.for_all Option.is_some entries then
        let values = List.map Option.get entries in
        Some
          (match aggregate_ty with
          | Hir.Array _ -> Hir.Global_array values
          | _ -> Hir.Global_struct values)
      else None
  | _ -> None

let rec expr s = function
  | Hir.EInt (v, t, _) -> Ok (Ir.Const (ty t, v))
  | Hir.EFloat (v, t, _) -> Ok (Ir.Const (ty t, v))
  | Hir.EBool (v, _) -> Ok (Ir.Const (Ir.I1, if v then 1L else 0L))
  | Hir.EVector
      (values, (Hir.Vec (lanes, (Hir.Bool | Hir.Int _ | Hir.Float _)) as t), span) ->
      if List.length values = lanes then Ok (Ir.Const_vector (ty t, values))
      else error span "malformed vector constant lane count"
  | Hir.EVector (_, _, span) -> error span "malformed vector constant type"
  | Hir.Vector_lit (elements, (Hir.Vec (lanes, _) as vector_ty), span) ->
      if List.length elements <> lanes then
        error span "malformed vector literal lane count"
      else
        let vector_ir_ty = ty vector_ty in
        let rec lanes_to_value value index = function
          | [] -> Ok value
          | element :: rest ->
              let* lane = expr s element in
              let id = fresh s in
              emit s
                (Ir.Insert
                   (id, vector_ir_ty, value, Ir.Const (Ir.I32, Int64.of_int index), lane));
              lanes_to_value (Ir.Local (id, vector_ir_ty)) (index + 1) rest
        in
        lanes_to_value (Ir.Zero vector_ir_ty) 0 elements
  | Hir.Vector_lit (_, _, span) -> error span "malformed vector literal type"
  | Hir.Null (t, _) -> Ok (Ir.Null (ty t))
  | Hir.EString (i, _, sp) ->
      if i < 0 || i >= List.length s.strings then error sp "missing interned string"
      else
        let id = fresh s in
        emit s (Ir.String_ptr (id, i, String.length (List.nth s.strings i)));
        Ok (Ir.Local (id, Ir.Pointer Ir.I8))
  | Hir.Local (local, sp) -> (
      match Hashtbl.find_opt s.env local.id with
      | None -> error sp ("unknown lowering local `" ^ local.name ^ "`")
      | Some (p, alignment) -> load_value s local.ty p alignment)
  | Hir.Global (name, global_ty, _) ->
      let* p = address s (Hir.Global (name, global_ty, Span.synthetic)) in
      let* alignment =
        place_alignment s (Hir.Global (name, global_ty, Span.synthetic))
      in
      load_value s global_ty p alignment
  | Hir.Function_address (name, _) -> Ok (Ir.Global (name, Ir.Pointer Ir.I8))
  | Hir.Unary (op, e, t, span) -> (
      let* v = expr s e in
      let rt = ty t in
      match op with
      | Ast.Neg ->
          let id = fresh s in
          if is_float_type t then emit s (Ir.Fneg (id, rt, v))
          else emit s (Ir.Bin (id, Sub, rt, zero rt, v));
          Ok (Ir.Local (id, rt))
      | Bit_not ->
          let* mask = ones span rt in
          let id = fresh s in
          emit s (Ir.Bin (id, Xor, rt, v, Ir.Const (rt, mask)));
          Ok (Ir.Local (id, rt))
      | Not when rt = Ir.I1 ->
          let id = fresh s in
          emit s (Ir.Cmp (id, Ir.Eq, Ir.I1, v, Ir.Const (Ir.I1, 0L)));
          Ok (Ir.Local (id, Ir.I1))
      | Not -> (
          match rt with
          | Ir.Vector (_, Ir.I1) ->
              let id = fresh s in
              emit s (Ir.Bin (id, Ir.Xor, rt, v, value_const s rt 1L));
              Ok (Ir.Local (id, rt))
          | _ -> error span "internal error: logical not has a non-bool type"))
  | Hir.Binary (((Ast.And | Ast.Or) as op), a, b, _, _) -> lower_short s op a b
  | Hir.Binary (op, a, b, _t, span)
    when match Hir.expr_ty a with Hir.Addr -> true | _ -> false -> (
      let* x = expr s a in
      let* y = expr s b in
      match op with
      | Ast.Add -> address_step s Ir.Add x y
      | Ast.Sub -> address_step s Ir.Sub x y
      | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge ->
          let pointer_int = ty (Hir.Int Hir.Usize) in
          let x_id = fresh s in
          emit s (Ir.Cast (x_id, "ptrtoint", Ir.Pointer Ir.I8, x, pointer_int));
          let y_id = fresh s in
          emit s (Ir.Cast (y_id, "ptrtoint", Ir.Pointer Ir.I8, y, pointer_int));
          let comparison =
            match op with
            | Ast.Eq -> Ir.Eq
            | Ast.Ne -> Ir.Ne
            | Ast.Lt -> Ir.Ult
            | Ast.Le -> Ir.Ule
            | Ast.Gt -> Ir.Ugt
            | _ -> Ir.Uge
          in
          let id = fresh s in
          emit s
            (Ir.Cmp
               ( id,
                 comparison,
                 pointer_int,
                 Ir.Local (x_id, pointer_int),
                 Ir.Local (y_id, pointer_int) ));
          Ok (Ir.Local (id, Ir.I1))
      | _ -> error span "internal error: invalid address binary operation")
  | Hir.Binary (op, a, b, t, span) ->
      let* x = expr s a in
      let* y = expr s b in
      if List.mem op [ Ast.Eq; Ne; Lt; Le; Gt; Ge ] then (
        let* comparison = cmp_for span (Hir.expr_ty a) op in
        let id = fresh s in
        emit s (Ir.Cmp (id, comparison, value_ty x, x, y));
        Ok (Ir.Local (id, ty t)))
      else emit_binary s span (Hir.expr_ty a) op (ty t) x y
  | Hir.Call (Hir.User n, args, t, sp) ->
      let* target =
        match List.find_opt (fun (f : Hir.func) -> f.name = n) s.functions with
        | Some target -> Ok target
        | None -> error sp ("unknown function `" ^ n ^ "` reached lowering")
      in
      let* vs = exprs s args in
      let c_function =
        if has_c_abi target || Hashtbl.mem s.addressed_functions n then Some target
        else None
      in
      let av =
        List.mapi
          (fun index v ->
            let extension =
              match c_function with
              | Some f -> (
                  match List.nth_opt f.params index with
                  | Some formal -> c_extension formal.ty
                  | None -> Ir.No_extension)
              | None -> Ir.No_extension
            in
            (value_ty v, extension, v))
          vs
      in
      let ret_extension =
        match c_function with Some _ -> c_extension t | None -> Ir.No_extension
      in
      if t = Hir.Void then (
        emit s (Ir.Call (None, Ir.No_extension, Ir.Void, n, av));
        Ok (Ir.Const (Ir.I1, 0L)))
      else
        let id = fresh s in
        emit s (Ir.Call (Some id, ret_extension, ty t, n, av));
        Ok (Ir.Local (id, ty t))
  | Hir.Call (Hir.Builtin b, args, t, span) -> lower_builtin s b args t span
  | Hir.Indirect_call (callee, args, result_ty, _) ->
      let* target = expr s callee in
      let* values = exprs s args in
      let arguments =
        List.map2
          (fun arg value -> (value_ty value, c_extension (Hir.expr_ty arg), value))
          args values
      in
      let result_extension = c_extension result_ty in
      if result_ty = Hir.Void then (
        emit s (Ir.Call_indirect (None, Ir.No_extension, Ir.Void, target, arguments));
        Ok (Ir.Const (Ir.I1, 0L)))
      else
        let id = fresh s in
        emit s
          (Ir.Call_indirect (Some id, result_extension, ty result_ty, target, arguments));
        Ok (Ir.Local (id, ty result_ty))
  | Hir.Cast (kind, e, t, _) ->
      let* v = expr s e in
      let st = value_ty v and dt = ty t in
      let vector_bitcast =
        match (kind, Hir.expr_ty e, t) with
        | Ast.Bitcast, Hir.Vec _, _ | Ast.Bitcast, _, Hir.Vec _ -> true
        | _ -> false
      in
      if st = dt && not vector_bitcast then Ok v
      else
        let k =
          match kind with
          | Ast.Bitcast -> (
              match (st, dt) with
              | Ir.Pointer _, Ir.Pointer _ -> ""
              | Ir.Pointer _, _ -> "ptrtoint"
              | _, Ir.Pointer _ -> "inttoptr"
              | _ -> "bitcast")
          | Ast.Zext -> "zext"
          | Ast.Sext -> "sext"
          | Ast.Trunc -> "trunc"
          | Ast.Float_widen -> "fpext"
        in
        if k = "" then Ok v
        else
          let id = fresh s in
          emit s (Ir.Cast (id, k, st, v, dt));
          Ok (Ir.Local (id, dt))
  | Hir.Raw_select (b, off, t, _) ->
      let* p = raw_address s b off in
      raw_load s t p
  | Hir.Address (e, _, _) ->
      let* p = address s e in
      let id = fresh s in
      emit s (Ir.Cast (id, "bitcast", Ir.value_ty p, p, Ir.Pointer Ir.I8));
      Ok (Ir.Local (id, Ir.Pointer Ir.I8))
  | Hir.Index (a, i, t, _) -> (
      match Hir.expr_ty a with
      | Hir.Vec (lanes, _) ->
          let* av = expr s a in
          let* iv = expr s i in
          let* iv = normalize_index s i iv in
          lane_guard s i iv lanes;
          let id = fresh s in
          emit s (Ir.Extract (id, value_ty av, av, iv));
          Ok (Ir.Local (id, ty t))
      | _ ->
          let* p = index_address s a i in
          let* alignment = place_alignment s (Hir.Index (a, i, t, Span.synthetic)) in
          load_value s t p alignment)
  | Hir.Field (a, _, t, off, _) ->
      let* p = field_address s a off in
      let* alignment = place_alignment s (Hir.Field (a, "", t, off, Span.synthetic)) in
      load_value s t p alignment
  | Hir.Sizeof (_, n, _) | Hir.Alignof (_, n, _) | Hir.Offsetof (_, _, n, _) ->
      Ok (Ir.Const (ty (Hir.Int Hir.Usize), Int64.of_int n))
  | Hir.Ternary (c, a, b, t, _) -> lower_ternary s c a b t
  | Hir.Splat (e, t, _) ->
      let* x = expr s e in
      let vt = ty t in
      let i1 = fresh s in
      emit s (Ir.Insert (i1, vt, Ir.Zero vt, Ir.Const (Ir.I32, 0L), x));
      let i2 = fresh s in
      emit s (Ir.Shuffle_zero (i2, vt, Ir.Local (i1, vt)));
      Ok (Ir.Local (i2, vt))
  | Hir.Const_array (n, t, _) ->
      let ptr_id = fresh s in
      emit s (Ir.Global_ptr (ptr_id, n, storage_ty t));
      let* alignment = place_alignment s (Hir.Const_array (n, t, Span.synthetic)) in
      load_value s t (Ir.Local (ptr_id, Ir.Pointer (ty t))) alignment

and address_step s operation pointer offset =
  let pointer_int = ty (Hir.Int Hir.Usize) in
  let pointer_bits = fresh s in
  emit s (Ir.Cast (pointer_bits, "ptrtoint", Ir.Pointer Ir.I8, pointer, pointer_int));
  let result_bits = fresh s in
  emit s
    (Ir.Bin
       ( result_bits,
         operation,
         pointer_int,
         Ir.Local (pointer_bits, pointer_int),
         offset ));
  let result = fresh s in
  emit s
    (Ir.Cast
       ( result,
         "inttoptr",
         pointer_int,
         Ir.Local (result_bits, pointer_int),
         Ir.Pointer Ir.I8 ));
  Ok (Ir.Local (result, Ir.Pointer Ir.I8))

and exprs s xs = Result_list.map (expr s) xs

and lower_builtin s b args t span =
  let* vs = exprs s args in
  let rt = ty t in
  match (b, vs) with
  | Volatile_load access_ty, [ pointer ] -> volatile_load s span access_ty pointer
  | Simd_load (access_ty, kind), values ->
      simd_memory_load s span access_ty kind args values
  | Addr_bits, [ x ] ->
      let id = fresh s in
      emit s (Ir.Cast (id, "ptrtoint", Ir.value_ty x, x, rt));
      Ok (Ir.Local (id, rt))
  | Addr_from_bits, [ x ] ->
      let id = fresh s in
      emit s (Ir.Cast (id, "inttoptr", Ir.value_ty x, x, Ir.Pointer Ir.I8));
      Ok (Ir.Local (id, Ir.Pointer Ir.I8))
  | Cvt target_ty, [ x ] ->
      let source_ty = Hir.expr_ty (List.hd args) in
      let source_element =
        match source_ty with Hir.Vec (_, element) -> element | ty -> ty
      in
      let target_element =
        match target_ty with Hir.Vec (_, element) -> element | ty -> ty
      in
      let opcode =
        match (source_element, target_element) with
        | Hir.Int (Hir.I8 | I16 | I32 | I64 | Isize), Hir.Float _ -> "sitofp"
        | Hir.Int _, Hir.Float _ -> "uitofp"
        | Hir.Float Hir.F32, Hir.Float Hir.F64 -> "fpext"
        | Hir.Float Hir.F64, Hir.Float Hir.F32 -> "fptrunc"
        | Hir.Float _, Hir.Int _ -> ""
        | _ -> ""
      in
      if opcode <> "" then (
        let id = fresh s in
        emit s (Ir.Cast (id, opcode, Ir.value_ty x, x, rt));
        Ok (Ir.Local (id, rt)))
      else
        let destination =
          match target_element with Hir.Int kind -> kind | _ -> Hir.I32
        in
        let width =
          match destination with
          | Hir.U8 | I8 -> 8
          | U16 | I16 -> 16
          | U32 | I32 -> 32
          | U64 | I64 -> 64
          | Usize | Isize -> (Target_layout.current ()).pointer_size * 8
        in
        let conversion =
          match destination with
          | Hir.I8 | I16 | I32 | I64 | Isize -> "fptosi"
          | _ -> "fptoui"
        in
        let format =
          match float_suffix span (ty source_ty) with
          | Ok suffix -> suffix
          | Error _ -> "f64"
        in
        let result_suffix =
          match rt with
          | Ir.Vector (lanes, elem) ->
              let elem_name =
                match elem with
                | Ir.I8 -> "i8"
                | I16 -> "i16"
                | I32 -> "i32"
                | I64 -> "i64"
                | _ -> "i64"
              in
              Printf.sprintf "v%d%s" lanes elem_name
          | _ -> Printf.sprintf "i%d" width
        in
        let id = fresh s in
        emit s
          (Ir.Call
             ( Some id,
               Ir.No_extension,
               rt,
               Printf.sprintf "llvm.%s.sat.%s.%s" conversion result_suffix format,
               [ (Ir.value_ty x, Ir.No_extension, x) ] ));
        Ok (Ir.Local (id, rt))
  | Float_math operation, values ->
      let suffix = float_suffix span rt in
      let* suffix = suffix in
      let name =
        match operation with
        | Float_sqrt -> "llvm.sqrt."
        | Float_fma -> "llvm.fma."
        | Float_floor -> "llvm.floor."
        | Float_ceil -> "llvm.ceil."
        | Float_round -> "llvm.roundeven."
        | Float_abs -> "llvm.fabs."
      in
      let* () =
        let expected = if operation = Float_fma then 3 else 1 in
        if List.length values = expected then Ok ()
        else error span "internal error: invalid float builtin arity"
      in
      let id = fresh s in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             rt,
             name ^ suffix,
             List.map (fun value -> (rt, Ir.No_extension, value)) values ));
      Ok (Ir.Local (id, rt))
  | (Handle_addr | Handle_from_addr _), [ x ] -> Ok x
  | (Rotl | Rotr), [ x; n ] ->
      let* n = shift_amount s span rt n in
      let* suffix = intrinsic_suffix span rt in
      let id = fresh s in
      let base = if b = Rotl then "llvm.fshl." else "llvm.fshr." in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             rt,
             base ^ suffix,
             [
               (rt, Ir.No_extension, x);
               (rt, Ir.No_extension, x);
               (rt, Ir.No_extension, n);
             ] ));
      Ok (Ir.Local (id, rt))
  | Popcount, [ x ] ->
      let* suffix = intrinsic_suffix span rt in
      let id = fresh s in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             rt,
             "llvm.ctpop." ^ suffix,
             [ (rt, Ir.No_extension, x) ] ));
      Ok (Ir.Local (id, rt))
  | (Ctz | Clz), [ x ] ->
      let* suffix = intrinsic_suffix span rt in
      let id = fresh s in
      let base = if b = Ctz then "llvm.cttz." else "llvm.ctlz." in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             rt,
             base ^ suffix,
             [
               (rt, Ir.No_extension, x); (Ir.I1, Ir.No_extension, Ir.Const (Ir.I1, 0L));
             ] ));
      Ok (Ir.Local (id, rt))
  | (Add_sat | Sub_sat), [ x; y ] ->
      let* suffix = intrinsic_suffix span rt in
      let* base =
        let rec kind_of = function
          | Hir.Int k -> Some k
          | Hir.Vec (_, element) -> kind_of element
          | _ -> None
        in
        match kind_of t with
        | None ->
            error span "internal error: saturating builtin requires integer operands"
        | Some k -> (
            let unsigned =
              match k with
              | Hir.U8 | U16 | U32 | U64 | Usize -> true
              | Hir.I8 | I16 | I32 | I64 | Isize -> false
            in
            match (b, unsigned) with
            | Add_sat, true -> Ok "llvm.uadd.sat."
            | Add_sat, false -> Ok "llvm.sadd.sat."
            | Sub_sat, true -> Ok "llvm.usub.sat."
            | Sub_sat, false -> Ok "llvm.ssub.sat."
            | ( ( Rotl | Rotr | Popcount | Ctz | Clz | Mul_hi | Any | All | Select
                | Shuffle | Permute | Reduce_sum | Reduce_min | Reduce_max | Reduce_and
                | Reduce_or | Reduce_xor | Compress | Expand | Addr_bits
                | Addr_from_bits | Cvt _ | Float_math _ | Handle_addr
                | Handle_from_addr _ | Volatile_load _ | Simd_load _ ),
                _ ) ->
                error span "internal error: invalid saturating builtin")
      in
      let id = fresh s in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             rt,
             base ^ suffix,
             [ (rt, Ir.No_extension, x); (rt, Ir.No_extension, y) ] ));
      Ok (Ir.Local (id, rt))
  | Mul_hi, [ x; y ] -> (
      let rec elem_of = function
        | Hir.Int k -> Some k
        | Hir.Vec (_, element) -> elem_of element
        | _ -> None
      in
      match elem_of t with
      | None ->
          error span "internal error: multiply-high builtin requires integer operands"
      | Some k ->
          let unsigned =
            match k with
            | Hir.U8 | U16 | U32 | U64 | Usize -> true
            | Hir.I8 | I16 | I32 | I64 | Isize -> false
          in
          let elem_ty, lanes =
            match rt with Ir.Vector (n, e) -> (e, n) | e -> (e, 1)
          in
          let* elem_bits = width span elem_ty in
          let* wide_elem =
            match elem_bits with
            | 8 -> Ok Ir.I16
            | 16 -> Ok Ir.I32
            | 32 -> Ok Ir.I64
            | 64 -> Ok Ir.I128
            | _ -> error span "internal error: multiply-high operand width"
          in
          let wide =
            match rt with
            | Ir.Vector (n, _) -> Ir.Vector (n, wide_elem)
            | _ -> wide_elem
          in
          let cast_kind = if unsigned then "zext" else "sext" in
          let ex = fresh s in
          emit s (Ir.Cast (ex, cast_kind, rt, x, wide));
          let ey = fresh s in
          emit s (Ir.Cast (ey, cast_kind, rt, y, wide));
          let product = fresh s in
          emit s
            (Ir.Bin (product, Ir.Mul, wide, Ir.Local (ex, wide), Ir.Local (ey, wide)));
          let amount =
            match rt with
            | Ir.Vector _ ->
                Ir.Const_vector (wide, List.init lanes (fun _ -> Int64.of_int elem_bits))
            | _ -> Ir.Const (wide, Int64.of_int elem_bits)
          in
          let high = fresh s in
          emit s
            (Ir.Bin
               ( high,
                 (if unsigned then Ir.Lshr else Ir.Ashr),
                 wide,
                 Ir.Local (product, wide),
                 amount ));
          let id = fresh s in
          emit s (Ir.Cast (id, "trunc", wide, Ir.Local (high, wide), rt));
          Ok (Ir.Local (id, rt)))
  | Select, [ m; y; z ] ->
      let id = fresh s in
      emit s (Ir.Select (id, m, y, z));
      Ok (Ir.Local (id, rt))
  | Shuffle, [ a; b; sel ] -> (
      match sel with
      | Ir.Const_vector (_, values) ->
          let id = fresh s in
          let mask = Ir.Const_vector (Ir.Vector (List.length values, Ir.I32), values) in
          emit s (Ir.Shufflevector (id, rt, a, b, mask));
          Ok (Ir.Local (id, rt))
      | _ -> error span "internal error: shuffle selectors must be constant")
  | Permute, [ v; idx ] ->
      let n = match Hir.expr_ty (List.hd args) with Hir.Vec (n, _) -> n | _ -> 1 in
      let iw = match value_ty idx with Ir.Vector (_, w) -> w | _ -> Ir.I32 in
      let elem_ty = match rt with Ir.Vector (_, e) -> e | _ -> Ir.I1 in
      let m = match rt with Ir.Vector (m, _) -> m | _ -> 1 in
      let rec chain j acc =
        if j = m then Ok acc
        else
          let slot = Ir.Const (Ir.I64, Int64.of_int j) in
          let iid = fresh s in
          emit s (Ir.Extract (iid, value_ty idx, idx, slot));
          let il = Ir.Local (iid, iw) in
          let all_indices_fit = match iw with Ir.I8 -> n >= 256 | _ -> false in
          let vl, sl =
            if all_indices_fit then (Ir.Const (Ir.I1, 1L), il)
            else
              let vid = fresh s in
              emit s (Ir.Cmp (vid, Ir.Ult, iw, il, Ir.Const (iw, Int64.of_int n)));
              let vl = Ir.Local (vid, Ir.I1) in
              let sid = fresh s in
              emit s (Ir.Select (sid, vl, il, Ir.Const (iw, 0L)));
              (vl, Ir.Local (sid, iw))
          in
          let eid = fresh s in
          emit s (Ir.Extract (eid, value_ty v, v, sl));
          let el = Ir.Local (eid, elem_ty) in
          let pid = fresh s in
          emit s (Ir.Select (pid, vl, el, Ir.Zero elem_ty));
          let pl = Ir.Local (pid, elem_ty) in
          let nid = fresh s in
          emit s (Ir.Insert (nid, rt, acc, slot, pl));
          chain (j + 1) (Ir.Local (nid, rt))
      in
      chain 0 (Ir.Zero rt)
  | (Reduce_sum | Reduce_min | Reduce_max | Reduce_and | Reduce_or | Reduce_xor), [ v ]
    ->
      let vty = Hir.expr_ty (List.hd args) in
      let lanes = match vty with Hir.Vec (n, _) -> n | _ -> 1 in
      let signed =
        match vty with
        | Hir.Vec (_, Hir.Int (Hir.I8 | I16 | I32 | I64 | Isize)) -> true
        | _ -> false
      in
      let bin op a b =
        let bid = fresh s in
        emit s (Ir.Bin (bid, op, rt, a, b));
        Ok (Ir.Local (bid, rt))
      in
      let pick cmp a b =
        let cid = fresh s in
        emit s (Ir.Cmp (cid, cmp, rt, a, b));
        let sid = fresh s in
        emit s (Ir.Select (sid, Ir.Local (cid, Ir.I1), a, b));
        Ok (Ir.Local (sid, rt))
      in
      let rec chain i acc =
        match acc with
        | None ->
            let eid = fresh s in
            emit s (Ir.Extract (eid, value_ty v, v, Ir.Const (Ir.I64, Int64.of_int i)));
            chain (i + 1) (Some (Ir.Local (eid, rt)))
        | Some a ->
            if i = lanes then Ok a
            else
              let eid = fresh s in
              emit s
                (Ir.Extract (eid, value_ty v, v, Ir.Const (Ir.I64, Int64.of_int i)));
              let el = Ir.Local (eid, rt) in
              let* next =
                match b with
                | Reduce_sum -> bin Ir.Add a el
                | Reduce_and -> bin Ir.And a el
                | Reduce_or -> bin Ir.Or a el
                | Reduce_xor -> bin Ir.Xor a el
                | Reduce_min -> pick (if signed then Ir.Slt else Ir.Ult) a el
                | Reduce_max -> pick (if signed then Ir.Sgt else Ir.Ugt) a el
                | _ -> error span "internal error: invalid reduction builtin"
              in
              chain (i + 1) (Some next)
      in
      chain 0 None
  | Compress, [ v; mk ]
    when (Target_layout.current ()).os = Target_layout.Wasi
         && match rt with Ir.Vector (n, _) -> n > 1 | _ -> false ->
      let bits_ty =
        match rt with
        | Ir.Vector (n, Ir.F32) -> Ir.Vector (n, Ir.I32)
        | Ir.Vector (n, Ir.F64) -> Ir.Vector (n, Ir.I64)
        | Ir.Vector (n, Ir.I1) -> Ir.Vector (n, Ir.I8)
        | t -> t
      in
      let convert kind value from target =
        if from = target then value
        else
          let id = fresh s in
          emit s (Ir.Cast (id, kind, from, value, target));
          Ir.Local (id, target)
      in
      let into, back =
        match rt with
        | Ir.Vector (_, Ir.I1) -> ("zext", "trunc")
        | _ -> ("bitcast", "bitcast")
      in
      let bitcast value from target =
        convert (if from = rt then into else back) value from target
      in
      let* name = intrinsic_suffix span bits_ty in
      let id = fresh s in
      emit s
        (Ir.Call
           ( Some id,
             Ir.No_extension,
             bits_ty,
             "llvm.experimental.vector.compress." ^ name,
             [
               (bits_ty, Ir.No_extension, bitcast v rt bits_ty);
               (value_ty mk, Ir.No_extension, mk);
               (bits_ty, Ir.No_extension, Ir.Zero bits_ty);
             ] ));
      Ok (bitcast (Ir.Local (id, bits_ty)) bits_ty rt)
  | Compress, [ v; mk ] ->
      let n = match Hir.expr_ty (List.hd args) with Hir.Vec (n, _) -> n | _ -> 1 in
      let elem_ty = match rt with Ir.Vector (_, e) -> e | _ -> Ir.I1 in
      let rec chain i acc count =
        if i = n then Ok acc
        else
          let slot = Ir.Const (Ir.I64, Int64.of_int i) in
          let mid = fresh s in
          emit s (Ir.Extract (mid, value_ty mk, mk, slot));
          let ml = Ir.Local (mid, Ir.I1) in
          let vid = fresh s in
          emit s (Ir.Extract (vid, value_ty v, v, slot));
          let vl = Ir.Local (vid, elem_ty) in
          let cid = fresh s in
          emit s (Ir.Select (cid, ml, count, Ir.Const (Ir.I64, 0L)));
          let cl = Ir.Local (cid, Ir.I64) in
          let iid = fresh s in
          emit s (Ir.Insert (iid, rt, acc, cl, vl));
          let il = Ir.Local (iid, rt) in
          let aid = fresh s in
          emit s (Ir.Select (aid, ml, il, acc));
          let n1 = fresh s in
          emit s (Ir.Bin (n1, Ir.Add, Ir.I64, count, Ir.Const (Ir.I64, 1L)));
          let nid = fresh s in
          emit s (Ir.Select (nid, ml, Ir.Local (n1, Ir.I64), count));
          chain (i + 1) (Ir.Local (aid, rt)) (Ir.Local (nid, Ir.I64))
      in
      chain 0 (Ir.Zero rt) (Ir.Const (Ir.I64, 0L))
  | Expand, [ v; mk ] ->
      let n = match Hir.expr_ty (List.hd args) with Hir.Vec (n, _) -> n | _ -> 1 in
      let elem_ty = match rt with Ir.Vector (_, e) -> e | _ -> Ir.I1 in
      let rec chain d acc count =
        if d = n then Ok acc
        else
          let mid = fresh s in
          emit s (Ir.Extract (mid, value_ty mk, mk, Ir.Const (Ir.I64, Int64.of_int d)));
          let ml = Ir.Local (mid, Ir.I1) in
          let cid = fresh s in
          emit s (Ir.Select (cid, ml, count, Ir.Const (Ir.I64, 0L)));
          let cl = Ir.Local (cid, Ir.I64) in
          let vid = fresh s in
          emit s (Ir.Extract (vid, value_ty v, v, cl));
          let vl = Ir.Local (vid, elem_ty) in
          let iid = fresh s in
          emit s (Ir.Insert (iid, rt, acc, Ir.Const (Ir.I64, Int64.of_int d), vl));
          let il = Ir.Local (iid, rt) in
          let aid = fresh s in
          emit s (Ir.Select (aid, ml, il, acc));
          let n1 = fresh s in
          emit s (Ir.Bin (n1, Ir.Add, Ir.I64, count, Ir.Const (Ir.I64, 1L)));
          let nid = fresh s in
          emit s (Ir.Select (nid, ml, Ir.Local (n1, Ir.I64), count));
          chain (d + 1) (Ir.Local (aid, rt)) (Ir.Local (nid, Ir.I64))
      in
      chain 0 (Ir.Zero rt) (Ir.Const (Ir.I64, 0L))
  | (Any | All), [ mv ] -> (
      let lanes =
        match Hir.expr_ty (List.hd args) with Hir.Vec (n, _) -> n | _ -> 1
      in
      let op = if b = Any then Ir.Or else Ir.And in
      let rec chain i acc =
        if i = lanes then Ok acc
        else
          let lid = fresh s in
          emit s (Ir.Extract (lid, value_ty mv, mv, Ir.Const (Ir.I64, Int64.of_int i)));
          let lv = Ir.Local (lid, Ir.I1) in
          match acc with
          | None -> chain (i + 1) (Some lv)
          | Some a ->
              let bid = fresh s in
              emit s (Ir.Bin (bid, op, Ir.I1, a, lv));
              chain (i + 1) (Some (Ir.Local (bid, Ir.I1)))
      in
      let* result = chain 0 None in
      match result with
      | Some v -> Ok v
      | None -> error span "internal error: mask reduction requires a vector")
  | _ -> error span "internal error: invalid builtin arity"

and lower_short s op a b =
  let* lv = expr s a in
  let* lv = require_bool (Hir.expr_span a) lv in
  let pred = s.current.id and rhs = fresh_block s and join = fresh_block s in
  s.current.term :=
    Some
      (Ir.CondBr
         ( lv,
           (if op = Ast.And then rhs.id else join.id),
           if op = Ast.And then join.id else rhs.id ));
  enter s rhs;
  let* rv = expr s b in
  let* rv = require_bool (Hir.expr_span b) rv in
  let rp = s.current.id in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  enter s join;
  let id = fresh s in
  emit s
    (Ir.Phi
       ( id,
         Ir.I1,
         [
           ((if op = Ast.And then Ir.Const (Ir.I1, 0L) else Ir.Const (Ir.I1, 1L)), pred);
           (rv, rp);
         ] ));
  Ok (Ir.Local (id, Ir.I1))

and lower_ternary s c a b t =
  let* cv = expr s c in
  let* cv = require_bool (Hir.expr_span c) cv in
  let tb = fresh_block s and eb = fresh_block s and join = fresh_block s in
  s.current.term := Some (Ir.CondBr (cv, tb.id, eb.id));
  enter s tb;
  let* tv = expr s a in
  let tp = s.current.id in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  enter s eb;
  let* ev = expr s b in
  let ep = s.current.id in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  enter s join;
  let id = fresh s in
  emit s (Ir.Phi (id, ty t, [ (tv, tp); (ev, ep) ]));
  Ok (Ir.Local (id, ty t))

and materialize s e =
  let* v = expr s e in
  let ht = Hir.expr_ty e in
  let* alignment = align s ht in
  let id = fresh s in
  emit s (Ir.Alloca (id, storage_ty ht, alignment));
  let p = Ir.Local (id, Ir.Pointer (storage_ty ht)) in
  let* () = store_value s ht v p alignment in
  Ok p

and address s e =
  match e with
  | Hir.Global (name, global_ty, _) ->
      let id = fresh s in
      emit s (Ir.Global_ptr (id, name, storage_ty global_ty));
      Ok (Ir.Local (id, Ir.Pointer (storage_ty global_ty)))
  | Hir.Local (local, sp) -> (
      match Hashtbl.find_opt s.env local.id with
      | Some (value, _) -> Ok value
      | None -> error sp ("unknown local `" ^ local.name ^ "`"))
  | Hir.Const_array (n, t, _) ->
      let id = fresh s in
      emit s (Ir.Global_ptr (id, n, storage_ty t));
      Ok (Ir.Local (id, Ir.Pointer (storage_ty t)))
  | Hir.Raw_select (b, off, _, _) -> raw_address s b off
  | Hir.Index (a, i, _, _) -> index_address s a i
  | Hir.Field (a, _, _, off, _) -> field_address s a off
  | _ -> materialize s e

and index_address s a i =
  match Hir.expr_ty a with
  | Hir.Array _ ->
      let* p = address s a in
      let* iv = expr s i in
      let* iv = normalize_index s i iv in
      let id = fresh s in
      emit s (Ir.Gep (id, ty (Hir.expr_ty a), p, [ Ir.Zero; Ir.Index iv ]));
      Ok (Ir.Local (id, Ir.Pointer Ir.I8))
  | _ -> error (Hir.expr_span a) "cannot take index address"

and raw_address s base off =
  let* bv = expr s base in
  let* ov = expr s off in
  let id = fresh s in
  emit s (Ir.Gep (id, Ir.I8, bv, [ Ir.Index ov ]));
  Ok (Ir.Local (id, Ir.Pointer Ir.I8))

and int_ty_bytes = function
  | Ir.I1 | Ir.I8 -> 1
  | Ir.I16 -> 2
  | Ir.I32 -> 4
  | Ir.F32 -> 4
  | Ir.I64 -> 8
  | Ir.F64 -> 8
  | Ir.I128 -> 16
  | _ -> 8

and volatile_chunk remaining =
  if remaining >= 16 then (16, Ir.I128)
  else if remaining >= 8 then (8, Ir.I64)
  else if remaining >= 4 then (4, Ir.I32)
  else if remaining >= 2 then (2, Ir.I16)
  else (1, Ir.I8)

and volatile_offset s pointer offset =
  if offset = 0 then Ok pointer
  else
    let id = fresh s in
    emit s
      (Ir.Gep (id, Ir.I8, pointer, [ Ir.Index (Ir.Const (Ir.I64, Int64.of_int offset)) ]));
    Ok (Ir.Local (id, Ir.Pointer Ir.I8))

and packed_bool_load s is_volatile lanes pointer =
  let vector_ty = Ir.Vector (lanes, Ir.I1) in
  let bytes = (lanes + 7) / 8 in
  let load id ty pointer =
    if is_volatile then Ir.Load_volatile (id, ty, pointer, 1)
    else Ir.Load (id, ty, pointer, 1)
  in
  let rec load_chunks byte_offset current =
    if byte_offset = bytes then Ok current
    else
      let chunk_bytes, chunk_ty = volatile_chunk (bytes - byte_offset) in
      let bits = chunk_bytes * 8 in
      let* chunk_pointer = volatile_offset s pointer byte_offset in
      let loaded = fresh s in
      emit s (load loaded chunk_ty chunk_pointer);
      let unpacked = fresh s in
      let unpacked_ty = Ir.Vector (bits, Ir.I1) in
      emit s
        (Ir.Cast
           (unpacked, "bitcast", chunk_ty, Ir.Local (loaded, chunk_ty), unpacked_ty));
      let first_lane = byte_offset * 8 in
      let count = min (lanes - first_lane) bits in
      let rec insert_lane i result =
        if i = count then Ok result
        else
          let lane = first_lane + i in
          let extracted = fresh s in
          emit s
            (Ir.Extract
               ( extracted,
                 unpacked_ty,
                 Ir.Local (unpacked, unpacked_ty),
                 Ir.Const (Ir.I64, Int64.of_int i) ));
          let inserted = fresh s in
          emit s
            (Ir.Insert
               ( inserted,
                 vector_ty,
                 result,
                 Ir.Const (Ir.I64, Int64.of_int lane),
                 Ir.Local (extracted, Ir.I1) ));
          insert_lane (i + 1) (Ir.Local (inserted, vector_ty))
      in
      let* result = insert_lane 0 current in
      load_chunks (byte_offset + chunk_bytes) result
  in
  load_chunks 0 (Ir.Undef vector_ty)

and packed_bool_store s is_volatile lanes value pointer =
  let bytes = (lanes + 7) / 8 in
  let store ty value pointer =
    if is_volatile then Ir.Store_volatile (ty, value, pointer, 1)
    else Ir.Store (ty, value, pointer, 1)
  in
  let rec store_chunks byte_offset =
    if byte_offset = bytes then Ok ()
    else
      let chunk_bytes, chunk_ty = volatile_chunk (bytes - byte_offset) in
      let bits = chunk_bytes * 8 in
      let first_lane = byte_offset * 8 in
      let lane_count = min bits (lanes - first_lane) in
      let mask =
        Ir.Const_vector
          ( Ir.Vector (bits, Ir.I32),
            List.init bits (fun i ->
                Int64.of_int (if first_lane + i < lanes then first_lane + i else 0)) )
      in
      let packed = fresh s in
      let packed_ty = Ir.Vector (bits, Ir.I1) in
      emit s (Ir.Shufflevector (packed, packed_ty, value, value, mask));
      let packed_value = Ir.Local (packed, packed_ty) in
      let* packed_value =
        if lane_count = bits then Ok packed_value
        else
          let valid_lanes =
            Ir.Const_vector
              (packed_ty, List.init bits (fun i -> if i < lane_count then 1L else 0L))
          in
          let masked = fresh s in
          emit s (Ir.Bin (masked, Ir.And, packed_ty, packed_value, valid_lanes));
          Ok (Ir.Local (masked, packed_ty))
      in
      let stored = fresh s in
      emit s (Ir.Cast (stored, "bitcast", packed_ty, packed_value, chunk_ty));
      let* chunk_pointer = volatile_offset s pointer byte_offset in
      emit s (store chunk_ty (Ir.Local (stored, chunk_ty)) chunk_pointer);
      store_chunks (byte_offset + chunk_bytes)
  in
  store_chunks 0

and volatile_load s span access_ty pointer =
  match access_ty with
  | Hir.Bool ->
      let loaded = fresh s in
      emit s (Ir.Load_volatile (loaded, Ir.I8, pointer, 1));
      let normalized = fresh s in
      emit s
        (Ir.Cmp
           (normalized, Ir.Ne, Ir.I8, Ir.Local (loaded, Ir.I8), Ir.Const (Ir.I8, 0L)));
      Ok (Ir.Local (normalized, Ir.I1))
  | Hir.Int _ ->
      let loaded = fresh s in
      emit s (Ir.Load_volatile (loaded, ty access_ty, pointer, 1));
      Ok (Ir.Local (loaded, ty access_ty))
  | Hir.Float _ ->
      let loaded = fresh s in
      emit s (Ir.Load_volatile (loaded, ty access_ty, pointer, 1));
      Ok (Ir.Local (loaded, ty access_ty))
  | Hir.Addr | Hir.Handle _ ->
      let loaded = fresh s in
      emit s (Ir.Load_volatile (loaded, Ir.Pointer Ir.I8, pointer, 1));
      Ok (Ir.Local (loaded, Ir.Pointer Ir.I8))
  | Hir.Vec (lanes, Hir.Int kind) ->
      let elem_ty = ty (Hir.Int kind) in
      let vector_ty = Ir.Vector (lanes, elem_ty) in
      let elem_bytes = int_ty_bytes elem_ty in
      let rec load_lane i current =
        if i = lanes then Ok current
        else
          let* lane_pointer = volatile_offset s pointer (i * elem_bytes) in
          let loaded = fresh s in
          emit s (Ir.Load_volatile (loaded, elem_ty, lane_pointer, 1));
          let inserted = fresh s in
          emit s
            (Ir.Insert
               ( inserted,
                 vector_ty,
                 current,
                 Ir.Const (Ir.I64, Int64.of_int i),
                 Ir.Local (loaded, elem_ty) ));
          load_lane (i + 1) (Ir.Local (inserted, vector_ty))
      in
      load_lane 0 (Ir.Undef vector_ty)
  | Hir.Vec (lanes, Hir.Bool) -> packed_bool_load s true lanes pointer
  | _ -> error span "internal error: invalid volatile access type"

and volatile_store s span access_ty value pointer =
  match access_ty with
  | Hir.Bool ->
      let stored = fresh s in
      emit s (Ir.Cast (stored, "zext", Ir.I1, value, Ir.I8));
      emit s (Ir.Store_volatile (Ir.I8, Ir.Local (stored, Ir.I8), pointer, 1));
      Ok ()
  | Hir.Int _ ->
      emit s (Ir.Store_volatile (ty access_ty, value, pointer, 1));
      Ok ()
  | Hir.Float _ ->
      emit s (Ir.Store_volatile (ty access_ty, value, pointer, 1));
      Ok ()
  | Hir.Addr | Hir.Handle _ ->
      emit s (Ir.Store_volatile (Ir.Pointer Ir.I8, value, pointer, 1));
      Ok ()
  | Hir.Vec (lanes, Hir.Int kind) ->
      let elem_ty = ty (Hir.Int kind) in
      let elem_bytes = int_ty_bytes elem_ty in
      let vector_ty = Ir.Vector (lanes, elem_ty) in
      let rec store_lane i =
        if i = lanes then Ok ()
        else
          let extracted = fresh s in
          emit s
            (Ir.Extract (extracted, vector_ty, value, Ir.Const (Ir.I64, Int64.of_int i)));
          let* lane_pointer = volatile_offset s pointer (i * elem_bytes) in
          emit s
            (Ir.Store_volatile (elem_ty, Ir.Local (extracted, elem_ty), lane_pointer, 1));
          store_lane (i + 1)
      in
      store_lane 0
  | Hir.Vec (lanes, Hir.Bool) -> packed_bool_store s true lanes value pointer
  | _ -> error span "internal error: invalid volatile access type"

and simd_memory_address s span stride base index_ty index_value =
  let* offset = normalize_index_type s index_ty span index_value in
  let pointer_int = ty (Hir.Int Hir.Usize) in
  let offset =
    match stride with
    | None | Some 1 -> offset
    | Some bytes ->
        let id = fresh s in
        emit s
          (Ir.Bin
             ( id,
               Ir.Mul,
               pointer_int,
               offset,
               Ir.Const (pointer_int, Int64.of_int bytes) ));
        Ir.Local (id, pointer_int)
  in
  match stride with
  | Some _ ->
      let id = fresh s in
      emit s (Ir.Gep (id, Ir.I8, base, [ Ir.Index offset ]));
      Ok (Ir.Local (id, Ir.Pointer Ir.I8))
  | None ->
      let bits = fresh s in
      emit s (Ir.Cast (bits, "ptrtoint", Ir.Pointer Ir.I8, base, pointer_int));
      let sum = fresh s in
      emit s (Ir.Bin (sum, Ir.Add, pointer_int, Ir.Local (bits, pointer_int), offset));
      let pointer = fresh s in
      emit s
        (Ir.Cast
           ( pointer,
             "inttoptr",
             pointer_int,
             Ir.Local (sum, pointer_int),
             Ir.Pointer Ir.I8 ));
      Ok (Ir.Local (pointer, Ir.Pointer Ir.I8))

and simd_memory_load_lane s span access_ty pointer =
  match access_ty with
  | Hir.Bool ->
      let loaded = fresh s in
      emit s (Ir.Load (loaded, Ir.I8, pointer, 1));
      let normalized = fresh s in
      emit s
        (Ir.Cmp
           (normalized, Ir.Ne, Ir.I8, Ir.Local (loaded, Ir.I8), Ir.Const (Ir.I8, 0L)));
      Ok (Ir.Local (normalized, Ir.I1))
  | Hir.Int _ ->
      let loaded = fresh s in
      emit s (Ir.Load (loaded, ty access_ty, pointer, 1));
      Ok (Ir.Local (loaded, ty access_ty))
  | Hir.Float _ ->
      let loaded = fresh s in
      emit s (Ir.Load (loaded, ty access_ty, pointer, 1));
      Ok (Ir.Local (loaded, ty access_ty))
  | _ -> error span "internal error: invalid SIMD memory element type"

and simd_memory_store_lane s span access_ty value pointer =
  match access_ty with
  | Hir.Bool ->
      let byte = fresh s in
      emit s (Ir.Cast (byte, "zext", Ir.I1, value, Ir.I8));
      emit s (Ir.Store (Ir.I8, Ir.Local (byte, Ir.I8), pointer, 1));
      Ok ()
  | Hir.Int _ ->
      emit s (Ir.Store (ty access_ty, value, pointer, 1));
      Ok ()
  | Hir.Float _ ->
      emit s (Ir.Store (ty access_ty, value, pointer, 1));
      Ok ()
  | _ -> error span "internal error: invalid SIMD memory element type"

and masked_elem_ty access_ty =
  match access_ty with Hir.Bool -> Ir.I8 | _ -> ty access_ty

and masked_mask_lanes span mask =
  match value_ty mask with
  | Ir.Vector (lanes, Ir.I1) -> Ok lanes
  | _ -> error span "internal error: masked SIMD memory mask is not a bool vector"

and masked_intrinsic span direction memory_ty =
  let* suffix =
    match memory_ty with
    | Ir.Vector (_, (Ir.F32 | Ir.F64)) -> float_suffix span memory_ty
    | _ -> intrinsic_suffix span memory_ty
  in
  Ok (Printf.sprintf "llvm.masked.%s.%s.p0" direction suffix)

and simd_masked_load s span access_ty base mask fallback =
  let* lanes = masked_mask_lanes span mask in
  let mask_ty = Ir.Vector (lanes, Ir.I1) in
  let memory_ty = Ir.Vector (lanes, masked_elem_ty access_ty) in
  let* name = masked_intrinsic span "load" memory_ty in
  let passthru =
    match access_ty with
    | Hir.Bool ->
        let id = fresh s in
        emit s (Ir.Cast (id, "zext", value_ty fallback, fallback, memory_ty));
        Ir.Local (id, memory_ty)
    | _ -> fallback
  in
  let loaded = fresh s in
  emit s
    (Ir.Call
       ( Some loaded,
         Ir.No_extension,
         memory_ty,
         name,
         [
           (Ir.Pointer Ir.I8, Ir.No_extension, base);
           (Ir.I32, Ir.No_extension, Ir.Const (Ir.I32, 1L));
           (mask_ty, Ir.No_extension, mask);
           (memory_ty, Ir.No_extension, passthru);
         ] ));
  match access_ty with
  | Hir.Bool ->
      let normalized = fresh s in
      emit s
        (Ir.Cmp
           ( normalized,
             Ir.Ne,
             memory_ty,
             Ir.Local (loaded, memory_ty),
             Ir.Zero memory_ty ));
      Ok (Ir.Local (normalized, mask_ty))
  | _ -> Ok (Ir.Local (loaded, memory_ty))

and simd_masked_store s span access_ty base mask stored =
  let* lanes = masked_mask_lanes span mask in
  let mask_ty = Ir.Vector (lanes, Ir.I1) in
  let memory_ty = Ir.Vector (lanes, masked_elem_ty access_ty) in
  let* name = masked_intrinsic span "store" memory_ty in
  let value =
    match access_ty with
    | Hir.Bool ->
        let id = fresh s in
        emit s (Ir.Cast (id, "zext", value_ty stored, stored, memory_ty));
        Ir.Local (id, memory_ty)
    | _ -> stored
  in
  emit s
    (Ir.Call
       ( None,
         Ir.No_extension,
         Ir.Void,
         name,
         [
           (memory_ty, Ir.No_extension, value);
           (Ir.Pointer Ir.I8, Ir.No_extension, base);
           (Ir.I32, Ir.No_extension, Ir.Const (Ir.I32, 1L));
           (mask_ty, Ir.No_extension, mask);
         ] ));
  Ok ()

and simd_memory_load s span access_ty kind args values =
  match (kind, args, values) with
  | Hir.Masked, [ _; _; _ ], [ base; mask; fallback ] when s.intrinsic_masked_loads ->
      simd_masked_load s span access_ty base mask fallback
  | Hir.Masked, [ _; _; _ ], [ base; mask; fallback ] ->
      simd_lane_load s span access_ty None (Hir.Int Hir.Usize) base mask fallback
        (Some (int_ty_bytes (ty access_ty)))
  | ( (Hir.Gather | Hir.Gather_bytes),
      [ _; index_expr; _; _ ],
      [ base; indices; mask; fallback ] ) ->
      let stride =
        if kind = Hir.Gather then Some (int_ty_bytes (ty access_ty)) else None
      in
      simd_lane_load s span access_ty (Some indices) (Hir.expr_ty index_expr) base mask
        fallback stride
  | _ -> error span "internal error: invalid SIMD memory load"

and simd_lane_load s span access_ty indices index_ty base mask fallback stride =
  let lanes = match value_ty mask with Ir.Vector (n, Ir.I1) -> n | _ -> 0 in
  let result_ty = Ir.Vector (lanes, ty access_ty) in
  let rec load_lane lane result =
    if lane = lanes then Ok result
    else
      let lane_index = Ir.Const (Ir.I64, Int64.of_int lane) in
      let mask_id = fresh s in
      emit s (Ir.Extract (mask_id, value_ty mask, mask, lane_index));
      let fallback_id = fresh s in
      emit s (Ir.Extract (fallback_id, value_ty fallback, fallback, lane_index));
      let inactive = s.current.id in
      let active = fresh_block s and join = fresh_block s in
      s.current.term := Some (Ir.CondBr (Ir.Local (mask_id, Ir.I1), active.id, join.id));
      enter s active;
      let index_value =
        match indices with
        | None -> Ir.Const (ty (Hir.Int Hir.Usize), Int64.of_int lane)
        | Some vector ->
            let index_id = fresh s in
            emit s (Ir.Extract (index_id, value_ty vector, vector, lane_index));
            Ir.Local
              ( index_id,
                match value_ty vector with Ir.Vector (_, elem) -> elem | _ -> Ir.I64 )
      in
      let* pointer = simd_memory_address s span stride base index_ty index_value in
      let* loaded = simd_memory_load_lane s span access_ty pointer in
      let active_end = s.current.id in
      s.current.term := Some (Ir.Br join.id);
      enter s join;
      let merged = fresh s in
      emit s
        (Ir.Phi
           ( merged,
             ty access_ty,
             [ (loaded, active_end); (Ir.Local (fallback_id, ty access_ty), inactive) ]
           ));
      let inserted = fresh s in
      emit s
        (Ir.Insert
           (inserted, result_ty, result, lane_index, Ir.Local (merged, ty access_ty)));
      load_lane (lane + 1) (Ir.Local (inserted, result_ty))
  in
  load_lane 0 (Ir.Zero result_ty)

and simd_memory_store s span access_ty kind args values =
  match (kind, args, values) with
  | Hir.Masked_store, [ _; _; _ ], [ base; mask; stored ] ->
      simd_masked_store s span access_ty base mask stored
  | ( (Hir.Scatter | Hir.Scatter_bytes),
      [ _; index_expr; _; _ ],
      [ base; indices; mask; stored ] ) ->
      let stride =
        if kind = Hir.Scatter then Some (int_ty_bytes (ty access_ty)) else None
      in
      let index_ty = Hir.expr_ty index_expr in
      let lanes = match value_ty mask with Ir.Vector (n, Ir.I1) -> n | _ -> 0 in
      let rec store_lane lane =
        if lane = lanes then Ok ()
        else
          let lane_index = Ir.Const (Ir.I64, Int64.of_int lane) in
          let mask_id = fresh s in
          emit s (Ir.Extract (mask_id, value_ty mask, mask, lane_index));
          let active = fresh_block s and join = fresh_block s in
          s.current.term :=
            Some (Ir.CondBr (Ir.Local (mask_id, Ir.I1), active.id, join.id));
          enter s active;
          let index_id = fresh s in
          emit s (Ir.Extract (index_id, value_ty indices, indices, lane_index));
          let index_value =
            Ir.Local
              ( index_id,
                match value_ty indices with Ir.Vector (_, elem) -> elem | _ -> Ir.I64 )
          in
          let* pointer = simd_memory_address s span stride base index_ty index_value in
          let value_id = fresh s in
          emit s (Ir.Extract (value_id, value_ty stored, stored, lane_index));
          let* () =
            simd_memory_store_lane s span access_ty
              (Ir.Local (value_id, ty access_ty))
              pointer
          in
          s.current.term := Some (Ir.Br join.id);
          enter s join;
          store_lane (lane + 1)
      in
      store_lane 0
  | _ -> error span "internal error: invalid SIMD memory store"

and raw_load s t p =
  match t with
  | Hir.Float _ ->
      let id = fresh s in
      emit s (Ir.Load (id, ty t, p, 1));
      Ok (Ir.Local (id, ty t))
  | Hir.Int k ->
      let it = ty (Hir.Int k) in
      let id = fresh s in
      emit s (Ir.Load (id, it, p, 1));
      Ok (Ir.Local (id, it))
  | Hir.Bool ->
      let id = fresh s in
      emit s (Ir.Load (id, Ir.I8, p, 1));
      let cid = fresh s in
      emit s (Ir.Cmp (cid, Ir.Ne, Ir.I8, Ir.Local (id, Ir.I8), Ir.Const (Ir.I8, 0L)));
      Ok (Ir.Local (cid, Ir.I1))
  | Hir.Addr | Hir.Handle _ ->
      let pt = Ir.Pointer Ir.I8 in
      let id = fresh s in
      emit s (Ir.Load (id, pt, p, 1));
      Ok (Ir.Local (id, pt))
  | Hir.Vec (lanes, Hir.Int k) ->
      let et = ty (Hir.Int k) in
      let eb = int_ty_bytes et in
      let vt = Ir.Vector (lanes, et) in
      let rec go i cur =
        if i = lanes then Ok cur
        else
          let off_id = fresh s in
          emit s
            (Ir.Gep
               ( off_id,
                 Ir.I8,
                 p,
                 [ Ir.Index (Ir.Const (Ir.I64, Int64.of_int (i * eb))) ] ));
          let lid = fresh s in
          emit s (Ir.Load (lid, et, Ir.Local (off_id, Ir.Pointer Ir.I8), 1));
          let iid = fresh s in
          emit s
            (Ir.Insert
               (iid, vt, cur, Ir.Const (Ir.I64, Int64.of_int i), Ir.Local (lid, et)));
          go (i + 1) (Ir.Local (iid, vt))
      in
      go 0 (Ir.Undef vt)
  | Hir.Vec (_, Hir.Float _) ->
      let id = fresh s in
      let vector_ty = ty t in
      emit s (Ir.Load (id, vector_ty, p, 1));
      Ok (Ir.Local (id, vector_ty))
  | Hir.Vec (lanes, Hir.Bool) -> packed_bool_load s false lanes p
  | _ -> error Span.synthetic "internal error: aggregate raw load"

and raw_store s t v p =
  match t with
  | Hir.Float _ ->
      emit s (Ir.Store (ty t, v, p, 1));
      Ok ()
  | Hir.Int k ->
      let it = ty (Hir.Int k) in
      emit s (Ir.Store (it, v, p, 1));
      Ok ()
  | Hir.Bool ->
      let zid = fresh s in
      emit s (Ir.Cast (zid, "zext", Ir.I1, v, Ir.I8));
      emit s (Ir.Store (Ir.I8, Ir.Local (zid, Ir.I8), p, 1));
      Ok ()
  | Hir.Addr | Hir.Handle _ ->
      emit s (Ir.Store (Ir.Pointer Ir.I8, v, p, 1));
      Ok ()
  | Hir.Vec (lanes, Hir.Int k) ->
      let et = ty (Hir.Int k) in
      let eb = int_ty_bytes et in
      let vt = Ir.Vector (lanes, et) in
      let rec go i =
        if i = lanes then Ok ()
        else
          let xid = fresh s in
          emit s (Ir.Extract (xid, vt, v, Ir.Const (Ir.I64, Int64.of_int i)));
          let off_id = fresh s in
          emit s
            (Ir.Gep
               ( off_id,
                 Ir.I8,
                 p,
                 [ Ir.Index (Ir.Const (Ir.I64, Int64.of_int (i * eb))) ] ));
          emit s
            (Ir.Store (et, Ir.Local (xid, et), Ir.Local (off_id, Ir.Pointer Ir.I8), 1));
          go (i + 1)
      in
      go 0
  | Hir.Vec (_, Hir.Float _) ->
      emit s (Ir.Store (ty t, v, p, 1));
      Ok ()
  | Hir.Vec (lanes, Hir.Bool) -> packed_bool_store s false lanes v p
  | _ -> error Span.synthetic "internal error: aggregate raw store"

and store_value s t value pointer alignment =
  match t with
  | Hir.Bool ->
      let byte = fresh s in
      emit s (Ir.Cast (byte, "zext", Ir.I1, value, Ir.I8));
      emit s (Ir.Store (Ir.I8, Ir.Local (byte, Ir.I8), pointer, alignment));
      Ok ()
  | Hir.Vec (lanes, Hir.Bool) when lanes mod 8 <> 0 -> raw_store s t value pointer
  | _ ->
      emit s (Ir.Store (ty t, value, pointer, alignment));
      Ok ()

and field_address s a off =
  let* p =
    if
      match a with
      | Hir.Local _ | Global _ | Index _ | Field _ | Const_array _ -> true
      | _ -> false
    then address s a
    else materialize s a
  in
  let id = fresh s in
  emit s (Ir.Gep (id, Ir.I8, p, [ Ir.Index (Ir.Const (Ir.I64, Int64.of_int off)) ]));
  Ok (Ir.Local (id, Ir.Pointer Ir.I8))

let copy_offset s pointer offset =
  let id = fresh s in
  emit s
    (Ir.Gep (id, Ir.I8, pointer, [ Ir.Index (Ir.Const (Ir.I64, Int64.of_int offset)) ]));
  Ok (Ir.Local (id, Ir.Pointer Ir.I8))

let capped_add left right = if left > 16 - right then 17 else left + right

let capped_mul left right =
  if left = 0 || right = 0 then 0 else if left > 16 / right then 17 else left * right

let rec copy_leaf_count structs = function
  | Hir.Struct name -> (
      match
        List.find_opt
          (fun (definition : Hir.struct_def) -> definition.name = name)
          structs
      with
      | None -> 17
      | Some definition ->
          List.fold_left
            (fun count (field : Hir.field) ->
              capped_add count (copy_leaf_count structs field.ty))
            0 definition.fields)
  | Hir.Array (length, element_ty) ->
      capped_mul length (copy_leaf_count structs element_ty)
  | Hir.Bool | Hir.Int _ | Hir.Float _ | Hir.Addr | Hir.Handle _ | Hir.Vec _ -> 1
  | _ -> 17

let rec copy_place s span ty destination source =
  match ty with
  | Hir.Struct name -> (
      match
        List.find_opt
          (fun (definition : Hir.struct_def) -> definition.name = name)
          s.structs
      with
      | None ->
          error span (Printf.sprintf "internal error: unknown struct `%s` in copy" name)
      | Some definition ->
          if definition.is_union then
            copy_array_loop s span definition.size 1 (Hir.Int Hir.U8) destination source
          else
            let rec fields = function
              | [] -> Ok ()
              | (field : Hir.field) :: rest ->
                  let* destination_field = copy_offset s destination field.offset in
                  let* source_field = copy_offset s source field.offset in
                  let* () = copy_place s span field.ty destination_field source_field in
                  fields rest
            in
            fields definition.fields)
  | Hir.Array (length, element_ty) ->
      let* stride, _ =
        match Hir.layout s.structs element_ty with
        | Ok layout -> Ok layout
        | Error message ->
            error span
              (Printf.sprintf "internal error: invalid array element in copy: %s"
                 message)
      in
      let leaves = capped_mul length (copy_leaf_count s.structs element_ty) in
      if leaves = 0 then Ok ()
      else if leaves > 16 then
        copy_array_loop s span length stride element_ty destination source
      else
        let rec elements index =
          if index = length then Ok ()
          else
            let offset = index * stride in
            let* destination_element = copy_offset s destination offset in
            let* source_element = copy_offset s source offset in
            let* () = copy_place s span element_ty destination_element source_element in
            elements (index + 1)
        in
        elements 0
  | Hir.Bool | Hir.Int _ | Hir.Float _ | Hir.Addr | Hir.Handle _ | Hir.Vec _ ->
      let* value = raw_load s ty source in
      raw_store s ty value destination
  | _ -> error span "internal error: invalid leaf type in aggregate copy"

and copy_array_loop s span length stride element_ty destination source =
  let preheader = s.current.id in
  let head = fresh_block s
  and body = fresh_block s
  and latch = fresh_block s
  and exit = fresh_block s in
  let index = fresh s in
  let next_index = fresh s in
  s.current.term := Some (Ir.Br head.id);
  enter s head;
  emit s
    (Ir.Phi
       ( index,
         Ir.I64,
         [
           (Ir.Const (Ir.I64, 0L), preheader); (Ir.Local (next_index, Ir.I64), latch.id);
         ] ));
  let condition = fresh s in
  emit s
    (Ir.Cmp
       ( condition,
         Ir.Ult,
         Ir.I64,
         Ir.Local (index, Ir.I64),
         Ir.Const (Ir.I64, Int64.of_int length) ));
  s.current.term := Some (Ir.CondBr (Ir.Local (condition, Ir.I1), body.id, exit.id));
  enter s body;
  let offset =
    if stride = 1 then Ir.Local (index, Ir.I64)
    else
      let scaled = fresh s in
      emit s
        (Ir.Bin
           ( scaled,
             Ir.Mul,
             Ir.I64,
             Ir.Local (index, Ir.I64),
             Ir.Const (Ir.I64, Int64.of_int stride) ));
      Ir.Local (scaled, Ir.I64)
  in
  let destination_element = fresh s in
  emit s (Ir.Gep (destination_element, Ir.I8, destination, [ Ir.Index offset ]));
  let source_element = fresh s in
  emit s (Ir.Gep (source_element, Ir.I8, source, [ Ir.Index offset ]));
  let* () =
    copy_place s span element_ty
      (Ir.Local (destination_element, Ir.Pointer Ir.I8))
      (Ir.Local (source_element, Ir.Pointer Ir.I8))
  in
  if open_block s then s.current.term := Some (Ir.Br latch.id);
  enter s latch;
  emit s
    (Ir.Bin (next_index, Ir.Add, Ir.I64, Ir.Local (index, Ir.I64), Ir.Const (Ir.I64, 1L)));
  s.current.term := Some (Ir.Br head.id);
  enter s exit;
  Ok ()

let rec emit_defer_body s body =
  List.fold_left
    (fun r st ->
      let* () = r in
      if open_block s then stmt s st else Ok ())
    (Ok ()) body

and emit_scope_defers s idx =
  match List.nth_opt s.defer_scopes idx with
  | None -> Ok ()
  | Some ds ->
      List.fold_left
        (fun r body ->
          let* () = r in
          emit_defer_body s body)
        (Ok ()) ds

and emit_lifetime s operation slot =
  emit s
    (Ir.Call
       ( None,
         Ir.No_extension,
         Ir.Void,
         "llvm.lifetime." ^ operation ^ ".p0",
         [
           (Ir.I64, Ir.No_extension, Ir.Const (Ir.I64, Int64.of_int slot.size));
           (Ir.value_ty slot.pointer, Ir.No_extension, slot.pointer);
         ] ));
  Ok ()

and emit_scope_lifetimes s slots =
  let rec emit_all = function
    | [] -> Ok ()
    | slot :: rest ->
        let* () = emit_lifetime s "end" slot in
        emit_all rest
  in
  emit_all slots

and emit_current_scope_lifetimes s =
  match s.scope_slots with
  | slots :: _ -> emit_scope_lifetimes s slots
  | [] -> error Span.synthetic "internal error: lifetime scope stack is empty"

and register_local_lifetime s local pointer =
  if (not s.sanitize_address) || not (Hashtbl.mem s.address_taken local.Hir.id) then
    Ok ()
  else
    match Hir.layout s.structs local.ty with
    | Error message ->
        error Span.synthetic
          (Printf.sprintf "internal error: local `%s` has no layout: %s" local.name
             message)
    | Ok (size, _) when size = 0 -> Ok ()
    | Ok (size, _) -> (
        match s.scope_slots with
        | slots :: rest ->
            let slot = { pointer; size } in
            let* () = emit_lifetime s "start" slot in
            s.scope_slots <- (slot :: slots) :: rest;
            Ok ()
        | [] -> error Span.synthetic "internal error: lifetime scope stack is empty")

and unwind s keep =
  let count = List.length s.defer_scopes - keep in
  let scopes =
    let rec take n xs =
      if n <= 0 then [] else match xs with [] -> [] | x :: xt -> x :: take (n - 1) xt
    in
    take count s.defer_scopes
  in
  let lifetimes =
    let rec take n = function
      | _ when n <= 0 -> []
      | [] -> []
      | scope :: rest -> scope :: take (n - 1) rest
    in
    take count s.scope_slots
  in
  let rec unwind_scopes scopes lifetimes =
    match (scopes, lifetimes) with
    | [], [] -> Ok ()
    | ds :: scope_rest, slots :: lifetime_rest ->
        let* () =
          List.fold_left
            (fun r body ->
              let* () = r in
              emit_defer_body s body)
            (Ok ()) ds
        in
        let* () = emit_scope_lifetimes s slots in
        unwind_scopes scope_rest lifetime_rest
    | _ -> error Span.synthetic "internal error: lifetime and defer scope mismatch"
  in
  unwind_scopes scopes lifetimes

and scoped s xs =
  push_scope s;
  let* () = stmt_list s xs in
  let* () = if open_block s then emit_scope_defers s 0 else Ok () in
  let* () = if open_block s then emit_current_scope_lifetimes s else Ok () in
  pop_scope s;
  Ok ()

and stmt s statement =
  mark s (Hir.stmt_span statement);
  match statement with
  | Hir.Let (local, init, _) -> (
      let* alignment = align s local.ty in
      let id = fresh s in
      emit_entry s (Ir.Alloca (id, storage_ty local.ty, alignment));
      let p = Ir.Local (id, Ir.Pointer (storage_ty local.ty)) in
      let* () = register_local_lifetime s local p in
      bind_local s local p alignment;
      match init with
      | None -> Ok ()
      | Some e ->
          let* v = expr s e in
          store_value s local.ty v p alignment)
  | Hir.Let_construct (local, construction, _) ->
      let* alignment = align s local.ty in
      let id = fresh s in
      emit_entry s (Ir.Alloca (id, storage_ty local.ty, alignment));
      let pointer = Ir.Local (id, Ir.Pointer (storage_ty local.ty)) in
      let* () = register_local_lifetime s local pointer in
      bind_local s local pointer alignment;
      construct_into ~alignment s pointer construction
  | Hir.View (local, place, span) ->
      let* pointer = address s place in
      let* alignment = place_alignment s place in
      if match value_ty pointer with Ir.Pointer _ -> true | _ -> false then (
        bind_local s local pointer alignment;
        Ok ())
      else error span "internal error: view address has the wrong type"
  | Hir.Copy (destination, source, copy_ty, scratch_free, span) ->
      let* destination_pointer = address s destination in
      let* source_pointer = address s source in
      if scratch_free then copy_place s span copy_ty destination_pointer source_pointer
      else
        let* size, _ =
          match Hir.layout s.structs copy_ty with
          | Ok layout -> Ok layout
          | Error message ->
              error span
                (Printf.sprintf "internal error: invalid copy layout: %s" message)
        in
        if size = 0 then Ok ()
        else
          let* alignment = align s copy_ty in
          let scratch_id = fresh s in
          emit_entry s (Ir.Alloca (scratch_id, storage_ty copy_ty, alignment));
          let scratch = Ir.Local (scratch_id, Ir.Pointer (storage_ty copy_ty)) in
          let* () = copy_place s span copy_ty scratch source_pointer in
          copy_place s span copy_ty destination_pointer scratch
  | Hir.Volatile_store (access_ty, pointer_expr, value_expr, span) ->
      let* pointer = expr s pointer_expr in
      let* value = expr s value_expr in
      volatile_store s span access_ty value pointer
  | Hir.Simd_store (access_ty, kind, args, span) ->
      let* values = exprs s args in
      simd_memory_store s span access_ty kind args values
  | Hir.Assign (target, e, _) -> (
      match target with
      | Hir.AIndex (a, i) when match Hir.expr_ty a with Hir.Vec _ -> true | _ -> false
        ->
          let* p, source_ty, vt, iv, alignment = vector_lane s a i in
          let* x = expr s e in
          let loaded_id = fresh s in
          emit s (Ir.Load (loaded_id, vt, p, alignment));
          let loaded = Ir.Local (loaded_id, vt) in
          let ins = fresh s in
          emit s (Ir.Insert (ins, vt, loaded, iv, x));
          store_value s source_ty (Ir.Local (ins, vt)) p alignment
      | Hir.ARaw (b, off, at) ->
          let* p = raw_address s b off in
          let* v = expr s e in
          raw_store s at v p
      | _ ->
          let* p = target_address s target in
          let* v = expr s e in
          let t = Hir.expr_ty e in
          let* alignment = target_alignment s target t in
          store_value s t v p alignment)
  | Hir.Compound_assign (target, op, e, t, span) -> (
      match target with
      | Hir.AIndex (a, i) when match Hir.expr_ty a with Hir.Vec _ -> true | _ -> false
        ->
          let* p, source_ty, vt, iv, alignment = vector_lane s a i in
          let loaded_id = fresh s in
          emit s (Ir.Load (loaded_id, vt, p, alignment));
          let loaded = Ir.Local (loaded_id, vt) in
          let eid = fresh s in
          emit s (Ir.Extract (eid, vt, loaded, iv));
          let it = ty t in
          let old = Ir.Local (eid, it) in
          let* rhs = expr s e in
          let* value = emit_binary s span t op it old rhs in
          let latest_id = fresh s in
          emit s (Ir.Load (latest_id, vt, p, alignment));
          let latest = Ir.Local (latest_id, vt) in
          let ins = fresh s in
          emit s (Ir.Insert (ins, vt, latest, iv, value));
          store_value s source_ty (Ir.Local (ins, vt)) p alignment
      | Hir.ARaw (b, off, at) ->
          let* p = raw_address s b off in
          let* old = raw_load s at p in
          let* rhs = expr s e in
          let* value =
            match (at, op) with
            | Hir.Addr, Ast.Add -> address_step s Ir.Add old rhs
            | Hir.Addr, Ast.Sub -> address_step s Ir.Sub old rhs
            | _ -> emit_binary s span t op (ty at) old rhs
          in
          raw_store s at value p
      | _ ->
          let* p = target_address s target in
          let it = ty t in
          let* alignment = target_alignment s target t in
          let* old = load_value s t p alignment in
          let* rhs = expr s e in
          let* value =
            match (t, op) with
            | Hir.Addr, Ast.Add -> address_step s Ir.Add old rhs
            | Hir.Addr, Ast.Sub -> address_step s Ir.Sub old rhs
            | _ -> emit_binary s span t op it old rhs
          in
          store_value s t value p alignment)
  | Hir.Expr (e, _) ->
      let* _ = expr s e in
      Ok ()
  | Hir.Return (e, _) ->
      let* v =
        match e with
        | None -> Ok None
        | Some x ->
            let* y = expr s x in
            Ok (Some (ty (Hir.expr_ty x), y))
      in
      let* () = unwind s 0 in
      s.current.term := Some (Ir.Ret v);
      Ok ()
  | Hir.Block (xs, _) -> scoped s xs
  | Hir.If (c, a, b, _) -> lower_if s c a b
  | Hir.Defer (xs, _) -> (
      match s.defer_scopes with
      | ds :: rest ->
          s.defer_scopes <- (xs :: ds) :: rest;
          Ok ()
      | [] -> error Span.synthetic "defer outside scope")
  | Hir.While (label, c, b, sp) -> lower_while s label c b sp
  | Hir.For (label, i, c, st, b, sp) -> lower_for s label i c st b sp
  | Hir.Switch (e, arms, d, span) -> lower_switch s e arms d span
  | Hir.Break (label, sp) -> (
      match target_loop s label with
      | Some l ->
          let* () = unwind s l.keep_scopes in
          s.current.term := Some (Ir.Br l.break_to);
          Ok ()
      | None -> error sp "break outside loop or switch")
  | Hir.Continue (label, sp) -> (
      match target_loop s label with
      | Some l ->
          let* () = unwind s l.keep_scopes in
          mark s sp;
          s.current.term := Some (Ir.Br l.continue_to);
          Ok ()
      | None -> error sp "continue outside loop")

and construct_into ?alignment s destination construction =
  let rec requires_bytewise_zero = function
    | Hir.Init_zero (_, true, _) | Hir.Init_aggregate (_, true, _, _) -> true
    | Hir.Init_aggregate (_, false, entries, _) ->
        List.exists requires_bytewise_zero entries
    | _ -> false
  in
  let literal_ty =
    match construction with Hir.Init_aggregate (t, _, _, _) -> Some t | _ -> None
  in
  match
    Option.bind literal_ty (fun t ->
        Option.map
          (fun value -> (t, value))
          (constant_construction s.structs construction))
  with
  | Some (t, value)
    when copy_leaf_count s.structs t > 16 && not (requires_bytewise_zero construction)
    ->
      let name = ".literal." ^ string_of_int (List.length !(s.literal_globals)) in
      s.literal_globals :=
        {
          Hir.name;
          ty = t;
          init_value = Some value;
          readonly = true;
          linkage = Ast.Internal_global;
        }
        :: !(s.literal_globals);
      let* source = address s (Hir.Global (name, t, Span.synthetic)) in
      copy_place s Span.synthetic t destination source
  | _ -> construct_entries ?alignment s destination construction

and construct_entries ?alignment s destination = function
  | Hir.Init_omitted _ -> Ok ()
  | Hir.Init_aggregate (Hir.Struct name, true, entries, span) -> (
      match find_struct s name with
      | None -> error span ("internal error: unknown construction struct `" ^ name ^ "`")
      | Some definition when definition.is_union ->
          error span "internal error: designated union construction"
      | Some definition when List.length definition.fields <> List.length entries ->
          error span "internal error: designated struct construction arity"
      | Some definition ->
          let* () =
            construct_entries ?alignment s destination
              (Hir.Init_zero (Hir.zero_initializer (Hir.Struct name), true, span))
          in
          let init_span = function
            | Hir.Init_value expression -> Hir.expr_span expression
            | Hir.Init_zero (_, _, span)
            | Hir.Init_omitted (_, span)
            | Hir.Init_aggregate (_, _, _, span) ->
                span
          in
          let ordered =
            List.combine definition.fields entries
            |> List.stable_sort (fun (_, left) (_, right) ->
                Span.compare (init_span left) (init_span right))
          in
          List.fold_left
            (fun result ((field : Hir.field), entry) ->
              let* () = result in
              let* pointer = copy_offset s destination field.offset in
              let* base_alignment =
                match alignment with
                | Some value -> Ok value
                | None -> align s (Hir.Struct name)
              in
              let* field_alignment = align s field.ty in
              construct_into
                ~alignment:
                  (min (offset_alignment base_alignment field.offset) field_alignment)
                s pointer entry)
            (Ok ()) ordered)
  | Hir.Init_value expression ->
      let* value = expr s expression in
      let value_ty = Hir.expr_ty expression in
      let* alignment =
        match alignment with Some value -> Ok value | None -> align s value_ty
      in
      store_value s value_ty value destination alignment
  | Hir.Init_zero (zero, bytewise, span) -> (
      match zero.zero_ty with
      | (Hir.Array _ | Hir.Struct _) as aggregate_ty ->
          let* size, _ =
            match Hir.layout s.structs aggregate_ty with
            | Ok layout -> Ok layout
            | Error message -> error span ("internal error: " ^ message)
          in
          if size <= 256 && not bytewise then
            let* a =
              match alignment with Some a -> Ok a | None -> align s aggregate_ty
            in
            store_value s aggregate_ty (Ir.Zero (ty aggregate_ty)) destination a
          else
            let preheader = s.current.id in
            let head = fresh_block s
            and body = fresh_block s
            and exit = fresh_block s in
            let index = fresh s and next = fresh s in
            s.current.term := Some (Ir.Br head.id);
            enter s head;
            emit s
              (Ir.Phi
                 ( index,
                   Ir.I64,
                   [
                     (Ir.Const (Ir.I64, 0L), preheader);
                     (Ir.Local (next, Ir.I64), body.id);
                   ] ));
            let condition = fresh s in
            emit s
              (Ir.Cmp
                 ( condition,
                   Ir.Ult,
                   Ir.I64,
                   Ir.Local (index, Ir.I64),
                   Ir.Const (Ir.I64, Int64.of_int size) ));
            s.current.term :=
              Some (Ir.CondBr (Ir.Local (condition, Ir.I1), body.id, exit.id));
            enter s body;
            let pointer = fresh s in
            emit s
              (Ir.Gep
                 (pointer, Ir.I8, destination, [ Ir.Index (Ir.Local (index, Ir.I64)) ]));
            emit s
              (Ir.Store
                 (Ir.I8, Ir.Const (Ir.I8, 0L), Ir.Local (pointer, Ir.Pointer Ir.I8), 1));
            emit s
              (Ir.Bin
                 (next, Ir.Add, Ir.I64, Ir.Local (index, Ir.I64), Ir.Const (Ir.I64, 1L)));
            s.current.term := Some (Ir.Br head.id);
            enter s exit;
            Ok ()
      | _ -> error span "internal error: zero initializer requires an array or struct")
  | Hir.Init_aggregate (Hir.Array (length, element_ty), false, elements, span) ->
      if List.length elements <> length then
        error span "internal error: array construction arity"
      else
        let aggregate_ty = Ir.Array (length, storage_ty element_ty) in
        let* base_alignment =
          match alignment with
          | Some value -> Ok value
          | None -> align s (Hir.Array (length, element_ty))
        in
        let* element_size, _ =
          match Hir.layout s.structs element_ty with
          | Ok layout -> Ok layout
          | Error message -> error span ("internal error: " ^ message)
        in
        let* element_natural_alignment = align s element_ty in
        let rec go index = function
          | [] -> Ok ()
          | element :: rest ->
              let pointer_id = fresh s in
              emit s
                (Ir.Gep
                   ( pointer_id,
                     aggregate_ty,
                     destination,
                     [ Ir.Zero; Ir.Index (Ir.Const (Ir.I64, Int64.of_int index)) ] ));
              let element_alignment =
                min
                  (offset_alignment base_alignment (index * element_size))
                  element_natural_alignment
              in
              let* () =
                construct_into ~alignment:element_alignment s
                  (Ir.Local (pointer_id, Ir.Pointer (storage_ty element_ty)))
                  element
              in
              go (index + 1) rest
        in
        go 0 elements
  | Hir.Init_aggregate (Hir.Struct name, false, elements, span) -> (
      match find_struct s name with
      | None -> error span ("internal error: unknown construction struct `" ^ name ^ "`")
      | Some definition ->
          if definition.is_union then
            match (definition.fields, elements) with
            | field :: _, [ entry ] ->
                let* base_alignment =
                  match alignment with
                  | Some value -> Ok value
                  | None -> align s (Hir.Struct name)
                in
                let* field_natural_alignment = align s field.Hir.ty in
                let field_alignment =
                  min
                    (offset_alignment base_alignment field.Hir.offset)
                    field_natural_alignment
                in
                let* () =
                  construct_entries ~alignment:base_alignment s destination
                    (Hir.Init_zero (Hir.zero_initializer (Hir.Struct name), true, span))
                in
                let* pointer = copy_offset s destination field.Hir.offset in
                construct_into ~alignment:field_alignment s pointer entry
            | _ -> error span "internal error: union construction arity"
          else if List.length elements <> List.length definition.fields then
            error span "internal error: struct construction arity"
          else
            let rec go fields entries =
              match (fields, entries) with
              | [], [] -> Ok ()
              | field :: rest_fields, entry :: rest_entries ->
                  let* pointer = copy_offset s destination field.Hir.offset in
                  let* base_alignment =
                    match alignment with
                    | Some value -> Ok value
                    | None -> align s (Hir.Struct name)
                  in
                  let* field_natural_alignment = align s field.ty in
                  let field_alignment =
                    min
                      (offset_alignment base_alignment field.offset)
                      field_natural_alignment
                  in
                  let* () = construct_into ~alignment:field_alignment s pointer entry in
                  go rest_fields rest_entries
              | _ -> error span "internal error: struct construction arity"
            in
            go definition.fields elements)
  | Hir.Init_aggregate (_, _, _, span) ->
      error span "internal error: construction requires an array or struct"

and stmt_list s xs =
  List.fold_left
    (fun r st ->
      let* () = r in
      if open_block s then stmt s st else Ok ())
    (Ok ()) xs

and target_address s = function
  | Hir.ALocal local -> (
      match Hashtbl.find_opt s.env local.id with
      | Some (pointer, _) -> Ok pointer
      | None -> error Span.synthetic ("unknown local `" ^ local.name ^ "`"))
  | Hir.AGlobal (name, global_ty) ->
      let id = fresh s in
      emit s (Ir.Global_ptr (id, name, storage_ty global_ty));
      Ok (Ir.Local (id, Ir.Pointer (storage_ty global_ty)))
  | Hir.ARaw (b, off, _) -> raw_address s b off
  | Hir.AIndex (a, i) -> index_address s a i
  | Hir.AField (a, _, off) -> field_address s a off

and vector_lane s aggregate index =
  let source_ty = Hir.expr_ty aggregate in
  let* pointer = address s aggregate in
  let vector_ty = ty source_ty in
  let* alignment = place_alignment s aggregate in
  let* iv = expr s index in
  let lanes = match source_ty with Hir.Vec (n, _) -> n | _ -> 1 in
  let* iv = normalize_index s index iv in
  lane_guard s index iv lanes;
  Ok (pointer, source_ty, vector_ty, iv, alignment)

and lower_if s c a b =
  mark s (Hir.expr_span c);
  let* cv = expr s c in
  let* cv = require_bool (Hir.expr_span c) cv in
  let tb = fresh_block s and eb = fresh_block s and join = fresh_block s in
  s.current.term := Some (Ir.CondBr (cv, tb.id, eb.id));
  enter s tb;
  let* () = scoped s a in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  enter s eb;
  let* () = match b with None -> Ok () | Some xs -> scoped s xs in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  enter s join;
  let falls_through =
    (Hir.block_flow a).falls_through
    || match b with None -> true | Some body -> (Hir.block_flow body).falls_through
  in
  if not falls_through then s.current.term := Some Ir.Unreachable;
  Ok ()

and target_loop s label =
  match label with
  | None -> List.nth_opt s.loops 0
  | Some name -> List.find_opt (fun loop -> loop.label = Some name) s.loops

and loop_backedge s span target =
  if open_block s then (
    mark s span;
    s.current.term := Some (Ir.Br target))

and lower_while s label c body span =
  let head = fresh_block s and bb = fresh_block s and exit = fresh_block s in
  s.current.term := Some (Ir.Br head.id);
  enter s head;
  mark s (Hir.expr_span c);
  let* cv = expr s c in
  let* cv = require_bool (Hir.expr_span c) cv in
  s.current.term := Some (Ir.CondBr (cv, bb.id, exit.id));
  enter s bb;
  s.loops <-
    {
      label;
      break_to = exit.id;
      continue_to = head.id;
      keep_scopes = List.length s.defer_scopes;
    }
    :: s.loops;
  let* () = scoped s body in
  s.loops <- List.tl s.loops;
  loop_backedge s span head.id;
  enter s exit;
  if Hir.condition_is_true c && not (Hir.loop_body_has_break label body) then
    s.current.term := Some Ir.Unreachable;
  Ok ()

and lower_for s label init cond step body span =
  push_scope s;
  let lowered =
    let* () = match init with None -> Ok () | Some x -> stmt s x in
    let head = fresh_block s
    and bb = fresh_block s
    and sb = fresh_block s
    and exit = fresh_block s in
    s.current.term := Some (Ir.Br head.id);
    enter s head;
    let* () =
      match cond with
      | None ->
          s.current.term := Some (Ir.Br bb.id);
          Ok ()
      | Some c ->
          mark s (Hir.expr_span c);
          let* v = expr s c in
          let* condition = require_bool (Hir.expr_span c) v in
          s.current.term := Some (Ir.CondBr (condition, bb.id, exit.id));
          Ok ()
    in
    enter s bb;
    s.loops <-
      {
        label;
        break_to = exit.id;
        continue_to = sb.id;
        keep_scopes = List.length s.defer_scopes;
      }
      :: s.loops;
    let* () = scoped s body in
    s.loops <- List.tl s.loops;
    if open_block s then s.current.term := Some (Ir.Br sb.id);
    enter s sb;
    let* () = match step with None -> Ok () | Some x -> stmt s x in
    loop_backedge s span head.id;
    enter s exit;
    let* () = if open_block s then emit_scope_defers s 0 else Ok () in
    let* () = if open_block s then emit_current_scope_lifetimes s else Ok () in
    let unconditional =
      match cond with None -> true | Some condition -> Hir.condition_is_true condition
    in
    if unconditional && not (Hir.loop_body_has_break label body) then
      s.current.term := Some Ir.Unreachable;
    Ok ()
  in
  pop_scope s;
  lowered

and lower_switch s e arms default span =
  mark s (Hir.expr_span e);
  let* v = expr s e in
  let join = fresh_block s in
  let blocks = List.map (fun _ -> fresh_block s) arms in
  let db = fresh_block s in
  let rec lower_cases arms blocks =
    match (arms, blocks) with
    | [], [] -> Ok []
    | (cases, _) :: arm_rest, block :: block_rest ->
        let rec values acc = function
          | [] -> Ok (List.rev acc)
          | case :: rest ->
              let* value =
                match case with
                | Hir.EInt (value, _, _) -> Ok value
                | Hir.EBool (value, _) -> Ok (if value then 1L else 0L)
                | _ ->
                    error (Hir.expr_span case)
                      "internal error: non-constant switch case"
              in
              values ((value, block.id) :: acc) rest
        in
        let* case_values = values [] cases in
        let* rest = lower_cases arm_rest block_rest in
        Ok (case_values @ rest)
    | _ -> error span "internal error: switch arm/block mismatch"
  in
  let* cases = lower_cases arms blocks in
  s.current.term := Some (Ir.Switch (value_ty v, v, cases, db.id));
  let rec each as_ bs =
    match (as_, bs) with
    | [], [] -> Ok ()
    | (_, body) :: at, b :: bt ->
        enter s b;
        let* () = scoped s body in
        if open_block s then s.current.term := Some (Ir.Br join.id);
        each at bt
    | _ -> error span "internal error: switch arm/block mismatch"
  in
  let* () = each arms blocks in
  enter s db;
  let* () = match default with None -> Ok () | Some xs -> scoped s xs in
  if open_block s then s.current.term := Some (Ir.Br join.id);
  enter s join;
  let falls_through =
    let branches = List.map (fun (_, body) -> Hir.block_flow body) arms in
    let branches =
      match default with
      | None -> Hir.flowing :: branches
      | Some body -> Hir.block_flow body :: branches
    in
    List.exists (fun (flow : Hir.flow_summary) -> flow.falls_through) branches
  in
  if not falls_through then s.current.term := Some Ir.Unreachable;
  Ok ()

let address_taken_locals ?function_addresses body =
  let views = Hashtbl.create 16 in
  let addressed = Hashtbl.create 16 in
  let rec resolve_view id =
    match Hashtbl.find_opt views id with
    | Some owner when owner <> id -> resolve_view owner
    | _ -> id
  in
  let rec root_local = function
    | Hir.Local (local, _) -> Some (resolve_view local.id)
    | Hir.Field (base, _, _, _, _)
    | Hir.Index (base, _, _, _)
    | Hir.Raw_select (base, _, _, _) ->
        root_local base
    | _ -> None
  in
  let rec collect_expr = function
    | Hir.Address (place, _, _) ->
        Option.iter (fun id -> Hashtbl.replace addressed id ()) (root_local place);
        collect_expr place
    | Hir.Vector_lit (items, _, _) -> List.iter collect_expr items
    | Hir.Unary (_, value, _, _) | Hir.Cast (_, value, _, _) | Hir.Splat (value, _, _)
      ->
        collect_expr value
    | Hir.Binary (_, left, right, _, _)
    | Hir.Index (left, right, _, _)
    | Hir.Raw_select (left, right, _, _) ->
        collect_expr left;
        collect_expr right
    | Hir.Call (_, args, _, _) -> List.iter collect_expr args
    | Hir.Indirect_call (callee, args, _, _) ->
        collect_expr callee;
        List.iter collect_expr args
    | Hir.Field (base, _, _, _, _) -> collect_expr base
    | Hir.Ternary (condition, yes, no, _, _) ->
        collect_expr condition;
        collect_expr yes;
        collect_expr no
    | Hir.Function_address (name, _) ->
        Option.iter (fun table -> Hashtbl.replace table name ()) function_addresses
    | Hir.EInt _ | Hir.EFloat _ | Hir.EBool _ | Hir.EVector _ | Hir.Null _
    | Hir.EString _ | Hir.Local _ | Hir.Global _ | Hir.Sizeof _ | Hir.Alignof _
    | Hir.Offsetof _ | Hir.Const_array _ ->
        ()
  in
  let rec collect_construction = function
    | Hir.Init_value value -> collect_expr value
    | Hir.Init_zero _ -> ()
    | Hir.Init_aggregate (_, _, entries, _) -> List.iter collect_construction entries
    | Hir.Init_omitted _ -> ()
  and collect_target = function
    | Hir.ARaw (base, offset, _) | Hir.AIndex (base, offset) ->
        collect_expr base;
        collect_expr offset
    | Hir.AField (base, _, _) -> collect_expr base
    | Hir.ALocal _ | Hir.AGlobal _ -> ()
  and collect_stmt = function
    | Hir.Let (_, value, _) -> Option.iter collect_expr value
    | Hir.Let_construct (_, construction, _) -> collect_construction construction
    | Hir.View (local, place, _) ->
        Option.iter (Hashtbl.replace views local.id) (root_local place);
        collect_expr place
    | Hir.Copy (destination, source, _, _, _) ->
        collect_expr destination;
        collect_expr source
    | Hir.Volatile_store (_, pointer, value, _) ->
        collect_expr pointer;
        collect_expr value
    | Hir.Simd_store (_, _, args, _) -> List.iter collect_expr args
    | Hir.Assign (target, value, _) | Hir.Compound_assign (target, _, value, _, _) ->
        collect_target target;
        collect_expr value
    | Hir.Return (value, _) -> Option.iter collect_expr value
    | Hir.If (condition, yes, no, _) ->
        collect_expr condition;
        collect_list yes;
        Option.iter collect_list no
    | Hir.While (_, condition, body, _) ->
        collect_expr condition;
        collect_list body
    | Hir.For (_, init, condition, step, body, _) ->
        Option.iter collect_stmt init;
        Option.iter collect_expr condition;
        Option.iter collect_stmt step;
        collect_list body
    | Hir.Switch (value, arms, default, _) ->
        collect_expr value;
        List.iter
          (fun (cases, body) ->
            List.iter collect_expr cases;
            collect_list body)
          arms;
        Option.iter collect_list default
    | Hir.Defer (body, _) | Hir.Block (body, _) -> collect_list body
    | Hir.Break _ | Hir.Continue _ -> ()
    | Hir.Expr (value, _) -> collect_expr value
  and collect_list statements = List.iter collect_stmt statements in
  collect_list body;
  addressed

let lower_func ~sanitize_address ~intrinsic_masked_loads ~address_taken
    ~addressed_functions literal_globals structs strings functions f =
  let c_abi = has_c_abi f || Hashtbl.mem addressed_functions f.name in
  let source =
    if Span.is_synthetic f.Hir.span then None
    else Some (f.Hir.span.Span.file, f.Hir.span.Span.line)
  in
  let* () =
    Result_list.iter
      (fun (local : Hir.local) -> layout_ok structs local.ty)
      f.Hir.params
  in
  let* () = if f.Hir.ret = Hir.Void then Ok () else layout_ok structs f.Hir.ret in
  let params =
    List.mapi
      (fun index (local : Hir.local) ->
        ({
           Ir.name = "a" ^ string_of_int index;
           ty = ty local.ty;
           extension = (if c_abi then c_extension local.ty else Ir.No_extension);
         }
          : Ir.param))
      f.Hir.params
  in
  let ret_extension = if c_abi then c_extension f.ret else Ir.No_extension in
  match f.body with
  | Hir.Declaration ->
      Ok
        {
          Ir.name = f.name;
          params;
          ret = ty f.ret;
          ret_extension;
          blocks = [];
          linkage = Ir.External;
          variadic = f.variadic;
          source;
          display = f.display;
        }
  | Hir.Statements body ->
      let* () =
        if f.ret <> Hir.Void && (Hir.block_flow body).falls_through then
          error Span.synthetic
            (Printf.sprintf
               "internal error: non-void function `%s` reached lowering with \
                fallthrough"
               f.name)
        else Ok ()
      in
      let entry = { id = 0; instrs = Queue.create (); term = ref None } in
      let blocks = Queue.create () in
      Queue.add entry blocks;
      let s =
        {
          literal_globals;
          next_value = 0;
          next_block = 1;
          blocks;
          entry;
          current = entry;
          env = Hashtbl.create 32;
          env_scopes = [];
          ret = ty f.ret;
          structs;
          strings;
          functions;
          addressed_functions;
          defer_scopes = [];
          scope_slots = [];
          address_taken;
          sanitize_address;
          intrinsic_masked_loads;
          loops = [];
          file = Option.map fst source;
          location = None;
        }
      in
      push_scope s;
      let rec bind_parameters locals parameters =
        match (locals, parameters) with
        | [], [] -> Ok ()
        | (local : Hir.local) :: local_rest, (parameter : Ir.param) :: parameter_rest ->
            let* alignment = align s local.ty in
            let id = fresh s in
            emit s (Ir.Alloca (id, storage_ty local.ty, alignment));
            let p = Ir.Local (id, Ir.Pointer (storage_ty local.ty)) in
            let* () = register_local_lifetime s local p in
            bind_local s local p alignment;
            let* () =
              store_value s local.ty
                (Ir.Param (parameter.name, ty local.ty))
                p alignment
            in
            bind_parameters local_rest parameter_rest
        | _ ->
            error Span.synthetic
              (Printf.sprintf "internal error: function `%s` parameter handoff mismatch"
                 f.name)
      in
      let* () = bind_parameters f.params params in
      let* () = scoped s body in
      let* () = if open_block s then emit_scope_defers s 0 else Ok () in
      let* () = if open_block s then emit_current_scope_lifetimes s else Ok () in
      let* () =
        if not (open_block s) then Ok ()
        else if s.ret = Ir.Void then (
          s.current.term := Some (Ir.Ret None);
          Ok ())
        else
          error Span.synthetic
            (Printf.sprintf
               "internal error: non-void function `%s` ended lowering without a \
                terminator"
               f.name)
      in
      pop_scope s;
      let* blocks =
        Result_list.map
          (fun b ->
            match !(b.term) with
            | None ->
                error Span.synthetic
                  (Printf.sprintf
                     "internal error: function `%s` block %d ended lowering without a \
                      terminator"
                     f.name b.id)
            | Some terminator ->
                Ok
                  ({
                     Ir.id = b.id;
                     label = "b" ^ string_of_int b.id;
                     instrs = List.of_seq (Queue.to_seq b.instrs);
                     terminator;
                   }
                    : Ir.block))
          (List.of_seq (Queue.to_seq s.blocks))
      in
      Ok
        {
          Ir.name = f.name;
          params;
          ret = ty f.ret;
          ret_extension;
          blocks;
          linkage =
            (if f.name = "main" then Ir.External
             else if f.linkage = Hir.Internal then Ir.Internal
             else External);
          variadic = f.variadic;
          source;
          display = f.display;
        }

let intrinsic_decls funcs =
  let calls =
    List.concat_map
      (fun (f : Ir.func) ->
        List.concat_map
          (fun (b : Ir.block) ->
            List.filter_map
              (function
                | Ir.Call (_, _, ret, name, args)
                  when String.starts_with ~prefix:"llvm." name ->
                    Some (name, ret, List.map (fun (ty, _, _) -> ty) args)
                | Ir.Trap -> Some ("llvm.trap", Ir.Void, [])
                | _ -> None)
              b.instrs)
          f.blocks)
      funcs
  in
  let unique = Hashtbl.create 16 in
  List.iter (fun (name, ret, args) -> Hashtbl.replace unique name (ret, args)) calls;
  Hashtbl.to_seq unique
  |> Seq.map (fun (name, (ret, args)) ->
      {
        Ir.name;
        params =
          List.mapi
            (fun i ty ->
              { Ir.name = "a" ^ string_of_int i; ty; extension = Ir.No_extension })
            args;
        ret;
        ret_extension = Ir.No_extension;
        blocks = [];
        linkage = Ir.External;
        variadic = false;
        source = None;
        display = None;
      })
  |> List.of_seq
  |> List.sort (fun (a : Ir.func) b -> String.compare a.name b.name)

let global_storage structs (global : Hir.global) =
  match Hir.layout structs global.ty with
  | Error message ->
      error Span.synthetic
        (Printf.sprintf "internal error: global %s has no layout: %s" global.name
           message)
  | Ok (size, align) -> (
      let data = Bytes.make size (Char.chr 0) in
      let pointers = ref [] in
      let object_size ty = fst (Result.get_ok (Hir.layout structs ty)) in
      let set offset value =
        if offset < 0 || offset >= size then
          failwith "initializer exceeds global storage";
        Bytes.set data offset (Char.chr (value land 255))
      in
      let write_integer offset width value =
        for index = 0 to width - 1 do
          set (offset + index)
            (Int64.to_int (Int64.shift_right_logical value (index * 8)))
        done
      in
      let rec write offset ty = function
        | Hir.Global_array [] | Hir.Global_struct [] -> ()
        | Hir.Global_zero zero ->
            if not (Hir.ty_equal zero.zero_ty ty) then
              failwith "zero initializer type mismatch"
        | Hir.Global_int value -> write_integer offset (object_size ty) value
        | Hir.Global_float value -> write_integer offset (object_size ty) value
        | Hir.Global_bool value -> set offset (if value then 1 else 0)
        | Hir.Global_null -> ()
        | Hir.Global_address (symbol, displacement) ->
            pointers := (offset, symbol, displacement) :: !pointers
        | Hir.Global_vector values when ty = Hir.Vec (List.length values, Hir.Bool) ->
            List.iteri
              (fun index value ->
                if value <> 0L then
                  let byte = offset + (index / 8) in
                  set byte (Char.code (Bytes.get data byte) lor (1 lsl (index mod 8))))
              values
        | Hir.Global_vector _
          when match ty with Hir.Vec (_, Hir.Bool) -> true | _ -> false ->
            failwith "vector initializer lane mismatch"
        | Hir.Global_vector values ->
            let width =
              match ty with
              | Hir.Vec (lanes, element) when lanes = List.length values ->
                  object_size element
              | _ -> failwith "vector initializer type or lane mismatch"
            in
            List.iteri
              (fun index value -> write_integer (offset + (index * width)) width value)
              values
        | Hir.Global_array values ->
            let element, width =
              match ty with
              | Hir.Array (length, element) when length = List.length values ->
                  (element, object_size element)
              | _ -> failwith "array initializer type or length mismatch"
            in
            List.iteri
              (fun index value -> write (offset + (index * width)) element value)
              values
        | Hir.Global_struct values -> (
            match ty with
            | Hir.Struct name ->
                let definition =
                  List.find_opt
                    (fun (definition : Hir.struct_def) -> definition.name = name)
                    structs
                  |> Option.get
                in
                if definition.is_union then
                  match (definition.fields, values) with
                  | field :: _, value :: _ ->
                      write (offset + field.offset) field.ty value
                  | _ -> failwith "union initializer type mismatch"
                else
                  List.iter2
                    (fun (field : Hir.field) value ->
                      write (offset + field.offset) field.ty value)
                    definition.fields values
            | _ -> failwith "struct initializer type mismatch")
      in
      try
        let bytes =
          match global.init_value with
          | None -> None
          | Some value ->
              write 0 global.ty value;
              Some (Bytes.to_string data)
        in
        Ok
          (Ir.Storage_global
             {
               name = global.name;
               storage_ty = storage_ty global.ty;
               size;
               bytes;
               pointers = List.rev !pointers;
               readonly = global.readonly;
               align;
               linkage = global.linkage;
             })
      with Failure message | Invalid_argument message ->
        error Span.synthetic
          (Printf.sprintf "internal error: global %s: %s" global.name message))

let lower ?(sanitize_address = false) ?(intrinsic_masked_loads = true) (p : Hir.program)
    =
  let* () =
    match Target_layout.pointer_integer_bits (Target_layout.current ()) with
    | Ok 32 | Ok 64 -> Ok ()
    | Ok bits ->
        error Span.synthetic
          (Printf.sprintf "internal error: unsupported pointer width: %d" bits)
    | Error message -> error Span.synthetic ("internal error: " ^ message)
  in
  let no_layout t m =
    error Span.synthetic
      (Printf.sprintf "internal error: type `%s` has no layout: %s" (Hir.ty_name t) m)
  in
  let field_size t =
    match Hir.layout p.structs t with
    | Ok (s, _) when s >= 0 -> Ok s
    | Ok _ -> no_layout t "negative object size"
    | Error m -> no_layout t m
  in
  let field_align t =
    match Hir.layout p.structs t with
    | Ok (_, a) -> (
        match Target_layout.validate_type_alignment (Target_layout.current ()) a with
        | Ok () -> Ok a
        | Error m -> no_layout t m)
    | Error m -> no_layout t m
  in
  let malformed_struct (d : Hir.struct_def) message =
    error Span.synthetic
      (Printf.sprintf "internal error: struct `%s` has malformed layout: %s" d.name
         message)
  in
  let addressed_functions = Hashtbl.create 16 in
  let function_names = Hashtbl.create (List.length p.funcs) in
  List.iter (fun (f : Hir.func) -> Hashtbl.replace function_names f.name ()) p.funcs;
  List.iter
    (fun (f : Hir.func) ->
      match f.body with
      | Hir.Declaration -> ()
      | Hir.Statements body ->
          ignore (address_taken_locals ~function_addresses:addressed_functions body))
    p.funcs;
  let rec collect_global_addresses = function
    | Hir.Global_address (name, _) when Hashtbl.mem function_names name ->
        Hashtbl.replace addressed_functions name ()
    | Hir.Global_array entries | Hir.Global_struct entries ->
        List.iter collect_global_addresses entries
    | Hir.Global_zero _ | Hir.Global_int _ | Hir.Global_float _ | Hir.Global_bool _
    | Hir.Global_null | Hir.Global_address _ | Hir.Global_vector _ ->
        ()
  in
  List.iter
    (fun (global : Hir.global) ->
      Option.iter collect_global_addresses global.init_value)
    p.globals;
  let literal_globals = ref [] in
  let* funcs =
    Result_list.map
      (fun (f : Hir.func) ->
        let address_taken =
          match (sanitize_address, f.Hir.body) with
          | true, Hir.Statements body -> address_taken_locals body
          | _ -> Hashtbl.create 0
        in
        lower_func ~sanitize_address ~intrinsic_masked_loads ~address_taken
          ~addressed_functions literal_globals p.Hir.structs p.strings p.funcs f)
      p.funcs
  in
  let* structs =
    Result_list.map
      (fun (d : Hir.struct_def) ->
        let* () =
          match
            Target_layout.validate_type_alignment (Target_layout.current ()) d.align
          with
          | Ok () -> Ok ()
          | Error message -> malformed_struct d message
        in
        let* fields, used, natural_align =
          if d.byte_storage then
            let rec validate used = function
              | [] -> Ok ([ Ir.Array (d.size, Ir.I8) ], d.size, 1)
              | (field : Hir.field) :: rest ->
                  let* size = field_size field.ty in
                  let* _ = field_align field.ty in
                  if field.offset < 0 || field.offset > max_int - size then
                    malformed_struct d
                      (Printf.sprintf "field `%s` end offset overflows" field.name)
                  else
                    let field_end = field.offset + size in
                    if field_end > d.size then
                      malformed_struct d
                        (Printf.sprintf "field `%s` exceeds its size" field.name)
                    else validate (max used field_end) rest
            in
            validate 0 d.fields
          else if d.is_union then
            let rec validate natural_align = function
              | [] -> Ok ([ Ir.Array (d.size, Ir.I8) ], d.size, natural_align)
              | (field : Hir.field) :: rest ->
                  let* size = field_size field.ty in
                  let* align = field_align field.ty in
                  if field.offset <> 0 then
                    malformed_struct d
                      (Printf.sprintf "union field `%s` is not at zero" field.name)
                  else if size > d.size then
                    malformed_struct d
                      (Printf.sprintf "union field `%s` exceeds its size" field.name)
                  else validate (max natural_align align) rest
            in
            validate 1 d.fields
          else
            let rec go offset out used natural_align = function
              | [] -> Ok (List.rev out, used, natural_align)
              | (f : Hir.field) :: rest ->
                  let* size = field_size f.ty in
                  let* align = field_align f.ty in
                  if f.offset < offset then
                    malformed_struct d
                      (Printf.sprintf "field `%s` overlaps a preceding field" f.name)
                  else if f.offset mod align <> 0 then
                    malformed_struct d
                      (Printf.sprintf "field `%s` is not aligned to %d" f.name align)
                  else if f.offset > max_int - size then
                    malformed_struct d
                      (Printf.sprintf "field `%s` end offset overflows" f.name)
                  else
                    let field_end = f.offset + size in
                    let padding = f.offset - offset in
                    let out =
                      if padding > 0 then Ir.Array (padding, Ir.I8) :: out else out
                    in
                    go field_end (storage_ty f.ty :: out) (max used field_end)
                      (max natural_align align) rest
            in
            go 0 [] 0 1 d.fields
        in
        if d.align < natural_align then
          malformed_struct d
            (Printf.sprintf "alignment %d is less than natural alignment %d" d.align
               natural_align)
        else if d.size < used then malformed_struct d "size is smaller than its fields"
        else if d.size mod d.align <> 0 then
          malformed_struct d
            (Printf.sprintf "size %d is not a multiple of alignment %d" d.size d.align)
        else
          let fields =
            if d.align > natural_align then
              Ir.Array (0, Ir.Vector (d.align, Ir.I8)) :: fields
            else fields
          in
          Ok { Ir.name = d.name; fields; tail_padding = d.size - used })
      p.structs
  in
  let strings =
    List.mapi
      (fun i x -> Ir.String_global { name = ".str." ^ string_of_int i; bytes = x })
      p.strings
  in
  let* arrays =
    Result_list.map
      (fun (a : Hir.const_arr_def) ->
        match a.ty with
        | Hir.Array (_, e) ->
            let* align = field_align e in
            Ok
              (Ir.Array_global
                 { name = a.name; elem_ty = storage_ty e; elems = a.elems; align })
        | _ ->
            Ok
              (Ir.Array_global { name = a.name; elem_ty = Ir.I8; elems = []; align = 1 }))
      p.const_arrays
  in
  let* storage_globals =
    Result_list.map (global_storage p.structs) (p.globals @ List.rev !literal_globals)
  in
  let module_ =
    {
      Ir.target_triple = (Target_layout.current ()).triple;
      data_layout = (Target_layout.current ()).llvm_data_layout;
      structs;
      globals = strings @ arrays @ storage_globals;
      funcs = funcs @ intrinsic_decls funcs;
      no_inline_functions = [];
    }
  in
  match Ir.validate module_ with
  | Ok () -> Ok module_
  | Error message -> error Span.synthetic ("internal error: " ^ message)
