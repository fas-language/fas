(* Lowers checked HIR to Fir (roadmap v0.6: one IR).

   Operands are named with Low's small types (a result id reserved with
   [fresh], a parameter, a constant...) and each instruction is a call into
   Lower_emit, which appends Fir to the current block at once. *)

module E = Lower_emit

(* a block of the function being lowered: its Fir block index *)
type block_state = { id : int }

type loop = {
  label : string option;
  break_to : int;
  continue_to : int;
  keep_scopes : int;
}

type local_slot = { pointer : Low.value; size : int }

type state = {
  fn : E.fn;  (** the Fir function being built *)
  literal_globals : Hir.global list ref;
  mutable next_value : int;
  mutable current : block_state;
  env : (int, Low.value * int) Hashtbl.t;
  mutable env_scopes : (int * (Low.value * int) option) list list;
  ret : Low.ty;
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

let fresh_block s = { id = E.new_block s.fn }

(* The source position of the code that follows. Stack slots
   (E.alloca_entry) go at the very top of the entry block instead: appending
   them where they are declared would put a slot declared after a call behind
   that call, and inlining the call then turns the slot into a dynamic stack
   allocation. *)
let mark s (span : Span.t) =
  if (not (Span.is_synthetic span)) && Some span.Span.file = s.file then (
    s.location <- Some (span.Span.line, span.Span.column);
    E.location s.fn span.Span.line span.Span.column)

let enter s block =
  s.current <- block;
  E.enter s.fn block.id s.location

let open_block s = E.is_open s.fn s.current.id

let value_ty = function
  | Low.Const (t, _)
  | Const_vector (t, _)
  | Null t
  | Undef t
  | Zero t
  | Local (_, t)
  | Param (_, t)
  | Global (_, t) ->
      t

let rec ty = function
  | Hir.Bool -> Low.I1
  | Hir.Float Hir.F32 -> Low.F32
  | Hir.Float Hir.F64 -> Low.F64
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
  | Hir.Addr | Hir.Handle _ -> Low.Pointer Low.I8
  | Hir.Vec (n, t) -> Low.Vector (n, ty t)
  | Hir.Array (n, t) -> Low.Array (n, storage_ty t)
  | Hir.Struct n -> Low.Struct n
  | Hir.Opaque n -> Low.Struct n
  | Hir.Void -> Low.Void

and storage_ty = function
  | Hir.Bool -> Low.I8
  | Hir.Array (n, t) -> Low.Array (n, storage_ty t)
  | Hir.Struct n -> Low.Struct n
  | Hir.Opaque n -> Low.Struct n
  | t -> ty t

let ir_extension = function
  | Target_layout.C_no_extension -> Low.No_extension
  | Target_layout.C_sign_extension -> Low.Sign_extension
  | Target_layout.C_zero_extension -> Low.Zero_extension

let c_extension = function
  | Hir.Bool -> ir_extension (Target_layout.current ()).c_abi_bool_extension
  | Hir.Int kind ->
      Target_layout.c_integer_extension (Target_layout.current ())
        ~signed:
          (match kind with Hir.I8 | I16 | I32 | I64 | Isize -> true | _ -> false)
        (Hir.int_bytes kind * 8)
      |> ir_extension
  | _ -> Low.No_extension

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
        match Low.value_ty pointer with
        | Low.Pointer Low.I8 -> pointer
        | pointer_ty ->
            let id = fresh s in
            E.cast s.fn id "bitcast" pointer_ty pointer (Low.Pointer Low.I8);
            Low.Local (id, Low.Pointer Low.I8)
      in
      let loaded = fresh s in
      E.load s.fn loaded Low.I8 byte_pointer alignment;
      let normalized = fresh s in
      E.cmp s.fn normalized Low.Ne Low.I8 (Low.Local (loaded, Low.I8)) (Low.Const (Low.I8, 0L));
      Ok (Low.Local (normalized, Low.I1))
  | _ ->
      let loaded = fresh s in
      E.load s.fn loaded (ty value_ty) pointer alignment;
      Ok (Low.Local (loaded, ty value_ty))

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

let zero t = Low.Const (t, 0L)

let ones span = function
  | Low.I1 -> Ok 1L
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
  | Ast.Eq when is_float_type t -> Ok Low.Foeq
  | Ne when is_float_type t -> Ok Low.Fune
  | Lt when is_float_type t -> Ok Low.Folt
  | Le when is_float_type t -> Ok Low.Fole
  | Gt when is_float_type t -> Ok Low.Fogt
  | Ge when is_float_type t -> Ok Low.Foge
  | Ast.Eq -> Ok Low.Eq
  | Ne -> Ok Ne
  | Lt -> Ok (if unsigned t then Ult else Slt)
  | Le -> Ok (if unsigned t then Ule else Sle)
  | Gt -> Ok (if unsigned t then Ugt else Sgt)
  | Ge -> Ok (if unsigned t then Uge else Sge)
  | _ ->
      error span "internal error: non-comparison operator reached comparison lowering"

let bin_for span t = function
  | Ast.Add when is_float_type t -> Ok Low.Fadd
  | Sub when is_float_type t -> Ok Low.Fsub
  | Mul when is_float_type t -> Ok Low.Fmul
  | Div when is_float_type t -> Ok Low.Fdiv
  | Ast.Add -> Ok Low.Add
  | Sub -> Ok Sub
  | Mul -> Ok Mul
  | Div -> Ok (if unsigned t then Udiv else Sdiv)
  | Rem -> Ok (if unsigned t then Urem else Srem)
  | Bit_and -> Ok And
  | Bit_or -> Ok Or
  | Bit_xor -> Ok Xor
  | _ -> error span "internal error: non-arithmetic operator reached binary lowering"

let width span = function
  | Low.I1 -> Ok 1
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
    E.cast s.fn id k from v target;
    Ok (Low.Local (id, target))

let splat_scalar s span vector_ty elem_ty value =
  let* value = coerce s span value elem_ty in
  let inserted = fresh s in
  E.insert s.fn inserted vector_ty (Low.Zero vector_ty) (Low.Const (Low.I32, 0L)) value;
  let shuffled = fresh s in
  E.shuffle_zero s.fn shuffled vector_ty (Low.Local (inserted, vector_ty));
  Ok (Low.Local (shuffled, vector_ty))

let shift_amount s span target value =
  let elem_ty = match target with Low.Vector (_, t) -> t | t -> t in
  match (target, value_ty value) with
  | (Low.Vector (lanes, _) as target_ty), Low.Vector (count_lanes, count_elem)
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
          E.cast s.fn id k from value target_ty;
          Ok (Low.Local (id, target_ty))
      in
      let* mask_vec =
        splat_scalar s span target_ty elem_ty (Low.Const (elem_ty, mask))
      in
      let id = fresh s in
      E.bin s.fn id Low.And target_ty count mask_vec;
      Ok (Low.Local (id, target_ty))
  | _ -> (
      let* value = coerce s span value elem_ty in
      let* bits = width span elem_ty in
      let mask = Int64.of_int (bits - 1) in
      let id = fresh s in
      E.bin s.fn id Low.And elem_ty value (Low.Const (elem_ty, mask));
      let value = Low.Local (id, elem_ty) in
      match target with
      | Low.Vector _ -> splat_scalar s span target elem_ty value
      | _ -> Ok value)

let rec intrinsic_suffix span = function
  | Low.I8 -> Ok "i8"
  | Low.I16 -> Ok "i16"
  | Low.I32 -> Ok "i32"
  | Low.I64 -> Ok "i64"
  | Low.Vector (lanes, elem) ->
      let* elem_suffix = intrinsic_suffix span elem in
      Ok (Printf.sprintf "v%d%s" lanes elem_suffix)
  | _ -> error span "internal error: integer intrinsic has a non-integer type"

let rec float_suffix span = function
  | Low.F32 -> Ok "f32"
  | Low.F64 -> Ok "f64"
  | Low.Vector (lanes, elem) ->
      let* suffix = float_suffix span elem in
      Ok (Printf.sprintf "v%d%s" lanes suffix)
  | _ -> error span "internal error: float intrinsic has a non-float type"

let value_const s t value =
  match t with
  | Low.Vector (_, elem) ->
      let inserted = fresh s in
      E.insert s.fn inserted t (Low.Zero t) (Low.Const (Low.I32, 0L)) (Low.Const (elem, value));
      let shuffled = fresh s in
      E.shuffle_zero s.fn shuffled t (Low.Local (inserted, t));
      Low.Local (shuffled, t)
  | _ -> Low.Const (t, value)

let compare s cmp t lhs rhs =
  let id = fresh s in
  E.cmp s.fn id cmp t lhs rhs;
  match t with
  | Low.Vector (lanes, _) -> Low.Local (id, Low.Vector (lanes, Low.I1))
  | _ -> Low.Local (id, Low.I1)

let reduce_any s span value =
  match value_ty value with
  | Low.I1 -> Ok value
  | Low.Vector (lanes, Low.I1) as t ->
      let id = fresh s in
      E.call s.fn (Some id) Low.No_extension Low.I1 (Printf.sprintf "llvm.vector.reduce.or.v%di1" lanes) [ (t, Low.No_extension, value) ];
      Ok (Low.Local (id, Low.I1))
  | _ -> error span "internal error: boolean reduction has a non-boolean type"

let condition_ty value =
  match value_ty value with
  | Low.I1 as t -> t
  | Low.Vector (_, Low.I1) as t -> t
  | _ -> Low.I1

let combine_conditions s lhs rhs =
  let t = condition_ty lhs in
  let id = fresh s in
  E.bin s.fn id Low.And t lhs rhs;
  Low.Local (id, t)

let either_condition s lhs rhs =
  let t = condition_ty lhs in
  let id = fresh s in
  E.bin s.fn id Low.Or t lhs rhs;
  Low.Local (id, t)

let emit_trap s =
  E.trap s.fn;
  E.unreachable s.fn

let guard_condition s condition =
  let trap = fresh_block s and valid = fresh_block s in
  E.cond_br s.fn condition trap.id valid.id;
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
      let below = compare s Low.Slt ity iv (value_const s ity 0L) in
      let above = compare s Low.Sge ity iv limit in
      either_condition s below above
    else compare s Low.Uge ity iv limit
  in
  guard_condition s condition

let division_guard s span source_ty binop ir_ty lhs rhs =
  let zero_condition = compare s Low.Eq ir_ty rhs (value_const s ir_ty 0L) in
  let* condition =
    if binop = Low.Sdiv && signed_type source_ty then
      let* bits = width span (match ir_ty with Low.Vector (_, t) -> t | t -> t) in
      let minimum = Int64.shift_left 1L (bits - 1) in
      let lhs_minimum = compare s Low.Eq ir_ty lhs (value_const s ir_ty minimum) in
      let rhs_minus_one =
        compare s Low.Eq ir_ty rhs (value_const s ir_ty Int64.minus_one)
      in
      Ok
        (either_condition s zero_condition
           (combine_conditions s lhs_minimum rhs_minus_one))
    else Ok zero_condition
  in
  match value_ty condition with
  | Low.I1 ->
      guard_condition s condition;
      Ok ()
  | Low.Vector (lanes, Low.I1) ->
      let* any_lane = reduce_any s span condition in
      let ordered = fresh_block s and proceed = fresh_block s in
      E.cond_br s.fn any_lane ordered.id proceed.id;
      enter s ordered;
      let rec guard_lane lane =
        if lane >= lanes then Ok ()
        else
          let id = fresh s in
          E.extract s.fn id (value_ty condition) condition (Low.Const (Low.I64, Int64.of_int lane));
          let trap = fresh_block s and next = fresh_block s in
          E.cond_br s.fn (Low.Local (id, Low.I1)) trap.id next.id;
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
      | Ast.Shr -> if unsigned source_ty then Low.Lshr else Low.Ashr
      | _ -> Low.Shl
    in
    let result = fresh s in
    E.bin s.fn result binop ir_ty lhs count;
    Ok (Low.Local (result, ir_ty)))
  else
    let* binop = bin_for span source_ty op in
    let division =
      match binop with Low.Sdiv | Srem | Udiv | Urem -> true | _ -> false
    in
    let* () =
      if division then division_guard s span source_ty binop ir_ty lhs rhs else Ok ()
    in
    if binop = Low.Srem then (
      let is_minus_one =
        compare s Low.Eq ir_ty rhs (value_const s ir_ty Int64.minus_one)
      in
      let safe_rhs_id = fresh s in
      let safe_rhs = value_const s ir_ty 1L in
      E.select s.fn safe_rhs_id is_minus_one safe_rhs rhs;
      let raw_id = fresh s in
      E.bin s.fn raw_id binop ir_ty lhs (Low.Local (safe_rhs_id, ir_ty));
      let result_id = fresh s in
      E.select s.fn result_id is_minus_one (value_const s ir_ty 0L) (Low.Local (raw_id, ir_ty));
      Ok (Low.Local (result_id, ir_ty)))
    else
      let result = fresh s in
      E.bin s.fn result binop ir_ty lhs rhs;
      Ok (Low.Local (result, ir_ty))

let require_bool span v =
  match value_ty v with
  | Low.I1 -> Ok v
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
    E.cast s.fn id kind (value_ty value) value pointer_int;
    Ok (Low.Local (id, pointer_int))

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
  | Hir.EInt (v, t, _) -> Ok (Low.Const (ty t, v))
  | Hir.EFloat (v, t, _) -> Ok (Low.Const (ty t, v))
  | Hir.EBool (v, _) -> Ok (Low.Const (Low.I1, if v then 1L else 0L))
  | Hir.EVector
      (values, (Hir.Vec (lanes, (Hir.Bool | Hir.Int _ | Hir.Float _)) as t), span) ->
      if List.length values = lanes then Ok (Low.Const_vector (ty t, values))
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
              E.insert s.fn id vector_ir_ty value (Low.Const (Low.I32, Int64.of_int index)) lane;
              lanes_to_value (Low.Local (id, vector_ir_ty)) (index + 1) rest
        in
        lanes_to_value (Low.Zero vector_ir_ty) 0 elements
  | Hir.Vector_lit (_, _, span) -> error span "malformed vector literal type"
  | Hir.Null (t, _) -> Ok (Low.Null (ty t))
  | Hir.EString (i, _, sp) ->
      if i < 0 || i >= List.length s.strings then error sp "missing interned string"
      else
        let id = fresh s in
        E.string_ptr s.fn id i;
        Ok (Low.Local (id, Low.Pointer Low.I8))
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
  | Hir.Function_address (name, _) -> Ok (Low.Global (name, Low.Pointer Low.I8))
  | Hir.Unary (op, e, t, span) -> (
      let* v = expr s e in
      let rt = ty t in
      match op with
      | Ast.Neg ->
          let id = fresh s in
          if is_float_type t then E.fneg s.fn id rt v
          else
            E.bin s.fn id Sub rt (match rt with Low.Vector _ -> Low.Zero rt | _ -> zero rt) v;
          Ok (Low.Local (id, rt))
      | Bit_not ->
          let* mask =
            ones span (match rt with Low.Vector (_, elem) -> elem | _ -> rt)
          in
          let all_ones = value_const s rt mask in
          let id = fresh s in
          E.bin s.fn id Xor rt v all_ones;
          Ok (Low.Local (id, rt))
      | Not when rt = Low.I1 ->
          let id = fresh s in
          E.cmp s.fn id Low.Eq Low.I1 v (Low.Const (Low.I1, 0L));
          Ok (Low.Local (id, Low.I1))
      | Not -> (
          match rt with
          | Low.Vector (_, Low.I1) ->
              let id = fresh s in
              E.bin s.fn id Low.Xor rt v (value_const s rt 1L);
              Ok (Low.Local (id, rt))
          | _ -> error span "internal error: logical not has a non-bool type"))
  | Hir.Binary (((Ast.And | Ast.Or) as op), a, b, _, _) -> lower_short s op a b
  | Hir.Binary (op, a, b, _t, span)
    when match Hir.expr_ty a with Hir.Addr -> true | _ -> false -> (
      let* x = expr s a in
      let* y = expr s b in
      match op with
      | Ast.Add -> address_step s Low.Add x y
      | Ast.Sub -> address_step s Low.Sub x y
      | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge ->
          let pointer_int = ty (Hir.Int Hir.Usize) in
          let x_id = fresh s in
          E.cast s.fn x_id "ptrtoint" (Low.Pointer Low.I8) x pointer_int;
          let y_id = fresh s in
          E.cast s.fn y_id "ptrtoint" (Low.Pointer Low.I8) y pointer_int;
          let comparison =
            match op with
            | Ast.Eq -> Low.Eq
            | Ast.Ne -> Low.Ne
            | Ast.Lt -> Low.Ult
            | Ast.Le -> Low.Ule
            | Ast.Gt -> Low.Ugt
            | _ -> Low.Uge
          in
          let id = fresh s in
          E.cmp s.fn id comparison pointer_int (Low.Local (x_id, pointer_int)) (Low.Local (y_id, pointer_int));
          Ok (Low.Local (id, Low.I1))
      | _ -> error span "internal error: invalid address binary operation")
  | Hir.Binary (op, a, b, t, span) ->
      let* x = expr s a in
      let* y = expr s b in
      if List.mem op [ Ast.Eq; Ne; Lt; Le; Gt; Ge ] then (
        let* comparison = cmp_for span (Hir.expr_ty a) op in
        let id = fresh s in
        E.cmp s.fn id comparison (value_ty x) x y;
        Ok (Low.Local (id, ty t)))
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
                  | None -> Low.No_extension)
              | None -> Low.No_extension
            in
            (value_ty v, extension, v))
          vs
      in
      let ret_extension =
        match c_function with Some _ -> c_extension t | None -> Low.No_extension
      in
      if t = Hir.Void then (
        E.call s.fn None Low.No_extension Low.Void n av;
        Ok (Low.Const (Low.I1, 0L)))
      else
        let id = fresh s in
        E.call s.fn (Some id) ret_extension (ty t) n av;
        Ok (Low.Local (id, ty t))
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
        E.call_indirect s.fn None Low.No_extension Low.Void target arguments;
        Ok (Low.Const (Low.I1, 0L)))
      else
        let id = fresh s in
        E.call_indirect s.fn (Some id) result_extension (ty result_ty) target arguments;
        Ok (Low.Local (id, ty result_ty))
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
              | Low.Pointer _, Low.Pointer _ -> ""
              | Low.Pointer _, _ -> "ptrtoint"
              | _, Low.Pointer _ -> "inttoptr"
              | _ -> "bitcast")
          | Ast.Zext -> "zext"
          | Ast.Sext -> "sext"
          | Ast.Trunc -> "trunc"
          | Ast.Float_widen -> "fpext"
        in
        if k = "" then Ok v
        else
          let id = fresh s in
          E.cast s.fn id k st v dt;
          Ok (Low.Local (id, dt))
  | Hir.Raw_select (b, off, t, _) ->
      let* p = raw_address s b off in
      raw_load s t p
  | Hir.Address (e, _, _) ->
      let* p = address s e in
      let id = fresh s in
      E.cast s.fn id "bitcast" (Low.value_ty p) p (Low.Pointer Low.I8);
      Ok (Low.Local (id, Low.Pointer Low.I8))
  | Hir.Index (a, i, t, _) -> (
      match Hir.expr_ty a with
      | Hir.Vec (lanes, _) ->
          let* av = expr s a in
          let* iv = expr s i in
          let* iv = normalize_index s i iv in
          lane_guard s i iv lanes;
          let id = fresh s in
          E.extract s.fn id (value_ty av) av iv;
          Ok (Low.Local (id, ty t))
      | _ ->
          let* p = index_address s a i in
          let* alignment = place_alignment s (Hir.Index (a, i, t, Span.synthetic)) in
          load_value s t p alignment)
  | Hir.Field (a, _, t, off, _) ->
      let* p = field_address s a off in
      let* alignment = place_alignment s (Hir.Field (a, "", t, off, Span.synthetic)) in
      load_value s t p alignment
  | Hir.Sizeof (_, n, _) | Hir.Alignof (_, n, _) | Hir.Offsetof (_, _, n, _) ->
      Ok (Low.Const (ty (Hir.Int Hir.Usize), Int64.of_int n))
  | Hir.Ternary (c, a, b, t, _) -> lower_ternary s c a b t
  | Hir.Splat (e, t, _) ->
      let* x = expr s e in
      let vt = ty t in
      let i1 = fresh s in
      E.insert s.fn i1 vt (Low.Zero vt) (Low.Const (Low.I32, 0L)) x;
      let i2 = fresh s in
      E.shuffle_zero s.fn i2 vt (Low.Local (i1, vt));
      Ok (Low.Local (i2, vt))
  | Hir.Const_array (n, t, _) ->
      let ptr_id = fresh s in
      E.global_ptr s.fn ptr_id n;
      let* alignment = place_alignment s (Hir.Const_array (n, t, Span.synthetic)) in
      load_value s t (Low.Local (ptr_id, Low.Pointer (ty t))) alignment

and address_step s operation pointer offset =
  let pointer_int = ty (Hir.Int Hir.Usize) in
  let pointer_bits = fresh s in
  E.cast s.fn pointer_bits "ptrtoint" (Low.Pointer Low.I8) pointer pointer_int;
  let result_bits = fresh s in
  E.bin s.fn result_bits operation pointer_int (Low.Local (pointer_bits, pointer_int)) offset;
  let result = fresh s in
  E.cast s.fn result "inttoptr" pointer_int (Low.Local (result_bits, pointer_int)) (Low.Pointer Low.I8);
  Ok (Low.Local (result, Low.Pointer Low.I8))

and exprs s xs = Result_list.map (expr s) xs

and lower_builtin s b args t span =
  let* vs = exprs s args in
  let rt = ty t in
  match (b, vs) with
  | Volatile_load access_ty, [ pointer ] -> volatile_load s span access_ty pointer
  | Atomic (op, order, _), values ->
      (* the accessed type: the stored value's for a store, else the result's *)
      let access =
        match (op, values) with
        | Hir.Atomic_store, [ _; v ] -> Low.value_ty v
        | _ -> rt
      in
      if rt = Low.Void then (
        E.atomic s.fn None op order access values;
        Ok (Low.Const (Low.I1, 0L)))
      else
        let id = fresh s in
        E.atomic s.fn (Some id) op order access values;
        Ok (Low.Local (id, rt))
  | Simd_load (access_ty, kind), values ->
      simd_memory_load s span access_ty kind args values
  | Addr_bits, [ x ] ->
      let id = fresh s in
      E.cast s.fn id "ptrtoint" (Low.value_ty x) x rt;
      Ok (Low.Local (id, rt))
  | Syscall, values ->
      let* words =
        Result_list.map
          (fun (arg, v) ->
            match (Hir.expr_ty arg, Low.value_ty v) with
            | _, Low.Pointer _ ->
                let id = fresh s in
                E.cast s.fn id "ptrtoint" (Low.value_ty v) v Low.I64;
                Ok (Low.Local (id, Low.I64))
            | _, Low.I64 -> Ok v
            | Hir.Int (Hir.I8 | I16 | I32 | Isize), from ->
                let id = fresh s in
                E.cast s.fn id "sext" from v Low.I64;
                Ok (Low.Local (id, Low.I64))
            | _, from ->
                let id = fresh s in
                E.cast s.fn id "zext" from v Low.I64;
                Ok (Low.Local (id, Low.I64)))
          (List.combine args values)
      in
      let id = fresh s in
      E.syscall s.fn id words;
      Ok (Low.Local (id, Low.I64))
  | Addr_from_bits, [ x ] ->
      let id = fresh s in
      E.cast s.fn id "inttoptr" (Low.value_ty x) x (Low.Pointer Low.I8);
      Ok (Low.Local (id, Low.Pointer Low.I8))
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
        E.cast s.fn id opcode (Low.value_ty x) x rt;
        Ok (Low.Local (id, rt)))
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
          | Low.Vector (lanes, elem) ->
              let elem_name =
                match elem with
                | Low.I8 -> "i8"
                | I16 -> "i16"
                | I32 -> "i32"
                | I64 -> "i64"
                | _ -> "i64"
              in
              Printf.sprintf "v%d%s" lanes elem_name
          | _ -> Printf.sprintf "i%d" width
        in
        let id = fresh s in
        E.call s.fn (Some id) Low.No_extension rt (Printf.sprintf "llvm.%s.sat.%s.%s" conversion result_suffix format) [ (Low.value_ty x, Low.No_extension, x) ];
        Ok (Low.Local (id, rt))
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
      E.call s.fn (Some id) Low.No_extension rt (name ^ suffix) (List.map (fun value -> (rt, Low.No_extension, value)) values);
      Ok (Low.Local (id, rt))
  | (Handle_addr | Handle_from_addr _), [ x ] -> Ok x
  | (Rotl | Rotr), [ x; n ] ->
      let* n = shift_amount s span rt n in
      let* suffix = intrinsic_suffix span rt in
      let id = fresh s in
      let base = if b = Rotl then "llvm.fshl." else "llvm.fshr." in
      E.call s.fn (Some id) Low.No_extension rt (base ^ suffix) [ (rt, Low.No_extension, x); (rt, Low.No_extension, x); (rt, Low.No_extension, n); ];
      Ok (Low.Local (id, rt))
  | Popcount, [ x ] ->
      let* suffix = intrinsic_suffix span rt in
      let id = fresh s in
      E.call s.fn (Some id) Low.No_extension rt ("llvm.ctpop." ^ suffix) [ (rt, Low.No_extension, x) ];
      Ok (Low.Local (id, rt))
  | (Ctz | Clz), [ x ] ->
      let* suffix = intrinsic_suffix span rt in
      let id = fresh s in
      let base = if b = Ctz then "llvm.cttz." else "llvm.ctlz." in
      E.call s.fn (Some id) Low.No_extension rt (base ^ suffix) [ (rt, Low.No_extension, x); (Low.I1, Low.No_extension, Low.Const (Low.I1, 0L)); ];
      Ok (Low.Local (id, rt))
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
                | Reduce_or | Reduce_xor | Compress | Expand | Addr_bits | Syscall
                | Addr_from_bits | Cvt _ | Float_math _ | Handle_addr
                | Handle_from_addr _ | Volatile_load _ | Simd_load _ | Atomic _ ),
                _ ) ->
                error span "internal error: invalid saturating builtin")
      in
      let id = fresh s in
      E.call s.fn (Some id) Low.No_extension rt (base ^ suffix) [ (rt, Low.No_extension, x); (rt, Low.No_extension, y) ];
      Ok (Low.Local (id, rt))
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
            match rt with Low.Vector (n, e) -> (e, n) | e -> (e, 1)
          in
          let* elem_bits = width span elem_ty in
          let* wide_elem =
            match elem_bits with
            | 8 -> Ok Low.I16
            | 16 -> Ok Low.I32
            | 32 -> Ok Low.I64
            | 64 -> Ok Low.I128
            | _ -> error span "internal error: multiply-high operand width"
          in
          let wide =
            match rt with
            | Low.Vector (n, _) -> Low.Vector (n, wide_elem)
            | _ -> wide_elem
          in
          let cast_kind = if unsigned then "zext" else "sext" in
          let ex = fresh s in
          E.cast s.fn ex cast_kind rt x wide;
          let ey = fresh s in
          E.cast s.fn ey cast_kind rt y wide;
          let product = fresh s in
          E.bin s.fn product Low.Mul wide (Low.Local (ex, wide)) (Low.Local (ey, wide));
          let amount =
            match rt with
            | Low.Vector _ ->
                Low.Const_vector (wide, List.init lanes (fun _ -> Int64.of_int elem_bits))
            | _ -> Low.Const (wide, Int64.of_int elem_bits)
          in
          let high = fresh s in
          E.bin s.fn high (if unsigned then Low.Lshr else Low.Ashr) wide (Low.Local (product, wide)) amount;
          let id = fresh s in
          E.cast s.fn id "trunc" wide (Low.Local (high, wide)) rt;
          Ok (Low.Local (id, rt)))
  | Select, [ m; y; z ] ->
      let id = fresh s in
      E.select s.fn id m y z;
      Ok (Low.Local (id, rt))
  | Shuffle, [ a; b; sel ] -> (
      match sel with
      | Low.Const_vector (_, values) ->
          let id = fresh s in
          let mask = Low.Const_vector (Low.Vector (List.length values, Low.I32), values) in
          E.shufflevector s.fn id rt a b mask;
          Ok (Low.Local (id, rt))
      | _ -> error span "internal error: shuffle selectors must be constant")
  | Permute, [ v; idx ] ->
      let n = match Hir.expr_ty (List.hd args) with Hir.Vec (n, _) -> n | _ -> 1 in
      let iw = match value_ty idx with Low.Vector (_, w) -> w | _ -> Low.I32 in
      let elem_ty = match rt with Low.Vector (_, e) -> e | _ -> Low.I1 in
      let m = match rt with Low.Vector (m, _) -> m | _ -> 1 in
      let rec chain j acc =
        if j = m then Ok acc
        else
          let slot = Low.Const (Low.I64, Int64.of_int j) in
          let iid = fresh s in
          E.extract s.fn iid (value_ty idx) idx slot;
          let il = Low.Local (iid, iw) in
          let all_indices_fit = match iw with Low.I8 -> n >= 256 | _ -> false in
          let vl, sl =
            if all_indices_fit then (Low.Const (Low.I1, 1L), il)
            else
              let vid = fresh s in
              E.cmp s.fn vid Low.Ult iw il (Low.Const (iw, Int64.of_int n));
              let vl = Low.Local (vid, Low.I1) in
              let sid = fresh s in
              E.select s.fn sid vl il (Low.Const (iw, 0L));
              (vl, Low.Local (sid, iw))
          in
          let eid = fresh s in
          E.extract s.fn eid (value_ty v) v sl;
          let el = Low.Local (eid, elem_ty) in
          let pid = fresh s in
          E.select s.fn pid vl el (Low.Zero elem_ty);
          let pl = Low.Local (pid, elem_ty) in
          let nid = fresh s in
          E.insert s.fn nid rt acc slot pl;
          chain (j + 1) (Low.Local (nid, rt))
      in
      chain 0 (Low.Zero rt)
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
        E.bin s.fn bid op rt a b;
        Ok (Low.Local (bid, rt))
      in
      let pick cmp a b =
        let cid = fresh s in
        E.cmp s.fn cid cmp rt a b;
        let sid = fresh s in
        E.select s.fn sid (Low.Local (cid, Low.I1)) a b;
        Ok (Low.Local (sid, rt))
      in
      let rec chain i acc =
        match acc with
        | None ->
            let eid = fresh s in
            E.extract s.fn eid (value_ty v) v (Low.Const (Low.I64, Int64.of_int i));
            chain (i + 1) (Some (Low.Local (eid, rt)))
        | Some a ->
            if i = lanes then Ok a
            else
              let eid = fresh s in
              E.extract s.fn eid (value_ty v) v (Low.Const (Low.I64, Int64.of_int i));
              let el = Low.Local (eid, rt) in
              let* next =
                match b with
                | Reduce_sum -> bin Low.Add a el
                | Reduce_and -> bin Low.And a el
                | Reduce_or -> bin Low.Or a el
                | Reduce_xor -> bin Low.Xor a el
                | Reduce_min -> pick (if signed then Low.Slt else Low.Ult) a el
                | Reduce_max -> pick (if signed then Low.Sgt else Low.Ugt) a el
                | _ -> error span "internal error: invalid reduction builtin"
              in
              chain (i + 1) (Some next)
      in
      chain 0 None
  | Compress, [ v; mk ]
    when (Target_layout.current ()).os = Target_layout.Wasi
         && match rt with Low.Vector (n, _) -> n > 1 | _ -> false ->
      let bits_ty =
        match rt with
        | Low.Vector (n, Low.F32) -> Low.Vector (n, Low.I32)
        | Low.Vector (n, Low.F64) -> Low.Vector (n, Low.I64)
        | Low.Vector (n, Low.I1) -> Low.Vector (n, Low.I8)
        | t -> t
      in
      let convert kind value from target =
        if from = target then value
        else
          let id = fresh s in
          E.cast s.fn id kind from value target;
          Low.Local (id, target)
      in
      let into, back =
        match rt with
        | Low.Vector (_, Low.I1) -> ("zext", "trunc")
        | _ -> ("bitcast", "bitcast")
      in
      let bitcast value from target =
        convert (if from = rt then into else back) value from target
      in
      let* name = intrinsic_suffix span bits_ty in
      let id = fresh s in
      E.call s.fn (Some id) Low.No_extension bits_ty ("llvm.experimental.vector.compress." ^ name) [ (bits_ty, Low.No_extension, bitcast v rt bits_ty); (value_ty mk, Low.No_extension, mk); (bits_ty, Low.No_extension, Low.Zero bits_ty); ];
      Ok (bitcast (Low.Local (id, bits_ty)) bits_ty rt)
  | Compress, [ v; mk ] ->
      let n = match Hir.expr_ty (List.hd args) with Hir.Vec (n, _) -> n | _ -> 1 in
      let elem_ty = match rt with Low.Vector (_, e) -> e | _ -> Low.I1 in
      let rec chain i acc count =
        if i = n then Ok acc
        else
          let slot = Low.Const (Low.I64, Int64.of_int i) in
          let mid = fresh s in
          E.extract s.fn mid (value_ty mk) mk slot;
          let ml = Low.Local (mid, Low.I1) in
          let vid = fresh s in
          E.extract s.fn vid (value_ty v) v slot;
          let vl = Low.Local (vid, elem_ty) in
          let cid = fresh s in
          E.select s.fn cid ml count (Low.Const (Low.I64, 0L));
          let cl = Low.Local (cid, Low.I64) in
          let iid = fresh s in
          E.insert s.fn iid rt acc cl vl;
          let il = Low.Local (iid, rt) in
          let aid = fresh s in
          E.select s.fn aid ml il acc;
          let n1 = fresh s in
          E.bin s.fn n1 Low.Add Low.I64 count (Low.Const (Low.I64, 1L));
          let nid = fresh s in
          E.select s.fn nid ml (Low.Local (n1, Low.I64)) count;
          chain (i + 1) (Low.Local (aid, rt)) (Low.Local (nid, Low.I64))
      in
      chain 0 (Low.Zero rt) (Low.Const (Low.I64, 0L))
  | Expand, [ v; mk ] ->
      let n = match Hir.expr_ty (List.hd args) with Hir.Vec (n, _) -> n | _ -> 1 in
      let elem_ty = match rt with Low.Vector (_, e) -> e | _ -> Low.I1 in
      let rec chain d acc count =
        if d = n then Ok acc
        else
          let mid = fresh s in
          E.extract s.fn mid (value_ty mk) mk (Low.Const (Low.I64, Int64.of_int d));
          let ml = Low.Local (mid, Low.I1) in
          let cid = fresh s in
          E.select s.fn cid ml count (Low.Const (Low.I64, 0L));
          let cl = Low.Local (cid, Low.I64) in
          let vid = fresh s in
          E.extract s.fn vid (value_ty v) v cl;
          let vl = Low.Local (vid, elem_ty) in
          let iid = fresh s in
          E.insert s.fn iid rt acc (Low.Const (Low.I64, Int64.of_int d)) vl;
          let il = Low.Local (iid, rt) in
          let aid = fresh s in
          E.select s.fn aid ml il acc;
          let n1 = fresh s in
          E.bin s.fn n1 Low.Add Low.I64 count (Low.Const (Low.I64, 1L));
          let nid = fresh s in
          E.select s.fn nid ml (Low.Local (n1, Low.I64)) count;
          chain (d + 1) (Low.Local (aid, rt)) (Low.Local (nid, Low.I64))
      in
      chain 0 (Low.Zero rt) (Low.Const (Low.I64, 0L))
  | (Any | All), [ mv ] -> (
      let lanes =
        match Hir.expr_ty (List.hd args) with Hir.Vec (n, _) -> n | _ -> 1
      in
      let op = if b = Any then Low.Or else Low.And in
      let rec chain i acc =
        if i = lanes then Ok acc
        else
          let lid = fresh s in
          E.extract s.fn lid (value_ty mv) mv (Low.Const (Low.I64, Int64.of_int i));
          let lv = Low.Local (lid, Low.I1) in
          match acc with
          | None -> chain (i + 1) (Some lv)
          | Some a ->
              let bid = fresh s in
              E.bin s.fn bid op Low.I1 a lv;
              chain (i + 1) (Some (Low.Local (bid, Low.I1)))
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
  E.cond_br s.fn lv (if op = Ast.And then rhs.id else join.id) (if op = Ast.And then join.id else rhs.id);
  enter s rhs;
  let* rv = expr s b in
  let* rv = require_bool (Hir.expr_span b) rv in
  let rp = s.current.id in
  if open_block s then E.br s.fn join.id;
  enter s join;
  let id = fresh s in
  E.phi s.fn id Low.I1 [ ((if op = Ast.And then Low.Const (Low.I1, 0L) else Low.Const (Low.I1, 1L)), pred); (rv, rp); ];
  Ok (Low.Local (id, Low.I1))

and lower_ternary s c a b t =
  let* cv = expr s c in
  let* cv = require_bool (Hir.expr_span c) cv in
  let tb = fresh_block s and eb = fresh_block s and join = fresh_block s in
  E.cond_br s.fn cv tb.id eb.id;
  enter s tb;
  let* tv = expr s a in
  let tp = s.current.id in
  if open_block s then E.br s.fn join.id;
  enter s eb;
  let* ev = expr s b in
  let ep = s.current.id in
  if open_block s then E.br s.fn join.id;
  enter s join;
  let id = fresh s in
  E.phi s.fn id (ty t) [ (tv, tp); (ev, ep) ];
  Ok (Low.Local (id, ty t))

and materialize s e =
  let* v = expr s e in
  let ht = Hir.expr_ty e in
  let* alignment = align s ht in
  let id = fresh s in
  E.alloca_entry s.fn id (storage_ty ht) alignment;
  let p = Low.Local (id, Low.Pointer (storage_ty ht)) in
  let* () = store_value s ht v p alignment in
  Ok p

and address s e =
  match e with
  | Hir.Global (name, global_ty, _) ->
      let id = fresh s in
      E.global_ptr s.fn id name;
      Ok (Low.Local (id, Low.Pointer (storage_ty global_ty)))
  | Hir.Local (local, sp) -> (
      match Hashtbl.find_opt s.env local.id with
      | Some (value, _) -> Ok value
      | None -> error sp ("unknown local `" ^ local.name ^ "`"))
  | Hir.Const_array (n, t, _) ->
      let id = fresh s in
      E.global_ptr s.fn id n;
      Ok (Low.Local (id, Low.Pointer (storage_ty t)))
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
      E.gep s.fn id (ty (Hir.expr_ty a)) p [ Low.Zero; Low.Index iv ];
      Ok (Low.Local (id, Low.Pointer Low.I8))
  | _ -> error (Hir.expr_span a) "cannot take index address"

and raw_address s base off =
  let* bv = expr s base in
  let* ov = expr s off in
  let id = fresh s in
  E.gep s.fn id Low.I8 bv [ Low.Index ov ];
  Ok (Low.Local (id, Low.Pointer Low.I8))

and int_ty_bytes = function
  | Low.I1 | Low.I8 -> 1
  | Low.I16 -> 2
  | Low.I32 -> 4
  | Low.F32 -> 4
  | Low.I64 -> 8
  | Low.F64 -> 8
  | Low.I128 -> 16
  | _ -> 8

and volatile_chunk remaining =
  if remaining >= 16 then (16, Low.I128)
  else if remaining >= 8 then (8, Low.I64)
  else if remaining >= 4 then (4, Low.I32)
  else if remaining >= 2 then (2, Low.I16)
  else (1, Low.I8)

and volatile_offset s pointer offset =
  if offset = 0 then Ok pointer
  else
    let id = fresh s in
    E.gep s.fn id Low.I8 pointer [ Low.Index (Low.Const (Low.I64, Int64.of_int offset)) ];
    Ok (Low.Local (id, Low.Pointer Low.I8))

and packed_bool_load s is_volatile lanes pointer =
  let vector_ty = Low.Vector (lanes, Low.I1) in
  let bytes = (lanes + 7) / 8 in
  let load id ty pointer =
    if is_volatile then E.load_volatile s.fn id ty pointer 1
    else E.load s.fn id ty pointer 1
  in
  let rec load_chunks byte_offset current =
    if byte_offset = bytes then Ok current
    else
      let chunk_bytes, chunk_ty = volatile_chunk (bytes - byte_offset) in
      let bits = chunk_bytes * 8 in
      let* chunk_pointer = volatile_offset s pointer byte_offset in
      let loaded = fresh s in
      load loaded chunk_ty chunk_pointer;
      let unpacked = fresh s in
      let unpacked_ty = Low.Vector (bits, Low.I1) in
      E.cast s.fn unpacked "bitcast" chunk_ty (Low.Local (loaded, chunk_ty)) unpacked_ty;
      let first_lane = byte_offset * 8 in
      let count = min (lanes - first_lane) bits in
      let rec insert_lane i result =
        if i = count then Ok result
        else
          let lane = first_lane + i in
          let extracted = fresh s in
          E.extract s.fn extracted unpacked_ty (Low.Local (unpacked, unpacked_ty)) (Low.Const (Low.I64, Int64.of_int i));
          let inserted = fresh s in
          E.insert s.fn inserted vector_ty result (Low.Const (Low.I64, Int64.of_int lane)) (Low.Local (extracted, Low.I1));
          insert_lane (i + 1) (Low.Local (inserted, vector_ty))
      in
      let* result = insert_lane 0 current in
      load_chunks (byte_offset + chunk_bytes) result
  in
  load_chunks 0 (Low.Undef vector_ty)

and packed_bool_store s is_volatile lanes value pointer =
  let bytes = (lanes + 7) / 8 in
  let store ty value pointer =
    if is_volatile then E.store_volatile s.fn ty value pointer 1
    else E.store s.fn ty value pointer 1
  in
  let rec store_chunks byte_offset =
    if byte_offset = bytes then Ok ()
    else
      let chunk_bytes, chunk_ty = volatile_chunk (bytes - byte_offset) in
      let bits = chunk_bytes * 8 in
      let first_lane = byte_offset * 8 in
      let lane_count = min bits (lanes - first_lane) in
      let mask =
        Low.Const_vector
          ( Low.Vector (bits, Low.I32),
            List.init bits (fun i ->
                Int64.of_int (if first_lane + i < lanes then first_lane + i else 0)) )
      in
      let packed = fresh s in
      let packed_ty = Low.Vector (bits, Low.I1) in
      E.shufflevector s.fn packed packed_ty value value mask;
      let packed_value = Low.Local (packed, packed_ty) in
      let* packed_value =
        if lane_count = bits then Ok packed_value
        else
          let valid_lanes =
            Low.Const_vector
              (packed_ty, List.init bits (fun i -> if i < lane_count then 1L else 0L))
          in
          let masked = fresh s in
          E.bin s.fn masked Low.And packed_ty packed_value valid_lanes;
          Ok (Low.Local (masked, packed_ty))
      in
      let stored = fresh s in
      E.cast s.fn stored "bitcast" packed_ty packed_value chunk_ty;
      let* chunk_pointer = volatile_offset s pointer byte_offset in
      store chunk_ty (Low.Local (stored, chunk_ty)) chunk_pointer;
      store_chunks (byte_offset + chunk_bytes)
  in
  store_chunks 0

and volatile_load s span access_ty pointer =
  match access_ty with
  | Hir.Bool ->
      let loaded = fresh s in
      E.load_volatile s.fn loaded Low.I8 pointer 1;
      let normalized = fresh s in
      E.cmp s.fn normalized Low.Ne Low.I8 (Low.Local (loaded, Low.I8)) (Low.Const (Low.I8, 0L));
      Ok (Low.Local (normalized, Low.I1))
  | Hir.Int _ ->
      let loaded = fresh s in
      E.load_volatile s.fn loaded (ty access_ty) pointer 1;
      Ok (Low.Local (loaded, ty access_ty))
  | Hir.Float _ ->
      let loaded = fresh s in
      E.load_volatile s.fn loaded (ty access_ty) pointer 1;
      Ok (Low.Local (loaded, ty access_ty))
  | Hir.Addr | Hir.Handle _ ->
      let loaded = fresh s in
      E.load_volatile s.fn loaded (Low.Pointer Low.I8) pointer 1;
      Ok (Low.Local (loaded, Low.Pointer Low.I8))
  | Hir.Vec (lanes, Hir.Int kind) ->
      let elem_ty = ty (Hir.Int kind) in
      let vector_ty = Low.Vector (lanes, elem_ty) in
      let elem_bytes = int_ty_bytes elem_ty in
      let rec load_lane i current =
        if i = lanes then Ok current
        else
          let* lane_pointer = volatile_offset s pointer (i * elem_bytes) in
          let loaded = fresh s in
          E.load_volatile s.fn loaded elem_ty lane_pointer 1;
          let inserted = fresh s in
          E.insert s.fn inserted vector_ty current (Low.Const (Low.I64, Int64.of_int i)) (Low.Local (loaded, elem_ty));
          load_lane (i + 1) (Low.Local (inserted, vector_ty))
      in
      load_lane 0 (Low.Undef vector_ty)
  | Hir.Vec (lanes, Hir.Bool) -> packed_bool_load s true lanes pointer
  | _ -> error span "internal error: invalid volatile access type"

and volatile_store s span access_ty value pointer =
  match access_ty with
  | Hir.Bool ->
      let stored = fresh s in
      E.cast s.fn stored "zext" Low.I1 value Low.I8;
      E.store_volatile s.fn Low.I8 (Low.Local (stored, Low.I8)) pointer 1;
      Ok ()
  | Hir.Int _ ->
      E.store_volatile s.fn (ty access_ty) value pointer 1;
      Ok ()
  | Hir.Float _ ->
      E.store_volatile s.fn (ty access_ty) value pointer 1;
      Ok ()
  | Hir.Addr | Hir.Handle _ ->
      E.store_volatile s.fn (Low.Pointer Low.I8) value pointer 1;
      Ok ()
  | Hir.Vec (lanes, Hir.Int kind) ->
      let elem_ty = ty (Hir.Int kind) in
      let elem_bytes = int_ty_bytes elem_ty in
      let vector_ty = Low.Vector (lanes, elem_ty) in
      let rec store_lane i =
        if i = lanes then Ok ()
        else
          let extracted = fresh s in
          E.extract s.fn extracted vector_ty value (Low.Const (Low.I64, Int64.of_int i));
          let* lane_pointer = volatile_offset s pointer (i * elem_bytes) in
          E.store_volatile s.fn elem_ty (Low.Local (extracted, elem_ty)) lane_pointer 1;
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
        E.bin s.fn id Low.Mul pointer_int offset (Low.Const (pointer_int, Int64.of_int bytes));
        Low.Local (id, pointer_int)
  in
  match stride with
  | Some _ ->
      let id = fresh s in
      E.gep s.fn id Low.I8 base [ Low.Index offset ];
      Ok (Low.Local (id, Low.Pointer Low.I8))
  | None ->
      let bits = fresh s in
      E.cast s.fn bits "ptrtoint" (Low.Pointer Low.I8) base pointer_int;
      let sum = fresh s in
      E.bin s.fn sum Low.Add pointer_int (Low.Local (bits, pointer_int)) offset;
      let pointer = fresh s in
      E.cast s.fn pointer "inttoptr" pointer_int (Low.Local (sum, pointer_int)) (Low.Pointer Low.I8);
      Ok (Low.Local (pointer, Low.Pointer Low.I8))

and simd_memory_load_lane s span access_ty pointer =
  match access_ty with
  | Hir.Bool ->
      let loaded = fresh s in
      E.load s.fn loaded Low.I8 pointer 1;
      let normalized = fresh s in
      E.cmp s.fn normalized Low.Ne Low.I8 (Low.Local (loaded, Low.I8)) (Low.Const (Low.I8, 0L));
      Ok (Low.Local (normalized, Low.I1))
  | Hir.Int _ ->
      let loaded = fresh s in
      E.load s.fn loaded (ty access_ty) pointer 1;
      Ok (Low.Local (loaded, ty access_ty))
  | Hir.Float _ ->
      let loaded = fresh s in
      E.load s.fn loaded (ty access_ty) pointer 1;
      Ok (Low.Local (loaded, ty access_ty))
  | _ -> error span "internal error: invalid SIMD memory element type"

and simd_memory_store_lane s span access_ty value pointer =
  match access_ty with
  | Hir.Bool ->
      let byte = fresh s in
      E.cast s.fn byte "zext" Low.I1 value Low.I8;
      E.store s.fn Low.I8 (Low.Local (byte, Low.I8)) pointer 1;
      Ok ()
  | Hir.Int _ ->
      E.store s.fn (ty access_ty) value pointer 1;
      Ok ()
  | Hir.Float _ ->
      E.store s.fn (ty access_ty) value pointer 1;
      Ok ()
  | _ -> error span "internal error: invalid SIMD memory element type"

and masked_elem_ty access_ty =
  match access_ty with Hir.Bool -> Low.I8 | _ -> ty access_ty

and masked_mask_lanes span mask =
  match value_ty mask with
  | Low.Vector (lanes, Low.I1) -> Ok lanes
  | _ -> error span "internal error: masked SIMD memory mask is not a bool vector"

and masked_intrinsic span direction memory_ty =
  let* suffix =
    match memory_ty with
    | Low.Vector (_, (Low.F32 | Low.F64)) -> float_suffix span memory_ty
    | _ -> intrinsic_suffix span memory_ty
  in
  Ok (Printf.sprintf "llvm.masked.%s.%s.p0" direction suffix)

and simd_masked_load s span access_ty base mask fallback =
  let* lanes = masked_mask_lanes span mask in
  let mask_ty = Low.Vector (lanes, Low.I1) in
  let memory_ty = Low.Vector (lanes, masked_elem_ty access_ty) in
  let* name = masked_intrinsic span "load" memory_ty in
  let passthru =
    match access_ty with
    | Hir.Bool ->
        let id = fresh s in
        E.cast s.fn id "zext" (value_ty fallback) fallback memory_ty;
        Low.Local (id, memory_ty)
    | _ -> fallback
  in
  let loaded = fresh s in
  E.call s.fn (Some loaded) Low.No_extension memory_ty name [ (Low.Pointer Low.I8, Low.No_extension, base); (Low.I32, Low.No_extension, Low.Const (Low.I32, 1L)); (mask_ty, Low.No_extension, mask); (memory_ty, Low.No_extension, passthru); ];
  match access_ty with
  | Hir.Bool ->
      let normalized = fresh s in
      E.cmp s.fn normalized Low.Ne memory_ty (Low.Local (loaded, memory_ty)) (Low.Zero memory_ty);
      Ok (Low.Local (normalized, mask_ty))
  | _ -> Ok (Low.Local (loaded, memory_ty))

and simd_masked_store s span access_ty base mask stored =
  let* lanes = masked_mask_lanes span mask in
  let mask_ty = Low.Vector (lanes, Low.I1) in
  let memory_ty = Low.Vector (lanes, masked_elem_ty access_ty) in
  let* name = masked_intrinsic span "store" memory_ty in
  let value =
    match access_ty with
    | Hir.Bool ->
        let id = fresh s in
        E.cast s.fn id "zext" (value_ty stored) stored memory_ty;
        Low.Local (id, memory_ty)
    | _ -> stored
  in
  E.call s.fn None Low.No_extension Low.Void name [ (memory_ty, Low.No_extension, value); (Low.Pointer Low.I8, Low.No_extension, base); (Low.I32, Low.No_extension, Low.Const (Low.I32, 1L)); (mask_ty, Low.No_extension, mask); ];
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
  let lanes = match value_ty mask with Low.Vector (n, Low.I1) -> n | _ -> 0 in
  let result_ty = Low.Vector (lanes, ty access_ty) in
  let rec load_lane lane result =
    if lane = lanes then Ok result
    else
      let lane_index = Low.Const (Low.I64, Int64.of_int lane) in
      let mask_id = fresh s in
      E.extract s.fn mask_id (value_ty mask) mask lane_index;
      let fallback_id = fresh s in
      E.extract s.fn fallback_id (value_ty fallback) fallback lane_index;
      let inactive = s.current.id in
      let active = fresh_block s and join = fresh_block s in
      E.cond_br s.fn (Low.Local (mask_id, Low.I1)) active.id join.id;
      enter s active;
      let index_value =
        match indices with
        | None -> Low.Const (ty (Hir.Int Hir.Usize), Int64.of_int lane)
        | Some vector ->
            let index_id = fresh s in
            E.extract s.fn index_id (value_ty vector) vector lane_index;
            Low.Local
              ( index_id,
                match value_ty vector with Low.Vector (_, elem) -> elem | _ -> Low.I64 )
      in
      let* pointer = simd_memory_address s span stride base index_ty index_value in
      let* loaded = simd_memory_load_lane s span access_ty pointer in
      let active_end = s.current.id in
      E.br s.fn join.id;
      enter s join;
      let merged = fresh s in
      E.phi s.fn merged (ty access_ty) [ (loaded, active_end); (Low.Local (fallback_id, ty access_ty), inactive) ];
      let inserted = fresh s in
      E.insert s.fn inserted result_ty result lane_index (Low.Local (merged, ty access_ty));
      load_lane (lane + 1) (Low.Local (inserted, result_ty))
  in
  load_lane 0 (Low.Zero result_ty)

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
      let lanes = match value_ty mask with Low.Vector (n, Low.I1) -> n | _ -> 0 in
      let rec store_lane lane =
        if lane = lanes then Ok ()
        else
          let lane_index = Low.Const (Low.I64, Int64.of_int lane) in
          let mask_id = fresh s in
          E.extract s.fn mask_id (value_ty mask) mask lane_index;
          let active = fresh_block s and join = fresh_block s in
          E.cond_br s.fn (Low.Local (mask_id, Low.I1)) active.id join.id;
          enter s active;
          let index_id = fresh s in
          E.extract s.fn index_id (value_ty indices) indices lane_index;
          let index_value =
            Low.Local
              ( index_id,
                match value_ty indices with Low.Vector (_, elem) -> elem | _ -> Low.I64 )
          in
          let* pointer = simd_memory_address s span stride base index_ty index_value in
          let value_id = fresh s in
          E.extract s.fn value_id (value_ty stored) stored lane_index;
          let* () =
            simd_memory_store_lane s span access_ty
              (Low.Local (value_id, ty access_ty))
              pointer
          in
          E.br s.fn join.id;
          enter s join;
          store_lane (lane + 1)
      in
      store_lane 0
  | _ -> error span "internal error: invalid SIMD memory store"

and raw_load s t p =
  match t with
  | Hir.Float _ ->
      let id = fresh s in
      E.load s.fn id (ty t) p 1;
      Ok (Low.Local (id, ty t))
  | Hir.Int k ->
      let it = ty (Hir.Int k) in
      let id = fresh s in
      E.load s.fn id it p 1;
      Ok (Low.Local (id, it))
  | Hir.Bool ->
      let id = fresh s in
      E.load s.fn id Low.I8 p 1;
      let cid = fresh s in
      E.cmp s.fn cid Low.Ne Low.I8 (Low.Local (id, Low.I8)) (Low.Const (Low.I8, 0L));
      Ok (Low.Local (cid, Low.I1))
  | Hir.Addr | Hir.Handle _ ->
      let pt = Low.Pointer Low.I8 in
      let id = fresh s in
      E.load s.fn id pt p 1;
      Ok (Low.Local (id, pt))
  | Hir.Vec (_, (Hir.Float _ | Hir.Int _)) ->
      (* every lane type is whole bytes: one unaligned vector load *)
      let id = fresh s in
      let vector_ty = ty t in
      E.load s.fn id vector_ty p 1;
      Ok (Low.Local (id, vector_ty))
  | Hir.Vec (lanes, Hir.Bool) -> packed_bool_load s false lanes p
  | _ -> error Span.synthetic "internal error: aggregate raw load"

and raw_store s t v p =
  match t with
  | Hir.Float _ ->
      E.store s.fn (ty t) v p 1;
      Ok ()
  | Hir.Int k ->
      let it = ty (Hir.Int k) in
      E.store s.fn it v p 1;
      Ok ()
  | Hir.Bool ->
      let zid = fresh s in
      E.cast s.fn zid "zext" Low.I1 v Low.I8;
      E.store s.fn Low.I8 (Low.Local (zid, Low.I8)) p 1;
      Ok ()
  | Hir.Addr | Hir.Handle _ ->
      E.store s.fn (Low.Pointer Low.I8) v p 1;
      Ok ()
  | Hir.Vec (_, (Hir.Float _ | Hir.Int _)) ->
      E.store s.fn (ty t) v p 1;
      Ok ()
  | Hir.Vec (lanes, Hir.Bool) -> packed_bool_store s false lanes v p
  | _ -> error Span.synthetic "internal error: aggregate raw store"

and store_value s t value pointer alignment =
  match t with
  | Hir.Bool ->
      let byte = fresh s in
      E.cast s.fn byte "zext" Low.I1 value Low.I8;
      E.store s.fn Low.I8 (Low.Local (byte, Low.I8)) pointer alignment;
      Ok ()
  | Hir.Vec (lanes, Hir.Bool) when lanes mod 8 <> 0 -> raw_store s t value pointer
  | _ ->
      E.store s.fn (ty t) value pointer alignment;
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
  E.gep s.fn id Low.I8 p [ Low.Index (Low.Const (Low.I64, Int64.of_int off)) ];
  Ok (Low.Local (id, Low.Pointer Low.I8))

let copy_offset s pointer offset =
  let id = fresh s in
  E.gep s.fn id Low.I8 pointer [ Low.Index (Low.Const (Low.I64, Int64.of_int offset)) ];
  Ok (Low.Local (id, Low.Pointer Low.I8))

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
  E.br s.fn head.id;
  enter s head;
  E.phi s.fn index Low.I64 [ (Low.Const (Low.I64, 0L), preheader); (Low.Local (next_index, Low.I64), latch.id); ];
  let condition = fresh s in
  E.cmp s.fn condition Low.Ult Low.I64 (Low.Local (index, Low.I64)) (Low.Const (Low.I64, Int64.of_int length));
  E.cond_br s.fn (Low.Local (condition, Low.I1)) body.id exit.id;
  enter s body;
  let offset =
    if stride = 1 then Low.Local (index, Low.I64)
    else
      let scaled = fresh s in
      E.bin s.fn scaled Low.Mul Low.I64 (Low.Local (index, Low.I64)) (Low.Const (Low.I64, Int64.of_int stride));
      Low.Local (scaled, Low.I64)
  in
  let destination_element = fresh s in
  E.gep s.fn destination_element Low.I8 destination [ Low.Index offset ];
  let source_element = fresh s in
  E.gep s.fn source_element Low.I8 source [ Low.Index offset ];
  let* () =
    copy_place s span element_ty
      (Low.Local (destination_element, Low.Pointer Low.I8))
      (Low.Local (source_element, Low.Pointer Low.I8))
  in
  if open_block s then E.br s.fn latch.id;
  enter s latch;
  E.bin s.fn next_index Low.Add Low.I64 (Low.Local (index, Low.I64)) (Low.Const (Low.I64, 1L));
  E.br s.fn head.id;
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
  E.call s.fn None Low.No_extension Low.Void ("llvm.lifetime." ^ operation ^ ".p0") [ (Low.I64, Low.No_extension, Low.Const (Low.I64, Int64.of_int slot.size)); (Low.value_ty slot.pointer, Low.No_extension, slot.pointer); ];
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
      E.alloca_entry s.fn id (storage_ty local.ty) alignment;
      let p = Low.Local (id, Low.Pointer (storage_ty local.ty)) in
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
      E.alloca_entry s.fn id (storage_ty local.ty) alignment;
      let pointer = Low.Local (id, Low.Pointer (storage_ty local.ty)) in
      let* () = register_local_lifetime s local pointer in
      bind_local s local pointer alignment;
      construct_into ~alignment s pointer construction
  | Hir.View (local, place, span) ->
      let* pointer = address s place in
      let* alignment = place_alignment s place in
      if match value_ty pointer with Low.Pointer _ -> true | _ -> false then (
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
          E.alloca_entry s.fn scratch_id (storage_ty copy_ty) alignment;
          let scratch = Low.Local (scratch_id, Low.Pointer (storage_ty copy_ty)) in
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
          E.load s.fn loaded_id vt p alignment;
          let loaded = Low.Local (loaded_id, vt) in
          let ins = fresh s in
          E.insert s.fn ins vt loaded iv x;
          store_value s source_ty (Low.Local (ins, vt)) p alignment
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
          E.load s.fn loaded_id vt p alignment;
          let loaded = Low.Local (loaded_id, vt) in
          let eid = fresh s in
          E.extract s.fn eid vt loaded iv;
          let it = ty t in
          let old = Low.Local (eid, it) in
          let* rhs = expr s e in
          let* value = emit_binary s span t op it old rhs in
          let latest_id = fresh s in
          E.load s.fn latest_id vt p alignment;
          let latest = Low.Local (latest_id, vt) in
          let ins = fresh s in
          E.insert s.fn ins vt latest iv value;
          store_value s source_ty (Low.Local (ins, vt)) p alignment
      | Hir.ARaw (b, off, at) ->
          let* p = raw_address s b off in
          let* old = raw_load s at p in
          let* rhs = expr s e in
          let* value =
            match (at, op) with
            | Hir.Addr, Ast.Add -> address_step s Low.Add old rhs
            | Hir.Addr, Ast.Sub -> address_step s Low.Sub old rhs
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
            | Hir.Addr, Ast.Add -> address_step s Low.Add old rhs
            | Hir.Addr, Ast.Sub -> address_step s Low.Sub old rhs
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
      E.ret s.fn v;
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
          E.br s.fn l.break_to;
          Ok ()
      | None -> error sp "break outside loop or switch")
  | Hir.Continue (label, sp) -> (
      match target_loop s label with
      | Some l ->
          let* () = unwind s l.keep_scopes in
          mark s sp;
          E.br s.fn l.continue_to;
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
            store_value s aggregate_ty (Low.Zero (ty aggregate_ty)) destination a
          else
            let preheader = s.current.id in
            let head = fresh_block s
            and body = fresh_block s
            and exit = fresh_block s in
            let index = fresh s and next = fresh s in
            E.br s.fn head.id;
            enter s head;
            E.phi s.fn index Low.I64 [ (Low.Const (Low.I64, 0L), preheader); (Low.Local (next, Low.I64), body.id); ];
            let condition = fresh s in
            E.cmp s.fn condition Low.Ult Low.I64 (Low.Local (index, Low.I64)) (Low.Const (Low.I64, Int64.of_int size));
            E.cond_br s.fn (Low.Local (condition, Low.I1)) body.id exit.id;
            enter s body;
            let pointer = fresh s in
            E.gep s.fn pointer Low.I8 destination [ Low.Index (Low.Local (index, Low.I64)) ];
            E.store s.fn Low.I8 (Low.Const (Low.I8, 0L)) (Low.Local (pointer, Low.Pointer Low.I8)) 1;
            E.bin s.fn next Low.Add Low.I64 (Low.Local (index, Low.I64)) (Low.Const (Low.I64, 1L));
            E.br s.fn head.id;
            enter s exit;
            Ok ()
      | _ -> error span "internal error: zero initializer requires an array or struct")
  | Hir.Init_aggregate (Hir.Array (length, element_ty), false, elements, span) ->
      if List.length elements <> length then
        error span "internal error: array construction arity"
      else
        let aggregate_ty = Low.Array (length, storage_ty element_ty) in
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
              E.gep s.fn pointer_id aggregate_ty destination [ Low.Zero; Low.Index (Low.Const (Low.I64, Int64.of_int index)) ];
              let element_alignment =
                min
                  (offset_alignment base_alignment (index * element_size))
                  element_natural_alignment
              in
              let* () =
                construct_into ~alignment:element_alignment s
                  (Low.Local (pointer_id, Low.Pointer (storage_ty element_ty)))
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
      E.global_ptr s.fn id name;
      Ok (Low.Local (id, Low.Pointer (storage_ty global_ty)))
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
  E.cond_br s.fn cv tb.id eb.id;
  enter s tb;
  let* () = scoped s a in
  if open_block s then E.br s.fn join.id;
  enter s eb;
  let* () = match b with None -> Ok () | Some xs -> scoped s xs in
  if open_block s then E.br s.fn join.id;
  enter s join;
  let falls_through =
    (Hir.block_flow a).falls_through
    || match b with None -> true | Some body -> (Hir.block_flow body).falls_through
  in
  if not falls_through then E.unreachable s.fn;
  Ok ()

and target_loop s label =
  match label with
  | None -> List.nth_opt s.loops 0
  | Some name -> List.find_opt (fun loop -> loop.label = Some name) s.loops

and loop_backedge s span target =
  if open_block s then (
    mark s span;
    E.br s.fn target)

and lower_while s label c body span =
  let head = fresh_block s and bb = fresh_block s and exit = fresh_block s in
  E.br s.fn head.id;
  enter s head;
  mark s (Hir.expr_span c);
  let* cv = expr s c in
  let* cv = require_bool (Hir.expr_span c) cv in
  E.cond_br s.fn cv bb.id exit.id;
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
    E.unreachable s.fn;
  Ok ()

and lower_for s label init cond step body span =
  push_scope s;
  let lowered =
    let* () = match init with None -> Ok () | Some x -> stmt s x in
    let head = fresh_block s
    and bb = fresh_block s
    and sb = fresh_block s
    and exit = fresh_block s in
    E.br s.fn head.id;
    enter s head;
    let* () =
      match cond with
      | None ->
          E.br s.fn bb.id;
          Ok ()
      | Some c ->
          mark s (Hir.expr_span c);
          let* v = expr s c in
          let* condition = require_bool (Hir.expr_span c) v in
          E.cond_br s.fn condition bb.id exit.id;
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
    if open_block s then E.br s.fn sb.id;
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
      E.unreachable s.fn;
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
  E.switch s.fn (value_ty v) v cases db.id;
  let rec each as_ bs =
    match (as_, bs) with
    | [], [] -> Ok ()
    | (_, body) :: at, b :: bt ->
        enter s b;
        let* () = scoped s body in
        if open_block s then E.br s.fn join.id;
        each at bt
    | _ -> error span "internal error: switch arm/block mismatch"
  in
  let* () = each arms blocks in
  enter s db;
  let* () = match default with None -> Ok () | Some xs -> scoped s xs in
  if open_block s then E.br s.fn join.id;
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
  if not falls_through then E.unreachable s.fn;
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

let lower_func mc ~sanitize_address ~intrinsic_masked_loads ~address_taken
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
  (* (name, type, extension) *)
  let params =
    List.mapi
      (fun index (local : Hir.local) ->
        ( "a" ^ string_of_int index,
          ty local.ty,
          if c_abi then c_extension local.ty else Low.No_extension ))
      f.Hir.params
  in
  let ret_extension = if c_abi then c_extension f.ret else Low.No_extension in
  match f.body with
  | Hir.Declaration ->
      E.declare mc ~name:f.name
        ~params:(List.map (fun (_, t, e) -> (t, e)) params)
        ~ret:(ty f.ret) ~ret_extension ~variadic:f.variadic ~display:f.display;
      Ok ()
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
      let linkage =
        if f.name = "main" then Fir.Global
        else if f.linkage = Hir.Internal then Fir.Local
        else Fir.Global
      in
      let fn =
        E.start mc ~name:f.name ~linkage ~params ~ret:(ty f.ret) ~ret_extension
          ~variadic:f.variadic ~display:f.display ~source
      in
      let s =
        {
          fn;
          literal_globals;
          next_value = 0;
          current = { id = Fir.entry };
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
        | (local : Hir.local) :: local_rest, (name, _, _) :: parameter_rest ->
            let* alignment = align s local.ty in
            let id = fresh s in
            E.alloca s.fn id (storage_ty local.ty) alignment;
            let p = Low.Local (id, Low.Pointer (storage_ty local.ty)) in
            let* () = register_local_lifetime s local p in
            bind_local s local p alignment;
            let* () =
              store_value s local.ty (Low.Param (name, ty local.ty)) p alignment
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
        else if s.ret = Low.Void then (
          E.ret s.fn None;
          Ok ())
        else
          error Span.synthetic
            (Printf.sprintf
               "internal error: non-void function `%s` ended lowering without a \
                terminator"
               f.name)
      in
      pop_scope s;
      match E.finish s.fn with
      | Ok () -> Ok ()
      | Error b ->
          error Span.synthetic
            (Printf.sprintf
               "internal error: function `%s` block %d ended lowering without a \
                terminator"
               f.name b)

let global_storage mc structs (global : Hir.global) =
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
        E.storage_global mc ~name:global.name ~size ~bytes ~pointers:(List.rev !pointers)
          ~readonly:global.readonly ~align ~linkage:global.linkage;
        Ok ()
      with Failure message | Invalid_argument message ->
        error Span.synthetic
          (Printf.sprintf "internal error: global %s: %s" global.name message))

let lower_module ?(sanitize_address = false) ?(intrinsic_masked_loads = true) (p : Hir.program)
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
  let lay : E.layouts = Hashtbl.create 16 in
  let* () =
    Result_list.iter
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
              | [] -> Ok ([ Low.Array (d.size, Low.I8) ], d.size, 1)
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
              | [] -> Ok ([ Low.Array (d.size, Low.I8) ], d.size, natural_align)
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
                      if padding > 0 then Low.Array (padding, Low.I8) :: out else out
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
              Low.Array (0, Low.Vector (d.align, Low.I8)) :: fields
            else fields
          in
          (* the old Ir's struct type: Lower_emit only needs the layout *)
          let _ = (fields, used) in
          Ok (Hashtbl.replace lay d.name (d.size, d.align)))
      p.structs
  in
  let mc = E.create_module ~triple:(Target_layout.current ()).triple lay in
  (* variadic functions: their fixed parameter count, for call signatures *)
  List.iter
    (fun (f : Hir.func) ->
      if f.variadic then Hashtbl.replace mc.variadic f.name (List.length f.params))
    p.funcs;
  let literal_globals = ref [] in
  let* () =
    Result_list.iter
      (fun (f : Hir.func) ->
        let address_taken =
          match (sanitize_address, f.Hir.body) with
          | true, Hir.Statements body -> address_taken_locals body
          | _ -> Hashtbl.create 0
        in
        lower_func mc ~sanitize_address ~intrinsic_masked_loads ~address_taken
          ~addressed_functions literal_globals p.Hir.structs p.strings p.funcs f)
      p.funcs
  in
  List.iteri (fun i x -> E.string_global mc i x) p.strings;
  let* () =
    Result_list.iter
      (fun (a : Hir.const_arr_def) ->
        match a.ty with
        | Hir.Array (_, e) ->
            let* align = field_align e in
            E.array_global mc ~name:a.name ~elem_ty:(storage_ty e) ~elems:a.elems ~align;
            Ok ()
        | _ ->
            E.array_global mc ~name:a.name ~elem_ty:Low.I8 ~elems:[] ~align:1;
            Ok ())
      p.const_arrays
  in
  let* () =
    Result_list.iter (global_storage mc p.structs) (p.globals @ List.rev !literal_globals)
  in
  match Fir_verify.verify mc.m with
  | [] -> Ok mc.m
  | errors -> error Span.synthetic ("internal error: invalid Fir:\n" ^ String.concat "\n" errors)

let lower ?sanitize_address ?intrinsic_masked_loads p =
  (* a malformed request to Lower_emit is a lowering bug *)
  try lower_module ?sanitize_address ?intrinsic_masked_loads p
  with E.Unsupported message -> error Span.synthetic ("internal error: lowering to Fir: " ^ message)
