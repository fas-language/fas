open Sema_numeric
open Sema_types

let error span message = Error [ Diag.error span message ]

let global_error span message =
  Error [ Diag.error ~issue:Diag.Not_constant span message ]

let ( let* ) result continuation =
  match result with
  | Error diagnostics -> Error diagnostics
  | Ok value -> continuation value

let lookup name table = List.find_opt (fun (entry, _, _) -> entry = name) table

let shuffle_indices_in_range lane_ty n values =
  let signed =
    match lane_ty with
    | Hir.Int (Hir.I8 | Hir.I16 | Hir.I32 | Hir.I64 | Hir.Isize) -> true
    | _ -> false
  in
  List.for_all
    (fun v ->
      let v = if signed then sign_extend_value lane_ty v else v in
      Int64.compare v 0L >= 0 && Int64.compare v (Int64.of_int (2 * n)) < 0)
    values

let ty_name = Hir.ty_name

type unresolved_shape =
  | Unresolved_int
  | Unresolved_float
  | Unresolved_vector
  | Unresolved_null

let rec unresolved_shape_of expression =
  let combined left right =
    match (unresolved_shape_of left, unresolved_shape_of right) with
    | Some left_shape, Some right_shape when left_shape = right_shape -> Some left_shape
    | _ -> None
  in
  match expression with
  | Ast.Parenthesized (expression, _) -> unresolved_shape_of expression
  | Ast.Int_lit _ -> Some Unresolved_int
  | Ast.Float_lit _ -> Some Unresolved_float
  | Ast.Null _ -> Some Unresolved_null
  | Ast.Unary (Ast.Neg, operand, _) -> (
      match unresolved_shape_of operand with
      | Some (Unresolved_int | Unresolved_float | Unresolved_vector) as shape -> shape
      | _ -> None)
  | Ast.Unary (Ast.Bit_not, operand, _) -> (
      match unresolved_shape_of operand with
      | Some Unresolved_int -> Some Unresolved_int
      | _ -> None)
  | Ast.Binary
      ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
        | Ast.Bit_xor | Ast.Shl | Ast.Shr ),
        left,
        right,
        _ ) ->
      combined left right
  | Ast.Ternary (_, yes, no, _) -> combined yes no
  | Ast.Splat (_, _) | Ast.Array_lit _ -> Some Unresolved_vector
  | _ -> None

let rec unresolved_vector_elements expression =
  match expression with
  | Ast.Splat (element, _) -> (
      match unresolved_shape_of element with Some Unresolved_int -> true | _ -> false)
  | Ast.Binary
      ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
        | Ast.Bit_xor ),
        left,
        right,
        _ ) ->
      unresolved_vector_elements left && unresolved_vector_elements right
  | Ast.Unary (Ast.Neg, operand, _) -> unresolved_vector_elements operand
  | Ast.Ternary (_, yes, no, _) ->
      unresolved_vector_elements yes && unresolved_vector_elements no
  | _ -> false

let rec is_if_operand = function
  | Ast.Ternary _ -> true
  | Ast.Parenthesized (expression, _) -> is_if_operand expression
  | _ -> false

let operand_type_hint operation expected left right =
  if is_if_operand left || is_if_operand right then None
  else
    match operation with
    | Ast.Add | Ast.Sub -> (
        match expected with Some Hir.Addr -> Some (Hir.Int Hir.Usize) | e -> e)
    | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or | Ast.Bit_xor -> expected
    | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
        match expected with
        | Some (Hir.Vec (lanes, _))
          when unresolved_vector_elements left && unresolved_vector_elements right ->
            Some (Hir.Vec (lanes, Hir.Int Hir.I32))
        | _ -> None)
    | Ast.And | Ast.Or -> None
    | Ast.Shl | Ast.Shr -> None

let contextual_peer_type operation peer operand =
  if is_if_operand operand then None
  else
    match (peer, unresolved_shape_of operand, operation) with
    | Hir.Addr, Some Unresolved_null, (Ast.Eq | Ast.Ne) -> Some Hir.Addr
    | Hir.Addr, _, _ -> Some (Hir.Int Hir.Usize)
    | Hir.Handle _, Some Unresolved_int, _ -> None
    | peer, _, _ -> Some peer

let sat_apply name kind x y =
  let bits = int_bits kind in
  if is_unsigned (Hir.Int kind) then
    let max_w =
      if bits >= 64 then Int64.minus_one else Int64.sub (Int64.shift_left 1L bits) 1L
    in
    match name with
    | "add_sat" ->
        let sum = Int64.add x y in
        if Int64.unsigned_compare sum x < 0 || Int64.unsigned_compare sum max_w > 0 then
          max_w
        else sum
    | _ ->
        let difference = Int64.sub x y in
        if Int64.unsigned_compare y x > 0 then 0L else difference
  else
    let max_s =
      if bits >= 64 then Int64.max_int
      else Int64.sub (Int64.shift_left 1L (bits - 1)) 1L
    in
    let min_s =
      if bits >= 64 then Int64.min_int else Int64.neg (Int64.shift_left 1L (bits - 1))
    in
    let xs = sign_extend_value (Hir.Int kind) x in
    let ys = sign_extend_value (Hir.Int kind) y in
    match name with
    | "add_sat" ->
        if Int64.compare ys 0L > 0 && Int64.compare xs (Int64.sub max_s ys) > 0 then
          max_s
        else if Int64.compare ys 0L < 0 && Int64.compare xs (Int64.sub min_s ys) < 0
        then min_s
        else Int64.add xs ys
    | _ ->
        if Int64.compare ys 0L < 0 && Int64.compare xs (Int64.add max_s ys) > 0 then
          max_s
        else if Int64.compare ys 0L > 0 && Int64.compare xs (Int64.add min_s ys) < 0
        then min_s
        else Int64.sub xs ys

let mulhi_apply kind x y =
  let bits = int_bits kind in
  if bits < 64 then
    let signed = not (is_unsigned (Hir.Int kind)) in
    let xs = if signed then sign_extend_value (Hir.Int kind) x else x in
    let ys = if signed then sign_extend_value (Hir.Int kind) y else y in
    let product = Int64.mul xs ys in
    if signed then Int64.shift_right product bits
    else Int64.shift_right_logical product bits
  else if is_unsigned (Hir.Int kind) then
    let al = Int64.logand x 0xFFFFFFFFL in
    let ah = Int64.shift_right_logical x 32 in
    let bl = Int64.logand y 0xFFFFFFFFL in
    let bh = Int64.shift_right_logical y 32 in
    let ll = Int64.mul al bl in
    let lh = Int64.mul al bh in
    let hl = Int64.mul ah bl in
    let hh = Int64.mul ah bh in
    let mid =
      Int64.add
        (Int64.add (Int64.shift_right_logical ll 32) (Int64.logand lh 0xFFFFFFFFL))
        (Int64.logand hl 0xFFFFFFFFL)
    in
    Int64.add hh
      (Int64.add
         (Int64.shift_right_logical lh 32)
         (Int64.add
            (Int64.shift_right_logical hl 32)
            (Int64.shift_right_logical mid 32)))
  else
    let x0 = Int64.logand x 0xFFFFFFFFL in
    let x1 = Int64.shift_right x 32 in
    let y0 = Int64.logand y 0xFFFFFFFFL in
    let y1 = Int64.shift_right y 32 in
    let z0 = Int64.mul x0 y0 in
    let t = Int64.add (Int64.mul x1 y0) (Int64.shift_right_logical z0 32) in
    let z1 = Int64.add (Int64.logand t 0xFFFFFFFFL) (Int64.mul x0 y1) in
    Int64.add (Int64.mul x1 y1)
      (Int64.add (Int64.shift_right t 32) (Int64.shift_right z1 32))

let sat_or_mulhi name kind x y =
  if name = "mul_hi" then mulhi_apply kind x y else sat_apply name kind x y

let float_value ty bits =
  match ty with
  | Hir.Float Hir.F32 -> Int32.float_of_bits (Int64.to_int32 bits)
  | Hir.Float Hir.F64 -> Int64.float_of_bits bits
  | _ -> invalid_arg "internal error: non-float vector lane"

let float_comparison_bits ty operation left right =
  let left = float_value ty left and right = float_value ty right in
  let result =
    match operation with
    | Ast.Eq -> left = right
    | Ast.Ne -> left <> right
    | Ast.Lt -> left < right
    | Ast.Le -> left <= right
    | Ast.Gt -> left > right
    | Ast.Ge -> left >= right
    | _ -> invalid_arg "internal error: non-comparison float operation"
  in
  if result then 1L else 0L

let float_binary_bits ty operation left right =
  match ty with
  | Hir.Float Hir.F32 ->
      let opcode =
        match operation with Ast.Add -> 0 | Ast.Sub -> 1 | Ast.Mul -> 2 | _ -> 3
      in
      Float_parse.f32_binary_bits opcode left right
  | Hir.Float Hir.F64 ->
      let left = Int64.float_of_bits left and right = Int64.float_of_bits right in
      Int64.bits_of_float
        (match operation with
        | Ast.Add -> left +. right
        | Ast.Sub -> left -. right
        | Ast.Mul -> left *. right
        | _ -> left /. right)
  | _ -> invalid_arg "internal error: non-float vector lane"

let active_layout_type_bindings = ref []
and active_array_length_queries = ref []

type layout_query_state = {
  definitions : Hir.struct_def list ref;
  instances : (string, string) Hashtbl.t;
  next_instance : int ref;
  active : (string * string) list ref;
}

let active_layout_query_state = ref None
let layout_sources : (string, string) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 16

let layout_source name =
  Option.value ~default:name (Stdlib.Hashtbl.find_opt layout_sources name)

let rec query_layout ~structs ~named_types ~generic_structs ~globals ~evaluate consts
    span ty =
  let owns_state, state =
    match !active_layout_query_state with
    | Some state -> (false, state)
    | None ->
        ( true,
          {
            definitions = ref structs;
            instances = Hashtbl.create 16;
            next_instance = ref 0;
            active = ref [];
          } )
  in
  if owns_state then (
    active_layout_query_state := Some state;
    Stdlib.Hashtbl.reset layout_sources);
  Fun.protect
    ~finally:(fun () -> if owns_state then active_layout_query_state := None)
    (fun () ->
      query_layout_in_state state ~named_types ~generic_structs ~globals ~evaluate
        consts span ty)

and query_layout_in_state state ~named_types ~generic_structs ~globals ~evaluate consts
    span ty =
  let source_int = function
    | Ast.U8 -> Hir.U8
    | U16 -> U16
    | U32 -> U32
    | U64 -> U64
    | I8 -> I8
    | I16 -> I16
    | I32 -> I32
    | I64 -> I64
    | Usize -> Usize
    | Isize -> Isize
  in
  let templates =
    List.filter_map
      (function
        | Ast.Struct { name; generic_params; fields; align; _ } ->
            Some (name, (generic_params, fields, align))
        | _ -> None)
      generic_structs
  in
  let definitions = state.definitions in
  let instances = state.instances in
  let next_instance = state.next_instance in
  let active = state.active in
  let add_error message = Error [ Diag.error span message ] in
  let cyclic_dependency name =
    add_error (Printf.sprintf "cyclic constant dependency involving `%s`" name)
  in
  let rec resolve_type type_bindings const_bindings at = function
    | Ast.Bool -> Ok Hir.Bool
    | Ast.Void -> Ok Hir.Void
    | Ast.Int kind -> Ok (Hir.Int (source_int kind))
    | Ast.Float Ast.F32 -> Ok (Hir.Float Hir.F32)
    | Ast.Float Ast.F64 -> Ok (Hir.Float Hir.F64)
    | Ast.Addr -> Ok Hir.Addr
    | Ast.Handle ty -> (
        let* ty = resolve_type type_bindings const_bindings at ty in
        match ty with
        | Hir.Opaque name -> Ok (Hir.Handle name)
        | _ -> add_error "handle type argument must be an opaque type")
    | Ast.Array (length, ty) ->
        let* length = resolve_length "array" type_bindings const_bindings length in
        let* ty = resolve_type type_bindings const_bindings at ty in
        Ok (Hir.Array (length, ty))
    | Ast.Vec (length, ty) -> (
        let* length = resolve_length "vector" type_bindings const_bindings length in
        let* ty = resolve_type type_bindings const_bindings at ty in
        match vec_cap_error length ty with
        | Some message -> add_error message
        | None -> Ok (Hir.Vec (length, ty)))
    | Ast.Named_type (name, type_span) -> (
        match List.assoc_opt name type_bindings with
        | Some ty -> Ok ty
        | None -> (
            match List.assoc_opt name named_types with
            | Some Struct_name ->
                let* structure = instantiate type_bindings const_bindings name at [] in
                Ok (Hir.Struct structure)
            | Some Generic_struct_name ->
                add_error
                  (Printf.sprintf "generic struct `%s` requires type arguments" name)
            | _ ->
                source_ty_with_values ~globals named_types (const_bindings @ consts) at
                  (Ast.Named_type (name, type_span))))
    | Ast.Applied_type (name, _, arguments, application_span) ->
        let* structure =
          instantiate type_bindings const_bindings name application_span arguments
        in
        Ok (Hir.Struct structure)
  and resolve_length kind type_bindings const_bindings length =
    let previous = !active_layout_type_bindings in
    active_layout_type_bindings := type_bindings;
    let result =
      evaluate (const_bindings @ consts) length.expression
        (aggregate_length_expected length.expression)
    in
    active_layout_type_bindings := previous;
    let result = remap_length_cycle length.span result in
    length_value kind length.span result
  and length_value kind at = function
    | Error diagnostics -> Error diagnostics
    | Ok (ty, value) ->
        if not (is_int ty) then
          Error [ Diag.error at (Printf.sprintf "%s length must be an integer" kind) ]
        else if is_unsigned ty then
          if Int64.unsigned_compare value (Int64.of_int max_int) > 0 then
            Error
              [
                Diag.error at
                  (Printf.sprintf "%s length `%s` is too large" kind
                     (unsigned_int64_to_string value));
              ]
          else Ok (Int64.to_int value)
        else
          let value = sign_extend_value ty value in
          if value < 0L then
            Error
              [
                Diag.error at
                  (Printf.sprintf "%s length cannot be negative: `%Ld`" kind value);
              ]
          else if value > Int64.of_int max_int then
            Error
              [
                Diag.error at (Printf.sprintf "%s length `%Ld` is too large" kind value);
              ]
          else Ok (Int64.to_int value)
  and instantiate outer_type_bindings outer_const_bindings name at arguments =
    match List.assoc_opt name templates with
    | None -> (
        match List.assoc_opt name named_types with
        | Some Struct_name ->
            add_error (Printf.sprintf "struct `%s` is not generic" name)
        | _ -> add_error (Printf.sprintf "unknown generic struct `%s`" name))
    | Some (params, fields, align) -> (
        if List.length arguments <> List.length params then
          Error
            [
              Diag.error at
                (generic_arity_message "generic struct" name (List.length params)
                   (List.length arguments));
            ]
        else if params = [] && arguments <> [] then
          Error [ Diag.error at (Printf.sprintf "struct `%s` is not generic" name) ]
        else
          let rec bind type_bindings const_bindings params arguments =
            match (params, arguments) with
            | [], [] -> Ok (List.rev type_bindings, List.rev const_bindings)
            | Ast.Type_param { name = parameter; _ } :: params, argument :: arguments ->
                let* ty =
                  match argument with
                  | Ast.Type_arg ty | Ast.Type_or_index ty ->
                      resolve_type outer_type_bindings outer_const_bindings at ty
                  | Ast.Name_arg (type_name, type_span) ->
                      resolve_type outer_type_bindings outer_const_bindings at
                        (Ast.Named_type (type_name, type_span))
                  | Ast.Const_arg expression ->
                      Error
                        [
                          Diag.error (Ast.expr_span expression)
                            "expected a type argument";
                        ]
                in
                bind ((parameter, ty) :: type_bindings) const_bindings params arguments
            | Ast.Const_param parameter :: params, argument :: arguments ->
                let* const_ty =
                  resolve_type
                    (type_bindings @ outer_type_bindings)
                    (const_bindings @ outer_const_bindings)
                    parameter.span parameter.ty
                in
                let* expression =
                  match argument with
                  | Ast.Const_arg expression -> Ok expression
                  | Ast.Name_arg (constant, at) -> Ok (Ast.Ident (constant, at))
                  | Ast.Type_or_index ty -> Ok (Ast.index_expression ty)
                  | Ast.Type_arg _ ->
                      Error [ Diag.error at "expected a const argument" ]
                in
                let previous = !active_layout_type_bindings in
                active_layout_type_bindings := type_bindings @ outer_type_bindings;
                let evaluated =
                  evaluate (outer_const_bindings @ consts) expression (Some const_ty)
                in
                active_layout_type_bindings := previous;
                let* actual_ty, value = evaluated in
                if not (Hir.ty_equal actual_ty const_ty) then
                  Error
                    [
                      Diag.error (Ast.expr_span expression)
                        "const argument type mismatch";
                    ]
                else
                  bind type_bindings
                    ((parameter.name, const_ty, value) :: const_bindings)
                    params arguments
            | _ -> add_error "generic argument arity mismatch"
          in
          let* type_bindings, const_bindings = bind [] [] params arguments in
          let key =
            name ^ "|"
            ^ String.concat "|"
                (List.map
                   (function
                     | Ast.Type_param { name; _ } -> (
                         name ^ "="
                         ^
                         match List.assoc_opt name type_bindings with
                         | Some ty -> Hir.ty_name ty
                         | None -> "?")
                     | Ast.Const_param { name; _ } -> (
                         name ^ "="
                         ^
                         match lookup name const_bindings with
                         | Some (_, _, value) -> Int64.to_string value
                         | None -> "?"))
                   params)
          in
          match Hashtbl.find_opt instances key with
          | Some instance
            when List.exists
                   (fun (active_instance, _) -> active_instance = instance)
                   !active ->
              cyclic_dependency name
          | Some instance -> Ok instance
          | None when List.exists (fun (_, active_name) -> active_name = name) !active
            ->
              cyclic_dependency name
          | None ->
              let instance = Printf.sprintf "__const_layout_%d" !next_instance in
              incr next_instance;
              Hashtbl.add instances key instance;
              Stdlib.Hashtbl.replace layout_sources instance name;
              let previous_active = !active in
              active := (instance, name) :: previous_active;
              let fields =
                Result_list.map
                  (fun (field : Ast.field) ->
                    let* ty =
                      resolve_type type_bindings const_bindings field.span field.ty
                    in
                    if contains_active_type !active ty then
                      let _, recursive_name = List.hd !active in
                      Error
                        [
                          Diag.error field.span
                            (Printf.sprintf "recursive by-value struct `%s`"
                               recursive_name);
                        ]
                    else Ok (field.name, ty, field.unsupported_reason))
                  fields
              in
              let result =
                let* fields = fields in
                let hir_fields =
                  List.map
                    (fun (name, ty, unsupported_reason) ->
                      { Hir.name; ty; offset = 0; unsupported_reason })
                    fields
                in
                let* definition = layout_definition instance align hir_fields in
                definitions := definition :: !definitions;
                Ok instance
              in
              active := previous_active;
              result)
  and contains_active_type active = function
    | Hir.Struct name -> List.exists (fun (active_name, _) -> active_name = name) active
    | Hir.Array (_, element) -> contains_active_type active element
    | _ -> false
  and layout_definition name explicit fields =
    let rec place offset natural fields acc =
      match fields with
      | [] ->
          let align = max natural (Option.value ~default:1 explicit) in
          let* size =
            Target_layout.round_up_size offset align
            |> Result.map_error (fun message -> [ Diag.error span message ])
          in
          Ok
            {
              Hir.name;
              fields = List.rev acc;
              size;
              align;
              is_union = false;
              byte_storage = false;
            }
      | (field : Hir.field) :: rest ->
          let* size, align =
            Hir.layout !definitions field.ty
            |> Result.map_error (fun message -> [ Diag.error span message ])
          in
          let* field_offset =
            Target_layout.round_up_size offset align
            |> Result.map_error (fun message -> [ Diag.error span message ])
          in
          let* field_end =
            Hir.add_size field_offset size
            |> Result.map_error (fun message -> [ Diag.error span message ])
          in
          place field_end (max natural align) rest
            ({ field with offset = field_offset } :: acc)
    in
    place 0 1 fields []
  in
  let* ty = resolve_type !active_layout_type_bindings [] span ty in
  Ok (ty, !definitions)

let index : Sema_index.t option ref = ref None

let rec const_expr ?(structs = []) ?(named_types = []) ?(generic_structs = [])
    ?(arrays = []) ?(array_lengths = []) ?(globals = []) ?resolve consts expected
    ?(check_only = false) ?(validate_dead = true) ?(allow_widen = true) expression =
  let result =
    const_expr_widened ~structs ~named_types ~generic_structs ~arrays ~array_lengths
      ~globals ?resolve consts expected ~check_only ~validate_dead ~allow_widen
      expression
  in
  (match (!index, result) with
  | Some index, Ok (ty, value) ->
      Sema_index.record_const index (Ast.expr_span expression) ty value
  | _ -> ());
  result

and const_leaf_type ?(structs = []) ?(named_types = []) ?(generic_structs = [])
    ?(arrays = []) ?(array_lengths = []) ?(globals = []) ?resolve consts expression =
  match
    const_expr_inner ~structs ~named_types ~generic_structs ~arrays ~array_lengths
      ~globals ?resolve consts None ~check_only:true expression
  with
  | Ok (ty, _) -> Some ty
  | Error _ -> (
      match
        vector_const_expr_inner ~structs ~named_types ~generic_structs ~arrays
          ~array_lengths ~globals ?resolve consts None ~check_only:true expression
      with
      | Ok (ty, _) -> Some ty
      | Error _ -> None)

and const_expr_widened ~structs ~named_types ~generic_structs ~arrays ~array_lengths
    ~globals ?resolve consts expected ~check_only ~validate_dead ~allow_widen expression
    =
  let leaf_type =
    const_leaf_type ~structs ~named_types ~generic_structs ~arrays ~array_lengths
      ~globals ?resolve consts
  in
  let result =
    const_expr_inner ~structs ~named_types ~generic_structs ~arrays ~array_lengths
      ~globals ?resolve consts expected ~check_only ~validate_dead expression
  in
  let result =
    match (allow_widen, expected, result) with
    | true, Some target, Ok (actual, value) -> (
        match Sema_types.convert_expected_kind ~expression actual target with
        | Some kind ->
            Ok (target, Sema_types.widen_integer_value kind actual target value)
        | None when scalar_conversion_pair actual target -> (
            match
              ensure_expected ~leaf_type ~expression actual target
                (Ast.expr_span expression)
            with
            | Error diagnostics -> Error diagnostics
            | Ok () -> Ok (actual, value))
        | None -> Ok (actual, value))
    | _ -> result
  in
  match (expression, result) with
  | Ast.Binary (op, left, right, span), Error _ -> (
      match Sema_types.comparison_chain_diagnostic span op left right with
      | Some diagnostic -> Error [ diagnostic ]
      | None -> result)
  | _ -> result

and const_expr_inner ?(structs = []) ?(named_types = []) ?(generic_structs = [])
    ?(arrays = []) ?(array_lengths = []) ?(globals = []) ?resolve consts expected
    ?(check_only = false) ?(validate_dead = true) expression =
  let leaf_type =
    const_leaf_type ~structs ~named_types ~generic_structs ~arrays ~array_lengths
      ~globals ?resolve consts
  in
  match expression with
  | (Ast.C_dereference _ | Ast.C_dot_star _) as expression ->
      Error
        [ Sema_types.c_pointer_selection_diagnostic ~constant:true expected expression ]
  | Ast.Parenthesized (value, _) ->
      const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths ~globals
        ?resolve consts expected ~check_only ~validate_dead value
  | Ast.Call (Ast.Generic_args (Ast.Ident ("call_addr", _), _, span), _, _) ->
      error span "call_addr cannot be used in constant evaluation"
  | Ast.Int_lit (raw, s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error s m ]) in
      let ty = Option.value ~default:(Hir.Int Hir.I32) expected in
      if fits_literal ty v then Ok (ty, mask_value ty v)
      else
        Error
          [
            Sema_types.integer_literal_error s ty ~negative:false ~text:raw ~magnitude:v;
          ]
  | Ast.Float_lit (raw, s) -> (
      let ty = Option.value ~default:(Hir.Float Hir.F64) expected in
      match ty with
      | Hir.Float _ ->
          let* bits =
            parse_float_literal ty raw
            |> Result.map_error (fun message -> [ Diag.error s message ])
          in
          Ok (ty, bits)
      | _ ->
          error s
            (Printf.sprintf "floating point literal cannot be used as `%s`" (ty_name ty))
      )
  | Ast.Null s -> (
      match expected with
      | Some (Hir.Addr | Hir.Handle _) as ty -> Ok (Option.get ty, 0L)
      | _ -> error s "null requires an addr or handle context")
  | Ast.Bool_lit (v, _) -> Ok (Hir.Bool, if v then 1L else 0L)
  | Ast.Ident (n, s) -> (
      match lookup n consts with
      | Some (_, t, v) -> Ok (t, v)
      | None when List.mem n globals ->
          error s (Printf.sprintf "global `%s` is not a constant" n)
      | None when Option.is_some (Names.value_misuse_message n) ->
          error s (Option.get (Names.value_misuse_message n))
      | None -> (
          match resolve with
          | Some resolve -> resolve ~check_only n s
          | None -> error s (Printf.sprintf "unknown name `%s`" n)))
  | Ast.Sizeof_value (value, span) ->
      let operand_ty =
        match value with
        | Ast.Ident (name, _) -> (
            match lookup name consts with
            | Some (_, ty, _) -> Some ty
            | None -> Option.map (fun (_, ty, _) -> ty) (lookup name arrays))
        | Ast.Int_lit _ -> Some (Hir.Int Hir.I32)
        | Ast.Float_lit _ -> Some (Hir.Float Hir.F64)
        | Ast.Bool_lit _ -> Some Hir.Bool
        | _ -> None
      in
      let help =
        Option.map
          (fun ty ->
            Printf.sprintf "write `sizeof[%s]`" (Sema_types.diagnostic_ty_name ty))
          operand_ty
      in
      Error [ Diag.error ?help span "`sizeof` needs a type in brackets" ]
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, is), s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error is m ]) in
      let t = Option.value ~default:(Hir.Int Hir.I32) expected in
      let allowed =
        match t with
        | Hir.Int ((Hir.I8 | I16 | I32 | I64 | Isize) as k) ->
            let b = int_bits k in
            (b = 64 && v = Int64.min_int) || (b < 64 && v = Int64.shift_left 1L (b - 1))
        | _ -> false
      in
      if fits_negative_literal t v || allowed then Ok (t, mask_value t (Int64.neg v))
      else
        Error
          [
            Sema_types.integer_literal_error s t ~negative:true ~text:("-" ^ raw)
              ~magnitude:v;
          ]
  | Ast.Unary (Ast.Neg, e, s) ->
      let* t, v =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts
          (if is_if_operand e then None else expected)
          ~check_only ~validate_dead ~allow_widen:false e
      in
      if is_float t then
        Ok
          ( t,
            Int64.logxor v
              (if t = Hir.Float Hir.F32 then 0x80000000L else Int64.min_int) )
      else if not (is_int t) then error s "unary minus requires an integer"
      else Ok (t, mask_value t (Int64.neg v))
  | Ast.Unary (Ast.Bit_not, e, s) ->
      let* t, v =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts
          (if is_if_operand e then None else expected)
          ~check_only ~validate_dead ~allow_widen:false e
      in
      if not (is_int t) then error s "bitwise not requires an integer"
      else Ok (t, mask_value t (Int64.lognot v))
  | Ast.Unary (Ast.Not, e, _s) ->
      let* t, v =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts None ~check_only ~validate_dead e
      in
      if t <> Hir.Bool then Error [ Sema_types.logical_not_error e t ]
      else Ok (Hir.Bool, if v = 0L then 1L else 0L)
  | Ast.Binary (((Ast.And | Ast.Or) as op), l, r, _s) ->
      let* lt, lv =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts None ~check_only ~validate_dead l
      in
      if lt <> Hir.Bool then
        Error
          [
            Sema_types.logical_operand_error
              (if op = Ast.And then "&&" else "||")
              "left" l lt;
          ]
      else if
        (not check_only) && ((op = Ast.And && lv = 0L) || (op = Ast.Or && lv <> 0L))
      then
        let* _ =
          const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
            ~globals ?resolve consts None ~check_only:true ~validate_dead r
        in
        Ok (Hir.Bool, if op = Ast.And then 0L else 1L)
      else
        let* rt, rv =
          const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
            ~globals ?resolve consts None ~check_only ~validate_dead r
        in
        if rt <> Hir.Bool then
          Error
            [
              Sema_types.logical_operand_error
                (if op = Ast.And then "&&" else "||")
                "right" r rt;
            ]
        else Ok (Hir.Bool, if rv <> 0L then 1L else 0L)
  | Ast.Binary (((Ast.Shl | Ast.Shr) as op), l, r, _s) ->
      let* lt, lv =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts
          (if is_if_operand l then None
           else match expected with Some (Hir.Int _) -> expected | _ -> None)
          ~check_only ~validate_dead ~allow_widen:false l
      in
      let* rt, rv =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts None ~check_only ~validate_dead r
      in
      let* () =
        match lt with
        | Hir.Int _ -> Ok ()
        | _ -> Error [ Sema_types.shift_value_error op (Ast.expr_span l) lt ]
      in
      let* () =
        match rt with
        | Hir.Int _ -> Ok ()
        | _ -> Error [ Sema_types.shift_count_error op (Ast.expr_span r) rt ]
      in
      let bits = match lt with Hir.Int k -> int_bits k | _ -> 64 in
      let k = Int64.to_int (Int64.logand rv (Int64.of_int (bits - 1))) in
      let v =
        if k = 0 then lv
        else
          match op with
          | Ast.Shl -> Int64.shift_left lv k
          | _ ->
              if is_unsigned lt then Int64.shift_right_logical lv k
              else Int64.shift_right (sign_extend_bits lt lv) k
      in
      Ok (lt, mask_value lt v)
  | Ast.Binary (op, l, r, s) ->
      let comparison_chain =
        match l with
        | Ast.Binary ((Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge), _, _, _) ->
            true
        | _ -> false
      in
      let parenthesized_comparison =
        match l with
        | Ast.Parenthesized
            ( Ast.Binary ((Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge), _, _, _),
              _ ) ->
            true
        | _ -> false
      in
      let* (lt, lv), (rt, rv) =
        let hint = operand_type_hint op expected l r in
        match (unresolved_shape_of l, unresolved_shape_of r) with
        | Some _, None ->
            let* rt, rv =
              const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
                ~globals ?resolve consts hint ~check_only ~validate_dead
                ~allow_widen:false r
            in
            let* lt, lv =
              const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
                ~globals ?resolve consts
                (contextual_peer_type op rt l)
                ~check_only ~validate_dead ~allow_widen:false l
            in
            Ok ((lt, lv), (rt, rv))
        | _ ->
            let* lt, lv =
              const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
                ~globals ?resolve consts hint ~check_only ~validate_dead
                ~allow_widen:false l
            in
            let* rt, rv =
              const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
                ~globals ?resolve consts
                (if comparison_chain || parenthesized_comparison then None
                 else contextual_peer_type op lt r)
                ~check_only ~validate_dead ~allow_widen:false r
            in
            Ok ((lt, lv), (rt, rv))
      in
      let original_lt = lt and original_rt = rt in
      let lt, lv, rt, rv =
        match
          if is_int lt && is_int rt then Sema_types.common_integer_type lt rt else None
        with
        | Some common ->
            let widen ty value =
              match Sema_types.implicit_integer_widen ty common with
              | Some kind -> Sema_types.widen_integer_value kind ty common value
              | None -> value
            in
            (common, widen lt lv, common, widen rt rv)
        | None -> (lt, lv, rt, rv)
      in
      let comparison_chain_rewrite_valid =
        match (op, l) with
        | ( (Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge),
            Ast.Binary (((Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge) as inner), first, middle, _)
          ) ->
            let check_pair _operation left right =
              match (unresolved_shape_of left, unresolved_shape_of right) with
              | Some _, None ->
                  let* right_ty, _ =
                    const_expr ~structs ~named_types ~generic_structs ~arrays
                      ~array_lengths ~globals ?resolve consts None ~check_only:true
                      right
                  in
                  let* left_ty, _ =
                    const_expr ~structs ~named_types ~generic_structs ~arrays
                      ~array_lengths ~globals ?resolve consts (Some right_ty)
                      ~check_only:true left
                  in
                  Ok (left_ty, right_ty)
              | _ ->
                  let* left_ty, _ =
                    const_expr ~structs ~named_types ~generic_structs ~arrays
                      ~array_lengths ~globals ?resolve consts None ~check_only:true left
                  in
                  let* right_ty, _ =
                    const_expr ~structs ~named_types ~generic_structs ~arrays
                      ~array_lengths ~globals ?resolve consts (Some left_ty)
                      ~check_only:true right
                  in
                  Ok (left_ty, right_ty)
            in
            let rewrite_result =
              let* first_ty, middle_ty = check_pair inner first middle in
              let* _ =
                binary_result_type ~leaf_type ~left_expression:first
                  ~right_expression:middle s inner first_ty middle_ty
              in
              let* last_ty, _ =
                const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
                  ~globals ?resolve consts (Some middle_ty) ~check_only:true r
              in
              let* _ =
                binary_result_type ~leaf_type ~left_expression:middle
                  ~right_expression:r s op middle_ty last_ty
              in
              Ok ()
            in
            Result.is_ok rewrite_result
        | _ -> true
      in
      let* result_ty =
        binary_result_type ~leaf_type ~left_expression:l ~right_expression:r
          ?result_expected:expected ~comparison_chain_rewrite_valid s op original_lt
          original_rt
      in
      if is_float lt && is_float rt then
        let common = Option.get (common_numeric_type lt rt) in
        let widen ty value =
          match Sema_types.implicit_numeric_widen ty common with
          | Some Ast.Float_widen -> widen_integer_value Ast.Float_widen ty common value
          | _ -> value
        in
        let lv = widen lt lv and rv = widen rt rv in
        let result =
          match op with
          | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge ->
              float_comparison_bits common op lv rv
          | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> float_binary_bits common op lv rv
          | _ -> assert false
        in
        Ok (result_ty, mask_value result_ty result)
      else if (not check_only) && (op = Ast.Div || op = Ast.Rem) && rv = 0L then
        error s "division by zero in constant expression"
      else
        let signed_lv = sign_extend_value lt lv in
        let signed_rv = sign_extend_value rt rv in
        let signed_min =
          match lt with
          | Hir.Int k ->
              let bits = int_bits k in
              if bits = 64 then Int64.min_int
              else Int64.neg (Int64.shift_left 1L (bits - 1))
          | _ -> 0L
        in
        if
          (not check_only) && op = Ast.Div
          && (not (is_unsigned lt))
          && signed_lv = signed_min && signed_rv = Int64.minus_one
        then error s (Sema_types.signed_division_overflow_message lt lv rv)
        else
          let cmp =
            if is_unsigned lt || lt = Hir.Addr then Int64.unsigned_compare lv rv
            else Int64.compare signed_lv signed_rv
          in
          let result =
            match op with
            | Ast.Add -> Int64.add lv rv
            | Sub -> Int64.sub lv rv
            | Mul -> Int64.mul lv rv
            | Bit_and -> Int64.logand lv rv
            | Bit_or -> Int64.logor lv rv
            | Bit_xor -> Int64.logxor lv rv
            | Div ->
                if check_only && rv = 0L then 0L
                else if
                  check_only
                  && (not (is_unsigned lt))
                  && signed_lv = signed_min && signed_rv = Int64.minus_one
                then 0L
                else if is_unsigned lt then Int64.unsigned_div lv rv
                else Int64.div signed_lv signed_rv
            | Rem ->
                if check_only && rv = 0L then 0L
                else if is_unsigned lt then Int64.unsigned_rem lv rv
                else Int64.rem signed_lv signed_rv
            | Eq -> if lv = rv then 1L else 0L
            | Ne -> if lv <> rv then 1L else 0L
            | Lt -> if cmp < 0 then 1L else 0L
            | Le -> if cmp <= 0 then 1L else 0L
            | Gt -> if cmp > 0 then 1L else 0L
            | Ge -> if cmp >= 0 then 1L else 0L
            | Ast.Shl | Ast.Shr ->
                let bits = match lt with Hir.Int q -> int_bits q | _ -> 64 in
                let k = Int64.to_int (Int64.logand rv (Int64.of_int (bits - 1))) in
                if k = 0 then lv
                else if op = Ast.Shl then Int64.shift_left lv k
                else if is_unsigned lt then Int64.shift_right_logical lv k
                else Int64.shift_right (sign_extend_bits lt lv) k
            | And -> if lv <> 0L && rv <> 0L then 1L else 0L
            | Or -> if lv <> 0L || rv <> 0L then 1L else 0L
          in
          Ok (result_ty, mask_value result_ty result)
  | Ast.Ternary (c, a, b, _s) ->
      let* ct, cv =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts None ~check_only ~validate_dead c
      in
      if ct <> Hir.Bool then Error [ Sema_types.condition_error "if" c ct ]
      else if Option.is_none expected && validate_dead then
        let branch expression branch_expected selected =
          const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
            ~globals ?resolve consts branch_expected
            ~check_only:(check_only || not selected) ~validate_dead expression
        in
        let left_shape = unresolved_shape_of a
        and right_shape = unresolved_shape_of b in
        let branch_hint shape peer_type =
          match (shape, peer_type) with
          | Some Unresolved_int, (Hir.Int _ as ty)
          | Some Unresolved_float, (Hir.Float _ as ty)
          | Some Unresolved_vector, (Hir.Vec _ as ty) ->
              Some ty
          | _ -> None
        in
        let* (left_type, left_value), (right_type, right_value) =
          if
            right_shape = None
            && List.mem left_shape
                 [ Some Unresolved_int; Some Unresolved_float; Some Unresolved_vector ]
          then
            let* right_type, right_value = branch b None (cv = 0L) in
            let left_expected = branch_hint left_shape right_type in
            let* left_type, left_value = branch a left_expected (cv <> 0L) in
            Ok ((left_type, left_value), (right_type, right_value))
          else
            let* left_type, left_value = branch a None (cv <> 0L) in
            let right_expected = branch_hint right_shape left_type in
            let* right_type, right_value = branch b right_expected (cv = 0L) in
            Ok ((left_type, left_value), (right_type, right_value))
        in
        let* result_type, left_widen, right_widen =
          unify_if_branches ~leaf_type a left_type b right_type
          |> Result.map_error (fun diagnostic -> [ diagnostic ])
        in
        let selected_type, selected_value, selected_widen =
          if cv <> 0L then (left_type, left_value, left_widen)
          else (right_type, right_value, right_widen)
        in
        let value =
          Option.fold ~none:selected_value
            ~some:(fun kind ->
              widen_integer_value kind selected_type result_type selected_value)
            selected_widen
        in
        Ok (result_type, value)
      else if cv <> 0L then
        let* at, av =
          const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
            ~globals ?resolve consts expected ~check_only ~validate_dead a
        in
        let* () =
          if not validate_dead then Ok ()
          else
            let* bt, _ =
              const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
                ~globals ?resolve consts (Some at) ~check_only:true ~validate_dead b
            in
            ensure_expected ~leaf_type ~expression:b bt at (Ast.expr_span b)
        in
        Ok (at, av)
      else
        let* bt, bv =
          const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
            ~globals ?resolve consts expected ~check_only ~validate_dead b
        in
        let* () =
          if not validate_dead then Ok ()
          else
            let* at, _ =
              const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
                ~globals ?resolve consts (Some bt) ~check_only:true ~validate_dead a
            in
            ensure_expected ~leaf_type ~expression:a at bt (Ast.expr_span a)
        in
        Ok (bt, bv)
  | Ast.Cast (k, dst, e, s) ->
      let* dt = source_ty_with_values named_types consts s dst in
      let scalar_source () =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts ~check_only ~validate_dead None e
      in
      let* st, v, reshaped =
        match scalar_source () with
        | Ok (st, v) -> Ok (st, v, false)
        | Error scalar_error -> (
            match
              vector_const_expr ~structs ~named_types ~generic_structs ~arrays
                ~array_lengths ~globals ?resolve consts None e
            with
            | Ok (st, values) when k = Ast.Bitcast && cast_legal k st dt -> (
                match constant_bitcast st values dt with
                | Ok [ value ] -> Ok (st, value, true)
                | _ -> Error scalar_error)
            | _ -> Error scalar_error)
      in
      let* () =
        if cast_legal k st dt then Ok ()
        else Error [ Sema_types.cast_error ~expression:e k st dt s ]
      in
      let* () =
        if
          ((st = Hir.Bool || is_int st) && (dt = Hir.Bool || is_int dt))
          || (k = Ast.Bitcast && (is_float st || is_float dt))
          || (k = Ast.Float_widen && is_float st && is_float dt)
          || reshaped
        then Ok ()
        else error s "constant cast requires scalar numeric types"
      in
      let sb = match st with Hir.Bool -> 1 | Hir.Int q -> int_bits q | _ -> 0 in
      let result =
        if reshaped then v
        else
          match k with
          | Ast.Float_widen -> widen_integer_value Ast.Float_widen st dt v
          | Ast.Sext when sb < 64 ->
              let shift = 64 - sb in
              Int64.shift_right (Int64.shift_left v shift) shift
          | Ast.Trunc when dt = Hir.Bool -> Int64.logand v 1L
          | _ -> v
      in
      Ok (dt, mask_value dt result)
  | Ast.Call
      ( Ast.Generic_args (Ast.Ident ("cvt", _), generic_args, target_span),
        [ value ],
        call_span ) ->
      let* target =
        match generic_args with
        | [ Ast.Type_arg ty ] -> source_ty_with_values named_types consts target_span ty
        | [ Ast.Name_arg (name, name_span) ] ->
            source_ty_with_values named_types consts name_span
              (Ast.Named_type (name, name_span))
        | _ -> error target_span "cvt needs one target type: cvt[T](value)"
      in
      let* source, input =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts None ~check_only ~validate_dead value
      in
      let* output =
        match (source, target) with
        | Hir.Int kind, Hir.Float float_kind ->
            Ok
              (Float_parse.int_to_float_bits
                 (sign_extend_value source input)
                 (int_bits kind)
                 (not (is_unsigned source))
                 (float_kind = Hir.F32))
        | Hir.Float Hir.F32, Hir.Float Hir.F64 ->
            Ok (widen_integer_value Ast.Float_widen source target input)
        | Hir.Float Hir.F64, Hir.Float Hir.F32 ->
            Ok (Int64.of_int32 (Int32.bits_of_float (Int64.float_of_bits input)))
        | Hir.Float ((Hir.F32 | Hir.F64) as kind), Hir.Int integer_kind ->
            Float_parse.float_to_int_bits input
              (if kind = Hir.F32 then 32 else 64)
              (int_bits integer_kind)
              (not (is_unsigned target))
            |> mask_value target |> Result.ok
        | _ -> error call_span "invalid constant cvt conversion"
      in
      Ok (target, output)
  | Ast.Call (Ast.Ident ("len", _), [ Ast.String_lit (cstr, v, _) ], s) ->
      if cstr && String.contains v '\000' then
        error s "C string literal cannot contain embedded NUL"
      else Ok (Hir.Int Hir.Usize, Int64.of_int (String.length v))
  | Ast.Call (Ast.Ident ("len", _), [ Ast.Ident (name, _) ], s) -> (
      match lookup name arrays with
      | Some (_, Hir.Array (n, _), _) -> Ok (Hir.Int Hir.Usize, Int64.of_int n)
      | _ -> (
          match List.assoc_opt name array_lengths with
          | None -> error s "len requires a fixed array or string literal"
          | Some (length : Ast.aggregate_length) ->
              if List.mem name !active_array_length_queries then
                error length.span
                  (Printf.sprintf "cyclic constant dependency involving `%s`" name)
              else
                let previous = !active_array_length_queries in
                active_array_length_queries := name :: previous;
                let result =
                  const_expr ~structs ~named_types ~generic_structs ~arrays
                    ~array_lengths ~globals ?resolve consts None ~check_only
                    ~validate_dead length.expression
                in
                active_array_length_queries := previous;
                let result = remap_length_cycle length.span result in
                let* ty, bits = result in
                if is_int ty then Ok (Hir.Int Hir.Usize, bits)
                else error s "len requires a fixed array or string literal"))
  | Ast.Call (Ast.Ident (name, _), [ arg ], _s) when name = "any" || name = "all" -> (
      match
        vector_const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts None arg
      with
      | Ok (Hir.Vec (_, Hir.Bool), values) ->
          let result =
            if name = "any" then List.exists (fun v -> v <> 0L) values
            else List.for_all (fun v -> v <> 0L) values
          in
          Ok (Hir.Bool, if result then 1L else 0L)
      | Ok _ -> error (Ast.expr_span arg) "builtin argument must be a bool vector"
      | Error e -> (
          match
            const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
              ~globals ?resolve consts None ~check_only arg
          with
          | Ok (actual, _) ->
              error (Ast.expr_span arg)
                (Printf.sprintf "`%s` needs a bool vector, got `%s`" name
                   (Sema_types.diagnostic_ty_name actual))
          | Error _ -> Error e))
  | Ast.Call (Ast.Ident (name, _), [ arg ], _s)
    when name = "reduce_sum" || name = "reduce_min" || name = "reduce_max"
         || name = "reduce_and" || name = "reduce_or" || name = "reduce_xor" -> (
      match
        vector_const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts None arg
      with
      | Ok (Hir.Vec (_, Hir.Int kind), (_ :: _ as values)) ->
          let ty = Hir.Int kind in
          let signed =
            match kind with
            | Hir.I8 | Hir.I16 | Hir.I32 | Hir.I64 | Hir.Isize -> true
            | _ -> false
          in
          let result =
            match name with
            | "reduce_sum" ->
                List.fold_left (fun acc v -> mask_value ty (Int64.add acc v)) 0L values
            | "reduce_and" ->
                List.fold_left Int64.logand (mask_value ty (-1L)) values
                |> mask_value ty
            | "reduce_or" -> List.fold_left Int64.logor 0L values |> mask_value ty
            | "reduce_xor" -> List.fold_left Int64.logxor 0L values |> mask_value ty
            | "reduce_min" | "reduce_max" ->
                let better a b =
                  let c =
                    if signed then
                      Int64.compare (sign_extend_value ty a) (sign_extend_value ty b)
                    else Int64.unsigned_compare a b
                  in
                  if name = "reduce_min" then c <= 0 else c >= 0
                in
                List.fold_left
                  (fun acc v -> if better acc v then acc else v)
                  (List.hd values) values
            | _ -> 0L
          in
          Ok (ty, result)
      | Ok ((Hir.Vec (_, Hir.Float _) as actual), _) when name = "reduce_sum" ->
          Error
            [
              Diag.error
                ~help:"the order is not defined; write the additions explicitly"
                (Ast.expr_span arg)
                (Printf.sprintf "`%s` needs an integer vector, got `%s`" name
                   (Sema_types.diagnostic_ty_name actual));
            ]
      | Ok (actual, _) ->
          error (Ast.expr_span arg)
            (Printf.sprintf "`%s` needs an integer vector, got `%s`" name
               (Sema_types.diagnostic_ty_name actual))
      | Error e -> (
          match
            const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
              ~globals ?resolve consts None ~check_only arg
          with
          | Ok (actual, _) ->
              error (Ast.expr_span arg)
                (Printf.sprintf "`%s` needs an integer vector, got `%s`" name
                   (Sema_types.diagnostic_ty_name actual))
          | Error _ -> Error e))
  | Ast.Call (Ast.Ident (name, _), args, s) -> (
      let math_type =
        match Names.float_math name with
        | None -> None
        | Some _ ->
            let peer =
              List.find_map
                (fun argument ->
                  if Option.is_some (unresolved_shape_of argument) then None
                  else
                    match
                      const_expr ~structs ~named_types ~generic_structs ~arrays
                        ~array_lengths ~globals ?resolve consts None ~check_only
                        argument
                    with
                    | Ok ((Hir.Float _ as ty), _) -> Some ty
                    | _ -> None)
                args
            in
            Some
              (Option.value peer
                 ~default:
                   (match expected with
                   | Some (Hir.Float _ as ty) -> ty
                   | _ -> Hir.Float Hir.F64))
      in
      let* vals =
        Result_list.map
          (fun a ->
            let expected =
              match (math_type, unresolved_shape_of a) with
              | Some ty, Some Unresolved_float -> Some ty
              | _ -> (
                  match a with
                  | Ast.Ternary _ | Ast.Parenthesized (Ast.Ternary _, _) -> None
                  | _ -> expected)
            in
            const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
              ~globals ?resolve consts expected ~check_only ~validate_dead
              ~allow_widen:false a)
          args
      in
      match (name, vals) with
      | math_name, values when Option.is_some (Names.float_math math_name) -> (
          let operation = Option.get (Names.float_math math_name) in
          let is_float = function Hir.Float _ -> true | _ -> false in
          match (operation, values) with
          | Names.Fma, [ (ty, a); (ty2, b); (ty3, c) ]
            when is_float ty && ty = ty2 && ty = ty3 ->
              Ok (ty, Float_parse.float_fma_bits (ty = Hir.Float Hir.F32) a b c)
          | ( (Names.Sqrt | Names.Floor | Names.Ceil | Names.Round | Names.Abs),
              [ (ty, input) ] )
            when is_float ty ->
              let opcode =
                match operation with
                | Names.Sqrt -> 0
                | Names.Floor -> 1
                | Names.Ceil -> 2
                | Names.Round -> 3
                | Names.Abs -> 4
                | Names.Fma -> assert false
              in
              Ok (ty, Float_parse.float_unary_bits opcode (ty = Hir.Float Hir.F32) input)
          | _ -> error s (Printf.sprintf "invalid constant builtin call to `%s`" name))
      | ("rotl" | "rotr"), [ (t, x); (count_ty, n) ] ->
          let count_expression = List.nth args 1 in
          let* () =
            if is_int t then Ok ()
            else
              Error
                [ Sema_types.rotate_value_error name (Ast.expr_span (List.hd args)) t ]
          in
          let* () =
            if is_int count_ty then Ok ()
            else
              Error
                [
                  Sema_types.rotate_count_error name
                    (Ast.expr_span count_expression)
                    count_ty;
                ]
          in
          let bits = match t with Hir.Int q -> int_bits q | _ -> 64 in
          let k = Int64.to_int (Int64.logand n (Int64.of_int (bits - 1))) in
          let v =
            if k = 0 then x
            else
              match name with
              | "rotl" ->
                  Int64.logor (Int64.shift_left x k)
                    (Int64.shift_right_logical x (bits - k))
              | _ ->
                  Int64.logor
                    (Int64.shift_right_logical x k)
                    (Int64.shift_left x (bits - k))
          in
          Ok (t, mask_value t v)
      | "popcount", [ (t, x) ] ->
          if is_int t then Ok (t, Int64.of_int (popcount64 x))
          else error s "builtin argument must be an integer or an integer vector"
      | ("ctz" | "clz"), [ (t, 0L) ] ->
          if is_int t then
            let bits = match t with Hir.Int q -> int_bits q | _ -> 64 in
            Ok (t, Int64.of_int bits)
          else error s "builtin argument must be an integer or an integer vector"
      | "ctz", [ (t, x) ] ->
          if is_int t then Ok (t, Int64.of_int (trailing64 x))
          else error s "builtin argument must be an integer or an integer vector"
      | "clz", [ (t, x) ] ->
          if is_int t then
            let b = match t with Hir.Int q -> int_bits q | _ -> 64 in
            Ok (t, Int64.of_int (leading64 x - (64 - b)))
          else error s "builtin argument must be an integer or an integer vector"
      | ("add_sat" | "sub_sat" | "mul_hi"), [ (t, x); (t2, y) ] -> (
          match t with
          | Hir.Int kind when t = t2 -> Ok (t, mask_value t (sat_or_mulhi name kind x y))
          | Hir.Int _ ->
              error
                (Ast.expr_span (List.nth args 1))
                (Printf.sprintf
                   "argument 2 of `%s` is `%s`, expected the type of argument 1 (`%s`)"
                   name
                   (Sema_types.diagnostic_ty_name t2)
                   (Sema_types.diagnostic_ty_name t))
          | _ ->
              error
                (Ast.expr_span (List.hd args))
                (Printf.sprintf "`%s` needs an integer or integer vector, got `%s`" name
                   (Sema_types.diagnostic_ty_name t)))
      | _ ->
          let display_name =
            match String.index_opt name '$' with
            | Some index
              when String.starts_with ~prefix:"$spec$"
                     (String.sub name index (String.length name - index)) ->
                String.sub name 0 index
            | _ -> name
          in
          if Option.is_some (Names.missing_type_arguments_message name) then
            global_error s (Option.get (Names.missing_type_arguments_message name))
          else if Option.is_some (Names.value_operation name) then
            global_error s (Printf.sprintf "invalid constant builtin call to `%s`" name)
          else
            global_error s
              (Printf.sprintf "call to `%s` is not a constant expression" display_name))
  | Ast.Sizeof (source_type, s) ->
      let evaluate values expression expected =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve values expected ~check_only ~validate_dead expression
      in
      let* t, layout_structs =
        query_layout ~structs ~named_types ~generic_structs ~globals ~evaluate consts s
          source_type
      in
      let type_span =
        Option.value ~default:s
          (Sema_types.opaque_source_span named_types [] source_type)
      in
      let* n, _ = layout_diag type_span layout_structs t in
      Ok (Hir.Int Hir.Usize, Int64.of_int n)
  | Ast.Alignof (source_type, s) ->
      let evaluate values expression expected =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve values expected ~check_only ~validate_dead expression
      in
      let* t, layout_structs =
        query_layout ~structs ~named_types ~generic_structs ~globals ~evaluate consts s
          source_type
      in
      let type_span =
        Option.value ~default:s
          (Sema_types.opaque_source_span named_types [] source_type)
      in
      let* _, n = layout_diag type_span layout_structs t in
      Ok (Hir.Int Hir.Usize, Int64.of_int n)
  | Ast.Offsetof (source_ty, n, field_span, s) -> (
      let evaluate values expression expected =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve values expected ~check_only ~validate_dead expression
      in
      let* t, layout_structs =
        query_layout ~structs ~named_types ~generic_structs ~globals ~evaluate consts s
          source_ty
      in
      match t with
      | Hir.Struct sn -> (
          match field_info layout_structs sn n with
          | Some { unsupported_reason = Some reason; _ } -> error s reason
          | Some f ->
              Option.iter
                (fun index ->
                  Sema_index.refer_field index field_span (layout_source sn) n)
                !index;
              Ok (Hir.Int Hir.Usize, Int64.of_int f.offset)
          | None ->
              error s
                (Sema_types.missing_field_message ~record_name:(Ast.type_name source_ty)
                   n (Hir.Struct sn)))
      | _ -> error s "offsetof requires a struct")
  | expr -> global_error (Ast.expr_span expr) "expression is not compile-time constant"

and vector_const_expr ?(structs = []) ?(named_types = []) ?(generic_structs = [])
    ?(arrays = []) ?(array_lengths = []) ?(globals = []) ?resolve consts expected
    ?(check_only = false) expression =
  let result =
    vector_const_expr_inner ~structs ~named_types ~generic_structs ~arrays
      ~array_lengths ~globals ?resolve consts expected ~check_only expression
  in
  match (expression, result) with
  | Ast.Binary (op, left, right, span), Error _ -> (
      match Sema_types.comparison_chain_diagnostic span op left right with
      | Some diagnostic -> Error [ diagnostic ]
      | None -> result)
  | _ -> result

and vector_const_expr_inner ?(structs = []) ?(named_types = []) ?(generic_structs = [])
    ?(arrays = []) ?(array_lengths = []) ?(globals = []) ?resolve consts expected
    ?(check_only = false) expression =
  let leaf_type =
    const_leaf_type ~structs ~named_types ~generic_structs ~arrays ~array_lengths
      ~globals ?resolve consts
  in
  let lane_type = function Hir.Vec (_, element) -> Some element | _ -> None in
  let lane_mask ty value = mask_value ty value in
  let lane_signed ty value = sign_extend_value ty value in
  let evaluate =
    vector_const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
      ~globals ?resolve consts ~check_only
  in
  let evaluate_peer peer expression =
    match (peer, unresolved_shape_of expression) with
    | Hir.Vec _, Some Unresolved_int ->
        let* ty, value =
          const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
            ~globals ?resolve consts None ~check_only expression
        in
        Ok (ty, [ value ])
    | _ -> evaluate (Some peer) expression
  in
  let pair left right =
    match (unresolved_shape_of left, unresolved_shape_of right) with
    | Some _, None ->
        let* rt, rv = evaluate expected right in
        let* lt, lv = evaluate_peer rt left in
        Ok (lt, lv, rt, rv)
    | _ ->
        let* lt, lv = evaluate expected left in
        let* rt, rv = evaluate_peer lt right in
        Ok (lt, lv, rt, rv)
  in
  match expression with
  | (Ast.C_dereference _ | Ast.C_dot_star _) as expression ->
      Error
        [ Sema_types.c_pointer_selection_diagnostic ~constant:true expected expression ]
  | Ast.Parenthesized (value, _) -> evaluate expected value
  | Ast.Ident (name, span) -> (
      match lookup name arrays with
      | Some (_, (Hir.Vec _ as ty), values) -> Ok (ty, values)
      | _ -> error span "constant expression requires a known vector constant")
  | Ast.Array_lit (elements, span) -> (
      let* vector_ty =
        match expected with
        | Some (Hir.Vec _ as ty) -> Ok ty
        | _ -> error span "expression is not a compile-time vector constant"
      in
      match vector_ty with
      | Hir.Vec (lanes, element_ty) ->
          if List.length elements <> lanes then
            error span "wrong number of vector literal lanes"
          else
            let rec values acc = function
              | [] -> Ok (List.rev acc)
              | element :: rest ->
                  let* actual_ty, value =
                    const_expr ~structs ~named_types ~generic_structs ~arrays
                      ~array_lengths ~globals ?resolve consts (Some element_ty)
                      ~check_only element
                  in
                  let* () =
                    ensure_expected ~leaf_type ~expression:element actual_ty element_ty
                      (Ast.expr_span element)
                  in
                  values (lane_mask element_ty value :: acc) rest
            in
            let* values = values [] elements in
            Ok (vector_ty, values)
      | _ -> error span "vector literal requires a vector type")
  | Ast.Splat (value, span) -> (
      match expected with
      | Some (Hir.Vec (lanes, element) as ty) ->
          let* actual, value =
            const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
              ~globals ?resolve consts (Some element) ~check_only value
          in
          let* () =
            ensure_expected ~leaf_type ~expression actual element
              (Ast.expr_span expression)
          in
          Ok (ty, List.init lanes (fun _ -> lane_mask element value))
      | _ ->
          error span
            "`splat` needs a vector destination or vector operand to determine its \
             lane count")
  | Ast.Unary (Ast.Not, value, _span) -> (
      let* ty, values = evaluate expected value in
      match ty with
      | Hir.Vec (_, Hir.Bool) ->
          Ok (ty, List.map (fun value -> if value = 0L then 1L else 0L) values)
      | _ -> Error [ Sema_types.logical_not_error value ty ])
  | Ast.Unary (Ast.Neg, value, _span) -> (
      let* ty, values = evaluate expected value in
      match lane_type ty with
      | Some (Hir.Int _ as element) ->
          Ok (ty, List.map (fun value -> lane_mask element (Int64.neg value)) values)
      | Some (Hir.Float Hir.F32) ->
          Ok (ty, List.map (fun value -> Int64.logxor value 0x8000_0000L) values)
      | Some (Hir.Float Hir.F64) ->
          Ok (ty, List.map (fun value -> Int64.logxor value Int64.min_int) values)
      | _ -> error (Ast.expr_span value) "unary negation requires a numeric vector")
  | Ast.Binary (((Ast.Shl | Ast.Shr) as op), value, count, _span) ->
      let* ty, values = evaluate expected value in
      let* element =
        match lane_type ty with
        | Some (Hir.Int _ as element) -> Ok element
        | _ -> Error [ Sema_types.shift_value_error op (Ast.expr_span value) ty ]
      in
      let* count_ty, counts =
        match (ty, count) with
        | Hir.Vec _, Ast.Splat _ ->
            Error [ Sema_types.shift_count_splat_error op value count ]
        | _ -> (
            match evaluate None count with
            | Ok (count_ty, counts) -> Ok (count_ty, counts)
            | Error _ -> (
                match
                  const_expr ~structs ~named_types ~generic_structs ~arrays
                    ~array_lengths ~globals ?resolve consts None ~check_only count
                with
                | Ok (count_ty, count_value) -> Ok (count_ty, [ count_value ])
                | Error diagnostics -> Error diagnostics))
      in
      let* per_lane =
        match count_ty with
        | Hir.Int _ -> Ok (List.map (fun _ -> List.hd counts) values)
        | Hir.Vec (count_lanes, (Hir.Int _ as count_element)) ->
            if count_lanes = List.length values then Ok counts
            else
              Error
                [
                  Sema_types.shift_count_lanes_error op (Ast.expr_span count)
                    (Hir.Vec (count_lanes, count_element))
                    (Hir.Vec (List.length values, count_element));
                ]
        | _ -> Error [ Sema_types.shift_count_error op (Ast.expr_span count) count_ty ]
      in
      let bits = Option.get (integer_value_bit_width element) in
      let apply amount value =
        let amount = Int64.to_int (Int64.logand amount (Int64.of_int (bits - 1))) in
        if amount = 0 then value
        else
          match op with
          | Ast.Shl -> Int64.shift_left value amount
          | _ ->
              if is_unsigned element then Int64.shift_right_logical value amount
              else Int64.shift_right (lane_signed element value) amount
      in
      Ok (ty, List.map2 (fun c v -> lane_mask element (apply c v)) per_lane values)
  | Ast.Binary (operation, left, right, span) -> (
      let hint = operand_type_hint operation expected left right in
      let* left_ty, left_values, right_ty, right_values =
        match (unresolved_shape_of left, unresolved_shape_of right) with
        | Some _, None ->
            let* right_ty, right_values = evaluate hint right in
            let* left_ty, left_values =
              evaluate
                (match right_ty with
                | Hir.Addr -> Some (Hir.Int Hir.Usize)
                | t -> Some t)
                left
            in
            Ok (left_ty, left_values, right_ty, right_values)
        | _ ->
            let* left_ty, left_values = evaluate hint left in
            let* right_ty, right_values =
              evaluate
                (match left_ty with
                | Hir.Addr -> Some (Hir.Int Hir.Usize)
                | t -> Some t)
                right
            in
            Ok (left_ty, left_values, right_ty, right_values)
      in
      let* result_ty =
        binary_result_type ~leaf_type ~left_expression:left ~right_expression:right span
          operation left_ty right_ty
      in
      let* element =
        match lane_type left_ty with
        | Some element -> Ok element
        | None -> error span "vector operation requires vector operands"
      in
      let float_element = match element with Hir.Float _ -> true | _ -> false in
      let signed_min =
        match element with
        | Hir.Int kind ->
            let bits = int_bits kind in
            if bits = 64 then Int64.min_int
            else Int64.neg (Int64.shift_left 1L (bits - 1))
        | _ -> 0L
      in
      let rec first_offense lefts rights =
        match (lefts, rights) with
        | left :: left_rest, right :: right_rest ->
            if
              (not float_element)
              && (operation = Ast.Div || operation = Ast.Rem)
              && right = 0L
            then Some "division by zero in constant expression"
            else if
              (not float_element) && operation = Ast.Div
              && (not (is_unsigned element))
              && lane_signed element left = signed_min
              && lane_signed element right = Int64.minus_one
            then Some (Sema_types.signed_division_overflow_message element left right)
            else first_offense left_rest right_rest
        | _ -> None
      in
      let offense =
        if check_only then None else first_offense left_values right_values
      in
      match offense with
      | Some message -> error span message
      | None ->
          let result_element =
            match operation with
            | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> Hir.Bool
            | _ -> element
          in
          let apply left right =
            if float_element then
              match operation with
              | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge ->
                  float_comparison_bits element operation left right
              | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div ->
                  float_binary_bits element operation left right
              | _ -> 0L
            else
              let signed_left = lane_signed element left in
              let signed_right = lane_signed element right in
              let compare =
                if is_unsigned element then Int64.unsigned_compare left right
                else Int64.compare signed_left signed_right
              in
              match operation with
              | Ast.Add -> Int64.add left right
              | Ast.Sub -> Int64.sub left right
              | Ast.Mul -> Int64.mul left right
              | Ast.Bit_and -> Int64.logand left right
              | Ast.Bit_or -> Int64.logor left right
              | Ast.Bit_xor -> Int64.logxor left right
              | Ast.Div ->
                  if check_only && right = 0L then 0L
                  else if
                    check_only
                    && (not (is_unsigned element))
                    && signed_left = signed_min
                    && signed_right = Int64.minus_one
                  then 0L
                  else if is_unsigned element then Int64.unsigned_div left right
                  else Int64.div signed_left signed_right
              | Ast.Rem ->
                  if check_only && right = 0L then 0L
                  else if is_unsigned element then Int64.unsigned_rem left right
                  else Int64.rem signed_left signed_right
              | Ast.Eq -> if left = right then 1L else 0L
              | Ast.Ne -> if left <> right then 1L else 0L
              | Ast.Lt -> if compare < 0 then 1L else 0L
              | Ast.Le -> if compare <= 0 then 1L else 0L
              | Ast.Gt -> if compare > 0 then 1L else 0L
              | Ast.Ge -> if compare >= 0 then 1L else 0L
              | Ast.Shl | Ast.Shr ->
                  let bits = Option.get (integer_value_bit_width element) in
                  let amount =
                    Int64.to_int (Int64.logand right (Int64.of_int (bits - 1)))
                  in
                  if amount = 0 then left
                  else if operation = Ast.Shl then Int64.shift_left left amount
                  else if is_unsigned element then Int64.shift_right_logical left amount
                  else Int64.shift_right (lane_signed element left) amount
              | Ast.And | Ast.Or -> 0L
          in
          Ok
            ( result_ty,
              List.map2
                (fun left right -> lane_mask result_element (apply left right))
                left_values right_values ))
  | Ast.Call (Ast.Ident (name, _), args, span)
    when Option.is_some (Names.float_math name) ->
      let operation = Option.get (Names.float_math name) in
      let expected_arity = if operation = Names.Fma then 3 else 1 in
      if List.length args <> expected_arity then
        error span
          (Printf.sprintf "builtin `%s` expects %d argument%s" name expected_arity
             (if expected_arity = 1 then "" else "s"))
      else
        let* ty, first = evaluate expected (List.hd args) in
        let* element =
          match lane_type ty with
          | Some (Hir.Float _ as element) -> Ok element
          | _ ->
              error span (Printf.sprintf "invalid constant builtin call to `%s`" name)
        in
        let is_f32 = element = Hir.Float Hir.F32 in
        let* result =
          match (operation, args) with
          | Names.Fma, [ _; second; third ] ->
              let* second_ty, second_values = evaluate (Some ty) second in
              let* third_ty, third_values = evaluate (Some ty) third in
              if second_ty <> ty || third_ty <> ty then
                error span (Printf.sprintf "invalid constant builtin call to `%s`" name)
              else
                Ok
                  (List.mapi
                     (fun index a ->
                       Float_parse.float_fma_bits is_f32 a
                         (List.nth second_values index)
                         (List.nth third_values index))
                     first)
          | (Names.Sqrt | Names.Floor | Names.Ceil | Names.Round | Names.Abs), [ _ ] ->
              let opcode =
                match operation with
                | Names.Sqrt -> 0
                | Names.Floor -> 1
                | Names.Ceil -> 2
                | Names.Round -> 3
                | Names.Abs -> 4
                | Names.Fma -> assert false
              in
              Ok (List.map (Float_parse.float_unary_bits opcode is_f32) first)
          | _ ->
              error span (Printf.sprintf "invalid constant builtin call to `%s`" name)
        in
        Ok (ty, result)
  | Ast.Call
      ( Ast.Generic_args (Ast.Ident ("cvt", _), generic_args, target_span),
        [ value ],
        call_span ) ->
      let* target =
        match generic_args with
        | [ Ast.Type_arg ty ] -> source_ty_with_values named_types consts target_span ty
        | [ Ast.Name_arg (name, name_span) ] ->
            source_ty_with_values named_types consts name_span
              (Ast.Named_type (name, name_span))
        | _ -> error target_span "cvt needs one target type: cvt[T](value)"
      in
      let* source, values = evaluate None value in
      let* source_lanes, source_element, target_element =
        match (source, target) with
        | Hir.Vec (source_lanes, source_element), Hir.Vec (target_lanes, target_element)
          when source_lanes = target_lanes ->
            Ok (source_lanes, source_element, target_element)
        | _ -> error call_span "constant vector cast requires matching lane counts"
      in
      let integer = function Hir.Int _ -> true | _ -> false in
      let floating = function Hir.Float _ -> true | _ -> false in
      let convert value =
        match (source_element, target_element) with
        | Hir.Int kind, Hir.Float float_kind ->
            Float_parse.int_to_float_bits
              (sign_extend_value source_element value)
              (int_bits kind)
              (not (is_unsigned source_element))
              (float_kind = Hir.F32)
        | Hir.Float Hir.F32, Hir.Float Hir.F64 ->
            widen_integer_value Ast.Float_widen source_element target_element value
        | Hir.Float Hir.F64, Hir.Float Hir.F32 ->
            Int64.of_int32 (Int32.bits_of_float (Int64.float_of_bits value))
        | Hir.Float ((Hir.F32 | Hir.F64) as float_kind), Hir.Int integer_kind ->
            Float_parse.float_to_int_bits value
              (if float_kind = Hir.F32 then 32 else 64)
              (int_bits integer_kind)
              (not (is_unsigned target_element))
            |> mask_value target_element
        | _ -> value
      in
      if
        (integer source_element && floating target_element)
        || (floating source_element && integer target_element)
        || floating source_element && floating target_element
           && source_element <> target_element
      then Ok (Hir.Vec (source_lanes, target_element), List.map convert values)
      else error call_span "invalid constant cvt conversion"
  | Ast.Call (Ast.Ident (name, _), [ value ], span)
    when List.mem name [ "popcount"; "clz"; "ctz" ] ->
      let* ty, values = evaluate expected value in
      let* kind =
        match lane_type ty with
        | Some (Hir.Int k) -> Ok k
        | _ -> error span "builtin argument must be an integer or an integer vector"
      in
      let apply value =
        match name with
        | "popcount" -> Int64.of_int (popcount64 value)
        | "ctz" ->
            if value = 0L then Int64.of_int (int_bits kind)
            else Int64.of_int (trailing64 value)
        | _ ->
            let bits = int_bits kind in
            if value = 0L then Int64.of_int bits
            else Int64.of_int (leading64 value - (64 - bits))
      in
      Ok (ty, List.map (fun value -> lane_mask (Hir.Int kind) (apply value)) values)
  | Ast.Call (Ast.Ident (name, _), [ value; count_expression ], _span)
    when List.mem name [ "rotl"; "rotr" ] ->
      let* ty, values = evaluate expected value in
      let* element =
        match lane_type ty with
        | Some (Hir.Int _ as element) -> Ok element
        | _ -> Error [ Sema_types.rotate_value_error name (Ast.expr_span value) ty ]
      in
      let* count_ty, count =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts None ~check_only count_expression
      in
      if not (is_int count_ty) then
        Error
          [
            Sema_types.rotate_count_error name (Ast.expr_span count_expression) count_ty;
          ]
      else
        let bits = Option.get (integer_value_bit_width element) in
        let amount = Int64.to_int (Int64.logand count (Int64.of_int (bits - 1))) in
        let apply value =
          if amount = 0 then value
          else
            match name with
            | "rotl" ->
                Int64.logor
                  (Int64.shift_left value amount)
                  (Int64.shift_right_logical value (bits - amount))
            | "rotr" ->
                Int64.logor
                  (Int64.shift_right_logical value amount)
                  (Int64.shift_left value (bits - amount))
            | _ -> value
        in
        Ok (ty, List.map (fun value -> lane_mask element (apply value)) values)
  | Ast.Call (Ast.Ident (name, _), [ a; idx ], span) when name = "permute" ->
      let* at, avalues = evaluate expected a in
      let* n, element =
        match at with
        | Hir.Vec (n, ((Hir.Int _ | Hir.Bool | Hir.Float _) as e)) -> Ok (n, e)
        | _ -> error span "permute value must be a vector"
      in
      let* idx_ty, idx_values = evaluate None idx in
      let* () =
        match idx_ty with
        | Hir.Vec (_, Hir.Int (Hir.U8 | Hir.U16 | Hir.U32 | Hir.U64 | Hir.Usize)) ->
            Ok ()
        | _ -> error span "permute indices must be an unsigned integer vector"
      in
      let source = Array.of_list avalues in
      Ok
        ( Hir.Vec (List.length idx_values, element),
          List.map
            (fun v ->
              if Int64.unsigned_compare v (Int64.of_int n) < 0 then
                lane_mask element source.(Int64.to_int v)
              else 0L)
            idx_values )
  | Ast.Call (Ast.Ident (name, _), [ a; b; sel ], _span) when name = "shuffle" ->
      let* at, avalues, bt, bvalues = pair a b in
      let* () =
        if at = bt then Ok ()
        else
          error (Ast.expr_span b)
            (Printf.sprintf
               "argument 2 of `shuffle` is `%s`, expected the type of argument 1 (`%s`)"
               (Sema_types.diagnostic_ty_name bt)
               (Sema_types.diagnostic_ty_name at))
      in
      let* n, element =
        match at with
        | Hir.Vec (n, ((Hir.Int _ | Hir.Bool | Hir.Float _) as e)) -> Ok (n, e)
        | _ ->
            error (Ast.expr_span a)
              (Printf.sprintf
                 "argument 1 of `shuffle` must be an integer or bool vector, got `%s`"
                 (Sema_types.diagnostic_ty_name at))
      in
      let* sel_ty, sel_values =
        match
          evaluate
            (match sel with
            | Ast.Array_lit (entries, _) ->
                Some (Hir.Vec (List.length entries, Hir.Int Hir.I64))
            | _ -> None)
            sel
        with
        | Ok result -> Ok result
        | Error diagnostics -> Error diagnostics
      in
      let* sel_elem =
        match sel_ty with
        | Hir.Vec (_, (Hir.Int _ as e)) -> Ok e
        | _ ->
            error (Ast.expr_span sel)
              (Printf.sprintf
                 "shuffle indices must be a compile-time constant integer vector, got \
                  `%s`"
                 (Sema_types.diagnostic_ty_name sel_ty))
      in
      let* () =
        if shuffle_indices_in_range sel_elem n sel_values then Ok ()
        else
          let signed =
            match sel_elem with
            | Hir.Int (Hir.I8 | Hir.I16 | Hir.I32 | Hir.I64 | Hir.Isize) -> true
            | _ -> false
          in
          let bad =
            List.find_opt
              (fun value ->
                let value =
                  if signed then sign_extend_value sel_elem value else value
                in
                Int64.compare value 0L < 0
                || Int64.compare value (Int64.of_int (2 * n)) >= 0)
              sel_values
          in
          error (Ast.expr_span sel)
            (Printf.sprintf "shuffle index `%s` is out of range for %d lanes"
               (match bad with
               | Some value when signed ->
                   Int64.to_string (sign_extend_value sel_elem value)
               | Some value -> Printf.sprintf "%Lu" value
               | None -> "?")
               (2 * n))
      in
      let source = Array.of_list (avalues @ bvalues) in
      Ok
        ( Hir.Vec (List.length sel_values, element),
          List.map (fun v -> lane_mask element source.(Int64.to_int v)) sel_values )
  | Ast.Call (Ast.Ident (name, _), [ varg; marg ], span)
    when name = "compress" || name = "expand" -> (
      let* vals_ty, values = evaluate expected varg in
      let* masks_ty, masks = evaluate None marg in
      let out () =
        if name = "compress" then
          let chosen =
            List.combine values masks
            |> List.filter_map (fun (v, mk) -> if mk <> 0L then Some v else None)
          in
          chosen @ List.init (List.length values - List.length chosen) (fun _ -> 0L)
        else
          let vals = Array.of_list values in
          let _, rev_out =
            List.fold_left
              (fun (cursor, acc) mk ->
                if mk <> 0L then (cursor + 1, vals.(cursor) :: acc)
                else (cursor, 0L :: acc))
              (0, []) masks
          in
          List.rev rev_out
      in
      match (vals_ty, masks_ty) with
      | ( Hir.Vec (n, ((Hir.Int _ | Hir.Bool | Hir.Float _) as elem)),
          Hir.Vec (mcount, Hir.Bool) )
        when n = mcount ->
          Ok (Hir.Vec (n, elem), out ())
      | Hir.Vec _, Hir.Vec (_, Hir.Bool) ->
          error span
            (Printf.sprintf "%s values and mask must have the same lane count" name)
      | Hir.Vec _, _ -> error span (Printf.sprintf "%s mask must be a bool vector" name)
      | _ -> error span (Printf.sprintf "%s values must be a vector" name))
  | Ast.Call (Ast.Ident (name, _), [ m; y; z ], span) when name = "select" ->
      let* mask_ty, mask_values = evaluate None m in
      let* yes_ty, yes_values, no_ty, no_values = pair y z in
      let* lanes =
        match mask_ty with
        | Hir.Vec (n, Hir.Bool) -> Ok n
        | _ ->
            error (Ast.expr_span m)
              (Printf.sprintf "argument 1 of `select` is `%s`, expected a bool vector"
                 (Sema_types.diagnostic_ty_name mask_ty))
      in
      let* () =
        if yes_ty = no_ty then Ok ()
        else
          error (Ast.expr_span z)
            (Printf.sprintf
               "argument 3 of `select` is `%s`, expected the type of argument 2 (`%s`)"
               (Sema_types.diagnostic_ty_name no_ty)
               (Sema_types.diagnostic_ty_name yes_ty))
      in
      let* element =
        match yes_ty with
        | Hir.Vec (n, ((Hir.Int _ | Hir.Bool | Hir.Float _) as e)) when n = lanes ->
            Ok e
        | _ -> error span "select values must be vectors with the mask lane count"
      in
      Ok
        ( yes_ty,
          List.map2
            (fun mv (yv, nv) -> lane_mask element (if mv <> 0L then yv else nv))
            mask_values
            (List.combine yes_values no_values) )
  | Ast.Call (Ast.Ident (name, _), [ left; right ], _span)
    when List.mem name [ "add_sat"; "sub_sat"; "mul_hi" ] ->
      let* left_ty, left_values, right_ty, right_values = pair left right in
      let* () =
        if left_ty = right_ty then Ok ()
        else
          match Sema_types.integer_literal_vector_argument_error name right left_ty with
          | Some diagnostic -> Error [ diagnostic ]
          | None ->
              error (Ast.expr_span right)
                (Printf.sprintf
                   "argument 2 of `%s` is `%s`, expected the type of argument 1 (`%s`)"
                   name
                   (Sema_types.diagnostic_ty_name right_ty)
                   (Sema_types.diagnostic_ty_name left_ty))
      in
      let* kind =
        match lane_type left_ty with
        | Some (Hir.Int k) -> Ok k
        | _ ->
            error (Ast.expr_span left)
              (Printf.sprintf "`%s` needs an integer or integer vector, got `%s`" name
                 (Sema_types.diagnostic_ty_name left_ty))
      in
      Ok
        ( left_ty,
          List.map2
            (fun a b -> lane_mask (Hir.Int kind) (sat_or_mulhi name kind a b))
            left_values right_values )
  | Ast.Ternary (condition, yes, no, _span) ->
      let* expected =
        if
          expected = None
          && unresolved_shape_of yes = Some Unresolved_vector
          && unresolved_shape_of no = None
        then
          let* peer, _ =
            vector_const_expr ~structs ~named_types ~generic_structs ~arrays
              ~array_lengths ~globals ?resolve consts None ~check_only:true no
          in
          Ok (match peer with Hir.Vec _ -> Some peer | _ -> None)
        else Ok expected
      in
      let ensure_branch expression actual expected =
        let context =
          if scalar_conversion_pair actual expected then "if-expression branch"
          else "value"
        in
        ensure_expected ~leaf_type ~context ~expression actual expected
          (Ast.expr_span expression)
      in
      let ensure_target expression actual =
        match expected with
        | Some target when scalar_conversion_pair actual target ->
            ensure_branch expression actual target
        | _ -> Ok ()
      in
      let* condition_ty, condition_value =
        const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
          ~globals ?resolve consts None ~check_only condition
      in
      if condition_ty <> Hir.Bool then
        Error [ Sema_types.condition_error "if" condition condition_ty ]
      else if condition_value <> 0L then
        let* yes_ty, yes_values = evaluate expected yes in
        let* () = ensure_target yes yes_ty in
        let* no_ty, _ =
          vector_const_expr ~structs ~named_types ~generic_structs ~arrays
            ~array_lengths ~globals ?resolve consts (Some yes_ty) ~check_only:true no
        in
        let* () = ensure_branch no no_ty yes_ty in
        Ok (yes_ty, yes_values)
      else
        let* no_ty, no_values = evaluate expected no in
        let* () = ensure_target no no_ty in
        let* yes_ty, _ =
          vector_const_expr ~structs ~named_types ~generic_structs ~arrays
            ~array_lengths ~globals ?resolve consts (Some no_ty) ~check_only:true yes
        in
        let* () = ensure_branch yes yes_ty no_ty in
        Ok (no_ty, no_values)
  | Ast.Cast (kind, destination, value, span) ->
      let* destination = source_ty_with_values named_types consts span destination in
      let* source, values =
        match evaluate None value with
        | Ok result -> Ok result
        | Error _ ->
            let* source, value =
              const_expr ~structs ~named_types ~generic_structs ~arrays ~array_lengths
                ~globals ?resolve consts None ~check_only value
            in
            Ok (source, [ value ])
      in
      let* () =
        if cast_legal kind source destination then Ok ()
        else
          Error [ Sema_types.cast_error ~expression:value kind source destination span ]
      in
      if kind = Ast.Bitcast then
        let* values =
          match constant_bitcast source values destination with
          | Ok values -> Ok values
          | Error () -> error span "illegal constant bitcast representation"
        in
        Ok (destination, values)
      else
        let* source_element, destination_element =
          match (source, destination) with
          | ( Hir.Vec (source_lanes, source_element),
              Hir.Vec (destination_lanes, destination_element) )
            when source_lanes = destination_lanes ->
              Ok (source_element, destination_element)
          | _ -> error span "constant vector cast requires matching lane counts"
        in
        let source_width = Option.get (integer_value_bit_width source_element) in
        let convert value =
          let value =
            match kind with
            | Ast.Sext when source_width < 64 ->
                let shift = 64 - source_width in
                Int64.shift_right (Int64.shift_left value shift) shift
            | Ast.Trunc when destination_element = Hir.Bool -> Int64.logand value 1L
            | Ast.Zext | Ast.Sext | Ast.Trunc | Ast.Bitcast | Ast.Float_widen -> value
          in
          lane_mask destination_element value
        in
        Ok (destination, List.map convert values)
  | _ ->
      error (Ast.expr_span expression)
        "expression is not a compile-time vector constant"

let resolve_scalar_declarations ?(globals = []) ?(array_lengths = [])
    ?(generic_structs = []) ~structs ~named_types ~resolve_type ~strict items =
  let scalar_constant_type = function
    | Hir.Bool | Hir.Int _ | Hir.Float _ | Hir.Addr | Hir.Handle _ -> true
    | _ -> false
  in
  let declarations =
    List.filter_map
      (function
        | Ast.Const { name; ty_span; ty; value; _ } -> Some (name, (ty, value, ty_span))
        | _ -> None)
      items
  in
  let declaration_table = Hashtbl.create (List.length declarations) in
  List.iter
    (fun (name, declaration) -> Hashtbl.replace declaration_table name declaration)
    declarations;
  let values = Hashtbl.create (List.length declarations) in
  let visiting = Hashtbl.create (List.length declarations) in
  let requires_non_scalar = function
    | [ diagnostic ] ->
        diagnostic.Diag.message = "constant expression requires a known scalar constant"
        || diagnostic.Diag.message
           = "constant expression requires a known vector constant"
    | _ -> false
  in
  let rec resolve ~check_only name span =
    match Hashtbl.find_opt values name with
    | Some (ty, value) -> Ok (ty, value)
    | None -> (
        match Hashtbl.find_opt declaration_table name with
        | None -> global_error span "constant expression requires a known constant"
        | Some (source_type, initial_value, declaration_span) -> (
            let* ty = resolve_type declaration_span source_type in
            if not (scalar_constant_type ty) then
              error span "constant expression requires a known scalar constant"
            else if check_only then Ok (ty, 0L)
            else if Hashtbl.mem visiting name then
              error span
                (Printf.sprintf "cyclic constant dependency involving `%s`" name)
            else
              let () = Hashtbl.add visiting name () in
              let result =
                let* actual_ty, value =
                  const_expr ~structs ~named_types ~generic_structs ~array_lengths
                    ~globals ~resolve [] (Some ty) initial_value
                in
                if Hir.ty_equal actual_ty ty then Ok (ty, value)
                else
                  error (Ast.expr_span initial_value)
                    (constant_initializer_type_message actual_ty ty)
              in
              Hashtbl.remove visiting name;
              match result with
              | Error
                  [
                    {
                      Diag.message = "constant expression requires a known constant";
                      primary;
                      _;
                    };
                  ] ->
                  error primary
                    (Printf.sprintf
                       "constant `%s` initializer uses nonconstant value `%s`" name
                       (Ast.expr_name initial_value))
              | Error _ as failure -> failure
              | Ok (ty, value) as resolved ->
                  Hashtbl.replace values name (ty, value);
                  resolved))
  in
  let rec collect = function
    | [] ->
        Ok
          (List.filter_map
             (fun (name, _) ->
               match Hashtbl.find_opt values name with
               | Some (ty, value) -> Some (name, ty, value)
               | None -> None)
             declarations)
    | (name, (source_type, _, span)) :: rest -> (
        match resolve_type span source_type with
        | Error _ -> collect rest
        | Ok ty when not (scalar_constant_type ty) -> collect rest
        | Ok _ -> (
            match resolve ~check_only:false name span with
            | Ok _ -> collect rest
            | Error diagnostics when requires_non_scalar diagnostics -> collect rest
            | Error ([ { Diag.message; _ } ] as diagnostics)
              when String.starts_with ~prefix:"cyclic constant dependency" message ->
                Error diagnostics
            | Error _ when not strict -> collect rest
            | Error _ as failure -> failure))
  in
  collect declarations
