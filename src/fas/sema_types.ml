type named_type_kind =
  | Struct_name
  | Generic_struct_name
  | Opaque_name
  | C_record_name of string * string option
  | Alias_name of Ast.ty
  | Unsupported_name of string * string

type named_types = (string * named_type_kind) list
type const_values = (string * Hir.ty * int64) list

let ( let* ) result continuation =
  match result with Error error -> Error error | Ok value -> continuation value

let error span message = Error [ Diag.error span message ]

let truthiness_help expression ty =
  match (expression, ty) with
  | Ast.Ident (name, _), Hir.Int _ ->
      Some (Printf.sprintf "Fas has no implicit truth values; write `%s != 0`" name)
  | Ast.Ident (name, _), Hir.Addr ->
      Some (Printf.sprintf "Fas has no implicit truth values; write `%s != null`" name)
  | expression, Hir.Float _ ->
      Some
        (Printf.sprintf "Fas has no implicit truth values; write `%s != 0.0`"
           (Ast.expr_name expression))
  | (Ast.Call _ | Ast.Binary _), Hir.Int _ ->
      Some
        (Printf.sprintf "Fas has no implicit truth values; write `%s != 0`"
           (Ast.expr_name expression))
  | _ -> None

let c_pointer_selection_help value type_hint expected =
  let type_name =
    match (type_hint, expected) with
    | Some name, _ when List.mem name Names.scalar_type_names && name <> "void" ->
        Some name
    | None, Some ((Hir.Bool | Hir.Int _) as ty) -> Some (Hir.ty_name ty)
    | _ -> None
  in
  match (value, type_name) with
  | Ast.Ident (name, _), Some type_name ->
      Some (Printf.sprintf "write `%s[%s]`" name type_name)
  | _ -> None

let c_pointer_selection_diagnostic ?(constant = false) expected = function
  | Ast.C_dereference (Ast.Ident (name, _), Some marker, span)
    when String.starts_with ~prefix:"__fas_c_pointer_cast__:" marker ->
      let cast_type =
        String.sub marker
          (String.length "__fas_c_pointer_cast__:")
          (String.length marker - String.length "__fas_c_pointer_cast__:")
      in
      let help =
        if expected = Some Hir.Addr then Some (Printf.sprintf "write `%s`" name)
        else if List.mem cast_type Names.scalar_type_names then
          Some (Printf.sprintf "write `%s[%s]`" name cast_type)
        else None
      in
      Diag.error ?help span
        (Printf.sprintf "C pointer cast `(%s*)` is not Fas syntax; `addr` is untyped"
           cast_type)
  | Ast.C_dereference (value, type_hint, span) ->
      Diag.error
        ?help:
          (if constant then None else c_pointer_selection_help value type_hint expected)
        span "Fas has no unary `*`; read through an `addr` with `p[T]`"
  | Ast.C_dot_star (value, span) ->
      Diag.error
        ?help:(if constant then None else c_pointer_selection_help value None expected)
        span "Fas has no `.*`; read through an `addr` with `p[T]`"
  | _ -> assert false

let rec expression_start_span = function
  | Ast.Binary (_, left, _, _)
  | Ast.Unary (_, left, _)
  | Ast.C_dereference (left, _, _)
  | Ast.C_dot_star (left, _) ->
      expression_start_span left
  | Ast.Parenthesized (value, _) -> expression_start_span value
  | expression -> Ast.expr_span expression

let condition_error construct expression ty =
  Diag.error
    ?help:(truthiness_help expression ty)
    (expression_start_span expression)
    (Printf.sprintf "condition of `%s` is `%s`, not `bool`" construct (Hir.ty_name ty))

let logical_operand_error ?help operation side expression ty =
  let help = Option.fold ~none:(truthiness_help expression ty) ~some:Option.some help in
  Diag.error ?help (Ast.expr_span expression)
    (Printf.sprintf "%s operand of `%s` is `%s`, not `bool`" side operation
       (Hir.ty_name ty))

let logical_not_error expression ty =
  let help =
    match (expression, ty) with
    | Ast.Ident (name, _), Hir.Int _ -> Some (Printf.sprintf "write `%s == 0`" name)
    | Ast.Ident (name, _), Hir.Addr -> Some (Printf.sprintf "write `%s == null`" name)
    | _, Hir.Float _ ->
        Some (Printf.sprintf "write `%s == 0.0`" (Ast.expr_name expression))
    | _ -> None
  in
  Diag.error ?help (Ast.expr_span expression)
    (Printf.sprintf "operator `!` needs `bool` or a bool vector, got `%s`"
       (Hir.ty_name ty))

let edit_distance left right =
  let left_length = String.length left in
  let right_length = String.length right in
  let previous = Array.init (right_length + 1) Fun.id in
  let current = Array.make (right_length + 1) 0 in
  for i = 1 to left_length do
    current.(0) <- i;
    for j = 1 to right_length do
      let substitution =
        previous.(j - 1) + if left.[i - 1] = right.[j - 1] then 0 else 1
      in
      current.(j) <- min (min (previous.(j) + 1) (current.(j - 1) + 1)) substitution
    done;
    Array.blit current 0 previous 0 (right_length + 1)
  done;
  previous.(right_length)

let primitive_type_names =
  [
    "bool";
    "void";
    "addr";
    "u8";
    "u16";
    "u32";
    "u64";
    "i8";
    "i16";
    "i32";
    "i64";
    "usize";
    "isize";
  ]

let unknown_type_error visible_types span name =
  let candidates =
    List.sort_uniq String.compare (primitive_type_names @ visible_types)
    |> List.filter (fun candidate -> edit_distance name candidate <= 2)
  in
  let help =
    match candidates with
    | [ candidate ] -> Some (Printf.sprintf "did you mean `%s`?" candidate)
    | _ -> None
  in
  Diag.error ?help span (Printf.sprintf "unknown type `%s`" name)

let similar_name_help visible_names name =
  match
    List.filter
      (fun candidate -> edit_distance name candidate <= 2)
      (List.sort_uniq String.compare visible_names)
  with
  | [ candidate ] -> Some (Printf.sprintf "did you mean `%s`?" candidate)
  | _ -> None

let designated_record_entries record_name is_union fields entries span =
  let designator_span = function
    | Ast.Designated_field (_, _, _, span) | Ast.Designated_index (_, _, span) ->
        Some span
    | _ -> None
  in
  let designators = List.filter_map designator_span entries in
  if designators = [] then Ok None
  else if is_union then
    let span = List.hd designators in
    Error
      [
        Diag.error ~help:"initialize the union with one positional value" span
          (Printf.sprintf "designated initializer is not supported for union `%s`"
             record_name);
      ]
  else if List.length designators <> List.length entries then
    Error
      [
        Diag.error ~help:"use only `.field = value` entries" span
          "cannot mix designated and positional entries in one initializer";
      ]
  else
    let rec collect assigned = function
      | [] ->
          Ok
            (Some
               (List.map
                  (fun (field : Hir.field) ->
                    (field, List.assoc_opt field.name assigned))
                  fields))
      | Ast.Designated_field (name, _, value, at) :: rest -> (
          match List.find_opt (fun (field : Hir.field) -> field.name = name) fields with
          | None ->
              Error
                [
                  Diag.error
                    ?help:
                      (similar_name_help
                         (List.map (fun (field : Hir.field) -> field.name) fields)
                         name)
                    at
                    (Printf.sprintf "record `%s` has no field `%s`" record_name name);
                ]
          | Some _ when List.mem_assoc name assigned ->
              Error
                [
                  Diag.error ~help:"remove the repeated field designator" at
                    (Printf.sprintf "field `%s` is designated more than once" name);
                ]
          | Some _ -> collect ((name, value) :: assigned) rest)
      | Ast.Designated_index (_, _, at) :: _ ->
          Error
            [
              Diag.error ~help:"write positional array elements" at
                "array designators are not supported";
            ]
      | _ :: _ ->
          Error
            [
              Diag.error ~help:"use only `.field = value` entries" span
                "cannot mix designated and positional entries in one initializer";
            ]
    in
    collect [] entries

let invalid_designator_error ty expression =
  let help, message, span =
    match expression with
    | Ast.Designated_field (name, _, _, span) ->
        ( (match ty with
          | Hir.Array _ ->
              Some "initialize an array with positional entries `{a, b, ...}`"
          | _ -> None),
          Printf.sprintf "field designator `.%s` requires a struct type, got `%s`" name
            (Hir.ty_name ty),
          span )
    | Ast.Designated_index (_, _, span) ->
        (Some "write positional elements", "array designators are not supported", span)
    | _ ->
        ( Some "remove the designator",
          "designator requires an aggregate initializer",
          Ast.expr_span expression )
  in
  Diag.error ?help span message

let unknown_type_name message =
  let prefix = "unknown type `" in
  if String.starts_with ~prefix message && String.ends_with ~suffix:"`" message then
    Some
      (String.sub message (String.length prefix)
         (String.length message - String.length prefix - 1))
  else None

let raw_access_needs_type_error ?(index = "i") ?(allow_help = true) base span =
  let help =
    match (allow_help, base) with
    | true, Ast.Ident (name, _) when index <> "T" ->
        Some
          (Printf.sprintf "write `%s[T, %s]`, e.g. `%s[u8, %s]`" name index name index)
    | _ -> None
  in
  Diag.error ?help span "raw access on `addr` needs an element type"

let src_int = function
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

let vec_cap_error length (element : Hir.ty) =
  if length > 256 then Some "vector lane count exceeds the portable cap of 256"
  else
    let lane_bits =
      match element with
      | Hir.Bool -> Some 1
      | Hir.Int (Hir.U8 | Hir.I8) -> Some 8
      | Hir.Int (Hir.U16 | Hir.I16) -> Some 16
      | Hir.Int (Hir.U32 | Hir.I32) -> Some 32
      | Hir.Int (Hir.U64 | Hir.I64 | Hir.Usize | Hir.Isize) -> Some 64
      | Hir.Float Hir.F32 -> Some 32
      | Hir.Float Hir.F64 -> Some 64
      | Hir.Addr | Hir.Handle _ -> Some 64
      | _ -> None
    in
    match lane_bits with
    | Some bits when length * bits > 2048 ->
        Some "vector size exceeds the portable cap of 2048 bits"
    | _ -> None

let rec source_ty named_types = function
  | Ast.Bool -> Ok Hir.Bool
  | Ast.Void -> Ok Hir.Void
  | Ast.Int kind -> Ok (Hir.Int (src_int kind))
  | Ast.Float Ast.F32 -> Ok (Hir.Float Hir.F32)
  | Ast.Float Ast.F64 -> Ok (Hir.Float Hir.F64)
  | Ast.Addr -> Ok Hir.Addr
  | Ast.Handle ty ->
      Result.map (fun name -> Hir.Handle name) (handle_target named_types ty)
  | Ast.Array (length, ty) ->
      source_aggregate named_types "array"
        (fun n element -> Hir.Array (n, element))
        length.text ty
  | Ast.Vec (length, ty) -> (
      match
        source_aggregate named_types "vector"
          (fun n element -> Hir.Vec (n, element))
          length.text ty
      with
      | Ok (Hir.Vec (n, element)) -> (
          match vec_cap_error n element with
          | Some message -> Error message
          | None -> Ok (Hir.Vec (n, element)))
      | result -> result)
  | Ast.Named_type ("void", _) -> Ok Hir.Void
  | Ast.Named_type (name, _) when Option.is_some (Names.type_misuse_message name) ->
      Error (Option.get (Names.type_misuse_message name))
  | Ast.Named_type (name, _) -> (
      match List.assoc_opt name named_types with
      | Some Struct_name -> Ok (Hir.Struct name)
      | Some Generic_struct_name ->
          Error (Printf.sprintf "generic struct `%s` requires type arguments" name)
      | Some Opaque_name -> Ok (Hir.Opaque name)
      | Some (C_record_name (record, None)) -> Ok (Hir.Struct record)
      | Some (C_record_name (_, Some reason)) ->
          Error (Printf.sprintf "C declaration `%s` is not supported: %s" name reason)
      | Some (Alias_name ty) -> source_ty named_types ty
      | Some (Unsupported_name (entity, reason)) ->
          Error (Printf.sprintf "C declaration `%s` is not supported: %s" entity reason)
      | None -> Error (Printf.sprintf "unknown type `%s`" name))
  | Ast.Applied_type _ ->
      Error "internal error: generic type application reached ordinary type checking"

and handle_target named_types = function
  | Ast.Named_type (name, _) -> (
      match List.assoc_opt name named_types with
      | Some Struct_name when String.contains name '$' -> Ok name
      | Some (C_record_name (record, _)) -> Ok record
      | _ -> (
          match source_ty named_types (Ast.Named_type (name, Span.synthetic)) with
          | Ok (Hir.Opaque name) -> Ok name
          | Ok _ -> Error "handle type argument must be an opaque type"
          | Error message -> Error message))
  | ty -> (
      match source_ty named_types ty with
      | Ok (Hir.Opaque name) -> Ok name
      | Ok _ -> Error "handle type argument must be an opaque type"
      | Error message -> Error message)

and source_aggregate named_types kind make raw element =
  match int_of_string_opt raw with
  | Some length when length < 0 ->
      Error (Printf.sprintf "%s length cannot be negative: `%d`" kind length)
  | Some length ->
      let* element = source_ty named_types element in
      Ok (make length element)
  | None when Sema_numeric.integer_exceeds_max_int raw ->
      Error (Printf.sprintf "%s length `%s` is too large" kind raw)
  | None -> Error (Printf.sprintf "%s length must be an integer constant" kind)

let rec named_type_span name = function
  | Ast.Named_type (candidate, span) when candidate = name -> Some span
  | Ast.Handle ty | Ast.Array (_, ty) | Ast.Vec (_, ty) -> named_type_span name ty
  | Ast.Applied_type (_, _, arguments, _) ->
      List.find_map
        (function
          | Ast.Type_arg ty | Ast.Type_or_index ty -> named_type_span name ty
          | _ -> None)
        arguments
  | _ -> None

let rec misused_type_span = function
  | Ast.Named_type (name, span) when Option.is_some (Names.type_misuse_message name) ->
      Some (name, span)
  | Ast.Handle ty | Ast.Array (_, ty) | Ast.Vec (_, ty) -> misused_type_span ty
  | Ast.Applied_type (_, _, arguments, _) ->
      List.find_map
        (function
          | Ast.Type_arg ty | Ast.Type_or_index ty -> misused_type_span ty | _ -> None)
        arguments
  | _ -> None

let rec source_type_span_opt = function
  | Ast.Named_type (_, span) | Ast.Applied_type (_, _, _, span) -> Some span
  | Ast.Handle ty -> source_type_span_opt ty
  | Ast.Array (length, ty) | Ast.Vec (length, ty) -> (
      match source_type_span_opt ty with
      | Some _ as span -> span
      | None -> Some length.span)
  | _ -> None

let source_type_span fallback ty =
  Option.value ~default:fallback (source_type_span_opt ty)

let rec opaque_source_span named_types seen = function
  | Ast.Named_type (name, span) when not (List.mem name seen) -> (
      match List.assoc_opt name named_types with
      | Some Opaque_name -> Some span
      | Some (Alias_name ty) ->
          Option.map (fun _ -> span) (opaque_source_span named_types (name :: seen) ty)
      | _ -> None)
  | Ast.Array (_, ty) | Ast.Vec (_, ty) -> opaque_source_span named_types seen ty
  | Ast.Applied_type (_, _, arguments, _) ->
      List.find_map
        (function
          | Ast.Type_arg ty | Ast.Type_or_index ty ->
              opaque_source_span named_types seen ty
          | _ -> None)
        arguments
  | _ -> None

let rec vector_element_source_span fallback = function
  | Ast.Vec (_, (Ast.Vec _ as inner)) -> vector_element_source_span fallback inner
  | Ast.Vec (_, inner) -> Some (source_type_span fallback inner)
  | Ast.Array (_, inner) | Ast.Handle inner -> vector_element_source_span fallback inner
  | Ast.Applied_type (_, _, arguments, _) ->
      List.find_map
        (function
          | Ast.Type_arg ty | Ast.Type_or_index ty ->
              vector_element_source_span fallback ty
          | _ -> None)
        arguments
  | _ -> None

let source_ty_diag named_types span ty =
  source_ty named_types ty
  |> Result.map_error (fun message ->
      match (unknown_type_name message, misused_type_span ty) with
      | Some name, _ ->
          [
            unknown_type_error (List.map fst named_types)
              (Option.value ~default:span (named_type_span name ty))
              name;
          ]
      | None, Some (name, type_span) ->
          [ Diag.error type_span (Option.get (Names.type_misuse_message name)) ]
      | None, None ->
          let primary =
            if
              String.starts_with ~prefix:"array length" message
              || String.starts_with ~prefix:"vector length" message
            then
              let rec bad_length_span = function
                | Ast.Array (length, element) | Ast.Vec (length, element) -> (
                    match int_of_string_opt length.text with
                    | Some value when value >= 0 -> bad_length_span element
                    | Some _ | None -> Some length.span)
                | Ast.Handle inner -> bad_length_span inner
                | Ast.Applied_type (_, _, arguments, _) ->
                    List.find_map
                      (function
                        | Ast.Type_arg ty | Ast.Type_or_index ty -> bad_length_span ty
                        | _ -> None)
                      arguments
                | _ -> None
              in
              Option.value ~default:span (bad_length_span ty)
            else if String.starts_with ~prefix:"vector element type" message then
              Option.value ~default:span (vector_element_source_span span ty)
            else span
          in
          [ Diag.error primary message ])

let declared_c_type_name message =
  let quoted prefix =
    if String.starts_with ~prefix message && String.ends_with ~suffix:"`" message then
      Some
        (String.sub message (String.length prefix)
           (String.length message - String.length prefix - 1))
    else None
  in
  match quoted "unknown type `" with
  | Some name -> Some name
  | None -> quoted "unknown generic struct `"

let rec declared_c_type_span name = function
  | Ast.Named_type (other, span) when other = name -> Some (span, None)
  | Ast.Applied_type (other, span, arguments, _) when other = name ->
      let length =
        match arguments with
        | Ast.Const_arg (Ast.Int_lit (text, _)) :: _ -> text
        | _ -> "N"
      in
      Some (span, Some length)
  | Ast.Handle ty | Ast.Array (_, ty) | Ast.Vec (_, ty) -> declared_c_type_span name ty
  | _ -> None

let declaration_type_error ~declaration ~variable ty result =
  let remap (diagnostic : Diag.t) =
    match declared_c_type_name diagnostic.message with
    | None -> diagnostic
    | Some name -> (
        match declared_c_type_span name ty with
        | None -> diagnostic
        | Some (primary, length) -> (
            match Diag.declaration_type_message ~declaration ~variable ?length name with
            | None -> diagnostic
            | Some message -> { diagnostic with primary; message }))
  in
  Result.map_error (List.map remap) result

let lookup name table = List.find_opt (fun (entry, _, _) -> entry = name) table

let aggregate_length_value kind span ty value =
  if not (Sema_numeric.is_int ty) then
    error span (Printf.sprintf "%s length must be an integer" kind)
  else if Sema_numeric.is_unsigned ty then
    if Int64.unsigned_compare value (Int64.of_int max_int) > 0 then
      error span
        (Printf.sprintf "%s length `%s` is too large" kind
           (Sema_numeric.unsigned_int64_to_string value))
    else Ok (Int64.to_int value)
  else
    let value = Sema_numeric.sign_extend_value ty value in
    if value < 0L then
      error span (Printf.sprintf "%s length cannot be negative: `%Ld`" kind value)
    else if value > Int64.of_int max_int then
      error span (Printf.sprintf "%s length `%Ld` is too large" kind value)
    else Ok (Int64.to_int value)

let aggregate_length_expected _ = Some (Hir.Int Hir.Usize)

let remap_length_cycle span = function
  | Error [ diagnostic ]
    when String.starts_with ~prefix:"cyclic constant dependency" diagnostic.Diag.message
    ->
      Error [ { diagnostic with primary = span } ]
  | result -> result

let resolve_aggregate_length ?(globals = []) ?(kind = "array") values span length =
  match lookup length values with
  | None when List.mem length globals ->
      error span (Printf.sprintf "global `%s` is not a constant" length)
  | None when Sema_numeric.integer_exceeds_max_int length ->
      error span (Printf.sprintf "%s length `%s` is too large" kind length)
  | None -> Ok length
  | Some (_, ty, value) ->
      Result.map string_of_int (aggregate_length_value kind span ty value)

let rec source_ty_with_values ?(globals = []) named_types values span = function
  | Ast.Array (length_info, ty) -> (
      let* length =
        resolve_aggregate_length ~kind:"array" ~globals values length_info.span
          length_info.text
      in
      let* ty = source_ty_with_values ~globals named_types values span ty in
      try
        let length = int_of_string length in
        if length < 0 then
          error length_info.span
            (Printf.sprintf "array length cannot be negative: `%d`" length)
        else Ok (Hir.Array (length, ty))
      with Failure _ ->
        error length_info.span "array length must be an integer constant")
  | Ast.Vec (length_info, ty) -> (
      let* length =
        resolve_aggregate_length ~kind:"vector" ~globals values length_info.span
          length_info.text
      in
      let* ty = source_ty_with_values ~globals named_types values span ty in
      try
        let length = int_of_string length in
        if length < 0 then
          error length_info.span
            (Printf.sprintf "vector length cannot be negative: `%d`" length)
        else
          match vec_cap_error length ty with
          | Some message -> error length_info.span message
          | None -> Ok (Hir.Vec (length, ty))
      with Failure _ ->
        error length_info.span "vector length must be an integer constant")
  | ty -> source_ty_diag named_types span ty

let layout_diag span structs ty =
  Hir.layout structs ty
  |> Result.map_error (fun message ->
      let help =
        match ty with
        | Hir.Opaque name -> Some (Printf.sprintf "write `handle[%s]`" name)
        | _ -> None
      in
      [ Diag.error ?help span message ])

let field_info structs name field =
  match
    List.find_opt (fun (struct_def : Hir.struct_def) -> struct_def.name = name) structs
  with
  | None -> None
  | Some struct_def ->
      List.find_opt
        (fun (candidate : Hir.field) -> candidate.name = field)
        struct_def.fields

let compatible actual expected = Hir.ty_equal actual expected
let diagnostic_ty_name ty = Hir.ty_name ty

let scalar_conversion_pair actual expected =
  match (actual, expected) with
  | Hir.Int _, (Hir.Int _ | Hir.Bool | Hir.Addr) | (Hir.Bool | Hir.Addr), Hir.Int _ ->
      true
  | Hir.Float _, (Hir.Float _ | Hir.Int _) | Hir.Int _, Hir.Float _ -> true
  | _ -> false

let float_literal_type_error ty span =
  Diag.error span
    (Printf.sprintf "floating point literal cannot be used as `%s`"
       (diagnostic_ty_name ty))

let equal_range_integers actual expected =
  actual <> expected
  &&
  match (actual, expected) with
  | Hir.Int (Hir.U64 | Hir.Usize), Hir.Int (Hir.U64 | Hir.Usize)
  | Hir.Int (Hir.I64 | Hir.Isize), Hir.Int (Hir.I64 | Hir.Isize) ->
      true
  | _ -> false

let implicit_integer_widen actual expected =
  match (actual, expected) with
  | Hir.Int _, Hir.Int _ ->
      let actual_bits = Option.get (Sema_numeric.integer_value_bit_width actual) in
      let expected_bits = Option.get (Sema_numeric.integer_value_bit_width expected) in
      if expected_bits = actual_bits && equal_range_integers actual expected then
        Some Ast.Bitcast
      else if expected_bits <= actual_bits then None
      else if Sema_numeric.is_unsigned actual then Some Ast.Zext
      else if not (Sema_numeric.is_unsigned expected) then Some Ast.Sext
      else None
  | _ -> None

let implicit_numeric_widen actual expected =
  match implicit_integer_widen actual expected with
  | Some _ as kind -> kind
  | None when actual = Hir.Float Hir.F32 && expected = Hir.Float Hir.F64 ->
      Some Ast.Float_widen
  | None -> None

let common_integer_type left right =
  if Hir.ty_equal left right then Some left
  else
    match (implicit_integer_widen left right, implicit_integer_widen right left) with
    | Some _, Some _ -> (
        match right with
        | Hir.Int (Hir.Usize | Hir.Isize) -> Some right
        | _ -> Some left)
    | Some _, _ -> Some right
    | _, Some _ -> Some left
    | _ -> None

let common_numeric_type left right =
  if Sema_numeric.is_int left && Sema_numeric.is_int right then
    common_integer_type left right
  else if Sema_numeric.is_float left && Sema_numeric.is_float right then
    Some
      (if left = Hir.Float Hir.F64 || right = Hir.Float Hir.F64 then Hir.Float Hir.F64
       else left)
  else None

let rec narrow_arithmetic_result ~unsigned = function
  | Ast.Parenthesized (expression, _) -> narrow_arithmetic_result ~unsigned expression
  | Ast.Select (expression, [ _ ], _) -> narrow_arithmetic_result ~unsigned expression
  | Ast.Unary ((Ast.Neg | Ast.Bit_not), _, _) -> true
  | Ast.Binary ((Ast.Add | Ast.Sub | Ast.Mul | Ast.Shl), _, _, _) -> true
  | Ast.Binary ((Ast.Div | Ast.Rem), left, right, _) ->
      (not unsigned)
      || narrow_arithmetic_result ~unsigned left
      || narrow_arithmetic_result ~unsigned right
  | Ast.Binary (Ast.Shr, left, _, _) -> narrow_arithmetic_result ~unsigned left
  | Ast.Binary ((Ast.Bit_and | Ast.Bit_or | Ast.Bit_xor), left, right, _) ->
      narrow_arithmetic_result ~unsigned left
      || narrow_arithmetic_result ~unsigned right
  | Ast.Ternary (_, yes, no, _) ->
      narrow_arithmetic_result ~unsigned yes || narrow_arithmetic_result ~unsigned no
  | _ -> false

let rec narrow_float_result expression =
  match expression with
  | Ast.Parenthesized (expression, _) -> narrow_float_result expression
  | Ast.Select (expression, [ _ ], _) -> narrow_float_result expression
  | Ast.Unary (Ast.Neg, _, _) -> true
  | Ast.Binary ((Ast.Add | Ast.Sub | Ast.Mul | Ast.Div), _, _, _) -> true
  | Ast.Call (Ast.Ident (name, _), _, _) when Option.is_some (Names.float_math name) ->
      true
  | Ast.Ternary (_, yes, no, _) -> narrow_float_result yes || narrow_float_result no
  | _ -> false

let narrow_value_result actual expression =
  if Sema_numeric.is_float actual then
    narrow_arithmetic_result ~unsigned:false expression
    || narrow_float_result expression
  else narrow_arithmetic_result ~unsigned:(Sema_numeric.is_unsigned actual) expression

let rec float_literal_expression = function
  | Ast.Float_lit _ -> true
  | Ast.Parenthesized (expression, _) | Ast.Unary (Ast.Neg, expression, _) ->
      float_literal_expression expression
  | _ -> false

let rec integer_literal_expression = function
  | Ast.Int_lit _ -> true
  | Ast.Parenthesized (expression, _)
  | Ast.Unary ((Ast.Neg | Ast.Bit_not), expression, _) ->
      integer_literal_expression expression
  | Ast.Binary
      ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
        | Ast.Bit_xor | Ast.Shl | Ast.Shr ),
        left,
        right,
        _ ) ->
      integer_literal_expression left && integer_literal_expression right
  | Ast.Ternary (_, yes, no, _) ->
      integer_literal_expression yes && integer_literal_expression no
  | _ -> false

let rec unparenthesized = function
  | Ast.Parenthesized (expression, _) -> unparenthesized expression
  | expression -> expression

let ast_int_kind = function
  | Hir.U8 -> Ast.U8
  | Hir.U16 -> Ast.U16
  | Hir.U32 -> Ast.U32
  | Hir.U64 -> Ast.U64
  | Hir.I8 -> Ast.I8
  | Hir.I16 -> Ast.I16
  | Hir.I32 -> Ast.I32
  | Hir.I64 -> Ast.I64
  | Hir.Usize -> Ast.Usize
  | Hir.Isize -> Ast.Isize

let signed_counterpart = function
  | Hir.U8 | Hir.I8 -> Hir.I8
  | Hir.U16 | Hir.I16 -> Hir.I16
  | Hir.U32 | Hir.I32 -> Hir.I32
  | Hir.U64 | Hir.I64 -> Hir.I64
  | Hir.Usize | Hir.Isize -> Hir.Isize

let arithmetic_binop = function
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Shl | Ast.Shr | Ast.Bit_and
  | Ast.Bit_or | Ast.Bit_xor ->
      true
  | _ -> false

let rec implicit_source checked expression =
  match (unparenthesized expression, checked) with
  | Ast.Cast _, _ -> checked
  | _, Some (Hir.Cast ((Ast.Zext | Ast.Sext), inner, _, _)) ->
      implicit_source (Some inner) expression
  | _ -> checked

let binary_operands operation = function
  | Some (Hir.Binary (checked_operation, left, right, _, _))
    when checked_operation = operation ->
      (Some left, Some right)
  | _ -> (None, None)

let unary_operand operation = function
  | Some (Hir.Unary (checked_operation, value, _, _)) when checked_operation = operation
    ->
      Some value
  | _ -> None

let ternary_branches = function
  | Some (Hir.Ternary (_, yes, no, _, _)) -> (Some yes, Some no)
  | _ -> (None, None)

let ast_int_unsigned = function
  | Ast.U8 | Ast.U16 | Ast.U32 | Ast.U64 | Ast.Usize -> true
  | Ast.I8 | Ast.I16 | Ast.I32 | Ast.I64 | Ast.Isize -> false

let type_extension ~default = function
  | Hir.Int kind | Hir.Vec (_, Hir.Int kind) ->
      if Sema_numeric.is_unsigned (Hir.Int kind) then Ast.Zext else Ast.Sext
  | _ -> default

let leaf_extension ?leaf_type ~default expression checked =
  match unparenthesized expression with
  | Ast.Cast (_, (Ast.Int kind | Ast.Vec (_, Ast.Int kind)), _, _) ->
      if ast_int_unsigned kind then Ast.Zext else Ast.Sext
  | Ast.Cast _ -> default
  | _ -> (
      match (implicit_source checked expression, leaf_type) with
      | Some checked, _ -> type_extension ~default (Hir.expr_ty checked)
      | None, Some leaf_type -> (
          match leaf_type expression with
          | Some ty -> type_extension ~default ty
          | None -> default)
      | None, None -> default)

let extension_text = function Ast.Zext -> "zext" | _ -> "sext"

let rec widened_arithmetic ?leaf_type ~unsigned (extension, target) checked expression =
  let checked = implicit_source checked expression in
  let widen = widened_arithmetic ?leaf_type ~unsigned (extension, target) in
  let widen_or_keep child_checked child =
    if narrow_arithmetic_result ~unsigned child then widen child_checked child
    else Some (unparenthesized child)
  in
  match expression with
  | Ast.Parenthesized (inner, _) -> widen checked inner
  | Ast.Binary (((Ast.Shl | Ast.Shr) as op), left, right, span) ->
      let left_checked, _ = binary_operands op checked in
      Option.map
        (fun left -> Ast.Binary (op, left, unparenthesized right, span))
        (widen left_checked left)
  | Ast.Binary (op, left, right, span) when arithmetic_binop op ->
      let left_checked, right_checked = binary_operands op checked in
      if integer_literal_expression left && not (integer_literal_expression right) then
        Option.map
          (fun right -> Ast.Binary (op, unparenthesized left, right, span))
          (widen right_checked right)
      else
        Option.bind (widen left_checked left) (fun left ->
            Option.map
              (fun right -> Ast.Binary (op, left, right, span))
              (widen_or_keep right_checked right))
  | Ast.Unary (((Ast.Neg | Ast.Bit_not) as op), value, span) ->
      Option.map
        (fun value -> Ast.Unary (op, value, span))
        (widen (unary_operand op checked) value)
  | Ast.Ternary (condition, yes, no, span)
    when narrow_arithmetic_result ~unsigned expression -> (
      let yes_checked, no_checked = ternary_branches checked in
      match (widen yes_checked yes, widen no_checked no) with
      | Some yes, Some no -> Some (Ast.Ternary (condition, yes, no, span))
      | _ -> None)
  | _ ->
      if narrow_arithmetic_result ~unsigned expression then None
      else if integer_literal_expression expression then Some expression
      else
        Some
          (Ast.Cast
             ( leaf_extension ?leaf_type ~default:extension expression checked,
               target,
               expression,
               Ast.expr_span expression ))

let convert_expected_kind ?expression actual expected =
  match expression with
  | Some expression
    when narrow_value_result actual expression
         && not (equal_range_integers actual expected) ->
      None
  | _ -> implicit_numeric_widen actual expected

let widen_integer_value kind actual expected value =
  match kind with
  | Ast.Zext -> Sema_numeric.mask_value expected value
  | Ast.Sext ->
      Sema_numeric.mask_value expected (Sema_numeric.sign_extend_value actual value)
  | Ast.Float_widen -> Int64.bits_of_float (Int32.float_of_bits (Int64.to_int32 value))
  | Ast.Trunc | Ast.Bitcast -> value

let function_arity_message name expected actual =
  Printf.sprintf "function `%s` expects %d %s, got %d" name expected
    (if expected = 1 then "argument" else "arguments")
    actual

let generic_arity_message kind name expected actual =
  Printf.sprintf "%s `%s` expects %d generic argument%s, got %d" kind name expected
    (if expected = 1 then "" else "s")
    actual

let constant_initializer_type_message actual expected =
  Printf.sprintf "constant initializer has type `%s`, expected `%s`"
    (diagnostic_ty_name actual) (diagnostic_ty_name expected)

let constant_array_element_type_message actual expected =
  Printf.sprintf "constant array element has type `%s`, expected `%s`"
    (diagnostic_ty_name actual) (diagnostic_ty_name expected)

let shift_operator_name = function
  | Ast.Shl -> "<<"
  | Ast.Shr -> ">>"
  | _ -> assert false

let shift_value_error operator span ty =
  Diag.error span
    (Printf.sprintf
       "left operand of `%s` has type `%s`, expected an integer or integer vector"
       (shift_operator_name operator)
       (diagnostic_ty_name ty))

let shift_count_error operator span ty =
  Diag.error span
    (Printf.sprintf "right operand of `%s` has type `%s`, expected an integer"
       (shift_operator_name operator)
       (diagnostic_ty_name ty))

let shift_count_lanes_error operator span count_ty value_ty =
  Diag.error span
    (Printf.sprintf
       "right operand of `%s` has type `%s`, expected `%s` to match the value"
       (shift_operator_name operator)
       (diagnostic_ty_name count_ty) (diagnostic_ty_name value_ty))

let shift_count_splat_error operator value count =
  let scalar_count =
    match count with
    | Ast.Splat (element, _) -> Ast.expr_name element
    | _ -> assert false
  in
  Diag.error
    ~help:
      (Printf.sprintf "write `%s %s %s`" (Ast.expr_name value)
         (shift_operator_name operator)
         scalar_count)
    (Ast.expr_span count)
    (Printf.sprintf "the shift count `%s` has no vector type" (Ast.expr_name count))

let rotate_value_error name span ty =
  Diag.error span
    (Printf.sprintf "`%s` value must be an integer or integer vector, got `%s`" name
       (diagnostic_ty_name ty))

let rotate_count_error name span ty =
  Diag.error span
    (Printf.sprintf "rotate count for `%s` must be a scalar integer, got `%s`" name
       (diagnostic_ty_name ty))

let signed_division_overflow_message ty x y =
  let signed = Sema_numeric.sign_extend_value ty in
  Printf.sprintf "signed division `%Ld / %Ld` overflows `%s`" (signed x) (signed y)
    (diagnostic_ty_name ty)

let cast_name = function
  | Ast.Zext -> "zext"
  | Ast.Sext -> "sext"
  | Ast.Trunc -> "trunc"
  | Ast.Bitcast -> "bitcast"
  | Ast.Float_widen -> "fpext"

let cast_integer_value = function
  | Hir.Int _ | Hir.Vec (_, Hir.Int _) -> true
  | Hir.Bool | Hir.Float _ | Hir.Addr | Hir.Handle _ | Hir.Array _ | Hir.Struct _
  | Hir.Opaque _ | Hir.Void | Hir.Vec _ ->
      false

let integer_literal_value = function
  | Ast.Int_lit (raw, _) ->
      Option.map
        (fun value -> (false, value))
        (Result.to_option (Sema_numeric.parse_integer raw))
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, _), _) ->
      Option.map
        (fun value -> (true, value))
        (Result.to_option (Sema_numeric.parse_integer raw))
  | _ -> None

let integer_literal_error span ty ~negative ~text ~magnitude =
  match ty with
  | Hir.Float _ ->
      Diag.error
        ~help:(Printf.sprintf "write `%s.0`" text)
        span "integer literals are not floating point literals"
  | Hir.Bool ->
      Diag.error span "integer literal is not a `bool`; write `true` or `false`"
  | _ ->
      let help =
        if negative && Sema_numeric.is_unsigned ty && magnitude = 1L then
          Some
            (Printf.sprintf
               "an unsigned type has no negative values; write 0x%LX for all ones"
               (Sema_numeric.literal_limit ~negative:false ty))
        else None
      in
      Diag.error ?help span
        (Printf.sprintf "integer literal is out of range for %s: `%s`"
           (diagnostic_ty_name ty) text)

let integer_literal_vector_argument_error name expression expected =
  match (integer_literal_value expression, expected) with
  | Some (negative, magnitude), Hir.Vec (_, (Hir.Int _ as element))
    when (if negative then Sema_numeric.fits_negative_literal
          else Sema_numeric.fits_literal)
           element magnitude ->
      Some
        (Diag.error
           ~help:(Printf.sprintf "write `splat(%s)`" (Ast.expr_name expression))
           (Ast.expr_span expression)
           (Printf.sprintf "argument 2 of `%s` is an integer literal, expected `%s`"
              name (diagnostic_ty_name expected)))
  | Some (true, magnitude), Hir.Vec (_, (Hir.Int _ as element))
    when Sema_numeric.is_unsigned element && magnitude <> 0L ->
      Some
        (integer_literal_error (Ast.expr_span expression) element ~negative:true
           ~text:(Ast.expr_name expression) ~magnitude)
  | _ -> None

let integer_maximum ty =
  Sema_numeric.literal_limit ~negative:false ty |> Sema_numeric.unsigned_int64_to_string

let same_width_integer_pair source destination =
  match (source, destination) with
  | Hir.Int Hir.Usize, Hir.Int Hir.U64
  | Hir.Int Hir.U64, Hir.Int Hir.Usize
  | Hir.Int Hir.Isize, Hir.Int Hir.I64
  | Hir.Int Hir.I64, Hir.Int Hir.Isize ->
      true
  | _ -> false

let same_width_scalar_pair source destination =
  match
    ( Sema_numeric.integer_value_bit_width source,
      Sema_numeric.integer_value_bit_width destination )
  with
  | Some source_bits, Some destination_bits -> source_bits = destination_bits
  | _ -> false

let cast_error ?expression kind source destination span =
  let name = cast_name kind in
  let source_name, destination_name =
    ( (match expression with
      | Some expression when Option.is_some (integer_literal_value expression) ->
          Printf.sprintf "integer literal `%s`" (Ast.expr_name expression)
      | _ -> Printf.sprintf "`%s`" (diagnostic_ty_name source)),
      Printf.sprintf "`%s`" (diagnostic_ty_name destination) )
  in
  let scalar_integer_pair =
    match (source, destination) with Hir.Int _, Hir.Int _ -> true | _ -> false
  in
  let lane_clause =
    if scalar_integer_pair then "" else " with the same vector lane count"
  in
  let reason =
    match (kind, source, destination) with
    | Ast.Float_widen, Hir.Float Hir.F32, Hir.Float Hir.F64 ->
        "the conversion is implicit for plain `f32` values"
    | _, Hir.Addr, Hir.Int Hir.Usize -> "address-to-integer conversion uses `addr_bits`"
    | _, Hir.Int Hir.Usize, Hir.Addr ->
        "integer-to-address conversion uses `addr_from_bits`"
    | Ast.Bitcast, _, _ -> (
        match
          ( Sema_numeric.integer_value_bit_width source,
            Sema_numeric.integer_value_bit_width destination )
        with
        | Some source_bits, Some destination_bits when source_bits <> destination_bits
          ->
            Printf.sprintf "the source is %d bits and the destination is %d bits"
              source_bits destination_bits
        | _
          when (match source with Hir.Float _ -> true | _ -> false)
               || match destination with Hir.Float _ -> true | _ -> false ->
            "both types must be bool, integer, floating-point, or integer-vector types \
             of equal width"
        | _ ->
            "both types must be bool, integer, or integer-vector types of equal width")
    | (Ast.Zext | Ast.Sext), _, _
      when not (cast_integer_value source && cast_integer_value destination) ->
        "the source and destination must be integer types"
    | (Ast.Zext | Ast.Sext | Ast.Trunc), _, _ when Hir.ty_equal source destination ->
        Printf.sprintf "the value already has type `%s`" (diagnostic_ty_name source)
    | (Ast.Zext | Ast.Sext | Ast.Trunc), _, _
      when same_width_integer_pair source destination ->
        Printf.sprintf
          "`%s` and `%s` have the same width and convert implicitly; no conversion is \
           needed"
          (diagnostic_ty_name source)
          (diagnostic_ty_name destination)
    | (Ast.Zext | Ast.Sext | Ast.Trunc), Hir.Int _, Hir.Int _
      when same_width_scalar_pair source destination ->
        "the source and destination have the same width; use `bitcast`"
    | (Ast.Zext | Ast.Sext), _, _ ->
        "the destination must be a wider integer type" ^ lane_clause
    | Ast.Trunc, _, _
      when not (cast_integer_value source && cast_integer_value destination) ->
        "the source and destination must be integer types"
    | Ast.Trunc, _, _ -> "the destination must be a narrower integer type" ^ lane_clause
    | Ast.Float_widen, _, _ -> "only `f32` can widen to `f64`"
  in
  let message =
    Printf.sprintf "illegal `%s` from %s to %s: %s" name source_name destination_name
      reason
  in
  let help =
    match (kind, source, destination, expression) with
    | _, Hir.Addr, Hir.Int Hir.Usize, Some (Ast.Ident (variable, _)) ->
        Some (Printf.sprintf "write `addr_bits(%s)`" variable)
    | _, Hir.Int Hir.Usize, Hir.Addr, Some (Ast.Ident (variable, _)) ->
        Some (Printf.sprintf "write `addr_from_bits(%s)`" variable)
    | Ast.Trunc, Hir.Int _, Hir.Int _, Some (Ast.Ident (variable, _))
      when Option.get (Sema_numeric.integer_value_bit_width destination)
           > Option.get (Sema_numeric.integer_value_bit_width source) ->
        let extension = if Sema_numeric.is_unsigned source then "zext" else "sext" in
        Some
          (Printf.sprintf "write `%s[%s](%s)`" extension
             (diagnostic_ty_name destination)
             variable)
    | (Ast.Zext | Ast.Sext), Hir.Int _, Hir.Int _, Some (Ast.Ident (variable, _))
      when Option.get (Sema_numeric.integer_value_bit_width destination)
           < Option.get (Sema_numeric.integer_value_bit_width source) ->
        Some
          (Printf.sprintf "write `trunc[%s](%s)`"
             (diagnostic_ty_name destination)
             variable)
    | (Ast.Zext | Ast.Sext | Ast.Trunc), _, _, Some expression
      when same_width_integer_pair source destination ->
        Some (Printf.sprintf "write `%s`" (Ast.expr_name expression))
    | (Ast.Zext | Ast.Sext | Ast.Trunc), Hir.Int _, Hir.Int _, Some expression
      when same_width_scalar_pair source destination
           && not (Hir.ty_equal source destination) ->
        Some
          (Printf.sprintf "write `bitcast[%s](%s)`"
             (diagnostic_ty_name destination)
             (Ast.expr_name expression))
    | _ -> None
  in
  Diag.error ?help span message

let cast_target_error kind target =
  let expected =
    match kind with
    | Ast.Zext | Ast.Sext -> "an integer or integer-vector type"
    | Ast.Trunc -> "an integer or integer-vector type"
    | Ast.Bitcast -> "a bool, integer, floating-point, or integer-vector type"
    | Ast.Float_widen -> "`f64`"
  in
  Printf.sprintf "illegal `%s` target `%s`: expected %s" (cast_name kind)
    (diagnostic_ty_name target) expected

let missing_field_message ?record_name field receiver =
  match receiver with
  | Hir.Struct record ->
      Printf.sprintf "record `%s` has no field `%s`"
        (Option.value ~default:record record_name)
        field
  | _ -> Printf.sprintf "no field `%s` on `%s`" field (diagnostic_ty_name receiver)

let record_field_count_message record expected actual =
  let noun = if expected = 1 then "field" else "fields" in
  Printf.sprintf "record `%s` has %d %s, got %d" record expected noun actual

let array_element_count_message expected actual =
  Printf.sprintf "array of %d elements, got %d" expected actual

let aggregate_count_error_span span n xs =
  Option.fold ~none:span ~some:Ast.expr_span (List.nth_opt xs n)

let ensure_expected ?(context = "value") ?expression ?checked_expression ?leaf_type
    ?(widening_help = true) actual expected span =
  if
    compatible actual expected
    || Option.is_some (convert_expected_kind ?expression actual expected)
  then Ok ()
  else if
    match (checked_expression, expected) with
    | Some (Hir.Address (place, _, _)), Hir.Handle target -> (
        match Hir.expr_ty place with
        | Hir.Struct source -> source <> target
        | _ -> false)
    | _ -> false
  then
    let source_record =
      match checked_expression with
      | Some (Hir.Address (place, _, _)) -> diagnostic_ty_name (Hir.expr_ty place)
      | _ -> assert false
    in
    Diag.error span
      (Printf.sprintf "%s is `addr` to `%s`, expected `%s`" context source_record
         (diagnostic_ty_name expected))
    |> fun diagnostic -> Error [ diagnostic ]
  else if scalar_conversion_pair actual expected then
    let action, relation =
      match context with
      | "return value" -> ("return", "as")
      | context when String.starts_with ~prefix:"argument " context -> ("pass", "as")
      | context
        when String.starts_with ~prefix:"value for " context
             || context = "assigned value" ->
          ("assign", "to")
      | _ -> ("use", "as")
    in
    let source = diagnostic_ty_name actual
    and destination = diagnostic_ty_name expected in
    let source_bits = Sema_numeric.integer_value_bit_width actual
    and destination_bits = Sema_numeric.integer_value_bit_width expected in
    let narrow_result =
      match (expression, source_bits, destination_bits, actual, expected) with
      | Some source, Some from_bits, Some to_bits, Hir.Int _, Hir.Int _ ->
          narrow_arithmetic_result ~unsigned:(Sema_numeric.is_unsigned actual) source
          && from_bits < to_bits
      | _ -> false
    in
    let narrow_float =
      actual = Hir.Float Hir.F32 && expected = Hir.Float Hir.F64
      && Option.fold ~none:false ~some:narrow_float_result expression
    in
    let signed_to_unsigned =
      (not (Sema_numeric.is_unsigned actual)) && Sema_numeric.is_unsigned expected
    in
    let reason =
      match (actual, expected) with
      | Hir.Int _, Hir.Int _
        when (not (Sema_numeric.is_unsigned actual))
             && Sema_numeric.is_unsigned expected ->
          "negative values change meaning"
      | Hir.Int _, Hir.Int _ when Sema_numeric.is_unsigned actual ->
          Printf.sprintf "values above %s %s" (integer_maximum expected)
            (if
               source_bits = destination_bits && not (Sema_numeric.is_unsigned expected)
             then "change meaning"
             else "would be lost")
      | Hir.Int _, Hir.Int _ ->
          Printf.sprintf "values outside %Ld..%s would be lost"
            (Int64.neg (Sema_numeric.literal_limit ~negative:true expected))
            (integer_maximum expected)
      | Hir.Bool, Hir.Int _ | Hir.Int _, Hir.Bool ->
          "bool and integer values use different representations"
      | Hir.Addr, Hir.Int _ -> "addresses convert to integers with `addr_bits`"
      | Hir.Int _, Hir.Addr -> "integers convert to addresses with `addr_from_bits`"
      | Hir.Float Hir.F32, Hir.Float Hir.F64 ->
          "the `f32` result may round before it reaches `f64`"
      | Hir.Float _, Hir.Float _ -> "floating-point widths need explicit conversion"
      | Hir.Float _, Hir.Int _ ->
          "floating-point values need explicit conversion to integer"
      | Hir.Int _, Hir.Float _ ->
          "integer values need explicit conversion to floating point"
      | _ -> assert false
    in
    let message =
      match expression with
      | Some expression when narrow_float ->
          Printf.sprintf
            "`%s` is computed in `f32` and may round before it reaches `f64`"
            (Ast.expr_name expression)
      | Some expression when narrow_result ->
          Printf.sprintf "`%s` is computed in `%s` and may wrap before it reaches `%s`"
            (Ast.expr_name expression) source destination
      | _ ->
          Printf.sprintf "cannot %s `%s` %s `%s`: %s" action source relation destination
            reason
    in
    let operand = Option.fold ~none:"value" ~some:Ast.expr_name expression in
    let lane_index_help =
      match (expression, actual, expected) with
      | Some expression, Hir.Int _, Hir.Int target_kind
        when narrow_result && widening_help -> (
          match unparenthesized expression with
          | Ast.Select (base_expression, [ Ast.Const_arg index ], _) -> (
              match (unparenthesized base_expression, checked_expression) with
              | ( Ast.Binary
                    ( ((Ast.Add | Ast.Sub | Ast.Mul | Ast.Div) as operator),
                      left,
                      right,
                      _ ),
                  Some (Hir.Index (base, _, _, _)) ) -> (
                  match Hir.expr_ty base with
                  | Hir.Vec (lanes, Hir.Int _) ->
                      let element_kind =
                        if signed_to_unsigned then signed_counterpart target_kind
                        else target_kind
                      in
                      let vector_ty =
                        diagnostic_ty_name (Hir.Vec (lanes, Hir.Int element_kind))
                      in
                      let default_extension =
                        if Sema_numeric.is_unsigned actual then Ast.Zext else Ast.Sext
                      in
                      let left_checked, right_checked =
                        binary_operands operator
                          (implicit_source (Some base) base_expression)
                      in
                      let left_extension =
                        leaf_extension ~default:default_extension left left_checked
                      and right_extension =
                        leaf_extension ~default:default_extension right right_checked
                      in
                      let operator_text =
                        match operator with
                        | Ast.Add -> "+"
                        | Ast.Sub -> "-"
                        | Ast.Mul -> "*"
                        | Ast.Div -> "/"
                        | _ -> assert false
                      in
                      let left = Ast.expr_name left and right = Ast.expr_name right in
                      let widened_left =
                        Printf.sprintf "%s[%s](%s)"
                          (extension_text left_extension)
                          vector_ty left
                      and widened_right =
                        Printf.sprintf "%s[%s](%s)"
                          (extension_text right_extension)
                          vector_ty right
                      in
                      let widened =
                        Printf.sprintf "(%s %s %s)[%s]" widened_left operator_text
                          widened_right (Ast.expr_name index)
                      in
                      let widened =
                        if signed_to_unsigned then
                          Printf.sprintf "bitcast[%s](%s)" destination widened
                        else widened
                      in
                      Some (Printf.sprintf "widen vector operands first: `%s`" widened)
                  | _ -> None)
              | _ -> None)
          | _ -> None)
      | _ -> None
    in
    let help =
      match (expression, actual, expected) with
      | _ when Option.is_some lane_index_help -> lane_index_help
      | ( Some
            (Ast.Binary
               (((Ast.Add | Ast.Sub | Ast.Mul | Ast.Div) as operator), left, right, _)),
          Hir.Float Hir.F32,
          Hir.Float Hir.F64 )
        when narrow_float && widening_help ->
          let operator_text =
            match operator with
            | Ast.Add -> "+"
            | Ast.Sub -> "-"
            | Ast.Mul -> "*"
            | Ast.Div -> "/"
            | _ -> assert false
          in
          let help_expression =
            if float_literal_expression left && not (float_literal_expression right)
            then
              Printf.sprintf "%s %s cvt[f64](%s)" (Ast.expr_name left) operator_text
                (Ast.expr_name right)
            else
              Printf.sprintf "cvt[f64](%s) %s %s" (Ast.expr_name left) operator_text
                (Ast.expr_name right)
          in
          Some (Printf.sprintf "write `%s`" help_expression)
      | Some (Ast.Unary (Ast.Neg, value, _)), Hir.Float Hir.F32, Hir.Float Hir.F64
        when narrow_float && widening_help ->
          Some (Printf.sprintf "write `-cvt[f64](%s)`" (Ast.expr_name value))
      | Some expression, Hir.Float _, (Hir.Float _ | Hir.Int _)
      | Some expression, Hir.Int _, Hir.Float _ ->
          Some
            (Printf.sprintf "write `cvt[%s](%s)`" destination (Ast.expr_name expression))
      | ( Some ((Ast.Binary _ | Ast.Unary ((Ast.Neg | Ast.Bit_not), _, _)) as expression),
          Hir.Int _,
          Hir.Int target_kind )
        when narrow_result && widening_help ->
          let extension =
            if Sema_numeric.is_unsigned actual then Ast.Zext else Ast.Sext
          in
          let widening_kind =
            if signed_to_unsigned then signed_counterpart target_kind else target_kind
          in
          Option.map
            (fun widened ->
              let widened =
                if signed_to_unsigned then
                  Ast.Cast
                    ( Ast.Bitcast,
                      Ast.Int (ast_int_kind target_kind),
                      widened,
                      Ast.expr_span widened )
                else widened
              in
              Printf.sprintf "widen an operand first: `%s`" (Ast.expr_name widened))
            (widened_arithmetic ?leaf_type
               ~unsigned:(Sema_numeric.is_unsigned actual)
               (extension, Ast.Int (ast_int_kind widening_kind))
               checked_expression expression)
      | _, Hir.Int _, Hir.Int _
        when (not (Sema_numeric.is_unsigned actual))
             && Sema_numeric.is_unsigned expected
             && source_bits = destination_bits ->
          Some
            (Printf.sprintf
               "reinterpret the bits with `bitcast[%s](%s)`, or widen with \
                `sext`/`zext` first if that is what you mean"
               destination operand)
      | _, Hir.Int _, Hir.Int _
        when (not (Sema_numeric.is_unsigned actual))
             && Sema_numeric.is_unsigned expected ->
          let extension = if source_bits < destination_bits then "sext" else "trunc" in
          Some
            (Printf.sprintf "write `%s[%s](%s)` if intended" extension destination
               operand)
      | _, Hir.Int _, Hir.Int _ when source_bits = destination_bits ->
          Some (Printf.sprintf "write `bitcast[%s](%s)`" destination operand)
      | _, Hir.Int _, Hir.Int _ when source_bits > destination_bits ->
          Some
            (Printf.sprintf "keep the low bits with `trunc[%s](%s)`" destination operand)
      | _, Hir.Bool, Hir.Int _ ->
          Some (Printf.sprintf "write `zext[%s](%s)`" destination operand)
      | _, Hir.Int _, Hir.Bool -> Some (Printf.sprintf "write `%s != 0`" operand)
      | _, Hir.Int _, Hir.Addr ->
          let cast =
            if actual = Hir.Int Hir.Usize then operand
            else if source_bits = Some 64 then
              Printf.sprintf "bitcast[usize](%s)" operand
            else
              let extension =
                if Sema_numeric.is_unsigned actual then "zext" else "sext"
              in
              Printf.sprintf "%s[usize](%s)" extension operand
          in
          Some (Printf.sprintf "write `addr_from_bits(%s)`" cast)
      | _, Hir.Addr, Hir.Int _ ->
          Some
            (if expected = Hir.Int Hir.Usize then
               Printf.sprintf "write `addr_bits(%s)`" operand
             else
               Printf.sprintf "write `%s[%s](addr_bits(%s))`"
                 (if destination_bits = Some 64 then "bitcast" else "trunc")
                 destination operand)
      | _ -> None
    in
    Diag.error ?help span message |> fun diagnostic -> Error [ diagnostic ]
  else
    let help =
      match (expression, actual, expected) with
      | Some (Ast.Ident (name, _)), Hir.Int _, Hir.Int _
        when let actual_bits =
               Option.get (Sema_numeric.integer_value_bit_width actual)
             in
             let expected_bits =
               Option.get (Sema_numeric.integer_value_bit_width expected)
             in
             expected_bits > actual_bits
             && Sema_numeric.is_unsigned actual = Sema_numeric.is_unsigned expected ->
          let extension = if Sema_numeric.is_unsigned actual then "zext" else "sext" in
          Some
            (Printf.sprintf "write `%s[%s](%s)`" extension (diagnostic_ty_name expected)
               name)
      | _ -> None
    in
    Diag.error ?help span
      (Printf.sprintf "%s is `%s`, expected `%s`" context (diagnostic_ty_name actual)
         (diagnostic_ty_name expected))
    |> fun diagnostic -> Error [ diagnostic ]

let unify_if_branches ?leaf_type ?left_checked ?right_checked left_expression left_type
    right_expression right_type =
  let narrow_error expression checked actual target is_float =
    let help =
      match
        ensure_expected ~context:"if-expression branch" ?leaf_type
          ?checked_expression:checked ~expression actual target
          (Ast.expr_span expression)
      with
      | Error [ diagnostic ] -> diagnostic.Diag.help
      | _ -> None
    in
    Diag.error ?help (Ast.expr_span expression)
      (if is_float then
         Printf.sprintf
           "if-expression branches are `%s` and `%s`: `f32` arithmetic may round \
            before widening"
           (diagnostic_ty_name left_type)
           (diagnostic_ty_name right_type)
       else
         Printf.sprintf
           "if-expression branches are `%s` and `%s`: narrow arithmetic may wrap \
            before widening"
           (diagnostic_ty_name left_type)
           (diagnostic_ty_name right_type))
  in
  let int_branches = Sema_numeric.is_int left_type && Sema_numeric.is_int right_type in
  let float_branches =
    Sema_numeric.is_float left_type && Sema_numeric.is_float right_type
  in
  let common =
    if int_branches then common_integer_type left_type right_type
    else if float_branches then common_numeric_type left_type right_type
    else if compatible left_type right_type then Some left_type
    else None
  in
  match common with
  | None ->
      Error
        (Diag.error
           (Ast.expr_span right_expression)
           (Printf.sprintf "if-expression branches %s `%s` and `%s`%s"
              (if int_branches || float_branches then "are" else "have different types:")
              (diagnostic_ty_name left_type)
              (diagnostic_ty_name right_type)
              (if int_branches then ": no type holds both"
               else if float_branches then ": no common floating type"
               else "")))
  | Some result_type ->
      let left_widen = implicit_numeric_widen left_type result_type
      and right_widen = implicit_numeric_widen right_type result_type in
      if
        Option.is_some left_widen
        && (not (equal_range_integers left_type result_type))
        && narrow_value_result left_type left_expression
      then
        Error
          (narrow_error left_expression left_checked left_type result_type
             (left_type = Hir.Float Hir.F32))
      else if
        Option.is_some right_widen
        && (not (equal_range_integers right_type result_type))
        && narrow_value_result right_type right_expression
      then
        Error
          (narrow_error right_expression right_checked right_type result_type
             (right_type = Hir.Float Hir.F32))
      else Ok (result_type, left_widen, right_widen)

let binary_operator_name = function
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Rem -> "%"
  | Ast.Bit_and -> "&"
  | Ast.Bit_or -> "|"
  | Ast.Bit_xor -> "^"
  | Ast.Eq -> "=="
  | Ast.Ne -> "!="
  | Ast.Lt -> "<"
  | Ast.Le -> "<="
  | Ast.Gt -> ">"
  | Ast.Ge -> ">="
  | Ast.And -> "&&"
  | Ast.Or -> "||"
  | Ast.Shl -> "<<"
  | Ast.Shr -> ">>"

let is_comparison = function
  | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> true
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
  | Ast.Bit_xor | Ast.And | Ast.Or | Ast.Shl | Ast.Shr ->
      false

let is_simple_comparison_operand = function
  | Ast.Int_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _ | Ast.Ident _ -> true
  | _ -> false

let comparison_chain_diagnostic span operation left right =
  match left with
  | Ast.Binary (inner_operation, first, middle, _)
    when is_comparison inner_operation && is_comparison operation ->
      let help =
        if is_simple_comparison_operand middle && is_simple_comparison_operand right
        then
          Some
            (Printf.sprintf "write `%s %s %s && %s %s %s`" (Ast.expr_name first)
               (binary_operator_name inner_operation)
               (Ast.expr_name middle) (Ast.expr_name middle)
               (binary_operator_name operation)
               (Ast.expr_name right))
        else None
      in
      Some (Diag.error ?help span "comparisons cannot be chained")
  | _ -> None

let ordering_direction = function
  | Ast.Lt | Ast.Le -> Some `Ascending
  | Ast.Gt | Ast.Ge -> Some `Descending
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
  | Ast.Bit_xor | Ast.Eq | Ast.Ne | Ast.And | Ast.Or | Ast.Shl | Ast.Shr ->
      None

let binary_widening_target left_expression right_expression left right =
  match (left, right, left_expression, right_expression) with
  | Hir.Int _, Hir.Int _, Some left_expression, Some right_expression
    when Sema_numeric.is_unsigned left = Sema_numeric.is_unsigned right ->
      let left_width = Option.get (Sema_numeric.integer_value_bit_width left) in
      let right_width = Option.get (Sema_numeric.integer_value_bit_width right) in
      if left_width < right_width then Some (left_expression, left, right)
      else if right_width < left_width then Some (right_expression, right, left)
      else None
  | _ -> None

let binary_widening_help = function
  | Some (Ast.Ident (name, _), actual, expected) ->
      let extension = if Sema_numeric.is_unsigned actual then "zext" else "sext" in
      Some
        (Printf.sprintf "write `%s[%s](%s)`" extension (diagnostic_ty_name expected)
           name)
  | _ -> None

let mixed_widen_help operation target left_extension right_extension left_expr
    right_expr =
  Some
    (Printf.sprintf "widen both to `%s`: `%s[%s](%s) %s %s[%s](%s)`" target
       left_extension target (Ast.expr_name left_expr)
       (binary_operator_name operation)
       right_extension target (Ast.expr_name right_expr))

let binary_result_type ?left_expression ?right_expression ?left_checked ?right_checked
    ?leaf_type ?result_expected ?(comparison_chain_rewrite_valid = true) span operation
    left right =
  let result_span expression =
    match expression with Some expression -> Ast.expr_span expression | None -> span
  in
  let comparison_chain () =
    match (left_expression, right_expression) with
    | Some (Ast.Binary (inner_operation, first, middle, _)), Some last
      when is_comparison inner_operation && is_comparison operation ->
        let message =
          match (ordering_direction inner_operation, ordering_direction operation) with
          | Some inner_direction, Some outer_direction
            when inner_direction = outer_direction
                 && left = Hir.Bool && Sema_numeric.is_int right
                 && comparison_chain_rewrite_valid ->
              Printf.sprintf "comparisons cannot be chained; write `%s && %s %s %s`"
                (Ast.expr_name (Ast.Binary (inner_operation, first, middle, span)))
                (Ast.expr_name middle)
                (binary_operator_name operation)
                (Ast.expr_name last)
          | _
            when (operation = Ast.Eq || operation = Ast.Ne)
                 && left = Hir.Bool && right = Hir.Bool ->
              "comparisons cannot be chained; add parentheses"
          | _ -> ""
        in
        if message = "" then None
        else Some (Diag.error (result_span right_expression) message)
    | _ -> None
  in
  let parenthesized_comparison =
    match left_expression with
    | Some (Ast.Parenthesized (Ast.Binary (inner, _, _, _), _)) -> is_comparison inner
    | _ -> false
  in
  match comparison_chain () with
  | Some diagnostic -> Error [ diagnostic ]
  | None
    when parenthesized_comparison
         && (match operation with
           | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> true
           | _ -> false)
         && not
              (Sema_numeric.is_int left || left = Hir.Addr || Sema_numeric.is_float left
              ||
              match left with
              | Hir.Vec (_, (Hir.Int _ | Hir.Float _)) -> true
              | _ -> false) ->
      let error_span = if left = right then result_span left_expression else span in
      error error_span
        (Printf.sprintf
           "ordered comparison `%s` needs an integer or integer vector, got `%s`"
           (binary_operator_name operation)
           (diagnostic_ty_name left))
  | None
    when (operation = Ast.Bit_and || operation = Ast.Bit_or || operation = Ast.Bit_xor)
         && (left = Hir.Addr || right = Hir.Addr) ->
      let offending =
        if left = Hir.Addr && right <> Hir.Addr then left_expression
        else right_expression
      in
      error (result_span offending)
        (Printf.sprintf "bitwise `%s` is not defined for `addr`"
           (binary_operator_name operation))
  | None
    when (operation = Ast.Add || operation = Ast.Sub)
         && (left = Hir.Addr || right = Hir.Addr) ->
      error
        (result_span right_expression)
        (Printf.sprintf
           "address arithmetic for `%s` has operands `%s` and `%s`; expected a scalar \
            integer offset"
           (binary_operator_name operation)
           (diagnostic_ty_name left) (diagnostic_ty_name right))
  | None
    when (operation = Ast.Mul || operation = Ast.Div || operation = Ast.Rem)
         && (left = Hir.Addr || right = Hir.Addr) ->
      let offending = if left = Hir.Addr then left_expression else right_expression in
      error (result_span offending)
        (Printf.sprintf "arithmetic `%s` is not defined for `addr`"
           (binary_operator_name operation))
  | None
    when left <> right && operation <> Ast.Shl && operation <> Ast.Shr
         && Sema_numeric.is_int left && Sema_numeric.is_int right
         && Option.is_none (common_integer_type left right) ->
      let help =
        match (left, right, left_expression, right_expression) with
        | Hir.Int Hir.U32, Hir.Int Hir.I32, Some left_expr, Some right_expr ->
            mixed_widen_help operation "i64" "zext" "sext" left_expr right_expr
        | Hir.Int Hir.I32, Hir.Int Hir.U32, Some left_expr, Some right_expr ->
            mixed_widen_help operation "i64" "sext" "zext" left_expr right_expr
        | ( Hir.Int (Hir.U8 | Hir.I8),
            Hir.Int (Hir.U8 | Hir.I8),
            Some left_expr,
            Some right_expr )
          when left <> right ->
            mixed_widen_help operation "i16"
              (if Sema_numeric.is_unsigned left then "zext" else "sext")
              (if Sema_numeric.is_unsigned right then "zext" else "sext")
              left_expr right_expr
        | _ -> None
      in
      let help =
        match (result_expected, help) with
        | Some expected, Some _ when not (is_comparison operation) ->
            let common = Hir.Int Hir.I64 in
            if
              Hir.ty_equal expected common
              || Option.is_some (implicit_integer_widen common expected)
            then help
            else None
        | _ -> help
      in
      let message =
        if is_comparison operation then
          Printf.sprintf "cannot compare `%s` with `%s`: no type holds both"
            (diagnostic_ty_name left) (diagnostic_ty_name right)
        else
          Printf.sprintf "cannot apply `%s` to `%s` and `%s`: no type holds both"
            (binary_operator_name operation)
            (diagnostic_ty_name left) (diagnostic_ty_name right)
      in
      Diag.error ?help (result_span right_expression) message |> fun diagnostic ->
      Error [ diagnostic ]
  | None
    when left <> right && operation <> Ast.Shl && operation <> Ast.Shr
         && Sema_numeric.is_int left && Sema_numeric.is_int right
         && Option.is_some (common_integer_type left right) -> (
      let common = Option.get (common_integer_type left right) in
      let check_narrow expression checked actual =
        match expression with
        | Some expression
          when Option.is_some (implicit_integer_widen actual common)
               && narrow_arithmetic_result
                    ~unsigned:(Sema_numeric.is_unsigned actual)
                    expression ->
            ensure_expected ~expression ?checked_expression:checked ?leaf_type actual
              common (Ast.expr_span expression)
        | _ -> Ok ()
      in
      let* () = check_narrow left_expression left_checked left in
      let* () = check_narrow right_expression right_checked right in
      match operation with
      | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> Ok Hir.Bool
      | _ -> Ok common)
  | None
    when match (left, right) with
         | ( Hir.Vec (lanes, (Hir.Float _ as element)),
             Hir.Vec (right_lanes, right_element) )
           when lanes = right_lanes && element = right_element ->
             true
         | _ -> false -> (
      if operation = Ast.Rem then
        error
          (result_span right_expression)
          "remainder `%` is not defined for floating-point vectors"
      else
        match operation with
        | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge ->
            let lanes = match left with Hir.Vec (lanes, _) -> lanes | _ -> 0 in
            Ok (Hir.Vec (lanes, Hir.Bool))
        | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> Ok left
        | Ast.Bit_and | Ast.Bit_or | Ast.Bit_xor ->
            error (result_span left_expression)
              (Printf.sprintf
                 "operator `%s` needs integer or integer vector operands, got `%s`"
                 (binary_operator_name operation)
                 (diagnostic_ty_name left))
        | Ast.And | Ast.Or -> assert false
        | Ast.Rem -> assert false
        | Ast.Shl | Ast.Shr -> Error [ shift_value_error operation span left ])
  | None when Sema_numeric.is_float left && Sema_numeric.is_float right -> (
      let common = Option.get (common_numeric_type left right) in
      let check_narrow expression actual =
        match expression with
        | Some expression
          when Option.is_some (implicit_numeric_widen actual common)
               && narrow_float_result expression ->
            ensure_expected ~expression actual common (Ast.expr_span expression)
        | _ -> Ok ()
      in
      if operation = Ast.Rem then
        let right_span = result_span right_expression in
        let left_name = Option.fold ~none:"x" ~some:Ast.expr_name left_expression
        and right_name = Option.fold ~none:"y" ~some:Ast.expr_name right_expression in
        let diagnostic =
          Diag.error
            ~help:
              (Printf.sprintf "use C `fmod(%s, %s)` from `<math.h>`" left_name
                 right_name)
            right_span "remainder `%` is not defined for floating point"
        in
        Error [ diagnostic ]
      else
        let* () =
          if common = Hir.Float Hir.F64 then
            let* () = check_narrow left_expression left in
            check_narrow right_expression right
          else Ok ()
        in
        match operation with
        | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> Ok Hir.Bool
        | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> Ok common
        | Ast.Bit_and | Ast.Bit_or | Ast.Bit_xor ->
            error (result_span left_expression)
              (Printf.sprintf
                 "operator `%s` needs integer or integer vector operands, got `%s`"
                 (binary_operator_name operation)
                 (diagnostic_ty_name left))
        | Ast.And | Ast.Or -> assert false
        | Ast.Rem -> assert false
        | Ast.Shl | Ast.Shr -> Error [ shift_value_error operation span left ])
  | None
    when not
           (Hir.ty_equal left right
           || (operation = Ast.Eq || operation = Ast.Ne)
              && (compatible left right || compatible right left)) ->
      let widening =
        binary_widening_target left_expression right_expression left right
      in
      let offending = Option.map (fun (expression, _, _) -> expression) widening in
      let span =
        match offending with
        | Some expression -> result_span (Some expression)
        | None -> result_span right_expression
      in
      let message =
        Printf.sprintf "operands of `%s` have different types: `%s` and `%s`"
          (binary_operator_name operation)
          (diagnostic_ty_name left) (diagnostic_ty_name right)
      in
      let result_type =
        match operation with
        | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> Some Hir.Bool
        | _ -> Option.map (fun (_, _, ty) -> ty) widening
      in
      let widening_help =
        match (result_expected, result_type) with
        | None, _ -> binary_widening_help widening
        | Some expected, Some actual when Hir.ty_equal expected actual ->
            binary_widening_help widening
        | _ -> None
      in
      Diag.error ?help:widening_help span message |> fun diagnostic ->
      Error [ diagnostic ]
  | None -> (
      match operation with
      | Ast.Eq | Ast.Ne -> (
          match left with
          | Hir.Vec (lanes, (Hir.Bool | Hir.Int _ | Hir.Float _)) ->
              Ok (Hir.Vec (lanes, Hir.Bool))
          | Hir.Addr | Hir.Handle _ -> Ok Hir.Bool
          | _ when Sema_numeric.is_scalar left -> Ok Hir.Bool
          | _ ->
              error (result_span left_expression)
                (Printf.sprintf "operator `%s` does not accept `%s`"
                   (binary_operator_name operation)
                   (diagnostic_ty_name left)))
      | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
          if Sema_numeric.is_int left || Sema_numeric.is_float left || left = Hir.Addr
          then Ok Hir.Bool
          else
            match left with
            | Hir.Vec (lanes, (Hir.Int _ | Hir.Float _)) ->
                Ok (Hir.Vec (lanes, Hir.Bool))
            | _ ->
                error (result_span left_expression)
                  (Printf.sprintf
                     "ordered comparison `%s` needs an integer or integer vector, got \
                      `%s`"
                     (binary_operator_name operation)
                     (diagnostic_ty_name left)))
      | Ast.Bit_and | Ast.Bit_or | Ast.Bit_xor -> (
          if Sema_numeric.is_numeric left then Ok left
          else if left = Hir.Bool then Ok Hir.Bool
          else
            match left with
            | Hir.Vec (_, Hir.Bool) -> Ok left
            | _ ->
                error (result_span left_expression)
                  (Printf.sprintf
                     "operator `%s` needs integer or integer vector operands, got `%s`"
                     (binary_operator_name operation)
                     (diagnostic_ty_name left)))
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem ->
          if Sema_numeric.is_numeric left then Ok left
          else if Sema_numeric.is_float left then
            if operation = Ast.Rem then
              let left_name = Option.fold ~none:"x" ~some:Ast.expr_name left_expression
              and right_name =
                Option.fold ~none:"y" ~some:Ast.expr_name right_expression
              in
              let help =
                Printf.sprintf "use C `fmod(%s, %s)` from `<math.h>`" left_name
                  right_name
              in
              Error
                [
                  Diag.error ~help
                    (result_span right_expression)
                    "remainder `%` is not defined for floating point";
                ]
            else Ok left
          else
            error (result_span left_expression)
              (Printf.sprintf
                 "operator `%s` needs integer or integer vector operands, got `%s`"
                 (binary_operator_name operation)
                 (diagnostic_ty_name left))
      | Ast.And | Ast.Or ->
          if left = Hir.Bool && right = Hir.Bool then Ok Hir.Bool
          else
            let operation = if operation = Ast.And then "&&" else "||" in
            let side, expression, ty =
              if left <> Hir.Bool then ("left", left_expression, left)
              else ("right", right_expression, right)
            in
            let diagnostic =
              match expression with
              | Some expression -> logical_operand_error operation side expression ty
              | None ->
                  Diag.error span
                    (Printf.sprintf "%s operand of `%s` is `%s`, not `bool`" side
                       operation (Hir.ty_name ty))
            in
            Error [ diagnostic ]
      | Ast.Shl | Ast.Shr -> Ok left)

let variadic_promote expression =
  match Hir.expr_ty expression with
  | Hir.Bool | Hir.Int (Hir.U8 | Hir.U16) ->
      Hir.Cast (Ast.Zext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | Hir.Int (Hir.I8 | Hir.I16) ->
      Hir.Cast (Ast.Sext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | _ -> expression
