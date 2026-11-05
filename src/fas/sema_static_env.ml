open Sema_types

let rec symbolic_expression names = function
  | Ast.Addr_of _ | Ast.String_lit (true, _, _) -> true
  | Ast.Ident (name, _) -> List.mem name names
  | Ast.Designated_field _ | Ast.Designated_index _ -> true
  | Ast.Array_lit (items, _) -> List.exists (symbolic_expression names) items
  | Ast.Unary (_, x, _)
  | Ast.C_dereference (x, _, _)
  | Ast.C_dot_star (x, _)
  | Ast.Parenthesized (x, _)
  | Ast.Cast (_, _, x, _)
  | Ast.Handle_from_addr (_, x, _)
  | Ast.Field (x, _, _)
  | Ast.Arrow_field (x, _, _, _)
  | Ast.Splat (x, _) ->
      symbolic_expression names x
  | Ast.Binary (_, x, y, _) ->
      symbolic_expression names x || symbolic_expression names y
  | Ast.Call (Ast.Ident ("len", _), _, _) -> false
  | Ast.Call (x, xs, _) -> List.exists (symbolic_expression names) (x :: xs)
  | Ast.Select (x, args, _) | Ast.Generic_args (x, args, _) ->
      symbolic_expression names x
      || List.exists
           (function Ast.Const_arg x -> symbolic_expression names x | _ -> false)
           args
  | Ast.Ternary (x, y, z, _) -> List.exists (symbolic_expression names) [ x; y; z ]
  | _ -> false

let names ~named_types items =
  let rec address_type seen = function
    | Ast.Addr | Ast.Handle _ | Ast.Vec _ -> true
    | Ast.Array (_, element) -> address_type seen element
    | Ast.Named_type (name, _) when not (List.mem name seen) ->
        List.exists
          (function
            | Ast.Struct { name = actual; fields; is_union; _ } when name = actual ->
                is_union
                || List.exists
                     (fun (field : Ast.field) -> address_type (name :: seen) field.ty)
                     fields
            | _ -> false)
          items
    | _ -> false
  in
  let address_aggregate = function
    | (Ast.Array _ | Ast.Named_type _) as ty -> address_type [] ty
    | _ -> false
  in
  let rec resolve seen = function
    | Ast.Named_type (name, _) as ty when not (List.mem name seen) -> (
        match List.assoc_opt name named_types with
        | Some (Alias_name aliased) -> resolve (name :: seen) aliased
        | _ -> ty)
    | ty -> ty
  in
  let record ty =
    match resolve [] ty with
    | Ast.Applied_type _ -> true
    | Ast.Named_type (name, _) -> (
        match List.assoc_opt name named_types with
        | Some (Struct_name | C_record_name (_, None)) -> true
        | _ -> false)
    | _ -> false
  in
  let aggregate ty =
    match resolve [] ty with
    | Ast.Array (_, element) -> (
        match resolve [] element with Ast.Array _ -> true | element -> record element)
    | ty -> record ty
  in
  let rec collect known =
    let next =
      List.filter_map
        (function
          | Ast.Const { name; ty; value; _ }
            when address_aggregate ty || aggregate ty || symbolic_expression known value
            ->
              Some name
          | _ -> None)
        items
    in
    if next = known then known else collect next
  in
  collect []

let ordinary_program names (program : Ast.program) =
  {
    Ast.items =
      List.filter
        (function Ast.Const { name; _ } -> not (List.mem name names) | _ -> true)
        program.items;
  }

let storage_items names items =
  List.filter_map
    (function
      | Ast.Const { name; ty_span; ty; value; span; name_span; extent }
        when List.mem name names ->
          Some
            (Ast.Global
               {
                 name;
                 name_span;
                 ty_span;
                 ty;
                 init = Some value;
                 span;
                 linkage = Ast.Internal_global;
                 extent;
               })
      | Ast.Global _ as item -> Some item
      | _ -> None)
    items

let declarations ~source_obj names items =
  Result_list.map
    (function
      | Ast.Global { name; ty_span; ty; linkage; _ } ->
          Result.map
            (fun ty ->
              (name, ty, if List.mem name names then Ast.Import_const_c else linkage))
            (source_obj ty_span ty)
      | _ ->
          Error
            [ Diag.error Span.synthetic "internal error: invalid static declaration" ])
    (storage_items names items)

let address_value c ty expression =
  let error span message = Error [ Diag.error span message ] in
  let ( let* ) x f = match x with Ok v -> f v | Error _ as e -> e in
  let c_string_error span =
    let help =
      match expression with
      | Ast.String_lit (false, text, _)
        when String.for_all
               (fun character ->
                 let code = Char.code character in
                 code >= 32 && code <= 126 && character <> '"' && character <> '\\')
               text ->
          Some (Printf.sprintf "write `c\"%s\"`" text)
      | _ -> None
    in
    Error [ Diag.error ?help span "address constants require a C string literal" ]
  in
  let rec address_constant_read = function
    | Ast.Ident (name, span) -> (
        match
          List.find_map
            (function
              | Ast.Const { name = constant; value; _ } when constant = name ->
                  Some value
              | _ -> None)
            c.Sema_context.generic_structs
        with
        | Some value -> Some (name, span, value)
        | None -> None)
    | Ast.Handle_from_addr (_, value, _) -> address_constant_read value
    | _ -> None
  in
  let address_constant_error (name, span, value) =
    let rec plain = function
      | Ast.Ident _ -> true
      | Ast.Field (base, _, _) | Ast.Select (base, [ Ast.Const_arg _ ], _) -> plain base
      | _ -> false
    in
    let help =
      match value with
      | Ast.Addr_of (target, _) when plain target ->
          Some (Printf.sprintf "write `%s`" (Ast.expr_name value))
      | _ -> None
    in
    Error
      [
        Diag.error ?help span
          (Printf.sprintf
             "`%s` is an address constant; another constant initializer cannot read it"
             name);
      ]
  in
  let rec place = function
    | Ast.Ident (name, span) -> (
        match Sema_context.lookup_global name c.Sema_context.globals with
        | Some (_, ty, _) -> Ok (name, ty, 0)
        | None -> (
            match List.find_opt (fun (entry, _, _) -> entry = name) c.arrays with
            | Some (_, (Hir.Array _ as ty), _) -> Ok (name, ty, 0)
            | _ ->
                if List.mem name c.external_c_functions then Ok (name, Hir.Addr, 0)
                else
                  error span
                    "address initializer requires static storage and constant selectors"
            ))
    | Ast.Field (base, field, span) -> (
        let* name, ty, previous = place base in
        match ty with
        | Hir.Struct structure -> (
            match Sema_types.field_info c.structs structure field with
            | Some { unsupported_reason = Some reason; _ } ->
                error (Ast.expr_span base) reason
            | Some field -> Ok (name, field.ty, previous + field.offset)
            | None -> error span (Sema_types.missing_field_message field ty))
        | _ ->
            error span
              "address initializer requires static storage and constant selectors")
    | Ast.Arrow_field (_, _, _, field_span) ->
        error field_span
          "address initializer requires static storage and constant selectors"
    | Ast.Select (base, [ argument ], span) -> (
        let* name, ty, previous = place base in
        let* index = Sema_check.select_value_arg span argument in
        let* index_ty, index =
          Sema_constants.const_expr
            ~array_lengths:
              (Sema_context.static_array_lengths c.top_level_bindings c.globals)
            ~structs:c.structs ~named_types:c.named_types
            ~generic_structs:c.generic_structs ~arrays:c.arrays c.consts None index
        in
        match ty with
        | Hir.Array (length, element) when Sema_numeric.is_int index_ty ->
            if Int64.unsigned_compare index (Int64.of_int length) >= 0 then
              error span "array index is out of bounds"
            else
              let* size, _ =
                Hir.layout c.structs element
                |> Result.map_error (fun message -> [ Diag.error span message ])
              in
              Ok (name, element, previous + (Int64.to_int index * size))
        | _ ->
            error span
              "address initializer requires static storage and constant selectors")
    | value ->
        error (Ast.expr_span value)
          "address initializer requires static storage and constant selectors"
  in
  let rec address = function
    | Hir.Null _ -> Ok Hir.Global_null
    | Hir.EString (id, _, span) ->
        let rec c_literal = function
          | Ast.String_lit (true, _, _) -> true
          | Ast.Handle_from_addr (_, value, _) | Ast.Call (_, [ value ], _) ->
              c_literal value
          | _ -> false
        in
        if c_literal expression then
          Ok (Hir.Global_address (".str." ^ string_of_int id, 0))
        else c_string_error span
    | Hir.Address _ ->
        let rec target = function
          | Ast.Addr_of (base, _) -> place base
          | Ast.Handle_from_addr (_, value, _) -> target value
          | Ast.Call (_, [ value ], _) -> target value
          | value -> (
              match address_constant_read value with
              | Some read -> address_constant_error read
              | None ->
                  error (Ast.expr_span value)
                    "address constants can only be stored in `addr` or `handle[T]` \
                     slots")
        in
        let* name, _, offset = target expression in
        Ok (Hir.Global_address (name, offset))
    | Hir.Function_address (name, _) -> Ok (Hir.Global_address (name, 0))
    | Hir.Call (Hir.Builtin (Hir.Handle_from_addr _), [ value ], _, _) -> address value
    | value -> (
        match expression with
        | Ast.Binary ((Ast.Add | Ast.Sub), (Ast.Addr_of (target, _) as address), _, _)
          ->
            let help =
              match place target with
              | Ok (_, Hir.Array _, _) ->
                  Some (Printf.sprintf "write `&%s[k]`" (Ast.expr_name target))
              | Ok (_, Hir.Struct name, _) -> (
                  match
                    List.find_opt
                      (fun (definition : Hir.struct_def) -> definition.name = name)
                      c.structs
                  with
                  | Some { fields = [ field ]; _ } ->
                      Some
                        (Printf.sprintf "write `&%s.%s`" (Ast.expr_name target)
                           field.name)
                  | Some _ | None -> None)
              | _ -> None
            in
            let message =
              Printf.sprintf "address constant `%s` cannot be used in arithmetic"
                (Ast.expr_name address)
            in
            Error [ Diag.error ?help (Ast.expr_span address) message ]
        | Ast.Select (_, [ Ast.Const_arg _ ], _) ->
            error (Ast.expr_span expression)
              (Printf.sprintf
                 "`%s` is not an address constant; write `&G`, `&G.field`, `&G[k]`, \
                  `c\"...\"` or `null`"
                 (Ast.expr_name expression))
        | _ -> (
            match address_constant_read expression with
            | Some read -> address_constant_error read
            | None ->
                error (Hir.expr_span value)
                  "address constants can only be stored in `addr` or `handle[T]` slots")
        )
  in
  let* () =
    match expression with
    | Ast.String_lit (false, _, span) -> c_string_error span
    | _ -> Ok ()
  in
  let record_handle_address =
    match (ty, expression) with
    | Hir.Handle record, Ast.Addr_of (target, _) -> (
        match place target with
        | Ok (_, Hir.Struct actual, _) ->
            actual = record
            && List.exists
                 (function
                   | _, Sema_types.C_record_name (imported, _) -> imported = record
                   | _ -> false)
                 c.named_types
        | _ -> false)
    | _ -> false
  in
  let* checked =
    Sema_check.check_expr c
      (Some (if record_handle_address then Hir.Addr else ty))
      expression
  in
  if not (record_handle_address || Hir.ty_equal ty (Hir.expr_ty checked)) then
    error (Ast.expr_span expression)
      (constant_initializer_type_message (Hir.expr_ty checked) ty)
  else address checked

let source_array_lengths items =
  List.filter_map
    (function
      | Ast.Const { name; ty = Ast.Array (length, _); _ } -> Some (name, length)
      | _ -> None)
    items
