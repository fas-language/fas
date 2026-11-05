open Sema_constants
open Sema_types

let error span message = Error [ Diag.error span message ]

let ( let* ) result next =
  match result with Ok value -> next value | Error _ as e -> e

let collect ~array_lengths ~source_obj ~structs ~named_types ~generic_structs ~consts
    ~arrays ~global_names ~address_value ~readonly_names items =
  let rec value span ty expression =
    let array_items, struct_items =
      match expression with
      | Ast.Array_lit (items, _) -> (Some items, Some items)
      | _ -> (None, None)
    in
    let map wrap ty items =
      let* items = Result_list.map (value span ty) items in
      Ok (wrap items)
    in
    let designator =
      match expression with
      | Ast.Array_lit (items, _) ->
          List.find_opt
            (function
              | Ast.Designated_field _ | Ast.Designated_index _ -> true | _ -> false)
            items
      | _ -> None
    in
    let* () =
      match (ty, designator) with
      | Hir.Struct _, _ -> Ok ()
      | _, Some entry -> Error [ Sema_types.invalid_designator_error ty entry ]
      | _ -> Ok ()
    in
    match ty with
    | (Hir.Addr | Hir.Handle _) when expression <> Ast.Null (Ast.expr_span expression)
      ->
        address_value ty expression
    | Hir.Bool | Hir.Int _ | Hir.Float _ | Hir.Addr | Hir.Handle _ ->
        let* actual, bits =
          const_expr ~array_lengths ~structs ~named_types ~generic_structs ~arrays
            ~globals:global_names consts (Some ty) expression
        in
        if not (Hir.ty_equal actual ty) then
          error (Ast.expr_span expression) (constant_initializer_type_message actual ty)
        else
          Ok
            (match ty with
            | Hir.Bool -> Hir.Global_bool (bits <> 0L)
            | Hir.Float _ -> Hir.Global_float bits
            | Hir.Addr | Hir.Handle _ -> Hir.Global_null
            | _ -> Hir.Global_int bits)
    | Hir.Vec _ ->
        let* actual, values =
          vector_const_expr ~array_lengths ~structs ~named_types ~generic_structs
            ~arrays ~globals:global_names consts (Some ty) expression
        in
        if not (Hir.ty_equal actual ty) then
          error (Ast.expr_span expression) (constant_initializer_type_message actual ty)
        else Ok (Hir.Global_vector values)
    | Hir.Array (length, element) -> (
        match array_items with
        | Some [] -> Ok (Hir.Global_zero (Hir.zero_initializer ty))
        | Some items when List.length items = length ->
            map (fun xs -> Hir.Global_array xs) element items
        | Some items ->
            error
              (Sema_types.aggregate_count_error_span (Ast.expr_span expression) length
                 items)
              (Sema_types.array_element_count_message length (List.length items))
        | None -> global_error span "global initializer must be a constant expression")
    | Hir.Struct name -> (
        let items = match struct_items with Some items -> Some items | _ -> None in
        match
          ( items,
            List.find_opt
              (fun (definition : Hir.struct_def) -> definition.name = name)
              structs )
        with
        | Some [], Some _ -> Ok (Hir.Global_zero (Hir.zero_initializer ty))
        | Some [ item ], Some { Hir.is_union = true; fields = field :: _; _ }
          when not
                 (match item with
                 | Ast.Designated_field _ | Ast.Designated_index _ -> true
                 | _ -> false) -> (
            match field.unsupported_reason with
            | Some reason -> error (Ast.expr_span item) reason
            | None ->
                let* value = value span field.ty item in
                Ok (Hir.Global_struct [ value ]))
        | Some items, Some definition -> (
            let* designated =
              Sema_types.designated_record_entries name definition.is_union
                definition.fields items (Ast.expr_span expression)
            in
            match designated with
            | Some fields -> (
                match
                  List.find_opt
                    (fun (field : Hir.field) -> Option.is_some field.unsupported_reason)
                    definition.fields
                with
                | Some { unsupported_reason = Some reason; _ } -> error span reason
                | _ ->
                    let* values =
                      Result_list.map
                        (fun ((field : Hir.field), item) ->
                          match item with
                          | Some item -> value span field.ty item
                          | None -> Ok (Hir.Global_zero (Hir.zero_initializer field.ty)))
                        fields
                    in
                    Ok (Hir.Global_struct values))
            | None when definition.is_union -> (
                match items with
                | [ item ] when List.length definition.fields = 1 -> (
                    let field = List.hd definition.fields in
                    match field.unsupported_reason with
                    | Some reason -> error (Ast.expr_span item) reason
                    | None ->
                        let* value = value span field.ty item in
                        Ok (Hir.Global_struct [ value ]))
                | _ ->
                    let expected = min 1 (List.length definition.fields) in
                    error
                      (Sema_types.aggregate_count_error_span (Ast.expr_span expression)
                         expected items)
                      (Sema_types.record_field_count_message name expected
                         (List.length items)))
            | None when List.length items = List.length definition.fields -> (
                match
                  List.find_opt
                    (fun (field : Hir.field) -> Option.is_some field.unsupported_reason)
                    definition.fields
                with
                | Some { unsupported_reason = Some reason; _ } -> error span reason
                | _ ->
                    let* values =
                      Result_list.map
                        (fun ((field : Hir.field), item) -> value span field.ty item)
                        (List.combine definition.fields items)
                    in
                    Ok (Hir.Global_struct values))
            | None ->
                let expected = List.length definition.fields in
                error
                  (Sema_types.aggregate_count_error_span (Ast.expr_span expression)
                     expected items)
                  (Sema_types.record_field_count_message name expected
                     (List.length items)))
        | _, None -> error span (Printf.sprintf "unknown struct `%s`" name)
        | _ -> global_error span "global initializer must be a constant expression")
    | Hir.Opaque _ | Hir.Void ->
        error span "global type does not have a supported object initializer"
  in
  let evaluate name span ty expression =
    match value span ty expression with
    | Error [ { Diag.issue = Diag.Not_constant; _ } ] ->
        error (Ast.expr_span expression)
          (Printf.sprintf
             "global initializer must be a constant expression for `%s`; `%s` is not \
              constant"
             name (Ast.expr_name expression))
    | result -> result
  in
  let rec globals acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Global { name; ty_span; ty; init; linkage; _ } :: rest ->
        let* ty = source_obj ty_span ty in
        let* init_value =
          match init with
          | None -> Ok None
          | Some expression ->
              let* value = evaluate name (Ast.expr_span expression) ty expression in
              Ok (Some value)
        in
        globals
          ({
             Hir.name;
             ty;
             init_value;
             linkage;
             readonly = List.mem name readonly_names;
           }
          :: acc)
          rest
    | _ :: rest -> globals acc rest
  in
  globals [] items
