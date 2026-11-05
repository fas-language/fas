open Sema_constants
open Sema_specialization
open Sema_types

let simd_memory_names =
  [
    "masked_load"; "masked_store"; "gather"; "scatter"; "gather_bytes"; "scatter_bytes";
  ]

open Sema_context
module String_set = Set.Make (String)

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let resolve_statements resolve statements =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | statement :: rest -> (
        let* resolved = resolve statement in
        match (statement, resolved) with
        | Ast.If _, Ast.Block (body, _) when Ast.block_terminates body ->
            Ok (List.rev (resolved :: acc))
        | _ -> go (resolved :: acc) rest)
  in
  go [] statements

let view_source_type = function
  | Some (Ast.Handle _ as ty) -> Some ty
  | source_type -> source_type

let monomorphize_types ~check_expr ~check_stmt ~check_target ~target_ty
    ~generic_const_argument ?eval_context ?(eval_globals = []) ?(array_lengths = [])
    ?(c_aliases = []) ?(c_unsupported = []) ?(c_nonnull_parameters = [])
    ?(c_string_parameters = []) ?(c_record_calls = []) ?(c_alloc_size_parameters = [])
    ?(eager_functions = false) ?recovered ~top_level_bindings ~limits ~type_node_account
    specializations program =
  let eval_structs, eval_named_types, eval_consts, eval_arrays =
    match eval_context with
    | None -> ([], [], [], [])
    | Some (structs, named_types, consts, arrays) ->
        (structs, named_types, consts, arrays)
  in
  let expanded_nodes = ref (Ast.count_expanded_nodes program) in
  let generic_structs =
    List.filter (function Ast.Struct _ -> true | _ -> false) program.Ast.items
  in
  let preserve_layout_queries = ref false in
  let with_preserved_layout_queries f =
    let previous = !preserve_layout_queries in
    preserve_layout_queries := true;
    let result = f () in
    preserve_layout_queries := previous;
    result
  in
  let struct_templates =
    List.filter_map
      (function
        | Ast.Struct ({ name; generic_params = _ :: _; _ } as template) ->
            Some (name, Ast.Struct template)
        | _ -> None)
      program.Ast.items
  in
  let function_templates =
    List.filter_map
      (function
        | Ast.Func ({ name; generic_params = _ :: _; _ } as template) ->
            Some (name, Ast.Func template)
        | _ -> None)
      program.Ast.items
  in
  let struct_names =
    List.filter_map
      (function Ast.Struct { name; _ } -> Some name | _ -> None)
      program.Ast.items
    |> String_set.of_list
  in
  let function_names =
    List.filter_map
      (function Ast.Func { name; _ } -> Some name | _ -> None)
      program.Ast.items
    |> String_set.of_list
  in
  let global_names, global_value_names =
    List.fold_left
      (fun (globals, values) -> function
        | Ast.Const { name; _ } -> (globals, name :: values)
        | Ast.Global { name; _ } -> (name :: globals, name :: values)
        | _ -> (globals, values))
      ([], []) program.Ast.items
  in
  let global_value_names = String_set.of_list global_value_names in
  let named_type_names =
    List.filter_map
      (function
        | Ast.Opaque { name; _ } | Ast.Struct { name; _ } -> Some name | _ -> None)
      program.Ast.items
    @ List.map fst c_aliases @ List.map fst c_unsupported
    |> String_set.of_list
  in
  let validation_signatures =
    List.filter_map
      (function
        | Ast.Func { name; generic_params = []; params; ret; variadic; span; _ } -> (
            let result =
              let* params =
                Result_list.map
                  (fun (parameter : Ast.param) ->
                    let* ty =
                      source_ty_with_values ~globals:global_names eval_named_types
                        eval_consts parameter.span parameter.ty
                    in
                    Ok (parameter.name, ty))
                  params
              in
              let* ret =
                source_ty_with_values ~globals:global_names eval_named_types eval_consts
                  span ret
              in
              Ok (name, { params; ret; variadic })
            in
            match result with Ok signature -> Some signature | Error _ -> None)
        | _ -> None)
      program.Ast.items
  in
  let generic_type_names = ref [] in
  let local_values = ref String_set.empty in
  let shadowed_constants = ref String_set.empty in
  let inference_types = ref [] in
  let inference_raw_view_types = ref [] in
  let set_inference_raw_view_type name ty =
    inference_raw_view_types :=
      (name, ty) :: List.remove_assoc name !inference_raw_view_types
  in
  let set_inference_type name ty =
    inference_types := (name, ty) :: List.remove_assoc name !inference_types;
    inference_raw_view_types := List.remove_assoc name !inference_raw_view_types
  in
  let set_inference_params params =
    inference_types := [];
    inference_raw_view_types := [];
    List.iter
      (fun (parameter : Ast.param) ->
        set_inference_type parameter.name (Some parameter.ty))
      (List.rev params)
  in
  let with_local_values names f =
    let previous = !local_values in
    let previous_shadowed_constants = !shadowed_constants in
    let previous_inference_types = !inference_types in
    let previous_inference_raw_view_types = !inference_raw_view_types in
    local_values := names;
    shadowed_constants := String_set.inter names previous_shadowed_constants;
    inference_types :=
      List.filter (fun (name, _) -> String_set.mem name names) !inference_types;
    inference_raw_view_types :=
      List.filter (fun (name, _) -> String_set.mem name names) !inference_raw_view_types;
    let result = f () in
    local_values := previous;
    shadowed_constants := previous_shadowed_constants;
    inference_types := previous_inference_types;
    inference_raw_view_types := previous_inference_raw_view_types;
    result
  in
  let ambiguous_name = function
    | Ast.Named_type (name, _) | Ast.Applied_type (name, _, _, _) -> Some name
    | _ -> None
  in
  let is_shadowed_argument = function
    | Ast.Type_or_index ty -> (
        match ambiguous_name ty with
        | Some name -> String_set.mem name !local_values
        | None -> false)
    | Ast.Name_arg (name, _) -> String_set.mem name !local_values
    | _ -> false
  in
  let bind_argument = function
    | Ast.Type_or_index (Ast.Applied_type (name, _, _, _) as ty)
      when String_set.mem name !local_values
           || not (List.mem_assoc name struct_templates) ->
        Ast.Const_arg (Ast.index_expression ty)
    | Ast.Type_or_index ty -> Ast.Type_arg ty
    | argument -> argument
  in
  let type_param_names generic_params =
    List.filter_map
      (function Ast.Type_param { name; _ } -> Some name | Ast.Const_param _ -> None)
      generic_params
  in
  let with_generic_type_names names f =
    let previous = !generic_type_names in
    generic_type_names := names;
    let result = f () in
    generic_type_names := previous;
    result
  in
  let generated = ref [] in
  let current_trace = ref [] in
  let resolved_type_key values span ty =
    match
      source_ty_with_values ~globals:global_names eval_named_types
        (values @ eval_consts) span ty
    with
    | Ok ty -> Hir.ty_name ty
    | Error _ -> specialization_type_key ty
  in
  let nearest_kind value_names type_names name =
    if String_set.mem name value_names then Some (`Value None)
    else if String_set.mem name type_names then Some (`Type None)
    else
      match lookup_top_level name top_level_bindings with
      | Some { declaration_id; declaration_kind = Top_type; _ } ->
          Some (`Type (Some declaration_id))
      | Some { declaration_id; declaration_kind = Top_function; _ } ->
          Some (`Function declaration_id)
      | Some { declaration_id; declaration_kind = Top_const; _ } ->
          Some (`Value (Some declaration_id))
      | Some { declaration_id; declaration_kind = Top_global; _ } ->
          Some (`Value (Some declaration_id))
      | None when String_set.mem name named_type_names -> Some (`Type None)
      | None when String_set.mem name function_names -> Some (`Function (-1))
      | None when String_set.mem name global_value_names -> Some (`Value None)
      | None -> None
  in
  let rec type_mentions names = function
    | Ast.Array (length, ty) | Ast.Vec (length, ty) ->
        expression_mentions names length.Ast.expression || type_mentions names ty
    | Ast.Handle ty -> type_mentions names ty
    | Ast.Applied_type (_, _, arguments, _) ->
        List.exists (generic_argument_mentions names) arguments
    | Ast.Named_type (name, _) -> List.mem name names
    | Ast.Bool | Ast.Void | Ast.Int _ | Ast.Float _ | Ast.Addr -> false
  and generic_argument_mentions names = function
    | Ast.Type_arg ty | Ast.Type_or_index ty -> type_mentions names ty
    | Ast.Const_arg expression -> expression_mentions names expression
    | Ast.Name_arg (name, _) -> List.mem name names
  and expression_mentions names = function
    | Ast.Ident (name, _) -> List.mem name names
    | Ast.Unary (_, expression, _)
    | Ast.C_dereference (expression, _, _)
    | Ast.C_dot_star (expression, _)
    | Ast.Parenthesized (expression, _)
    | Ast.Addr_of (expression, _)
    | Ast.Splat (expression, _) ->
        expression_mentions names expression
    | Ast.Binary (_, left, right, _) ->
        expression_mentions names left || expression_mentions names right
    | Ast.Select (base, args, _) ->
        expression_mentions names base
        || List.exists (generic_argument_mentions names) args
    | Ast.Call (callee, arguments, _) ->
        expression_mentions names callee
        || List.exists (expression_mentions names) arguments
    | Ast.Generic_args (callee, arguments, _) ->
        expression_mentions names callee
        || List.exists (generic_argument_mentions names) arguments
    | Ast.Cast (_, ty, expression, _) ->
        type_mentions names ty || expression_mentions names expression
    | Ast.Handle_from_addr (ty, expression, _) ->
        type_mentions names ty || expression_mentions names expression
    | Ast.Sizeof_value (expression, _) -> expression_mentions names expression
    | Ast.Sizeof (ty, _) | Ast.Alignof (ty, _) | Ast.Offsetof (ty, _, _, _) ->
        type_mentions names ty
    | Ast.Field (expression, _, _) | Ast.Arrow_field (expression, _, _, _) ->
        expression_mentions names expression
    | Ast.Ternary (condition, yes, no, _) ->
        expression_mentions names condition
        || expression_mentions names yes || expression_mentions names no
    | Ast.Array_lit (elements, _) -> List.exists (expression_mentions names) elements
    | Ast.Designated_field (_, _, value, _) -> expression_mentions names value
    | Ast.Designated_index (index, value, _) ->
        expression_mentions names index || expression_mentions names value
    | Ast.Int_lit _ | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _
      ->
        false
  in
  let shadowed_constant_error expression =
    match
      String_set.elements !shadowed_constants
      |> List.find_opt (fun name -> expression_mentions [ name ] expression)
    with
    | Some name ->
        error (Ast.expr_span expression)
          (Printf.sprintf "`%s` is not a compile-time constant" name)
    | None -> Ok ()
  in
  let rec validate_type_names value_names type_names span = function
    | Ast.Handle ty -> validate_type_names value_names type_names span ty
    | Ast.Addr -> Ok ()
    | Ast.Array (length, ty) ->
        validate_aggregate_length_names value_names type_names span "array" length ty
    | Ast.Vec (length, ty) ->
        validate_aggregate_length_names value_names type_names span "vector" length ty
    | Ast.Named_type (name, type_span)
      when Option.is_some (Names.type_misuse_message name) ->
        error type_span (Option.get (Names.type_misuse_message name))
    | Ast.Named_type ("void", _) -> Ok ()
    | Ast.Named_type (name, type_span) when String_set.mem name !local_values ->
        error type_span (Printf.sprintf "`%s` is a value, not a type" name)
    | Ast.Named_type (name, type_span) -> (
        match nearest_kind value_names type_names name with
        | Some (`Type _) -> Ok ()
        | Some (`Value _) ->
            error type_span (Printf.sprintf "`%s` is a value, not a type" name)
        | Some (`Function _) ->
            error type_span (Printf.sprintf "`%s` is a function, not a type" name)
        | None ->
            Error
              [
                unknown_type_error
                  (String_set.elements type_names @ !generic_type_names)
                  type_span name;
              ])
    | Ast.Applied_type (name, _, arguments, application_span) ->
        if String_set.mem name value_names then
          error span (Printf.sprintf "`%s` is a value, not a type" name)
        else if not (String_set.mem name struct_names) then
          error span (Printf.sprintf "unknown generic struct `%s`" name)
        else
          Result_list.iter
            (validate_generic_argument_names value_names type_names application_span)
            arguments
    | Ast.Bool | Ast.Void | Ast.Int _ | Ast.Float _ -> Ok ()
  and validate_aggregate_length_names value_names type_names span _kind length ty =
    let* () = validate_expression_names value_names type_names length.Ast.expression in
    validate_type_names value_names type_names span ty
  and validate_generic_argument_names value_names type_names fallback_span = function
    | Ast.Type_or_index (Ast.Applied_type (name, _, _, _) as ty) -> (
        match nearest_kind value_names type_names name with
        | Some (`Type _) -> validate_type_names value_names type_names fallback_span ty
        | _ ->
            validate_expression_names value_names type_names (Ast.index_expression ty))
    | Ast.Type_arg ty | Ast.Type_or_index ty ->
        validate_type_names value_names type_names fallback_span ty
    | Ast.Const_arg expression ->
        validate_expression_names value_names type_names expression
    | Ast.Name_arg (name, span) ->
        if
          String_set.mem name value_names
          || String_set.mem name global_value_names
          || String_set.mem name named_type_names
          || String_set.mem name type_names
        then Ok ()
        else error span (Printf.sprintf "unknown name `%s`" name)
  and validate_selection_argument_names value_names type_names span = function
    | Ast.Name_arg ("..", _) -> Ok ()
    | Ast.Type_arg (Ast.Named_type ("void", _)) -> Ok ()
    | Ast.Type_or_index ty -> (
        match ambiguous_name ty with
        | Some name -> (
            match nearest_kind value_names type_names name with
            | Some (`Value _) ->
                validate_expression_names value_names type_names
                  (Ast.index_expression ty)
            | _ -> validate_type_names value_names type_names span ty)
        | None -> validate_type_names value_names type_names span ty)
    | argument -> validate_generic_argument_names value_names type_names span argument
  and validate_expression_names value_names type_names = function
    | Ast.Ident (name, span) -> (
        match nearest_kind value_names type_names name with
        | Some (`Value _) -> Ok ()
        | Some (`Type _) ->
            error span (Printf.sprintf "`%s` is a type, not a value" name)
        | Some (`Function _) ->
            error span (Printf.sprintf "`%s` is a function, not a value" name)
        | None -> (
            match Names.value_misuse_message name with
            | Some message -> error span message
            | None -> error span (Printf.sprintf "unknown name `%s`" name)))
    | Ast.Unary (_, expression, _)
    | Ast.C_dereference (expression, _, _)
    | Ast.C_dot_star (expression, _)
    | Ast.Parenthesized (expression, _)
    | Ast.Addr_of (expression, _)
    | Ast.Splat (expression, _) ->
        validate_expression_names value_names type_names expression
    | Ast.Binary (_, left, right, _) ->
        let* () = validate_expression_names value_names type_names left in
        validate_expression_names value_names type_names right
    | Ast.Select (base, args, span) ->
        let* () = validate_expression_names value_names type_names base in
        Result_list.iter
          (validate_selection_argument_names value_names type_names span)
          args
    | Ast.Call (Ast.Ident (name, span), arguments, _) ->
        let* () =
          if Names.reserved_binding_name name then Ok ()
          else
            match nearest_kind value_names type_names name with
            | Some (`Function _) -> Ok ()
            | Some (`Value _) ->
                error span (Printf.sprintf "`%s` is a value, not a function" name)
            | Some (`Type _) ->
                error span (Printf.sprintf "`%s` is a type, not a function" name)
            | None -> error span (Printf.sprintf "unknown function `%s`" name)
        in
        Result_list.iter (validate_expression_names value_names type_names) arguments
    | Ast.Call (callee, arguments, _) ->
        let* () = validate_expression_names value_names type_names callee in
        Result_list.iter (validate_expression_names value_names type_names) arguments
    | Ast.Generic_args (Ast.Ident (name, _), arguments, span)
      when name = "volatile_load" || name = "volatile_store" || name = "call_addr"
           || name = "cvt"
           || List.mem name simd_memory_names ->
        Result_list.iter
          (validate_generic_argument_names value_names type_names span)
          arguments
    | Ast.Generic_args (Ast.Ident (name, name_span), _, _)
      when Option.is_some (Names.takes_no_type_arguments_message name) ->
        error name_span (Option.get (Names.takes_no_type_arguments_message name))
    | Ast.Generic_args (Ast.Ident (name, span), arguments, application_span) ->
        let* () =
          match nearest_kind value_names type_names name with
          | Some (`Function _) -> Ok ()
          | Some (`Value _) ->
              error span (Printf.sprintf "`%s` is a value, not a function" name)
          | Some (`Type _) ->
              error span (Printf.sprintf "`%s` is a type, not a function" name)
          | None -> error span (Printf.sprintf "unknown generic function `%s`" name)
        in
        let* () =
          match List.assoc_opt name function_templates with
          | None -> error span (Printf.sprintf "function `%s` is not generic" name)
          | Some (Ast.Func { generic_params; _ }) ->
              if List.length arguments <> List.length generic_params then
                error application_span
                  (generic_arity_message "generic function" name
                     (List.length generic_params) (List.length arguments))
              else
                Result_list.iter
                  (fun (parameter, argument) ->
                    match (parameter, argument) with
                    | Ast.Type_param _, (Ast.Type_arg _ | Ast.Type_or_index _) -> Ok ()
                    | Ast.Type_param _, Ast.Name_arg (argument_name, argument_span) -> (
                        match nearest_kind value_names type_names argument_name with
                        | Some (`Type _) -> Ok ()
                        | Some _ -> error argument_span "expected a type argument"
                        | None -> Ok ())
                    | Ast.Const_param _, Ast.Const_arg _ -> Ok ()
                    | Ast.Const_param _, Ast.Name_arg (argument_name, argument_span)
                      -> (
                        match nearest_kind value_names type_names argument_name with
                        | Some (`Value _) -> Ok ()
                        | Some _ -> error argument_span "expected a const argument"
                        | None -> Ok ())
                    | ( Ast.Const_param _,
                        (Ast.Type_arg (Ast.Applied_type _) | Ast.Type_or_index _) ) ->
                        Ok ()
                    | Ast.Type_param _, _ ->
                        error application_span "expected a type argument"
                    | Ast.Const_param _, _ ->
                        error application_span "expected a const argument")
                  (List.combine generic_params arguments)
          | Some _ -> error span (Printf.sprintf "`%s` is not a function" name)
        in
        Result_list.iter
          (validate_generic_argument_names value_names type_names application_span)
          arguments
    | Ast.Generic_args (callee, arguments, application_span) ->
        let* () = validate_expression_names value_names type_names callee in
        Result_list.iter
          (validate_generic_argument_names value_names type_names application_span)
          arguments
    | Ast.Cast (_, ty, expression, span) ->
        let* () = validate_type_names value_names type_names span ty in
        validate_expression_names value_names type_names expression
    | Ast.Handle_from_addr (ty, expression, span) ->
        let* () = validate_type_names value_names type_names span ty in
        validate_expression_names value_names type_names expression
    | Ast.Sizeof_value (expression, _) ->
        validate_expression_names value_names type_names expression
    | Ast.Sizeof (ty, span) | Ast.Alignof (ty, span) | Ast.Offsetof (ty, _, _, span) ->
        validate_type_names value_names type_names span ty
    | Ast.Field (expression, _, _) | Ast.Arrow_field (expression, _, _, _) ->
        validate_expression_names value_names type_names expression
    | Ast.Ternary (condition, yes, no, _) ->
        let* () = validate_expression_names value_names type_names condition in
        let* () = validate_expression_names value_names type_names yes in
        validate_expression_names value_names type_names no
    | Ast.Array_lit (elements, _) ->
        Result_list.iter (validate_expression_names value_names type_names) elements
    | Ast.Designated_field (_, _, value, _) ->
        validate_expression_names value_names type_names value
    | Ast.Designated_index (index, value, _) ->
        let* () = validate_expression_names value_names type_names index in
        validate_expression_names value_names type_names value
    | Ast.Int_lit _ | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _
      ->
        Ok ()
  in
  let rec validate_target_names value_names type_names = function
    | Ast.Target_ident (name, span) -> (
        match nearest_kind value_names type_names name with
        | Some (`Value _)
          when String_set.mem name value_names || List.mem name global_names ->
            Ok ()
        | Some (`Value _) ->
            error span (Printf.sprintf "constant `%s` is not assignable" name)
        | Some (`Type _) ->
            error span (Printf.sprintf "type `%s` is not assignable" name)
        | Some (`Function _) ->
            error span (Printf.sprintf "function `%s` is not assignable" name)
        | None -> error span (Printf.sprintf "unknown assignment target `%s`" name))
    | Ast.Target_select (base, args) ->
        let* () = validate_expression_names value_names type_names base in
        Result_list.iter
          (validate_generic_argument_names value_names type_names (Ast.expr_span base))
          args
    | Ast.Target_field (base, _, _) ->
        validate_expression_names value_names type_names base
  and validate_statement_names value_names type_names scope_names = function
    | Ast.Let { name; ty; ty_span; init; span } ->
        if String_set.mem name scope_names then
          error span (Printf.sprintf "duplicate local `%s`" name)
        else
          let* () =
            Diag.local_type_error ~span ~ty_span name
              (validate_type_names value_names type_names span ty)
            |> Sema_types.declaration_type_error ~declaration:"locals" ~variable:name ty
          in
          let* () = validate_binding_name span name in
          let* () =
            match init with
            | None -> Ok ()
            | Some expression ->
                validate_expression_names value_names type_names expression
          in
          Ok (String_set.add name value_names, String_set.add name scope_names)
    | Ast.View { name; place; span; _ } ->
        let* () = validate_binding_name span name in
        if String_set.mem name scope_names then
          error span (Printf.sprintf "duplicate local `%s`" name)
        else
          let* () = validate_expression_names value_names type_names place in
          Ok (String_set.add name value_names, String_set.add name scope_names)
    | Ast.Assign (target, expression, _)
    | Ast.Compound_assign (target, _, expression, _, _) ->
        let* () = validate_target_names value_names type_names target in
        let* () = validate_expression_names value_names type_names expression in
        Ok (value_names, scope_names)
    | Ast.Return (expression, _) ->
        let* () =
          match expression with
          | None -> Ok ()
          | Some expression ->
              validate_expression_names value_names type_names expression
        in
        Ok (value_names, scope_names)
    | Ast.If (condition, yes, no, _) ->
        let* () = validate_expression_names value_names type_names condition in
        let* () = validate_statement_block_names value_names type_names yes in
        let* () =
          match no with
          | None -> Ok ()
          | Some statements ->
              validate_statement_block_names value_names type_names statements
        in
        Ok (value_names, scope_names)
    | Ast.While (_, condition, body, _) ->
        let* () = validate_expression_names value_names type_names condition in
        let* () = validate_statement_block_names value_names type_names body in
        Ok (value_names, scope_names)
    | Ast.Defer (body, _) | Ast.Block (body, _) ->
        let* () = validate_statement_block_names value_names type_names body in
        Ok (value_names, scope_names)
    | Ast.Expr_stmt (expression, _, _) ->
        let* () = validate_expression_names value_names type_names expression in
        Ok (value_names, scope_names)
    | Ast.For (_, init, condition, step, body, _) ->
        let* loop_names, loop_scope_names =
          match init with
          | None -> Ok (value_names, String_set.empty)
          | Some statement ->
              validate_statement_names value_names type_names String_set.empty statement
        in
        let* () =
          match condition with
          | None -> Ok ()
          | Some expression ->
              validate_expression_names loop_names type_names expression
        in
        let* () = validate_statement_block_names loop_names type_names body in
        let* _ =
          match step with
          | None -> Ok (loop_names, loop_scope_names)
          | Some statement ->
              validate_statement_names loop_names type_names loop_scope_names statement
        in
        Ok (value_names, scope_names)
    | Ast.Switch (expression, cases, default, _) ->
        let* () = validate_expression_names value_names type_names expression in
        let* () =
          Result_list.iter
            (fun (values, body) ->
              let* () =
                Result_list.iter
                  (validate_expression_names value_names type_names)
                  values
              in
              validate_statement_block_names value_names type_names body)
            cases
        in
        let* () =
          match default with
          | None -> Ok ()
          | Some body -> validate_statement_block_names value_names type_names body
        in
        Ok (value_names, scope_names)
    | Ast.Const_for { name; ty; ty_span; low; high; body; span; _ } ->
        let* () = validate_binding_name span name in
        let* () = validate_type_names value_names type_names ty_span ty in
        let* () = validate_expression_names value_names type_names low in
        let* () = validate_expression_names value_names type_names high in
        let* () =
          validate_statement_block_names ~scope_names:(String_set.singleton name)
            (String_set.add name value_names)
            type_names body
        in
        Ok (value_names, scope_names)
    | Ast.Const_copy { name; body; _ } ->
        let* () =
          validate_statement_block_names ~scope_names:(String_set.singleton name)
            (String_set.add name value_names)
            type_names body
        in
        Ok (value_names, scope_names)
    | Ast.Break _ | Ast.Continue _ -> Ok (value_names, scope_names)
  and validate_statement_block_names ?(scope_names = String_set.empty) value_names
      type_names = function
    | [] -> Ok ()
    | statement :: rest ->
        let* value_names, scope_names =
          validate_statement_names value_names type_names scope_names statement
        in
        validate_statement_block_names ~scope_names value_names type_names rest
  in
  let rec validate_statement_duplicates scope_names = function
    | Ast.Let { name; span; _ } ->
        if String_set.mem name scope_names then
          error span (Printf.sprintf "duplicate local `%s`" name)
        else Ok (String_set.add name scope_names)
    | Ast.View { name; span; _ } ->
        if String_set.mem name scope_names then
          error span (Printf.sprintf "duplicate local `%s`" name)
        else Ok (String_set.add name scope_names)
    | Ast.If (_, yes, no, _) ->
        let* () = validate_statement_block_duplicates yes in
        let* () =
          match no with
          | None -> Ok ()
          | Some statements -> validate_statement_block_duplicates statements
        in
        Ok scope_names
    | Ast.While (_, _, body, _) | Ast.Defer (body, _) | Ast.Block (body, _) ->
        let* () = validate_statement_block_duplicates body in
        Ok scope_names
    | Ast.For (_, init, _, step, body, _) ->
        let* loop_scope_names =
          match init with
          | None -> Ok String_set.empty
          | Some statement -> validate_statement_duplicates String_set.empty statement
        in
        let* () = validate_statement_block_duplicates body in
        let* _ =
          match step with
          | None -> Ok loop_scope_names
          | Some statement -> validate_statement_duplicates loop_scope_names statement
        in
        Ok scope_names
    | Ast.Switch (_, cases, default, _) ->
        let* () =
          Result_list.iter
            (fun (_, body) -> validate_statement_block_duplicates body)
            cases
        in
        let* () =
          match default with
          | None -> Ok ()
          | Some body -> validate_statement_block_duplicates body
        in
        Ok scope_names
    | Ast.Const_for { name; body; _ } | Ast.Const_copy { name; body; _ } ->
        let* () =
          validate_statement_block_duplicates ~scope_names:(String_set.singleton name)
            body
        in
        Ok scope_names
    | Ast.Assign _ | Ast.Compound_assign _ | Ast.Return _ | Ast.Expr_stmt _
    | Ast.Break _ | Ast.Continue _ ->
        Ok scope_names
  and validate_statement_block_duplicates ?(scope_names = String_set.empty) = function
    | [] -> Ok ()
    | statement :: rest ->
        let* scope_names = validate_statement_duplicates scope_names statement in
        validate_statement_block_duplicates ~scope_names rest
  in
  let make_legality_context ret_ty =
    {
      structs = eval_structs;
      generic_structs;
      named_types = eval_named_types;
      consts = eval_consts;
      arrays = eval_arrays;
      globals = eval_globals;
      c_unsupported;
      c_nonnull_parameters;
      c_string_parameters;
      c_record_calls;
      record_result_call = false;
      record_call_checked = false;
      c_alloc_size_parameters;
      signatures = validation_signatures;
      external_c_functions =
        List.filter_map
          (function
            | Ast.Func { name; linkage = Ast.External_c; _ } -> Some name | _ -> None)
          program.items;
      templates = function_templates;
      top_level_bindings;
      specializations;
      spec_depth = 0;
      spec_trace = [];
      flow = Sema_flow.create ~initial_scope:true eval_structs;
      handle_view_expansion = false;
      expression_depth = 0;
      unresolved_shapes = Hashtbl.create 0;
      unresolved_shapes_unique = false;
      string_pool = create_string_pool limits;
      ret_ty;
      limits;
      index = None;
    }
  in
  let specialized_args name span =
    match find_by_name specializations Struct_specialization name with
    | Some
        {
          payload = Struct_payload { template = Ast.Struct item; substitutions; values };
          _;
        } ->
        Some
          ( item.name,
            List.map
              (function
                | Ast.Type_param { name; _ } ->
                    Ast.Type_arg (List.assoc name substitutions)
                | Ast.Const_param { name; _ } ->
                    let _, _, value = List.find (fun (n, _, _) -> n = name) values in
                    Ast.Const_arg (Ast.Int_lit (Int64.to_string value, span)))
              item.generic_params )
    | _ -> None
  in
  let rec source_type_of_hir span = function
    | Hir.Bool -> Ast.Bool
    | Hir.Int Hir.U8 -> Ast.Int Ast.U8
    | Hir.Int Hir.U16 -> Ast.Int Ast.U16
    | Hir.Int Hir.U32 -> Ast.Int Ast.U32
    | Hir.Int Hir.U64 -> Ast.Int Ast.U64
    | Hir.Int Hir.I8 -> Ast.Int Ast.I8
    | Hir.Int Hir.I16 -> Ast.Int Ast.I16
    | Hir.Int Hir.I32 -> Ast.Int Ast.I32
    | Hir.Int Hir.I64 -> Ast.Int Ast.I64
    | Hir.Int Hir.Usize -> Ast.Int Ast.Usize
    | Hir.Int Hir.Isize -> Ast.Int Ast.Isize
    | Hir.Float Hir.F32 -> Ast.Float Ast.F32
    | Hir.Float Hir.F64 -> Ast.Float Ast.F64
    | Hir.Addr -> Ast.Addr
    | Hir.Handle name -> (
        match specialized_args name span with
        | Some (template, args) ->
            Ast.Handle (Ast.Applied_type (template, span, args, span))
        | None -> Ast.Handle (Ast.Named_type (name, span)))
    | Hir.Array (count, element) ->
        Ast.Array
          ( Ast.aggregate_length (Ast.Int_lit (string_of_int count, span)) span,
            source_type_of_hir span element )
    | Hir.Vec (count, element) ->
        Ast.Vec
          ( Ast.aggregate_length (Ast.Int_lit (string_of_int count, span)) span,
            source_type_of_hir span element )
    | Hir.Struct name -> (
        match specialized_args name span with
        | Some (template, args) -> Ast.Applied_type (template, span, args, span)
        | None -> Ast.Named_type (name, span))
    | Hir.Opaque name -> Ast.Named_type (name, span)
    | Hir.Void -> Ast.Void
  in
  let rec substitute_field_type substitutions = function
    | Ast.Named_type (parameter, _) as ty ->
        Option.value ~default:ty (List.assoc_opt parameter substitutions)
    | Ast.Handle ty -> Ast.Handle (substitute_field_type substitutions ty)
    | Ast.Array (length, ty) ->
        Ast.Array (length, substitute_field_type substitutions ty)
    | Ast.Vec (length, ty) -> Ast.Vec (length, substitute_field_type substitutions ty)
    | Ast.Applied_type (name, name_span, arguments, span) ->
        Ast.Applied_type
          ( name,
            name_span,
            List.map
              (function
                | Ast.Type_arg ty ->
                    Ast.Type_arg (substitute_field_type substitutions ty)
                | Ast.Type_or_index ty ->
                    Ast.Type_or_index (substitute_field_type substitutions ty)
                | Ast.Name_arg (parameter, span) -> (
                    match List.assoc_opt parameter substitutions with
                    | Some ty -> Ast.Type_arg (substitute_field_type substitutions ty)
                    | None -> Ast.Name_arg (parameter, span))
                | argument -> argument)
              arguments,
            span )
    | (Ast.Addr | Ast.Bool | Ast.Void | Ast.Int _ | Ast.Float _) as ty -> ty
  in
  let generic_field_type source_type field_name =
    let instance =
      match source_type with
      | Ast.Applied_type (template, _, arguments, _) -> Some (template, arguments)
      | Ast.Named_type (name, source_span) -> (
          match specialized_args name source_span with
          | Some _ as instance -> instance
          | None -> Some (name, []))
      | _ -> None
    in
    match instance with
    | None -> None
    | Some (template, arguments) ->
        List.find_map
          (function
            | Ast.Struct { name; generic_params; fields; _ } when name = template ->
                let rec substitutions parameters arguments =
                  match (parameters, arguments) with
                  | [], [] -> []
                  | ( Ast.Type_param { name; _ } :: rest,
                      (Ast.Type_arg ty | Ast.Type_or_index ty) :: args ) ->
                      (name, ty) :: substitutions rest args
                  | Ast.Const_param _ :: rest, _ :: args -> substitutions rest args
                  | _ -> []
                in
                let substitutions = substitutions generic_params arguments in
                fields
                |> List.find_opt (fun (field : Ast.field) -> field.name = field_name)
                |> Option.map (fun (field : Ast.field) ->
                    substitute_field_type substitutions field.ty)
            | _ -> None)
          generic_structs
  in
  let global_source_type name =
    List.find_map
      (function
        | Ast.Const { name = constant; ty; _ } when constant = name -> Some ty
        | Ast.Global { name = global; ty; _ } when global = name -> Some ty
        | _ -> None)
      program.items
  in
  let rec source_place_type = function
    | Ast.Parenthesized (expression, _) -> source_place_type expression
    | Ast.Ident (name, _)
      when String_set.mem name !local_values || List.mem_assoc name !inference_types ->
        Option.join (List.assoc_opt name !inference_types)
    | Ast.Ident (name, _) -> global_source_type name
    | Ast.Binary ((Ast.Add | Ast.Sub), left, _, _) ->
        Option.bind (source_place_type left) (function
          | Ast.Addr -> Some Ast.Addr
          | _ -> None)
    | Ast.Field (base, field, _) ->
        Option.bind (source_place_type base) (fun ty -> generic_field_type ty field)
    | Ast.Select (base, [ Ast.Const_arg _ ], _)
    | Ast.Select (base, [ Ast.Name_arg _ ], _) ->
        Option.bind (source_place_type base) (function
          | Ast.Array (_, element) | Ast.Vec (_, element) -> Some element
          | _ -> None)
    | Ast.Select (base, [ Ast.Type_arg ty ], _)
    | Ast.Select (base, [ Ast.Type_or_index ty ], _) -> (
        match source_place_type base with Some Ast.Addr -> Some ty | _ -> None)
    | Ast.Select
        ( base,
          [
            (Ast.Type_arg ty | Ast.Type_or_index ty); (Ast.Const_arg _ | Ast.Name_arg _);
          ],
          _ ) -> (
        match source_place_type base with Some Ast.Addr -> Some ty | _ -> None)
    | _ -> None
  in
  let view_place_type place = view_source_type (source_place_type place) in
  let inference_context span =
    let inferred_globals =
      List.filter_map
        (function
          | Ast.Global { name; ty; linkage; span; _ } -> (
              match
                source_ty_with_values ~globals:global_names eval_named_types eval_consts
                  span ty
              with
              | Ok ty -> Some (name, ty, linkage)
              | Error _ -> None)
          | _ -> None)
        program.items
    in
    let globals =
      List.fold_left
        (fun globals (name, ty, linkage) ->
          if List.exists (fun (existing, _, _) -> existing = name) globals then globals
          else globals @ [ (name, ty, linkage) ])
        eval_globals inferred_globals
    in
    let shadowed =
      String_set.union !local_values
        (List.map fst !inference_types |> String_set.of_list)
    in
    let context =
      filter_context_names
        (fun name -> not (String_set.mem name shadowed))
        { (make_legality_context Hir.Void) with globals }
    in
    List.iter
      (fun (local, source_type) ->
        match source_type with
        | Some source_type when String_set.mem local !local_values -> (
            match
              source_ty_with_values ~globals:global_names eval_named_types eval_consts
                span source_type
            with
            | Ok ty -> (
                match Sema_flow.add_local local ty context.flow span with
                | Ok binding ->
                    Sema_flow.mark_init binding context.flow;
                    Option.iter
                      (fun element ->
                        match
                          source_ty_with_values ~globals:global_names eval_named_types
                            eval_consts span element
                        with
                        | Ok element ->
                            Sema_flow.bind_raw_view context.flow binding element
                              Sema_flow.Mutable_access None
                        | Error _ -> ())
                      (List.assoc_opt local !inference_raw_view_types)
                | Error _ -> ())
            | Error _ -> ())
        | _ -> ())
      !inference_types;
    context
  in
  let checked_view_types name name_span place span =
    let context = inference_context span in
    Sema_flow.push context.flow;
    let checked = check_stmt context (Ast.View { name; name_span; place; span }) in
    let source_type, raw_type =
      match checked with
      | Ok (Hir.View (binding, _, _)) ->
          (Some (source_type_of_hir span binding.ty), None)
      | Ok (Hir.Let (binding, _, _)) ->
          ( Some (source_type_of_hir span binding.ty),
            Option.map (source_type_of_hir span)
              (Sema_flow.raw_view_type context.flow binding) )
      | _ -> (None, None)
    in
    Sema_flow.pop context.flow;
    let source_type =
      match view_place_type place with
      | Some (Ast.Handle _) as source_type -> source_type
      | _ -> (
          match source_type with Some _ -> source_type | None -> view_place_type place)
    in
    (source_type, raw_type)
  in
  let infer_generic_arguments name generic_params params arguments span =
    let type_params = type_param_names generic_params in
    let const_params = const_params generic_params |> List.map (fun p -> p.Ast.name) in
    let inferred_types = ref [] and inferred_consts = ref [] and origins = ref [] in
    let direct_type_parameter _ =
      List.for_all
        (fun (param : Ast.param) ->
          match param.ty with
          | Ast.Handle _ | Ast.Array _ | Ast.Vec _ | Ast.Applied_type _ -> false
          | _ -> true)
        params
    in
    let integer_shape ty =
      let name = Ast.type_name ty in
      if not (String.starts_with ~prefix:"u" name || String.starts_with ~prefix:"i" name)
      then None
      else
        let bits =
          if name = "usize" || name = "isize" || String.ends_with ~suffix:"64" name then
            64
          else if String.ends_with ~suffix:"32" name then 32
          else if String.ends_with ~suffix:"16" name then 16
          else if String.ends_with ~suffix:"8" name then 8
          else 0
        in
        if bits = 0 then None else Some (bits, String.starts_with ~prefix:"u" name)
    in
    let type_holds actual expected =
      Ast.type_name actual = Ast.type_name expected
      ||
      match (integer_shape actual, integer_shape expected) with
      | Some (actual_bits, actual_unsigned), Some (expected_bits, expected_unsigned) ->
          expected_bits > actual_bits && (actual_unsigned || not expected_unsigned)
      | _ -> false
    in
    let bind_type parameter ty argument =
      match List.assoc_opt parameter !inferred_types with
      | None ->
          inferred_types := (parameter, ty) :: !inferred_types;
          origins := (parameter, argument) :: !origins;
          Ok ()
      | Some previous when Ast.type_name previous = Ast.type_name ty -> Ok ()
      | Some previous ->
          let previous_argument = Ast.expr_name (List.assoc parameter !origins) in
          let candidate =
            if not (direct_type_parameter parameter) then None
            else if type_holds previous ty then Some ty
            else if type_holds ty previous then Some previous
            else None
          in
          let help =
            match candidate with
            | None -> None
            | Some candidate ->
                Some
                  (Printf.sprintf "write `%s[%s](%s)`" name (Ast.type_name candidate)
                     (String.concat ", " (List.map Ast.expr_name arguments)))
          in
          Error
            [
              Diag.error ?help span
                (Printf.sprintf "cannot infer `%s` for `%s`: `%s` is `%s`, `%s` is `%s`"
                   parameter name previous_argument (Ast.type_name previous)
                   (Ast.expr_name argument) (Ast.type_name ty));
            ]
    in
    let bind_const parameter value argument =
      match List.assoc_opt parameter !inferred_consts with
      | None ->
          inferred_consts := (parameter, value) :: !inferred_consts;
          origins := (parameter, argument) :: !origins;
          Ok ()
      | Some previous when previous = value -> Ok ()
      | Some previous ->
          let previous_argument = Ast.expr_name (List.assoc parameter !origins) in
          error span
            (Printf.sprintf
               "cannot infer `%s` for `%s`: `%s` gives `%s`, `%s` gives `%s`" parameter
               name previous_argument (Int64.to_string previous)
               (Ast.expr_name argument) (Int64.to_string value))
    in
    let int_value (length : Ast.aggregate_length) =
      try Some (Int64.of_string length.text) with _ -> None
    in
    let rec match_type argument pattern actual =
      match (pattern, actual) with
      | Ast.Named_type (n, _), actual when List.mem n type_params ->
          bind_type n actual argument
      | Ast.Handle p, Ast.Handle a -> match_type argument p a
      | Ast.Vec (length, p), Ast.Vec (actual_length, a) ->
          let* () =
            match (length.expression, int_value actual_length) with
            | Ast.Ident (n, _), Some value when List.mem n const_params ->
                bind_const n value argument
            | _ -> Ok ()
          in
          match_type argument p a
      | Ast.Applied_type (template, _, expected, _), actual -> (
          let actual =
            match actual with
            | Ast.Applied_type (name, _, args, _) -> Some (name, args)
            | Ast.Named_type (name, _) -> specialized_args name span
            | _ -> None
          in
          match actual with
          | Some (name, actual_args) when name = template ->
              let rec args expected actual =
                match (expected, actual) with
                | [], [] -> Ok ()
                | ( (Ast.Type_arg p | Ast.Type_or_index p) :: es,
                    (Ast.Type_arg a | Ast.Type_or_index a) :: rest ) ->
                    let* () = match_type argument p a in
                    args es rest
                | ( ( Ast.Name_arg (n, _)
                    | Ast.Const_arg (Ast.Ident (n, _))
                    | Ast.Type_or_index (Ast.Named_type (n, _)) )
                    :: es,
                    Ast.Const_arg (Ast.Int_lit (v, _)) :: rest )
                  when List.mem n const_params ->
                    let* () =
                      try bind_const n (Int64.of_string v) argument
                      with Failure _ -> Ok ()
                    in
                    args es rest
                | Ast.Name_arg (n, _) :: es, Ast.Type_arg a :: rest
                  when List.mem n type_params ->
                    let* () = bind_type n a argument in
                    args es rest
                | _ -> Ok ()
              in
              args expected actual_args
          | _ -> Ok ())
      | _ -> Ok ()
    in
    let inference_context = inference_context span in
    let rec literal_kind = function
      | Ast.Int_lit _ -> Some `Int
      | Ast.Float_lit _ -> Some `Float
      | Ast.Parenthesized (value, _) | Ast.Unary (Ast.Neg, value, _) ->
          literal_kind value
      | _ -> None
    in
    let checked_type argument =
      match check_expr inference_context None argument with
      | Error _ -> None
      | Ok checked -> Some (source_type_of_hir span (Hir.expr_ty checked))
    in
    let rec actual_type = function
      | Ast.Parenthesized (value, _) -> actual_type value
      | argument when Option.is_some (unresolved_shape_of argument) -> None
      | argument -> (
          let place = source_place_type argument in
          let checked = checked_type argument in
          match checked with Some _ as ty -> ty | None -> place)
    in
    let* () =
      List.fold_left2
        (fun result (parameter : Ast.param) argument ->
          let* () = result in
          match actual_type argument with
          | Some actual -> match_type argument parameter.ty actual
          | None -> Ok ())
        (Ok ()) params arguments
    in
    let literal_only parameter =
      let occurrences =
        List.filter_map
          (fun ((param : Ast.param), argument) ->
            match param.ty with
            | Ast.Named_type (name, _) when name = parameter -> Some argument
            | _ -> None)
          (List.combine params arguments)
      in
      let kinds = List.map literal_kind occurrences in
      occurrences <> []
      && List.for_all Option.is_some kinds
      && List.for_all (( = ) (List.hd kinds)) kinds
    in
    let missing parameter =
      let message, help =
        if List.mem parameter type_params && literal_only parameter then
          let kind =
            match
              List.find_map
                (fun ((param : Ast.param), argument) ->
                  match param.ty with
                  | Ast.Named_type (name, _) when name = parameter ->
                      literal_kind argument
                  | _ -> None)
                (List.combine params arguments)
            with
            | Some `Float -> "f64"
            | _ -> "i32"
          in
          ( Printf.sprintf "cannot infer `%s` for `%s` from literals only" parameter name,
            Printf.sprintf "write `%s[%s](%s)`" name kind
              (String.concat ", " (List.map Ast.expr_name arguments)) )
        else
          ( Printf.sprintf
              "cannot infer `%s` for `%s`; provide explicit generic arguments" parameter
              name,
            Printf.sprintf "write the explicit generic argument list for `%s`" name )
      in
      Diag.error ~help span message
    in
    let rec make_args acc = function
      | [] -> Ok (List.rev acc)
      | Ast.Type_param { name = parameter; _ } :: rest -> (
          match List.assoc_opt parameter !inferred_types with
          | Some ty -> make_args (Ast.Type_arg ty :: acc) rest
          | None -> Error [ missing parameter ])
      | Ast.Const_param { name = parameter; _ } :: rest -> (
          match List.assoc_opt parameter !inferred_consts with
          | Some value ->
              make_args
                (Ast.Const_arg (Ast.Int_lit (Int64.to_string value, span)) :: acc)
                rest
          | None -> Error [ missing parameter ])
    in
    make_args [] generic_params
  in
  let rec has_generic_type = function
    | Ast.Applied_type _ -> true
    | Ast.Array (_, ty) | Ast.Vec (_, ty) | Ast.Handle ty -> has_generic_type ty
    | _ -> false
  in
  let rec has_generic_arguments = function
    | Ast.Generic_args _ -> true
    | Ast.Unary (_, expression, _)
    | Ast.C_dereference (expression, _, _)
    | Ast.C_dot_star (expression, _)
    | Ast.Parenthesized (expression, _)
    | Ast.Addr_of (expression, _)
    | Ast.Splat (expression, _)
    | Ast.Sizeof_value (expression, _)
    | Ast.Field (expression, _, _)
    | Ast.Arrow_field (expression, _, _, _) ->
        has_generic_arguments expression
    | Ast.Binary (_, left, right, _) ->
        has_generic_arguments left || has_generic_arguments right
    | Ast.Select (base, args, _) ->
        has_generic_arguments base
        || List.exists
             (function
               | Ast.Const_arg e -> has_generic_arguments e
               | Ast.Type_arg ty | Ast.Type_or_index ty -> has_generic_type ty
               | Ast.Name_arg _ -> false)
             args
    | Ast.Call (Ast.Ident (name, _), arguments, _) ->
        List.mem_assoc name function_templates
        || List.exists has_generic_arguments arguments
    | Ast.Call (callee, arguments, _) ->
        has_generic_arguments callee || List.exists has_generic_arguments arguments
    | Ast.Cast (_, ty, expression, _) | Ast.Handle_from_addr (ty, expression, _) ->
        has_generic_type ty || has_generic_arguments expression
    | Ast.Sizeof (ty, _) | Ast.Alignof (ty, _) | Ast.Offsetof (ty, _, _, _) ->
        has_generic_type ty
    | Ast.Ternary (condition, yes, no, _) ->
        has_generic_arguments condition
        || has_generic_arguments yes || has_generic_arguments no
    | Ast.Array_lit (values, _) -> List.exists has_generic_arguments values
    | Ast.Designated_field (_, _, value, _) -> has_generic_arguments value
    | Ast.Designated_index (index, value, _) ->
        has_generic_arguments index || has_generic_arguments value
    | Ast.Ident _ | Ast.Int_lit _ | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _
    | Ast.String_lit _ ->
        false
  in
  let rec substitute_validation_type type_substitutions const_substitutions = function
    | Ast.Named_type (name, _) as ty ->
        Option.value ~default:ty (List.assoc_opt name type_substitutions)
    | Ast.Addr -> Ast.Addr
    | Ast.Handle ty ->
        Ast.Handle
          (substitute_validation_type type_substitutions const_substitutions ty)
    | Ast.Array (length, ty) ->
        Ast.Array
          ( substitute_validation_length type_substitutions const_substitutions length,
            substitute_validation_type type_substitutions const_substitutions ty )
    | Ast.Vec (length, ty) ->
        Ast.Vec
          ( substitute_validation_length type_substitutions const_substitutions length,
            substitute_validation_type type_substitutions const_substitutions ty )
    | Ast.Applied_type (name, name_span, arguments, span) ->
        Ast.Applied_type
          ( name,
            name_span,
            List.map
              (substitute_validation_argument type_substitutions const_substitutions)
              arguments,
            span )
    | (Ast.Bool | Ast.Void | Ast.Int _ | Ast.Float _) as ty -> ty
  and substitute_validation_length type_substitutions const_substitutions length =
    let expression =
      substitute_validation_expression type_substitutions const_substitutions
        length.Ast.expression
    in
    { length with Ast.expression; text = Ast.expr_name expression }
  and substitute_validation_argument type_substitutions const_substitutions = function
    | Ast.Type_arg ty | Ast.Type_or_index ty ->
        Ast.Type_arg
          (substitute_validation_type type_substitutions const_substitutions ty)
    | Ast.Const_arg expression ->
        Ast.Const_arg
          (substitute_validation_expression type_substitutions const_substitutions
             expression)
    | Ast.Name_arg (name, span) -> (
        match List.assoc_opt name type_substitutions with
        | Some ty ->
            Ast.Type_arg
              (substitute_validation_type type_substitutions const_substitutions ty)
        | None -> (
            match List.assoc_opt name const_substitutions with
            | Some expression -> Ast.Const_arg expression
            | None -> Ast.Name_arg (name, span)))
  and substitute_validation_expression type_substitutions const_substitutions expression
      =
    let sub = substitute_validation_expression type_substitutions const_substitutions in
    match expression with
    | Ast.Ident (name, _) as expression ->
        Option.value ~default:expression (List.assoc_opt name const_substitutions)
    | Ast.Unary (op, e, s) -> Ast.Unary (op, sub e, s)
    | Ast.Parenthesized (e, s) -> Ast.Parenthesized (sub e, s)
    | Ast.Binary (op, l, r, s) -> Ast.Binary (op, sub l, sub r, s)
    | Ast.Cast (k, ty, e, s) ->
        Ast.Cast
          ( k,
            substitute_validation_type type_substitutions const_substitutions ty,
            sub e,
            s )
    | Ast.Select (b, args, s) ->
        Ast.Select
          ( sub b,
            List.map
              (substitute_validation_argument type_substitutions const_substitutions)
              args,
            s )
    | Ast.Sizeof (ty, s) ->
        Ast.Sizeof
          (substitute_validation_type type_substitutions const_substitutions ty, s)
    | Ast.Sizeof_value (e, s) -> Ast.Sizeof_value (sub e, s)
    | Ast.Alignof (ty, s) ->
        Ast.Alignof
          (substitute_validation_type type_substitutions const_substitutions ty, s)
    | Ast.Offsetof (ty, f, field_span, s) ->
        Ast.Offsetof
          ( substitute_validation_type type_substitutions const_substitutions ty,
            f,
            field_span,
            s )
    | Ast.Ternary (c, y, n, s) -> Ast.Ternary (sub c, sub y, sub n, s)
    | expression -> expression
  in
  let dependent_vector_type dependent type_span ty =
    match
      source_ty_with_values ~globals:global_names eval_named_types eval_consts type_span
        ty
    with
    | Ok ty -> Some (ty, false)
    | Error _ -> (
        match ty with
        | Ast.Vec (_, element) when not (type_mentions dependent element) ->
            Option.map
              (fun element -> (Hir.Vec (1, element), true))
              (Result.to_option
                 (source_ty_with_values ~globals:global_names eval_named_types
                    eval_consts type_span element))
        | _ -> None)
  in
  let add_dependent_local dependent c name type_span ty =
    match dependent_vector_type dependent type_span ty with
    | None -> Ok ()
    | Some (ty, _) ->
        let* binding = add_local name ty c type_span in
        mark_init binding c;
        Ok ()
  in
  let rec validate_non_dependent_expression c dependent expected expression =
    if
      (not (expression_mentions dependent expression))
      && not (has_generic_arguments expression)
    then
      let* checked = check_expr c expected expression in
      match expected with
      | None -> Ok ()
      | Some expected ->
          ensure_expected ~checked_expression:checked ~expression (Hir.expr_ty checked)
            expected (Ast.expr_span expression)
    else
      match expression with
      | Ast.Unary (_, value, _)
      | Ast.C_dereference (value, _, _)
      | Ast.C_dot_star (value, _)
      | Ast.Parenthesized (value, _)
      | Ast.Addr_of (value, _)
      | Ast.Splat (value, _) ->
          validate_non_dependent_expression c dependent None value
      | Ast.Binary (_, left, right, _) ->
          let peer_type peer operand =
            let vector_type () =
              let before = Sema_flow.snapshot c.flow in
              let checked = check_expr c None peer in
              Sema_flow.restore c.flow before;
              match checked with
              | Ok value -> (
                  match Hir.expr_ty value with Hir.Vec _ as ty -> Some ty | _ -> None)
              | Error _ -> None
            in
            match operand with
            | Ast.Splat _
            | Ast.Call (Ast.Ident ("splat", _), _, _)
            | Ast.Parenthesized (Ast.Splat _, _)
            | Ast.Parenthesized (Ast.Call (Ast.Ident ("splat", _), _, _), _) ->
                vector_type ()
            | Ast.Array_lit (entries, _) -> (
                match vector_type () with
                | Some (Hir.Vec (_, element)) ->
                    Some (Hir.Vec (List.length entries, element))
                | _ -> None)
            | _ -> None
          in
          let left_expected = peer_type right left
          and right_expected = peer_type left right in
          let* () = validate_non_dependent_expression c dependent left_expected left in
          validate_non_dependent_expression c dependent right_expected right
      | Ast.Select (base, args, _) ->
          let* () = validate_non_dependent_expression c dependent None base in
          Result_list.iter
            (fun arg ->
              match arg with
              | Ast.Const_arg expression ->
                  validate_non_dependent_expression c dependent None expression
              | Ast.Type_arg _ | Ast.Type_or_index _ | Ast.Name_arg _ -> Ok ())
            args
      | Ast.Call (Ast.Ident (name, _), arguments, span) -> (
          match List.assoc_opt name validation_signatures with
          | Some signature -> (
              if
                (not signature.variadic)
                && List.length arguments <> List.length signature.params
                || signature.variadic
                   && List.length arguments < List.length signature.params
              then
                error span
                  (function_arity_message name
                     (List.length signature.params)
                     (List.length arguments))
              else
                let rec validate_arguments formals arguments =
                  match (formals, arguments) with
                  | [], trailing ->
                      Result_list.iter
                        (validate_non_dependent_expression c dependent None)
                        trailing
                  | (_, formal) :: formals, argument :: arguments ->
                      let* () =
                        validate_non_dependent_expression c dependent (Some formal)
                          argument
                      in
                      validate_arguments formals arguments
                  | _ ->
                      error span
                        (function_arity_message name
                           (List.length signature.params)
                           (List.length arguments))
                in
                let* () = validate_arguments signature.params arguments in
                match expected with
                | None -> Ok ()
                | Some expected ->
                    ensure_expected
                      ~context:(Printf.sprintf "result of `%s`" name)
                      signature.ret expected span)
          | None ->
              Result_list.iter
                (validate_non_dependent_expression c dependent None)
                arguments)
      | Ast.Call
          (Ast.Generic_args (Ast.Ident (name, _), generic_arguments, _), arguments, span)
        -> (
          match List.assoc_opt name function_templates with
          | Some (Ast.Func { generic_params; params; ret; _ }) -> (
              let rec resolve_arguments substitutions values parameters arguments =
                match (parameters, arguments) with
                | [], [] -> Ok (Some (substitutions, values))
                | Ast.Type_param { name; _ } :: parameters, argument :: arguments -> (
                    match argument with
                    | Ast.Type_arg ty when not (type_mentions dependent ty) ->
                        resolve_arguments ((name, ty) :: substitutions) values
                          parameters arguments
                    | Ast.Name_arg (argument_name, argument_span)
                      when not (List.mem argument_name dependent) ->
                        resolve_arguments
                          ((name, Ast.Named_type (argument_name, argument_span))
                          :: substitutions)
                          values parameters arguments
                    | _ -> Ok None)
                | Ast.Const_param parameter :: parameters, argument :: arguments ->
                    let* expression = generic_const_argument span argument in
                    if expression_mentions dependent expression then Ok None
                    else
                      let substituted =
                        substitute_validation_type substitutions [] parameter.ty
                      in
                      let* () =
                        charge_expanded_type type_node_account ~span:parameter.span
                          ~where:(Printf.sprintf "declaration `%s`" name)
                          substituted
                      in
                      let* ty =
                        source_ty_with_values ~globals:global_names eval_named_types
                          eval_consts parameter.span substituted
                      in
                      let* actual_ty, value =
                        const_expr ~structs:eval_structs ~named_types:eval_named_types
                          ~generic_structs ~globals:global_names
                          ~array_lengths:
                            (array_lengths
                            @ static_array_lengths top_level_bindings eval_globals)
                          ~arrays:eval_arrays eval_consts (Some ty) expression
                      in
                      let* () =
                        ensure_expected
                          ~context:(Printf.sprintf "value for `%s`" parameter.name)
                          ~expression actual_ty ty (Ast.expr_span expression)
                      in
                      resolve_arguments substitutions
                        ((parameter.name, ty, value) :: values)
                        parameters arguments
                | _ -> Ok None
              in
              let* resolved =
                resolve_arguments [] [] generic_params generic_arguments
              in
              match resolved with
              | None ->
                  Result_list.iter
                    (validate_non_dependent_expression c dependent None)
                    arguments
              | Some (substitutions, values) -> (
                  let* formals =
                    Result_list.map
                      (fun (parameter : Ast.param) ->
                        let substituted =
                          substitute_validation_type substitutions [] parameter.ty
                        in
                        let* () =
                          charge_expanded_type type_node_account ~span:parameter.span
                            ~where:(Printf.sprintf "declaration `%s`" name)
                            substituted
                        in
                        source_ty_with_values ~globals:global_names eval_named_types
                          values parameter.span substituted)
                      params
                  in
                  if List.length formals <> List.length arguments then
                    error span
                      (function_arity_message name (List.length formals)
                         (List.length arguments))
                  else
                    let* () =
                      Result_list.iter
                        (fun (formal, argument) ->
                          validate_non_dependent_expression c dependent (Some formal)
                            argument)
                        (List.combine formals arguments)
                    in
                    let* return_ty =
                      let substituted =
                        substitute_validation_type substitutions [] ret
                      in
                      let* () =
                        charge_expanded_type type_node_account ~span
                          ~where:(Printf.sprintf "declaration `%s`" name)
                          substituted
                      in
                      source_ty_with_values ~globals:global_names eval_named_types
                        values span substituted
                    in
                    match expected with
                    | None -> Ok ()
                    | Some expected ->
                        ensure_expected
                          ~context:(Printf.sprintf "result of `%s`" name)
                          return_ty expected span))
          | _ ->
              Result_list.iter
                (validate_non_dependent_expression c dependent None)
                arguments)
      | Ast.Call (callee, arguments, _) ->
          let* () = validate_non_dependent_expression c dependent None callee in
          Result_list.iter
            (validate_non_dependent_expression c dependent None)
            arguments
      | Ast.Generic_args (Ast.Ident _, arguments, _) ->
          Result_list.iter
            (function
              | Ast.Const_arg value ->
                  validate_non_dependent_expression c dependent None value
              | Ast.Type_arg _ | Ast.Type_or_index _ | Ast.Name_arg _ -> Ok ())
            arguments
      | Ast.Generic_args (_, arguments, _) ->
          Result_list.iter
            (function
              | Ast.Const_arg value ->
                  validate_non_dependent_expression c dependent None value
              | Ast.Type_arg _ | Ast.Type_or_index _ | Ast.Name_arg _ -> Ok ())
            arguments
      | Ast.Cast (kind, destination, value, span) ->
          let* () =
            if type_mentions dependent destination || has_generic_type destination then
              Ok ()
            else
              let* destination =
                source_ty_with_values ~globals:global_names eval_named_types eval_consts
                  span destination
              in
              let valid =
                match (kind, destination) with
                | (Ast.Zext | Ast.Sext), (Hir.Int _ | Hir.Vec (_, Hir.Int _)) -> true
                | Ast.Trunc, (Hir.Bool | Hir.Int _ | Hir.Vec (_, (Hir.Bool | Hir.Int _)))
                  ->
                    true
                | ( Ast.Bitcast,
                    ( Hir.Bool | Hir.Int _
                    | Hir.Vec (_, (Hir.Bool | Hir.Int _ | Hir.Float _)) ) ) ->
                    true
                | _ -> false
              in
              if valid then Ok ()
              else
                match check_expr c None value with
                | Ok checked ->
                    Error
                      [
                        Sema_types.cast_error ~expression:value kind
                          (Hir.expr_ty checked) destination span;
                      ]
                | Error _ -> error span (Sema_types.cast_target_error kind destination)
          in
          validate_non_dependent_expression c dependent None value
      | Ast.Handle_from_addr (destination, value, span) ->
          let* () =
            if type_mentions dependent destination || has_generic_type destination then
              Ok ()
            else
              handle_target eval_named_types destination
              |> Result.map_error (fun message -> [ Diag.error span message ])
              |> Result.map (fun _ -> ())
          in
          validate_non_dependent_expression c dependent None value
      | Ast.Field (value, _, _) | Ast.Arrow_field (value, _, _, _) ->
          validate_non_dependent_expression c dependent None value
      | Ast.Sizeof_value (value, _) ->
          validate_non_dependent_expression c dependent None value
      | Ast.Ternary (condition, yes, no, _) ->
          let* () = validate_non_dependent_expression c dependent None condition in
          let* () = validate_non_dependent_expression c dependent expected yes in
          validate_non_dependent_expression c dependent expected no
      | Ast.Array_lit (values, _) ->
          Result_list.iter (validate_non_dependent_expression c dependent None) values
      | Ast.Designated_field (_, _, value, _) ->
          validate_non_dependent_expression c dependent expected value
      | Ast.Designated_index (index, value, _) ->
          let* () = validate_non_dependent_expression c dependent None index in
          validate_non_dependent_expression c dependent expected value
      | Ast.Sizeof _ | Ast.Alignof _ | Ast.Offsetof _ | Ast.Ident _ | Ast.Int_lit _
      | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _ ->
          Ok ()
  in
  let validate_non_dependent_condition c dependent label expression =
    if expression_mentions dependent expression then
      validate_non_dependent_expression c dependent None expression
    else
      let* checked = check_expr c None expression in
      if Hir.expr_ty checked = Hir.Bool then Ok ()
      else Error [ Sema_types.condition_error label expression (Hir.expr_ty checked) ]
  in
  let target_mentions names = function
    | Ast.Target_ident (name, _) -> List.mem name names
    | Ast.Target_field (expression, _, _) -> expression_mentions names expression
    | Ast.Target_select (base, args) ->
        expression_mentions names base
        || List.exists (generic_argument_mentions names) args
  in
  let dependent_local_types = ref [] in
  let with_dependent_local_types f =
    let previous_types = !dependent_local_types in
    let previous_inference_types = !inference_types in
    let previous_raw_view_types = !inference_raw_view_types in
    let result = f () in
    dependent_local_types := previous_types;
    inference_types := previous_inference_types;
    inference_raw_view_types := previous_raw_view_types;
    result
  in
  let dependent_generic_field_type source_type field_name =
    let instance =
      match source_type with
      | Ast.Applied_type (template, _, arguments, _) -> Some (template, arguments)
      | Ast.Named_type (name, _) -> Some (name, [])
      | _ -> None
    in
    match instance with
    | None -> None
    | Some (template, arguments) ->
        List.find_map
          (function
            | Ast.Struct { name; generic_params; fields; _ } when name = template ->
                let rec substitutions types consts parameters arguments =
                  match (parameters, arguments) with
                  | [], [] -> (types, consts)
                  | ( Ast.Type_param { name; _ } :: rest,
                      (Ast.Type_arg ty | Ast.Type_or_index ty) :: args ) ->
                      substitutions ((name, ty) :: types) consts rest args
                  | Ast.Type_param { name; _ } :: rest, Ast.Name_arg (arg, span) :: args
                    ->
                      substitutions
                        ((name, Ast.Named_type (arg, span)) :: types)
                        consts rest args
                  | ( Ast.Const_param { name; _ } :: rest,
                      Ast.Const_arg expression :: args ) ->
                      substitutions types ((name, expression) :: consts) rest args
                  | ( Ast.Const_param { name; _ } :: rest,
                      Ast.Name_arg (arg, span) :: args ) ->
                      substitutions types
                        ((name, Ast.Ident (arg, span)) :: consts)
                        rest args
                  | ( Ast.Const_param { name; _ } :: rest,
                      Ast.Type_or_index (Ast.Named_type (arg, span)) :: args ) ->
                      substitutions types
                        ((name, Ast.Ident (arg, span)) :: consts)
                        rest args
                  | _ -> ([], [])
                in
                let type_substitutions, const_substitutions =
                  substitutions [] [] generic_params arguments
                in
                fields
                |> List.find_opt (fun (field : Ast.field) -> field.name = field_name)
                |> Option.map (fun (field : Ast.field) ->
                    substitute_validation_type type_substitutions const_substitutions
                      field.ty)
            | _ -> None)
          generic_structs
  in
  let rec dependent_expression_type = function
    | Ast.Parenthesized (expression, _) -> dependent_expression_type expression
    | Ast.Ident (name, _) -> List.assoc_opt name !dependent_local_types
    | Ast.Field (base, field, _) ->
        Option.bind (dependent_expression_type base) (fun ty ->
            dependent_generic_field_type ty field)
    | Ast.Select (base, [ Ast.Const_arg _ ], _)
    | Ast.Select (base, [ Ast.Name_arg _ ], _) ->
        Option.bind (dependent_expression_type base) (function
          | Ast.Array (_, element) | Ast.Vec (_, element) -> Some element
          | _ -> None)
    | Ast.Select (base, [ Ast.Type_arg ty ], _)
    | Ast.Select (base, [ Ast.Type_or_index ty ], _) ->
        Option.bind (dependent_expression_type base) (function
          | Ast.Addr -> Some ty
          | _ -> None)
    | _ -> None
  in
  let dependent_target_type = function
    | Ast.Target_ident (name, _) -> List.assoc_opt name !dependent_local_types
    | Ast.Target_field (base, field, _) ->
        Option.bind (dependent_expression_type base) (fun ty ->
            dependent_generic_field_type ty field)
    | Ast.Target_select (base, _) -> dependent_expression_type base
  in
  let rec validate_non_dependent_statements c dependent expected_return statements =
    match statements with
    | [] -> Ok dependent
    | statement :: rest ->
        let* dependent =
          validate_non_dependent_statement c dependent expected_return statement
        in
        validate_non_dependent_statements c dependent expected_return rest
  and validate_non_dependent_block c dependent expected_return statements =
    push c;
    let result =
      with_dependent_local_types (fun () ->
          validate_non_dependent_statements c dependent expected_return statements)
    in
    pop c;
    let* _ = result in
    Ok ()
  and validate_non_dependent_statement c dependent expected_return = function
    | Ast.Let { name; ty = source_ty; init; span; _ } as statement ->
        if type_mentions dependent source_ty || has_generic_type source_ty then begin
          dependent_local_types :=
            (name, source_ty) :: List.remove_assoc name !dependent_local_types;
          set_inference_type name (Some source_ty);
          let* () =
            if type_mentions dependent source_ty then
              add_dependent_local dependent c name span source_ty
            else Ok ()
          in
          Ok (name :: dependent)
        end
        else
          let* ty =
            source_ty_with_values ~globals:global_names eval_named_types eval_consts
              span source_ty
            |> Diag.local_type_error name
          in
          if
            (match ty with Hir.Array _ | Hir.Struct _ -> true | _ -> false)
            &&
            match init with
            | Some value ->
                (not (expression_mentions dependent value))
                && not (has_generic_arguments value)
            | None -> false
          then (
            let* _ = check_stmt c statement in
            dependent_local_types :=
              (name, source_ty) :: List.remove_assoc name !dependent_local_types;
            set_inference_type name (Some source_ty);
            Ok (List.filter (fun dependent_name -> dependent_name <> name) dependent))
          else
            let* () =
              match init with
              | None -> Ok ()
              | Some value ->
                  validate_non_dependent_expression c dependent (Some ty) value
            in
            let* binding = add_local name ty c span in
            if Option.is_some init then mark_init binding c;
            dependent_local_types :=
              (name, source_ty) :: List.remove_assoc name !dependent_local_types;
            set_inference_type name (Some source_ty);
            Ok (List.filter (fun dependent_name -> dependent_name <> name) dependent)
    | Ast.View { name; name_span; place; _ } as statement ->
        dependent_local_types := List.remove_assoc name !dependent_local_types;
        if expression_mentions dependent place || has_generic_arguments place then (
          let source_type = view_place_type place in
          (match source_type with
          | Some source_type ->
              dependent_local_types :=
                (name, source_type) :: List.remove_assoc name !dependent_local_types
          | None -> ());
          set_inference_type name source_type;
          Ok (name :: dependent))
        else
          let* () =
            let* _ = check_stmt c statement in
            Ok ()
          in
          let source_type, raw_type =
            checked_view_types name name_span place (Ast.expr_span place)
          in
          set_inference_type name source_type;
          Option.iter (set_inference_raw_view_type name) raw_type;
          Ok (List.filter (fun dependent_name -> dependent_name <> name) dependent)
    | (Ast.Assign (target, value, _) | Ast.Compound_assign (target, _, value, _, _)) as
      statement ->
        let vector_literal_expected expected placeholder = function
          | Ast.Array_lit (entries, _) when placeholder -> (
              match expected with
              | Some (Hir.Vec (_, element)) ->
                  Some (Hir.Vec (List.length entries, element))
              | _ -> expected)
          | _ when placeholder -> None
          | _ -> expected
        in
        if target_mentions dependent target then
          let expected, placeholder =
            match
              Option.bind (dependent_target_type target) (fun ty ->
                  dependent_vector_type dependent (Ast.expr_span value) ty)
            with
            | Some (ty, placeholder) -> (Some ty, placeholder)
            | None -> (
                match check_target c target with
                | Ok checked -> (target_ty c checked.target, false)
                | Error _ -> (None, false))
          in
          if
            placeholder && not (match value with Ast.Array_lit _ -> true | _ -> false)
          then Ok dependent
          else
            let* () =
              validate_non_dependent_expression c dependent
                (vector_literal_expected expected placeholder value)
                value
            in
            Ok dependent
        else if expression_mentions dependent value || has_generic_arguments value then
          let* checked_target = check_target c target in
          let* expected =
            match target_ty c checked_target.target with
            | Some ty -> Ok ty
            | None -> error (Ast.expr_span value) "assignment target has no type"
          in
          let* () =
            validate_non_dependent_expression c dependent (Some expected) value
          in
          Ok dependent
        else
          let* () =
            let* _ = check_stmt c statement in
            Ok ()
          in
          Ok dependent
    | Ast.Return (value, _) ->
        let* () =
          match value with
          | None -> Ok ()
          | Some value ->
              validate_non_dependent_expression c dependent expected_return value
        in
        Ok dependent
    | Ast.If (condition, yes, no, _) ->
        let* () = validate_non_dependent_condition c dependent "if" condition in
        let* () = validate_non_dependent_block c dependent expected_return yes in
        let* () =
          match no with
          | None -> Ok ()
          | Some no -> validate_non_dependent_block c dependent expected_return no
        in
        Ok dependent
    | Ast.While (_, condition, body, _) ->
        let* () = validate_non_dependent_condition c dependent "while" condition in
        let* () = validate_non_dependent_block c dependent expected_return body in
        Ok dependent
    | Ast.Defer (body, _) | Ast.Block (body, _) ->
        let* () = validate_non_dependent_block c dependent expected_return body in
        Ok dependent
    | Ast.Expr_stmt (expression, _, _) ->
        let* () = validate_non_dependent_expression c dependent None expression in
        Ok dependent
    | Ast.For (_, init, condition, step, body, _) ->
        push c;
        let result =
          with_dependent_local_types (fun () ->
              let* loop_dependent =
                match init with
                | None -> Ok dependent
                | Some init ->
                    validate_non_dependent_statement c dependent expected_return init
              in
              let* () =
                match condition with
                | None -> Ok ()
                | Some condition ->
                    validate_non_dependent_condition c loop_dependent "for" condition
              in
              let* () =
                validate_non_dependent_block c loop_dependent expected_return body
              in
              match step with
              | None -> Ok loop_dependent
              | Some step ->
                  validate_non_dependent_statement c loop_dependent expected_return step)
        in
        pop c;
        let* _ = result in
        Ok dependent
    | Ast.Switch (expression, cases, default, _) ->
        let* () = validate_non_dependent_expression c dependent None expression in
        let* () =
          Result_list.iter
            (fun (values, body) ->
              let* () =
                Result_list.iter
                  (validate_non_dependent_expression c dependent None)
                  values
              in
              validate_non_dependent_block c dependent expected_return body)
            cases
        in
        let* () =
          match default with
          | None -> Ok ()
          | Some body -> validate_non_dependent_block c dependent expected_return body
        in
        Ok dependent
    | Ast.Break _ | Ast.Continue _ | Ast.Const_for _ | Ast.Const_copy _ -> Ok dependent
  in
  let validate_generic_legality ~span ~ret ~params ~generic_params statements =
    let generic_value_names =
      List.map
        (fun (parameter : Ast.const_param) -> parameter.name)
        (const_params generic_params)
    in
    let dependent = type_param_names generic_params @ generic_value_names in
    let expected_return =
      if type_mentions dependent ret then None
      else
        match
          source_ty_with_values ~globals:global_names eval_named_types eval_consts span
            ret
        with
        | Ok ty -> Some ty
        | Error _ -> None
    in
    let context =
      make_legality_context (Option.value ~default:Hir.Void expected_return)
    in
    let* () =
      Result_list.iter
        (function
          | Ast.Type_param _ -> Ok ()
          | Ast.Const_param ({ name; ty = source_ty; span; _ } : Ast.const_param) ->
              let* ty =
                source_ty_with_values ~globals:global_names eval_named_types eval_consts
                  span source_ty
              in
              let* binding = add_local name ty context span in
              mark_init binding context;
              set_inference_type name (Some source_ty);
              Ok ())
        generic_params
    in
    let rec add_parameters dependent = function
      | [] -> Ok dependent
      | (parameter : Ast.param) :: rest ->
          if type_mentions dependent parameter.ty then begin
            dependent_local_types :=
              (parameter.name, parameter.ty)
              :: List.remove_assoc parameter.name !dependent_local_types;
            let* () =
              add_dependent_local dependent context parameter.name parameter.span
                parameter.ty
            in
            add_parameters (parameter.name :: dependent) rest
          end
          else
            let* ty =
              source_ty_with_values ~globals:global_names eval_named_types eval_consts
                parameter.span parameter.ty
            in
            let* binding = add_local parameter.name ty context parameter.span in
            mark_init binding context;
            add_parameters
              (List.filter (fun name -> name <> parameter.name) dependent)
              rest
    in
    let* dependent = add_parameters dependent params in
    let* _ =
      validate_non_dependent_statements context dependent expected_return statements
    in
    Ok ()
  in
  let rec has_unresolved_application = function
    | Ast.Applied_type _ -> true
    | Ast.Handle ty | Ast.Array (_, ty) | Ast.Vec (_, ty) ->
        has_unresolved_application ty
    | Ast.Bool | Ast.Void | Ast.Int _ | Ast.Float _ | Ast.Named_type _ | Ast.Addr ->
        false
  in
  let rec resolve_ty ?(values = []) ?(defer_const_structs = false) substitutions depth
      span = function
    | Ast.Bool -> Ok Ast.Bool
    | Ast.Void -> Ok Ast.Void
    | Ast.Int kind -> Ok (Ast.Int kind)
    | Ast.Float kind -> Ok (Ast.Float kind)
    | Ast.Addr -> Ok Ast.Addr
    | Ast.Handle ty ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Handle ty)
    | Ast.Array (length_info, ty) ->
        let* length =
          resolve_length "array" length_info ~values ~defer_const_structs substitutions
            depth
        in
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Array (length, ty))
    | Ast.Vec (length_info, ty) ->
        let* length =
          resolve_length "vector" length_info ~values ~defer_const_structs substitutions
            depth
        in
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok (Ast.Vec (length, ty))
    | Ast.Named_type ("void", type_span) -> Ok (Ast.Named_type ("void", type_span))
    | Ast.Named_type (name, type_span)
      when Option.is_some (Names.type_misuse_message name) ->
        error type_span (Option.get (Names.type_misuse_message name))
    | Ast.Named_type (name, type_span) -> (
        if String_set.mem name !local_values then
          error type_span (Printf.sprintf "`%s` is a value, not a type" name)
        else
          match List.assoc_opt name substitutions with
          | Some ty -> Ok ty
          | None ->
              if List.mem_assoc name struct_templates then
                error type_span
                  (Printf.sprintf "generic struct `%s` requires type arguments" name)
              else if
                String_set.mem name named_type_names
                || List.mem name !generic_type_names
                || Option.is_some
                     (find_by_name specializations Struct_specialization name)
              then Ok (Ast.Named_type (name, type_span))
              else
                Error
                  [
                    unknown_type_error
                      (String_set.elements named_type_names
                      @ !generic_type_names @ List.map fst eval_named_types)
                      type_span name;
                  ])
    | Ast.Applied_type (name, _, _, _) when String_set.mem name !local_values ->
        error span (Printf.sprintf "`%s` is a value, not a type" name)
    | Ast.Applied_type (name, name_span, arguments, application_span) -> (
        let arguments = List.map bind_argument arguments in
        match List.assoc_opt name struct_templates with
        | None ->
            if String_set.mem name struct_names then
              error span (Printf.sprintf "struct `%s` is not generic" name)
            else error span (Printf.sprintf "unknown generic struct `%s`" name)
        | Some (Ast.Struct ({ generic_params; _ } as template)) ->
            if List.length arguments <> List.length generic_params then
              error application_span
                (generic_arity_message "generic struct" name
                   (List.length generic_params) (List.length arguments))
            else if defer_const_structs && has_const_params generic_params then
              let rec resolve_arguments resolved params arguments =
                match (params, arguments) with
                | [], [] -> Ok (List.rev resolved)
                | Ast.Type_param _ :: params, argument :: arguments ->
                    let* ty =
                      match argument with
                      | Ast.Type_arg ty | Ast.Type_or_index ty ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span ty
                      | Ast.Name_arg (name, _) ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span
                            (Ast.Named_type (name, span))
                      | Ast.Const_arg expression ->
                          error (Ast.expr_span expression) "expected a type argument"
                    in
                    resolve_arguments (Ast.Type_arg ty :: resolved) params arguments
                | Ast.Const_param _ :: params, argument :: arguments ->
                    let* expression = generic_const_argument span argument in
                    let* () = shadowed_constant_error expression in
                    let* expression =
                      resolve_expr ~values ~defer_const_structs substitutions depth
                        expression
                    in
                    resolve_arguments
                      (Ast.Const_arg expression :: resolved)
                      params arguments
                | _ -> error span "generic argument arity mismatch"
              in
              let* arguments = resolve_arguments [] generic_params arguments in
              Ok (Ast.Applied_type (name, name_span, arguments, application_span))
            else
              let rec resolve_arguments resolved diagnostic types bindings values_out
                  source_arguments params arguments =
                match (params, arguments) with
                | [], [] ->
                    Ok
                      ( List.rev resolved,
                        List.rev diagnostic,
                        List.rev types,
                        List.rev bindings,
                        List.rev values_out,
                        List.rev source_arguments )
                | ( Ast.Type_param { name = parameter; _ } :: params,
                    argument :: arguments ) ->
                    let* argument =
                      match argument with
                      | Ast.Type_arg ty | Ast.Type_or_index ty ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span ty
                      | Ast.Name_arg (name, _) ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span
                            (Ast.Named_type (name, span))
                      | Ast.Const_arg expression ->
                          error (Ast.expr_span expression) "expected a type argument"
                    in
                    let diagnostic_argument =
                      Diagnostic_type_argument
                        (diagnostic_type_of_ast specializations argument)
                    in
                    resolve_arguments
                      (Type_specialization_arg (resolved_type_key values span argument)
                      :: resolved)
                      (diagnostic_argument :: diagnostic)
                      (argument :: types)
                      ((parameter, argument) :: bindings)
                      values_out
                      (Ast.Type_arg argument :: source_arguments)
                      params arguments
                | Ast.Const_param parameter :: params, argument :: arguments ->
                    let* expression = generic_const_argument span argument in
                    let* expression =
                      if substitutions = [] then Ok expression
                      else
                        resolve_expr ~values ~defer_const_structs substitutions depth
                          expression
                    in
                    let* () = shadowed_constant_error expression in
                    let* const_ty = source_ty_diag [] parameter.span parameter.ty in
                    let* actual_ty, value =
                      const_expr ~structs:eval_structs ~named_types:eval_named_types
                        ~generic_structs ~globals:global_names
                        ~array_lengths:
                          (array_lengths
                          @ static_array_lengths top_level_bindings eval_globals)
                        ~arrays:eval_arrays (values @ eval_consts) (Some const_ty)
                        expression
                    in
                    if not (equal actual_ty const_ty) then
                      error (Ast.expr_span expression) "const argument type mismatch"
                    else
                      resolve_arguments
                        (Const_specialization_arg (const_ty, value) :: resolved)
                        (Diagnostic_const_argument (const_ty, value) :: diagnostic)
                        types bindings
                        ((parameter.name, const_ty, value) :: values_out)
                        (Ast.Const_arg expression :: source_arguments)
                        params arguments
                | _ -> error span "generic argument arity mismatch"
              in
              let* ( ordered_arguments,
                     diagnostic_arguments,
                     type_arguments,
                     substitutions,
                     values,
                     source_arguments ) =
                resolve_arguments [] [] [] [] [] [] generic_params arguments
              in
              if List.exists has_unresolved_application type_arguments then
                Ok
                  (Ast.Applied_type (name, name_span, source_arguments, application_span))
              else
                let specialization_name =
                  if values = [] then mangle_type_specialization name type_arguments
                  else mangle_mixed_specialization name ordered_arguments
                in
                let* declaration_id =
                  specialization_declaration_id top_level_bindings Top_type span name
                in
                let key = (Struct_specialization, declaration_id, ordered_arguments) in
                let frame =
                  {
                    template_name = name;
                    arguments = diagnostic_arguments;
                    application_span;
                  }
                in
                let specialization =
                  {
                    key;
                    name = specialization_name;
                    depth;
                    payload =
                      Struct_payload
                        { template = Ast.Struct template; substitutions; values };
                    trace = !current_trace @ [ frame ];
                    pending_frame = None;
                  }
                in
                let* specialization =
                  Sema_specialization.request specializations ~limits ~depth ~span
                    ~description:"struct specialization" specialization
                in
                Ok (Ast.Named_type (specialization.name, span))
        | Some _ -> error span "internal error: generic struct template is malformed")
  and resolve_length kind length_info ~values ~defer_const_structs substitutions depth =
    let shadowed =
      String_set.elements !shadowed_constants
      |> List.find_opt (fun name ->
          expression_mentions [ name ] length_info.Ast.expression)
    in
    let* () =
      match shadowed with
      | Some name ->
          error length_info.span
            (Printf.sprintf "`%s` is not a compile-time constant" name)
      | None -> Ok ()
    in
    let* expression =
      resolve_expr ~values ~defer_const_structs substitutions depth
        length_info.Ast.expression
    in
    let evaluated =
      const_expr ~structs:eval_structs ~named_types:eval_named_types ~generic_structs
        ~arrays:eval_arrays
        ~array_lengths:
          (array_lengths @ static_array_lengths top_level_bindings eval_globals)
        ~globals:global_names (values @ eval_consts)
        (aggregate_length_expected expression)
        expression
    in
    let deferred_constants = String_set.diff !local_values !shadowed_constants in
    match evaluated with
    | Error [ diagnostic ]
      when String.starts_with ~prefix:"unknown name `" diagnostic.Diag.message
           && (String_set.exists
                 (fun name -> expression_mentions [ name ] expression)
                 global_value_names
              || String_set.exists
                   (fun name -> expression_mentions [ name ] expression)
                   deferred_constants) ->
        Ok length_info
    | Error [ _ ] as result -> remap_length_cycle length_info.span result
    | Error diagnostics -> Error diagnostics
    | Ok (ty, value) ->
        let* length = aggregate_length_value kind length_info.span ty value in
        let expression = Ast.Int_lit (string_of_int length, length_info.span) in
        Ok (Ast.aggregate_length expression length_info.span)
  and resolve_expr ?(values = []) ?(defer_const_structs = false) substitutions depth
      expression =
    let resolve_exprs arguments =
      Result_list.map
        (resolve_expr ~values ~defer_const_structs substitutions depth)
        arguments
    in
    let rec resolve_layout_parts span = function
      | Ast.Named_type (name, _) as ty ->
          Ok (Option.value ~default:ty (List.assoc_opt name substitutions))
      | Ast.Handle ty ->
          let* ty = resolve_layout_parts span ty in
          Ok (Ast.Handle ty)
      | (Ast.Array (length, ty) | Ast.Vec (length, ty)) as aggregate ->
          let* expression =
            resolve_expr ~values ~defer_const_structs substitutions depth
              length.Ast.expression
          in
          let* ty = resolve_layout_parts span ty in
          let length = { length with Ast.expression } in
          Ok
            (match aggregate with
            | Ast.Array _ -> Ast.Array (length, ty)
            | _ -> Ast.Vec (length, ty))
      | Ast.Applied_type (name, name_span, arguments, application_span) ->
          let resolve_argument = function
            | Ast.Type_arg ty | Ast.Type_or_index ty ->
                let* ty = resolve_layout_parts span ty in
                Ok (Ast.Type_arg ty)
            | Ast.Const_arg expression ->
                if substitutions = [] then Ok (Ast.Const_arg expression)
                else
                  let* expression =
                    resolve_expr ~values ~defer_const_structs substitutions depth
                      expression
                  in
                  Ok (Ast.Const_arg expression)
            | Ast.Name_arg (name, name_span) as argument -> (
                match List.assoc_opt name substitutions with
                | None -> Ok argument
                | Some ty ->
                    let* ty = resolve_layout_parts name_span ty in
                    Ok (Ast.Type_arg ty))
          in
          let* arguments = Result_list.map resolve_argument arguments in
          Ok (Ast.Applied_type (name, name_span, arguments, application_span))
      | ty -> Ok ty
    in
    let resolve_layout_type span ty =
      let* ty = resolve_layout_parts span ty in
      if substitutions = [] then
        let* _ = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        Ok ty
      else Ok ty
    in
    match expression with
    | ( Ast.Int_lit _ | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _
      | Ast.Ident _ ) as expression ->
        Ok expression
    | Ast.Unary (op, expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Unary (op, expression, span))
    | Ast.C_dereference (expression, type_hint, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.C_dereference (expression, type_hint, span))
    | Ast.C_dot_star (expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.C_dot_star (expression, span))
    | Ast.Parenthesized (expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Parenthesized (expression, span))
    | Ast.Binary (op, left, right, span) ->
        let* left =
          resolve_expr ~values ~defer_const_structs substitutions depth left
        in
        let* right =
          resolve_expr ~values ~defer_const_structs substitutions depth right
        in
        Ok (Ast.Binary (op, left, right, span))
    | Ast.Call (Ast.Ident (name, ident_span), arguments, span)
      when not (String_set.mem name !local_values) -> (
        match List.assoc_opt name function_templates with
        | Some (Ast.Func { generic_params = _ :: _ as generic_params; params; _ })
          when List.length params = List.length arguments ->
            let* arguments = resolve_exprs arguments in
            let* generic_arguments =
              infer_generic_arguments name generic_params params arguments span
            in
            let* callee =
              resolve_expr ~values ~defer_const_structs substitutions depth
                (Ast.Generic_args (Ast.Ident (name, ident_span), generic_arguments, span))
            in
            Ok (Ast.Call (callee, arguments, span))
        | _ ->
            let* arguments = resolve_exprs arguments in
            Ok (Ast.Call (Ast.Ident (name, ident_span), arguments, span)))
    | Ast.Call (callee, arguments, span) ->
        let* callee =
          resolve_expr ~values ~defer_const_structs substitutions depth callee
        in
        let* arguments = resolve_exprs arguments in
        Ok (Ast.Call (callee, arguments, span))
    | Ast.Generic_args (Ast.Ident (name, ident_span), arguments, span)
      when name = "volatile_load" || name = "volatile_store" || name = "call_addr"
           || name = "cvt"
           || List.mem name simd_memory_names ->
        let resolve_argument argument =
          match
            if is_shadowed_argument argument then argument else bind_argument argument
          with
          | Ast.Type_arg ty | Ast.Type_or_index ty ->
              let* ty =
                resolve_ty ~values ~defer_const_structs substitutions depth span ty
              in
              Ok (Ast.Type_arg ty)
          | Ast.Name_arg (name, name_span) ->
              let* ty =
                resolve_ty ~values ~defer_const_structs substitutions depth name_span
                  (Ast.Named_type (name, name_span))
              in
              Ok (Ast.Type_arg ty)
          | Ast.Const_arg expression ->
              let* expression =
                resolve_expr ~values ~defer_const_structs substitutions depth expression
              in
              Ok (Ast.Const_arg expression)
        in
        let* arguments = Result_list.map resolve_argument arguments in
        Ok (Ast.Generic_args (Ast.Ident (name, ident_span), arguments, span))
    | Ast.Generic_args (Ast.Ident (name, name_span), _, _)
      when Option.is_some (Names.takes_no_type_arguments_message name) ->
        error name_span (Option.get (Names.takes_no_type_arguments_message name))
    | Ast.Generic_args (Ast.Ident (name, ident_span), arguments, span) -> (
        let arguments = List.map bind_argument arguments in
        match List.assoc_opt name function_templates with
        | None ->
            if String_set.mem name function_names then
              error span (Printf.sprintf "function `%s` is not generic" name)
            else error span (Printf.sprintf "unknown generic function `%s`" name)
        | Some (Ast.Func ({ generic_params; _ } as template)) ->
            if List.length arguments <> List.length generic_params then
              error span
                (generic_arity_message "generic function" name
                   (List.length generic_params) (List.length arguments))
            else
              let rec resolve_arguments types bindings consts staged diagnostic resolved
                  params arguments =
                match (params, arguments) with
                | [], [] ->
                    Ok
                      ( List.rev types,
                        List.rev bindings,
                        List.rev consts,
                        List.rev staged,
                        List.rev diagnostic,
                        List.rev resolved )
                | ( Ast.Type_param { name = parameter; _ } :: params,
                    argument :: arguments ) ->
                    let* argument =
                      match argument with
                      | Ast.Type_arg ty | Ast.Type_or_index ty ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            span ty
                      | Ast.Name_arg (name, name_span) ->
                          resolve_ty ~values ~defer_const_structs substitutions depth
                            name_span
                            (Ast.Named_type (name, name_span))
                      | Ast.Const_arg expression ->
                          error (Ast.expr_span expression) "expected a type argument"
                    in
                    resolve_arguments (argument :: types)
                      ((parameter, argument) :: bindings)
                      consts
                      (Staged_type_arg (resolved_type_key values span argument)
                      :: staged)
                      (Pending_diagnostic_type
                         (diagnostic_type_of_ast specializations argument)
                      :: diagnostic)
                      (Ast.Type_arg argument :: resolved)
                      params arguments
                | ( Ast.Const_param { name = parameter; _ } :: params,
                    argument :: arguments ) ->
                    let* expression = generic_const_argument span argument in
                    let* expression =
                      resolve_expr ~values ~defer_const_structs substitutions depth
                        expression
                    in
                    let argument = Ast.Const_arg expression in
                    resolve_arguments types bindings (argument :: consts)
                      (Staged_const_arg parameter :: staged)
                      (Pending_diagnostic_const parameter :: diagnostic)
                      (argument :: resolved) params arguments
                | _ -> error span "generic argument arity mismatch"
              in
              let* ( type_arguments,
                     type_substitutions,
                     const_arguments,
                     staged_args,
                     diagnostic_args,
                     resolved_arguments ) =
                resolve_arguments [] [] [] [] [] [] generic_params arguments
              in
              if List.exists has_unresolved_application type_arguments then
                Ok
                  (Ast.Generic_args
                     (Ast.Ident (name, ident_span), resolved_arguments, span))
              else if type_arguments = [] then
                let* () =
                  if not eager_functions then Ok ()
                  else
                    let const_params = const_params generic_params in
                    let rec eval acc params arguments =
                      match (params, arguments) with
                      | [], [] -> Ok (List.rev acc)
                      | parameter :: params, argument :: arguments ->
                          let* expression = generic_const_argument span argument in
                          let* const_ty =
                            source_ty_diag eval_named_types parameter.Ast.span
                              parameter.ty
                          in
                          let* actual_ty, value =
                            const_expr ~structs:eval_structs ~globals:global_names
                              ~generic_structs ~named_types:eval_named_types
                              ~array_lengths:
                                (array_lengths
                                @ static_array_lengths top_level_bindings eval_globals)
                              ~arrays:eval_arrays (values @ eval_consts) (Some const_ty)
                              expression
                          in
                          if not (equal actual_ty const_ty) then
                            error (Ast.expr_span expression)
                              "const argument type mismatch"
                          else
                            eval
                              ((parameter.name, const_ty, value) :: acc)
                              params arguments
                      | _ -> error span "const argument arity mismatch"
                    in
                    let* concrete_values = eval [] const_params const_arguments in
                    let* staged =
                      staged_specialization_identity specializations name
                        concrete_values
                    in
                    let* specialization_name, key =
                      match staged with
                      | None ->
                          let* declaration_id =
                            specialization_declaration_id top_level_bindings
                              Top_function span name
                          in
                          Ok
                            ( mangle_specialization name concrete_values,
                              function_specialization_key declaration_id concrete_values
                            )
                      | Some (origin_id, origin_name, arguments, _, _) ->
                          Ok
                            ( mangle_mixed_specialization origin_name arguments,
                              (Function_specialization, origin_id, arguments) )
                    in
                    let frame_name, frame_arguments, frame_span =
                      match staged with
                      | None ->
                          ( name,
                            List.map
                              (fun (_, ty, value) ->
                                Diagnostic_const_argument (ty, value))
                              concrete_values,
                            span )
                      | Some (_, origin_name, _, arguments, application_span) ->
                          (origin_name, arguments, application_span)
                    in
                    let frame =
                      {
                        template_name = frame_name;
                        arguments = frame_arguments;
                        application_span = frame_span;
                      }
                    in
                    let specialization =
                      {
                        key;
                        name = specialization_name;
                        depth;
                        payload =
                          Function_payload
                            {
                              item = Ast.Func template;
                              substitutions = [];
                              values = concrete_values;
                              staged_args = None;
                            };
                        trace = !current_trace @ [ frame ];
                        pending_frame = None;
                      }
                    in
                    let* _ =
                      Sema_specialization.request specializations ~limits ~depth ~span
                        ~description:"const specialization" specialization
                    in
                    Ok ()
                in
                Ok
                  (Ast.Generic_args (Ast.Ident (name, ident_span), const_arguments, span))
              else
                let specialization_name =
                  mangle_type_specialization name type_arguments
                in
                let* declaration_id =
                  specialization_declaration_id top_level_bindings Top_function span
                    name
                in
                let key =
                  ( Function_specialization,
                    declaration_id,
                    List.map
                      (fun ty ->
                        Type_specialization_arg (resolved_type_key values span ty))
                      type_arguments )
                in
                let pending_frame =
                  if const_arguments = [] then None
                  else
                    Some
                      {
                        pending_arguments = diagnostic_args;
                        pending_application_span = span;
                      }
                in
                let trace =
                  if const_arguments <> [] then !current_trace
                  else
                    let arguments =
                      List.map
                        (function
                          | Pending_diagnostic_type ty -> Diagnostic_type_argument ty
                          | Pending_diagnostic_const _ -> assert false)
                        diagnostic_args
                    in
                    !current_trace
                    @ [ { template_name = name; arguments; application_span = span } ]
                in
                let specialization =
                  {
                    key;
                    name = specialization_name;
                    depth;
                    payload =
                      Function_payload
                        {
                          item = Ast.Func template;
                          substitutions = type_substitutions;
                          values = [];
                          staged_args =
                            (if const_arguments = [] then None else Some staged_args);
                        };
                    trace;
                    pending_frame;
                  }
                in
                let* specialization =
                  Sema_specialization.request specializations ~limits ~depth ~span
                    ~description:"function specialization" specialization
                in
                if const_arguments = [] then
                  Ok (Ast.Ident (specialization.name, ident_span))
                else
                  Ok
                    (Ast.Generic_args
                       ( Ast.Ident (specialization.name, ident_span),
                         const_arguments,
                         span ))
        | Some _ -> error span "internal error: generic function template is malformed")
    | Ast.Generic_args (_, _, span) ->
        error span "generic call target must be a function name"
    | Ast.Cast (kind, ty, expression, span) ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Cast (kind, ty, expression, span))
    | Ast.Handle_from_addr (ty, expression, span) ->
        let* ty = resolve_ty ~values ~defer_const_structs substitutions depth span ty in
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Handle_from_addr (ty, expression, span))
    | Ast.Select (base, args, span) ->
        let* base =
          resolve_expr ~values ~defer_const_structs substitutions depth base
        in
        let* args =
          Result_list.map
            (fun arg ->
              match arg with
              | Ast.Name_arg ("..", _) -> Ok arg
              | Ast.Type_arg (Ast.Named_type ("void", _) as ty) -> Ok (Ast.Type_arg ty)
              | (Ast.Type_or_index _ | Ast.Name_arg _) when is_shadowed_argument arg ->
                  Ok arg
              | _ -> (
                  match bind_argument arg with
                  | Ast.Type_arg ty | Ast.Type_or_index ty ->
                      let* ty =
                        resolve_ty ~values ~defer_const_structs substitutions depth span
                          ty
                      in
                      Ok (Ast.Type_arg ty)
                  | Ast.Const_arg expression ->
                      let* expression =
                        resolve_expr ~values ~defer_const_structs substitutions depth
                          expression
                      in
                      Ok (Ast.Const_arg expression)
                  | Ast.Name_arg (name, _) -> (
                      match List.assoc_opt name substitutions with
                      | Some ty -> Ok (Ast.Type_arg ty)
                      | None -> Ok arg)))
            args
        in
        Ok (Ast.Select (base, args, span))
    | Ast.Field (base, name, span) ->
        let* base =
          resolve_expr ~values ~defer_const_structs substitutions depth base
        in
        Ok (Ast.Field (base, name, span))
    | Ast.Arrow_field (base, name, operator_span, field_span) ->
        let* base =
          resolve_expr ~values ~defer_const_structs substitutions depth base
        in
        Ok (Ast.Arrow_field (base, name, operator_span, field_span))
    | Ast.Sizeof_value (value, span) ->
        let* value =
          resolve_expr ~values ~defer_const_structs substitutions depth value
        in
        Ok (Ast.Sizeof_value (value, span))
    | Ast.Addr_of (expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Addr_of (expression, span))
    | Ast.Sizeof (ty, span) ->
        if has_generic_type ty && !preserve_layout_queries then
          Ok (Ast.Sizeof (ty, span))
        else if has_generic_type ty then
          let* ty = resolve_layout_type span ty in
          Ok (Ast.Sizeof (ty, span))
        else
          let* ty =
            resolve_ty ~values ~defer_const_structs substitutions depth span ty
          in
          Ok (Ast.Sizeof (ty, span))
    | Ast.Alignof (ty, span) ->
        if has_generic_type ty && !preserve_layout_queries then
          Ok (Ast.Alignof (ty, span))
        else if has_generic_type ty then
          let* ty = resolve_layout_type span ty in
          Ok (Ast.Alignof (ty, span))
        else
          let* ty =
            resolve_ty ~values ~defer_const_structs substitutions depth span ty
          in
          Ok (Ast.Alignof (ty, span))
    | Ast.Offsetof (ty, field, field_span, span) ->
        if has_generic_type ty && !preserve_layout_queries then
          Ok (Ast.Offsetof (ty, field, field_span, span))
        else if has_generic_type ty then
          let* ty = resolve_layout_type span ty in
          Ok (Ast.Offsetof (ty, field, field_span, span))
        else
          let* ty =
            resolve_ty ~values ~defer_const_structs substitutions depth span ty
          in
          Ok (Ast.Offsetof (ty, field, field_span, span))
    | Ast.Splat (expression, span) ->
        let* expression =
          resolve_expr ~values ~defer_const_structs substitutions depth expression
        in
        Ok (Ast.Splat (expression, span))
    | Ast.Ternary (condition, yes, no, span) ->
        let resolve = resolve_expr ~values ~defer_const_structs substitutions depth in
        let* condition = resolve condition in
        let* yes = resolve yes in
        let* no = resolve no in
        Ok (Ast.Ternary (condition, yes, no, span))
    | Ast.Array_lit (elements, span) ->
        let* elements = resolve_exprs elements in
        Ok (Ast.Array_lit (elements, span))
    | Ast.Designated_field (name, name_span, value, span) ->
        let* value =
          resolve_expr ~values ~defer_const_structs substitutions depth value
        in
        Ok (Ast.Designated_field (name, name_span, value, span))
    | Ast.Designated_index (index, value, span) ->
        let resolve = resolve_expr ~values ~defer_const_structs substitutions depth in
        let* index = resolve index in
        let* value = resolve value in
        Ok (Ast.Designated_index (index, value, span))
  and resolve_target ?(values = []) ?(defer_const_structs = false) substitutions depth =
    function
    | Ast.Target_ident _ as target -> Ok target
    | Ast.Target_select (base, args) ->
        let resolve = resolve_expr ~values ~defer_const_structs substitutions depth in
        let* base = resolve base in
        let* args =
          Result_list.map
            (fun arg ->
              match arg with
              | (Ast.Type_or_index _ | Ast.Name_arg _) when is_shadowed_argument arg ->
                  Ok arg
              | _ -> (
                  match bind_argument arg with
                  | Ast.Type_arg ty | Ast.Type_or_index ty ->
                      let* ty =
                        resolve_ty ~values ~defer_const_structs substitutions depth
                          (Ast.expr_span base) ty
                      in
                      Ok (Ast.Type_arg ty)
                  | Ast.Const_arg expression ->
                      let* expression = resolve expression in
                      Ok (Ast.Const_arg expression)
                  | Ast.Name_arg (name, _) -> (
                      match List.assoc_opt name substitutions with
                      | Some ty -> Ok (Ast.Type_arg ty)
                      | None -> Ok arg)))
            args
        in
        Ok (Ast.Target_select (base, args))
    | Ast.Target_field (base, name, span) ->
        let* base =
          resolve_expr ~values ~defer_const_structs substitutions depth base
        in
        Ok (Ast.Target_field (base, name, span))
  and resolve_stmt ?(values = []) ?(defer_const_structs = false) substitutions depth
      statement =
    let previous = !local_values in
    let previous_shadowed_constants = !shadowed_constants in
    let result =
      match statement with
      | Ast.Let { name; ty; ty_span; init; span } ->
          let* ty =
            Diag.local_type_error ~span ~ty_span name
              (resolve_ty ~values ~defer_const_structs substitutions depth ty_span ty)
            |> Sema_types.declaration_type_error ~declaration:"locals" ~variable:name ty
          in
          let* init =
            match init with
            | None -> Ok None
            | Some expression ->
                let* expression =
                  resolve_expr ~values ~defer_const_structs substitutions depth
                    expression
                in
                Ok (Some expression)
          in
          local_values := String_set.add name !local_values;
          shadowed_constants := String_set.add name !shadowed_constants;
          set_inference_type name (Some ty);
          Ok (Ast.Let { name; ty; ty_span; init; span })
      | Ast.View { name; name_span; place; span } ->
          let* place =
            resolve_expr ~values ~defer_const_structs substitutions depth place
          in
          let inference_type, raw_type = checked_view_types name name_span place span in
          local_values := String_set.add name !local_values;
          shadowed_constants := String_set.add name !shadowed_constants;
          set_inference_type name inference_type;
          Option.iter (set_inference_raw_view_type name) raw_type;
          Ok (Ast.View { name; name_span; place; span })
      | Ast.Assign (target, expression, span) ->
          let* target =
            resolve_target ~values ~defer_const_structs substitutions depth target
          in
          let* expression =
            resolve_expr ~values ~defer_const_structs substitutions depth expression
          in
          Ok (Ast.Assign (target, expression, span))
      | Ast.Compound_assign (target, op, expression, span, operator_span) ->
          let* target =
            resolve_target ~values ~defer_const_structs substitutions depth target
          in
          let* expression =
            resolve_expr ~values ~defer_const_structs substitutions depth expression
          in
          Ok (Ast.Compound_assign (target, op, expression, span, operator_span))
      | Ast.Return (expression, span) ->
          let* expression =
            match expression with
            | None -> Ok None
            | Some expression ->
                let* expression =
                  resolve_expr ~values ~defer_const_structs substitutions depth
                    expression
                in
                Ok (Some expression)
          in
          Ok (Ast.Return (expression, span))
      | Ast.If (condition, yes, no, span) -> (
          let unresolved_condition = condition in
          let* condition =
            resolve_expr ~values ~defer_const_structs substitutions depth condition
          in
          let resolve = resolve_stmt ~values ~defer_const_structs substitutions depth in
          let specialization_values =
            List.filter
              (fun (name, _, _) -> not (String_set.mem name !shadowed_constants))
              values
          in
          let constant_environment =
            List.filter
              (fun (name, _, _) -> not (String_set.mem name !shadowed_constants))
              (values @ eval_consts)
          in
          let specialization_names =
            List.map (fun (name, _, _) -> name) specialization_values
          in
          let known_condition =
            if not (expression_mentions specialization_names unresolved_condition) then
              None
            else
              match
                const_expr ~structs:eval_structs ~named_types:eval_named_types
                  ~generic_structs ~globals:global_names
                  ~array_lengths:
                    (array_lengths
                    @ static_array_lengths top_level_bindings eval_globals)
                  ~arrays:eval_arrays constant_environment None condition
              with
              | Ok (_, value) -> Some (value <> 0L)
              | Error _ -> None
          in
          match known_condition with
          | Some false ->
              let* no =
                match no with
                | None -> Ok []
                | Some statements ->
                    with_local_values !local_values (fun () ->
                        resolve_statements resolve statements)
              in
              Ok (Ast.Block (no, span))
          | Some true ->
              let* yes =
                with_local_values !local_values (fun () ->
                    resolve_statements resolve yes)
              in
              Ok (Ast.Block (yes, span))
          | None ->
              let* yes =
                with_local_values !local_values (fun () ->
                    resolve_statements resolve yes)
              in
              let* no =
                match no with
                | None -> Ok None
                | Some statements ->
                    let* statements =
                      with_local_values !local_values (fun () ->
                          resolve_statements resolve statements)
                    in
                    Ok (Some statements)
              in
              Ok (Ast.If (condition, yes, no, span)))
      | Ast.While (label, condition, body, span) ->
          let* condition =
            resolve_expr ~values ~defer_const_structs substitutions depth condition
          in
          let* body =
            with_local_values !local_values (fun () ->
                resolve_statements
                  (resolve_stmt ~values ~defer_const_structs substitutions depth)
                  body)
          in
          Ok (Ast.While (label, condition, body, span))
      | (Ast.Break _ | Ast.Continue _) as statement -> Ok statement
      | Ast.Const_for _ when not eager_functions -> Ok statement
      | Ast.Const_for { name; name_span; ty; ty_span; low; high; body; span } ->
          let* () =
            match Ast.copy_breaks body with
            | span :: _ ->
                error span
                  "`break` cannot leave a `for const` loop; its copies are \
                   straight-line code"
            | [] -> Ok ()
          in
          let* () =
            if List.exists (fun (value_name, _, _) -> value_name = name) values then
              error name_span
                (Printf.sprintf
                   "`for const` index `%s` shadows a constant of the same name" name)
            else Ok ()
          in
          let* ty =
            resolve_ty ~values ~defer_const_structs substitutions depth ty_span ty
          in
          let* index_ty = source_ty_diag [] ty_span ty in
          let* () =
            match index_ty with
            | Hir.Int _ -> Ok ()
            | _ -> error ty_span "a `for const` index must have an integer type"
          in
          let constant_environment =
            List.filter
              (fun (name, _, _) -> not (String_set.mem name !shadowed_constants))
              (values @ eval_consts)
          in
          let bound expression =
            let* expression =
              resolve_expr ~values ~defer_const_structs substitutions depth expression
            in
            match
              const_expr ~structs:eval_structs ~named_types:eval_named_types
                ~generic_structs ~globals:global_names
                ~array_lengths:
                  (array_lengths @ static_array_lengths top_level_bindings eval_globals)
                ~arrays:eval_arrays constant_environment (Some index_ty) expression
            with
            | Ok (_, value) -> Ok value
            | Error _
              when expression_mentions (String_set.elements !local_values) expression ->
                error (Ast.expr_span expression)
                  "`for const` bounds must be compile-time constants"
            | Error diagnostics -> Error diagnostics
          in
          let unsigned = Sema_numeric.is_unsigned index_ty in
          let widen value =
            if unsigned then value else Sema_numeric.sign_extend_value index_ty value
          in
          let* low = bound low in
          let* high = bound high in
          let low = widen low and high = widen high in
          let compare = if unsigned then Int64.unsigned_compare else Int64.compare in
          let count = if compare low high >= 0 then 0L else Int64.sub high low in
          let budget = limits.Limits.max_const_loop_iterations in
          if Int64.unsigned_compare count (Int64.of_int budget) > 0 then
            error span
              (Printf.sprintf
                 "`for const` loop has %s iterations: budget max_const_loop_iterations \
                  of %d (profile %s)"
                 (Sema_numeric.unsigned_int64_to_string count)
                 budget
                 (Limits.budget_profile_name limits))
          else
            let copy_nodes = Ast.count_stmt_nodes body in
            let* () =
              if
                Int64.unsigned_compare count
                  (Int64.of_int
                     ((limits.Limits.max_ast_nodes - !expanded_nodes) / max 1 copy_nodes))
                > 0
              then
                error span
                  (Printf.sprintf
                     "cumulative expanded AST nodes exceed budget max_ast_nodes of %d \
                      (profile %s)"
                     limits.Limits.max_ast_nodes
                     (Limits.budget_profile_name limits))
              else (
                expanded_nodes := !expanded_nodes + (Int64.to_int count * copy_nodes);
                Ok ())
            in
            let render value =
              if unsigned then Sema_numeric.unsigned_int64_to_string value
              else Int64.to_string value
            in
            let copy offset =
              let wide = Int64.add low (Int64.of_int offset) in
              let value = Sema_numeric.mask_value index_ty wide in
              let note = Printf.sprintf "in iteration `%s = %s`" name (render wide) in
              let* body =
                with_local_values (String_set.remove name !local_values) (fun () ->
                    resolve_statements
                      (resolve_stmt
                         ~values:((name, index_ty, value) :: values)
                         ~defer_const_structs substitutions depth)
                      body)
                |> Result.map_error
                     (List.map (fun (diagnostic : Diag.t) ->
                          { diagnostic with notes = diagnostic.notes @ [ note ] }))
              in
              Ok (Ast.Const_copy { name; ty; value; note; body; span })
            in
            let* copies =
              Result_list.map copy (List.init (Int64.to_int count) Fun.id)
            in
            Ok (Ast.Block (copies, span))
      | Ast.Const_copy { name; ty; value; note; body; span } ->
          let* index_ty = source_ty_diag [] span ty in
          let* body =
            with_local_values (String_set.remove name !local_values) (fun () ->
                resolve_statements
                  (resolve_stmt
                     ~values:((name, index_ty, value) :: values)
                     ~defer_const_structs substitutions depth)
                  body)
          in
          Ok (Ast.Const_copy { name; ty; value; note; body; span })
      | Ast.Defer (body, span) ->
          let* body =
            with_local_values !local_values (fun () ->
                resolve_statements
                  (resolve_stmt ~values ~defer_const_structs substitutions depth)
                  body)
          in
          Ok (Ast.Defer (body, span))
      | Ast.Expr_stmt (expression, span, finish) ->
          let* expression =
            resolve_expr ~values ~defer_const_structs substitutions depth expression
          in
          Ok (Ast.Expr_stmt (expression, span, finish))
      | Ast.Block (body, span) ->
          let* body =
            with_local_values !local_values (fun () ->
                resolve_statements
                  (resolve_stmt ~values ~defer_const_structs substitutions depth)
                  body)
          in
          Ok (Ast.Block (body, span))
      | Ast.For (label, init, condition, step, body, span) ->
          with_local_values !local_values (fun () ->
              let resolve_optional resolve = function
                | None -> Ok None
                | Some value ->
                    let* value = resolve value in
                    Ok (Some value)
              in
              let resolve_stmt =
                resolve_stmt ~values ~defer_const_structs substitutions depth
              in
              let resolve_expr =
                resolve_expr ~values ~defer_const_structs substitutions depth
              in
              let* init = resolve_optional resolve_stmt init in
              let* condition = resolve_optional resolve_expr condition in
              let* step = resolve_optional resolve_stmt step in
              let* body =
                with_local_values !local_values (fun () ->
                    resolve_statements resolve_stmt body)
              in
              Ok (Ast.For (label, init, condition, step, body, span)))
      | Ast.Switch (expression, cases, default, span) ->
          let resolve_expr =
            resolve_expr ~values ~defer_const_structs substitutions depth
          in
          let resolve_stmt =
            resolve_stmt ~values ~defer_const_structs substitutions depth
          in
          let* expression = resolve_expr expression in
          let* cases =
            Result_list.map
              (fun (values, body) ->
                let* values = Result_list.map resolve_expr values in
                let* body =
                  with_local_values !local_values (fun () ->
                      resolve_statements resolve_stmt body)
                in
                Ok (values, body))
              cases
          in
          let* default =
            match default with
            | None -> Ok None
            | Some body ->
                let* body =
                  with_local_values !local_values (fun () ->
                      resolve_statements resolve_stmt body)
                in
                Ok (Some body)
          in
          Ok (Ast.Switch (expression, cases, default, span))
    in
    (match statement with
    | Ast.Let _ | Ast.View _ -> ()
    | _ ->
        local_values := previous;
        shadowed_constants := previous_shadowed_constants);
    result
  and resolve_function ?(values = []) substitutions depth specialization_name = function
    | Ast.Func ({ params; ret; body; generic_params; span; _ } as item) ->
        with_generic_type_names (type_param_names generic_params) (fun () ->
            let body_type_names =
              type_param_names generic_params |> String_set.of_list
            in
            let body_value_names =
              List.map (fun (parameter : Ast.param) -> parameter.name) params
              @ List.map
                  (fun (parameter : Ast.const_param) -> parameter.name)
                  (const_params generic_params)
              |> String_set.of_list
            in
            let previous_local_values = !local_values in
            let previous_shadowed_constants = !shadowed_constants in
            local_values := body_value_names;
            shadowed_constants :=
              List.map (fun (parameter : Ast.param) -> parameter.name) params
              |> String_set.of_list;
            let defer_const_structs = values = [] && has_const_params generic_params in
            let* params =
              Result_list.map
                (fun (parameter : Ast.param) ->
                  let* ty =
                    resolve_ty ~values ~defer_const_structs substitutions depth
                      parameter.span parameter.ty
                  in
                  Ok ({ parameter with ty } : Ast.param))
                params
            in
            set_inference_params params;
            let* ret =
              resolve_ty ~values ~defer_const_structs substitutions depth span ret
            in
            let* generic_params =
              Result_list.map
                (function
                  | Ast.Type_param _ -> Ok None
                  | Ast.Const_param parameter ->
                      let* ty =
                        resolve_ty ~values ~defer_const_structs substitutions depth
                          parameter.span parameter.ty
                      in
                      Ok (Some (Ast.Const_param { parameter with ty })))
                generic_params
            in
            let generic_params = List.filter_map Fun.id generic_params in
            List.iter
              (function
                | Ast.Const_param parameter ->
                    set_inference_type parameter.name (Some parameter.ty)
                | Ast.Type_param _ -> ())
              generic_params;
            let* body =
              match body with
              | Ast.Declaration -> Ok Ast.Declaration
              | Ast.Statements statements ->
                  let* () =
                    validate_statement_block_names
                      ~scope_names:(String_set.union body_value_names body_type_names)
                      body_value_names body_type_names statements
                  in
                  let* () =
                    let validation_params =
                      List.map2
                        (fun (original : Ast.param) (resolved : Ast.param) ->
                          match (original.ty, resolved.ty) with
                          | Ast.Vec _, Ast.Vec _ -> resolved
                          | _ -> original)
                        item.params params
                    in
                    if (not eager_functions) || item.generic_params = [] then Ok ()
                    else
                      validate_generic_legality ~span:item.span ~ret:item.ret
                        ~params:validation_params ~generic_params:item.generic_params
                        statements
                  in
                  let* statements =
                    resolve_statements
                      (resolve_stmt ~values ~defer_const_structs substitutions depth)
                      statements
                  in
                  Ok (Ast.Statements statements)
            in
            local_values := previous_local_values;
            shadowed_constants := previous_shadowed_constants;
            inference_types := [];
            inference_raw_view_types := [];
            Ok
              (Ast.Func
                 {
                   item with
                   name = specialization_name;
                   params;
                   ret;
                   body;
                   linkage = Ast.Internal;
                   variadic = false;
                   generic_params;
                 }))
    | _ -> error Span.synthetic "internal error: function specialization is malformed"
  and resolve_item = function
    | (Ast.Use _ | Ast.Pkg _) as item -> Ok item
    | Ast.Struct { generic_params = _ :: _; _ } as item -> Ok item
    | Ast.Struct ({ fields; _ } as item) ->
        let* fields =
          Result_list.map
            (fun (field : Ast.field) ->
              let* ty =
                resolve_ty ~values:eval_consts [] 0 field.span field.ty
                |> Sema_types.declaration_type_error ~declaration:"fields"
                     ~variable:field.name field.ty
              in
              Ok ({ field with ty } : Ast.field))
            fields
        in
        Ok (Ast.Struct { item with fields })
    | Ast.Opaque _ as item -> Ok item
    | Ast.Const ({ ty; value; ty_span; _ } as item) ->
        let* ty = resolve_ty ~values:eval_consts [] 0 ty_span ty in
        let* value =
          with_preserved_layout_queries (fun () -> resolve_expr [] 0 value)
        in
        Ok (Ast.Const { item with ty; value })
    | Ast.Global ({ ty; init; ty_span; _ } as item) ->
        let* ty = resolve_ty ~values:eval_consts [] 0 ty_span ty in
        let* init =
          match init with
          | None -> Ok None
          | Some value ->
              let* value =
                with_preserved_layout_queries (fun () -> resolve_expr [] 0 value)
              in
              Ok (Some value)
        in
        Ok (Ast.Global { item with ty; init })
    | Ast.Func { generic_params = _ :: _; _ } as item -> Ok item
    | Ast.Func ({ params; ret; body; generic_params; span; _ } as item) ->
        let defer_const_structs = not eager_functions in
        let* params =
          Result_list.map
            (fun (parameter : Ast.param) ->
              let* ty =
                resolve_ty ~defer_const_structs [] 0 parameter.span parameter.ty
                |> Sema_types.declaration_type_error ~declaration:"parameters"
                     ~variable:parameter.name parameter.ty
              in
              Ok ({ parameter with ty } : Ast.param))
            params
        in
        let* ret = resolve_ty ~defer_const_structs [] 0 span ret in
        let* generic_params =
          Result_list.map
            (function
              | Ast.Type_param _ as parameter -> Ok parameter
              | Ast.Const_param parameter ->
                  let* ty =
                    resolve_ty ~defer_const_structs [] 0 parameter.span parameter.ty
                  in
                  Ok (Ast.Const_param { parameter with ty }))
            generic_params
        in
        set_inference_params params;
        let* body =
          match body with
          | Ast.Declaration -> Ok Ast.Declaration
          | Ast.Statements statements -> (
              let resolved =
                let body_value_names =
                  List.map (fun (parameter : Ast.param) -> parameter.name) params
                  |> String_set.of_list
                in
                let* () =
                  validate_statement_block_duplicates ~scope_names:body_value_names
                    statements
                in
                let* statements =
                  with_local_values body_value_names (fun () ->
                      shadowed_constants := body_value_names;
                      resolve_statements
                        (resolve_stmt ~defer_const_structs [] 0)
                        statements)
                in
                Ok (Ast.Statements statements)
              in
              match (resolved, recovered) with
              | Error diagnostics, Some recorded
                when List.for_all Sema_flow.recoverable diagnostics ->
                  recorded := List.rev_append diagnostics !recorded;
                  Ok Ast.Declaration
              | resolved, _ -> resolved)
        in
        inference_types := [];
        inference_raw_view_types := [];
        Ok (Ast.Func { item with params; ret; body; generic_params })
  in
  let with_current_trace trace f =
    let previous = !current_trace in
    current_trace := trace;
    let result = f () in
    current_trace := previous;
    result
  in
  let resolve_program_item = function
    | Ast.Func { name; generic_params = []; _ } as item ->
        let trace = specialization_trace specializations Function_specialization name in
        with_current_trace trace (fun () -> resolve_item item)
    | item -> resolve_item item
  in
  let* items = Result_list.map resolve_program_item program.Ast.items in
  let late_functions = ref [] in
  let rec materialize () =
    match Sema_specialization.take_pending specializations with
    | None -> Ok ()
    | Some
        ({
           payload = Struct_payload { template; substitutions; values };
           name;
           depth;
           _;
         } as specialization) ->
        let result =
          with_current_trace specialization.trace (fun () ->
              match template with
              | Ast.Struct
                  { name_span; fields; align; size; span; generic_params; is_union; _ }
                ->
                  let* fields =
                    with_generic_type_names (type_param_names generic_params) (fun () ->
                        Result_list.map
                          (fun (field : Ast.field) ->
                            let* ty =
                              resolve_ty ~values substitutions (depth + 1) field.span
                                field.ty
                            in
                            Ok ({ field with ty } : Ast.field))
                          fields)
                  in
                  let generated_item =
                    Ast.Struct
                      {
                        name;
                        name_span;
                        generic_params = [];
                        fields;
                        align;
                        size;
                        is_union;
                        span;
                        extent = span;
                      }
                  in
                  let* () =
                    charge_expanded_item type_node_account
                      ~span:(specialization_application_span specialization)
                      ~where:(Printf.sprintf "struct specialization `%s`" name)
                      generated_item
                  in
                  generated := generated_item :: !generated;
                  Ok ()
              | _ ->
                  error Span.synthetic
                    "internal error: struct specialization is malformed")
          |> trace_result specializations specialization.trace
        in
        let* () = result in
        materialize ()
    | Some
        ({
           payload =
             Function_payload { item; substitutions; values = []; staged_args = _ };
           name;
           depth;
           _;
         } as specialization)
      when substitutions <> [] ->
        let* item =
          with_current_trace specialization.trace (fun () ->
              resolve_function substitutions (depth + 1) name item)
          |> trace_result specializations specialization.trace
        in
        let* () =
          charge_expanded_item type_node_account
            ~span:(specialization_application_span specialization)
            ~where:(Printf.sprintf "function specialization `%s`" name)
            item
        in
        generated := item :: !generated;
        materialize ()
    | Some
        ({
           payload =
             Function_payload { item; substitutions = []; values; staged_args = _ };
           name;
           depth;
           _;
         } as specialization)
      when eager_functions && values <> [] ->
        let* item =
          with_current_trace specialization.trace (fun () ->
              resolve_function ~values [] (depth + 1) name item)
          |> trace_result specializations specialization.trace
        in
        let* () =
          charge_expanded_item type_node_account
            ~span:(specialization_application_span specialization)
            ~where:(Printf.sprintf "function specialization `%s`" name)
            item
        in
        let specialization =
          {
            specialization with
            payload =
              Function_payload { item; substitutions = []; values; staged_args = None };
          }
        in
        Sema_specialization.update_materialized specializations specialization;
        late_functions := specialization :: !late_functions;
        materialize ()
    | Some { payload = Function_payload _; _ } ->
        error Span.synthetic "internal error: const specialization was queued too early"
  in
  let* () = materialize () in
  List.iter
    (Sema_specialization.requeue_materialized specializations)
    (List.rev !late_functions);
  Ok ({ Ast.items = items @ List.rev !generated } : Ast.program)
