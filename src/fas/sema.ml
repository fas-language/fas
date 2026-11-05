open Sema_numeric
open Sema_specialization
open Sema_types
open Sema_context
module String_set = Set.Make (String)
module String_map = Map.Make (String)

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let phase_invariant = function
  | Ok () -> Ok ()
  | Error message -> error Span.synthetic ("internal error: " ^ message)

let validate_generic_params named_types params =
  let* () =
    Result_list.iter
      (function
        | Ast.Type_param { name; span } -> validate_binding_name span name
        | Ast.Const_param cp ->
            let* () = validate_binding_name cp.span cp.name in
            let* t = source_ty_diag named_types cp.ty_span cp.ty in
            if t = Hir.Bool || is_int t then Ok ()
            else error cp.span "const parameter type must be a scalar integer or bool")
      params
  in
  let rec dup seen = function
    | [] -> Ok ()
    | Ast.Type_param { name; span } :: rest | Ast.Const_param { name; span; _ } :: rest
      ->
        if List.mem name seen then
          error span (Printf.sprintf "duplicate generic parameter `%s`" name)
        else dup (name :: seen) rest
  in
  dup [] params

let validate_function_params generic_params params =
  let generic_names =
    List.map
      (function Ast.Type_param { name; _ } | Ast.Const_param { name; _ } -> name)
      generic_params
  in
  let rec validate seen = function
    | [] -> Ok ()
    | (parameter : Ast.param) :: rest ->
        let* () = validate_binding_name parameter.span parameter.name in
        if List.mem parameter.name generic_names then
          error parameter.span
            (Printf.sprintf "parameter `%s` conflicts with a generic parameter"
               parameter.name)
        else if List.mem parameter.name seen then
          error parameter.span
            (Printf.sprintf "duplicate parameter `%s`" parameter.name)
        else validate (parameter.name :: seen) rest
  in
  validate [] params

let extern_c_value_type = function
  | Hir.Bool | Hir.Int _ | Hir.Float _ | Hir.Addr | Hir.Handle _ -> true
  | Hir.Void | Hir.Array _ | Hir.Vec _ | Hir.Struct _ | Hir.Opaque _ -> false

let aggregate_value_type = function Hir.Array _ | Hir.Struct _ -> true | _ -> false
let diagnostic_source_ty_name ty = Ast.type_name ty

let aggregate_parameter_error span name ty_name =
  error span
    (Printf.sprintf
       "aggregate parameter `%s` of type `%s` cannot be passed by value; declare `%s` \
        as `addr`; callers pass its address with `&`"
       name ty_name name)

let aggregate_result_error span ty_name =
  error span
    (Printf.sprintf
       "aggregate result `%s` cannot be returned by value; return `void` and take the \
        destination as an `addr` parameter"
       ty_name)

let extern_c_handle_name named_types = function
  | Ast.Named_type (name, _) -> (
      match List.assoc_opt name named_types with
      | Some (C_record_name (_, None)) -> Some name
      | _ -> None)
  | _ -> None

let extern_c_aggregate_parameter_error span name ty_name handle_name =
  let handle_suffix =
    match handle_name with
    | Some name -> Printf.sprintf " or `handle[%s]`" name
    | None -> ""
  in
  error span
    (Printf.sprintf
       "aggregate parameter `%s` of type `%s` cannot be passed by value; declare `%s` \
        as `addr`%s"
       name ty_name name handle_suffix)

let extern_c_aggregate_result_error span ty_name handle_name =
  let handle_suffix =
    match handle_name with
    | Some name -> Printf.sprintf " or `handle[%s]`" name
    | None -> ""
  in
  error span
    (Printf.sprintf
       "aggregate result `%s` cannot be returned by value; return `void` and take the \
        destination as an `addr`%s parameter"
       ty_name handle_suffix)

let extern_c_struct_value_error span name =
  error span
    (Printf.sprintf
       "C functions that take or return struct `%s` by value cannot be imported" name)

let always_aggregate_type named_types generic_params ty =
  let generic_type_names =
    List.filter_map
      (function Ast.Type_param { name; _ } -> Some name | _ -> None)
      generic_params
  in
  let rec is_aggregate = function
    | Ast.Array _ -> true
    | Ast.Named_type (name, _) when List.mem name generic_type_names -> false
    | Ast.Named_type (name, _) -> (
        match List.assoc_opt name named_types with
        | Some Struct_name | Some (C_record_name (_, None)) -> true
        | Some (Alias_name ty) -> is_aggregate ty
        | _ -> false)
    | Ast.Applied_type (name, _, _, _) ->
        List.assoc_opt name named_types = Some Generic_struct_name
    | _ -> false
  in
  is_aggregate ty

let validate_native_aggregate_signature span params converted ret =
  let rec validate_params params converted =
    match (params, converted) with
    | [], [] -> Ok ()
    | (param : Ast.param) :: param_rest, (_, ty) :: converted_rest ->
        if aggregate_value_type ty then
          aggregate_parameter_error param.ty_span param.name
            (diagnostic_source_ty_name param.ty)
        else validate_params param_rest converted_rest
    | _ -> error span "internal error: parameter list mismatch"
  in
  let* () = validate_params params converted in
  if aggregate_value_type ret then
    aggregate_result_error span (Sema_types.diagnostic_ty_name ret)
  else Ok ()

let validate_extern_c_signature ~named_types ~ret_source ~bodyless span params converted
    ret =
  let rec validate_params params converted =
    match (params, converted) with
    | [], [] -> Ok ()
    | (param : Ast.param) :: param_rest, (_, ty) :: converted_rest ->
        if extern_c_value_type ty then validate_params param_rest converted_rest
        else if aggregate_value_type ty then
          match (bodyless, ty) with
          | true, Hir.Struct name -> extern_c_struct_value_error param.ty_span name
          | _ ->
              extern_c_aggregate_parameter_error param.ty_span param.name
                (diagnostic_source_ty_name param.ty)
                (extern_c_handle_name named_types param.ty)
        else
          error param.ty_span
            (Printf.sprintf
               "extern \"C\" parameter `%s` cannot use `%s` by value; use a pointer"
               param.name (Hir.ty_name ty))
    | _ -> error span "internal error: extern parameter list mismatch"
  in
  let* () = validate_params params converted in
  if ret = Hir.Void || extern_c_value_type ret then Ok ()
  else if aggregate_value_type ret then
    match (bodyless, ret) with
    | true, Hir.Struct name -> extern_c_struct_value_error span name
    | _ ->
        extern_c_aggregate_result_error span
          (Sema_types.diagnostic_ty_name ret)
          (extern_c_handle_name named_types ret_source)
  else
    error span
      (Printf.sprintf "extern \"C\" cannot return `%s` by value; use an output pointer"
         (Hir.ty_name ret))

let check_program ?(limits = Limits.default) ?(c_aliases = []) ?(c_unsupported = [])
    ?(c_nonnull_parameters = []) ?(c_string_parameters = []) ?(c_record_calls = [])
    ?(c_alloc_size_parameters = []) ?(c_records = []) ?(all_errors = false)
    ?specializations ?index program =
  let global_names =
    List.filter_map
      (function Ast.Global { name; _ } -> Some name | _ -> None)
      program.Ast.items
  in
  let* () =
    if limits.Limits.max_type_nodes >= 0 then Ok ()
    else
      error Span.synthetic
        (Printf.sprintf "budget max_type_nodes must not be negative (profile %s)"
           (Limits.budget_profile_name limits))
  in
  let type_node_account = create_type_node_account limits in
  let recovered = if all_errors then Some (ref []) else None in
  let specializations =
    Option.value specializations ~default:(Sema_specialization.create ())
  in
  let declaration = function
    | Ast.Use _ | Ast.Pkg _ -> None
    | Ast.Opaque { name; span; _ } -> Some (name, Top_type, span)
    | Ast.Struct { name; name_span; _ } -> Some (name, Top_type, name_span)
    | Ast.Const { name; name_span; _ } -> Some (name, Top_const, name_span)
    | Ast.Global { name; span; _ } -> Some (name, Top_global, span)
    | Ast.Func { name; name_span; _ } -> Some (name, Top_function, name_span)
  in
  let rec validate_declarations next_id seen bindings = function
    | [] -> Ok (List.rev bindings)
    | Ast.Global { name = "main"; span; _ } :: _ ->
        error span "global `main` cannot be the program entry point"
    | item :: rest -> (
        match declaration item with
        | None -> validate_declarations next_id seen bindings rest
        | Some (name, kind, span) -> (
            let* () = validate_binding_name span name in
            match String_map.find_opt name seen with
            | None ->
                let binding =
                  {
                    declaration_id = next_id;
                    declaration_name = name;
                    declaration_kind = kind;
                  }
                in
                validate_declarations (next_id + 1)
                  (String_map.add name (kind, span) seen)
                  (binding :: bindings) rest
            | Some (previous, first_span) ->
                let previous_definition_note =
                  "first definition is at " ^ Span.to_string first_span
                in
                let duplicate message =
                  Error [ Diag.error ~notes:[ previous_definition_note ] span message ]
                in
                if previous = kind then
                  let label =
                    match kind with
                    | Top_type -> "type"
                    | Top_const -> "const"
                    | Top_function -> "function"
                    | Top_global -> "global"
                  in
                  duplicate
                    (if kind = Top_function then
                       Printf.sprintf "duplicate function `%s`" name
                     else Printf.sprintf "duplicate %s `%s`" label name)
                else duplicate (Printf.sprintf "duplicate declaration `%s`" name)))
  in
  let* top_level_bindings =
    validate_declarations 0 String_map.empty [] program.Ast.items
  in
  Option.iter
    (fun index ->
      Sema_index.set_struct_source index
        (Sema_specialization.specialization_source_name specializations
           Struct_specialization);
      Sema_index.declare_program index ~c_aliases:(List.map fst c_aliases) program)
    index;
  let source_program = program in
  let* () =
    Sema_invariants.check_declarations
      (List.map
         (fun binding -> (binding.declaration_id, binding.declaration_name))
         top_level_bindings)
    |> phase_invariant
  in
  let rec collect_named_types seen acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Opaque { name; span; _ } :: rest ->
        if String_set.mem name seen then
          error span (Printf.sprintf "duplicate type `%s`" name)
        else
          collect_named_types (String_set.add name seen) ((name, Opaque_name) :: acc)
            rest
    | Ast.Struct { name; name_span; generic_params; _ } :: rest ->
        if String_set.mem name seen then
          error name_span (Printf.sprintf "duplicate type `%s`" name)
        else
          let kind = if generic_params = [] then Struct_name else Generic_struct_name in
          collect_named_types (String_set.add name seen) ((name, kind) :: acc) rest
    | _ :: rest -> collect_named_types seen acc rest
  in
  let add_c_types named_types =
    let seen = Hashtbl.create (List.length named_types + List.length c_aliases) in
    List.iter (fun (name, _) -> Hashtbl.replace seen name ()) named_types;
    let extras =
      List.map (fun (name, ty) -> (name, Alias_name ty)) c_aliases
      @ List.map
          (fun (name, reason) -> (name, Unsupported_name (name, reason)))
          c_unsupported
      |> List.filter (fun (name, _) ->
          if Hashtbl.mem seen name then false
          else (
            Hashtbl.add seen name ();
            true))
    in
    named_types @ extras
  in
  let add_c_records named_types =
    let seen = Hashtbl.create (List.length c_records) in
    let records =
      List.rev c_records
      |> List.filter_map (fun (name, record, reason) ->
          if Hashtbl.mem seen name then None
          else (
            Hashtbl.add seen name ();
            Some (name, C_record_name (record, reason))))
    in
    let named_types =
      List.filter
        (fun (name, _) ->
          if Hashtbl.mem seen name then (
            Hashtbl.remove seen name;
            false)
          else true)
        named_types
    in
    records @ named_types
  in
  let* named_types = collect_named_types String_set.empty [] program.Ast.items in
  let named_types = add_c_records (add_c_types named_types) in
  let struct_layout_metadata items =
    let structs =
      List.filter_map
        (function
          | Ast.Struct { name; fields; size; _ } -> Some (name, fields, size)
          | _ -> None)
        items
    in
    let field_offsets =
      List.filter_map
        (fun (name, fields, _) ->
          let offsets =
            List.filter_map
              (fun (field : Ast.field) ->
                Option.map (fun offset -> (field.name, offset)) field.offset)
              fields
          in
          if offsets = [] then None else Some (name, offsets))
        structs
    and field_reasons =
      List.filter_map
        (fun (name, fields, _) ->
          let reasons =
            List.filter_map
              (fun (field : Ast.field) ->
                Option.map (fun reason -> (field.name, reason)) field.unsupported_reason)
              fields
          in
          if reasons = [] then None else Some (name, reasons))
        structs
    and struct_sizes =
      List.filter_map
        (fun (name, _, size) -> Option.map (fun size -> (name, size)) size)
        structs
    in
    let byte_storage = List.map fst field_offsets in
    (field_offsets, field_reasons, byte_storage, struct_sizes)
  in
  let base_field_offsets, base_field_reasons, base_byte_storage, base_struct_sizes =
    struct_layout_metadata program.Ast.items
  in
  let* () =
    Result_list.iter
      (function
        | Ast.Struct { generic_params; _ } ->
            validate_generic_params named_types generic_params
        | Ast.Func { generic_params; params; _ } ->
            let* () = validate_generic_params named_types generic_params in
            validate_function_params generic_params params
        | _ -> Ok ())
      program.Ast.items
  in
  let base_structs_src =
    List.filter_map
      (function
        | Ast.Struct { name; generic_params = []; fields; align; _ } -> (
            let fields =
              Result_list.map
                (fun (field : Ast.field) ->
                  let* ty = source_ty named_types field.ty in
                  Ok (field.name, ty))
                fields
            in
            match fields with
            | Ok fields -> Some (name, fields, align)
            | Error _ -> None)
        | _ -> None)
      program.Ast.items
  in
  let base_union_names =
    List.filter_map
      (function Ast.Struct { name; is_union = true; _ } -> Some name | _ -> None)
      program.Ast.items
  in
  let base_struct_cache =
    Hir.struct_layout_cache ~unions:base_union_names ~field_offsets:base_field_offsets
      ~field_reasons:base_field_reasons ~byte_storage:base_byte_storage
      ~struct_sizes:base_struct_sizes base_structs_src
  in
  let base_structs =
    List.filter_map
      (fun (name, _, _) ->
        match Hir.compute_struct_cached base_struct_cache name with
        | Ok definition -> Some definition
        | Error _ -> None)
      base_structs_src
  in
  let symbolic_names = Sema_static_env.names ~named_types program.Ast.items in
  let ordinary_program = Sema_static_env.ordinary_program symbolic_names program in
  let* early_consts =
    Sema_constants.resolve_scalar_declarations
      ~array_lengths:(Sema_static_env.source_array_lengths program.Ast.items)
      ~globals:global_names ~structs:base_structs ~named_types
      ~generic_structs:program.Ast.items
      ~resolve_type:(fun span ty -> source_ty_diag named_types span ty)
      ~strict:false ordinary_program.Ast.items
  in
  let* program =
    Sema_substitution.monomorphize_types ~check_expr:Sema_check.check_expr
      ~check_stmt:Sema_check.check_stmt ~check_target:Sema_check.check_target
      ~target_ty:Sema_check.target_ty
      ~generic_const_argument:Sema_check.generic_const_argument
      ~array_lengths:(Sema_static_env.source_array_lengths program.Ast.items)
      ~eval_context:(base_structs, named_types, early_consts, [])
      ~c_aliases ~c_unsupported ~c_nonnull_parameters ~c_alloc_size_parameters
      ~c_string_parameters ~c_record_calls ~top_level_bindings ?recovered ~limits
      ~type_node_account specializations program
  in
  let* named_types = collect_named_types String_set.empty [] program.Ast.items in
  let named_types = add_c_records (add_c_types named_types) in
  let union_names =
    List.filter_map
      (function Ast.Struct { name; is_union = true; _ } -> Some name | _ -> None)
      program.Ast.items
  in
  let field_offsets, field_reasons, byte_storage, struct_sizes =
    struct_layout_metadata program.Ast.items
  in
  let rec collect_structs named_types acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Struct { generic_params = _ :: _; align; span; _ } :: rest ->
        let* () = Sema_limits.validate_struct_alignment limits span align in
        collect_structs named_types acc rest
    | Ast.Struct { name; fields; align; span; _ } :: rest ->
        let result =
          let* () = Sema_limits.validate_struct_alignment limits span align in
          let rec collect_fields seen out = function
            | [] -> Ok (List.rev out)
            | (f : Ast.field) :: fields ->
                if String_set.mem f.name seen then
                  error f.span (Printf.sprintf "duplicate field `%s`" f.name)
                else
                  let* ty = source_ty_diag named_types f.ty_span f.ty in
                  collect_fields (String_set.add f.name seen) ((f.name, ty) :: out)
                    fields
          in
          collect_fields String_set.empty [] fields
        in
        let* fields =
          result
          |> trace_result specializations
               (specialization_trace specializations Struct_specialization name)
        in
        collect_structs named_types ((name, fields, align) :: acc) rest
    | _ :: rest -> collect_structs named_types acc rest
  in
  let* structs_src = collect_structs named_types [] program.Ast.items in
  let build structs_src =
    let rec mentions_type target = function
      | Ast.Named_type (name, _) -> name = target
      | Ast.Handle ty | Ast.Array (_, ty) | Ast.Vec (_, ty) -> mentions_type target ty
      | Ast.Applied_type (_, _, arguments, _) ->
          List.exists
            (function
              | Ast.Type_arg ty | Ast.Type_or_index ty -> mentions_type target ty
              | _ -> false)
            arguments
      | _ -> false
    in
    let rec contains_void = function
      | Ast.Void -> true
      | Ast.Handle ty | Ast.Array (_, ty) | Ast.Vec (_, ty) -> contains_void ty
      | _ -> false
    in
    let layout_error_field ?overflow_field message =
      let overflow_field =
        match (message, overflow_field) with
        | "aggregate size overflows", Some (struct_name, field_name) ->
            program.items
            |> List.find_map (function
              | Ast.Struct { name; fields; _ } when name = struct_name ->
                  List.find_opt
                    (fun (field : Ast.field) -> field.name = field_name)
                    fields
              | _ -> None)
        | _ -> None
      in
      let target =
        let extract prefix =
          if not (String.starts_with ~prefix message) then None
          else
            let start = String.length prefix in
            match String.index_from_opt message start '`' with
            | Some stop -> Some (String.sub message start (stop - start))
            | None -> None
        in
        match extract "opaque type `" with
        | Some _ as name -> name
        | None -> extract "recursive by-value struct `"
      in
      let field_matches (field : Ast.field) =
        if message = "void has no object layout" then contains_void field.ty
        else
          Option.fold ~none:false ~some:(fun name -> mentions_type name field.ty) target
      in
      match overflow_field with
      | Some _ as field -> field
      | None ->
          program.items
          |> List.find_map (function
            | Ast.Struct { fields; _ } ->
                List.find_map
                  (fun (field : Ast.field) ->
                    if field_matches field then Some field else None)
                  fields
            | _ -> None)
    in
    let layout_error_diagnostic ?overflow_field message =
      match layout_error_field ?overflow_field message with
      | Some field when message = "void has no object layout" ->
          Diag.error field.ty_span
            (Printf.sprintf "field `%s` cannot have type `void`" field.name)
      | Some field when message = "aggregate size overflows" ->
          Diag.error field.span message
      | Some field ->
          let opaque_name =
            if String.starts_with ~prefix:"opaque type `" message then
              let start = String.length "opaque type `" in
              Option.map
                (fun stop -> String.sub message start (stop - start))
                (String.index_from_opt message start '`')
            else None
          in
          let primary =
            Option.value ~default:field.ty_span
              (Sema_types.opaque_source_span named_types [] field.ty)
          in
          Diag.error
            ?help:(Option.map (Printf.sprintf "write `handle[%s]`") opaque_name)
            primary message
      | None -> Diag.error Span.synthetic message
    in
    let cache =
      Hir.struct_layout_cache ~unions:union_names ~field_offsets ~field_reasons
        ~byte_storage ~struct_sizes structs_src
    in
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | (name, _, _) :: xs ->
          let overflow_field = ref None in
          let* s =
            Hir.compute_struct_cached ~overflow_field cache name
            |> Result.map_error (fun m ->
                [ layout_error_diagnostic ?overflow_field:!overflow_field m ])
            |> trace_result specializations
                 (specialization_trace specializations Struct_specialization name)
          in
          go (s :: acc) xs
    in
    go [] structs_src
  in
  let* structs = build structs_src in
  let source_obj span source_type =
    let* t = source_ty_diag named_types span source_type in
    Sema_limits.validate_object
      ?opaque_span:(Sema_types.opaque_source_span named_types [] source_type)
      ?vector_element_span:(Sema_types.vector_element_source_span span source_type)
      limits structs span t
  in
  let map_params convert params =
    Result_list.map
      (fun (param : Ast.param) ->
        let* ty = convert param in
        Ok (param.name, ty))
      params
  in
  let symbolic_names = Sema_static_env.names ~named_types program.Ast.items in
  let ordinary_program = Sema_static_env.ordinary_program symbolic_names program in
  let* scalar_consts =
    Sema_constants.resolve_scalar_declarations
      ~array_lengths:(Sema_static_env.source_array_lengths program.Ast.items)
      ~globals:global_names ~generic_structs:program.Ast.items ~structs ~named_types
      ~resolve_type:source_obj ~strict:true ordinary_program.Ast.items
  in
  let* consts_ordered, arrays_ordered, arrays_names =
    Sema_const_env.collect
      ~array_lengths:(Sema_static_env.source_array_lengths program.Ast.items)
      ~generic_structs:program.Ast.items ~global_names ~source_obj ~structs ~named_types
      ~scalar_consts ordinary_program
  in
  let* () =
    Sema_invariants.check_const_environment
      ~declared:
        (List.filter_map
           (function Ast.Const { name; _ } -> Some name | _ -> None)
           ordinary_program.Ast.items)
      ~early:(List.map (fun (name, _, _) -> name) scalar_consts)
      ~consts:(List.map (fun (name, _, _) -> name) consts_ordered)
      ~arrays:(List.map (fun (name, _, _) -> name) arrays_ordered)
    |> phase_invariant
  in
  let* eval_globals =
    Sema_static_env.declarations ~source_obj symbolic_names program.items
  in
  let* program =
    Sema_substitution.monomorphize_types ~check_expr:Sema_check.check_expr
      ~check_stmt:Sema_check.check_stmt ~check_target:Sema_check.check_target
      ~target_ty:Sema_check.target_ty
      ~generic_const_argument:Sema_check.generic_const_argument
      ~array_lengths:(Sema_static_env.source_array_lengths program.Ast.items)
      ~eval_context:(structs, named_types, consts_ordered, arrays_ordered)
      ~eval_globals ~c_aliases ~c_unsupported ~c_nonnull_parameters
      ~c_alloc_size_parameters ~c_string_parameters ~c_record_calls
      ~eager_functions:true ~top_level_bindings ?recovered ~limits ~type_node_account
      specializations program
  in
  let* named_types = collect_named_types String_set.empty [] program.Ast.items in
  let named_types = add_c_records (add_c_types named_types) in
  let* structs_src = collect_structs named_types [] program.Ast.items in
  let* structs = build structs_src in
  let validate_object span t = Sema_limits.validate_object limits structs span t in
  let source_obj span t =
    let* t = source_ty_diag named_types span t in
    validate_object span t
  in
  let source_return span t =
    let* t = source_ty_diag named_types span t in
    if t = Hir.Void then Ok t else validate_object span t
  in
  let program_strings = create_string_pool limits in
  let* storage_declarations =
    Sema_static_env.declarations ~source_obj symbolic_names program.items
  in
  let address_context =
    {
      structs;
      generic_structs = program.Ast.items;
      named_types;
      consts = consts_ordered;
      arrays = arrays_ordered;
      globals = storage_declarations;
      c_unsupported;
      c_nonnull_parameters;
      c_string_parameters;
      c_record_calls;
      record_result_call = false;
      record_call_checked = false;
      c_alloc_size_parameters;
      signatures = [];
      external_c_functions =
        List.filter_map
          (function
            | Ast.Func { name; linkage = Ast.External_c; _ } -> Some name | _ -> None)
          program.items;
      templates = [];
      top_level_bindings;
      specializations;
      spec_depth = 0;
      spec_trace = [];
      flow = Sema_flow.create ~initial_scope:false structs;
      handle_view_expansion = false;
      expression_depth = 0;
      unresolved_shapes = Hashtbl.create 0;
      unresolved_shapes_unique = false;
      string_pool = program_strings;
      ret_ty = Hir.Void;
      limits;
      index = None;
    }
  in
  let* globals =
    Sema_global_env.collect
      ~array_lengths:(Sema_static_env.source_array_lengths program.Ast.items)
      ~generic_structs:program.Ast.items ~source_obj ~structs ~named_types
      ~consts:consts_ordered ~arrays:arrays_ordered ~global_names
      ~address_value:(Sema_static_env.address_value address_context)
      ~readonly_names:symbolic_names
      (Sema_static_env.storage_items symbolic_names program.items)
  in
  let source_params =
    map_params (fun (param : Ast.param) -> source_obj param.ty_span param.ty)
  in
  let sigs = ref [] and declared_functions = ref String_set.empty in
  let validate_entry_signature span params ret =
    let valid_params =
      params = [] || params = [ ("argc", Hir.Int Hir.I32); ("argv", Hir.Addr) ]
    in
    if ret = Hir.Int Hir.I32 && valid_params then Ok ()
    else
      error span
        "entry point `main` must have signature `fn main() i32` or `fn main(argc i32, \
         argv addr) i32`"
  in
  let* () =
    List.fold_left
      (fun r item ->
        let* () = r in
        match item with
        | Ast.Func
            ({ name; params; ret; variadic; linkage; span; generic_params; _ } as func)
          ->
            let name_span = func.name_span in
            let ret_span = func.ret_span in
            let* () = validate_binding_name span name in
            if String_set.mem name !declared_functions then
              error name_span (Printf.sprintf "duplicate function `%s`" name)
            else if List.mem name arrays_names then
              error span (Printf.sprintf "duplicate declaration `%s`" name)
            else if name = "main" && generic_params <> [] then
              error span "entry point `main` cannot have generic parameters"
            else
              let () = declared_functions := String_set.add name !declared_functions in
              let* () = validate_function_params generic_params params in
              let* () =
                if linkage = Ast.External_c && generic_params <> [] then
                  error span
                    (if has_type_params generic_params then
                       "extern \"C\" functions cannot have type parameters"
                     else "extern \"C\" functions cannot have const parameters")
                else Ok ()
              in
              let* () = validate_generic_params named_types generic_params in
              let* () =
                if generic_params = [] then Ok ()
                else
                  match
                    List.find_opt
                      (fun (parameter : Ast.param) ->
                        always_aggregate_type named_types generic_params parameter.ty)
                      params
                  with
                  | Some parameter ->
                      aggregate_parameter_error parameter.ty_span parameter.name
                        (diagnostic_source_ty_name parameter.ty)
                  | None ->
                      if always_aggregate_type named_types generic_params ret then
                        aggregate_result_error ret_span (diagnostic_source_ty_name ret)
                      else Ok ()
              in
              if variadic && linkage <> Ast.External_c then
                error span "variadic functions require extern \"C\""
              else if generic_params <> [] then Ok ()
              else
                let* ps =
                  map_params
                    (fun (param : Ast.param) -> source_obj param.ty_span param.ty)
                    params
                in
                let* rt = source_return ret_span ret in
                let* () =
                  if name = "main" then validate_entry_signature span ps rt else Ok ()
                in
                let* () =
                  (if linkage = Ast.External_c then
                     validate_extern_c_signature ~named_types ~ret_source:ret
                       ~bodyless:(func.body = Ast.Declaration) ret_span params ps rt
                   else validate_native_aggregate_signature ret_span params ps rt)
                  |> trace_result specializations
                       (specialization_trace specializations Function_specialization
                          name)
                in
                sigs := (name, { params = ps; ret = rt; variadic }) :: !sigs;
                Ok ()
        | _ -> Ok ())
      (Ok ()) program.items
  in
  let sigs_ordered = List.rev !sigs in
  let templates =
    List.filter_map
      (fun item ->
        match item with
        | Ast.Func { name; generic_params = _ :: _; _ } -> Some (name, item)
        | _ -> None)
      program.items
  in
  let funcs = ref [] in
  let hir_linkage = function
    | Ast.External_c -> Hir.External_c
    | Ast.Internal -> Hir.Internal
  in
  let make_context ~extra_consts ~spec_depth ~spec_trace ~ret_ty =
    {
      structs;
      generic_structs = program.Ast.items;
      named_types;
      consts =
        (if extra_consts = [] then consts_ordered else extra_consts @ consts_ordered);
      arrays = arrays_ordered;
      globals =
        List.map
          (fun (global : Hir.global) ->
            ( global.name,
              global.ty,
              if global.readonly then Ast.Import_const_c else global.linkage ))
          globals;
      c_unsupported;
      c_nonnull_parameters;
      c_string_parameters;
      c_record_calls;
      record_result_call = false;
      record_call_checked = false;
      c_alloc_size_parameters;
      signatures = sigs_ordered;
      external_c_functions =
        List.filter_map
          (function
            | Ast.Func { name; linkage = Ast.External_c; _ } -> Some name | _ -> None)
          program.items;
      templates;
      top_level_bindings;
      specializations;
      spec_depth;
      spec_trace;
      flow = Sema_flow.create ~recover:all_errors ~initial_scope:false structs;
      handle_view_expansion = false;
      expression_depth = 0;
      unresolved_shapes = Hashtbl.create 0;
      unresolved_shapes_unique = false;
      string_pool = program_strings;
      ret_ty;
      limits;
      index;
    }
  in
  let hir_params params =
    List.mapi (fun id (name, ty) -> ({ Hir.name; ty; id } : Hir.local)) params
  in
  let own_loop_label = function
    | None -> []
    | Some (label : Ast.loop_label) -> [ (label.name, label.span) ]
  in
  let rec loop_labels_in_block statements =
    List.concat_map loop_labels_in_statement statements
  and loop_labels_in_statement = function
    | Ast.While (label, _, body, _) -> own_loop_label label @ loop_labels_in_block body
    | Ast.For (label, _, _, _, body, _) ->
        own_loop_label label @ loop_labels_in_block body
    | Ast.If (_, yes, no, _) ->
        loop_labels_in_block yes @ Option.fold ~none:[] ~some:loop_labels_in_block no
    | Ast.Defer (body, _)
    | Ast.Block (body, _)
    | Ast.Const_for { body; _ }
    | Ast.Const_copy { body; _ } ->
        loop_labels_in_block body
    | Ast.Switch (_, cases, default, _) ->
        List.concat_map (fun (_, body) -> loop_labels_in_block body) cases
        @ Option.fold ~none:[] ~some:loop_labels_in_block default
    | Ast.Let _ | Ast.View _ | Ast.Assign _ | Ast.Compound_assign _ | Ast.Return _
    | Ast.Break _ | Ast.Continue _ | Ast.Expr_stmt _ ->
        []
  in
  let display_name name =
    Sema_specialization.specialization_application_name specializations
      Function_specialization name
  in
  let check_function_body ~name ~diagnostic_name ~description ~span ~diagnostic_span
      ~params ~param_spans ~ret ~stmts ~linkage ~variadic ~extra_consts ~spec_depth
      ~spec_trace ~require_return =
    let context = make_context ~extra_consts ~spec_depth ~spec_trace ~ret_ty:ret in
    Option.iter
      (fun index ->
        let template, instance =
          match Sema_specialization.current_instantiation_frame spec_trace with
          | Some frame ->
              ( frame.template_name,
                List.map Sema_specialization.render_diagnostic_argument frame.arguments
              )
          | None -> (name, [])
        in
        Sema_index.enter_body index ~template:(Some template) ~instance)
      index;
    Sema_flow.set_loop_labels context.flow (loop_labels_in_block stmts);
    let* params =
      Result_list.map
        (fun ((param_name, param_ty), param_span) ->
          let* binding = add_local param_name param_ty context span in
          index_local context binding param_span;
          mark_init binding context;
          Ok binding)
        (List.combine params param_spans)
    in
    let body = Sema_check.check_block context stmts in
    Option.iter Sema_index.leave_body index;
    let* body =
      match (body, Sema_flow.recovered context.flow) with
      | Ok body, [] -> Ok body
      | Ok _, recorded -> Error recorded
      | Error diagnostics, recorded -> Error (recorded @ diagnostics)
    in
    let* () =
      if require_return && ret <> Hir.Void && (Hir.block_flow body).falls_through then
        error diagnostic_span
          (description ^ " `" ^ diagnostic_name ^ "` returning `" ^ Hir.ty_name ret
         ^ "` may reach the end without `return`")
      else Ok ()
    in
    Ok
      ({
         Hir.name;
         params;
         ret;
         body = Hir.Statements body;
         linkage;
         variadic;
         span;
         display = display_name name;
       }
        : Hir.func)
  in
  let add_func func = funcs := func :: !funcs in
  let check_func = function
    | Ast.Func
        ({ name; params; ret; body; linkage; variadic; generic_params = []; span; _ } as
         func) ->
        let name_span = func.name_span in
        let ret_span = func.ret_span in
        let trace = specialization_trace specializations Function_specialization name in
        let diagnostic_name =
          specialization_source_name specializations Function_specialization name
        in
        let spec_depth =
          match
            Sema_specialization.find_by_name specializations Function_specialization
              name
          with
          | Some specialization -> specialization.depth + 1
          | None -> 0
        in
        let description = if trace = [] then "function" else "specialized function" in
        let result =
          let* ret = source_return ret_span ret in
          let* params = source_params params in
          let linkage = hir_linkage linkage in
          match body with
          | Ast.Declaration ->
              Option.iter
                (fun index ->
                  Sema_index.bind_signature index ~name
                    (List.map2
                       (fun (param : Ast.param) (_, ty) -> (param.span, ty))
                       func.params params))
                index;
              Ok
                ({
                   Hir.name;
                   params = hir_params params;
                   ret;
                   body = Hir.Declaration;
                   linkage;
                   variadic;
                   span;
                   display = display_name name;
                 }
                  : Hir.func)
          | Ast.Statements stmts ->
              check_function_body ~name ~diagnostic_name ~description ~span
                ~diagnostic_span:name_span ~params
                ~param_spans:
                  (List.map (fun (param : Ast.param) -> param.span) func.params)
                ~ret ~stmts ~linkage ~variadic ~extra_consts:[] ~spec_depth
                ~spec_trace:trace ~require_return:true
        in
        let* func = result |> trace_result specializations trace in
        add_func func;
        Ok ()
    | _ -> Ok ()
  in
  let collected =
    ref (match recovered with Some recorded -> !recorded | None -> [])
  in
  let continuing result =
    match result with
    | Error diagnostics
      when all_errors && List.for_all Sema_flow.recoverable diagnostics ->
        collected := List.rev_append diagnostics !collected;
        Ok ()
    | result -> result
  in
  let* () = Result_list.iter (fun item -> continuing (check_func item)) program.items in
  let rec materialize () =
    match Sema_specialization.take_pending specializations with
    | None -> Ok ()
    | Some sp -> (
        match sp.payload with
        | Function_payload
            {
              item =
                Ast.Func
                  ({
                     params;
                     ret;
                     body = Ast.Statements stmts;
                     linkage;
                     variadic;
                     span;
                     _;
                   } as func);
              substitutions = [];
              values;
              staged_args = _;
            } ->
            let result =
              let source_params = params in
              let* ret = source_ty_with_values named_types values func.ret_span ret in
              let* ret =
                if ret = Hir.Void then Ok ret else validate_object func.ret_span ret
              in
              let* params =
                Result_list.map
                  (fun (parameter : Ast.param) ->
                    let* ty =
                      source_ty_with_values named_types values parameter.ty_span
                        parameter.ty
                    in
                    let* ty = validate_object parameter.ty_span ty in
                    Ok (parameter.name, ty))
                  params
              in
              let* () =
                validate_native_aggregate_signature func.ret_span source_params params
                  ret
              in
              check_function_body ~name:sp.name
                ~diagnostic_name:
                  (specialization_source_name specializations Function_specialization
                     sp.name)
                ~description:"specialized function" ~span
                ~diagnostic_span:func.name_span ~params
                ~param_spans:
                  (List.map (fun (param : Ast.param) -> param.span) source_params)
                ~ret ~stmts ~linkage:(hir_linkage linkage) ~variadic
                ~extra_consts:values ~spec_depth:(sp.depth + 1) ~spec_trace:sp.trace
                ~require_return:true
            in
            let* () =
              continuing
                (let* func = result |> trace_result specializations sp.trace in
                 add_func func;
                 Ok ())
            in
            materialize ()
        | Function_payload _ -> materialize ()
        | Struct_payload _ ->
            error Span.synthetic
              "internal error: struct specialization was queued too late")
  in
  let* () = materialize () in
  let* () =
    match !collected with
    | [] -> Ok ()
    | collected ->
        let kept =
          List.fold_left
            (fun kept (diagnostic : Diag.t) ->
              if
                List.exists
                  (fun (seen : Diag.t) ->
                    seen.primary = diagnostic.primary
                    && seen.message = diagnostic.message)
                  kept
              then kept
              else diagnostic :: kept)
            [] (List.rev collected)
          |> List.rev
        in
        let files =
          List.fold_left
            (fun files (diagnostic : Diag.t) ->
              if List.mem diagnostic.primary.Span.file files then files
              else files @ [ diagnostic.primary.Span.file ])
            [] kept
        in
        let rank file =
          let rec find index = function
            | [] -> index
            | name :: rest -> if name = file then index else find (index + 1) rest
          in
          find 0 files
        in
        Error
          (List.stable_sort
             (fun (left : Diag.t) (right : Diag.t) ->
               compare
                 (rank left.primary.Span.file, left.primary.Span.start_offset)
                 (rank right.primary.Span.file, right.primary.Span.start_offset))
             kept)
  in
  let* () =
    Sema_invariants.check_materialization
      ~pending:(Sema_specialization.pending_count specializations)
      ~functions:(List.map (fun (func : Hir.func) -> func.name) (List.rev !funcs))
    |> phase_invariant
  in
  let hconsts =
    List.map
      (fun (n, t, v) -> ({ Hir.name = n; ty = t; bits = v } : Hir.const_def))
      consts_ordered
  in
  let harrays =
    List.filter_map
      (fun (n, t, vs) ->
        match t with
        | Hir.Array _ -> Some ({ Hir.name = n; ty = t; elems = vs } : Hir.const_arr_def)
        | _ -> None)
      arrays_ordered
  in
  Option.iter
    (fun index ->
      let instantiated =
        Sema_specialization.fold_by_name
          (fun _ specialization names ->
            match
              Sema_specialization.current_instantiation_frame specialization.trace
            with
            | Some frame -> String_set.add frame.template_name names
            | None -> names)
          specializations String_set.empty
      in
      Sema_index.reference_program index
        ~instantiated:(fun name -> String_set.mem name instantiated)
        source_program;
      let struct_specializations =
        Sema_specialization.fold_by_name
          (fun (kind, generated) specialization acc ->
            match kind with
            | Sema_specialization.Struct_specialization ->
                (generated, specialization) :: acc
            | Sema_specialization.Function_specialization -> acc)
          specializations []
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in
      List.iter
        (fun (generated, (specialization : Sema_specialization.specialization)) ->
          match
            Sema_specialization.current_instantiation_frame specialization.trace
          with
          | Some frame ->
              Sema_index.record_struct_instance index ~generated
                ~template:frame.template_name
                ~args:
                  (List.map Sema_specialization.render_diagnostic_argument
                     frame.arguments)
          | None -> ())
        struct_specializations;
      Sema_index.record_program index ~structs ~consts:hconsts ~arrays:harrays ~globals;
      Sema_index.finish index
        ~render:(Sema_specialization.source_facing_text specializations))
    index;
  Ok
    ({
       Hir.structs;
       consts = hconsts;
       const_arrays = harrays;
       globals;
       funcs = List.rev !funcs;
       strings = List.rev program_strings.reversed;
     }
      : Hir.program)

let check ?limits ?c_aliases ?c_unsupported ?c_nonnull_parameters ?c_string_parameters
    ?c_record_calls ?c_alloc_size_parameters ?c_records ?all_errors ?specializations
    ?index program =
  let run () =
    check_program ?limits ?c_aliases ?c_unsupported ?c_nonnull_parameters
      ?c_string_parameters ?c_record_calls ?c_alloc_size_parameters ?c_records
      ?all_errors ?specializations ?index program
  in
  match index with
  | None -> run ()
  | Some _ ->
      Sema_constants.index := index;
      Fun.protect ~finally:(fun () -> Sema_constants.index := None) run
