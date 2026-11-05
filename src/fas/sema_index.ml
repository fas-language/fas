type kind =
  | Function
  | Parameter
  | Local
  | View
  | Global
  | Const
  | Const_array
  | Struct
  | Union
  | Opaque
  | Field
  | Type_parameter
  | Const_parameter
  | Type_alias

type declaration = {
  name : string;
  kind : kind;
  name_span : Span.t;
  item_span : Span.t;
  container : string option;
  c_origin : (string * int) option;
}

type reference = { use_span : Span.t; declaration : declaration }

type expression_type = {
  span : Span.t;
  instance : string list;
  ty : string;
  value : string option;
  rank : int;
}

type generic_call = {
  callee_span : Span.t;
  instance : string list;
  arguments : string list;
  explicit : bool;
  order : int;
}

type layout = {
  layout_size : int;
  layout_align : int;
  layout_fields : (string * string * int) list;
}

type t = {
  mutable declared : declaration list;
  top : (string, declaration) Hashtbl.t;
  fields : (string * string, declaration) Hashtbl.t;
  generics : (string * string, declaration) Hashtbl.t;
  bindings : (Span.t, declaration) Hashtbl.t;
  locals : (int, declaration) Hashtbl.t;
  explicit_calls : (Span.t, unit) Hashtbl.t;
  c_import_spans : (Span.t, unit) Hashtbl.t;
  origins : (string, string * int) Hashtbl.t;
  mutable c_imported : bool;
  c_unsupported : (string, string) Hashtbl.t;
  c_aliases : (string, string) Hashtbl.t;
  c_params : (string, string list) Hashtbl.t;
  c_names : (string, string option list) Hashtbl.t;
  c_record_calls : (string, int list * bool) Hashtbl.t;
  c_nonnull : (string, int list) Hashtbl.t;
  c_strings : (string, int list) Hashtbl.t;
  c_alloc : (string, int list list) Hashtbl.t;
  referenced : (Span.t * string * Span.t * kind, reference) Hashtbl.t;
  types : (Span.t * string list, expression_type) Hashtbl.t;
  calls : (Span.t * string list, generic_call) Hashtbl.t;
  mutable template : string option;
  mutable struct_source : string -> string;
  mutable instance : string list;
  signatures : (string, string) Hashtbl.t;
  generic_names : (string, string list) Hashtbl.t;
  struct_headers : (string, string) Hashtbl.t;
  local_types : (Span.t * string list, int * string) Hashtbl.t;
  declared_types : (Span.t, string) Hashtbl.t;
  global_types : (string, string) Hashtbl.t;
  array_decls : (string, string * int) Hashtbl.t;
  layouts : (string, layout) Hashtbl.t;
  consts_text : (string, string) Hashtbl.t;
  source_types : (Span.t, string) Hashtbl.t;
  written_fields : (string, (string * string) list) Hashtbl.t;
  const_types : (string * string, string) Hashtbl.t;
  instance_order : (string * string list, int) Hashtbl.t;
  template_instances : (string, string list list) Hashtbl.t;
  struct_instances : (string, string * string list) Hashtbl.t;
  instance_layouts : (string, (int * string list * layout) list) Hashtbl.t;
  enum_owners : (string, string option) Hashtbl.t;
  storage_consts : (string, Ast.ty * Ast.expr) Hashtbl.t;
}

let create () =
  {
    declared = [];
    top = Hashtbl.create 64;
    fields = Hashtbl.create 64;
    generics = Hashtbl.create 16;
    bindings = Hashtbl.create 64;
    locals = Hashtbl.create 32;
    explicit_calls = Hashtbl.create 16;
    c_import_spans = Hashtbl.create 16;
    origins = Hashtbl.create 64;
    c_imported = false;
    c_unsupported = Hashtbl.create 16;
    c_aliases = Hashtbl.create 16;
    c_params = Hashtbl.create 64;
    c_names = Hashtbl.create 64;
    c_record_calls = Hashtbl.create 16;
    c_nonnull = Hashtbl.create 16;
    c_strings = Hashtbl.create 16;
    c_alloc = Hashtbl.create 16;
    referenced = Hashtbl.create 256;
    types = Hashtbl.create 256;
    calls = Hashtbl.create 16;
    template = None;
    struct_source = Fun.id;
    instance = [];
    signatures = Hashtbl.create 64;
    generic_names = Hashtbl.create 16;
    struct_headers = Hashtbl.create 16;
    local_types = Hashtbl.create 64;
    declared_types = Hashtbl.create 64;
    global_types = Hashtbl.create 16;
    array_decls = Hashtbl.create 16;
    layouts = Hashtbl.create 16;
    consts_text = Hashtbl.create 16;
    source_types = Hashtbl.create 64;
    written_fields = Hashtbl.create 16;
    const_types = Hashtbl.create 16;
    instance_order = Hashtbl.create 16;
    template_instances = Hashtbl.create 16;
    struct_instances = Hashtbl.create 16;
    instance_layouts = Hashtbl.create 16;
    enum_owners = Hashtbl.create 64;
    storage_consts = Hashtbl.create 16;
  }

let kind_name = function
  | Function -> "function"
  | Parameter -> "parameter"
  | Local -> "local"
  | View -> "view"
  | Global -> "global"
  | Const -> "const"
  | Const_array -> "const array"
  | Struct -> "struct"
  | Union -> "union"
  | Opaque -> "opaque"
  | Field -> "field"
  | Type_parameter -> "type parameter"
  | Const_parameter -> "const parameter"
  | Type_alias -> "type alias"

let by_span compare_rest left right =
  match Span.compare left right with
  | 0 -> (
      match Int.compare left.Span.end_offset right.Span.end_offset with
      | 0 -> compare_rest ()
      | n -> n)
  | n -> n

let declarations index =
  List.sort
    (fun left right ->
      by_span
        (fun () -> compare (left.name, left.kind) (right.name, right.kind))
        left.name_span right.name_span)
    index.declared

let references index =
  Hashtbl.fold (fun _ reference acc -> reference :: acc) index.referenced []
  |> List.sort (fun left right ->
      by_span
        (fun () ->
          by_span
            (fun () -> compare left.declaration.name right.declaration.name)
            left.declaration.name_span right.declaration.name_span)
        left.use_span right.use_span)

let expression_types index =
  Hashtbl.fold (fun _ entry acc -> entry :: acc) index.types []
  |> List.sort (fun (left : expression_type) (right : expression_type) ->
      by_span (fun () -> compare left.instance right.instance) left.span right.span)

let generic_calls index =
  Hashtbl.fold (fun _ entry acc -> entry :: acc) index.calls []
  |> List.sort (fun (left : generic_call) (right : generic_call) ->
      by_span
        (fun () -> compare left.instance right.instance)
        left.callee_span right.callee_span)

let set_c_origins index origins =
  List.iter (fun (name, origin) -> Hashtbl.replace index.origins name origin) origins

let set_c_enum_owners index owners =
  List.iter (fun (name, owner) -> Hashtbl.replace index.enum_owners name owner) owners

let is_enumerator index (declaration : declaration) =
  declaration.kind = Const && Hashtbl.mem index.enum_owners declaration.name

let set_c_import index ~unsupported ~aliases ~nonnull ~strings ~alloc ~parameter_names
    ~records =
  index.c_imported <- true;
  List.iter (fun (name, call) -> Hashtbl.replace index.c_record_calls name call) records;
  List.iter
    (fun (name, names) -> Hashtbl.replace index.c_names name names)
    parameter_names;
  List.iter
    (fun (name, reason) -> Hashtbl.replace index.c_unsupported name reason)
    unsupported;
  List.iter (fun (name, target) -> Hashtbl.replace index.c_aliases name target) aliases;
  List.iter
    (fun (name, positions) -> Hashtbl.replace index.c_nonnull name positions)
    nonnull;
  List.iter
    (fun (name, positions) ->
      Hashtbl.replace index.c_strings name (List.map fst positions))
    strings;
  List.iter (fun (name, groups) -> Hashtbl.replace index.c_alloc name groups) alloc

let has_c_import index = index.c_imported

let c_unsupported index =
  Hashtbl.fold (fun name reason acc -> (name, reason) :: acc) index.c_unsupported []

let c_origin_named index name = Hashtbl.find_opt index.origins name
let declare index declaration = index.declared <- declaration :: index.declared

let refer index use_span (declaration : declaration) =
  if not (Span.is_synthetic use_span) then
    Hashtbl.replace index.referenced
      (use_span, declaration.name, declaration.name_span, declaration.kind)
      { use_span; declaration }

let rec statement_children = function
  | Ast.If (_, yes, no, _) -> yes @ Option.value ~default:[] no
  | Ast.While (_, _, body, _)
  | Ast.Defer (body, _)
  | Ast.Block (body, _)
  | Ast.Const_for { body; _ }
  | Ast.Const_copy { body; _ } ->
      body
  | Ast.For (_, init, _, step, body, _) ->
      Option.to_list init @ Option.to_list step @ body
  | Ast.Switch (_, cases, default, _) ->
      List.concat_map snd cases @ Option.value ~default:[] default
  | Ast.Let _ | Ast.View _ | Ast.Assign _ | Ast.Compound_assign _ | Ast.Return _
  | Ast.Break _ | Ast.Continue _ | Ast.Expr_stmt _ ->
      []

and statement_expressions = function
  | Ast.Let { init; _ } -> Option.to_list init
  | Ast.View { place; _ } -> [ place ]
  | Ast.Assign (target, value, _) | Ast.Compound_assign (target, _, value, _, _) ->
      target_expressions target @ [ value ]
  | Ast.Return (value, _) -> Option.to_list value
  | Ast.If (condition, _, _, _) | Ast.While (_, condition, _, _) -> [ condition ]
  | Ast.For (_, _, condition, _, _, _) -> Option.to_list condition
  | Ast.Switch (subject, cases, _, _) -> subject :: List.concat_map fst cases
  | Ast.Expr_stmt (value, _, _) -> [ value ]
  | Ast.Const_for { low; high; _ } -> [ low; high ]
  | Ast.Break _ | Ast.Continue _ | Ast.Defer _ | Ast.Block _ | Ast.Const_copy _ -> []

and target_expressions = function
  | Ast.Target_ident _ -> []
  | Ast.Target_select (base, arguments) ->
      base :: List.concat_map argument_expressions arguments
  | Ast.Target_field (base, _, _) -> [ base ]

and argument_expressions = function
  | Ast.Const_arg expression -> [ expression ]
  | Ast.Type_arg _ | Ast.Type_or_index _ | Ast.Name_arg _ -> []

let expression_children = function
  | Ast.Int_lit _ | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _
  | Ast.Ident _ | Ast.Sizeof _ | Ast.Alignof _ | Ast.Offsetof _ ->
      []
  | Ast.Unary (_, e, _)
  | Ast.C_dereference (e, _, _)
  | Ast.C_dot_star (e, _)
  | Ast.Parenthesized (e, _)
  | Ast.Cast (_, _, e, _)
  | Ast.Field (e, _, _)
  | Ast.Arrow_field (e, _, _, _)
  | Ast.Addr_of (e, _)
  | Ast.Handle_from_addr (_, e, _)
  | Ast.Sizeof_value (e, _)
  | Ast.Splat (e, _)
  | Ast.Designated_field (_, _, e, _) ->
      [ e ]
  | Ast.Binary (_, left, right, _) | Ast.Designated_index (left, right, _) ->
      [ left; right ]
  | Ast.Call (callee, arguments, _) -> callee :: arguments
  | Ast.Generic_args (callee, _, _) | Ast.Select (callee, _, _) -> [ callee ]
  | Ast.Ternary (a, b, c, _) -> [ a; b; c ]
  | Ast.Array_lit (elements, _) -> elements

let expression_types_of = function
  | Ast.Cast (_, ty, _, _)
  | Ast.Handle_from_addr (ty, _, _)
  | Ast.Sizeof (ty, _)
  | Ast.Alignof (ty, _)
  | Ast.Offsetof (ty, _, _, _) ->
      [ ty ]
  | _ -> []

let expression_arguments = function
  | Ast.Generic_args (_, arguments, _) | Ast.Select (_, arguments, _) -> arguments
  | _ -> []

let rec iter_expression f expression =
  f expression;
  List.iter (iter_expression f) (expression_children expression)

let rec iter_statement on_statement on_expression statement =
  on_statement statement;
  List.iter (iter_expression on_expression) (statement_expressions statement);
  List.iter (iter_statement on_statement on_expression) (statement_children statement)

let generic_text parameters =
  match parameters with
  | [] -> ""
  | _ ->
      "["
      ^ String.concat ", "
          (List.map
             (function
               | Ast.Type_param { name; _ } -> name
               | Ast.Const_param { name; ty; _ } -> name ^ " const " ^ Ast.type_name ty)
             parameters)
      ^ "]"

let generic_param_names parameters =
  List.map
    (function Ast.Type_param { name; _ } | Ast.Const_param { name; _ } -> name)
    parameters

let c_display_params index name params =
  let names = Option.value ~default:[] (Hashtbl.find_opt index.c_names name) in
  List.mapi
    (fun position (parameter : Ast.param) ->
      match Option.join (List.nth_opt names position) with
      | Some header -> { parameter with Ast.name = header }
      | None -> parameter)
    params

let c_record_shape index name params ret =
  match Hashtbl.find_opt index.c_record_calls name with
  | None -> (params, ret)
  | Some (positions, has_result) ->
      let unwrap = function Ast.Handle inner -> inner | ty -> ty in
      let params, ret =
        match (has_result, params) with
        | true, (result : Ast.param) :: rest -> (rest, unwrap result.ty)
        | _ -> (params, ret)
      in
      ( List.mapi
          (fun position (parameter : Ast.param) ->
            if List.mem position positions then
              { parameter with Ast.ty = unwrap parameter.ty }
            else parameter)
          params,
        ret )

let signature_text name ~linkage ~generic_params ~params ~variadic ~ret =
  let keyword =
    match linkage with Ast.Internal -> "fn " | Ast.External_c -> "extern fn "
  in
  let shown =
    List.map (fun (p : Ast.param) -> p.name ^ " " ^ Ast.type_name p.ty) params
    @ if variadic then [ "..." ] else []
  in
  Printf.sprintf "%s%s%s(%s) %s" keyword name (generic_text generic_params)
    (String.concat ", " shown) (Ast.type_name ret)

let struct_header keyword name generic_params align =
  keyword ^ " " ^ name ^ generic_text generic_params
  ^ match align with None -> "" | Some n -> Printf.sprintf " @align(%d)" n

let declare_program index ~c_aliases (program : Ast.program) =
  let origin name = Hashtbl.find_opt index.origins name in
  let add ?container ?c_origin name kind name_span item_span =
    let declaration = { name; kind; name_span; item_span; container; c_origin } in
    declare index declaration;
    declaration
  in
  let top ?c_origin name kind name_span item_span =
    let name_span, item_span =
      match c_origin with
      | Some _ ->
          Hashtbl.replace index.c_import_spans item_span ();
          (Span.synthetic, Span.synthetic)
      | None -> (name_span, item_span)
    in
    let declaration = add ?c_origin name kind name_span item_span in
    Hashtbl.replace index.top name declaration
  in
  let generics owner item_span parameters =
    List.iter
      (fun parameter ->
        let name, kind, span =
          match parameter with
          | Ast.Type_param { name; span } -> (name, Type_parameter, span)
          | Ast.Const_param { name; span; ty; _ } ->
              Hashtbl.replace index.const_types (owner, name) (Ast.type_name ty);
              (name, Const_parameter, span)
        in
        let declaration = add ~container:owner name kind span item_span in
        Hashtbl.replace index.generics (owner, name) declaration)
      parameters
  in
  let local owner item_span name kind span =
    let declaration = add ~container:owner name kind span item_span in
    Hashtbl.replace index.bindings span declaration
  in
  List.iter
    (function
      | Ast.Use _ | Ast.Pkg _ -> ()
      | Ast.Const { name; name_span; ty; value; extent; _ } ->
          let kind =
            match ty with Ast.Array _ | Ast.Vec _ -> Const_array | _ -> Const
          in
          Hashtbl.replace index.storage_consts name (ty, value);
          top ?c_origin:(origin name) name kind name_span extent
      | Ast.Global { name; name_span; extent; _ } ->
          top ?c_origin:(origin name) name Global name_span extent
      | Ast.Opaque { name; name_span; extent; _ } ->
          top ?c_origin:(origin name) name Opaque name_span extent
      | Ast.Struct
          { name; name_span; generic_params; fields; is_union; extent; align; _ } ->
          let c_origin = origin name in
          let keyword = if is_union then "union" else "struct" in
          Hashtbl.replace index.struct_headers name
            (struct_header keyword name generic_params align);
          Hashtbl.replace index.generic_names name (generic_param_names generic_params);
          if generic_params <> [] then
            Hashtbl.replace index.written_fields name
              (List.map
                 (fun (field : Ast.field) -> (field.name, Ast.type_name field.ty))
                 fields);
          top ?c_origin name (if is_union then Union else Struct) name_span extent;
          generics name extent generic_params;
          List.iter
            (fun (field : Ast.field) ->
              let field_span, field_extent =
                match c_origin with
                | Some _ -> (Span.synthetic, Span.synthetic)
                | None -> (field.span, extent)
              in
              let declaration =
                add ~container:name ?c_origin field.name Field field_span field_extent
              in
              Hashtbl.replace index.fields (name, field.name) declaration)
            fields
      | Ast.Func
          {
            name;
            name_span;
            params;
            body;
            generic_params;
            extent;
            linkage;
            ret;
            variadic;
            _;
          } -> (
          let c_origin = origin name in
          let shown_params, ret =
            if Option.is_some c_origin then
              let params, ret = c_record_shape index name params ret in
              (c_display_params index name params, ret)
            else (params, ret)
          in
          top ?c_origin name Function name_span extent;
          Hashtbl.replace index.signatures name
            (signature_text name ~linkage ~generic_params ~params:shown_params ~variadic
               ~ret);
          Hashtbl.replace index.generic_names name (generic_param_names generic_params);
          if Option.is_some c_origin then
            Hashtbl.replace index.c_params name
              (List.map (fun (parameter : Ast.param) -> parameter.name) shown_params);
          generics name extent generic_params;
          if Option.is_none c_origin then
            List.iter
              (fun (parameter : Ast.param) ->
                local name extent parameter.name Parameter parameter.span;
                Hashtbl.replace index.source_types parameter.span
                  (Ast.type_name parameter.ty))
              params;
          match body with
          | Ast.Declaration -> ()
          | Ast.Statements statements ->
              List.iter
                (iter_statement
                   (function
                     | Ast.Let { name = local_name; span; ty = local_ty; _ } ->
                         local name extent local_name Local span;
                         Hashtbl.replace index.source_types span
                           (Ast.type_name local_ty)
                     | Ast.View { name = local_name; name_span; _ } ->
                         local name extent local_name View name_span
                     | _ -> ())
                   (function
                     | Ast.Call (Ast.Generic_args (Ast.Ident (_, span), _, _), _, _) ->
                         Hashtbl.replace index.explicit_calls span ()
                     | _ -> ()))
                statements))
    program.items;
  List.iter
    (fun name ->
      match origin name with
      | Some (file, _) as c_origin
        when file <> "<unknown>" && not (Hashtbl.mem index.top name) ->
          top ?c_origin name Type_alias Span.synthetic Span.synthetic
      | _ -> ())
    c_aliases

let reference_program index ~instantiated (program : Ast.program) =
  let resolve owner shadowed span name =
    if not (List.mem name shadowed) then
      match
        Option.bind owner (fun owner -> Hashtbl.find_opt index.generics (owner, name))
      with
      | Some declaration -> refer index span declaration
      | None -> (
          match Hashtbl.find_opt index.top name with
          | Some declaration -> refer index span declaration
          | None -> ())
  in
  let rec static_expression owner shadowed expression =
    iter_expression
      (fun expression ->
        (match expression with
        | Ast.Ident (name, span) -> resolve owner shadowed span name
        | _ -> ());
        positions owner shadowed expression)
      expression
  and positions owner shadowed expression =
    List.iter (ty owner shadowed) (expression_types_of expression);
    List.iter (argument owner shadowed) (expression_arguments expression)
  and ty owner shadowed = function
    | Ast.Named_type (name, span) -> resolve owner shadowed span name
    | Ast.Applied_type (name, name_span, arguments, _) ->
        resolve owner shadowed name_span name;
        List.iter (argument owner shadowed) arguments
    | Ast.Array (length, element) | Ast.Vec (length, element) ->
        static_expression owner shadowed length.Ast.expression;
        ty owner shadowed element
    | Ast.Handle element -> ty owner shadowed element
    | Ast.Bool | Ast.Int _ | Ast.Float _ | Ast.Addr | Ast.Void -> ()
  and argument owner shadowed = function
    | Ast.Type_arg t | Ast.Type_or_index t -> ty owner shadowed t
    | Ast.Const_arg expression -> static_expression owner shadowed expression
    | Ast.Name_arg (name, span) -> resolve owner shadowed span name
  in
  let rec body owner shadowed statements =
    List.fold_left
      (fun shadowed statement ->
        let expressions = statement_expressions statement in
        List.iter
          (iter_expression (fun expression -> positions owner shadowed expression))
          expressions;
        match statement with
        | Ast.Let { name; ty = local_ty; _ } ->
            ty owner shadowed local_ty;
            name :: shadowed
        | Ast.View { name; _ } -> name :: shadowed
        | Ast.Const_for { name; ty = index_ty; body = statements; _ } ->
            ty owner shadowed index_ty;
            ignore (body owner (name :: shadowed) statements);
            shadowed
        | Ast.For (_, init, _, step, statements, _) ->
            let inner = body owner shadowed (Option.to_list init) in
            ignore (body owner inner (Option.to_list step @ statements));
            shadowed
        | statement ->
            List.iter
              (fun statements -> ignore (body owner shadowed statements))
              (match statement with
              | Ast.If (_, yes, no, _) -> yes :: Option.to_list no
              | Ast.While (_, _, statements, _)
              | Ast.Defer (statements, _)
              | Ast.Block (statements, _)
              | Ast.Const_copy { body = statements; _ } ->
                  [ statements ]
              | Ast.Switch (_, cases, default, _) ->
                  List.map snd cases @ Option.to_list default
              | _ -> []);
            shadowed)
      shadowed statements
  in
  List.iter
    (function
      | Ast.Use _ | Ast.Pkg _ -> ()
      | Ast.Const { ty = const_ty; value; _ } ->
          ty None [] const_ty;
          static_expression None [] value
      | Ast.Global { ty = global_ty; init; _ } ->
          ty None [] global_ty;
          Option.iter (static_expression None []) init
      | Ast.Opaque _ -> ()
      | Ast.Struct { name; generic_params; fields; _ } ->
          if generic_params = [] || instantiated name then
            List.iter (fun (field : Ast.field) -> ty (Some name) [] field.ty) fields
      | Ast.Func { name; params; ret; body = function_body; generic_params; _ } ->
          if generic_params = [] || instantiated name then (
            let owner = Some name in
            List.iter
              (function
                | Ast.Const_param { ty = param_ty; _ } -> ty owner [] param_ty
                | Ast.Type_param _ -> ())
              generic_params;
            List.iter (fun (parameter : Ast.param) -> ty owner [] parameter.ty) params;
            ty owner [] ret;
            match function_body with
            | Ast.Declaration -> ()
            | Ast.Statements statements ->
                ignore
                  (body owner
                     (List.map (fun (parameter : Ast.param) -> parameter.name) params)
                     statements)))
    program.items

let register_instance index template instance =
  if not (Hashtbl.mem index.instance_order (template, instance)) then (
    Hashtbl.replace index.instance_order (template, instance)
      (Hashtbl.length index.instance_order);
    let previous =
      Option.value ~default:[] (Hashtbl.find_opt index.template_instances template)
    in
    Hashtbl.replace index.template_instances template (previous @ [ instance ]))

let instance_rank index =
  match index.template with
  | None -> 0
  | Some template ->
      Option.value ~default:0
        (Hashtbl.find_opt index.instance_order (template, index.instance))

let enter_body index ~template ~instance =
  Hashtbl.reset index.locals;
  Option.iter (fun template -> register_instance index template instance) template;
  index.template <- template;
  index.instance <- instance

let record_local_type index span (ty : Hir.ty) =
  Hashtbl.replace index.local_types (span, index.instance)
    (instance_rank index, Hir.ty_name ty)

let bind_local index id (ty : Hir.ty) span =
  match Hashtbl.find_opt index.bindings span with
  | Some declaration ->
      Hashtbl.replace index.locals id declaration;
      record_local_type index span ty
  | None -> Hashtbl.remove index.locals id

let refer_local index span id =
  Option.iter (refer index span) (Hashtbl.find_opt index.locals id)

let refer_name index span name =
  match
    Option.bind index.template (fun owner ->
        Hashtbl.find_opt index.generics (owner, name))
  with
  | Some declaration -> refer index span declaration
  | None -> Option.iter (refer index span) (Hashtbl.find_opt index.top name)

let set_struct_source index source = index.struct_source <- source

let refer_field index span record field =
  let declaration =
    match Hashtbl.find_opt index.fields (record, field) with
    | Some _ as found -> found
    | None -> Hashtbl.find_opt index.fields (index.struct_source record, field)
  in
  Option.iter (refer index span) declaration

let record_type index span ty value =
  if not (Span.is_synthetic span || Hashtbl.mem index.c_import_spans span) then
    let key = (span, index.instance) in
    let value =
      match (value, Hashtbl.find_opt index.types key) with
      | None, Some previous when previous.ty = ty -> previous.value
      | _ -> value
    in
    Hashtbl.replace index.types key
      { span; instance = index.instance; ty; value; rank = instance_rank index }

let render_value ty value =
  let float_text precision number =
    let text = Printf.sprintf "%.*g" precision number in
    if String.exists (fun c -> c = '.' || c = 'e' || c = 'n' || c = 'i') text then text
    else text ^ ".0"
  in
  match ty with
  | Hir.Float Hir.F32 -> float_text 9 (Int32.float_of_bits (Int64.to_int32 value))
  | Hir.Float Hir.F64 -> float_text 17 (Int64.float_of_bits value)
  | Hir.Addr | Hir.Handle _ -> "null"
  | _ -> Sema_specialization.render_diagnostic_const ty value

let record_const index span ty value =
  if
    not
      (Span.is_synthetic span
      || Hashtbl.mem index.c_import_spans span
      || Hashtbl.mem index.types (span, index.instance))
  then record_type index span (Hir.ty_name ty) (Some (render_value ty value))

let leave_body index =
  Hashtbl.reset index.locals;
  index.template <- None;
  index.instance <- []

let bind_signature index ~name params =
  enter_body index ~template:(Some name) ~instance:[];
  List.iter
    (fun (span, ty) ->
      match Hashtbl.find_opt index.bindings span with
      | Some { kind = Parameter; _ } -> record_local_type index span ty
      | Some _ | None -> ())
    params;
  leave_body index

let record_generic_call index span arguments =
  if not (Span.is_synthetic span) then
    Hashtbl.replace index.calls (span, index.instance)
      {
        callee_span = span;
        instance = index.instance;
        arguments;
        explicit = Hashtbl.mem index.explicit_calls span;
        order = instance_rank index;
      }

let unique items =
  List.fold_left
    (fun acc item -> if List.mem item acc then acc else acc @ [ item ])
    [] items

let union_text texts = String.concat " | " (unique texts)

let finish index ~render =
  let memo = Hashtbl.create 64 in
  let render text =
    match Hashtbl.find_opt memo text with
    | Some rendered -> rendered
    | None ->
        let rendered = render text in
        Hashtbl.replace memo text rendered;
        rendered
  in
  Hashtbl.filter_map_inplace
    (fun _ (entry : expression_type) ->
      Some
        { entry with ty = render entry.ty; instance = List.map render entry.instance })
    index.types;
  Hashtbl.filter_map_inplace
    (fun _ (entry : generic_call) ->
      Some
        {
          entry with
          arguments = List.map render entry.arguments;
          instance = List.map render entry.instance;
        })
    index.calls;
  let ranked = Hashtbl.create 64 in
  Hashtbl.iter
    (fun (span, _) (rank, raw) ->
      let previous = Option.value ~default:[] (Hashtbl.find_opt ranked span) in
      Hashtbl.replace ranked span ((rank, render raw) :: previous))
    index.local_types;
  Hashtbl.reset index.declared_types;
  Hashtbl.iter
    (fun span entries ->
      let ordered =
        List.stable_sort (fun (left, _) (right, _) -> Int.compare left right) entries
      in
      Hashtbl.replace index.declared_types span (union_text (List.map snd ordered)))
    ranked;
  Hashtbl.filter_map_inplace
    (fun _ instances -> Some (unique (List.map (List.map render) instances)))
    index.template_instances;
  Hashtbl.filter_map_inplace (fun _ raw -> Some (render raw)) index.global_types;
  Hashtbl.filter_map_inplace
    (fun _ entries ->
      Some
        (List.map
           (fun (rank, args, (layout : layout)) ->
             ( rank,
               List.map render args,
               {
                 layout with
                 layout_fields =
                   List.map
                     (fun (field, ty, offset) -> (field, render ty, offset))
                     layout.layout_fields;
               } ))
           entries
        |> List.stable_sort (fun (left, _, _) (right, _, _) -> Int.compare left right)))
    index.instance_layouts;
  Hashtbl.filter_map_inplace
    (fun _ (layout : layout) ->
      Some
        {
          layout with
          layout_fields =
            List.map
              (fun (field, ty, offset) -> (field, render ty, offset))
              layout.layout_fields;
        })
    index.layouts;
  index.template <- None;
  index.instance <- [];
  Hashtbl.reset index.locals

let const_text (c : Hir.const_def) =
  let value = render_value c.Hir.ty c.Hir.bits in
  let hex =
    match c.Hir.ty with
    | Hir.Int _ when Sema_numeric.is_unsigned c.Hir.ty ->
        let masked = Sema_numeric.mask_value c.Hir.ty c.Hir.bits in
        if Int64.unsigned_compare masked 256L >= 0 then Printf.sprintf " (0x%Lx)" masked
        else ""
    | _ -> ""
  in
  Printf.sprintf "const %s %s = %s%s" c.Hir.name (Hir.ty_name c.Hir.ty) value hex

let record_array index name (ty : Hir.ty) =
  match ty with
  | Hir.Array (count, _) ->
      Hashtbl.replace index.array_decls name (Hir.ty_name ty, count)
  | _ -> ()

let record_program index ~(structs : Hir.struct_def list) ~(consts : Hir.const_def list)
    ~(arrays : Hir.const_arr_def list) ~(globals : Hir.global list) =
  List.iter
    (fun (s : Hir.struct_def) ->
      let layout =
        {
          layout_size = s.Hir.size;
          layout_align = s.Hir.align;
          layout_fields =
            List.map
              (fun (f : Hir.field) -> (f.Hir.name, Hir.ty_name f.Hir.ty, f.Hir.offset))
              s.Hir.fields;
        }
      in
      match Hashtbl.find_opt index.struct_instances s.Hir.name with
      | None -> Hashtbl.replace index.layouts s.Hir.name layout
      | Some (template, args) ->
          let rank =
            Option.value ~default:0
              (Hashtbl.find_opt index.instance_order (template, args))
          in
          let previous =
            Option.value ~default:[] (Hashtbl.find_opt index.instance_layouts template)
          in
          Hashtbl.replace index.instance_layouts template
            ((rank, args, layout) :: previous))
    structs;
  List.iter
    (fun (c : Hir.const_def) ->
      Hashtbl.replace index.consts_text c.Hir.name (const_text c))
    consts;
  List.iter
    (fun (a : Hir.const_arr_def) -> record_array index a.Hir.name a.Hir.ty)
    arrays;
  List.iter
    (fun (g : Hir.global) ->
      Hashtbl.replace index.global_types g.Hir.name (Hir.ty_name g.Hir.ty);
      record_array index g.Hir.name g.Hir.ty)
    globals

let record_struct_instance index ~generated ~template ~args =
  if Hashtbl.mem index.struct_headers template then (
    register_instance index template args;
    Hashtbl.replace index.struct_instances generated (template, args))

let instantiation_lines index callee names =
  if names = [] then []
  else
    Hashtbl.fold
      (fun _ (call : generic_call) acc ->
        if call.callee_span = callee then call.arguments :: acc else acc)
      index.calls []
    |> List.sort_uniq compare
    |> List.filter_map (fun arguments ->
        if List.length arguments = List.length names then
          Some
            (String.concat ", "
               (List.map2
                  (fun name argument -> name ^ " = " ^ argument)
                  names arguments))
        else None)

let field_text field ty offset = Printf.sprintf "%s %s at offset %d" field ty offset
let instance_text template args = template ^ "[" ^ String.concat ", " args ^ "]"

let layout_block label (layout : layout) =
  let rows =
    List.map
      (fun (field, ty, offset) -> (field, ty, Printf.sprintf "at offset %d" offset))
      layout.layout_fields
  in
  let column select =
    2
    + List.fold_left
        (fun longest row -> max longest (String.length (select row)))
        0 rows
  in
  let name_width = column (fun (field, _, _) -> field) in
  let type_width = column (fun (_, ty, _) -> ty) in
  let size =
    Printf.sprintf "size %d, align %d" layout.layout_size layout.layout_align
  in
  let heading = match label with None -> size | Some label -> label ^ ": " ^ size in
  let lines =
    List.map
      (fun (field, ty, place) ->
        Printf.sprintf "%-*s%-*s%s" name_width field type_width ty place)
      rows
  in
  "```text\n" ^ String.concat "\n" (heading :: lines) ^ "\n```"

let rec plain_address_path = function
  | Ast.Ident _ -> true
  | Ast.Field (base, _, _) -> plain_address_path base
  | Ast.Select (base, [ Ast.Const_arg (Ast.Int_lit _ | Ast.Ident _) ], _) ->
      plain_address_path base
  | _ -> false

let storage_const_describe index name =
  match Hashtbl.find_opt index.storage_consts name with
  | Some (Ast.Named_type (ty, _), _) ->
      Option.map
        (fun layout -> ("const " ^ name ^ " " ^ ty, [ layout_block None layout ]))
        (Hashtbl.find_opt index.layouts ty)
  | Some (Ast.Addr, (Ast.Addr_of (target, _) as value)) when plain_address_path target
    ->
      Some ("const " ^ name ^ " addr = " ^ Ast.expr_name value, [])
  | Some (Ast.Addr, _) -> Some ("const " ^ name ^ " addr", [])
  | Some _ | None -> None

let enumerator_facts index (declaration : declaration) =
  if not (is_enumerator index declaration) then []
  else
    match Hashtbl.find index.enum_owners declaration.name with
    | None -> [ "from anonymous enum" ]
    | Some owner -> [ "from enum " ^ owner ]

let is_generic index owner =
  match Hashtbl.find_opt index.generic_names owner with
  | Some (_ :: _) -> true
  | Some [] | None -> false

let instance_bindings index owner =
  let names = Option.value ~default:[] (Hashtbl.find_opt index.generic_names owner) in
  let instances =
    Option.value ~default:[] (Hashtbl.find_opt index.template_instances owner)
  in
  List.filter_map
    (fun args ->
      if List.length args = List.length names then Some (List.combine names args)
      else None)
    instances

let instance_facts index owner =
  List.map
    (fun bindings ->
      String.concat ", " (List.map (fun (name, arg) -> name ^ " = " ^ arg) bindings))
    (instance_bindings index owner)

let parameter_facts index owner name =
  List.filter_map
    (fun bindings ->
      Option.map (fun arg -> name ^ " = " ^ arg) (List.assoc_opt name bindings))
    (instance_bindings index owner)

let generic_layouts index owner =
  let entries =
    Option.value ~default:[] (Hashtbl.find_opt index.instance_layouts owner)
  in
  List.map
    (fun (_, args, layout) -> layout_block (Some (instance_text owner args)) layout)
    entries

let generic_field_facts index owner name =
  let entries =
    Option.value ~default:[] (Hashtbl.find_opt index.instance_layouts owner)
  in
  List.filter_map
    (fun (_, args, (layout : layout)) ->
      List.find_map
        (fun (field, ty, offset) ->
          if field = name then
            Some (instance_text owner args ^ ": " ^ field_text field ty offset)
          else None)
        layout.layout_fields)
    entries

let c_fact_text index name =
  match Hashtbl.find_opt index.c_params name with
  | None -> []
  | Some names -> (
      let named positions =
        List.filter_map (fun position -> List.nth_opt names (position - 1)) positions
      in
      let nonnull =
        match Hashtbl.find_opt index.c_nonnull name with
        | Some (_ :: _ as positions) ->
            [ "nonnull: " ^ String.concat ", " (named positions) ]
        | _ -> []
      in
      let strings =
        match Hashtbl.find_opt index.c_strings name with
        | Some (_ :: _ as positions) ->
            [ "C string: " ^ String.concat ", " (named positions) ]
        | _ -> []
      in
      let alloc =
        List.map
          (fun group -> "alloc_size: " ^ String.concat " * " (named group))
          (Option.value ~default:[] (Hashtbl.find_opt index.c_alloc name))
      in
      match nonnull @ strings @ alloc with
      | [] -> []
      | facts -> [ String.concat "; " facts ])

let elements_text count =
  if count = 1 then "1 element" else Printf.sprintf "%d elements" count

let describe index ?use (declaration : declaration) =
  let name = declaration.name in
  let declared_type () = Hashtbl.find_opt index.declared_types declaration.name_span in
  let generic_owner () =
    match declaration.container with
    | Some owner when is_generic index owner -> Some owner
    | Some _ | None -> None
  in
  match declaration.kind with
  | Local | Parameter -> (
      match generic_owner () with
      | Some owner ->
          let text =
            match Hashtbl.find_opt index.source_types declaration.name_span with
            | Some _ as source -> source
            | None -> declared_type ()
          in
          Option.map (fun ty -> (name ^ " " ^ ty, instance_facts index owner)) text
      | None -> Option.map (fun ty -> (name ^ " " ^ ty, [])) (declared_type ()))
  | View -> (
      let header ty = "view " ^ name ^ " " ^ ty in
      match generic_owner () with
      | Some owner ->
          Option.map
            (fun ty -> (header ty, instance_facts index owner))
            (declared_type ())
      | None -> Option.map (fun ty -> (header ty, [])) (declared_type ()))
  | Global -> (
      match Hashtbl.find_opt index.array_decls name with
      | Some (ty, count) -> Some (name ^ " " ^ ty, [ elements_text count ])
      | None ->
          Option.map
            (fun ty -> (name ^ " " ^ ty, []))
            (Hashtbl.find_opt index.global_types name))
  | Const -> (
      match Hashtbl.find_opt index.consts_text name with
      | Some text -> Some (text, enumerator_facts index declaration)
      | None -> storage_const_describe index name)
  | Const_array ->
      Option.map
        (fun (ty, count) -> ("const " ^ name ^ " " ^ ty, [ elements_text count ]))
        (Hashtbl.find_opt index.array_decls name)
  | Opaque -> Some ("opaque " ^ name, [ "no layout" ])
  | Union ->
      Option.map
        (fun header ->
          ( header,
            match Hashtbl.find_opt index.layouts name with
            | None -> []
            | Some layout -> [ layout_block None layout ] ))
        (Hashtbl.find_opt index.struct_headers name)
  | Function ->
      Option.map
        (fun signature ->
          let names =
            Option.value ~default:[] (Hashtbl.find_opt index.generic_names name)
          in
          let lines =
            match use with
            | None -> []
            | Some callee -> instantiation_lines index callee names
          in
          (signature, lines @ c_fact_text index name))
        (Hashtbl.find_opt index.signatures name)
  | Type_parameter ->
      Option.map
        (fun owner -> (name, parameter_facts index owner name))
        (generic_owner ())
  | Const_parameter -> (
      match (declaration.container, generic_owner ()) with
      | Some owner, Some _ ->
          Option.map
            (fun ty -> (name ^ " " ^ ty, parameter_facts index owner name))
            (Hashtbl.find_opt index.const_types (owner, name))
      | _ -> None)
  | Struct ->
      Option.map
        (fun header ->
          if is_generic index name then
            let written =
              Option.value ~default:[] (Hashtbl.find_opt index.written_fields name)
            in
            ( header,
              List.map (fun (field, ty) -> field ^ " " ^ ty) written
              @ generic_layouts index name )
          else
            ( header,
              match Hashtbl.find_opt index.layouts name with
              | None -> []
              | Some layout -> [ layout_block None layout ] ))
        (Hashtbl.find_opt index.struct_headers name)
  | Field -> (
      match declaration.container with
      | None -> None
      | Some owner when is_generic index owner -> (
          match
            List.assoc_opt name
              (Option.value ~default:[] (Hashtbl.find_opt index.written_fields owner))
          with
          | None -> None
          | Some ty -> Some (name ^ " " ^ ty, generic_field_facts index owner name))
      | Some owner -> (
          match Hashtbl.find_opt index.layouts owner with
          | None -> None
          | Some layout ->
              List.find_map
                (fun (field, ty, offset) ->
                  if field = name then Some (field_text field ty offset, []) else None)
                layout.layout_fields))
  | Type_alias -> (
      match (declaration.c_origin, Hashtbl.find_opt index.c_aliases name) with
      | Some _, Some target -> Some ("type " ^ name ^ " = " ^ target, [])
      | _ -> None)
