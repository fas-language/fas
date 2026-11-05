type kind = Function_specialization | Struct_specialization

type arg =
  | Type_specialization_arg of string
  | Const_specialization_arg of Hir.ty * int64

type key = kind * int * arg list

type diagnostic_type =
  | Diagnostic_bool
  | Diagnostic_void
  | Diagnostic_int of Ast.int_kind
  | Diagnostic_float of Ast.float_kind
  | Diagnostic_ptr of diagnostic_type
  | Diagnostic_const_ptr of diagnostic_type
  | Diagnostic_addr
  | Diagnostic_handle of diagnostic_type
  | Diagnostic_array of string * diagnostic_type
  | Diagnostic_vec of string * diagnostic_type
  | Diagnostic_named of string
  | Diagnostic_applied of string * diagnostic_argument list

and diagnostic_argument =
  | Diagnostic_type_argument of diagnostic_type
  | Diagnostic_const_argument of Hir.ty * int64
  | Diagnostic_source_const_argument of string

type instantiation_frame = {
  template_name : string;
  arguments : diagnostic_argument list;
  application_span : Span.t;
}

type pending_diagnostic_argument =
  | Pending_diagnostic_type of diagnostic_type
  | Pending_diagnostic_const of string

type pending_instantiation_frame = {
  pending_arguments : pending_diagnostic_argument list;
  pending_application_span : Span.t;
}

type staged_arg = Staged_type_arg of string | Staged_const_arg of string

type payload =
  | Function_payload of {
      item : Ast.item;
      substitutions : (string * Ast.ty) list;
      values : (string * Hir.ty * int64) list;
      staged_args : staged_arg list option;
    }
  | Struct_payload of {
      template : Ast.item;
      substitutions : (string * Ast.ty) list;
      values : (string * Hir.ty * int64) list;
    }

type specialization = {
  key : key;
  name : string;
  depth : int;
  payload : payload;
  trace : instantiation_frame list;
  pending_frame : pending_instantiation_frame option;
}

module Cache = Hashtbl.Make (struct
  type t = key

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type t = {
  cache : specialization Cache.t;
  queue : specialization Queue.t;
  by_name : (kind * string, specialization) Hashtbl.t;
}

let create () =
  { cache = Cache.create 32; queue = Queue.create (); by_name = Hashtbl.create 32 }

let request state ~limits ~depth ~span ~description specialization =
  match Cache.find_opt state.cache specialization.key with
  | Some existing -> Ok existing
  | None ->
      if depth >= limits.Limits.max_specialization_depth then
        Error
          [
            Diag.error span
              (Printf.sprintf
                 "%s recursion depth limit exceeded: budget max_specialization_depth \
                  of %d (profile %s)"
                 description limits.Limits.max_specialization_depth
                 (Limits.budget_profile_name limits));
          ]
      else if Cache.length state.cache >= limits.Limits.max_specializations then
        Error
          [
            Diag.error span
              (Printf.sprintf
                 "%s count limit exceeded: budget max_specializations of %d (profile \
                  %s)"
                 description limits.Limits.max_specializations
                 (Limits.budget_profile_name limits));
          ]
      else (
        Cache.add state.cache specialization.key specialization;
        let kind, _, _ = specialization.key in
        Hashtbl.replace state.by_name (kind, specialization.name) specialization;
        Queue.add specialization state.queue;
        Ok specialization)

let find_by_name state kind name = Hashtbl.find_opt state.by_name (kind, name)
let fold_by_name f state initial = Hashtbl.fold f state.by_name initial
let take_pending state = Queue.take_opt state.queue
let pending_count state = Queue.length state.queue
let requeue_materialized state specialization = Queue.add specialization state.queue

let update_materialized state specialization =
  Cache.replace state.cache specialization.key specialization;
  let kind, _, _ = specialization.key in
  Hashtbl.replace state.by_name (kind, specialization.name) specialization

let current_instantiation_frame trace =
  match List.rev trace with frame :: _ -> Some frame | [] -> None

let rec diagnostic_type_of_ast specializations = function
  | Ast.Bool -> Diagnostic_bool
  | Ast.Void -> Diagnostic_void
  | Ast.Int kind -> Diagnostic_int kind
  | Ast.Float kind -> Diagnostic_float kind
  | Ast.Addr -> Diagnostic_addr
  | Ast.Handle ty -> Diagnostic_handle (diagnostic_type_of_ast specializations ty)
  | Ast.Array (length, ty) ->
      Diagnostic_array (length.Ast.text, diagnostic_type_of_ast specializations ty)
  | Ast.Vec (length, ty) ->
      Diagnostic_vec (length.Ast.text, diagnostic_type_of_ast specializations ty)
  | Ast.Named_type (name, _) -> (
      match find_by_name specializations Struct_specialization name with
      | Some specialization -> (
          match current_instantiation_frame specialization.trace with
          | Some frame -> Diagnostic_applied (frame.template_name, frame.arguments)
          | None -> Diagnostic_named name)
      | None -> Diagnostic_named name)
  | Ast.Applied_type (name, _, arguments, _) ->
      let arguments =
        List.map
          (function
            | Ast.Type_arg ty | Ast.Type_or_index ty ->
                Diagnostic_type_argument (diagnostic_type_of_ast specializations ty)
            | Ast.Const_arg expression ->
                Diagnostic_source_const_argument (Ast.expr_name expression)
            | Ast.Name_arg (name, _) -> Diagnostic_type_argument (Diagnostic_named name))
          arguments
      in
      Diagnostic_applied (name, arguments)

let render_diagnostic_const ty value =
  match ty with
  | Hir.Bool -> if value = 0L then "false" else "true"
  | Hir.Int kind ->
      let bits = Sema_numeric.int_bits kind in
      if Sema_numeric.is_unsigned ty then
        let value = Sema_numeric.mask_value ty value in
        if bits = 64 then Printf.sprintf "%Lu" value else Int64.to_string value
      else Int64.to_string (Sema_numeric.sign_extend_value ty value)
  | _ -> Int64.to_string value

let rec render_diagnostic_type = function
  | Diagnostic_bool -> "bool"
  | Diagnostic_void -> "void"
  | Diagnostic_int kind -> Ast.type_name (Ast.Int kind)
  | Diagnostic_float kind -> Ast.type_name (Ast.Float kind)
  | Diagnostic_ptr ty -> "ptr[" ^ render_diagnostic_type ty ^ "]"
  | Diagnostic_const_ptr ty -> "ptr[const " ^ render_diagnostic_type ty ^ "]"
  | Diagnostic_addr -> "addr"
  | Diagnostic_handle ty -> "handle[" ^ render_diagnostic_type ty ^ "]"
  | Diagnostic_array (length, ty) ->
      "arr[" ^ length ^ ", " ^ render_diagnostic_type ty ^ "]"
  | Diagnostic_vec (length, ty) ->
      "vec[" ^ length ^ ", " ^ render_diagnostic_type ty ^ "]"
  | Diagnostic_named name -> name
  | Diagnostic_applied (name, arguments) ->
      name ^ "["
      ^ String.concat ", " (List.map render_diagnostic_argument arguments)
      ^ "]"

and render_diagnostic_argument = function
  | Diagnostic_type_argument ty -> render_diagnostic_type ty
  | Diagnostic_const_argument (ty, value) -> render_diagnostic_const ty value
  | Diagnostic_source_const_argument value -> value

let render_instantiation_application frame =
  frame.template_name ^ "["
  ^ String.concat ", " (List.map render_diagnostic_argument frame.arguments)
  ^ "]"

let instantiation_note frame =
  let application = render_instantiation_application frame in
  Printf.sprintf "while instantiating `%s` at %s" application
    (Span.to_string frame.application_span)

let replace_all text target replacement =
  let target_length = String.length target in
  if target_length = 0 then text
  else
    let buffer = Buffer.create (String.length text) in
    let rec go offset =
      if offset + target_length > String.length text then
        Buffer.add_substring buffer text offset (String.length text - offset)
      else if String.sub text offset target_length = target then (
        Buffer.add_string buffer replacement;
        go (offset + target_length))
      else (
        Buffer.add_char buffer text.[offset];
        go (offset + 1))
    in
    go 0;
    Buffer.contents buffer

let source_name_replacements specializations =
  fold_by_name
    (fun (_, name) specialization replacements ->
      match
        (specialization.pending_frame, current_instantiation_frame specialization.trace)
      with
      | None, Some frame ->
          (name, render_instantiation_application frame) :: replacements
      | _ -> replacements)
    specializations []
  |> List.sort (fun (left, _) (right, _) ->
      let by_length = Int.compare (String.length right) (String.length left) in
      if by_length <> 0 then by_length else String.compare left right)

let source_facing_text specializations text =
  List.fold_left
    (fun text (generated, source) -> replace_all text generated source)
    text
    (source_name_replacements specializations)

let append_instantiation_trace specializations trace diagnostics =
  let notes = List.map instantiation_note trace in
  List.map
    (fun (diagnostic : Diag.t) ->
      let local_declaration_syntax =
        Diag.is_local_declaration_message diagnostic.message
      in
      {
        diagnostic with
        message = source_facing_text specializations diagnostic.message;
        notes =
          (List.map (source_facing_text specializations) diagnostic.notes
          @ if local_declaration_syntax then [] else notes);
        help = Option.map (source_facing_text specializations) diagnostic.help;
      })
    diagnostics

let trace_result specializations trace =
  Result.map_error (append_instantiation_trace specializations trace)

let specialization_trace specializations kind name =
  match find_by_name specializations kind name with
  | Some specialization -> specialization.trace
  | None -> []

let specialization_source_name specializations kind name =
  match
    current_instantiation_frame (specialization_trace specializations kind name)
  with
  | Some frame -> frame.template_name
  | None -> name

let specialization_application_name specializations kind name =
  match find_by_name specializations kind name with
  | Some specialization ->
      Option.map render_instantiation_application
        (current_instantiation_frame specialization.trace)
  | None -> None

let error span message = Error [ Diag.error span message ]

let ( let* ) result continuation =
  match result with
  | Error diagnostics -> Error diagnostics
  | Ok value -> continuation value

let const_key_value (t : Hir.ty) v = Hir.ty_name t ^ ":" ^ Int64.to_string v

let mangle_specialization base values =
  base ^ Ast.specialization_name_delimiter
  ^ string_of_int (String.length base)
  ^ ":"
  ^ String.concat ";"
      (List.map
         (fun (n, t, v) ->
           string_of_int (String.length n) ^ ":" ^ n ^ "=" ^ const_key_value t v)
         values)

let function_specialization_key declaration_id values =
  ( Function_specialization,
    declaration_id,
    List.map (fun (_, t, v) -> Const_specialization_arg (t, v)) values )

let staged_specialization_identity state name values =
  let staged =
    match find_by_name state Function_specialization name with
    | Some
        {
          key = Function_specialization, origin_id, _;
          payload =
            Function_payload
              {
                item = Ast.Func { name = origin_name; _ };
                staged_args = Some arguments;
                _;
              };
          pending_frame = Some pending;
          _;
        } ->
        Some (origin_id, origin_name, arguments, pending)
    | _ -> None
  in
  match staged with
  | None -> Ok None
  | Some (origin_id, origin_name, arguments, pending) ->
      let rec resolve acc = function
        | [] -> Ok (List.rev acc)
        | Staged_type_arg key :: rest ->
            resolve (Type_specialization_arg key :: acc) rest
        | Staged_const_arg name :: rest -> (
            match List.find_opt (fun (parameter, _, _) -> parameter = name) values with
            | Some (_, ty, value) ->
                resolve (Const_specialization_arg (ty, value) :: acc) rest
            | None ->
                error Span.synthetic
                  "internal error: staged const specialization argument is missing")
      in
      let* arguments = resolve [] arguments in
      let rec resolve_diagnostic acc = function
        | [] -> Ok (List.rev acc)
        | Pending_diagnostic_type ty :: rest ->
            resolve_diagnostic (Diagnostic_type_argument ty :: acc) rest
        | Pending_diagnostic_const name :: rest -> (
            match List.find_opt (fun (parameter, _, _) -> parameter = name) values with
            | Some (_, ty, value) ->
                resolve_diagnostic (Diagnostic_const_argument (ty, value) :: acc) rest
            | None ->
                error Span.synthetic
                  "internal error: staged const diagnostic argument is missing")
      in
      let* diagnostic_arguments = resolve_diagnostic [] pending.pending_arguments in
      Ok
        (Some
           ( origin_id,
             origin_name,
             arguments,
             diagnostic_arguments,
             pending.pending_application_span ))

let mangle_mixed_specialization base arguments =
  let argument_name = function
    | Type_specialization_arg key -> "t" ^ string_of_int (String.length key) ^ ":" ^ key
    | Const_specialization_arg (ty, value) ->
        let key = const_key_value ty value in
        "c" ^ string_of_int (String.length key) ^ ":" ^ key
  in
  base ^ Ast.specialization_name_delimiter
  ^ String.concat ";" (List.map argument_name arguments)

let rec specialization_type_key = function
  | Ast.Bool -> "bool"
  | Ast.Void -> "void"
  | Ast.Int kind -> Ast.type_name (Ast.Int kind)
  | Ast.Float kind -> Ast.type_name (Ast.Float kind)
  | Ast.Addr -> "addr"
  | Ast.Handle ty ->
      let key = specialization_type_key ty in
      "handle" ^ string_of_int (String.length key) ^ "_" ^ key
  | Ast.Array (length, ty) ->
      let length =
        try string_of_int (int_of_string length.text) with Failure _ -> length.text
      in
      let key = specialization_type_key ty in
      "arr" ^ length ^ "_" ^ string_of_int (String.length key) ^ "_" ^ key
  | Ast.Vec (length, ty) ->
      let length =
        try string_of_int (int_of_string length.text) with Failure _ -> length.text
      in
      let key = specialization_type_key ty in
      "vec" ^ length ^ "_" ^ string_of_int (String.length key) ^ "_" ^ key
  | Ast.Named_type (name, _) ->
      "named" ^ string_of_int (String.length name) ^ "_" ^ name
  | Ast.Applied_type (name, _, _, _) ->
      "applied" ^ string_of_int (String.length name) ^ "_" ^ name

let mangle_type_specialization base arguments =
  base ^ Ast.specialization_name_delimiter
  ^ String.concat "$"
      (List.map
         (fun ty ->
           let key = specialization_type_key ty in
           string_of_int (String.length key) ^ "_" ^ key)
         arguments)
