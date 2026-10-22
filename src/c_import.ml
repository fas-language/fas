module Hashtbl = C_import_json.Hashtbl

module PairHashtbl = Stdlib.Hashtbl.Make (struct
  type t = string * string

  let equal (a, b) (c, d) = String.equal a c && String.equal b d
  let hash = Stdlib.Hashtbl.hash
end)

let string_index entries = Hashtbl.of_seq (List.to_seq (List.rev entries))

type header = { spelling : Ast.c_header; span : Span.t }

type static_function = {
  name : string;
  return_type : string;
  parameter_types : string list;
  variadic : bool;
  void_result : bool;
  return_function_pointer : bool;
  record_parameters : int list;
  record_result : bool;
  signature : string;
  span : Span.t;
}

type adapter = {
  c_name : string;
  symbol : string;
  code : string;
  file : string;
  line : int;
  signature : string;
}

let include_line source fragment_path = function
  | { spelling = Ast.C_quoted path; _ } ->
      let path =
        if Filename.is_relative path then Filename.concat (Filename.dirname source) path
        else path
      in
      Printf.sprintf "#include %S\n" path
  | { spelling = Ast.C_system path; _ } -> "#include <" ^ path ^ ">\n"
  | { spelling = Ast.C_fragment _; _ } ->
      Printf.sprintf "#include %S\n" (Option.get fragment_path)

let find_text text needle start =
  let limit = String.length text - String.length needle in
  let rec find index =
    if index > limit then None
    else if String.sub text index (String.length needle) = needle then Some index
    else find (index + 1)
  in
  find start

let first_error output =
  String.split_on_char '\n' output
  |> List.find_opt (fun line -> Option.is_some (find_text line "error:" 0))

let missing_header_path message =
  let suffix = " file not found" in
  if not (String.ends_with ~suffix message) then None
  else
    let quoted = String.sub message 0 (String.length message - String.length suffix) in
    let last = String.length quoted - 1 in
    if last < 0 || (quoted.[last] <> '\'' && quoted.[last] <> '"') then None
    else
      let quote = quoted.[last] in
      let rec opening start =
        if start < 0 then None
        else if quoted.[start] = quote then Some start
        else opening (start - 1)
      in
      match opening (last - 1) with
      | Some start -> Some (String.sub quoted (start + 1) (last - start - 1))
      | _ -> None

let error_location line =
  match find_text line "error:" 0 with
  | None -> None
  | Some error_at -> (
      let error_at =
        if error_at >= 6 && String.sub line (error_at - 6) 6 = "fatal " then
          error_at - 6
        else error_at
      in
      let prefix = String.trim (String.sub line 0 error_at) in
      let prefix =
        if String.ends_with ~suffix:":" prefix then
          String.sub prefix 0 (String.length prefix - 1)
        else prefix
      in
      match String.rindex_opt prefix ':' with
      | None -> None
      | Some column_end -> (
          let before_column = String.sub prefix 0 column_end in
          match String.rindex_opt before_column ':' with
          | None -> None
          | Some line_end ->
              let file = String.sub before_column 0 line_end in
              let line =
                String.sub before_column (line_end + 1)
                  (String.length before_column - line_end - 1)
              and column =
                String.sub prefix (column_end + 1)
                  (String.length prefix - column_end - 1)
              in
              Option.bind (int_of_string_opt line) (fun line ->
                  Option.map
                    (fun column -> (file, line, column))
                    (int_of_string_opt column))))

let compilation_error ?source ?(headers = []) ?(prefix = "C compilation failed")
    fallback output =
  let line = Option.value ~default:(String.trim output) (first_error output) in
  let raw_message =
    match find_text line "error:" 0 with
    | None -> line
    | Some start ->
        String.trim (String.sub line (start + 6) (String.length line - start - 6))
  in
  let message, missing_header =
    match missing_header_path raw_message with
    | None -> (raw_message, false)
    | Some path ->
        let spelling =
          Option.bind source (fun source ->
              List.find_map
                (fun header ->
                  match header.spelling with
                  | Ast.C_quoted spelling ->
                      let resolved =
                        if Filename.is_relative spelling then
                          Filename.concat (Filename.dirname source) spelling
                        else spelling
                      in
                      if resolved = path then Some spelling else None
                  | Ast.C_system _ | Ast.C_fragment _ -> None)
                headers)
          |> Option.value ~default:path
        in
        (Printf.sprintf "C header `%s` not found" spelling, true)
  in
  let span, notes =
    match error_location line with
    | Some (file, line, column) when Filename.check_suffix file ".fas" ->
        (Span.make ~file ~start_offset:0 ~end_offset:0 ~line ~column, [])
    | Some (file, line, column) when prefix = "assembly failed" ->
        (Span.make ~file ~start_offset:0 ~end_offset:0 ~line ~column, [])
    | Some (file, line, column) ->
        let notes =
          if String.starts_with ~prefix:"fas-c-import-" (Filename.basename file) then []
          else [ Printf.sprintf "%s:%d:%d" file line column ]
        in
        (fallback, notes)
    | None -> (fallback, [])
  in
  Diag.error ~notes span
    (if missing_header then message
     else if message = "" then prefix
     else prefix ^ ": " ^ message)

let unit_line unit_path output =
  match find_text output (unit_path ^ ":") 0 with
  | None -> None
  | Some start -> (
      let digit = start + String.length unit_path + 1 in
      let finish = try String.index_from output digit ':' with Not_found -> digit in
      try Some (int_of_string (String.sub output digit (finish - digit)))
      with Failure _ -> None)

let error_span (headers : header list) line =
  Option.bind line (fun line -> List.nth_opt headers (line - 1)) |> function
  | None -> (List.hd headers).span
  | Some header -> header.span

let replace_all text needle replacement =
  let output = Buffer.create (String.length text) in
  let rec copy offset =
    match find_text text needle offset with
    | Some index ->
        Buffer.add_substring output text offset (index - offset);
        Buffer.add_string output replacement;
        copy (index + String.length needle)
    | None -> Buffer.add_substring output text offset (String.length text - offset)
  in
  if needle = "" then text
  else (
    copy 0;
    Buffer.contents output)

let normalize_import_failure ~unit_path paths output =
  let digest = Digest.to_hex (Digest.string unit_path) in
  let output = replace_all output digest "<hash>" in
  unit_path :: paths
  |> List.sort (fun left right ->
      Int.compare (String.length right) (String.length left))
  |> List.fold_left (fun output path -> replace_all output path "<C import>") output

let macro_definitions text names =
  let wanted = Hashtbl.create 16 and definitions = Hashtbl.create 16 in
  List.iter (fun name -> Hashtbl.replace wanted name ()) names;
  let real_file = ref false and file_name = ref "" and line_number = ref 0 in
  let take prefix line =
    if not (String.starts_with ~prefix line) then None
    else
      let start = String.length prefix and stop = ref (String.length prefix) in
      while
        !stop < String.length line
        &&
        match line.[!stop] with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
        | _ -> false
      do
        incr stop
      done;
      if !stop = start then None
      else
        Some
          ( String.sub line start (!stop - start),
            !stop < String.length line && line.[!stop] = '(',
            String.trim (String.sub line !stop (String.length line - !stop)) <> "" )
  in
  let marker line =
    match String.index_opt line '"' with
    | None -> None
    | Some first -> (
        match String.index_from_opt line (first + 1) '"' with
        | None -> None
        | Some last ->
            Option.map
              (fun number -> (String.sub line (first + 1) (last - first - 1), number))
              (int_of_string_opt (String.trim (String.sub line 2 (first - 2)))))
  in
  String.split_on_char '\n' text
  |> List.iter (fun line ->
      if String.starts_with ~prefix:"# " line then
        begin match marker line with
        | Some (name, number) when not (String.starts_with ~prefix:"<" name) ->
            real_file := true;
            file_name := name;
            line_number := number
        | _ -> real_file := false
        end
      else begin
        if !real_file then
          begin match take "#define " line with
          | Some (name, function_like, has_value) when Hashtbl.mem wanted name ->
              Hashtbl.replace definitions name
                (function_like, has_value, (!file_name, !line_number))
          | _ -> (
              match take "#undef " line with
              | Some (name, _, _) -> Hashtbl.remove definitions name
              | None -> ())
          end;
        incr line_number
      end);
  List.filter_map
    (fun name ->
      Option.map
        (fun (function_like, has_value, origin) ->
          (name, function_like, has_value, origin))
        (Hashtbl.find_opt definitions name))
    names

let macro_type = function
  | (1 | 2) as code ->
      Some ((if code = 1 then "signed char" else "unsigned char"), 8, code = 2)
  | (3 | 4) as code ->
      Some ((if code = 3 then "short" else "unsigned short"), 16, code = 4)
  | (5 | 6) as code -> Some ((if code = 5 then "int" else "unsigned int"), 32, code = 6)
  | (7 | 8) as code ->
      Some ((if code = 7 then "long" else "unsigned long"), 64, code = 8)
  | (9 | 10) as code ->
      Some ((if code = 9 then "long long" else "unsigned long long"), 64, code = 10)
  | 11 -> Some ("_Bool", 8, true)
  | 12 -> Some ("char", 8, false)
  | _ -> None

let macro_probe_types =
  "signed char:1, unsigned char:2, short:3, unsigned short:4, int:5, "
  ^ "unsigned int:6, long:7, unsigned long:8, long long:9, "
  ^ "unsigned long long:10, _Bool:11, char:12, default:0"

let llvm_integer_index ir =
  let integers = Hashtbl.create 128
  and value =
    let rec find = function
      | "constant" :: ty :: literal :: _ when String.starts_with ~prefix:"i" ty ->
          let literal = String.trim literal in
          Some
            ( ty,
              if String.ends_with ~suffix:"," literal then
                String.sub literal 0 (String.length literal - 1)
              else literal )
      | _ :: rest -> find rest
      | [] -> None
    in
    find
  in
  String.split_on_char '\n' ir
  |> List.iter (fun line ->
      let line = String.trim line in
      match String.index_opt line '=' with
      | Some equal when String.starts_with ~prefix:"@" line -> (
          let name = String.sub line 1 (equal - 1) |> String.trim in
          let fields = String.split_on_char ' ' line |> List.filter (( <> ) "") in
          match value fields with
          | Some value when not (Hashtbl.mem integers name) ->
              Hashtbl.add integers name value
          | _ -> ())
      | _ -> ());
  integers

let alloc_size_parameters ir =
  let number line start =
    let stop = ref start in
    while !stop < String.length line && line.[!stop] >= '0' && line.[!stop] <= '9' do
      incr stop
    done;
    if !stop = start then None
    else int_of_string_opt (String.sub line start (!stop - start))
  in
  let lines = String.split_on_char '\n' ir
  and groups = Stdlib.Hashtbl.create 16
  and found = Hashtbl.create 16 in
  let indices line =
    match find_text line "allocsize(" 0 with
    | None -> []
    | Some at ->
        Option.fold ~none:[]
          ~some:(fun stop ->
            String.sub line (at + 10) (stop - at - 10)
            |> String.split_on_char ','
            |> List.filter_map (fun value ->
                Option.map succ (int_of_string_opt (String.trim value))))
          (String.index_from_opt line (at + 10) ')')
  in
  List.iter
    (fun line ->
      Option.bind (find_text line "attributes #" 0) (fun at -> number line (at + 12))
      |> Option.iter (fun group -> Stdlib.Hashtbl.replace groups group (indices line)))
    lines;
  List.iter
    (fun line ->
      if String.starts_with ~prefix:"declare " (String.trim line) then
        Option.iter
          (fun at ->
            Option.iter
              (fun stop ->
                let quoted = line.[at + 1] = '"' in
                let name =
                  String.sub line
                    (at + 1 + if quoted then 1 else 0)
                    (stop - at - 1 - if quoted then 1 else 0)
                in
                let group =
                  Option.bind (find_text line "#" stop) (fun hash ->
                      number line (hash + 1))
                in
                Option.bind group (Stdlib.Hashtbl.find_opt groups)
                |> Option.iter (fun values ->
                    if values <> [] then
                      Hashtbl.replace found name
                        (values
                        :: Option.value ~default:[] (Hashtbl.find_opt found name))))
              (String.index_from_opt line at '('))
          (find_text line "@" 0))
    lines;
  Hashtbl.fold
    (fun name values acc -> (name, List.sort_uniq compare values) :: acc)
    found []
  |> List.sort compare

let fas_macro_value width unsigned literal =
  let value = Int64.of_string literal in
  if unsigned && width < 64 then
    Int64.logand value (Int64.sub (Int64.shift_left 1L width) 1L) |> Int64.to_string
  else if unsigned && value < 0L then Printf.sprintf "0x%Lx" value
  else Int64.to_string value

let c_import_args ~cc ~c_flags ~source =
  [ cc; "-x"; "c"; "--target=x86_64-unknown-linux-gnu" ]
  @ c_flags
  @ [ "-iquote"; Filename.dirname source ]

let imported_macro_definitions ~cc ~c_flags ~source ~unit_path ~macro_names =
  if macro_names = [] then Ok []
  else
    let argv =
      Array.of_list (c_import_args ~cc ~c_flags ~source @ [ "-E"; "-dD"; unit_path ])
    in
    match Process.run argv with
    | Error failure -> Error failure.stderr
    | Ok (text, _) -> Ok (macro_definitions text macro_names)

let imported_macro_expansions ~cc ~c_flags ~source ~unit_path ~paths candidates =
  if candidates = [] then Ok []
  else
    let probe = Filename.temp_file "fas-c-macro-expand-" ".c" in
    paths := probe :: !paths;
    let digest = Digest.to_hex (Digest.string unit_path) in
    let marker index side = Printf.sprintf "__fas_macro_%s_%s_%d" digest side index in
    let output = open_out_bin probe in
    Printf.fprintf output "#include %S\n" unit_path;
    List.iteri
      (fun index (name, _, _, _) ->
        Printf.fprintf output "%s %s %s\n" (marker index "b") name (marker index "e"))
      candidates;
    close_out output;
    match
      Process.run
        (Array.of_list (c_import_args ~cc ~c_flags ~source @ [ "-E"; "-P"; probe ]))
    with
    | Error failure -> Error failure.stderr
    | Ok (text, _) ->
        let lines = String.split_on_char '\n' text in
        Ok
          (List.mapi
             (fun index (name, _, _, _) ->
               let first = marker index "b" and last = marker index "e" in
               let prefix = first ^ " " and suffix = " " ^ last in
               let expansion =
                 List.find_map
                   (fun line ->
                     if String.starts_with ~prefix line && String.ends_with ~suffix line
                     then
                       Some
                         (String.sub line (String.length prefix)
                            (String.length line - String.length prefix
                           - String.length suffix)
                         |> String.trim)
                     else None)
                   lines
               in
               (name, expansion))
             candidates)

let macro_float_literal expansion =
  let rec strip text =
    let text = String.trim text in
    if String.length text < 2 || text.[0] <> '(' then text
    else
      let depth = ref 0 and closing = ref (-1) in
      String.iteri
        (fun i c ->
          if !closing < 0 then
            if c = '(' then incr depth
            else if c = ')' then (
              decr depth;
              if !depth = 0 then closing := i))
        text;
      if !closing = String.length text - 1 then
        strip (String.sub text 1 (String.length text - 2))
      else text
  in
  let text = strip expansion in
  let negative, text =
    if String.length text > 0 && text.[0] = '-' then
      (true, strip (String.sub text 1 (String.length text - 1)))
    else (false, text)
  in
  match Float_parse.literal_kind text land 3 with
  | 0 -> None
  | 3 -> Some (Error "long double types are not supported")
  | kind -> (
      let ty = if kind = 2 then Hir.Float Hir.F32 else Hir.Float Hir.F64 in
      match Sema_numeric.parse_float_literal ty text with
      | Error reason -> Some (Error reason)
      | Ok _ ->
          let type_name = if kind = 2 then "f32" else "f64" in
          Some
            (Ok
               ( String.sub text 0 (String.length text - if kind = 2 then 1 else 0),
                 type_name,
                 negative )))

let float_macro_node name origin result =
  let fields =
    (("name", C_import_json.Str name) :: origin)
    @
    match result with
    | Error reason -> [ ("reason", C_import_json.Str reason) ]
    | Ok (literal, type_name, negative) ->
        [
          ("type", C_import_json.Str type_name);
          ("literal", C_import_json.Str literal);
          ("negative", C_import_json.Bool negative);
        ]
  in
  C_import_json.make_obj (("kind", C_import_json.Str "FasFloatMacro") :: fields)

type builtin_info = Integer of Ast.ty | Unsupported_integer | Floating of int

let builtin_info_of_name = function
  | "_Bool" | "bool" -> Some (Integer Ast.Bool)
  | "void" -> Some (Integer Ast.Void)
  | "char" | "signed char" -> Some (Integer (Ast.Int Ast.I8))
  | "unsigned char" -> Some (Integer (Ast.Int Ast.U8))
  | "short" -> Some (Integer (Ast.Int Ast.I16))
  | "unsigned short" -> Some (Integer (Ast.Int Ast.U16))
  | "int" -> Some (Integer (Ast.Int Ast.I32))
  | "unsigned int" -> Some (Integer (Ast.Int Ast.U32))
  | "long" | "long long" -> Some (Integer (Ast.Int Ast.I64))
  | "unsigned long" | "unsigned long long" -> Some (Integer (Ast.Int Ast.U64))
  | "__int128" | "unsigned __int128" -> Some Unsupported_integer
  | "float" -> Some (Floating 32)
  | "double" -> Some (Floating 64)
  | "long double" | "__fp16" | "_Float16" | "_Float128" | "__float128" | "__bf16" ->
      Some (Floating 0)
  | _ -> None

let imported_structured_type_nodes ~cc ~c_flags ~source ~unit_path ~paths
    ?referenced_names ~probe_all_declarations ~macro_candidates declarations =
  let field key node = C_import_json.field key node in
  let string key node = Option.bind (field key node) C_import_json.string in
  let children node =
    Option.fold ~none:[] ~some:C_import_json.array (field "inner" node)
  in
  let type_node node =
    children node
    |> List.find_opt (fun child ->
        Option.fold ~none:false
          ~some:(String.ends_with ~suffix:"Type")
          (string "kind" child))
  in
  let referenced name =
    probe_all_declarations
    || Option.fold ~none:true ~some:(List.exists (String.equal name)) referenced_names
  in
  let rec flatten acc node = List.fold_left flatten (node :: acc) (children node) in
  let id node = string "id" node in
  let name node = string "name" node in
  let kind node = string "kind" node in
  let source_offset node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (field "expansionLoc" loc))
        (field "loc" node)
    in
    Option.bind loc (fun loc ->
        Option.bind (field "offset" loc) (function
          | C_import_json.Num value -> int_of_string_opt value
          | C_import_json.Str value -> int_of_string_opt value
          | _ -> None))
  in
  let source_key node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (field "expansionLoc" loc))
        (field "loc" node)
    in
    let file =
      Option.bind loc (fun loc ->
          match string "presumedFile" loc with
          | Some _ as file -> file
          | None -> string "file" loc)
    in
    Option.bind file (fun file ->
        Option.map
          (fun offset -> "field:" ^ file ^ ":" ^ string_of_int offset)
          (source_offset node))
  in
  let all_nodes = List.rev (List.fold_left flatten [] declarations) in
  let alias_types_by_id = Hashtbl.create 32 in
  List.iter
    (fun node ->
      if kind node = Some "TypedefDecl" then
        Option.iter
          (fun id ->
            Option.iter (Hashtbl.replace alias_types_by_id id) (type_node node))
          (id node))
    all_nodes;
  let rec direct_record_id seen node =
    match kind node with
    | Some "RecordType" -> Option.bind (field "decl" node) (string "id")
    | Some "TypedefType" -> (
        match Option.bind (field "decl" node) (string "id") with
        | Some id when not (List.mem id seen) ->
            Option.bind
              (Hashtbl.find_opt alias_types_by_id id)
              (direct_record_id (id :: seen))
        | _ -> None)
    | Some
        ( "ElaboratedType" | "AttributedType" | "ParenType" | "QualType"
        | "MacroQualifiedType" | "ConstantArrayType" | "IncompleteArrayType" ) ->
        children node |> List.find_map (direct_record_id seen)
    | _ -> None
  in
  let primary_decl_ids = PairHashtbl.create (List.length all_nodes)
  and primary_type_ids = PairHashtbl.create 64 in
  let anonymous_enum_type_name node =
    Option.bind (field "loc" node) (fun loc ->
        Option.bind (string "file" loc) (fun file ->
            let number key =
              Option.bind (field key loc) (function
                | C_import_json.Num value -> int_of_string_opt value
                | C_import_json.Str value -> int_of_string_opt value
                | _ -> None)
            in
            Option.bind (number "line") (fun line ->
                Option.map
                  (Printf.sprintf "enum (unnamed at %s:%d:%d)" file line)
                  (number "col"))))
  in
  List.iter
    (fun node ->
      (match (kind node, id node, name node) with
      | Some (("TypedefDecl" | "RecordDecl" | "EnumDecl") as kind), Some id, Some name
        ->
          PairHashtbl.replace primary_decl_ids (kind, name) id
      | Some "EnumDecl", Some id, None ->
          Option.iter
            (fun type_name ->
              PairHashtbl.replace primary_type_ids ("EnumType", type_name) id)
            (anonymous_enum_type_name node)
      | _ -> ());
      match
        ( kind node,
          Option.bind (field "decl" node) (string "id"),
          Option.bind (field "type" node) (string "qualType") )
      with
      | Some (("RecordType" | "EnumType") as kind), Some id, Some name ->
          PairHashtbl.replace primary_type_ids (kind, name) id
      | _ -> ())
    all_nodes;
  let top_declarations =
    List.filter
      (fun node ->
        List.mem (kind node)
          [ Some "FunctionDecl"; Some "VarDecl"; Some "EnumConstantDecl" ]
        && (kind node = Some "EnumConstantDecl"
           || Option.fold ~none:false ~some:referenced (name node))
        && field "isImplicit" node <> Some (C_import_json.Bool true))
      all_nodes
  in
  let is_builtin_function node =
    kind node = Some "FunctionDecl"
    && List.exists (fun child -> kind child = Some "BuiltinAttr") (children node)
  in
  let alias_names_by_record = Hashtbl.create 32
  and record_base_names_by_id = Hashtbl.create 32 in
  let rec array_alias_type node =
    match kind node with
    | Some ("ConstantArrayType" | "IncompleteArrayType") -> true
    | Some "TypedefType" ->
        Option.bind (field "decl" node) (fun declaration ->
            Option.bind (string "id" declaration) (Hashtbl.find_opt alias_types_by_id))
        |> Option.fold ~none:false ~some:array_alias_type
    | _ -> List.exists array_alias_type (children node)
  in
  let add_alias table record_id alias =
    Hashtbl.replace table record_id
      (alias :: Option.value ~default:[] (Hashtbl.find_opt table record_id))
  in
  List.iter
    (fun node ->
      if kind node = Some "TypedefDecl" then
        match (name node, type_node node) with
        | Some alias, Some alias_type ->
            Option.iter
              (fun record_id ->
                add_alias alias_names_by_record record_id alias;
                if not (array_alias_type alias_type) then
                  Hashtbl.replace record_base_names_by_id record_id alias)
              (direct_record_id [] alias_type)
        | _ -> ())
    declarations;
  let roots = List.filter (fun node -> kind node = Some "RecordDecl") all_nodes in
  let probes = Hashtbl.create (List.length top_declarations + 64) in
  let add_probe ?(names = []) target expression =
    Option.iter (fun target -> Hashtbl.replace probes target (expression, names)) target
  in
  let strip_nullability value =
    let output = Buffer.create (String.length value) in
    let identifier_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    let rec scan offset =
      if offset < String.length value then
        if identifier_char value.[offset] then (
          let finish = ref (offset + 1) in
          while !finish < String.length value && identifier_char value.[!finish] do
            incr finish
          done;
          let token = String.sub value offset (!finish - offset) in
          if not (List.mem token [ "_Nonnull"; "_Nullable"; "_Null_unspecified" ]) then
            Buffer.add_substring output value offset (!finish - offset);
          scan !finish)
        else (
          Buffer.add_char output value.[offset];
          scan (offset + 1))
    in
    scan 0;
    Buffer.contents output
  in
  let has_restrict_qualifier value =
    String.map
      (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c | _ -> ' ')
      value
    |> String.split_on_char ' '
    |> List.exists (fun qualifier ->
        List.mem qualifier [ "restrict"; "__restrict"; "__restrict__" ])
  in
  List.iter
    (fun node ->
      let c_name = Option.value ~default:"" (name node) in
      if not (is_builtin_function node) then add_probe (Some ("decl:" ^ c_name)) c_name)
    top_declarations;
  List.iter
    (fun node ->
      match (kind node, name node) with
      | Some "FunctionDecl", Some function_name ->
          children node
          |> List.filter (fun child -> kind child = Some "ParmVarDecl")
          |> List.iteri (fun index parameter ->
              Option.iter
                (fun parameter_type ->
                  if has_restrict_qualifier parameter_type then
                    add_probe
                      (Some (Printf.sprintf "parameter:%s:%d" function_name index))
                      (strip_nullability parameter_type))
                (Option.bind (field "type" parameter) (string "qualType")))
      | _ -> ())
    top_declarations;
  let collision_names = Hashtbl.create 16 in
  List.iter
    (fun root ->
      if field "completeDefinition" root = Some (C_import_json.Bool true) then
        let root_id = id root in
        let base =
          match (string "tagUsed" root, name root, root_id) with
          | Some tag, Some record_name, _ -> Some (tag ^ " " ^ record_name, record_name)
          | _, None, Some id ->
              Option.map
                (fun alias -> (alias, alias))
                (Hashtbl.find_opt record_base_names_by_id id)
          | _ -> None
        in
        Option.iter
          (fun (base_type, root_name) ->
            let fields = ref [] in
            let rec collect record =
              let nested_records =
                children record
                |> List.filter (fun child -> kind child = Some "RecordDecl")
              in
              children record
              |> List.iter (fun child ->
                  if kind child = Some "FieldDecl" then
                    match name child with
                    | Some field_name
                      when field "isBitfield" child <> Some (C_import_json.Bool true) ->
                        Option.iter
                          (fun target ->
                            fields :=
                              ( field_name,
                                target,
                                ( "((" ^ base_type ^ " *)0)->" ^ field_name,
                                  [ field_name ] ) )
                              :: !fields)
                          (source_key child)
                    | None ->
                        Option.iter collect
                          (List.find_opt
                             (fun nested -> source_offset nested = source_offset child)
                             nested_records)
                    | _ -> ())
            in
            collect root;
            let by_name = Hashtbl.create 16 in
            List.iter
              (fun (field_name, _, _) ->
                Hashtbl.replace by_name field_name
                  (1 + Option.value ~default:0 (Hashtbl.find_opt by_name field_name)))
              !fields;
            let duplicates =
              Hashtbl.fold
                (fun field_name count acc ->
                  if count > 1 then field_name :: acc else acc)
                by_name []
            in
            if duplicates <> [] then Hashtbl.replace collision_names root_name ();
            List.iter
              (fun (field_name, target, (expression, names)) ->
                if not (List.mem field_name duplicates) then
                  add_probe ~names (Some target) expression)
              !fields)
          base)
    roots;
  let candidates =
    Hashtbl.fold (fun target probe acc -> (target, probe) :: acc) probes []
  in
  let probe_prefix = "__fas_probe_" ^ Digest.to_hex (Digest.string unit_path) ^ "_" in
  let prefix = probe_prefix ^ "type_" in
  let macro_prefix = probe_prefix ^ "macro_" in
  let indexed =
    List.mapi (fun index (target, expression) -> (index, target, expression)) candidates
  in
  let should_run = indexed <> [] || macro_candidates <> [] in
  let probe = Filename.temp_file "fas-c-type-probe-" ".c" in
  paths := probe :: !paths;
  let macro_start_line =
    List.fold_left
      (fun line (_, _, (_, names)) -> line + 1 + (3 * List.length names))
      2 indexed
  in
  let append_type_probes () =
    let out = open_out_bin probe in
    Fun.protect
      ~finally:(fun () -> close_out_noerr out)
      (fun () ->
        Printf.fprintf out "#include %S\n" unit_path;
        List.iter
          (fun (index, _, (expression, names)) ->
            List.iter
              (fun name ->
                Printf.fprintf out "#pragma push_macro(%S)\n#undef %s\n" name name)
              names;
            Printf.fprintf out "typedef __typeof__(%s) %s%d;\n" expression prefix index;
            List.iter (Printf.fprintf out "#pragma pop_macro(%S)\n") (List.rev names))
          indexed;
        List.iteri
          (fun index (name, _, _, _) ->
            Printf.fprintf out "typedef __typeof__((%s)) %s%d;\n" name macro_prefix
              index)
          macro_candidates)
  in
  let common =
    [ "-x"; "c"; "--target=x86_64-unknown-linux-gnu" ]
    @ c_flags
    @ [ "-iquote"; Filename.dirname source ]
  in
  if should_run then append_type_probes ();
  let ast_path = Filename.temp_file "fas-c-type-probe-" ".json" in
  paths := ast_path :: !paths;
  let ast_argv =
    Array.of_list
      ([
         cc;
         "-fsyntax-only";
         "-Xclang";
         "-ast-dump=json";
         "-Xclang";
         "-skip-function-bodies";
         "-Xclang";
         "-ast-dump-filter=" ^ probe_prefix;
       ]
      @ common @ [ probe ])
  in
  let failure =
    if not should_run then None
    else
      match Process.run_to_file ast_argv ast_path with
      | Ok _ -> None
      | Error failure -> Some failure
  in
  let errors =
    Option.fold ~none:[]
      ~some:(fun (failure : Process.failure) ->
        String.split_on_char '\n' failure.stderr |> List.filter_map error_location)
      failure
  in
  let macro_error (file, line, _) =
    file = probe && line >= macro_start_line
    && line < macro_start_line + List.length macro_candidates
  in
  let bad_macros = Stdlib.Hashtbl.create 8 in
  List.iter
    (fun location ->
      if macro_error location then
        let _, line, _ = location in
        Stdlib.Hashtbl.replace bad_macros (line - macro_start_line) ())
    errors;
  let structured_failure =
    Option.bind failure (fun (failure : Process.failure) ->
        if errors <> [] && List.for_all macro_error errors then None
        else Some failure.stderr)
  in
  match structured_failure with
  | Some message -> Error message
  | None ->
      let ast_nodes =
        if not should_run then []
        else
          let ast_channel = open_in_bin ast_path in
          Fun.protect
            ~finally:(fun () -> close_in_noerr ast_channel)
            (fun () -> C_import_json.declarations ~filtered:true ast_channel)
      in
      let secondary_aliases = Hashtbl.create 32 in
      let rec collect_secondary_aliases node =
        Option.iter
          (fun declaration ->
            match (kind declaration, string "id" declaration, name declaration) with
            | Some "TypedefDecl", Some id, Some alias ->
                Hashtbl.replace secondary_aliases id alias
            | _ -> ())
          (if kind node = Some "TypedefType" then field "decl" node else None);
        List.iter collect_secondary_aliases
          (children node
          @ Option.to_list (field "decl" node)
          @ Option.to_list (field "type" node))
      in
      List.iter collect_secondary_aliases ast_nodes;
      let replace_field key value = function
        | C_import_json.Obj fields ->
            let replace index =
              let fields = Array.copy fields in
              Array.unsafe_set fields index (key, value);
              fields
            in
            C_import_json.Obj
              (Option.fold
                 ~none:(Array.append fields [| (key, value) |])
                 ~some:replace
                 (Array.find_index (fun (name, _) -> String.equal name key) fields))
        | node -> node
      in
      let remap_type_reference node =
        let remapped_decl =
          match kind node with
          | Some ("TypedefType" | "RecordType" | "EnumType") ->
              Option.bind (field "decl" node) (fun declaration ->
                  let decl_kind = string "kind" declaration in
                  let named_id =
                    Option.bind (name declaration) (fun name ->
                        if name = "" then None
                        else
                          Option.bind decl_kind (fun kind ->
                              PairHashtbl.find_opt primary_decl_ids (kind, name)))
                  in
                  let type_id =
                    match (decl_kind, string "kind" node) with
                    | Some ("RecordDecl" | "EnumDecl"), Some type_kind ->
                        Option.bind (field "type" node) (fun ty ->
                            Option.bind (string "qualType" ty) (fun type_name ->
                                PairHashtbl.find_opt primary_type_ids
                                  (type_kind, type_name)))
                    | _ -> None
                  in
                  Option.map
                    (fun id ->
                      replace_field "decl"
                        (replace_field "id" (C_import_json.Str id) declaration)
                        node)
                    (match named_id with Some _ -> named_id | None -> type_id))
          | _ -> None
        in
        let remapped_alias =
          Option.bind (field "typeAliasDeclId" node) (function
            | C_import_json.Str id ->
                Option.bind (Hashtbl.find_opt secondary_aliases id) (fun alias ->
                    PairHashtbl.find_opt primary_decl_ids ("TypedefDecl", alias))
            | _ -> None)
        in
        let node = Option.value ~default:node remapped_decl in
        Option.fold ~none:node
          ~some:(fun id -> replace_field "typeAliasDeclId" (C_import_json.Str id) node)
          remapped_alias
      in
      let rec remap_probe_node = function
        | C_import_json.Arr values ->
            C_import_json.Arr (List.map remap_probe_node values)
        | C_import_json.Obj fields ->
            remap_type_reference
              (C_import_json.Obj
                 (Array.map (fun (key, value) -> (key, remap_probe_node value)) fields))
        | value -> value
      in
      let ast_nodes = List.map remap_probe_node ast_nodes in
      let is_type_node node =
        Option.fold ~none:false ~some:(String.ends_with ~suffix:"Type") (kind node)
      in
      let probe_types =
        List.filter_map
          (fun node ->
            match (kind node, name node) with
            | Some "TypedefDecl", Some probe_name
              when String.starts_with ~prefix probe_name ->
                let index =
                  String.sub probe_name (String.length prefix)
                    (String.length probe_name - String.length prefix)
                in
                Option.bind (int_of_string_opt index) (fun index ->
                    match
                      ( List.nth_opt indexed index,
                        List.find_opt is_type_node (children node) )
                    with
                    | Some (_, target, (expression, _)), Some type_of_expr ->
                        Option.map
                          (fun tree ->
                            C_import_json.make_obj
                              [
                                ("kind", C_import_json.Str "FasTypeProbe");
                                ("target", C_import_json.Str target);
                                ("tree", tree);
                                ("expression", C_import_json.Str expression);
                              ])
                          (List.find_opt is_type_node
                             (List.rev (children type_of_expr)))
                    | _ -> None)
            | _ -> None)
          ast_nodes
      in
      let aliases = Hashtbl.create 32 in
      let rec collect_aliases node =
        if kind node = Some "TypedefType" then
          Option.iter
            (fun declaration ->
              if kind declaration = Some "TypedefDecl" then
                match (string "id" declaration, name declaration) with
                | Some id, Some alias -> Hashtbl.replace aliases id alias
                | _ -> ())
            (field "decl" node);
        List.iter collect_aliases (children node)
      in
      List.iter collect_aliases ast_nodes;
      let macro_infos =
        List.mapi
          (fun index _ ->
            let probe_name = macro_prefix ^ string_of_int index in
            let declaration =
              List.find_opt
                (fun node ->
                  kind node = Some "TypedefDecl" && name node = Some probe_name)
                ast_nodes
            in
            let ty = Option.bind declaration (field "type") in
            let type_name =
              Option.bind ty (fun ty ->
                  match string "desugaredQualType" ty with
                  | Some _ as name -> name
                  | None -> string "qualType" ty)
            in
            let alias =
              Option.bind ty (fun ty ->
                  Option.bind (string "typeAliasDeclId" ty) (Hashtbl.find_opt aliases))
            in
            (type_name, alias, not (Stdlib.Hashtbl.mem bad_macros index)))
          macro_candidates
      in
      let collision_nodes =
        Hashtbl.fold (fun name () acc -> name :: acc) collision_names []
        |> List.map (fun name ->
            C_import_json.make_obj
              [
                ("kind", C_import_json.Str "FasTypeCollision");
                ("name", C_import_json.Str name);
              ])
      in
      Ok (declarations @ probe_types @ collision_nodes, macro_infos)

let imported_probe_nodes ~cc ~c_flags ~source ~unit_path ~paths ~definitions
    ~macro_candidates ~macro_expansions ~macro_infos ~alloc_size_declarations
    declarations =
  let field name node = C_import_json.field name node in
  let text name node = Option.bind (field name node) C_import_json.string in
  let children node =
    Option.fold ~none:[] ~some:C_import_json.array (field "inner" node)
  in
  let kind node = text "kind" node in
  let name node = text "name" node in
  let object_of_kind kind fields =
    C_import_json.make_obj (("kind", C_import_json.Str kind) :: fields)
  in
  let origins = Hashtbl.create 64 in
  List.iter
    (fun (name, _, _, (file, line)) -> Hashtbl.replace origins name (file, line))
    definitions;
  let macro_origin_fields name =
    match Hashtbl.find_opt origins name with
    | Some (file, line) ->
        [
          ("declarationFile", C_import_json.Str file);
          ("declarationLine", C_import_json.Str (string_of_int line));
        ]
    | None -> []
  in
  let float_macros =
    List.filter_map
      (fun (name, expansion) ->
        Option.bind expansion (fun expansion ->
            Option.map
              (float_macro_node name (macro_origin_fields name))
              (macro_float_literal expansion)))
      macro_expansions
  in
  let rec flatten node = node :: List.concat_map flatten (children node) in
  let all_nodes = List.concat_map flatten declarations in
  let rec enum_decl_id node =
    if kind node = Some "EnumType" then Option.bind (field "decl" node) (text "id")
    else List.find_map enum_decl_id (children node)
  in
  let enum_aliases =
    List.filter (fun node -> kind node = Some "TypedefDecl") all_nodes
  in
  let enum_candidates =
    all_nodes
    |> List.filter_map (fun node ->
        if kind node <> Some "EnumDecl" then None
        else
          Option.bind (text "id" node) (fun id ->
              let c_type =
                match name node with
                | Some name -> Some ("enum " ^ name)
                | None -> (
                    match
                      List.find_map
                        (fun alias ->
                          if enum_decl_id alias = Some id then name alias else None)
                        enum_aliases
                    with
                    | Some _ as alias -> alias
                    | None ->
                        List.find_map
                          (fun probe ->
                            if kind probe <> Some "FasTypeProbe" then None
                            else
                              match (field "tree" probe, text "expression" probe) with
                              | Some tree, Some expression
                                when enum_decl_id tree = Some id ->
                                  Some ("__typeof__(" ^ expression ^ ")")
                              | _ -> None)
                          all_nodes)
              in
              Option.map (fun c_type -> (id, c_type)) c_type))
    |> List.sort_uniq compare
  in
  let layout_candidates =
    List.concat_map
      (fun node ->
        match (kind node, name node) with
        | Some "TypedefDecl", Some name
          when List.exists
                 (fun child -> kind child = Some "AlignedAttr")
                 (children node) ->
            [ ("typedef", name, name, []) ]
        | Some "RecordDecl", Some record_name
          when field "completeDefinition" node = Some (C_import_json.Bool true)
               && List.exists
                    (fun child ->
                      List.mem (kind child) [ Some "PackedAttr"; Some "AlignedAttr" ])
                    (children node) ->
            let tag = Option.value ~default:"struct" (text "tagUsed" node) in
            let fields =
              children node
              |> List.filter (fun child ->
                  kind child = Some "FieldDecl"
                  && field "isBitfield" child <> Some (C_import_json.Bool true))
              |> List.filter_map name
            in
            [ ("record", record_name, tag ^ " " ^ record_name, fields) ]
        | _ -> [])
      declarations
    |> List.sort_uniq compare
  in
  let alloc_size_functions =
    List.concat_map flatten alloc_size_declarations
    |> List.filter (fun node ->
        kind node = Some "FunctionDecl"
        && (List.exists (fun child -> kind child = Some "AllocSizeAttr") (children node)
           || List.mem (name node)
                [
                  Some "malloc";
                  Some "calloc";
                  Some "realloc";
                  Some "aligned_alloc";
                  Some "_mm_malloc";
                ]))
    |> List.filter_map name |> List.sort_uniq compare
  in
  let indexed_macros =
    List.mapi
      (fun index ((name, _, _, _), (type_name, alias, valid)) ->
        (index, name, type_name, alias, valid))
      (List.combine macro_candidates macro_infos)
    |> List.filter_map (fun (index, name, type_name, alias, valid) ->
        match (type_name, valid) with
        | Some type_name, true -> (
            match builtin_info_of_name type_name with
            | Some (Integer (Ast.Bool | Ast.Int _)) -> Some (index, name, alias)
            | _ -> None)
        | _ -> None)
  in
  let has_probes =
    enum_candidates <> [] || layout_candidates <> [] || indexed_macros <> []
    || alloc_size_functions <> [] || float_macros <> []
  in
  if not has_probes then Ok ([], [], [], [])
  else
    let probe = Filename.temp_file "fas-c-import-probes-" ".c" in
    paths := probe :: !paths;
    let digest = Digest.to_hex (Digest.string unit_path) in
    let enum_prefix = "__fas_enumty_" ^ digest ^ "_" in
    let layout_prefix = "__fas_tdlay_" ^ digest ^ "_" in
    let macro_prefix = "__fas_mv_" ^ digest ^ "_" in
    let write_probe () =
      let output = open_out_bin probe in
      Fun.protect
        ~finally:(fun () -> close_out_noerr output)
        (fun () ->
          Printf.fprintf output "#include %S\n" unit_path;
          List.iteri
            (fun index (_, c_type) ->
              Printf.fprintf output
                "static const int %s%d __attribute__((used)) = _Generic((%s)0, %s); \
                 static const unsigned long long %sa_%d __attribute__((used)) = \
                 _Alignof(%s);\n"
                enum_prefix index c_type macro_probe_types enum_prefix index c_type)
            enum_candidates;
          List.iteri
            (fun index (_, _, c_type, fields) ->
              Printf.fprintf output
                "static const unsigned long long %ss_%d __attribute__((used)) = \
                 sizeof(%s); static const unsigned long long %sa_%d \
                 __attribute__((used)) = _Alignof(%s);"
                layout_prefix index c_type layout_prefix index c_type;
              List.iteri
                (fun field_index field ->
                  Printf.fprintf output
                    " static const unsigned long long %so_%d_%d __attribute__((used)) \
                     = __builtin_offsetof(%s, %s);"
                    layout_prefix index field_index c_type field)
                fields;
              output_char output '\n')
            layout_candidates;
          List.iter
            (fun (index, macro_name, _) ->
              Printf.fprintf output
                "static const int %st_%d __attribute__((used)) = \
                 __builtin_constant_p(%s) ? _Generic((%s), %s) : 0; static const \
                 __typeof__((%s)) %sv_%d __attribute__((used)) = \
                 __builtin_constant_p(%s) ? (%s) : 0;\n"
                macro_prefix index macro_name macro_name macro_probe_types macro_name
                macro_prefix index macro_name macro_name)
            indexed_macros;
          List.iteri
            (fun index function_name ->
              Printf.fprintf output
                "static __typeof__(&%s) __fas_allocsize_ref_%d __attribute__((used)) = \
                 &%s;\n"
                function_name index function_name)
            alloc_size_functions)
    in
    write_probe ();
    let common =
      [ "-x"; "c"; "--target=x86_64-unknown-linux-gnu" ]
      @ c_flags
      @ [ "-iquote"; Filename.dirname source ]
    in
    let argv =
      Array.of_list
        ([ cc; "-S"; "-emit-llvm"; "-o"; "-" ]
        @ common
        @ [ "-Xclang=-skip-function-bodies"; probe ])
    in
    match Process.run argv with
    | Error failure -> Error failure.stderr
    | Ok (ir, _) ->
        let integers = llvm_integer_index ir in
        let enum_types =
          enum_candidates
          |> List.mapi (fun index (id, _) -> (index, id))
          |> List.filter_map (fun (index, id) ->
              Option.bind
                (Hashtbl.find_opt integers (enum_prefix ^ string_of_int index))
                (fun (_, code) ->
                  Option.bind (int_of_string_opt code) (fun code ->
                      Option.bind (macro_type code) (fun _ ->
                          Option.map
                            (fun (_, align) ->
                              object_of_kind "FasEnumType"
                                [
                                  ("enumId", C_import_json.Str id);
                                  ( "underlyingCode",
                                    C_import_json.Str (string_of_int code) );
                                  ("align", C_import_json.Str align);
                                ])
                            (Hashtbl.find_opt integers
                               (enum_prefix ^ "a_" ^ string_of_int index))))))
        in
        let typedef_layouts =
          layout_candidates
          |> List.mapi (fun index candidate -> (index, candidate))
          |> List.filter_map (fun (index, (candidate_kind, name, _, fields)) ->
              match
                ( Hashtbl.find_opt integers (layout_prefix ^ "s_" ^ string_of_int index),
                  Hashtbl.find_opt integers (layout_prefix ^ "a_" ^ string_of_int index)
                )
              with
              | Some (_, size), Some (_, align) -> (
                  match (int_of_string_opt size, int_of_string_opt align) with
                  | Some size, Some align ->
                      let extra =
                        if candidate_kind = "typedef" then
                          [ ("name", C_import_json.Str name) ]
                        else
                          let offsets =
                            List.mapi
                              (fun field_index field ->
                                Option.map
                                  (fun (_, value) -> field ^ ":" ^ value)
                                  (Hashtbl.find_opt integers
                                     (layout_prefix ^ "o_" ^ string_of_int index ^ "_"
                                    ^ string_of_int field_index)))
                              fields
                            |> List.filter_map Fun.id |> String.concat ","
                          in
                          [
                            ("recordName", C_import_json.Str name);
                            ("offsets", C_import_json.Str offsets);
                          ]
                      in
                      Some
                        (object_of_kind
                           (if candidate_kind = "typedef" then "FasTypedefLayout"
                            else "FasRecordLayout")
                           ([
                              ("size", C_import_json.Str (string_of_int size));
                              ("align", C_import_json.Str (string_of_int align));
                            ]
                           @ extra))
                  | _ -> None)
              | _ -> None)
        in
        let imported_macros =
          indexed_macros
          |> List.filter_map (fun (index, name, alias) ->
              match
                ( Hashtbl.find_opt integers (macro_prefix ^ "v_" ^ string_of_int index),
                  Hashtbl.find_opt integers (macro_prefix ^ "t_" ^ string_of_int index)
                )
              with
              | Some (value_type, value), Some (_, code)
                when String.starts_with ~prefix:"i" value_type ->
                  Option.bind
                    (int_of_string_opt
                       (String.sub value_type 1 (String.length value_type - 1)))
                    (fun width ->
                      Option.bind (int_of_string_opt code) (fun code ->
                          Option.bind (macro_type code) (fun (c_type, bits, unsigned) ->
                              if width <> bits then None
                              else
                                let extras =
                                  [
                                    ("macroType", C_import_json.Str c_type);
                                    ( "macroTypeCode",
                                      C_import_json.Str (string_of_int code) );
                                    ( "value",
                                      C_import_json.Str
                                        (fas_macro_value bits unsigned value) );
                                  ]
                                  @ Option.to_list
                                      (Option.map
                                         (fun alias ->
                                           ( "macroTypeAliasName",
                                             C_import_json.Str alias ))
                                         alias)
                                in
                                let node =
                                  object_of_kind "FasIntegerMacro"
                                    (("name", C_import_json.Str name)
                                    :: (macro_origin_fields name @ extras))
                                in
                                Some node)))
              | _ -> None)
        in
        let imported_names =
          List.filter_map (fun node -> text "name" node) (imported_macros @ float_macros)
        in
        let macros =
          imported_macros @ float_macros
          @ List.filter_map
              (fun (name, _, _, _) ->
                if List.mem name imported_names then None
                else
                  Some
                    (object_of_kind "FasInvisibleMacro"
                       [ ("name", C_import_json.Str name) ]))
              definitions
        in
        let alloc_size_parameters = alloc_size_parameters ir in
        Ok (enum_types, typedef_layouts, macros, alloc_size_parameters)

let stable_unit_name source headers =
  let contents =
    Marshal.to_string (List.map (fun header -> header.spelling) headers) []
  in
  "fas-c-import-" ^ Digest.to_hex (Digest.string (source ^ "\000" ^ contents)) ^ ".c"

let reproducible_flags name path =
  [
    "-Xclang";
    "-main-file-name";
    "-Xclang";
    name;
    "-ffile-prefix-map=" ^ path ^ "=" ^ name;
  ]

let remove_artifact path =
  (try Sys.remove path with Sys_error _ -> ());
  let directory = Filename.dirname path in
  if String.starts_with ~prefix:"fas-c-unit-" (Filename.basename directory) then
    try Unix.rmdir directory with Unix.Unix_error _ -> ()

let feature_test_macros =
  String.split_on_char ' '
    "_GNU_SOURCE _DEFAULT_SOURCE _BSD_SOURCE _SVID_SOURCE _POSIX_C_SOURCE \
     _XOPEN_SOURCE _XOPEN_SOURCE_EXTENDED _ISOC99_SOURCE _ISOC11_SOURCE _ISOC2X_SOURCE \
     _LARGEFILE64_SOURCE _FILE_OFFSET_BITS _TIME_BITS _REENTRANT _THREAD_SAFE \
     _FORTIFY_SOURCE"

let trace_entries trace =
  String.split_on_char '\n' trace
  |> List.filter_map (fun line ->
      let depth = ref 0 in
      while !depth < String.length line && line.[!depth] = '.' do
        incr depth
      done;
      if !depth > 0 && !depth < String.length line && line.[!depth] = ' ' then
        Some (!depth, String.sub line (!depth + 1) (String.length line - !depth - 1))
      else None)

let defined_macro line =
  let identifier c =
    c = '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
  in
  let words =
    String.map (fun c -> if c = '\t' then ' ' else c) line
    |> String.split_on_char ' '
    |> List.filter (( <> ) "")
  in
  let rec value = function
    | [] -> []
    | word :: _
      when String.starts_with ~prefix:"/*" word || String.starts_with ~prefix:"//" word
      ->
        []
    | word :: rest -> word :: value rest
  in
  match words with
  | "#" :: "define" :: name :: rest | "#define" :: name :: rest ->
      Some
        ( String.of_seq (Seq.take_while identifier (String.to_seq name)),
          String.concat " " (value rest) )
  | _ -> None

let command_line_macros c_flags =
  let rec collect = function
    | ("-I" | "-isystem") :: _ :: rest -> collect rest
    | "-D" :: definition :: rest -> definition :: collect rest
    | flag :: rest when String.starts_with ~prefix:"-D" flag ->
        String.sub flag 2 (String.length flag - 2) :: collect rest
    | _ :: rest -> collect rest
    | [] -> []
  in
  collect c_flags
  |> List.map (fun definition ->
      match String.index_opt definition '=' with
      | Some index ->
          ( String.sub definition 0 index,
            String.sub definition (index + 1) (String.length definition - index - 1) )
      | None -> (definition, "1"))

let feature_macro_in_block definitions (header : header) (fragment : Ast.c_fragment) =
  String.split_on_char '\n' fragment.text
  |> List.mapi (fun index line -> (index, line))
  |> List.find_map (fun (index, line) ->
      match defined_macro line with
      | Some (name, value) when List.mem name feature_test_macros -> (
          let span =
            Span.make ~file:header.span.Span.file ~start_offset:0 ~end_offset:0
              ~line:(header.span.Span.line + 1 + index)
              ~column:(String.index line '#' + 1)
          in
          let written = if value = "" then "1" else value in
          match List.assoc_opt name (List.rev definitions) with
          | Some defined when defined = written -> None
          | Some defined ->
              Some
                (Diag.error span
                   ("C block defines `" ^ name ^ "` as `" ^ written
                  ^ "` but the command line sets `" ^ defined ^ "`"))
          | None ->
              Some
                (Diag.error
                   ~help:
                     ("remove this line and pass `-D" ^ name
                     ^ (if value = "" then "" else "=" ^ value)
                     ^ "` on the command line, or move this block before the other C \
                        imports")
                   span
                   ("C block defines `" ^ name
                  ^ "` after earlier C imports already read system headers")))
      | _ -> None)

let late_feature_macro_error ~source ~c_flags blocks trace =
  let user_prefix = Filename.dirname source ^ "/" in
  let definitions = command_line_macros c_flags in
  let system_seen = ref false and error = ref None in
  List.iter
    (fun (_, path) ->
      if Option.is_none !error then
        match List.assoc_opt path blocks with
        | Some (header, fragment) ->
            if !system_seen then
              error := feature_macro_in_block definitions header fragment
        | None ->
            if
              not
                (String.starts_with ~prefix:"fas-c-fragment-" (Filename.basename path)
                || String.starts_with ~prefix:user_prefix path)
            then system_seen := true)
    (trace_entries trace);
  !error

let import_argv ~cc ~c_flags ~source ~headers unit_path =
  Array.of_list
    ([
       cc;
       "-x";
       "c";
       "-fsyntax-only";
       "-Xclang";
       "-ast-dump=json";
       "-Xclang";
       "-skip-function-bodies";
       "-Xclang";
       "-fdump-record-layouts-complete";
       "-H";
       "--target=x86_64-unknown-linux-gnu";
     ]
    @ c_flags
    @ reproducible_flags (stable_unit_name source headers) unit_path
    @ [ "-fno-builtin"; "-iquote"; Filename.dirname source; unit_path ])

let header_use_files ~source headers fragment_paths trace =
  let indexed = List.mapi (fun index header -> (index, header)) headers in
  let fragments =
    List.combine
      (List.filter_map
         (fun (index, header) ->
           match header.spelling with Ast.C_fragment _ -> Some index | _ -> None)
         indexed)
      fragment_paths
  in
  let matches path (index, header) =
    match header.spelling with
    | Ast.C_quoted spelling ->
        let written =
          if Filename.is_relative spelling then
            Filename.concat (Filename.dirname source) spelling
          else spelling
        in
        path = written || String.ends_with ~suffix:("/" ^ spelling) path
    | Ast.C_system spelling ->
        path = spelling || String.ends_with ~suffix:("/" ^ spelling) path
    | Ast.C_fragment _ -> List.assoc_opt index fragments = Some path
  in
  let claimed = Stdlib.Hashtbl.create 8
  and files = Hashtbl.create 32
  and current = ref None in
  List.iter
    (fun (depth, path) ->
      if depth = 1 then (
        let candidates = List.filter (matches path) indexed in
        (current :=
           match
             List.find_opt
               (fun (index, _) -> not (Stdlib.Hashtbl.mem claimed index))
               candidates
           with
           | Some (index, _) -> Some index
           | None -> Option.map fst (List.nth_opt candidates 0));
        Option.iter (fun index -> Stdlib.Hashtbl.replace claimed index ()) !current);
      match !current with
      | Some index when not (Hashtbl.mem files path) -> Hashtbl.add files path index
      | _ -> ())
    (trace_entries trace);
  Hashtbl.fold
    (fun path index acc -> (path ^ "\t" ^ string_of_int index) :: acc)
    files []
  |> List.sort compare |> String.concat "\n"

let import_fresh ~cc ~keep ?alloc_size_out ?(retain = false) ?(c_flags = [])
    ?(macro_names = []) ?referenced_names ?trace_out source headers =
  let directory = Filename.temp_dir "fas-c-unit-" "" in
  let unit_path = Filename.concat directory (stable_unit_name source headers) in
  let json_path = Filename.temp_file "fas-c-import-" ".json" in
  let fragment_paths = ref [] in
  let macro_paths = ref [] in
  let completed = ref false in
  let cleanup () =
    let remove path = try Sys.remove path with Sys_error _ -> () in
    if (not keep) && ((not retain) || not !completed) then
      List.iter remove_artifact (unit_path :: !fragment_paths);
    List.iter remove !macro_paths;
    remove json_path
  in
  Fun.protect ~finally:cleanup (fun () ->
      let channel = open_out_bin unit_path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () ->
          List.iter
            (fun header ->
              let fragment_path =
                match header.spelling with
                | Ast.C_fragment fragment ->
                    let path = Filename.temp_file "fas-c-fragment-" ".c" in
                    fragment_paths := path :: !fragment_paths;
                    let fragment_channel = open_out_bin path in
                    Fun.protect
                      ~finally:(fun () -> close_out_noerr fragment_channel)
                      (fun () ->
                        Printf.fprintf fragment_channel "#line %d %S\n%s\n"
                          (header.span.Span.line + 1) source fragment.text);
                    Some path
                | Ast.C_quoted _ | Ast.C_system _ -> None
              in
              output_string channel (include_line source fragment_path header))
            headers);
      let argv = import_argv ~cc ~c_flags ~source ~headers unit_path in
      if keep then
        prerr_endline
          ("fas: Clang import command: " ^ String.concat " " (Array.to_list argv));
      let blocks =
        List.filter_map
          (fun header ->
            match header.spelling with
            | Ast.C_fragment fragment -> Some (header, fragment)
            | Ast.C_quoted _ | Ast.C_system _ -> None)
          headers
        |> List.combine (List.rev !fragment_paths)
      in
      let run = Process.run_to_file argv json_path in
      Option.iter
        (fun output ->
          match run with
          | Ok trace -> output := List.map snd (trace_entries trace)
          | Error _ -> ())
        trace_out;
      match run with
      | Error failure ->
          Error
            [
              compilation_error ~source ~headers
                (error_span headers (unit_line unit_path failure.stderr))
                failure.stderr;
            ]
      | Ok trace
        when Option.is_none (late_feature_macro_error ~source ~c_flags blocks trace)
        -> (
          try
            let channel = open_in_bin json_path in
            let layouts, raw_declarations =
              Fun.protect
                ~finally:(fun () -> close_in_noerr channel)
                (fun () -> C_import_json.declarations_with_layout channel)
            in
            let origins = Hashtbl.create 32 and root = ref "" in
            trace_entries trace
            |> List.iter (fun (depth, path) ->
                if depth = 1 then root := path;
                Hashtbl.replace origins path !root);
            let annotate_declarations declarations =
              List.map
                (fun node ->
                  let open C_import_json in
                  let origin =
                    Option.bind (field "loc" node) (fun loc ->
                        Option.bind (field "file" loc) string)
                  in
                  match (node, Option.bind origin (Hashtbl.find_opt origins)) with
                  | Obj _, Some header ->
                      make_obj (("fasHeader", Str header) :: obj_fields node)
                  | _ -> node)
                declarations
            in
            let declarations = annotate_declarations raw_declarations in
            let original_functions = Hashtbl.create 128 in
            List.iter
              (fun node ->
                if
                  C_import_json.field "kind" node
                  = Some (C_import_json.Str "FunctionDecl")
                then
                  Option.iter
                    (fun name -> Hashtbl.replace original_functions name node)
                    (Option.bind (C_import_json.field "name" node) C_import_json.string))
              raw_declarations;
            let macro_names =
              List.filter
                (fun name ->
                  not
                    (List.exists
                       (fun node ->
                         C_import_json.field "kind" node
                         = Some (C_import_json.Str "FunctionDecl")
                         && C_import_json.field "name" node
                            = Some (C_import_json.Str name))
                       declarations))
                macro_names
            in
            let map_error stage = function
              | Ok value -> Ok value
              | Error message -> Error (stage, message)
            in
            let ( let* ) result next =
              match result with Ok value -> next value | Error error -> Error error
            in
            let pipeline =
              let* definitions =
                map_error "internal error: C macro import failed: "
                  (imported_macro_definitions ~cc ~c_flags ~source ~unit_path
                     ~macro_names)
              in
              let macro_candidates =
                List.filter
                  (fun (_, function_like, has_value, _) ->
                    (not function_like) && has_value)
                  definitions
              in
              let* macro_expansions =
                map_error "internal error: C macro expansion failed: "
                  (imported_macro_expansions ~cc ~c_flags ~source ~unit_path
                     ~paths:macro_paths macro_candidates)
              in
              let* structured_ast, macro_infos =
                map_error "internal error: C structured type import failed: "
                  (imported_structured_type_nodes ~cc ~c_flags ~source ~unit_path
                     ~paths:macro_paths ?referenced_names ~probe_all_declarations:keep
                     ~macro_candidates declarations)
              in
              let declarations =
                List.map
                  (fun node ->
                    if
                      C_import_json.field "kind" node
                      = Some (C_import_json.Str "FunctionDecl")
                    then
                      let name =
                        Option.bind
                          (C_import_json.field "name" node)
                          C_import_json.string
                      in
                      Option.bind name (Hashtbl.find_opt original_functions)
                      |> Option.value ~default:node
                    else node)
                  structured_ast
                |> annotate_declarations
              in
              let* enum_types, typedef_layouts, macros, alloc_size_parameters =
                map_error "internal error: C import probe failed: "
                  (imported_probe_nodes ~cc ~c_flags ~source ~unit_path
                     ~paths:macro_paths ~definitions ~macro_candidates ~macro_expansions
                     ~macro_infos ~alloc_size_declarations:structured_ast declarations)
              in
              Ok
                ( declarations @ enum_types @ typedef_layouts
                  @ C_import_json.make_obj
                      [
                        ("kind", C_import_json.Str "FasLayoutDump");
                        ("value", C_import_json.Str layouts);
                      ]
                    :: C_import_json.make_obj
                         [
                           ("kind", C_import_json.Str "FasUseFiles");
                           ( "value",
                             C_import_json.Str
                               (header_use_files ~source headers
                                  (List.rev !fragment_paths) trace) );
                         ]
                    :: macros,
                  alloc_size_parameters )
            in
            match pipeline with
            | Error (stage, message) ->
                Error
                  [
                    Diag.error (List.hd headers).span
                      (stage ^ normalize_import_failure ~unit_path !macro_paths message);
                  ]
            | Ok (all_declarations, alloc_size_parameters) ->
                Option.iter
                  (fun output -> output := alloc_size_parameters)
                  alloc_size_out;
                completed := true;
                Ok
                  ( all_declarations,
                    (if keep then Some (unit_path :: List.rev !fragment_paths) else None),
                    if retain then unit_path :: List.rev !fragment_paths else [] )
          with Failure message ->
            Error
              [
                Diag.error (List.hd headers).span
                  ("internal error: C import JSON reader: " ^ message);
              ])
      | Ok trace ->
          Error
            (Option.to_list (late_feature_macro_error ~source ~c_flags blocks trace)))

type import_entry = {
  cache_name : string;
  stamps : (string * (int * float) option) list;
  unit_text : string;
  fragments : string list;
  payload : string;
}

type cache = import_entry Hashtbl.t

let create_cache () : cache = Hashtbl.create 8

let file_stamp path =
  try
    let stat = Unix.stat path in
    Some (stat.Unix.st_size, stat.Unix.st_mtime)
  with Unix.Unix_error _ -> None

let entry_valid entry =
  List.for_all Sys.file_exists entry.fragments
  && List.for_all (fun (path, stamp) -> file_stamp path = stamp) entry.stamps

let clear_cache (cache : cache) =
  Hashtbl.iter (fun _ entry -> List.iter remove_artifact entry.fragments) cache;
  Hashtbl.reset cache

let read_text path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let materialize name text =
  let directory = Filename.temp_dir "fas-c-unit-" "" in
  let path = Filename.concat directory name in
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text);
  path

let cache_key ~cc ~c_flags ~macro_names ~referenced_names ~alloc ~retain source headers
    =
  let argv = import_argv ~cc ~c_flags ~source ~headers "<unit>" in
  let unit_text =
    List.mapi
      (fun index header ->
        let fragment_path =
          match header.spelling with
          | Ast.C_fragment _ -> Some (string_of_int index)
          | Ast.C_quoted _ | Ast.C_system _ -> None
        in
        include_line source fragment_path header)
      headers
    |> String.concat ""
  in
  let fragments =
    List.filter_map
      (fun header ->
        match header.spelling with
        | Ast.C_fragment fragment -> Some (header.span.Span.line, fragment.text)
        | Ast.C_quoted _ | Ast.C_system _ -> None)
      headers
  in
  Marshal.to_string
    (argv, unit_text, fragments, source, macro_names, referenced_names, alloc, retain)
    []

let import ~cc ~keep ?alloc_size_out ?(retain = false) ?(c_flags = [])
    ?(macro_names = []) ?referenced_names ?cache source headers =
  let fresh trace_out =
    import_fresh ~cc ~keep ?alloc_size_out ~retain ~c_flags ~macro_names
      ?referenced_names ?trace_out source headers
  in
  match cache with
  | Some (cache : cache) when (not keep) && retain -> (
      let key =
        cache_key ~cc ~c_flags ~macro_names ~referenced_names
          ~alloc:(Option.is_some alloc_size_out)
          ~retain source headers
      in
      let name = stable_unit_name source headers in
      match Hashtbl.find_opt cache key with
      | Some entry when entry_valid entry ->
          let declarations, alloc_value =
            (Marshal.from_string entry.payload 0 : _ * string option)
          in
          (match (alloc_size_out, alloc_value) with
          | Some output, Some text -> output := Marshal.from_string text 0
          | _ -> ());
          Ok (declarations, None, [ materialize name entry.unit_text ])
      | _ -> (
          let trace = ref [] in
          match fresh (Some trace) with
          | Ok (declarations, kept, (unit :: fragments as artifacts)) ->
              let stamps = List.map (fun path -> (path, file_stamp path)) !trace in
              if List.for_all (fun (_, stamp) -> Option.is_some stamp) stamps then (
                let alloc_value =
                  Option.map (fun output -> Marshal.to_string !output []) alloc_size_out
                in
                Hashtbl.filter_map_inplace
                  (fun _ entry ->
                    if String.equal entry.cache_name name then (
                      List.iter remove_artifact entry.fragments;
                      None)
                    else Some entry)
                  cache;
                Hashtbl.replace cache key
                  {
                    cache_name = name;
                    stamps;
                    unit_text = read_text unit;
                    fragments;
                    payload = Marshal.to_string (declarations, alloc_value) [];
                  };
                Ok (declarations, kept, [ unit ]))
              else Ok (declarations, kept, artifacts)
          | result -> result))
  | _ -> fresh None

let get name = C_import_json.field name
let text = function C_import_json.Str value -> Some value | _ -> None
let string name value = Option.bind (get name value) text
let children value = Option.fold ~none:[] ~some:C_import_json.array (get "inner" value)

let rec trim value =
  if value = "" then value
  else
    match value.[0] with
    | ' ' | '\t' | '\n' | '\r' -> trim (String.sub value 1 (String.length value - 1))
    | _ -> (
        let last = String.length value - 1 in
        match value.[last] with
        | ' ' | '\t' | '\n' | '\r' -> trim (String.sub value 0 last)
        | _ -> value)

let builtin_integer_type = function Integer ty -> Some ty | _ -> None
let c_type_name node = Option.bind (get "type" node) (string "qualType")

let function_result_spelling raw =
  match find_text raw "(*(" 0 with
  | None -> None
  | Some pointer ->
      let open_index = pointer + 2 in
      let rec close depth index =
        if index = String.length raw then None
        else
          let depth =
            depth + if raw.[index] = '(' then 1 else if raw.[index] = ')' then -1 else 0
          in
          if depth = 0 then Some index else close depth (index + 1)
      in
      Option.map
        (fun close ->
          String.sub raw 0 open_index
          ^ String.sub raw (close + 1) (String.length raw - close - 1))
        (close 0 open_index)

let c_function_result raw fallback =
  Option.fold ~none:fallback ~some:trim (function_result_spelling raw)

let c_named_type raw name =
  match find_text raw "(*" 0 with
  | None -> raw ^ " " ^ name
  | Some pointer -> (
      match String.index_from_opt raw (pointer + 2) ')' with
      | None -> raw ^ " " ^ name
      | Some close ->
          String.sub raw 0 close
          ^ (if close = pointer + 2 || raw.[close - 1] = '*' || raw.[close - 1] = ' '
             then ""
             else " ")
          ^ name
          ^ String.sub raw close (String.length raw - close))

let type_node node =
  children node
  |> List.find_opt (fun child ->
      Option.fold ~none:false
        ~some:(String.ends_with ~suffix:"Type")
        (string "kind" child))

let type_node_id node = Option.bind (get "decl" node) (string "id")

let record_name node =
  match string "name" node with Some name when name <> "" -> Some name | _ -> None

let type_qualifier_names node =
  Option.value ~default:"" (string "qualifiers" node)
  |> String.map (function
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c
    | _ -> ' ')
  |> String.split_on_char ' '
  |> List.filter (( <> ) "")

let rec type_qualifiers node =
  type_qualifier_names node @ List.concat_map type_qualifiers (children node)

let rec type_has_address_space node =
  let own =
    Option.fold ~none:false
      ~some:(String.starts_with ~prefix:"__attribute__((address_space(")
      (string "qualifiers" node)
  in
  own || List.exists type_has_address_space (children node)

let rec type_has_vector node =
  List.mem (string "kind" node)
    [ Some "VectorType"; Some "ExtVectorType"; Some "DependentSizedExtVectorType" ]
  || List.exists type_has_vector (children node)

let rec type_has_function ~alias_type_node seen node =
  match string "kind" node with
  | Some ("FunctionProtoType" | "FunctionNoProtoType") -> true
  | Some "TypedefType" -> (
      match type_node_id node with
      | Some id when not (List.mem id seen) ->
          Option.fold ~none:false
            ~some:(type_has_function ~alias_type_node (id :: seen))
            (alias_type_node id)
      | _ -> false)
  | _ -> List.exists (type_has_function ~alias_type_node seen) (children node)

let rec enum_decl_id node =
  match string "kind" node with
  | Some "EnumType" -> type_node_id node
  | _ -> List.find_map enum_decl_id (children node)

let add_one_decimal value =
  let negative = String.starts_with ~prefix:"-" value in
  let digits =
    if negative then String.sub value 1 (String.length value - 1) else value
  in
  let adjust amount =
    let chars = Bytes.of_string digits in
    let carry = ref amount in
    let index = ref (Bytes.length chars - 1) in
    while !index >= 0 && !carry <> 0 do
      let digit = Char.code (Bytes.get chars !index) - Char.code '0' in
      let next = digit + !carry in
      if next >= 10 then (
        Bytes.set chars !index (Char.chr (Char.code '0' + next - 10));
        carry := 1)
      else if next < 0 then (
        Bytes.set chars !index (Char.chr (Char.code '0' + next + 10));
        carry := -1)
      else (
        Bytes.set chars !index (Char.chr (Char.code '0' + next));
        carry := 0);
      decr index
    done;
    if !carry > 0 then "1" ^ Bytes.to_string chars
    else if !carry < 0 then Bytes.to_string chars
    else Bytes.to_string chars
  in
  if negative then
    let next = adjust (-1) |> String.trim in
    if String.for_all (( = ) '0') next then "0" else "-" ^ next
  else adjust 1

let anonymous_record_reason = "anonymous records are not supported"

let anonymous_field_reason ~field_name record =
  let kind = if string "tagUsed" record = Some "union" then "union" else "struct" in
  match record_name record with
  | Some tag ->
      Printf.sprintf "anonymous record type of field `%s` in `%s %s` is not supported"
        field_name kind tag
  | None ->
      Printf.sprintf "anonymous record type of field `%s` is not supported" field_name

let type_result ?(allow_arrays = false) ~alias_name ~alias_type_node ~resolve_alias
    ~record_ids ~visible_record_ids ~enum_types ~allow_record ctype =
  let child_type node =
    children node
    |> List.find_opt (fun child ->
        Option.fold ~none:false
          ~some:(String.ends_with ~suffix:"Type")
          (string "kind" child))
  in
  let rec record_value = function
    | Ast.Named_type _ -> true
    | Ast.Array (_, ty) -> record_value ty
    | _ -> false
  in
  let contains_array = function Ast.Array _ -> true | _ -> false in
  let rec resolve seen node =
    if type_has_address_space node then Error "C address spaces are not supported"
    else
      match string "kind" node with
      | Some "QualType" ->
          Option.fold ~none:(Error "unsupported C type") ~some:(resolve seen)
            (child_type node)
      | Some "TypedefType" -> (
          match type_node_id node with
          | Some id when not (List.mem id seen) -> (
              match alias_name id with
              | Some name -> (
                  match resolve_alias name with
                  | Error "over-aligned typedef has alignment greater than its size" ->
                      Error
                        (Printf.sprintf
                           "over-aligned typedef `%s` has alignment greater than its \
                            size"
                           name)
                  | result -> result)
              | None -> Error "typedef has no canonical type")
          | Some _ -> Error "recursive C typedef is not supported"
          | None -> Error "typedef has no canonical type")
      | Some "PredefinedSugarType" -> (
          match Option.bind (get "type" node) (string "qualType") with
          | Some "__size_t" -> Ok (Ast.Int Ast.Usize)
          | Some "__ptrdiff_t" -> Ok (Ast.Int Ast.Isize)
          | _ ->
              Option.fold ~none:(Error "unsupported C type") ~some:(resolve seen)
                (child_type node))
      | Some
          ( "ElaboratedType" | "ParenType" | "MacroQualifiedType" | "AdjustedType"
          | "DecayedType" | "AttributedType" | "TypeOfType" | "TypeOfExprType" ) ->
          let children =
            children node
            |> List.filter (fun child ->
                Option.fold ~none:false
                  ~some:(String.ends_with ~suffix:"Type")
                  (string "kind" child))
          in
          Option.fold ~none:(Error "unsupported C type") ~some:(resolve seen)
            (List.find_opt (fun _ -> true) (List.rev children))
      | Some "BuiltinType" -> (
          match
            Option.bind (get "type" node) (fun ty ->
                Option.bind (string "qualType" ty) builtin_info_of_name)
          with
          | None -> Error "unsupported C builtin type"
          | Some (Integer ty) -> Ok ty
          | Some Unsupported_integer -> Error "`__int128` has no Fas type"
          | Some (Floating 32) -> Ok (Ast.Float Ast.F32)
          | Some (Floating 64) -> Ok (Ast.Float Ast.F64)
          | Some (Floating _) -> Error "floating-point types are not supported")
      | Some "ComplexType" -> Error "floating-point types are not supported"
      | Some ("VectorType" | "ExtVectorType" | "DependentSizedExtVectorType") ->
          Error "vector types are not supported by value"
      | Some "BitIntType" -> Error "`_BitInt` has no Fas type"
      | Some "EnumType" -> (
          match type_node_id node with
          | Some id ->
              Option.value ~default:(Error "enum representation is not supported")
                (Hashtbl.find_opt enum_types id)
          | None -> Error "enum representation is not supported")
      | Some "RecordType" -> (
          match type_node_id node with
          | Some id ->
              Option.fold ~none:(Error anonymous_record_reason)
                ~some:(fun name -> Ok (Ast.Named_type (name, Span.synthetic)))
                (Hashtbl.find_opt record_ids id)
          | None -> Error anonymous_record_reason)
      | Some ("PointerType" | "BlockPointerType" | "ObjCObjectPointerType") -> (
          let pointee = child_type node in
          match pointee with
          | None -> Error "pointer target type is not supported"
          | Some pointee when type_has_function ~alias_type_node [] pointee ->
              Ok Ast.Addr
          | Some pointee when type_has_address_space pointee ->
              Error "C address spaces are not supported"
          | Some pointee ->
              let rec target seen node =
                match string "kind" node with
                | Some "QualType" ->
                    Option.fold ~none:(Error "pointer target type is not supported")
                      ~some:(target seen) (child_type node)
                | Some "TypedefType" -> (
                    match type_node_id node with
                    | Some id when not (List.mem id seen) ->
                        Option.fold ~none:(Error "pointer target type is not supported")
                          ~some:(target (id :: seen))
                          (alias_type_node id)
                    | _ -> Error "recursive C typedef is not supported")
                | Some "RecordType" -> (
                    match type_node_id node with
                    | Some id -> (
                        match
                          ( Hashtbl.find_opt record_ids id,
                            Hashtbl.mem visible_record_ids id )
                        with
                        | Some name, true ->
                            Ok (Ast.Handle (Ast.Named_type (name, Span.synthetic)))
                        | _ -> Ok Ast.Addr)
                    | None -> Ok Ast.Addr)
                | Some
                    ( "BuiltinType" | "BitIntType" | "EnumType" | "PointerType"
                    | "BlockPointerType" | "ConstantArrayType" | "IncompleteArrayType"
                    | "VariableArrayType" | "DependentSizedArrayType" | "VectorType"
                    | "ExtVectorType" | "DependentSizedExtVectorType" ) ->
                    Ok Ast.Addr
                | Some ("FunctionProtoType" | "FunctionNoProtoType") -> Ok Ast.Addr
                | Some
                    ( "ElaboratedType" | "ParenType" | "MacroQualifiedType"
                    | "AttributedType" | "TypeOfType" | "TypeOfExprType"
                    | "PredefinedSugarType" ) ->
                    Option.fold ~none:(Error "pointer target type is not supported")
                      ~some:(target seen) (child_type node)
                | _ -> Error "pointer target type is not supported"
              in
              target [] pointee)
      | Some "ConstantArrayType" -> (
          let size =
            match get "size" node with
            | Some (C_import_json.Num value) -> int_of_string_opt value
            | Some (C_import_json.Str value) -> int_of_string_opt value
            | _ -> None
          in
          match (size, child_type node) with
          | Some size, Some element when size >= 0 ->
              Result.map
                (fun ty -> Ast.Array (Ast.int_aggregate_length size Span.synthetic, ty))
                (resolve seen element)
          | _ -> Error "array types are not supported by value")
      | Some "IncompleteArrayType" -> Error "arrays of unknown size are not supported"
      | Some ("VariableArrayType" | "DependentSizedArrayType") ->
          Error "array types are not supported by value"
      | Some ("FunctionProtoType" | "FunctionNoProtoType") -> Ok Ast.Addr
      | _ ->
          Error
            ("unsupported C type "
            ^ Option.value ~default:""
                (Option.bind (get "type" node) (string "qualType")))
  in
  match resolve [] ctype with
  | Ok ty when (not allow_arrays) && contains_array ty ->
      Error "array types are not supported by value"
  | Ok ty when (not allow_record) && record_value ty ->
      Error "struct and union values are not supported"
  | result -> result

type mapped = {
  items : Ast.item list;
  aliases : (string * Ast.ty) list;
  unsupported : (string * string) list;
  identities : (string * string) list;
  manifest : string list;
  use_files : (string * header) list;
  nonnull_parameters : (string * int list) list;
  c_string_parameters : (string * (int * string option) list) list;
  alloc_size_parameters : (string * int list list) list;
  parameter_names : (string * string option list) list;
  static_functions : static_function list;
  record_calls : (string * (int list * bool)) list;
  records : (string * string * string option) list;
  record_types : (string * string * string option) list;
  incomplete_arrays : (string * Ast.ty) list;
  container_names : string list;
  enum_owners : (string * string option) list;
}

let adapter_symbol source name =
  "__fas_c_adapter_" ^ Digest.to_hex (Digest.string source) ^ "_" ^ name

let adapter_c_name symbol =
  let prefix = "__fas_c_adapter_" in
  let start = String.length prefix + 32 in
  if
    String.length symbol > start + 1
    && String.starts_with ~prefix symbol
    && String.for_all
         (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
         (String.sub symbol (String.length prefix) 32)
    && symbol.[start] = '_'
  then Some (String.sub symbol (start + 1) (String.length symbol - start - 1))
  else None

let make_adapter ~occupied source (static : static_function) =
  if static.variadic then
    Error ("static variadic function `" ^ static.name ^ "` needs an adapter")
  else
    let base = adapter_symbol source static.name in
    let rec choose suffix =
      let symbol = if suffix = 0 then base else base ^ "_" ^ string_of_int suffix in
      if List.mem symbol occupied then choose (suffix + 1) else symbol
    in
    let symbol = choose 0 in
    let by_value i = List.mem i static.record_parameters in
    let params =
      List.mapi
        (fun i ty ->
          let name = "fas_arg" ^ string_of_int i in
          if by_value i then ty ^ " *" ^ name else c_named_type ty name)
        static.parameter_types
    in
    let call_args =
      List.mapi
        (fun i _ -> (if by_value i then "*" else "") ^ "fas_arg" ^ string_of_int i)
        static.parameter_types
    in
    let unqualified ty =
      if String.starts_with ~prefix:"const " ty then
        String.sub ty 6 (String.length ty - 6)
      else ty
    in
    let params =
      if static.record_result then
        (unqualified static.return_type ^ " *fas_result") :: params
      else params
    in
    let params = if params = [] then "void" else String.concat ", " params in
    let call = static.name ^ "(" ^ String.concat ", " call_args ^ ")" in
    let body =
      if static.record_result then "*fas_result = " ^ call ^ ";"
      else if static.void_result then call ^ ";"
      else "return " ^ call ^ ";"
    in
    let return_type, return_typedef =
      if static.record_result then ("void", "")
      else if static.return_function_pointer then
        let alias = symbol ^ "_result" in
        (alias, "typedef " ^ c_named_type static.return_type alias ^ ";\n")
      else (static.return_type, "")
    in
    Ok
      {
        c_name = static.name;
        symbol;
        code =
          Printf.sprintf
            "#line %d %S\n%s__attribute__((visibility(\"hidden\"))) %s %s(%s) { %s }\n"
            static.span.Span.line source return_typedef return_type symbol params body;
        file = source;
        line = static.span.Span.line;
        signature = static.signature;
      }

let append_adapters path adapters =
  let channel = open_out_gen [ Open_append; Open_binary ] 0o600 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      List.iter (fun (adapter : adapter) -> output_string channel adapter.code) adapters)

let adapter_manifest (adapter : adapter) =
  Printf.sprintf "%s\tadapter for %s\t%s\t\t%s:%d" adapter.symbol adapter.c_name
    adapter.signature adapter.file adapter.line

let item_name = function
  | Ast.Opaque { name; _ }
  | Ast.Struct { name; _ }
  | Ast.Const { name; _ }
  | Ast.Global { name; _ }
  | Ast.Func { name; _ } ->
      name
  | _ -> ""

type clang_layout = {
  layout_name : string;
  size : int;
  align : int;
  offsets : (string * int) list;
  members : (string * int) list;
  direct_members : (string * int) list;
  direct_offsets : (string * int) list;
}

let clang_layouts text =
  let lines = String.split_on_char '\n' text in
  let after marker line =
    match find_text line marker 0 with
    | None -> None
    | Some at ->
        let start = at + String.length marker in
        let finish = ref start in
        while
          !finish < String.length line && line.[!finish] >= '0' && line.[!finish] <= '9'
        do
          incr finish
        done;
        int_of_string_opt (String.sub line start (!finish - start))
  in
  let index value items =
    let rec find i = function
      | [] -> None
      | item :: rest -> if item = value then Some i else find (i + 1) rest
    in
    find 0 items
  in
  let rec groups current acc = function
    | [] -> List.rev (finish current acc)
    | line :: rest when find_text line "*** Dumping AST Record Layout" 0 = Some 0 ->
        groups [ line ] (finish current acc) rest
    | line :: rest -> groups (line :: current) acc rest
  and finish group acc =
    match List.rev group with
    | [] -> acc
    | lines -> (
        let header =
          List.find_map
            (fun line ->
              match String.index_opt line '|' with
              | None -> None
              | Some bar ->
                  let right =
                    String.sub line (bar + 1) (String.length line - bar - 1)
                  in
                  let body = trim right in
                  if
                    String.length right - String.length body = 1
                    && (String.starts_with ~prefix:"struct " body
                       || String.starts_with ~prefix:"union " body)
                  then Some (line, body)
                  else None)
            lines
        in
        let summary =
          List.find_opt (fun line -> Option.is_some (after "sizeof=" line)) lines
        in
        match (header, summary) with
        | Some (header, name), Some summary -> (
            match (after "sizeof=" summary, after "align=" summary) with
            | Some size, Some align ->
                let header_at = Option.get (index header lines) in
                let summary_at = Option.get (index summary lines) in
                let members =
                  List.mapi (fun i line -> (i, line)) lines
                  |> List.filter_map (fun (i, line) ->
                      if i <= header_at || i >= summary_at then None
                      else
                        match String.index_opt line '|' with
                        | None -> None
                        | Some bar ->
                            let left = trim (String.sub line 0 bar) in
                            let right =
                              String.sub line (bar + 1) (String.length line - bar - 1)
                            in
                            let body = trim right in
                            if String.length right - String.length body < 3 then None
                            else
                              Option.map
                                (fun offset -> (body, offset))
                                (int_of_string_opt left))
                in
                let direct_members =
                  List.mapi (fun i line -> (i, line)) lines
                  |> List.filter_map (fun (i, line) ->
                      if i <= header_at || i >= summary_at then None
                      else
                        match String.index_opt line '|' with
                        | None -> None
                        | Some bar ->
                            let left = trim (String.sub line 0 bar) in
                            let right =
                              String.sub line (bar + 1) (String.length line - bar - 1)
                            in
                            let rec indentation count =
                              if count < String.length right && right.[count] = ' ' then
                                indentation (count + 1)
                              else count
                            in
                            let body = trim right in
                            if indentation 0 <> 3 then None
                            else
                              Option.map
                                (fun offset -> (body, offset))
                                (int_of_string_opt left))
                in
                let offsets =
                  List.mapi (fun i line -> (i, line)) lines
                  |> List.filter_map (fun (i, line) ->
                      if i <= header_at || i >= summary_at then None
                      else
                        match String.index_opt line '|' with
                        | None -> None
                        | Some bar ->
                            let left = trim (String.sub line 0 bar) in
                            let right =
                              String.sub line (bar + 1) (String.length line - bar - 1)
                            in
                            let body = trim right in
                            if String.length right - String.length body < 3 then None
                            else
                              let field =
                                match
                                  List.rev
                                    (String.split_on_char ' ' body
                                    |> List.filter (( <> ) ""))
                                with
                                | name :: _ -> Some name
                                | [] -> None
                              in
                              Option.bind (int_of_string_opt left) (fun offset ->
                                  Option.map (fun field -> (field, offset)) field))
                in
                let direct_offsets =
                  List.filter_map
                    (fun (body, offset) ->
                      match
                        List.rev
                          (String.split_on_char ' ' body |> List.filter (( <> ) ""))
                      with
                      | field :: _ -> Some (field, offset)
                      | [] -> None)
                    direct_members
                in
                {
                  layout_name = name;
                  size;
                  align;
                  offsets;
                  members;
                  direct_members;
                  direct_offsets;
                }
                :: acc
            | _ -> acc)
        | _ -> acc)
  in
  groups [] [] lines

let map_declarations ?(container = false) ?(alloc_size_parameters = [])
    ?referenced_names ?(headers = []) ~span declarations =
  let use_files =
    List.find_map
      (function
        | node
          when C_import_json.field "kind" node = Some (C_import_json.Str "FasUseFiles")
          ->
            Option.bind (C_import_json.field "value" node) C_import_json.string
        | _ -> None)
      declarations
    |> Option.fold ~none:[] ~some:(String.split_on_char '\n')
    |> List.filter_map (fun line ->
        match String.rindex_opt line '\t' with
        | None -> None
        | Some stop ->
            Option.bind
              (int_of_string_opt
                 (String.sub line (stop + 1) (String.length line - stop - 1)))
              (fun index ->
                Option.map
                  (fun (header : header) -> (String.sub line 0 stop, header))
                  (List.nth_opt headers index)))
  in
  let layout_dump =
    List.find_map
      (function
        | node
          when C_import_json.field "kind" node
               = Some (C_import_json.Str "FasLayoutDump") ->
            Option.bind (C_import_json.field "value" node) C_import_json.string
        | _ -> None)
      declarations
  in
  let layouts = Option.fold ~none:[] ~some:clang_layouts layout_dump in
  let layouts_by_name = Hashtbl.create (List.length layouts)
  and layouts_by_suffix = Hashtbl.create 32 in
  List.iteri
    (fun index layout ->
      if not (Hashtbl.mem layouts_by_name layout.layout_name) then
        Hashtbl.add layouts_by_name layout.layout_name (index, layout);
      let rec last_suffix start found =
        match find_text layout.layout_name "::(unnamed at " start with
        | Some at -> last_suffix (at + 1) (Some at)
        | None -> found
      in
      Option.iter
        (fun at ->
          let suffix =
            String.sub layout.layout_name at (String.length layout.layout_name - at)
          in
          if not (Hashtbl.mem layouts_by_suffix suffix) then
            Hashtbl.add layouts_by_suffix suffix (index, layout))
        (last_suffix 0 None))
    layouts;
  let find_layout names suffix =
    let exact = List.filter_map (Hashtbl.find_opt layouts_by_name) names in
    let candidates =
      exact @ Option.to_list (Option.bind suffix (Hashtbl.find_opt layouts_by_suffix))
    in
    match List.sort (fun (left, _) (right, _) -> compare left right) candidates with
    | (_, layout) :: _ -> Some layout
    | [] -> None
  in
  let typedef_layouts = Hashtbl.create 16 in
  List.iter
    (fun node ->
      if string "kind" node = Some "FasTypedefLayout" then
        match (string "name" node, string "size" node, string "align" node) with
        | Some name, Some size, Some align -> (
            match (int_of_string_opt size, int_of_string_opt align) with
            | Some size, Some align -> Hashtbl.replace typedef_layouts name (size, align)
            | _ -> ())
        | _ -> ())
    declarations;
  let structured_nodes =
    List.filter
      (fun node ->
        List.mem (string "kind" node)
          [
            Some "FasTypeProbe";
            Some "FasTypeCollision";
            Some "FasEnumType";
            Some "FasRecordLayout";
          ])
      declarations
  in
  let structured_types = Hashtbl.create 128 and colliding_records = Hashtbl.create 16 in
  List.iter
    (fun node ->
      match string "kind" node with
      | Some "FasTypeProbe" -> (
          match (string "target" node, get "tree" node) with
          | Some target, Some tree -> Hashtbl.replace structured_types target tree
          | _ -> ())
      | Some "FasTypeCollision" ->
          Option.iter
            (fun name -> Hashtbl.replace colliding_records name ())
            (string "name" node)
      | _ -> ())
    structured_nodes;
  let nodes =
    C_import_json.array (C_import_json.Arr declarations)
    |> List.filter (fun node ->
        not
          (List.mem (string "kind" node)
             [
               Some "FasLayoutDump";
               Some "FasUseFiles";
               Some "FasTypedefLayout";
               Some "FasTypeProbe";
               Some "FasTypeCollision";
               Some "FasEnumType";
               Some "FasRecordLayout";
             ]))
  in
  let rec flatten node = node :: List.concat_map flatten (children node) in
  let all_nodes = List.concat_map flatten nodes in
  let rec record_scope_records node =
    if string "kind" node = Some "RecordDecl" then
      node :: List.concat_map record_scope_records (children node)
    else []
  in
  let scope_records = List.concat_map record_scope_records nodes in
  let enum_probe_types = Hashtbl.create 32 in
  List.iter
    (fun node ->
      if string "kind" node = Some "FasEnumType" then
        match
          (string "enumId" node, string "underlyingCode" node, string "align" node)
        with
        | Some id, Some code, Some align ->
            Option.iter
              (fun result -> Hashtbl.replace enum_probe_types id result)
              (Option.bind (int_of_string_opt code) (fun code ->
                   Option.bind (int_of_string_opt align) (fun align ->
                       Option.bind (macro_type code) (fun (_, bits, unsigned) ->
                           let ty =
                             if code = 11 then Some Ast.Bool
                             else
                               match (bits, unsigned) with
                               | 8, false -> Some (Ast.Int Ast.I8)
                               | 8, true -> Some (Ast.Int Ast.U8)
                               | 16, false -> Some (Ast.Int Ast.I16)
                               | 16, true -> Some (Ast.Int Ast.U16)
                               | 32, false -> Some (Ast.Int Ast.I32)
                               | 32, true -> Some (Ast.Int Ast.U32)
                               | 64, false -> Some (Ast.Int Ast.I64)
                               | 64, true -> Some (Ast.Int Ast.U64)
                               | _ -> None
                           in
                           Option.map
                             (fun ty ->
                               if align > bits / 8 then
                                 Error "over-aligned C enums are not supported"
                               else Ok ty)
                             ty))))
        | _ -> ())
    structured_nodes;
  let record_layout_probes = Hashtbl.create 32 in
  List.iter
    (fun node ->
      if string "kind" node = Some "FasRecordLayout" then
        match
          ( string "recordName" node,
            string "size" node,
            string "align" node,
            string "offsets" node )
        with
        | Some name, Some size, Some align, Some offsets -> (
            let field_offsets =
              String.split_on_char ',' offsets
              |> List.filter_map (fun offset ->
                  match String.split_on_char ':' offset with
                  | [ name; value ] ->
                      Option.map (fun value -> (name, value)) (int_of_string_opt value)
                  | _ -> None)
            in
            match (int_of_string_opt size, int_of_string_opt align) with
            | Some size, Some align ->
                Hashtbl.replace record_layout_probes name (size, align, field_offsets)
            | _ -> ())
        | _ -> ())
    structured_nodes;
  let integer_width = function
    | Ast.Int (Ast.I8 | Ast.U8) -> Some 8
    | Ast.Int (Ast.I16 | Ast.U16) -> Some 16
    | Ast.Int (Ast.I32 | Ast.U32) -> Some 32
    | Ast.Int (Ast.I64 | Ast.U64 | Ast.Isize | Ast.Usize) -> Some 64
    | _ -> None
  in
  let enum_constant_type node =
    let rec builtin_type seen tree =
      match string "kind" tree with
      | Some "BuiltinType" ->
          Option.bind
            (Option.bind (get "type" tree) (fun ty ->
                 Option.bind (string "qualType" ty) builtin_info_of_name))
            builtin_integer_type
      | Some "TypedefType" -> (
          match type_node_id tree with
          | Some id when not (List.mem id seen) ->
              List.find_map
                (fun alias ->
                  if
                    string "kind" alias = Some "TypedefDecl"
                    && string "id" alias = Some id
                  then Option.bind (type_node alias) (builtin_type (id :: seen))
                  else None)
                nodes
          | _ -> None)
      | Some
          ( "QualType" | "ElaboratedType" | "ParenType" | "MacroQualifiedType"
          | "AttributedType" | "TypeOfType" | "TypeOfExprType" | "PredefinedSugarType"
            ) ->
          Option.bind (type_node tree) (builtin_type seen)
      | _ -> None
    in
    let constants =
      children node
      |> List.filter (fun child -> string "kind" child = Some "EnumConstantDecl")
    in
    let typed_constants =
      List.filter_map
        (fun child ->
          Option.bind (record_name child) (fun name ->
              Option.bind
                (Hashtbl.find_opt structured_types ("decl:" ^ name))
                (builtin_type [])))
        constants
    in
    match typed_constants with
    | first :: rest
      when List.length typed_constants = List.length constants
           && List.for_all (( = ) first) rest ->
        Some first
    | _ -> None
  in
  let enum_representation node underlying previous =
    match (enum_constant_type node, underlying) with
    | Some constant_type, Ok underlying_type -> (
        match (integer_width constant_type, integer_width underlying_type) with
        | Some constant_width, Some underlying_width
          when constant_width = underlying_width ->
            Ok constant_type
        | _ -> underlying)
    | _ -> Option.value ~default:underlying previous
  in
  let macro_shadow_names =
    List.filter_map
      (fun node ->
        match string "kind" node with
        | Some ("FasIntegerMacro" | "FasFloatMacro") -> string "name" node
        | _ -> None)
      nodes
  in
  let nodes =
    List.filter
      (fun node ->
        match string "kind" node with
        | Some ("FasIntegerMacro" | "FasFloatMacro" | "FasInvisibleMacro") -> true
        | _ ->
            not
              (List.mem
                 (Option.value ~default:"" (record_name node))
                 macro_shadow_names))
      nodes
  in
  let visible_records =
    List.filter
      (fun node ->
        not (List.mem (Option.value ~default:"" (record_name node)) macro_shadow_names))
      scope_records
  in
  let records = Hashtbl.create 64
  and record_ids = Hashtbl.create 64
  and record_nodes_by_id = Hashtbl.create 64
  and anonymous_record_ids = Hashtbl.create 32
  and anonymous_record_aliases = Hashtbl.create 32
  and enums = Hashtbl.create 32
  and enum_id_types = Hashtbl.create 32
  and enum_id_names = Hashtbl.create 32
  and alias_nodes = Hashtbl.create 64
  and alias_nodes_by_id = Hashtbl.create 64
  and alias_names_by_id = Hashtbl.create 64 in
  List.iter
    (fun node ->
      match string "kind" node with
      | Some "RecordDecl" -> (
          match
            (Option.bind (get "id" node) C_import_json.string, record_name node)
          with
          | Some id, Some name ->
              Hashtbl.replace records name (Some name);
              Hashtbl.replace record_ids id name;
              Hashtbl.replace record_nodes_by_id id node
          | Some id, None ->
              Hashtbl.replace anonymous_record_ids id ();
              Hashtbl.replace record_nodes_by_id id node
          | None, _ -> ())
      | Some "TypedefDecl" -> (
          match (record_name node, get "id" node) with
          | Some name, Some (C_import_json.Str id) ->
              Hashtbl.replace alias_nodes name node;
              Hashtbl.replace alias_nodes_by_id id node;
              Hashtbl.replace alias_names_by_id id name
          | _ -> ())
      | _ -> ())
    all_nodes;
  let rec direct_record_type_id seen node =
    match string "kind" node with
    | Some "RecordType" -> type_node_id node
    | Some "TypedefType" -> (
        match type_node_id node with
        | Some id when not (List.mem id seen) ->
            Option.bind (Hashtbl.find_opt alias_nodes_by_id id) (fun alias_node ->
                Option.bind (type_node alias_node) (direct_record_type_id (id :: seen)))
        | _ -> None)
    | Some
        ( "ElaboratedType" | "ParenType" | "MacroQualifiedType" | "AttributedType"
        | "TypeOfType" | "PredefinedSugarType" | "QualType" ) ->
        List.find_map (direct_record_type_id seen) (children node)
    | _ -> None
  in
  let typedef_record_id node =
    Option.bind (type_node node) (direct_record_type_id [])
  in
  List.iter
    (fun node ->
      if string "kind" node = Some "EnumDecl" then
        match string "id" node with
        | Some id ->
            Option.iter (Hashtbl.replace enum_id_names id) (record_name node);
            Option.iter
              (fun underlying ->
                let previous =
                  match Hashtbl.find_opt enum_id_types id with
                  | Some _ as previous -> previous
                  | None -> Option.bind (record_name node) (Hashtbl.find_opt enums)
                in
                let representation = enum_representation node underlying previous in
                Hashtbl.replace enum_id_types id representation;
                Option.iter
                  (fun name -> Hashtbl.replace enums name representation)
                  (record_name node))
              (Hashtbl.find_opt enum_probe_types id)
        | None -> ())
    all_nodes;
  List.iter
    (fun node ->
      match (string "kind" node, record_name node, typedef_record_id node) with
      | Some "TypedefDecl", Some name, Some id when Hashtbl.mem anonymous_record_ids id
        ->
          let canonical = Option.value ~default:name (Hashtbl.find_opt record_ids id) in
          Hashtbl.replace records canonical (Some canonical);
          Hashtbl.replace anonymous_record_aliases canonical ();
          Hashtbl.replace record_ids id canonical;
          Hashtbl.replace record_nodes_by_id id (Hashtbl.find record_nodes_by_id id)
      | _ -> ())
    all_nodes;
  let ordinary_names = Hashtbl.create 128
  and typedef_record_ids = Hashtbl.create 128
  and reversed_record_typedef_names = Hashtbl.create 128 in
  List.iter
    (fun node ->
      match string "kind" node with
      | Some ("FunctionDecl" | "VarDecl") ->
          Option.iter
            (fun name -> Hashtbl.replace ordinary_names name ())
            (record_name node)
      | Some "TypedefDecl" -> (
          match (record_name node, typedef_record_id node) with
          | Some name, target ->
              let previous =
                Option.value ~default:[] (Hashtbl.find_opt typedef_record_ids name)
              in
              Hashtbl.replace typedef_record_ids name (target :: previous);
              Option.iter
                (fun id ->
                  let previous =
                    Option.value ~default:[]
                      (Hashtbl.find_opt reversed_record_typedef_names id)
                  in
                  Hashtbl.replace reversed_record_typedef_names id (name :: previous))
                target
          | _ -> ())
      | Some "EnumDecl" ->
          List.iter
            (fun child ->
              if string "kind" child = Some "EnumConstantDecl" then
                Option.iter
                  (fun name -> Hashtbl.replace ordinary_names name ())
                  (record_name child))
            (children node)
      | _ -> ())
    nodes;
  let record_typedef_names =
    Hashtbl.create (Hashtbl.length reversed_record_typedef_names)
  in
  Hashtbl.iter
    (fun id names -> Hashtbl.add record_typedef_names id (List.rev names))
    reversed_record_typedef_names;
  let collides_with_ordinary tag id =
    Hashtbl.mem ordinary_names tag
    || Option.fold ~none:false
         ~some:(List.exists (fun target -> target <> Some id))
         (Hashtbl.find_opt typedef_record_ids tag)
  in
  Hashtbl.iter
    (fun id node ->
      match record_name node with
      | Some tag when collides_with_ordinary tag id ->
          let alias =
            Option.bind
              (Hashtbl.find_opt record_typedef_names id)
              (List.find_opt (fun name -> name <> tag))
          in
          (match alias with
          | Some name ->
              Hashtbl.replace records name (Some name);
              Hashtbl.replace record_ids id name
          | None -> Hashtbl.remove record_ids id);
          Hashtbl.replace records tag alias
      | _ -> ())
    record_nodes_by_id;
  let visible_record_ids = Hashtbl.create 64 in
  let add_visible_record id =
    if Hashtbl.mem record_ids id then Hashtbl.replace visible_record_ids id ()
  in
  List.iter
    (fun node -> Option.iter add_visible_record (string "id" node))
    visible_records;
  List.iter
    (fun node ->
      if string "kind" node = Some "TypedefDecl" then
        Option.iter add_visible_record (typedef_record_id node))
    all_nodes;
  List.iter
    (fun node ->
      match (string "kind" node, record_name node, enum_decl_id node) with
      | Some "TypedefDecl", Some name, Some id ->
          let representation =
            match Hashtbl.find_opt enum_id_names id with
            | Some enum_name -> (
                match Hashtbl.find_opt enums enum_name with
                | Some _ as representation -> representation
                | None -> Hashtbl.find_opt enum_id_types id)
            | None -> Hashtbl.find_opt enum_id_types id
          in
          Option.iter
            (fun representation ->
              Hashtbl.replace enum_id_types id representation;
              Hashtbl.replace enums name representation)
            representation
      | _ -> ())
    all_nodes;
  let aliases = Hashtbl.create 64 in
  let aliases_being_resolved = Hashtbl.create 16 in
  let alias_name id = Hashtbl.find_opt alias_names_by_id id in
  let alias_type_node id =
    Option.bind (Hashtbl.find_opt alias_nodes_by_id id) type_node
  in
  let machine_integer_type = function
    | "size_t" | "uintptr_t" -> Some (Ast.Int Ast.Usize)
    | "ssize_t" | "ptrdiff_t" | "intptr_t" -> Some (Ast.Int Ast.Isize)
    | _ -> None
  in
  let rec alias name =
    match Hashtbl.find_opt aliases name with
    | Some result -> result
    | None when Hashtbl.mem aliases_being_resolved name ->
        Error "recursive C typedef is not supported"
    | None -> (
        Hashtbl.replace aliases_being_resolved name ();
        match Hashtbl.find_opt typedef_layouts name with
        | Some (size, align) when align > size ->
            let result =
              Error
                (Printf.sprintf
                   "over-aligned typedef has alignment greater than its size")
            in
            Hashtbl.replace aliases name result;
            Hashtbl.remove aliases_being_resolved name;
            result
        | _ ->
            let result =
              match machine_integer_type name with
              | Some ty -> Ok ty
              | None -> (
                  match Hashtbl.find_opt alias_nodes name with
                  | None -> Error ("unknown C typedef " ^ name)
                  | Some node ->
                      Option.fold ~none:(Error "typedef has no canonical type")
                        ~some:
                          (type_result ~allow_arrays:true ~alias_name ~alias_type_node
                             ~resolve_alias:alias ~record_ids ~visible_record_ids
                             ~enum_types:enum_id_types ~allow_record:true)
                        (type_node node))
            in
            Hashtbl.replace aliases name result;
            Hashtbl.remove aliases_being_resolved name;
            result)
  in
  let source_location_key node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
        (get "loc" node)
    in
    let file =
      Option.bind loc (fun loc ->
          match string "presumedFile" loc with
          | Some _ as file -> file
          | None -> string "file" loc)
    in
    let offset =
      Option.bind loc (fun loc ->
          Option.bind (get "offset" loc) (function
            | C_import_json.Num value -> int_of_string_opt value
            | C_import_json.Str value -> int_of_string_opt value
            | _ -> None))
    in
    Option.bind file (fun file ->
        Option.map (fun offset -> "field:" ^ file ^ ":" ^ string_of_int offset) offset)
  in
  let type_tree node =
    match string "kind" node with
    | Some "TypedefDecl" -> type_node node
    | Some ("FunctionDecl" | "VarDecl" | "EnumConstantDecl") ->
        Option.bind (record_name node) (fun name ->
            Hashtbl.find_opt structured_types ("decl:" ^ name))
    | Some "FieldDecl" -> (
        let key = source_location_key node in
        match Option.bind key (Hashtbl.find_opt structured_types) with
        | Some _ as tree -> tree
        | None -> (
            let type_info = get "type" node in
            match
              Option.bind type_info (fun ty ->
                  Option.bind (string "typeAliasDeclId" ty) alias_type_node)
            with
            | Some _ as tree -> tree
            | None ->
                let rec floating_tree spelling =
                  let array_suffix () =
                    if not (String.ends_with ~suffix:"]" spelling) then None
                    else
                      Option.bind (String.rindex_opt spelling '[') (fun open_index ->
                          let size_start = open_index + 1 in
                          let size =
                            String.sub spelling size_start
                              (String.length spelling - size_start - 1)
                            |> int_of_string_opt
                          in
                          Option.map
                            (fun size -> (String.sub spelling 0 open_index, size))
                            size)
                  in
                  match array_suffix () with
                  | Some (element, size) ->
                      Option.map
                        (fun element ->
                          C_import_json.make_obj
                            [
                              ("kind", C_import_json.Str "ConstantArrayType");
                              ("size", C_import_json.Num (string_of_int size));
                              ("inner", C_import_json.Arr [ element ]);
                            ])
                        (floating_tree element)
                  | None -> (
                      match builtin_info_of_name spelling with
                      | Some (Floating _) ->
                          Some
                            (C_import_json.make_obj
                               [
                                 ("kind", C_import_json.Str "BuiltinType");
                                 ( "type",
                                   C_import_json.make_obj
                                     [ ("qualType", C_import_json.Str spelling) ] );
                               ])
                      | _ when spelling = "_Complex double" ->
                          Some
                            (C_import_json.make_obj
                               [ ("kind", C_import_json.Str "ComplexType") ])
                      | _ -> None)
                in
                Option.bind type_info (fun ty ->
                    Option.bind (string "qualType" ty) floating_tree)))
    | _ -> (
        let direct =
          Option.bind
            (Option.bind (get "id" node) C_import_json.string)
            (Hashtbl.find_opt structured_types)
        in
        match direct with
        | Some _ -> direct
        | None ->
            Option.bind (get "type" node) (fun ty ->
                Option.bind (string "typeAliasDeclId" ty) alias_type_node))
  in
  let rec qualifiers_in_type seen node =
    let own = type_qualifiers node |> List.filter (( <> ) "") in
    let nested =
      match string "kind" node with
      | Some "TypedefType" -> (
          match type_node_id node with
          | Some id when not (List.mem id seen) ->
              Option.fold ~none:[]
                ~some:(qualifiers_in_type (id :: seen))
                (alias_type_node id)
          | _ -> [])
      | _ -> List.concat_map (qualifiers_in_type seen) (children node)
    in
    own @ nested
  in
  let c_qualifiers node =
    Option.fold ~none:[] ~some:(qualifiers_in_type []) (type_tree node)
    |> List.sort_uniq compare
  in
  let parameter_qualifier_words node =
    let words value =
      String.map
        (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c | _ -> ' ')
        value
      |> String.split_on_char ' '
    in
    Option.fold ~none:[]
      ~some:(fun ty ->
        [ "qualType" ]
        |> List.filter_map (fun key -> string key ty)
        |> List.concat_map words)
      (get "type" node)
  in
  let parameter_spelled_qualifiers node =
    parameter_qualifier_words node
    |> List.filter (fun qualifier -> List.mem qualifier [ "const"; "volatile" ])
  in
  let parameter_spells_restrict node =
    parameter_qualifier_words node
    |> List.exists (fun qualifier ->
        List.mem qualifier [ "restrict"; "__restrict"; "__restrict__" ])
  in
  let restricted_function_names =
    nodes
    |> List.filter_map (fun declaration ->
        match (string "kind" declaration, record_name declaration) with
        | Some "FunctionDecl", Some name
          when List.exists
                 (fun parameter ->
                   string "kind" parameter = Some "ParmVarDecl"
                   && parameter_spells_restrict parameter)
                 (children declaration) ->
            Some (name, ())
        | _ -> None)
    |> string_index
  in
  let function_spells_restrict node =
    Option.fold ~none:false
      ~some:(Hashtbl.mem restricted_function_names)
      (record_name node)
  in
  let parameter_structural_qualifiers node =
    Option.fold ~none:[] ~some:(qualifiers_in_type []) (type_tree node)
  in
  let parameter_probe_qualifiers function_name index =
    Option.fold ~none:[] ~some:(qualifiers_in_type [])
      (Hashtbl.find_opt structured_types
         (Printf.sprintf "parameter:%s:%d" function_name index))
    |> List.filter (fun qualifier ->
        List.mem qualifier [ "restrict"; "__restrict"; "__restrict__" ])
  in
  let rec function_type_nodes node =
    match string "kind" node with
    | Some ("FunctionProtoType" | "FunctionNoProtoType") ->
        Some
          (children node
          |> List.filter (fun child ->
              Option.fold ~none:false
                ~some:(String.ends_with ~suffix:"Type")
                (string "kind" child)))
    | Some
        ( "QualType" | "ElaboratedType" | "ParenType" | "MacroQualifiedType"
        | "AttributedType" | "TypeOfType" | "TypeOfExprType" ) ->
        Option.bind (type_node node) function_type_nodes
    | _ -> None
  in
  let function_qualifiers node parameter_nodes =
    let function_name = Option.value ~default:"" (record_name node) in
    let source_parameters = List.map parameter_spelled_qualifiers parameter_nodes in
    let parameter_qualifiers =
      List.mapi
        (fun index parameter ->
          parameter_structural_qualifiers parameter
          @ parameter_probe_qualifiers function_name index)
        parameter_nodes
    in
    let from_declaration = List.concat parameter_qualifiers in
    match Option.bind (type_tree node) function_type_nodes with
    | Some (result :: parameter_types)
      when List.length parameter_types = List.length parameter_nodes ->
        let parameter_types =
          List.map2
            (fun tree (source, qualifiers) ->
              qualifiers_in_type [] tree @ qualifiers
              |> List.filter (fun qualifier ->
                  qualifier <> "volatile" || List.mem "volatile" source))
            parameter_types
            (List.combine source_parameters parameter_qualifiers)
          |> List.concat
        in
        qualifiers_in_type [] result @ parameter_types @ from_declaration
    | _ -> c_qualifiers node @ from_declaration |> List.sort_uniq compare
  in
  let rec top_level_const seen node =
    match string "kind" node with
    | Some "QualType" ->
        let own_const =
          Option.fold ~none:false
            ~some:(fun value ->
              String.map
                (function
                  | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c | _ -> ' ')
                value
              |> String.split_on_char ' ' |> List.mem "const")
            (string "qualifiers" node)
        in
        own_const
        || Option.fold ~none:false
             ~some:(fun child ->
               if string "kind" child = Some "ConstantArrayType" then
                 top_level_const seen child
               else false)
             (type_node node)
    | Some "TypedefType" -> (
        match type_node_id node with
        | Some id when not (List.mem id seen) ->
            Option.fold ~none:false
              ~some:(top_level_const (id :: seen))
              (alias_type_node id)
        | _ -> false)
    | Some
        ( "ElaboratedType" | "ParenType" | "MacroQualifiedType" | "AdjustedType"
        | "DecayedType" | "AttributedType" | "TypeOfType" | "TypeOfExprType"
        | "PredefinedSugarType" ) ->
        Option.fold ~none:false ~some:(top_level_const seen) (type_node node)
    | Some "ConstantArrayType" ->
        Option.fold ~none:false ~some:(top_level_const seen) (type_node node)
    | _ -> false
  in
  let top_level_const_node node =
    Option.fold ~none:false ~some:(top_level_const []) (type_tree node)
  in
  let parse_type ?(allow_arrays = false) ~allow_record tree =
    type_result ~allow_arrays ~alias_name ~alias_type_node ~resolve_alias:alias
      ~record_ids ~visible_record_ids ~enum_types:enum_id_types ~allow_record tree
  in
  let rec contains_function_type seen node =
    match string "kind" node with
    | Some ("FunctionProtoType" | "FunctionNoProtoType") -> true
    | Some "TypedefType" -> (
        match type_node_id node with
        | Some id when not (List.mem id seen) ->
            Option.fold ~none:false
              ~some:(contains_function_type (id :: seen))
              (alias_type_node id)
        | _ -> false)
    | _ -> List.exists (contains_function_type seen) (children node)
  in
  let rec function_pointer_type seen node =
    match string "kind" node with
    | Some "PointerType" ->
        Option.fold ~none:false ~some:(contains_function_type []) (type_node node)
    | Some "TypedefType" -> (
        match type_node_id node with
        | Some id when not (List.mem id seen) ->
            Option.fold ~none:false
              ~some:(function_pointer_type (id :: seen))
              (alias_type_node id)
        | _ -> false)
    | Some
        ( "QualType" | "ElaboratedType" | "ParenType" | "MacroQualifiedType"
        | "AttributedType" | "TypeOfType" | "TypeOfExprType" | "PredefinedSugarType" )
      ->
        Option.fold ~none:false ~some:(function_pointer_type seen) (type_node node)
    | _ -> false
  in
  let rec record_id_in_type seen node =
    match string "kind" node with
    | Some "RecordType" -> type_node_id node
    | Some "TypedefType" -> (
        match type_node_id node with
        | Some id when not (List.mem id seen) ->
            Option.bind (alias_type_node id) (record_id_in_type (id :: seen))
        | _ -> None)
    | _ -> List.find_map (record_id_in_type seen) (children node)
  in
  let record_decl_id node = Option.bind (type_tree node) (record_id_in_type []) in
  let declaration_location node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
        (get "loc" node)
    in
    let loc_value key fallback =
      Option.bind loc (fun loc ->
          match get key loc with Some _ as value -> value | None -> get fallback loc)
    in
    let number = function
      | C_import_json.Num value -> int_of_string_opt value
      | C_import_json.Str value -> int_of_string_opt value
      | _ -> None
    in
    ( (match Option.bind loc (string "presumedFile") with
      | Some _ as file -> file
      | None -> Option.bind loc (string "file")),
      Option.bind (loc_value "presumedLine" "line") number )
  in
  Hashtbl.iter (fun name _ -> ignore (alias name)) alias_nodes;
  let typed_aliases =
    Hashtbl.fold
      (fun name result acc ->
        match result with
        | Ok (Ast.Named_type (target, _)) when target = name -> acc
        | Ok ty when not (Names.reserved_binding_name name) -> (name, ty) :: acc
        | Ok _ -> acc
        | Error _ -> acc)
      aliases []
    |> List.sort compare
  in
  let entities = Hashtbl.create 256
  and unsupported = Hashtbl.create 128
  and manifest = Hashtbl.create 256
  and nonnull_parameters = Hashtbl.create 32
  and c_string_parameters = Hashtbl.create 32
  and incomplete_arrays = Hashtbl.create 16
  and static_functions = Hashtbl.create 32
  and record_calls = Hashtbl.create 16
  and header_parameter_names = Hashtbl.create 64
  and enum_owners = Hashtbl.create 64
  and items = ref [] in
  let source_cache = Hashtbl.create 16 in
  let source_text path =
    match Hashtbl.find_opt source_cache path with
    | Some text -> text
    | None ->
        let text =
          try
            let channel = open_in_bin path in
            Fun.protect
              ~finally:(fun () -> close_in_noerr channel)
              (fun () -> Some (really_input_string channel (in_channel_length channel)))
          with Sys_error _ -> None
        in
        Hashtbl.replace source_cache path text;
        text
  in
  let json_integer value =
    match value with
    | C_import_json.Num value | C_import_json.Str value -> int_of_string_opt value
    | _ -> None
  in
  let attribute_source node =
    Option.bind (get "range" node) (fun range ->
        Option.bind (get "begin" range) (fun begin_location ->
            Option.bind (get "end" range) (fun end_location ->
                let begin_location =
                  Option.value ~default:begin_location
                    (get "expansionLoc" begin_location)
                and end_location =
                  Option.value ~default:end_location (get "spellingLoc" end_location)
                in
                let path =
                  match string "file" begin_location with
                  | Some _ as path -> path
                  | None -> string "file" end_location
                in
                let start = Option.bind (get "offset" begin_location) json_integer
                and finish =
                  Option.bind
                    (Option.bind (get "offset" end_location) json_integer)
                    (fun offset ->
                      Option.map (( + ) offset)
                        (Option.bind (get "tokLen" end_location) json_integer))
                in
                match (path, start, finish) with
                | Some path, Some start, Some finish when start >= 0 && finish >= start
                  ->
                    Option.bind (source_text path) (fun text ->
                        if finish > String.length text then None
                        else Some (String.sub text start (finish - start)))
                | _ -> None)))
  in
  let parse_nonnull_attribute node =
    match attribute_source node with
    | None -> None
    | Some source -> (
        match find_text source "nonnull" 0 with
        | None -> None
        | Some start ->
            let identifier_char = function
              | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
              | _ -> false
            in
            let rec identifier_end index =
              if index < String.length source && identifier_char source.[index] then
                identifier_end (index + 1)
              else index
            in
            let rec spaces index =
              if
                index < String.length source
                && List.mem source.[index] [ ' '; '\n'; '\r'; '\t' ]
              then spaces (index + 1)
              else index
            in
            let after = spaces (identifier_end (start + String.length "nonnull")) in
            if after = String.length source || source.[after] <> '(' then Some None
            else
              let numbers = ref [] and digits = Buffer.create 8 and valid = ref true in
              let flush () =
                if Buffer.length digits > 0 then (
                  (match int_of_string_opt (Buffer.contents digits) with
                  | Some value when value > 0 -> numbers := value :: !numbers
                  | _ -> valid := false);
                  Buffer.clear digits)
              in
              for index = after + 1 to String.length source - 1 do
                match source.[index] with
                | '0' .. '9' as digit -> Buffer.add_char digits digit
                | ' ' | '\n' | '\r' | '\t' | ',' | '(' | ')' -> flush ()
                | _ -> valid := false
              done;
              flush ();
              if (not !valid) || !numbers = [] then None
              else Some (Some (List.rev !numbers)))
  in
  let parameter_type_texts parameter =
    Option.fold ~none:[]
      ~some:(fun ty ->
        [ "qualType"; "desugaredQualType" ]
        |> List.filter_map (fun field -> string field ty))
      (get "type" parameter)
  in
  let parameter_is_pointer parameter =
    parameter_type_texts parameter
    |> List.exists (fun ty -> Option.is_some (String.index_opt ty '*'))
  in
  let rec plain_char_ptr pointer n =
    match string "kind" n with
    | Some "BuiltinType" when not pointer ->
        Option.bind (get "type" n) (string "qualType") = Some "char"
    | Some "PointerType" when pointer ->
        Option.fold ~none:false ~some:(plain_char_ptr false) (type_node n)
    | Some "DecayedType" when pointer ->
        children n |> List.rev
        |> List.find_opt (fun child ->
            Option.fold ~none:false
              ~some:(String.ends_with ~suffix:"Type")
              (string "kind" child))
        |> Option.fold ~none:false ~some:(plain_char_ptr pointer)
    | Some kind when kind <> "PointerType" && String.ends_with ~suffix:"Type" kind ->
        Option.fold ~none:false ~some:(plain_char_ptr pointer) (type_node n)
    | _ -> false
  in
  let parameter_is_plain_char_pointer name i =
    match
      Option.bind
        (Hashtbl.find_opt structured_types ("decl:" ^ name))
        function_type_nodes
    with
    | Some ns ->
        Option.fold ~none:false ~some:(plain_char_ptr true) (List.nth_opt ns (i + 1))
    | None -> false
  in
  let parameter_is_nonnull parameter =
    let spelled = Option.bind (get "type" parameter) (string "qualType") in
    match spelled with
    | None -> false
    | Some spelled -> (
        match find_text spelled "_Nonnull" 0 with
        | None -> false
        | Some marker -> (
            match String.rindex_opt spelled '*' with
            | Some star -> marker > star
            | None ->
                parameter_type_texts parameter
                |> List.exists (fun ty -> Option.is_some (String.index_opt ty '*'))))
  in
  let function_nonnull_positions node parameters =
    let pointers =
      List.mapi (fun index parameter -> (index + 1, parameter)) parameters
      |> List.filter_map (fun (index, parameter) ->
          if parameter_is_pointer parameter then Some index else None)
    in
    let from_attributes =
      children node
      |> List.filter (fun child -> string "kind" child = Some "NonNullAttr")
      |> List.concat_map (fun attribute ->
          match parse_nonnull_attribute attribute with
          | Some None -> pointers
          | Some (Some indices) -> indices
          | None -> [])
    in
    let from_types =
      List.mapi (fun index parameter -> (index + 1, parameter)) parameters
      |> List.filter_map (fun (index, parameter) ->
          if parameter_is_nonnull parameter then Some index else None)
    in
    List.sort_uniq compare (from_attributes @ from_types)
  in
  let add_unsupported name reason = Hashtbl.replace unsupported name reason in
  Hashtbl.iter
    (fun name result ->
      if Names.reserved_binding_name name then
        match result with
        | Ok _ -> add_unsupported name (Names.reserved_c_name_reason name)
        | Error _ -> ())
    aliases;
  let add_item name spelling signature item origin obligations reason
      ?(normalize_restrict = Option.is_some (find_text spelling "restrict" 0))
      ?(entity_scope = "") ?(keep_unsupported_item = false) ?(parameter_names = []) () =
    let reason =
      if Names.reserved_binding_name name then Some (Names.reserved_c_name_reason name)
      else reason
    in
    let item =
      if Option.is_some reason && not keep_unsupported_item then None else item
    in
    let declaration_file, line = origin in
    let obligations =
      if normalize_restrict then
        List.map
          (function "__restrict" | "__restrict__" -> "restrict" | q -> q)
          obligations
      else obligations
    in
    let qualifier_text = List.sort_uniq compare obligations |> String.concat "," in
    let reason_text =
      Option.fold ~none:"" ~some:(fun r -> " unsupported=" ^ r) reason
    in
    let manifest_line =
      Printf.sprintf "%s\t%s\t%s\t%s\t%s:%d%s" name spelling signature qualifier_text
        (Option.value ~default:"<unknown>" declaration_file)
        (Option.value ~default:0 line)
        reason_text
    in
    Hashtbl.replace manifest name manifest_line;
    (match reason with Some reason -> add_unsupported name reason | None -> ());
    let identity_parts =
      [ signature; qualifier_text; Option.value ~default:"" reason ]
      @ if entity_scope = "" then [] else [ entity_scope ]
    in
    let identity = String.concat "\000" identity_parts in
    match Hashtbl.find_opt entities name with
    | Some previous when previous = identity -> ()
    | Some _ ->
        Hashtbl.replace entities name "\000conflict";
        add_unsupported name "conflicting C declarations"
    | None ->
        Hashtbl.add entities name identity;
        Hashtbl.replace header_parameter_names name parameter_names;
        Option.iter (fun item -> items := item :: !items) item
  in
  let origin node =
    let file, line = declaration_location node in
    (file, line)
  in
  let declaration_spelling node name =
    match Option.bind (get "type" node) (string "qualType") with
    | Some raw -> raw ^ " " ^ name
    | None ->
        let tag = Option.value ~default:"record" (string "tagUsed" node) in
        tag ^ " " ^ name
  in
  let quals node = c_qualifiers node in
  let as_type ?(allow_arrays = false) ~allow_record node =
    match type_tree node with
    | None -> Error "declaration has no structural C type"
    | Some tree -> parse_type ~allow_arrays ~allow_record tree
  in
  let record_definitions = Hashtbl.create 64 in
  List.iter
    (fun node ->
      if
        string "kind" node = Some "RecordDecl"
        && get "completeDefinition" node = Some (C_import_json.Bool true)
      then
        match Option.bind (get "id" node) C_import_json.string with
        | Some id ->
            Option.iter
              (fun name -> Hashtbl.replace record_definitions name node)
              (Hashtbl.find_opt record_ids id)
        | None -> ())
    visible_records;
  let anonymous_record_layout node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
        (get "loc" node)
    in
    Option.bind loc (fun loc ->
        let file =
          match string "presumedFile" loc with
          | Some _ as file -> file
          | None -> string "file" loc
        in
        let number = function
          | C_import_json.Num value -> int_of_string_opt value
          | C_import_json.Str value -> int_of_string_opt value
          | _ -> None
        in
        let line =
          Option.bind
            (match get "presumedLine" loc with
            | Some _ as line -> line
            | None -> get "line" loc)
            number
        and col = Option.bind (get "col" loc) number in
        match (file, line, col) with
        | Some file, Some line, Some col ->
            let tag = Option.value ~default:"struct" (string "tagUsed" node) in
            let name = Printf.sprintf "%s (unnamed at %s:%d:%d)" tag file line col in
            let suffix = Printf.sprintf "::(unnamed at %s:%d:%d)" file line col in
            find_layout [ name ] (Some suffix)
        | _ -> None)
  in
  let record_layout ?anonymous_record node name =
    let tag = Option.value ~default:"struct" (string "tagUsed" node) in
    let direct = tag ^ " " ^ Option.value ~default:name (record_name node) in
    let base =
      match Option.bind anonymous_record anonymous_record_layout with
      | Some _ as layout -> layout
      | None -> (
          match find_layout [ direct ] None with
          | Some layout -> Some layout
          | None ->
              let loc =
                Option.map
                  (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
                  (get "loc" node)
              in
              Option.bind loc (fun loc ->
                  let file =
                    match string "presumedFile" loc with
                    | Some _ as file -> file
                    | None -> string "file" loc
                  in
                  let number = function
                    | C_import_json.Num value -> int_of_string_opt value
                    | _ -> None
                  in
                  let line =
                    Option.bind
                      (match get "presumedLine" loc with
                      | Some _ as line -> line
                      | None -> get "line" loc)
                      number
                  in
                  let col = Option.bind (get "col" loc) number in
                  match (file, line, col) with
                  | Some file, Some line, Some col ->
                      let spellings =
                        [
                          Printf.sprintf "%s %s::(unnamed at %s:%d:%d)" tag name file
                            line col;
                          Printf.sprintf "%s (unnamed at %s:%d:%d)" tag file line col;
                        ]
                      in
                      find_layout spellings
                        (Some (Printf.sprintf "::(unnamed at %s:%d:%d)" file line col))
                  | _ -> None))
    in
    match base with
    | Some layout -> (
        match
          ( Hashtbl.find_opt record_layout_probes name,
            Hashtbl.find_opt typedef_layouts name )
        with
        | Some (size, align, offsets), _ ->
            Some { layout with size; align; direct_offsets = offsets }
        | None, Some (size, align) -> Some { layout with size; align }
        | _ -> Some layout)
    | None -> None
  in
  let layout_member_offset layout field field_index =
    match record_name field with
    | Some name -> (
        match List.assoc_opt name layout.direct_offsets with
        | Some _ as found -> found
        | None -> Option.map snd (List.nth_opt layout.direct_members field_index))
    | None -> Option.map snd (List.nth_opt layout.direct_members field_index)
  in
  let source_offset node =
    let loc =
      Option.map
        (fun loc -> Option.value ~default:loc (get "expansionLoc" loc))
        (get "loc" node)
    in
    Option.bind loc (fun loc ->
        Option.bind (get "offset" loc) (function
          | C_import_json.Num value -> int_of_string_opt value
          | C_import_json.Str value -> int_of_string_opt value
          | _ -> None))
  in
  let records_by_name =
    Hashtbl.fold (fun name node acc -> (name, node) :: acc) record_definitions []
    |> List.sort compare
  in
  let raw_records =
    List.map
      (fun (name, node) ->
        let unsupported = ref None and blocked = ref false in
        let reject reason =
          if Option.is_none !unsupported then unsupported := Some reason
        in
        let rec fields ?anonymous_record base record =
          let record_fields =
            children record
            |> List.filter (fun child -> string "kind" child = Some "FieldDecl")
            |> List.mapi (fun index field -> (index, field))
          in
          let layout = record_layout ?anonymous_record record name in
          List.concat_map
            (fun (field_index, field) ->
              let field_name = record_name field in
              if top_level_const_node field then (
                blocked := true;
                [])
              else if get "isBitfield" field = Some (C_import_json.Bool true) then (
                reject "bit-fields are not supported";
                [])
              else
                match field_name with
                | None -> (
                    let nested =
                      match
                        Option.bind (record_decl_id field)
                          (Hashtbl.find_opt record_nodes_by_id)
                      with
                      | Some nested -> Some nested
                      | None ->
                          let offset = source_offset field in
                          children record
                          |> List.find_opt (fun child ->
                              string "kind" child = Some "RecordDecl"
                              && record_name child = None
                              && source_offset child = offset)
                    in
                    match nested with
                    | Some nested when record_name nested = None -> (
                        let nested_layout =
                          Option.bind layout (fun layout ->
                              Option.map
                                (fun offset -> (layout, offset))
                                (layout_member_offset layout field field_index))
                        in
                        match nested_layout with
                        | None ->
                            reject "anonymous member layout is not available";
                            []
                        | Some (_, relative) ->
                            if relative < 0 || base > max_int - relative then (
                              reject "anonymous member layout is not representable";
                              [])
                            else
                              let transparent =
                                string "tagUsed" nested = Some "union"
                                && List.exists
                                     (fun child ->
                                       string "kind" child = Some "TransparentUnionAttr")
                                     (children nested)
                              in
                              if transparent then (
                                reject "transparent unions are not supported";
                                [])
                              else
                                fields ~anonymous_record:nested (base + relative) nested
                        )
                    | _ ->
                        reject "anonymous member type is not supported";
                        [])
                | Some field_name -> (
                    let reason, ty =
                      match type_tree field with
                      | None -> (Some "field has no structural C type", None)
                      | Some tree -> (
                          match
                            parse_type ~allow_arrays:true ~allow_record:true tree
                          with
                          | Ok ty -> (None, Some ty)
                          | Error "arrays of unknown size are not supported" ->
                              (Some "flexible array members are not supported", None)
                          | Error "function pointers are not supported" ->
                              (Some "function-pointer fields are not supported", None)
                          | Error reason when reason = anonymous_record_reason ->
                              (Some (anonymous_field_reason ~field_name record), None)
                          | Error reason -> (Some reason, None))
                    in
                    Option.iter reject reason;
                    match
                      ( ty,
                        Option.bind layout (fun layout ->
                            layout_member_offset layout field field_index) )
                    with
                    | Some ty, Some relative
                      when relative >= 0 && base <= max_int - relative ->
                        [
                          {
                            Ast.name = field_name;
                            ty;
                            ty_span = span;
                            span;
                            offset = Some (base + relative);
                            unsupported_reason = reason;
                          };
                        ]
                    | Some _, _ ->
                        reject "record field layout is not available";
                        []
                    | None, _ -> []))
            record_fields
        in
        let fields = fields 0 node in
        let field_names = List.map (fun (field : Ast.field) -> field.name) fields in
        if List.length (List.sort_uniq compare field_names) <> List.length field_names
        then unsupported := Some "anonymous member field names collide";
        let reason =
          if
            string "tagUsed" node = Some "union"
            && List.exists
                 (fun child -> string "kind" child = Some "TransparentUnionAttr")
                 (children node)
          then Some "transparent unions are not supported"
          else !unsupported
        in
        (name, node, fields, record_layout node name, reason, !blocked))
      records_by_name
  in
  let raw_records_index =
    string_index
      (List.map (fun ((name, _, _, _, _, _) as record) -> (name, record)) raw_records)
  in
  let rec contains_const_fields = function
    | Ast.Named_type (name, _) -> (
        match Hashtbl.find_opt raw_records_index name with
        | Some (_, _, fields, _, _, blocked) ->
            blocked
            || List.exists
                 (fun (field : Ast.field) -> contains_const_fields field.ty)
                 fields
        | None -> false)
    | Ast.Array (_, element) -> contains_const_fields element
    | _ -> false
  in
  let raw_records =
    List.map
      (fun (name, node, fields, layout, reason, blocked) ->
        ( name,
          node,
          fields,
          layout,
          reason,
          blocked
          || List.exists
               (fun (field : Ast.field) -> contains_const_fields field.ty)
               fields ))
      raw_records
  in
  let hir_ty =
    let rec convert = function
      | Ast.Bool -> Some Hir.Bool
      | Ast.Int Ast.U8 -> Some (Hir.Int Hir.U8)
      | Ast.Int U16 -> Some (Hir.Int Hir.U16)
      | Ast.Int U32 -> Some (Hir.Int Hir.U32)
      | Ast.Int U64 -> Some (Hir.Int Hir.U64)
      | Ast.Int I8 -> Some (Hir.Int Hir.I8)
      | Ast.Int I16 -> Some (Hir.Int Hir.I16)
      | Ast.Int I32 -> Some (Hir.Int Hir.I32)
      | Ast.Int I64 -> Some (Hir.Int Hir.I64)
      | Ast.Int Usize -> Some (Hir.Int Hir.Usize)
      | Ast.Int Isize -> Some (Hir.Int Hir.Isize)
      | Ast.Float Ast.F32 -> Some (Hir.Float Hir.F32)
      | Ast.Float Ast.F64 -> Some (Hir.Float Hir.F64)
      | Ast.Addr -> Some Hir.Addr
      | Ast.Handle (Ast.Named_type (name, _)) -> Some (Hir.Handle name)
      | Ast.Named_type (name, _) -> Some (Hir.Struct name)
      | Ast.Array (length, ty) ->
          Option.bind (int_of_string_opt length.text) (fun n ->
              Option.map (fun ty -> Hir.Array (n, ty)) (convert ty))
      | _ -> None
    in
    convert
  in
  let candidate_fields (name, _, fields, _, reason, blocked) =
    if Option.is_some reason || blocked then None
    else
      let fields =
        List.map
          (fun (field : Ast.field) ->
            Option.map (fun ty -> (field.name, ty)) (hir_ty field.ty))
          fields
      in
      if List.exists Option.is_none fields then None
      else Some (name, List.map Option.get fields)
  in
  let field_offsets =
    List.filter_map
      (fun (name, _, fields, _, _, _) ->
        let offsets =
          List.filter_map
            (fun (field : Ast.field) ->
              Option.map (fun offset -> (field.name, offset)) field.offset)
            fields
        in
        if offsets = [] then None else Some (name, offsets))
      raw_records
  in
  let field_reasons =
    List.filter_map
      (fun (name, _, fields, _, _, _) ->
        let reasons =
          List.filter_map
            (fun (field : Ast.field) ->
              Option.map (fun reason -> (field.name, reason)) field.unsupported_reason)
            fields
        in
        if reasons = [] then None else Some (name, reasons))
      raw_records
  in
  let byte_storage = List.map fst field_offsets in
  let union_names =
    List.filter_map
      (fun (name, node, _, _, _, _) ->
        if string "tagUsed" node = Some "union" then Some name else None)
      raw_records
  in
  let alignments =
    List.filter_map
      (fun (name, _, _, layout, reason, blocked) ->
        if Option.is_some reason || blocked then None
        else Option.map (fun layout -> (name, Some layout.align)) layout)
      raw_records
  in
  let alignments_index = string_index alignments in
  let struct_sizes =
    List.filter_map
      (fun (name, _, _, layout, reason, blocked) ->
        if Option.is_some reason || blocked then None
        else Option.map (fun layout -> (name, layout.size)) layout)
      raw_records
  in
  let struct_declarations =
    List.map
      (fun (name, fields) ->
        let align = Hashtbl.find_opt alignments_index name |> Option.join in
        (name, fields, align))
      (List.filter_map candidate_fields raw_records)
  in
  let layouts_cache =
    Hir.struct_layout_cache ~unions:union_names ~field_offsets ~field_reasons
      ~byte_storage ~struct_sizes struct_declarations
  in
  let record_results =
    List.map
      (fun (name, node, fields, layout, reason, blocked) ->
        let reason =
          if Option.is_some reason || blocked then reason
          else
            match (layout, Hir.compute_struct_cached layouts_cache name) with
            | Some clang, Ok fas
              when clang.align > 0
                   && clang.size mod clang.align = 0
                   && clang.size = fas.size && clang.align = fas.align
                   &&
                   let offsets =
                     Option.value
                       ~default:(string_index clang.direct_offsets)
                       (Hashtbl.find_opt layouts_cache.field_offsets_index name)
                   in
                   List.for_all
                     (fun (field : Hir.field) ->
                       Hashtbl.find_opt offsets field.name = Some field.offset)
                     fas.fields ->
                None
            | _ -> Some "record layout differs from C"
        in
        (name, node, fields, reason, blocked, fst (declaration_location node), layout))
      raw_records
  in
  let record_results_index =
    string_index
      (List.map
         (fun ((name, _, _, _, _, _, _) as result) -> (name, result))
         record_results)
  in
  let record_value_reason name =
    match Hashtbl.find_opt record_results_index name with
    | Some (_, _, _, _, true, _, _) -> Some "const fields are not supported"
    | Some (_, _, _, Some reason, _, _, _) -> Some reason
    | Some _ -> None
    | None when Hashtbl.mem records name ->
        Some "struct and union values are not supported"
    | None -> None
  in
  let first_record_node name =
    List.filter
      (fun node ->
        string "kind" node = Some "RecordDecl"
        && Option.bind
             (Option.bind (get "id" node) C_import_json.string)
             (fun id -> Hashtbl.find_opt record_ids id)
           = Some name)
      visible_records
    |> List.sort (fun left right ->
        compare (declaration_location left) (declaration_location right))
    |> function
    | [] -> None
    | node :: _ -> Some node
  in
  let rec type_value_reason = function
    | Ast.Named_type (name, _) -> record_value_reason name
    | Ast.Array (_, element) -> type_value_reason element
    | _ -> None
  in
  let record_types =
    List.concat_map
      (fun (name, _, _, reason, blocked, _, _) ->
        if blocked then [ (name, name, Some "const fields are not supported") ]
        else [ (name, name, reason) ])
      record_results
    @ Hashtbl.fold
        (fun name result acc ->
          match result with
          | Ok (Ast.Named_type (target, _))
            when target <> name && Hashtbl.mem record_definitions target ->
              (name, target, record_value_reason target) :: acc
          | _ -> acc)
        aliases []
  in
  List.iter
    (fun (name, node, fields, reason, blocked, file, c_layout) ->
      let is_union = string "tagUsed" node = Some "union" in
      let kind = if is_union then "union" else "struct" in
      let spelling =
        if Hashtbl.mem anonymous_record_aliases name then name else kind ^ " " ^ name
      in
      let layout =
        if blocked || Option.is_some reason then None
        else
          let align = Hashtbl.find_opt alignments_index name |> Option.join in
          Some align
      in
      let item, signature =
        match layout with
        | Some align ->
            ( Ast.Struct
                {
                  name;
                  name_span = span;
                  generic_params = [];
                  fields;
                  align;
                  size = Option.map (fun layout -> layout.size) c_layout;
                  is_union;
                  span;
                  extent = span;
                },
              kind ^ " " ^ name
              ^ (if is_union then
                   Option.fold ~none:""
                     ~some:(fun layout ->
                       Printf.sprintf " size=%d align=%d" layout.size layout.align)
                     c_layout
                 else "")
              ^ " {"
              ^ String.concat ", "
                  (List.map
                     (fun (field : Ast.field) ->
                       let c_field =
                         children node
                         |> List.find_opt (fun child ->
                             string "kind" child = Some "FieldDecl"
                             && record_name child = Some field.name)
                       in
                       let c_type = Option.bind c_field c_type_name in
                       let is_function_pointer =
                         Option.fold ~none:false
                           ~some:(fun field ->
                             Option.fold ~none:false ~some:(function_pointer_type [])
                               (type_tree field))
                           c_field
                       in
                       field.name ^ " " ^ Ast.type_name field.ty
                       ^ (if is_union then " @0" else "")
                       ^ Option.fold ~none:""
                           ~some:(fun raw -> " (C " ^ raw ^ ")")
                           (match (c_type, is_function_pointer) with
                           | Some raw, true -> Some raw
                           | _ -> None))
                     fields)
              ^ "}" )
        | None ->
            ( Ast.Opaque { name; name_span = span; span; extent = span },
              if blocked then "opaque " ^ name ^ " (const fields are not supported)"
              else "opaque " ^ name )
      in
      let reason = if blocked then Some "const fields are not supported" else reason in
      add_item name spelling signature (Some item)
        ( file,
          Option.bind (get "loc" node) (fun loc ->
              Option.bind (get "line" loc) (function
                | C_import_json.Num n -> int_of_string_opt n
                | _ -> None)) )
        [] reason ~keep_unsupported_item:true ())
    record_results;
  Hashtbl.iter
    (fun name visible ->
      if visible = Some name && not (Hashtbl.mem record_definitions name) then
        match first_record_node name with
        | None -> ()
        | Some node ->
            let item = Ast.Opaque { name; name_span = span; span; extent = span } in
            add_item name
              (declaration_spelling node name)
              ("opaque " ^ name) (Some item) (origin node) [] None ())
    records;
  let function_type node =
    match type_tree node with
    | None -> Error "function declaration has no structural C type"
    | Some tree -> (
        let tree =
          if string "kind" tree = Some "AttributedType" then
            Option.value ~default:tree (type_node tree)
          else tree
        in
        match string "kind" tree with
        | Some ("FunctionProtoType" | "FunctionNoProtoType") -> (
            let calling_convention = string "cc" tree in
            if
              Option.fold ~none:false
                ~some:(fun cc -> cc <> "cdecl" && cc <> "CC_C")
                calling_convention
            then Error "non-default calling conventions are not supported"
            else if type_has_address_space tree then
              Error "C address spaces are not supported"
            else
              let type_children =
                children tree
                |> List.filter (fun child ->
                    Option.fold ~none:false
                      ~some:(String.ends_with ~suffix:"Type")
                      (string "kind" child))
              in
              match type_children with
              | [] -> Error "function result type is not available"
              | result_node :: parameter_nodes -> (
                  let record_type = function
                    | Ast.Named_type (record, _) -> (
                        match record_value_reason record with
                        | Some reason -> Error reason
                        | None -> Ok ())
                    | _ -> Ok ()
                  in
                  match
                    Result.bind (parse_type ~allow_record:true result_node) (fun ty ->
                        Result.map (fun () -> ty) (record_type ty))
                  with
                  | Error reason -> Error reason
                  | Ok result ->
                      let rec parse_parameters acc = function
                        | [] -> Ok (List.rev acc)
                        | parameter :: rest -> (
                            match
                              Result.bind (parse_type ~allow_record:true parameter)
                                (fun ty -> Result.map (fun () -> ty) (record_type ty))
                            with
                            | Error reason -> Error reason
                            | Ok ty -> parse_parameters ((ty, []) :: acc) rest)
                      in
                      let variadic =
                        get "variadic" tree = Some (C_import_json.Bool true)
                      in
                      let by_value = function Ast.Named_type _ -> true | _ -> false in
                      Result.bind (parse_parameters [] parameter_nodes)
                        (fun parameters ->
                          if
                            variadic
                            && (by_value result
                               || List.exists (fun (ty, _) -> by_value ty) parameters)
                          then
                            Error
                              "variadic functions with struct and union values are not \
                               supported"
                          else Ok (result, parameters, variadic))))
        | _ -> Error "function declaration has no structural function type")
  in
  Hashtbl.iter
    (fun name result ->
      match result with
      | Ok ty ->
          if not (Hashtbl.mem alias_nodes name) then
            let signature = "enum " ^ name ^ " as " ^ Ast.type_name ty in
            add_item name ("enum " ^ name) signature None (None, None) [] None ()
      | Error reason -> add_unsupported name reason)
    enums;
  let enum_aliases =
    Hashtbl.fold
      (fun name result acc ->
        match result with
        | Ok ty
          when (not (List.mem_assoc name typed_aliases))
               && not (Names.reserved_binding_name name) ->
            (name, ty) :: acc
        | _ -> acc)
      enums []
  in
  let top_enum_ids =
    nodes
    |> List.filter_map (fun node ->
        if string "kind" node = Some "EnumDecl" then
          Option.map (fun id -> (id, ())) (string "id" node)
        else None)
    |> string_index
  in
  let nested_enum_nodes =
    all_nodes
    |> List.filter (fun node ->
        string "kind" node = Some "EnumDecl"
        && not (Hashtbl.mem top_enum_ids (Option.value ~default:"" (string "id" node))))
  in
  let macro_origin node =
    match
      ( string "declarationFile" node,
        Option.bind (string "declarationLine" node) int_of_string_opt )
    with
    | Some file, Some line -> (Some file, Some line)
    | _ -> (None, None)
  in
  List.iter
    (fun node ->
      let name = Option.value ~default:"" (string "name" node) in
      let kind = string "kind" node in
      match (kind, name) with
      | Some "FasIntegerMacro", name when name <> "" -> (
          let macro_code =
            Option.bind (string "macroTypeCode" node) int_of_string_opt
          in
          let value = string "value" node in
          match (Option.bind macro_code macro_type, value) with
          | Some (_, bits, unsigned), Some value ->
              let inferred_ty =
                match (bits, unsigned, macro_code) with
                | _, _, Some 11 -> Some Ast.Bool
                | 8, false, _ -> Some (Ast.Int Ast.I8)
                | 8, true, _ -> Some (Ast.Int Ast.U8)
                | 16, false, _ -> Some (Ast.Int Ast.I16)
                | 16, true, _ -> Some (Ast.Int Ast.U16)
                | 32, false, _ -> Some (Ast.Int Ast.I32)
                | 32, true, _ -> Some (Ast.Int Ast.U32)
                | 64, false, _ -> Some (Ast.Int Ast.I64)
                | 64, true, _ -> Some (Ast.Int Ast.U64)
                | _ -> None
              in
              let machine_alias_ty =
                Option.bind (string "macroTypeAliasName" node) (fun alias_name ->
                    match alias alias_name with
                    | Ok (Ast.Int (Ast.Usize | Ast.Isize) as ty) -> Some ty
                    | _ -> None)
              in
              let ty =
                match machine_alias_ty with Some _ as ty -> ty | None -> inferred_ty
              in
              Option.iter
                (fun ty ->
                  let macro_span = span in
                  let expression =
                    if String.starts_with ~prefix:"-" value then
                      Ast.Unary
                        ( Ast.Neg,
                          Ast.Int_lit
                            (String.sub value 1 (String.length value - 1), macro_span),
                          macro_span )
                    else Ast.Int_lit (value, macro_span)
                  in
                  add_item name ("macro " ^ name)
                    ("macro " ^ Ast.type_name ty ^ " " ^ value)
                    (Some (Ast.const_item name macro_span ty expression macro_span))
                    (macro_origin node) [] None ())
                ty
          | _ -> ())
      | Some "FasFloatMacro", name when name <> "" ->
          let record =
            match string "reason" node with
            | None ->
                Option.bind (string "type" node) (fun type_name ->
                    Option.map
                      (fun literal ->
                        let ty =
                          if type_name = "f32" then Ast.Float Ast.F32
                          else Ast.Float Ast.F64
                        in
                        let negative =
                          get "negative" node = Some (C_import_json.Bool true)
                        in
                        let value = (if negative then "-" else "") ^ literal in
                        let lit = Ast.Float_lit (literal, span) in
                        let expression =
                          if negative then Ast.Unary (Ast.Neg, lit, span) else lit
                        in
                        ( Some (Ast.const_item name span ty expression span),
                          "macro " ^ type_name ^ " " ^ value,
                          None ))
                      (string "literal" node))
            | Some reason -> Some (None, "macro unsupported", Some reason)
          in
          Option.iter
            (fun (item, signature, reason) ->
              add_item name ("macro " ^ name) signature item (macro_origin node) []
                reason ~keep_unsupported_item:true ())
            record
      | Some "RecordDecl", _ -> ()
      | Some "EnumDecl", _ ->
          let previous_value = ref None in
          List.iter
            (fun child ->
              if
                string "kind" child = Some "EnumConstantDecl"
                && not
                     (List.mem
                        (Option.value ~default:"" (record_name child))
                        macro_shadow_names)
              then
                match (string "name" child, type_tree child) with
                | Some constant, Some _ -> (
                    let constant_type = as_type ~allow_record:false child in
                    let explicit_value =
                      let rec find_value = function
                        | [] -> None
                        | node :: rest -> (
                            match string "value" node with
                            | Some value -> Some value
                            | None -> find_value (children node @ rest))
                      in
                      find_value (children child)
                    in
                    let value =
                      match explicit_value with
                      | Some value -> Some value
                      | None ->
                          Some
                            (Option.fold ~none:"0" ~some:add_one_decimal !previous_value)
                    in
                    previous_value := value;
                    match (constant_type, value) with
                    | Ok (Ast.Int _ as ty), Some value ->
                        Hashtbl.replace enum_owners constant (record_name node);
                        let expression =
                          if String.starts_with ~prefix:"-" value then
                            Ast.Unary
                              ( Ast.Neg,
                                Ast.Int_lit
                                  (String.sub value 1 (String.length value - 1), span),
                                span )
                          else Ast.Int_lit (value, span)
                        in
                        let item = Ast.const_item constant span ty expression span in
                        add_item constant
                          (declaration_spelling child constant)
                          (Ast.type_name ty ^ " " ^ value)
                          (Some item) (origin child) (quals child) None ()
                    | Ok _, _ ->
                        add_unsupported constant "enum constant type is not an integer"
                    | Error reason, _ -> add_unsupported constant reason)
                | _ -> ())
            (children node)
      | Some "TypedefDecl", _ -> (
          match Hashtbl.find_opt aliases name with
          | Some (Ok (Ast.Named_type (target, _))) when target = name -> ()
          | Some (Ok ty) ->
              add_item name
                (declaration_spelling node name)
                ("typedef " ^ Ast.type_name ty)
                None (origin node) (quals node) None ()
          | Some (Error reason) ->
              add_item name
                (declaration_spelling node name)
                "typedef" None (origin node) (quals node) (Some reason) ()
          | None -> ())
      | Some "FunctionDecl", _
        when get "isImplicit" node = Some (C_import_json.Bool true) ->
          ()
      | Some "FunctionDecl", _ ->
          let origin = origin node in
          let is_static = string "storageClass" node = Some "static" in
          let parameter_nodes =
            children node
            |> List.filter (fun child -> string "kind" child = Some "ParmVarDecl")
          in
          let nonnull_positions = function_nonnull_positions node parameter_nodes in
          (if nonnull_positions <> [] then
             let previous =
               Option.value ~default:[] (Hashtbl.find_opt nonnull_parameters name)
             in
             Hashtbl.replace nonnull_parameters name
               (List.sort_uniq compare (previous @ nonnull_positions)));
          let function_info = function_type node in
          (match function_info with
          | Ok (_, parameter_types, _)
            when List.length parameter_nodes = List.length parameter_types ->
              let positions =
                List.mapi
                  (fun index (parameter, (_, _)) ->
                    if
                      parameter_is_plain_char_pointer name index
                      && not
                           (List.exists
                              (fun (later_ty, _) ->
                                match later_ty with Ast.Int _ -> true | _ -> false)
                              (List.filteri
                                 (fun later _ -> later > index)
                                 parameter_types))
                    then Some (index + 1, string "name" parameter)
                    else None)
                  (List.combine parameter_nodes parameter_types)
                |> List.filter_map Fun.id
              in
              if positions <> [] then Hashtbl.replace c_string_parameters name positions
          | _ -> ());
          let variadic =
            match function_info with
            | Ok (_, _, variadic) -> variadic
            | Error _ -> false
          in
          let parameters =
            match function_info with
            | Ok (_, parameter_types, _)
              when List.length parameter_nodes = List.length parameter_types ->
                List.mapi
                  (fun index (parameter, (ty, qualifiers)) ->
                    ( Option.value
                        ~default:("arg" ^ string_of_int index)
                        (string "name" parameter),
                      Ok ty,
                      qualifiers ))
                  (List.combine parameter_nodes parameter_types)
            | _ -> []
          in
          let signature =
            match function_info with
            | Error reason -> Error reason
            | Ok (_, _, _) when List.length parameter_nodes <> List.length parameters ->
                Error "function parameter types are not available"
            | Ok (ret, _, _) ->
                let rec values acc = function
                  | [] -> Ok (List.rev acc)
                  | (name, Ok ty, _) :: rest -> values ((name, ty) :: acc) rest
                  | (_, Error reason, _) :: _ -> Error reason
                in
                Result.map (fun params -> (params, ret)) (values [] parameters)
          in
          let signature_name =
            match signature with
            | Ok (params, ret) ->
                "fn("
                ^ String.concat "," (List.map (fun (_, ty) -> Ast.type_name ty) params)
                ^ (if variadic then ",..." else "")
                ^ ")->" ^ Ast.type_name ret
            | Error reason -> "unsupported: " ^ reason
          in
          let reason =
            if
              List.exists
                (fun child -> string "kind" child = Some "BuiltinAttr")
                (children node)
            then Some "Clang builtin function"
            else match signature with Ok _ -> None | Error reason -> Some reason
          in
          let record_parameters, record_result =
            match signature with
            | Ok (params, ret) ->
                ( List.concat
                    (List.mapi
                       (fun index (_, ty) ->
                         match ty with Ast.Named_type _ -> [ index ] | _ -> [])
                       params),
                  match ret with Ast.Named_type _ -> true | _ -> false )
            | Error _ -> ([], false)
          in
          let by_value = record_parameters <> [] || record_result in
          if by_value then
            Hashtbl.replace record_calls name (record_parameters, record_result);
          (match
             (is_static || by_value, signature, c_type_name node, type_tree node)
           with
          | true, Ok (_, ret), Some c_signature, Some function_tree -> (
              match String.index_opt c_signature '(' with
              | Some open_paren ->
                  let parameter_types =
                    parameter_nodes
                    |> List.filter_map (fun child ->
                        Option.bind (get "type" child) (string "qualType"))
                  in
                  let result_type =
                    match
                      children function_tree
                      |> List.filter (fun child ->
                          Option.fold ~none:false
                            ~some:(String.ends_with ~suffix:"Type")
                            (string "kind" child))
                    with
                    | result :: _ -> Some result
                    | [] -> None
                  in
                  Hashtbl.replace static_functions name
                    {
                      name;
                      return_type =
                        c_function_result c_signature
                          (String.sub c_signature 0 open_paren);
                      parameter_types;
                      variadic;
                      void_result = ret = Ast.Void;
                      return_function_pointer =
                        Option.fold ~none:false ~some:(function_pointer_type [])
                          result_type;
                      record_parameters;
                      record_result;
                      signature = signature_name;
                      span;
                    }
              | None -> ())
          | _ -> ());
          let item =
            match (signature, reason) with
            | Ok (params, ret), None ->
                let handle = function
                  | Ast.Named_type _ as ty -> Ast.Handle ty
                  | ty -> ty
                in
                let params =
                  List.mapi
                    (fun index (_, ty) ->
                      ({
                         Ast.name = "arg" ^ string_of_int index;
                         ty = handle ty;
                         ty_span = span;
                         span;
                       }
                        : Ast.param))
                    params
                in
                let params, ret =
                  if record_result then
                    ( ({ Ast.name = "result"; ty = Ast.Handle ret; ty_span = span; span }
                        : Ast.param)
                      :: params,
                      Ast.Void )
                  else (params, ret)
                in
                Some
                  (Ast.Func
                     {
                       name;
                       name_span = span;
                       ret_span = span;
                       params;
                       ret;
                       body = Ast.Declaration;
                       linkage = Ast.External_c;
                       variadic;
                       generic_params = [];
                       span;
                       extent = span;
                       body_span = span;
                     })
            | _ -> None
          in
          add_item name
            (declaration_spelling node name)
            signature_name item origin
            (function_qualifiers node parameter_nodes
            @ List.concat_map (fun (_, _, q) -> q) parameters)
            reason
            ~normalize_restrict:(function_spells_restrict node)
            ~entity_scope:(if is_static then span.Span.file else "")
            ~parameter_names:
              (List.map
                 (fun parameter ->
                   match string "name" parameter with
                   | Some "" | None -> None
                   | Some name -> Some name)
                 parameter_nodes)
            ()
      | Some "VarDecl", _ ->
          let ty =
            match as_type ~allow_arrays:true ~allow_record:true node with
            | Error reason when reason = anonymous_record_reason ->
                Error "struct and union values are not supported"
            | result -> result
          in
          let rec incomplete_element seen tree =
            match string "kind" tree with
            | Some "IncompleteArrayType" ->
                Option.bind (type_node tree) (fun element ->
                    match parse_type ~allow_arrays:true ~allow_record:true element with
                    | Ok ty -> Some ty
                    | Error _ -> None)
            | Some "TypedefType" -> (
                match type_node_id tree with
                | Some id when not (List.mem id seen) ->
                    Option.bind (alias_type_node id) (incomplete_element (id :: seen))
                | _ -> None)
            | Some
                ( "QualType" | "ElaboratedType" | "ParenType" | "MacroQualifiedType"
                | "AttributedType" | "TypeOfType" | "TypeOfExprType"
                | "PredefinedSugarType" ) ->
                Option.bind (type_node tree) (incomplete_element seen)
            | _ -> None
          in
          let incomplete_element =
            Option.bind (type_tree node) (incomplete_element [])
          in
          Option.iter (Hashtbl.replace incomplete_arrays name) incomplete_element;
          let storage = string "storageClass" node in
          let reason =
            if storage = Some "static" then
              Some "static C globals are not externally visible"
            else
              match ty with
              | Error reason -> Some reason
              | Ok ty -> type_value_reason ty
          in
          let item =
            match (ty, reason) with
            | Ok ty, None ->
                Some
                  (Ast.Global
                     {
                       name;
                       ty_span = span;
                       ty;
                       init = None;
                       linkage =
                         (if top_level_const_node node then Ast.Import_const_c
                          else Ast.Import_c);
                       span;
                       name_span = span;
                       extent = span;
                     })
            | _ -> None
          in
          let signature =
            match ty with
            | Ok ty -> Ast.type_name ty
            | Error reason -> "unsupported: " ^ reason
          in
          add_item name
            (declaration_spelling node name)
            signature item (origin node) (quals node) reason ()
      | _ -> ())
    (nodes @ nested_enum_nodes);
  let items =
    !items
    |> List.filter (fun item ->
        Hashtbl.find_opt entities (item_name item) <> Some "\000conflict"
        &&
        match item with
        | Ast.Func { name; _ } | Ast.Global { name; _ } ->
            Option.fold ~none:true ~some:(List.mem name) referenced_names
        | _ -> true)
    |> List.sort (fun left right -> String.compare (item_name left) (item_name right))
  in
  let identities =
    Hashtbl.fold (fun name identity acc -> (name, identity) :: acc) entities []
  in
  {
    records =
      Hashtbl.fold
        (fun name visible acc ->
          if visible <> Some name then acc
          else
            let node =
              match Hashtbl.find_opt record_definitions name with
              | Some node -> Some node
              | None -> first_record_node name
            in
            match node with
            | None -> acc
            | Some node ->
                let spelling =
                  if Hashtbl.mem anonymous_record_aliases name then name
                  else
                    Option.value ~default:"struct" (string "tagUsed" node) ^ " " ^ name
                in
                let origin =
                  if Hashtbl.mem anonymous_record_aliases name then
                    match
                      List.find_opt
                        (fun node ->
                          string "kind" node = Some "TypedefDecl"
                          && string "name" node = Some name)
                        nodes
                    with
                    | Some alias -> (
                        match string "fasHeader" alias with
                        | Some header -> Some header
                        | None -> fst (declaration_location node))
                    | None -> fst (declaration_location node)
                  else
                    match string "fasHeader" node with
                    | Some header -> Some header
                    | None -> fst (declaration_location node)
                in
                (name, spelling, origin) :: acc)
        records []
      |> List.sort compare;
    record_types;
    enum_owners =
      Hashtbl.fold (fun name owner acc -> (name, owner) :: acc) enum_owners []
      |> List.sort compare;
    incomplete_arrays =
      Hashtbl.fold (fun name ty acc -> (name, ty) :: acc) incomplete_arrays [];
    container_names =
      (if container then
         List.map item_name items
         @ Hashtbl.fold (fun name _ acc -> name :: acc) unsupported []
         |> List.sort_uniq String.compare
       else []);
    items;
    aliases =
      List.filter
        (fun (name, _) -> Hashtbl.find_opt entities name <> Some "\000conflict")
        (List.sort compare (typed_aliases @ enum_aliases));
    unsupported =
      Hashtbl.fold (fun name reason acc -> (name, reason) :: acc) unsupported []
      |> List.sort compare;
    identities;
    manifest =
      Hashtbl.fold (fun _ line acc -> line :: acc) manifest [] |> List.sort compare;
    use_files;
    nonnull_parameters =
      Hashtbl.fold
        (fun name positions acc -> (name, positions) :: acc)
        nonnull_parameters []
      |> List.sort compare;
    c_string_parameters =
      Hashtbl.fold
        (fun name positions acc -> (name, positions) :: acc)
        c_string_parameters []
      |> List.sort compare;
    alloc_size_parameters;
    parameter_names =
      Hashtbl.fold
        (fun name names acc -> (name, names) :: acc)
        header_parameter_names []
      |> List.sort compare;
    static_functions =
      Hashtbl.fold (fun _ static acc -> static :: acc) static_functions []
      |> List.sort (fun a b -> String.compare a.name b.name);
    record_calls =
      Hashtbl.fold (fun name call acc -> (name, call) :: acc) record_calls []
      |> List.sort compare;
  }

let merge_imports mappings =
  let identities = Hashtbl.create 256 in
  List.iter
    (fun (name, identity) ->
      match Hashtbl.find_opt identities name with
      | Some previous when previous <> identity ->
          Hashtbl.replace identities name "\000conflict"
      | Some _ -> ()
      | None -> Hashtbl.add identities name identity)
    (List.concat_map (fun mapping -> mapping.identities) mappings);
  let bad name = Hashtbl.find_opt identities name = Some "\000conflict" in
  let items =
    List.concat_map (fun mapping -> mapping.items) mappings
    |> List.filter (fun item -> not (bad (item_name item)))
    |> List.sort_uniq (fun left right ->
        String.compare (item_name left) (item_name right))
  in
  let aliases =
    List.concat_map (fun mapping -> mapping.aliases) mappings
    |> List.sort_uniq compare
    |> List.filter (fun (name, _) -> not (bad name))
  in
  let unsupported =
    List.concat_map (fun mapping -> mapping.unsupported) mappings
    |> List.filter (fun (name, _) -> not (bad name))
    |> fun entries ->
    entries
    @ Hashtbl.fold
        (fun name identity acc ->
          if identity = "\000conflict" then (name, "conflicting C declarations") :: acc
          else acc)
        identities []
    |> List.sort_uniq compare
  in
  {
    items;
    aliases;
    records =
      List.concat_map (fun mapping -> mapping.records) mappings
      |> List.sort_uniq compare;
    record_types =
      List.concat_map (fun mapping -> mapping.record_types) mappings
      |> List.sort_uniq compare
      |> List.filter (fun (name, _, _) -> not (bad name));
    incomplete_arrays =
      List.concat_map (fun mapping -> mapping.incomplete_arrays) mappings
      |> List.sort_uniq compare
      |> List.filter (fun (name, _) -> not (bad name));
    container_names =
      List.concat_map (fun mapping -> mapping.container_names) mappings
      |> List.sort_uniq String.compare;
    enum_owners =
      List.concat_map (fun mapping -> mapping.enum_owners) mappings
      |> List.filter (fun (name, _) -> not (bad name))
      |> List.sort_uniq compare;
    unsupported;
    identities = [];
    parameter_names =
      List.concat_map (fun mapping -> mapping.parameter_names) mappings
      |> List.filter (fun (name, _) -> not (bad name))
      |> List.fold_left
           (fun merged (name, names) ->
             if List.mem_assoc name merged then merged else (name, names) :: merged)
           []
      |> List.sort compare;
    manifest = List.concat_map (fun mapping -> mapping.manifest) mappings;
    use_files = List.concat_map (fun mapping -> mapping.use_files) mappings;
    nonnull_parameters =
      List.concat_map (fun mapping -> mapping.nonnull_parameters) mappings
      |> List.filter (fun (name, _) -> not (bad name))
      |> List.fold_left
           (fun merged (name, positions) ->
             let previous = Option.value ~default:[] (List.assoc_opt name merged) in
             (name, List.sort_uniq compare (positions @ previous))
             :: List.remove_assoc name merged)
           []
      |> List.sort compare;
    c_string_parameters =
      List.concat_map (fun mapping -> mapping.c_string_parameters) mappings
      |> List.filter (fun (name, _) -> not (bad name))
      |> List.sort_uniq compare;
    alloc_size_parameters =
      List.concat_map (fun mapping -> mapping.alloc_size_parameters) mappings
      |> List.filter (fun (name, _) -> not (bad name))
      |> List.fold_left
           (fun merged (name, indices) ->
             let previous = Option.value ~default:[] (List.assoc_opt name merged) in
             (name, List.sort_uniq compare (indices @ previous))
             :: List.remove_assoc name merged)
           []
      |> List.sort compare;
    static_functions =
      List.concat_map (fun mapping -> mapping.static_functions) mappings;
    record_calls =
      List.concat_map (fun mapping -> mapping.record_calls) mappings
      |> List.filter (fun (name, _) -> not (bad name))
      |> List.sort_uniq compare;
  }

let canonical_type aliases ty =
  let rec canonical = function
    | Ast.Named_type (name, _) as ty ->
        Option.value ~default:ty (List.assoc_opt name aliases)
    | Ast.Handle inner -> Ast.Handle (canonical inner)
    | Ast.Array (length, inner) -> Ast.Array (length, canonical inner)
    | Ast.Vec (length, inner) -> Ast.Vec (length, canonical inner)
    | ty -> ty
  in
  canonical ty

let canonical_type_equal aliases left right =
  Ast.type_name (canonical_type aliases left)
  = Ast.type_name (canonical_type aliases right)

let c_signature aliases = function
  | Ast.Func { params; ret; variadic; _ } ->
      "fn("
      ^ String.concat ","
          (List.map
             (fun (p : Ast.param) -> Ast.type_name (canonical_type aliases p.ty))
             params)
      ^ (if variadic then ",..." else "")
      ^ ")->"
      ^ Ast.type_name (canonical_type aliases ret)
  | Ast.Global { ty; linkage; _ } ->
      (if linkage = Ast.Import_const_c then "const " else "")
      ^ Ast.type_name (canonical_type aliases ty)
  | _ -> "unsupported C declaration"

let source_signature aliases = function
  | Ast.Func { linkage = Ast.External_c; generic_params = []; _ } as item ->
      Some (c_signature aliases item)
  | Ast.Global { linkage = Ast.Import_c; init = None; _ } as item ->
      Some (c_signature aliases item)
  | Ast.Global { linkage = Ast.Export_c; init = Some _; _ } as item ->
      Some (c_signature aliases item)
  | _ -> None

let strip_unsupported_reason location =
  let marker = " unsupported=" in
  let length = String.length marker in
  let rec find index =
    if index + length > String.length location then location
    else if String.sub location index length = marker then String.sub location 0 index
    else find (index + 1)
  in
  find 0

let declared_at imported name =
  List.find_map
    (fun line ->
      match String.split_on_char '\t' line with
      | first :: _ :: _ :: _ :: location :: _ when first = name -> (
          let location = strip_unsupported_reason location in
          match String.rindex_opt location ':' with
          | Some colon ->
              Option.map
                (fun number -> (String.sub location 0 colon, number))
                (int_of_string_opt
                   (String.sub location (colon + 1)
                      (String.length location - colon - 1)))
          | None -> None)
      | _ -> None)
    imported.manifest

let c_declaration_note imported name =
  let located =
    Option.bind (declared_at imported name) (fun (file, number) ->
        Option.map
          (fun (header : header) -> (header, Printf.sprintf "%s:%d" file number))
          (List.assoc_opt file imported.use_files))
  in
  let chosen =
    match located with
    | Some (header, location) -> Some (header, Some location)
    | None -> (
        match
          List.sort_uniq
            (fun (a : header) (b : header) -> compare a.spelling b.spelling)
            (List.map snd imported.use_files)
        with
        | [ header ] -> Some (header, None)
        | _ -> None)
  in
  match chosen with
  | None -> None
  | Some (header, location) -> (
      match (header.spelling, location) with
      | Ast.C_fragment _, _ ->
          Some
            (Printf.sprintf "`%s` is also declared by the C block at %s" name
               (Span.to_string header.span))
      | (Ast.C_quoted path | Ast.C_system path), location -> (
          let text =
            match header.spelling with
            | Ast.C_quoted _ -> Printf.sprintf "\"%s\"" path
            | _ -> Printf.sprintf "<%s>" path
          in
          match location with
          | Some location ->
              Some
                (Printf.sprintf "`%s` is also declared by `%s` at %s" name text location)
          | None -> Some (Printf.sprintf "`%s` is also declared by `%s`" name text)))

let reconcile_source ?(container_mismatch_to_clang = false) source_items imported =
  let confirmed = ref [] in
  let bindings = List.map (fun item -> (item_name item, item)) imported.items in
  let check item =
    let name = item_name item in
    let binding = List.assoc_opt name bindings in
    if
      name = ""
      || binding = None
         && not
              (List.mem_assoc name imported.aliases
              || List.mem_assoc name imported.unsupported)
    then None
    else
      let duplicate () =
        Diag.error
          ?notes:(Option.map (fun note -> [ note ]) (c_declaration_note imported name))
          (Ast.item_span item)
          (Printf.sprintf "duplicate declaration `%s`" name)
      in
      match (item, binding, source_signature imported.aliases item) with
      | _, Some foreign, Some native ->
          let actual = c_signature imported.aliases foreign in
          if actual = native then (
            confirmed := name :: !confirmed;
            None)
          else if container_mismatch_to_clang && List.mem name imported.container_names
          then (
            confirmed := name :: !confirmed;
            None)
          else
            Some
              (Diag.error (Ast.item_span item)
                 (Printf.sprintf
                    "C declaration `%s` has type `%s`, but Fas declares `%s`" name
                    actual native))
      | _, None, Some _ when List.mem_assoc name imported.incomplete_arrays -> (
          let element = List.assoc name imported.incomplete_arrays in
          match item with
          | Ast.Global
              { linkage = Ast.Export_c; init = Some _; ty = Ast.Array (_, actual); _ }
            when canonical_type_equal imported.aliases actual element ->
              confirmed := name :: !confirmed;
              None
          | Ast.Global
              { linkage = Ast.Export_c; init = Some _; ty = Ast.Array (_, actual); _ }
            ->
              Some
                (Diag.error (Ast.item_span item)
                   (Printf.sprintf
                      "C declaration `%s` has type `arr[?, %s]`, but Fas declares `%s`"
                      name (Ast.type_name element)
                      (Ast.type_name
                         (Ast.Array
                            ( Ast.aggregate_length
                                (Ast.Ident ("?", Span.synthetic))
                                Span.synthetic,
                              actual )))))
          | _ -> Some (duplicate ()))
      | _ -> Some (duplicate ())
  in
  match List.find_map check source_items with
  | Some diagnostic -> Error [ diagnostic ]
  | None ->
      Ok
        {
          imported with
          items =
            List.filter
              (fun item -> not (List.mem (item_name item) !confirmed))
              imported.items;
          unsupported =
            List.filter
              (fun (name, _) -> not (List.mem name !confirmed))
              imported.unsupported;
          manifest =
            List.filter
              (fun line ->
                match String.index_opt line '\t' with
                | None -> true
                | Some stop ->
                    not
                      (List.mem (String.sub line 0 stop) !confirmed
                      && List.mem_assoc (String.sub line 0 stop)
                           imported.incomplete_arrays))
              imported.manifest;
          incomplete_arrays =
            List.filter
              (fun (name, _) -> not (List.mem name !confirmed))
              imported.incomplete_arrays;
          parameter_names =
            List.filter
              (fun (name, _) -> not (List.mem name !confirmed))
              imported.parameter_names;
        }

let prune_unreferenced_records ~referenced_names imported =
  let items = Hashtbl.create 256 in
  List.iter (fun item -> Hashtbl.replace items (item_name item) item) imported.items;
  let aliases = Hashtbl.create 64 in
  List.iter (fun (name, ty) -> Hashtbl.replace aliases name ty) imported.aliases;
  let reached = Hashtbl.create 256 in
  let rec visit name =
    if not (Hashtbl.mem reached name) then (
      Hashtbl.replace reached name ();
      let from_item =
        match Hashtbl.find_opt items name with
        | Some item -> Ast.used_names { Ast.items = [ item ] }
        | None -> []
      in
      let from_alias =
        match Hashtbl.find_opt aliases name with
        | Some ty ->
            Ast.used_names
              {
                Ast.items =
                  [
                    Ast.Global
                      {
                        name;
                        ty_span = Span.synthetic;
                        ty;
                        init = None;
                        linkage = Ast.Import_c;
                        span = Span.synthetic;
                        name_span = Span.synthetic;
                        extent = Span.synthetic;
                      };
                  ];
              }
        | None -> []
      in
      List.iter visit (from_item @ from_alias))
  in
  List.iter visit referenced_names;
  {
    imported with
    items =
      List.map
        (function
          | Ast.Struct { name; span; _ } when not (Hashtbl.mem reached name) ->
              Ast.Opaque { name; name_span = span; span; extent = span }
          | item -> item)
        imported.items;
  }

let manifest_text imported =
  let rows = Hashtbl.create 256 in
  List.sort compare imported.manifest
  |> List.iter (fun line ->
      match String.index_opt line '\t' with
      | None -> ()
      | Some stop ->
          let name = String.sub line 0 stop in
          if not (Hashtbl.mem rows name) then Hashtbl.add rows name line);
  let lines =
    Hashtbl.fold
      (fun name line acc ->
        match String.split_on_char '\t' line with
        | _ :: spelling :: signature :: qualifiers :: location :: _ ->
            let location =
              match find_text location " unsupported=" 0 with
              | Some stop -> String.sub location 0 stop
              | None -> location
            in
            let reason =
              Option.fold ~none:""
                ~some:(fun value -> " unsupported=" ^ value)
                (List.assoc_opt name imported.unsupported)
            in
            Printf.sprintf "%s\t%s\t%s\t%s\t%s%s" name spelling signature qualifiers
              location reason
            :: acc
        | _ -> acc)
      rows []
  in
  let lines =
    lines
    @ List.filter_map
        (fun (name, reason) ->
          if Hashtbl.mem rows name then None
          else
            Some
              (Printf.sprintf "%s\t\tunsupported\t\t<unknown>:0 unsupported=%s" name
                 reason))
        imported.unsupported
    |> List.sort compare
  in
  if lines = [] then "" else String.concat "\n" lines ^ "\n"
