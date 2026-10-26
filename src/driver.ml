let read_file path =
  try
    if Sys.file_exists path && Sys.is_directory path then
      Error (path ^ ": is a directory")
    else
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          let length = in_channel_length channel in
          Ok (really_input_string channel length))
  with Sys_error message -> Error message

let path_error_detail path message =
  let prefixes = List.sort_uniq String.compare [ path; Filename.basename path ] in
  match
    List.find_map
      (fun prefix ->
        let prefix = prefix ^ ": " in
        if String.starts_with ~prefix message then
          Some
            (String.sub message (String.length prefix)
               (String.length message - String.length prefix))
        else None)
      prefixes
  with
  | Some detail -> detail
  | None -> message

let normalize_absolute path =
  let root, separator, path =
    if Sys.win32 && String.length path >= 2 && path.[1] = ':' then
      ( String.sub path 0 2 ^ "\\",
        "\\",
        String.map
          (function '\\' -> '/' | c -> c)
          (String.sub path 2 (String.length path - 2)) )
    else ("/", "/", path)
  in
  let parts = String.split_on_char '/' path in
  let parts =
    List.fold_left
      (fun acc part ->
        match part with
        | "" | "." -> acc
        | ".." -> ( match acc with [] -> [] | _ :: rest -> rest)
        | _ -> part :: acc)
      [] parts
    |> List.rev
  in
  root ^ String.concat separator parts

let rec canonical_path path =
  try Unix.realpath path
  with Unix.Unix_error _ ->
    let absolute =
      if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
    in
    let parent = Filename.dirname absolute in
    if parent = absolute then normalize_absolute absolute
    else
      normalize_absolute
        (Filename.concat (canonical_path parent) (Filename.basename absolute))

let use_path_error path =
  if String.contains path '\000' then
    Some "Fas dependency paths cannot contain NUL bytes"
  else if not (Filename.is_relative path) then
    Some
      (Printf.sprintf
         "absolute Fas dependency path `%s` is not supported; use a relative path" path)
  else if not (Filename.check_suffix path ".fas") then
    Some (Printf.sprintf "Fas dependency path `%s` must end in lowercase `.fas`" path)
  else None

let display_dependency_path importer dependency =
  let directory = Filename.dirname importer in
  if directory = "." then dependency
  else if Sys.win32 then directory ^ "/" ^ dependency
  else Filename.concat directory dependency

let use_target importer_display importer_canonical dependency =
  let target = Filename.concat (Filename.dirname importer_canonical) dependency in
  (display_dependency_path importer_display dependency, canonical_path target)

let include_chain_note paths = "include chain: " ^ String.concat " -> " paths

let with_chain diagnostics paths =
  if List.length paths < 2 then diagnostics
  else
    List.map
      (fun (diagnostic : Diag.t) ->
        { diagnostic with Diag.notes = diagnostic.notes @ [ include_chain_note paths ] })
      diagnostics

let definition_file note =
  let prefix = "first definition is at " in
  let prefix_length = String.length prefix in
  if String.length note < prefix_length || String.sub note 0 prefix_length <> prefix
  then None
  else
    let location = String.sub note prefix_length (String.length note - prefix_length) in
    try
      let last = String.rindex location ':' in
      let before_column = String.sub location 0 last in
      let line = String.rindex before_column ':' in
      Some (String.sub before_column 0 line)
    with Not_found -> None

let add_include_chains chains diagnostics =
  let chain_for file = List.assoc_opt file chains in
  List.map
    (fun (diagnostic : Diag.t) ->
      let files =
        diagnostic.primary.Span.file :: List.filter_map definition_file diagnostic.notes
      in
      let notes =
        List.filter_map
          (fun file ->
            Option.bind (chain_for file) (function
              | _ :: _ :: _ as chain -> Some (include_chain_note chain)
              | _ -> None))
          files
        |> List.sort_uniq String.compare
      in
      { diagnostic with notes = diagnostic.notes @ notes })
    diagnostics

let c_export_diagnostic (program : Ast.program) message =
  let tick_values text =
    let rec collect index acc =
      match String.index_from_opt text index '`' with
      | None -> List.rev acc
      | Some start -> (
          match String.index_from_opt text (start + 1) '`' with
          | None -> List.rev acc
          | Some stop ->
              collect (stop + 1) (String.sub text (start + 1) (stop - start - 1) :: acc)
          )
    in
    collect 0 []
  in
  let export_name = List.hd (tick_values message) in
  let export =
    List.find_map
      (function
        | Ast.Global { name; ty; linkage = Ast.Export_c; init = Some _; span; _ }
          when name = export_name ->
            Some (span, [ ty ])
        | Ast.Func { name; params; ret; linkage = Ast.External_c; body; span; _ }
          when name = export_name && body <> Ast.Declaration ->
            Some (span, ret :: List.map (fun (param : Ast.param) -> param.ty) params)
        | _ -> None)
      program.items
  in
  let primary, types = Option.value ~default:(Span.synthetic, []) export in
  let field_note =
    match tick_values message with
    | _ :: offending :: _
      when String.ends_with ~suffix:"is a reserved C or C++ identifier" message ->
        let structs =
          List.filter_map
            (function
              | Ast.Struct { name; fields; _ } -> Some (name, fields) | _ -> None)
            program.items
        in
        let rec find visited = function
          | Ast.Named_type (name, _) when not (List.mem name visited) ->
              Option.bind (List.assoc_opt name structs) (fun fields ->
                  match
                    List.find_opt
                      (fun (field : Ast.field) -> field.Ast.name = offending)
                      fields
                  with
                  | Some (field : Ast.field) -> Some field.Ast.span
                  | None ->
                      List.find_map
                        (fun (field : Ast.field) -> find (name :: visited) field.Ast.ty)
                        fields)
          | Ast.Applied_type (name, _, args, _) -> (
              let from_fields = find visited (Ast.Named_type (name, Span.synthetic)) in
              match
                List.find_map
                  (function Ast.Type_arg ty -> find visited ty | _ -> None)
                  args
              with
              | Some _ as found -> found
              | None -> from_fields)
          | Ast.Array (_, ty) | Ast.Vec (_, ty) | Ast.Handle ty -> find visited ty
          | _ -> None
        in
        Option.map
          (fun span ->
            Printf.sprintf "offending field `%s` is declared here: %s" offending
              (Span.to_string span))
          (List.find_map (find []) types)
    | _ -> None
  in
  Diag.error ?notes:(Option.map (fun note -> [ note ]) field_note) primary message

let stat_for overlay canonical =
  match overlay canonical with
  | Some text ->
      {
        Unix.st_dev = 0;
        st_ino = 0;
        st_kind = Unix.S_REG;
        st_perm = 0o644;
        st_nlink = 1;
        st_uid = 0;
        st_gid = 0;
        st_rdev = 0;
        st_size = String.length text;
        st_atime = 0.0;
        st_mtime = 0.0;
        st_ctime = 0.0;
      }
  | None -> Unix.stat canonical

let read_for overlay canonical =
  match overlay canonical with Some text -> Ok text | None -> read_file canonical

type edit =
  | Drop_use of string * Span.t
  | Drop_c_unit of string
  | Remove_item of string * Span.t
  | Stub_fn of string * Span.t

type failure = {
  target : string option;
  use_span : Span.t;
  parent_display : string;
  parent_canonical : string;
}

type gap = { file : string; start_offset : int; end_offset : int }
type recovered = { recovered_index : Sema_index.t; gaps : gap list }

type recovery = {
  mutable files : (string * string * Ast.item list) list;
  mutable failures : failure list;
  mutable c_units : (string * C_import.header list * Ast.pkg list) list;
  mutable original_items : Ast.item list;
  mutable blank_errors : bool;
  mutable blanked : gap list;
}

let new_recovery () =
  {
    files = [];
    failures = [];
    c_units = [];
    original_items = [];
    blank_errors = false;
    blanked = [];
  }

let item_extent = function
  | Ast.Use { span; _ } | Ast.Pkg { span; _ } -> span
  | Ast.Const { extent; _ }
  | Ast.Global { extent; _ }
  | Ast.Struct { extent; _ }
  | Ast.Opaque { extent; _ }
  | Ast.Func { extent; _ } ->
      extent

let within outer inner =
  outer.Span.file = inner.Span.file
  && outer.Span.start_offset <= inner.Span.start_offset
  && inner.Span.end_offset <= outer.Span.end_offset

let apply_edits edits canonical items =
  List.fold_left
    (fun items edit ->
      match edit with
      | Drop_use (file, span) when file = canonical ->
          List.filter
            (function
              | Ast.Use { span = use_span; c_header = None; _ } -> use_span <> span
              | _ -> true)
            items
      | Drop_c_unit file when file = canonical ->
          List.filter
            (function
              | Ast.Use { c_header = Some _; _ } | Ast.Pkg _ -> false | _ -> true)
            items
      | Remove_item (file, span) when file = canonical ->
          List.filter (fun item -> item_extent item <> span) items
      | Stub_fn (file, span) when file = canonical ->
          List.map
            (function
              | Ast.Func fn when fn.body_span = span ->
                  Ast.Func { fn with body = Ast.Declaration }
              | item -> item)
            items
      | _ -> items)
    items edits

let load_program_with ?(overlay = fun _ -> None) ?recovery ?(edits = []) ~limits root =
  let recovery = Option.value recovery ~default:(new_recovery ()) in
  let loaded = Hashtbl.create 16 in
  let chains = Hashtbl.create 16 in
  let load_order = ref [] in
  let total_bytes = ref 0 in
  let budget_error span name value =
    Error
      [
        Diag.error span
          (Printf.sprintf "dependency closure exceeds budget %s of %d (profile %s)" name
             value
             (Limits.budget_profile_name limits));
      ]
  in
  let rec visit chain display_path spelling path use_span =
    let canonical = canonical_path path in
    if Hashtbl.mem loaded canonical then Ok ()
    else
      let file_chain = chain @ [ display_path ] in
      let primary = Option.value use_span ~default:Span.synthetic in
      if limits.Limits.max_use_files < 0 then
        Error
          [
            Diag.error primary
              (Printf.sprintf "budget max_use_files must not be negative (profile %s)"
                 (Limits.budget_profile_name limits));
          ]
      else if Hashtbl.length loaded >= limits.Limits.max_use_files then
        budget_error primary "max_use_files" limits.Limits.max_use_files
      else if limits.Limits.max_use_bytes < 0 then
        Error
          [
            Diag.error primary
              (Printf.sprintf "budget max_use_bytes must not be negative (profile %s)"
                 (Limits.budget_profile_name limits));
          ]
      else
        match stat_for overlay canonical with
        | stats when stats.Unix.st_kind = Unix.S_DIR ->
            let diagnostics =
              [
                Diag.error primary
                  (Printf.sprintf "Fas dependency `%s` is a directory" spelling);
              ]
            in
            if Option.is_some use_span then Error (with_chain diagnostics file_chain)
            else Error diagnostics
        | stats when stats.Unix.st_size > limits.Limits.max_use_bytes - !total_bytes ->
            budget_error primary "max_use_bytes" limits.Limits.max_use_bytes
        | _ -> (
            match read_for overlay canonical with
            | Error message ->
                let message = path_error_detail canonical message in
                let diagnostics =
                  [
                    Diag.error primary
                      (Printf.sprintf "cannot read Fas dependency path `%s`: %s"
                         spelling message);
                  ]
                in
                if Option.is_some use_span then
                  Error (with_chain diagnostics file_chain)
                else Error diagnostics
            | Ok text -> (
                if String.length text > limits.Limits.max_use_bytes - !total_bytes then
                  budget_error primary "max_use_bytes" limits.Limits.max_use_bytes
                else
                  let source = Source.create ~file:display_path ~text in
                  let outcome =
                    if recovery.blank_errors then Parser.parse_blanking ~limits source
                    else
                      Result.map
                        (fun parsed -> (parsed, []))
                        (Parser.parse ~limits source)
                  in
                  match outcome with
                  | Error diagnostics -> Error (with_chain diagnostics file_chain)
                  | Ok (parsed, blanked) ->
                      recovery.blanked <-
                        recovery.blanked
                        @ List.map
                            (fun (start_offset, end_offset) ->
                              { file = canonical; start_offset; end_offset })
                            blanked;
                      recovery.original_items <-
                        parsed.Ast.items @ recovery.original_items;
                      let program =
                        { Ast.items = apply_edits edits canonical parsed.Ast.items }
                      in
                      total_bytes := !total_bytes + String.length text;
                      Hashtbl.add loaded canonical program;
                      load_order := canonical :: !load_order;
                      Hashtbl.add chains display_path file_chain;
                      recovery.files <-
                        (display_path, canonical, program.Ast.items) :: recovery.files;
                      let note_failure target span =
                        recovery.failures <-
                          recovery.failures
                          @ [
                              {
                                target;
                                use_span = span;
                                parent_display = display_path;
                                parent_canonical = canonical;
                              };
                            ]
                      in
                      let rec dependencies = function
                        | [] -> Ok ()
                        | Ast.Use { path = dependency; c_header = None; span; _ }
                          :: rest -> (
                            match use_path_error dependency with
                            | Some message ->
                                note_failure None span;
                                Error
                                  (with_chain [ Diag.error span message ] file_chain)
                            | None -> (
                                let target_display, target_canonical =
                                  use_target display_path canonical dependency
                                in
                                match
                                  visit file_chain target_display dependency
                                    target_canonical (Some span)
                                with
                                | Error diagnostics ->
                                    note_failure (Some target_display) span;
                                    Error diagnostics
                                | Ok () -> dependencies rest))
                        | Ast.Use { c_header = Some _; _ } :: rest -> dependencies rest
                        | _ :: rest -> dependencies rest
                      in
                      dependencies program.Ast.items))
        | exception Unix.Unix_error (error, _, _) ->
            let message = Unix.error_message error in
            let diagnostics =
              [
                Diag.error primary
                  (Printf.sprintf "cannot read Fas dependency path `%s`: %s" spelling
                     message);
              ]
            in
            if Option.is_some use_span then Error (with_chain diagnostics file_chain)
            else Error diagnostics
  in
  match visit [] root root root None with
  | Error diagnostics -> Error diagnostics
  | Ok () ->
      let files =
        Hashtbl.fold (fun path program acc -> (path, program) :: acc) loaded []
        |> List.sort (fun (left, _) (right, _) -> String.compare left right)
      in
      let ordered_files =
        List.rev !load_order
        |> List.filter_map (fun path ->
            Option.map (fun program -> (path, program)) (Hashtbl.find_opt loaded path))
      in
      let items =
        List.concat_map
          (fun (_, program) ->
            List.filter
              (function Ast.Use _ | Ast.Pkg _ -> false | _ -> true)
              program.Ast.items)
          files
      in
      let imports language =
        List.filter_map
          (fun (path, program) ->
            let headers =
              List.filter_map
                (function
                  | Ast.Use { path = language_name; c_header = Some spelling; span; _ }
                    when language_name = language ->
                      Some C_import.{ spelling; span }
                  | _ -> None)
                program.Ast.items
            in
            let pkgs =
              if language <> "C" then []
              else
                List.filter_map
                  (function Ast.Pkg { pkg; _ } -> Some pkg | _ -> None)
                  program.Ast.items
            in
            if headers = [] && pkgs = [] then None else Some (path, headers, pkgs))
          ordered_files
      in
      let c_units = imports "C" in
      recovery.c_units <- c_units;
      let chains =
        Hashtbl.fold (fun path chain acc -> (path, chain) :: acc) chains []
      in
      Ok
        ( { Ast.items },
          List.map (fun (path, chain) -> (path, chain)) chains,
          c_units,
          imports "asm" )

let write_file path text =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let is_non_regular_output path =
  try (Unix.stat path).Unix.st_kind <> Unix.S_REG with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false
  | Unix.Unix_error _ -> true

let with_staged_output config write =
  if (not config.Cli.output_explicit) || is_non_regular_output config.Cli.output then
    write config.Cli.output
  else
    let staged =
      try
        Ok
          (Filename.temp_file
             ~temp_dir:(Filename.dirname config.Cli.output)
             ".fas-output-" ".tmp")
      with Sys_error _ -> Error ()
    in
    match staged with
    | Error () ->
        Error
          [
            Diag.error Span.synthetic
              (Printf.sprintf "output I/O failed for `%s`" config.Cli.output);
          ]
    | Ok staged_path -> (
        match write staged_path with
        | Error diagnostics ->
            (try Sys.remove staged_path with Sys_error _ -> ());
            Error diagnostics
        | Ok output -> (
            try
              Sys.rename staged_path config.Cli.output;
              Ok output
            with Sys_error _ ->
              (try Sys.remove staged_path with Sys_error _ -> ());
              Error
                [
                  Diag.error Span.synthetic
                    (Printf.sprintf "output I/O failed for `%s`" config.Cli.output);
                ]))

let emit_text config text =
  if config.Cli.output_explicit then
    with_staged_output config (fun path ->
        try
          write_file path text;
          Ok ""
        with Sys_error _ ->
          Error
            [
              Diag.error Span.synthetic
                (Printf.sprintf "output I/O failed for `%s`" config.Cli.output);
            ])
  else Ok text

let version = "0.6.3"

let find_executable name =
  let is_executable path =
    try
      Unix.access path [ Unix.X_OK ];
      not (Sys.is_directory path)
    with Unix.Unix_error _ | Sys_error _ -> false
  in
  let names =
    if Sys.win32 && not (Filename.check_suffix name ".exe") then [ name ^ ".exe"; name ]
    else [ name ]
  in
  if String.contains name '/' || (Sys.win32 && String.contains name '\\') then
    List.find_opt is_executable names
  else
    let directories =
      String.split_on_char
        (if Sys.win32 then ';' else ':')
        (Option.value ~default:"" (Sys.getenv_opt "PATH"))
    in
    let candidates =
      List.concat_map
        (fun directory ->
          List.map (Filename.concat (if directory = "" then "." else directory)) names)
        directories
    in
    List.find_opt is_executable candidates

let executable_path name =
  match find_executable name with Some path -> path | None -> name

let llvm_suffix = "-22"

let reports_llvm_22 path =
  match Process.run [| path; "--version" |] with
  | Ok (stdout, _) ->
      let needle = "version 22." in
      let rec scan index =
        index + String.length needle <= String.length stdout
        && (String.sub stdout index (String.length needle) = needle || scan (index + 1))
      in
      scan 0
  | Error _ -> false

let resolved_tools = Hashtbl.create 4

let tool name default =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> value
  | _ when not (String.ends_with ~suffix:llvm_suffix default) -> default
  | _ -> (
      match Hashtbl.find_opt resolved_tools default with
      | Some resolved -> resolved
      | None ->
          let plain =
            String.sub default 0 (String.length default - String.length llvm_suffix)
          in
          let resolved =
            if Option.is_some (find_executable default) then default
            else
              match find_executable plain with
              | Some path when reports_llvm_22 path -> plain
              | _ -> default
          in
          Hashtbl.replace resolved_tools default resolved;
          resolved)

let mingw_cc = "x86_64-w64-mingw32-clang"

let c_compiler () =
  match (Target_layout.current ()).os with
  | Target_layout.Windows
    when (not Sys.win32)
         && (match Sys.getenv_opt "FAS_CC" with
           | Some value -> value = ""
           | None -> true)
         && Option.is_some (find_executable mingw_cc) ->
      mingw_cc
  | Target_layout.Wasi when Option.is_none (Sys.getenv_opt "FAS_CC") ->
      let sdk =
        match Sys.getenv_opt "WASI_SDK_PATH" with
        | Some path when path <> "" -> path
        | _ -> "/opt/wasi-sdk"
      in
      Filename.concat (Filename.concat sdk "bin") "clang"
  | _ -> tool "FAS_CC" "clang-22"

let tools () = (tool "FAS_OPT" "opt-22", tool "FAS_LLC" "llc-22", c_compiler ())

let c_target_flags () =
  match (Target_layout.current ()).os with
  | Target_layout.Linux -> []
  | Target_layout.Windows | Target_layout.Wasi ->
      [ "--target=" ^ (Target_layout.current ()).triple ]

let wasm () = (Target_layout.current ()).os = Target_layout.Wasi
let profdata () = tool "FAS_PROFDATA" "llvm-profdata-22"

let first_non_empty_line text =
  String.split_on_char '\n' text
  |> List.map String.trim
  |> List.find_opt (fun line -> line <> "")

let tool_version path =
  match Process.run [| path; "--version" |] with
  | Ok (stdout, stderr) -> (
      match first_non_empty_line stdout with
      | Some line -> Some line
      | None -> first_non_empty_line stderr)
  | Error _ -> None

let version_lines () =
  let opt, llc, cc = tools () in
  let tool_line label name =
    let path = executable_path name in
    let found = Option.value ~default:"not found" (tool_version path) in
    Printf.sprintf "%s: %s (%s)" label path found
  in
  [
    Printf.sprintf "fas %s (%s)" version Build_commit.commit;
    tool_line "opt" opt;
    tool_line "llc" llc;
    tool_line "cc" cc;
    tool_line "profdata" (profdata ());
    tool_line "perf" (tool "FAS_PERF" "perf");
    tool_line "objdump" (tool "FAS_OBJDUMP" "llvm-objdump-22");
    tool_line "symbolizer" (tool "FAS_SYMBOLIZER" "llvm-symbolizer-22");
    tool_line "mca" (tool "FAS_MCA" "llvm-mca-22");
  ]

let status_name = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

let tool_error label failure =
  let output =
    if failure.Process.stderr <> "" then failure.stderr
    else if failure.stdout <> "" then failure.stdout
    else "no tool output"
  in
  Diag.error Span.synthetic
    (Printf.sprintf "%s failed (%s): %s" label (status_name failure.status)
       (String.trim output))

let ( let* ) result next =
  match result with Ok value -> next value | Error diagnostics -> Error diagnostics

let pkg_config_words text =
  let words = ref [] and current = Buffer.create 16 in
  let flush () =
    if Buffer.length current > 0 then (
      words := Buffer.contents current :: !words;
      Buffer.clear current)
  in
  let length = String.length text in
  let rec scan index =
    if index < length then
      match text.[index] with
      | ' ' | '\t' | '\n' | '\r' ->
          flush ();
          scan (index + 1)
      | '\\' when index + 1 < length ->
          Buffer.add_char current text.[index + 1];
          scan (index + 2)
      | character ->
          Buffer.add_char current character;
          scan (index + 1)
  in
  scan 0;
  flush ();
  List.rev !words

let accepted_pkg_flags words =
  let flag_prefixes = [ "-I"; "-isystem"; "-D"; "-U" ] in
  let rec loop accepted = function
    | [] -> Ok (List.rev accepted)
    | (("-I" | "-isystem" | "-D" | "-U") as flag) :: value :: rest ->
        loop ((flag ^ value) :: accepted) rest
    | "-pthread" :: rest -> loop ("-pthread" :: accepted) rest
    | token :: rest
      when List.exists
             (fun prefix ->
               String.starts_with ~prefix token
               && String.length token > String.length prefix)
             flag_prefixes ->
        loop (token :: accepted) rest
    | token :: _ -> Error token
  in
  loop [] words

let flags_text = function [] -> "(none)" | flags -> String.concat " " flags

let edit_distance a b =
  let n = String.length a and m = String.length b in
  let previous = Array.init (m + 1) Fun.id in
  for i = 1 to n do
    let current = Array.make (m + 1) i in
    for j = 1 to m do
      let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      current.(j) <-
        min (min (previous.(j) + 1) (current.(j - 1) + 1)) (previous.(j - 1) + cost)
    done;
    Array.blit current 0 previous 0 (m + 1)
  done;
  previous.(m)

let common_prefix a b =
  let limit = min (String.length a) (String.length b) in
  let rec go index =
    if index < limit && a.[index] = b.[index] then go (index + 1) else index
  in
  go 0

let rec dedupe_first = function
  | [] -> []
  | x :: rest -> x :: dedupe_first (List.filter (fun y -> y <> x) rest)

let rec dedupe_last = function
  | [] -> []
  | x :: rest -> if List.mem x rest then dedupe_last rest else x :: dedupe_last rest

let pkg_module_names modules =
  let is_operator word = List.mem word [ "="; "=="; "!="; "<"; "<="; ">"; ">=" ] in
  let strip_comma word =
    if String.ends_with ~suffix:"," word then String.sub word 0 (String.length word - 1)
    else word
  in
  let rec loop after_operator = function
    | [] -> []
    | word :: rest when is_operator word -> loop true rest
    | _ :: rest when after_operator -> loop false rest
    | word :: rest -> (
        match strip_comma word with
        | "" -> loop false rest
        | name -> name :: loop false rest)
  in
  loop false (pkg_config_words modules)

let similar_packages_help path modules =
  match Process.run [| path; "--list-all" |] with
  | Error _ -> None
  | Ok (stdout, _) -> (
      let requested = pkg_module_names modules in
      let names =
        String.split_on_char '\n' stdout
        |> List.filter_map (fun line ->
            match pkg_config_words line with name :: _ -> Some name | [] -> None)
        |> List.filter (fun name -> not (List.mem name requested))
      in
      let words = List.map String.lowercase_ascii requested in
      let similar name =
        let lower = String.lowercase_ascii name in
        List.exists
          (fun word -> edit_distance lower word <= 2 || common_prefix lower word >= 3)
          words
      in
      let rank name =
        let lower = String.lowercase_ascii name in
        let distance =
          List.fold_left
            (fun best word -> min best (edit_distance lower word))
            max_int words
        in
        let prefix =
          List.fold_left (fun best word -> max best (common_prefix lower word)) 0 words
        in
        (distance, -prefix, lower, name)
      in
      let matches =
        dedupe_first (List.filter similar names)
        |> List.sort (fun a b -> compare (rank a) (rank b))
        |> List.filteri (fun index _ -> index < 3)
      in
      match matches with
      | [] -> None
      | _ ->
          Some ("installed packages with a similar name: " ^ String.concat ", " matches)
      )

let pkg_failure_text ~exit_status status line =
  match status with
  | Unix.WEXITED code when exit_status ->
      Printf.sprintf "pkg-config exited with status %d: %s" code line
  | Unix.WEXITED _ -> line
  | status when exit_status ->
      Printf.sprintf "pkg-config failed (%s): %s" (status_name status) line
  | _ -> line

let pkg_flags ?(exit_status = true) ~keep ~option ~accept ~kind ~help (pkg : Ast.pkg) =
  let modules = pkg.Ast.modules and span = pkg.Ast.modules_span in
  let configured = tool "FAS_PKG_CONFIG" "pkg-config" in
  match find_executable configured with
  | None ->
      Error
        [
          Diag.error span
            (Printf.sprintf "`%s` not found; set FAS_PKG_CONFIG or install pkg-config"
               configured);
        ]
  | Some path -> (
      match Process.run [| path; option; modules |] with
      | Error failure ->
          let message =
            match first_non_empty_line failure.Process.stderr with
            | Some line -> pkg_failure_text ~exit_status failure.status line
            | None ->
                Printf.sprintf "`%s %s %s` failed (%s)" configured option modules
                  (status_name failure.status)
          in
          Error [ Diag.error ?help:(similar_packages_help path modules) span message ]
      | Ok (stdout, _) -> (
          match accept (pkg_config_words stdout) with
          | Error token ->
              Error
                [
                  Diag.error ~help span
                    (Printf.sprintf
                       "pkg-config %s `%s` from pkg \"%s\" is not supported" kind token
                       modules);
                ]
          | Ok flags ->
              if keep then
                prerr_endline
                  (Printf.sprintf "fas: pkg-config: %s %s %S -> %s" configured option
                     modules (flags_text flags));
              Ok flags))

let pkg_cflags ?(exit_status = true) ~keep (pkg : Ast.pkg) =
  pkg_flags ~exit_status ~keep ~option:"--cflags" ~accept:accepted_pkg_flags
    ~kind:"flag" ~help:"pass the flags you need with -I or -D" pkg

let combined_c_flags command_flags accepted =
  let defined flag =
    String.starts_with ~prefix:"-D" flag || String.starts_with ~prefix:"-U" flag
  in
  let rec joined = function
    | (("-I" | "-isystem" | "-D") as flag) :: value :: rest ->
        (flag ^ value) :: joined rest
    | flag :: rest -> flag :: joined rest
    | [] -> []
  in
  let present = joined command_flags in
  let fresh flag = not (List.mem flag present) in
  let all = List.concat accepted in
  let defines = List.filter defined all |> dedupe_last |> List.filter fresh in
  let includes =
    List.filter (fun flag -> not (defined flag)) all
    |> dedupe_first |> List.filter fresh
  in
  defines @ command_flags @ includes

let pkg_cflags_message modules =
  let pkg = { Ast.modules; modules_span = Span.synthetic } in
  match pkg_cflags ~exit_status:false ~keep:false pkg with
  | Ok flags -> Ok flags
  | Error diagnostics ->
      Error
        (String.concat "\n"
           (List.map (fun (diag : Diag.t) -> diag.message) diagnostics))

let pkg_c_flags ~keep command_flags packages =
  let rec collect accepted notes = function
    | [] -> Ok (combined_c_flags command_flags (List.rev accepted), List.rev notes)
    | (pkg : Ast.pkg) :: rest ->
        let* flags = pkg_cflags ~keep pkg in
        let note =
          Printf.sprintf "flags from pkg \"%s\": %s" pkg.Ast.modules (flags_text flags)
        in
        collect (flags :: accepted) (note :: notes) rest
  in
  collect [] [] packages

let accepted_link_flags words =
  let joined_prefixes = [ "-L"; "-l"; "-Wl,-rpath," ] in
  let rec loop accepted = function
    | [] -> Ok (List.rev accepted)
    | (("-L" | "-l") as flag) :: value :: rest
      when not (String.starts_with ~prefix:"-" value) ->
        loop ((flag ^ value) :: accepted) rest
    | "-pthread" :: rest -> loop ("-pthread" :: accepted) rest
    | token :: rest
      when List.exists
             (fun prefix ->
               String.starts_with ~prefix token
               && String.length token > String.length prefix)
             joined_prefixes ->
        loop (token :: accepted) rest
    | token :: _ -> Error token
  in
  loop [] words

type link_cache = (string, (string list, Diag.t list) result) Hashtbl.t

let new_link_cache () : link_cache = Hashtbl.create 4

let pkg_link_flags ~keep ~cache packages =
  let distinct =
    List.fold_left
      (fun seen (pkg : Ast.pkg) ->
        if
          List.exists
            (fun (earlier : Ast.pkg) -> earlier.Ast.modules = pkg.Ast.modules)
            seen
        then seen
        else seen @ [ pkg ])
      [] packages
  in
  let rec collect accepted = function
    | [] -> Ok (dedupe_last (List.concat (List.rev accepted)))
    | (pkg : Ast.pkg) :: rest ->
        let* flags =
          match Hashtbl.find_opt cache pkg.Ast.modules with
          | Some known -> known
          | None ->
              let found =
                pkg_flags ~keep ~option:"--libs" ~accept:accepted_link_flags
                  ~kind:"link flag" ~help:"pass the flags you need with -l or -L" pkg
              in
              Hashtbl.replace cache pkg.Ast.modules found;
              found
        in
        collect (flags :: accepted) rest
  in
  collect [] distinct

let limits = Limits.default

let ast_budget result =
  match result with Ok () -> Ok () | Error diagnostic -> Error [ diagnostic ]

let ir_budget program result =
  match result with
  | Ok () -> Ok ()
  | Error (offender, message) ->
      let span =
        match offender with
        | Some name -> (
            match Ast.item_span_by_name program name with
            | Some span -> span
            | None -> Span.synthetic)
        | None -> Span.synthetic
      in
      Error [ Diag.error span message ]

let run_tool label argv =
  match Process.run argv with
  | Ok _ -> Ok ()
  | Error failure -> Error [ tool_error label failure ]

let remove path = try Sys.remove path with Sys_error _ -> ()
let optimization_level level = max 0 (min 3 level)

let host_cpu_of_driver_output text =
  let words =
    String.split_on_char '\n' text
    |> List.concat_map (fun line ->
        String.split_on_char '"' line |> List.filteri (fun i _ -> i mod 2 = 1))
  in
  let rec scan cpu features = function
    | "-target-cpu" :: value :: rest -> scan (Some value) features rest
    | "-target-feature" :: value :: rest -> scan cpu (value :: features) rest
    | _ :: rest -> scan cpu features rest
    | [] -> Option.map (fun cpu -> (cpu, String.concat "," (List.rev features))) cpu
  in
  scan None [] words

let host_cpu =
  lazy
    (let _, _, cc = tools () in
     match
       Process.run [| cc; "-march=native"; "-###"; "-x"; "c"; "-c"; Filename.null |]
     with
     | Error failure ->
         Error (Printf.sprintf "%s failed (%s)" cc (status_name failure.Process.status))
     | Ok (_, stderr) ->
         Option.to_result
           ~none:("no -target-cpu in " ^ cc ^ " output")
           (host_cpu_of_driver_output stderr))

let resolve_target config =
  match config.Cli.target with
  | Cli.Portable -> Ok config
  | Cli.Force_native when wasm () ->
      Ok
        {
          config with
          Cli.target_cpu = Some "generic";
          target_features = Some "+simd128";
        }
  | Cli.Force_native -> (
      match Lazy.force host_cpu with
      | Ok (cpu, features) ->
          Ok { config with Cli.target_cpu = Some cpu; target_features = Some features }
      | Error reason ->
          Error
            [
              Diag.error Span.synthetic
                ("--target=force-native: cannot determine the host CPU (" ^ reason ^ ")");
            ])

let target_attributes config =
  match (config.Cli.target, config.Cli.target_cpu, config.Cli.target_features) with
  | Cli.Portable, None, None when wasm () ->
      Some { Ir.target_cpu = "generic"; target_tune = None; target_features = None }
  | Cli.Portable, None, None ->
      Some
        {
          Ir.target_cpu = "x86-64";
          target_tune = Some (Option.value config.Cli.tune_cpu ~default:"generic");
          target_features = None;
        }
  | Cli.Force_native, Some cpu, Some features ->
      Some { Ir.target_cpu = cpu; target_tune = None; target_features = Some features }
  | _ -> None

let host_march config =
  match config.Cli.target with
  | Cli.Force_native when wasm () -> [ "-msimd128" ]
  | Cli.Force_native -> [ "-march=native" ]
  | Cli.Portable -> []

let c_tuning config =
  match config.Cli.tune_cpu with None -> [] | Some cpu -> [ "-mtune=" ^ cpu ]

let render_ir ?(redirect = Fun.id) ?(sanitize_address = false) ?target ~debug_info ir =
  match
    Ir.render_bounded ~redirect ~sanitize_address ~debug_info ?target
      ~budget:limits.Limits.max_rendered_ir_bytes ir
  with
  | Ok text -> Ok text
  | Error message -> Error [ Diag.error Span.synthetic message ]

let unroll_values = [ 600; 1200; 2400; 4800 ]
let unroll_name value = "unroll=" ^ string_of_int value

let tune_option_kinds =
  [
    ("unroll", List.map unroll_name unroll_values);
    ("O3", [ "O3" ]);
    ("inline", [ "inline=1000" ]);
    ("interleave", [ "interleave=4" ]);
    ("max-bandwidth", [ "max-bandwidth" ]);
  ]

let tune_option_names = List.concat_map snd tune_option_kinds

let option_kind name =
  List.find_map
    (fun (kind, names) -> if List.mem name names then Some kind else None)
    tune_option_kinds

let is_unroll_name name = option_kind name = Some "unroll"

let option_value name =
  match String.index_opt name '=' with
  | Some index -> String.sub name (index + 1) (String.length name - index - 1)
  | None -> ""

let tune_opt_flag name =
  match option_kind name with
  | Some "unroll" -> [ "-unroll-threshold=" ^ option_value name ]
  | Some "inline" -> [ "-inline-threshold=" ^ option_value name ]
  | Some "interleave" -> [ "-force-vector-interleave=" ^ option_value name ]
  | Some "max-bandwidth" -> [ "-vectorizer-maximize-bandwidth" ]
  | Some _ | None -> []

let tune_opt_flags config =
  if optimization_level config.Cli.optimization > 0 then
    List.concat_map
      (fun name ->
        if List.mem name config.Cli.tune_options then tune_opt_flag name else [])
      tune_option_names
  else []

let tune_enabled config name =
  optimization_level config.Cli.optimization > 0
  && List.mem name config.Cli.tune_options

let effective_level config =
  if tune_enabled config "O3" then 3 else config.Cli.optimization

let opt_pass config =
  let configured =
    match Sys.getenv_opt "FAS_OPT_PASSES" with
    | Some value when value <> "" -> value
    | _ -> Printf.sprintf "default<O%d>" (optimization_level (effective_level config))
  in
  if List.mem "address" config.Cli.sanitizers then "asan," ^ configured else configured

let llc_opt level = Printf.sprintf "-O%d" (optimization_level level)

let c_sanitizer_flags config =
  let sanitizers =
    List.filter
      (fun name -> List.mem name config.Cli.sanitizers)
      [ "address"; "undefined" ]
  in
  match sanitizers with
  | [] -> []
  | _ -> [ "-fsanitize=" ^ String.concat "," sanitizers ]

let verify_llvm opt file =
  run_tool
    ("LLVM verification with " ^ opt)
    [| opt; "-passes=verify"; file; "-disable-output" |]

let pass_report config = opt_pass config

type profile = { indexed_path : string; temp_paths : string list }
type pgo = Instrument | Use of profile
type choice = Default | Pgo | Native | Native_pgo | Mtune | Mtune_pgo

type profile_header = {
  choice : choice;
  align_loops : int option;
  align_functions : int option;
  options : string list;
  cpu : string;
  counts : string;
}

let profile_format = "fas profile 3"
let profile_error message = Error [ Diag.error Span.synthetic message ]

let choice_name = function
  | Default -> "default"
  | Pgo -> "pgo"
  | Native -> "native"
  | Native_pgo -> "native-pgo"
  | Mtune -> "mtune"
  | Mtune_pgo -> "mtune-pgo"

let choice_of_name = function
  | "default" -> Some Default
  | "pgo" -> Some Pgo
  | "native" -> Some Native
  | "native-pgo" -> Some Native_pgo
  | "mtune" -> Some Mtune
  | "mtune-pgo" -> Some Mtune_pgo
  | _ -> None

let align_loops_name = function None -> "def" | Some n -> string_of_int n

let align_loops_of_name = function
  | "def" -> Some None
  | "32" -> Some (Some 32)
  | "64" -> Some (Some 64)
  | _ -> None

let header_value prefix line =
  if String.starts_with ~prefix line then
    Some
      (String.sub line (String.length prefix)
         (String.length line - String.length prefix))
  else None

let profile_cpu line = Option.value (header_value "cpu " line) ~default:line

let align_functions_of_name = function
  | "def" -> Some None
  | "64" -> Some (Some 64)
  | _ -> None

let profile_line_error path number expected =
  profile_error
    (Printf.sprintf "profile `%s` line %d must be `%s`" path number expected)

let profile_choice path line =
  match Option.bind line (header_value "choice ") with
  | None ->
      profile_line_error path 5 "choice default|pgo|native|native-pgo|mtune|mtune-pgo"
  | Some value -> (
      match choice_of_name value with
      | Some choice -> Ok choice
      | None ->
          profile_error
            (Printf.sprintf
               "profile `%s` has an unknown choice `%s`; expected default, pgo, \
                native, native-pgo, mtune or mtune-pgo"
               path value))

let profile_align_loops path line =
  match Option.bind line (header_value "align-loops ") with
  | None -> profile_line_error path 6 "align-loops def|32|64"
  | Some value -> (
      match align_loops_of_name value with
      | Some align_loops -> Ok align_loops
      | None ->
          profile_error
            (Printf.sprintf
               "profile `%s` has an unknown align-loops value `%s`; expected def, 32 \
                or 64"
               path value))

let profile_align_functions path line =
  match Option.bind line (header_value "align-functions ") with
  | None -> profile_line_error path 7 "align-functions def|64"
  | Some value -> (
      match align_functions_of_name value with
      | Some align_functions -> Ok align_functions
      | None ->
          profile_error
            (Printf.sprintf
               "profile `%s` has an unknown align-functions value `%s`; expected def \
                or 64"
               path value))

let or_list = function
  | [] -> ""
  | [ only ] -> only
  | names ->
      let reversed = List.rev names in
      String.concat ", " (List.rev (List.tl reversed)) ^ " or " ^ List.hd reversed

let kind_repeat seen name =
  Option.bind (option_kind name) (fun kind ->
      if List.exists (fun other -> option_kind other = Some kind) seen then Some kind
      else None)

let profile_options path line =
  let expected =
    "options "
    ^ String.concat " " (List.map (fun name -> "[" ^ name ^ "]") tune_option_names)
  in
  let names =
    match line with
    | Some "options" -> Some []
    | _ ->
        Option.bind line (header_value "options ")
        |> Option.map (fun value ->
            List.filter (fun name -> name <> "") (String.split_on_char ' ' value))
  in
  match names with
  | None -> profile_line_error path 8 expected
  | Some names ->
      let rec check seen = function
        | [] -> Ok (List.rev seen)
        | name :: _ when not (List.mem name tune_option_names) ->
            profile_error
              (Printf.sprintf "profile `%s` has an unknown option `%s`; expected %s"
                 path name (or_list tune_option_names))
        | name :: _ when List.mem name seen ->
            profile_error
              (Printf.sprintf "profile `%s` repeats the option `%s`" path name)
        | name :: rest -> (
            match kind_repeat seen name with
            | Some kind ->
                profile_error
                  (Printf.sprintf "profile `%s` has two %s values" path kind)
            | None -> check (name :: seen) rest)
      in
      check [] names

let profile_measured path number line =
  match Option.bind line (header_value "measured ") with
  | Some _ -> Ok ()
  | None ->
      profile_line_error path number
        "measured RATIO band BAND rounds R placements K metric M"

let profile_lines text =
  String.split_on_char '\n' text
  |> List.map (fun line ->
      let length = String.length line in
      if length > 0 && line.[length - 1] = '\r' then String.sub line 0 (length - 1)
      else line)

let normalize_header line =
  String.map (fun c -> if c = '\t' then ' ' else c) line
  |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "")
  |> String.concat " "

let profile_cpu_line path line =
  match Option.bind line (header_value "cpu ") with
  | Some cpu -> Ok cpu
  | None -> profile_line_error path 4 "cpu NAME"

let profile_header path text =
  let lines = profile_lines text in
  let header number = Option.map normalize_header (List.nth_opt lines (number - 1)) in
  let no_counts () =
    profile_error (Printf.sprintf "profile `%s` has no `counts` section" path)
  in
  let counts_section ?(positional = true) number =
    match header number with
    | Some "counts" ->
        Ok (String.concat "\n" (List.filteri (fun index _ -> index >= number) lines))
    | (Some _ | None) when positional -> profile_line_error path number "counts"
    | Some _ | None -> no_counts ()
  in
  match header 1 with
  | Some "fas profile 1" when header 5 = Some "counts" ->
      let* counts = counts_section ~positional:false 5 in
      Ok
        {
          choice = Pgo;
          align_loops = None;
          align_functions = None;
          options = [];
          cpu = profile_cpu (Option.value (header 4) ~default:"");
          counts;
        }
  | Some "fas profile 1" -> no_counts ()
  | Some "fas profile 2" ->
      let* cpu = profile_cpu_line path (header 4) in
      let* choice = profile_choice path (header 5) in
      let* align_loops = profile_align_loops path (header 6) in
      let* () = profile_measured path 7 (header 7) in
      let* counts = counts_section 8 in
      Ok { choice; align_loops; align_functions = None; options = []; cpu; counts }
  | Some "fas profile 3" ->
      let* cpu = profile_cpu_line path (header 4) in
      let* choice = profile_choice path (header 5) in
      let* align_loops = profile_align_loops path (header 6) in
      let* align_functions = profile_align_functions path (header 7) in
      let* options = profile_options path (header 8) in
      let* () = profile_measured path 9 (header 9) in
      let* counts = counts_section 10 in
      Ok { choice; align_loops; align_functions; options; cpu; counts }
  | _ ->
      profile_error
        (Printf.sprintf
           "`%s` is not a fas profile (expected `fas profile 1`, `fas profile 2` or \
            `fas profile 3` on line 1)"
           path)

let without_prefix prefix line = Option.value (header_value prefix line) ~default:line

let missing_profdata tool_path =
  profile_error
    (Printf.sprintf "profile merge needs llvm-profdata (FAS_PROFDATA is `%s`)" tool_path)

let profile_io f =
  try f ()
  with Sys_error message ->
    Error [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ]

let merge_profile path counts temps =
  let text_path = Filename.temp_file "fas-profile-" ".proftext" in
  temps := text_path :: !temps;
  let indexed_path = Filename.temp_file "fas-profile-" ".profdata" in
  temps := indexed_path :: !temps;
  write_file text_path counts;
  let tool_path = profdata () in
  match Process.run [| tool_path; "merge"; "-o"; indexed_path; text_path |] with
  | Ok _ -> Ok indexed_path
  | Error failure when failure.Process.status = Unix.WEXITED 127 ->
      missing_profdata tool_path
  | Error failure ->
      let reason =
        match first_non_empty_line failure.Process.stderr with
        | Some line ->
            line |> without_prefix "warning: " |> without_prefix "error: "
            |> without_prefix (text_path ^ ": ")
        | None -> status_name failure.Process.status
      in
      profile_error (Printf.sprintf "profile `%s` has invalid counts: %s" path reason)

let contains_text needle haystack =
  let needle_length = String.length needle and length = String.length haystack in
  let rec from index =
    index + needle_length <= length
    && (String.sub haystack index needle_length = needle || from (index + 1))
  in
  from 0

let check_clang () =
  let _, _, cc = tools () in
  let rejected () =
    Error
      [
        Diag.error Span.synthetic
          (Printf.sprintf
             "`fas perf --tune` needs Clang as the C compiler; FAS_CC is `%s`" cc);
      ]
  in
  match Process.run [| cc; "--version" |] with
  | Ok (stdout, stderr) when contains_text "clang" (stdout ^ stderr) -> Ok ()
  | Ok _ | Error _ -> rejected ()

let tool_output_lines path =
  match Process.run [| path; "--version" |] with
  | Ok (stdout, _) -> List.map String.trim (String.split_on_char '\n' stdout)
  | Error _ -> []

let llc_host_cpu () =
  let _, llc, _ = tools () in
  let host_prefix = "Host CPU:" in
  tool_output_lines llc
  |> List.find_map (fun line ->
      if String.starts_with ~prefix:host_prefix line then
        let rest = String.length host_prefix in
        Some (String.trim (String.sub line rest (String.length line - rest)))
      else None)
  |> Option.value ~default:"unknown"

let tune_header () =
  let opt, _, _ = tools () in
  let llvm =
    tool_output_lines opt
    |> List.find_opt (contains_text "LLVM version")
    |> Option.value ~default:"unknown"
  in
  [ profile_format; "fas " ^ version; "llvm " ^ llvm; "cpu " ^ llc_host_cpu () ]

let count_profile_functions text =
  String.split_on_char '\n' text
  |> List.filter (String.starts_with ~prefix:"# Func Hash:")
  |> List.length

let write_atomically path text =
  let temp = Filename.temp_file ~temp_dir:(Filename.dirname path) "fas-tune-" ".tmp" in
  try
    write_file temp text;
    Unix.chmod temp 0o644;
    Sys.rename temp path
  with (Sys_error _ | Unix.Unix_error _) as error ->
    remove temp;
    raise error

let io_error message =
  Error [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ]

let merge_profraw ~scratch ~profraw =
  let merged = Filename.concat scratch "merged.proftext" in
  let tool_path = profdata () in
  let argv = Array.of_list ([ tool_path; "merge"; "--text"; "-o"; merged ] @ profraw) in
  match Process.run argv with
  | Error failure when failure.Process.status = Unix.WEXITED 127 ->
      missing_profdata tool_path
  | Error failure -> Error [ tool_error ("profile merge with " ^ tool_path) failure ]
  | Ok _ -> (
      match read_file merged with
      | Error message -> io_error message
      | Ok text -> Ok text)

let write_profile_file output text =
  try Ok (write_atomically output text) with
  | Sys_error message -> io_error message
  | Unix.Unix_error (code, _, _) -> io_error (Unix.error_message code)

let with_profile config f =
  match config.Cli.profile with
  | None -> f config None
  | Some path ->
      let temps = ref [] in
      let cleanup () = if not config.Cli.keep then List.iter remove !temps in
      Fun.protect ~finally:cleanup (fun () ->
          let* text =
            read_file path
            |> Result.map_error (fun _ ->
                [
                  Diag.error Span.synthetic
                    (Printf.sprintf "cannot read profile `%s`" path);
                ])
          in
          let* header = profile_header path text in
          let native = header.choice = Native || header.choice = Native_pgo in
          let mtune = header.choice = Mtune || header.choice = Mtune_pgo in
          let pgo_choice =
            header.choice = Pgo || header.choice = Native_pgo
            || header.choice = Mtune_pgo
          in
          let* () =
            if native && config.Cli.target_explicit && config.Cli.target = Cli.Portable
            then
              profile_error
                (Printf.sprintf
                   "profile `%s` has the native choice `%s`; it cannot build with \
                    `--target=portable`"
                   path (choice_name header.choice))
            else Ok ()
          in
          let* () =
            if pgo_choice && String.trim header.counts = "" then
              profile_error
                (Printf.sprintf "profile `%s` has no counts for a pgo choice" path)
            else Ok ()
          in
          let* () =
            if native && header.cpu <> llc_host_cpu () then
              profile_error
                (Printf.sprintf
                   "profile `%s` was tuned for the CPU `%s`; this machine is `%s` (a \
                    native choice runs only on the CPU it was measured on)"
                   path header.cpu (llc_host_cpu ()))
            else Ok ()
          in
          let config =
            {
              config with
              Cli.align_loops = header.align_loops;
              Cli.align_functions = header.align_functions;
              Cli.tune_options = header.options;
              tune_cpu = (if mtune then Some header.cpu else config.Cli.tune_cpu);
              target = (if native then Cli.Force_native else config.Cli.target);
            }
          in
          match header.choice with
          | Default | Native | Mtune -> f config None
          | Pgo | Native_pgo | Mtune_pgo ->
              let* indexed_path =
                profile_io (fun () -> merge_profile path header.counts temps)
              in
              f config (Some (Use { indexed_path; temp_paths = !temps })))

let pgo_opt_flags config pgo =
  match pgo with
  | Some Instrument -> [ "--pgo-kind=pgo-instr-gen-pipeline" ]
  | Some (Use p) when optimization_level config.Cli.optimization > 0 ->
      [ "--pgo-kind=pgo-instr-use-pipeline"; "--profile-file=" ^ p.indexed_path ]
  | _ -> []

let pgo_c_flags config pgo =
  match pgo with
  | Some Instrument -> [ "-fprofile-generate" ]
  | Some (Use p) when optimization_level config.Cli.optimization > 0 ->
      [
        "-fprofile-use=" ^ p.indexed_path;
        "-Wno-profile-instr-out-of-date";
        "-Wno-profile-instr-unprofiled";
        "-Wno-profile-instr-missing";
      ]
  | _ -> []

let pgo_unit_name_flags pgo unit_path =
  match pgo with
  | None -> []
  | Some _ ->
      let depth =
        String.fold_left (fun n c -> if c = '/' then n + 1 else n) 0 unit_path
      in
      [ "-mllvm"; "-static-func-strip-dirname-prefix=" ^ string_of_int depth ]

let report_kept config pgo paths opt llc cc =
  if config.Cli.keep then (
    let temps =
      match pgo with Some (Use p) -> p.temp_paths | Some Instrument | None -> []
    in
    prerr_endline ("fas: kept intermediates: " ^ String.concat ", " (paths @ temps));
    let tune_flags =
      match tune_opt_flags config with
      | [] -> ""
      | flags -> "; opt flags " ^ String.concat " " flags
    in
    prerr_endline
      (Printf.sprintf
         "fas: tools FAS_OPT=%s FAS_LLC=%s FAS_CC=%s FAS_PROFDATA=%s; opt passes=%s%s; \
          verify before and after opt; llc %s"
         opt llc cc (profdata ()) (pass_report config) tune_flags
         (llc_opt (effective_level config))))

let with_debug_c_inputs ~enabled ~pgo_flags config cc f =
  if not (enabled || pgo_flags <> []) then f config.Cli.link_inputs
  else
    let objects = ref [] in
    let cleanup () = if not config.Cli.keep then List.iter remove !objects in
    Fun.protect ~finally:cleanup (fun () ->
        let rec compile acc = function
          | [] -> f (List.rev acc)
          | path :: rest when Filename.check_suffix path ".c" ->
              let object_path = Filename.temp_file "fas-c-link-" ".o" in
              objects := object_path :: !objects;
              let debug = if enabled then [ "-g" ] else [] in
              let* () =
                run_tool cc
                  (Array.of_list
                     ([ cc ] @ c_target_flags () @ debug
                     @ [ "-c"; path; "-o"; object_path ]
                     @ host_march config @ c_tuning config @ config.Cli.c_flags
                     @ c_sanitizer_flags config @ pgo_flags))
              in
              compile (object_path :: acc) rest
          | path :: rest -> compile (path :: acc) rest
        in
        compile [] config.Cli.link_inputs)

let executable_command config cc asm_path c_objects link_flags =
  let math_library = if List.mem "-lm" config.Cli.link_inputs then [] else [ "-lm" ] in
  let argv =
    Array.of_list
      (cc :: asm_path
      :: (c_target_flags () @ c_objects @ host_march config @ config.Cli.c_flags
        @ c_sanitizer_flags config @ config.Cli.link_inputs @ link_flags @ math_library
         @ (if wasm () then [ "-Wl,-z,stack-size=8388608" ] else [])
         @ [ "-o"; config.Cli.output ]))
  in
  if config.Cli.keep then
    prerr_endline ("fas: CC command: " ^ String.concat " " (Array.to_list argv));
  argv

let remark_option = function
  | Some path -> [ "-pass-remarks-output=" ^ path ]
  | None -> []

let target_cpu_option config =
  match (config.Cli.target, config.Cli.target_cpu) with
  | _, Some cpu -> [ "-mcpu=" ^ cpu ]
  | Cli.Portable, None when wasm () -> [ "-mcpu=generic" ]
  | Cli.Portable, None -> [ "-mcpu=x86-64" ]
  | Cli.Force_native, None -> []

let log2_bytes n =
  let rec go k = if 1 lsl k >= n then k else go (k + 1) in
  go 0

let align_functions_option config =
  match config.Cli.align_functions with
  | Some n when optimization_level config.Cli.optimization > 0 ->
      [ "-align-all-functions=" ^ string_of_int (log2_bytes n) ]
  | _ -> []

let run_llc ?llc_remarks config llc ~filetype ~input ~output =
  run_tool llc
    (Array.of_list
       ([
          llc;
          llc_opt (effective_level config);
          (if wasm () then "-relocation-model=static" else "-relocation-model=pic");
          "-filetype=" ^ filetype;
        ]
       @ (match config.Cli.align_loops with
         | Some n -> [ "-align-loops=" ^ string_of_int n ]
         | None -> [])
       @ align_functions_option config @ target_cpu_option config
       @ remark_option llc_remarks @ [ input; "-o"; output ]))

let build_assembly ?llc_remarks config llc opt_path asm_path =
  let* () =
    run_llc ?llc_remarks config llc ~filetype:"asm" ~input:opt_path ~output:asm_path
  in
  let* generated =
    read_file asm_path
    |> Result.map_error (fun message ->
        [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ])
  in
  Ok generated

let optimize_module ?remarks ~pgo config opt ll_path opt_path =
  let* () = verify_llvm opt ll_path in
  let* () =
    let address_scope_option =
      if List.mem "address" config.Cli.sanitizers then [ "-asan-use-after-scope" ]
      else []
    in
    run_tool opt
      (Array.of_list
         ([ opt; "-passes=" ^ opt_pass config ]
         @ tune_opt_flags config @ target_cpu_option config @ remark_option remarks
         @ address_scope_option @ pgo_opt_flags config pgo
         @ [ ll_path; "-S"; "-o"; opt_path ]))
  in
  verify_llvm opt opt_path

let emit_tools_unprotected ?remarks ?llc_remarks ?llvm_text ?(c_debug = false) ~pgo
    config program ir c_objects link_flags redirect =
  let* () = ir_budget program (Ir.check_static_data_bytes ~limits ir) in
  let* ll_text =
    render_ir ~redirect
      ~sanitize_address:(List.mem "address" config.Cli.sanitizers)
      ~debug_info:config.Cli.debug ?target:(target_attributes config) ir
  in
  Option.iter (fun report -> report ll_text) llvm_text;
  let ll_path = Filename.temp_file "fas-module-" ".ll" in
  let opt_path = Filename.temp_file "fas-opt-" ".ll" in
  let asm_path = Filename.temp_file "fas-module-" ".s" in
  let cleanup () =
    if not config.Cli.keep then List.iter remove [ ll_path; opt_path; asm_path ]
  in
  Fun.protect ~finally:cleanup (fun () ->
      write_file ll_path ll_text;
      let opt, llc, cc = tools () in
      report_kept config pgo [ ll_path; opt_path; asm_path ] opt llc cc;
      let* () = optimize_module ?remarks ~pgo config opt ll_path opt_path in
      match config.emit with
      | Cli.Asm ->
          let* assembly = build_assembly ?llc_remarks config llc opt_path asm_path in
          with_staged_output config (fun output ->
              try
                write_file output assembly;
                Ok ""
              with Sys_error _ ->
                Error
                  [
                    Diag.error Span.synthetic
                      (Printf.sprintf "output I/O failed for `%s`" config.output);
                  ])
      | Cli.Obj ->
          with_staged_output config (fun output ->
              let* () =
                if c_objects = [] then
                  run_llc ?llc_remarks config llc ~filetype:"obj" ~input:opt_path
                    ~output
                else
                  let fas_object = Filename.temp_file "fas-object-" ".o" in
                  let cleanup_object () =
                    if not config.Cli.keep then remove fas_object
                  in
                  Fun.protect ~finally:cleanup_object (fun () ->
                      if config.Cli.keep then
                        prerr_endline ("fas: kept Fas object: " ^ fas_object);
                      let* () =
                        run_llc ?llc_remarks config llc ~filetype:"obj" ~input:opt_path
                          ~output:fas_object
                      in
                      let argv =
                        Array.of_list
                          ((cc :: c_target_flags ())
                          @ (if wasm () then [ "-nostdlib" ] else [])
                          @ ("-r" :: fas_object :: c_objects)
                          @ [ "-o"; output ])
                      in
                      if config.Cli.keep then
                        prerr_endline
                          ("fas: CC command: " ^ String.concat " " (Array.to_list argv));
                      run_tool cc argv)
              in
              Ok "")
      | Cli.Executable ->
          let* _ = build_assembly ?llc_remarks config llc opt_path asm_path in
          let link_flags =
            match pgo with
            | Some Instrument -> "-fprofile-generate" :: link_flags
            | _ -> link_flags
          in
          with_staged_output config (fun output ->
              let output_config = { config with Cli.output } in
              let* () =
                with_debug_c_inputs ~enabled:c_debug ~pgo_flags:(pgo_c_flags config pgo)
                  output_config cc (fun link_inputs ->
                    run_tool cc
                      (executable_command
                         { output_config with Cli.link_inputs }
                         cc asm_path c_objects link_flags))
              in
              Ok "")
      | Cli.Ir | Cli.Ir_json | Cli.Llvm | Cli.Optimized_llvm | Cli.Header ->
          invalid_arg "Driver.emit_tools: non-tool emission")

let emit_tools ?remarks ?llc_remarks ?llvm_text ?c_debug ~pgo config program ir
    c_objects link_flags redirect =
  try
    emit_tools_unprotected ?remarks ?llc_remarks ?llvm_text ?c_debug ~pgo config program
      ir c_objects link_flags redirect
  with
  | Sys_error message ->
      Error [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ]
  | Unix.Unix_error (code, operation, argument) ->
      Error
        [
          Diag.error Span.synthetic
            (Printf.sprintf "backend %s(%s) failed: %s" operation argument
               (Unix.error_message code));
        ]

let with_fas_module_id text =
  if not (String.starts_with ~prefix:"; ModuleID = " text) then text
  else
    let rest =
      match String.index_opt text '\n' with
      | Some newline -> String.sub text newline (String.length text - newline)
      | None -> ""
    in
    "; ModuleID = 'fas'" ^ rest

let emit_optimized_llvm ~pgo config program ir redirect =
  let* () = ir_budget program (Ir.check_static_data_bytes ~limits ir) in
  let* ll_text =
    render_ir ~redirect
      ~sanitize_address:(List.mem "address" config.Cli.sanitizers)
      ~debug_info:config.Cli.debug ?target:(target_attributes config) ir
  in
  let ll_path = Filename.temp_file "fas-module-" ".ll" in
  let opt_path = Filename.temp_file "fas-opt-" ".ll" in
  let cleanup () = if not config.Cli.keep then List.iter remove [ ll_path; opt_path ] in
  Fun.protect ~finally:cleanup (fun () ->
      write_file ll_path ll_text;
      let opt, llc, cc = tools () in
      report_kept config pgo [ ll_path; opt_path ] opt llc cc;
      let* () = optimize_module ~pgo config opt ll_path opt_path in
      let* text =
        read_file opt_path
        |> Result.map_error (fun message ->
            [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ])
      in
      emit_text config (with_fas_module_id text))

let apply_no_inline config ir =
  let squeeze text =
    String.concat "" (String.split_on_char ' ' text |> List.filter (( <> ) ""))
  in
  let defined (f : Ir.func) = f.blocks <> [] in
  let template (f : Ir.func) = Ast.specialization_template_name f.name in
  let resolve name =
    let base =
      match String.index_opt name '[' with
      | Some index -> String.sub name 0 index
      | None -> name
    in
    let matches =
      if String.contains name '[' then
        List.filter
          (fun (f : Ir.func) ->
            template f = Some base
            && Option.fold ~none:false
                 ~some:(fun display -> squeeze display = squeeze name)
                 f.display)
          ir.Ir.funcs
      else
        List.filter
          (fun (f : Ir.func) -> f.name = name || template f = Some name)
          ir.Ir.funcs
    in
    match (matches, List.filter defined matches) with
    | [], _ ->
        let instances =
          List.filter_map
            (fun (f : Ir.func) ->
              if template f = Some base && defined f then f.display else None)
            ir.Ir.funcs
        in
        let shown = List.filteri (fun index _ -> index < 4) instances in
        let help =
          if instances = [] then None
          else
            Some
              (Printf.sprintf "emitted instances: %s%s"
                 (String.concat ", " (List.map (Printf.sprintf "`%s`") shown))
                 (if List.length instances > 4 then ", ..." else ""))
        in
        Error
          [
            {
              (Diag.error Span.synthetic
                 (Printf.sprintf "--no-inline function `%s` was not emitted" name))
              with
              help;
            };
          ]
    | _, [] ->
        Error
          [
            Diag.error Span.synthetic
              (Printf.sprintf "--no-inline function `%s` is not a normal definition"
                 name);
          ]
    | _, defined -> Ok (List.map (fun (f : Ir.func) -> f.name) defined)
  in
  let* names =
    List.fold_left
      (fun result name ->
        let* acc = result in
        let* found = resolve name in
        Ok (acc @ found))
      (Ok []) config.Cli.no_inline_functions
  in
  let names = List.sort_uniq String.compare names in
  let names =
    List.filter_map
      (fun (f : Ir.func) -> if List.mem f.name names then Some f.name else None)
      ir.Ir.funcs
  in
  Ok { ir with Ir.no_inline_functions = names }

type imported_c_unit = {
  source : string;
  stable_name : string;
  unit_path : string;
  use_span : Span.t;
  has_fragments : bool;
  assembly : string option;
  static_functions : C_import.static_function list;
  c_names : string list;
  c_flags : string list;
}

let compile_c_units ~pgo config cc units adapters standard_headers declaration_headers
    declarations artifacts =
  let rec compile acc = function
    | [] -> Ok (List.rev acc)
    | unit :: rest -> (
        let has_adapters =
          List.exists (fun adapter -> adapter.C_import.file = unit.source) adapters
        in
        if (not unit.has_fragments) && not has_adapters then compile acc rest
        else
          let* original =
            match read_file unit.unit_path with
            | Ok text -> Ok text
            | Error message -> Error [ Diag.error unit.use_span message ]
          in
          (if Option.is_none unit.assembly then
             let lines = String.split_on_char '\n' original in
             let include_path line =
               if not (String.starts_with ~prefix:"#include " line) then None
               else
                 try Some (C_import.quoted_include_path line)
                 with Scanf.Scan_failure _ -> None
             in
             let is_fragment_path path =
               String.starts_with ~prefix:"fas-c-fragment-" (Filename.basename path)
             in
             let is_fragment line =
               Option.fold ~none:false ~some:is_fragment_path (include_path line)
             in
             let is_preprocessor_fragment line =
               match include_path line with
               | Some path when is_fragment_path path -> (
                   match read_file path with
                   | Error _ -> false
                   | Ok contents ->
                       String.split_on_char '\n' contents
                       |> List.for_all (fun line ->
                           let line = String.trim line in
                           line = ""
                           || String.starts_with ~prefix:"#" line
                           || String.starts_with ~prefix:"//" line
                           || String.starts_with ~prefix:"/*" line
                           || String.starts_with ~prefix:"*" line
                           || String.starts_with ~prefix:"*/" line))
               | _ -> false
             in
             let is_header line =
               String.starts_with ~prefix:"#include <" line
               || String.starts_with ~prefix:"#include \"" line
                  && not (is_fragment line)
             in
             let rec prefix acc = function
               | line :: rest
                 when String.trim line = ""
                      || is_header line || is_preprocessor_fragment line ->
                   prefix (line :: acc) rest
               | rest -> (List.rev acc, rest)
             in
             let before, after = prefix [] lines in
             let missing_headers =
               List.filter
                 (fun header -> not (List.mem (String.trim header) lines))
                 declaration_headers
             in
             let generated =
               standard_headers ^ String.concat "" missing_headers ^ declarations
             in
             let before = String.concat "\n" before
             and after = String.concat "\n" after in
             let output =
               (if before = "" then "" else before ^ "\n") ^ generated ^ after
             in
             write_file unit.unit_path output);
          let object_path =
            if config.Cli.emit = Cli.Header && Option.is_none unit.assembly then ""
            else Filename.temp_file "fas-c-object-" ".o"
          in
          let argv =
            Array.of_list
              ([
                 cc;
                 "--target=" ^ (Target_layout.current ()).triple;
                 (if wasm () then "-fno-PIC" else "-fPIC");
                 llc_opt (effective_level config);
               ]
              @ host_march config @ c_tuning config @ unit.c_flags
              @ (if Option.is_none unit.assembly then
                   C_import.reproducible_flags unit.stable_name unit.unit_path
                 else [])
              @ (if Option.is_none unit.assembly && config.Cli.emit <> Cli.Header then
                   c_sanitizer_flags config
                 else [])
              @ (if Option.is_none unit.assembly then pgo_c_flags config pgo else [])
              @ (if Option.is_none unit.assembly then
                   pgo_unit_name_flags pgo unit.unit_path
                 else [])
              @ (if Option.is_some unit.assembly then
                   [ "-iquote"; Filename.dirname unit.source ]
                 else [])
              @ (if config.Cli.emit = Cli.Header && Option.is_none unit.assembly then
                   [ "-fsyntax-only"; unit.unit_path ]
                 else if Option.is_some unit.assembly then
                   [
                     "-x";
                     Option.get unit.assembly;
                     "-c";
                     unit.unit_path;
                     "-o";
                     object_path;
                   ]
                 else [ "-c"; unit.unit_path; "-o"; object_path ])
              @
              if Option.is_none unit.assembly then
                [ "-iquote"; Filename.dirname unit.source ]
              else [])
          in
          if config.Cli.keep then
            prerr_endline ("fas: CC command: " ^ String.concat " " (Array.to_list argv));
          match Process.run argv with
          | Ok _ when config.Cli.emit = Cli.Header && Option.is_none unit.assembly ->
              compile acc rest
          | Ok _ ->
              artifacts := object_path :: !artifacts;
              if config.Cli.keep then
                prerr_endline ("fas: kept C object: " ^ object_path);
              compile (object_path :: acc) rest
          | Error failure ->
              remove object_path;
              let prefix =
                if Option.is_some unit.assembly then "assembly failed"
                else "C compilation failed"
              in
              Error [ C_import.compilation_error ~prefix unit.use_span failure.stderr ])
  in
  compile [] units

let add_static_adapters units referenced_names ir =
  let referenced = Hashtbl.create (List.length referenced_names) in
  List.iter (fun name -> Hashtbl.replace referenced name ()) referenced_names;
  let occupied = Hashtbl.create 64 in
  List.iter
    (fun unit -> List.iter (fun name -> Hashtbl.replace occupied name ()) unit.c_names)
    units;
  List.iter (fun (func : Ir.func) -> Hashtbl.replace occupied func.name ()) ir.Ir.funcs;
  List.iter
    (function
      | Ir.String_global { name; _ }
      | Ir.Array_global { name; _ }
      | Ir.Storage_global { name; _ } ->
          Hashtbl.replace occupied name ())
    ir.Ir.globals;
  let all_adapters = ref [] in
  let rec collect = function
    | [] -> Ok (List.rev !all_adapters)
    | unit :: rest ->
        let rec make = function
          | [] -> collect rest
          | static :: tail when Hashtbl.mem referenced static.C_import.name -> (
              match
                C_import.make_adapter
                  ~occupied:
                    (Hashtbl.fold (fun name _ names -> name :: names) occupied [])
                  unit.source static
              with
              | Ok adapter ->
                  all_adapters := adapter :: !all_adapters;
                  Hashtbl.replace occupied adapter.symbol ();
                  make tail
              | Error message -> Error [ Diag.error static.span message ])
          | _ :: tail -> make tail
        in
        make unit.static_functions
  in
  let* adapters = collect units in
  List.iter
    (fun unit ->
      let adapters =
        List.filter (fun adapter -> adapter.C_import.file = unit.source) adapters
      in
      if adapters <> [] && Option.is_none unit.assembly then
        C_import.append_adapters unit.unit_path adapters)
    units;
  let entry =
    match List.find_opt (fun (f : Ir.func) -> f.name = "main") ir.Ir.funcs with
    | Some f when wasm () ->
        [ ("main", if f.params = [] then "__main_void" else "__main_argc_argv") ]
    | _ -> []
  in
  let redirects =
    entry
    @ List.map
        (fun (adapter : C_import.adapter) -> (adapter.c_name, adapter.symbol))
        adapters
  in
  let redirect name = Option.value ~default:name (List.assoc_opt name redirects) in
  match Ir.validate ir with
  | Ok () -> Ok (ir, adapters, redirect)
  | Error message -> Error [ Diag.error Span.synthetic ("internal error: " ^ message) ]

type checked = {
  program : Ast.program;
  ir : Ir.module_;
  redirect : string -> string;
  adapters : C_import.adapter list;
  c_objects : string list;
  declaration_headers : string list;
  declarations : string;
  artifacts : string list;
  link_pkgs : Ast.pkg list;
  keep : bool;
}

let release checked =
  if not checked.keep then List.iter C_import.remove_artifact checked.artifacts

let manifest_origin line =
  match String.split_on_char '\t' line with
  | name :: _ :: _ :: _ :: location :: _ -> (
      let marker = " unsupported=" in
      let rec find index =
        if index + String.length marker > String.length location then None
        else if String.sub location index (String.length marker) = marker then
          Some index
        else find (index + 1)
      in
      let location =
        match find 0 with Some stop -> String.sub location 0 stop | None -> location
      in
      match String.rindex_opt location ':' with
      | Some colon -> (
          let file = String.sub location 0 colon in
          match
            int_of_string_opt
              (String.sub location (colon + 1) (String.length location - colon - 1))
          with
          | Some line -> Some (name, (file, line))
          | None -> None)
      | None -> None)
  | _ -> None

let load_program ?overlay ~limits root = load_program_with ?overlay ~limits root

let analyze_resolved ?overlay ?cache ?index ?recovery ?(edits = []) ?pgo config =
  let c_artifacts = ref [] in
  let succeeded = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not (!succeeded || config.Cli.keep) then
        List.iter C_import.remove_artifact !c_artifacts)
    (fun () ->
      match load_program_with ?overlay ?recovery ~edits ~limits config.Cli.input with
      | Error diagnostics -> Error diagnostics
      | Ok (program, chains, c_imports, assembly_imports) -> (
          let _, _, cc = tools () in
          let imported = ref [] in
          let c_units = ref [] in
          let referenced_program =
            match recovery with
            | Some recovery -> { Ast.items = recovery.original_items }
            | None -> program
          in
          let referenced_names = Ast.unresolved_names referenced_program in
          let declared_names =
            List.filter_map
              (function
                | Ast.Const { name; _ }
                | Ast.Global { name; _ }
                | Ast.Struct { name; _ }
                | Ast.Opaque { name; _ }
                | Ast.Func { name; _ } ->
                    Some name
                | Ast.Use _ | Ast.Pkg _ -> None)
              program.Ast.items
          in
          let macro_names =
            List.filter
              (fun name -> Option.is_none (Names.value_operation name))
              (List.sort_uniq String.compare (referenced_names @ declared_names))
          in
          let* () =
            let rec import = function
              | [] -> Ok ()
              | (_, [], pkgs) :: rest ->
                  let* _ = pkg_c_flags ~keep:config.Cli.keep config.Cli.c_flags pkgs in
                  import rest
              | (source, headers, pkgs) :: rest ->
                  let* unit_c_flags, pkg_notes =
                    pkg_c_flags ~keep:config.Cli.keep config.Cli.c_flags pkgs
                  in
                  let alloc_size_parameters = ref [] in
                  let* declarations, kept, artifacts =
                    C_import.import ~alloc_size_out:alloc_size_parameters ~cc
                      ~keep:config.Cli.keep ~retain:true ~c_flags:unit_c_flags ?cache
                      ~macro_names ~referenced_names source headers
                    |> Result.map_error
                         (List.map (fun (diagnostic : Diag.t) ->
                              { diagnostic with notes = diagnostic.notes @ pkg_notes }))
                  in
                  c_artifacts := artifacts @ !c_artifacts;
                  let mapped =
                    C_import.map_declarations ~referenced_names ~headers
                      ~span:(List.hd headers).C_import.span
                      ~alloc_size_parameters:!alloc_size_parameters
                      ~container:
                        (List.exists
                           (fun header ->
                             match header.C_import.spelling with
                             | Ast.C_fragment _ -> true
                             | Ast.C_quoted _ | Ast.C_system _ -> false)
                           headers)
                      declarations
                  in
                  c_units :=
                    {
                      source;
                      stable_name = C_import.stable_unit_name source headers;
                      assembly = None;
                      unit_path = List.hd artifacts;
                      use_span = (List.hd headers).C_import.span;
                      has_fragments =
                        List.exists
                          (fun header ->
                            match header.C_import.spelling with
                            | Ast.C_fragment _ -> true
                            | Ast.C_quoted _ | Ast.C_system _ -> false)
                          headers;
                      static_functions = mapped.static_functions;
                      c_flags = unit_c_flags;
                      c_names =
                        List.filter_map
                          (fun line ->
                            Option.map
                              (fun stop -> String.sub line 0 stop)
                              (String.index_opt line '\t'))
                          mapped.manifest;
                    }
                    :: !c_units;
                  imported := mapped :: !imported;
                  Option.iter
                    (function
                      | unit_path :: fragments ->
                          prerr_endline ("fas: kept C import unit: " ^ unit_path);
                          List.iter
                            (fun path ->
                              prerr_endline ("fas: kept C fragment: " ^ path))
                            fragments
                      | [] -> ())
                    kept;
                  import rest
            in
            import c_imports
          in
          let* () =
            Result_list.iter
              (fun (source, headers, _) ->
                let unit_path = Filename.temp_file "fas-assembly-unit-" ".S" in
                c_artifacts := unit_path :: !c_artifacts;
                let* texts =
                  Result_list.map
                    (fun (header : C_import.header) ->
                      let span = header.C_import.span in
                      match header.C_import.spelling with
                      | Ast.C_fragment fragment ->
                          Ok
                            (Printf.sprintf "#line %d %S\n%s\n" (span.Span.line + 1)
                               source fragment.text)
                      | Ast.C_quoted path_spelling ->
                          let path =
                            if Filename.is_relative path_spelling then
                              Filename.concat (Filename.dirname source) path_spelling
                            else path_spelling
                          in
                          let* text =
                            match read_file path with
                            | Ok text -> Ok text
                            | Error message ->
                                let message = path_error_detail path message in
                                Error
                                  [
                                    Diag.error span
                                      ("cannot read assembly input `" ^ path_spelling
                                     ^ "`: " ^ message);
                                  ]
                          in
                          if Filename.check_suffix path ".s" then
                            Ok (Printf.sprintf ".include %S\n" path)
                          else Ok (Printf.sprintf "#line 1 %S\n%s\n" path text)
                      | Ast.C_system _ ->
                          Error
                            [
                              Diag.error span
                                "assembly unit requires a quoted path or container";
                            ])
                    headers
                in
                write_file unit_path (String.concat "" texts);
                c_units :=
                  {
                    source;
                    stable_name = "";
                    unit_path;
                    assembly =
                      Some
                        (if
                           List.for_all
                             (fun (header : C_import.header) ->
                               match header.spelling with
                               | Ast.C_quoted path -> Filename.check_suffix path ".s"
                               | _ -> false)
                             headers
                         then "assembler"
                         else "assembler-with-cpp");
                    use_span = (List.hd headers).C_import.span;
                    has_fragments = true;
                    static_functions = [];
                    c_names = [];
                    c_flags = config.Cli.c_flags;
                  }
                  :: !c_units;
                Ok ())
              assembly_imports
          in
          let* imported =
            C_import.merge_imports (List.rev !imported)
            |> C_import.reconcile_source
                 ~container_mismatch_to_clang:(config.Cli.emit <> Cli.Header)
                 program.items
          in
          let imported =
            C_import.prune_unreferenced_records ~referenced_names imported
          in
          Option.iter
            (fun index ->
              let origins = List.filter_map manifest_origin imported.manifest in
              Sema_index.set_c_origins index origins;
              Sema_index.set_c_enum_owners index imported.enum_owners;
              Sema_index.set_c_import index ~unsupported:imported.unsupported
                ~aliases:
                  (List.map
                     (fun (name, ty) -> (name, Ast.type_name ty))
                     imported.aliases)
                ~nonnull:imported.nonnull_parameters
                ~strings:imported.c_string_parameters
                ~alloc:imported.alloc_size_parameters
                ~parameter_names:imported.parameter_names ~records:imported.record_calls)
            index;
          let program = { Ast.items = program.items @ imported.items } in
          let* () = ast_budget (Ast.check_expanded_nodes ~limits program) in
          let specializations = Sema_specialization.create () in
          let* hir =
            match
              Sema.check ?index ~limits ~all_errors:config.Cli.all_errors
                ~c_aliases:imported.aliases ~c_unsupported:imported.unsupported
                ~c_nonnull_parameters:imported.nonnull_parameters
                ~c_string_parameters:imported.c_string_parameters
                ~c_record_calls:imported.record_calls
                ~c_alloc_size_parameters:imported.alloc_size_parameters
                ~c_records:imported.record_types ~specializations program
            with
            | Ok hir -> Ok hir
            | Error diagnostics -> Error (add_include_chains chains diagnostics)
          in
          match recovery with
          | Some _ -> Ok None
          | None ->
              let records =
                List.map
                  (fun (name, spelling, origin) ->
                    let required_header =
                      List.find_map
                        (fun (source, headers, _) ->
                          List.find_map
                            (fun (h : C_import.header) ->
                              match h.spelling with
                              | Ast.C_quoted path ->
                                  let path =
                                    if Filename.is_relative path then
                                      Filename.concat (Filename.dirname source) path
                                    else path
                                  in
                                  if origin = Some path then
                                    Some (C_import.quoted_include path)
                                  else None
                              | Ast.C_system path ->
                                  if
                                    Option.fold ~none:false
                                      ~some:(fun file ->
                                        String.ends_with ~suffix:path file)
                                      origin
                                  then Some ("#include <" ^ path ^ ">\n")
                                  else None
                              | Ast.C_fragment _ -> None)
                            headers)
                        c_imports
                    in
                    C_exports.{ name; spelling; header = required_header })
                  imported.records
              in
              let declarations, declaration_headers, declaration_errors =
                C_exports.declarations
                  ~reserved:(List.concat_map (fun unit -> unit.c_names) !c_units)
                  ~specializations records hir
              in
              let declaration_headers =
                let candidates =
                  List.concat_map
                    (fun (source, headers, _) ->
                      List.filter_map
                        (fun (header : C_import.header) ->
                          match header.spelling with
                          | Ast.C_quoted path ->
                              let path =
                                if Filename.is_relative path then
                                  Filename.concat (Filename.dirname source) path
                                else path
                              in
                              Some (C_import.quoted_include path)
                          | Ast.C_system path -> Some ("#include <" ^ path ^ ">\n")
                          | Ast.C_fragment _ -> None)
                        headers)
                    c_imports
                in
                let ordered =
                  List.filter
                    (fun header -> List.mem header candidates)
                    declaration_headers
                  |> List.fold_left
                       (fun ordered header ->
                         if List.mem header ordered then ordered
                         else ordered @ [ header ])
                       []
                in
                ordered
                @ List.filter
                    (fun header -> not (List.mem header ordered))
                    declaration_headers
              in
              let* () =
                match (config.Cli.emit, declaration_errors) with
                | Cli.Header, message :: _ ->
                    Error [ c_export_diagnostic program message ]
                | _ -> Ok ()
              in
              let* ir =
                Lower.lower
                  ~sanitize_address:(List.mem "address" config.Cli.sanitizers)
                  ~intrinsic_masked_loads:(config.Cli.target = Cli.Force_native)
                  hir
              in
              let* ir = apply_no_inline config ir in
              let* ir, adapters, redirect =
                add_static_adapters (List.rev !c_units) referenced_names ir
              in
              let imported =
                {
                  imported with
                  manifest =
                    imported.manifest @ List.map C_import.adapter_manifest adapters;
                }
              in
              let* () =
                if config.Cli.keep && c_imports <> [] then (
                  let base = Filename.basename config.Cli.input in
                  let name =
                    try Filename.chop_extension base with Invalid_argument _ -> base
                  in
                  let path =
                    Filename.temp_file
                      ~temp_dir:(Filename.get_temp_dir_name ())
                      (name ^ ".bindings-") ".txt"
                  in
                  write_file path (C_import.manifest_text imported);
                  prerr_endline ("fas: kept C bindings: " ^ path));
                Ok ()
              in
              let* () = ir_budget program (Ir.check_lowered_nodes ~limits ir) in
              let* () = ir_budget program (Ir.check_stack_scratch_bytes ~limits ir) in
              let* c_objects =
                compile_c_units ~pgo config cc (List.rev !c_units) adapters
                  (C_exports.includes declarations)
                  declaration_headers declarations c_artifacts
              in
              succeeded := true;
              Ok
                (Some
                   {
                     program;
                     ir;
                     redirect;
                     adapters;
                     c_objects;
                     declaration_headers;
                     declarations;
                     artifacts = !c_artifacts;
                     link_pkgs = List.concat_map (fun (_, _, pkgs) -> pkgs) c_imports;
                     keep = config.Cli.keep;
                   })))

let analyze_with ?overlay ?cache ?index ?recovery ?edits config =
  let* config = resolve_target config in
  try analyze_resolved ?overlay ?cache ?index ?recovery ?edits config with
  | Sys_error message ->
      Error [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ]
  | Unix.Unix_error (code, operation, argument) ->
      Error
        [
          Diag.error Span.synthetic
            (Printf.sprintf "backend %s(%s) failed: %s" operation argument
               (Unix.error_message code));
        ]

let complete = function
  | Ok (Some checked) -> Ok checked
  | Ok None ->
      Error
        [ Diag.error Span.synthetic "internal error: analysis stopped before lowering" ]
  | Error diagnostics -> Error diagnostics

let analyze ?overlay ?cache ?index config =
  complete (analyze_with ?overlay ?cache ?index config)

let check ?overlay ?cache ?index config =
  match analyze ?overlay ?cache ?index config with
  | Error diagnostics -> Error diagnostics
  | Ok checked ->
      let kept = ref false in
      Fun.protect
        ~finally:(fun () -> if not !kept then release checked)
        (fun () ->
          match Ir.render_debug_bounded ~limits checked.ir with
          | Ok _ ->
              kept := true;
              Ok checked
          | Error message -> Error [ Diag.error Span.synthetic message ])

let mentions needle text =
  let size = String.length needle and total = String.length text in
  let rec at index =
    index + size <= total && (String.sub text index size = needle || at (index + 1))
  in
  at 0

let names_header file name = file = name || String.ends_with ~suffix:("/" ^ name) file

let culprit_item canonical primary = function
  | Ast.Pkg { span; _ } when within span primary -> Some (Drop_c_unit canonical)
  | Ast.Use { c_header = Some _; span; _ } when within span primary ->
      Some (Drop_c_unit canonical)
  | Ast.Use _ -> None
  | Ast.Func
      {
        body = Ast.Statements _;
        linkage = Ast.Internal;
        generic_params = [];
        extent;
        body_span;
        _;
      }
    when within extent primary ->
      Some (Stub_fn (canonical, body_span))
  | item when within (item_extent item) primary ->
      Some (Remove_item (canonical, item_extent item))
  | _ -> None

let culprit recovery diagnostics =
  match List.find_opt (fun d -> d.Diag.severity = Diag.Error) diagnostics with
  | None -> None
  | Some { Diag.primary; message; _ } -> (
      if
        Span.is_synthetic primary
        || mentions "internal error:" message
        || mentions "budget" message
      then None
      else
        let file = primary.Span.file in
        let failed =
          List.find_map
            (fun failure ->
              if
                failure.target = Some file
                || (failure.use_span = primary && failure.parent_display = file)
              then Some (Drop_use (failure.parent_canonical, failure.use_span))
              else None)
            recovery.failures
        in
        match failed with
        | Some _ -> failed
        | None -> (
            let in_file =
              match
                List.find_opt (fun (display, _, _) -> display = file) recovery.files
              with
              | None -> None
              | Some (_, canonical, items) ->
                  List.find_map (culprit_item canonical primary) items
            in
            match in_file with
            | Some _ -> in_file
            | None ->
                List.find_map
                  (fun (canonical, headers, _) ->
                    let named (header : C_import.header) =
                      match header.C_import.spelling with
                      | Ast.C_quoted name | Ast.C_system name -> names_header file name
                      | Ast.C_fragment _ -> false
                    in
                    if List.exists named headers then Some (Drop_c_unit canonical)
                    else None)
                  recovery.c_units))

let gaps_of edits =
  List.filter_map
    (function
      | Remove_item (file, span) | Stub_fn (file, span) ->
          Some
            {
              file;
              start_offset = span.Span.start_offset;
              end_offset = span.Span.end_offset;
            }
      | Drop_use _ | Drop_c_unit _ -> None)
    edits

let recovery_runs = 64
let recovery_seconds = 2.0

let recover ?overlay ?cache config =
  let started = Unix.gettimeofday () in
  let rec attempt runs edits =
    let recovery = new_recovery () in
    recovery.blank_errors <- true;
    let index = Sema_index.create () in
    match analyze_with ?overlay ?cache ~index ~recovery ~edits config with
    | Ok _ -> Ok { recovered_index = index; gaps = gaps_of edits @ recovery.blanked }
    | Error diagnostics -> (
        if
          runs + 1 >= recovery_runs
          || Unix.gettimeofday () -. started >= recovery_seconds
        then Error diagnostics
        else
          match culprit recovery diagnostics with
          | Some edit when not (List.mem edit edits) ->
              attempt (runs + 1) (edits @ [ edit ])
          | _ -> Error diagnostics)
  in
  attempt 0 []

let run_unprotected ?header_output ?remarks ?llc_remarks ?llvm_text ?c_debug
    ?(link_cache = new_link_cache ()) ~pgo config =
  match complete (analyze_resolved ?pgo config) with
  | Error diagnostics -> Error diagnostics
  | Ok checked ->
      Fun.protect
        ~finally:(fun () -> release checked)
        (fun () ->
          let {
            program;
            ir;
            redirect;
            adapters;
            c_objects;
            declaration_headers;
            declarations;
            link_pkgs;
            _;
          } =
            checked
          in
          match config.emit with
          | Cli.Header ->
              let output = Option.value ~default:config.Cli.output header_output in
              let name =
                if config.Cli.output_explicit then Filename.basename output
                else Filename.remove_extension (Filename.basename config.Cli.input)
              in
              let headers =
                List.map
                  (fun line ->
                    if String.starts_with ~prefix:"#include \"" line then
                      let path = C_import.quoted_include_path line in
                      C_import.quoted_include
                        (C_exports.relative_path
                           (if config.Cli.output_explicit then Filename.dirname output
                            else Sys.getcwd ())
                           path)
                    else line)
                  declaration_headers
              in
              emit_text config (C_exports.header ~name ~headers declarations)
          | Cli.Ir_json ->
              emit_text config
                (Ir_json.render ~redirect:Fun.id
                   ~c_adapters:
                     (List.map
                        (fun (adapter : C_import.adapter) ->
                          (adapter.c_name, adapter.symbol))
                        adapters)
                   ir)
          | Cli.Ir -> (
              match Ir.render_debug_bounded ~limits ir with
              | Ok text -> emit_text config text
              | Error message -> Error [ Diag.error Span.synthetic message ])
          | Cli.Llvm ->
              let* text =
                render_ir ~redirect
                  ~sanitize_address:(List.mem "address" config.Cli.sanitizers)
                  ~debug_info:config.Cli.debug ?target:(target_attributes config) ir
              in
              let opt, _, _ = tools () in
              let path = Filename.temp_file "fas-verify-" ".ll" in
              if config.Cli.keep then (
                prerr_endline ("fas: kept intermediate: " ^ path);
                prerr_endline
                  (Printf.sprintf "fas: tool FAS_OPT=%s; opt passes=verify" opt));
              Fun.protect
                ~finally:(fun () -> if not config.Cli.keep then remove path)
                (fun () ->
                  write_file path text;
                  let* () = verify_llvm opt path in
                  emit_text config text)
          | Cli.Optimized_llvm -> emit_optimized_llvm ~pgo config program ir redirect
          | Cli.Asm | Cli.Obj ->
              emit_tools ?remarks ?llc_remarks ?llvm_text ?c_debug ~pgo config program
                ir c_objects [] redirect
          | Cli.Executable ->
              let* () =
                if
                  config.Cli.link_inputs = [] && c_objects = [] && link_pkgs = []
                  && not
                       (List.exists (fun (f : Ir.func) -> f.name = "main") ir.Ir.funcs)
                then
                  Error
                    [
                      Diag.error Span.synthetic
                        (Printf.sprintf "`%s` has no `fn main`" config.Cli.input);
                    ]
                else Ok ()
              in
              let* link_flags =
                pkg_link_flags ~keep:config.Cli.keep ~cache:link_cache link_pkgs
              in
              emit_tools ?remarks ?llc_remarks ?llvm_text ?c_debug ~pgo config program
                ir c_objects link_flags redirect)

let run_unstaged ?header_output ?remarks ?llc_remarks ?llvm_text ?c_debug ?link_cache
    ~pgo config =
  try
    run_unprotected ?header_output ?remarks ?llc_remarks ?llvm_text ?c_debug ?link_cache
      ~pgo config
  with
  | Sys_error message ->
      Error [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ]
  | Unix.Unix_error (code, operation, argument) ->
      Error
        [
          Diag.error Span.synthetic
            (Printf.sprintf "backend %s(%s) failed: %s" operation argument
               (Unix.error_message code));
        ]

let same_as_input config =
  canonical_path config.Cli.input = canonical_path config.Cli.output

let run_resolved ?remarks ?llc_remarks ?llvm_text ?c_debug ?link_cache ~pgo config =
  if not config.Cli.output_explicit then
    run_unstaged ?remarks ?llc_remarks ?llvm_text ?c_debug ?link_cache ~pgo config
  else if same_as_input config then
    Error [ Diag.error Span.synthetic "output path must differ from the input file" ]
  else
    match
      run_unstaged ?remarks ?llc_remarks ?llvm_text ?c_debug ?link_cache ~pgo config
    with
    | Ok _ as result -> result
    | Error _ as result ->
        if not (is_non_regular_output config.Cli.output) then remove config.Cli.output;
        result

let output_is_directory config =
  config.Cli.output_explicit
  && Sys.file_exists config.Cli.output
  && Sys.is_directory config.Cli.output

let run ?remarks ?llc_remarks ?llvm_text ?c_debug ?link_cache ?(instrument = false)
    config =
  if output_is_directory config then
    Error
      [
        Diag.error Span.synthetic
          (Printf.sprintf "output path `%s` is a directory" config.Cli.output);
      ]
  else
    let with_pgo f =
      if instrument then f config (Some Instrument) else with_profile config f
    in
    with_pgo (fun config pgo ->
        let* config = resolve_target config in
        run_resolved ?remarks ?llc_remarks ?llvm_text ?c_debug ?link_cache ~pgo config)
