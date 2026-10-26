type document = { uri : string; text : string }

type snapshot = {
  index : Sema_index.t;
  texts : (string, string) Hashtbl.t;
  gaps : Driver.gap list;
}

type summary = { uses : string list; has_main : bool }

exception Invalid of string

type state = {
  options : Cli.lsp_options;
  cache : C_import.cache;
  documents : (string, document) Hashtbl.t;
  contributions : (string, (string * string * Json.t) list) Hashtbl.t;
  loads : (string, string list) Hashtbl.t;
  snapshots : (string, snapshot) Hashtbl.t;
  good : (string, snapshot) Hashtbl.t;
  c_roots : (string, Sema_index.t) Hashtbl.t;
  mutable folders : string list;
  mutable dynamic_watch : bool;
  scanned : (string, summary) Hashtbl.t;
  open_summaries : (string, summary) Hashtbl.t;
  routes : (string, string) Hashtbl.t;
  mutable initialized : bool;
  mutable shut_down : bool;
  mutable exit_code : int option;
  mutable due : bool;
  mutable dirty : string list;
  mutable clear : string list;
}

let create options =
  {
    options;
    cache = C_import.create_cache ();
    documents = Hashtbl.create 8;
    contributions = Hashtbl.create 8;
    loads = Hashtbl.create 8;
    snapshots = Hashtbl.create 8;
    good = Hashtbl.create 8;
    c_roots = Hashtbl.create 8;
    folders = [];
    dynamic_watch = false;
    scanned = Hashtbl.create 64;
    open_summaries = Hashtbl.create 8;
    routes = Hashtbl.create 8;
    initialized = false;
    shut_down = false;
    exit_code = None;
    due = false;
    dirty = [];
    clear = [];
  }

let send json = Lsp_frame.write_message stdout (Json.to_string json)
let envelope fields = Json.Obj (("jsonrpc", Json.Str "2.0") :: fields)
let respond id result = send (envelope [ ("id", id); ("result", result) ])

let fail id code message =
  send
    (envelope
       [
         ("id", id);
         ( "error",
           Json.Obj
             [ ("code", Json.Num (string_of_int code)); ("message", Json.Str message) ]
         );
       ])

let notify meth params =
  send (envelope [ ("method", Json.Str meth); ("params", params) ])

let log message =
  notify "window/logMessage"
    (Json.Obj [ ("type", Json.Num "1"); ("message", Json.Str message) ])

let member key = function Json.Obj fields -> List.assoc_opt key fields | _ -> None
let child key value = match member key value with Some v -> v | None -> Json.Null

let string_member key value =
  match member key value with Some (Json.Str s) -> Some s | _ -> None

let hex_value c =
  match c with
  | '0' .. '9' -> Some (Char.code c - 48)
  | 'a' .. 'f' -> Some (Char.code c - 87)
  | 'A' .. 'F' -> Some (Char.code c - 55)
  | _ -> None

let percent_decode text =
  let n = String.length text in
  let buffer = Buffer.create n in
  let rec go i =
    if i >= n then Some (Buffer.contents buffer)
    else if text.[i] <> '%' then (
      Buffer.add_char buffer text.[i];
      go (i + 1))
    else if i + 2 >= n then None
    else
      match (hex_value text.[i + 1], hex_value text.[i + 2]) with
      | Some high, Some low ->
          Buffer.add_char buffer (Char.chr ((high * 16) + low));
          go (i + 3)
      | _ -> None
  in
  go 0

let has_drive path at =
  String.length path > at + 1
  && path.[at + 1] = ':'
  && match path.[at] with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

let host_path path =
  if Sys.win32 && path.[0] = '/' && has_drive path 1 then
    String.map
      (function '/' -> '\\' | c -> c)
      (String.sub path 1 (String.length path - 1))
  else path

let uri_to_path uri =
  let prefix = "file://" in
  let plen = String.length prefix in
  if String.length uri > plen && String.sub uri 0 plen = prefix then
    let rest = String.sub uri plen (String.length uri - plen) in
    if rest.[0] = '/' then Option.map host_path (percent_decode rest) else None
  else None

let path_to_uri path =
  let buffer = Buffer.create (String.length path + 8) in
  Buffer.add_string buffer "file://";
  let path =
    if Sys.win32 then
      let path = String.map (function '\\' -> '/' | c -> c) path in
      if has_drive path 0 then (
        Buffer.add_char buffer '/';
        Buffer.add_string buffer (String.sub path 0 2);
        String.sub path 2 (String.length path - 2))
      else path
    else path
  in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' | '/' ->
          Buffer.add_char buffer c
      | _ -> Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code c)))
    path;
  Buffer.contents buffer

let key_of_uri uri =
  match uri_to_path uri with
  | Some path when Filename.check_suffix path ".fas" ->
      Some (Driver.canonical_path path)
  | _ -> None

let read_text path =
  try In_channel.with_open_bin path In_channel.input_all with Sys_error _ -> ""

let overlay state path =
  match Hashtbl.find_opt state.documents path with
  | Some document -> Some document.text
  | None -> None

let disk_text path =
  try
    if Sys.is_directory path then None
    else Some (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error _ -> None

let current_text state key =
  match Hashtbl.find_opt state.documents key with
  | Some document -> Some document.text
  | None -> disk_text key

let config_for state key : Cli.t =
  {
    Cli.input = key;
    output = key;
    output_explicit = false;
    target_explicit = false;
    os = state.options.lsp_os;
    emit = Cli.Ir;
    keep = false;
    optimization = 2;
    debug = false;
    no_inline_functions = [];
    sanitizers = [];
    link_inputs = [];
    c_flags = state.options.lsp_c_flags;
    diagnostics = Cli.Text;
    target_cpu = None;
    tune_cpu = None;
    target = state.options.lsp_target;
    target_features = None;
    profile = None;
    all_errors = false;
    align_loops = None;
    align_functions = None;
    tune_options = [];
  }

let lsp_message (diag : Diag.t) =
  let help = match diag.help with None -> [] | Some text -> [ "help: " ^ text ] in
  let notes = List.map (fun note -> "note: " ^ note) diag.notes in
  String.concat "\n" ((diag.message :: help) @ notes)

let position_json (line, character) =
  Json.Obj
    [
      ("line", Json.Num (string_of_int line));
      ("character", Json.Num (string_of_int character));
    ]

let diagnostic_json text (span : Span.t) message =
  let zero = (0, 0) in
  let start, finish =
    if Span.is_synthetic span then (zero, zero)
    else
      ( Lsp_position.position_of_offset text span.start_offset,
        Lsp_position.position_of_offset text span.end_offset )
  in
  Json.Obj
    [
      ( "range",
        Json.Obj [ ("start", position_json start); ("end", position_json finish) ] );
      ("severity", Json.Num "1");
      ("source", Json.Str "fas");
      ("message", Json.Str message);
    ]

let uri_of_key state key =
  match Hashtbl.find_opt state.documents key with
  | Some document -> document.uri
  | None -> path_to_uri key

let registration_id = "fas-roots-watch"
let workspace_limit = 2000
let route_of state key = Option.value ~default:key (Hashtbl.find_opt state.routes key)

let summarise key text =
  match Parser.parse ~limits:Limits.default (Source.create ~file:key ~text) with
  | Error _ -> { uses = []; has_main = false }
  | Ok program ->
      let uses, has_main =
        List.fold_left
          (fun (uses, has_main) item ->
            match item with
            | Ast.Use { path; c_header = None; _ }
              when Driver.use_path_error path = None ->
                (snd (Driver.use_target key key path) :: uses, has_main)
            | Ast.Func { name = "main"; _ } -> (uses, true)
            | _ -> (uses, has_main))
          ([], false) program.Ast.items
      in
      { uses = List.sort_uniq String.compare uses; has_main }

let skipped_directory name =
  name.[0] = '.' || List.mem name [ "build"; "target"; "node_modules" ]

let workspace_files folders =
  let found = ref [] in
  let seen = Hashtbl.create 64 in
  let rec walk dir =
    let entries = try Sys.readdir dir with Sys_error _ -> [||] in
    Array.sort String.compare entries;
    Array.iter
      (fun name ->
        let path = Filename.concat dir name in
        if Hashtbl.length seen < workspace_limit then
          match Unix.lstat path with
          | { Unix.st_kind = Unix.S_DIR; _ } ->
              if not (skipped_directory name) then walk path
          | { Unix.st_kind = Unix.S_REG; _ }
            when Filename.check_suffix name ".fas" && not (Hashtbl.mem seen path) ->
              Hashtbl.replace seen path ();
              found := path :: !found
          | _ -> ()
          | exception Unix.Unix_error _ -> ())
      entries
  in
  List.iter walk folders;
  List.rev !found

let scan state =
  Hashtbl.reset state.scanned;
  let folders =
    List.sort_uniq String.compare (List.map Driver.canonical_path state.folders)
  in
  List.iter
    (fun path ->
      let key = Driver.canonical_path path in
      match disk_text key with
      | Some text -> Hashtbl.replace state.scanned key (summarise key text)
      | None -> ())
    (workspace_files folders)

let summary_of state key =
  match Hashtbl.find_opt state.open_summaries key with
  | Some summary -> Some summary
  | None -> (
      match Hashtbl.find_opt state.scanned key with
      | Some summary -> Some summary
      | None -> Option.map (summarise key) (disk_text key))

let candidates state =
  let keys table acc = Hashtbl.fold (fun key _ acc -> key :: acc) table acc in
  List.sort_uniq String.compare (keys state.scanned (keys state.documents []))

let reach state start =
  let seen = Hashtbl.create 16 in
  let rec visit = function
    | [] -> ()
    | key :: rest when Hashtbl.mem seen key -> visit rest
    | key :: rest ->
        Hashtbl.replace seen key ();
        let next = match summary_of state key with Some s -> s.uses | None -> [] in
        visit (next @ rest)
  in
  visit [ start ];
  Hashtbl.fold (fun key () acc -> key :: acc) seen []

let rank state root =
  let has_main =
    match summary_of state root with Some s -> s.has_main | None -> false
  in
  (not has_main, String.length root, root)

let compute_routes state =
  let keys = candidates state in
  let loaders = Hashtbl.create 16 in
  List.iter
    (fun root ->
      List.iter
        (fun key ->
          if key <> root then
            Hashtbl.replace loaders key
              (root :: Option.value ~default:[] (Hashtbl.find_opt loaders key)))
        (reach state root))
    keys;
  let keys =
    List.sort_uniq String.compare
      (Hashtbl.fold (fun key _ acc -> key :: acc) loaders keys)
  in
  Hashtbl.reset state.routes;
  List.iter
    (fun key ->
      let root =
        match Hashtbl.find_opt loaders key with
        | None | Some [] -> key
        | Some roots ->
            List.hd (List.sort (fun a b -> compare (rank state a) (rank state b)) roots)
      in
      Hashtbl.replace state.routes key root)
    keys

let drop_root state key =
  Hashtbl.remove state.contributions key;
  Hashtbl.remove state.loads key;
  Hashtbl.remove state.snapshots key;
  Hashtbl.remove state.good key;
  Hashtbl.remove state.c_roots key

let diagnostic_target state root_key root_uri (span : Span.t) =
  if Span.is_synthetic span then (root_key, root_uri, "")
  else
    let canonical = Driver.canonical_path span.file in
    match Hashtbl.find_opt state.documents canonical with
    | Some document -> (canonical, document.uri, document.text)
    | None ->
        if Sys.file_exists canonical && not (Sys.is_directory canonical) then
          (canonical, path_to_uri canonical, read_text canonical)
        else (root_key, root_uri, "")

let contributions_of state root_key root_uri diags =
  List.map
    (fun (diag : Diag.t) ->
      let key, uri, text = diagnostic_target state root_key root_uri diag.primary in
      (key, uri, diagnostic_json text diag.primary (lsp_message diag)))
    diags

let visible_to state root key =
  match Hashtbl.find_opt state.routes key with
  | Some owner -> owner = root
  | None -> true

let publish state clear uris =
  let roots =
    List.sort
      (fun (a, _) (b, _) -> String.compare a b)
      (Hashtbl.fold (fun key value acc -> (key, value) :: acc) state.contributions [])
  in
  let diagnostics_for uri =
    List.fold_left
      (fun acc (root, fresh) ->
        List.fold_left
          (fun acc (key, target_uri, diagnostic) ->
            if
              target_uri = uri && visible_to state root key
              && not (List.mem diagnostic acc)
            then diagnostic :: acc
            else acc)
          acc fresh)
      [] roots
    |> List.rev
  in
  List.iter
    (fun uri ->
      notify "textDocument/publishDiagnostics"
        (Json.Obj
           [ ("uri", Json.Str uri); ("diagnostics", Json.Arr (diagnostics_for uri)) ]))
    (List.sort_uniq String.compare (clear @ uris))

let previous_uris state key =
  match Hashtbl.find_opt state.contributions key with
  | Some fresh -> List.map (fun (_, uri, _) -> uri) fresh
  | None -> []

let analyze_root state key =
  match current_text state key with
  | None ->
      let previous = previous_uris state key in
      drop_root state key;
      uri_of_key state key :: previous
  | Some text ->
      if Sys.getenv_opt "FAS_LSP_TRACE" = Some "1" then log ("analyse " ^ key);
      let root_uri = uri_of_key state key in
      let previous = previous_uris state key in
      let loaded = ref [ key ] in
      let texts = Hashtbl.create 8 in
      let observe path =
        match Hashtbl.find_opt texts path with
        | Some text -> Some text
        | None ->
            loaded := path :: !loaded;
            let text =
              match overlay state path with
              | Some text -> Some text
              | None -> disk_text path
            in
            Option.iter (Hashtbl.replace texts path) text;
            text
      in
      let index = Sema_index.create () in
      let diags, index =
        match
          Driver.check ~overlay:observe ~cache:state.cache ~index (config_for state key)
        with
        | Ok checked ->
            Driver.release checked;
            Hashtbl.replace texts key text;
            let snapshot = { index; texts; gaps = [] } in
            Hashtbl.replace state.good key snapshot;
            Hashtbl.replace state.snapshots key snapshot;
            ([], index)
        | Error diags -> (
            match
              Driver.recover ~overlay:observe ~cache:state.cache (config_for state key)
            with
            | Ok recovered ->
                Hashtbl.replace texts key text;
                let index = recovered.Driver.recovered_index in
                Hashtbl.replace state.snapshots key
                  { index; texts; gaps = recovered.Driver.gaps };
                (diags, index)
            | Error _ -> (diags, index))
      in
      if Sema_index.has_c_import index then Hashtbl.replace state.c_roots key index
      else Hashtbl.remove state.c_roots key;
      let fresh = contributions_of state key root_uri diags in
      Hashtbl.replace state.contributions key fresh;
      Hashtbl.replace state.loads key (List.sort_uniq String.compare !loaded);
      (root_uri :: previous) @ List.map (fun (_, uri, _) -> uri) fresh

let dependants state path =
  let roots =
    Hashtbl.fold
      (fun root loaded acc ->
        if root = path || List.mem path loaded then root :: acc else acc)
      state.loads []
  in
  if Hashtbl.mem state.documents path then path :: roots else roots

let refresh state clear dirty =
  let changed =
    List.filter_map
      (fun key ->
        Option.map
          (fun (document : document) -> document.uri)
          (Hashtbl.find_opt state.documents key))
      dirty
  in
  let before = Hashtbl.copy state.routes in
  compute_routes state;
  let moved =
    Hashtbl.fold
      (fun key root acc ->
        if Hashtbl.find_opt before key <> Some root then key :: acc else acc)
      state.routes []
  in
  let needed =
    List.sort_uniq String.compare
      (Hashtbl.fold (fun key _ acc -> route_of state key :: acc) state.documents [])
  in
  let analysed = Hashtbl.fold (fun key _ acc -> key :: acc) state.contributions [] in
  let stale = List.filter (fun key -> not (List.mem key needed)) analysed in
  let stale_uris = List.concat_map (previous_uris state) stale in
  List.iter (drop_root state) stale;
  let pending =
    List.filter
      (fun key -> (not (Hashtbl.mem state.contributions key)) || List.mem key dirty)
      needed
  in
  let uris = List.concat_map (analyze_root state) pending in
  let moved_uris = List.map (uri_of_key state) moved in
  publish state clear (changed @ stale_uris @ moved_uris @ uris)

let defer state clear dirty =
  state.due <- true;
  state.clear <- clear @ state.clear;
  state.dirty <- dirty @ state.dirty

let flush state =
  if state.due then (
    let clear = state.clear and dirty = state.dirty in
    state.due <- false;
    state.clear <- [];
    state.dirty <- [];
    try refresh state clear dirty with exn -> log (Printexc.to_string exn))

let open_document state params =
  let document = child "textDocument" params in
  match (string_member "uri" document, string_member "text" document) with
  | Some uri, Some text -> (
      match key_of_uri uri with
      | Some key ->
          Hashtbl.replace state.documents key { uri; text };
          Hashtbl.replace state.open_summaries key (summarise key text);
          defer state [] (dependants state key)
      | None -> ())
  | _ -> ()

let change_document state params =
  let text =
    match member "contentChanges" params with
    | Some (Json.Arr changes) ->
        List.fold_left
          (fun acc change ->
            match string_member "text" change with
            | Some text -> Some text
            | None -> acc)
          None changes
    | _ -> None
  in
  match (string_member "uri" (child "textDocument" params), text) with
  | Some uri, Some text -> (
      match key_of_uri uri with
      | Some key when Hashtbl.mem state.documents key ->
          let document = Hashtbl.find state.documents key in
          Hashtbl.replace state.documents key { document with text };
          let dirty = dependants state key in
          let uses_before =
            Option.map
              (fun (summary : summary) -> summary.uses)
              (Hashtbl.find_opt state.open_summaries key)
          in
          let summary = summarise key text in
          Hashtbl.replace state.open_summaries key summary;
          if uses_before <> Some summary.uses then scan state;
          defer state [] dirty
      | _ -> ())
  | _ -> ()

let save_document state params =
  match string_member "uri" (child "textDocument" params) with
  | Some uri -> (
      match key_of_uri uri with
      | Some key -> defer state [] (dependants state key)
      | None -> ())
  | None -> ()

let close_document state params =
  match string_member "uri" (child "textDocument" params) with
  | Some uri -> (
      match key_of_uri uri with
      | Some key -> (
          match Hashtbl.find_opt state.documents key with
          | None -> ()
          | Some document ->
              let dirty = dependants state key in
              Hashtbl.remove state.documents key;
              Hashtbl.remove state.open_summaries key;
              (if Hashtbl.mem state.scanned key then
                 match disk_text key with
                 | Some text -> Hashtbl.replace state.scanned key (summarise key text)
                 | None -> Hashtbl.remove state.scanned key);
              defer state [ document.uri ] dirty)
      | None -> ())
  | None -> ()

let workspace_folders params =
  let from_uri uri = match uri_to_path uri with Some path -> [ path ] | None -> [] in
  match member "workspaceFolders" params with
  | Some (Json.Arr (_ :: _ as folders)) ->
      List.concat_map
        (fun folder ->
          match string_member "uri" folder with Some uri -> from_uri uri | None -> [])
        folders
  | _ -> (
      match string_member "rootUri" params with Some uri -> from_uri uri | None -> [])

let dynamic_watch params =
  member "dynamicRegistration"
    (child "didChangeWatchedFiles" (child "workspace" (child "capabilities" params)))
  = Some (Json.Bool true)

let register_watchers state =
  if state.dynamic_watch then
    send
      (envelope
         [
           ("id", Json.Str registration_id);
           ("method", Json.Str "client/registerCapability");
           ( "params",
             Json.Obj
               [
                 ( "registrations",
                   Json.Arr
                     [
                       Json.Obj
                         [
                           ("id", Json.Str registration_id);
                           ("method", Json.Str "workspace/didChangeWatchedFiles");
                           ( "registerOptions",
                             Json.Obj
                               [
                                 ( "watchers",
                                   Json.Arr
                                     [
                                       Json.Obj [ ("globPattern", Json.Str "**/*.fas") ];
                                     ] );
                               ] );
                         ];
                     ] );
               ] );
         ])

let on_initialized state =
  scan state;
  register_watchers state

let on_watched_files state =
  scan state;
  defer state [] (Hashtbl.fold (fun key _ acc -> key :: acc) state.contributions [])

let invalid message = raise (Invalid message)

let document_uri params =
  match string_member "uri" (child "textDocument" params) with
  | Some uri -> uri
  | None -> invalid "params.textDocument.uri must be a string"

let index_member name value =
  match member name value with
  | Some (Json.Num text) -> (
      match int_of_string_opt text with Some n when n >= 0 -> Some n | _ -> None)
  | _ -> None

let request_point params =
  let uri = document_uri params in
  let position = child "position" params in
  match (index_member "line" position, index_member "character" position) with
  | Some line, Some character -> (uri, line, character)
  | _ -> invalid "params.position must hold non-negative integer line and character"

let include_declaration params =
  match member "includeDeclaration" (child "context" params) with
  | Some (Json.Bool flag) -> flag
  | _ -> invalid "params.context.includeDeclaration must be a boolean"

let snapshot_for state key = Hashtbl.find_opt state.snapshots (route_of state key)

let mapped state snapshot (span : Span.t) =
  let key = Driver.canonical_path span.file in
  match (Hashtbl.find_opt snapshot.texts key, current_text state key) with
  | Some analyzed, Some current -> (
      let map = Lsp_map.create ~analyzed ~current in
      let positions = Lsp_position.index analyzed in
      let first = Lsp_position.position positions span.start_offset in
      let last = Lsp_position.position positions span.end_offset in
      match (Lsp_map.to_current map (fst first), Lsp_map.to_current map (fst last)) with
      | Some first_line, Some last_line ->
          Some (key, (first_line, snd first), (last_line, snd last))
      | _ -> None)
  | _ -> None

let range_json first last =
  Json.Obj [ ("start", position_json first); ("end", position_json last) ]

let locate (snapshot : snapshot) key current line character =
  match Hashtbl.find_opt snapshot.texts key with
  | None -> None
  | Some analyzed -> (
      let map = Lsp_map.create ~analyzed ~current in
      match Lsp_map.to_analyzed map line with
      | None -> None
      | Some analyzed_line ->
          Some (Lsp_position.offset_of_position analyzed (analyzed_line, character)))

let gap_at (snapshot : snapshot) key offset =
  List.find_opt
    (fun (gap : Driver.gap) ->
      gap.Driver.start_offset <= offset
      && offset < gap.Driver.end_offset
      && gap.Driver.file = key)
    snapshot.gaps

let in_gap snapshot key offset = Option.is_some (gap_at snapshot key offset)

let outline_range (d : Sema_index.declaration) =
  match d.Sema_index.kind with
  | Parameter | Local | View | Field | Type_parameter | Const_parameter ->
      d.Sema_index.name_span
  | _ ->
      if Span.is_synthetic d.Sema_index.item_span then d.Sema_index.name_span
      else d.Sema_index.item_span

let gap_of_declaration state snapshot key text source (d : Sema_index.declaration) =
  let gap_of_span span =
    match mapped state source span with
    | Some (_, (line, character), _) -> (
        match locate snapshot key text line character with
        | Some offset -> gap_at snapshot key offset
        | None -> None)
    | None -> None
  in
  let outline = outline_range d in
  let last_byte =
    { outline with Span.start_offset = max 0 (outline.Span.end_offset - 1) }
  in
  match gap_of_span last_byte with
  | Some gap -> Some gap
  | None -> gap_of_span d.Sema_index.name_span

let resolve state uri line character =
  match key_of_uri uri with
  | None -> None
  | Some key -> (
      match (snapshot_for state key, current_text state key) with
      | Some snapshot, Some current -> (
          match locate snapshot key current line character with
          | None -> None
          | Some offset when in_gap snapshot key offset -> (
              match Hashtbl.find_opt state.good (route_of state key) with
              | None -> None
              | Some good -> (
                  match locate good key current line character with
                  | None -> None
                  | Some offset -> Some (good, key, offset)))
          | Some offset -> Some (snapshot, key, offset))
      | _ -> None)

let target (snapshot : snapshot) key offset =
  let spans =
    List.map
      (fun (r : Sema_index.reference) ->
        (r.Sema_index.use_span, r.Sema_index.declaration))
      (Sema_index.references snapshot.index)
    @ List.map
        (fun (d : Sema_index.declaration) -> (d.Sema_index.name_span, d))
        (Sema_index.declarations snapshot.index)
  in
  let spans =
    List.filter
      (fun ((span : Span.t), _) ->
        (not (Span.is_synthetic span)) && Driver.canonical_path span.file = key)
      spans
  in
  let contains ((span : Span.t), _) =
    span.start_offset <= offset && offset < span.end_offset
  in
  let ends_at ((span : Span.t), _) = span.end_offset = offset in
  match List.find_opt contains spans with
  | Some (_, declaration) -> Some declaration
  | None -> Option.map snd (List.find_opt ends_at spans)

let is_word_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

let line_start text line =
  let length = String.length text in
  let rec go i current =
    if current >= line || i >= length then i
    else
      match String.index_from_opt text i '\n' with
      | None -> length
      | Some j -> go (j + 1) (current + 1)
  in
  go 0 1

let line_of_offset text offset =
  let count = ref 1 in
  for i = 0 to min offset (String.length text) - 1 do
    if text.[i] = '\n' then incr count
  done;
  !count

let c_declaration_end text start =
  let length = String.length text in
  let rec block i =
    if i + 1 >= length then length
    else if text.[i] = '*' && text.[i + 1] = '/' then i + 2
    else block (i + 1)
  in
  let rec line i = if i >= length || text.[i] = '\n' then i else line (i + 1) in
  let rec quoted quote i =
    if i >= length then length
    else if text.[i] = '\\' then quoted quote (i + 2)
    else if text.[i] = quote then i + 1
    else quoted quote (i + 1)
  in
  let rec scan i braces parens =
    if i >= length then length
    else
      match text.[i] with
      | '/' when i + 1 < length && text.[i + 1] = '*' ->
          scan (block (i + 2)) braces parens
      | '/' when i + 1 < length && text.[i + 1] = '/' -> scan (line i) braces parens
      | ('"' | '\'') as quote -> scan (quoted quote (i + 1)) braces parens
      | '(' -> scan (i + 1) braces (parens + 1)
      | ')' -> scan (i + 1) braces (parens - 1)
      | '{' -> scan (i + 1) (braces + 1) parens
      | '}' when braces <= 1 -> i + 1
      | '}' -> scan (i + 1) (braces - 1) parens
      | ';' when braces = 0 && parens = 0 -> i + 1
      | _ -> scan (i + 1) braces parens
  in
  scan start 0 0

let normalise_c_text raw =
  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let keep_newlines = String.contains raw '{' in
  let length = String.length raw in
  let buffer = Buffer.create length in
  let rec go i =
    if i < length then
      if is_space raw.[i] then begin
        let j = ref i and newline = ref false in
        while !j < length && is_space raw.[!j] do
          if raw.[!j] = '\n' then newline := true;
          incr j
        done;
        if Buffer.length buffer > 0 && !j < length then
          Buffer.add_string buffer (if keep_newlines && !newline then "\n" else " ");
        go !j
      end
      else begin
        Buffer.add_char buffer raw.[i];
        go (i + 1)
      end
  in
  go 0;
  Buffer.contents buffer

let c_declaration_text text start =
  normalise_c_text (String.sub text start (c_declaration_end text start - start))

let c_field_line text origin name =
  let start = line_start text origin in
  let stop = c_declaration_end text start in
  let length = String.length name in
  let boundary i = i < 0 || i >= String.length text || not (is_word_char text.[i]) in
  let rec find i =
    if i + length > stop then origin
    else if String.sub text i length = name && boundary (i - 1) && boundary (i + length)
    then line_of_offset text i
    else find (i + 1)
  in
  find start

let c_line_of (declaration : Sema_index.declaration) text origin =
  match (declaration.Sema_index.kind, text) with
  | Sema_index.Field, Some text -> c_field_line text origin declaration.Sema_index.name
  | _ -> origin

let c_define_column text line name =
  let length = String.length text in
  let start = line_start text line in
  let stop =
    match String.index_from_opt text start '\n' with Some j -> j | None -> length
  in
  let row = String.sub text start (stop - start) in
  let size = String.length row in
  let blank i = i < size && (row.[i] = ' ' || row.[i] = '\t') in
  let rec skip i = if blank i then skip (i + 1) else i in
  let word_at i word =
    let n = String.length word in
    i + n <= size && String.sub row i n = word
  in
  let hash = skip 0 in
  let keyword = skip (hash + 1) in
  if hash < size && row.[hash] = '#' && word_at keyword "define" && blank (keyword + 6)
  then
    let at = skip (keyword + 6) in
    let after = at + String.length name in
    if word_at at name && (after >= size || not (is_word_char row.[after])) then Some at
    else None
  else None

let c_macro_text text start =
  let length = String.length text in
  let rec logical_end i =
    if i >= length then length
    else if text.[i] = '\n' then
      if i > start && text.[i - 1] = '\\' then logical_end (i + 1) else i
    else logical_end (i + 1)
  in
  String.sub text start (logical_end start - start)
  |> String.map (fun c -> if c = '\t' || c = '\r' || c = '\n' then ' ' else c)
  |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "" && word <> "\\")
  |> String.concat " "

let c_parts ~enumerator (declaration : Sema_index.declaration) =
  match declaration.Sema_index.c_origin with
  | None | Some ("<unknown>", _) -> []
  | Some (header, origin) ->
      let text = disk_text header in
      let line = c_line_of declaration text origin in
      let fence =
        match text with
        | None -> []
        | Some _ when enumerator -> []
        | Some text -> (
            match c_define_column text line declaration.Sema_index.name with
            | Some _ ->
                [ "```c\n" ^ c_macro_text text (line_start text line) ^ "\n```" ]
            | None ->
                let code = c_declaration_text text (line_start text line) in
                if code = "" then [] else [ "```c\n" ^ code ^ "\n```" ])
      in
      fence @ [ Printf.sprintf "%s:%d" header line ]

let c_location header line column =
  if header = "<unknown>" then Json.Null
  else
    let row = max 0 (line - 1) in
    Json.Obj
      [
        ("uri", Json.Str (path_to_uri (Driver.canonical_path header)));
        ("range", range_json (row, column) (row, column));
      ]

let c_declaration_location (declaration : Sema_index.declaration) header origin =
  let text = disk_text header in
  let line = c_line_of declaration text origin in
  let column =
    Option.value ~default:0
      (Option.bind text (fun text ->
           c_define_column text line declaration.Sema_index.name))
  in
  c_location header line column

let definition state params =
  let uri, line, character = request_point params in
  match resolve state uri line character with
  | None -> Json.Null
  | Some (snapshot, key, offset) -> (
      match target snapshot key offset with
      | None -> Json.Null
      | Some ({ Sema_index.c_origin = Some (header, origin); _ } as declaration) ->
          c_declaration_location declaration header origin
      | Some declaration -> (
          match mapped state snapshot declaration.Sema_index.name_span with
          | Some (file, first, last) ->
              Json.Obj
                [
                  ("uri", Json.Str (uri_of_key state file));
                  ("range", range_json first last);
                ]
          | None -> Json.Null))

let same_span (a : Span.t) (b : Span.t) =
  a.start_offset = b.start_offset
  && a.end_offset = b.end_offset
  && Driver.canonical_path a.file = Driver.canonical_path b.file

let references state params =
  let uri, line, character = request_point params in
  let with_declaration = include_declaration params in
  match resolve state uri line character with
  | None -> Json.Null
  | Some (snapshot, key, offset) -> (
      match target snapshot key offset with
      | None -> Json.Null
      | Some declaration ->
          let same (other : Sema_index.declaration) =
            other.Sema_index.name = declaration.Sema_index.name
            && other.Sema_index.kind = declaration.Sema_index.kind
            && other.Sema_index.c_origin = declaration.Sema_index.c_origin
            && same_span other.Sema_index.name_span declaration.Sema_index.name_span
          in
          let uses =
            List.filter_map
              (fun (r : Sema_index.reference) ->
                if same r.Sema_index.declaration then Some r.Sema_index.use_span
                else None)
              (Sema_index.references snapshot.index)
          in
          let declared =
            match declaration.Sema_index.c_origin with
            | Some (header, origin) when with_declaration && header <> "<unknown>" ->
                [ c_declaration_location declaration header origin ]
            | _ -> []
          in
          let spans =
            if with_declaration then declaration.Sema_index.name_span :: uses else uses
          in
          let keyed =
            List.filter_map
              (fun span ->
                Option.map
                  (fun (file, (l1, c1), (l2, c2)) ->
                    (uri_of_key state file, l1, c1, l2, c2))
                  (mapped state snapshot span))
              spans
          in
          Json.Arr
            (declared
            @ List.map
                (fun (location_uri, l1, c1, l2, c2) ->
                  Json.Obj
                    [
                      ("uri", Json.Str location_uri);
                      ("range", range_json (l1, c1) (l2, c2));
                    ])
                (List.sort_uniq compare keyed)))

let symbol_kind kind =
  Sema_index.(
    match kind with
    | Function -> 12
    | Parameter | Local | View | Global -> 13
    | Const | Const_array -> 14
    | Struct | Union | Opaque | Type_alias -> 23
    | Field -> 8
    | Type_parameter | Const_parameter -> 26)

let span_range positions (span : Span.t) =
  range_json
    (Lsp_position.position positions span.start_offset)
    (Lsp_position.position positions span.end_offset)

let symbol_json place (d : Sema_index.declaration) children =
  match (place (outline_range d), place d.Sema_index.name_span) with
  | Some range, Some selection ->
      Some
        (Json.Obj
           [
             ("name", Json.Str d.Sema_index.name);
             ("detail", Json.Str "");
             ("kind", Json.Num (string_of_int (symbol_kind d.Sema_index.kind)));
             ("range", range);
             ("selectionRange", selection);
             ("children", Json.Arr children);
           ])
  | _ -> None

let outline_declarations index key =
  List.filter
    (fun (d : Sema_index.declaration) ->
      d.Sema_index.c_origin = None
      && (not (Span.is_synthetic d.Sema_index.name_span))
      && Driver.canonical_path d.Sema_index.name_span.Span.file = key)
    (Sema_index.declarations index)

let mapped_entries state source declarations =
  List.filter_map
    (fun (d : Sema_index.declaration) ->
      match mapped state source d.Sema_index.name_span with
      | Some (_, start, _) ->
          let place span =
            Option.map
              (fun (_, first, last) -> range_json first last)
              (mapped state source span)
          in
          Some (start, place, d)
      | None -> None)
    declarations

let outline_symbols entries =
  let by_position (a, _, _) (b, _, _) = compare a b in
  let children = Hashtbl.create 64 in
  List.iter
    (fun ((_, _, (c : Sema_index.declaration)) as entry) ->
      Option.iter
        (fun parent ->
          Hashtbl.replace children parent
            (entry :: Option.value ~default:[] (Hashtbl.find_opt children parent)))
        c.Sema_index.container)
    (List.rev entries);
  let child_symbols parent wanted =
    Option.value ~default:[] (Hashtbl.find_opt children parent)
    |> List.filter (fun (_, _, (c : Sema_index.declaration)) -> wanted c)
    |> List.sort by_position
    |> List.filter_map (fun (_, place, c) -> symbol_json place c [])
  in
  let symbol (_, place, (d : Sema_index.declaration)) =
    let nested =
      match d.Sema_index.kind with
      | Sema_index.Function -> child_symbols d.Sema_index.name (fun _ -> true)
      | Sema_index.Struct | Sema_index.Union ->
          child_symbols d.Sema_index.name (fun (c : Sema_index.declaration) ->
              c.Sema_index.kind = Sema_index.Field)
      | _ -> []
    in
    symbol_json place d nested
  in
  let roots =
    List.filter
      (fun (_, _, (d : Sema_index.declaration)) -> d.Sema_index.container = None)
      entries
    |> List.sort by_position
  in
  Json.Arr (List.filter_map symbol roots)

let kept_entries state snapshot key text good declarations =
  let positions = Lsp_position.index text in
  List.filter_map
    (fun (d : Sema_index.declaration) ->
      match
        ( gap_of_declaration state snapshot key text good d,
          mapped state good d.Sema_index.name_span )
      with
      | Some (gap : Driver.gap), Some (_, start, _) ->
          let fallback =
            range_json
              (Lsp_position.position positions gap.Driver.start_offset)
              (Lsp_position.position positions gap.Driver.end_offset)
          in
          let place span =
            match mapped state good span with
            | Some (_, first, last) -> Some (range_json first last)
            | None -> Some fallback
          in
          Some (start, place, d)
      | _ -> None)
    declarations

let document_symbols state params =
  let uri = document_uri params in
  match key_of_uri uri with
  | None -> Json.Null
  | Some key -> (
      match current_text state key with
      | None -> Json.Null
      | Some text -> (
          match Parser.parse (Source.create ~file:key ~text) with
          | Ok program ->
              let index = Sema_index.create () in
              Sema_index.declare_program index ~c_aliases:[] program;
              let positions = Lsp_position.index text in
              outline_symbols
                (List.map
                   (fun (d : Sema_index.declaration) ->
                     ( Lsp_position.position positions
                         d.Sema_index.name_span.Span.start_offset,
                       (fun span -> Some (span_range positions span)),
                       d ))
                   (outline_declarations index key))
          | Error _ -> (
              match snapshot_for state key with
              | None -> Json.Arr []
              | Some snapshot ->
                  let kept =
                    match Hashtbl.find_opt state.good (route_of state key) with
                    | None -> []
                    | Some good ->
                        kept_entries state snapshot key text good
                          (outline_declarations good.index key)
                  in
                  let shadowed (start, _, _) =
                    List.exists (fun (kept_start, _, _) -> kept_start = start) kept
                  in
                  outline_symbols
                    (List.filter
                       (fun entry -> not (shadowed entry))
                       (mapped_entries state snapshot
                          (outline_declarations snapshot.index key))
                    @ kept))))

let identifier_span text (declaration : Sema_index.declaration) =
  let span = declaration.Sema_index.name_span in
  let name = declaration.Sema_index.name in
  let length = String.length name in
  let limit = min span.end_offset (String.length text) in
  let boundary i = i < 0 || i >= String.length text || not (is_word_char text.[i]) in
  let rec scan i =
    if i + length > limit then None
    else if String.sub text i length = name && boundary (i - 1) && boundary (i + length)
    then Some (i, i + length)
    else scan (i + 1)
  in
  scan span.start_offset

let hover_declaration (snapshot : snapshot) key offset =
  match Hashtbl.find_opt snapshot.texts key with
  | None -> None
  | Some text ->
      let inside (span : Span.t) =
        (not (Span.is_synthetic span)) && Driver.canonical_path span.file = key
      in
      let declared =
        List.filter_map
          (fun (d : Sema_index.declaration) ->
            if not (inside d.name_span) then None
            else
              match identifier_span text d with
              | Some (first, last) when first <= offset && offset < last ->
                  Some (last - first, d, None)
              | _ -> None)
          (Sema_index.declarations snapshot.index)
      in
      let used =
        List.filter_map
          (fun (r : Sema_index.reference) ->
            let span = r.use_span in
            if inside span && span.start_offset <= offset && offset < span.end_offset
            then Some (span.end_offset - span.start_offset, r.declaration, Some span)
            else None)
          (Sema_index.references snapshot.index)
      in
      let best =
        List.fold_left
          (fun best ((length, _, _) as candidate) ->
            match best with
            | Some (best_length, _, _) when best_length <= length -> best
            | _ -> Some candidate)
          None (used @ declared)
      in
      Option.map (fun (_, declaration, use) -> (declaration, use)) best

let expression_type (snapshot : snapshot) key offset =
  let containing =
    List.filter
      (fun (e : Sema_index.expression_type) ->
        let span = e.span in
        (not (Span.is_synthetic span))
        && Driver.canonical_path span.file = key
        && span.start_offset <= offset && offset < span.end_offset)
      (Sema_index.expression_types snapshot.index)
  in
  let width (e : Sema_index.expression_type) =
    e.span.end_offset - e.span.start_offset
  in
  match containing with
  | [] -> None
  | first :: _ ->
      let smallest =
        List.fold_left (fun acc e -> min acc (width e)) (width first) containing
      in
      let at_smallest = List.find (fun e -> width e = smallest) containing in
      let same (e : Sema_index.expression_type) =
        e.span.start_offset = at_smallest.span.start_offset
        && e.span.end_offset = at_smallest.span.end_offset
      in
      let at_best =
        List.stable_sort
          (fun (a : Sema_index.expression_type) (b : Sema_index.expression_type) ->
            Int.compare a.rank b.rank)
          (List.filter same containing)
      in
      let texts =
        List.fold_left
          (fun acc (e : Sema_index.expression_type) ->
            if List.mem e.ty acc then acc else acc @ [ e.ty ])
          [] at_best
      in
      Some (String.concat " | " texts, at_smallest.span)

let hover_markdown (header, facts) extra =
  String.concat "\n\n" ((("```fas\n" ^ header ^ "\n```") :: facts) @ extra)

let word_bounds text offset =
  let length = String.length text in
  let offset = min offset length in
  let rec back i = if i > 0 && is_word_char text.[i - 1] then back (i - 1) else i in
  let rec forward i =
    if i < length && is_word_char text.[i] then forward (i + 1) else i
  in
  (back offset, forward offset)

let unsupported_hover state params =
  let uri, line, character = request_point params in
  match key_of_uri uri with
  | None -> None
  | Some key -> (
      match current_text state key with
      | None -> None
      | Some current -> (
          let first, last =
            word_bounds current
              (Lsp_position.offset_of_position current (line, character))
          in
          if first >= last then None
          else
            let name = String.sub current first (last - first) in
            match
              Option.bind
                (Hashtbl.find_opt state.c_roots (route_of state key))
                (fun index ->
                  Option.map
                    (fun reason -> (index, reason))
                    (List.assoc_opt name (Sema_index.c_unsupported index)))
            with
            | None -> None
            | Some (index, reason) ->
                let origin =
                  match Sema_index.c_origin_named index name with
                  | Some (header, row) -> [ Printf.sprintf "%s:%d" header row ]
                  | None -> []
                in
                let value =
                  String.concat "\n\n"
                    (Printf.sprintf "C declaration `%s` is not supported: %s" name
                       reason
                    :: origin)
                in
                Some
                  (Json.Obj
                     [
                       ( "contents",
                         Json.Obj
                           [ ("kind", Json.Str "markdown"); ("value", Json.Str value) ]
                       );
                       ( "range",
                         range_json
                           (Lsp_position.position_of_offset current first)
                           (Lsp_position.position_of_offset current last) );
                     ])))

let pkg_strings text offset =
  let length = String.length text in
  let offset = min offset length in
  let rec line_start i =
    if i > 0 && text.[i - 1] <> '\n' then line_start (i - 1) else i
  in
  let rec line_stop i =
    if i < length && text.[i] <> '\n' then line_stop (i + 1) else i
  in
  let first = line_start offset in
  let line = String.sub text first (line_stop offset - first) in
  let width = String.length line in
  let rec skip_spaces i =
    if i < width && (line.[i] = ' ' || line.[i] = '\t') then skip_spaces (i + 1) else i
  in
  let use_before index = String.trim (String.sub line 0 index) = "use" in
  let candidate index =
    if
      String.sub line index 3 = "pkg"
      && (index = 0 || not (is_word_char line.[index - 1]))
      && use_before index
    then
      let quote = skip_spaces (index + 3) in
      if quote > index + 3 && quote < width && line.[quote] = '"' then
        Option.map
          (fun close ->
            ( first + quote,
              first + close,
              String.sub line (quote + 1) (close - quote - 1) ))
          (String.index_from_opt line (quote + 1) '"')
      else None
    else None
  in
  List.filter_map candidate (List.init (max 0 (width - 2)) Fun.id)

let pkg_hover_markdown modules result =
  let command = Printf.sprintf "```sh\npkg-config --cflags \"%s\"\n```" modules in
  let body =
    match result with
    | Ok [] -> "(none)"
    | Ok flags -> "```text\n" ^ String.concat "\n" flags ^ "\n```"
    | Error message -> message
  in
  command ^ "\n\n" ^ body

let pkg_hover state params =
  let uri, line, character = request_point params in
  match key_of_uri uri with
  | None -> None
  | Some key -> (
      match current_text state key with
      | None -> None
      | Some current -> (
          let offset = Lsp_position.offset_of_position current (line, character) in
          match
            List.find_opt
              (fun (first, close, _) -> first <= offset && offset <= close)
              (pkg_strings current offset)
          with
          | None -> None
          | Some (first, close, modules) ->
              let value =
                pkg_hover_markdown modules (Driver.pkg_cflags_message modules)
              in
              Some
                (Json.Obj
                   [
                     ( "contents",
                       Json.Obj
                         [ ("kind", Json.Str "markdown"); ("value", Json.Str value) ] );
                     ( "range",
                       range_json
                         (Lsp_position.position_of_offset current first)
                         (Lsp_position.position_of_offset current (close + 1)) );
                   ])))

let hover_result state params =
  let uri, line, character = request_point params in
  match resolve state uri line character with
  | None -> None
  | Some (snapshot, key, offset) -> (
      let located =
        match hover_declaration snapshot key offset with
        | Some (declaration, use) ->
            let span =
              match use with
              | Some span -> Some span
              | None ->
                  Option.map
                    (fun (first, last) ->
                      let name_span = declaration.Sema_index.name_span in
                      { name_span with Span.start_offset = first; end_offset = last })
                    (Option.bind (Hashtbl.find_opt snapshot.texts key) (fun text ->
                         identifier_span text declaration))
            in
            Option.map
              (fun content ->
                ( content,
                  c_parts
                    ~enumerator:(Sema_index.is_enumerator snapshot.index declaration)
                    declaration,
                  span ))
              (Sema_index.describe snapshot.index ?use declaration)
        | None ->
            Option.map
              (fun (ty, span) -> ((ty, []), [], Some span))
              (expression_type snapshot key offset)
      in
      match located with
      | None -> None
      | Some (content, extra, span) -> (
          match Option.bind span (mapped state snapshot) with
          | None -> None
          | Some (_, first, last) ->
              Some
                (Json.Obj
                   [
                     ( "contents",
                       Json.Obj
                         [
                           ("kind", Json.Str "markdown");
                           ("value", Json.Str (hover_markdown content extra));
                         ] );
                     ("range", range_json first last);
                   ])))

let hover state params =
  match pkg_hover state params with
  | Some result -> result
  | None -> (
      match hover_result state params with
      | Some result -> result
      | None -> Option.value ~default:Json.Null (unsupported_hover state params))

let inlay_range params =
  let range = child "range" params in
  let position name =
    let value = child name range in
    match (index_member "line" value, index_member "character" value) with
    | Some line, Some character -> (line, character)
    | _ -> invalid "params.range must hold non-negative integer start and end positions"
  in
  let first = position "start" in
  let last = position "end" in
  if compare first last > 0 then invalid "params.range start must not follow end"
  else (first, last)

let inlay_label instances =
  let render arguments = "[" ^ String.concat ", " arguments ^ "]" in
  let labels = List.map render instances in
  let distinct =
    List.fold_left
      (fun acc label -> if List.mem label acc then acc else acc @ [ label ])
      [] labels
  in
  String.concat " | " distinct

let inlay_snapshot state key =
  match snapshot_for state key with
  | Some snapshot when snapshot.gaps = [] -> Some snapshot
  | Some _ -> Hashtbl.find_opt state.good (route_of state key)
  | None -> None

let inlay_hints state params =
  let uri = document_uri params in
  let first, last = inlay_range params in
  match key_of_uri uri with
  | None -> Json.Arr []
  | Some key -> (
      match inlay_snapshot state key with
      | None -> Json.Arr []
      | Some snapshot ->
          let groups = Hashtbl.create 16 in
          List.iter
            (fun (call : Sema_index.generic_call) ->
              let span = call.Sema_index.callee_span in
              if
                (not call.Sema_index.explicit)
                && (not (Span.is_synthetic span))
                && Driver.canonical_path span.file = key
              then
                let previous =
                  Option.value ~default:[] (Hashtbl.find_opt groups span)
                in
                Hashtbl.replace groups span (call :: previous))
            (Sema_index.generic_calls snapshot.index);
          Hashtbl.fold
            (fun span calls acc ->
              let ordered =
                List.stable_sort
                  (fun (a : Sema_index.generic_call) (b : Sema_index.generic_call) ->
                    Int.compare a.Sema_index.order b.Sema_index.order)
                  (List.rev calls)
              in
              let label =
                inlay_label
                  (List.map
                     (fun (c : Sema_index.generic_call) -> c.Sema_index.arguments)
                     ordered)
              in
              match mapped state snapshot span with
              | Some (_, _, position) when first <= position && position <= last ->
                  (position, label) :: acc
              | _ -> acc)
            groups []
          |> List.sort (fun (a, _) (b, _) -> compare a b)
          |> List.map (fun (position, label) ->
              Json.Obj
                [
                  ("position", position_json position);
                  ("label", Json.Str label);
                  ("kind", Json.Num "1");
                  ("paddingLeft", Json.Bool false);
                ])
          |> fun hints -> Json.Arr hints)

let initialize_result =
  Json.Obj
    [
      ( "capabilities",
        Json.Obj
          [
            ("textDocumentSync", Json.Num "1");
            ("definitionProvider", Json.Bool true);
            ("referencesProvider", Json.Bool true);
            ("documentSymbolProvider", Json.Bool true);
            ("hoverProvider", Json.Bool true);
            ("inlayHintProvider", Json.Bool true);
          ] );
      ( "serverInfo",
        Json.Obj [ ("name", Json.Str "fas"); ("version", Json.Str Driver.version) ] );
    ]

let answer id compute =
  match compute () with
  | result -> respond id result
  | exception Invalid message -> fail id (-32602) message

let handle_request state id meth params =
  if state.shut_down then fail id (-32600) "server is shutting down"
  else if (not state.initialized) && meth <> "initialize" then
    fail id (-32002) "server not initialized"
  else (
    flush state;
    match meth with
    | "initialize" ->
        if state.initialized then fail id (-32600) "server already initialized"
        else (
          state.initialized <- true;
          state.folders <- workspace_folders params;
          state.dynamic_watch <- dynamic_watch params;
          respond id initialize_result)
    | "shutdown" ->
        state.shut_down <- true;
        respond id Json.Null
    | "textDocument/definition" -> answer id (fun () -> definition state params)
    | "textDocument/references" -> answer id (fun () -> references state params)
    | "textDocument/hover" -> answer id (fun () -> hover state params)
    | "textDocument/inlayHint" -> answer id (fun () -> inlay_hints state params)
    | "textDocument/documentSymbol" ->
        answer id (fun () -> document_symbols state params)
    | _ -> fail id (-32601) ("method not found: " ^ meth))

let handle_notification state meth params =
  match meth with
  | "exit" -> state.exit_code <- Some (if state.shut_down then 0 else 1)
  | _ when (not state.initialized) || state.shut_down -> ()
  | "initialized" -> on_initialized state
  | "workspace/didChangeWatchedFiles" -> on_watched_files state
  | "textDocument/didOpen" -> open_document state params
  | "textDocument/didChange" -> change_document state params
  | "textDocument/didSave" -> save_document state params
  | "textDocument/didClose" -> close_document state params
  | _ -> ()

let handle_body state body =
  match Json.parse body with
  | Error _ -> fail Json.Null (-32700) "parse error"
  | Ok (Json.Obj fields as message) -> (
      let params = child "params" message in
      match (string_member "method" message, List.assoc_opt "id" fields) with
      | Some meth, Some id -> (
          try handle_request state id meth params
          with exn -> fail id (-32603) (Printexc.to_string exn))
      | Some meth, None -> (
          try handle_notification state meth params
          with exn -> log (Printexc.to_string exn))
      | None, Some _ -> ()
      | None, None -> fail Json.Null (-32600) "invalid request")
  | Ok _ -> fail Json.Null (-32600) "invalid request"

let rec readable timeout =
  match Unix.select [ Unix.stdin ] [] [] timeout with
  | ready, _, _ -> ready <> []
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> readable timeout
  | exception Unix.Unix_error _ -> true

let rec read_input chunk =
  match Unix.read Unix.stdin chunk 0 (Bytes.length chunk) with
  | count -> count
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_input chunk
  | exception Unix.Unix_error _ -> 0

let await state =
  if not (readable (if state.due then 0.02 else -1.0)) then (
    flush state;
    ignore (readable (-1.0)))

let run options =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  let state = create options in
  let chunk = Bytes.create 65536 in
  let finish code =
    flush state;
    code
  in
  let rec loop skipping pending =
    match state.exit_code with
    | Some code -> finish code
    | None -> (
        let synced, pending =
          if skipping then Lsp_frame.resynchronise pending else (true, pending)
        in
        match
          if synced then Lsp_frame.take_message pending else Lsp_frame.Incomplete
        with
        | Lsp_frame.Message (body, rest) ->
            (try handle_body state body with exn -> log (Printexc.to_string exn));
            loop false rest
        | Malformed (reason, rest) ->
            fail Json.Null (-32700) reason;
            loop true rest
        | Incomplete -> (
            await state;
            match read_input chunk with
            | 0 -> finish (if state.shut_down then 0 else 1)
            | n -> loop skipping (pending ^ Bytes.sub_string chunk 0 n)))
  in
  let code = loop false "" in
  C_import.clear_cache state.cache;
  code
