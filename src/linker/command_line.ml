type item =
  | Path of string * bool * bool
  | Library of string * bool
  | Static_library of string * bool
  | Group of item list
  | As_needed of item

type config = {
  output : string;
  entry : string;
  inputs : Input.t list;
  statistics : bool;
  map_file : string option;
  options : Options.t;
  relocatable : bool;
}

type action = Help | Version | Link of config

let parse ?(link_only = false) arguments =
  let exception Stop of action in
  let application_option flag =
    if link_only then Error.fail "%s is not accepted among compiler link inputs" flag
  in
  try
    let output = ref "a.out" and entry = ref "_start" in
    let relocatable = ref false in
    let map_file = ref None in
    let gc_sections = ref false and undefined = ref [] in
    let pie = ref false and interpreter = ref None in
    let no_interpreter = ref false in
    let runpath = ref [] in
    let new_dtags = ref true in
    let allow_shlib_undefined = ref true and rpath_link = ref [] in
    let export_dynamic = ref false in
    let relro = ref true in
    let shared = ref false and soname = ref None in
    let no_undefined = ref false in
    let allow_multiple_definition = ref false in
    let version_script = ref None in
    let hash_style = ref Options.Sysv and build_id = ref Options.No_id in
    let directories = ref [] and whole = ref false and statistics = ref false in
    let sysroot = ref None and nostdlib = ref false in
    let as_needed = ref false and static_libraries = ref false in
    let states = ref [] in
    let dependency item = if !as_needed then As_needed item else item in
    let library_item name =
      dependency
        (if !static_libraries then Static_library (name, !whole)
         else Library (name, !whole))
    in
    let args = Response_file.expand arguments in
    let value flag = function
      | [] -> Error.fail "%s requires an argument" flag
      | value :: rest -> (value, rest)
    in
    let rec options depth collected positional args =
      match args with
      | [] ->
          if depth <> 0 then Error.fail "missing --end-group";
          if !states <> [] then Error.fail "missing --pop-state";
          (List.rev collected, [])
      | arg :: rest when positional ->
          options depth
            (dependency (Path (arg, !whole, !static_libraries)) :: collected)
            true rest
      | (("--help" | "-h") as flag) :: _ ->
          application_option flag;
          raise (Stop Help)
      | "--version" :: _ ->
          application_option "--version";
          raise (Stop Version)
      | "--" :: rest -> options depth collected true rest
      | (("-r" | "--relocatable") as flag) :: rest ->
          application_option flag;
          relocatable := true;
          options depth collected false rest
      | (("--as-needed" | "--no-as-needed") as flag) :: rest ->
          as_needed := flag = "--as-needed";
          options depth collected false rest
      | "--push-state" :: rest ->
          if List.length !states >= 64 then Error.fail "option state nesting exceeds 64";
          states := (!whole, !as_needed, !static_libraries) :: !states;
          options depth collected false rest
      | "--pop-state" :: rest ->
          (match !states with
          | [] -> Error.fail "--pop-state without --push-state"
          | (w, a, s) :: tail ->
              whole := w;
              as_needed := a;
              static_libraries := s;
              states := tail);
          options depth collected false rest
      | (("-Bstatic" | "-Bdynamic") as flag) :: rest ->
          static_libraries := flag = "-Bstatic";
          options depth collected false rest
      | "-static" :: rest ->
          static_libraries := true;
          pie := false;
          interpreter := None;
          no_interpreter := false;
          options depth collected false rest
      | "-static-pie" :: rest ->
          static_libraries := true;
          pie := true;
          interpreter := None;
          no_interpreter := true;
          options depth collected false rest
      | "--no-dynamic-linker" :: rest ->
          interpreter := None;
          no_interpreter := true;
          options depth collected false rest
      | "-m" :: rest ->
          let emulation, rest = value "-m" rest in
          if emulation <> "elf_x86_64" then
            Error.fail "unsupported emulation %s" emulation;
          options depth collected false rest
      | "--eh-frame-hdr" :: rest -> options depth collected false rest
      | "-nostdlib" :: rest ->
          nostdlib := true;
          options depth collected false rest
      | "--sysroot" :: rest ->
          let path, rest = value "--sysroot" rest in
          sysroot := Some path;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--sysroot=" arg ->
          sysroot := Some (String.sub arg 10 (String.length arg - 10));
          options depth collected false rest
      | "--library-path" :: rest ->
          let path, rest = value "--library-path" rest in
          directories := path :: !directories;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--library-path=" arg ->
          directories := String.sub arg 15 (String.length arg - 15) :: !directories;
          options depth collected false rest
      | "--build-id" :: rest ->
          build_id := Options.Sha1;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--build-id=" arg ->
          (build_id :=
             match String.sub arg 11 (String.length arg - 11) with
             | "none" -> Options.No_id
             | "sha1" -> Sha1
             | "md5" -> Md5
             | style -> Error.fail "unsupported build identifier style %s" style);
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--hash-style=" arg ->
          (hash_style :=
             match String.sub arg 13 (String.length arg - 13) with
             | "sysv" -> Options.Sysv
             | "gnu" -> Gnu
             | "both" -> Both
             | style -> Error.fail "unsupported hash style %s" style);
          options depth collected false rest
      | ("--end-group" | "-)") :: rest ->
          if depth = 0 then Error.fail "--end-group without --start-group";
          (List.rev collected, rest)
      | ("--start-group" | "-(") :: rest ->
          if depth >= 16 then Error.fail "archive group nesting exceeds 16";
          let group, rest = options (depth + 1) [] false rest in
          options depth (Group group :: collected) false rest
      | "--whole-archive" :: rest ->
          whole := true;
          options depth collected false rest
      | "--no-whole-archive" :: rest ->
          whole := false;
          options depth collected false rest
      | "-pie" :: rest ->
          pie := true;
          options depth collected false rest
      | "-shared" :: rest ->
          shared := true;
          options depth collected false rest
      | "--no-undefined" :: rest ->
          no_undefined := true;
          options depth collected false rest
      | "--allow-multiple-definition" :: rest ->
          allow_multiple_definition := true;
          options depth collected false rest
      | "--version-script" :: rest ->
          let path, rest = value "--version-script" rest in
          version_script := Some path;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--version-script=" arg ->
          version_script := Some (String.sub arg 17 (String.length arg - 17));
          options depth collected false rest
      | (("-soname" | "--soname") as flag) :: rest ->
          let name, rest = value flag rest in
          soname := Some name;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--soname=" arg ->
          soname := Some (String.sub arg 9 (String.length arg - 9));
          options depth collected false rest
      | "-no-pie" :: rest ->
          pie := false;
          options depth collected false rest
      | "-z" :: rest ->
          let policy, rest = value "-z" rest in
          (match policy with
          | "relro" -> relro := true
          | "norelro" -> relro := false
          | "text" -> () (* All output modes already reject text relocations. *)
          | "now" -> ()
          | "defs" -> no_undefined := true
          | "undefs" -> no_undefined := false
          | "muldefs" -> allow_multiple_definition := true
          | "lazy" -> Error.fail "lazy binding is not supported"
          | _ -> Error.fail "unsupported -z policy %s" policy);
          options depth collected false rest
      | (("-E" | "--export-dynamic" | "--no-export-dynamic") as flag) :: rest ->
          export_dynamic := flag <> "--no-export-dynamic";
          options depth collected false rest
      | (("--dynamic-linker" | "-dynamic-linker") as flag) :: rest ->
          no_interpreter := false;
          let path, rest = value flag rest in
          interpreter := Some path;
          options depth collected false rest
      | (("-rpath" | "--rpath") as flag) :: rest ->
          let path, rest = value flag rest in
          runpath := path :: !runpath;
          options depth collected false rest
      | (("--enable-new-dtags" | "--disable-new-dtags") as flag) :: rest ->
          new_dtags := flag = "--enable-new-dtags";
          options depth collected false rest
      | (("--allow-shlib-undefined" | "--no-allow-shlib-undefined") as flag) :: rest ->
          allow_shlib_undefined := flag = "--allow-shlib-undefined";
          options depth collected false rest
      | (("-rpath-link" | "--rpath-link") as flag) :: rest ->
          let path, rest = value flag rest in
          rpath_link := path :: !rpath_link;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--rpath-link=" arg ->
          rpath_link := String.sub arg 13 (String.length arg - 13) :: !rpath_link;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"-rpath-link=" arg ->
          rpath_link := String.sub arg 12 (String.length arg - 12) :: !rpath_link;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--rpath=" arg ->
          runpath := String.sub arg 8 (String.length arg - 8) :: !runpath;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--dynamic-linker=" arg ->
          no_interpreter := false;
          interpreter := Some (String.sub arg 17 (String.length arg - 17));
          options depth collected false rest
      | "--stats" :: rest ->
          application_option "--stats";
          statistics := true;
          options depth collected false rest
      | (("--gc-sections" | "--no-gc-sections") as flag) :: rest ->
          gc_sections := flag = "--gc-sections";
          options depth collected false rest
      | (("-u" | "--undefined") as flag) :: rest ->
          let argument, rest = value flag rest in
          undefined := argument :: !undefined;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"--undefined=" arg ->
          undefined := String.sub arg 12 (String.length arg - 12) :: !undefined;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"-u" arg && String.length arg > 2 ->
          undefined := String.sub arg 2 (String.length arg - 2) :: !undefined;
          options depth collected false rest
      | (("-Map" | "--Map") as flag) :: rest ->
          application_option flag;
          let argument, rest = value flag rest in
          map_file := Some argument;
          options depth collected false rest
      | arg :: rest
        when String.starts_with ~prefix:"-Map=" arg
             || String.starts_with ~prefix:"--Map=" arg ->
          application_option "-Map";
          let prefix = if arg.[1] = '-' then 6 else 5 in
          map_file := Some (String.sub arg prefix (String.length arg - prefix));
          options depth collected false rest
      | (("-o" | "--output" | "-e" | "--entry" | "-L" | "-l") as flag) :: rest -> (
          let argument, rest = value flag rest in
          match flag with
          | "-o" | "--output" ->
              application_option flag;
              output := argument;
              options depth collected false rest
          | "-e" | "--entry" ->
              application_option flag;
              entry := argument;
              options depth collected false rest
          | "-L" ->
              directories := argument :: !directories;
              options depth collected false rest
          | _ -> options depth (library_item argument :: collected) false rest)
      | arg :: rest when String.starts_with ~prefix:"-L" arg ->
          directories := String.sub arg 2 (String.length arg - 2) :: !directories;
          options depth collected false rest
      | arg :: rest when String.starts_with ~prefix:"-l" arg ->
          options depth
            (library_item (String.sub arg 2 (String.length arg - 2)) :: collected)
            false rest
      | arg :: _ when String.starts_with ~prefix:"-" arg ->
          Error.fail "unsupported option %s" arg
      | arg :: rest ->
          options depth
            (dependency (Path (arg, !whole, !static_libraries)) :: collected)
            false rest
    in
    let items, _ = options 0 [] false args in
    let directories = List.rev !directories in
    if
      !relocatable
      && (!shared || !pie || !interpreter <> None || !no_interpreter || !gc_sections
        || !map_file <> None || !build_id <> Options.No_id || !version_script <> None
        || !soname <> None || !runpath <> [] || !rpath_link <> []
        || (not !allow_shlib_undefined) || !export_dynamic || !entry <> "_start")
    then Error.fail "-r cannot be combined with final-image, GC, map or version options";
    if !shared && !pie then Error.fail "-shared cannot be combined with -pie";
    if !soname <> None && not !shared then Error.fail "-soname requires -shared";
    if !version_script <> None && not !shared then
      Error.fail "--version-script requires -shared";
    let rec input = function
      | Path (path, whole, static) ->
          let file = Input.File { path; whole } in
          if static then Input.Static file else file
      | Library (name, whole) -> Input.Library { name; whole; static = false }
      | Static_library (name, whole) -> Input.Library { name; whole; static = true }
      | Group children -> Input.Group (List.map input children)
      | As_needed child -> Input.As_needed (input child)
    in
    Link
      {
        output = !output;
        relocatable = !relocatable;
        entry = !entry;
        inputs = List.map input items;
        statistics = !statistics;
        map_file = !map_file;
        options =
          {
            Options.gc_sections = !gc_sections;
            hash_style = !hash_style;
            build_id = !build_id;
            new_dtags = !new_dtags;
            allow_shlib_undefined = !allow_shlib_undefined;
            rpath_link = List.rev !rpath_link;
            export_dynamic = !export_dynamic;
            library_paths = directories;
            sysroot = !sysroot;
            nostdlib = !nostdlib;
            relro = !relro;
            soname = !soname;
            no_undefined = !no_undefined;
            allow_multiple_definition = !allow_multiple_definition;
            version_script =
              Option.map
                (fun path ->
                  Version_script.parse ~path (File_io.read ~max_size:(1024 * 1024) path))
                !version_script;
            undefined = List.rev !undefined;
            runpath =
              (if !runpath = [] then None
               else Some (String.concat ":" (List.rev !runpath)));
            mode =
              (if !shared then Shared
               else if !pie && !no_interpreter then Static_pie
               else if !pie || !interpreter <> None then
                 Dynamic
                   {
                     pie = !pie;
                     interpreter =
                       Option.value !interpreter ~default:"/lib64/ld-linux-x86-64.so.2";
                   }
               else Static);
          };
      }
  with Stop action -> action
