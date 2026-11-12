type emit = Ir | Llvm | Optimized_llvm | Asm | Obj | Header | Executable
type diagnostics = Text | Json
type target = Portable | Force_native
type native_runtime = Auto | Freestanding | Hosted

type t = {
  input : string;
  output : string;
  output_explicit : bool;
  emit : emit;
  keep : bool;
  optimization : int;
  debug : bool;
  sanitizers : string list;
  link_inputs : string list;
  c_flags : string list;
  diagnostics : diagnostics;
  target_cpu : string option;
  target : target;
  target_explicit : bool;
  os : Target_layout.os;
  target_features : string option;
  all_errors : bool;
  native : bool;  (** built-in x86-64 code generator; false only under the LLVM oracle *)
  native_runtime : native_runtime;
}

type lsp_options = {
  lsp_c_flags : string list;
  lsp_target : target;
  lsp_os : Target_layout.os;
}

type command =
  | Run of t
  | Run_program of t * string list
  | Check of t
  | Lsp of lsp_options
  | Help
  | Version

type subcommand = Build | Run_subcommand | Check_subcommand

(* [FAS_ORACLE=llvm] is the hidden switch that keeps the LLVM pipeline as the
   test oracle: it restores --backend=, --emit-llvm, --target=force-native,
   FAS_OPT/FAS_LLC/FAS_OPT_PASSES and LLVM as the default backend, exactly as
   before removal A3. It is not part of the user interface and is not in help. *)
let oracle () = Sys.getenv_opt "FAS_ORACLE" = Some "llvm"

let usage =
  "usage: fas [options] file.fas [C inputs and link flags]\n\
  \       fas run file.fas [options] [-- program arguments]\n\
  \       fas check file.fas [options]\n\
   commands:\n\
  \  run FILE      build to a temporary executable, run it, delete it\n\
  \  check FILE    check without building (no code generation, no linking)\n\
  \  lsp           language server over stdio (for editors)\n\
   build:\n\
  \  -O0..-O3      accepted for compatibility and ignored; every build is the same\n\
  \  --os linux    operating system to build for (the only one today)\n\
   output:\n\
  \  -o PATH       output path (default a.out, INPUT.o with -c)\n\
  \  --emit-header write an ABI-checked C header (stdout without -o)\n\
  \  --emit-obj, -c emit an ELF object\n\
  \  --diagnostics=text|json  print diagnostics as text (default) or one JSON line\n\
  \  --all-errors  report every error in function bodies instead of stopping at the \
   first\n\
  \  --hosted     executable with system C startup and libraries (FAS_CC)\n\
  \  --freestanding  executable with Fas startup and no C tools\n\
  \               default: hosted for C inputs/imports, freestanding otherwise\n\
  \  --version     print the fas version and the C compiler in use\n\
  \  -h, --help    show this help\n\
   C inputs:\n\
  \  -I DIR, -isystem DIR, -D NAME[=VALUE] (attached or separate)\n\
  \  -l, -L        link flags (attached or separate)\n\
  \  -Wl,ARG,..., -Xlinker ARG  pass arguments to the built-in linker\n\
  \  FILE.c, FILE.o, FILE.a, FILE.so  C inputs linked with the program\n\
   debugging:\n\
  \  --emit-ir     print the lowered module as Fir text (src/fir/SPEC.md)\n\
  \  --keep        retain and report intermediate paths\n\
  \  -g            emit DWARF debug info (machine code unchanged)\n\
   environment:\n\
  \  FAS_CC        the C compiler for C inputs, C imports and --hosted (default \
   clang-22; without it, clang when it reports LLVM 22)"

let default_output os emit input =
  let source_output extension =
    Filename.remove_extension (Filename.basename input) ^ extension
  in
  match (emit, os) with
  | Asm, _ -> source_output ".s"
  | Obj, _ -> source_output ".o"
  | _, Target_layout.Linux -> "a.out"

(* External C drivers interpret these wrappers themselves. The built-in linker
   receives their payloads in the original order, without shell tokenization. *)
let native_linker_arguments arguments =
  let rec expand = function
    | [] -> []
    | "-Xlinker" :: argument :: rest -> argument :: expand rest
    | flag :: rest when String.starts_with ~prefix:"-Wl," flag ->
        String.split_on_char ',' (String.sub flag 4 (String.length flag - 4))
        @ expand rest
    | argument :: rest -> argument :: expand rest
  in
  expand arguments

let target_of_value value =
  match value with
  | "portable" -> Ok Portable
  | "force-native" when oracle () -> Ok Force_native
  | "force-native" ->
      Error
        "`--target=force-native` was removed; Fas builds baseline x86-64 code that \
         runs on any x86-64 CPU"
  | _ when not (oracle ()) ->
      Error ("unknown --target value `" ^ value ^ "`; the accepted value is `portable`")
  | _ ->
      Error
        ("unknown --target value `" ^ value
       ^ "`; accepted values are `portable` and `force-native`")

let os_of_value value =
  match value with
  | "linux" -> Ok Target_layout.Linux
  | "windows" | "wasi" ->
      Error ("`--os " ^ value ^ "` is no longer supported; Fas targets `linux` only")
  | _ -> Error ("unknown --os value `" ^ value ^ "`; the accepted value is `linux`")

let os_argument argv i =
  let flag = argv.(i) in
  if flag = "--os" then
    if i + 1 >= Array.length argv then Error "--os requires an argument"
    else Result.map (fun os -> (os, i + 2)) (os_of_value argv.(i + 1))
  else
    Result.map
      (fun os -> (os, i + 1))
      (os_of_value (String.sub flag 5 (String.length flag - 5)))

let parse_invocation ~subcommand argv =
  let n = Array.length argv in
  let header_selected = ref false and other_output = ref false in
  let link_inputs_rev = ref [] in
  let c_flags_rev = ref [] in
  let sanitizers = ref None in
  let diagnostics = ref Text in
  let target = ref Portable in
  let target_explicit = ref false in
  let os = ref Target_layout.host_os in
  let all_errors = ref false in
  let oracle = oracle () in
  let native = ref (not oracle) in
  let runtime = ref None in
  let add_link_inputs values =
    link_inputs_rev := List.rev_append values !link_inputs_rev
  in
  let add_c_flags values = c_flags_rev := List.rev_append values !c_flags_rev in
  let passthrough_file file =
    List.exists (Filename.check_suffix file) [ ".c"; ".o"; ".a"; ".so" ]
  in
  let rejected flag =
    match subcommand with
    | Build -> false
    | Run_subcommand ->
        List.mem flag [ "-o"; "-c"; "-S" ] || String.starts_with ~prefix:"--emit-" flag
    | Check_subcommand ->
        List.mem flag [ "-o"; "-c"; "-S"; "-l"; "-L" ]
        || String.starts_with ~prefix:"--emit-" flag
        || String.starts_with ~prefix:"-l" flag
        || String.starts_with ~prefix:"-L" flag
        || String.starts_with ~prefix:"-Wl," flag
        || flag = "-Xlinker" || passthrough_file flag
  in
  let rec loop i input output emit keep optimization optimization_explicit debug =
    if i >= n then
      if Option.is_none input then Error "no input files"
      else if !header_selected && !other_output then
        Error "--emit-header cannot be combined with other output modes"
      else if !link_inputs_rev <> [] && emit <> Executable then
        Error "C inputs and link flags require an executable output"
      else if Option.is_some !runtime && ((not !native) || emit <> Executable) then
        Error
          (if oracle then "--hosted/--freestanding require a --backend=native executable"
           else "--hosted/--freestanding require an executable output")
      else if Option.is_some !runtime && subcommand = Check_subcommand then
        Error "--hosted/--freestanding have no effect with fas check"
      else if !native && Option.is_some !sanitizers then
        Error
          (if oracle then "--sanitize is not supported by --backend=native"
           else "--sanitize is not supported yet")
      else
            let input = Option.get input in
            let output_explicit = Option.is_some output in
            let output =
              match output with
              | Some path -> path
              | None -> default_output !os emit input
            in
            let config =
              {
                input;
                output;
                output_explicit;
                emit;
                keep;
                optimization;
                debug;
                sanitizers = Option.value ~default:[] !sanitizers;
                link_inputs = List.rev !link_inputs_rev;
                c_flags = List.rev !c_flags_rev;
                diagnostics = !diagnostics;
                target_cpu = None;
                target = !target;
                target_explicit = !target_explicit;
                os = !os;
                target_features = None;
                all_errors = !all_errors;
                native = !native;
                native_runtime = Option.value ~default:Auto !runtime;
              }
            in
            Ok (if subcommand = Check_subcommand then Check config else Run config)
    else
      match argv.(i) with
      | "--help" | "-h" -> Ok Help
      | flag when rejected flag ->
          Error
            (match subcommand with
            | Check_subcommand ->
                "fas check checks one Fas file; `" ^ flag ^ "` is not accepted"
            | Build | Run_subcommand ->
                "fas run builds and runs an executable; `" ^ flag ^ "` is not accepted")
      | "-o" ->
          if i + 1 >= n then Error "-o requires an output path"
          else if argv.(i + 1) = "-" then Error "-o - is not supported"
          else
            loop (i + 2) input
              (Some argv.(i + 1))
              emit keep optimization optimization_explicit debug
      | "--emit-header" ->
          header_selected := true;
          loop (i + 1) input output Header keep optimization optimization_explicit debug
      | "--emit-ir" ->
          other_output := true;
          loop (i + 1) input output Ir keep optimization optimization_explicit debug
      | flag
        when (not oracle)
             && (flag = "--emit-llvm" || String.starts_with ~prefix:"--emit-llvm=" flag) ->
          Error
            "`--emit-llvm` was removed; Fas no longer uses LLVM (`--emit-ir` prints the \
             Fir module)"
      | flag when (not oracle) && String.starts_with ~prefix:"--backend=" flag ->
          Error "`--backend=` was removed; the built-in backend is the only one"
      | ("--emit-asm" | "-S") when not oracle ->
          Error
            "assembly text output is not supported; use -c for an ELF object (and \
             `objdump -d` to read it)"
      | "--emit-llvm" ->
          other_output := true;
          loop (i + 1) input output Llvm keep optimization optimization_explicit debug
      | "--emit-llvm=optimized" ->
          other_output := true;
          loop (i + 1) input output Optimized_llvm keep optimization
            optimization_explicit debug
      | flag when String.starts_with ~prefix:"--emit-llvm=" flag ->
          let value = String.sub flag 12 (String.length flag - 12) in
          Error
            ("unknown --emit-llvm value `" ^ value ^ "`; accepted value is `optimized`")
      | "--emit-asm" | "-S" ->
          other_output := true;
          loop (i + 1) input output Asm keep optimization optimization_explicit debug
      | "--emit-obj" | "-c" ->
          other_output := true;
          loop (i + 1) input output Obj keep optimization optimization_explicit debug
      | flag when String.starts_with ~prefix:"--diagnostics=" flag -> (
          let value = String.sub flag 14 (String.length flag - 14) in
          match value with
          | "text" ->
              diagnostics := Text;
              loop (i + 1) input output emit keep optimization optimization_explicit
                debug
          | "json" ->
              diagnostics := Json;
              loop (i + 1) input output emit keep optimization optimization_explicit
                debug
          | _ ->
              Error
                ("unknown --diagnostics value `" ^ value
               ^ "`; accepted values are `text` and `json`"))
      | flag when String.starts_with ~prefix:"--target=" flag -> (
          let value = String.sub flag 9 (String.length flag - 9) in
          let select selected =
            target := selected;
            target_explicit := true;
            loop (i + 1) input output emit keep optimization optimization_explicit debug
          in
          match target_of_value value with
          | Ok selected -> select selected
          | Error message -> Error message)
      | flag when flag = "--os" || String.starts_with ~prefix:"--os=" flag -> (
          match os_argument argv i with
          | Ok (selected, next) ->
              os := selected;
              loop next input output emit keep optimization optimization_explicit debug
          | Error message -> Error message)
      | "--keep" ->
          loop (i + 1) input output emit true optimization optimization_explicit debug
      | "--all-errors" ->
          all_errors := true;
          loop (i + 1) input output emit keep optimization optimization_explicit debug
      | "--backend=native" | "--backend=llvm" ->
          native := argv.(i) = "--backend=native";
          loop (i + 1) input output emit keep optimization optimization_explicit debug
      | ("--hosted" | "--freestanding") as flag ->
          let selected = if flag = "--hosted" then Hosted else Freestanding in
          if Option.is_some !runtime && !runtime <> Some selected then
            Error "--hosted and --freestanding cannot be combined"
          else (
            runtime := Some selected;
            loop (i + 1) input output emit keep optimization optimization_explicit debug)
      | "--profile" -> Error "`--profile` was removed (Fas has no profile-guided builds)"
      | flag when String.starts_with ~prefix:"--sanitize=" flag -> (
          let value = String.sub flag 11 (String.length flag - 11) in
          let names = String.split_on_char ',' value in
          if value = "" then Error "--sanitize requires a non-empty list"
          else
            let unknown =
              List.find_opt (fun name -> name <> "address" && name <> "undefined") names
            in
            match unknown with
            | Some name -> Error ("unknown sanitizer `" ^ name ^ "`")
            | None -> (
                let names = List.sort_uniq String.compare names in
                match !sanitizers with
                | Some previous when previous <> names ->
                    Error "conflicting --sanitize options"
                | _ ->
                    sanitizers := Some names;
                    loop (i + 1) input output emit keep optimization
                      optimization_explicit debug))
      | "-g" ->
          loop (i + 1) input output emit keep optimization optimization_explicit true
      | "--no-inline" ->
          Error "`--no-inline` was removed; Fas has no inliner yet"
      | ("-l" | "-L") as flag ->
          if i + 1 >= n || argv.(i + 1) = "" then Error (flag ^ " requires an argument")
          else (
            add_link_inputs [ flag; argv.(i + 1) ];
            loop (i + 2) input output emit keep optimization optimization_explicit debug)
      | "-Xlinker" ->
          if i + 1 >= n || argv.(i + 1) = "" then Error "-Xlinker requires an argument"
          else (
            add_link_inputs [ "-Xlinker"; argv.(i + 1) ];
            loop (i + 2) input output emit keep optimization optimization_explicit debug)
      | flag when String.starts_with ~prefix:"-Wl," flag ->
          let arguments = String.sub flag 4 (String.length flag - 4) in
          if List.exists (( = ) "") (String.split_on_char ',' arguments) then
            Error "-Wl, requires nonempty comma-separated linker arguments"
          else (
            add_link_inputs [ flag ];
            loop (i + 1) input output emit keep optimization optimization_explicit debug)
      | ("-I" | "-isystem" | "-D") as flag ->
          if i + 1 >= n || argv.(i + 1) = "" then Error (flag ^ " requires an argument")
          else (
            add_c_flags [ flag; argv.(i + 1) ];
            loop (i + 2) input output emit keep optimization optimization_explicit debug)
      | flag
        when (String.length flag > 2 && String.sub flag 0 2 = "-l")
             || (String.length flag > 2 && String.sub flag 0 2 = "-L") ->
          add_link_inputs [ flag ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
      | flag when String.starts_with ~prefix:"-isystem" flag && String.length flag > 8
        ->
          add_c_flags [ flag ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
      | flag when String.starts_with ~prefix:"-I" flag && String.length flag > 2 ->
          add_c_flags [ flag ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
      | flag when String.starts_with ~prefix:"-D" flag && String.length flag > 2 ->
          add_c_flags [ flag ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
      | flag
        when String.length flag = 3
             && flag.[0] = '-'
             && flag.[1] = 'O'
             && flag.[2] >= '0'
             && flag.[2] <= '3' ->
          loop (i + 1) input output emit keep
            (Char.code flag.[2] - Char.code '0')
            true debug
      | "-debug" -> Error "unknown option `-debug`; write `-g`"
      | flag when String.length flag > 0 && flag.[0] = '-' ->
          Error ("unknown option: " ^ flag)
      | file when passthrough_file file ->
          add_link_inputs [ file ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
      | file -> (
          match input with
          | None ->
              loop (i + 1) (Some file) output emit keep optimization
                optimization_explicit debug
          | Some _ ->
              Error
                "multiple input files are not supported; use \"path.fas\" for \
                 dependencies")
  in
  loop
    (if subcommand = Build then 1 else 2)
    None None
    (if subcommand = Check_subcommand then Ir else Executable)
    false 2 false false

let parse_lsp argv =
  let n = Array.length argv in
  let c_flags_rev = ref []
  and target = ref Portable
  and os = ref Target_layout.host_os in
  let rec loop i =
    if i >= n then
      Ok
        (Lsp { lsp_c_flags = List.rev !c_flags_rev; lsp_target = !target; lsp_os = !os })
    else
      match argv.(i) with
      | "--help" | "-h" -> Ok Help
      | ("-I" | "-isystem" | "-D") as flag ->
          if i + 1 >= n || argv.(i + 1) = "" then Error (flag ^ " requires an argument")
          else (
            c_flags_rev := argv.(i + 1) :: flag :: !c_flags_rev;
            loop (i + 2))
      | flag
        when List.exists
               (fun prefix ->
                 String.starts_with ~prefix flag
                 && String.length flag > String.length prefix)
               [ "-I"; "-isystem"; "-D" ] ->
          c_flags_rev := flag :: !c_flags_rev;
          loop (i + 1)
      | flag when String.starts_with ~prefix:"--target=" flag -> (
          let value = String.sub flag 9 (String.length flag - 9) in
          match target_of_value value with
          | Ok selected ->
              target := selected;
              loop (i + 1)
          | Error message -> Error message)
      | flag when flag = "--os" || String.starts_with ~prefix:"--os=" flag -> (
          match os_argument argv i with
          | Ok (selected, next) ->
              os := selected;
              loop next
          | Error message -> Error message)
      | flag when String.length flag > 0 && flag.[0] = '-' ->
          Error ("unknown option: " ^ flag)
      | file ->
          Error
            ("fas lsp takes no input file; the editor opens files, `" ^ file
           ^ "` is not accepted")
  in
  loop 2

let parse argv =
  let n = Array.length argv in
  if n > 1 && String.equal argv.(1) "lsp" then parse_lsp argv
  else if n > 1 && String.equal argv.(1) "perf" then
    Error "`fas perf` was removed; use the system `perf` on a `-g` build"
  else
    let subcommand =
      if n > 1 && String.equal argv.(1) "run" then Run_subcommand
      else if n > 1 && String.equal argv.(1) "check" then Check_subcommand
      else Build
    in
    let rec find i = if i >= n || String.equal argv.(i) "--" then i else find (i + 1) in
    let sep =
      if subcommand = Build || subcommand = Check_subcommand then n else find 2
    in
    if n > 1 && Array.exists (String.equal "--version") (Array.sub argv 1 (sep - 1))
    then
      if n = 2 then Ok Version
      else Error "--version cannot be combined with other arguments"
    else
      let args =
        match Array.to_list (Array.sub argv sep (n - sep)) with
        | _ :: args -> args
        | [] -> []
      in
      let build_argv = Array.sub argv 0 sep in
      match parse_invocation ~subcommand build_argv with
      | Ok (Run config) when subcommand = Run_subcommand -> Ok (Run_program (config, args))
      | result -> result
