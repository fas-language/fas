(* fas-fir: compile a Fir text module (src/fir/SPEC.md, docs FIR.md) with the
   native backend. This is the entry point for front ends written outside
   this compiler, such as a C compiler written in Fas that prints Fir text.

     fas-fir prog.fir -o prog     static Linux x86-64 executable; the
                                  module must define @main, which _start
                                  calls with (argc i32, argv ptr) and whose
                                  i32 result becomes the exit status
     fas-fir --hosted prog.fir -o prog   the same executable on the system
                                  C runtime ($FAS_CC's startup files and
                                  libc, found with `$FAS_CC -###`, linked by
                                  the Fas linker): libc's _start calls @main,
                                  and @main may call printf, calloc, ...
     fas-fir -c prog.fir -o prog.o   position-independent ELF object
     fas-fir --check prog.fir     parse and verify only; prints the module
                                  back in canonical form
     fas-fir --legalize prog.fir  prints the module after Fir_wide (integers
                                  wider than i64 or of odd widths rewritten
                                  to i64 limbs), verified again
     fas-fir --emit-llvm prog.fir prints the module as LLVM IR (Fir_llvm,
                                  the LLVM path's emitter)
     fas-fir --llvm ...           builds the executable or object through
                                  LLVM instead: Fir_llvm, then $FAS_CC
                                  (default clang) -O2; hosted, so @main is
                                  called by the C runtime

   "-" reads the module from stdin. Extra .o/.a inputs after the .fir file
   are linked in (executables only). The pipeline is the machine-IR backend
   (src/backend/mir): parse -> verify -> Fir_scalarize -> Fir_wide ->
   Mir_isel -> Mir_ra -> Mir_check -> Mir_emit; FAS_MIR_DUMP=1 prints the
   Mir. `--mir` is still accepted and changes nothing: it selected this
   backend while the old one (Native_gen) existed. *)

(* The command line is stable from v0.8 (SPEC §13): options keep their
   meaning, and the exit status is
     0  success
     1  the input was refused: unreadable, a parse error, a verifier error,
        a construct the native backend does not support, a link error, or a
        failed C compiler run (--llvm); one or more "fas-fir: ..." lines
        on stderr
     2  a usage error (unknown option, missing argument, options that do
        not go together); "fas-fir: ..." and a pointer to --help on stderr
     3  an internal error: a bug in fas-fir, reported as
        "fas-fir: internal error: ..." *)

let usage_text =
  "usage: fas-fir [-c [-fPIC | -fno-pic] | --check | --legalize | --emit-llvm] [-g] [--no-pie] [--hosted] [--llvm]\n\
  \              INPUT.fir [-o OUTPUT] [OBJECTS...]\n\
  \  (no flag)    build a static position-independent executable (default output: a.out);\n\
  \               OBJECTS (.o and .a files) are linked in\n\
  \  --no-pie     build a static executable at a fixed address instead\n\
  \  --hosted     link the executable on the C runtime (the startup files and libc of\n\
  \               $FAS_CC, default clang), for modules that call the C library\n\
  \  -c           build a position-independent ELF object (default output: INPUT with .o)\n\
  \  -fPIC        with -c: an object for a shared library (general-dynamic TLS, which\n\
  \               dlopen needs; the default, initial-exec, suits executables)\n\
  \  -g           add DWARF debug info (lines from the loc table, variables)\n\
  \  --check      parse and verify, print the canonical text\n\
  \  --legalize   print the module with wide and odd-width integers in limbs\n\
  \  --emit-llvm  print the module as LLVM IR\n\
  \  --llvm       build through LLVM ($FAS_CC, default clang) instead of the native backend\n\
  \  -o OUTPUT    the output file; for --check, --legalize and --emit-llvm, stdout by default\n\
  \  --help       print this help;  --version  print the version\n\
  \  INPUT may be - for stdin.\n\
  exit status: 0 success, 1 input refused (with a message), 2 usage error, 3 internal error\n"

type mode = Exe | Obj | Check | Legalize | Emit_llvm

let die fmt = Printf.ksprintf (fun s -> prerr_endline ("fas-fir: " ^ s); exit 1) fmt

let usage_error fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("fas-fir: " ^ s);
      prerr_endline "run 'fas-fir --help' for usage";
      exit 2)
    fmt

let internal fmt = Printf.ksprintf (fun s -> prerr_endline ("fas-fir: internal error: " ^ s); exit 3) fmt

let read_all ic =
  let b = Buffer.create 65536 in
  let chunk = Bytes.create 65536 in
  let rec go () =
    let n = input ic chunk 0 65536 in
    if n > 0 then begin Buffer.add_subbytes b chunk 0 n; go () end
  in
  go ();
  Buffer.contents b

(* A freestanding link has no C library: an undefined symbol there is most
   often a C function, so the message points at --hosted. *)
let freestanding_hint e =
  let has sub =
    let n = String.length sub in
    let rec at i = i + n <= String.length e && (String.sub e i n = sub || at (i + 1)) in
    at 0
  in
  if String.starts_with ~prefix:"native backend: linker: " e
     && (has "undefined symbol" || has "undefined non-default visibility symbol")
  then
    e ^ " (fas-fir links without the C library; use --hosted to link on libc)"
  else e

(* --hosted: the object without the Fas startup, then the C compiler's own
   link plan (`$FAS_CC -### ...`: crt1.o, crti.o, libc.a, libgcc ...) read as
   data and carried out by the Fas linker, as `fas --hosted` does
   (src/driver/hosted_link.ml). Static like the default link: -static-pie,
   or -static with --no-pie. *)
let hosted_link ~debug ~pie ~output ~extra ~iter_funcs ~has_asm m =
  let module L = Fas_linker in
  match Mir_emit.compile_object ~debug ~iter_funcs ~has_asm m with
  | Error _ as e -> e
  | Ok obj -> (
      let object_path = Filename.temp_file "fas-fir-hosted-" ".o" in
      Fun.protect
        ~finally:(fun () -> try Sys.remove object_path with Sys_error _ -> ())
        (fun () ->
          try
            Out_channel.with_open_bin object_path (fun oc -> Out_channel.output_bytes oc obj);
            let cc = Option.value (Sys.getenv_opt "FAS_CC") ~default:"clang" in
            let argv =
              [ cc; "-###"; "-fno-use-linker-plugin"; (if pie then "-static-pie" else "-static"); object_path ]
              @ extra @ [ "-lm"; "-o"; output ]
            in
            match Process.run (Array.of_list argv) with
            | Error failure ->
                Error (Printf.sprintf "--hosted: %s -### failed: %s" cc (String.trim failure.Process.stderr))
            | Ok (_, plan) ->
                let link = Hosted_link.linker_plan ~object_path plan in
                if link.output <> output || link.relocatable then
                  Error "--hosted: the C compiler changed the requested output"
                else begin
                  L.Static_link.link_input_files ~options:link.options ~entry:link.entry ?map_file:link.map_file
                    ~output link.inputs;
                  Ok ()
                end
          with
          | L.Error.Error msg -> Error ("--hosted: linker: " ^ msg)
          | Sys_error msg -> Error ("--hosted: " ^ msg)))

let main () =
  let mode = ref None and llvm = ref false and pie = ref None and debug = ref false and fpic = ref false and no_pic = ref false and hosted = ref false and input = ref None and output = ref None and extra = ref [] in
  let set_mode m flag =
    match !mode with
    | Some (_, f) when f <> flag -> usage_error "%s and %s do not go together" f flag
    | _ -> mode := Some (m, flag)
  in
  let rec args = function
    | [] -> ()
    | "-c" :: rest -> set_mode Obj "-c"; args rest
    | "--check" :: rest -> set_mode Check "--check"; args rest
    | "--legalize" :: rest -> set_mode Legalize "--legalize"; args rest
    | "--emit-llvm" :: rest -> set_mode Emit_llvm "--emit-llvm"; args rest
    | "--llvm" :: rest -> llvm := true; args rest
    | "--mir" :: rest -> args rest
    | "--hosted" :: rest -> hosted := true; args rest
    | "--pie" :: rest -> pie := Some true; args rest
    | "--no-pie" :: rest -> pie := Some false; args rest
    | "-g" :: rest -> debug := true; args rest
    | "-fPIC" :: rest -> fpic := true; no_pic := false; args rest
    | "-fno-pic" :: rest -> fpic := false; no_pic := true; args rest
    | [ "-o" ] -> usage_error "-o needs a file name"
    | "-o" :: o :: rest ->
        if !output <> None then usage_error "-o given twice";
        output := Some o;
        args rest
    | ("-h" | "--help") :: _ -> print_string usage_text; exit 0
    | "--version" :: _ ->
        Printf.printf "fas-fir %s (%s)\n" Driver.version Build_commit.commit;
        exit 0
    | a :: rest when !input = None && (a = "-" || not (String.starts_with ~prefix:"-" a)) ->
        input := Some a; args rest
    | a :: rest when not (String.starts_with ~prefix:"-" a) ->
        if Filename.check_suffix a ".fir" then usage_error "one .fir input at a time (%s and %s)" (Option.get !input) a;
        extra := a :: !extra;
        args rest
    | a :: _ -> usage_error "unknown option %s" a
  in
  args (List.tl (Array.to_list Sys.argv));
  let mode, flag = Option.value !mode ~default:(Exe, "") in
  if !extra <> [] && mode <> Exe then usage_error "%s takes no object inputs (%s)" flag (List.hd (List.rev !extra));
  if (!fpic || !no_pic) && mode <> Obj then usage_error "-fPIC and -fno-pic are for objects (-c)";
  if !pie <> None && mode <> Exe then usage_error "--pie and --no-pie are for executables";
  if !hosted && mode <> Exe then usage_error "--hosted is for executables";
  (* --llvm links through the C compiler: it is always hosted *)
  if !llvm && (mode = Check || mode = Legalize || mode = Emit_llvm) then usage_error "--llvm builds; it does not go with %s" flag;
  let mode = ref mode and pie = ref (Option.value !pie ~default:true) in
  let path = match !input with Some p -> p | None -> usage_error "no input file" in
  let name = if path = "-" then "<stdin>" else path in
  let verify errors =
    match errors with
    | [] -> ()
    | errs -> die "%s: verifier:\n%s" name (String.concat "\n" errs)
  in
  let input_error f =
    try f () with
    | Fir_parse.Error (line, msg) -> die "%s:%d: %s" name line msg
    | Sys_error e -> die "%s" e
  in
  let unsupported msg = die "%s: not supported by the native backend yet: %s" name msg in
  let run_native stream =
    let m = stream.Fir_stream.modul in
    let iter_funcs = Fir_stream.iter stream in
    verify (Fir_stream.verify stream);
    (* The CLI's initial normalization is also used by --legalize. Collect
       all wide helpers before native runtime helpers, keeping symbol
       order and section contents identical to the in-memory path. *)
    (try
      Fir_stream.normalize stream;
      Fir_stream.wide stream
    with Fir_scalarize.Unsupported e | Fir_wide.Unsupported e -> unsupported e);
    (match Fir_stream.verify stream with
    | [] -> ()
    | errs -> internal "%s: verifier, after Fir_wide:\n%s" name (String.concat "\n" errs));
    if !mode = Exe &&
      not (Fir.Vec.fold (fun acc (f : Fir.func) -> acc || (f.name = "main" && f.defined)) false m.funcs)
    then die "%s: no @main to start (use -c for an object)" name;
    let finish () =
      try
        if m.triple <> Target_layout.x86_64_linux.triple then
          Error ("native backend: only Linux x86-64 object emission is supported (target " ^ m.triple ^ ")")
        else begin
          Fir_stream.normalize stream;
          if Mir_runtime.uses_fma iter_funcs then begin
            Mir_runtime.add_helpers m;
            Fir_stream.wide stream
          end;
          Fir_stream.wide stream;
          match Fir_stream.verify stream with
          | [] -> Fir_ct.check ~iter_funcs m; Ok ()
          | errs -> Error ("native backend internal error: Fir after scalarization and Fir_wide:\n" ^ String.concat "\n" errs)
        end
      with
      | Fir_scalarize.Unsupported e | Fir_wide.Unsupported e | Fir_ct.Failure e ->
          Error ("native backend: " ^ e)
      | Invalid_argument e -> Error ("native backend internal error: " ^ e)
    in
    let result =
      match finish () with
      | Error _ as e -> e
      | Ok () ->
          let has_asm = Fir_stream.has_asm stream in
          match !mode with
          | Obj ->
              let write_obj obj =
                let out = Option.value !output ~default:(Filename.remove_extension (Filename.basename name) ^ ".o") in
                Out_channel.with_open_bin out (fun oc -> Out_channel.output_bytes oc obj)
              in
              Result.map write_obj (Mir_emit.compile_object ~debug:!debug ~pic:(not !no_pic) ~tls_gd:!fpic ~iter_funcs ~has_asm m)
          | Exe ->
              let output = Option.value !output ~default:"a.out" in
              if !hosted then hosted_link ~debug:!debug ~pie:!pie ~output ~extra:(List.rev !extra) ~iter_funcs ~has_asm m
              else
                let inputs = List.rev_map Fas_linker.Input.file !extra in
                let options = if !pie then Some { Fas_linker.Options.default with mode = Static_pie } else None in
                Result.map_error freestanding_hint
                  (Mir_emit.link_files ~debug:!debug ~iter_funcs ~has_asm ~inputs ?options ~output m)
          | _ -> assert false
    in
    match result with
    | Ok () -> ()
    | Error e when String.starts_with ~prefix:"native backend internal error" e -> internal "%s" e
    | Error e -> die "%s" e
  in
  if not !llvm && (!mode = Obj || !mode = Exe) then begin
    input_error (fun () ->
      let read ch = Fir_stream.with_input ch run_native in
      if path = "-" then read stdin else In_channel.with_open_bin path read);
    exit 0
  end;
  let text =
    input_error (fun () -> if path = "-" then read_all stdin else In_channel.with_open_bin path In_channel.input_all)
  in
  let m = input_error (fun () -> Fir_parse.parse_module text) in
  verify (Fir_verify.verify m);
  (* --check, --legalize and --emit-llvm print to -o, else stdout *)
  let print_out t =
    (match !output with
    | Some o -> (try Out_channel.with_open_bin o (fun oc -> output_string oc t) with Sys_error e -> die "%s" e)
    | None -> print_string t);
    exit 0
  in
  if !mode = Check then print_out (Fir_print.module_to_string m);
  let llvm_text () =
    match Fir_llvm.render_bounded ~debug_info:!debug ~budget:max_int m with
    | Ok t -> t
    | Error e -> die "%s: %s" name e
  in
  if !mode = Emit_llvm then print_out (llvm_text ());
  if !llvm then begin
    (* through LLVM: the text into a temporary .ll, then the C compiler *)
    let ll = Filename.temp_file "fas-fir" ".ll" in
    Out_channel.with_open_bin ll (fun oc -> output_string oc (llvm_text ()));
    let cc = Option.value (Sys.getenv_opt "FAS_CC") ~default:"clang" in
    let q = Filename.quote in
    let cmd =
      match !mode with
      | Obj ->
          let out = Option.value !output ~default:(Filename.remove_extension (Filename.basename name) ^ ".o") in
          Printf.sprintf "%s -O2 -w -fPIC %s -c -x ir %s -o %s" (q cc) (if !debug then "-g" else "") (q ll) (q out)
      | _ ->
          Printf.sprintf "%s -O2 -w %s %s -x ir %s -x none %s -lm -o %s" (q cc) (if !pie then "-pie" else "-no-pie")
            (if !debug then "-g" else "") (q ll)
            (String.concat " " (List.rev_map q !extra))
            (q (Option.value !output ~default:"a.out"))
    in
    let status = Sys.command cmd in
    Sys.remove ll;
    if status <> 0 then die "%s: %s failed" name cc;
    exit 0
  end;
  (* --legalize shows Fir_wide alone, without vector scalarization. *)
  Fir_trapdiv.run ~hw:true m;
  (try Fir_wide.run m with Fir_wide.Unsupported msg -> unsupported msg);
  (match Fir_verify.verify m with
  | [] -> ()
  | errs -> internal "%s: verifier, after Fir_wide:\n%s" name (String.concat "\n" errs));
  print_out (Fir_print.module_to_string m)

(* anything that escapes is a bug: exit 3, never an OCaml backtrace *)
let () =
  try main () with
  | Stack_overflow -> internal "stack overflow"
  | Out_of_memory -> internal "out of memory"
  | e -> internal "%s" (Printexc.to_string e)
