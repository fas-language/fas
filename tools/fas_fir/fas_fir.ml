(* fas-fir: compile a Fir text module (src/fir/SPEC.md, docs FIR.md) with the
   native backend. This is the entry point for front ends written outside
   this compiler, such as a C compiler written in Fas that prints Fir text.

     fas-fir prog.fir -o prog     static Linux x86-64 executable; the
                                  module must define @main, which _start
                                  calls with (argc i32, argv ptr) and whose
                                  i32 result becomes the exit status
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
  "usage: fas-fir [-c [-fPIC] | --check | --legalize | --emit-llvm] [-g] [--no-pie] [--llvm]\n\
  \              INPUT.fir [-o OUTPUT] [OBJECTS...]\n\
  \  (no flag)    build a static position-independent executable (default output: a.out);\n\
  \               OBJECTS (.o and .a files) are linked in\n\
  \  --no-pie     build a static executable at a fixed address instead\n\
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

let main () =
  let mode = ref None and llvm = ref false and pie = ref None and debug = ref false and fpic = ref false and input = ref None and output = ref None and extra = ref [] in
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
    | "--pie" :: rest -> pie := Some true; args rest
    | "--no-pie" :: rest -> pie := Some false; args rest
    | "-g" :: rest -> debug := true; args rest
    | "-fPIC" :: rest -> fpic := true; args rest
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
  if !fpic && mode <> Obj then usage_error "-fPIC is for objects (-c)";
  if !pie <> None && mode <> Exe then usage_error "--pie and --no-pie are for executables";
  if !llvm && (mode = Check || mode = Legalize || mode = Emit_llvm) then usage_error "--llvm builds; it does not go with %s" flag;
  let mode = ref mode and pie = ref (Option.value !pie ~default:true) in
  let path = match !input with Some p -> p | None -> usage_error "no input file" in
  let text =
    try if path = "-" then read_all stdin else In_channel.with_open_bin path In_channel.input_all
    with Sys_error e -> die "%s" e
  in
  let name = if path = "-" then "<stdin>" else path in
  let m =
    try Fir_parse.parse_module text
    with Fir_parse.Error (line, msg) -> die "%s:%d: %s" name line msg
  in
  (match Fir_verify.verify m with
  | [] -> ()
  | errs -> die "%s: verifier:\n%s" name (String.concat "\n" errs));
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
  (* the machine-IR backend has no vectors: lanes first, so that vectors
     of wide integers (<2 x i128>) become wide scalars for Fir_wide;
     --legalize shows Fir_wide alone *)
  if !mode <> Legalize then (
    try Fir_scalarize.run m
    with Fir_scalarize.Unsupported msg -> die "%s: not supported by the native backend yet: %s" name msg);
  (* the native backend computes in i1..i64 only *)
  (try Fir_wide.run m
   with Fir_wide.Unsupported msg -> die "%s: not supported by the native backend yet: %s" name msg);
  (match Fir_verify.verify m with
  | [] -> ()
  | errs -> internal "%s: verifier, after Fir_wide:\n%s" name (String.concat "\n" errs));
  if !mode = Legalize then print_out (Fir_print.module_to_string m);
  let write_obj obj =
    let out = Option.value !output ~default:(Filename.remove_extension (Filename.basename name) ^ ".o") in
    try Out_channel.with_open_bin out (fun oc -> Out_channel.output_bytes oc obj) with Sys_error e -> die "%s" e
  in
  let need_main () =
    if not (Fir.Vec.fold (fun acc (f : Fir.func) -> acc || (f.name = "main" && f.defined)) false m.funcs) then
      die "%s: no @main to start (use -c for an object)" name
  in
  let result =
    match !mode with
    | Obj -> Result.map write_obj (Mir_emit.compile_object ~debug:!debug ~tls_gd:!fpic m)
    | Exe | Check | Legalize | Emit_llvm ->
        need_main ();
        let inputs = List.rev_map Fas_linker.Input.file !extra in
        let options = if !pie then Some { Fas_linker.Options.default with mode = Static_pie } else None in
        Mir_emit.link_files ~debug:!debug ~inputs ?options ~output:(Option.value !output ~default:"a.out") m
  in
  match result with
  | Ok () -> ()
  | Error e when String.starts_with ~prefix:"native backend internal error" e -> internal "%s" e
  | Error e -> die "%s" e

(* anything that escapes is a bug: exit 3, never an OCaml backtrace *)
let () =
  try main () with
  | Stack_overflow -> internal "stack overflow"
  | Out_of_memory -> internal "out of memory"
  | e -> internal "%s" (Printexc.to_string e)
