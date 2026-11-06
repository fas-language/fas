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
     fas-fir --mir ...            the same, through the v0.4 machine IR
                                  (Fir_scalarize -> Fir_wide -> Mir_isel ->
                                  Mir_ra -> Mir_check -> Mir_emit) instead
                                  of Fir_to_ir and Native_gen;
                                  FAS_MIR_DUMP=1 prints it

   "-" reads the module from stdin. Extra .o/.a inputs after the .fir file
   are linked in (executables only). The pipeline is
   parse -> verify -> Fir_wide -> Fir_to_ir -> native backend, the same path the
   FAS_FIR=roundtrip test mode exercises on every native test. *)

let usage () =
  prerr_string
    "usage: fas-fir [-c | --check | --legalize] INPUT.fir [-o OUTPUT] [OBJECTS...]\n\
    \  (no flag)  build a static executable (default output: a.out)\n\
    \  -c         build a position-independent ELF object\n\
    \  --check    parse and verify, print the canonical text to stdout\n\
    \  --legalize print the module with wide and odd-width integers in limbs\n\
    \  --mir      generate code through the machine IR (roadmap v0.4)\n\
    \  INPUT may be - for stdin\n";
  exit 2

type mode = Exe | Obj | Check | Legalize

let die fmt = Printf.ksprintf (fun s -> prerr_endline ("fas-fir: " ^ s); exit 1) fmt

let read_all ic =
  let b = Buffer.create 65536 in
  let chunk = Bytes.create 65536 in
  let rec go () =
    let n = input ic chunk 0 65536 in
    if n > 0 then begin Buffer.add_subbytes b chunk 0 n; go () end
  in
  go ();
  Buffer.contents b

let () =
  let mode = ref Exe and input = ref None and output = ref None and extra = ref [] and mir = ref false in
  let rec args = function
    | [] -> ()
    | "-c" :: rest -> mode := Obj; args rest
    | "--check" :: rest -> mode := Check; args rest
    | "--legalize" :: rest -> mode := Legalize; args rest
    | "--mir" :: rest -> mir := true; args rest
    | "-o" :: o :: rest -> output := Some o; args rest
    | ("-h" | "--help") :: _ -> usage ()
    | a :: rest when !input = None && (a = "-" || not (String.starts_with ~prefix:"-" a)) ->
        input := Some a; args rest
    | a :: rest when not (String.starts_with ~prefix:"-" a) -> extra := a :: !extra; args rest
    | a :: _ -> die "unknown option %s" a
  in
  args (List.tl (Array.to_list Sys.argv));
  let path = match !input with Some p -> p | None -> usage () in
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
  if !mode = Check then begin
    print_string (Fir_print.module_to_string m);
    exit 0
  end;
  (* the machine-IR backend has no vectors: lanes first, so that vectors
     of wide integers (<2 x i128>) become wide scalars for Fir_wide *)
  if !mir then (
    try Fir_scalarize.run m
    with Fir_scalarize.Unsupported msg -> die "%s: not supported by the mir backend yet: %s" name msg);
  (* the native backend computes in i1..i64 only *)
  (try Fir_wide.run m
   with Fir_wide.Unsupported msg -> die "%s: not supported by the native backend yet: %s" name msg);
  (match Fir_verify.verify m with
  | [] -> ()
  | errs -> die "%s: verifier, after Fir_wide (a compiler bug):\n%s" name (String.concat "\n" errs));
  if !mode = Legalize then begin
    print_string (Fir_print.module_to_string m);
    exit 0
  end;
  let write_obj obj =
    let out = Option.value !output ~default:(Filename.remove_extension (Filename.basename name) ^ ".o") in
    Out_channel.with_open_bin out (fun oc -> Out_channel.output_bytes oc obj)
  in
  let need_main () =
    if not (Fir.Vec.fold (fun acc (f : Fir.func) -> acc || (f.name = "main" && f.defined)) false m.funcs) then
      die "%s: no @main to start (use -c for an object)" name
  in
  if !mir then begin
    let result =
      match !mode with
      | Obj -> Result.map write_obj (Mir_emit.compile_object m)
      | Exe | Check | Legalize ->
          need_main ();
          let inputs = List.rev_map Fas_linker.Input.file !extra in
          Mir_emit.link_files ~inputs ~output:(Option.value !output ~default:"a.out") m
    in
    (match result with Ok () -> () | Error e -> die "%s" e);
    exit 0
  end;
  (* Fir_to_ir takes the data layout and source names from a "like" module *)
  let like =
    { Ir.target_triple = m.Fir.triple;
      data_layout = (Target_layout.current ()).llvm_data_layout;
      structs = []; globals = []; funcs = [];
      no_inline_functions = []; inline_functions = [] }
  in
  let ir, symbols =
    try (Fir_to_ir.convert ~native:true ~like m, Fir_to_ir.symbols m)
    with Fir_to_ir.Unsupported msg -> die "%s: not supported by the native backend yet: %s" name msg
  in
  let result =
    match !mode with
    | Obj -> (
        match Native_gen.compile_object ~symbols ir with
        | Error e -> Error e
        | Ok obj ->
            let out = Option.value !output ~default:(Filename.remove_extension (Filename.basename name) ^ ".o") in
            Out_channel.with_open_bin out (fun oc -> Out_channel.output_bytes oc obj);
            Ok ())
    | Exe | Check | Legalize ->
        if not (Fir.Vec.fold (fun acc (f : Fir.func) -> acc || (f.name = "main" && f.defined)) false m.funcs) then
          die "%s: no @main to start (use -c for an object)" name;
        let output = Option.value !output ~default:"a.out" in
        let inputs = List.rev_map Fas_linker.Input.file !extra in
        Native_gen.link_files ~symbols ~inputs ~output ir
  in
  match result with Ok () -> () | Error e -> die "%s" e
