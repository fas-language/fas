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
  "usage: fas-fir [-c [-fPIC | -fno-pic] [-mno-red-zone] [-mcmodel=kernel] [-mno-sse | -mgeneral-regs-only] [-mno-80387] [--no-unwind-tables] [HARDENING...] | --check | --legalize | --emit-llvm] [-g] [--no-pie] [--hosted] [--llvm]\n\
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
  \  HARDENING (with -c), the kernel's mitigations, as gcc and clang spell them:\n\
  \    -mretpoline-external-thunk   indirect calls and jumps through __x86_indirect_thunk_REG\n\
  \    -mretpoline                  the same through local thunks __fas_retpoline_REG\n\
  \    -mindirect-branch-cs-prefix  a cs prefix on those through r8..r15\n\
  \    -mfunction-return=thunk-extern  returns jump to __x86_return_thunk\n\
  \    -fcf-protection=branch       endbr64 at function entries and indirect-jump targets\n\
  \    -fpatchable-function-entry=N[,M]  N nops at each entry, M of them before it\n\
  \  -mno-80387   with -c: refuse x87 code (f80 values, frem)\n\
  \  -mstack-alignment=N  with -c: assume rsp only N-aligned at entry (8 or 16, the\n\
  \               default; a larger N gets 16), padding anything aligned above it\n\
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
  let no_sse = ref false and general_regs_only = ref false and no_x87 = ref false in
  let unwind_tables = ref true in
  let red_zone = ref true and kernel_model = ref false and fold_branches = ref false in
  let hard = ref Mir_emit.no_hardening in
  let stack_align = ref 16 and regparm = ref 0 in
  let function_sections = ref false in
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
    | "--fold-branches" :: rest -> fold_branches := true; args rest
    | "-ffunction-sections" :: rest -> function_sections := true; args rest
    | "--hosted" :: rest -> hosted := true; args rest
    | "--pie" :: rest -> pie := Some true; args rest
    | "--no-pie" :: rest -> pie := Some false; args rest
    | "-g" :: rest -> debug := true; args rest
    | "--no-unwind-tables" :: rest -> unwind_tables := false; args rest
    | "-mno-sse" :: rest -> no_sse := true; args rest
    | "-mgeneral-regs-only" :: rest -> no_sse := true; general_regs_only := true; args rest
    | ("-mno-80387" | "-mno-x87") :: rest -> no_x87 := true; args rest
    | "-mno-red-zone" :: rest -> red_zone := false; args rest
    | "-mcmodel=kernel" :: rest -> kernel_model := true; args rest
    | "-mretpoline-external-thunk" :: rest -> hard := { !hard with thunk_calls = true; thunk_local = false }; args rest
    | "-mretpoline" :: rest -> hard := { !hard with thunk_calls = true; thunk_local = true }; args rest
    | "-mindirect-branch-cs-prefix" :: rest -> hard := { !hard with thunk_cs_prefix = true }; args rest
    | "-mfunction-return=thunk-extern" :: rest -> hard := { !hard with return_thunk = true }; args rest
    | "-mfunction-return=keep" :: rest -> hard := { !hard with return_thunk = false }; args rest
    | "-fcf-protection=branch" :: rest -> hard := { !hard with endbr = true }; args rest
    | "-fcf-protection=none" :: rest -> hard := { !hard with endbr = false }; args rest
    | a :: rest when String.starts_with ~prefix:"-fpatchable-function-entry=" a ->
        let v = String.sub a 27 (String.length a - 27) in
        let num s = match int_of_string_opt s with Some n when n >= 0 && n <= 0xffff && String.for_all (fun c -> c >= '0' && c <= '9') s -> n | _ -> usage_error "bad %s" a in
        let n, before =
          match String.split_on_char ',' v with
          | [ n ] -> (num n, 0)
          | [ n; m ] -> (num n, num m)
          | _ -> usage_error "bad %s" a
        in
        if before > n then usage_error "%s: M is more than N" a;
        hard := { !hard with patch_entry = (n, before) };
        args rest
    | a :: rest when String.starts_with ~prefix:"-mstack-alignment=" a ->
        (* clang's flag (gcc: -mpreferred-stack-boundary=log2 N): rsp at
           entry is N-aligned; the backend never assumes more than 16 *)
        let v = String.sub a 18 (String.length a - 18) in
        (match int_of_string_opt v with
         | Some n when String.for_all (fun c -> c >= '0' && c <= '9') v && n >= 4 && n land (n - 1) = 0 -> stack_align := min n 16
         | _ -> usage_error "bad %s (a power of two, at least 4)" a);
        args rest
    | ("-mregparm=0" | "-mregparm=1" | "-mregparm=2" | "-mregparm=3" as a) :: rest ->
        regparm := Char.code a.[10] - Char.code '0'; args rest
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
  if (not !red_zone || !kernel_model || !no_sse || !no_x87 || { !hard with thunks_used = Mir_emit.no_hardening.thunks_used } <> Mir_emit.no_hardening || !stack_align <> 16) && (mode <> Obj || !llvm) then
    usage_error "x86-64 code-model and register options require native object output (-c)";
  if not !unwind_tables && (mode <> Obj || !llvm) then
    usage_error "--no-unwind-tables requires native object output (-c)";
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
  let write_obj obj =
    let out = Option.value !output ~default:(Filename.remove_extension (Filename.basename name) ^ ".o") in
    Out_channel.with_open_bin out (fun oc -> Out_channel.output_bytes oc obj)
  in
  let run_i386 stream =
    if !mode <> Obj then unsupported "i386 executable emission (use -c)";
    if !debug then unsupported "i386 debug information";
    if (stream.Fir_stream.modul.triple = Fir.i386_code16) && !fpic then unsupported "real-mode PIC";
    if ((!hard).thunk_calls && not (!hard).thunk_local) || (!hard).return_thunk ||
       (!hard).patch_entry <> (0, 0) then
      unsupported "i386 external thunks or patchable entries";
    verify (Fir_stream.verify stream);
    Fir_stream.drop_unused stream;
    if !fold_branches then Fir_stream.fold stream;
    let pinned = Fir_stream.pinned_functions stream in
    Fir_stream.map stream (fun f -> if Fir_stream.can_fold pinned f then I386_opt.run f else f);
    I386_opt.inline_helpers stream pinned;
    Fir_stream.drop_unused stream;
    verify (Fir_stream.verify stream);
    let m = stream.Fir_stream.modul in
    match I386.compile_object ~stack_align:!stack_align ~regparm:!regparm ~function_sections:!function_sections
            ~endbr:(!hard).endbr ~pic:!fpic ~thunk_calls:(!hard).thunk_calls
            ~iter_funcs:(Fir_stream.iter stream) m with
    | Ok obj -> write_obj obj
    | Error e -> die "%s" e
  in
  let run_x86_64 stream =
    if !regparm <> 0 then usage_error "-mregparm requires i386";
    if !function_sections then usage_error "-ffunction-sections requires i386";
    if !stack_align < 8 then usage_error "x86-64 stack alignment must be at least 8";

    let m = stream.Fir_stream.modul in
    let iter_funcs = Fir_stream.iter stream in
    verify (Fir_stream.verify stream);
    (* The CLI's initial normalization is also used by --legalize. Collect
       all wide helpers before native runtime helpers, keeping symbol
       order and section contents identical to the in-memory path. *)
    (try
      if !fold_branches then Fir_stream.fold stream;
      (* Like gcc and clang at every -O level, never emit an unreferenced
         C-local definition: header statics may call functions that a
         freestanding image (the kernel decompressor, EFI stub) never links. *)
      if !fold_branches && !kernel_model then Fir_stream.fold_kernel_calls stream
      else Fir_stream.drop_unused stream;
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
              Result.map write_obj (Mir_emit.compile_object ~debug:!debug ~red_zone:!red_zone ~hard:!hard ~stack_align:!stack_align ~kernel_model:!kernel_model
                ~unwind_tables:!unwind_tables ~no_sse:!no_sse ~general_regs_only:!general_regs_only ~no_x87:!no_x87 ~pic:(not !no_pic) ~tls_gd:!fpic ~iter_funcs ~has_asm m)
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
      let read ch = Fir_stream.with_input ch (fun stream ->
        if Fir.is_i386 stream.Fir_stream.modul.triple then run_i386 stream else run_x86_64 stream) in
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
