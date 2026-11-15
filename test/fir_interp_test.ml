(* The Fir interpreter as an oracle over the hand-written programs of
   test/fas_fir: each NAME.fir, linked with putnum.fir, must print NAME.out
   and exit with its "; exit: N" status (default 0), as the native and LLVM
   builds do in test/fas_fir.sh. Programs that need what the interpreter
   cannot do (inline assembly, unwinding through it) are skipped, each for
   the reason given in [skip]. *)

let skip =
  [
    ("asm_goto", "inline assembly");
    ("asm_link", "inline assembly");
    ("module_asm", "module-level assembly");
    ("unwind", "unwinding needs the native unwinder");
  ]

let dir = "fas_fir"
let read path = In_channel.with_open_bin path In_channel.input_all

let parse path =
  match Fir_parse.parse_module (read path) with
  | m -> m
  | exception Fir_parse.Error (l, msg) ->
      failwith (Printf.sprintf "%s:%d: %s" path l msg)

let expected_exit text =
  List.fold_left
    (fun acc line ->
      match Scanf.sscanf_opt line "; exit: %d" Fun.id with Some n -> n | None -> acc)
    0
    (String.split_on_char '\n' text)

let () =
  let names =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".fir" && f <> "putnum.fir")
    |> List.map Filename.chop_extension
    |> List.sort compare
  in
  let fails = ref 0 and ran = ref 0 and skipped = ref 0 in
  List.iter
    (fun name ->
      if List.mem_assoc name skip then incr skipped
      else begin
        incr ran;
        let path = Filename.concat dir (name ^ ".fir") in
        let want_out = read (Filename.concat dir (name ^ ".out")) in
        let want_rc = expected_exit (read path) in
        match
          Fir_interp.run_program
            [ parse path; parse (Filename.concat dir "putnum.fir") ]
        with
        | r ->
            if r.status <> want_rc then begin
              Printf.printf "FAIL %s: exit %d, expected %d\n" name r.status want_rc;
              incr fails
            end
            else if r.stdout <> want_out then begin
              Printf.printf "FAIL %s: stdout differs\n--- want\n%s--- got\n%s" name
                want_out r.stdout;
              incr fails
            end
        | exception Fir_interp.Error msg ->
            Printf.printf "FAIL %s: %s\n" name msg;
            incr fails
        | exception e ->
            Printf.printf "FAIL %s: %s\n" name (Printexc.to_string e);
            incr fails
      end)
    names;
  Printf.printf "fir interp: %d programs, %d failed, %d skipped\n" !ran !fails !skipped;
  if !fails > 0 then exit 1
