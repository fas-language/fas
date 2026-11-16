let load_source path =
  try
    let channel = open_in_bin path in
    let text =
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () -> really_input_string channel (in_channel_length channel))
    in
    Some (Source.create ~file:path ~text)
  with Sys_error _ -> None

let report (config : Cli.t) diagnostics =
  match config.diagnostics with
  | Cli.Text ->
      let sources = Hashtbl.create 4 in
      let source (diagnostic : Diag.t) =
        let file = diagnostic.primary.Span.file in
        match Hashtbl.find_opt sources file with
        | Some source -> source
        | None ->
            let source = load_source file in
            Hashtbl.add sources file source;
            source
      in
      prerr_endline
        (String.concat "\n"
           (List.map
              (fun diagnostic -> Diag.render ~source:(source diagnostic) diagnostic)
              diagnostics))
  | Cli.Json -> prerr_endline (Diag.render_json diagnostics)

(* exit status for failed diagnostics: 3 when the compiler itself is at
   fault (an internal error, a native backend check such as the
   machine-code constant-time check included), else 1 *)
let failure_status diagnostics =
  let mentions needle (d : Diag.t) =
    let n = String.length needle and m = String.length d.Diag.message in
    let rec go i =
      i + n <= m && (String.sub d.Diag.message i n = needle || go (i + 1))
    in
    go 0
  in
  if
    List.exists
      (fun d -> d.Diag.severity = Diag.Error && mentions "internal error" d)
      diagnostics
  then 3
  else 1

let run_program (config : Cli.t) args =
  let suffix = match config.Cli.os with Target_layout.Linux -> "" in
  match Filename.temp_file "fas-run-" suffix with
  | exception Sys_error message ->
      prerr_endline ("fas: " ^ message);
      1
  | path ->
      Fun.protect
        ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
        (fun () ->
          match
            Driver.run { config with Cli.output = path; output_explicit = true }
          with
          | Error diagnostics ->
              report config diagnostics;
              failure_status diagnostics
          | Ok _ -> (
              let name =
                Option.value ~default:config.input
                  (Filename.chop_suffix_opt ~suffix:".fas" config.input)
              in
              let program, argv = (path, name :: args) in
              match Process.run_program program (Array.of_list argv) with
              | Ok status -> status
              | Error message ->
                  prerr_endline ("fas: cannot run " ^ path ^ ": " ^ message);
                  1))

let select_os = function
  | Ok (Cli.Run config | Cli.Check config | Cli.Run_program (config, _)) ->
      Target_layout.select config.Cli.os
  | Ok (Cli.Lsp options) -> Target_layout.select options.Cli.lsp_os
  | Ok (Cli.Help | Cli.Version) | Error _ -> ()

let () =
  let parsed = Cli.parse Sys.argv in
  select_os parsed;
  match parsed with
  | Ok Cli.Help -> print_endline Cli.usage
  | Ok Cli.Version -> List.iter print_endline (Driver.version_lines ())
  | Ok (Cli.Run config) -> (
      match Driver.run config with
      | Ok output -> print_string output
      | Error diagnostics ->
          report config diagnostics;
          exit (failure_status diagnostics))
  | Ok (Cli.Check config) -> (
      match Driver.check config with
      | Ok checked ->
          Driver.release checked;
          if config.diagnostics = Cli.Json then prerr_endline (Diag.render_json []);
          exit 0
      | Error diagnostics ->
          report config diagnostics;
          exit (failure_status diagnostics))
  | Ok (Cli.Lsp options) -> exit (Lsp.run options)
  | Ok (Cli.Run_program (config, args)) -> exit (run_program config args)
  | Error message ->
      prerr_endline message;
      exit 2
