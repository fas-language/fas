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

let run_program (config : Cli.t) args =
  let suffix =
    match config.Cli.os with
    | Target_layout.Windows -> ".exe"
    | Target_layout.Wasi -> ".wasm"
    | Target_layout.Linux -> ""
  in
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
              1
          | Ok _ -> (
              let name =
                Option.value ~default:config.input
                  (Filename.chop_suffix_opt ~suffix:".fas" config.input)
              in
              let program, argv =
                match config.Cli.os with
                | Target_layout.Wasi ->
                    let runner =
                      match Sys.getenv_opt "FAS_WASMTIME" with
                      | Some runner when runner <> "" -> runner
                      | _ -> "wasmtime"
                    in
                    ( Driver.executable_path runner,
                      runner :: "run" :: "--dir=." :: "--argv0" :: name :: path :: args
                    )
                | Target_layout.Linux | Target_layout.Windows -> (path, name :: args)
              in
              match Process.run_program program (Array.of_list argv) with
              | Ok status -> status
              | Error message ->
                  prerr_endline ("fas: cannot run " ^ path ^ ": " ^ message);
                  1))

let load_baseline (options : Cli.perf_options) =
  match options.compare with
  | None -> Ok None
  | Some path -> Result.map Option.some (Perf_report.load_baseline path)

let print_report (options : Cli.perf_options) baseline result =
  let diff =
    match baseline with
    | None -> Ok None
    | Some baseline -> Result.map Option.some (Perf_report.compare_with baseline result)
  in
  match diff with
  | Error message -> Error message
  | Ok diff ->
      if options.json then print_endline (Perf_report.json_report ?diff result)
      else List.iter prerr_endline (Perf_report.text_lines ?diff result);
      Ok ()

let native_rows ~link_cache (config : Cli.t) names dir =
  if not (Perf_tools.mca_available ()) then None
  else
    let exe = Filename.concat dir "native" in
    let remarks = Filename.concat dir "native-remarks.yaml" in
    let regalloc = Filename.concat dir "native-regalloc.yaml" in
    match
      Driver.run ~remarks ~llc_remarks:regalloc ~c_debug:true ~link_cache
        {
          config with
          Cli.debug = true;
          keep = false;
          target_cpu = Some "native";
          target = Cli.Portable;
          output = exe;
          output_explicit = true;
        }
    with
    | Error _ -> None
    | Ok _ -> (
        match Perf.located ~input:config.input names exe with
        | Ok (rows, _) ->
            Some
              {
                Perf.other = rows;
                other_notes =
                  Perf_tools.read_remarks remarks @ Perf_tools.read_remarks regalloc;
              }
        | Error _ -> None)

let program_name (config : Cli.t) =
  Option.value ~default:config.input
    (Filename.chop_suffix_opt ~suffix:".fas" config.input)

let perf_command (config : Cli.t) (options : Cli.perf_options) baseline args =
  match Filename.temp_dir "fas-perf-" "" with
  | exception Sys_error message ->
      prerr_endline ("fas: " ^ message);
      1
  | dir ->
      let exe = Filename.concat dir "program" in
      let remarks = Filename.concat dir "remarks.yaml" in
      let regalloc = Filename.concat dir "regalloc.yaml" in
      let data = Filename.concat dir "perf.data" in
      let link_cache = Driver.new_link_cache () in
      let llvm = ref "" in
      let build =
        Driver.run ~remarks ~llc_remarks:regalloc ~c_debug:true
          ~llvm_text:(fun text -> llvm := text)
          ~link_cache
          { config with Cli.debug = true; output = exe; output_explicit = true }
      in
      let status =
        match build with
        | Error diagnostics ->
            report config diagnostics;
            1
        | Ok _ -> (
            let names = Perf_tools.display_names !llvm in
            match Perf.located ~input:config.input names exe with
            | Error message ->
                prerr_endline message;
                1
            | Ok (rows, starts) -> (
                match Perf.select options.asm options.copy rows with
                | Error message ->
                    prerr_endline message;
                    2
                | Ok view -> (
                    let notes =
                      Perf_tools.read_remarks remarks @ Perf_tools.read_remarks regalloc
                    in
                    let program =
                      {
                        Perf.input = config.input;
                        exe;
                        names;
                        notes;
                        rows;
                        starts;
                        target = native_rows ~link_cache config names dir;
                      }
                    in
                    let show result =
                      match Result.bind result (print_report options baseline) with
                      | Ok () -> 0
                      | Error message ->
                          prerr_endline message;
                          1
                    in
                    if options.static then show (Perf.static_report program view)
                    else
                      let perf = Driver.tool "FAS_PERF" "perf" in
                      match Perf_tools.record ~perf ~data exe args with
                      | Error reason ->
                          ignore (show (Perf.static_report program view));
                          prerr_endline ("fas perf: no run: " ^ reason);
                          1
                      | Ok (status, text) -> (
                          match
                            show
                              (Perf.profile program view
                                 ~top:(Option.value options.top ~default:5)
                                 ~status text)
                          with
                          | 0 -> status
                          | failed -> failed))))
      in
      (if not config.keep then
         try
           Array.iter
             (fun name -> Sys.remove (Filename.concat dir name))
             (Sys.readdir dir);
           Sys.rmdir dir
         with Sys_error _ -> ());
      status

let select_os = function
  | Ok (Cli.Run config | Cli.Check config | Cli.Run_program (config, _)) ->
      Target_layout.select config.Cli.os
  | Ok (Cli.Perf (config, _, _)) -> Target_layout.select config.Cli.os
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
          exit 1)
  | Ok (Cli.Check config) -> (
      match Driver.check config with
      | Ok checked ->
          Driver.release checked;
          if config.diagnostics = Cli.Json then prerr_endline (Diag.render_json []);
          exit 0
      | Error diagnostics ->
          report config diagnostics;
          exit 1)
  | Ok (Cli.Lsp options) -> exit (Lsp.run options)
  | Ok (Cli.Run_program (config, args)) -> exit (run_program config args)
  | Ok (Cli.Perf (config, _, _)) when config.Cli.os <> Target_layout.Linux ->
      prerr_endline
        (match config.Cli.os with
        | Target_layout.Wasi ->
            "fas perf measures Linux builds only; this build is for WASI (`--os=wasi`)"
        | _ ->
            "fas perf measures Linux builds only; this build is for Windows \
             (`--os=windows`)");
      exit 2
  | Ok (Cli.Perf (config, options, args)) -> (
      match options.mode with
      | Cli.Tune output ->
          let output =
            Option.value output
              ~default:(Filename.remove_extension config.Cli.input ^ ".fasprof")
          in
          exit
            (Tune.run ~report:(report config) config ~output ~no_arena:options.no_arena
               ~yes:options.yes ~check_with:options.check_with
               ~argv0:(program_name config) args)
      | Cli.Measure -> (
          match load_baseline options with
          | Ok baseline -> exit (perf_command config options baseline args)
          | Error message ->
              prerr_endline message;
              exit 1))
  | Error message ->
      prerr_endline message;
      exit 2
