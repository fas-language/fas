(* The C compiler supplies host policy (CRT, interpreter, search paths and
   runtime libraries), never the linker implementation. Its -### plan is data;
   no printed command is executed and no shell is involved. *)
module L = Fas_linker

let ( let* ) = Result.bind

let run ~keep argv =
  if keep then
    prerr_endline
      ("fas: hosted C command: "
      ^ String.concat " " (List.map Filename.quote (Array.to_list argv)));
  match Process.run argv with
  | Ok result -> Ok result
  | Error failure ->
      Error
        (Printf.sprintf "hosted native: %s failed: %s" argv.(0)
           (String.trim failure.Process.stderr))

let parse arguments =
  match L.Command_line.parse arguments with
  | Link link -> link
  | Help | Version -> L.Error.fail "hosted native: expected a linker command"

let linker_plan ~object_path text =
  let candidates =
    String.split_on_char '\n' text
    |> List.filter_map (fun line ->
        (* Ignore version banners and environment assignments. Both supported
           drivers indent their commands, with an absolute executable path. *)
        let line = String.trim line in
        if line = "" || (line.[0] <> '/' && line.[0] <> '"') then None
        else
          let words = L.Response_file.tokenize ~path:"C compiler link plan" line in
          match words with
          | _command :: arguments
            when List.mem object_path arguments
                 && List.mem "-o" arguments
                 && not (List.mem "-###" arguments) ->
              Some arguments
          | _ -> None)
  in
  match candidates with
  | [ arguments ] -> parse arguments
  | _ ->
      L.Error.fail
        "hosted native: expected one GCC/Clang linker command from FAS_CC -###"

let link ~cc ~config ~c_objects ~link_flags ir =
  let* object_ = Native_gen.compile_object ir in
  let temporaries = ref [] in
  let temporary () =
    let path = Filename.temp_file "fas-hosted-" ".o" in
    temporaries := path :: !temporaries;
    if config.Cli.keep then prerr_endline ("fas: kept intermediate: " ^ path);
    path
  in
  Fun.protect
    ~finally:(fun () ->
      if not config.Cli.keep then
        List.iter
          (fun path -> try Sys.remove path with Sys_error _ -> ())
          !temporaries)
    (fun () ->
      try
        let c_link_flags = List.filter (( = ) "-pthread") link_flags in
        let raw_arguments =
          Cli.native_linker_arguments
            (config.Cli.link_inputs @ List.filter (( <> ) "-pthread") link_flags)
        in
        (* Enforce compiler-driver restrictions before accepting the C driver's
           own -o. In particular -Wl,-o must never redirect publication. *)
        let user =
          match L.Command_line.parse ~link_only:true raw_arguments with
          | Link link -> link
          | Help | Version -> assert false
        in
        let sysroot =
          match user.options.sysroot with
          | Some path -> [ "--sysroot=" ^ path ]
          | None -> []
        in
        if user.options.mode = L.Options.Shared then
          L.Error.fail "hosted native: shared output requires a separate object link";
        L.File_io.check_output ~output:config.Cli.output
          ~inputs:
            ((config.Cli.input :: c_objects) @ List.concat_map L.Input.paths user.inputs);
        let object_path = temporary () in
        let channel = open_out_bin object_path in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () ->
            output_bytes channel object_;
            close_out channel);
        let rec compile_sources = function
          | [] -> Ok []
          | (("-Xlinker" | "-l" | "-L") as flag) :: value :: rest ->
              let* rest = compile_sources rest in
              Ok (flag :: value :: rest)
          | path :: rest when Filename.check_suffix path ".c" ->
              let output = temporary () in
              let* _ =
                run ~keep:config.Cli.keep
                  (Array.of_list
                     (cc
                     :: (sysroot @ config.Cli.c_flags @ c_link_flags
                        @ [ "-fPIC"; "-O" ^ string_of_int config.Cli.optimization ]
                        @ (if config.Cli.debug then [ "-g" ] else [])
                        @ [ "-c"; path; "-o"; output ])))
              in
              let* rest = compile_sources rest in
              Ok (output :: rest)
          | argument :: rest ->
              let* rest = compile_sources rest in
              Ok (argument :: rest)
        in
        let* arguments = compile_sources config.Cli.link_inputs in
        let mode =
          match user.options.mode with
          | L.Options.Static_pie -> "-static-pie"
          | Dynamic { pie = true; _ } -> "-pie"
          | _ when List.mem "-static" raw_arguments -> "-static"
          | _ -> "-no-pie"
        in
        let* _, plan =
          run ~keep:config.Cli.keep
            (Array.of_list
               (cc
               :: ([ "-###"; "-fno-use-linker-plugin"; mode ]
                  @ sysroot @ [ object_path ] @ c_objects @ arguments @ link_flags
                  @ [ "-lm"; "-o"; config.Cli.output ])))
        in
        let link = linker_plan ~object_path plan in
        if link.output <> config.Cli.output || link.relocatable then
          L.Error.fail "hosted native: C compiler changed the requested output";
        L.Static_link.link_input_files ~options:link.options ~entry:link.entry
          ?map_file:link.map_file ~output:config.Cli.output link.inputs;
        Ok ()
      with
      | L.Error.Error message -> Error ("hosted native: " ^ message)
      | Sys_error message -> Error ("hosted native: " ^ message)
      | Unix.Unix_error (code, operation, path) ->
          Error
            (Printf.sprintf "hosted native: %s %s: %s" operation path
               (Unix.error_message code)))
