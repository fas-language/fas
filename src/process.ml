type failure = {
  argv : string array;
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let run_to_file argv output_path =
  if Array.length argv = 0 then
    Error
      {
        argv;
        status = Unix.WEXITED 127;
        stdout = "";
        stderr = "cannot run an empty argv";
      }
  else
    let err_path = Filename.temp_file "fas-err-" ".tmp" in
    Fun.protect
      ~finally:(fun () -> try Sys.remove err_path with Sys_error _ -> ())
      (fun () ->
        try
          let out_fd =
            Unix.openfile output_path
              [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
              0o600
          in
          let err_fd =
            Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
          in
          let pid =
            try Unix.create_process argv.(0) argv Unix.stdin out_fd err_fd
            with exn ->
              Unix.close out_fd;
              Unix.close err_fd;
              raise exn
          in
          Unix.close out_fd;
          Unix.close err_fd;
          let _, status = Unix.waitpid [] pid in
          let stderr = read_file err_path in
          match status with
          | Unix.WEXITED 0 -> Ok stderr
          | _ -> Error { argv; status; stdout = ""; stderr }
        with
        | Unix.Unix_error (code, operation, argument) ->
            Error
              {
                argv;
                status = Unix.WEXITED 127;
                stdout = "";
                stderr =
                  Printf.sprintf "%s(%s): %s" operation argument
                    (Unix.error_message code);
              }
        | Sys_error message ->
            Error { argv; status = Unix.WEXITED 127; stdout = ""; stderr = message })

let run argv =
  let path = Filename.temp_file "fas-out-" ".tmp" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      match run_to_file argv path with
      | Ok stderr -> Ok (read_file path, stderr)
      | Error failure -> Error { failure with stdout = read_file path })

let posix_signal signal =
  let table =
    List.concat
      [
        [ (Sys.sighup, 1); (Sys.sigint, 2); (Sys.sigquit, 3); (Sys.sigill, 4) ];
        [ (Sys.sigtrap, 5); (Sys.sigabrt, 6); (Sys.sigbus, 7); (Sys.sigfpe, 8) ];
        [ (Sys.sigkill, 9); (Sys.sigusr1, 10); (Sys.sigsegv, 11); (Sys.sigusr2, 12) ];
        [ (Sys.sigpipe, 13); (Sys.sigalrm, 14); (Sys.sigterm, 15); (Sys.sigxcpu, 24) ];
        [ (Sys.sigxfsz, 25); (Sys.sigvtalrm, 26); (Sys.sigprof, 27); (Sys.sigpoll, 29) ];
        [ (Sys.sigsys, 31) ];
      ]
  in
  Option.value ~default:signal (List.assoc_opt signal table)

let forwarded_signals = [ Sys.sigterm; Sys.sighup ]

let rec retry_interrupted f =
  try f () with Unix.Unix_error (Unix.EINTR, _, _) -> retry_interrupted f

let run_program_windows path argv =
  flush stdout;
  flush stderr;
  try
    let pid = Unix.create_process path argv Unix.stdin Unix.stdout Unix.stderr in
    match snd (retry_interrupted (fun () -> Unix.waitpid [] pid)) with
    | Unix.WEXITED code -> Ok code
    | Unix.WSIGNALED s | Unix.WSTOPPED s -> Ok (128 + posix_signal s)
  with Unix.Unix_error (code, _, _) -> Error (Unix.error_message code)

let run_program_posix path argv =
  let child = ref None in
  let forward signal =
    Option.iter
      (fun pid -> try Unix.kill pid signal with Unix.Unix_error _ -> ())
      !child
  in
  let previous_mask = Unix.sigprocmask Unix.SIG_BLOCK forwarded_signals in
  let saved =
    List.map (fun s -> (s, Sys.signal s Sys.Signal_ignore)) [ Sys.sigint; Sys.sigquit ]
    @ List.map
        (fun s -> (s, Sys.signal s (Sys.Signal_handle forward)))
        forwarded_signals
  in
  let restore () = List.iter (fun (s, behavior) -> Sys.set_signal s behavior) saved in
  let unblock () = ignore (Unix.sigprocmask Unix.SIG_SETMASK previous_mask) in
  Fun.protect
    ~finally:(fun () ->
      child := None;
      unblock ();
      restore ())
    (fun () ->
      try
        let read_end, write_end = Unix.pipe ~cloexec:true () in
        match Unix.fork () with
        | 0 ->
            restore ();
            unblock ();
            (try Unix.execvp path argv
             with Unix.Unix_error (code, _, _) ->
               let message = Unix.error_message code in
               ignore (Unix.write_substring write_end message 0 (String.length message)));
            Unix._exit 127
        | pid ->
            child := Some pid;
            unblock ();
            Unix.close write_end;
            let buffer = Bytes.create 512 in
            let count =
              retry_interrupted (fun () ->
                  Unix.read read_end buffer 0 (Bytes.length buffer))
            in
            Unix.close read_end;
            let status = snd (retry_interrupted (fun () -> Unix.waitpid [] pid)) in
            child := None;
            if count > 0 then Error (Bytes.sub_string buffer 0 count)
            else
              Ok
                (match status with
                | Unix.WEXITED code -> code
                | Unix.WSIGNALED s | Unix.WSTOPPED s -> 128 + posix_signal s)
      with Unix.Unix_error (code, _, _) -> Error (Unix.error_message code))

let run_program path argv =
  if Sys.win32 then run_program_windows path argv else run_program_posix path argv

let signal_names =
  [
    (Sys.sighup, "SIGHUP");
    (Sys.sigint, "SIGINT");
    (Sys.sigquit, "SIGQUIT");
    (Sys.sigill, "SIGILL");
    (Sys.sigtrap, "SIGTRAP");
    (Sys.sigabrt, "SIGABRT");
    (Sys.sigbus, "SIGBUS");
    (Sys.sigfpe, "SIGFPE");
    (Sys.sigkill, "SIGKILL");
    (Sys.sigusr1, "SIGUSR1");
    (Sys.sigsegv, "SIGSEGV");
    (Sys.sigusr2, "SIGUSR2");
    (Sys.sigpipe, "SIGPIPE");
    (Sys.sigalrm, "SIGALRM");
    (Sys.sigterm, "SIGTERM");
    (Sys.sigxcpu, "SIGXCPU");
    (Sys.sigxfsz, "SIGXFSZ");
    (Sys.sigvtalrm, "SIGVTALRM");
    (Sys.sigprof, "SIGPROF");
    (Sys.sigpoll, "SIGPOLL");
    (Sys.sigsys, "SIGSYS");
  ]

let signal_name signal =
  Option.value ~default:(string_of_int signal) (List.assoc_opt signal signal_names)

let run_inherited ~env path argv =
  flush stdout;
  flush stderr;
  try
    let pid =
      Unix.create_process_env path argv env Unix.stdin Unix.stdout Unix.stderr
    in
    Ok (snd (retry_interrupted (fun () -> Unix.waitpid [] pid)))
  with Unix.Unix_error (code, _, _) -> Error (Unix.error_message code)
