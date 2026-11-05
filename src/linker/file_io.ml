(* Bounded regular files only: O_NONBLOCK prevents a named pipe from hanging the
   linker before fstat can reject it. Each invocation owns its descriptors. *)
let read ~max_size path =
  Error.io (fun () ->
      let descriptor =
        Unix.openfile path [ Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC ] 0
      in
      let channel = Unix.in_channel_of_descr descriptor in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          if (Unix.fstat descriptor).Unix.st_kind <> Unix.S_REG then
            Error.fail "%s: input must be a regular file" path;
          let size = in_channel_length channel in
          if size > max_size then
            Error.fail "%s: input exceeds %d byte limit or remaining input budget" path
              max_size;
          try really_input_string channel size
          with End_of_file -> Error.fail "%s: input was truncated while reading" path))

(* Compiler configure/driver checks commonly link to /dev/null. Treat the
   canonical null device as an explicit discard request: still build and validate
   the complete image, but never open, chmod or rename over the device. Other
   special files retain the regular-file restriction. *)
let null_output output stat =
  stat.Unix.st_kind = Unix.S_CHR && Unix.realpath output = "/dev/null"

let check_output ~output ~inputs =
  Error.io (fun () ->
      let output_stat =
        try Some (Unix.stat output) with Unix.Unix_error (Unix.ENOENT, _, _) -> None
      in
      Option.iter
        (fun stat ->
          if stat.Unix.st_kind <> Unix.S_REG && not (null_output output stat) then
            Error.fail "%s: output must be a regular file" output)
        output_stat;
      List.iter
        (fun path ->
          match output_stat with
          | None -> ()
          | Some out ->
              let input = Unix.stat path in
              if
                input.Unix.st_dev = out.Unix.st_dev
                && input.Unix.st_ino = out.Unix.st_ino
              then Error.fail "%s: output would overwrite input %s" output path)
        inputs)

let distinct_outputs first second =
  Error.io (fun () ->
      let canonical path =
        Filename.concat (Unix.realpath (Filename.dirname path)) (Filename.basename path)
      in
      let same_inode =
        try
          let a = Unix.stat first and b = Unix.stat second in
          a.Unix.st_dev = b.Unix.st_dev && a.Unix.st_ino = b.Unix.st_ino
        with Unix.Unix_error (Unix.ENOENT, _, _) -> false
      in
      if canonical first = canonical second || same_inode then
        Error.fail "executable and map outputs must be different files")

let publish ?stats ~output ~permissions ~inputs make_image =
  Error.io (fun () ->
      check_output ~output ~inputs;
      let discard =
        try null_output output (Unix.stat output)
        with Unix.Unix_error (Unix.ENOENT, _, _) -> false
      in
      let image = make_image () in
      Stats.measure stats "write" (fun () ->
          if discard then Stats.add stats "discarded_output_bytes" (Bytes.length image)
          else begin
            let temporary, channel =
              Filename.open_temp_file ~temp_dir:(Filename.dirname output)
                ~mode:[ Open_binary ] ~perms:permissions ".fas-ld-" ".tmp"
            in
            Fun.protect
              ~finally:(fun () ->
                close_out_noerr channel;
                try Unix.unlink temporary
                with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
              (fun () ->
                let descriptor = Unix.descr_of_out_channel channel in
                (* Creation applies the caller's umask/default ACL without changing
                 process-wide state. Keep the image private until publication. *)
                let final_permissions =
                  (Unix.fstat descriptor).Unix.st_perm land permissions
                in
                Unix.fchmod descriptor 0o600;
                output_bytes channel image;
                flush channel;
                Unix.fchmod descriptor final_permissions;
                close_out channel;
                Unix.rename temporary output)
          end))
