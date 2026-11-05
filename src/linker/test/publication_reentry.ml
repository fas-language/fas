open Fas_linker

let () =
  let directory = Sys.argv.(1) in
  let mask = int_of_string Sys.argv.(2) in
  let check worker =
    let path suffix =
      Filename.concat directory (Printf.sprintf "%d-%s" worker suffix)
    in
    for iteration = 1 to 12 do
      List.iter
        (fun permissions ->
          let output = path (Printf.sprintf "%d-image" permissions) in
          let contents = Bytes.of_string (Printf.sprintf "%d:%d" worker iteration) in
          File_io.publish ~output ~permissions ~inputs:[] (fun () -> contents);
          assert ((Unix.stat output).Unix.st_perm = permissions land lnot mask);
          assert (File_io.read ~max_size:100 output = Bytes.to_string contents))
        [ 0o644; 0o755 ];
      let ordinary = path "ordinary" in
      (try Unix.unlink ordinary with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
      let descriptor = Unix.openfile ordinary [ Unix.O_CREAT; Unix.O_WRONLY ] 0o666 in
      assert ((Unix.fstat descriptor).Unix.st_perm = 0o666 land lnot mask);
      Unix.close descriptor
    done
  in
  List.init 3 (fun worker -> Domain.spawn (fun () -> check worker))
  |> List.iter Domain.join;
  print_endline
    "publication and ordinary file creation preserve concurrent umask policy"
