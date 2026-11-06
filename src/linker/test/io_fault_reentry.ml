open Fas_linker

let () =
  let paths = [ Sys.argv.(1); Sys.argv.(2) ] in
  let input = Sys.getenv "FAS_IO_INPUT" in
  Unix.putenv "FAS_IO_INPUT" "";
  let expected = Static_link.link paths in
  let descriptors () = Array.length (Sys.readdir "/proc/self/fd") in
  let before = descriptors () in
  Unix.putenv "FAS_IO_INPUT" input;
  for _ = 1 to 256 do
    match Static_link.link paths with
    | _ -> failwith "injected input failure did not fire"
    | exception Error.Error _ -> ()
  done;
  assert (descriptors () = before);
  Unix.putenv "FAS_IO_INPUT" "";
  assert (Static_link.link paths = expected);
  assert (descriptors () = before);
  print_endline "input failure recovery: 256 failures, no descriptor leak"
