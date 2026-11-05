open Fas_linker

let () =
  let paths = [ Sys.argv.(1); Sys.argv.(2) ] in
  let expected = Static_link.link paths in
  let check () = assert (Static_link.link paths = expected) in
  check ();
  (match Static_link.link [ Sys.argv.(1) ] with
  | _ -> failwith "expected undefined reference"
  | exception Error.Error _ -> ());
  check ();
  (match Static_link.link ~entry:"missing" paths with
  | _ -> failwith "expected undefined entry"
  | exception Error.Error _ -> ());
  check ();
  let workers = List.init 2 (fun _ -> Domain.spawn check) in
  List.iter Domain.join workers;
  print_endline "linker library: repeated, failed, and concurrent invocations passed"
