open Fas_linker

let config args =
  match Command_line.parse args with
  | Link config -> config
  | Help | Version -> failwith "expected link configuration"

let () =
  let rec split before = function
    | "--next" :: rest -> (config (List.rev before), config rest)
    | arg :: rest -> split (arg :: before) rest
    | [] -> failwith "expected two configurations separated by --next"
  in
  let first, second = split [] (Array.to_list Sys.argv |> List.tl) in
  let link (config : Command_line.config) =
    Static_link.link_inputs_with_map ~options:config.options ~entry:config.entry
      config.inputs
  in
  let first_result = link first and second_result = link second in
  assert (first_result <> second_result);
  let check config expected () =
    for _ = 1 to 8 do
      assert (link config = expected)
    done
  in
  check first first_result ();
  check second second_result ();
  let failed =
    {
      first with
      inputs =
        first.inputs
        @ [ Input.Library { name = "missing"; whole = false; static = false } ];
    }
  in
  (match link failed with
  | _ -> failwith "expected link failure"
  | exception Error.Error _ -> ());
  let without_search =
    { second with options = { second.options with library_paths = [] } }
  in
  (match link without_search with
  | _ -> failwith "SEARCH_DIR leaked between invocations"
  | exception Error.Error _ -> ());
  let workers =
    [
      Domain.spawn (check first first_result); Domain.spawn (check second second_result);
    ]
  in
  List.iter Domain.join workers;
  check first first_result ();
  check second second_result ();
  print_endline "repeat/failure/concurrent distinct sysroots passed"
