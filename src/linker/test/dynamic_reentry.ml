open Fas_linker

let () =
  match Command_line.parse (Array.to_list Sys.argv |> List.tl) with
  | Help | Version -> failwith "expected link configuration"
  | Link config ->
      let link () =
        let stats = Stats.create () in
        let result =
          if config.relocatable then
            (Partial_link.link_inputs ~stats ~options:config.options config.inputs, "")
          else
            Static_link.link_inputs_with_map ~stats ~options:config.options
              ~entry:config.entry config.inputs
        in
        assert (String.length (Stats.json stats) > 0);
        result
      in
      let expected = link () in
      let check () =
        for _ = 1 to 3 do
          assert (link () = expected)
        done
      in
      check ();
      (match
         if config.relocatable then
           Partial_link.link_inputs ~options:config.options
             [ Input.Object { path = "injected-invalid-object"; data = "invalid" } ]
         else if config.options.mode = Shared then
           Static_link.link_inputs ~options:config.options
             [ Input.Object { path = "injected-invalid-object"; data = "invalid" } ]
         else
           Static_link.link_inputs ~options:config.options
             ~entry:"__missing_reentry_test_entry" config.inputs
       with
      | _ -> failwith "expected link failure"
      | exception Error.Error _ -> ());
      check ();
      let workers = List.init 2 (fun _ -> Domain.spawn check) in
      List.iter Domain.join workers;
      check ();
      print_endline
        "dynamic library: repeat/failure/domain reentry and deterministic image/map \
         passed"
