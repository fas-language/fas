(* Untimed allocation sampling with OCaml 5.4's Memprof. See README.md. *)
open Fas_linker

let () =
  Printexc.record_backtrace true;
  let samples = Hashtbl.create 128 in
  let record (a : Gc.Memprof.allocation) =
    let stack = Printexc.raw_backtrace_to_string a.callstack in
    let previous = Option.value (Hashtbl.find_opt samples stack) ~default:0 in
    Hashtbl.replace samples stack (previous + a.n_samples);
    None
  in
  let profile =
    Gc.Memprof.start ~sampling_rate:0.002 ~callstack_size:10
      { Gc.Memprof.null_tracker with alloc_minor = record; alloc_major = record }
  in
  Error.io (fun () ->
      match Command_line.parse (List.tl (Array.to_list Sys.argv)) with
      | Link config ->
          if config.relocatable then
            Partial_link.link_input_files ~options:config.options ~output:config.output
              config.inputs
          else
            Static_link.link_input_files ~options:config.options ~entry:config.entry
              ?map_file:config.map_file ~output:config.output config.inputs
      | _ -> failwith "link arguments required");
  Gc.Memprof.stop ();
  Gc.Memprof.discard profile;
  Hashtbl.to_seq samples |> List.of_seq
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.iter (fun (stack, n) ->
      Printf.printf "samples=%d estimated_words=%.0f\n%s\n" n (float n /. 0.002) stack)
