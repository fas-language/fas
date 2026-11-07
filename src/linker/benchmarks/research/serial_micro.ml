(* Isolated CPU/allocation probes, not full-link performance predictions.
   Usage: serial_micro ARCHIVE OBJECT [OBJECT ...]
   Build against the release-profile Fas_linker library, as allocation_profile.ml.
   I/O and fixture parsing are outside the index/sort/copy measurements. *)
open Fas_linker

module Strings = Hashtbl.MakeSeeded (struct
  type t = string
  let equal = String.equal
  let seeded_hash = Hashtbl.seeded_hash
end)

let allocated () =
  let minor, promoted, major = Gc.counters () in
  minor +. major -. promoted

let case name f = (name, fun () -> ignore (Sys.opaque_identity (f ())))

let measure_many probes =
  let run f count =
    let words = allocated () and start = Sys.time () in
    for _ = 1 to count do f () done;
    (Sys.time () -. start, allocated () -. words)
  in
  let rec calibrate f n =
    let seconds, _ = run f n in
    if seconds < 0.05 && n < 4096 then calibrate f (n * 2) else n
  in
  let probes = Array.of_list (List.map (fun (name, f) -> name, f, calibrate f 1) probes) in
  for sample = 1 to 7 do
    (* Rotate alternatives within each sample to reduce scheduling/frequency drift. *)
    for position = 0 to Array.length probes - 1 do
      let name, f, count = probes.((sample - 1 + position) mod Array.length probes) in
      Gc.full_major ();
      let seconds, words = run f count in
      Printf.printf "{\"name\":%S,\"sample\":%d,\"iterations\":%d,\"cpu_ms_per_operation\":%.6f,\"words_per_operation\":%.0f}\n%!"
        name sample count (seconds *. 1000. /. float count) (words /. float count)
    done
  done

let () =
  if Array.length Sys.argv < 3 then failwith "ARCHIVE OBJECT [OBJECT ...] required";
  let path = Sys.argv.(1) in
  let data = File_io.read ~max_size:Elf_reader.max_file_size path in
  let archive = Archive.parse ~path data in
  let pairs =
    Array.to_list archive.members
    |> List.mapi (fun member (m : Archive.member) -> List.map (fun name -> name, member) m.exports)
    |> List.concat |> Array.of_list
  in
  let normalize rows = List.sort compare rows in
  let generic capacity () =
    let table = Hashtbl.create ~random:true capacity in
    Array.iter (fun (name, member) ->
        let previous = Option.value (Hashtbl.find_opt table name) ~default:[] in
        Hashtbl.replace table name (member :: previous)) pairs;
    table
  in
  let specialized capacity () =
    let table = Strings.create ~random:true capacity in
    Array.iter (fun (name, member) ->
        let previous = Option.value (Strings.find_opt table name) ~default:[] in
        Strings.replace table name (member :: previous)) pairs;
    table
  in
  let expected = normalize (Hashtbl.to_seq (generic 256 ()) |> List.of_seq) in
  List.iter (fun capacity ->
      assert (expected = normalize (Hashtbl.to_seq (generic capacity ()) |> List.of_seq));
      assert (expected = normalize (Strings.to_seq (specialized capacity ()) |> List.of_seq)))
    [256; Array.length pairs];
  measure_many [case "archive-container-and-index" (fun () -> Archive.parse ~path data);
                case "copy-all-member-bytes" (fun () ->
                    Array.map (fun (m : Archive.member) -> String.sub data m.offset m.size) archive.members)];
  measure_many [case "index-generic-256" (generic 256);
                case "index-generic-sized" (generic (Array.length pairs));
                case "index-string-256" (specialized 256);
                case "index-string-sized" (specialized (Array.length pairs))];
  Array.iteri (fun argument path ->
      if argument >= 2 then begin
        let object_ = Elf_reader.read path in
        let original = Array.copy object_.relocations in
        let cmp (a : Elf_reader.relocation) (b : Elf_reader.relocation) =
          let c = compare a.target b.target in
          if c <> 0 then c else compare a.offset b.offset
        in
        let sort algorithm () =
          let copy = Array.copy original in
          algorithm cmp copy;
          copy
        in
        let heap = sort Array.sort () and merge = sort Array.stable_sort () in
        let needs_sort = ref false in
        for i = 1 to Array.length original - 1 do
          if cmp original.(i - 1) original.(i) > 0 then needs_sort := true
        done;
        assert (Array.length heap = Array.length merge);
        Array.iteri (fun i r -> assert (cmp r merge.(i) = 0)) heap;
        Printf.printf "{\"object\":%S,\"relocations\":%d,\"needs_sort\":%b,\"sort_keys_match\":true}\n%!"
          path (Array.length original) !needs_sort;
        measure_many [case (Filename.basename path ^ ":heap-sort") (sort Array.sort);
                      case (Filename.basename path ^ ":merge-sort") (sort Array.stable_sort)];
        assert (original = object_.relocations)
      end) Sys.argv
