(* Statistics never force collections. Allocation counters describe this domain;
   sampled collection counts describe the process. Timing uses gettimeofday;
   backwards clock adjustments are clamped rather than reported as negative. *)
type phase = {
  name : string;
  wall : float;
  cpu : float;
  allocated : float;
  promoted : float;
}

type t = {
  start_wall : float;
  start_cpu : float;
  start_gc : Gc.stat;
  start_allocated : float;
  start_promoted : float;
  mutable phases : phase list;
  counts : (string, int) Hashtbl.t;
}

let counters () =
  let minor, promoted, major = Gc.counters () in
  (minor +. major -. promoted, promoted)

let create () =
  let allocated, promoted = counters () in
  {
    start_wall = Unix.gettimeofday ();
    start_cpu = Sys.time ();
    start_gc = Gc.quick_stat ();
    start_allocated = allocated;
    start_promoted = promoted;
    phases = [];
    counts = Hashtbl.create 16;
  }

let add stats name value =
  Option.iter
    (fun t ->
      let previous = Option.value (Hashtbl.find_opt t.counts name) ~default:0 in
      Hashtbl.replace t.counts name (previous + value))
    stats

let measure stats name f =
  match stats with
  | None -> f ()
  | Some t ->
      let wall = Unix.gettimeofday () and cpu = Sys.time () in
      let allocated, promoted = counters () in
      Fun.protect f ~finally:(fun () ->
          let end_allocated, end_promoted = counters () in
          t.phases <-
            {
              name;
              wall = max 0. (Unix.gettimeofday () -. wall);
              cpu = Sys.time () -. cpu;
              allocated = end_allocated -. allocated;
              promoted = end_promoted -. promoted;
            }
            :: t.phases)

let json t =
  let gc = Gc.quick_stat () in
  let allocated, promoted = counters () in
  let phases = Hashtbl.create 16 in
  List.iter
    (fun p ->
      let wall, cpu, allocated, promoted =
        Option.value (Hashtbl.find_opt phases p.name) ~default:(0., 0., 0., 0.)
      in
      Hashtbl.replace phases p.name
        (wall +. p.wall, cpu +. p.cpu, allocated +. p.allocated, promoted +. p.promoted))
    t.phases;
  let sorted table =
    Hashtbl.to_seq table |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let phase_json =
    sorted phases
    |> List.map (fun (name, (wall, cpu, allocated, promoted)) ->
        Printf.sprintf
          "%S:{\"wall_seconds\":%.9f,\"cpu_seconds\":%.9f,\"allocated_words\":%.0f,\"promoted_words\":%.0f}"
          name wall cpu allocated promoted)
    |> String.concat ","
  in
  let count_json =
    sorted t.counts
    |> List.map (fun (name, count) -> Printf.sprintf "%S:%d" name count)
    |> String.concat ","
  in
  Printf.sprintf
    "{\"wall_seconds\":%.9f,\"cpu_seconds\":%.9f,\"phases\":{%s},\"counts\":{%s},\"gc\":{\"allocated_words\":%.0f,\"promoted_words\":%.0f,\"minor_collections\":%d,\"major_collections\":%d}}"
    (max 0. (Unix.gettimeofday () -. t.start_wall))
    (Sys.time () -. t.start_cpu)
    phase_json count_json
    (allocated -. t.start_allocated)
    (promoted -. t.start_promoted)
    (gc.minor_collections - t.start_gc.minor_collections)
    (gc.major_collections - t.start_gc.major_collections)
