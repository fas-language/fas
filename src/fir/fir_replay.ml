(* A phase can replay its functions in order without retaining their OCaml
   heaps. Scratch files have no directory entries once opened, including
   when the driver exits from a diagnostic. *)
type chunk = { reader : in_channel; writer : out_channel; mutable count : int }
type 'a t = { mutable chunks : chunk list }

let create () = { chunks = [] }

let open_chunk () =
  let path, writer = Filename.open_temp_file ~mode:[ Open_binary ] "fas-fir-" ".bodies" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    match open_in_bin path with
    | reader -> { reader; writer; count = 0 }
    | exception e -> close_out_noerr writer; raise e)

let append (t : 'a t) (value : 'a) =
  (* Bound the size of each scratch file apart from a single, oversized
     function. The channels use separate offsets for writing and replay. *)
  let chunk = match t.chunks with
    | chunk :: _ when pos_out chunk.writer < 64 * 1024 * 1024 -> chunk
    | _ ->
        let chunk = open_chunk () in
        t.chunks <- chunk :: t.chunks;
        chunk
  in
  Marshal.to_channel chunk.writer value [];
  chunk.count <- chunk.count + 1

let iter (t : 'a t) (visit : 'a -> unit) =
  List.iter
    (fun chunk ->
      flush chunk.writer;
      seek_in chunk.reader 0;
      for _ = 1 to chunk.count do visit (Marshal.from_channel chunk.reader) done)
    (List.rev t.chunks)

let close t =
  List.iter (fun chunk -> close_in_noerr chunk.reader; close_out_noerr chunk.writer) t.chunks;
  t.chunks <- []
