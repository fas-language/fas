(* Module metadata and a replayable sequence of function bodies. Passes may
   also append small runtime helpers or extern declarations to the module. *)
open Fir

type t = { modul : modul; mutable bodies : func Fir_replay.t; mutable stored : int }

let with_input ch compile =
  let bodies = Fir_replay.create () in
  let modul =
    try Fir_parse.read_module ch (fun m f ->
      Fir_replay.append bodies f;
      add_func m (func_header f))
    with e -> Fir_replay.close bodies; raise e
  in
  let stream = { modul; bodies; stored = Vec.length modul.funcs } in
  Fun.protect ~finally:(fun () -> Fir_replay.close stream.bodies) (fun () -> compile stream)

let iter t visit =
  let n = Vec.length t.modul.funcs in
  Fir_replay.iter t.bodies visit;
  for i = t.stored to n - 1 do visit (Vec.get t.modul.funcs i) done

let map t transform =
  let output = Fir_replay.create () in
  let i = ref 0 in
  match iter t (fun f ->
    let f = transform f in
    Fir_replay.append output f;
    Vec.set t.modul.funcs !i (func_header f);
    incr i) with
  | () ->
      Fir_replay.close t.bodies;
      t.bodies <- output;
      t.stored <- !i
  | exception e -> Fir_replay.close output; raise e

let verify t = Fir_verify.verify ~iter_funcs:(iter t) t.modul

let normalize t = map t (fun f ->
  Fir_trapdiv.run_func ~hw:true f;
  Fir_atomic.run_func t.modul f;
  Fir_scalarize.run_func f)

let wide t = Fir_wide.rewrite t.modul (fun rewrite ->
  map t (fun f -> Fir_atomic.run_func t.modul f; rewrite f))

let has_asm t =
  let found = ref false in
  iter t (fun f ->
    if not !found then begin
      Vec.iter (fun (ir : inst_rec) -> match ir.op with Asm _ -> found := true | _ -> ()) f.insts;
      Vec.iter (fun (br : block_rec) -> match br.term with Asm_goto _ -> found := true | _ -> ()) f.blocks
    end);
  !found
