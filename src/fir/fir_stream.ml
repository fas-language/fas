(* Module metadata and a replayable sequence of function bodies. Passes may
   also append small runtime helpers or extern declarations to the module. *)
open Fir

type t = { modul : modul; mutable bodies : func Fir_replay.t; mutable stored : int }

let with_input ch compile =
  let bodies = Fir_replay.create () in
  (* Native lowering uses numeric value IDs; only the textual printer needs names. *)
  let modul =
    try Fir_parse.read_module ~keep_value_names:false ch (fun m f ->
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

let pinned_functions t =
  (* The module contains only function headers. Scan the streamed bodies
     as well so a block address in another function pins its target. *)
  let pinned = Hashtbl.create 16 in
  let pin fn = Hashtbl.replace pinned fn () in
  iter t (fun f ->
    Hashtbl.iter (fun c _ -> match c with Cblockaddr (fn, _) -> pin fn | _ -> ()) f.consts);
  Vec.iter (fun g ->
    Option.iter (List.iter (function
      | Blockref { fn; _ } | Blockdiff { fn; _ } -> pin fn
      | _ -> ())) g.init) t.modul.globals;
  pinned

(* Cleanup landing pads are exception edges, outside the ordinary CFG. *)
let can_fold pinned f =
  f.defined && not (Hashtbl.mem pinned f.name) && f.unwind_cleanup = None

let fold_body f =
  Fir_fold.fold_func ~forward:true f;
  ignore (remove_unreachable_blocks f)

let fold t =
  let pinned = pinned_functions t in
  map t (fun f -> if can_fold pinned f then fold_body f; f)

(* In asm, labels can be mentioned directly rather than as Fir operands.
   Treat each potential assembler symbol spelling as a reference. *)
let asm_symbols text visit =
  let symbol_char = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '.' -> true | _ -> false in
  let n = String.length text in
  let start = ref 0 in
  while !start < n do
    let last = ref !start in
    while !last < n && symbol_char text.[!last] do incr last done;
    if !last > !start then begin
      let name = String.sub text !start (!last - !start) in
      visit name;
      (* AT&T assembly prefixes immediate operands with $. *)
      if name.[0] = '$' && String.length name > 1 then visit (String.sub name 1 (String.length name - 1))
    end;
    start := max (!start + 1) !last
  done

let function_refs f ~address ~call =
  let use v = match const_of f v with
    | Some (Csym (name, _) | Cblockaddr (name, _)) -> address name
    | _ -> ()
  in
  Vec.iter (fun (b : block_rec) ->
    Vec.iter (fun i ->
      let ir = inst f i in
      (match ir.op with
      | Call { callee = Direct name; args; _ } -> call name args
      | Asm { tmpl; _ } -> asm_symbols tmpl address
      | _ -> ());
      iter_op_uses use ir.op) b.insts;
    (match b.term with Asm_goto { tmpl; _ } -> asm_symbols tmpl address | _ -> ());
    iter_term_uses use b.term) f.blocks;
  List.iter (fun v -> Option.iter (fun (name, _) -> address name) v.vsymbol) f.vars

let global_refs g visit =
  Option.iter (List.iter (function
    | Reloc { sym; _ } -> visit sym
    | Blockref { fn; _ } | Blockdiff { fn; _ } -> visit fn
    | Bytes _ | Zeros _ -> ())) g.init

let drop_unused t =
  let m = t.modul in
  let edges = Hashtbl.create 32 and live = Hashtbl.create 32 in
  let pending = Queue.create () in
  let root name =
    if Hashtbl.mem m.discardable name && not (Hashtbl.mem live name) then begin
      Hashtbl.add live name ();
      Queue.add name pending
    end
  in
  let refs name is_local collect =
    if is_local && Hashtbl.mem m.discardable name then begin
      let deps = ref [] in
      collect (fun sym -> if Hashtbl.mem m.discardable sym then deps := sym :: !deps);
      Hashtbl.replace edges name !deps
    end else collect root
  in
  iter t (fun f ->
    if f.defined then refs f.name (f.linkage = Local)
      (fun visit -> function_refs f ~address:visit ~call:(fun name _ -> visit name)));
  Vec.iter (fun g ->
    if g.gdefined then refs g.gname (g.glinkage = Local) (global_refs g)) m.globals;
  Vec.iter (fun a -> root a.target) m.aliases;
  Vec.iter (fun f -> root f.resolver) m.ifuncs;
  List.iter (fun s -> asm_symbols s root) m.module_asm;
  List.iter (fun v -> Option.iter (fun (name, _) -> root name) v.vsymbol) m.dglobals;
  while not (Queue.is_empty pending) do
    let name = Queue.take pending in
    List.iter root (Option.value (Hashtbl.find_opt edges name) ~default:[])
  done;
  let dead name = Hashtbl.mem edges name && not (Hashtbl.mem live name) in
  if Hashtbl.fold (fun name _ any -> any || dead name) edges false then begin
    Vec.iter (fun g ->
      if dead g.gname then begin g.gdefined <- false; g.init <- None end) m.globals;
    map t (fun f -> if dead f.name then { (func_header f) with defined = false } else f)
  end

(* A kernel translation unit cannot interpose its own global definitions.
   Local functions can additionally be specialized when their address never
   escapes and every direct call agrees on an argument's value. *)
let fold_kernel_calls t =
  let pinned = pinned_functions t in
  let get_const f v =
    match const_of f v with
    | Some ((Cint _ | Cnull _ | Czero (Int _ | Ptr _)) as c) -> Some c
    | _ -> None
  in
  let const_type = function
    | Cint (ty, _) | Cnull ty | Czero ty -> ty
    | _ -> assert false
  in
  let callable f = can_fold pinned f && f.linkage <> Weak in
  let constant_return f =
    let result = ref None and disagree = ref false in
    Vec.iter (fun (b : block_rec) ->
      match b.term with
      | Ret [|v|] ->
          (match !result, get_const f v with
          | _, None -> disagree := true
          | None, c -> result := c
          | Some a, Some c when a <> c -> disagree := true
          | _ -> ())
      | Ret _ -> disagree := true
      | _ -> ()) f.blocks;
    if !disagree then None else !result
  in
  let empty_return f =
    (* Parameter homes are written on entry. A short unconditional path
       containing only writes to those private homes has no effects. *)
    let effect_free i =
      match (inst f i).op with
      | Nop -> true
      | Alloca (size, _) -> (match int_value f size with Some n -> n >= 0L && n <= 256L | None -> false)
      | Store ({ volatile = false; order = Plain; _ }, _, p) ->
          (match def_of f (resolve f p) with
          | Result (p, _) -> (match (inst f p).op with Alloca _ -> true | _ -> false)
          | _ -> false)
      | _ -> false
    in
    let rec path b left =
      left > 0 && (let br = block f b in
        Vec.fold (fun ok i -> ok && effect_free i) true br.insts &&
        match br.term with
        | Ret [||] -> true
        | Jump target -> path target.dest (left - 1)
        | _ -> false)
    in
    path entry (num_blocks f)
  in
  let rounds = ref 0 and progress = ref true in
  while !progress && !rounds < 6 do
    incr rounds;
    progress := false;
    drop_unused t;
    let incoming = Hashtbl.create 32 and returns = Hashtbl.create 32 in
    let empty = Hashtbl.create 16 and escaped = Hashtbl.create 32 in
    let escape name = Hashtbl.replace escaped name () in
    iter t (fun f ->
      if callable f then begin
        Option.iter (Hashtbl.replace returns f.name) (constant_return f);
        if empty_return f then Hashtbl.add empty f.name ()
      end;
      if f.defined then function_refs f ~address:escape ~call:(fun name args ->
        let values = Array.map (get_const f) args in
        match Hashtbl.find_opt incoming name with
        | None -> Hashtbl.add incoming name values
        | Some prev ->
            Array.iteri (fun i c ->
              if i >= Array.length values || c <> values.(i) then prev.(i) <- None) prev));
    Vec.iter (fun g -> if g.gdefined then global_refs g escape) t.modul.globals;
    Vec.iter (fun a -> escape a.target) t.modul.aliases;
    Vec.iter (fun f -> escape f.resolver) t.modul.ifuncs;
    List.iter (fun asm -> asm_symbols asm escape) t.modul.module_asm;
    map t (fun f ->
      if can_fold pinned f then begin
        let replace = Hashtbl.create 8 in
        if f.linkage = Local && Hashtbl.mem t.modul.discardable f.name && not (Hashtbl.mem escaped f.name) then
          Option.iter (fun args ->
            Array.iteri (fun i p ->
              if i < Array.length args then Option.iter (fun c ->
                let ty = ty_of f p in
                if ty = const_type c then Hashtbl.add replace p (const f c ty)) args.(i))
              (block f entry).params) (Hashtbl.find_opt incoming f.name);
        let used = Hashtbl.create 32 in
        let use v = Hashtbl.replace used v () in
        Vec.iter (fun (b : block_rec) ->
          Vec.iter (fun i -> iter_op_uses use (inst f i).op) b.insts;
          iter_term_uses use b.term) f.blocks;
        let changed = ref false in
        Vec.iter (fun (b : block_rec) -> Vec.iter (fun i ->
          let ir = inst f i in
          match ir.op, ir.results with
          | Call { callee = Direct name; _ }, [||] when Hashtbl.mem empty name ->
              ir.op <- Nop; changed := true
          | Call { callee = Direct name; _ }, [|v|] when Hashtbl.mem used v ->
              Option.iter (fun c ->
                let ty = ty_of f v in
                if ty = const_type c then Hashtbl.replace replace v (const f c ty))
                (Hashtbl.find_opt returns name)
          | _ -> ()) b.insts) f.blocks;
        if !changed || Hashtbl.length replace > 0 then begin
          let replace_use v =
            match Hashtbl.find_opt replace v with
            | Some c -> progress := true; c
            | None -> v
          in
          Vec.iter (fun (b : block_rec) ->
            Vec.iter (fun i -> let ir = inst f i in ir.op <- map_op_uses replace_use ir.op) b.insts;
            b.term <- map_term_uses replace_use b.term) f.blocks;
          if !changed then progress := true;
          fold_body f
        end
      end;
      f)
  done;
  drop_unused t

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
