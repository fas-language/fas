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
  let never_returns f =
    Vec.fold (fun never (b : block_rec) ->
      never && match b.term with Ret _ -> false | _ -> true) true f.blocks
  in
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
  (* Kernel headers rely on GCC and Clang inlining small always_inline helpers
     whose constant arguments fold away calls to deliberately undefined
     functions (bitfield.h's __bad_mask, for example). When the call sites of
     a small local function disagree on an argument, each constant pattern
     gets its own clone, named like GCC's constprop clones. *)
  let clone_limit_insts = 400 and clone_limit_per_function = 32 in
  let clone_names = Hashtbl.create 32 and clones_of = Hashtbl.create 32 in
  let taken = Hashtbl.create 64 in
  iter t (fun f -> Hashtbl.replace taken f.name ());
  Vec.iter (fun g -> Hashtbl.replace taken g.gname ()) t.modul.globals;
  let fresh_clone_name name =
    let rec pick n =
      let candidate = Printf.sprintf "%s.constprop.%d" name n in
      if Hashtbl.mem taken candidate then pick (n + 1) else candidate
    in
    let candidate = pick 0 in
    Hashtbl.replace taken candidate ();
    candidate
  in
  (* A parameter whose only use is a store to a private slot that is never
     read is dead. Direct callers then pass zero, so an argument such as
     dm.c's &dm_dax_ops given to the !CONFIG_DAX alloc_dax stub no longer
     keeps the table and the functions it points to alive. *)
  let dead_params f =
    let other = Hashtbl.create 32 and stored = Hashtbl.create 8 in
    let use v = Hashtbl.replace other (resolve f v) () in
    (* Asm may read argument registers directly, as in a naked function. *)
    let asm = ref false in
    Vec.iter (fun (b : block_rec) ->
      Vec.iter (fun i ->
        match (inst f i).op with
        | Store ({ volatile = false; _ }, v, p) when resolve f v <> resolve f p ->
            Hashtbl.add stored (resolve f v) (resolve f p)
        | Asm _ as op -> asm := true; iter_op_uses use op
        | op -> iter_op_uses use op) b.insts;
      (match b.term with Asm_goto _ -> asm := true | _ -> ());
      iter_term_uses use b.term) f.blocks;
    let private_slot a =
      not (Hashtbl.mem other a) && not (Hashtbl.mem stored a)
      && (match def_of f a with
          | Result (i, _) -> (match (inst f i).op with Alloca _ -> true | _ -> false)
          | _ -> false)
    in
    Array.map (fun p ->
      match ty_of f p with
      | (Int _ | Ptr _) when not !asm ->
          let p = resolve f p in
          not (Hashtbl.mem other p) && List.for_all private_slot (Hashtbl.find_all stored p)
      | _ -> false) (block f entry).params
  in
  let rounds = ref 0 and progress = ref true in
  while !progress && !rounds < 12 do
    incr rounds;
    progress := false;
    drop_unused t;
    let incoming = Hashtbl.create 32 and returns = Hashtbl.create 32 in
    let noreturn = Hashtbl.create 16 in
    let empty = Hashtbl.create 16 and escaped = Hashtbl.create 32 in
    let cloneable = Hashtbl.create 32 and patterns = Hashtbl.create 32 in
    let defined = Hashtbl.create 64 and unused = Hashtbl.create 32 in
    let escape name = Hashtbl.replace escaped name () in
    iter t (fun f ->
      if f.defined then Hashtbl.replace defined f.name ();
      if callable f then begin
        if f.linkage = Local && never_returns f then Hashtbl.replace noreturn f.name ();
        Option.iter (Hashtbl.replace returns f.name) (constant_return f);
        if empty_return f then Hashtbl.add empty f.name ();
        let dead = dead_params f in
        if Array.exists Fun.id dead then Hashtbl.replace unused f.name dead;
        if f.linkage = Local && Hashtbl.mem t.modul.discardable f.name
           && Vec.length f.insts <= clone_limit_insts then
          Hashtbl.replace cloneable f.name (Array.map (ty_of f) (block f entry).params)
      end;
      if f.defined then function_refs f ~address:escape ~call:(fun name args ->
        let values = Array.map (get_const f) args in
        if Array.exists Option.is_some values then Hashtbl.replace patterns (name, values) ();
        match Hashtbl.find_opt incoming name with
        | None -> Hashtbl.add incoming name (Array.copy values)
        | Some prev ->
            Array.iteri (fun i c ->
              if i >= Array.length values || c <> values.(i) then prev.(i) <- None) prev));
    (* A pattern earns a clone when it fixes a parameter that the shared
       specialization below cannot, because other call sites disagree. *)
    let wanted = Hashtbl.create 16 in
    Hashtbl.iter (fun (name, values) () ->
      match Hashtbl.find_opt cloneable name, Hashtbl.find_opt incoming name with
      | Some param_tys, Some agreed when not (Hashtbl.mem clone_names (name, values)) ->
          let useful = ref false in
          Array.iteri (fun i c ->
            match c with
            | Some c when i < Array.length param_tys && param_tys.(i) = const_type c
                          && (i >= Array.length agreed || agreed.(i) = None)
                          && not (match Hashtbl.find_opt unused name with
                                  | Some dead -> i < Array.length dead && dead.(i)
                                  | None -> false) -> useful := true
            | _ -> ()) values;
          let count = Option.value (Hashtbl.find_opt clones_of name) ~default:0 in
          if !useful && count < clone_limit_per_function then begin
            Hashtbl.replace clones_of name (count + 1);
            let clone = fresh_clone_name name in
            Hashtbl.replace clone_names (name, values) clone;
            Hashtbl.replace defined clone ();
            Hashtbl.replace wanted name ((clone, values) :: Option.value (Hashtbl.find_opt wanted name) ~default:[])
          end
      | _ -> ()) patterns;
    let made = ref [] in
    Vec.iter (fun g -> if g.gdefined then global_refs g escape) t.modul.globals;
    Vec.iter (fun a -> escape a.target) t.modul.aliases;
    Vec.iter (fun f -> escape f.resolver) t.modul.ifuncs;
    List.iter (fun asm -> asm_symbols asm escape) t.modul.module_asm;
    let substitute f values =
      let replace = Hashtbl.create 8 in
      Array.iteri (fun i p ->
        if i < Array.length values then Option.iter (fun c ->
          let ty = ty_of f p in
          if ty = const_type c then Hashtbl.add replace p (const f c ty)) values.(i))
        (block f entry).params;
      let replace_use v = Option.value (Hashtbl.find_opt replace v) ~default:v in
      Vec.iter (fun (b : block_rec) ->
        Vec.iter (fun i -> let ir = inst f i in ir.op <- map_op_uses replace_use ir.op) b.insts;
        b.term <- map_term_uses replace_use b.term) f.blocks
    in
    map t (fun f ->
      (* Clone before this round rewrites f's own calls; the clones' calls
         are specialized in later rounds. *)
      List.iter (fun (name, values) ->
        let copy : func = Marshal.from_string (Marshal.to_string f []) 0 in
        let copy = { copy with name } in
        substitute copy values;
        fold_body copy;
        made := copy :: !made)
        (Option.value (Hashtbl.find_opt wanted f.name) ~default:[]);
      if f.defined then
        Vec.iter (fun (b : block_rec) -> Vec.iter (fun i ->
          let ir = inst f i in
          match ir.op with
          | Call ({ callee = Direct name; args; _ } as c) ->
              let zeroed = match Hashtbl.find_opt unused name with
                | None -> args
                | Some dead ->
                    Array.mapi (fun i v ->
                      match ty_of f v with
                      | (Int _ | Ptr _) as ty when i < Array.length dead && dead.(i) ->
                          let zero = const f (Czero ty) ty in
                          if resolve f v = zero then v else zero
                      | _ -> v) args
              in
              let values = Array.map (get_const f) zeroed in
              (* drop_unused may have removed a clone made in an earlier round. *)
              let callee = match Hashtbl.find_opt clone_names (name, values) with
                | Some clone when Hashtbl.mem defined clone -> Direct clone
                | _ -> Direct name
              in
              if zeroed <> args || callee <> Direct name then begin
                ir.op <- Call { c with args = zeroed; callee };
                progress := true
              end
          | _ -> ()) b.insts) f.blocks;
      if can_fold pinned f then begin
        (* Propagate a proven non-returning local definition into its
           callers, including wrappers that have no explicit annotation. *)
        let terminated = ref false in
        Vec.iter (fun (b : block_rec) ->
          let rec search index =
            if index < Vec.length b.insts then
              let ir = inst f (Vec.get b.insts index) in
              match ir.op with
              | Call ({ callee = Direct name; attrs; _ } as c)
                when not (has attrs a_noreturn) && Hashtbl.mem noreturn name ->
                  ir.op <- Call { c with attrs = attrs lor a_noreturn };
                  if index + 1 < Vec.length b.insts then begin
                    let kept = Vec.create ~capacity:(index + 1) 0 in
                    for i = 0 to index do ignore (Vec.push kept (Vec.get b.insts i)) done;
                    b.insts <- kept
                  end;
                  b.term <- Unreachable;
                  b.term_loc <- ir.loc;
                  terminated := true
              | _ -> search (index + 1)
          in
          search 0) f.blocks;
        if !terminated then begin
          progress := true;
          ignore (remove_unreachable_blocks f)
        end;
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
      f);
    List.iter (fun copy ->
      add_func t.modul copy;
      Hashtbl.replace t.modul.discardable copy.name ()) (List.rev !made);
    if !made <> [] then progress := true
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
