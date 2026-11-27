(* Promote private scalar locals before lowering to i386 frame slots. *)
open Fir

type access = Read of inst_rec | Write of inst_rec * value option

type local = {
  allocation : inst_rec;
  bytes : int64;
  mutable ty : ty option;
  accesses : (int, access list) Hashtbl.t;
}

let scalar_size = function
  | Ptr 0 -> Some 4L
  | Int n when n > 0 && n <= 64 ->
      Some (if n <= 8 then 1L else if n <= 16 then 2L else if n <= 32 then 4L else 8L)
  | _ -> None

let add_access local b event =
  let events = Option.value (Hashtbl.find_opt local.accesses b) ~default:[] in
  Hashtbl.replace local.accesses b (event :: events)

let private_locals f =
  let candidates = Hashtbl.create 16 in
  Vec.iter (fun i ->
    let ir = inst f i in
    match ir.op with
    | Alloca (n, _) ->
        (match int_value f n with
         | Some (1L | 2L | 4L | 8L as bytes) ->
             Hashtbl.add candidates ir.results.(0)
               { allocation = ir; bytes; ty = None; accesses = Hashtbl.create 8 }
         | _ -> ())
    | _ -> ()) (block f entry).insts;
  let escape v = Hashtbl.remove candidates (resolve f v) in
  let access b ptr ty event =
    let ptr = resolve f ptr in
    match Hashtbl.find_opt candidates ptr with
    | Some local when scalar_size ty = Some local.bytes &&
        (local.ty = None || local.ty = Some ty) ->
        local.ty <- Some ty;
        add_access local b event
    | Some _ -> escape ptr
    | None -> ()
  in
  Vec.iteri (fun b (br : block_rec) ->
    Vec.iter (fun i ->
      let ir = inst f i in
      match ir.op with
      | Load ({ volatile = false; order = Plain; _ }, p) ->
          access b p (ty_of f ir.results.(0)) (Read ir)
      | Store ({ volatile = false; order = Plain; _ }, v, p) ->
          escape v;
          access b p (ty_of f v) (Write (ir, Some v))
      | Memset (false, p, v, n) ->
          (match Hashtbl.find_opt candidates (resolve f p) with
           | Some local when int_value f n = Some local.bytes && int_value f v = Some 0L ->
               add_access local b (Write (ir, None))
           | _ -> escape p)
      | op -> iter_op_uses escape op) br.insts;
    iter_term_uses escape br.term) f.blocks;
  candidates

let promote f cfg rpo local ty =
  let n = num_blocks f in
  let events = Array.init n (fun b ->
    List.rev (Option.value (Hashtbl.find_opt local.accesses b) ~default:[])) in
  let written = Array.map (List.exists (function Write _ -> true | Read _ -> false)) events in
  let live = Array.make n false in
  let todo = Queue.create () in
  let require b =
    if not live.(b) then begin live.(b) <- true; Queue.add b todo end in
  Array.iteri (fun b -> function Read _ :: _ -> require b | _ -> ()) events;
  while not (Queue.is_empty todo) do
    Fir_dom.iter_preds cfg (Queue.take todo) (fun b -> if not written.(b) then require b)
  done;
  let undef = poison f ty in
  let params = Array.init n (fun b ->
    if b <> entry && live.(b) && cfg.Fir_dom.pred_start.(b + 1) - cfg.pred_start.(b) > 1
    then Some (add_param f b ty)
    else None) in
  let outgoing = Array.make n undef in
  Array.iter (fun b ->
    let current = ref (match params.(b) with
      | Some p -> p
      | None when b <> entry && live.(b) ->
          outgoing.(cfg.preds.(cfg.pred_start.(b)))
      | None -> undef) in
    List.iter (function
      | Read ir ->
          replace_all_uses f ir.results.(0) !current;
          ir.op <- Nop
      | Write (ir, value) ->
          current := (match value with Some v -> v | None -> zero f ty);
          ir.op <- Nop) events.(b);
    outgoing.(b) <- !current) rpo;
  let edge b target =
    match params.(target.dest) with
    | None -> target
    | Some _ -> { target with args = Array.append target.args [| outgoing.(b) |] } in
  Vec.iteri (fun b (br : block_rec) ->
    let edge = edge b in
    br.term <- match br.term with
      | Jump t -> Jump (edge t)
      | Br (c, t, e, w) -> Br (c, edge t, edge e, w)
      | Switch (c, d, ts, w) -> Switch (c, edge d, Array.map (fun (k, t) -> (k, edge t)) ts, w)
      | term -> term) f.blocks;
  Array.iteri (fun b -> function
    | None -> ()
    | Some p ->
        (* A join that merely forwards the same value needs no edge copies. *)
        let values = ref [] in
        Fir_dom.iter_preds cfg b (fun pred ->
          let v = resolve f outgoing.(pred) in
          if v <> p && not (List.mem v !values) then values := v :: !values);
        (match !values with [v] -> replace_all_uses f p v | _ -> ())) params;
  local.allocation.op <- Nop

let simplify_values f =
  let operand v =
    match def_of f (resolve f v) with
    | Result (i, _) -> (inst f i).op
    | _ -> Nop in
  let one = iconst f i1 1L in
  Vec.iter (fun (b : block_rec) ->
    Vec.iter (fun i ->
      let ir = inst f i in
      let replace v =
        replace_all_uses f ir.results.(0) v;
        ir.op <- Nop in
      match ir.op with
      | Ptradd (_, base, idx, scale, disp) ->
          (match const_of f base, int_value f idx with
           | Some (Csym (name, at)), Some idx ->
               let off = Int64.(add at (add (mul idx (of_int scale)) (of_int disp))) in
               let off = Int64.of_int32 (Int64.to_int32 off) in
               replace (const f (Csym (name, off)) (Ptr 0))
           | _ ->
               (match operand idx with
                | Conv ((Zext | Sext), narrowed) when ty_of f narrowed = Int 32 ->
                    (match ir.op with
                     | Ptradd (flags, _, _, _, _) -> ir.op <- Ptradd (flags, base, narrowed, scale, disp)
                     | _ -> assert false)
                | _ -> ()))
      | Icmp ((Eq | Ne as rel), x, y) when int_value f y = Some 0L ->
          (match operand x with
           | Conv ((Zext | Sext), v) when ty_of f v = i1 ->
               if rel = Ne then replace v else ir.op <- Ibin (Xor, 0, v, one)
           | _ -> ())
      | Ibin (Xor, _, x, y) when ty_of f x = i1 && int_value f y = Some 1L ->
          (match operand x with Ibin (Xor, _, v, k) when int_value f k = Some 1L -> replace v | _ -> ())
      | _ -> ()) b.insts;
    match b.term with
    | Br (v, yes, no, w) ->
        (match operand v with
         | Ibin (Xor, _, x, one) when int_value f one = Some 1L ->
             b.term <- Br (x, no, yes, (if w < 0 then w else 1000 - w))
         | _ -> ())
    | _ -> ()) f.blocks

let eliminate_unused f =
  let needed = Array.make (Vec.length f.insts) false in
  let used_params = Array.make (num_values f) false in
  let incoming = Array.make (num_blocks f) [] in
  Vec.iter (fun (b : block_rec) ->
    List.iter (fun (t : target) ->
      incoming.(t.dest) <- t.args :: incoming.(t.dest)) (Fir_fold.targets b.term)) f.blocks;
  let work = Queue.create () in
  let mark i =
    if not needed.(i) then begin needed.(i) <- true; Queue.add i work end in
  let rec value v =
    let v = resolve f v in
    match def_of f v with
    | Result (i, _) -> mark i
    | Param (b, k) when b <> entry && not used_params.(v) ->
        used_params.(v) <- true;
        List.iter (fun args -> value args.(k)) incoming.(b)
    | _ -> () in
  Vec.iter (fun (b : block_rec) ->
    Vec.iter (fun i ->
      match (inst f i).op with
      | Ibin ((Sdiv | Udiv | Srem | Urem), flags, _, _) when has flags f_trap -> mark i
      | Mark _ -> mark i
      | op when has_side_effects op -> mark i
      | _ -> ()) b.insts;
    match b.term with
    | Br (v, _, _, _) | Switch (v, _, _, _) -> value v
    | Jump _ -> ()
    | t -> iter_term_uses value t) f.blocks;
  while not (Queue.is_empty work) do
    iter_op_uses value (inst f (Queue.take work)).op
  done;
  Vec.iteri (fun b (br : block_rec) ->
    Vec.iter (fun i -> if not needed.(i) then (inst f i).op <- Nop) br.insts;
    if b <> entry then Array.iter (fun v ->
      if not used_params.(v) then replace_all_uses f v (poison f (ty_of f v))) br.params) f.blocks;
  Fir_fold.drop_params f;
  drop_nops f

let shortcut_jumps f =
  let follow (t : target) =
    let rec next budget t =
      let b = block f t.dest in
      if budget = 0 || Array.length b.params <> 0 || Vec.length b.insts <> 0 then t
      else match b.term with Jump dest -> next (budget - 1) dest | _ -> t in
    next (num_blocks f) t in
  Vec.iter (fun (b : block_rec) -> match b.term with
    | Jump t -> b.term <- Jump (follow t)
    | Br (c, y, n, w) ->
        let y = follow y and n = follow n in
        b.term <- if y = n then Jump y else Br (c, y, n, w)
    | Switch (v, d, cases, w) ->
        b.term <- Switch (v, follow d, Array.map (fun (k, t) -> (k, follow t)) cases, w)
    | _ -> ()) f.blocks;
  ignore (remove_unreachable_blocks f)

let run f =
  if f.defined && f.unwind_cleanup = None &&
     not (Vec.fold (fun found (b : block_rec) ->
        found || match b.term with Indirect_br _ | Asm_goto _ -> true | _ -> false) false f.blocks)
  then begin
    let cfg = Fir_dom.cfg f in
    let rpo = Fir_dom.reverse_postorder cfg in
    (* No promotion in a function with disconnected blocks. *)
    if Array.length rpo = num_blocks f then begin
      let locals = private_locals f in
      Hashtbl.to_seq locals |> List.of_seq |> List.sort (fun (a, _) (b, _) -> compare a b)
      |> List.iter (fun (_, local) ->
        match local.ty with Some ty -> promote f cfg rpo local ty | None -> ());
      Fir_stream.fold_body f;
      simplify_values f;
      eliminate_unused f;
      shortcut_jumps f
    end
  end;
  f

(* Inline bounded, straight-line local helpers after scalar promotion. The
   callee has no private stack objects or control-flow edges to clone. *)
let inline_helpers stream pinned =
  let candidates = Hashtbl.create 16 and calls = Hashtbl.create 32 in
  let take_body (f : func) =
    let safe = function
      | Conv _ | Ibin _ | Iun _ | Icmp _ | Ovf _ | Ptradd _ | Load _ | Store _ | Select _ | Freeze _ -> true
      | Asm { tmpl; _ } -> not (String.contains tmpl '.') && not (String.contains tmpl ':')
      | _ -> false in
    let rec collect budget b body =
      if budget = 0 then None
      else
        let br = block f b in
        let body = Vec.fold (fun is i -> i :: is) body br.insts in
        match br.term with
        | Jump t when Array.length t.args = 0 -> collect (budget - 1) t.dest body
        | Ret vs when List.length body <= 32 && List.for_all (fun i -> safe (inst f i).op) body ->
            Some (List.rev body, vs)
        | _ -> None in
    collect (num_blocks f) entry [] in
  Fir_stream.iter stream (fun f ->
    if Fir_stream.can_fold pinned f then begin
      Fir_stream.function_refs f ~address:(fun _ -> ()) ~call:(fun name _ ->
        let count = Option.value (Hashtbl.find_opt calls name) ~default:0 in
        Hashtbl.replace calls name (count + 1));
      if f.linkage = Local && Hashtbl.mem stream.modul.discardable f.name &&
         not f.sig_.variadic && not f.sig_.ms_abi && f.patchable = None &&
         not (has f.fattrs (fa_noinline lor fa_noreturn lor fa_secret lor fa_fenv)) then
        Option.iter (fun (body, ret) -> Hashtbl.add candidates f.name (f, body, ret))
          (take_body f)
    end);
  if Hashtbl.length candidates <> 0 then
    Fir_stream.map stream (fun f ->
      if Fir_stream.can_fold pinned f then begin
        let changed = ref false in
        let candidate = function
          | Call { sig_; callee = Direct name; args; attrs; va_aggs = []; _ }
              when not (has attrs (a_noreturn lor a_returns_twice)) ->
              (match Hashtbl.find_opt candidates name with
               | Some (source, body, ret) when source.sig_ = sig_ && source.code_section = f.code_section &&
                   (List.length body <= 5 || Hashtbl.find calls name = 1) ->
                   Some (source, body, ret, args)
               | _ -> None)
          | _ -> None in
        Vec.iter (fun (b : block_rec) ->
          let original = b.insts in
          if Vec.fold (fun yes i -> yes || candidate (inst f i).op <> None) false original then begin
            b.insts <- Vec.create ~capacity:(Vec.length original) 0;
            Vec.iter (fun i ->
              let ir = inst f i in
              match candidate ir.op with
              | None -> ignore (Vec.push b.insts i)
              | Some (source, body, ret, args) ->
                  changed := true;
                  let translated = Hashtbl.create 16 in
                  Array.iteri (fun k v -> Hashtbl.add translated v args.(k)) (block source entry).params;
                  let value v =
                    let v = resolve source v in
                    match def_of source v with
                    | Const c -> const f c (ty_of source v)
                    | _ -> Hashtbl.find translated v in
                  List.iter (fun i ->
                    let old = inst source i in
                    let op = map_op_uses value old.op in
                    let new_id = make_inst ~loc:ir.loc f op (Array.map (ty_of source) old.results) in
                    ignore (Vec.push b.insts new_id);
                    Array.iteri (fun k v -> Hashtbl.add translated v (inst f new_id).results.(k)) old.results)
                    body;
                  Array.iteri (fun k v -> replace_all_uses f v (value ret.(k))) ir.results)
              original
          end) f.blocks;
        if !changed then ignore (run f)
      end;
      f)
