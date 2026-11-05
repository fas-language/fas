(* Full unrolling of small counted loops for the native backend.

   A loop qualifies when its header [h] has exactly two predecessors, a
   preheader [p] outside the loop and one latch [l] inside, ends with a
   conditional branch on [icmp pred iv, lim] (one target inside the loop, the
   other the exit), and [iv] is a header phi [init from p; iv + step from l]
   with constant [init], [step] and [lim]. The trip count is found by
   running the induction variable at its width. Other exits are allowed only
   to blocks without phis that use no loop value (traps).

   The body is copied once per iteration: copy k's header phis keep a single
   incoming value (the preheader's for k = 0, copy k-1's latch value after),
   its conditional branch becomes a branch into the body, and its latch
   jumps to copy k+1's header. The original header stays as the final copy,
   with its phis fed from the last latch and a branch to the exit, so values
   it defines keep their ids for the code after the loop. Constant folding
   then turns the induction variable of every copy into a constant. *)

open Ir
module Hashtbl = Itbl

let on = Sys.getenv_opt "FAS_NATIVE_NOUNROLL" <> Some "1"
let max_trips = 16
let max_size = 320

let norm ty x =
  match ty with
  | I8 -> Int64.logand x 0xffL
  | I16 -> Int64.logand x 0xffffL
  | I32 -> Int64.logand x 0xffffffffL
  | I64 -> x
  | _ -> raise Exit

let signed ty x =
  let w = match ty with I8 -> 8 | I16 -> 16 | I32 -> 32 | _ -> 64 in
  if w = 64 then x else Int64.shift_right (Int64.shift_left x (64 - w)) (64 - w)

let holds pred ty a b =
  let a = norm ty a and b = norm ty b in
  let u = Int64.unsigned_compare a b and s = compare (signed ty a) (signed ty b) in
  match pred with
  | Eq -> a = b
  | Ne -> a <> b
  | Ult -> u < 0
  | Ule -> u <= 0
  | Ugt -> u > 0
  | Uge -> u >= 0
  | Slt -> s < 0
  | Sle -> s <= 0
  | Sgt -> s > 0
  | Sge -> s >= 0
  | _ -> raise Exit

(* Iterations of [for iv = init; iv pred lim == cont; iv += step] *)
let trips pred ty ~cont init step lim =
  let rec go v n =
    if n > max_trips then None
    else if holds pred ty v lim = cont then go (norm ty (Int64.add v step)) (n + 1)
    else Some n
  in
  go (norm ty init) 0

let set_def id ins =
  match Inline.def ins with Some old -> Inline.shift_def (id - old) ins | None -> ins

let uses_of ins =
  let l = ref [] in
  ignore
    (Ir_fold.map_instr
       (fun v ->
         (match v with Local (i, _) -> l := i :: !l | _ -> ());
         v)
       ins);
  !l

let term_uses t =
  let l = ref [] in
  ignore
    (Ir_fold.map_term
       (fun v ->
         (match v with Local (i, _) -> l := i :: !l | _ -> ());
         v)
       t);
  !l

(* Try to unroll the loop headed by [h]; None when it does not qualify *)
let unroll_at (f : func) by_id preds (h : block) =
  try
    let header_phis =
      List.filter_map
        (function Phi (i, t, inc) -> Some (i, t, inc) | _ -> None)
        h.instrs
    in
    let c, a, b =
      match h.terminator with
      | CondBr (Local (c, _), a, b) -> (c, a, b)
      | _ -> raise Exit
    in
    let pred, cty, iv, lim =
      match
        List.find_map
          (function
            | Cmp (i, p, t, Local (iv, _), Const (_, lim)) when i = c ->
                Some (p, t, iv, lim)
            | _ -> None)
          h.instrs
      with
      | Some x -> x
      | None -> raise Exit
    in
    let p, l = match preds h.id with [ x; y ] -> (x, y) | _ -> raise Exit in
    (* the loop: blocks reaching the latch without passing the header; the
       latch is dominated by the header when that walk misses the entry *)
    let entry = match f.blocks with e :: _ -> e.id | [] -> raise Exit in
    if h.id = entry then raise Exit;
    let loop_from l p =
      let body = Hashtbl.create 16 in
      Hashtbl.replace body h.id ();
      let rec walk x =
        if not (Hashtbl.mem body x) then begin
          Hashtbl.replace body x ();
          List.iter walk (preds x)
        end
      in
      walk l;
      if Hashtbl.mem body p || Hashtbl.mem body entry then None else Some body
    in
    let p, l, body =
      match loop_from l p with
      | Some body -> (p, l, body)
      | None -> (
          match loop_from p l with Some body -> (l, p, body) | None -> raise Exit)
    in
    let inside, exit_, cont =
      match (Hashtbl.mem body a, Hashtbl.mem body b) with
      | true, false -> (a, b, true)
      | false, true -> (b, a, false)
      | _ -> raise Exit
    in
    let init, step =
      match List.find_opt (fun (i, _, _) -> i = iv) header_phis with
      | Some (_, _, inc) -> (
          let from x = List.assoc_opt x (List.map (fun (v, bb) -> (bb, v)) inc) in
          match (from p, from l, List.length inc) with
          | Some (Const (_, init)), Some (Local (nx, _)), 2 -> (
              let st =
                List.find_map
                  (fun bid ->
                    let (blk : block) = Hashtbl.find by_id bid in
                    List.find_map
                      (function
                        | Bin (i, Add, _, Local (v, _), Const (_, s))
                        | Bin (i, Add, _, Const (_, s), Local (v, _))
                          when i = nx && v = iv ->
                            Some s
                        | _ -> None)
                      blk.instrs)
                  (Hashtbl.fold (fun k () acc -> k :: acc) body [])
              in
              match st with Some s -> (init, s) | None -> raise Exit)
          | _ -> raise Exit)
      | None -> raise Exit
    in
    let n =
      match trips pred cty ~cont init step lim with
      | Some n when n >= 1 -> n
      | _ -> raise Exit
    in
    let blocks_in = List.filter (fun (bb : block) -> Hashtbl.mem body bb.id) f.blocks in
    let size =
      List.fold_left
        (fun s (bb : block) ->
          s
          + List.length
              (List.filter (function Location _ -> false | _ -> true) bb.instrs))
        0 blocks_in
    in
    if size * n > max_size then raise Exit;
    (* every header predecessor is p or l, every phi of h has those two *)
    List.iter (fun (_, _, inc) -> if List.length inc <> 2 then raise Exit) header_phis;
    (* values defined in the loop outside the header *)
    let inner_defs = Hashtbl.create 64 and all_defs = Hashtbl.create 64 in
    List.iter
      (fun (bb : block) ->
        List.iter
          (fun ins ->
            match instruction_result ins with
            | Some (i, _) ->
                Hashtbl.replace all_defs i ();
                if bb.id <> h.id then Hashtbl.replace inner_defs i ()
            | None -> ())
          bb.instrs)
      blocks_in;
    (* side exits: no phis and no loop values *)
    List.iter
      (fun (bb : block) ->
        List.iter
          (fun s ->
            if (not (Hashtbl.mem body s)) && not (bb.id = h.id && s = exit_) then begin
              let (sb : block) = Hashtbl.find by_id s in
              if
                List.exists
                  (fun ins ->
                    (match ins with Phi _ -> true | _ -> false)
                    || List.exists (Hashtbl.mem all_defs) (uses_of ins))
                  sb.instrs
                || List.exists (Hashtbl.mem all_defs) (term_uses sb.terminator)
              then raise Exit
            end)
          (Inline.targets bb.terminator))
      blocks_in;
    (* values from inside the body must not be used after the loop *)
    List.iter
      (fun (bb : block) ->
        if not (Hashtbl.mem body bb.id) then
          if
            List.exists
              (fun ins -> List.exists (Hashtbl.mem inner_defs) (uses_of ins))
              bb.instrs
            || List.exists (Hashtbl.mem inner_defs) (term_uses bb.terminator)
          then raise Exit)
      f.blocks;
    let next_v, next_b = Inline.max_ids f in
    let next_v = ref (next_v + 1) and next_b = ref (next_b + 1) in
    (* per copy: block id map and value map *)
    let bmaps = Array.init n (fun _ -> Hashtbl.create 16) in
    let vmaps = Array.init n (fun _ -> Hashtbl.create 64) in
    for k = 0 to n - 1 do
      List.iter
        (fun (bb : block) ->
          Hashtbl.replace bmaps.(k) bb.id !next_b;
          incr next_b;
          List.iter
            (fun ins ->
              match instruction_result ins with
              | Some (i, t) ->
                  Hashtbl.replace vmaps.(k) i (Local (!next_v, t));
                  incr next_v
              | None -> ())
            bb.instrs)
        blocks_in
    done;
    let rename k v =
      match v with
      | Local (i, _) -> (
          match Hashtbl.find_opt vmaps.(k) i with Some w -> w | None -> v)
      | v -> v
    in
    let header_of k = if k >= n then h.id else Hashtbl.find bmaps.(k) h.id in
    let latch_of k = Hashtbl.find bmaps.(k) l in
    (* header phi inputs for copy k: preheader value, or last copy's latch value *)
    let phi_input k inc =
      if k = 0 then
        (List.assoc_opt p (List.map (fun (v, bb) -> (bb, v)) inc) |> Option.get, p)
      else
        ( rename (k - 1)
            (List.assoc_opt l (List.map (fun (v, bb) -> (bb, v)) inc) |> Option.get),
          latch_of (k - 1) )
    in
    let copy k (bb : block) : block =
      let bm = bmaps.(k) in
      let in_block x = match Hashtbl.find_opt bm x with Some y -> y | None -> x in
      let target x = if x = h.id then header_of (k + 1) else in_block x in
      let instrs =
        List.map
          (fun ins ->
            let id' =
              match instruction_result ins with
              | Some (i, _) -> (
                  match Hashtbl.find vmaps.(k) i with
                  | Local (j, _) -> Some j
                  | _ -> None)
              | None -> None
            in
            let ins =
              match ins with
              | Phi (i, t, inc) when bb.id = h.id -> Phi (i, t, [ phi_input k inc ])
              | Phi (i, t, inc) ->
                  Phi (i, t, List.map (fun (v, x) -> (rename k v, in_block x)) inc)
              | ins -> Ir_fold.map_instr (rename k) ins
            in
            match id' with Some j -> set_def j ins | None -> ins)
          bb.instrs
      in
      let terminator =
        if bb.id = h.id then Br (in_block inside)
        else
          match Ir_fold.map_term (rename k) bb.terminator with
          | Br x -> Br (target x)
          | CondBr (cv, x, y) -> CondBr (cv, target x, target y)
          | Switch (t, v, cases, d) ->
              Switch (t, v, List.map (fun (cv, x) -> (cv, target x)) cases, target d)
          | t -> t
      in
      {
        id = Hashtbl.find bm bb.id;
        label = Printf.sprintf "u%d" (Hashtbl.find bm bb.id);
        instrs;
        terminator;
      }
    in
    let final_h =
      {
        h with
        instrs =
          List.map
            (function Phi (i, t, inc) -> Phi (i, t, [ phi_input n inc ]) | ins -> ins)
            h.instrs;
        terminator = Br exit_;
      }
    in
    let blocks =
      List.concat_map
        (fun (bb : block) ->
          if bb.id = h.id then
            List.concat (List.init n (fun k -> List.map (copy k) blocks_in))
            @ [ final_h ]
          else if Hashtbl.mem body bb.id then []
          else if bb.id = p then
            [
              {
                bb with
                terminator =
                  (match bb.terminator with
                  | Br x when x = h.id -> Br (header_of 0)
                  | CondBr (cv, x, y) ->
                      CondBr
                        ( cv,
                          (if x = h.id then header_of 0 else x),
                          if y = h.id then header_of 0 else y )
                  | Switch (t, v, cases, d) ->
                      let m x = if x = h.id then header_of 0 else x in
                      Switch (t, v, List.map (fun (cv, x) -> (cv, m x)) cases, m d)
                  | t -> t);
              };
            ]
          else [ bb ])
        f.blocks
    in
    Some { f with blocks }
  with Exit | Not_found | Invalid_argument _ -> None

let func (f : func) =
  if not on then (f, false)
  else
    let changed = ref false in
    let rec go (f : func) budget =
      if budget = 0 then f
      else
        let by_id = Hashtbl.create 64 in
        List.iter (fun (b : block) -> Hashtbl.replace by_id b.id b) f.blocks;
        let preds = Hashtbl.create 64 in
        List.iter
          (fun (b : block) ->
            List.iter
              (fun s ->
                Hashtbl.replace preds s
                  (b.id :: Option.value ~default:[] (Hashtbl.find_opt preds s)))
              (List.sort_uniq compare (Inline.targets b.terminator)))
          f.blocks;
        let preds_of x = Option.value ~default:[] (Hashtbl.find_opt preds x) in
        match
          List.find_map
            (fun (b : block) ->
              match b.terminator with
              | CondBr _ -> unroll_at f by_id preds_of b
              | _ -> None)
            f.blocks
        with
        | Some f' ->
            changed := true;
            go f' (budget - 1)
        | None -> f
    in
    let f = go f 8 in
    (f, !changed)
