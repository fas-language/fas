(* Loop-invariant code motion: pure instructions of a natural loop whose
   operands are all defined outside it move to the loop's preheader (made by
   splitting the edge from the loop's single outside predecessor when that
   block branches elsewhere too). Inner loops go first, so a value hoisted
   into an inner preheader can leave the outer loop as well. Compares stay:
   one feeding a branch is fused into it. FAS_NATIVE_NOLICM=1 opts out. *)

open Ir

let on = Sys.getenv_opt "FAS_NATIVE_NOLICM" <> Some "1"
let hoistable = function Cmp _ | Phi _ -> false | ins -> Mem2reg.pure ins

let func (f : func) =
  match f.blocks with
  | [] -> f
  | entry :: _ ->
      let by_id = Hashtbl.create 64 in
      List.iter (fun (b : block) -> Hashtbl.replace by_id b.id b) f.blocks;
      let succs id =
        match Hashtbl.find_opt by_id id with
        | Some (b : block) -> terminator_successors b.terminator
        | None -> []
      in
      (* reverse postorder and dominators (Cooper, Harvey and Kennedy) *)
      let visited = Hashtbl.create 64 and post = ref [] in
      let rec dfs id =
        if not (Hashtbl.mem visited id) then begin
          Hashtbl.replace visited id ();
          List.iter dfs (succs id);
          post := id :: !post
        end
      in
      dfs entry.id;
      let rpo = Array.of_list !post in
      let n = Array.length rpo in
      let order = Hashtbl.create 64 in
      Array.iteri (fun i id -> Hashtbl.replace order id i) rpo;
      let preds = Hashtbl.create 64 in
      Array.iter
        (fun id ->
          List.iter
            (fun s ->
              Hashtbl.replace preds s
                (id :: Option.value ~default:[] (Hashtbl.find_opt preds s)))
            (succs id))
        rpo;
      let preds_of id = Option.value ~default:[] (Hashtbl.find_opt preds id) in
      let idom = Array.make n (-1) in
      idom.(0) <- 0;
      let rec intersect a b =
        if a = b then a
        else if a > b then intersect idom.(a) b
        else intersect a idom.(b)
      in
      let changed = ref true in
      while !changed do
        changed := false;
        for i = 1 to n - 1 do
          match
            List.filter_map
              (fun p ->
                match Hashtbl.find_opt order p with
                | Some j when idom.(j) >= 0 -> Some j
                | _ -> None)
              (preds_of rpo.(i))
          with
          | [] -> ()
          | first :: rest ->
              let d = List.fold_left intersect first rest in
              if idom.(i) <> d then begin
                idom.(i) <- d;
                changed := true
              end
        done
      done;
      let rec dominates a b =
        a = b || (b <> 0 && idom.(b) <> b && dominates a idom.(b))
      in
      (* natural loops, merged per header *)
      let bodies = Hashtbl.create 8 in
      Array.iteri
        (fun i id ->
          List.iter
            (fun s ->
              match Hashtbl.find_opt order s with
              | Some h when dominates h i ->
                  let body =
                    match Hashtbl.find_opt bodies s with
                    | Some t -> t
                    | None ->
                        let t = Hashtbl.create 16 in
                        Hashtbl.replace t s ();
                        Hashtbl.replace bodies s t;
                        t
                  in
                  let rec walk b =
                    if not (Hashtbl.mem body b) then begin
                      Hashtbl.replace body b ();
                      List.iter walk (preds_of b)
                    end
                  in
                  walk id
              | _ -> ())
            (succs id))
        rpo;
      if Hashtbl.length bodies = 0 then f
      else
        let loops =
          Hashtbl.fold (fun h body acc -> (h, body) :: acc) bodies []
          |> List.sort (fun (_, a) (_, b) ->
              compare (Hashtbl.length a) (Hashtbl.length b))
        in
        let def_block = Hashtbl.create 256 in
        Hashtbl.iter
          (fun _ (b : block) ->
            List.iter
              (fun ins ->
                match instruction_result ins with
                | Some (id, _) -> Hashtbl.replace def_block id b.id
                | None -> ())
              b.instrs)
          by_id;
        let next_block = ref (1 + Hashtbl.fold (fun id _ acc -> max id acc) by_id 0) in
        let instrs = Hashtbl.create 64 in
        Hashtbl.iter (fun id (b : block) -> Hashtbl.replace instrs id b.instrs) by_id;
        let terms = Hashtbl.create 64 in
        Hashtbl.iter (fun id (b : block) -> Hashtbl.replace terms id b.terminator) by_id;
        let new_blocks = ref [] in
        let moved = ref false in
        List.iter
          (fun (h, body) ->
            let outside =
              List.filter (fun p -> not (Hashtbl.mem body p)) (preds_of h)
            in
            match List.sort_uniq compare outside with
            | [ p ] when Hashtbl.mem order p -> (
                let invariant = function
                  | Local (id, _) -> (
                      match Hashtbl.find_opt def_block id with
                      | Some b -> not (Hashtbl.mem body b)
                      | None -> true)
                  | _ -> true
                in
                (* candidates, in the loop's layout order so operands come
                   before their users *)
                let hoisted = ref [] in
                let rec visit b =
                  List.iter (fun (ph, _, h) -> if h = b then visit_one ph) !new_blocks;
                  visit_one b
                and visit_one b =
                  if Hashtbl.mem body b then begin
                    let keep =
                      List.filter
                        (fun ins ->
                          let ok =
                            hoistable ins
                            &&
                            let all = ref true in
                            ignore
                              (Ir_fold.map_instr
                                 (fun v ->
                                   if not (invariant v) then all := false;
                                   v)
                                 ins);
                            !all
                          in
                          if ok then begin
                            hoisted := ins :: !hoisted;
                            (* now defined outside: users may follow *)
                            match instruction_result ins with
                            | Some (id, _) -> Hashtbl.replace def_block id (-1)
                            | None -> ()
                          end;
                          not ok)
                        (Hashtbl.find instrs b)
                    in
                    Hashtbl.replace instrs b keep
                  end
                in
                Array.iter visit rpo;
                match List.rev !hoisted with
                | [] -> ()
                | hs ->
                    moved := true;
                    let ph =
                      if
                        List.length (List.sort_uniq compare (succs p)) = 1
                        && List.length (Hashtbl.find terms p |> terminator_successors)
                           = 1
                      then p
                      else begin
                        let ph = !next_block in
                        incr next_block;
                        let retarget = function
                          | Br b when b = h -> Br ph
                          | CondBr (c, a, b) ->
                              CondBr
                                (c, (if a = h then ph else a), if b = h then ph else b)
                          | Switch (t, v, cases, d) ->
                              Switch
                                ( t,
                                  v,
                                  List.map
                                    (fun (c, b) -> (c, if b = h then ph else b))
                                    cases,
                                  if d = h then ph else d )
                          | t -> t
                        in
                        Hashtbl.replace terms p (retarget (Hashtbl.find terms p));
                        Hashtbl.replace terms ph (Br h);
                        Hashtbl.replace instrs ph [];
                        Hashtbl.replace instrs h
                          (List.map
                             (function
                               | Phi (i, t, ins) ->
                                   Phi
                                     ( i,
                                       t,
                                       List.map
                                         (fun (v, q) -> (v, if q = p then ph else q))
                                         ins )
                               | x -> x)
                             (Hashtbl.find instrs h));
                        new_blocks := (ph, p, h) :: !new_blocks;
                        (* the preheader belongs to every loop holding p *)
                        List.iter
                          (fun (_, b) ->
                            if Hashtbl.mem b p then Hashtbl.replace b ph ())
                          loops;
                        ph
                      end
                    in
                    List.iter
                      (fun ins ->
                        match instruction_result ins with
                        | Some (id, _) -> Hashtbl.replace def_block id ph
                        | None -> ())
                      hs;
                    Hashtbl.replace instrs ph (Hashtbl.find instrs ph @ hs))
            | _ -> ())
          loops;
        if not !moved then f
        else
          (* layout: a new preheader goes right before its header *)
          let blocks =
            List.concat_map
              (fun (b : block) ->
                let pre =
                  List.filter_map
                    (fun (ph, _, h) ->
                      if h = b.id then
                        Some
                          {
                            id = ph;
                            label = b.label ^ ".ph";
                            instrs = Hashtbl.find instrs ph;
                            terminator = Hashtbl.find terms ph;
                          }
                      else None)
                    !new_blocks
                in
                pre
                @ [
                    {
                      b with
                      instrs = Hashtbl.find instrs b.id;
                      terminator = Hashtbl.find terms b.id;
                    };
                  ])
              f.blocks
          in
          { f with blocks }

let module_ (m : module_) = if on then { m with funcs = List.map func m.funcs } else m
