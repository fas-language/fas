(* Mir_check must reject broken allocations, or its silence on the real
   allocator's output proves nothing. For every function of the Fir test
   programs (test/fas_fir) that the Mir path supports:
   - the allocator's output must pass the check;
   - each single mutation below must be caught: dropping one reload, or
     giving one virtual-register operand of an instruction a different
     register of its class.
   A mutation can be harmless by luck (the other register happens to hold
   the same value), so a small miss rate is tolerated, not zero detections. *)

open Mir

let files dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".fir")
  |> List.map (Filename.concat dir)

let checks mf ra = match Mir_check.run mf ra with () -> true | exception Mir_check.Failure _ -> false

let other_reg = function
  | G r ->
      let pool = Mir_ra.gpr_pool in
      let rec next = function a :: (b :: _ as rest) -> if a = r then b else next rest | _ -> List.hd pool in
      G (next pool)
  | X i -> X ((i + 1) mod 16)
  | V _ -> assert false

let () =
  let tried = ref 0 and caught = ref 0 and funcs = ref 0 in
  List.iter
    (fun path ->
      let m = Fir_parse.parse_module (In_channel.with_open_bin path In_channel.input_all) in
      Fir_wide.run m;
      let env = Mir_isel.env m in
      Fir.Vec.iter
        (fun (fn : Fir.func) ->
          if fn.defined then
            match Mir_isel.select ~pic:true env fn with
            | exception Mir_isel.Unsupported _ -> ()
            | mf ->
                let ra = Mir_ra.run mf in
                if not (checks mf ra) then failwith (Printf.sprintf "%s @%s: the real allocation fails the check" path fn.name);
                incr funcs;
                let mutate b items =
                  let items' = Array.copy ra.items in
                  items'.(b) <- items;
                  incr tried;
                  if not (checks mf { ra with items = items' }) then incr caught
                in
                Array.iteri
                  (fun b its ->
                    List.iteri
                      (fun k it ->
                        match it with
                        | Mir_ra.Reload _ -> mutate b (List.filteri (fun j _ -> j <> k) its)
                        | Orig (before, after) ->
                            let u, _ = uses_defs before in
                            if List.exists (function V _ -> true | _ -> false) u then begin
                              (* move the first used vreg to another register *)
                              let v = List.find (function V _ -> true | _ -> false) u in
                              let pairs = List.combine (all_regs before) (all_regs after) in
                              let r = List.assoc v pairs in
                              let r' = other_reg r in
                              let after' = map_regs (fun x -> if x = r then r' else x) after in
                              (* only operands that were the vreg's register move *)
                              let after' =
                                if List.exists (fun (b, a) -> a = r && b <> v) pairs then after else after'
                              in
                              if after' <> after then
                                mutate b (List.mapi (fun j x -> if j = k then Mir_ra.Orig (before, after') else x) its)
                            end
                        | Spill _ | Move _ | Ld _ | St _ | Remat _ | Elided _ -> ())
                      its)
                  ra.items)
        m.funcs)
    (files "fas_fir" @ files "fas_fir/c");
  Printf.printf "mir_check: %d functions, %d/%d mutations caught\n" !funcs !caught !tried;
  if !funcs < 10 || !tried < 50 || !caught * 100 < !tried * 95 then begin
    prerr_endline "mir_check_test: too few functions or mutations, or too many mutations missed";
    exit 1
  end
