(* Mir_ct must accept the real backend's code and catch leaks, or its
   silence proves nothing. For every function of the Fir test programs
   (test/fas_fir) that the Mir path supports:
   - the selected and allocated code must pass, with every register and
     with only four of each class (spills and reloads of secrets);
   - in a function with constant-time marks, each leak injected right after
     a `secret` mark (a division by the secret, a load through it, a
     conditional jump on flags set from it) must be refused, and the load
     and the jump injected after a `declassify` mark must pass. *)

open Mir

let files dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".fir")
  |> List.sort compare
  |> List.map (Filename.concat dir)

let passes mf ra =
  match Mir_ct.run mf ra with () -> true | exception Mir_ct.Failure _ -> false

let () =
  let injected = ref 0 and marked = ref 0 in
  (* the allocator reads FAS_MIR_REGS once, at start-up: test/dune runs
     this test again with FAS_MIR_REGS=4, so values spill and reload *)
  let run () =
    List.iter
      (fun path ->
        let m =
          Fir_parse.parse_module (In_channel.with_open_bin path In_channel.input_all)
        in
        Fir_wide.run m;
        let env = Mir_isel.env m in
        Fir.Vec.iter
          (fun (fn : Fir.func) ->
            if fn.defined then
              match Mir_isel.select ~pic:true env fn with
              | exception Mir_isel.Unsupported _ -> ()
              | mf ->
                  let ra = Mir_ra.run mf in
                  (match Mir_ct.run mf ra with
                  | () -> ()
                  | exception Mir_ct.Failure msg ->
                      failwith
                        (Printf.sprintf "%s @%s: the real code fails the check:\n%s"
                           path fn.name msg));
                  if Hashtbl.length mf.marks > 0 then incr marked;
                  Array.iteri
                    (fun b its ->
                      List.iteri
                        (fun k it ->
                          match it with
                          | Mir_ra.Orig (Copy (V v, _), (Copy (d, _) as after))
                            when Hashtbl.mem mf.marks v ->
                              let secret = Hashtbl.find mf.marks v in
                              let try_items extra term =
                                let items = Array.copy ra.items
                                and terms = Array.copy ra.terms in
                                items.(b) <-
                                  List.concat
                                    (List.mapi
                                       (fun j x -> if j = k then x :: extra else [ x ])
                                       its);
                                (match term with
                                | Some t -> terms.(b) <- t
                                | None -> ());
                                incr injected;
                                let ok = passes mf { ra with items; terms } in
                                if ok = secret then
                                  failwith
                                    (Printf.sprintf
                                       "%s @%s: block %d: %s after %s was %s" path
                                       fn.name b
                                       (String.concat "; "
                                          (List.map
                                             (function
                                               | Mir_ra.Orig (_, i) -> ins_s i
                                               | _ -> "?")
                                             extra))
                                       (ins_s after)
                                       (if ok then "accepted" else "refused"))
                              in
                              if class_of_phys d = Gpr then begin
                                let tmp = if d = G Rcx then G Rdx else G Rcx in
                                let o i = Mir_ra.Orig (i, i) in
                                (* div also reads rax and rdx, which may hold other secrets *)
                                if secret then
                                  try_items
                                    [ o (Copy (tmp, d)); o (Div (false, tmp)) ]
                                    None;
                                try_items [ o (Load (8, false, tmp, at_reg d)) ] None;
                                match ra.terms.(b) with
                                | Jmp s when k = List.length its - 1 ->
                                    try_items
                                      [ o (Test (d, d)) ]
                                      (Some (Jcc (X64.Ne, s, s)))
                                | _ -> ()
                              end
                          | _ -> ())
                        its)
                    ra.items)
          m.funcs)
      (files "fas_fir")
  in
  run ();
  if !marked = 0 then
    failwith "no function with constant-time marks: the test checks nothing";
  if !injected = 0 then failwith "no leak was injected: the test checks nothing";
  Printf.printf
    "mir_ct: %d marked functions, %d injected leaks caught or allowed as expected\n"
    !marked !injected
