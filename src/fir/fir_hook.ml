(* Test hook between the front end and the native backend.

   FAS_FIR=check      verify the Fir module and check that its printed text
                      parses back to the same text.
   FAS_FIR_DUMP=path  also write the printed Fir module to path.

   FAS_FIR=roundtrip  the same as check. Before v0.6 it also converted the
                      module back to the old Ir; scripts still use the name.

   Without FAS_FIR the hook is the identity and costs nothing. *)

let check (m : Fir.modul) =
  match Fir_verify.verify m with
  | _ :: _ as errs -> Error ("fir: verifier:\n" ^ String.concat "\n" errs)
  | [] -> (
      let text = Fir_print.module_to_string m in
      (match Sys.getenv_opt "FAS_FIR_DUMP" with
      | Some path ->
          let oc = open_out_bin path in
          output_string oc text;
          close_out oc
      | None -> ());
      match Fir_parse.parse_module text with
      | exception Fir_parse.Error (line, msg) -> Error (Printf.sprintf "fir: reparse line %d: %s" line msg)
      | m2 ->
          let text2 = Fir_print.module_to_string m2 in
          if text2 <> text then Error "fir: printed text does not round-trip" else Ok m)

let apply (m : Fir.modul) =
  match Sys.getenv_opt "FAS_FIR" with
  | None | Some "" -> Ok m
  | Some ("check" | "roundtrip") -> check m
  | Some other -> Error ("FAS_FIR: unknown mode " ^ other)
