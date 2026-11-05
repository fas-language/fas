(* Test hook between the front end and the native backend.

   FAS_FIR=check      convert the old Ir to Fir, verify it, check that the
                      printed text parses back to the same text; the old Ir
                      then goes on unchanged.
   FAS_FIR=roundtrip  as check, then convert the Fir back to old Ir and
                      compile that instead (exercises both converters).
   FAS_FIR_DUMP=path  also write the printed Fir module to path.
   FAS_FIR_IRDUMP=path  (roundtrip) write the converted-back Ir as LLVM text.

   Without FAS_FIR the hook is the identity and costs nothing. *)

let check (ir : Ir.module_) =
  match Fir_of_ir.convert ir with
  | exception Fir_of_ir.Unsupported msg -> Error ("fir: unsupported: " ^ msg)
  | m -> (
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
              if text2 <> text then Error "fir: printed text does not round-trip" else Ok m))

let apply (ir : Ir.module_) =
  match Sys.getenv_opt "FAS_FIR" with
  | None | Some "" -> Ok ir
  | Some "check" -> Result.map (fun _ -> ir) (check ir)
  | Some "roundtrip" -> (
      match check ir with
      | Error e -> Error e
      | Ok m -> (
          match Fir_to_ir.convert ~like:ir m with
          | exception Fir_to_ir.Unsupported msg -> Error ("fir -> ir: unsupported: " ^ msg)
          | back ->
              (match Sys.getenv_opt "FAS_FIR_IRDUMP" with
              | Some path ->
                  let oc = open_out_bin path in
                  output_string oc (Ir.render back);
                  close_out oc
              | None -> ());
              Ok back))
  | Some other -> Error ("FAS_FIR: unknown mode " ^ other)
