(* Mir_driver: the driver's entry to the native backend (`--backend=native`).

   The front end still produces the old Ir, so the module goes
   Ir -> Fir (Fir_of_ir) -> Mir_emit.

   The machine-IR backend does no optimization, so -O0..-O3 give the same
   code; [~optimization] is kept so the driver's calls need not change. *)

let to_fir (ir : Ir.module_) =
  match Fir_of_ir.convert ir with
  | exception Fir_of_ir.Unsupported msg -> Error ("native backend: not supported yet: " ^ msg)
  | m -> (
      match Fir_verify.verify m with
      | [] -> Ok m
      | errs -> Error ("native backend internal error: Fir from Ir:\n" ^ String.concat "\n" errs))

(* a position-independent ELF object; [debug] (-g) adds DWARF line tables *)
let compile_object ?debug ~optimization:_ ir = Result.bind (to_fir ir) (Mir_emit.compile_object ?debug)

(* a static executable, Fas startup *)
let link_files ?debug ~optimization:_ ?inputs ?options ~output ir =
  Result.bind (to_fir ir) (Mir_emit.link_files ?debug ?inputs ?options ~output)
