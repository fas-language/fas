(* Mir_driver: the driver's entry to the native backend (`--backend=native`).

   The front end still produces the old Ir, so for the machine-IR backend
   the module goes Ir -> Fir (Fir_of_ir) -> Mir_emit.

   The machine-IR backend runs at every optimization level; it does no
   optimization, so -O0..-O3 give the same code. FAS_NATIVE_LEGACY=1 still
   selects the old backend (Native_gen) until it is removed.

   Both entries take the same Ir and return the same results as the
   Native_gen functions they stand for. *)

let flag name = match Sys.getenv_opt name with Some ("" | "0") | None -> false | Some _ -> true

let use_mir ~optimization:_ = not (flag "FAS_NATIVE_LEGACY")

let to_fir (ir : Ir.module_) =
  match Fir_of_ir.convert ir with
  | exception Fir_of_ir.Unsupported msg -> Error ("native backend: not supported yet: " ^ msg)
  | m -> (
      match Fir_verify.verify m with
      | [] -> Ok m
      | errs -> Error ("native backend internal error: Fir from Ir:\n" ^ String.concat "\n" errs))

(* a position-independent ELF object *)
let compile_object ~optimization ir =
  if use_mir ~optimization then Result.bind (to_fir ir) Mir_emit.compile_object else Native_gen.compile_object ir

(* a static executable, Fas startup *)
let link_files ~optimization ?inputs ?options ~output ir =
  if use_mir ~optimization then Result.bind (to_fir ir) (Mir_emit.link_files ?inputs ?options ~output)
  else Native_gen.link_files ?inputs ?options ~output ir
