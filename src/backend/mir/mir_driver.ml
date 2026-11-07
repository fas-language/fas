(* Mir_driver: the driver's entry to the native backend (`--backend=native`).

   The driver hands over the lowered, folded Fir module (symbols already
   renamed for C adapters); it is verified, then compiled by Mir_emit. The
   module is consumed: the backend's passes rewrite it in place.

   The machine-IR backend does no optimization, so -O0..-O3 give the same
   code; [~optimization] is kept so the driver's calls need not change. *)

let verified (m : Fir.modul) =
  match Fir_verify.verify m with
  | [] -> Ok m
  | errs -> Error ("native backend internal error: invalid Fir:\n" ^ String.concat "\n" errs)

(* a position-independent ELF object; [debug] (-g) adds DWARF line tables *)
let compile_object ?debug ~optimization:_ m = Result.bind (verified m) (Mir_emit.compile_object ?debug)

(* a static executable, Fas startup *)
let link_files ?debug ~optimization:_ ?inputs ?options ~output m =
  Result.bind (verified m) (Mir_emit.link_files ?debug ?inputs ?options ~output)
