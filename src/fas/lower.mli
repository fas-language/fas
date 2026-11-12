(* Lowers a checked program to a Fir module (roadmap v0.6: one IR). The
   module is verified; an Error is an internal error. *)
val lower :
  ?intrinsic_masked_loads:bool ->
  Hir.program ->
  (Fir.modul, Diag.t list) result
