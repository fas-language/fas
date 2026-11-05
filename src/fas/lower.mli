val lower :
  ?sanitize_address:bool ->
  ?intrinsic_masked_loads:bool ->
  Hir.program ->
  (Ir.module_, Diag.t list) result
