val check :
  ?limits:Limits.t ->
  ?c_aliases:(string * Ast.ty) list ->
  ?c_unsupported:(string * string) list ->
  ?c_nonnull_parameters:(string * int list) list ->
  ?c_string_parameters:(string * (int * string option) list) list ->
  ?c_record_calls:(string * (int list * bool)) list ->
  ?c_alloc_size_parameters:(string * int list list) list ->
  ?c_records:(string * string * string option) list ->
  ?all_errors:bool ->
  ?specializations:Sema_specialization.t ->
  ?index:Sema_index.t ->
  Ast.program ->
  (Hir.program, Diag.t list) result
