type unresolved_shape =
  | Unresolved_int
  | Unresolved_float
  | Unresolved_vector
  | Unresolved_null

val unresolved_shape_of : Ast.expr -> unresolved_shape option
val index : Sema_index.t option ref
val layout_source : string -> string
val global_error : Span.t -> string -> ('a, Diag.t list) result

val const_expr :
  ?structs:Hir.struct_def list ->
  ?named_types:Sema_types.named_types ->
  ?generic_structs:Ast.item list ->
  ?arrays:(string * Hir.ty * int64 list) list ->
  ?array_lengths:(string * Ast.aggregate_length) list ->
  ?globals:string list ->
  ?resolve:(check_only:bool -> string -> Span.t -> (Hir.ty * int64, Diag.t list) result) ->
  Sema_types.const_values ->
  Hir.ty option ->
  ?check_only:bool ->
  ?validate_dead:bool ->
  ?allow_widen:bool ->
  Ast.expr ->
  (Hir.ty * int64, Diag.t list) result

val shuffle_indices_in_range : Hir.ty -> int -> int64 list -> bool

val vector_const_expr :
  ?structs:Hir.struct_def list ->
  ?named_types:Sema_types.named_types ->
  ?generic_structs:Ast.item list ->
  ?arrays:(string * Hir.ty * int64 list) list ->
  ?array_lengths:(string * Ast.aggregate_length) list ->
  ?globals:string list ->
  ?resolve:(check_only:bool -> string -> Span.t -> (Hir.ty * int64, Diag.t list) result) ->
  Sema_types.const_values ->
  Hir.ty option ->
  ?check_only:bool ->
  Ast.expr ->
  (Hir.ty * int64 list, Diag.t list) result

val resolve_scalar_declarations :
  ?globals:string list ->
  ?array_lengths:(string * Ast.aggregate_length) list ->
  ?generic_structs:Ast.item list ->
  structs:Hir.struct_def list ->
  named_types:Sema_types.named_types ->
  resolve_type:(Span.t -> Ast.ty -> (Hir.ty, Diag.t list) result) ->
  strict:bool ->
  Ast.item list ->
  (Sema_types.const_values, Diag.t list) result

val query_layout :
  structs:Hir.struct_def list ->
  named_types:Sema_types.named_types ->
  generic_structs:Ast.item list ->
  globals:string list ->
  evaluate:
    (Sema_types.const_values ->
    Ast.expr ->
    Hir.ty option ->
    (Hir.ty * int64, Diag.t list) result) ->
  Sema_types.const_values ->
  Span.t ->
  Ast.ty ->
  (Hir.ty * Hir.struct_def list, Diag.t list) result

val float_binary_bits : Hir.ty -> Ast.binop -> int64 -> int64 -> int64
val float_comparison_bits : Hir.ty -> Ast.binop -> int64 -> int64 -> int64

(* enum builtins, shared with sema_check *)
val enum_type_argument :
  (Span.t -> Ast.ty -> (Hir.ty, Diag.t list) result) ->
  string -> Ast.generic_arg list -> Span.t -> (string * Hir.int_kind, Diag.t list) result

val enum_operand_type :
  string -> Hir.int_kind -> Hir.ty -> Span.t -> (unit, Diag.t list) result

val enum_member_of : string -> int64 -> string option

type const_assert_outcome =
  | Assert_holds
  | Assert_fails of Diag.t list  (** false, or not a bool *)
  | Assert_not_constant of Diag.t list  (** the evaluator could not fold it *)

(** [check_const_assert ~consts cond message span] folds a `const assert`
    condition; a false condition's diagnostic carries [message]. *)
val check_const_assert :
  ?structs:Hir.struct_def list ->
  ?named_types:Sema_types.named_types ->
  ?generic_structs:Ast.item list ->
  ?arrays:(string * Hir.ty * int64 list) list ->
  ?array_lengths:(string * Ast.aggregate_length) list ->
  ?globals:string list ->
  consts:Sema_types.const_values ->
  Ast.expr ->
  string option ->
  Span.t ->
  const_assert_outcome
