type named_type_kind =
  | Struct_name
  | Generic_struct_name
  | Opaque_name
  | C_record_name of string * string option
  | Alias_name of Ast.ty
  | Unsupported_name of string * string

type named_types = (string * named_type_kind) list
type const_values = (string * Hir.ty * int64) list

val source_ty : named_types -> Ast.ty -> (Hir.ty, string) result
val handle_target : named_types -> Ast.ty -> (string, string) result
val source_ty_diag : named_types -> Span.t -> Ast.ty -> (Hir.ty, Diag.t list) result

val declaration_type_error :
  declaration:string ->
  variable:string ->
  Ast.ty ->
  ('a, Diag.t list) result ->
  ('a, Diag.t list) result

val source_type_span : Span.t -> Ast.ty -> Span.t
val vector_element_source_span : Span.t -> Ast.ty -> Span.t option
val opaque_source_span : named_types -> string list -> Ast.ty -> Span.t option
val vec_cap_error : int -> Hir.ty -> string option
val condition_error : string -> Ast.expr -> Hir.ty -> Diag.t

val c_pointer_selection_help :
  Ast.expr -> string option -> Hir.ty option -> string option

val c_pointer_selection_diagnostic :
  ?constant:bool -> Hir.ty option -> Ast.expr -> Diag.t

val logical_operand_error :
  ?help:string -> string -> string -> Ast.expr -> Hir.ty -> Diag.t

val logical_not_error : Ast.expr -> Hir.ty -> Diag.t
val shift_value_error : Ast.binop -> Span.t -> Hir.ty -> Diag.t
val shift_count_error : Ast.binop -> Span.t -> Hir.ty -> Diag.t
val shift_count_lanes_error : Ast.binop -> Span.t -> Hir.ty -> Hir.ty -> Diag.t
val shift_count_splat_error : Ast.binop -> Ast.expr -> Ast.expr -> Diag.t
val rotate_value_error : string -> Span.t -> Hir.ty -> Diag.t
val rotate_count_error : string -> Span.t -> Hir.ty -> Diag.t
val signed_division_overflow_message : Hir.ty -> int64 -> int64 -> string

val raw_access_needs_type_error :
  ?index:string -> ?allow_help:bool -> Ast.expr -> Span.t -> Diag.t

val unknown_type_error : string list -> Span.t -> string -> Diag.t
val similar_name_help : string list -> string -> string option

val designated_record_entries :
  string ->
  bool ->
  Hir.field list ->
  Ast.expr list ->
  Span.t ->
  ((Hir.field * Ast.expr option) list option, Diag.t list) result

val invalid_designator_error : Hir.ty -> Ast.expr -> Diag.t
val unknown_type_name : string -> string option

val resolve_aggregate_length :
  ?globals:string list ->
  ?kind:string ->
  const_values ->
  Span.t ->
  string ->
  (string, Diag.t list) result

val aggregate_length_value :
  string -> Span.t -> Hir.ty -> int64 -> (int, Diag.t list) result

val aggregate_length_expected : Ast.expr -> Hir.ty option
val remap_length_cycle : Span.t -> ('a, Diag.t list) result -> ('a, Diag.t list) result

val source_ty_with_values :
  ?globals:string list ->
  named_types ->
  const_values ->
  Span.t ->
  Ast.ty ->
  (Hir.ty, Diag.t list) result

val layout_diag :
  Span.t -> Hir.struct_def list -> Hir.ty -> (int * int, Diag.t list) result

val field_info : Hir.struct_def list -> string -> string -> Hir.field option
val compatible : Hir.ty -> Hir.ty -> bool
val scalar_conversion_pair : Hir.ty -> Hir.ty -> bool
val implicit_integer_widen : Hir.ty -> Hir.ty -> Ast.cast_kind option
val implicit_numeric_widen : Hir.ty -> Hir.ty -> Ast.cast_kind option
val common_integer_type : Hir.ty -> Hir.ty -> Hir.ty option
val common_numeric_type : Hir.ty -> Hir.ty -> Hir.ty option
val narrow_arithmetic_result : unsigned:bool -> Ast.expr -> bool
val narrow_float_result : Ast.expr -> bool

val unify_if_branches :
  ?leaf_type:(Ast.expr -> Hir.ty option) ->
  ?left_checked:Hir.expr ->
  ?right_checked:Hir.expr ->
  Ast.expr ->
  Hir.ty ->
  Ast.expr ->
  Hir.ty ->
  (Hir.ty * Ast.cast_kind option * Ast.cast_kind option, Diag.t) result

val convert_expected_kind :
  ?expression:Ast.expr -> Hir.ty -> Hir.ty -> Ast.cast_kind option

val widen_integer_value : Ast.cast_kind -> Hir.ty -> Hir.ty -> int64 -> int64
val diagnostic_ty_name : Hir.ty -> string
val float_literal_type_error : Hir.ty -> Span.t -> Diag.t
val function_arity_message : string -> int -> int -> string
val generic_arity_message : string -> string -> int -> int -> string
val constant_initializer_type_message : Hir.ty -> Hir.ty -> string
val constant_array_element_type_message : Hir.ty -> Hir.ty -> string

val cast_error :
  ?expression:Ast.expr -> Ast.cast_kind -> Hir.ty -> Hir.ty -> Span.t -> Diag.t

val integer_literal_error :
  Span.t -> Hir.ty -> negative:bool -> text:string -> magnitude:int64 -> Diag.t

val integer_literal_vector_argument_error :
  string -> Ast.expr -> Hir.ty -> Diag.t option

val cast_target_error : Ast.cast_kind -> Hir.ty -> string
val missing_field_message : ?record_name:string -> string -> Hir.ty -> string
val record_field_count_message : string -> int -> int -> string
val array_element_count_message : int -> int -> string
val aggregate_count_error_span : Span.t -> int -> Ast.expr list -> Span.t

val ensure_expected :
  ?context:string ->
  ?expression:Ast.expr ->
  ?checked_expression:Hir.expr ->
  ?leaf_type:(Ast.expr -> Hir.ty option) ->
  ?widening_help:bool ->
  Hir.ty ->
  Hir.ty ->
  Span.t ->
  (unit, Diag.t list) result

val binary_operator_name : Ast.binop -> string

val comparison_chain_diagnostic :
  Span.t -> Ast.binop -> Ast.expr -> Ast.expr -> Diag.t option

val binary_result_type :
  ?left_expression:Ast.expr ->
  ?right_expression:Ast.expr ->
  ?left_checked:Hir.expr ->
  ?right_checked:Hir.expr ->
  ?leaf_type:(Ast.expr -> Hir.ty option) ->
  ?result_expected:Hir.ty ->
  ?comparison_chain_rewrite_valid:bool ->
  Span.t ->
  Ast.binop ->
  Hir.ty ->
  Hir.ty ->
  (Hir.ty, Diag.t list) result

val variadic_promote : Hir.expr -> Hir.expr
