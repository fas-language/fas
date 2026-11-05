type kind = Function_specialization | Struct_specialization

type arg =
  | Type_specialization_arg of string
  | Const_specialization_arg of Hir.ty * int64

type key = kind * int * arg list

type diagnostic_type =
  | Diagnostic_bool
  | Diagnostic_void
  | Diagnostic_int of Ast.int_kind
  | Diagnostic_float of Ast.float_kind
  | Diagnostic_ptr of diagnostic_type
  | Diagnostic_const_ptr of diagnostic_type
  | Diagnostic_addr
  | Diagnostic_handle of diagnostic_type
  | Diagnostic_array of string * diagnostic_type
  | Diagnostic_vec of string * diagnostic_type
  | Diagnostic_named of string
  | Diagnostic_applied of string * diagnostic_argument list

and diagnostic_argument =
  | Diagnostic_type_argument of diagnostic_type
  | Diagnostic_const_argument of Hir.ty * int64
  | Diagnostic_source_const_argument of string

type instantiation_frame = {
  template_name : string;
  arguments : diagnostic_argument list;
  application_span : Span.t;
}

type pending_diagnostic_argument =
  | Pending_diagnostic_type of diagnostic_type
  | Pending_diagnostic_const of string

type pending_instantiation_frame = {
  pending_arguments : pending_diagnostic_argument list;
  pending_application_span : Span.t;
}

type staged_arg = Staged_type_arg of string | Staged_const_arg of string

type payload =
  | Function_payload of {
      item : Ast.item;
      substitutions : (string * Ast.ty) list;
      values : (string * Hir.ty * int64) list;
      staged_args : staged_arg list option;
    }
  | Struct_payload of {
      template : Ast.item;
      substitutions : (string * Ast.ty) list;
      values : (string * Hir.ty * int64) list;
    }

type specialization = {
  key : key;
  name : string;
  depth : int;
  payload : payload;
  trace : instantiation_frame list;
  pending_frame : pending_instantiation_frame option;
}

type t

val create : unit -> t

val request :
  t ->
  limits:Limits.t ->
  depth:int ->
  span:Span.t ->
  description:string ->
  specialization ->
  (specialization, Diag.t list) result

val find_by_name : t -> kind -> string -> specialization option
val fold_by_name : (kind * string -> specialization -> 'a -> 'a) -> t -> 'a -> 'a
val take_pending : t -> specialization option
val pending_count : t -> int
val requeue_materialized : t -> specialization -> unit
val update_materialized : t -> specialization -> unit
val diagnostic_type_of_ast : t -> Ast.ty -> diagnostic_type
val current_instantiation_frame : instantiation_frame list -> instantiation_frame option
val render_diagnostic_const : Hir.ty -> int64 -> string
val render_diagnostic_argument : diagnostic_argument -> string
val source_facing_text : t -> string -> string

val trace_result :
  t -> instantiation_frame list -> ('a, Diag.t list) result -> ('a, Diag.t list) result

val specialization_trace : t -> kind -> string -> instantiation_frame list
val specialization_source_name : t -> kind -> string -> string
val specialization_application_name : t -> kind -> string -> string option
val mangle_specialization : string -> (string * Hir.ty * int64) list -> string
val function_specialization_key : int -> (string * Hir.ty * int64) list -> key

val staged_specialization_identity :
  t ->
  string ->
  (string * Hir.ty * int64) list ->
  ( (int * string * arg list * diagnostic_argument list * Span.t) option,
    Diag.t list )
  result

val mangle_mixed_specialization : string -> arg list -> string
val specialization_type_key : Ast.ty -> string
val mangle_type_specialization : string -> Ast.ty list -> string
