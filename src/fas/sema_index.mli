type kind =
  | Function
  | Parameter
  | Local
  | View
  | Global
  | Const
  | Const_array
  | Struct
  | Union
  | Opaque
  | Field
  | Type_parameter
  | Const_parameter
  | Type_alias

type declaration = {
  name : string;
  kind : kind;
  name_span : Span.t;
  item_span : Span.t;
  container : string option;
  c_origin : (string * int) option;
}

type reference = { use_span : Span.t; declaration : declaration }

type expression_type = {
  span : Span.t;
  instance : string list;
  ty : string;
  value : string option;
  rank : int;
}

type generic_call = {
  callee_span : Span.t;
  instance : string list;
  arguments : string list;
  explicit : bool;
  order : int;
}

type t

val create : unit -> t
val declarations : t -> declaration list
val references : t -> reference list
val expression_types : t -> expression_type list
val generic_calls : t -> generic_call list
val kind_name : kind -> string
val set_c_origins : t -> (string * (string * int)) list -> unit
val set_c_enum_owners : t -> (string * string option) list -> unit
val is_enumerator : t -> declaration -> bool

val set_c_import :
  t ->
  unsupported:(string * string) list ->
  aliases:(string * string) list ->
  nonnull:(string * int list) list ->
  strings:(string * (int * string option) list) list ->
  alloc:(string * int list list) list ->
  parameter_names:(string * string option list) list ->
  unit

val has_c_import : t -> bool
val c_unsupported : t -> (string * string) list
val c_origin_named : t -> string -> (string * int) option
val declare_program : t -> c_aliases:string list -> Ast.program -> unit
val reference_program : t -> instantiated:(string -> bool) -> Ast.program -> unit
val enter_body : t -> template:string option -> instance:string list -> unit
val bind_local : t -> int -> Hir.ty -> Span.t -> unit
val refer_local : t -> Span.t -> int -> unit
val refer_name : t -> Span.t -> string -> unit
val set_struct_source : t -> (string -> string) -> unit
val refer_field : t -> Span.t -> string -> string -> unit
val record_type : t -> Span.t -> string -> string option -> unit
val render_value : Hir.ty -> int64 -> string
val record_const : t -> Span.t -> Hir.ty -> int64 -> unit
val leave_body : t -> unit
val bind_signature : t -> name:string -> (Span.t * Hir.ty) list -> unit
val record_generic_call : t -> Span.t -> string list -> unit

val record_struct_instance :
  t -> generated:string -> template:string -> args:string list -> unit

val record_program :
  t ->
  structs:Hir.struct_def list ->
  consts:Hir.const_def list ->
  arrays:Hir.const_arr_def list ->
  globals:Hir.global list ->
  unit

val finish : t -> render:(string -> string) -> unit
val describe : t -> ?use:Span.t -> declaration -> (string * string list) option
