type binding = Hir.local = { name : string; ty : Hir.ty; id : int }
type selector = Field of string | Element of int
type init_state = Uninit | Full | Unknown | Partial of (selector * init_state) list
type value_fact = { low : int64; high : int64; induction : int option }

type address_fact =
  | Null_address of int64
  | Function_address of string
  | Object_address of {
      identity : string;
      name : string;
      owner_name : string option;
      writable : bool;
      nullable : bool;
      owner : int option;
      extent : int64;
      offset : int64;
    }
  | Dead_local_address of string

type place_path = Exact of selector list | Dynamic_prefix of selector list
type view_access = Mutable_access | Constant_access | Readonly_access
type snapshot
type loop
type defer_capture
type t

val create : ?recover:bool -> initial_scope:bool -> Hir.struct_def list -> t
val lookup_local : string -> t -> binding option
val local_names : t -> string list
val ensure_new_local : string -> t -> Span.t -> (unit, Diag.t list) result
val add_local : string -> Hir.ty -> t -> Span.t -> (binding, Diag.t list) result
val view_origin : t -> binding -> (binding * place_path) option
val view_access : t -> binding -> view_access
val view_readonly_name : t -> binding -> string option
val is_view : t -> binding -> bool
val raw_view_type : t -> binding -> Hir.ty option
val bind_raw_view : t -> binding -> Hir.ty -> view_access -> string option -> unit

val bind_view :
  t ->
  binding ->
  binding option ->
  place_path option ->
  view_access ->
  string option ->
  unit

val push : t -> unit
val pop : t -> unit
val set_state : t -> binding -> selector list -> init_state -> unit
val value_of : t -> binding -> value_fact option
val set_value : t -> binding -> value_fact option -> unit
val forget_value : t -> binding -> unit
val forget_all_values : t -> unit
val address_of : t -> binding -> address_fact option
val set_address : t -> binding -> address_fact option -> unit
val set_call_object : t -> Span.t -> string -> int64 -> unit
val call_object : t -> Span.t -> (Span.t * string * int64) option
val forget_address : t -> binding -> unit
val forget_all_addresses : t -> unit
val forget_addresses_on_write : t -> unit
val mask_of : t -> binding -> bool list option
val set_mask : t -> binding -> bool list option -> unit
val forget_all_masks : t -> unit
val forget_mask : t -> binding -> unit
val value_reachable : t -> bool
val proof_checks_enabled : t -> bool
val note_binding_write : t -> int -> unit
val copy_state : t -> binding -> selector list -> binding -> selector list -> unit

val require_state :
  binding -> selector list -> t -> Span.t -> (unit, Diag.t list) result

val require_place_state :
  binding -> place_path -> t -> Span.t -> (unit, Diag.t list) result

val validate_exit_defers : t -> int -> (unit, Diag.t list) result
val mark_init : binding -> t -> unit
val with_dead_check : t -> bool -> (unit -> 'a) -> 'a
val checking_dead : t -> bool
val set_loop_labels : t -> (string * Span.t) list -> unit
val snapshot : t -> snapshot
val restore : t -> snapshot -> unit
val merge : t -> snapshot -> snapshot -> snapshot
val merge_values_into : t -> snapshot -> snapshot list -> snapshot
val falls_through : t -> bool
val set_falls_through : t -> bool -> unit
val finish_block_scope : t -> (unit, Diag.t list) result
val finish_statement : t -> before:snapshot -> terminates:bool -> unit
val validate_return : t -> Span.t -> (unit, Diag.t list) result
val check_loop_label : t -> string * Span.t -> (unit, Diag.t list) result

val begin_loop :
  ?label:string -> ?induction_binding:int -> ?induction_grid:int64 * int64 -> t -> loop

val end_loop : t -> unit

val finish_while :
  t -> loop -> condition_is_true:bool -> condition_is_false:bool -> unit

val prepare_for_step : t -> loop -> body_falls_through:bool -> unit

val finish_for :
  t ->
  loop ->
  unconditional:bool ->
  condition_is_false:bool ->
  single_iteration:bool ->
  unit

val record_break : t -> (string * Span.t) option -> Span.t -> (unit, Diag.t list) result

val record_continue :
  t -> (string * Span.t) option -> Span.t -> (unit, Diag.t list) result

val invalidate_induction_on_return : t -> unit
val induction_valid : t -> int -> bool
val induction_grid : t -> int -> (int64 * int64) option
val begin_defer : t -> Span.t -> (defer_capture, Diag.t list) result

val finish_defer :
  t ->
  defer_capture ->
  ('a, Diag.t list) result ->
  falls_through:('a -> bool) ->
  ('a, Diag.t list) result

type checkpoint

val recoverable : Diag.t -> bool
val recovering : t -> bool
val record : t -> Diag.t list -> unit
val recovered : t -> Diag.t list
val recovered_count : t -> int
val map_recovered_since : t -> int -> (Diag.t -> Diag.t) -> unit
val checkpoint : t -> checkpoint
val rewind : t -> checkpoint -> unit
val forget_facts : t -> unit
