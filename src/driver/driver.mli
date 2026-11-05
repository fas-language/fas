val use_path_error : string -> string option
val canonical_path : string -> string
val use_target : string -> string -> string -> string * string

val load_program :
  ?overlay:(string -> string option) ->
  limits:Limits.t ->
  string ->
  ( Ast.program
    * (string * string list) list
    * (string * C_import.header list * Ast.pkg list) list
    * (string * C_import.header list * Ast.pkg list) list,
    Diag.t list )
  result

type checked = {
  program : Ast.program;
  ir : Ir.module_;
  redirect : string -> string;
  adapters : C_import.adapter list;
  c_objects : string list;
  declaration_headers : string list;
  declarations : string;
  artifacts : string list;
  link_pkgs : Ast.pkg list;
  keep : bool;
}

val analyze :
  ?overlay:(string -> string option) ->
  ?cache:C_import.cache ->
  ?index:Sema_index.t ->
  Cli.t ->
  (checked, Diag.t list) result

val release : checked -> unit

type gap = { file : string; start_offset : int; end_offset : int }
type recovered = { recovered_index : Sema_index.t; gaps : gap list }

val recover :
  ?overlay:(string -> string option) ->
  ?cache:C_import.cache ->
  Cli.t ->
  (recovered, Diag.t list) result

val check :
  ?overlay:(string -> string option) ->
  ?cache:C_import.cache ->
  ?index:Sema_index.t ->
  Cli.t ->
  (checked, Diag.t list) result

type link_cache

val new_link_cache : unit -> link_cache

val run :
  ?remarks:string ->
  ?llc_remarks:string ->
  ?llvm_text:(string -> unit) ->
  ?c_debug:bool ->
  ?link_cache:link_cache ->
  ?instrument:bool ->
  Cli.t ->
  (string, Diag.t list) result

val check_clang : unit -> (unit, Diag.t list) result

type choice = Default | Pgo | Native | Native_pgo | Mtune | Mtune_pgo

val choice_name : choice -> string
val align_loops_name : int option -> string
val tune_option_kinds : (string * string list) list
val tune_option_names : string list
val option_kind : string -> string option
val is_unroll_name : string -> bool
val llc_host_cpu : unit -> string
val tune_header : unit -> string list

val merge_profraw :
  scratch:string -> profraw:string list -> (string, Diag.t list) result

val count_profile_functions : string -> int
val write_profile_file : string -> string -> (unit, Diag.t list) result
val pkg_cflags_message : string -> (string list, string) result
val tool : string -> string -> string
val executable_path : string -> string
val version : string
val version_lines : unit -> string list
