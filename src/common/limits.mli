type budget_version = V0_15

type t = {
  budget_version : budget_version;
  max_tokens : int;
  max_nesting : int;
  max_interned_string_bytes : int;
  max_rendered_ir_bytes : int;
  max_specializations : int;
  max_specialization_depth : int;
  max_const_loop_iterations : int;
  max_aggregate_elements : int;
  max_object_alignment : int;
  max_object_size : int;
  max_ast_nodes : int;
  max_ir_nodes : int;
  max_static_data_bytes : int;
  max_type_nodes : int;
  max_stack_scratch_bytes : int;
  max_use_files : int;
  max_use_bytes : int;
}

val budget_version_name : budget_version -> string
val default_budget_version : budget_version
val budget_profile_name : t -> string
val for_budget_version : budget_version -> t
val default : t
