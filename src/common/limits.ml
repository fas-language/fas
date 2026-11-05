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

let budget_version_name = function V0_15 -> "0.1.5"
let default_budget_version = V0_15
let budget_profile_name limits = budget_version_name limits.budget_version

let for_budget_version version =
  match version with
  | V0_15 ->
      {
        budget_version = version;
        max_tokens = 1_000_000;
        max_nesting = 128;
        max_interned_string_bytes = 4_000_000;
        max_rendered_ir_bytes = 32_000_000;
        max_specializations = 10_000;
        max_specialization_depth = 64;
        max_const_loop_iterations = 4_096;
        max_aggregate_elements = 1_000_000;
        max_object_alignment = 1_048_576;
        max_object_size = 1_073_741_824;
        max_ast_nodes = 4_000_000;
        max_ir_nodes = 4_000_000;
        max_static_data_bytes = 1_073_741_824;
        max_type_nodes = 4_000_000;
        max_stack_scratch_bytes = 1_073_741_824;
        max_use_files = 10_000;
        max_use_bytes = 1_073_741_824;
      }

let default = for_budget_version default_budget_version
