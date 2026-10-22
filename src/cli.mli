type emit = Ir | Ir_json | Llvm | Optimized_llvm | Asm | Obj | Header | Executable
type diagnostics = Text | Json
type target = Portable | Force_native

type t = {
  input : string;
  output : string;
  output_explicit : bool;
  emit : emit;
  keep : bool;
  optimization : int;
  debug : bool;
  no_inline_functions : string list;
  sanitizers : string list;
  link_inputs : string list;
  c_flags : string list;
  diagnostics : diagnostics;
  target_cpu : string option;
  tune_cpu : string option;
  target : target;
  target_explicit : bool;
  os : Target_layout.os;
  target_features : string option;
  profile : string option;
  all_errors : bool;
  align_loops : int option;
  align_functions : int option;
  tune_options : string list;
}

type perf_mode = Measure | Tune of string option

type perf_options = {
  top : int option;
  static : bool;
  json : bool;
  asm : (string * int) option;
  copy : int option;
  compare : string option;
  mode : perf_mode;
  no_arena : bool;
  yes : bool;
  check_with : string list option;
}

type lsp_options = {
  lsp_c_flags : string list;
  lsp_target : target;
  lsp_os : Target_layout.os;
}

type command =
  | Run of t
  | Run_program of t * string list
  | Check of t
  | Perf of t * perf_options * string list
  | Lsp of lsp_options
  | Help
  | Version

val parse : string array -> (command, string) result
val usage : string
