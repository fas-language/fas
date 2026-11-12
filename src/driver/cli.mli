type emit = Ir | Llvm | Optimized_llvm | Asm | Obj | Header | Executable
type diagnostics = Text | Json
type target = Portable | Force_native
type native_runtime = Auto | Freestanding | Hosted

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
  target : target;
  target_explicit : bool;
  os : Target_layout.os;
  target_features : string option;
  all_errors : bool;
  native : bool;  (** built-in x86-64 code generator; false only under the LLVM oracle *)
  native_runtime : native_runtime;
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
  | Lsp of lsp_options
  | Help
  | Version

val parse : string array -> (command, string) result
val oracle : unit -> bool
(** [FAS_ORACLE=llvm]: the hidden LLVM test-oracle mode (see cli.ml) *)

val usage : string
val native_linker_arguments : string list -> string list
