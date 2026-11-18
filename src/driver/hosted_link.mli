(* Discover the host C runtime through a compiler dry-run and perform the link
   with Fas. The generated native object has no Fas syscall startup. *)
val link :
  cc:string ->
  config:Cli.t ->
  c_objects:string list ->
  link_flags:string list ->
  Fir.modul ->
  (unit, string) result

(* The one linker command in the output of `FAS_CC -### ...` that links
   [object_path] (the C compiler's link plan, as data). fas-fir --hosted uses
   it too. Raises [Fas_linker.Error.Error] when there is not exactly one. *)
val linker_plan : object_path:string -> string -> Fas_linker.Command_line.config
