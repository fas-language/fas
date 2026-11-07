(* Discover the host C runtime through a compiler dry-run and perform the link
   with Fas. The generated native object has no Fas syscall startup. *)
val link :
  cc:string ->
  config:Cli.t ->
  c_objects:string list ->
  link_flags:string list ->
  Fir.modul ->
  (unit, string) result
