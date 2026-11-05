val link : ?stats:Stats.t -> ?options:Options.t -> ?entry:string -> string list -> bytes
(** Link the documented ELF64 object subset. All state belongs to the invocation. Raises
    [Error.Error] for unsupported or invalid inputs and filesystem errors. *)

val link_files :
  ?stats:Stats.t ->
  ?options:Options.t ->
  ?entry:string ->
  ?map_file:string ->
  output:string ->
  string list ->
  unit
(** Publish an executable by renaming a completed temporary file in the output
    directory. Existing output survives any failure before the rename. *)

val link_inputs :
  ?stats:Stats.t -> ?options:Options.t -> ?entry:string -> Input.t list -> bytes

val link_inputs_with_map :
  ?stats:Stats.t ->
  ?options:Options.t ->
  ?entry:string ->
  Input.t list ->
  bytes * string

val link_input_files :
  ?stats:Stats.t ->
  ?options:Options.t ->
  ?entry:string ->
  ?map_file:string ->
  output:string ->
  Input.t list ->
  unit
