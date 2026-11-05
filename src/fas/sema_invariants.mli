val check_declarations : (int * string) list -> (unit, string) result

val check_const_environment :
  declared:string list ->
  early:string list ->
  consts:string list ->
  arrays:string list ->
  (unit, string) result

val check_materialization :
  pending:int -> functions:string list -> (unit, string) result
