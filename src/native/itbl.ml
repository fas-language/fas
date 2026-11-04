(* Hash tables keyed by ints (value ids, block ids, registers). The native
   backend's tables are almost all int-keyed; the functor avoids the
   polymorphic hash and compare of Stdlib.Hashtbl on every access. *)
include Hashtbl.Make (struct
  type t = int

  let equal = Int.equal
  let hash x = x land max_int
end)
