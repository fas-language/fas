external get_soft : unit -> int = "fas_build_limit_get"
external set_soft : int -> unit = "fas_build_limit_set"

let cap = 4 * 1024 * 1024 * 1024

let with_cap f =
  let previous = get_soft () in
  let limit = if previous < 0 then cap else min cap previous in
  set_soft limit;
  Fun.protect ~finally:(fun () -> set_soft previous) f
