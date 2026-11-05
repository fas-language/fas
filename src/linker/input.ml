type t =
  | File of { path : string; whole : bool }
  | Object of { path : string; data : string }
  | Library of { name : string; whole : bool; static : bool }
  | Static of t
  | Group of t list
  | As_needed of t

let file path = File { path; whole = false }

let rec paths = function
  | File { path; _ } -> [ path ]
  | Object _ -> []
  | Library _ -> []
  | Static input -> paths input
  | Group inputs -> List.concat_map paths inputs
  | As_needed input -> paths input
