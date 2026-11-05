type t = {
  file : string;
  start_offset : int;
  end_offset : int;
  line : int;
  column : int;
}

let make ~file ~start_offset ~end_offset ~line ~column =
  { file; start_offset; end_offset; line; column }

let synthetic =
  { file = "<generated>"; start_offset = 0; end_offset = 0; line = 1; column = 1 }

let is_synthetic span = span = synthetic

let compare a b =
  match String.compare a.file b.file with
  | 0 -> Int.compare a.start_offset b.start_offset
  | n -> n

let to_string span = Printf.sprintf "%s:%d:%d" span.file span.line span.column
