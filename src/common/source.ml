type t = { file : string; text : string; line_starts : int array }

let create ~file ~text =
  let starts = ref [ 0 ] in
  String.iteri (fun i c -> if c = '\n' then starts := (i + 1) :: !starts) text;
  { file; text; line_starts = Array.of_list (List.rev !starts) }

let file source = source.file
let text source = source.text
let length source = String.length source.text

let span source ~start_offset ~end_offset =
  let rec find_line low high =
    if low + 1 >= high then low
    else
      let mid = (low + high) / 2 in
      if source.line_starts.(mid) <= start_offset then find_line mid high
      else find_line low mid
  in
  let line = find_line 0 (Array.length source.line_starts) in
  let line_start = source.line_starts.(line) in
  Span.make ~file:source.file ~start_offset ~end_offset ~line:(line + 1)
    ~column:(start_offset - line_start + 1)

let line_text source line =
  if line < 1 || line > Array.length source.line_starts then None
  else
    let start = source.line_starts.(line - 1) in
    let stop =
      if line = Array.length source.line_starts then String.length source.text
      else source.line_starts.(line) - 1
    in
    Some (String.sub source.text start (stop - start))

let excerpt source span =
  let start = max 0 (min (String.length source.text) span.Span.start_offset) in
  let stop = max start (min (String.length source.text) span.Span.end_offset) in
  String.sub source.text start (stop - start)
