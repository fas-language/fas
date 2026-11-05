type t = { analyzed : int; current : int; prefix : int; suffix : int }

let lines text = Array.of_list (String.split_on_char '\n' text)

let common_prefix a b =
  let limit = min (Array.length a) (Array.length b) in
  let rec go i = if i < limit && String.equal a.(i) b.(i) then go (i + 1) else i in
  go 0

let common_suffix a b prefix =
  let na = Array.length a and nb = Array.length b in
  let limit = min na nb - prefix in
  let rec go k =
    if k < limit && String.equal a.(na - 1 - k) b.(nb - 1 - k) then go (k + 1) else k
  in
  go 0

let create ~analyzed ~current =
  let a = lines analyzed and b = lines current in
  let prefix = common_prefix a b in
  let suffix = common_suffix a b prefix in
  { analyzed = Array.length a; current = Array.length b; prefix; suffix }

let to_current m line =
  if line < 0 || line >= m.analyzed then None
  else if line < m.prefix then Some line
  else if line >= m.analyzed - m.suffix then Some (line + m.current - m.analyzed)
  else None

let to_analyzed m line =
  if line < 0 || line >= m.current then None
  else if line < m.prefix then Some line
  else if line >= m.current - m.suffix then Some (line + m.analyzed - m.current)
  else None
