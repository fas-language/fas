let char_at text i =
  let length = String.length text in
  let byte k = Char.code text.[k] in
  let within lo hi k = k < length && byte k >= lo && byte k <= hi in
  let tail k = within 0x80 0xBF k in
  let lead = byte i in
  if lead < 0x80 then (1, 1)
  else if lead >= 0xC2 && lead <= 0xDF && tail (i + 1) then (2, 1)
  else if
    lead >= 0xE0 && lead <= 0xEF
    && within
         (if lead = 0xE0 then 0xA0 else 0x80)
         (if lead = 0xED then 0x9F else 0xBF)
         (i + 1)
    && tail (i + 2)
  then (3, 1)
  else if
    lead >= 0xF0 && lead <= 0xF4
    && within
         (if lead = 0xF0 then 0x90 else 0x80)
         (if lead = 0xF4 then 0x8F else 0xBF)
         (i + 1)
    && tail (i + 2)
    && tail (i + 3)
  then (4, 2)
  else (1, 1)

type t = { text : string; starts : int array }

let index text =
  let starts = ref [ 0 ] in
  String.iteri (fun i c -> if c = '\n' then starts := (i + 1) :: !starts) text;
  { text; starts = Array.of_list (List.rev !starts) }

let line_of starts offset =
  let rec search low high =
    if low >= high then low
    else
      let mid = (low + high + 1) / 2 in
      if starts.(mid) <= offset then search mid high else search low (mid - 1)
  in
  search 0 (Array.length starts - 1)

let position idx offset =
  let offset = max 0 (min offset (String.length idx.text)) in
  let line = line_of idx.starts offset in
  let rec units i acc =
    if i >= offset then acc
    else
      let width, size = char_at idx.text i in
      units (i + width) (acc + size)
  in
  (line, units idx.starts.(line) 0)

let offset idx (line, character) =
  let text = idx.text in
  let length = String.length text in
  if line < 0 || line >= Array.length idx.starts then length
  else
    let at_break i =
      i >= length
      || text.[i] = '\n'
      || (text.[i] = '\r' && i + 1 < length && text.[i + 1] = '\n')
    in
    let rec walk i units =
      if at_break i then i
      else
        let width, size = char_at text i in
        if units + size > character then i else walk (i + width) (units + size)
    in
    walk idx.starts.(line) 0

let position_of_offset text offset = position (index text) offset
let offset_of_position text point = offset (index text) point
