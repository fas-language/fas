type step = Message of string * string | Incomplete | Malformed of string * string

let index_of text pattern =
  let n = String.length text and m = String.length pattern in
  let matches i =
    let rec loop k = k = m || (text.[i + k] = pattern.[k] && loop (k + 1)) in
    loop 0
  in
  let rec search i =
    if i + m > n then None else if matches i then Some i else search (i + 1)
  in
  search 0

let strip_cr line =
  let n = String.length line in
  if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line

let content_length header =
  let found =
    List.fold_left
      (fun found line ->
        let line = strip_cr line in
        match String.index_opt line ':' with
        | None -> found
        | Some colon ->
            let name = String.lowercase_ascii (String.trim (String.sub line 0 colon)) in
            if name = "content-length" then
              let value =
                String.sub line (colon + 1) (String.length line - colon - 1)
              in
              Some (String.trim value)
            else found)
      None
      (String.split_on_char '\n' header)
  in
  match found with
  | None -> None
  | Some value ->
      let digits =
        value <> "" && String.for_all (fun c -> c >= '0' && c <= '9') value
      in
      if digits && String.length value <= 9 then int_of_string_opt value else None

let header_pattern = "content-length:"
let lowered text i k = String.lowercase_ascii (String.sub text i k)

let find_header text =
  let n = String.length text and m = String.length header_pattern in
  let rec find i =
    if i + m > n then None
    else if lowered text i m = header_pattern then Some i
    else find (i + 1)
  in
  find 0

let resynchronise text =
  let n = String.length text in
  match find_header text with
  | Some i -> (true, String.sub text i (n - i))
  | None ->
      let rec partial k =
        if k = 0 then ""
        else if lowered text (n - k) k = String.sub header_pattern 0 k then
          String.sub text (n - k) k
        else partial (k - 1)
      in
      (false, partial (min (String.length header_pattern - 1) n))

let take_message text =
  match index_of text "\r\n\r\n" with
  | None -> Incomplete
  | Some index -> (
      let header = String.sub text 0 index in
      let start = index + 4 in
      let available = String.length text - start in
      match content_length header with
      | None ->
          Malformed
            ("missing or invalid Content-Length header", String.sub text start available)
      | Some length ->
          if available < length then Incomplete
          else
            Message
              ( String.sub text start length,
                String.sub text (start + length) (available - length) ))

let write_message channel body =
  output_string channel
    (Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body));
  output_string channel body;
  flush channel
