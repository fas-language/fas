type t =
  | Null
  | Bool of bool
  | Num of string
  | Str of string
  | Arr of t list
  | Obj of (string * t) list

exception Malformed of string

let parse text =
  let n = String.length text in
  let pos = ref 0 in
  let fail what = raise (Malformed (Printf.sprintf "%s at byte %d" what !pos)) in
  let peek () = if !pos < n then text.[!pos] else '\000' in
  let advance () = incr pos in
  let rec skip () =
    match peek () with
    | (' ' | '\t' | '\n' | '\r') when !pos < n ->
        advance ();
        skip ()
    | _ -> ()
  in
  let expect c =
    skip ();
    if peek () = c && !pos < n then advance ()
    else fail (Printf.sprintf "expected '%c'" c)
  in
  let literal word value =
    let m = String.length word in
    if !pos + m <= n && String.sub text !pos m = word then (
      pos := !pos + m;
      value)
    else fail "invalid literal"
  in
  let hex4 () =
    if !pos + 4 > n then fail "bad unicode escape";
    let v = ref 0 in
    for i = !pos to !pos + 3 do
      let d =
        match text.[i] with
        | '0' .. '9' as c -> Char.code c - 48
        | 'a' .. 'f' as c -> Char.code c - 87
        | 'A' .. 'F' as c -> Char.code c - 55
        | _ -> fail "bad unicode escape"
      in
      v := (!v * 16) + d
    done;
    pos := !pos + 4;
    !v
  in
  let escape buffer =
    if !pos >= n then fail "unterminated escape";
    let e = text.[!pos] in
    advance ();
    match e with
    | '"' | '\\' | '/' -> Buffer.add_char buffer e
    | 'b' -> Buffer.add_char buffer '\b'
    | 'f' -> Buffer.add_char buffer '\012'
    | 'n' -> Buffer.add_char buffer '\n'
    | 'r' -> Buffer.add_char buffer '\r'
    | 't' -> Buffer.add_char buffer '\t'
    | 'u' ->
        let code = hex4 () in
        let unpaired () = fail "unpaired surrogate" in
        let code =
          if code >= 0xDC00 && code <= 0xDFFF then unpaired ()
          else if code < 0xD800 || code > 0xDBFF then code
          else if !pos + 2 <= n && text.[!pos] = '\\' && text.[!pos + 1] = 'u' then (
            pos := !pos + 2;
            let low = hex4 () in
            if low < 0xDC00 || low > 0xDFFF then unpaired ()
            else 0x10000 + ((code - 0xD800) lsl 10) + (low - 0xDC00))
          else unpaired ()
        in
        Buffer.add_utf_8_uchar buffer (Uchar.of_int code)
    | _ -> fail "bad escape"
  in
  let string () =
    expect '"';
    let buffer = Buffer.create 16 in
    let rec go () =
      if !pos >= n then fail "unterminated string";
      let c = text.[!pos] in
      advance ();
      match c with
      | '"' -> Buffer.contents buffer
      | '\\' ->
          escape buffer;
          go ()
      | c when Char.code c < 0x20 -> fail "control character in string"
      | c ->
          Buffer.add_char buffer c;
          go ()
    in
    go ()
  in
  let number () =
    let start = !pos in
    let digits () =
      let first = !pos in
      while !pos < n && text.[!pos] >= '0' && text.[!pos] <= '9' do
        advance ()
      done;
      if !pos = first then fail "expected digit"
    in
    if peek () = '-' then advance ();
    digits ();
    if peek () = '.' then (
      advance ();
      digits ());
    if peek () = 'e' || peek () = 'E' then (
      advance ();
      if peek () = '+' || peek () = '-' then advance ();
      digits ());
    Num (String.sub text start (!pos - start))
  in
  let collection close item =
    skip ();
    if peek () = close && !pos < n then (
      advance ();
      [])
    else
      let rec more acc =
        let x = item () in
        skip ();
        match peek () with
        | ',' when !pos < n ->
            advance ();
            more (x :: acc)
        | c when c = close && !pos < n ->
            advance ();
            List.rev (x :: acc)
        | _ -> fail "expected ',' or closing bracket"
      in
      more []
  in
  let rec value () =
    skip ();
    match peek () with
    | '{' when !pos < n ->
        advance ();
        Obj (collection '}' member)
    | '[' when !pos < n ->
        advance ();
        Arr (collection ']' value)
    | '"' -> Str (string ())
    | 't' -> literal "true" (Bool true)
    | 'f' -> literal "false" (Bool false)
    | 'n' -> literal "null" Null
    | '-' | '0' .. '9' -> number ()
    | _ -> fail "unexpected character"
  and member () =
    skip ();
    let key = string () in
    expect ':';
    (key, value ())
  in
  match value () with
  | v ->
      skip ();
      if !pos = n then Ok v else Error (Printf.sprintf "trailing text at byte %d" !pos)
  | exception Malformed message -> Error message

let field key = function
  | Obj fields -> Option.value ~default:Null (List.assoc_opt key fields)
  | _ -> Null

let quote text =
  let buffer = Buffer.create (String.length text + 2) in
  let length = String.length text in
  let rec add index =
    if index < length then
      let decoded = String.get_utf_8_uchar text index in
      let width = Uchar.utf_decode_length decoded in
      if Uchar.utf_decode_is_valid decoded && width > 1 then (
        Buffer.add_substring buffer text index width;
        add (index + width))
      else (
        (match text.[index] with
        | '"' -> Buffer.add_string buffer "\\\""
        | '\\' -> Buffer.add_string buffer "\\\\"
        | '\n' -> Buffer.add_string buffer "\\n"
        | '\r' -> Buffer.add_string buffer "\\r"
        | '\t' -> Buffer.add_string buffer "\\t"
        | '\b' -> Buffer.add_string buffer "\\b"
        | '\012' -> Buffer.add_string buffer "\\f"
        | c when Char.code c < 0x20 || Char.code c >= 0x80 ->
            Buffer.add_string buffer (Printf.sprintf "\\u%04x" (Char.code c))
        | c -> Buffer.add_char buffer c);
        add (index + 1))
  in
  Buffer.add_char buffer '"';
  add 0;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let rec to_string = function
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Num s -> s
  | Str s -> quote s
  | Arr items -> "[" ^ String.concat "," (List.map to_string items) ^ "]"
  | Obj fields ->
      "{"
      ^ String.concat ","
          (List.map (fun (key, value) -> quote key ^ ":" ^ to_string value) fields)
      ^ "}"
