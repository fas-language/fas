module Hashtbl = Stdlib.Hashtbl.Make (String)

type value =
  | Obj of (string * value) array
  | Arr of value list
  | Str of string
  | Num of string
  | Bool of bool
  | Null

type input = {
  channel : in_channel;
  data : bytes;
  mutable pos : int;
  mutable size : int;
  mutable last_file : string option;
  mutable last_line : string option;
  mutable presumed_file : string option;
  mutable presumed_line : string option;
}

let make_obj fields = Obj (Array.of_list fields)
let obj_fields = function Obj fields -> Array.to_list fields | _ -> []

let refill i =
  if i.pos = i.size then (
    i.size <- input i.channel i.data 0 (Bytes.length i.data);
    i.pos <- 0;
    if i.size = 0 then raise End_of_file)

let peek i =
  refill i;
  Bytes.get i.data i.pos

let take i =
  let c = peek i in
  i.pos <- i.pos + 1;
  c

let rec space i =
  match peek i with
  | ' ' | '\n' | '\r' | '\t' ->
      ignore (take i);
      space i
  | _ -> ()

let expect i c =
  space i;
  if take i <> c then failwith "invalid Clang JSON"

let hex i =
  let digit = function
    | '0' .. '9' as c -> Char.code c - 48
    | 'a' .. 'f' as c -> Char.code c - 87
    | 'A' .. 'F' as c -> Char.code c - 55
    | _ -> failwith "invalid Clang JSON escape"
  in
  let n = ref 0 in
  for _ = 1 to 4 do
    n := (!n lsl 4) lor digit (take i)
  done;
  !n

let read_string_slow i =
  let b = Buffer.create 32 in
  let rec loop () =
    match take i with
    | '"' -> Buffer.contents b
    | '\\' ->
        (match take i with
        | '"' -> Buffer.add_char b '"'
        | '\\' -> Buffer.add_char b '\\'
        | '/' -> Buffer.add_char b '/'
        | 'b' -> Buffer.add_char b '\b'
        | 'f' -> Buffer.add_char b '\012'
        | 'n' -> Buffer.add_char b '\n'
        | 'r' -> Buffer.add_char b '\r'
        | 't' -> Buffer.add_char b '\t'
        | 'u' ->
            let first = hex i in
            let code =
              if first >= 0xd800 && first <= 0xdbff then (
                if take i <> '\\' || take i <> 'u' then
                  failwith "invalid Clang JSON surrogate pair";
                let second = hex i in
                if second < 0xdc00 || second > 0xdfff then
                  failwith "invalid Clang JSON surrogate pair";
                0x10000 + (((first - 0xd800) lsl 10) lor (second - 0xdc00)))
              else first
            in
            Buffer.add_utf_8_uchar b (Uchar.of_int code)
        | _ -> failwith "invalid Clang JSON escape");
        loop ()
    | c ->
        Buffer.add_char b c;
        loop ()
  in
  loop ()

let read_string i =
  let start = i.pos in
  let rec scan pos =
    if pos >= i.size then (
      i.pos <- start;
      read_string_slow i)
    else
      match Bytes.get i.data pos with
      | '"' ->
          i.pos <- pos + 1;
          Bytes.sub_string i.data start (pos - start)
      | '\\' ->
          i.pos <- start;
          read_string_slow i
      | _ -> scan (pos + 1)
  in
  scan start

let rec skip_string i =
  let char = take i in
  if char <> '"' then (
    if char = '\\' then ignore (take i);
    skip_string i)

let record_location_value i field value =
  if String.equal field "file" then (
    i.last_file <- Some value;
    i.presumed_file <- None;
    i.presumed_line <- None)
  else if String.equal field "line" then (
    i.last_line <- Some value;
    i.presumed_line <- None)
  else if String.equal field "presumedFile" then i.presumed_file <- Some value
  else if String.equal field "presumedLine" then i.presumed_line <- Some value

let rec skip_value ?(location = false) ?(range = false) ?(field = "") i =
  space i;
  match take i with
  | '"' ->
      if
        location
        && (String.equal field "file" || String.equal field "line"
           || String.equal field "presumedFile"
           || String.equal field "presumedLine")
      then record_location_value i field (read_string i)
      else skip_string i
  | '{' ->
      let rec fields () =
        space i;
        if peek i = '}' then ignore (take i)
        else (
          expect i '"';
          let key = read_string i in
          expect i ':';
          let child_location =
            List.mem key [ "loc"; "expansionLoc"; "spellingLoc" ]
            || (range && List.mem key [ "begin"; "end" ])
            || location
               && List.mem key [ "file"; "line"; "presumedFile"; "presumedLine" ]
          in
          skip_value ~location:child_location ~range:(String.equal key "range")
            ~field:key i;
          space i;
          match take i with
          | '}' -> ()
          | ',' -> fields ()
          | _ -> failwith "invalid Clang JSON object")
      in
      fields ()
  | '[' ->
      let rec values () =
        space i;
        if peek i = ']' then ignore (take i)
        else (
          skip_value i;
          space i;
          match take i with
          | ']' -> ()
          | ',' -> values ()
          | _ -> failwith "invalid Clang JSON array")
      in
      values ()
  | _ ->
      if location && (String.equal field "line" || String.equal field "presumedLine")
      then (
        let b = Buffer.create 12 in
        while
          match peek i with
          | ',' | ']' | '}' | ' ' | '\n' | '\r' | '\t' -> false
          | _ -> true
        do
          Buffer.add_char b (take i)
        done;
        record_location_value i field (Buffer.contents b))
      else
        while
          match peek i with
          | ',' | ']' | '}' | ' ' | '\n' | '\r' | '\t' -> false
          | _ ->
              ignore (take i);
              true
        do
          ()
        done

let supported = function
  | "FunctionDecl" | "VarDecl" | "TypedefDecl" | "EnumDecl" | "RecordDecl" -> true
  | _ -> false

let string = function Str s -> Some s | _ -> None

let field_keys =
  String.split_on_char ' '
    ("kind id decl name type loc value storageClass inline tagUsed completeDefinition "
   ^ "fixedUnderlyingType isBitfield isImplicit inner qualType desugaredQualType file \
      line col typeAliasDeclId qualifiers size cc variadic offset expansionLoc "
   ^ "spellingLoc presumedFile presumedLine range begin end tokLen isMacroArgExpansion \
      args")

let keep_fields =
  Hashtbl.of_seq (List.to_seq (List.map (fun key -> (key, ())) field_keys))

let keep_field key = Hashtbl.mem keep_fields key

let read_key i =
  let start = i.pos in
  let matches key finish =
    let length = finish - start in
    String.length key = length
    &&
    let rec equal offset =
      offset = length
      || (Bytes.get i.data (start + offset) = key.[offset] && equal (offset + 1))
    in
    equal 0
  in
  let rec scan pos =
    if pos >= i.size then (
      i.pos <- start;
      let key = read_string_slow i in
      List.find_opt (String.equal key) field_keys)
    else
      match Bytes.get i.data pos with
      | '"' ->
          i.pos <- pos + 1;
          List.find_opt (fun key -> matches key pos) field_keys
      | '\\' ->
          i.pos <- start;
          let key = read_string_slow i in
          List.find_opt (String.equal key) field_keys
      | _ -> scan (pos + 1)
  in
  scan start

let rec json ?(location = false) ?(range = false) i =
  space i;
  match peek i with
  | '"' ->
      ignore (take i);
      Str (read_string i)
  | '{' -> object_value ~location ~range i
  | '[' -> array_value i
  | 't' -> literal i "true" (Bool true)
  | 'f' -> literal i "false" (Bool false)
  | 'n' -> literal i "null" Null
  | _ ->
      let b = Buffer.create 12 in
      while
        match peek i with
        | ',' | ']' | '}' | ' ' | '\n' | '\r' | '\t' -> false
        | _ -> true
      do
        Buffer.add_char b (take i)
      done;
      Num (Buffer.contents b)

and literal i text value =
  String.iter (fun c -> if take i <> c then failwith "invalid Clang JSON") text;
  value

and array_value i =
  expect i '[';
  space i;
  if peek i = ']' then (
    ignore (take i);
    Arr [])
  else
    let rec loop acc =
      let v = json i in
      match
        space i;
        take i
      with
      | ']' -> Arr (List.rev (v :: acc))
      | ',' -> loop (v :: acc)
      | _ -> failwith "invalid Clang JSON array"
    in
    loop []

and object_value ?(location = false) ?(range = false) i =
  let inherited_file = i.last_file and inherited_line = i.last_line in
  let inherited_presumed_file = i.presumed_file
  and inherited_presumed_line = i.presumed_line in
  let fields = ref [] in
  let contains key = List.exists (fun (field, _) -> String.equal field key) !fields in
  let add key value = if not (contains key) then fields := (key, value) :: !fields in
  let object_value () = Obj (Array.of_list (List.rev !fields)) in
  let location_fields () =
    if location then (
      let has_file = contains "file" in
      let has_line = contains "line" in
      Option.iter (fun value -> add "file" (Str value)) inherited_file;
      Option.iter (fun value -> add "line" (Num value)) inherited_line;
      Option.iter
        (fun value -> add "presumedFile" (Str value))
        (if has_file then None else inherited_presumed_file);
      Option.iter
        (fun value -> add "presumedLine" (Str value))
        (if has_file || has_line then None else inherited_presumed_line))
  in
  expect i '{';
  space i;
  if peek i = '}' then (
    ignore (take i);
    location_fields ();
    object_value ())
  else
    let rec loop kind =
      expect i '"';
      let key = Option.value ~default:"" (read_key i) in
      expect i ':';
      let keep =
        keep_field key
        && ((not (String.equal key "range")) || String.equal kind "NonNullAttr")
      in
      let skip_inner =
        String.equal key "inner"
        && (String.equal kind "VarDecl" || String.ends_with ~suffix:"Stmt" kind)
      in
      let child_location =
        List.mem key [ "loc"; "expansionLoc"; "spellingLoc" ]
        || (range && List.mem key [ "begin"; "end" ])
      in
      let value =
        if (not keep) || skip_inner then (
          skip_value ~location:child_location ~range:(String.equal key "range")
            ~field:key i;
          None)
        else Some (json ~location:child_location ~range:(String.equal key "range") i)
      in
      (match (location, key, value) with
      | true, ("file" | "line" | "presumedFile" | "presumedLine"), Some (Str value) ->
          record_location_value i key value
      | true, ("line" | "presumedLine"), Some (Num value) ->
          record_location_value i key value
      | _ -> ());
      let kind =
        if String.equal key "kind" then
          Option.value ~default:kind (Option.bind value string)
        else kind
      in
      Option.iter (fun value -> add key value) value;
      match
        space i;
        take i
      with
      | '}' ->
          location_fields ();
          object_value ()
      | ',' -> loop kind
      | _ -> failwith "invalid Clang JSON object"
    in
    loop ""

let declaration i =
  expect i '{';
  expect i '"';
  let first_field = Option.value ~default:"" (read_key i) in
  expect i ':';
  let first_value =
    if String.equal first_field "id" then Some (json i)
    else (
      skip_value i;
      None)
  in
  expect i ',';
  expect i '"';
  if not (Option.fold ~none:false ~some:(String.equal "kind") (read_key i)) then
    failwith "Clang declaration kind order changed";
  expect i ':';
  expect i '"';
  let kind = read_string i in
  let keep = supported kind in
  let acc =
    if keep then
      Option.fold ~none:[] ~some:(fun value -> [ (first_field, value) ]) first_value
    else []
  in
  let rec rest acc =
    match
      space i;
      take i
    with
    | '}' ->
        if keep then Some (make_obj (List.rev (("kind", Str kind) :: acc))) else None
    | ',' ->
        expect i '"';
        let key = Option.value ~default:"" (read_key i) in
        expect i ':';
        if keep && keep_field key && not (String.equal key "range") then
          rest
            (( key,
               json ~location:(List.mem key [ "loc"; "expansionLoc"; "spellingLoc" ]) i
             )
            :: acc)
        else (
          skip_value
            ~location:(List.mem key [ "loc"; "expansionLoc"; "spellingLoc" ])
            ~range:(String.equal key "range") ~field:key i;
          rest acc)
    | _ -> failwith "invalid Clang declaration"
  in
  rest acc

let declarations ?(root_consumed = false) ?(filtered = false) channel =
  let i =
    {
      channel;
      data = Bytes.create 65536;
      pos = 0;
      size = 0;
      last_file = None;
      last_line = None;
      presumed_file = None;
      presumed_line = None;
    }
  in
  let rec root acc =
    space i;
    if peek i = '}' then (
      ignore (take i);
      List.rev acc)
    else (
      expect i '"';
      let key = Option.value ~default:"" (read_key i) in
      expect i ':';
      let acc =
        if not (String.equal key "inner") then (
          skip_value i;
          acc)
        else (
          expect i '[';
          space i;
          if peek i = ']' then (
            ignore (take i);
            acc)
          else
            let rec items acc =
              let item = declaration i in
              let acc = Option.fold ~none:acc ~some:(fun v -> v :: acc) item in
              match
                space i;
                take i
              with
              | ']' -> acc
              | ',' -> items acc
              | _ -> failwith "invalid Clang declaration array"
            in
            items acc)
      in
      match
        space i;
        take i
      with
      | '}' -> List.rev acc
      | ',' -> root acc
      | _ -> failwith "invalid Clang translation unit")
  in
  let rec loop acc =
    try
      space i;
      loop (Option.fold ~none:acc ~some:(fun value -> value :: acc) (declaration i))
    with End_of_file -> List.rev acc
  in
  if filtered then loop []
  else (
    if not root_consumed then expect i '{';
    root [])

let declarations_with_layout channel =
  let layout = Buffer.create 4096 in
  let line = ref (input_line channel) in
  while not (String.equal !line "{") do
    Buffer.add_string layout !line;
    Buffer.add_char layout '\n';
    line := input_line channel
  done;
  (Buffer.contents layout, declarations ~root_consumed:true channel)

let field name = function
  | Obj fields ->
      Option.map snd (Array.find_opt (fun (key, _) -> String.equal key name) fields)
  | _ -> None

let array = function Arr values -> values | _ -> []
