(* The supported x86 properties have a four-byte bitmask payload. Keep note
   parsing independent of Elf_reader so malformed metadata is rejected on input. *)
module B = Binary

let operation = function
  | 0xc0000002 -> `And
  | 0xc0008001 | 0xc0008002 -> `Or
  | 0xc0010001 | 0xc0010002 -> `Or_and
  | kind -> Error.fail "unsupported GNU property type 0x%x" kind

let parse ~path ~offset ~size data =
  let fail fmt =
    Printf.ksprintf (fun text -> Error.fail "%s: .note.gnu.property: %s" path text) fmt
  in
  B.span ~context:(path ^ ": GNU properties") ~length:(String.length data) offset size;
  if size < 16 || size mod 8 <> 0 then fail "invalid note size";
  let note = ref offset and ending = offset + size in
  let properties = Hashtbl.create 5 in
  while !note < ending do
    if ending - !note < 16 then fail "truncated note header";
    if
      B.u32 data !note <> 4L
      || B.u32 data (!note + 8) <> 5L
      || String.sub data (!note + 12) 4 <> "GNU\000"
    then fail "expected NT_GNU_PROPERTY_TYPE_0 note";
    let descriptor = B.u32 data (!note + 4) in
    if descriptor > Int64.of_int (ending - !note - 16) || Int64.rem descriptor 16L <> 0L
    then fail "invalid descriptor size";
    let stop = !note + 16 + Int64.to_int descriptor in
    let cursor = ref (!note + 16) and previous = ref (-1) in
    while !cursor < stop do
      let kind = Int64.to_int (B.u32 data !cursor) in
      if kind <= !previous then fail "properties must be unique and ordered by type";
      previous := kind;
      ignore (operation kind);
      if B.u32 data (!cursor + 4) <> 4L then fail "invalid bitmask property size";
      if B.u32 data (!cursor + 12) <> 0L then fail "nonzero property padding";
      let value = B.u32 data (!cursor + 8) in
      let value =
        match Hashtbl.find_opt properties kind with
        | None -> value
        | Some old -> Int64.logor old value
      in
      Hashtbl.replace properties kind value;
      cursor := !cursor + 16
    done;
    note := stop
  done;
  Hashtbl.fold (fun kind value values -> (kind, value) :: values) properties []
  |> List.sort (fun (a, _) (b, _) -> compare a b)

let encode properties =
  let out = Bytes.make (16 + (List.length properties * 16)) '\000' in
  B.set_int out 0 4 4;
  B.set_int out 4 4 (Bytes.length out - 16);
  B.set_int out 8 4 5;
  Bytes.blit_string "GNU\000" 0 out 12 4;
  List.iteri
    (fun index (kind, value) ->
      let at = 16 + (index * 16) in
      B.set_int out at 4 kind;
      B.set_int out (at + 4) 4 4;
      B.set out (at + 8) 4 value)
    properties;
  Bytes.unsafe_to_string out
