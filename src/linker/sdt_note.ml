(* Nonallocatable SystemTap v3 probe metadata. Only the three address fields
   accept relocations; provider/name/argument strings remain bounded data. *)
let pointers ~path ~offset ~size data =
  let fail fmt =
    Printf.ksprintf (fun text -> Error.fail "%s: .note.stapsdt: %s" path text) fmt
  in
  Binary.span ~context:(path ^ ": SDT notes") ~length:(String.length data) offset size;
  let ending = offset + size and cursor = ref offset and count = ref 0 in
  let pointers = Hashtbl.create 16 in
  while !cursor < ending do
    incr count;
    if !count > 1_000_000 then fail "probe count exceeds 1000000";
    if ending - !cursor < 20 then fail "truncated probe header";
    if
      Binary.u32 data !cursor <> 8L
      || Binary.u32 data (!cursor + 8) <> 3L
      || String.sub data (!cursor + 12) 8 <> "stapsdt\000"
    then fail "invalid probe owner or type";
    let length = Binary.u32 data (!cursor + 4) in
    if length < 27L || length > Int64.of_int (ending - !cursor - 20) then
      fail "invalid probe descriptor length";
    let length = Int64.to_int length in
    let descriptor = !cursor + 20 in
    for n = 0 to 2 do
      Hashtbl.add pointers (descriptor - offset + (n * 8)) ()
    done;
    let text = ref (descriptor + 24) in
    for n = 0 to 2 do
      let value =
        Binary.string_at ~context:(path ^ ": SDT string") data ~offset:!text
          ~size:(descriptor + length - !text)
          0
      in
      if n < 2 && value = "" then fail "empty probe provider or name";
      text := !text + String.length value + 1
    done;
    if !text <> descriptor + length then fail "trailing probe descriptor bytes";
    let padded =
      Binary.align ~context:(path ^ ": SDT descriptor") ~limit:(ending - descriptor)
        length 4
    in
    cursor := descriptor + padded
  done;
  pointers
