(* SysV/GNU archives. Offsets in the symbol index name member headers, not member
   payloads. Parse and validate the container eagerly; parse ELF members lazily. *)
module B = Binary

type member = {
  name : string;
  header : int;
  offset : int;
  size : int;
  external_member : (string * int option) option;
  mutable exports : string list;
  mutable object_ : Elf_reader.t option;
  mutable loaded : bool;
}

type t = {
  path : string;
  data : string;
  members : member array;
  index : (string, int list) Hashtbl.t;
  indexed : bool;
  thin : bool;
  nested : (string, t) Hashtbl.t;
  members_by_header : (int, int) Hashtbl.t;
}

let magic = "!<arch>\n"
let thin_magic = "!<thin>\n"

let is_archive data =
  String.starts_with ~prefix:magic data || String.starts_with ~prefix:thin_magic data

let parse ~path data =
  let fail fmt = Printf.ksprintf (fun s -> Error.fail "%s: archive: %s" path s) fmt in
  let context = path ^ ": archive" and length = String.length data in
  let thin = String.starts_with ~prefix:thin_magic data in
  if not (is_archive data) then fail "invalid magic";
  let span offset size = B.span ~context ~length offset size in
  let decimal ~limit text =
    let text = String.trim text in
    if text = "" then fail "empty decimal field";
    let value = ref 0 in
    String.iter
      (fun c ->
        if c < '0' || c > '9' then fail "invalid decimal field %S" text;
        if !value > (limit - (Char.code c - 48)) / 10 then
          fail "decimal field exceeds supported size";
        value := (!value * 10) + Char.code c - 48)
      text;
    !value
  in
  let trim_end text =
    let ending = ref (String.length text) in
    while !ending > 0 && text.[!ending - 1] = ' ' do
      decr ending
    done;
    String.sub text 0 !ending
  in
  let raw = ref [] and cursor = ref 8 and count = ref 0 in
  let names = ref None and symbols = ref None in
  while !cursor < length do
    span !cursor 60;
    if String.sub data (!cursor + 58) 2 <> "`\n" then
      fail "invalid member header at offset %d" !cursor;
    let name = trim_end (String.sub data !cursor 16) in
    let embedded = (not thin) || List.mem name [ "//"; "/"; "/SYM64/" ] in
    let size =
      decimal
        ~limit:(if embedded then length else Elf_reader.max_file_size)
        (String.sub data (!cursor + 48) 10)
    in
    let offset = !cursor + 60 in
    if embedded then span offset size;
    (match name with
    | "//" ->
        if !names <> None then fail "duplicate long name table";
        names := Some (offset, size)
    | "/" | "/SYM64/" ->
        if !symbols <> None then fail "duplicate symbol index";
        symbols := Some (offset, size, if name = "/" then 4 else 8)
    | _ ->
        incr count;
        if !count > 65536 then fail "member count exceeds 65536 limit";
        raw := (name, !cursor, offset, size) :: !raw);
    cursor := offset + if embedded then size else 0;
    if embedded && size land 1 <> 0 then begin
      span !cursor 1;
      if data.[!cursor] <> '\n' then fail "invalid member padding";
      incr cursor
    end
  done;
  let long_names = Hashtbl.create ~random:true 16 in
  Option.iter
    (fun (offset, size) ->
      let ending = offset + size and pos = ref offset in
      while !pos < ending do
        (* GNU ar may include one final newline in the declared table size. *)
        if !pos = ending - 1 && data.[!pos] = '\n' then incr pos
        else begin
          let start = !pos in
          while
            !pos + 1 < ending && not (data.[!pos] = '/' && data.[!pos + 1] = '\n')
          do
            if !pos - start >= 65535 then fail "member name exceeds 65535 bytes";
            incr pos
          done;
          if !pos + 1 >= ending then fail "unterminated long member name";
          if !pos = start then fail "empty long member name";
          if Hashtbl.length long_names >= 65536 then
            fail "long name table exceeds 65536 entry limit";
          Hashtbl.add long_names (start - offset) (String.sub data start (!pos - start));
          pos := !pos + 2
        end
      done)
    !names;
  let members =
    List.rev !raw |> Array.of_list
    |> Array.map (fun (raw_name, header, offset, size) ->
        let nested = ref None in
        let name =
          if String.starts_with ~prefix:"#1/" raw_name then
            fail "BSD extended names/indexes are not supported yet"
          else if String.starts_with ~prefix:"/" raw_name then
            let reference = String.sub raw_name 1 (String.length raw_name - 1) in
            let reference =
              match String.index_opt reference ':' with
              | None -> reference
              | Some at ->
                  if not thin then
                    fail "nested member reference requires a thin archive";
                  let origin =
                    decimal ~limit:Elf_reader.max_file_size
                      (String.sub reference (at + 1) (String.length reference - at - 1))
                  in
                  if origin < 8 then fail "invalid nested member header offset";
                  nested := Some origin;
                  String.sub reference 0 at
            in
            let key = decimal ~limit:length reference in
            match Hashtbl.find_opt long_names key with
            | Some name -> name
            | None -> fail "invalid long member name offset %d" key
          else if String.ends_with ~suffix:"/" raw_name then
            String.sub raw_name 0 (String.length raw_name - 1)
          else raw_name
        in
        if name = "" || String.contains name '\000' then fail "invalid member name";
        if String.starts_with ~prefix:"__.SYMDEF" name then
          fail "BSD symbol indexes are not supported yet";
        let external_member =
          if not thin then None
          else
            Some
              ( (if Filename.is_relative name then
                   Filename.concat (Filename.dirname path) name
                 else name),
                !nested )
        in
        {
          name;
          header;
          offset;
          size;
          external_member;
          exports = [];
          object_ = None;
          loaded = false;
        })
  in
  let by_offset = Hashtbl.create ~random:true (Array.length members) in
  Array.iteri (fun i m -> Hashtbl.add by_offset m.header i) members;
  let index = Hashtbl.create ~random:true 256 in
  Option.iter
    (fun (offset, size, width) ->
      B.span ~context ~length:(offset + size) offset width;
      let integer at =
        let value =
          if width = 4 then
            Int64.logand (Int64.of_int32 (String.get_int32_be data at)) 0xffff_ffffL
          else String.get_int64_be data at
        in
        B.index ~context ~limit:length value
      in
      let count = integer offset in
      if count > 1_000_000 then fail "symbol index exceeds 1000000 entry limit";
      B.span ~context ~length:(offset + size) (offset + width) (count * width);
      let cursor = ref (offset + width + (count * width)) in
      for n = 0 to count - 1 do
        let member_offset = integer (offset + width + (n * width)) in
        let member =
          match Hashtbl.find_opt by_offset member_offset with
          | Some i -> i
          | None -> fail "symbol index points outside a member header: %d" member_offset
        in
        let name = B.string_at ~context data ~offset ~size (!cursor - offset) in
        if name = "" then fail "empty symbol in archive index";
        cursor := !cursor + String.length name + 1;
        List.iter
          (fun alias ->
            let previous = Option.value (Hashtbl.find_opt index alias) ~default:[] in
            Hashtbl.replace index alias (member :: previous);
            members.(member).exports <- alias :: members.(member).exports)
          (name :: Symver.aliases name)
      done;
      for n = !cursor to offset + size - 1 do
        if data.[n] <> '\000' then fail "trailing garbage in symbol index"
      done)
    !symbols;
  {
    path;
    data;
    members;
    index;
    indexed = !symbols <> None;
    thin;
    nested = Hashtbl.create ~random:true 4;
    members_by_header = by_offset;
  }

let object_ ?stats ?on_decoded
    ?(read_external = File_io.read ~max_size:Elf_reader.max_file_size) archive index =
  let member = archive.members.(index) in
  match member.object_ with
  | Some object_ -> object_
  | None ->
      let path = Printf.sprintf "%s(%s)" archive.path member.name in
      let data =
        match member.external_member with
        | None -> String.sub archive.data member.offset member.size
        | Some (file, None) -> read_external file
        | Some (file, Some header) -> (
            let key = Unix.realpath file in
            let nested =
              match Hashtbl.find_opt archive.nested key with
              | Some nested -> nested
              | None ->
                  let nested = parse ~path:file (read_external file) in
                  Hashtbl.add archive.nested key nested;
                  nested
            in
            if nested.thin then
              Error.fail "%s: nested container must be a regular archive" path;
            let target = Hashtbl.find_opt nested.members_by_header header in
            match target with
            | None ->
                Error.fail "%s: invalid nested member header offset %d" path header
            | Some index ->
                let m = nested.members.(index) in
                String.sub nested.data m.offset m.size)
      in
      let object_ =
        Stats.measure stats "parse" (fun () ->
            Symver.expand (Elf_reader.parse ?on_decoded ~path data))
      in
      let definitions = Hashtbl.create ~random:true (Array.length object_.symbols) in
      Array.iter
        (fun (s : Elf_reader.symbol) ->
          if s.binding <> 0 && s.section <> 0 then Hashtbl.replace definitions s.name ())
        object_.symbols;
      List.iter
        (fun name ->
          if not (Hashtbl.mem definitions name) then
            Error.fail "%s: archive index advertises missing definition %s" path name)
        member.exports;
      member.object_ <- Some object_;
      object_
