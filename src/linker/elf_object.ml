(* Target-independent object description, serialized as the supported ELF64
   x86-64 subset. Section/symbol references are zero-based input-array indices;
   ELF's null records and local/global ordering are supplied by the writer. *)
module B = Binary

type contents = Data of bytes | Bss of int
type section = { name : string; flags : int; alignment : int; contents : contents }
type definition = Undefined | Absolute of int64 | Section of int * int64

type symbol = {
  name : string;
  binding : int;
  kind : int;
  visibility : int;
  definition : definition;
  size : int64;
}

type relocation = {
  section : int;
  offset : int;
  symbol : int;
  kind : int;
  addend : int64;
}

type t = {
  sections : section array;
  symbols : symbol array;
  relocations : relocation array;
}

let write input =
  let limit = Elf_reader.max_file_size in
  let context = "ELF object emission" in
  let add = B.add ~context ~limit in
  let align = B.align ~context ~limit in
  let sections = input.sections and symbols = input.symbols in
  if Array.length sections >= 0xff00 then
    Error.fail "%s: extended section numbering is unsupported" context;
  if Array.length symbols > 999999 then Error.fail "%s: too many symbols" context;
  if Array.length input.relocations > 1_000_000 then
    Error.fail "%s: too many relocations" context;
  let relocs = Array.make (Array.length sections) [] in
  Array.iter
    (fun (r : relocation) ->
      if r.section < 0 || r.section >= Array.length sections then
        Error.fail "%s: invalid relocation section index" context;
      if r.symbol < 0 || r.symbol >= Array.length symbols then
        Error.fail "%s: invalid relocation symbol index" context;
      relocs.(r.section) <- r :: relocs.(r.section))
    input.relocations;
  let relocation_sections =
    Array.to_list (Array.mapi (fun index rs -> (index, List.rev rs)) relocs)
    |> List.filter (fun (_, rs) -> rs <> [])
  in
  let symtab_index = 1 + Array.length sections + List.length relocation_sections in
  let strtab_index = symtab_index + 1 and shstrtab_index = symtab_index + 2 in
  let count = symtab_index + 3 in
  if count >= 0xff00 then
    Error.fail "%s: extended section numbering is unsupported" context;
  let strings = Buffer.create 256 and section_strings = Buffer.create 128 in
  Buffer.add_char strings '\000';
  Buffer.add_char section_strings '\000';
  let string buffer name =
    if String.contains name '\000' || String.length name > 65535 then
      Error.fail "%s: invalid or oversized name" context;
    let offset = Buffer.length buffer in
    ignore (add offset (String.length name + 1));
    Buffer.add_string buffer name;
    Buffer.add_char buffer '\000';
    offset
  in
  let section_names =
    Array.map (fun (s : section) -> string section_strings s.name) sections
  in
  let relocation_names =
    List.map
      (fun (index, _) -> string section_strings (".rela" ^ sections.(index).name))
      relocation_sections
  in
  let symtab_name = string section_strings ".symtab" in
  let strtab_name = string section_strings ".strtab" in
  let shstrtab_name = string section_strings ".shstrtab" in
  let locals = ref [] and globals = ref [] in
  Array.iteri
    (fun index (s : symbol) ->
      if s.binding = 0 then locals := index :: !locals else globals := index :: !globals)
    symbols;
  let ordered = List.rev !locals @ List.rev !globals |> Array.of_list in
  let symbol_indices = Array.make (Array.length symbols) 0 in
  Array.iteri (fun index original -> symbol_indices.(original) <- index + 1) ordered;
  let symbol_names = Array.map (fun (s : symbol) -> string strings s.name) symbols in
  let cursor = ref 64 in
  let section_offsets =
    Array.map
      (fun (s : section) ->
        cursor := align !cursor (max 1 s.alignment);
        let offset = !cursor in
        (match s.contents with
        | Data bytes -> cursor := add !cursor (Bytes.length bytes)
        | Bss _ -> ());
        offset)
      sections
  in
  let relocation_offsets =
    List.map
      (fun (_, rs) ->
        cursor := align !cursor 8;
        let offset = !cursor in
        cursor := add !cursor (List.length rs * 24);
        offset)
      relocation_sections
  in
  let symtab_offset = align !cursor 8 in
  let symtab_size = (Array.length symbols + 1) * 24 in
  let strtab_offset = add symtab_offset symtab_size in
  let shstrtab_offset = add strtab_offset (Buffer.length strings) in
  let shoff = align (add shstrtab_offset (Buffer.length section_strings)) 8 in
  let out = Bytes.make (add shoff (count * 64)) '\000' in
  Bytes.blit_string "\127ELF\002\001\001" 0 out 0 7;
  B.set_int out 16 2 1;
  B.set_int out 18 2 62;
  B.set_int out 20 4 1;
  B.set_int out 40 8 shoff;
  B.set_int out 52 2 64;
  B.set_int out 58 2 64;
  B.set_int out 60 2 count;
  B.set_int out 62 2 shstrtab_index;
  let header index ~name ~kind ~flags ~offset ~size ~link ~info ~alignment ~entsize =
    let at = shoff + (index * 64) in
    List.iter
      (fun (field, width, value) -> B.set_int out (at + field) width value)
      [
        (0, 4, name);
        (4, 4, kind);
        (8, 8, flags);
        (24, 8, offset);
        (32, 8, size);
        (40, 4, link);
        (44, 4, info);
        (48, 8, alignment);
        (56, 8, entsize);
      ]
  in
  Array.iteri
    (fun index (s : section) ->
      let kind, size =
        match s.contents with
        | Data bytes -> (1, Bytes.length bytes)
        | Bss size -> (8, size)
      in
      (match s.contents with
      | Data bytes -> Bytes.blit bytes 0 out section_offsets.(index) size
      | Bss _ -> ());
      header (index + 1) ~name:section_names.(index) ~kind ~flags:s.flags
        ~offset:section_offsets.(index) ~size ~link:0 ~info:0 ~alignment:s.alignment
        ~entsize:0)
    sections;
  List.iteri
    (fun index ((target, rs), (offset, name)) ->
      header
        (1 + Array.length sections + index)
        ~name ~kind:4 ~flags:0x40 ~offset
        ~size:(List.length rs * 24)
        ~link:symtab_index ~info:(target + 1) ~alignment:8 ~entsize:24;
      List.iteri
        (fun index (r : relocation) ->
          let at = offset + (index * 24) in
          B.set_int out at 8 r.offset;
          B.set out (at + 8) 8
            (Int64.logor
               (Int64.shift_left (Int64.of_int symbol_indices.(r.symbol)) 32)
               (Int64.of_int r.kind));
          B.set out (at + 16) 8 r.addend)
        rs)
    (List.combine relocation_sections
       (List.combine relocation_offsets relocation_names));
  Array.iteri
    (fun index original ->
      let s = symbols.(original) in
      let at = symtab_offset + ((index + 1) * 24) in
      let section, value =
        match s.definition with
        | Undefined -> (0, 0L)
        | Absolute value -> (0xfff1, value)
        | Section (section, value) ->
            if section < 0 || section >= Array.length sections then
              Error.fail "%s: invalid symbol section index" context;
            (section + 1, value)
      in
      if
        s.binding < 0 || s.binding > 2 || s.kind < 0 || s.kind > 4 || s.visibility < 0
        || s.visibility > 3
      then Error.fail "%s: invalid symbol attributes" context;
      B.set_int out at 4 symbol_names.(original);
      B.set_int out (at + 4) 1 ((s.binding lsl 4) lor s.kind);
      B.set_int out (at + 5) 1 s.visibility;
      B.set_int out (at + 6) 2 section;
      B.set out (at + 8) 8 value;
      B.set out (at + 16) 8 s.size)
    ordered;
  Bytes.blit_string (Buffer.contents strings) 0 out strtab_offset
    (Buffer.length strings);
  Bytes.blit_string
    (Buffer.contents section_strings)
    0 out shstrtab_offset
    (Buffer.length section_strings);
  header symtab_index ~name:symtab_name ~kind:2 ~flags:0 ~offset:symtab_offset
    ~size:symtab_size ~link:strtab_index
    ~info:(List.length !locals + 1)
    ~alignment:8 ~entsize:24;
  header strtab_index ~name:strtab_name ~kind:3 ~flags:0 ~offset:strtab_offset
    ~size:(Buffer.length strings) ~link:0 ~info:0 ~alignment:1 ~entsize:0;
  header shstrtab_index ~name:shstrtab_name ~kind:3 ~flags:0 ~offset:shstrtab_offset
    ~size:(Buffer.length section_strings)
    ~link:0 ~info:0 ~alignment:1 ~entsize:0;
  (* Reuse the reader's complete validation contract before exposing bytes. This
     includes symbol spans, patch bounds/overlap, flags and supported attributes. *)
  ignore (Elf_reader.parse ~path:"<generated object>" (Bytes.unsafe_to_string out));
  out
