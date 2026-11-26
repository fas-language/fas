(* Target-independent object description, serialized as an x86 relocatable
   object. Section/symbol references are zero-based input-array indices;
   ELF's null records and local/global ordering are supplied by the writer. *)
module B = Binary

type contents = Data of bytes | Bss of int
type section = { name : string; flags : int; alignment : int; contents : contents }
type definition = Undefined | Absolute of int64 | Common of int64 | Section of int * int64

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

type encoding = X86_64 | I386

let write ?(encoding = X86_64) input =
  let limit = Elf_reader.max_file_size in
  let context = "ELF object emission" in
  let add = B.add ~context ~limit in
  let align = B.align ~context ~limit in
  let sections = input.sections and symbols = input.symbols in
  let elf32 = encoding = I386 in
  let word = if elf32 then 4 else 8 in
  let ehsize = if elf32 then 52 else 64 in
  let shsize = if elf32 then 40 else 64 in
  let symsize = if elf32 then 16 else 24 in
  let relsize = if elf32 then 8 else 24 in
  let reloc_name = if elf32 then ".rel" else ".rela" in
  let i386_width = function
    | 0 -> 0
    | 1 | 2 | 3 | 4 | 9 | 10 | 43 -> 4
    | 20 | 21 -> 2
    | 22 | 23 -> 1
    | kind -> Error.fail "%s: unsupported i386 relocation %d" context kind
  in
  let check_width width value =
    let bits = width * 8 in
    if value < Int64.neg (Int64.shift_left 1L (bits - 1)) || value >= Int64.shift_left 1L bits then
      Error.fail "%s: value 0x%Lx does not fit in %d bytes" context value width
  in
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
    (* Both string tables start with "\000": an empty name (a section
       symbol, which readers then name after its section) is offset 0. *)
    if name = "" then 0 else
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
      (fun (index, _) -> string section_strings (reloc_name ^ sections.(index).name))
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
  let cursor = ref ehsize in
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
        cursor := align !cursor word;
        let offset = !cursor in
        cursor := add !cursor (List.length rs * relsize);
        offset)
      relocation_sections
  in
  let symtab_offset = align !cursor word in
  let symtab_size = (Array.length symbols + 1) * symsize in
  let strtab_offset = add symtab_offset symtab_size in
  let shstrtab_offset = add strtab_offset (Buffer.length strings) in
  let shoff = align (add shstrtab_offset (Buffer.length section_strings)) word in
  let out = Bytes.make (add shoff (count * shsize)) '\000' in
  Bytes.blit_string (if elf32 then "\127ELF\001\001\001" else "\127ELF\002\001\001") 0 out 0 7;
  B.set_int out 16 2 1;
  B.set_int out 18 2 (if elf32 then 3 else 62);
  B.set_int out 20 4 1;
  B.set_int out (if elf32 then 32 else 40) word shoff;
  let sizes = if elf32 then 40 else 52 in
  B.set_int out sizes 2 ehsize;
  B.set_int out (sizes + 6) 2 shsize;
  B.set_int out (sizes + 8) 2 count;
  B.set_int out (sizes + 10) 2 shstrtab_index;
  let header index ~name ~kind ~flags ~offset ~size ~link ~info ~alignment ~entsize =
    let at = shoff + (index * shsize) in
    let offset_at = 8 + (2 * word) in
    let link_at = offset_at + (2 * word) in
    List.iter
      (fun (field, width, value) -> B.set_int out (at + field) width value)
      [
        (0, 4, name);
        (4, 4, kind);
        (8, word, flags);
        (offset_at, word, offset);
        (offset_at + word, word, size);
        (link_at, 4, link);
        (link_at + 4, 4, info);
        (link_at + 8, word, alignment);
        (link_at + 8 + word, word, entsize);
      ]
  in
  let section_kind name = function
    | Bss _ -> 8
    | Data _ ->
        if name = ".init_array" || String.starts_with ~prefix:".init_array." name then 14
        else if name = ".fini_array" || String.starts_with ~prefix:".fini_array." name then 15
        else if name = ".preinit_array" || String.starts_with ~prefix:".preinit_array." name then 16
        else if name = ".note.stapsdt" || name = ".note.ABI-tag" || name = ".note.gnu.property" then 7
        else 1
  in
  Array.iteri
    (fun index (s : section) ->
      let kind = section_kind s.name s.contents in
      let size =
        match s.contents with
        | Data bytes -> Bytes.length bytes
        | Bss size -> size
      in
      if elf32 && (size < 0 || size > 0xffff_ffff || s.flags < 0 || s.flags > 0xffff_ffff) then
        Error.fail "%s: section size or flags out of range" context;
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
        ~name ~kind:(if elf32 then 9 else 4) ~flags:0x40 ~offset
        ~size:(List.length rs * relsize)
        ~link:symtab_index ~info:(target + 1) ~alignment:word ~entsize:relsize;
      if elf32 then begin
        let length = match sections.(target).contents with Data bytes -> Bytes.length bytes | Bss _ -> 0 in
        let last = ref 0 in
        List.iter
          (fun (r : relocation) ->
            let width = i386_width r.kind in
            B.span ~context ~length r.offset width;
            if width <> 0 then begin
              if r.offset < !last then Error.fail "%s: overlapping i386 relocations" context;
              check_width width r.addend;
              B.set out (section_offsets.(target) + r.offset) width r.addend;
              last := r.offset + width
            end)
          (List.sort (fun (a : relocation) (b : relocation) -> compare a.offset b.offset) rs)
      end;
      List.iteri
        (fun index (r : relocation) ->
          let at = offset + (index * relsize) in
          B.set_int out at word r.offset;
          B.set out (at + word) word
            (Int64.logor
               (Int64.shift_left (Int64.of_int symbol_indices.(r.symbol)) (if elf32 then 8 else 32))
               (Int64.of_int r.kind));
          if not elf32 then B.set out (at + 16) 8 r.addend)
        rs)
    (List.combine relocation_sections
       (List.combine relocation_offsets relocation_names));
  Array.iteri
    (fun index original ->
      let s = symbols.(original) in
      let at = symtab_offset + ((index + 1) * symsize) in
      let section, value =
        match s.definition with
        | Undefined -> (0, 0L)
        | Absolute value -> (0xfff1, value)
        | Common alignment -> (0xfff2, alignment)
        | Section (section, value) ->
            if section < 0 || section >= Array.length sections then
              Error.fail "%s: invalid symbol section index" context;
            (section + 1, value)
      in
      if
        (* kinds 0-4 (NOTYPE..FILE), 6 (STT_TLS), and 10 (STT_GNU_IFUNC) *)
        s.binding < 0 || s.binding > 2 || s.kind < 0 || (s.kind > 4 && s.kind <> 6 && s.kind <> 10) || s.visibility < 0
        || s.visibility > 3
      then Error.fail "%s: invalid symbol attributes" context;
      if elf32 then begin
        check_width 4 value;
        if s.size < 0L || s.size > 0xffff_ffffL then
          Error.fail "%s: symbol size out of range" context;
        match s.definition with
        | Section (i, _) ->
            let length = match sections.(i).contents with Data bytes -> Bytes.length bytes | Bss n -> n in
            B.span ~context ~length (Int64.to_int value) (Int64.to_int s.size)
        | _ -> ()
      end;
      let attrs = at + (if elf32 then 12 else 4) in
      let value_at = at + (if elf32 then 4 else 8) in
      B.set_int out at 4 symbol_names.(original);
      B.set_int out attrs 1 ((s.binding lsl 4) lor s.kind);
      B.set_int out (attrs + 1) 1 s.visibility;
      B.set_int out (attrs + 2) 2 section;
      B.set out value_at word value;
      B.set out (value_at + word) word s.size)
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
    ~alignment:word ~entsize:symsize;
  header strtab_index ~name:strtab_name ~kind:3 ~flags:0 ~offset:strtab_offset
    ~size:(Buffer.length strings) ~link:0 ~info:0 ~alignment:1 ~entsize:0;
  header shstrtab_index ~name:shstrtab_name ~kind:3 ~flags:0 ~offset:shstrtab_offset
    ~size:(Buffer.length section_strings)
    ~link:0 ~info:0 ~alignment:1 ~entsize:0;
  (* The x86-64 reader also checks symbol spans, patch bounds and supported
     attributes. The i386 checks above validate its REL patches directly. *)
  if not elf32 then ignore (Elf_reader.parse ~path:"<generated object>" (Bytes.unsafe_to_string out));
  out
