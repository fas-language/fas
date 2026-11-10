(* Supported input is deliberately narrow. Unknown semantics fail closed rather
   than disappearing from a seemingly successful executable. *)
module B = Binary

let max_file_size = 128 * 1024 * 1024
let max_alignment = 2 * 1024 * 1024
let max_section_size = 1024 * 1024 * 1024

type section = {
  index : int;
  name : string;
  kind : int;
  flags : int;
  offset : int;
  size : int;
  link : int;
  info : int;
  alignment : int;
  entsize : int;
}

type symbol = {
  name : string;
  binding : int;
  kind : int;
  visibility : int;
  section : int;
  value : int64;
  size : int64;
}

type relocation = {
  target : int;
  offset : int;
  symbol : int;
  kind : int;
  addend : int64;
}

type group = { signature : string; comdat : bool; members : int array }

type t = {
  path : string;
  data : string;
  sections : section array;
  symbols : symbol array;
  relocations : relocation array;
  groups : group array;
}

let relocation_width = function
  | 0 | 35 -> Some 0
  | 1 | 17 | 24 | 29 -> Some 8
  | 2 | 4 | 9 | 10 | 11 | 19 | 20 | 21 | 22 | 23 | 26 | 34 | 41 | 42 -> Some 4
  | _ -> None

let parse ?(on_decoded = fun _ -> ()) ~path data =
  let length = String.length data in
  let fail fmt = Printf.ksprintf (fun s -> Error.fail "%s: %s" path s) fmt in
  if
    length >= 4
    && (String.sub data 0 4 = "BC\192\222" || String.sub data 0 4 = "\222\192\023\011")
  then fail "LLVM bitcode LTO is unsupported; compile native objects without -flto";
  let context what = path ^ ": " ^ what in
  let name_bytes = ref 0 in
  let name_context = context "name" in
  let name_budget_context = context "decoded name storage" in
  let name_at _what ~offset ~size name_offset =
    let name = B.string_at ~context:name_context data ~offset ~size name_offset in
    name_bytes :=
      B.add ~context:name_budget_context
        ~limit:(64 * 1024 * 1024)
        !name_bytes (String.length name);
    name
  in
  let idx what limit value =
    if value < 0L || value > Int64.of_int limit then
      fail "%s: value 0x%Lx exceeds supported limit %d" what value limit;
    Int64.to_int value
  in
  let span what offset size =
    if offset < 0 || size < 0 || offset > length || size > length - offset then
      B.span ~context:(context what) ~length offset size
  in
  if length > max_file_size then fail "input exceeds %d byte limit" max_file_size;
  span "ELF header" 0 64;
  if String.sub data 0 4 <> "\127ELF" then fail "expected an ELF object";
  if B.u8 data 4 <> 2 then fail "unsupported ELF class (expected ELF64)";
  if B.u8 data 5 <> 1 then fail "unsupported ELF byte order (expected little-endian)";
  if B.u8 data 6 <> 1 then fail "invalid ELF identification version";
  if (B.u8 data 7 <> 0 && B.u8 data 7 <> 3) || B.u8 data 8 <> 0 then
    fail "unsupported ELF OS ABI or ABI version";
  for n = 9 to 15 do
    if B.u8 data n <> 0 then fail "nonzero ELF identification padding"
  done;
  if B.u16 data 16 <> 1 then fail "unsupported ELF type (expected ET_REL)";
  if B.u16 data 18 <> 62 then fail "unsupported ELF machine (expected x86-64)";
  if B.u32 data 20 <> 1L then fail "invalid ELF version";
  if B.u64 data 24 <> 0L || B.u64 data 32 <> 0L || B.u16 data 56 <> 0 then
    fail "ET_REL input must not have an entry point or program headers";
  if B.u16 data 54 <> 0 then fail "unsupported ET_REL program header entry size";
  if B.u32 data 48 <> 0L then fail "unsupported ELF flags";
  if B.u16 data 52 <> 64 || B.u16 data 58 <> 64 then
    fail "invalid ELF header or section entry size";
  let count = B.u16 data 60 and names_index = B.u16 data 62 in
  if count = 0 || count >= 0xff00 || names_index = 0xffff then
    fail "unsupported extended section numbering";
  if names_index = 0 || names_index >= count then
    fail "missing or invalid section name table index";
  let shoff = idx "section table offset" length (B.u64 data 40) in
  if shoff < 64 || shoff land 7 <> 0 then fail "invalid section table offset";
  span "section table" shoff (count * 64);
  for n = 0 to 63 do
    if B.u8 data (shoff + n) <> 0 then fail "invalid null section header"
  done;
  let sections =
    Array.init count (fun index ->
        let at = shoff + (index * 64) in
        let kind = idx "section type" max_int (B.u32 data (at + 4)) in
        let flags = idx "section flags" max_int (B.u64 data (at + 8)) in
        (* NOBITS sh_offset has no file interpretation. Never use it to index input. *)
        let offset =
          if kind = 8 then 0 else idx "section offset" length (B.u64 data (at + 24))
        in
        let size = idx "section size" max_section_size (B.u64 data (at + 32)) in
        let link = idx "section link" (count - 1) (B.u32 data (at + 40)) in
        let info = idx "section info" max_int (B.u32 data (at + 44)) in
        let alignment = idx "section alignment" max_alignment (B.u64 data (at + 48)) in
        let entsize = idx "section entry size" max_file_size (B.u64 data (at + 56)) in
        (* GNU partial links and glibc CRT objects can retain nonzero sh_addr.
           ET_REL symbols and relocations remain section-relative; only the
           output layout assigns load addresses, so do not use this field. *)
        if alignment <> 0 && alignment land (alignment - 1) <> 0 then
          fail "section %d: alignment must be a power of two" index;
        if kind <> 8 then span (Printf.sprintf "section %d" index) offset size;
        {
          index;
          name = "";
          kind;
          flags;
          offset;
          size;
          link;
          info;
          alignment = max 1 alignment;
          entsize;
        })
  in
  let ranges =
    (0, 64, "ELF header")
    :: (shoff, shoff + (count * 64), "section table")
    :: (Array.to_list sections
       |> List.filter_map (fun (s : section) ->
           if s.kind = 8 || s.size = 0 then None
           else Some (s.offset, s.offset + s.size, Printf.sprintf "section %d" s.index))
       )
  in
  let ranges = List.sort (fun (a, _, _) (b, _, _) -> compare a b) ranges in
  let previous_end = ref 0 and previous_name = ref "" in
  List.iter
    (fun (start, ending, name) ->
      if start < !previous_end then fail "%s overlaps %s" name !previous_name;
      previous_end := ending;
      previous_name := name)
    ranges;
  let names = sections.(names_index) in
  if names.kind <> 3 || names.size = 0 then fail "invalid section name string table";
  let string_table (s : section) =
    if
      s.kind <> 3 || s.size = 0
      || B.u8 data s.offset <> 0
      || B.u8 data (s.offset + s.size - 1) <> 0
    then fail "section %d: invalid string table" s.index
  in
  string_table names;
  let sections =
    Array.mapi
      (fun index (s : section) ->
        let name_offset =
          idx "section name offset" (names.size - 1) (B.u32 data (shoff + (index * 64)))
        in
        let name =
          name_at "section name" ~offset:names.offset ~size:names.size name_offset
        in
        { s with name })
      sections
  in
  let sections =
    Array.map
      (fun (s : section) ->
        if s.kind = 0x70000001 then begin
          if s.name <> ".eh_frame" then
            fail "section %s: unsupported x86-64 unwind section" s.name;
          { s with kind = 1 }
        end
        else s)
      sections
  in
  let data, sections =
    if not (Array.exists (fun (s : section) -> s.flags land 0x800 <> 0) sections) then
      (data, sections)
    else begin
      let decoded_size = ref 0 in
      let normalized = Buffer.create length in
      Buffer.add_string normalized data;
      let sections =
        Array.map
          (fun (s : section) ->
            if s.flags land 0x800 = 0 then s
            else begin
              if
                s.kind <> 1
                || s.flags land 2 <> 0
                || not (String.starts_with ~prefix:".debug_" s.name)
              then
                fail "section %s: compression requires non-allocatable debug PROGBITS"
                  s.name;
              if s.size < 24 then fail "section %s: truncated compression header" s.name;
              if B.u32 data s.offset <> 1L then
                fail "section %s: unsupported compression type (only zlib)" s.name;
              if B.u32 data (s.offset + 4) <> 0L then
                fail "section %s: nonzero compression reserved field" s.name;
              let size =
                idx "uncompressed debug size" max_file_size (B.u64 data (s.offset + 8))
              in
              decoded_size :=
                B.add
                  ~context:(context "decoded debug storage")
                  ~limit:max_file_size !decoded_size size;
              on_decoded size;
              let alignment =
                idx "uncompressed debug alignment" max_alignment
                  (B.u64 data (s.offset + 16))
              in
              if alignment <> 0 && alignment land (alignment - 1) <> 0 then
                fail "section %s: invalid uncompressed alignment" s.name;
              let payload =
                Zlib_decode.decode ~context:(context s.name) ~size data
                  ~offset:(s.offset + 24) ~length:(s.size - 24)
              in
              let offset = Buffer.length normalized in
              Buffer.add_string normalized payload;
              {
                s with
                flags = s.flags land lnot 0x800;
                offset;
                size;
                alignment = max 1 alignment;
              }
            end)
          sections
      in
      (Buffer.contents normalized, sections)
    end
  in
  let symtab_index = ref None in
  let sdt_pointers = Hashtbl.create 4 in
  Array.iter
    (fun (s : section) ->
      if
        String.starts_with ~prefix:".gnu.lto" s.name
        || String.starts_with ~prefix:".llvm.lto" s.name
      then fail "section %s: LTO objects are unsupported; compile without -flto" s.name;
      if
        (s.name = ".eh_frame" && s.kind <> 1)
        || List.mem s.name [ ".note.stapsdt"; ".note.gnu.property"; ".note.ABI-tag" ]
           && s.kind <> 7
      then fail "section %s: invalid metadata section type" s.name;
      let allowed_flags =
        if s.kind = 4 then 0x240
        else if s.kind = 0x6fff4c03 then 0x80000000
        else if s.kind = 1 || s.kind = 8 then 0x200637
        else if s.kind = 14 || s.kind = 15 || s.kind = 16 then 0x200003
        else 7
      in
      if s.flags land lnot allowed_flags <> 0 then
        fail "section %s: unsupported flags 0x%x" s.name s.flags;
      if s.flags land 5 = 5 then
        fail "section %s: writable executable sections are unsupported" s.name;
      match s.kind with
      | 0 when s.index = 0 -> ()
      | 1 | 8 ->
          if s.flags land 0x400 <> 0 && s.flags land lnot 0x200 <> 0x403 then
            fail "section %s: invalid TLS flags" s.name;
          if s.kind = 8 && s.flags land 2 = 0 then
            fail "section %s: non-allocatable NOBITS is unsupported" s.name;
          if s.name = ".note.GNU-stack" && (s.size <> 0 || s.flags <> 0) then
            fail
              "section .note.GNU-stack: executable stack or nonempty stack marker is \
               unsupported";
          if s.name = ".eh_frame" && (s.flags land lnot 0x200 <> 2 || s.alignment > 8)
          then fail "section .eh_frame: unsupported unwind flags or alignment";
          let merge = s.flags land 0x10 <> 0 and strings = s.flags land 0x20 <> 0 in
          if strings && not merge then
            fail "section %s: SHF_STRINGS requires SHF_MERGE" s.name;
          if merge then begin
            if s.kind <> 1 || s.entsize = 0 || s.size mod s.entsize <> 0 then
              fail "section %s: invalid merge entry size" s.name;
            if strings && s.entsize <> 1 && s.entsize <> 2 && s.entsize <> 4 then
              fail "section %s: unsupported merge string character width" s.name;
            if strings && s.size <> 0 then
              for index = s.size - s.entsize to s.size - 1 do
                if B.u8 data (s.offset + index) <> 0 then
                  fail "section %s: unterminated merge strings" s.name
              done
          end;
          if s.link <> 0 || s.info <> 0 || ((not merge) && s.entsize <> 0) then
            fail "section %s: unsupported section metadata" s.name
      | 14 | 15 | 16 ->
          let expected =
            match s.kind with
            | 14 -> ".init_array"
            | 15 -> ".fini_array"
            | _ -> ".preinit_array"
          in
          if s.name <> expected then begin
            if not (String.starts_with ~prefix:(expected ^ ".") s.name) then
              fail "section %s: unexpected initialization array name" s.name;
            let suffix =
              String.sub s.name
                (String.length expected + 1)
                (String.length s.name - String.length expected - 1)
            in
            if
              suffix = ""
              || (not (String.for_all (fun c -> c >= '0' && c <= '9') suffix))
              ||
              match int_of_string_opt suffix with
              | Some p -> p > 65535
              | None -> true
            then fail "section %s: invalid initialization priority" s.name
          end;
          if
            s.flags land lnot 0x200 <> 3
            || s.link <> 0 || s.info <> 0
            || (s.entsize <> 0 && s.entsize <> 8)
            || s.size mod 8 <> 0
            || s.alignment > 8
          then fail "section %s: invalid initialization array metadata" s.name
      | 2 ->
          if !symtab_index <> None then fail "multiple symbol tables are unsupported";
          symtab_index := Some s.index;
          if s.flags <> 0 then
            fail "section %s: allocatable symbol table is unsupported" s.name
      | 3 ->
          string_table s;
          if s.flags <> 0 || s.link <> 0 || s.info <> 0 || s.entsize <> 0 then
            fail "section %s: unsupported string table metadata" s.name
      | 4 -> ()
      | 7 ->
          if s.name = ".note.stapsdt" then begin
            if
              s.flags <> 0 || s.alignment <> 4 || s.link <> 0 || s.info <> 0
              || s.entsize <> 0
            then fail "section %s: unsupported SDT note metadata" s.name;
            Hashtbl.add sdt_pointers s.index
              (Sdt_note.pointers ~path ~offset:s.offset ~size:s.size data)
          end
          else if s.name = ".note.ABI-tag" then
            begin if
              s.flags <> 2 || s.alignment <> 4 || s.link <> 0 || s.info <> 0
              || s.entsize <> 0 || s.size <> 32
              || B.u32 data s.offset <> 4L
              || B.u32 data (s.offset + 4) <> 16L
              || B.u32 data (s.offset + 8) <> 1L
              || String.sub data (s.offset + 12) 4 <> "GNU\000"
              || B.u32 data (s.offset + 16) <> 0L
            then fail "section %s: invalid GNU Linux ABI note" s.name
            end
          else begin
            if
              s.name <> ".note.gnu.property"
              || s.flags <> 2 || s.alignment <> 8 || s.link <> 0 || s.info <> 0
              || s.entsize <> 0
            then fail "section %s: unsupported note metadata" s.name;
            ignore (Gnu_property.parse ~path ~offset:s.offset ~size:s.size data)
          end
      | 17 ->
          if s.flags <> 0 || s.entsize <> 4 || s.size < 8 || s.size mod 4 <> 0 then
            fail "section %s: invalid section group metadata" s.name
      | 0x6fff4c03 ->
          if
            (s.flags <> 0 && s.flags <> 0x80000000)
            || s.info <> 0 || s.entsize <> 0 || s.alignment <> 1
          then fail "section %s: invalid LLVM address-significance metadata" s.name
      | _ -> fail "section %s: unsupported section type %d" s.name s.kind)
    sections;
  let symtab_index = Option.value !symtab_index ~default:0 in
  let symbols =
    if symtab_index = 0 then [||]
    else begin
      let symtab = sections.(symtab_index) in
      if symtab.entsize <> 24 || symtab.size mod 24 <> 0 || symtab.size = 0 then
        fail "invalid symbol table entry size";
      let symbol_count = symtab.size / 24 in
      if symbol_count > 1_000_000 then fail "symbol count exceeds supported limit";
      if symtab.info < 1 || symtab.info > symbol_count then
        fail "invalid first global symbol index";
      let strings = sections.(symtab.link) in
      string_table strings;
      for n = 0 to 23 do
        if B.u8 data (symtab.offset + n) <> 0 then fail "invalid null symbol"
      done;
      let symbols =
        Array.init symbol_count (fun index ->
            let at = symtab.offset + (index * 24) in
            let name_offset =
              idx "symbol name offset" (strings.size - 1) (B.u32 data at)
            in
            let name =
              name_at "symbol name" ~offset:strings.offset ~size:strings.size
                name_offset
            in
            let info = B.u8 data (at + 4) in
            let binding = info lsr 4 and kind = info land 15 in
            let section = B.u16 data (at + 6) in
            let value = B.u64 data (at + 8) and size = B.u64 data (at + 16) in
            if binding > 2 then fail "symbol %s: unsupported binding %d" name binding;
            if index < symtab.info <> (binding = 0) then
              fail "symbol %s: invalid local/global ordering" name;
            let visibility = B.u8 data (at + 5) in
            if (kind > 6 && kind <> 10) || visibility > 3 then
              fail "symbol %s: unsupported type or visibility" name;
            if binding <> 0 && name = "" then fail "global symbol has an empty name";
            if kind = 5 && section <> 0xfff2 then
              fail "symbol %s: STT_COMMON requires SHN_COMMON" name;
            if section = 0 then begin
              if index <> 0 && binding = 0 then
                fail "symbol %s: undefined local symbol" name;
              if value <> 0L then fail "symbol %s: invalid undefined symbol value" name
            end
            else if section = 0xfff1 then begin
              if kind = 6 then fail "symbol %s: absolute TLS definition" name;
              if kind = 10 then fail "symbol %s: absolute IFUNC definition" name
            end
            else if section = 0xfff2 then begin
              if kind = 10 then fail "symbol %s: common IFUNC definition" name;
              if kind = 6 then fail "symbol %s: common TLS is unsupported" name;
              if binding <> 1 then fail "symbol %s: common symbol must be global" name;
              let alignment = idx "common alignment" max_alignment value in
              if alignment <> 0 && alignment land (alignment - 1) <> 0 then
                fail "symbol %s: common alignment must be a power of two" name;
              ignore (idx "common size" max_section_size size)
            end
            else if section >= 0xff00 then
              fail "symbol %s: unsupported special section index 0x%x" name section
            else begin
              if section >= count then
                fail "symbol %s: invalid section index %d" name section;
              let target = sections.(section) in
              if kind = 6 <> (target.flags land 0x400 <> 0) && kind <> 3 then
                fail "symbol %s: TLS type does not match defining section" name;
              if
                (not (List.mem target.kind [ 1; 7; 8; 14; 15; 16 ]))
                && (not (target.kind = 17 && binding = 0))
                && not
                     (target.kind = 0x6fff4c03 && binding = 0 && kind = 3 && value = 0L
                    && size = 0L)
              then fail "symbol %s: unsupported defining section" name;
              if
                value < 0L || size < 0L
                || value > Int64.of_int target.size
                || size > Int64.sub (Int64.of_int target.size) value
              then fail "symbol %s: value or size exceeds its section" name;
              if
                target.name = ".note.GNU-stack"
                && (kind <> 3 || binding <> 0 || value <> 0L || size <> 0L)
              then fail "symbol %s: definition in discarded stack marker" name;
              if
                kind = 10
                && (target.flags land 6 <> 6 || value >= Int64.of_int target.size)
              then fail "symbol %s: IFUNC resolver must be inside executable code" name
            end;
            { name; binding; kind; visibility; section; value; size })
      in
      symbols
    end
  in
  let symbol_count = Array.length symbols in
  let memberships = Array.make count (-1) in
  let groups = ref [] and group_count = ref 0 in
  Array.iter
    (fun (s : section) ->
      if s.kind = 17 then begin
        if s.link <> symtab_index || s.info = 0 || s.info >= symbol_count then
          fail "section %s: invalid group signature symbol" s.name;
        let symbol = symbols.(s.info) in
        let signature =
          if
            symbol.name = "" && symbol.kind = 3 && symbol.section <> 0
            && symbol.section < count
          then sections.(symbol.section).name
          else symbol.name
        in
        if signature = "" then fail "section %s: empty group signature" s.name;
        let flags = B.u32 data s.offset in
        if flags <> 0L && flags <> 1L then
          fail "section %s: unsupported group flags" s.name;
        if (s.size / 4) - 1 >= count then
          fail "section %s: group member count exceeds section count" s.name;
        let members =
          Array.init
            ((s.size / 4) - 1)
            (fun index ->
              let member =
                idx "group member" (count - 1) (B.u32 data (s.offset + 4 + (index * 4)))
              in
              let target = sections.(member) in
              if
                member <= s.index
                || target.flags land 0x200 = 0
                || (not (List.mem target.kind [ 1; 8; 4; 14; 15; 16 ]))
                || memberships.(member) <> -1
              then fail "section %s: invalid or repeated group member %d" s.name member;
              memberships.(member) <- !group_count;
              member)
        in
        groups := { signature; comdat = flags = 1L; members } :: !groups;
        incr group_count
      end)
    sections;
  Array.iter
    (fun (s : section) ->
      if s.flags land 0x200 <> 0 && memberships.(s.index) = -1 then
        fail "section %s: SHF_GROUP section has no group" s.name;
      if s.kind = 4 && s.info < count && memberships.(s.index) <> memberships.(s.info)
      then fail "section %s: relocation and target group membership differ" s.name)
    sections;
  let groups = Array.of_list (List.rev !groups) in
  Array.iter
    (fun (s : section) ->
      if s.kind = 0x6fff4c03 && s.link <> 0 then begin
        (* GNU/LLD -r invalidate unremapped addrsig tables with sh_link=0.
           Such contents cannot be interpreted as current symbol indices. *)
        if symtab_index = 0 || s.link <> symtab_index then
          fail "section %s: invalid address-significance symbol table" s.name;
        let at = ref s.offset and ending = s.offset + s.size in
        while !at < ending do
          let value = ref 0 and shift = ref 0 and more = ref true in
          while !more do
            if !at = ending || !shift >= 21 then
              fail "section %s: malformed address-significance ULEB128" s.name;
            let byte = B.u8 data !at in
            incr at;
            value := !value lor ((byte land 127) lsl !shift);
            shift := !shift + 7;
            more := byte land 128 <> 0
          done;
          if !value >= symbol_count then
            fail "section %s: address-significance symbol index out of range" s.name
        done
      end)
    sections;
  let relocation_count =
    Array.fold_left
      (fun n (s : section) ->
        if s.kind <> 4 then n
        else begin
          if s.entsize <> 24 || s.size mod 24 <> 0 then
            fail "section %s: invalid RELA entry size" s.name;
          if symtab_index = 0 || s.link <> symtab_index then
            fail "section %s: invalid relocation symbol table" s.name;
          if s.info = 0 || s.info >= count then
            fail "section %s: invalid relocation target index" s.name;
          let target = sections.(s.info) in
          if
            (not (List.mem target.kind [ 1; 14; 15; 16 ]))
            && not (target.kind = 7 && target.name = ".note.stapsdt")
            || target.name = ".note.GNU-stack"
          then fail "section %s: unsupported relocation target" s.name;
          B.add ~context:(context "relocation count") ~limit:1_000_000 n (s.size / 24)
        end)
      0 sections
  in
  let empty_reloc = { target = 0; offset = 0; symbol = 0; kind = 0; addend = 0L } in
  let relocations = Array.make relocation_count empty_reloc and next = ref 0 in
  Array.iter
    (fun (s : section) ->
      if s.kind = 4 then
        for n = 0 to (s.size / 24) - 1 do
          let at = s.offset + (n * 24) in
          let info = B.u64 data (at + 8) in
          let kind = idx "relocation type" max_int (Int64.logand info 0xffff_ffffL) in
          let symbol =
            idx "relocation symbol index" (symbol_count - 1)
              (Int64.shift_right_logical info 32)
          in
          let target = sections.(s.info) in
          let offset = idx "relocation offset" target.size (B.u64 data at) in
          if
            target.name = ".note.stapsdt" && kind <> 0
            && (kind <> 1
               || not (Hashtbl.mem (Hashtbl.find sdt_pointers target.index) offset))
          then
            fail "section .note.stapsdt: relocation must target a probe address field";
          let width =
            match relocation_width kind with
            | Some width -> width
            | None ->
                fail "section %s+0x%x: unsupported x86-64 relocation %d" target.name
                  offset kind
          in
          if width > target.size - offset then
            B.span
              ~context:(context (Printf.sprintf "relocation in %s" target.name))
              ~length:target.size offset width;
          relocations.(!next) <-
            { target = s.info; offset; symbol; kind; addend = B.u64 data (at + 16) };
          incr next
        done)
    sections;
  (* Keep emission order intact. Ordered input needs no scratch storage; for
     unordered input, only targets whose ranges need sorting get copied.
     One late text relocation must not sort all ordered debug relocations. *)
  let compare_reloc (a : relocation) (b : relocation) =
    let c = compare a.target b.target in
    if c <> 0 then c else compare a.offset b.offset
  in
  let sorted = ref true in
  for n = 1 to Array.length relocations - 1 do
    if compare_reloc relocations.(n - 1) relocations.(n) > 0 then sorted := false
  done;
  let validate ranges =
    let previous_target = ref (-1) and previous_end = ref 0 in
    Array.iter
      (fun (r : relocation) ->
        if r.kind <> 0 then begin
          if r.target = !previous_target && r.offset < !previous_end then
            fail "section %s+0x%x: overlapping relocations are unsupported"
              sections.(r.target).name r.offset;
          previous_target := r.target;
          previous_end := r.offset + Option.get (relocation_width r.kind)
        end)
      ranges
  in
  if !sorted then validate relocations
  else if Array.exists (fun (r : relocation) -> r.kind = 35) relocations then begin
    (* TLSDESC_CALL has zero width. Keep the original whole-array ordering for
       these rare inputs, including its equal-offset marker validation. *)
    let ranges = Array.copy relocations in
    Array.sort compare_reloc ranges;
    validate ranges
  end
  else begin
    let ends = Array.make count 0 and counts = Array.make count 0 in
    let pending = Array.make count false in
    Array.iter
      (fun (r : relocation) ->
        if r.kind <> 0 then begin
          counts.(r.target) <- counts.(r.target) + 1;
          if r.offset < ends.(r.target) then pending.(r.target) <- true;
          ends.(r.target) <- r.offset + Option.get (relocation_width r.kind)
        end)
      relocations;
    let ranges =
      Array.mapi
        (fun target size -> if pending.(target) then Array.make size empty_reloc else [||])
        counts
    in
    Array.fill counts 0 count 0;
    Array.iter
      (fun (r : relocation) ->
        if r.kind <> 0 && pending.(r.target) then begin
          ranges.(r.target).(counts.(r.target)) <- r;
          counts.(r.target) <- counts.(r.target) + 1
        end)
      relocations;
    (* Target order preserves the first overlapping-range diagnostic. A target
       not copied above has already proved its ranges disjoint in input order. *)
    Array.iter
      (fun ranges ->
        if Array.length ranges <> 0 then begin
          Array.stable_sort compare_reloc ranges;
          validate ranges
        end)
      ranges
  end;
  { path; data; sections; symbols; relocations; groups }

let read ?(max_size = max_file_size) path =
  File_io.read ~max_size:(min max_file_size max_size) path |> parse ~path
