(* Static ELF64 executable writer and linker for the native backend.

   Layout, with no dynamic loader and no PIE, so every address is known here:

     file 0x0000  ELF header, program headers
     file 0x1000  .text, then .rodata          PT_LOAD R+X at 0x401000
     next page    .data, then .bss             PT_LOAD R+W

   Symbols are text offsets (functions) or offsets in rodata/data/bss. All
   relocations are resolved here: RIP-relative rel32 inside .text and
   absolute 64-bit addresses inside .text, .rodata and .data. A symbol table
   is written too, so `perf`, `gdb` and `objdump` show function names. *)

type section = Text | Rodata | Data | Bss

type symbol = {
  name : string;
  section : section;
  offset : int;
  size : int;
  func : bool;
}

type input = {
  text : bytes;
  text_relocs : X64.reloc list;
  rodata : bytes;
  rodata_relocs : X64.reloc list;  (** only [Abs64] *)
  data : bytes;
  data_relocs : X64.reloc list;  (** only [Abs64] *)
  bss_size : int;
  symbols : symbol list;
  entry : string;
}

let base = 0x400000
let page = 0x1000
let align_up v a = (v + a - 1) / a * a

let set_u16 b o v =
  Bytes.set b o (Char.unsafe_chr (v land 0xff));
  Bytes.set b (o + 1) (Char.unsafe_chr ((v lsr 8) land 0xff))

let set_u32 b o v =
  set_u16 b o (v land 0xffff);
  set_u16 b (o + 2) ((v lsr 16) land 0xffff)

let set_u64 b o v =
  set_u32 b o (v land 0xffff_ffff);
  set_u32 b (o + 4) ((v lsr 32) land 0xffff_ffff)

let link (i : input) =
  let text_off = page in
  let text_addr = base + text_off in
  let rodata_off = align_up (text_off + Bytes.length i.text) 64 in
  let rodata_addr = base + rodata_off in
  let rx_end = rodata_off + Bytes.length i.rodata in
  let data_off = align_up rx_end page in
  let data_addr = base + data_off in
  let bss_addr = align_up (data_addr + Bytes.length i.data) 64 in
  let section_addr = function
    | Text -> text_addr
    | Rodata -> rodata_addr
    | Data -> data_addr
    | Bss -> bss_addr
  in
  let table = Hashtbl.create 64 in
  List.iter
    (fun (s : symbol) ->
      if Hashtbl.mem table s.name then invalid_arg ("elf64: duplicate symbol " ^ s.name);
      Hashtbl.replace table s.name (section_addr s.section + s.offset))
    i.symbols;
  let address name =
    match Hashtbl.find_opt table name with
    | Some a -> a
    | None -> invalid_arg ("elf64: undefined symbol " ^ name)
  in
  let apply bytes section_base = function
    | X64.Rel32 { offset; symbol; addend } ->
        let v = address symbol + addend - (section_base + offset + 4) in
        if v < -0x8000_0000 || v > 0x7fff_ffff then
          invalid_arg ("elf64: rel32 out of range for " ^ symbol);
        set_u32 bytes offset v
    | X64.Abs64 { offset; symbol; addend } ->
        set_u64 bytes offset (address symbol + addend)
  in
  let text = Bytes.copy i.text
  and rodata = Bytes.copy i.rodata
  and data = Bytes.copy i.data in
  List.iter (apply text text_addr) i.text_relocs;
  List.iter (apply rodata rodata_addr) i.rodata_relocs;
  List.iter (apply data data_addr) i.data_relocs;
  (* Symbol table and string tables after the loaded data *)
  let strtab = Buffer.create 256 in
  Buffer.add_char strtab '\000';
  let syms = List.filter (fun (s : symbol) -> s.func || s.size > 0) i.symbols in
  let sym_entries =
    List.map
      (fun (s : symbol) ->
        let name_off = Buffer.length strtab in
        Buffer.add_string strtab s.name;
        Buffer.add_char strtab '\000';
        (name_off, s))
      syms
  in
  let shstrtab =
    "\000.text\000.rodata\000.data\000.bss\000.symtab\000.strtab\000.shstrtab\000"
  in
  let sh_name n =
    let rec find i =
      if String.sub shstrtab i (String.length n) = n then i else find (i + 1)
    in
    find 0
  in
  let data_end = data_off + Bytes.length data in
  let symtab_off = align_up data_end 8 in
  let symtab_size = 24 * (1 + List.length sym_entries) in
  let strtab_off = symtab_off + symtab_size in
  let shstrtab_off = strtab_off + Buffer.length strtab in
  let shdr_off = align_up (shstrtab_off + String.length shstrtab) 8 in
  let nsections = 8 in
  let total = shdr_off + (64 * nsections) in
  let out = Bytes.make total '\000' in
  (* ELF header *)
  Bytes.blit_string "\x7fELF\x02\x01\x01" 0 out 0 7;
  set_u16 out 16 2 (* ET_EXEC *);
  set_u16 out 18 62 (* EM_X86_64 *);
  set_u32 out 20 1;
  set_u64 out 24 (address i.entry);
  set_u64 out 32 64 (* phoff *);
  set_u64 out 40 shdr_off;
  set_u16 out 52 64;
  set_u16 out 54 56;
  set_u16 out 56 2 (* phnum *);
  set_u16 out 58 64;
  set_u16 out 60 nsections;
  set_u16 out 62 7 (* shstrndx *);
  (* Program headers *)
  let phdr o ~flags ~offset ~addr ~filesz ~memsz =
    set_u32 out o 1 (* PT_LOAD *);
    set_u32 out (o + 4) flags;
    set_u64 out (o + 8) offset;
    set_u64 out (o + 16) addr;
    set_u64 out (o + 24) addr;
    set_u64 out (o + 32) filesz;
    set_u64 out (o + 40) memsz;
    set_u64 out (o + 48) page
  in
  phdr 64 ~flags:5 ~offset:0 ~addr:base ~filesz:rx_end ~memsz:rx_end;
  let rw_memsz = bss_addr + i.bss_size - data_addr in
  phdr 120 ~flags:6 ~offset:data_off ~addr:data_addr ~filesz:(Bytes.length data)
    ~memsz:(max rw_memsz 1);
  Bytes.blit text 0 out text_off (Bytes.length text);
  Bytes.blit rodata 0 out rodata_off (Bytes.length rodata);
  Bytes.blit data 0 out data_off (Bytes.length data);
  (* Symbols: index 0 is null *)
  let section_index = function Text -> 1 | Rodata -> 2 | Data -> 3 | Bss -> 4 in
  List.iteri
    (fun n (name_off, (s : symbol)) ->
      let o = symtab_off + (24 * (n + 1)) in
      set_u32 out o name_off;
      Bytes.set out (o + 4) (Char.chr (if s.func then 0x02 else 0x01))
      (* LOCAL, FUNC/OBJECT *);
      set_u16 out (o + 6) (section_index s.section);
      set_u64 out (o + 8) (section_addr s.section + s.offset);
      set_u64 out (o + 16) s.size)
    sym_entries;
  Bytes.blit_string (Buffer.contents strtab) 0 out strtab_off (Buffer.length strtab);
  Bytes.blit_string shstrtab 0 out shstrtab_off (String.length shstrtab);
  (* Section headers *)
  let shdr n ~name ~typ ~flags ~addr ~offset ~size ~link ~info ~align ~entsize =
    let o = shdr_off + (64 * n) in
    set_u32 out o name;
    set_u32 out (o + 4) typ;
    set_u64 out (o + 8) flags;
    set_u64 out (o + 16) addr;
    set_u64 out (o + 24) offset;
    set_u64 out (o + 32) size;
    set_u32 out (o + 40) link;
    set_u32 out (o + 44) info;
    set_u64 out (o + 48) align;
    set_u64 out (o + 56) entsize
  in
  shdr 1 ~name:(sh_name ".text") ~typ:1 ~flags:6 ~addr:text_addr ~offset:text_off
    ~size:(Bytes.length text) ~link:0 ~info:0 ~align:16 ~entsize:0;
  shdr 2 ~name:(sh_name ".rodata") ~typ:1 ~flags:2 ~addr:rodata_addr ~offset:rodata_off
    ~size:(Bytes.length rodata) ~link:0 ~info:0 ~align:64 ~entsize:0;
  shdr 3 ~name:(sh_name ".data") ~typ:1 ~flags:3 ~addr:data_addr ~offset:data_off
    ~size:(Bytes.length data) ~link:0 ~info:0 ~align:64 ~entsize:0;
  shdr 4 ~name:(sh_name ".bss") ~typ:8 ~flags:3 ~addr:bss_addr ~offset:data_end
    ~size:i.bss_size ~link:0 ~info:0 ~align:64 ~entsize:0;
  shdr 5 ~name:(sh_name ".symtab") ~typ:2 ~flags:0 ~addr:0 ~offset:symtab_off
    ~size:symtab_size ~link:6
    ~info:(1 + List.length sym_entries)
    ~align:8 ~entsize:24;
  shdr 6 ~name:(sh_name ".strtab") ~typ:3 ~flags:0 ~addr:0 ~offset:strtab_off
    ~size:(Buffer.length strtab) ~link:0 ~info:0 ~align:1 ~entsize:0;
  shdr 7 ~name:(sh_name ".shstrtab") ~typ:3 ~flags:0 ~addr:0 ~offset:shstrtab_off
    ~size:(String.length shstrtab) ~link:0 ~info:0 ~align:1 ~entsize:0;
  out
