(* Bridge encoder fixups to ELF RELA. Encoder Rel32 uses the address AFTER the
   four-byte field; ELF PC32 uses the address OF the field, hence addend - 4. *)
module O = Fas_linker.Elf_object

let section = function Elf64.Text -> 0 | Rodata -> 1 | Data -> 2 | Bss -> 3

let emit ~global ~alignments (input : Elf64.input) =
  let definitions = Hashtbl.create ~random:true 64 in
  let symbols = ref [] and count = ref 0 in
  let add (symbol : O.symbol) =
    if Hashtbl.mem definitions symbol.name then
      Fas_linker.Error.fail "native object: duplicate symbol %s" symbol.name;
    let index = !count in
    incr count;
    Hashtbl.add definitions symbol.name index;
    symbols := symbol :: !symbols;
    index
  in
  List.iter
    (fun (s : Elf64.symbol) ->
      ignore
        (add
           {
             name = s.name;
             binding = (if global s.name then 1 else 0);
             kind = (if s.func then 2 else 1);
             visibility = 0;
             definition = O.Section (section s.section, Int64.of_int s.offset);
             size = Int64.of_int s.size;
           }))
    input.symbols;
  let reference name =
    match Hashtbl.find_opt definitions name with
    | Some index -> index
    | None ->
        add
          {
            name;
            binding = 1;
            kind = 0;
            visibility = 0;
            definition = O.Undefined;
            size = 0L;
          }
  in
  let relocations = ref [] in
  List.iteri
    (fun section rs ->
      List.iter
        (fun relocation ->
          let offset, name, kind, addend =
            match relocation with
            | X64.Rel32 { offset; symbol; addend } ->
                (offset, symbol, 2, Int64.sub (Int64.of_int addend) 4L)
            | X64.Call32 { offset; symbol; addend } ->
                (offset, symbol, 4, Int64.sub (Int64.of_int addend) 4L)
            | X64.Abs32s { offset; symbol; addend } ->
                (offset, symbol, 11, Int64.of_int addend)
            | X64.Abs64 { offset; symbol; addend } ->
                (offset, symbol, 1, Int64.of_int addend)
            | X64.Gotpcrel { offset; symbol; addend } ->
                (* R_X86_64_REX_GOTPCRELX; same next-byte adjustment as PC32 *)
                (offset, symbol, 42, Int64.sub (Int64.of_int addend) 4L)
          in
          let symbol = reference name in
          relocations :=
            ({ section; offset; symbol; kind; addend } : O.relocation) :: !relocations)
        rs)
    [ input.text_relocs; input.rodata_relocs; input.data_relocs ];
  O.write
    {
      sections =
        [|
          {
            name = ".text";
            flags = 6;
            alignment = alignments.(0);
            contents = O.Data input.text;
          };
          {
            name = ".rodata";
            flags = 2;
            alignment = alignments.(1);
            contents = O.Data input.rodata;
          };
          {
            name = ".data";
            flags = 3;
            alignment = alignments.(2);
            contents = O.Data input.data;
          };
          {
            name = ".bss";
            flags = 3;
            alignment = alignments.(3);
            contents = O.Bss input.bss_size;
          };
          {
            name = ".note.GNU-stack";
            flags = 0;
            alignment = 1;
            contents = O.Data Bytes.empty;
          };
        |];
      symbols = Array.of_list (List.rev !symbols);
      relocations = Array.of_list (List.rev !relocations);
    }
