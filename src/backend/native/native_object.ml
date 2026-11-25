(* Bridge encoder fixups to ELF RELA. Encoder Rel32 uses the address AFTER the
   four-byte field; ELF PC32 uses the address OF the field, hence addend - 4. *)
module O = Fas_linker.Elf_object

(* A section beyond the fixed ones: a named section (`section "s"` in Fir)
   or .tdata/.tbss (flags with SHF_TLS = 0x400). Symbols in it are
   [Elf64.Extra i], i its position in the [extra] list. *)
type extra = {
  name : string;
  flags : int;  (** SHF_* *)
  alignment : int;
  contents : O.contents;
  relocs : X64.reloc list;
}

(* [binding name] overrides the binding of a symbol (STB_WEAK for a weak
   definition or reference, STB_LOCAL for a local one the Ir calls external);
   [visibility name] is its STV_* value. Both default to the plain behaviour.
   [eh_frame] adds an .eh_frame section (call frame information, from the
   machine-IR backend) with its relocations; it comes last, after
   .note.GNU-stack, so the other section indices do not move. [extra]
   sections come after it. A symbol defined in an SHF_TLS section is
   STT_TLS; [tls name] marks an undefined one as STT_TLS too. [notype name]
   marks a defined label as STT_NOTYPE (a plain asm label: objtool accepts
   only that or STT_FUNC in its annotation sections). *)
let emit ?(binding = fun _ -> None) ?(visibility = fun _ -> 0) ?(tls = fun _ -> false) ?(ifunc = fun _ -> false) ?(notype = fun _ -> false) ?eh_frame
    ?(extra = []) ?(commons = []) ~global ~alignments (input : Elf64.input) =
  let first_extra = if eh_frame = None then 5 else 6 in
  let extra_flags = Array.of_list (List.map (fun (x : extra) -> x.flags) extra) in
  let section = function
    | Elf64.Text -> 0
    | Rodata -> 1
    | Data -> 2
    | Bss -> 3
    | Extra i -> first_extra + i
  in
  let tls_section = function Elf64.Extra i -> extra_flags.(i) land 0x400 <> 0 | _ -> false in
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
  (* Like GNU as and llvm-mc, a local .L label is not a symbol: a relocation
     against it goes through its section's STT_SECTION symbol with the label
     offset in the addend. objtool reads its annotation sections only in
     that form (and rejected the old STT_OBJECT labels, silently dropping
     the kernel's ORC and return-site tables). *)
  let resolved_binding name = match binding name with Some b -> b | None -> if global name then 1 else 0 in
  let labels = Hashtbl.create 16 and section_symbols = Hashtbl.create 8 in
  let section_symbol index =
    match Hashtbl.find_opt section_symbols index with
    | Some symbol -> symbol
    | None ->
        let symbol = !count in
        incr count;
        symbols :=
          { O.name = ""; binding = 0; kind = 3; visibility = 0; definition = O.Section (index, 0L); size = 0L }
          :: !symbols;
        Hashtbl.add section_symbols index symbol;
        symbol
  in
  List.iter
    (fun (s : Elf64.symbol) ->
      if String.starts_with ~prefix:".L" s.name && resolved_binding s.name = 0 && not (tls_section s.section) then
        Hashtbl.replace labels s.name (section s.section, s.offset)
      else
      ignore
        (add
           {
             name = s.name;
             binding = resolved_binding s.name;
             kind =
               (if ifunc s.name then 10
                else if s.func then 2
                else if tls_section s.section then 6
                else if notype s.name then 0
                else 1);
             visibility = visibility s.name;
             definition = O.Section (section s.section, Int64.of_int s.offset);
             size = Int64.of_int s.size;
           }))
    input.symbols;
  List.iter
    (fun (name, size, align) ->
      ignore
        (add
           {
             name;
             binding = 1;
             kind = 1;
             visibility = visibility name;
             definition = O.Common (Int64.of_int align);
             size = Int64.of_int size;
           }))
    commons;
  let reference name =
    match Hashtbl.find_opt definitions name with
    | Some index -> index
    | None ->
        add
          {
            name;
            binding = (match binding name with Some 2 -> 2 | _ -> 1);
            kind = (if tls name then 6 else 0);
            visibility = visibility name;
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
            | X64.Abs32 { offset; symbol; addend } -> (offset, symbol, 10, Int64.of_int addend)
            | X64.Abs64 { offset; symbol; addend } ->
                (offset, symbol, 1, Int64.of_int addend)
            | X64.Gotpcrel { offset; symbol; addend } ->
                (* R_X86_64_REX_GOTPCRELX; same next-byte adjustment as PC32 *)
                (offset, symbol, 42, Int64.sub (Int64.of_int addend) 4L)
            | X64.Dtpoff32 { offset; symbol; addend } -> (offset, symbol, 21, Int64.of_int addend)
            | X64.Tpoff32 { offset; symbol; addend } -> (offset, symbol, 23, Int64.of_int addend)
            | X64.Gottpoff { offset; symbol; addend } ->
                (offset, symbol, 22, Int64.sub (Int64.of_int addend) 4L)
            | X64.Tlsgd { offset; symbol; addend } ->
                (offset, symbol, 19, Int64.sub (Int64.of_int addend) 4L)
            | X64.Raw { offset; symbol; kind; addend } -> (offset, symbol, kind, Int64.of_int addend)
          in
          let symbol, addend =
            match Hashtbl.find_opt labels name with
            | Some (index, offset) -> (section_symbol index, Int64.add addend (Int64.of_int offset))
            | None -> (reference name, addend)
          in
          relocations :=
            ({ section; offset; symbol; kind; addend } : O.relocation) :: !relocations)
        rs)
    ([ input.text_relocs; input.rodata_relocs; input.data_relocs ]
    @ (* sections 3 (.bss) and 4 (.note.GNU-stack) have none *)
    [ []; [] ]
    @ (match eh_frame with Some (_, relocs) -> [ relocs ] | None -> [])
    @ List.map (fun (x : extra) -> x.relocs) extra);
  let eh_frame_section =
    match eh_frame with
    | Some (bytes, _) -> [ { O.name = ".eh_frame"; flags = 2; alignment = 8; contents = O.Data bytes } ]
    | None -> []
  in
  O.write
    {
      sections =
        (fun base extra -> Array.append base (Array.of_list extra))
        [|
          {
            O.name = ".text";
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
        |]
        @@ eh_frame_section
        @ List.map
            (fun (x : extra) ->
              { O.name = x.name; flags = x.flags; alignment = x.alignment; contents = x.contents })
            extra;
      symbols = Array.of_list (List.rev !symbols);
      relocations = Array.of_list (List.rev !relocations);
    }
