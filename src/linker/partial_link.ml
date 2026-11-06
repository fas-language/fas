(* ET_REL combination keeps section contributions separate. No addresses are
   assigned and no relocation expressions are evaluated at this stage. *)
module E = Elf_reader
module B = Binary

type section = { mutable header : E.section; mutable bytes : bytes }

let combine ?stats (resolved : Resolver.t) =
  let kept =
    Array.mapi
      (fun file (o : E.t) ->
        Array.init (Array.length o.sections) (fun n ->
            not resolved.discarded.(file).(n)))
      resolved.objects
  in
  let resolved, kept, _ = Properties.prepare ?stats ~kept resolved in
  let objects = resolved.objects in
  let maps =
    Array.map (fun (o : E.t) -> Array.make (Array.length o.sections) 0) objects
  in
  let sections = ref [] and section_count = ref 0 in
  let add (header : E.section) bytes =
    incr section_count;
    if !section_count >= 0xff00 - 4 then
      Error.fail "partial link: too many output sections";
    let section = { header = { header with index = !section_count }; bytes } in
    sections := section :: !sections;
    section
  in
  let fresh name kind flags alignment entsize =
    {
      E.index = 0;
      name;
      kind;
      flags;
      offset = 0;
      size = 0;
      link = 0;
      info = 0;
      alignment;
      entsize;
    }
  in
  let groups = ref [] in
  (* ELF requires each group header to precede all its members. *)
  Array.iteri
    (fun file (o : E.t) ->
      Array.iter
        (fun (s : E.section) ->
          if s.kind = 17 then begin
            let members =
              Array.init
                ((s.size / 4) - 1)
                (fun n -> Int64.to_int (B.u32 o.data (s.offset + 4 + (n * 4))))
            in
            if
              Array.length members = 0
              || Array.exists (fun n -> kept.(file).(n)) members
            then begin
              let output = add s (Bytes.create s.size) in
              maps.(file).(s.index) <- output.header.index;
              groups := (file, s, members, output) :: !groups
            end
          end)
        o.sections)
    objects;
  Array.iteri
    (fun file (o : E.t) ->
      Array.iter
        (fun (s : E.section) ->
          if
            kept.(file).(s.index)
            && List.mem s.kind [ 1; 7; 8; 14; 15; 16 ]
            && s.name <> ".note.GNU-stack"
            && (file < resolved.input_count || o.path <> "<common>")
          then begin
            let bytes =
              if s.kind = 8 then Bytes.empty
              else Bytes.of_string (String.sub o.data s.offset s.size)
            in
            let output = add s bytes in
            maps.(file).(s.index) <- output.header.index
          end)
        o.sections)
    objects;
  let relocations = ref [] in
  Array.iteri
    (fun file (o : E.t) ->
      Array.iter
        (fun (s : E.section) ->
          if s.kind = 4 && kept.(file).(s.index) && maps.(file).(s.info) <> 0 then begin
            let output = add s (Bytes.of_string (String.sub o.data s.offset s.size)) in
            maps.(file).(s.index) <- output.header.index;
            relocations := (file, s, output) :: !relocations
          end)
        o.sections)
    objects;
  let symbol_maps =
    Array.map (fun (o : E.t) -> Array.make (Array.length o.symbols) 0) objects
  in
  let symbols = ref [] and count = ref 0 in
  let symbol (s : E.symbol) =
    incr count;
    if !count >= 1_000_000 then Error.fail "partial link: too many output symbols";
    symbols := s :: !symbols;
    !count
  in
  let remap_symbol file (s : E.symbol) =
    if s.section = 0 || s.section >= 0xff00 then s
    else { s with section = maps.(file).(s.section) }
  in
  Array.iteri
    (fun file (o : E.t) ->
      Array.iteri
        (fun n (s : E.symbol) ->
          if
            n <> 0 && s.binding = 0
            && (s.section >= 0xff00 || (s.section <> 0 && maps.(file).(s.section) <> 0))
          then symbol_maps.(file).(n) <- symbol (remap_symbol file s))
        o.symbols)
    objects;
  let first_global = !count + 1 in
  let globals = Hashtbl.create ~random:true 64 and order = ref [] in
  Array.iter
    (fun (o : E.t) ->
      Array.iter
        (fun (s : E.symbol) ->
          if s.binding <> 0 then begin
            let name = Symver.key resolved.symver s.name in
            match Hashtbl.find_opt globals name with
            | None ->
                Hashtbl.add globals name { s with name };
                order := name :: !order
            | Some old ->
                let visibility =
                  if
                    Resolver.visibility_rank s.visibility
                    > Resolver.visibility_rank old.visibility
                  then s.visibility
                  else old.visibility
                in
                Hashtbl.replace globals name
                  {
                    old with
                    visibility;
                    kind = (if old.kind = 0 then s.kind else old.kind);
                  }
          end)
        o.symbols)
    objects;
  (* Command-line undefined roots may have no declaration in any selected
     object. Preserve them in ET_REL so a later link can still extract their
     providers, following GNU ld's partial-link policy. *)
  Hashtbl.fold
    (fun name () names ->
      let name = Symver.key resolved.symver name in
      if Hashtbl.mem globals name then names else name :: names)
    resolved.strong_undefined []
  |> List.sort_uniq String.compare
  |> List.iter (fun name ->
         Hashtbl.add globals name
           { E.name; binding = 1; kind = 0; visibility = 0; section = 0;
             value = 0L; size = 0L };
         order := name :: !order);
  let global_indices = Hashtbl.create ~random:true (Hashtbl.length globals) in
  List.iter
    (fun name ->
      let declaration : E.symbol = Hashtbl.find globals name in
      let output =
        match Hashtbl.find_opt resolved.definitions name with
        | Some d ->
            let s = remap_symbol d.file objects.(d.file).symbols.(d.symbol) in
            if s.section = 0 then
              Error.fail "partial link: definition %s lost its section" name;
            { s with name; visibility = d.visibility }
        | None ->
            {
              declaration with
              section = 0;
              value = 0L;
              binding = (if Hashtbl.mem resolved.strong_undefined name then 1 else 2);
            }
      in
      Hashtbl.add global_indices name (symbol output))
    (List.rev !order);
  Array.iteri
    (fun file (o : E.t) ->
      Array.iteri
        (fun n (s : E.symbol) ->
          if s.binding <> 0 then
            symbol_maps.(file).(n) <-
              Hashtbl.find global_indices (Symver.key resolved.symver s.name))
        o.symbols)
    objects;
  List.iter
    (fun (file, (original : E.section), output) ->
      let o = objects.(file) in
      for n = 0 to (original.E.size / 24) - 1 do
        let at = n * 24 in
        let info = B.u64 o.data (original.offset + at + 8) in
        let old = Int64.to_int (Int64.shift_right_logical info 32) in
        let mapped = symbol_maps.(file).(old) in
        if old <> 0 && mapped = 0 then begin
          if
            o.sections.(original.info).flags land 2 <> 0
            && o.sections.(original.info).name <> ".eh_frame"
          then
            Error.fail "%s: partial relocation references discarded local symbol %s"
              o.path o.symbols.(old).name;
          B.set_int output.bytes (at + 16) 8 0
        end;
        B.set output.bytes (at + 8) 8
          (Int64.logor
             (Int64.shift_left (Int64.of_int mapped) 32)
             (Int64.logand info 0xffff_ffffL))
      done;
      output.header <- { output.header with info = maps.(file).(original.info) })
    !relocations;
  List.iter
    (fun (file, (original : E.section), members, output) ->
      let flags = B.u32 objects.(file).data original.E.offset in
      B.set output.bytes 0 4 flags;
      Array.iteri
        (fun n member ->
          let mapped = maps.(file).(member) in
          if mapped = 0 then Error.fail "partial link: incomplete group membership";
          B.set_int output.bytes (4 + (n * 4)) 4 mapped)
        members;
      output.header <- { output.header with info = symbol_maps.(file).(original.info) })
    !groups;
  let significance = Buffer.create 32 and has_significance = ref false in
  let uleb value =
    let rec write value =
      Buffer.add_char significance
        (Char.chr (value land 127 lor if value > 127 then 128 else 0));
      if value > 127 then write (value lsr 7)
    in
    write value
  in
  let significant = Array.make (!count + 1) false in
  Array.iteri
    (fun file (o : E.t) ->
      let tables =
        Array.to_list o.sections
        |> List.filter (fun (s : E.section) -> s.kind = 0x6fff4c03)
      in
      if tables <> [] then has_significance := true;
      if tables = [] || List.exists (fun (s : E.section) -> s.link = 0) tables then
        Array.iter (fun mapped -> significant.(mapped) <- true) symbol_maps.(file)
      else
        List.iter
          (fun (s : E.section) ->
            let at = ref s.offset in
            while !at < s.offset + s.size do
              let value = ref 0 and shift = ref 0 and more = ref true in
              while !more do
                let byte = B.u8 o.data !at in
                incr at;
                value := !value lor ((byte land 127) lsl !shift);
                shift := !shift + 7;
                more := byte land 128 <> 0
              done;
              significant.(symbol_maps.(file).(!value)) <- true
            done)
          tables)
    objects;
  Array.iteri (fun index marked -> if index <> 0 && marked then uleb index) significant;
  let significance_section =
    if not !has_significance then None
    else
      let bytes = Bytes.of_string (Buffer.contents significance) in
      Some
        (add
           {
             (fresh ".llvm_addrsig" 0x6fff4c03 0x80000000 1 0) with
             size = Bytes.length bytes;
           }
           bytes)
  in
  ignore (add (fresh ".note.GNU-stack" 1 0 1 0) Bytes.empty);
  let strings = Buffer.create 256 in
  Buffer.add_char strings '\000';
  let string buffer name =
    let offset = Buffer.length buffer in
    ignore
      (B.add ~context:"partial string storage" ~limit:E.max_file_size offset
         (String.length name + 1));
    Buffer.add_string buffer name;
    Buffer.add_char buffer '\000';
    offset
  in
  let symbol_data = Bytes.make ((!count + 1) * 24) '\000' in
  List.iteri
    (fun n (s : E.symbol) ->
      let at = (n + 1) * 24 in
      B.set_int symbol_data at 4 (string strings s.name);
      B.set_int symbol_data (at + 4) 1 ((s.binding lsl 4) lor s.kind);
      B.set_int symbol_data (at + 5) 1 s.visibility;
      B.set_int symbol_data (at + 6) 2 s.section;
      B.set symbol_data (at + 8) 8 s.value;
      B.set symbol_data (at + 16) 8 s.size)
    (List.rev !symbols);
  let symtab =
    add
      {
        (fresh ".symtab" 2 0 8 24) with
        size = Bytes.length symbol_data;
        info = first_global;
      }
      symbol_data
  in
  let strtab_data = Bytes.of_string (Buffer.contents strings) in
  let strtab =
    add { (fresh ".strtab" 3 0 1 0) with size = Bytes.length strtab_data } strtab_data
  in
  symtab.header <- { symtab.header with link = strtab.header.index };
  List.iter
    (fun (_, _, s) -> s.header <- { s.header with link = symtab.header.index })
    !relocations;
  List.iter
    (fun (_, _, _, s) -> s.header <- { s.header with link = symtab.header.index })
    !groups;
  Option.iter
    (fun s -> s.header <- { s.header with link = symtab.header.index })
    significance_section;
  let shstrings = Buffer.create 128 in
  Buffer.add_char shstrings '\000';
  let shstrtab = add (fresh ".shstrtab" 3 0 1 0) Bytes.empty in
  let sections = List.rev !sections |> Array.of_list in
  let names = Array.map (fun s -> string shstrings s.header.name) sections in
  shstrtab.bytes <- Bytes.of_string (Buffer.contents shstrings);
  shstrtab.header <- { shstrtab.header with size = Bytes.length shstrtab.bytes };
  let limit = E.max_file_size in
  let add = B.add ~context:"partial output size" ~limit in
  let align = B.align ~context:"partial output layout" ~limit in
  let cursor = ref 64 in
  Array.iter
    (fun s ->
      cursor := align !cursor (max 1 s.header.alignment);
      s.header <- { s.header with offset = !cursor };
      if s.header.kind <> 8 then cursor := add !cursor s.header.size)
    sections;
  let shoff = align !cursor 8 in
  let output = Bytes.make (add shoff ((!section_count + 1) * 64)) '\000' in
  Bytes.blit_string "\127ELF\002\001\001" 0 output 0 7;
  B.set_int output 16 2 1;
  B.set_int output 18 2 62;
  B.set_int output 20 4 1;
  B.set_int output 40 8 shoff;
  B.set_int output 52 2 64;
  B.set_int output 58 2 64;
  B.set_int output 60 2 (!section_count + 1);
  B.set_int output 62 2 shstrtab.header.index;
  Array.iteri
    (fun n s ->
      let h = s.header in
      if h.kind <> 8 then Bytes.blit s.bytes 0 output h.offset h.size;
      let at = shoff + (h.index * 64) in
      List.iter
        (fun (field, width, value) -> B.set_int output (at + field) width value)
        [
          (0, 4, names.(n));
          (4, 4, h.kind);
          (8, 8, h.flags);
          (24, 8, h.offset);
          (32, 8, h.size);
          (40, 4, h.link);
          (44, 4, h.info);
          (48, 8, h.alignment);
          (56, 8, h.entsize);
        ])
    sections;
  ignore (E.parse ~path:"<partial output>" (Bytes.unsafe_to_string output));
  Stats.add stats "output_bytes" (Bytes.length output);
  output

let link_inputs ?stats ?(options = Options.default) ?(on_input = fun _ -> ()) inputs =
  if
    options.mode <> Options.Static
    || options.gc_sections
    || options.version_script <> None
    || options.build_id <> Options.No_id
    || (not options.allow_shlib_undefined)
    || options.rpath_link <> [] || options.runpath <> None || options.soname <> None
    || options.export_dynamic
  then Error.fail "partial link requires relocatable output policy";
  Error.io (fun () ->
      let saw_file = ref false in
      let resolved =
        Resolver.resolve ?stats ~partial:true ~options ~entry:"_start"
          ~on_input:(fun path -> saw_file := true; on_input path) inputs
      in
      if resolved.input_count = 0 && not !saw_file then
        Error.fail "partial link: no input files";
      Stats.measure stats "emit_relocatable" (fun () -> combine ?stats resolved))

let link_input_files ?stats ?options ~output inputs =
  File_io.publish ?stats ~output ~permissions:0o644
    ~inputs:(List.concat_map Input.paths inputs) (fun () ->
      let expanded = ref [] in
      let image =
        link_inputs ?stats ?options
          ~on_input:(fun path -> expanded := path :: !expanded)
          inputs
      in
      File_io.check_output ~output ~inputs:!expanded;
      image)
