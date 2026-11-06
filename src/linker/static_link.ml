open Layout

let link_objects ?stats ?map ~(options : Options.t) ~entry (resolved : Resolver.t) =
  Array.iteri
    (fun file (object_ : E.t) ->
      Array.iter
        (fun (section : E.section) ->
          if
            section.size <> 0
            && section.flags land 2 <> 0
            && (not resolved.discarded.(file).(section.index))
            && List.exists
                 (fun name ->
                   section.name = name
                   || String.starts_with ~prefix:(name ^ ".") section.name)
                 [ ".ctors"; ".dtors" ]
          then
            Error.fail
              "%s: legacy lifecycle section %s is unsupported in final output; use \
               .init_array/.fini_array"
              object_.path section.name)
        object_.sections)
    resolved.objects;
  let resolved, module_base = Tls.prepare resolved in
  let resolved = Shared_output.prepare ~options resolved in
  Shared_dependencies.validate resolved.dependencies ~defined:(fun name version ->
      let find name =
        match Hashtbl.find_opt resolved.definitions name with
        | Some (d : Resolver.resolved) when d.visibility = 0 || d.visibility = 3 ->
            Some resolved.objects.(d.file).symbols.(d.symbol).kind
        | _ -> None
      in
      match version with
      | None -> find name
      | Some version -> (
          match find (name ^ "@" ^ version) with
          | Some _ as kind -> kind
          | None ->
              if Symver.version resolved.symver name = None then find name else None));
  let resolved, frames =
    Stats.measure stats "unwind" (fun () -> Unwind.normalize ?stats resolved)
  in
  let kept =
    Stats.measure stats "gc" (fun () ->
        Section_gc.select ?stats ~options ~entry ~frames resolved)
  in
  let resolved, kept, copies = Copy_relocation.prepare ?stats ~options ~kept resolved in
  Array.iteri
    (fun file (object_ : E.t) ->
      Array.iter
        (fun (relocation : E.relocation) ->
          if
            relocation.kind <> 0
            && kept.(file).(relocation.target)
            && object_.sections.(relocation.target).flags land 2 <> 0
          then begin
            let symbol = object_.symbols.(relocation.symbol) in
            let definition =
              if symbol.binding = 0 then Some (file, symbol)
              else
                Option.map
                  (fun (d : Resolver.resolved) ->
                    (d.file, resolved.objects.(d.file).symbols.(d.symbol)))
                  (Hashtbl.find_opt resolved.definitions symbol.name)
            in
            match definition with
            | Some (source, symbol)
              when symbol.section <> 0 && symbol.section < 0xff00
                   && resolved.objects.(source).sections.(symbol.section).flags land 2
                      = 0 ->
                Error.fail
                  "%s: allocatable section %s references non-allocatable symbol %s"
                  object_.path object_.sections.(relocation.target).name symbol.name
            | _ -> ()
          end)
        object_.relocations)
    resolved.objects;
  Tls.validate ~options ~kept resolved;
  let resolved =
    if options.mode = Options.Static_pie then Relax.static_pie ?stats ~kept resolved
    else resolved
  in
  let resolved, kept, property_file = Properties.prepare ?stats ~kept resolved in
  let resolved, kept, build_id_file = Build_id.prepare ~options ~kept resolved in
  let resolved, kept, got =
    Got.prepare ?stats ~static_ifunc:(options.mode = Static)
      ~canonical_functions:
        (match options.mode with Dynamic { pie = false; _ } -> true | _ -> false)
      ~kept resolved
  in
  let resolved, kept, unwind = Unwind.prepare ?stats ~kept ~frames resolved in
  let resolved, kept, static_ifunc =
    Static_ifunc.prepare ?stats ~options ~kept resolved
  in
  let resolved, kept, dynamic =
    Dynamic.prepare ?stats ~options ~kept ~got ~module_base ~copies resolved
  in
  let base = match dynamic with Some d when d.Dynamic.pie -> 0 | _ -> Layout.base in
  let objects = resolved.objects in
  if Array.length objects = 0 then Error.fail "no input objects";
  let aliases =
    Stats.measure stats "merge" (fun () -> Merge_sections.plan ?stats ~kept objects)
  in
  if entry = "" then Error.fail "entry symbol must not be empty";
  let add = B.add ~context:"output layout" ~limit:max_output in
  let align value alignment =
    B.align ~context:"output layout" ~limit:max_output value alignment
  in
  let layout =
    Layout.make ~base ~relro:(dynamic <> None && options.relro) ~kept ~aliases objects
  in
  let placements = layout.placements and content = layout.content in
  let content_count = ref (List.length layout.sections) in
  let cursor = ref layout.cursor and segments = ref (List.rev layout.segments) in
  Option.iter
    (fun file ->
      let registry_sections = Hashtbl.create ~random:true 16 in
      List.iter
        (fun (section : Layout.section) ->
          if Section_registry.identifier section.name then
            Hashtbl.replace registry_sections section.name section)
        layout.sections;
      let ending predicate memory =
        List.fold_left
          (fun value s ->
            if predicate s then
              max value (base + s.offset + if memory then s.memory_size else s.file_size)
            else value)
          (base + page) !segments
      in
      let data_end = ending (fun _ -> true) false in
      let image_end = ending (fun _ -> true) true in
      let text_end = ending (fun s -> s.flags land 1 <> 0) true in
      let array_boundary kind last =
        let value = ref None in
        Array.iteri
          (fun source (o : E.t) ->
            Array.iter
              (fun (section : E.section) ->
                if section.kind = kind then
                  match placements.(source).(section.index) with
                  | None -> ()
                  | Some p ->
                      let address = p.address + if last then section.size else 0 in
                      value :=
                        Some
                          (match !value with
                          | None -> address
                          | Some previous ->
                              if last then max previous address
                              else min previous address))
              o.sections)
          objects;
        Option.value !value ~default:0
      in
      Array.iteri
        (fun index (s : E.symbol) ->
          if index <> 0 then begin
            let value =
              match s.name with
              | "__executable_start" | "__ehdr_start" -> base
              | "__rela_iplt_start" ->
                  Static_ifunc.boundary ~placements static_ifunc false
              | "__rela_iplt_end" -> Static_ifunc.boundary ~placements static_ifunc true
              | "__bss_start" | "_edata" | "edata" -> data_end
              | "_etext" | "etext" -> text_end
              | "_end" | "end" -> image_end
              | "__preinit_array_start" -> array_boundary 16 false
              | "__preinit_array_end" -> array_boundary 16 true
              | "__init_array_start" -> array_boundary 14 false
              | "__init_array_end" -> array_boundary 14 true
              | "__fini_array_start" -> array_boundary 15 false
              | "__fini_array_end" -> array_boundary 15 true
              | "_GLOBAL_OFFSET_TABLE_" ->
                  (Option.get placements.(Option.get got.Got.file).(1)).address
              | "_DYNAMIC" ->
                  (Dynamic.placement ~placements (Option.get dynamic) Dynamic.dynamic)
                    .address
              | name -> (
                  match Section_registry.boundary name with
                  | None -> assert false
                  | Some (name, last) -> (
                      match Hashtbl.find_opt registry_sections name with
                      | Some section ->
                          section.address + if last then section.size else 0
                      (* A boundary used only by discarded code does not retain
                         its section, and has no live relocation to resolve. *)
                      | None -> 0))
            in
            objects.(file).symbols.(index) <- { s with value = Int64.of_int value }
          end)
        objects.(file).symbols)
    resolved.predefined;
  let get_placement file index =
    match placements.(file).(index) with
    | Some p -> p
    | None ->
        Error.fail "%s: symbol references a section excluded from output"
          objects.(file).path
  in
  let definitions = resolved.definitions in
  let live file (s : E.symbol) =
    s.section = 0xfff1
    || s.section <> 0 && s.section < 0xff00
       && List.mem objects.(file).sections.(s.section).kind [ 1; 7; 8; 14; 15; 16 ]
       && kept.(file).(s.section)
       && placements.(file).(s.section) <> None
  in
  let address file (s : E.symbol) =
    let module_base_symbol =
      match module_base with
      | Some (source, index) -> file = source && s == objects.(source).symbols.(index)
      | None -> false
    in
    if module_base_symbol then 0L
    else if s.section = 0xfff1 then s.value
    else
      let adjustment =
        if s.kind = 6 then base + (Option.get layout.tls).offset else 0
      in
      Int64.add
        (Int64.of_int ((get_placement file s.section).address - adjustment))
        s.value
  in
  let values =
    Array.mapi
      (fun file (o : E.t) ->
        Array.mapi
          (fun index (s : E.symbol) ->
            if index = 0 then Some 0L
            else if s.binding = 0 then
              if live file s then Some (address file s) else None
            else
              match Hashtbl.find_opt definitions s.name with
              | Some (definition : Resolver.resolved) ->
                  let symbol = objects.(definition.file).symbols.(definition.symbol) in
                  if live definition.file symbol then
                    Some (address definition.file symbol)
                  else None
              | None ->
                  if dynamic <> None && Hashtbl.mem resolved.imports s.name then Some 0L
                  else if
                    s.binding = 2 && not (Hashtbl.mem resolved.strong_undefined s.name)
                  then Some 0L
                  else None)
          o.symbols)
      objects
  in
  let entry_address =
    if options.mode = Options.Shared then 0L
    else
      match Hashtbl.find_opt definitions entry with
      | None -> Error.fail "undefined entry symbol %s" entry
      | Some (definition : Resolver.resolved) ->
          let file = definition.file in
          let symbol = objects.(file).symbols.(definition.symbol) in
          if symbol.kind = 10 then Error.fail "entry symbol %s cannot be an IFUNC" entry;
          if symbol.section = 0xfff1 then
            Error.fail "entry symbol %s must be defined in executable code" entry;
          let section = objects.(file).sections.(symbol.section) in
          if section.flags land 6 <> 6 || symbol.value >= Int64.of_int section.size then
            Error.fail "entry symbol %s must be inside an executable section" entry;
          address file symbol
  in
  let string_buffer () =
    let b = Buffer.create 256 in
    Buffer.add_char b '\000';
    b
  in
  let add_string buffer string =
    let offset = Buffer.length buffer in
    ignore (add offset (String.length string + 1));
    Buffer.add_string buffer string;
    Buffer.add_char buffer '\000';
    offset
  in
  let strtab = string_buffer () and shstrtab = string_buffer () in
  let locals = ref [] and globals = ref [] in
  Array.iteri
    (fun file (o : E.t) ->
      Array.iteri
        (fun index (s : E.symbol) ->
          let canonical =
            s.binding = 0
            ||
            match Hashtbl.find_opt definitions s.name with
            | Some (d : Resolver.resolved) -> d.file = file && d.symbol = index
            | None -> false
          in
          if index <> 0 && live file s && canonical then begin
            let name = add_string strtab s.name in
            let entry = (file, s, name) in
            if s.binding = 0 then locals := entry :: !locals
            else globals := entry :: !globals
          end)
        o.symbols)
    objects;
  let locals = List.rev !locals and globals = List.rev !globals in
  let symbols = locals @ globals in
  let symbol_count = List.length symbols + 1 in
  let symtab_size =
    B.add ~context:"output symbol count" ~limit:(max_output / 24) 0 symbol_count * 24
  in
  let content_names =
    List.map (fun (s : Layout.section) -> add_string shstrtab s.name) layout.sections
  in
  let symtab_name = add_string shstrtab ".symtab" in
  let strtab_name = add_string shstrtab ".strtab" in
  let shstrtab_name = add_string shstrtab ".shstrtab" in
  let symtab_offset = align !cursor 8 in
  let strtab_offset = add symtab_offset symtab_size in
  let shstrtab_offset = add strtab_offset (Buffer.length strtab) in
  let shoff = align (add shstrtab_offset (Buffer.length shstrtab)) 8 in
  let section_count = !content_count + 4 in
  let size = add shoff (section_count * 64) in
  let out = Bytes.make size '\000' in
  List.iter
    (fun (s : Layout.section) ->
      if s.flags land 4 <> 0 && (s.name = ".init" || s.name = ".fini") then
        Bytes.fill out s.offset s.size '\144')
    layout.sections;
  List.iter
    (fun s ->
      if s.section.kind <> 8 then
        Bytes.blit_string objects.(s.file).data s.section.offset out s.placement.offset
          s.section.size)
    content;
  Option.iter
    (fun file ->
      let placement = get_placement file 1 in
      Array.iteri
        (fun slot (source_file, symbol) ->
          let value =
            match Static_ifunc.stub ~placements static_ifunc source_file symbol with
            | Some address -> Int64.of_int address
            | None -> (
                match got.models.(slot) with
                | Got.Local_module -> 1L
                | Got.Local_offset -> 0L
                | Got.Descriptor | Got.Descriptor_argument -> 0L
                | Got.Address
                  when objects.(source_file).symbols.(symbol).binding <> 0
                       && Hashtbl.mem got.canonical_imports
                            (Symver.key resolved.symver
                               objects.(source_file).symbols.(symbol).name) ->
                    Int64.of_int
                      (Option.get
                         (Option.bind dynamic (fun d ->
                              Dynamic.canonical_stub ~placements ~resolved d source_file
                                symbol)))
                | Got.Address when Got.canonical_ifunc resolved got source_file symbol
                  -> (
                    match
                      Option.bind dynamic (fun d ->
                          Dynamic.ifunc_stub ~placements ~resolved d source_file symbol)
                    with
                    | Some address -> Int64.of_int address
                    | None -> Error.fail "missing canonical IFUNC stub")
                | model -> (
                    match values.(source_file).(symbol) with
                    | Some _
                      when Hashtbl.mem resolved.imports
                             objects.(source_file).symbols.(symbol).name
                           && model <> Got.Address ->
                        0L
                    | Some value
                      when model = Got.Thread_offset && options.mode <> Shared ->
                        Tls.thread_offset layout.tls value
                    | Some _ when model = Got.Module_id -> 1L
                    | Some value -> value
                    | None ->
                        Error.fail "%s: GOT references undefined symbol %s"
                          objects.(source_file).path
                          objects.(source_file).symbols.(symbol).name))
          in
          B.set out (placement.offset + (slot * 8)) 8 value)
        got.slots)
    got.file;
  Stats.measure stats "relocate" (fun () ->
      Array.iteri
        (fun file (o : E.t) ->
          Array.iteri
            (fun index (r : E.relocation) ->
              if r.kind <> 0 && r.kind <> 35 && kept.(file).(r.target) then begin
                let target = get_placement file r.target in
                let symbol = o.symbols.(r.symbol) in
                let context () =
                  Printf.sprintf "%s: %s+0x%x referencing %s" o.path
                    o.sections.(r.target).name r.offset
                    (if symbol.name = "" then Printf.sprintf "symbol #%d" r.symbol
                     else symbol.name)
                in
                let runtime_stub =
                  let static_stub =
                    if
                      o.sections.(r.target).name = ".eh_frame"
                      || Got.relocation r.kind || Got.base_relocation r.kind
                    then None
                    else Static_ifunc.stub ~placements static_ifunc file r.symbol
                  in
                  if static_stub <> None then static_stub
                  else if
                    symbol.binding <> 0
                    && (not (Got.relocation r.kind || Got.base_relocation r.kind))
                    && Hashtbl.mem got.canonical_imports
                         (Symver.key resolved.symver symbol.name)
                  then
                    Option.bind dynamic (fun d ->
                        Dynamic.canonical_stub ~placements ~resolved d file r.symbol)
                  else if
                    (r.kind = 4 || r.kind = 2) && got.references.(file).(index) >= 0
                  then
                    Option.bind dynamic (fun d ->
                        Dynamic.stub ~placements d got.references.(file).(index))
                  else if r.kind = 1 && Got.canonical_ifunc resolved got file r.symbol
                  then
                    Option.bind dynamic (fun d ->
                        Dynamic.ifunc_stub ~placements ~resolved d file r.symbol)
                  else None
                in
                let value =
                  match runtime_stub with
                  | Some address -> Int64.of_int address
                  | None -> (
                      if Got.base_relocation r.kind then
                        Int64.of_int (get_placement (Option.get got.file) 1).address
                      else if Got.relocation r.kind then
                        Int64.of_int
                          ((get_placement (Option.get got.file) 1).address
                          + (got.references.(file).(index) * 8))
                      else
                        match values.(file).(r.symbol) with
                        | Some value -> value
                        | None
                          when (symbol.section <> 0
                               || Hashtbl.mem definitions symbol.name)
                               && (o.sections.(r.target).flags land 2 = 0
                                  || o.sections.(r.target).name = ".eh_frame") ->
                            0L
                        | None ->
                            Error.fail "%s: undefined symbol %s" (context ())
                              symbol.name)
                in
                let value =
                  if r.kind = 23 then Tls.thread_offset layout.tls value else value
                in
                let addend =
                  if
                    (o.sections.(r.target).name = ".eh_frame"
                    || o.sections.(r.target).name = ".note.stapsdt")
                    && values.(file).(r.symbol) = None
                  then 0L
                  else r.addend
                in
                X86_64.apply_lazy ~context out ~offset:(target.offset + r.offset)
                  ~kind:r.kind ~symbol:value ~addend
                  ~place:(Int64.of_int (target.address + r.offset))
              end)
            o.relocations)
        objects);
  Unwind.emit ~out ~placements ~objects unwind;
  Static_ifunc.emit ~out ~placements ~values static_ifunc;
  Option.iter
    (Dynamic.emit ~out ~placements ~sections:layout.sections ~objects ~base
       ~tls:layout.tls)
    dynamic;
  Bytes.blit_string "\127ELF\002\001\001" 0 out 0 7;
  B.set_int out 16 2 (match dynamic with Some d when d.Dynamic.pie -> 3 | _ -> 2);
  B.set_int out 18 2 62;
  B.set_int out 20 4 1;
  B.set out 24 8 entry_address;
  B.set_int out 32 8 64;
  B.set_int out 40 8 shoff;
  B.set_int out 52 2 64;
  B.set_int out 54 2 56;
  let segments = List.rev !segments in
  let phnum =
    List.length segments + 2
    + (if unwind.header_file = None then 0 else 1)
    + (if property_file = None then 0 else 2)
    + (if build_id_file = None then 0 else 1)
    + (if layout.tls = None then 0 else 1)
    + (if layout.relro = None then 0 else 1)
    +
    if dynamic = None then 0
    else if options.mode = Options.Shared then 1
    else if options.mode = Options.Static_pie then 2
    else 3
  in
  let header_size = 64 + (phnum * 56) in
  B.set_int out 56 2 phnum;
  B.set_int out 58 2 64;
  B.set_int out 60 2 section_count;
  B.set_int out 62 2 (!content_count + 3);
  let program_header index ~kind ~flags ~offset ~address ~file_size ~memory_size
      ~alignment =
    let at = 64 + (index * 56) in
    B.set_int out at 4 kind;
    B.set_int out (at + 4) 4 flags;
    B.set_int out (at + 8) 8 offset;
    B.set_int out (at + 16) 8 address;
    B.set_int out (at + 24) 8 address;
    B.set_int out (at + 32) 8 file_size;
    B.set_int out (at + 40) 8 memory_size;
    B.set_int out (at + 48) 8 alignment
  in
  let prefix =
    if dynamic = None || options.mode = Options.Shared then 0
    else if options.mode = Options.Static_pie then 1
    else 2
  in
  Option.iter
    (fun d ->
      if options.mode <> Options.Shared then begin
        program_header 0 ~kind:6 ~flags:4 ~offset:64 ~address:(base + 64)
          ~file_size:(phnum * 56) ~memory_size:(phnum * 56) ~alignment:8;
        if options.mode <> Options.Static_pie then begin
          let p = Dynamic.placement ~placements d Dynamic.interp in
          let size = objects.(d.file).sections.(Dynamic.interp).size in
          program_header 1 ~kind:3 ~flags:4 ~offset:p.offset ~address:p.address
            ~file_size:size ~memory_size:size ~alignment:1
        end
      end)
    dynamic;
  program_header prefix ~kind:1 ~flags:4 ~offset:0 ~address:base ~file_size:header_size
    ~memory_size:header_size ~alignment:page;
  List.iteri
    (fun index s ->
      program_header
        (prefix + index + 1)
        ~kind:1 ~flags:s.flags ~offset:s.offset ~address:(base + s.offset)
        ~file_size:s.file_size ~memory_size:s.memory_size ~alignment:s.alignment)
    segments;
  let extra_header = ref (prefix + List.length segments + 1) in
  Option.iter
    (fun (relro : Layout.segment) ->
      program_header !extra_header ~kind:0x6474e552 ~flags:4 ~offset:relro.offset
        ~address:(base + relro.offset) ~file_size:relro.file_size
        ~memory_size:relro.memory_size ~alignment:1;
      incr extra_header)
    layout.relro;
  Option.iter
    (fun (tls : Layout.segment) ->
      program_header !extra_header ~kind:7 ~flags:4 ~offset:tls.offset
        ~address:(base + tls.offset) ~file_size:tls.file_size
        ~memory_size:tls.memory_size ~alignment:tls.alignment;
      incr extra_header)
    layout.tls;
  Option.iter
    (fun d ->
      let p = Dynamic.placement ~placements d Dynamic.dynamic in
      let size = objects.(d.file).sections.(Dynamic.dynamic).size in
      program_header !extra_header ~kind:2 ~flags:6 ~offset:p.offset ~address:p.address
        ~file_size:size ~memory_size:size ~alignment:8;
      incr extra_header)
    dynamic;
  Option.iter
    (fun file ->
      let p = get_placement file 1 in
      let size = objects.(file).sections.(1).size in
      program_header !extra_header ~kind:0x6474e550 ~flags:4 ~offset:p.offset
        ~address:p.address ~file_size:size ~memory_size:size ~alignment:4;
      incr extra_header)
    unwind.header_file;
  Option.iter
    (fun file ->
      let p = get_placement file 1 in
      let size = objects.(file).sections.(1).size in
      List.iter
        (fun kind ->
          program_header !extra_header ~kind ~flags:4 ~offset:p.offset
            ~address:p.address ~file_size:size ~memory_size:size ~alignment:8;
          incr extra_header)
        [ 4; 0x6474e553 ])
    property_file;
  program_header (phnum - 1) ~kind:0x6474e551 ~flags:6 ~offset:0 ~address:0 ~file_size:0
    ~memory_size:0 ~alignment:16;
  Option.iter
    (fun file ->
      let p = get_placement file 1 in
      let size = objects.(file).sections.(1).size in
      program_header !extra_header ~kind:4 ~flags:4 ~offset:p.offset ~address:p.address
        ~file_size:size ~memory_size:size ~alignment:4;
      incr extra_header)
    build_id_file;
  List.iteri
    (fun index (file, (s : E.symbol), name) ->
      let at = symtab_offset + ((index + 1) * 24) in
      B.set_int out at 4 name;
      B.set_int out (at + 4) 1 ((s.binding lsl 4) lor s.kind);
      let visibility =
        if s.binding = 0 then s.visibility
        else (Hashtbl.find definitions s.name).Resolver.visibility
      in
      B.set_int out (at + 5) 1 visibility;
      B.set_int out (at + 6) 2
        (if s.section = 0xfff1 then 0xfff1
         else (get_placement file s.section).output_index);
      B.set out (at + 8) 8 (address file s);
      B.set out (at + 16) 8 s.size)
    symbols;
  Bytes.blit_string (Buffer.contents strtab) 0 out strtab_offset (Buffer.length strtab);
  Bytes.blit_string (Buffer.contents shstrtab) 0 out shstrtab_offset
    (Buffer.length shstrtab);
  let section_header index ~name ~kind ~flags ~address ~offset ~size ~link ~info
      ~alignment ~entsize =
    let at = shoff + (index * 64) in
    List.iter
      (fun (o, w, v) -> B.set_int out (at + o) w v)
      [
        (0, 4, name);
        (4, 4, kind);
        (8, 8, flags);
        (16, 8, address);
        (24, 8, offset);
        (32, 8, size);
        (40, 4, link);
        (44, 4, info);
        (48, 8, alignment);
        (56, 8, entsize);
      ]
  in
  List.iter2
    (fun (s : Layout.section) name ->
      let link, info =
        match Option.bind dynamic (fun d -> Dynamic.metadata ~placements d s.index) with
        | Some metadata -> metadata
        | None -> (0, 0)
      in
      section_header s.index ~name ~kind:s.kind ~flags:s.flags ~address:s.address
        ~offset:s.offset ~size:s.size ~link ~info ~alignment:s.alignment
        ~entsize:s.entsize)
    layout.sections content_names;
  section_header (!content_count + 1) ~name:symtab_name ~kind:2 ~flags:0 ~address:0
    ~offset:symtab_offset ~size:symtab_size ~link:(!content_count + 2)
    ~info:(List.length locals + 1)
    ~alignment:8 ~entsize:24;
  section_header (!content_count + 2) ~name:strtab_name ~kind:3 ~flags:0 ~address:0
    ~offset:strtab_offset ~size:(Buffer.length strtab) ~link:0 ~info:0 ~alignment:1
    ~entsize:0;
  section_header (!content_count + 3) ~name:shstrtab_name ~kind:3 ~flags:0 ~address:0
    ~offset:shstrtab_offset ~size:(Buffer.length shstrtab) ~link:0 ~info:0 ~alignment:1
    ~entsize:0;
  Option.iter
    (fun buffer ->
      let line format =
        Printf.ksprintf
          (fun text ->
            if Buffer.length buffer + String.length text > 128 * 1024 * 1024 then
              Error.fail "map output exceeds 128 MiB limit";
            Buffer.add_string buffer text)
          format
      in
      line "fas-ld map v1\nentry %016Lx %S\n" entry_address entry;
      line
        "# section: address file-offset file-size memory-size alignment flags name input\n";
      List.iter
        (fun s ->
          line "section %016x %016x %016x %016x %x %x %S %S\n" s.placement.address
            s.placement.offset
            (if s.section.kind = 8 then 0 else s.section.size)
            s.section.size s.section.alignment
            (s.section.flags land lnot 0x200)
            s.section.name objects.(s.file).path)
        content;
      line "# symbol: address size binding name input\n";
      List.iter
        (fun (file, (s : E.symbol), _) ->
          line "symbol %016Lx %016Lx %s %S %S\n" (address file s) s.size
            (if s.binding = 0 then "local"
             else if s.binding = 2 then "weak"
             else "global")
            s.name objects.(file).path)
        symbols)
    map;
  Stats.add stats "output_bytes" size;
  Option.iter
    (fun file ->
      Build_id.emit ~style:options.build_id ~offset:(get_placement file 1).offset out)
    build_id_file;
  out

let build_inputs ?stats ?map ?on_input ?(options = Options.default) ?(entry = "_start")
    inputs =
  Error.io (fun () ->
      let resolved = Resolver.resolve ?stats ?on_input ~options ~entry inputs in
      Stats.measure stats "emit" (fun () ->
          link_objects ?stats ?map ~options ~entry resolved))

let link_inputs ?stats ?options ?entry inputs =
  build_inputs ?stats ?options ?entry inputs

let link_inputs_with_map ?stats ?options ?entry inputs =
  let map = Buffer.create 1024 in
  let image = build_inputs ?stats ~map ?options ?entry inputs in
  (image, Buffer.contents map)

let link ?stats ?options ?entry paths =
  link_inputs ?stats ?options ?entry (List.map Input.file paths)

let link_input_files ?stats ?options ?entry ?map_file ~output inputs =
  let paths = List.concat_map Input.paths inputs in
  let metadata_inputs =
    match options with
    | Some { Options.version_script = Some script; _ } -> [ script.path ]
    | _ -> []
  in
  let paths = paths @ metadata_inputs in
  Option.iter
    (fun path ->
      File_io.distinct_outputs output path;
      File_io.check_output ~output:path ~inputs:paths)
    map_file;
  let permissions =
    match options with Some o when o.Options.mode = Shared -> 0o644 | _ -> 0o755
  in
  File_io.publish ?stats ~output ~permissions ~inputs:paths (fun () ->
      let expanded = ref metadata_inputs in
      let on_input path = expanded := path :: !expanded in
      let map = Option.map (fun _ -> Buffer.create 1024) map_file in
      let image = build_inputs ?stats ?map ~on_input ?options ?entry inputs in
      File_io.check_output ~output ~inputs:!expanded;
      Option.iter
        (fun path ->
          File_io.publish ~output:path ~permissions:0o644 ~inputs:!expanded (fun () ->
              Bytes.of_string (Buffer.contents (Option.get map))))
        map_file;
      image)

let link_files ?stats ?options ?entry ?map_file ~output paths =
  link_input_files ?stats ?options ?entry ?map_file ~output (List.map Input.file paths)
