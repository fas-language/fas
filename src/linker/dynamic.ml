(* Loader-backed executables: eager GOT imports, direct PLT stubs and RELA.
   All tables and decisions belong to this invocation. *)
module E = Elf_reader
module B = Binary

let interp = 1
let dynstr = 2
let dynsym = 3
let hash = 4
let rela = 5
let dynamic = 6
let plt = 7
let versym = 8
let verneed = 9
let verdef = 10
let gnu_hash = 11

type target = Input of int * int | Got of int | Copy of int * int
type fixup = { target : target; symbol : int; kind : int }

type import = {
  key : string;
  provider : Resolver.import;
  name : int;
  version : int;
  index : int;
}

type export = {
  source : int;
  symbol : int;
  image_relative : bool;
  name : int;
  visibility : int;
  index : int;
  version : int;
  dynamic_name : string;
}

type t = {
  file : int;
  pie : bool;
  shared : bool;
  static_tls : bool;
  relative_count : int;
  fixups : fixup array;
  got : Got.t;
  imports : import array;
  exports : export array;
  bound_tls : (int * int) array;
  module_base : (int * int) option;
  indices : (string, int) Hashtbl.t;
  stubs : (int * int) array;
  stub_indices : (int, int) Hashtbl.t;
  ifunc_symbols : (int, unit) Hashtbl.t;
  ifunc_slots : (int * int, int) Hashtbl.t;
  needed : int array;
  runpath : int option;
  new_dtags : bool;
  soname : int option;
  verneed_count : int;
  verdef_count : int;
  lifecycle : Lifecycle.t;
  hash_style : Options.hash_style;
}

let relative_symbol (resolved : Resolver.t) file index =
  let symbol = resolved.objects.(file).symbols.(index) in
  let definition =
    if index = 0 then None
    else if symbol.binding = 0 then Some (file, symbol)
    else
      Option.map
        (fun (d : Resolver.resolved) ->
          (d.file, resolved.objects.(d.file).symbols.(d.symbol)))
        (Hashtbl.find_opt resolved.definitions symbol.name)
  in
  match definition with
  | None -> false
  | Some (file, _) when resolved.predefined = Some file -> true
  | Some (_, s) when s.section = 0 || s.section >= 0xff00 || s.kind = 6 -> false
  | Some (file, s) -> resolved.objects.(file).sections.(s.section).flags land 2 <> 0

let prepare ?stats ~(options : Options.t) ~kept ~(got : Got.t) ~module_base ~copies
    (resolved : Resolver.t) =
  match options.mode with
  | Static ->
      if options.runpath <> None then Error.fail "-rpath requires dynamic output";
      if options.soname <> None then Error.fail "SONAME requires shared output";
      (resolved, kept, None)
  | (Static_pie | Dynamic _ | Shared) as mode ->
      let shared = mode = Options.Shared in
      let static_pie = mode = Options.Static_pie in
      let static_tls = shared && Array.exists (( = ) Got.Thread_offset) got.models in
      let pie, interpreter =
        match mode with
        | Dynamic { pie; interpreter } -> (pie, interpreter)
        | Shared -> (true, "")
        | Static_pie -> (true, "")
        | Static -> assert false
      in
      let lifecycle = Lifecycle.prepare ~kept resolved in
      if
        ((not shared) && (not static_pie) && interpreter = "")
        || String.length interpreter > 4095
        || String.contains interpreter '\000'
      then
        Error.fail "dynamic interpreter must be a nonempty path of at most 4095 bytes";
      let strings = Buffer.create 256 in
      Buffer.add_char strings '\000';
      let string_offsets = Hashtbl.create 32 in
      let string name =
        if String.contains name '\000' || String.length name > 65535 then
          Error.fail "invalid or oversized dynamic name";
        match Hashtbl.find_opt string_offsets name with
        | Some offset -> offset
        | None ->
            let offset = Buffer.length strings in
            ignore
              (B.add ~context:"dynamic strings" ~limit:Layout.max_output offset
                 (String.length name + 1));
            Buffer.add_string strings name;
            Buffer.add_char strings '\000';
            Hashtbl.add string_offsets name offset;
            offset
      in
      let needed =
        Array.map (fun (s : Shared_object.t) -> string s.needed) resolved.shared
      in
      let runpath = Option.map string options.runpath in
      let soname = Option.map string options.soname in
      if soname <> None && not shared then Error.fail "SONAME requires shared output";
      Option.iter
        (fun name -> if name = "" then Error.fail "empty SONAME")
        options.soname;
      let definition_nodes =
        match options.version_script with
        | Some script when Version_script.named script -> script.nodes
        | _ -> [||]
      in
      let export_version =
        let nodes = Hashtbl.create ~random:true 16 in
        Array.iteri
          (fun n (node : Version_script.node) -> Hashtbl.add nodes node.name (n + 2))
          definition_nodes;
        let explicit name =
          match Symver.version resolved.symver name with
          | None -> None
          | Some v ->
              let id =
                if v.version = "" then 1
                else
                  match Hashtbl.find_opt nodes v.version with
                  | Some id -> id
                  | None ->
                      Error.fail "symbol %s references missing version node %s" name
                        v.version
              in
              Some (id lor if v.default || id = 1 then 0 else 0x8000)
        in
        match options.version_script with
        | None -> fun name -> Option.value (explicit name) ~default:1
        | Some script -> (
            let policy = Version_script.policy script in
            fun name ->
              match explicit name with
              | Some id -> id
              | None -> ( match policy name with Global id -> id | Local -> 0))
      in
      let definition_count = Array.length definition_nodes in
      let definition_data =
        if definition_count = 0 then ""
        else begin
          let base_name = Option.value options.soname ~default:"fas-shared" in
          let records =
            (base_name, None, 1, 1)
            :: Array.to_list
                 (Array.mapi
                    (fun n (node : Version_script.node) ->
                      (node.name, node.parent, n + 2, 0))
                    definition_nodes)
          in
          let length =
            List.fold_left
              (fun n (_, parent, _, _) -> n + 28 + if parent = None then 0 else 8)
              0 records
          in
          let bytes = Bytes.make length '\000' and cursor = ref 0 in
          List.iteri
            (fun n (name, parent, id, flags) ->
              let at = !cursor and size = 28 + if parent = None then 0 else 8 in
              B.set_int bytes at 2 1;
              B.set_int bytes (at + 2) 2 flags;
              B.set_int bytes (at + 4) 2 id;
              B.set_int bytes (at + 6) 2 (if parent = None then 1 else 2);
              B.set bytes (at + 8) 4 (Symbol_hash.sysv name);
              B.set_int bytes (at + 12) 4 20;
              B.set_int bytes (at + 16) 4
                (if n + 1 = List.length records then 0 else size);
              B.set_int bytes (at + 20) 4 (string name);
              Option.iter
                (fun parent ->
                  B.set_int bytes (at + 24) 4 8;
                  B.set_int bytes (at + 28) 4 (string parent))
                parent;
              cursor := at + size)
            records;
          Bytes.unsafe_to_string bytes
        end
      in
      (* Local descriptors still need a TLS dynsym: symbol zero denotes an
         undefined weak descriptor to the loader. Reserve local entries before
         all global symbols, canonicalizing definitions across input objects. *)
      let bound_indices = Hashtbl.create ~random:true 16 in
      let bound_tls = ref [] and bound_count = ref 0 in
      let bound_definition file symbol =
        let s = resolved.objects.(file).symbols.(symbol) in
        if s.binding = 0 then (file, symbol)
        else
          match Hashtbl.find_opt resolved.definitions s.name with
          | Some d -> (d.file, d.symbol)
          | None ->
              Error.fail "%s: undefined TLS symbol %s" resolved.objects.(file).path
                s.name
      in
      Array.iteri
        (fun slot (file, symbol) ->
          let s = resolved.objects.(file).symbols.(symbol) in
          if
            got.models.(slot) = Got.Descriptor
            && (s.binding = 0 || not (Hashtbl.mem resolved.imports s.name))
          then begin
            let key = bound_definition file symbol in
            if not (Hashtbl.mem bound_indices key) then begin
              incr bound_count;
              Hashtbl.add bound_indices key !bound_count;
              bound_tls := key :: !bound_tls
            end
          end)
        got.slots;
      let bound_tls = Array.of_list (List.rev !bound_tls) in
      let local_count = Array.length bound_tls in
      let imports = ref [] and import_count = ref local_count in
      let indices = Hashtbl.create ~random:true 32 in
      let versions = Hashtbl.create ~random:true 16 and version_order = ref [] in
      let version_id library version =
        match Hashtbl.find_opt versions (library, version) with
        | Some id -> id
        | None ->
            let id = 2 + definition_count + Hashtbl.length versions in
            if id >= 0x8000 then Error.fail "too many required symbol versions";
            Hashtbl.add versions (library, version) id;
            version_order := (library, version, id, string version) :: !version_order;
            id
      in
      let import ?(copy = false) key =
        let original = key in
        let key = Got.import_key resolved got key in
        match
          if (not copy) && Copy_relocation.mem copies key then None
          (* Aliases share a dynamic index, but their visibility can differ.
             A locally bound spelling must not acquire its default version's
             preemptible import merely through canonicalization. *)
            else Hashtbl.find_opt resolved.imports original
        with
        | None -> None
        | Some (provider : Resolver.import) ->
            if provider.visibility <> 0 then
              Error.fail "undefined non-default visibility symbol %s" key;
            let index =
              match Hashtbl.find_opt indices key with
              | Some index -> index
              | None ->
                  incr import_count;
                  if !import_count > 1_000_000 then
                    Error.fail "too many dynamic imports";
                  let index = !import_count in
                  let version =
                    match (provider.library, provider.symbol.version) with
                    | Some library, Some version -> version_id library version
                    | _ -> 1
                  in
                  let name = string provider.symbol.name in
                  imports := { key; provider; name; version; index } :: !imports;
                  Hashtbl.add indices key index;
                  index
            in
            Some index
      in
      let relative = ref [] and symbolic = ref [] in
      Array.iter
        (fun (entry : Copy_relocation.entry) ->
          let symbol = Option.get (import ~copy:true entry.key) in
          symbolic :=
            { target = Copy (entry.file, entry.symbol); symbol; kind = 5 } :: !symbolic)
        copies.entries;
      let stubs = ref [] and stub_indices = Hashtbl.create ~random:true 16 in
      let ifunc_slots = Hashtbl.create ~random:true 16 in
      Array.iteri
        (fun slot (file, symbol) ->
          if got.models.(slot) = Got.Ifunc then
            Option.iter
              (fun key -> Hashtbl.replace ifunc_slots key slot)
              (Got.definition resolved file symbol))
        got.slots;
      let stub symbol slot =
        if slot < 0 then Error.fail "missing GOT slot for dynamic function call";
        if not (Hashtbl.mem stub_indices slot) then begin
          let index = Hashtbl.length stub_indices in
          Hashtbl.add stub_indices slot index;
          stubs := (symbol, slot) :: !stubs
        end
      in
      Array.iteri
        (fun file (o : E.t) ->
          Array.iteri
            (fun index (r : E.relocation) ->
              let target = o.sections.(r.target) in
              if
                r.kind <> 0 && r.kind <> 35
                && kept.(file).(r.target)
                && target.flags land 2 <> 0
              then begin
                let s = o.symbols.(r.symbol) in
                let imported =
                  if
                    s.binding = 0
                    || (target.name = ".eh_frame" || List.mem r.kind [ 17; 21 ])
                       && Hashtbl.mem resolved.definitions s.name
                  then None
                  else if (not pie) && List.mem r.kind [ 10; 11 ] then
                    match Hashtbl.find_opt resolved.imports s.name with
                    | Some i when i.library = None && i.symbol.binding = 2 -> None
                    | _ -> import s.name
                  else import s.name
                in
                let relative_symbol = relative_symbol resolved file r.symbol in
                let fail description =
                  Error.fail
                    "%s: %s+0x%x referencing %s: %s is unsupported in dynamic output"
                    o.path target.name r.offset s.name description
                in
                match imported with
                | Some symbol ->
                    let canonical =
                      Hashtbl.find_opt got.canonical_imports
                        (Symver.key resolved.symver s.name)
                    in
                    if canonical <> None && not (Got.relocation r.kind) then
                      stub symbol (Option.get canonical)
                    else if r.kind = 4 then begin
                      let provider = Hashtbl.find resolved.imports s.name in
                      if
                        provider.symbol.kind <> 0 && provider.symbol.kind <> 2
                        && provider.symbol.kind <> 10
                      then fail "function call to a data symbol";
                      stub symbol got.references.(file).(index)
                    end
                    else if r.kind = 1 then begin
                      if target.flags land 1 = 0 then
                        fail "import relocation in read-only memory";
                      symbolic :=
                        { target = Input (file, index); symbol; kind = 1 } :: !symbolic
                    end
                    else if not (Got.relocation r.kind) then
                      fail
                        "direct import relocation (compile position-independent code)"
                | None ->
                    if Got.ifunc resolved file r.symbol && target.name <> ".eh_frame"
                    then
                      begin if r.kind = 4 || r.kind = 2 then
                        stub 0 got.references.(file).(index)
                      else if r.kind = 1 then begin
                        if target.flags land 1 = 0 then
                          fail "IFUNC relocation in read-only memory";
                        let canonical =
                          Got.canonical_ifunc resolved got file r.symbol
                        in
                        if r.addend <> 0L && not canonical then
                          fail "nonzero addend on local IFUNC address";
                        if not canonical then
                          symbolic :=
                            { target = Input (file, index); symbol = 0; kind = 37 }
                            :: !symbolic
                        else if pie then
                          relative :=
                            { target = Input (file, index); symbol = 0; kind = 8 }
                            :: !relative
                      end
                      else if not (Got.relocation r.kind) then
                        fail "direct local IFUNC reference"
                      end
                    else begin
                      if
                        s.section = 0 && s.binding <> 0
                        && Hashtbl.mem resolved.strong_undefined s.name
                        && not (Hashtbl.mem resolved.definitions s.name)
                      then Error.fail "%s: undefined symbol %s" o.path s.name;
                      if pie && relative_symbol && r.kind = 1 then begin
                        if target.flags land 1 = 0 then
                          fail "absolute relocation in read-only memory";
                        relative :=
                          { target = Input (file, index); symbol = 0; kind = 8 }
                          :: !relative
                      end
                      else if pie && relative_symbol && (r.kind = 10 || r.kind = 11)
                      then
                        fail
                          "narrow absolute address relocation (compile \
                           position-independent code)"
                      else if
                        pie && (not relative_symbol)
                        && (r.kind = 2 || r.kind = 4 || r.kind = 24)
                        && not
                             (static_pie && r.kind = 4
                             && target.flags land 4 <> 0
                             && s.section = 0 && s.binding = 2
                             && (not (Hashtbl.mem resolved.definitions s.name))
                             && not (Hashtbl.mem resolved.strong_undefined s.name))
                      then
                        fail "PC-relative reference to an absolute or undefined address"
                    end
              end)
            o.relocations)
        resolved.objects;
      Array.iteri
        (fun slot (file, symbol) ->
          let s = resolved.objects.(file).symbols.(symbol) in
          let model = got.models.(slot) in
          let imported =
            if s.binding = 0 || model = Got.Local_module || model = Got.Local_offset
            then None
            else import s.name
          in
          match imported with
          | _ when model = Got.Descriptor_argument -> ()
          | Some _
            when model = Got.Address
                 && Hashtbl.mem got.canonical_imports
                      (Symver.key resolved.symver s.name) ->
              ()
          | Some symbol ->
              let kind =
                match model with
                | Address | Ifunc -> 6
                | Plt -> 7
                | Thread_offset -> 18
                | Module_id -> 16
                | Dynamic_offset -> 17
                | Descriptor -> 36
                | Local_module | Local_offset | Descriptor_argument -> assert false
              in
              symbolic := { target = Got slot; symbol; kind } :: !symbolic
          | None
            when model = Got.Address && pie
                 && relative_symbol resolved file symbol
                 && ((not (Got.ifunc resolved file symbol))
                    || Got.canonical_ifunc resolved got file symbol) ->
              relative := { target = Got slot; symbol = 0; kind = 8 } :: !relative
          | None when shared && List.mem model [ Got.Module_id; Got.Local_module ] ->
              symbolic := { target = Got slot; symbol = 0; kind = 16 } :: !symbolic
          | None when shared && model = Got.Thread_offset ->
              symbolic := { target = Got slot; symbol = 0; kind = 18 } :: !symbolic
          | None when model = Got.Descriptor ->
              let symbol = Hashtbl.find bound_indices (bound_definition file symbol) in
              symbolic := { target = Got slot; symbol; kind = 36 } :: !symbolic
          | None
            when (model = Got.Ifunc
                 || model = Got.Address
                    && not (Got.canonical_ifunc resolved got file symbol))
                 && Got.ifunc resolved file symbol ->
              symbolic := { target = Got slot; symbol = 0; kind = 37 } :: !symbolic
          | None -> ())
        got.slots;
      let relative_count = List.length !relative in
      let fixups = Array.of_list (List.rev !relative @ List.rev !symbolic) in
      let imports = Array.of_list (List.rev !imports) in
      let exports = ref [] in
      let symbol_count = ref (!import_count + 1) in
      Array.iteri
        (fun source (o : E.t) ->
          Array.iteri
            (fun index (symbol : E.symbol) ->
              match Hashtbl.find_opt resolved.definitions symbol.name with
              | Some (d : Resolver.resolved)
                when index <> 0 && d.file = source && d.symbol = index
                     && (not (Symver.alias resolved.symver symbol.name))
                     && (d.visibility = 0 || d.visibility = 3)
                     && (shared || options.export_dynamic
                        || Hashtbl.mem resolved.shared_references symbol.name)
                     && (symbol.section = 0xfff1
                        || symbol.section <> 0 && symbol.section < 0xff00
                           && kept.(source).(symbol.section)
                           && o.sections.(symbol.section).flags land 2 <> 0) ->
                  exports :=
                    {
                      source;
                      symbol = index;
                      image_relative = resolved.predefined = Some source;
                      name = string (Symver.name resolved.symver symbol.name);
                      dynamic_name = Symver.name resolved.symver symbol.name;
                      visibility = d.visibility;
                      version =
                        (if Copy_relocation.mem copies symbol.name then
                           let provider = Hashtbl.find resolved.imports symbol.name in
                           match (provider.library, provider.symbol.version) with
                           | Some library, Some version -> version_id library version
                           | _ -> 1
                         else export_version symbol.name);
                      index =
                        (match Hashtbl.find_opt indices symbol.name with
                        | Some index -> index
                        | None ->
                            let index = !symbol_count in
                            incr symbol_count;
                            index);
                    }
                    :: !exports
              | _ -> ())
            o.symbols)
        resolved.objects;
      let exports = Array.of_list (List.rev !exports) in
      let symbol_count = !symbol_count in
      if symbol_count > 1_000_000 then Error.fail "too many dynamic symbols";
      let symbol_names = Array.make symbol_count "" in
      Array.iter
        (fun (imported : import) ->
          symbol_names.(imported.index) <- imported.provider.symbol.name)
        imports;
      Array.iter
        (fun (exported : export) ->
          symbol_names.(exported.index) <- exported.dynamic_name)
        exports;
      let remap, gnu_data =
        if options.hash_style = Options.Sysv then (Array.init symbol_count Fun.id, "")
        else Gnu_hash.build ~first:(local_count + 1) symbol_names
      in
      let ordered_names = Array.make symbol_count "" in
      Array.iteri (fun n name -> ordered_names.(remap.(n)) <- name) symbol_names;
      let imports =
        Array.map (fun (i : import) -> { i with index = remap.(i.index) }) imports
      in
      let exports =
        Array.map (fun (e : export) -> { e with index = remap.(e.index) }) exports
      in
      let fixups =
        Array.map (fun (f : fixup) -> { f with symbol = remap.(f.symbol) }) fixups
      in
      Hashtbl.filter_map_inplace (fun _ index -> Some remap.(index)) indices;
      let ifunc_symbols = Hashtbl.create ~random:true 16 in
      Array.iter
        (fun (imported : import) ->
          if imported.provider.symbol.kind = 10 then
            Hashtbl.replace ifunc_symbols imported.index ())
        imports;
      Array.iter
        (fun exported ->
          if resolved.objects.(exported.source).symbols.(exported.symbol).kind = 10 then
            Hashtbl.replace ifunc_symbols exported.index ())
        exports;
      let stubs =
        Array.of_list (List.rev !stubs)
        |> Array.map (fun (symbol, slot) -> (remap.(symbol), slot))
      in
      let version_groups = Array.make (Array.length resolved.shared) [] in
      List.iter
        (fun (library, version, id, name) ->
          version_groups.(library) <- (id, name, version) :: version_groups.(library))
        !version_order;
      let nonempty =
        Array.to_list
          (Array.mapi
             (fun library versions -> (library, List.rev versions))
             version_groups)
        |> List.filter (fun (_, versions) -> versions <> [])
      in
      let version_data =
        Bytes.make
          (List.fold_left
             (fun size (_, versions) -> size + 16 + (List.length versions * 16))
             0 nonempty)
          '\000'
      in
      let cursor = ref 0 in
      List.iteri
        (fun group (library, versions) ->
          let version_count = List.length versions in
          let start = !cursor and size = 16 + (version_count * 16) in
          B.set_int version_data start 2 1;
          B.set_int version_data (start + 2) 2 version_count;
          B.set_int version_data (start + 4) 4 needed.(library);
          B.set_int version_data (start + 8) 4 16;
          B.set_int version_data (start + 12) 4
            (if group + 1 = List.length nonempty then 0 else size);
          List.iteri
            (fun n (id, name, text) ->
              let at = start + 16 + (n * 16) in
              B.set version_data at 4 (Symbol_hash.sysv text);
              B.set_int version_data (at + 6) 2 id;
              B.set_int version_data (at + 8) 4 name;
              B.set_int version_data (at + 12) 4
                (if n + 1 = version_count then 0 else 16))
            versions;
          cursor := start + size)
        nonempty;
      let data = Buffer.create 256 and sections = ref [] in
      let section index name kind flags alignment entsize contents =
        let offset = Buffer.length data in
        Buffer.add_string data contents;
        sections :=
          ({
             E.index;
             name;
             kind;
             flags;
             offset;
             size = String.length contents;
             link = 0;
             info = 0;
             alignment;
             entsize;
           }
            : E.section)
          :: !sections
      in
      section 0 "" 0 0 1 0 "";
      section interp ".interp" 1 2 1 0
        (if shared || static_pie then "" else interpreter ^ "\000");
      section dynstr ".dynstr" 3 2 1 0 (Buffer.contents strings);
      let symbols = Bytes.make (symbol_count * 24) '\000' in
      let version_symbols = Bytes.make (symbol_count * 2) '\000' in
      Array.iter
        (fun (imported : import) ->
          let at = imported.index * 24 in
          let symbol = imported.provider.symbol in
          B.set_int symbols at 4 imported.name;
          B.set_int symbols (at + 4) 1
            ((symbol.binding lsl 4) lor if symbol.kind = 10 then 2 else symbol.kind);
          B.set version_symbols (imported.index * 2) 2 (Int64.of_int imported.version))
        imports;
      Array.iter
        (fun exported ->
          B.set_int version_symbols (exported.index * 2) 2 exported.version)
        exports;
      section dynsym ".dynsym" 11 2 8 24 (Bytes.unsafe_to_string symbols);
      let hash_data =
        if options.hash_style = Options.Gnu then ""
        else begin
          let buckets = max 1 ((symbol_count - 1) / 4) in
          let hash_data = Bytes.make (8 + ((buckets + symbol_count) * 4)) '\000' in
          B.set_int hash_data 0 4 buckets;
          B.set_int hash_data 4 4 symbol_count;
          Array.iteri
            (fun n name ->
              if n > local_count then begin
                let bucket =
                  Int64.to_int
                    (Int64.rem (Symbol_hash.sysv name) (Int64.of_int buckets))
                in
                let at = 8 + (bucket * 4) in
                let previous = B.u32 (Bytes.unsafe_to_string hash_data) at in
                B.set hash_data (8 + ((buckets + n) * 4)) 4 previous;
                B.set_int hash_data at 4 n
              end)
            ordered_names;
          Bytes.unsafe_to_string hash_data
        end
      in
      section hash ".hash" 5 2 4 4 hash_data;
      section rela ".rela.dyn" 4 2 8 24 (String.make (24 * Array.length fixups) '\000');
      let verneed_count = List.length nonempty in
      let verdef_count = if definition_count = 0 then 0 else definition_count + 1 in
      let tag_count =
        13
        + (if options.hash_style = Options.Both then 1 else 0)
        + Array.length needed + Lifecycle.tag_count lifecycle
        + (if runpath = None then 0 else 1)
        + (if soname = None then 0 else 1)
        - (if shared then 1 else 0)
        + (if verneed_count = 0 && verdef_count = 0 then 0 else 1)
        + (if verneed_count = 0 then 0 else 2)
        + if verdef_count = 0 then 0 else 2
      in
      section dynamic ".dynamic" 6 3 8 16 (String.make (tag_count * 16) '\000');
      section plt ".plt" 1 6 16 16 (String.make (Array.length stubs * 16) '\000');
      section versym ".gnu.version" 0x6fffffff 2 2 2
        (Bytes.unsafe_to_string version_symbols);
      section verneed ".gnu.version_r" 0x6ffffffe 2 8 0
        (Bytes.unsafe_to_string version_data);
      section verdef ".gnu.version_d" 0x6ffffffd 2 8 0 definition_data;
      section gnu_hash ".gnu.hash" 0x6ffffff6 2 8 0 gnu_data;
      let sections = Array.of_list (List.rev !sections) in
      let object_ : E.t =
        {
          path = "<dynamic metadata>";
          data = Buffer.contents data;
          sections;
          symbols = [||];
          relocations = [||];
          groups = [||];
        }
      in
      let file = Array.length resolved.objects in
      Stats.add stats "dynamic_relative_relocations" relative_count;
      Stats.add stats "dynamic_imports" (Array.length imports);
      Stats.add stats "dynamic_exports" (Array.length exports);
      Stats.add stats "plt_entries" (Array.length stubs);
      ( {
          resolved with
          objects = Array.append resolved.objects [| object_ |];
          discarded =
            Array.append resolved.discarded
              [| Array.make (Array.length sections) false |];
        },
        Array.append kept
          [|
            Array.init (Array.length sections) (fun index ->
                index <> 0
                && ((not (shared || static_pie)) || index <> interp)
                && (index <> versym || verneed_count <> 0 || verdef_count <> 0)
                && (index <> verneed || verneed_count <> 0)
                && (index <> verdef || verdef_count <> 0)
                && (index <> hash || options.hash_style <> Options.Gnu)
                && (index <> gnu_hash || options.hash_style <> Options.Sysv));
          |],
        Some
          {
            file;
            pie;
            shared;
            static_tls;
            relative_count;
            fixups;
            got;
            imports;
            exports;
            bound_tls;
            module_base;
            indices;
            stubs;
            stub_indices;
            ifunc_symbols;
            ifunc_slots;
            needed;
            runpath;
            new_dtags = options.new_dtags;
            soname;
            verneed_count;
            verdef_count;
            lifecycle;
            hash_style = options.hash_style;
          } )

let placement ~placements t section : Layout.placement =
  Option.get placements.(t.file).(section)

let metadata ~placements t index =
  let matches section =
    match placements.(t.file).(section) with
    | Some (p : Layout.placement) -> p.output_index = index
    | None -> false
  in
  let output section = (placement ~placements t section).output_index in
  if matches dynsym then Some (output dynstr, Array.length t.bound_tls + 1)
  else if matches hash || matches gnu_hash || matches rela || matches versym then
    Some (output dynsym, 0)
  else if matches dynamic then Some (output dynstr, 0)
  else if matches verneed then Some (output dynstr, t.verneed_count)
  else if matches verdef then Some (output dynstr, t.verdef_count)
  else None

let stub ~placements t slot =
  Option.map
    (fun index -> (placement ~placements t plt).address + (index * 16))
    (Hashtbl.find_opt t.stub_indices slot)

let ifunc_stub ~placements ~resolved t file symbol =
  Option.bind (Got.definition resolved file symbol) (fun key ->
      Option.bind (Hashtbl.find_opt t.ifunc_slots key) (stub ~placements t))

let canonical_stub ~placements ~(resolved : Resolver.t) t file symbol =
  let s = resolved.objects.(file).symbols.(symbol) in
  if s.binding = 0 then None
  else
    Option.bind
      (Hashtbl.find_opt t.got.canonical_imports (Symver.key resolved.symver s.name))
      (stub ~placements t)

let emit ~out ~placements ~sections ~objects ~base ~tls t =
  let p = placement ~placements t in
  (* An ET_EXEC's non-PIC function addresses use one canonical PLT address.
     Its call slot uses JUMP_SLOT so the loader skips this undefined dynsym
     value when resolving the implementation, avoiding a self-referential GOT. *)
  Array.iter
    (fun (imported : import) ->
      Option.iter
        (fun slot ->
          let address = Option.get (stub ~placements t slot) in
          B.set_int out ((p dynsym).offset + (imported.index * 24) + 8) 8 address)
        (Hashtbl.find_opt t.got.canonical_imports imported.key))
    t.imports;
  Array.iteri
    (fun n (file, index) ->
      let symbol = objects.(file).E.symbols.(index) in
      let source : Layout.placement = Option.get placements.(file).(symbol.section) in
      let at = (p dynsym).offset + ((n + 1) * 24) in
      B.set_int out (at + 4) 1 6;
      B.set_int out (at + 6) 2 source.output_index;
      B.set out (at + 8) 8
        (if t.module_base = Some (file, index) then 0L
         else
           Int64.add symbol.value
             (Int64.of_int (source.address - base - (Option.get tls).Layout.offset)));
      B.set out (at + 16) 8 symbol.size)
    t.bound_tls;
  Array.iter
    (fun exported ->
      let symbol = objects.(exported.source).E.symbols.(exported.symbol) in
      let section, value =
        if exported.image_relative then
          (Layout.image_symbol_section sections symbol.value, symbol.value)
        else if symbol.section = 0xfff1 then (0xfff1, symbol.value)
        else
          let source : Layout.placement =
            Option.get placements.(exported.source).(symbol.section)
          in
          let address =
            source.address
            - if symbol.kind = 6 then base + (Option.get tls).Layout.offset else 0
          in
          (source.output_index, Int64.add (Int64.of_int address) symbol.value)
      in
      let at = (p dynsym).offset + (exported.index * 24) in
      B.set_int out at 4 exported.name;
      B.set_int out (at + 4) 1 ((symbol.binding lsl 4) lor symbol.kind);
      B.set_int out (at + 5) 1 exported.visibility;
      B.set_int out (at + 6) 2 section;
      B.set out (at + 8) 8 value;
      B.set out (at + 16) 8 symbol.size)
    t.exports;
  let targets =
    Array.map
      (fun fixup ->
        let offset, address =
          match fixup.target with
          | Input (file, index) ->
              let r = objects.(file).E.relocations.(index) in
              let target : Layout.placement = Option.get placements.(file).(r.target) in
              (target.offset + r.offset, target.address + r.offset)
          | Got slot ->
              let target : Layout.placement =
                Option.get placements.(Option.get t.got.file).(1)
              in
              (target.offset + (slot * 8), target.address + (slot * 8))
          | Copy (file, index) ->
              let symbol = objects.(file).E.symbols.(index) in
              let target : Layout.placement =
                Option.get placements.(file).(symbol.section)
              in
              ( target.offset + Int64.to_int symbol.value,
                target.address + Int64.to_int symbol.value )
        in
        (fixup, offset, address))
      t.fixups
  in
  (* Resolve ordinary addresses before executing local resolvers. RELACOUNT
     covers the leading RELATIVE block only. *)
  let rank fixup =
    if fixup.kind = 8 then 0
    else if fixup.kind = 37 || Hashtbl.mem t.ifunc_symbols fixup.symbol then 2
    else 1
  in
  Array.sort
    (fun (a, _, aa) (b, _, bb) ->
      let c = compare (rank a) (rank b) in
      if c <> 0 then c else compare aa bb)
    targets;
  Array.iteri
    (fun index (fixup, offset, address) ->
      let at = (p rela).offset + (index * 24) in
      let addend =
        if List.mem fixup.kind [ 5; 6; 16; 36 ] then 0L
        else
          match fixup.target with
          | Input (file, index) when fixup.kind = 1 ->
              objects.(file).E.relocations.(index).addend
          | _ -> B.u64 (Bytes.unsafe_to_string out) offset
      in
      B.set_int out at 8 address;
      B.set out (at + 8) 8
        (Int64.logor
           (Int64.shift_left (Int64.of_int fixup.symbol) 32)
           (Int64.of_int fixup.kind));
      B.set out (at + 16) 8 addend;
      if fixup.kind <> 5 then B.set_int out offset 8 0)
    targets;
  Array.iteri
    (fun index (_, slot) ->
      let at = (p plt).offset + (index * 16) in
      let address = (p plt).address + (index * 16) in
      let got : Layout.placement = Option.get placements.(Option.get t.got.file).(1) in
      Bytes.blit_string "\243\015\030\250\255\037" 0 out at 6;
      X86_64.apply ~context:"dynamic PLT" out ~offset:(at + 6) ~kind:2
        ~symbol:(Int64.of_int (got.address + (slot * 8)))
        ~addend:(-4L)
        ~place:(Int64.of_int (address + 6));
      Bytes.blit_string "\102\015\031\068\000\000" 0 out (at + 10) 6)
    t.stubs;
  let tags =
    Array.to_list (Array.map (fun offset -> (1, offset)) t.needed)
    @ Lifecycle.tags ~placements ~objects t.lifecycle
    @ (match t.runpath with
      | None -> []
      | Some offset -> [ ((if t.new_dtags then 29 else 15), offset) ])
    @ (match t.soname with None -> [] | Some offset -> [ (14, offset) ])
    @ (if t.shared then [] else [ (21, 0) ])
    @ (if t.verneed_count = 0 && t.verdef_count = 0 then []
       else [ (0x6ffffff0, (p versym).address) ])
    @ (if t.verdef_count = 0 then []
       else [ (0x6ffffffc, (p verdef).address); (0x6ffffffd, t.verdef_count) ])
    @ (if t.verneed_count = 0 then []
       else [ (0x6ffffffe, (p verneed).address); (0x6fffffff, t.verneed_count) ])
    @ (if t.hash_style = Options.Gnu then [] else [ (4, (p hash).address) ])
    @ (if t.hash_style = Options.Sysv then []
       else [ (0x6ffffef5, (p gnu_hash).address) ])
    @ [
        (5, (p dynstr).address);
        (6, (p dynsym).address);
        (10, objects.(t.file).sections.(dynstr).size);
        (11, 24);
        (7, (p rela).address);
        (8, 24 * Array.length targets);
        (9, 24);
        (0x6ffffff9, t.relative_count);
        (0x6ffffffb, if t.pie && not t.shared then 0x08000001 else 1);
        (30, 8 lor if t.static_tls then 16 else 0);
        (0, 0);
      ]
  in
  List.iteri
    (fun index (tag, value) ->
      let at = (p dynamic).offset + (index * 16) in
      B.set_int out at 8 tag;
      B.set_int out (at + 8) 8 value)
    tags
