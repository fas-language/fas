module E = Elf_reader

type regular = { file : int; symbol : int; path : string; binding : int }
type common = { size : int; alignment : int }
type definition = Regular of regular | Common of common

type state = {
  name : string;
  mutable definition : definition option;
  mutable strong_undefined : bool;
  mutable shared_undefined : bool;
  mutable visibility : int;
}

type resolved = { file : int; symbol : int; visibility : int }
type import = { library : int option; symbol : Shared_object.symbol; visibility : int }

type t = {
  objects : E.t array;
  input_count : int;
  shared : Shared_object.t array;
  dependencies : Shared_dependencies.t;
  imports : (string, import) Hashtbl.t;
  shared_references : (string, unit) Hashtbl.t;
  definitions : (string, resolved) Hashtbl.t;
  strong_undefined : (string, unit) Hashtbl.t;
  predefined : int option;
  discarded : bool array array;
  symver : Symver.t;
}

let predefined_names =
  [
    "__executable_start";
    "__ehdr_start";
    "__bss_start";
    "_etext";
    "etext";
    "_edata";
    "edata";
    "_end";
    "end";
    "_GLOBAL_OFFSET_TABLE_";
    "__preinit_array_start";
    "__preinit_array_end";
    "__init_array_start";
    "__init_array_end";
    "__fini_array_start";
    "__fini_array_end";
    "__rela_iplt_start";
    "__rela_iplt_end";
  ]

(* Image-header addresses can be zero before loading, which is not a usable
   dynamic lookup definition. Startup boundaries and loader internals also
   belong to their own image. Explicit definitions supplied by inputs retain
   their visibility; this policy applies only to synthesized definitions. *)
let hidden_predefined name =
  Section_registry.boundary name <> None
  || List.mem name
       [
         "__executable_start";
         "__ehdr_start";
         "_GLOBAL_OFFSET_TABLE_";
         "_DYNAMIC";
         "__preinit_array_start";
         "__preinit_array_end";
         "__init_array_start";
         "__init_array_end";
         "__fini_array_start";
         "__fini_array_end";
         "__rela_iplt_start";
         "__rela_iplt_end";
       ]

type prepared =
  | Object of E.t * bool ref
  | Shared of Shared_object.t * bool ref * bool
  | Archive of Archive.t * bool
  | Group of prepared list
  | Sequence of prepared list

module Pending = Set.Make (Int)

let visibility_rank = function 0 -> 0 | 3 -> 1 | 2 -> 2 | _ -> 3

let resolve ?stats ?(partial = false) ?(on_input = fun _ -> ())
    ?(options = Options.default) ~entry inputs =
  if entry = "" then Error.fail "entry symbol must not be empty";
  let sysroot = Option.bind options.sysroot Sysroot.normalize in
  let directories =
    ref (List.map (Sysroot.directory ~root:sysroot) options.library_paths)
  in
  let add_directory path =
    if List.length !directories >= 4096 then
      Error.fail "library search exceeds 4096 directories";
    directories := !directories @ [ Sysroot.directory ~root:sysroot path ]
  in
  if List.length !directories > 4096 then
    Error.fail "library search exceeds 4096 directories";
  let probes = ref 0 in
  let exists path =
    incr probes;
    if !probes > 1_000_000 then Error.fail "library search exceeds 1000000 probes";
    Sysroot.validate path;
    Sys.file_exists path
  in
  let paths = List.concat_map Input.paths inputs in
  if List.length paths > 1024 then Error.fail "input count exceeds 1024 file limit";
  let bytes = ref 0 in
  let on_decoded size =
    bytes :=
      Binary.add ~context:"combined input and decoded debug size"
        ~limit:(256 * 1024 * 1024)
        !bytes size;
    Stats.add stats "decoded_debug_bytes" size
  in
  let file_count = ref 0 in
  let external_files = Hashtbl.create ~random:true 16 in
  let read_extra ~counter path =
    let canonical = Unix.realpath path in
    match Hashtbl.find_opt external_files canonical with
    | Some data -> data
    | None ->
        incr file_count;
        if !file_count > 1024 then Error.fail "input count exceeds 1024 file limit";
        on_input path;
        let data =
          Stats.measure stats "read" (fun () ->
              File_io.read
                ~max_size:(min E.max_file_size ((256 * 1024 * 1024) - !bytes))
                path)
        in
        bytes := !bytes + String.length data;
        Stats.add stats counter 1;
        Stats.add stats "input_bytes" (String.length data);
        Hashtbl.add external_files canonical data;
        data
  in
  let read_external = read_extra ~counter:"thin_member_files" in
  (* Snapshot repeated positional inputs once per invocation. Parsing remains
     per occurrence: archive loaded/member state must never leak between passes.
     Charge every occurrence to the original logical file/byte budgets. *)
  let input_contents = Hashtbl.create ~random:true 16 in
  let read_input path =
    incr file_count;
    if !file_count > 1024 then Error.fail "input count exceeds 1024 file limit";
    on_input path;
    let data =
      Stats.measure stats "read" (fun () ->
          let canonical = Unix.realpath path in
          let max_size = min E.max_file_size ((256 * 1024 * 1024) - !bytes) in
          match Hashtbl.find_opt input_contents canonical with
          | Some data ->
              if String.length data > max_size then
                Error.fail "%s: input exceeds %d byte limit or remaining input budget"
                  path max_size;
              Stats.add stats "input_read_cache_hits" 1;
              data
          | None ->
              let data = File_io.read ~max_size path in
              Hashtbl.add input_contents canonical data;
              data)
    in
    bytes := !bytes + String.length data;
    Stats.add stats "input_files" 1;
    Stats.add stats "input_bytes" (String.length data);
    data
  in
  let add_script_directories commands =
    if not options.nostdlib then
      List.iter
        (function Linker_script.Search_dir path -> add_directory path | _ -> ())
        commands
  in
  let rec prepare ~depth ~scripts ~as_needed ~static_libraries = function
    | Input.Static input ->
        if depth >= 16 then Error.fail "input policy nesting exceeds 16";
        prepare ~depth:(depth + 1) ~scripts ~as_needed ~static_libraries:true input
    | Input.Library { name; whole; static } ->
        let static_libraries = static_libraries || static in
        let path =
          Library_search.find ~exists
            ~dynamic:
              ((not static_libraries)
              && not (Options.requires_static_inputs options.mode))
            ~directories:!directories name
        in
        prepare ~depth ~scripts ~as_needed ~static_libraries
          (Input.File { path; whole })
    | Input.As_needed input ->
        if depth >= 16 then Error.fail "input policy nesting exceeds 16";
        prepare ~depth:(depth + 1) ~scripts ~as_needed:true ~static_libraries input
    | Input.Object { path; data } ->
        bytes :=
          Binary.add ~context:"combined input size"
            ~limit:(256 * 1024 * 1024)
            !bytes (String.length data);
        Stats.add stats "input_bytes" (String.length data);
        Stats.add stats "in_memory_objects" 1;
        Object
          ( Stats.measure stats "parse" (fun () -> E.parse ~on_decoded ~path data),
            ref false )
    | Input.File { path; whole } ->
        let data = read_input path in
        if Archive.is_archive data || String.starts_with ~prefix:"!<thin>\n" data then begin
          let archive =
            Stats.measure stats "parse" (fun () -> Archive.parse ~path data)
          in
          Array.iter
            (fun (member : Archive.member) ->
              Option.iter
                (fun (file, _) -> if Sys.file_exists file then on_input file)
                member.external_member)
            archive.members;
          if (not whole) && (not archive.indexed) && Array.length archive.members <> 0
          then
            Error.fail
              "%s: archive has no symbol index (run ar s, or use --whole-archive)" path;
          Stats.add stats "archives" 1;
          Archive (archive, whole)
        end
        else if Shared_object.is_shared data then begin
          if options.mode = Options.Static_pie then
            Error.fail "%s: static PIE cannot use shared-library inputs" path;
          if Options.requires_static_inputs options.mode then
            Error.fail "%s: shared input requires -pie or --dynamic-linker" path;
          Shared
            ( Stats.measure stats "parse" (fun () -> Shared_object.parse ~path data),
              ref false,
              as_needed )
        end
        else if Linker_script.is_script data then begin
          if depth >= 16 then Error.fail "%s: script nesting exceeds 16" path;
          let canonical = Unix.realpath path in
          if List.mem canonical scripts then Error.fail "%s: cyclic linker script" path;
          Stats.add stats "linker_scripts" 1;
          let commands = Linker_script.parse ~path data in
          add_script_directories commands;
          let locate name =
            if String.starts_with ~prefix:"-l" name then
              Library_search.find ~exists
                ~dynamic:
                  ((not static_libraries)
                  && not (Options.requires_static_inputs options.mode))
                ~directories:!directories
                (String.sub name 2 (String.length name - 2))
            else begin
              let candidates =
                match Sysroot.file ~root:sysroot name with
                | Some path -> [ path ]
                | None when not (Filename.is_relative name) ->
                    [
                      (if Sysroot.contains ~root:sysroot canonical then
                         Sysroot.target ~root:sysroot name
                       else name);
                    ]
                | None ->
                    Filename.concat (Filename.dirname path) name
                    :: name
                    :: List.map (fun dir -> Filename.concat dir name) !directories
              in
              match List.find_opt exists candidates with
              | Some found -> found
              | None -> Error.fail "%s: cannot find script input %s" path name
            end
          in
          let rec item as_needed = function
            | Linker_script.File name ->
                prepare ~depth:(depth + 1) ~scripts:(canonical :: scripts) ~as_needed
                  ~static_libraries
                  (Input.File { path = locate name; whole })
            | Linker_script.As_needed children ->
                Sequence (List.map (item true) children)
          in
          Sequence
            (List.map
               (function
                 | Linker_script.Input children ->
                     Sequence (List.map (item as_needed) children)
                 | Linker_script.Group children ->
                     Group (List.map (item as_needed) children)
                 | Linker_script.Search_dir _ -> Sequence [])
               commands)
        end
        else
          Object
            ( Stats.measure stats "parse" (fun () -> E.parse ~on_decoded ~path data),
              ref false )
    | Input.Group children ->
        if depth >= 16 then Error.fail "input group nesting exceeds 16";
        Group
          (List.map
             (prepare ~depth:(depth + 1) ~scripts ~as_needed ~static_libraries)
             children)
  in
  let prepared =
    List.map
      (prepare ~depth:0 ~scripts:[] ~as_needed:false ~static_libraries:false)
      inputs
  in
  let states = Hashtbl.create ~random:true 256 and order = ref [] in
  let get name =
    match Hashtbl.find_opt states name with
    | Some s -> s
    | None ->
        let s =
          {
            name;
            definition = None;
            strong_undefined = false;
            shared_undefined = false;
            visibility = 0;
          }
        in
        Hashtbl.add states name s;
        order := s :: !order;
        s
  in
  if (not partial) && options.mode <> Options.Shared then
    (get entry).strong_undefined <- true;
  List.iter
    (fun name ->
      if name = "" then Error.fail "undefined root symbol must not be empty";
      (get name).strong_undefined <- true)
    options.undefined;
  let objects = ref [] and object_count = ref 0 in
  let shared = ref [] and shared_count = ref 0 in
  let shared_names = Hashtbl.create ~random:true 16 in
  let shared_references = Hashtbl.create ~random:true 256 in
  let shared_symbol_count = ref 0 in
  let providers = Hashtbl.create ~random:true 256 in
  let discarded = ref [] and signatures = Hashtbl.create ~random:true 64 in
  let symbol_count = ref 0 and relocation_count = ref 0 in
  let add_object (o : E.t) =
    let o = Symver.expand o in
    let removed = Array.make (Array.length o.sections) false in
    Array.iter
      (fun (group : E.group) ->
        if group.comdat then
          if Hashtbl.mem signatures group.signature then begin
            Array.iter (fun index -> removed.(index) <- true) group.members;
            Stats.add stats "discarded_comdat_groups" 1
          end
          else Hashtbl.add signatures group.signature ())
      o.groups;
    if !object_count >= 65536 then
      Error.fail "extracted object count exceeds 65536 limit";
    symbol_count :=
      Binary.add ~context:"combined symbol count" ~limit:4_000_000 !symbol_count
        (Array.length o.symbols);
    relocation_count :=
      Binary.add ~context:"combined relocation count" ~limit:4_000_000 !relocation_count
        (Array.length o.relocations);
    let file = !object_count in
    incr object_count;
    objects := o :: !objects;
    discarded := removed :: !discarded;
    Stats.add stats "objects" 1;
    Stats.add stats "symbols" (Array.length o.symbols);
    Stats.add stats "relocations" (Array.length o.relocations);
    Array.iteri
      (fun index (symbol : E.symbol) ->
        if symbol.binding <> 0 then begin
          let s = get symbol.name in
          if visibility_rank symbol.visibility > visibility_rank s.visibility then
            s.visibility <- symbol.visibility;
          if
            symbol.section = 0
            || (symbol.section < Array.length removed && removed.(symbol.section))
          then
            begin if symbol.binding = 1 then s.strong_undefined <- true
            end
          else begin
            let candidate =
              if symbol.section = 0xfff2 then
                Common
                  {
                    size = Int64.to_int symbol.size;
                    alignment = max 1 (Int64.to_int symbol.value);
                  }
              else
                Regular
                  { file; symbol = index; path = o.path; binding = symbol.binding }
            in
            let replace =
              match (s.definition, candidate) with
              | None, _ -> true
              | Some (Common old), Common c ->
                  s.definition <-
                    Some
                      (Common
                         {
                           size = max old.size c.size;
                           alignment = max old.alignment c.alignment;
                         });
                  false
              | Some (Regular r), Common _ -> r.binding = 2
              | Some (Common _), Regular _ -> symbol.binding = 1
              | Some (Regular old), Regular r ->
                  if
                    old.binding = 1 && r.binding = 1
                    && not options.allow_multiple_definition
                  then
                    Error.fail "duplicate symbol %s: %s and %s" s.name old.path r.path;
                  old.binding = 2 && r.binding = 1
            in
            if replace then s.definition <- Some candidate
          end
        end)
      o.symbols
  in
  let needed state =
    match state.definition with
    | None ->
        (state.strong_undefined || state.shared_undefined)
        && not (Hashtbl.mem providers state.name)
    | Some (Common _) -> true
    | Some (Regular _) -> false
  in
  let extract archive whole =
    if whole then
      Array.iteri
        (fun index (m : Archive.member) ->
          if not m.loaded then begin
            add_object (Archive.object_ ?stats ~on_decoded ~read_external archive index);
            m.loaded <- true;
            Stats.add stats "extracted_members" 1
          end)
        archive.Archive.members
    else begin
      (* Match GNU's forward archive passes. Newly needed earlier members wait
         for the next pass; immediately jumping backwards changes weak binding
         decisions. The index avoids scanning unrelated members in either pass. *)
      let pending = ref Pending.empty and deferred = ref Pending.empty in
      let cursor = ref 0 in
      let enqueue name =
        let state = get name in
        if needed state then
          match Hashtbl.find_opt archive.index name with
          | None -> ()
          | Some members ->
              List.iter
                (fun index ->
                  if not archive.members.(index).loaded then
                    if index >= !cursor then pending := Pending.add index !pending
                    else deferred := Pending.add index !deferred)
                members
      in
      List.iter (fun s -> enqueue s.name) !order;
      while not (Pending.is_empty !pending && Pending.is_empty !deferred) do
        if Pending.is_empty !pending then begin
          pending := !deferred;
          deferred := Pending.empty;
          cursor := 0
        end;
        let index = Pending.min_elt !pending in
        pending := Pending.remove index !pending;
        cursor := index + 1;
        let member = archive.members.(index) in
        let still_needed = List.exists (fun name -> needed (get name)) member.exports in
        if (not member.loaded) && still_needed then begin
          let object_ =
            Archive.object_ ?stats ~on_decoded ~read_external archive index
          in
          let provides_needed =
            Array.exists
              (fun (symbol : E.symbol) ->
                if symbol.binding = 0 || symbol.section = 0 then false
                else
                  match (get symbol.name).definition with
                  | None -> needed (get symbol.name)
                  | Some (Common _) -> symbol.binding = 1 && symbol.section <> 0xfff2
                  | Some (Regular _) -> false)
              object_.symbols
          in
          if provides_needed then begin
            add_object object_;
            member.loaded <- true;
            Stats.add stats "extracted_members" 1;
            Array.iter
              (fun (s : E.symbol) -> if s.binding <> 0 then enqueue s.name)
              object_.symbols
          end
        end
      done
    end
  in
  let rec process = function
    | Object (object_, loaded) ->
        if not !loaded then begin
          add_object object_;
          loaded := true
        end
    | Shared (library, loaded, as_needed) ->
        let required =
          (not as_needed)
          || Array.exists
               (fun (symbol : Shared_object.symbol) ->
                 let required name =
                   match Hashtbl.find_opt states name with
                   | Some state -> needed state
                   | None -> false
                 in
                 (symbol.default && required symbol.name)
                 || Option.fold ~none:false
                      ~some:(fun version -> required (symbol.name ^ "@" ^ version))
                      symbol.version)
               library.exports
        in
        if required && (not !loaded) && not (Hashtbl.mem shared_names library.needed)
        then begin
          Hashtbl.add shared_names library.needed ();
          let index = !shared_count in
          incr shared_count;
          shared := library :: !shared;
          loaded := true;
          Stats.add stats "shared_libraries" 1;
          shared_symbol_count :=
            Binary.add ~context:"combined shared symbol count" ~limit:4_000_000
              !shared_symbol_count
              (Array.length library.exports + Array.length library.references);
          Array.iter
            (fun (reference : Shared_object.reference) ->
              Hashtbl.replace shared_references reference.name ();
              if reference.undefined && reference.binding = 1 then
                (get reference.name).shared_undefined <- true)
            library.references;
          Array.iter
            (fun (symbol : Shared_object.symbol) ->
              let add name =
                if not (Hashtbl.mem providers name) then
                  Hashtbl.add providers name (index, symbol)
              in
              if symbol.default then add symbol.name;
              Option.iter
                (fun version -> add (symbol.name ^ "@" ^ version))
                symbol.version)
            library.exports
        end
    | Archive (archive, whole) -> extract archive whole
    | Sequence children -> List.iter process children
    | Group children ->
        let rec fixed_point () =
          let previous = (!object_count, !shared_count) in
          List.iter process children;
          if (!object_count, !shared_count) <> previous then fixed_point ()
        in
        fixed_point ()
  in
  Stats.measure stats "resolve" (fun () -> List.iter process prepared);
  let input_count = !object_count in
  let definitions = Hashtbl.create ~random:true (Hashtbl.length states) in
  let strong_undefined = Hashtbl.create ~random:true 16 in
  let imports = Hashtbl.create ~random:true 16 in
  let common_symbols = ref [] and common_size = ref 0 and common_alignment = ref 1 in
  let common_file = !object_count in
  let common_count = ref 0 in
  let predefined_states = ref [] in
  let registries =
    Section_registry.collect
      ~discarded:(Array.of_list (List.rev !discarded))
      (Array.of_list (List.rev !objects))
  in
  List.iter
    (fun (s : state) ->
      if s.strong_undefined then Hashtbl.add strong_undefined s.name ();
      match s.definition with
      | None
        when (not partial)
             && (List.mem s.name predefined_names
                || (s.name = "_DYNAMIC" && options.mode <> Options.Static)
                || Section_registry.available registries s.name) ->
          predefined_states := s :: !predefined_states
      | None ->
          (match Hashtbl.find_opt providers s.name with
          | Some _ when (not s.strong_undefined) && s.visibility <> 0 -> ()
          | Some (library, symbol) ->
              Hashtbl.add imports s.name
                {
                  library = Some library;
                  symbol =
                    { symbol with binding = (if s.strong_undefined then 1 else 2) };
                  visibility = s.visibility;
                }
          | None
            when (not (Options.requires_static_inputs options.mode))
                 && (not s.strong_undefined) && s.visibility = 0 ->
              Hashtbl.add imports s.name
                {
                  library = None;
                  visibility = 0;
                  symbol =
                    {
                      Shared_object.name = s.name;
                      kind = 0;
                      binding = 2;
                      size = 0L;
                      version = None;
                      default = true;
                    };
                }
          | None -> ());
          if
            (not partial) && s.strong_undefined && s.visibility <> 0
            && not options.gc_sections
          then Error.fail "undefined non-default visibility symbol %s" s.name
      | Some (Regular r) ->
          Hashtbl.add definitions s.name
            { file = r.file; symbol = r.symbol; visibility = s.visibility }
      | Some (Common c) ->
          if not partial then
            common_size :=
              Binary.align ~context:"common symbol layout" ~limit:E.max_section_size
                !common_size c.alignment;
          incr common_count;
          let symbol : E.symbol =
            {
              name = s.name;
              binding = 1;
              kind = 1;
              visibility = s.visibility;
              section = (if partial then 0xfff2 else 1);
              value = Int64.of_int (if partial then c.alignment else !common_size);
              size = Int64.of_int c.size;
            }
          in
          common_symbols := symbol :: !common_symbols;
          if not partial then
            common_size :=
              Binary.add ~context:"common symbol layout" ~limit:E.max_section_size
                !common_size c.size;
          common_alignment := max !common_alignment c.alignment;
          Hashtbl.add definitions s.name
            { file = common_file; symbol = !common_count; visibility = s.visibility })
    (List.rev !order);
  if !common_count <> 0 then begin
    let null_section : E.section =
      {
        index = 0;
        name = "";
        kind = 0;
        flags = 0;
        offset = 0;
        size = 0;
        link = 0;
        info = 0;
        alignment = 1;
        entsize = 0;
      }
    in
    let null_symbol : E.symbol =
      {
        name = "";
        binding = 0;
        kind = 0;
        visibility = 0;
        section = 0;
        value = 0L;
        size = 0L;
      }
    in
    let common_object : E.t =
      {
        path = "<common>";
        data = "";
        sections =
          [|
            null_section;
            {
              null_section with
              index = 1;
              name = ".bss.common";
              kind = 8;
              flags = 3;
              size = !common_size;
              alignment = !common_alignment;
            };
          |];
        symbols = Array.of_list (null_symbol :: List.rev !common_symbols);
        relocations = [||];
        groups = [||];
      }
    in
    objects := common_object :: !objects;
    discarded := Array.make 2 false :: !discarded
  end;
  let predefined =
    match List.rev !predefined_states with
    | [] -> None
    | states ->
        let file = List.length !objects in
        let null : E.symbol =
          {
            name = "";
            binding = 0;
            kind = 0;
            visibility = 0;
            section = 0;
            value = 0L;
            size = 0L;
          }
        in
        let symbols =
          null
          :: List.mapi
               (fun index (s : state) ->
                 let visibility =
                   if hidden_predefined s.name && visibility_rank s.visibility < 2 then
                     2
                   else s.visibility
                 in
                 Hashtbl.add definitions s.name { file; symbol = index + 1; visibility };
                 { null with name = s.name; binding = 1; visibility; section = 0xfff1 })
               states
          |> Array.of_list
        in
        objects :=
          {
            E.path = "<linker symbols>";
            data = "";
            sections = [||];
            symbols;
            relocations = [||];
            groups = [||];
          }
          :: !objects;
        discarded := [||] :: !discarded;
        Stats.add stats "linker_symbols" (List.length states);
        Some file
  in
  let objects = Array.of_list (List.rev !objects) in
  let symver =
    Symver.plan ~objects ~definition:(fun name ->
        Option.map
          (fun (d : resolved) -> (d.file, d.symbol))
          (Hashtbl.find_opt definitions name))
  in
  let shared = Array.of_list (List.rev !shared) in
  let dependencies =
    Shared_dependencies.load ?stats
      ~read:(read_extra ~counter:"dependency_files")
      ~options shared
  in
  Array.iter
    (fun (library : Shared_object.t) ->
      Array.iter
        (fun (reference : Shared_object.reference) ->
          Hashtbl.replace shared_references reference.name ();
          Option.iter
            (fun version ->
              Hashtbl.replace shared_references (reference.name ^ "@" ^ version) ())
            reference.version)
        library.references)
    dependencies.libraries;
  {
    objects;
    input_count;
    shared;
    dependencies;
    imports;
    shared_references;
    definitions;
    strong_undefined;
    predefined;
    discarded = Array.of_list (List.rev !discarded);
    symver;
  }
