(* Direct executable references to DSO data need storage in the executable.
   The loader initializes it with COPY and interposes the exported definition
   for ordinary DSO lookups. Shared output keeps its existing PIC requirement. *)
module E = Elf_reader
module S = Shared_object

type entry = { key : string; file : int; symbol : int }
type t = { entries : entry array; names : (string, unit) Hashtbl.t }

let mem t key = Hashtbl.mem t.names key

let prepare ?stats ~(options : Options.t) ~kept (resolved : Resolver.t) =
  let names = Hashtbl.create ~random:true 16 in
  let empty = { entries = [||]; names } in
  match options.mode with
  | Options.Static | Static_pie | Shared -> (resolved, kept, empty)
  | Dynamic _ ->
      let requested = ref [] in
      Array.iteri
        (fun file (object_ : E.t) ->
          Array.iter
            (fun (r : E.relocation) ->
              let target = object_.sections.(r.target) in
              if
                kept.(file).(r.target)
                && target.flags land 2 <> 0
                && (List.mem r.kind [ 2; 10; 11; 24 ]
                   || (r.kind = 1 && target.flags land 1 = 0))
              then begin
                let symbol = object_.symbols.(r.symbol) in
                if
                  symbol.binding <> 0
                  && not (Hashtbl.mem resolved.definitions symbol.name)
                then
                  match Hashtbl.find_opt resolved.imports symbol.name with
                  | Some provider
                    when provider.symbol.kind = 1 && provider.library <> None ->
                      if not (Hashtbl.mem names symbol.name) then begin
                        Hashtbl.add names symbol.name ();
                        requested := (symbol.name, provider) :: !requested
                      end
                  | _ -> ()
              end)
            object_.relocations)
        resolved.objects;
      if !requested = [] then (resolved, kept, empty)
      else begin
        (* Reject overlapping exported storage for now. Creating independent
           copies would silently break aliases, including distinct versions.
           One sweep per selected DSO avoids an exports-by-copies scan. *)
        let overlaps = Hashtbl.create ~random:true 8 in
        let overlapping library =
          match Hashtbl.find_opt overlaps library with
          | Some found -> found
          | None ->
              let source = resolved.shared.(library) in
              let locations =
                Hashtbl.fold
                  (fun key (loc : S.location) acc -> (key, loc) :: acc)
                  source.locations []
                |> List.sort (fun (_, (a : S.location)) (_, (b : S.location)) ->
                    let c = compare a.section b.section in
                    if c <> 0 then c else Int64.unsigned_compare a.value b.value)
              in
              let found = Hashtbl.create ~random:true 16 in
              let previous = ref None in
              List.iter
                (fun (key, (loc : S.location)) ->
                  let size = if loc.size > 0L then loc.size else 1L in
                  let delta = Int64.sub loc.value loc.start in
                  let size =
                    if delta >= 0L && delta <= Int64.of_int loc.length then
                      min size (Int64.sub (Int64.of_int loc.length) delta)
                    else 0L
                  in
                  let ending = Int64.add loc.value size in
                  match !previous with
                  | Some (old_key, section, old_end)
                    when section = loc.section
                         && Int64.unsigned_compare loc.value old_end < 0 ->
                      Hashtbl.replace found old_key ();
                      Hashtbl.replace found key ();
                      if Int64.unsigned_compare ending old_end > 0 then
                        previous := Some (key, loc.section, ending)
                  | _ -> previous := Some (key, loc.section, ending))
                locations;
              Hashtbl.add overlaps library found;
              found
        in
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
        let sections = ref [ null_section ] and symbols = ref [ null ] in
        let entries = ref [] and count = ref 0 and allocated = ref 0 in
        let data = Buffer.create 16 in
        let file = Array.length resolved.objects in
        let identities = Hashtbl.create ~random:true 16 in
        List.iter
          (fun (key, (provider : Resolver.import)) ->
            let library = Option.get provider.library in
            let source = resolved.shared.(library) in
            let symbol = provider.symbol in
            let fail reason =
              Error.fail
                "%s: COPY relocation for %s: %s; compile position-independent code"
                source.path key reason
            in
            let identity = (symbol.name, symbol.version) in
            let loc =
              match Hashtbl.find_opt source.locations identity with
              | Some loc -> loc
              | None -> fail "data has no regular storage"
            in
            if loc.visibility <> 0 then fail "protected data cannot be copied";
            if symbol.binding = 10 then fail "GNU unique data cannot be copied";
            if (not (List.mem loc.kind [ 1; 8 ])) || Int64.logand loc.flags 0x402L <> 2L
            then fail "data requires allocatable non-TLS PROGBITS or NOBITS";
            let delta = Int64.sub loc.value loc.start in
            if
              symbol.size <= 0L
              || symbol.size > Int64.of_int E.max_section_size
              || delta < 0L
              || delta > Int64.of_int loc.length
              || symbol.size > Int64.sub (Int64.of_int loc.length) delta
            then fail "zero-sized or out-of-bounds data storage";
            if Hashtbl.mem (overlapping library) identity then
              fail "overlapping exported data aliases are unsupported";
            if Hashtbl.mem identities (library, identity) then
              fail "mixed version spellings of copied data are unsupported";
            Hashtbl.add identities (library, identity) ();
            if loc.alignment < 0L then fail "invalid storage alignment";
            let alignment = max 1L loc.alignment in
            if
              alignment > Int64.of_int E.max_alignment
              || Int64.logand alignment (Int64.pred alignment) <> 0L
              || Int64.logand loc.start (Int64.pred alignment) <> 0L
            then fail "invalid or unsupported storage alignment";
            let alignment = ref (Int64.to_int alignment) in
            while Int64.logand loc.value (Int64.of_int (!alignment - 1)) <> 0L do
              alignment := !alignment / 2
            done;
            let size = Int64.to_int symbol.size in
            allocated :=
              Binary.add ~context:"COPY storage"
                ~limit:(256 * 1024 * 1024)
                !allocated size;
            incr count;
            let section = !count in
            sections :=
              {
                null_section with
                index = section;
                name = (if loc.read_only then ".data.rel.ro.copy" else ".bss.copy");
                kind = (if loc.read_only then 1 else 8);
                flags = 3;
                offset = Buffer.length data;
                size;
                alignment = !alignment;
              }
              :: !sections;
            if loc.read_only then Buffer.add_string data (String.make size '\000');
            symbols :=
              {
                null with
                name = key;
                binding = symbol.binding;
                kind = 1;
                section;
                size = symbol.size;
              }
              :: !symbols;
            Hashtbl.add resolved.definitions key
              { Resolver.file; symbol = !count; visibility = 0 };
            Hashtbl.replace resolved.shared_references key ();
            entries := { key; file; symbol = !count } :: !entries)
          (List.rev !requested);
        let object_ : E.t =
          {
            path = "<COPY storage>";
            data = Buffer.contents data;
            sections = Array.of_list (List.rev !sections);
            symbols = Array.of_list (List.rev !symbols);
            relocations = [||];
            groups = [||];
          }
        in
        Stats.add stats "copy_relocations" !count;
        Stats.add stats "copy_storage_bytes" !allocated;
        ( {
            resolved with
            objects = Array.append resolved.objects [| object_ |];
            discarded =
              Array.append resolved.discarded [| Array.make (!count + 1) false |];
          },
          Array.append kept [| Array.init (!count + 1) (fun n -> n <> 0) |],
          { entries = Array.of_list (List.rev !entries); names } )
      end
