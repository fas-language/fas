module E = Elf_reader

let relocation = function 9 | 19 | 20 | 22 | 34 | 41 | 42 -> true | _ -> false
let base_relocation = function 26 | 29 -> true | _ -> false

type key = Zero | Local of int * int | Global of string

type model =
  | Address
  | Thread_offset
  | Module_id
  | Dynamic_offset
  | Local_module
  | Local_offset
  | Descriptor
  | Descriptor_argument
  | Ifunc

type t = {
  file : int option;
  slots : (int * int) array;
  models : model array;
  references : int array array;
  canonical_ifuncs : (int * int, unit) Hashtbl.t;
}

let definition (resolved : Resolver.t) file symbol =
  let s = resolved.objects.(file).symbols.(symbol) in
  if s.binding = 0 then Some (file, symbol)
  else
    Option.map
      (fun (d : Resolver.resolved) -> (d.file, d.symbol))
      (Hashtbl.find_opt resolved.definitions s.name)

let canonical_ifunc resolved t file symbol =
  Option.fold ~none:false
    ~some:(Hashtbl.mem t.canonical_ifuncs)
    (definition resolved file symbol)

let ifunc (resolved : Resolver.t) file symbol =
  let s = resolved.objects.(file).symbols.(symbol) in
  if s.binding = 0 then s.kind = 10
  else
    match Hashtbl.find_opt resolved.definitions s.name with
    | Some d -> resolved.objects.(d.file).symbols.(d.symbol).kind = 10
    | None -> false

let prepare ?stats ?(static_ifunc = false) ~kept (resolved : Resolver.t) =
  let objects = resolved.objects in
  let references =
    Array.map (fun (o : E.t) -> Array.make (Array.length o.relocations) (-1)) objects
  in
  let indices = Hashtbl.create ~random:true 64 and slots = ref [] and count = ref 0 in
  let models = ref [] in
  let canonical_ifuncs = Hashtbl.create ~random:true 16 in
  Array.iteri
    (fun file (o : E.t) ->
      Array.iteri
        (fun index (r : E.relocation) ->
          if
            (relocation r.kind
            || (not static_ifunc)
               && (r.kind = 4
                  || r.kind = 2
                     && o.sections.(r.target).name <> ".eh_frame"
                     && ifunc resolved file r.symbol)
               && o.sections.(r.target).flags land 2 <> 0
               && (ifunc resolved file r.symbol
                  || o.symbols.(r.symbol).binding <> 0
                     && Hashtbl.mem resolved.imports o.symbols.(r.symbol).name))
            && kept.(file).(r.target)
          then begin
            let symbol = o.symbols.(r.symbol) in
            if r.kind = 2 && ifunc resolved file r.symbol then
              Option.iter
                (fun key -> Hashtbl.replace canonical_ifuncs key ())
                (definition resolved file r.symbol);
            let key =
              if r.symbol = 0 || r.kind = 20 then Zero
              else if symbol.binding = 0 then Local (file, r.symbol)
              else Global symbol.name
            in
            let model =
              match r.kind with
              | 19 -> Module_id
              | 20 -> Local_module
              | 22 -> Thread_offset
              | 34 -> Descriptor
              | (2 | 4) when ifunc resolved file r.symbol -> Ifunc
              | _ -> Address
            in
            let slot =
              match Hashtbl.find_opt indices (key, model) with
              | Some slot -> slot
              | None ->
                  let slot = !count in
                  incr count;
                  Hashtbl.add indices (key, model) slot;
                  slots := (file, r.symbol) :: !slots;
                  models := model :: !models;
                  if r.kind = 19 || r.kind = 20 || r.kind = 34 then begin
                    incr count;
                    slots := (file, r.symbol) :: !slots;
                    models :=
                      (if r.kind = 19 then Dynamic_offset
                       else if r.kind = 34 then Descriptor_argument
                       else Local_offset)
                      :: !models
                  end;
                  slot
            in
            references.(file).(index) <- slot
          end)
        o.relocations)
    objects;
  let slots = Array.of_list (List.rev !slots) in
  let models = Array.of_list (List.rev !models) in
  let needs_base =
    match
      ( Hashtbl.find_opt resolved.definitions "_GLOBAL_OFFSET_TABLE_",
        resolved.predefined )
    with
    | Some d, Some file -> d.Resolver.file = file
    | _ -> false
  in
  let needs_base =
    needs_base
    || Array.exists
         (fun (o : E.t) ->
           Array.exists (fun (r : E.relocation) -> base_relocation r.kind) o.relocations)
         objects
  in
  if !count = 0 && not needs_base then
    (resolved, kept, { file = None; slots; models; references; canonical_ifuncs })
  else begin
    let size = !count * 8 in
    let section : E.section =
      {
        index = 1;
        name = ".got";
        kind = 1;
        flags = 3;
        offset = 0;
        size;
        link = 0;
        info = 0;
        alignment = 8;
        entsize = 8;
      }
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
    let object_ : E.t =
      {
        path = "<GOT>";
        data = String.make size '\000';
        sections =
          [|
            {
              section with
              index = 0;
              name = "";
              kind = 0;
              flags = 0;
              size = 0;
              entsize = 0;
            };
            section;
          |];
        symbols = [| null |];
        relocations = [||];
        groups = [||];
      }
    in
    let file = Array.length objects in
    Stats.add stats "got_entries" !count;
    ( {
        resolved with
        objects = Array.append objects [| object_ |];
        discarded = Array.append resolved.discarded [| [| false; false |] |];
      },
      Array.append kept [| [| false; true |] |],
      {
        file = Some file;
        slots;
        models;
        references = Array.append references [| [||] |];
        canonical_ifuncs;
      } )
  end
