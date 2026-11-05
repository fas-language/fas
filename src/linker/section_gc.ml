module E = Elf_reader

let select ?stats ~(options : Options.t) ~entry ~frames (resolved : Resolver.t) =
  let objects = resolved.objects in
  let kept =
    Array.mapi
      (fun file (o : E.t) ->
        Array.map
          (fun (s : E.section) ->
            (not resolved.discarded.(file).(s.index))
            && ((not options.gc_sections) || s.flags land 2 = 0))
          o.sections)
      objects
  in
  if options.gc_sections then begin
    let registries = Section_registry.collect ~discarded:resolved.discarded objects in
    let pending = Queue.create () in
    let mark file section =
      if
        section <> 0 && section < 0xff00
        && (not resolved.discarded.(file).(section))
        && not kept.(file).(section)
      then begin
        kept.(file).(section) <- true;
        Queue.add (file, section) pending
      end
    in
    let definition (d : Resolver.resolved) =
      let s = objects.(d.file).symbols.(d.symbol) in
      match (resolved.predefined = Some d.file, Section_registry.boundary s.name) with
      | true, Some (name, _) ->
          List.iter
            (fun (file, (section : E.section)) -> mark file section.index)
            (Option.value (Hashtbl.find_opt registries name) ~default:[])
      | _ -> mark d.file s.section
    in
    let symbol file (s : E.symbol) =
      if s.binding = 0 then mark file s.section
      else
        match Hashtbl.find_opt resolved.definitions s.name with
        | None -> ()
        | Some d -> definition d
    in
    let root name =
      match Hashtbl.find_opt resolved.definitions name with
      | None -> ()
      | Some d -> definition d
    in
    if options.mode <> Options.Shared then root entry;
    List.iter root options.undefined;
    if options.mode <> Options.Static then begin
      root "_init";
      root "_fini"
    end;
    if options.mode <> Options.Static then
      Hashtbl.iter
        (fun name (d : Resolver.resolved) ->
          if
            (d.visibility = 0 || d.visibility = 3)
            && (options.mode = Options.Shared || options.export_dynamic
               || Hashtbl.mem resolved.shared_references name)
          then root name)
        resolved.definitions;
    let edges =
      Array.map (fun (o : E.t) -> Array.make (Array.length o.sections) []) objects
    in
    let memberships =
      Array.map (fun (o : E.t) -> Array.make (Array.length o.sections) (-1)) objects
    in
    let group_live =
      Array.map (fun (o : E.t) -> Array.make (Array.length o.groups) false) objects
    in
    Array.iteri
      (fun file (o : E.t) ->
        Array.iter
          (fun (s : E.section) ->
            if
              s.flags land 0x200000 <> 0
              || List.mem s.kind [ 14; 15; 16 ]
              || s.name = ".init" || s.name = ".fini" || s.name = ".note.ABI-tag"
              || (s.name = ".eh_frame" && s.flags land 0x200 = 0)
            then mark file s.index)
          o.sections;
        Array.iter
          (fun (r : E.relocation) ->
            if r.kind <> 0 && o.sections.(r.target).name <> ".eh_frame" then
              edges.(file).(r.target) <- (file, r.symbol) :: edges.(file).(r.target))
          o.relocations;
        Array.iteri
          (fun index (g : E.group) ->
            Array.iter (fun section -> memberships.(file).(section) <- index) g.members)
          o.groups)
      objects;
    (* An FDE describes code; its initial-location relocation must not root that
       code. Live code does retain its personality and LSDA, including their
       ordinary relocation edges to typeinfo and runtime entry points. *)
    Array.iter
      (fun (frame : Unwind.frame) ->
        if not resolved.discarded.(frame.file).(frame.section) then begin
          let object_ = objects.(frame.file) in
          let s = object_.symbols.(object_.relocations.(frame.relocation).symbol) in
          let definition =
            if s.binding = 0 then Some (frame.file, s)
            else
              Option.map
                (fun (d : Resolver.resolved) ->
                  (d.file, objects.(d.file).symbols.(d.symbol)))
                (Hashtbl.find_opt resolved.definitions s.name)
          in
          match definition with
          | Some (file, s) when s.section <> 0 && s.section < 0xff00 ->
              Array.iter
                (fun index ->
                  edges.(file).(s.section) <-
                    (frame.file, object_.relocations.(index).symbol)
                    :: edges.(file).(s.section))
                frame.metadata
          | Some (_, s) when s.section = 0xfff1 && frame.range <> 0L ->
              Array.iter
                (fun index ->
                  symbol frame.file object_.symbols.(object_.relocations.(index).symbol))
                frame.metadata
          | _ -> ()
        end)
      frames;
    while not (Queue.is_empty pending) do
      let file, section = Queue.take pending in
      let object_ = objects.(file) in
      let group = memberships.(file).(section) in
      if group <> -1 && not group_live.(file).(group) then begin
        group_live.(file).(group) <- true;
        Array.iter (mark file) object_.groups.(group).members
      end;
      List.iter
        (fun (source, index) -> symbol source objects.(source).symbols.(index))
        edges.(file).(section)
    done
  end;
  Array.iteri
    (fun file (o : E.t) ->
      Array.iter
        (fun (s : E.section) ->
          if List.mem s.kind [ 1; 7; 8; 14; 15; 16 ] && not kept.(file).(s.index) then
            Stats.add stats "discarded_sections" 1)
        o.sections)
    objects;
  kept
