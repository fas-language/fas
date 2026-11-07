module E = Elf_reader

let prepare (resolved : Resolver.t) =
  (* Resolution has no inferred type for an absent weak provider. Keep the
     input's TLS declaration for undefined-weak descriptor lookup. *)
  Array.iter
    (fun (o : E.t) ->
      Array.iter
        (fun (s : E.symbol) ->
          if s.kind = 6 && s.binding <> 0 then
            match Hashtbl.find_opt resolved.imports s.name with
            | Some i when i.library = None && i.symbol.kind = 0 ->
                Hashtbl.replace resolved.imports s.name
                  { i with symbol = { i.symbol with kind = 6 } }
            | _ -> ())
        o.symbols)
    resolved.objects;
  let name = "_TLS_MODULE_BASE_" in
  let requested =
    Array.exists
      (fun (o : E.t) ->
        Array.exists (fun (s : E.symbol) -> s.name = name && s.section = 0) o.symbols)
      resolved.objects
  in
  if (not requested) || Hashtbl.mem resolved.definitions name then (resolved, None)
  else begin
    let template = ref None in
    Array.iteri
      (fun file (o : E.t) ->
        Array.iter
          (fun (s : E.section) ->
            if
              !template = None
              && s.flags land 0x402 = 0x402
              && not resolved.discarded.(file).(s.index)
            then template := Some (file, s.index))
          o.sections)
      resolved.objects;
    match !template with
    | None -> (resolved, None)
    | Some (file, section) ->
        let object_ = resolved.objects.(file) in
        let symbol = Array.length object_.symbols in
        let symbols =
          Array.append object_.symbols
            [|
              {
                E.name;
                binding = 1;
                kind = 6;
                visibility = 2;
                section;
                value = 0L;
                size = 0L;
              };
            |]
        in
        let objects = Array.copy resolved.objects in
        objects.(file) <- { object_ with symbols };
        Hashtbl.replace resolved.definitions name
          { Resolver.file; symbol; visibility = 2 };
        Hashtbl.remove resolved.imports name;
        ({ resolved with objects }, Some (file, symbol))
  end

let symbol (resolved : Resolver.t) file index =
  let s = resolved.objects.(file).symbols.(index) in
  if s.binding = 0 then s.kind = 6
  else
    match Hashtbl.find_opt resolved.definitions s.name with
    | Some (d : Resolver.resolved) ->
        resolved.objects.(d.file).symbols.(d.symbol).kind = 6
    | None -> (
        match Hashtbl.find_opt resolved.imports s.name with
        | Some (i : Resolver.import) -> i.symbol.kind = 6
        | None -> s.kind = 6)

let relocation = function 17 | 19 | 20 | 21 | 22 | 23 | 34 | 35 -> true | _ -> false

let validate ~(options : Options.t) ~kept resolved =
  let template =
    Array.exists Fun.id
      (Array.mapi
         (fun file (o : E.t) ->
           Array.exists
             (fun (s : E.section) -> kept.(file).(s.index) && s.flags land 0x400 <> 0)
             o.sections)
         resolved.Resolver.objects)
  in
  Array.iteri
    (fun file (o : E.t) ->
      Array.iter
        (fun (r : E.relocation) ->
          if r.kind <> 0 && kept.(file).(r.target) then begin
            let tls = symbol resolved file r.symbol in
            if relocation r.kind then begin
              (* Instruction/GOT TLS references require a loaded section. Debug
                 relocations do not keep their definitions alive under GC, so
                 making a runtime descriptor for one can reference a discarded
                 TLS template. DTPOFF debug offsets remain valid. *)
              if
                List.mem r.kind [ 19; 20; 22; 34; 35 ]
                && o.sections.(r.target).flags land 2 = 0
              then
                Error.fail
                  "%s: TLS GOT/call relocation in non-allocatable section %s is \
                   unsupported"
                  o.path o.sections.(r.target).name;
              if r.kind = 20 && not template then
                Error.fail "%s: TLSLD requires a local TLS template" o.path;
              if
                Options.requires_static_inputs options.mode
                && not (List.mem r.kind [ 17; 21; 22; 23 ])
              then
                if options.mode = Static_pie then
                  Error.fail "%s: static PIE supports only local-exec/initial-exec TLS"
                    o.path
                else Error.fail "%s: this TLS relocation requires dynamic output" o.path;
              if options.mode = Shared && r.kind = 23 then
                Error.fail "%s: local-exec TLS is invalid in shared output" o.path;
              if r.kind <> 20 && not tls then
                Error.fail "%s: TLS relocation references non-TLS symbol %s" o.path
                  o.symbols.(r.symbol).name;
              if
                List.mem r.kind [ 17; 21; 23 ]
                && Hashtbl.mem resolved.imports o.symbols.(r.symbol).name
                && not (Hashtbl.mem resolved.definitions o.symbols.(r.symbol).name)
              then
                Error.fail "%s: local TLS relocation references imported symbol %s"
                  o.path o.symbols.(r.symbol).name
            end
            else if tls && o.sections.(r.target).flags land 2 <> 0 then
              Error.fail "%s: non-TLS relocation references TLS symbol %s" o.path
                o.symbols.(r.symbol).name
          end)
        o.relocations)
    resolved.objects

let thread_offset (tls : Layout.segment option) value =
  match tls with
  | None -> Error.fail "local TLS reference without a TLS template"
  | Some tls ->
      let size =
        Binary.align ~context:"TLS block" ~limit:Layout.max_image tls.memory_size
          tls.alignment
      in
      Int64.sub value (Int64.of_int size)
