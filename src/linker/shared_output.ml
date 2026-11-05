module E = Elf_reader

(* A DSO's default-visible definitions participate in runtime lookup, including
   references from the defining DSO. Keep the regular definition for GC/layout
   and add a runtime lookup descriptor for GOT/PLT/dynamic relocation planning. *)
let prepare ~(options : Options.t) (resolved : Resolver.t) =
  let version_policy = Option.map Version_script.policy options.version_script in
  let exact_local =
    match options.version_script with
    | None -> fun _ -> false
    | Some script -> Version_script.exact_local script
  in
  if options.version_script <> None && options.mode <> Options.Shared then
    Error.fail "version scripts require shared output";
  let version_nodes = Hashtbl.create ~random:true 16 in
  Option.iter
    (fun (script : Version_script.t) ->
      Array.iter
        (fun (node : Version_script.node) -> Hashtbl.add version_nodes node.name ())
        script.nodes)
    options.version_script;
  Hashtbl.iter
    (fun name (_ : Resolver.resolved) ->
      match Symver.version resolved.symver name with
      | None -> ()
      | Some v ->
          if options.mode <> Options.Shared then
            Error.fail "defined symbol version %s requires shared output" name;
          if v.version <> "" && not (Hashtbl.mem version_nodes v.version) then
            Error.fail "symbol %s references missing version node %s" name v.version)
    resolved.definitions;
  if options.mode = Options.Shared then begin
    Option.iter
      (fun policy ->
        Hashtbl.filter_map_inplace
          (fun name (d : Resolver.resolved) ->
            match
              match Symver.version resolved.symver name with
              | Some v -> if exact_local v.name then Version_script.Local else Global 1
              | None -> policy name
            with
            | Version_script.Local -> Some { d with visibility = 2 }
            | Global _ -> Some d)
          resolved.definitions)
      version_policy;
    let add (symbol : E.symbol) visibility =
      Hashtbl.replace resolved.imports symbol.name
        {
          Resolver.library = None;
          visibility;
          symbol =
            {
              Shared_object.name = Symver.name resolved.symver symbol.name;
              kind = symbol.kind;
              binding =
                (if
                   symbol.section = 0
                   && Hashtbl.mem resolved.strong_undefined symbol.name
                 then 1
                 else symbol.binding);
              size = symbol.size;
              version =
                Option.bind (Symver.version resolved.symver symbol.name) (fun v ->
                    if v.version = "" then None else Some v.version);
              default =
                (match Symver.version resolved.symver symbol.name with
                | Some v -> v.default
                | None -> true);
            };
        }
    in
    Array.iteri
      (fun file (object_ : E.t) ->
        Array.iter
          (fun (section : E.section) ->
            if section.kind = 16 && section.size <> 0 then
              Error.fail "%s: preinit arrays are invalid in shared output" object_.path)
          object_.sections;
        Array.iteri
          (fun index (symbol : E.symbol) ->
            if index <> 0 && symbol.binding <> 0 then
              match Hashtbl.find_opt resolved.definitions symbol.name with
              | Some (d : Resolver.resolved)
                when d.file = file && d.symbol = index && d.visibility = 0
                     && resolved.predefined <> Some file ->
                  add symbol 0
              | None
                when symbol.section = 0 && symbol.visibility = 0
                     && ((not options.no_undefined) || symbol.binding = 2)
                     && not (Hashtbl.mem resolved.imports symbol.name) ->
                  if Symver.parse symbol.name <> None then
                    Error.fail "%s: undefined versioned symbol %s requires a provider"
                      object_.path symbol.name;
                  add symbol 0
              | _ -> ())
          object_.symbols)
      resolved.objects
  end;
  resolved
