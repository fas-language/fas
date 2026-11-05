(* Link-time validation is deliberately separate from direct input resolution:
   an indirect DSO must not satisfy a regular object's reference or gain a new
   DT_NEEDED entry merely because it was inspected. *)
module S = Shared_object

type t = { libraries : S.t array; names : (string * string, S.t) Hashtbl.t }

let expand ~origin text =
  if String.length text > 65536 then
    Error.fail "dependency search path exceeds 65536 bytes";
  let output = Buffer.create (String.length text) in
  let add text =
    if String.length text > 65536 - Buffer.length output then
      Error.fail "dependency search path exceeds 65536 bytes";
    Buffer.add_string output text
  in
  let rec scan at =
    if at < String.length text then
      if text.[at] = '$' then
        let prefix value =
          let size = String.length value in
          size <= String.length text - at
          &&
          let rec same n = n = size || (text.[at + n] = value.[n] && same (n + 1)) in
          same 0
        in
        if prefix "${ORIGIN}" then begin
          add origin;
          scan (at + 9)
        end
        else if
          prefix "$ORIGIN"
          && (at + 7 = String.length text
             ||
             match text.[at + 7] with
             | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> false
             | _ -> true)
        then begin
          add origin;
          scan (at + 7)
        end
        else Error.fail "unsupported dependency path token in %s" text
      else begin
        Buffer.add_char output text.[at];
        if Buffer.length output > 65536 then
          Error.fail "dependency search path exceeds 65536 bytes";
        scan (at + 1)
      end
  in
  scan 0;
  Buffer.contents output

let paths ?sysroot ?(prefixes = false) ~origin text =
  if String.length text > 65536 then
    Error.fail "dependency search path exceeds 65536 bytes";
  let parts = String.split_on_char ':' text in
  if List.length parts > 4096 then
    Error.fail "dependency search exceeds 4096 directories";
  List.map
    (fun text ->
      let text =
        if prefixes then Sysroot.directory ~root:sysroot text
        else if not (Filename.is_relative text) then Sysroot.target ~root:sysroot text
        else text
      in
      if text = "" then "." else expand ~origin text)
    parts

let bounded_paths paths =
  if List.length paths > 4096 then
    Error.fail "dependency search exceeds 4096 directories";
  paths

let load ?stats ~read ~(options : Options.t) direct =
  if options.allow_shlib_undefined then { libraries = [||]; names = Hashtbl.create 0 }
  else begin
    let sysroot = Option.bind options.sysroot Sysroot.normalize in
    let known = Hashtbl.create ~random:true 32 in
    let names = Hashtbl.create ~random:true 32 in
    let visited = Hashtbl.create ~random:true 32 in
    let pending = Queue.create () in
    Array.iter (fun (s : S.t) -> Hashtbl.replace known s.needed s) direct;
    Array.iter (fun s -> Queue.add (s, []) pending) direct;
    let libraries = ref [] and symbols = ref 0 and edges = ref 0 and probes = ref 0 in
    let explicit =
      List.fold_left
        (fun found text ->
          bounded_paths
            (found @ paths ?sysroot ~prefixes:true ~origin:(Sys.getcwd ()) text))
        [] options.rpath_link
    in
    while not (Queue.is_empty pending) do
      let (library : S.t), inherited = Queue.take pending in
      if not (Hashtbl.mem visited library.path) then begin
        Hashtbl.add visited library.path ();
        if Hashtbl.length visited > 1024 then
          Error.fail "transitive shared-library count exceeds 1024";
        libraries := library :: !libraries;
        symbols :=
          Binary.add ~context:"transitive shared symbol count" ~limit:4_000_000 !symbols
            (Array.length library.exports
            + Array.length library.references
            + Array.length library.defined_versions
            + Array.length library.version_requirements);
        let path =
          if Filename.is_relative library.path then
            Filename.concat (Sys.getcwd ()) library.path
          else library.path
        in
        let origin = Filename.dirname path in
        let inherited =
          match (library.runpath, library.rpath) with
          | None, Some text -> bounded_paths (paths ?sysroot ~origin text @ inherited)
          | _ -> inherited
        in
        let search =
          bounded_paths
            (explicit
            @
            match library.runpath with
            | Some text -> paths ?sysroot ~origin text
            | None -> inherited)
        in
        Array.iter
          (fun name ->
            incr edges;
            if !edges > 65536 then
              Error.fail "transitive dependency count exceeds 65536";
            let expanded =
              if not (Filename.is_relative name) then Sysroot.target ~root:sysroot name
              else expand ~origin name
            in
            let dependency =
              match Hashtbl.find_opt known expanded with
              | Some s -> s
              | None ->
                  let probe path =
                    incr probes;
                    if !probes > 1_000_000 then
                      Error.fail "dependency search exceeds 1000000 probes";
                    Sys.file_exists path
                  in
                  let rec find = function
                    | [] -> None
                    | directory :: rest ->
                        let path = Filename.concat directory expanded in
                        if probe path then Some path else find rest
                  in
                  let path =
                    match
                      if String.contains expanded '/' then
                        if probe expanded then Some expanded else None
                      else find search
                    with
                    | Some path -> path
                    | None ->
                        Error.fail "%s: cannot find DT_NEEDED %s (use -rpath-link)"
                          library.path name
                  in
                  let data = read path in
                  let s =
                    Stats.measure stats "parse_dependency" (fun () ->
                        S.parse ~path data)
                  in
                  (* Basename lookup reuses the first SONAME identity. Explicit
                     expanded paths can load distinct DSOs with the same SONAME. *)
                  let s =
                    match Hashtbl.find_opt known s.needed with
                    | Some existing when not (String.contains expanded '/') -> existing
                    | Some _ -> s
                    | None ->
                        Hashtbl.add known s.needed s;
                        s
                  in
                  Hashtbl.replace known expanded s;
                  Stats.add stats "transitive_shared_libraries" 1;
                  s
            in
            Hashtbl.replace names (library.path, name) dependency;
            Queue.add (dependency, inherited) pending)
          library.dependencies
      end
    done;
    { libraries = Array.of_list (List.rev !libraries); names }
  end

let validate ~defined graph =
  let libraries = graph.libraries in
  let providers = Hashtbl.create ~random:true 256 in
  let versions = Hashtbl.create ~random:true 32 in
  Array.iter
    (fun (library : S.t) ->
      Array.iter
        (fun version -> Hashtbl.replace versions (library.path, version) ())
        library.defined_versions;
      Array.iter
        (fun (symbol : S.symbol) ->
          let add version =
            let key = (symbol.name, version) in
            if not (Hashtbl.mem providers key) then Hashtbl.add providers key symbol
          in
          if symbol.default then add None;
          Option.iter (fun version -> add (Some version)) symbol.version)
        library.exports)
    libraries;
  Array.iter
    (fun (library : S.t) ->
      Array.iter
        (fun (requirement : S.version_requirement) ->
          if not requirement.weak then
            match Hashtbl.find_opt graph.names (library.path, requirement.provider) with
            | Some provider
              when Hashtbl.mem versions (provider.path, requirement.version) ->
                ()
            | _ ->
                Error.fail "%s: required version %s not found in %s" library.path
                  requirement.version requirement.provider)
        library.version_requirements;
      Array.iter
        (fun (reference : S.reference) ->
          if reference.undefined && reference.binding = 1 then begin
            let name =
              match reference.version with
              | None -> reference.name
              | Some version -> reference.name ^ "@" ^ version
            in
            let kind =
              match defined reference.name reference.version with
              | Some kind -> Some kind
              | None ->
                  Option.map
                    (fun (s : S.symbol) -> s.kind)
                    (Hashtbl.find_opt providers (reference.name, reference.version))
            in
            match kind with
            | None ->
                Error.fail "%s: unresolved shared-library reference %s" library.path
                  name
            | Some kind when kind = 6 <> (reference.kind = 6) ->
                Error.fail "%s: shared-library TLS type mismatch for %s" library.path
                  name
            | _ -> ()
          end)
        library.references)
    libraries
