module E = Elf_reader

type t = { arrays : (int * (int * int) array) list; functions : (int * int * int) list }

let prepare ~kept (resolved : Resolver.t) =
  let arrays = ref [] in
  List.iter
    (fun kind ->
      let members = ref [] in
      Array.iteri
        (fun file (o : E.t) ->
          Array.iter
            (fun (s : E.section) ->
              if s.kind = kind && s.size <> 0 && kept.(file).(s.index) then
                members := (file, s.index) :: !members)
            o.sections)
        resolved.objects;
      if !members <> [] then
        arrays := (kind, Array.of_list (List.rev !members)) :: !arrays)
    [ 16; 14; 15 ];
  let functions =
    List.filter_map
      (fun (name, tag) ->
        match Hashtbl.find_opt resolved.definitions name with
        | None -> None
        | Some (d : Resolver.resolved) ->
            let s = resolved.objects.(d.file).symbols.(d.symbol) in
            if
              s.section = 0 || s.section >= 0xff00
              || (not kept.(d.file).(s.section))
              || resolved.objects.(d.file).sections.(s.section).flags land 6 <> 6
              || s.value
                 >= Int64.of_int resolved.objects.(d.file).sections.(s.section).size
            then Error.fail "%s must be inside executable code" name;
            Some (tag, d.file, d.symbol))
      [ ("_init", 12); ("_fini", 13) ]
  in
  { arrays = List.rev !arrays; functions }

let tag_count t = List.length t.functions + (2 * List.length t.arrays)

let tags ~placements ~objects t =
  List.map
    (fun (tag, file, index) ->
      let s = objects.(file).E.symbols.(index) in
      let p : Layout.placement = Option.get placements.(file).(s.section) in
      (tag, p.address + Int64.to_int s.value))
    t.functions
  @ List.concat_map
      (fun (kind, members) ->
        let first = ref max_int and last = ref 0 in
        Array.iter
          (fun (file, index) ->
            let p : Layout.placement = Option.get placements.(file).(index) in
            first := min !first p.address;
            last := max !last (p.address + objects.(file).E.sections.(index).size))
          members;
        let address, size =
          match kind with 14 -> (25, 27) | 15 -> (26, 28) | _ -> (32, 33)
        in
        [ (address, !first); (size, !last - !first) ])
      t.arrays
