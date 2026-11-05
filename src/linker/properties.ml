module E = Elf_reader

let prepare ?stats ~kept (resolved : Resolver.t) =
  let values = Hashtbl.create 8 in
  Array.iteri
    (fun file (o : E.t) ->
      let seen = ref false in
      Array.iter
        (fun (s : E.section) ->
          if s.kind = 7 && s.name = ".note.gnu.property" then begin
            if !seen then Error.fail "%s: multiple GNU property notes" o.path;
            seen := true;
            kept.(file).(s.index) <- false;
            List.iter
              (fun (kind, value) ->
                match Hashtbl.find_opt values kind with
                | None -> Hashtbl.add values kind (value, 1)
                | Some (previous, count) ->
                    let value =
                      match Gnu_property.operation kind with
                      | `And -> Int64.logand previous value
                      | `Or | `Or_and -> Int64.logor previous value
                    in
                    Hashtbl.replace values kind (value, count + 1))
              (Gnu_property.parse ~path:o.path ~offset:s.offset ~size:s.size o.data)
          end)
        o.sections)
    resolved.objects;
  let properties =
    Hashtbl.fold
      (fun kind (value, count) properties ->
        let present =
          match Gnu_property.operation kind with
          | `Or -> true
          | `And | `Or_and -> count = resolved.input_count
        in
        if present && value <> 0L then (kind, value) :: properties else properties)
      values []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  if properties = [] then (resolved, kept, None)
  else
    let data = Gnu_property.encode properties in
    let section : E.section =
      {
        index = 1;
        name = ".note.gnu.property";
        kind = 7;
        flags = 2;
        offset = 0;
        size = String.length data;
        link = 0;
        info = 0;
        alignment = 8;
        entsize = 0;
      }
    in
    let object_ : E.t =
      {
        path = "<GNU properties>";
        data;
        sections =
          [|
            { section with index = 0; name = ""; kind = 0; flags = 0; size = 0 };
            section;
          |];
        symbols = [||];
        relocations = [||];
        groups = [||];
      }
    in
    let file = Array.length resolved.objects in
    Stats.add stats "gnu_properties" (List.length properties);
    ( {
        resolved with
        objects = Array.append resolved.objects [| object_ |];
        discarded = Array.append resolved.discarded [| [| false; false |] |];
      },
      Array.append kept [| [| false; true |] |],
      Some file )
