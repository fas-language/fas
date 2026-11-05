module E = Elf_reader

let identifier name =
  let first = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false in
  let rest = function '0' .. '9' -> true | c -> first c in
  String.length name <> 0 && first name.[0] && String.for_all rest name

let boundary name =
  let suffix prefix last =
    if String.starts_with ~prefix name then
      let section =
        String.sub name (String.length prefix)
          (String.length name - String.length prefix)
      in
      if identifier section then Some (section, last) else None
    else None
  in
  match suffix "__start_" false with
  | Some _ as result -> result
  | None -> suffix "__stop_" true

let eligible (s : E.section) =
  identifier s.name
  && List.mem s.kind [ 1; 8 ]
  && s.flags land 2 <> 0
  && s.flags land 0x400 = 0

let signature (s : E.section) =
  (s.kind, s.flags land lnot (0x200 lor 0x200000), s.entsize)

(* Loaded contributions only: boundary references do not by themselves extract
   archive members. COMDAT losers are excluded before checking compatibility. *)
let collect ~discarded objects =
  let groups = Hashtbl.create ~random:true 16 in
  Array.iteri
    (fun file (o : E.t) ->
      Array.iter
        (fun (s : E.section) ->
          if identifier s.name && not discarded.(file).(s.index) then
            let previous = Option.value (Hashtbl.find_opt groups s.name) ~default:[] in
            Hashtbl.replace groups s.name ((file, s) :: previous))
        o.sections)
    objects;
  groups

let available groups name =
  match boundary name with
  | None -> false
  | Some (section, _) -> (
      match Hashtbl.find_opt groups section with
      | None -> false
      | Some [] -> assert false
      | Some ((_, first) :: _ as members) ->
          List.iter
            (fun (_, s) ->
              if not (eligible s) then
                Error.fail
                  "%s: section boundaries require allocatable non-TLS PROGBITS or \
                   NOBITS section %s"
                  name section;
              if signature s <> signature first then
                Error.fail "%s: incompatible contributions to registry section %s" name
                  section)
            members;
          true)
