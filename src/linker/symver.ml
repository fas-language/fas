(* GNU assembler encodes symbol-version aliases in ET_REL symbol names. Keep
   those names for diagnostics and add resolution aliases for default versions. *)
module E = Elf_reader

type version = { name : string; version : string; default : bool }
type t = { aliases : (string, string) Hashtbl.t }

let parse name =
  match String.index_opt name '@' with
  | None -> None
  | Some at ->
      let default = at + 1 < String.length name && name.[at + 1] = '@' in
      let start = at + if default then 2 else 1 in
      let version = String.sub name start (String.length name - start) in
      if version = "" then
        Error.fail "empty/base symbol version aliases are unsupported: %s" name;
      if at = 0 || String.contains version '@' || (default && version = "") then
        Error.fail "invalid symbol version alias %s" name;
      Some { name = String.sub name 0 at; version; default }

let aliases name =
  match parse name with
  | Some v when v.default -> [ v.name; v.name ^ "@" ^ v.version ]
  | _ -> []

let expand (object_ : E.t) =
  if
    not
      (Array.exists
         (fun (s : E.symbol) -> s.binding <> 0 && String.contains s.name '@')
         object_.symbols)
  then object_
  else begin
    let definitions = Hashtbl.create ~random:true 16 in
    Array.iter
      (fun (s : E.symbol) ->
        if s.binding <> 0 && s.section <> 0 then Hashtbl.replace definitions s.name s)
      object_.symbols;
    let extra = ref [] in
    Array.iter
      (fun (s : E.symbol) ->
        if s.binding <> 0 then
          match parse s.name with
          | Some v when v.default ->
              if s.section = 0 then
                Error.fail "%s: undefined default symbol version %s" object_.path s.name;
              List.iter
                (fun name ->
                  match Hashtbl.find_opt definitions name with
                  | Some old
                    when old.section = s.section && old.value = s.value
                         && old.kind = s.kind ->
                      ()
                  | Some _ ->
                      Error.fail "%s: conflicting default symbol version %s"
                        object_.path name
                  | None ->
                      let alias = { s with name } in
                      Hashtbl.add definitions name alias;
                      extra := alias :: !extra)
                (aliases s.name)
          | _ -> ())
      object_.symbols;
    match !extra with
    | [] -> object_
    | extra ->
        {
          object_ with
          symbols = Array.append object_.symbols (Array.of_list (List.rev extra));
        }
  end

let plan ~objects ~definition =
  let aliases = Hashtbl.create ~random:true 16 in
  Array.iteri
    (fun file (o : E.t) ->
      Array.iteri
        (fun index (s : E.symbol) ->
          if s.binding <> 0 && definition s.name = Some (file, index) then
            match parse s.name with
            | Some v when v.default ->
                List.iter
                  (fun alias ->
                    match definition alias with
                    | Some (source, symbol) ->
                        let target = objects.(source).E.symbols.(symbol) in
                        if
                          source = file && target.section = s.section
                          && target.value = s.value && target.kind = s.kind
                        then Hashtbl.replace aliases alias s.name
                    | None -> ())
                  (v.name :: [ v.name ^ "@" ^ v.version ])
            | _ -> ())
        o.symbols)
    objects;
  { aliases }

let key t name = Option.value (Hashtbl.find_opt t.aliases name) ~default:name
let alias t name = Hashtbl.mem t.aliases name
let version t name = parse (key t name)

let name t original =
  match version t original with Some v -> v.name | None -> original
