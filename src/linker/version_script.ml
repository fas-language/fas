(* Deliberately small VERSION grammar: named/anonymous nodes, one earlier parent,
   exact names and terminal '*' patterns. Reject other glob/demangling syntax. *)
type rule = { pattern : string; local : bool; version : int }
type node = { name : string; parent : string option }
type t = { path : string; nodes : node array; rules : rule list }
type token = Word of string | Punct of char | End
type binding = Local | Global of int

let parse ~path data =
  if String.length data > 1024 * 1024 then
    Error.fail "%s: version script exceeds 1 MiB" path;
  let pos = ref 0 and count = ref 0 and length = String.length data in
  let fail fmt =
    Printf.ksprintf
      (fun s -> Error.fail "%s: version script byte %d: %s" path !pos s)
      fmt
  in
  let starts text =
    !pos + String.length text <= length
    && String.sub data !pos (String.length text) = text
  in
  let rec space () =
    if !pos < length then
      match data.[!pos] with
      | ' ' | '\n' | '\r' | '\t' ->
          incr pos;
          space ()
      | '#' ->
          while !pos < length && data.[!pos] <> '\n' do
            incr pos
          done;
          space ()
      | '/' when starts "/*" ->
          pos := !pos + 2;
          while !pos < length && not (starts "*/") do
            incr pos
          done;
          if !pos = length then fail "unterminated comment";
          pos := !pos + 2;
          space ()
      | _ -> ()
  in
  let next () =
    space ();
    incr count;
    if !count > 10000 then fail "token count exceeds 10000";
    if !pos = length then End
    else if List.mem data.[!pos] [ '{'; '}'; ':'; ';' ] then begin
      let c = data.[!pos] in
      incr pos;
      Punct c
    end
    else begin
      let start = !pos in
      while
        !pos < length
        && (not (List.mem data.[!pos] [ ' '; '\n'; '\r'; '\t'; '{'; '}'; ':'; ';' ]))
        && not (starts "/*")
      do
        incr pos
      done;
      let word = String.sub data start (!pos - start) in
      if word = "" || String.length word > 65535 then fail "invalid word";
      Word word
    end
  in
  let look = ref (next ()) in
  let advance () = look := next () in
  let expect c = if !look <> Punct c then fail "expected %c" c else advance () in
  let identifier word =
    if
      word = ""
      || not
           (String.for_all
              (function
                | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '-' -> true
                | _ -> false)
              word)
    then fail "unsupported version name %S" word
  in
  let names = Hashtbl.create 16 and nodes = ref [] and rules = ref [] in
  let rule_count = ref 0 and anonymous = ref false in
  while !look <> End do
    if List.length !nodes >= 256 then fail "node count exceeds 256";
    let name =
      match !look with
      | Word name ->
          identifier name;
          advance ();
          name
      | Punct '{' ->
          anonymous := true;
          ""
      | _ -> fail "expected version node"
    in
    if Hashtbl.mem names name then fail "duplicate version node %s" name;
    if !anonymous && (!nodes <> [] || name <> "") then
      fail "anonymous node must stand alone";
    let version = if name = "" then 1 else List.length !nodes + 2 in
    expect '{';
    let local = ref false in
    while !look <> Punct '}' do
      match !look with
      | Word ("global" as section) | Word ("local" as section) ->
          advance ();
          expect ':';
          local := section = "local"
      | Word pattern ->
          if
            String.exists
              (fun c -> List.mem c [ '?'; '['; ']'; '"'; '\''; '\\'; '@'; '('; ')' ])
              pattern
          then fail "unsupported pattern %S" pattern;
          String.iteri
            (fun n c ->
              if c = '*' && n <> String.length pattern - 1 then
                fail "only terminal '*' patterns are supported")
            pattern;
          incr rule_count;
          if !rule_count > 1024 then fail "pattern count exceeds 1024";
          rules := { pattern; local = !local; version } :: !rules;
          advance ();
          expect ';'
      | _ -> fail "expected symbol pattern or closing brace"
    done;
    advance ();
    let parent =
      match !look with
      | Word parent ->
          if name = "" || not (Hashtbl.mem names parent) then
            fail "parent %s must name an earlier node" parent;
          advance ();
          Some parent
      | _ -> None
    in
    expect ';';
    Hashtbl.add names name ();
    nodes := { name; parent } :: !nodes
  done;
  if !nodes = [] then fail "empty script";
  { path; nodes = Array.of_list (List.rev !nodes); rules = List.rev !rules }

let named t = Array.length t.nodes <> 0 && t.nodes.(0).name <> ""

let exact_local t =
  let locals = Hashtbl.create ~random:true 16
  and globals = Hashtbl.create ~random:true 16 in
  List.iter
    (fun rule ->
      if not (String.contains rule.pattern '*') then
        Hashtbl.replace (if rule.local then locals else globals) rule.pattern ())
    t.rules;
  fun name -> Hashtbl.mem locals name && not (Hashtbl.mem globals name)

let policy t =
  let cache = Hashtbl.create 64 and work = ref 0 in
  fun name ->
    match Hashtbl.find_opt cache name with
    | Some binding -> binding
    | None ->
        let selected = ref None in
        List.iter
          (fun rule ->
            work :=
              Binary.add
                ~context:(t.path ^ ": version matching work")
                ~limit:(64 * 1024 * 1024)
                !work
                (String.length name + String.length rule.pattern);
            let wildcard = String.ends_with ~suffix:"*" rule.pattern in
            let prefix =
              if wildcard then String.sub rule.pattern 0 (String.length rule.pattern - 1)
              else rule.pattern
            in
            let matches =
              if wildcard then String.starts_with ~prefix name else name = prefix
            in
            if matches then begin
              let rank = if wildcard then String.length prefix else max_int in
              match !selected with
              | Some (old_rank, _) when rank < old_rank -> ()
              | Some (old_rank, old) when rank = old_rank ->
                  if (not old.local) && (not rule.local) && old.version <> rule.version
                  then Error.fail "%s: ambiguous versions for %s" t.path name;
                  if not rule.local then selected := Some (rank, rule)
              | _ -> selected := Some (rank, rule)
            end)
          t.rules;
        let binding =
          match !selected with
          | None -> Global 1
          | Some (_, rule) -> if rule.local then Local else Global rule.version
        in
        Hashtbl.add cache name binding;
        binding
