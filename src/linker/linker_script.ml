(* Deliberately bounded implicit input-script grammar, sufficient for the pinned
   glibc sysroot. Unknown commands are errors, never silently ignored. *)
type item = File of string | As_needed of item list
type command = Input of item list | Group of item list | Search_dir of string
type token = Word of string | Left | Right | Comma | Semi | End

let is_script data =
  String.length data <> 0
  && String.for_all
       (fun c ->
         c = '\n' || c = '\r' || c = '\t' || (Char.code c >= 32 && Char.code c <> 127))
       data

let parse ~path data =
  if String.length data > 4 * 1024 * 1024 then
    Error.fail "%s: script exceeds 4 MiB" path;
  let cursor = ref 0 and tokens = ref 0 and length = String.length data in
  let fail fmt =
    Printf.ksprintf
      (fun message -> Error.fail "%s: linker script byte %d: %s" path !cursor message)
      fmt
  in
  let starts text =
    !cursor + String.length text <= length
    && String.sub data !cursor (String.length text) = text
  in
  let rec space () =
    if !cursor < length then
      match data.[!cursor] with
      | ' ' | '\n' | '\r' | '\t' ->
          incr cursor;
          space ()
      | '#' ->
          while !cursor < length && data.[!cursor] <> '\n' do
            incr cursor
          done;
          space ()
      | '/' when starts "/*" ->
          cursor := !cursor + 2;
          while !cursor < length && not (starts "*/") do
            incr cursor
          done;
          if !cursor = length then fail "unterminated comment";
          cursor := !cursor + 2;
          space ()
      | _ -> ()
  in
  let next () =
    space ();
    incr tokens;
    if !tokens > 100_000 then fail "token count exceeds 100000";
    if !cursor = length then End
    else begin
      let c = data.[!cursor] in
      incr cursor;
      match c with
      | '(' -> Left
      | ')' -> Right
      | ',' -> Comma
      | ';' -> Semi
      | ('"' | '\'') as quote ->
          let text = Buffer.create 32 in
          while !cursor < length && data.[!cursor] <> quote do
            if data.[!cursor] = '\\' then begin
              incr cursor;
              if !cursor = length then fail "unterminated quoted escape"
            end;
            Buffer.add_char text data.[!cursor];
            incr cursor
          done;
          if !cursor = length then fail "unterminated quoted filename";
          incr cursor;
          Word (Buffer.contents text)
      | _ ->
          let start = !cursor - 1 in
          while
            !cursor < length
            && (not
                  (List.mem data.[!cursor]
                     [ ' '; '\n'; '\r'; '\t'; '('; ')'; ','; ';'; '"'; '\'' ]))
            && not (starts "/*")
          do
            incr cursor
          done;
          Word (String.sub data start (!cursor - start))
    end
  in
  let look = ref (next ()) in
  let advance () = look := next () in
  let expect token = if !look <> token then fail "unexpected token" else advance () in
  let rec items depth =
    if depth > 16 then fail "AS_NEEDED nesting exceeds 16";
    let rec loop collected =
      match !look with
      | Right ->
          advance ();
          List.rev collected
      | End -> fail "missing closing parenthesis"
      | Comma ->
          advance ();
          loop collected
      | Word "AS_NEEDED" ->
          advance ();
          expect Left;
          let children = items (depth + 1) in
          loop (As_needed children :: collected)
      | Word name ->
          if name = "" || String.length name > 65535 || String.contains name '\000' then
            fail "invalid or oversized filename";
          advance ();
          loop (File name :: collected)
      | _ -> fail "expected input filename"
    in
    loop []
  in
  let rec commands collected =
    match !look with
    | End -> List.rev collected
    | Semi ->
        advance ();
        commands collected
    | Word ("INPUT" as name) | Word ("GROUP" as name) ->
        advance ();
        expect Left;
        let children = items 0 in
        let command = if name = "INPUT" then Input children else Group children in
        commands (command :: collected)
    | Word "OUTPUT_FORMAT" ->
        advance ();
        expect Left;
        (match !look with
        | Word "elf64-x86-64" -> advance ()
        | _ -> fail "only OUTPUT_FORMAT(elf64-x86-64) is supported");
        expect Right;
        commands collected
    | Word "SEARCH_DIR" ->
        advance ();
        expect Left;
        let path =
          match !look with
          | Word path when path <> "" && not (String.contains path '\000') ->
              if String.length path > 65535 then fail "oversized search directory";
              advance ();
              path
          | _ -> fail "expected search directory"
        in
        expect Right;
        commands (Search_dir path :: collected)
    | Word command -> fail "unsupported command %s" command
    | _ -> fail "expected INPUT, GROUP, OUTPUT_FORMAT or SEARCH_DIR"
  in
  commands []
