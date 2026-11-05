(* GNU-style argument quoting, without shell substitution or comment syntax.
   All paths, including nested @files, are relative to the working directory. *)
let tokenize ~path data =
  let words = ref [] and word = Buffer.create 64 in
  let active = ref false and quote = ref None and index = ref 0 in
  let push () =
    if !active then begin
      words := Buffer.contents word :: !words;
      Buffer.clear word;
      active := false
    end
  in
  while !index < String.length data do
    let c = data.[!index] in
    if c = '\000' then Error.fail "%s: NUL in response file" path;
    (match (c, !quote) with
    | '\\', _ ->
        incr index;
        if !index >= String.length data then
          Error.fail "%s: trailing backslash in response file" path;
        if data.[!index] = '\000' then Error.fail "%s: NUL in response file" path;
        Buffer.add_char word data.[!index];
        active := true
    | ('\'' | '"'), None ->
        quote := Some c;
        active := true
    | c, Some q when c = q -> quote := None
    | (' ' | '\t' | '\r' | '\n' | '\012'), None -> push ()
    | _ ->
        Buffer.add_char word c;
        active := true);
    incr index
  done;
  if !quote <> None then Error.fail "%s: unterminated quote in response file" path;
  push ();
  List.rev !words

let expand arguments =
  let bytes = ref 0 and count = ref 0 in
  let rec expand stack argument =
    if String.starts_with ~prefix:"@" argument then begin
      if List.length stack >= 16 then Error.fail "response file nesting exceeds 16";
      let path = String.sub argument 1 (String.length argument - 1) in
      let identity = Error.io (fun () -> Unix.realpath path) in
      if List.mem identity stack then Error.fail "%s: recursive response file" path;
      let data = File_io.read ~max_size:((16 * 1024 * 1024) - !bytes) path in
      bytes := !bytes + String.length data;
      tokenize ~path data |> List.concat_map (expand (identity :: stack))
    end
    else begin
      incr count;
      if !count > 100_000 then Error.fail "expanded argument count exceeds 100000";
      [ argument ]
    end
  in
  List.concat_map (expand []) arguments
