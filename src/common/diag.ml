type severity = Error | Warning
type issue = General | Not_constant

type t = {
  severity : severity;
  issue : issue;
  primary : Span.t;
  message : string;
  notes : string list;
  help : string option;
}

let error ?(issue = General) ?(notes = []) ?help primary message =
  { severity = Error; issue; primary; message; notes; help }

let warning ?(notes = []) ?help primary message =
  { severity = Warning; issue = General; primary; message; notes; help }

let local_declaration_message ?variable name =
  if List.mem name [ "let"; "auto"; "mut" ] then
    Some
      (Printf.sprintf
         "locals are declared as `name Type = value`; `%s` is not a Fas keyword" name)
  else
    match (Names.c_fixed_width_equivalent name, variable) with
    | Some equivalent, Some variable ->
        Some
          (Printf.sprintf
             "`%s` is not a Fas type; locals are declared as `name Type = value`, e.g. \
              `%s %s`"
             name variable equivalent)
    | _ -> (
        match Names.c_type_equivalent name with
        | Some equivalent ->
            Some
              (Printf.sprintf
                 "`%s` is not a Fas type; locals are declared as `name Type = value`, \
                  e.g. `x %s`"
                 name equivalent)
        | None -> None)

let is_local_declaration_message message =
  List.exists
    (fun name -> local_declaration_message name = Some message)
    [ "let"; "auto"; "mut" ]
  || List.exists
       (fun name ->
         String.starts_with
           ~prefix:
             (Printf.sprintf "`%s` is not a Fas type; locals are declared as" name)
           message)
       (Names.c_type_words @ List.map fst Names.c_fixed_width_types)

let local_type_error ?span ?ty_span name result =
  Result.map_error
    (List.map (fun diagnostic ->
         let prefix = "unknown type `" in
         if not (String.starts_with ~prefix diagnostic.message) then diagnostic
         else
           let other =
             String.sub diagnostic.message (String.length prefix)
               (String.length diagnostic.message - String.length prefix - 1)
           in
           let declared_by_name = local_declaration_message ~variable:other name in
           let declared_by_type =
             match declared_by_name with
             | Some _ -> None
             | None -> local_declaration_message ~variable:name other
           in
           let message =
             match (declared_by_name, declared_by_type) with
             | Some message, _ | None, Some message -> message
             | None, None when Names.parser_type_name name ->
                 Printf.sprintf
                   "locals are declared as `name Type = value`; write `%s %s`" other
                   name
             | None, None -> diagnostic.message
           in
           let primary =
             match (declared_by_type, ty_span, span) with
             | Some _, Some (ty_span : Span.t), _ when message <> diagnostic.message ->
                 {
                   ty_span with
                   Span.end_offset = ty_span.start_offset + String.length other;
                 }
             | _, _, Some (span : Span.t) when message <> diagnostic.message ->
                 { span with Span.end_offset = span.start_offset + String.length name }
             | _ -> diagnostic.primary
           in
           { diagnostic with primary; message }))
    result

let declaration_type_message ~declaration ~variable ?length name =
  let equivalent = Names.c_type_equivalent name in
  Option.map
    (fun equivalent ->
      let example =
        match length with
        | Some length -> Printf.sprintf "%s arr[%s,%s]" variable length equivalent
        | None -> Printf.sprintf "%s %s" variable equivalent
      in
      Printf.sprintf "`%s` is not a Fas type; %s are declared as `name Type`, e.g. `%s`"
        name declaration example)
    equivalent

let is_continuation byte = Char.code byte land 0xC0 = 0x80

let code_points text from upto =
  let count = ref 0 in
  for i = from to upto - 1 do
    if not (is_continuation text.[i]) then incr count
  done;
  !count

let caret_padding line bytes =
  let padding = Buffer.create bytes in
  for i = 0 to bytes - 1 do
    match line.[i] with
    | '\t' -> Buffer.add_char padding '\t'
    | byte when is_continuation byte -> ()
    | _ -> Buffer.add_char padding ' '
  done;
  Buffer.contents padding

let render_one ~source diagnostic =
  let level = if diagnostic.severity = Warning then "warning" else "error" in
  let heading =
    if Span.is_synthetic diagnostic.primary then level
    else Span.to_string diagnostic.primary ^ ": " ^ level
  in
  let excerpt =
    match source with
    | None -> ""
    | Some src when Source.file src = diagnostic.primary.Span.file -> (
        match Source.line_text src diagnostic.primary.Span.line with
        | None -> ""
        | Some line ->
            let text = Source.text src in
            let offset_for_line line =
              let rec find current offset =
                if current >= line then offset
                else
                  match String.index_from_opt text offset '\n' with
                  | Some newline -> find (current + 1) (newline + 1)
                  | None -> String.length text
              in
              find 1 0
            in
            let offset_only =
              diagnostic.primary.Span.start_offset = 0
              && diagnostic.primary.Span.end_offset = 0
            in
            let line_start =
              if offset_only then offset_for_line diagnostic.primary.Span.line
              else
                max 0
                  (diagnostic.primary.Span.start_offset
                  - (diagnostic.primary.Span.column - 1))
            in
            let line_stop =
              match String.index_from_opt text line_start '\n' with
              | Some stop -> stop
              | None -> String.length text
            in
            let start =
              if offset_only then
                min line_stop (line_start + diagnostic.primary.Span.column - 1)
              else max line_start (min line_stop diagnostic.primary.Span.start_offset)
            in
            let stop =
              if offset_only then min line_stop (start + 1)
              else max start (min line_stop diagnostic.primary.Span.end_offset)
            in
            let width = max 1 (code_points text start stop) in
            let prefix_length = max 0 (min (String.length line) (start - line_start)) in
            let prefix = caret_padding line prefix_length in
            Printf.sprintf "\n  %s\n  %s^%s\n" line prefix (String.make (width - 1) '~')
        )
    | Some _ -> ""
  in
  let notes = List.map (fun n -> Printf.sprintf "note: %s\n" n) diagnostic.notes in
  let help =
    match diagnostic.help with
    | None -> ""
    | Some text ->
        Printf.sprintf "%shelp: %s\n" (if excerpt = "" then "\n" else "") text
  in
  let notes =
    if excerpt = "" && help = "" && notes <> [] then "\n" :: notes else notes
  in
  Printf.sprintf "%s: %s%s%s%s" heading diagnostic.message excerpt help
    (String.concat "" notes)

let render ~source diagnostic = render_one ~source diagnostic

let render_all ~source diagnostics =
  String.concat "\n" (List.map (render_one ~source) diagnostics)

let json_string = Json.quote

let json_diagnostic diagnostic =
  let span = diagnostic.primary in
  let location =
    if Span.is_synthetic span then
      [
        ("file", "null");
        ("line", "null");
        ("column", "null");
        ("start_offset", "null");
        ("end_offset", "null");
      ]
    else
      [
        ("file", json_string span.Span.file);
        ("line", string_of_int span.Span.line);
        ("column", string_of_int span.Span.column);
        ("start_offset", string_of_int span.Span.start_offset);
        ("end_offset", string_of_int span.Span.end_offset);
      ]
  in
  let severity = if diagnostic.severity = Warning then "warning" else "error" in
  let fields =
    (("severity", json_string severity) :: location)
    @ [
        ("message", json_string diagnostic.message);
        ("notes", "[" ^ String.concat "," (List.map json_string diagnostic.notes) ^ "]");
        ( "help",
          match diagnostic.help with None -> "null" | Some text -> json_string text );
      ]
  in
  "{"
  ^ String.concat ","
      (List.map (fun (key, value) -> json_string key ^ ":" ^ value) fields)
  ^ "}"

let render_json diagnostics =
  "{\"diagnostics\":[" ^ String.concat "," (List.map json_diagnostic diagnostics) ^ "]}"
