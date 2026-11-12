let ident_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

let word_at text i word =
  let n = String.length text and m = String.length word in
  i + m <= n
  && String.sub text i m = word
  && (i + m = n || not (ident_char text.[i + m]))
  && (i = 0 || not (ident_char text.[i - 1]))

let skip_space_comments text i =
  let n = String.length text in
  let rec loop j =
    if j >= n then j
    else if text.[j] = ' ' || text.[j] = '\t' || text.[j] = '\r' || text.[j] = '\n' then
      loop (j + 1)
    else if j + 1 < n && text.[j] = '/' && text.[j + 1] = '/' then
      let rec line k = if k < n && text.[k] <> '\n' then line (k + 1) else k in
      loop (line (j + 2))
    else if j + 1 < n && text.[j] = '/' && text.[j + 1] = '*' then
      let rec block k =
        if k + 1 < n && not (text.[k] = '*' && text.[k + 1] = '/') then block (k + 1)
        else min n (k + 2)
      in
      loop (block (j + 2))
    else j
  in
  loop i

let blank_range buffer text start stop =
  for i = start to stop - 1 do
    if text.[i] <> '\n' then Bytes.set buffer i ' '
  done

let extract_containers source =
  let text = Source.text source and n = Source.length source in
  let buffer = Bytes.of_string text in
  let c_containers = ref [] in
  let at_line_start i =
    i = 0
    ||
    let start =
      1 + Option.value ~default:(-1) (String.rindex_from_opt text (i - 1) '\n')
    in
    String.for_all
      (function ' ' | '\t' | '\r' -> true | _ -> false)
      (String.sub text start (i - start))
  in
  let line_end i = Option.value ~default:n (String.index_from_opt text i '\n') in
  let rec skip_while stop predicate i =
    if i < stop && predicate text.[i] then skip_while stop predicate (i + 1) else i
  in
  let container_prefix i =
    let end_pos = line_end i in
    let stop =
      if end_pos > i && text.[end_pos - 1] = '\r' then end_pos - 1 else end_pos
    in
    let blank c = c = ' ' || c = '\t' in
    let j = skip_while stop blank (i + 3) in
    let width =
      if j + 3 <= stop && String.sub text j 3 = "\"C\"" then 3
      else if j + 5 <= stop && String.sub text j 5 = "\"asm\"" then 5
      else 0
    in
    if width = 0 then None
    else
      let j = skip_while stop blank (j + width) in
      if j + 2 > stop || String.sub text j 2 <> "<<" then None
      else
        let tag_start = j + 2 in
        let tag_stop = skip_while stop (fun c -> c <> ' ' && c <> '\t') tag_start in
        let tag = String.sub text tag_start (tag_stop - tag_start) in
        let valid =
          tag <> ""
          && ident_char tag.[0]
          && (not (tag.[0] >= '0' && tag.[0] <= '9'))
          && String.for_all ident_char tag
        in
        if not valid then Some (Error "C container tag must be an ASCII identifier")
        else if tag_stop <> stop then Some (Error "trailing text after C container tag")
        else Some (Ok tag)
  in
  let find_terminator tag start =
    let rec scan line_start =
      let stop = line_end line_start in
      let line = String.sub text line_start (stop - line_start) in
      if line = tag || line = tag ^ "\r" then Some (line_start, min n (stop + 1))
      else if stop = n then None
      else scan (stop + 1)
    in
    scan start
  in
  let container_error i message =
    Error
      [ Diag.error (Source.span source ~start_offset:i ~end_offset:(i + 3)) message ]
  in
  let extract_container i tag =
    let payload_start = min n (line_end i + 1) in
    match find_terminator tag payload_start with
    | None ->
        let stop = line_end i in
        let path_start = skip_while stop (fun c -> c = ' ' || c = '\t') (i + 3) in
        let kind =
          if path_start + 5 <= stop && String.sub text path_start 5 = "\"asm\"" then
            "assembly unit"
          else "C container"
        in
        container_error i (kind ^ " is missing terminator `" ^ tag ^ "`")
    | Some (terminator_start, after_terminator) ->
        c_containers :=
          ( i,
            Ast.
              {
                tag;
                text = String.sub text payload_start (terminator_start - payload_start);
              } )
          :: !c_containers;
        blank_range buffer text payload_start after_terminator;
        Ok after_terminator
  in
  let rec scan i =
    if i >= n then Ok (Bytes.to_string buffer, List.rev !c_containers)
    else if text.[i] = '"' || text.[i] = '\'' then
      let quote = text.[i] in
      let rec skip j =
        if j >= n then n
        else if text.[j] = '\\' then skip (j + 2)
        else if text.[j] = quote then j + 1
        else skip (j + 1)
      in
      scan (skip (i + 1))
    else if i + 1 < n && text.[i] = '/' && text.[i + 1] = '/' then
      let rec skip j = if j < n && text.[j] <> '\n' then skip (j + 1) else j in
      scan (skip (i + 2))
    else if i + 1 < n && text.[i] = '/' && text.[i + 1] = '*' then
      let rec skip j =
        if j + 1 < n && not (text.[j] = '*' && text.[j + 1] = '/') then skip (j + 1)
        else min n (j + 2)
      in
      scan (skip (i + 2))
    else if text.[i] = 'u' && word_at text i "use" && at_line_start i then
      match container_prefix i with
      | None -> scan (i + 1)
      | Some (Error message) -> container_error i message
      | Some (Ok tag) -> Result.bind (extract_container i tag) scan
    else if word_at text i "asm" && word_at text (skip_space_comments text (i + 3)) "fn"
    then
      container_error i
        "asm fn was removed in v0.3; use a use \"asm\" unit and an extern \"C\" \
         declaration"
    else scan (i + 1)
  in
  scan 0

module P = struct
  type t = {
    tokens : Token.t array;
    mutable pos : int;
    c_containers : (int * Ast.c_fragment) list;
    limits : Limits.t;
    mutable depth : int;
    mutable nesting : int;
    mutable block_expression_depth : int option;
    mutable delimited_depth : int;
  }

  let skip_delimited_newlines p =
    while
      p.delimited_depth > 0
      && p.pos < Array.length p.tokens - 1
      && p.tokens.(p.pos).Token.kind = Token.Newline
    do
      p.pos <- p.pos + 1
    done

  let peek p =
    skip_delimited_newlines p;
    p.tokens.(min p.pos (Array.length p.tokens - 1))

  let peek_n p n =
    skip_delimited_newlines p;
    let rec find i remaining =
      if i >= Array.length p.tokens - 1 then p.tokens.(Array.length p.tokens - 1)
      else if p.delimited_depth > 0 && p.tokens.(i).Token.kind = Token.Newline then
        find (i + 1) remaining
      else if remaining = 0 then p.tokens.(i)
      else find (i + 1) (remaining - 1)
    in
    find p.pos n

  let bump p =
    let t = peek p in
    if p.pos < Array.length p.tokens - 1 then p.pos <- p.pos + 1;
    t

  let at p k = (peek p).Token.kind = k

  let eat p k =
    if at p k then (
      ignore (bump p);
      true)
    else false

  let expected p kind =
    if eat p kind then Ok ()
    else
      Error
        [
          Diag.error (peek p).span
            ("expected " ^ Token.show kind ^ ", found " ^ Token.show (peek p).kind);
        ]

  let ident p =
    let token = bump p in
    match token.kind with
    | Token.Ident s -> Ok s
    | t ->
        Error [ Diag.error token.span ("expected identifier, found " ^ Token.show t) ]

  let field_ident p =
    let token = bump p in
    match token.kind with
    | Token.Ident name -> Ok (name, token.span)
    | ( Token.Kw_fn | Token.Kw_return | Token.Kw_if | Token.Kw_else | Token.Kw_while
      | Token.Kw_break | Token.Kw_continue | Token.Kw_const | Token.Kw_var
      | Token.Kw_struct | Token.Kw_opaque | Token.Kw_extern | Token.Kw_defer
      | Token.Kw_use | Token.Kw_for | Token.Kw_switch | Token.Kw_case | Token.Kw_default
        ) as kind ->
        let shown = Token.show kind in
        Ok (String.sub shown 1 (String.length shown - 2), token.span)
    | kind ->
        Error
          [ Diag.error token.span ("expected identifier, found " ^ Token.show kind) ]

  let skip_newlines p =
    while at p Token.Newline do
      ignore (bump p)
    done

  let delimited p parse =
    let previous = p.delimited_depth in
    p.delimited_depth <- previous + 1;
    let result = parse () in
    p.delimited_depth <- previous;
    result

  let end_stmt p =
    if eat p Token.Semi then (
      skip_newlines p;
      Ok ())
    else if at p Token.Newline then (
      skip_newlines p;
      Ok ())
    else if at p Token.Rbrace || at p Token.Eof then Ok ()
    else Error [ Diag.error (peek p).span "expected end of statement (newline or `;`)" ]

  let string p =
    match (bump p).kind with
    | Token.String s -> Ok s
    | t ->
        Error
          [
            Diag.error (peek p).span ("expected string literal, found " ^ Token.show t);
          ]

  let integer p =
    match (bump p).kind with
    | Token.Int s -> Ok s
    | t ->
        Error [ Diag.error (peek p).span ("expected integer, found " ^ Token.show t) ]

  let int_value p =
    match integer p with
    | Error e -> Error e
    | Ok s -> (
        try Ok (int_of_string s)
        with Failure _ -> Error [ Diag.error (peek p).span "integer is out of range" ])

  let bind r f = match r with Error e -> Error e | Ok x -> f x
  let ( let* ) = bind
  let span p = (peek p).Token.span

  let extent p (start : Span.t) =
    let rec last i =
      if i < 0 then start
      else
        match p.tokens.(i).Token.kind with
        | Token.Newline | Token.Semi | Token.Eof -> last (i - 1)
        | _ -> p.tokens.(i).Token.span
    in
    let stop = last (p.pos - 1) in
    if stop.Span.end_offset < start.Span.start_offset then start
    else { start with Span.end_offset = stop.Span.end_offset }

  let c_type_word name = name = "void" || Names.c_type_word name

  let type_word name =
    c_type_word name || List.mem name Names.scalar_type_names || name = "addr"

  let error span message = Error [ Diag.error span message ]

  let c_array_declaration p =
    match ((peek p).kind, (peek_n p 1).kind, (peek_n p 2).kind) with
    | Token.Ident element, Token.Ident name, Token.Lbracket
      when type_word element && Names.type_constructor name = None ->
        let count =
          match (peek_n p 3).kind with Token.Int count -> count | _ -> "N"
        in
        let fas_element =
          Option.value ~default:element (Names.c_type_equivalent element)
        in
        let primary = if c_type_word element then span p else (peek_n p 1).span in
        Some
          (error primary
             (Printf.sprintf
                "C array declaration `%s %s[%s]` is not Fas syntax; write `%s \
                 arr[%s,%s]`"
                element name count name count fas_element))
    | _ -> None

  let c_declaration_syntax p =
    match ((peek p).kind, (peek_n p 1).kind, (peek_n p 2).kind) with
    | Token.Ident word, Token.Star, Token.Ident _ when c_type_word word ->
        let first = span p and star = (peek_n p 1).span in
        let primary =
          Span.make ~file:first.Span.file ~start_offset:first.Span.start_offset
            ~end_offset:star.Span.end_offset ~line:first.Span.line
            ~column:first.Span.column
        in
        let message =
          match word with
          | "char" | "void" ->
              Printf.sprintf "C type `%s*` is not a Fas type; use `addr`" word
          | _ ->
              Printf.sprintf
                "C type `%s*` is not a Fas type; use `addr`, or `handle[T]` when `T` \
                 is an `opaque` type"
                word
        in
        Some (error primary message)
    | Token.Ident word, Token.Lparen, Token.Star when c_type_word word ->
        Some
          (error (span p)
             "C function pointer declarations are not Fas syntax; carry a function \
              address in an `addr` and call it with `call_addr[R](p, args...)`")
    | _ -> None

  let within_nesting p s message parse =
    p.nesting <- p.nesting + 1;
    if p.nesting + p.depth > p.limits.Limits.max_nesting then (
      p.nesting <- p.nesting - 1;
      Error [ Diag.error s message ])
    else
      let result = parse () in
      p.nesting <- p.nesting - 1;
      result

  let rec ty p =
    let start = span p in
    let* t = ty_unchecked p in
    if at p Token.Star then c_pointer_type p start else Ok t

  and ty_unchecked p =
    within_nesting p (span p) "type nesting exceeds the configured limit" (fun () ->
        ty_inner p)

  and c_pointer_type p start =
    let rec stars finish =
      if at p Token.Star then stars (bump p).Token.span else finish
    in
    let finish = stars start in
    error
      { start with Span.end_offset = finish.Span.end_offset }
      "typed pointers are not Fas types; use `addr`"

  and ty_inner p =
    match (peek p).kind with
    | Token.Ident "bool" ->
        ignore (bump p);
        Ok Ast.Bool
    | Token.Ident "void" ->
        ignore (bump p);
        Ok Ast.Void
    | Token.Ident "ptr" ->
        let s = span p in
        ignore (bump p);
        Error [ Diag.error s "typed pointers are not Fas types; use `addr`" ]
    | Token.Ident name when Names.type_constructor name = Some Names.Address -> (
        let s = span p in
        ignore (bump p);
        let previous = p.pos in
        if not (at p Token.Lbracket) then Ok Ast.Addr
        else
          match generic_args p with
          | Ok _ ->
              let finish = p.tokens.(p.pos - 1).Token.span in
              error
                { s with Span.end_offset = finish.Span.end_offset }
                "typed pointers are not Fas types; use `addr`"
          | Error _ ->
              p.pos <- previous;
              Ok Ast.Addr)
    | Token.Ident "f32" ->
        ignore (bump p);
        Ok (Ast.Float Ast.F32)
    | Token.Ident "f64" ->
        ignore (bump p);
        Ok (Ast.Float Ast.F64)
    | Token.Ident name when Names.type_constructor name = Some Names.Handle ->
        ignore (bump p);
        let* () = expected p Token.Lbracket in
        delimited p (fun () ->
            let* t = ty p in
            let* () = expected p Token.Rbracket in
            Ok (Ast.Handle t))
    | Token.Ident name when Names.type_constructor name = Some Names.Array ->
        ignore (bump p);
        let* () = expected p Token.Lbracket in
        delimited p (fun () ->
            let* length = aggregate_length p in
            let* () =
              if at p Token.Rbracket then
                error (span p) "array type `arr` needs an element type after its length"
              else expected p Token.Comma
            in
            let* t = ty p in
            let* () = expected p Token.Rbracket in
            Ok (Ast.Array (length, t)))
    | Token.Ident name when Names.type_constructor name = Some Names.Vector ->
        ignore (bump p);
        let* () = expected p Token.Lbracket in
        delimited p (fun () ->
            let* length = aggregate_length p in
            let* () = expected p Token.Comma in
            let element_span = span p in
            let* t = ty p in
            let* () = expected p Token.Rbracket in
            match t with
            | Ast.Addr ->
                error element_span
                  "vector element type must be `bool` or an integer type"
            | _ -> Ok (Ast.Vec (length, t)))
    | Token.Ident s when List.mem s Names.scalar_type_names -> (
        ignore (bump p);
        match s with
        | "u8" -> Ok (Ast.Int Ast.U8)
        | "u16" -> Ok (Ast.Int Ast.U16)
        | "u32" -> Ok (Ast.Int Ast.U32)
        | "u64" -> Ok (Ast.Int Ast.U64)
        | "i8" -> Ok (Ast.Int Ast.I8)
        | "i16" -> Ok (Ast.Int Ast.I16)
        | "i32" -> Ok (Ast.Int Ast.I32)
        | "i64" -> Ok (Ast.Int Ast.I64)
        | "usize" -> Ok (Ast.Int Ast.U64)
        | "isize" -> Ok (Ast.Int Ast.I64)
        | _ -> assert false)
    | Token.Ident "char" when (peek_n p 1).kind = Token.Star ->
        let first = (peek p).span and last = (peek_n p 1).span in
        let primary =
          Span.make ~file:first.Span.file ~start_offset:first.Span.start_offset
            ~end_offset:last.Span.end_offset ~line:first.Span.line
            ~column:first.Span.column
        in
        error primary "C type `char*` is not a Fas type; use `addr`"
    | Token.Ident s ->
        let token = bump p in
        if at p Token.Lbracket then
          let span = span p in
          let* args = generic_args p in
          Ok (Ast.Applied_type (s, token.span, args, span))
        else Ok (Ast.Named_type (s, token.span))
    | t -> Error [ Diag.error (span p) ("expected a type, found " ^ Token.show t) ]

  and select_payloads p =
    delimited p (fun () ->
        let rec go acc =
          let* arg = select_payload p in
          if eat p Token.Comma then go (arg :: acc)
          else
            let* () = expected p Token.Rbracket in
            Ok (List.rev (arg :: acc))
        in
        go [])

  and select_payload p =
    match (peek p).kind with
    | Token.Ident "void" ->
        let type_span = span p in
        ignore (bump p);
        Ok (Ast.Type_arg (Ast.Named_type ("void", type_span)))
    | Token.Dot_dot ->
        let token = bump p in
        Ok (Ast.Name_arg ("..", token.span))
    | Token.Dot when (peek_n p 1).kind = Token.Dot ->
        let first = bump p in
        let last = bump p in
        let sp =
          Span.make ~file:first.span.Span.file
            ~start_offset:first.span.Span.start_offset
            ~end_offset:last.span.Span.end_offset ~line:first.span.Span.line
            ~column:first.span.Span.column
        in
        Ok (Ast.Name_arg ("..", sp))
    | _ -> generic_arg p

  and aggregate_length p =
    let start = span p in
    let nesting = p.nesting in
    p.nesting <- max 0 (nesting - 1);
    let parsed = expr p in
    p.nesting <- nesting;
    let* expression = parsed in
    let finish = p.tokens.(p.pos - 1).Token.span in
    let length_span = { start with Span.end_offset = finish.Span.end_offset } in
    Ok (Ast.aggregate_length expression length_span)

  and generic_args p =
    let* () = expected p Token.Lbracket in
    delimited p (fun () ->
        let rec go acc =
          let* arg = generic_arg p in
          if eat p Token.Comma then go (arg :: acc)
          else
            let* () = expected p Token.Rbracket in
            Ok (List.rev (arg :: acc))
        in
        go [])

  and generic_arg p =
    match ((peek p).kind, (peek_n p 1).kind) with
    | Token.Ident ("true" | "false"), _ ->
        let* e = expr p in
        Ok (Ast.Const_arg e)
    | Token.Ident ("sizeof" | "alignof" | "offsetof"), Token.Lbracket ->
        let* e = expr p in
        Ok (Ast.Const_arg e)
    | Token.Ident _, Token.Lbracket when generic_application_is_call p ->
        let* e = expr p in
        Ok (Ast.Const_arg e)
    | Token.Ident name, (Token.Comma | Token.Rbracket)
      when not (Names.parser_type_name name) ->
        let s = span p in
        ignore (bump p);
        Ok (Ast.Name_arg (name, s))
    | Token.Ident name, _ when Names.parser_type_name name ->
        let* t = ty p in
        Ok (Ast.Type_arg t)
    | Token.Ident _, Token.Lbracket ->
        let previous = p.pos in
        let* t = ty_unchecked p in
        if at p Token.Comma || at p Token.Rbracket then Ok (Ast.Type_or_index t)
        else (
          p.pos <- previous;
          let* e = expr p in
          Ok (Ast.Const_arg e))
    | _ ->
        let* e = expr p in
        Ok (Ast.Const_arg e)

  and generic_application_is_call p =
    let rec scan offset depth =
      match (peek_n p offset).kind with
      | Token.Lbracket -> scan (offset + 1) (depth + 1)
      | Token.Rbracket when depth = 1 -> (peek_n p (offset + 1)).kind = Token.Lparen
      | Token.Rbracket when depth > 1 -> scan (offset + 1) (depth - 1)
      | Token.Newline when depth > 0 -> scan (offset + 1) depth
      | Token.Eof | Token.Newline -> false
      | _ -> scan (offset + 1) depth
    in
    scan 1 0

  and starts_type p =
    match ((peek p).kind, (peek_n p 1).kind) with
    | Token.Ident _, Token.Ident _ -> true
    | _ -> false

  and starts_struct_literal p =
    let rec scan offset brackets =
      match (peek_n p offset).kind with
      | Token.Lbracket -> scan (offset + 1) (brackets + 1)
      | Token.Rbracket when brackets > 0 -> scan (offset + 1) (brackets - 1)
      | Token.Rparen when brackets = 0 -> (peek_n p (offset + 1)).kind = Token.Lbrace
      | Token.Newline -> scan (offset + 1) brackets
      | Token.Eof -> false
      | _ -> scan (offset + 1) brackets
    in
    p.block_expression_depth <> Some p.depth
    && match (peek_n p 1).kind with Token.Ident _ -> scan 1 0 | _ -> false

  and compound_op = function
    | Token.Plus_eq -> Some Ast.Add
    | Token.Minus_eq -> Some Ast.Sub
    | Token.Star_eq -> Some Ast.Mul
    | Token.Slash_eq -> Some Ast.Div
    | Token.Percent_eq -> Some Ast.Rem
    | Token.Amp_eq -> Some Ast.Bit_and
    | Token.Pipe_eq -> Some Ast.Bit_or
    | Token.Caret_eq -> Some Ast.Bit_xor
    | Token.Ltlt_eq -> Some Ast.Shl
    | Token.Gtgt_eq -> Some Ast.Shr
    | _ -> None

  and finish_statement p consume_end = if consume_end then end_stmt p else Ok ()

  and pkg_line p =
    ignore (bump p);
    match (peek p).kind with
    | Token.String modules ->
        let modules_span = span p in
        ignore (bump p);
        Ok Ast.{ modules; modules_span }
    | _ ->
        error (peek p).span
          "`pkg` must be followed by a module string, e.g. `use pkg \"sdl2\"`"

  and c_header p use_span =
    match (peek p).kind with
    | Token.String header ->
        ignore (bump p);
        Ok (Ast.C_quoted header)
    | Token.Ltlt -> (
        ignore (bump p);
        let* tag = ident p in
        match List.assoc_opt use_span.Span.start_offset p.c_containers with
        | Some fragment when fragment.Ast.tag = tag -> Ok (Ast.C_fragment fragment)
        | _ -> Error [ Diag.error use_span "C container extraction failed" ])
    | Token.Newline | Token.Eof ->
        Error [ Diag.error use_span "`use \"C\"` needs a C header name" ]
    | Token.Lt ->
        ignore (bump p);
        let rec path parts =
          match (bump p).kind with
          | Token.Gt ->
              if parts = [] then
                Error [ Diag.error use_span "C header name cannot be empty" ]
              else Ok (Ast.C_system (String.concat "" (List.rev parts)))
          | Token.Dot -> path ("." :: parts)
          | Token.Dot_dot -> path (".." :: parts)
          | Token.Slash -> path ("/" :: parts)
          | Token.Minus -> path ("-" :: parts)
          | Token.Plus -> path ("+" :: parts)
          | Token.Newline | Token.Eof ->
              Error [ Diag.error use_span "expected `>` in C header name" ]
          | kind -> (
              match Token.c_header_component kind with
              | Some component -> path (component :: parts)
              | None -> Error [ Diag.error use_span "expected `>` in C header name" ])
        in
        path []
    | Token.Ident first when (peek_n p 1).kind = Token.Dot ->
        let name =
          match (peek_n p 2).kind with
          | Token.Ident second -> first ^ "." ^ second
          | _ -> first
        in
        error (span p)
          (Printf.sprintf "C header path `%s` needs quotes or angle brackets" name)
    | _ -> Error [ Diag.error use_span "expected a quoted or angle-bracket C header" ]

  and items p =
    skip_newlines p;
    if at p Token.Eof then Ok []
    else
      let* xs = item p in
      let* ys = items p in
      Ok (xs @ ys)

  and item p =
    match (peek p).kind with
    | Token.Kw_use ->
        let s = span p in
        ignore (bump p);
        if (peek p).kind = Token.Ident "pkg" then
          let* pkg = pkg_line p in
          let span = extent p s in
          let* () = end_stmt p in
          Ok [ Ast.Pkg { pkg; span } ]
        else if
          match (peek p).kind with
          | Token.Ident ("linux" | "windows" | "wasi") -> true
          | _ -> false
        then begin
          (* `use linux "file.fas"`: a Fas file that is part of the program
             only when building for that OS. The driver skips it for the
             others, so it is never read there. Linux is the only target
             today; the form stays as the hook for future ones. *)
          let os_span = span p in
          let* os =
            match (peek p).kind with
            | Token.Ident "linux" -> Ok Target_layout.Linux
            | Token.Ident name ->
                error os_span
                  (Printf.sprintf "`%s` is no longer a supported OS; Fas targets `linux` only" name)
            | _ -> error os_span "expected an OS name"
          in
          ignore (bump p);
          let path_span = span p in
          let* path = string p in
          if path = "C" || path = "asm" then
            error path_span
              "an OS-conditional use takes a Fas file; put C headers and blocks \
               inside that file"
          else
            let* () = end_stmt p in
            Ok [ Ast.Use { path; c_header = None; os = Some os; span = path_span } ]
        end
        else
          let path_span = span p in
          let* path = string p in
          if path = "C" && (peek p).kind = Token.Ident "pkg" then
            error (span p) "`pkg` goes on its own line: `use pkg \"MODULES\"`"
          else if path = "C" || path = "asm" then
            let header_start = span p in
            let* header = c_header p s in
            let* () = end_stmt p in
            let span =
              match header with
              | Ast.C_quoted _ | Ast.C_system _ -> header_start
              | Ast.C_fragment _ -> s
            in
            Ok [ Ast.Use { path; c_header = Some header; os = None; span } ]
          else
            let* () = end_stmt p in
            Ok [ Ast.Use { path; c_header = None; os = None; span = path_span } ]
    | Token.Kw_const ->
        let start = span p in
        let* x = const_item p in
        Ok [ Ast.with_extent (extent p start) x ]
    | Token.Kw_var ->
        let start = span p in
        let* x = global_item p Ast.Internal_global in
        Ok [ Ast.with_extent (extent p start) x ]
    | Token.Kw_struct ->
        let start = span p in
        let* x = struct_item p in
        Ok [ Ast.with_extent (extent p start) x ]
    | Token.Kw_opaque ->
        let start = span p in
        let* x = opaque_item p in
        Ok [ Ast.with_extent (extent p start) x ]
    | Token.Kw_extern -> extern_block p
    | Token.Kw_fn ->
        let start = span p in
        let* x = fn_item p false in
        Ok [ Ast.with_extent (extent p start) x ]
    | Token.Ident "static" ->
        error (span p)
          "C `static` is not a Fas keyword; use `fn` for a function or `var` for a \
           global"
    | Token.Ident result
      when type_word result
           && (match (peek_n p 1).kind with Token.Ident _ -> true | _ -> false)
           && (peek_n p 2).kind = Token.Lparen ->
        error (span p)
          (Printf.sprintf
             "C `%s` function prototypes are not Fas syntax; use `fn` declarations"
             result)
    | Token.At ->
        let s = span p in
        let* () = expected p Token.At in
        let name_span = span p in
        let* name = ident p in
        let attribute_span =
          Span.make ~file:s.Span.file ~start_offset:s.Span.start_offset
            ~end_offset:name_span.Span.end_offset ~line:s.Span.line
            ~column:s.Span.column
        in
        Error
          [
            (if name = "align" then
               Diag.error ~help:"write `struct P @align(16) {`" attribute_span
                 "attribute `@align` applies to structs"
             else Diag.error attribute_span ("unknown attribute `@" ^ name ^ "`"));
          ]
    | t ->
        Error
          [ Diag.error (span p) ("expected a top-level item, found " ^ Token.show t) ]

  and const_item p =
    let s = span p in
    let* () = expected p Token.Kw_const in
    let name_span = span p in
    match ((peek p).kind, (peek_n p 1).kind, (peek_n p 2).kind) with
    | Token.Ident c_type, Token.Ident _, Token.Assign when c_type_word c_type ->
        error (span p)
          (Printf.sprintf
             "C `const %s` globals are not Fas syntax; Fas puts the name before its \
              type"
             c_type)
    | _ ->
        let* name = ident p in
        let ty_span = span p in
        let* ty = ty p in
        let* () = expected p Token.Assign in
        skip_newlines p;
        let* value =
          if at p Token.Lbrace then
            let value_span = span p in
            let* () = expected p Token.Lbrace in
            let* xs = literal_elements p in
            Ok (Ast.Array_lit (xs, value_span))
          else expr p
        in
        let* () = end_stmt p in
        Ok (Ast.Const { name; name_span; ty_span; ty; value; span = s; extent = s })

  and global_item p linkage =
    let s = span p in
    let* () = expected p Token.Kw_var in
    let name_span = span p in
    let* name = ident p in
    let* () =
      if at p Token.Assign then
        error (span p) "global `var` declarations need an explicit type before `=`"
      else Ok ()
    in
    let ty_span = span p in
    let* ty = ty p in
    let* init =
      if eat p Token.Assign then (
        skip_newlines p;
        let* value = expr p in
        Ok (Some value))
      else Ok None
    in
    let linkage =
      match (linkage, init) with
      | Ast.Internal_global, _ -> Ast.Internal_global
      | (Ast.Export_c | Ast.Import_c | Ast.Import_const_c), Some _ -> Ast.Export_c
      | (Ast.Export_c | Ast.Import_c | Ast.Import_const_c), None -> Ast.Import_c
    in
    let* () = end_stmt p in
    Ok
      (Ast.Global { name; name_span; ty_span; ty; init; linkage; span = s; extent = s })

  and struct_item p =
    let s = span p in
    let* () = expected p Token.Kw_struct in
    let name_span = span p in
    let* name = ident p in
    let* generic_params = generic_params p in
    let* () =
      if
        at p Token.Lbrace
        && c_type_word
             (match (peek_n p 1).kind with
             | Token.Ident field_type -> field_type
             | _ -> "")
        && (match (peek_n p 2).kind with Token.Ident _ -> true | _ -> false)
        && (peek_n p 3).kind = Token.Semi
      then
        error s
          "C struct field declarations are not Fas syntax; fields put the name before \
           the type"
      else Ok ()
    in
    let* align =
      if at p Token.At then
        let at_span = span p in
        let* () = expected p Token.At in
        let* attr_name = ident p in
        if attr_name <> "align" then
          Error [ Diag.error at_span ("unknown attribute `@" ^ attr_name ^ "`") ]
        else
          let* () = expected p Token.Lparen in
          delimited p (fun () ->
              let* n = int_value p in
              let* () = expected p Token.Rparen in
              Ok (Some n))
      else Ok None
    in
    let* () = expected p Token.Lbrace in
    skip_newlines p;
    let rec fields acc =
      if at p Token.Rbrace then Ok (List.rev acc)
      else
        let fs = span p in
        let* n = ident p in
        let ty_span = span p in
        let* t = ty p in
        let* () =
          if eat p Token.Comma then (
            skip_newlines p;
            Ok ())
          else (
            skip_newlines p;
            Ok ())
        in
        fields
          (({
              Ast.name = n;
              ty = t;
              ty_span;
              span = fs;
              offset = None;
              unsupported_reason = None;
            }
             : Ast.field)
          :: acc)
    in
    let* fs = fields [] in
    let* () = expected p Token.Rbrace in
    let* () = end_stmt p in
    Ok
      (Ast.Struct
         {
           name;
           name_span;
           generic_params;
           fields = fs;
           align;
           size = None;
           is_union = false;
           span = s;
           extent = s;
         })

  and opaque_item p =
    let s = span p in
    let* () = expected p Token.Kw_opaque in
    let name_span = span p in
    let* name = ident p in
    let* () = end_stmt p in
    Ok (Ast.Opaque { name; name_span; span = s; extent = s })

  and generic_params p =
    if not (at p Token.Lbracket) then Ok []
    else
      let* () = expected p Token.Lbracket in
      delimited p (fun () ->
          let rec go acc =
            let s = span p in
            let* n = ident p in
            let* next =
              if eat p Token.Kw_const then
                let ty_span = span p in
                let* t = ty p in
                Ok (Ast.Const_param { Ast.name = n; ty = t; ty_span; span = s })
              else Ok (Ast.Type_param { name = n; span = s })
            in
            if eat p Token.Comma then go (next :: acc)
            else
              let* () = expected p Token.Rbracket in
              Ok (List.rev (next :: acc))
          in
          go [])

  and signature p allow_variadic =
    let* () = expected p Token.Lparen in
    let* ps, variadic =
      delimited p (fun () ->
          let rec params acc variadic =
            if at p Token.Rparen then
              let* () = expected p Token.Rparen in
              Ok (List.rev acc, variadic)
            else if at p Token.Ellipsis then
              if not allow_variadic then
                Error [ Diag.error (span p) "`...` is legal only in extern \"C\"" ]
              else if acc = [] then
                Error
                  [
                    Diag.error (span p) "a variadic declaration needs a fixed parameter";
                  ]
              else
                let* () = expected p Token.Ellipsis in
                let* () = expected p Token.Rparen in
                Ok (List.rev acc, true)
            else if
              acc = []
              && (peek p).kind = Token.Ident "void"
              && (peek_n p 1).kind = Token.Rparen
            then
              error (span p)
                "C `void` parameter spelling is not Fas syntax; use an empty parameter \
                 list"
            else
              let ps = span p in
              match (peek p).kind with
              | Token.Ident name
                when type_word name
                     &&
                     match (peek_n p 1).kind with
                     | Token.Ident _ -> true
                     | _ -> false ->
                  error ps
                    (Printf.sprintf
                       "C parameter order puts `%s` before the name; Fas parameters \
                        put the name first"
                       name)
              | _ ->
                  let* name = ident p in
                  let ty_span = span p in
                  let* t = ty p in
                  let param : Ast.param = { Ast.name; ty = t; ty_span; span = ps } in
                  ignore (eat p Token.Comma);
                  params (param :: acc) variadic
          in
          params [] false)
    in
    let ret_span = span p in
    let* ret =
      if at p Token.Lbrace then
        error (span p) "function result type is required before the body"
      else ty p
    in
    Ok (ps, ret, variadic, ret_span)

  and extern_block p =
    let s = span p in
    let* () = expected p Token.Kw_extern in
    let* () =
      match ((peek p).kind, (peek_n p 1).kind, (peek_n p 2).kind) with
      | Token.Ident c_type, Token.Ident _, Token.Lparen when c_type_word c_type ->
          error (span p)
            (Printf.sprintf
               "C extern function prototypes starting with `%s` are not Fas syntax; \
                use `extern \"C\"` and `fn`"
               c_type)
      | Token.Ident c_type, Token.Ident _, (Token.Semi | Token.Assign)
        when c_type_word c_type ->
          error (span p)
            (Printf.sprintf
               "C extern global declarations starting with `%s` are not Fas syntax; \
                use `extern \"C\"` and `var`"
               c_type)
      | _ -> Ok ()
    in
    let* abi = string p in
    if abi <> "C" then Error [ Diag.error s "only extern \"C\" is supported" ]
    else
      let* () = expected p Token.Lbrace in
      skip_newlines p;
      let rec ds acc =
        if at p Token.Rbrace then
          let* () = expected p Token.Rbrace in
          let* () = end_stmt p in
          Ok (List.rev acc)
        else if at p Token.Kw_var then
          let start = span p in
          let* global = global_item p Ast.Export_c in
          ds (Ast.with_extent (extent p start) global :: acc)
        else
          let start = span p in
          let* x = fn_item p true in
          match x with
          | Ast.Func f ->
              ds
                (Ast.Func { f with linkage = Ast.External_c; extent = extent p start }
                :: acc)
          | _ -> Error [ Diag.error s "invalid extern declaration" ]
      in
      ds []

  and fn_item p allow_variadic =
    let s = span p in
    let* () = expected p Token.Kw_fn in
    let name_span = span p in
    let* name = ident p in
    let* () =
      if type_word name && match (peek p).kind with Token.Ident _ -> true | _ -> false
      then
        error (span p)
          (Printf.sprintf
             "Fas function result types follow the parameters; `fn %s name` puts the \
              type first"
             name)
      else Ok ()
    in
    let* generic_params = generic_params p in
    let* ps, ret, var, ret_span = signature p allow_variadic in
    let* () =
      if at p Token.At then
        let at_span = span p in
        let* () = expected p Token.At in
        let* attr_name = ident p in
        (* Removal A5: there is no inliner until the optimizer exists. *)
        if attr_name = "inline" then
          Error
            [
              Diag.error ~help:"delete `@inline`" at_span
                "`@inline` was removed; Fas has no inliner yet";
            ]
        else if attr_name = "align" then
          Error
            [
              Diag.error ~help:"write `struct P @align(16) {`" at_span
                "attribute `@align` applies to structs";
            ]
        else Error [ Diag.error at_span ("unknown attribute `@" ^ attr_name ^ "`") ]
      else Ok ()
    in
    if allow_variadic then
      if at p Token.Lbrace then
        if var then
          Error
            [
              Diag.error (span p) "extern \"C\" function definitions cannot be variadic";
            ]
        else
          let bstart = span p in
          let* body = block p in
          let body_span = extent p bstart in
          let* () = end_stmt p in
          Ok
            (Ast.Func
               {
                 name;
                 name_span;
                 ret_span;
                 params = ps;
                 ret;
                 body = Ast.Statements body;
                 linkage = Ast.External_c;
                 variadic = false;
                 generic_params;
                 span = s;
                 extent = s;
                 body_span;
               })
      else
        let* () = end_stmt p in
        Ok
          (Ast.Func
             {
               name;
               name_span;
               ret_span;
               params = ps;
               ret;
               body = Ast.Declaration;
               linkage = Ast.External_c;
               variadic = var;
               generic_params;
               span = s;
               extent = s;
               body_span = s;
             })
    else
      let bstart = span p in
      let* body = block p in
      let body_span = extent p bstart in
      Ok
        (Ast.Func
           {
             name;
             name_span;
             ret_span;
             params = ps;
             ret;
             body = Ast.Statements body;
             linkage = Ast.Internal;
             variadic = var;
             generic_params;
             span = s;
             extent = s;
             body_span;
           })

  and block p =
    let s = span p in
    let* () = expected p Token.Lbrace in
    p.depth <- p.depth + 1;
    if p.depth + p.nesting > p.limits.Limits.max_nesting then
      Error [ Diag.error s "block nesting exceeds the configured limit" ]
    else (
      skip_newlines p;
      let rec go acc =
        if at p Token.Rbrace then (
          let* () = expected p Token.Rbrace in
          p.depth <- p.depth - 1;
          Ok (List.rev acc))
        else if at p Token.Eof then Error [ Diag.error s "unterminated block" ]
        else
          let* x = stmt p in
          skip_newlines p;
          go (x :: acc)
      in
      go [])

  and stmt p =
    match (peek p).kind with
    | Token.Kw_use when (peek_n p 1).kind = Token.String "asm" ->
        Error [ Diag.error (span p) "assembly unit must be at top level" ]
    | Token.Kw_use when List.mem_assoc (span p).Span.start_offset p.c_containers ->
        Error [ Diag.error (span p) "C container must be at top level" ]
    | Token.Kw_var ->
        Error
          [
            Diag.error (span p)
              "`var` declares globals; locals are declared as `name Type = value`";
          ]
    | Token.Kw_const when (peek_n p 1).kind = Token.Ident "int" ->
        error (span p)
          "C `const int` locals are not Fas syntax; Fas locals put the name before the \
           type"
    | Token.Kw_struct
      when match ((peek_n p 1).kind, (peek_n p 2).kind) with
           | Token.Ident _, Token.Ident _ -> true
           | _ -> false ->
        let record =
          match (peek_n p 1).kind with Token.Ident name -> name | _ -> "T"
        in
        let name = match (peek_n p 2).kind with Token.Ident name -> name | _ -> "x" in
        error (span p)
          (Printf.sprintf
             "C `struct %s %s` declarations are not Fas syntax; write `%s %s`" record
             name name record)
    | Token.Ident "goto"
      when match (peek_n p 1).kind with Token.Ident _ -> true | _ -> false ->
        error (span p)
          "Fas has no `goto` labels; use `break` or `continue`, optionally with a loop \
           label"
    | Token.Ident "do" when (peek_n p 1).kind = Token.Lbrace ->
        error (peek_n p 1).span "C `do`/`while` loops are not Fas syntax; use `while`"
    | (Token.Plus | Token.Minus) as op
      when (peek_n p 1).kind = op
           && (peek p).span.Span.end_offset = (peek_n p 1).span.Span.start_offset
           && (match (peek_n p 2).kind with Token.Ident _ -> true | _ -> false)
           &&
           match (peek_n p 3).kind with
           | Token.Newline | Token.Semi | Token.Rbrace | Token.Eof -> true
           | _ -> false ->
        let name = match (peek_n p 2).kind with Token.Ident name -> name | _ -> "x" in
        let operator = if op = Token.Plus then "++" else "--" in
        let assignment = if op = Token.Plus then "+=" else "-=" in
        let first = (peek p).span and last = (peek_n p 1).span in
        let operator_span =
          Span.make ~file:first.Span.file ~start_offset:first.Span.start_offset
            ~end_offset:last.Span.end_offset ~line:first.Span.line
            ~column:first.Span.column
        in
        Error
          [
            Diag.error operator_span
              (Printf.sprintf "Fas has no prefix `%s` operator; write `%s %s 1`"
                 operator name assignment);
          ]
    | Token.Ident name
      when (match (peek_n p 1).kind with
             | Token.Plus | Token.Minus -> true
             | _ -> false)
           && (peek_n p 2).kind = (peek_n p 1).kind
           && (peek_n p 1).span.Span.end_offset = (peek_n p 2).span.Span.start_offset
           &&
           match (peek_n p 3).kind with
           | Token.Newline | Token.Semi | Token.Rbrace | Token.Eof -> true
           | _ -> false ->
        let op = (peek_n p 1).kind in
        let operator = if op = Token.Plus then "++" else "--" in
        let assignment = if op = Token.Plus then "+=" else "-=" in
        let first = (peek_n p 1).span and last = (peek_n p 2).span in
        let operator_span =
          Span.make ~file:first.Span.file ~start_offset:first.Span.start_offset
            ~end_offset:last.Span.end_offset ~line:first.Span.line
            ~column:first.Span.column
        in
        Error
          [
            Diag.error operator_span
              (Printf.sprintf "Fas has no postfix `%s` operator; write `%s %s 1`"
                 operator name assignment);
          ]
    | Token.Ident "view" -> view_statement p true
    | Token.Ident _
      when (peek_n p 1).kind = Token.Colon && (peek_n p 2).kind <> Token.Assign ->
        labeled_loop p
    | Token.Ident _ -> (
        match c_array_declaration p with
        | Some result -> result
        | None -> (
            match c_declaration_syntax p with
            | Some result -> result
            | None when starts_type p -> declaration p
            | None -> assignment_or_expr p))
    | Token.Kw_return ->
        let s = span p in
        ignore (bump p);
        let* x =
          if at p Token.Newline || at p Token.Semi || at p Token.Rbrace then Ok None
          else
            let* e = expr p in
            Ok (Some e)
        in
        let* () = end_stmt p in
        Ok (Ast.Return (x, s))
    | Token.Kw_if ->
        let s = span p in
        ignore (bump p);
        let* c = expr_before_block p in
        let* a = block p in
        skip_newlines p;
        let* b =
          if eat p Token.Kw_else then (
            skip_newlines p;
            if at p Token.Kw_if then
              let* x = stmt p in
              Ok (Some [ x ])
            else
              let* x = block p in
              Ok (Some x))
          else Ok None
        in
        Ok (Ast.If (c, a, b, s))
    | Token.Kw_while -> while_stmt p None
    | Token.Kw_for -> for_stmt p None
    | Token.Kw_switch -> switch_stmt p
    | Token.Kw_break ->
        let s = span p in
        ignore (bump p);
        let target =
          match (peek p).kind with
          | Token.Ident name ->
              let label_span = span p in
              ignore (bump p);
              Some (name, label_span)
          | _ -> None
        in
        let* () = end_stmt p in
        Ok (Ast.Break (target, s))
    | Token.Kw_continue ->
        let s = span p in
        ignore (bump p);
        let target =
          match (peek p).kind with
          | Token.Ident name ->
              let label_span = span p in
              ignore (bump p);
              Some (name, label_span)
          | _ -> None
        in
        let* () = end_stmt p in
        Ok (Ast.Continue (target, s))
    | Token.Kw_defer ->
        let s = span p in
        ignore (bump p);
        let* b = block p in
        Ok (Ast.Defer (b, s))
    | Token.Lbrace ->
        let s = span p in
        let* b = block p in
        Ok (Ast.Block (b, s))
    | _ -> assignment_or_expr p

  and labeled_loop p =
    let name =
      match (bump p).kind with Token.Ident name -> name | _ -> assert false
    in
    let label = { Ast.name; span = p.tokens.(p.pos - 1).Token.span } in
    ignore (bump p);
    match (peek p).kind with
    | Token.Kw_while -> while_stmt p (Some label)
    | Token.Kw_for -> for_stmt p (Some label)
    | _ ->
        error label.span
          (Printf.sprintf "loop label `%s` must precede a `while` or `for` statement"
             name)

  and while_stmt p label =
    let s = span p in
    ignore (bump p);
    let* c = expr_before_block p in
    let* b = block p in
    Ok (Ast.While (label, c, b, s))

  and view_statement p consume_end =
    let s = span p in
    ignore (bump p);
    let name_span = span p in
    let* name = ident p in
    let* () = expected p Token.Assign in
    let* place = expr p in
    let* () = finish_statement p consume_end in
    Ok (Ast.View { name; name_span; place; span = s })

  and declaration_with_end p consume_end =
    let s = span p in
    let* name = ident p in
    let ty_span = span p in
    let* ty = ty p in
    let* () =
      match (peek p).Token.kind with
      | Token.Ident _ when List.mem name [ "let"; "auto"; "mut" ] -> (
          match Diag.local_declaration_message name with
          | Some message -> Error [ Diag.error s message ]
          | None -> Ok ())
      | _ -> Ok ()
    in
    let* init =
      if eat p Token.Assign then
        match (peek p).Token.kind with
        | Token.Ident "raw" ->
            Error
              [
                Diag.error (span p)
                  (Printf.sprintf
                     "Fas has no `= raw`; write `%s %s` to declare without initializing"
                     name (Ast.type_name ty));
              ]
        | _ ->
            let* e = expr p in
            Ok (Some e)
      else Ok None
    in
    let* () = finish_statement p consume_end in
    Ok (Ast.Let { name; ty; ty_span; init; span = s })

  and declaration p = declaration_with_end p true

  and target e =
    match e with
    | Ast.Ident (n, span) -> Ok (Ast.Target_ident (n, span))
    | Ast.Select (a, args, _) -> Ok (Ast.Target_select (a, args))
    | Ast.Field (a, n, span) -> Ok (Ast.Target_field (a, n, span))
    | Ast.C_dereference (_, _, span) ->
        Error
          [ Diag.error span "Fas has no unary `*`; read through an `addr` with `p[T]`" ]
    | Ast.C_dot_star (_, span) ->
        Error [ Diag.error span "Fas has no `.*`; read through an `addr` with `p[T]`" ]
    | _ -> Error [ Diag.error (Ast.expr_span e) "invalid assignment target" ]

  and assignment_or_expr_with_end p consume_end =
    let s = span p in
    let* lhs = expr p in
    match compound_op (peek p).kind with
    | Some op ->
        let operator_span = (peek p).span in
        ignore (bump p);
        let* rhs = expr p in
        let* t = target lhs in
        let* () = finish_statement p consume_end in
        Ok (Ast.Compound_assign (t, op, rhs, s, operator_span))
    | None ->
        if eat p Token.Assign then
          let* rhs = expr p in
          if at p Token.Assign then
            error (span p)
              "Fas assignments are statements, not chained assignment expressions"
          else
            let* t = target lhs in
            let* () = finish_statement p consume_end in
            Ok (Ast.Assign (t, rhs, s))
        else
          let last = p.tokens.(max 0 (p.pos - 1)) in
          let finish =
            Span.make ~file:s.Span.file ~start_offset:s.Span.start_offset
              ~end_offset:last.Token.span.Span.end_offset ~line:s.Span.line
              ~column:s.Span.column
          in
          let* () = finish_statement p consume_end in
          Ok (Ast.Expr_stmt (lhs, s, finish))

  and assignment_or_expr p = assignment_or_expr_with_end p true

  and for_clause p =
    if at p Token.Kw_var then
      Error
        [
          Diag.error (span p)
            "`var` declares globals; locals are declared as `name Type = value`";
        ]
    else if at p (Token.Ident "view") then view_statement p false
    else if starts_type p then declaration_with_end p false
    else assignment_or_expr_with_end p false

  and for_stmt p label =
    let s = span p in
    if (peek_n p 1).kind = Token.Kw_const then (
      match label with
      | Some (label : Ast.loop_label) ->
          error label.span "a `for const` loop cannot have a label"
      | None ->
          ignore (bump p);
          ignore (bump p);
          let name_span = span p in
          let* name = ident p in
          let ty_span = span p in
          let* ty = ty p in
          let* () =
            if at p (Token.Ident "in") then (
              ignore (bump p);
              Ok ())
            else
              error (span p)
                (Printf.sprintf "expected `in` after `for const %s %s`" name
                   (Ast.type_name ty))
          in
          let* low = expr p in
          let* () = expected p Token.Dot_dot in
          let* high = expr_before_block p in
          let* body = block p in
          Ok (Ast.Const_for { name; name_span; ty; ty_span; low; high; body; span = s }))
    else if (peek_n p 1).kind = Token.Lparen then
      error (peek_n p 2).span
        "C `for (;;)` syntax has parentheses; Fas writes `for ; ;` with an optional \
         condition"
    else
      let () = ignore (bump p) in
      let* init =
        if at p Token.Semi then (
          ignore (bump p);
          Ok None)
        else
          let* x = for_clause p in
          let* () = expected p Token.Semi in
          Ok (Some x)
      in
      let* cond =
        if at p Token.Semi then Ok None
        else
          let* e = expr p in
          Ok (Some e)
      in
      let* () = expected p Token.Semi in
      let* step =
        if at p Token.Lbrace then Ok None
        else
          let* x = for_clause_before_block p in
          Ok (Some x)
      in
      let* body = block p in
      Ok (Ast.For (label, init, cond, step, body, s))

  and switch_stmt p =
    let s = span p in
    within_nesting p s "switch nesting exceeds the configured limit" (fun () ->
        ignore (bump p);
        let* scr = expr_before_block p in
        skip_newlines p;
        let* () = expected p Token.Lbrace in
        let rec cases arms default =
          skip_newlines p;
          match (peek p).kind with
          | Token.Kw_case ->
              ignore (bump p);
              let rec values acc =
                let* e = expr p in
                if at p Token.Comma then (
                  ignore (bump p);
                  values (e :: acc))
                else Ok (List.rev (e :: acc))
              in
              let* es = values [] in
              let* () = expected p Token.Colon in
              let* b = case_body p in
              cases ((es, b) :: arms) default
          | Token.Kw_default -> (
              let default_span = span p in
              ignore (bump p);
              match default with
              | Some (first_span, _) ->
                  Error
                    [
                      Diag.error
                        ~notes:[ "first default is at " ^ Span.to_string first_span ]
                        default_span "duplicate default arm";
                    ]
              | None ->
                  let* () = expected p Token.Colon in
                  let* b = case_body p in
                  cases arms (Some (default_span, b)))
          | Token.Rbrace ->
              let* () = expected p Token.Rbrace in
              Ok (Ast.Switch (scr, List.rev arms, Option.map snd default, s))
          | _ -> Error [ Diag.error (span p) "expected `case`, `default`, or `}`" ]
        in
        cases [] None)

  and case_body p =
    skip_newlines p;
    let rec go acc =
      if at p Token.Kw_case || at p Token.Kw_default || at p Token.Rbrace then
        Ok (List.rev acc)
      else
        let* x = stmt p in
        skip_newlines p;
        go (x :: acc)
    in
    go []

  and expr_before_block p =
    let previous = p.block_expression_depth in
    p.block_expression_depth <- Some (p.depth + 1);
    let result = expr p in
    p.block_expression_depth <- previous;
    result

  and for_clause_before_block p =
    let previous = p.block_expression_depth in
    p.block_expression_depth <- Some (p.depth + 1);
    let result =
      match ((peek p).kind, (peek_n p 1).kind, (peek_n p 2).kind) with
      | Token.Ident name, ((Token.Plus | Token.Minus) as op), second
        when second = op
             && (peek_n p 1).span.Span.end_offset = (peek_n p 2).span.Span.start_offset
        ->
          let operator = if op = Token.Plus then "++" else "--" in
          let assignment = if op = Token.Plus then "+=" else "-=" in
          let first = (peek_n p 1).span and last = (peek_n p 2).span in
          let operator_span =
            Span.make ~file:first.Span.file ~start_offset:first.Span.start_offset
              ~end_offset:last.Span.end_offset ~line:first.Span.line
              ~column:first.Span.column
          in
          Error
            [
              Diag.error operator_span
                (Printf.sprintf "Fas has no postfix `%s` operator; write `%s %s 1`"
                   operator name assignment);
            ]
      | _ -> for_clause p
    in
    p.block_expression_depth <- previous;
    result

  and expr p =
    p.depth <- p.depth + 1;
    if p.depth + p.nesting > p.limits.Limits.max_nesting then
      Error [ Diag.error (span p) "expression nesting exceeds the configured limit" ]
    else
      let r = ternary p in
      p.depth <- p.depth - 1;
      r

  and ternary p = if at p Token.Kw_if then if_expression p else or_ p

  and if_expression p =
    let s = span p in
    let* () = expected p Token.Kw_if in
    let* condition = expr_before_block p in
    let* yes = if_expression_branch p in
    skip_newlines p;
    if not (eat p Token.Kw_else) then
      let close = p.tokens.(p.pos - 1).Token.span in
      Error [ Diag.error close "if-expression requires an `else` branch" ]
    else (
      skip_newlines p;
      let* no = if at p Token.Kw_if then if_expression p else if_expression_branch p in
      let operator =
        match (peek p).Token.kind with
        | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
        | Token.Amp | Token.Pipe | Token.Caret | Token.Eqeq | Token.Neq | Token.Lt
        | Token.Le | Token.Gt | Token.Ge | Token.Ltlt | Token.Gtgt | Token.Andand
        | Token.Oror ->
            true
        | _ -> false
      in
      if operator then
        Error
          [
            Diag.error ~help:"parenthesize the if-expression before this operator"
              (span p) "an if-expression cannot be followed by a binary operator";
          ]
      else Ok (Ast.Ternary (condition, yes, no, s)))

  and if_expression_branch p =
    let* () = expected p Token.Lbrace in
    skip_newlines p;
    let* expression = expr p in
    skip_newlines p;
    let* () = expected p Token.Rbrace in
    Ok expression

  and continued_operand p next =
    let before = p.pos in
    skip_newlines p;
    match (peek p).kind with
    | Token.Rbrace | Token.Rparen | Token.Rbracket | Token.Comma | Token.Semi
    | Token.Eof ->
        p.pos <- before;
        next p
    | _ -> next p

  and binary p next ops =
    let* first = next p in
    let rec go lhs =
      match (peek p).kind with
      | Token.Minus
        when (peek_n p 1).kind = Token.Gt
             && (peek p).span.Span.end_offset = (peek_n p 1).span.Span.start_offset ->
          let first = (peek p).span and last = (peek_n p 1).span in
          let operator_span =
            Span.make ~file:first.Span.file ~start_offset:first.Span.start_offset
              ~end_offset:last.Span.end_offset ~line:first.Span.line
              ~column:first.Span.column
          in
          ignore (bump p);
          ignore (bump p);
          let field_span = span p in
          let* field = ident p in
          go (Ast.Arrow_field (lhs, field, operator_span, field_span))
      | k when List.mem_assoc k ops ->
          let op = List.assoc k ops in
          let s = span p in
          ignore (bump p);
          let* rhs = continued_operand p next in
          go (Ast.Binary (op, lhs, rhs, s))
      | _ -> Ok lhs
    in
    go first

  and or_ p = binary p and_ [ (Token.Oror, Ast.Or) ]
  and and_ p = binary p equality [ (Token.Andand, Ast.And) ]
  and bitor p = binary p bitxor [ (Token.Pipe, Ast.Bit_or) ]
  and bitxor p = binary p bitand [ (Token.Caret, Ast.Bit_xor) ]
  and bitand p = binary p shift [ (Token.Amp, Ast.Bit_and) ]
  and equality p = binary p relational [ (Token.Eqeq, Ast.Eq); (Token.Neq, Ast.Ne) ]

  and relational p =
    binary p bitor
      [ (Token.Lt, Ast.Lt); (Token.Le, Ast.Le); (Token.Gt, Ast.Gt); (Token.Ge, Ast.Ge) ]

  and shift p = binary p additive [ (Token.Ltlt, Ast.Shl); (Token.Gtgt, Ast.Shr) ]

  and additive p =
    binary p multiplicative [ (Token.Plus, Ast.Add); (Token.Minus, Ast.Sub) ]

  and multiplicative p =
    binary p unary
      [ (Token.Star, Ast.Mul); (Token.Slash, Ast.Div); (Token.Percent, Ast.Rem) ]

  and unary p =
    match (peek p).kind with
    | (Token.Plus | Token.Minus) as op
      when (peek_n p 1).kind = op
           && (peek p).span.Span.end_offset = (peek_n p 1).span.Span.start_offset ->
        let operator = if op = Token.Plus then "++" else "--" in
        let first = (peek p).span and last = (peek_n p 1).span in
        let operator_span =
          Span.make ~file:first.Span.file ~start_offset:first.Span.start_offset
            ~end_offset:last.Span.end_offset ~line:first.Span.line
            ~column:first.Span.column
        in
        error operator_span (Printf.sprintf "Fas has no prefix `%s` operator" operator)
    | Token.Star ->
        let operator_span = span p in
        within_nesting p operator_span "unary nesting exceeds the configured limit"
          (fun () ->
            ignore (bump p);
            let* cast_type, value =
              match c_cast_type p with
              | Some cast when String.ends_with ~suffix:"*" cast || cast = "addr" ->
                  ignore (bump p);
                  ignore (bump p);
                  if String.ends_with ~suffix:"*" cast then ignore (bump p);
                  ignore (bump p);
                  let* value = unary p in
                  let cast_type =
                    if cast = "addr" then None
                    else Some (String.sub cast 0 (String.length cast - 1))
                  in
                  Ok (cast_type, value)
              | _ ->
                  let* value = unary p in
                  Ok (None, value)
            in
            Ok (Ast.C_dereference (value, cast_type, operator_span)))
    | Token.Amp ->
        let s = span p in
        within_nesting p s "unary nesting exceeds the configured limit" (fun () ->
            ignore (bump p);
            let* e = unary p in
            Ok (Ast.Addr_of (e, s)))
    | Token.Minus ->
        let s = span p in
        within_nesting p s "unary nesting exceeds the configured limit" (fun () ->
            ignore (bump p);
            let* e = unary p in
            Ok (Ast.Unary (Ast.Neg, e, s)))
    | Token.Not ->
        let s = span p in
        within_nesting p s "unary nesting exceeds the configured limit" (fun () ->
            ignore (bump p);
            let* e = unary p in
            Ok (Ast.Unary (Ast.Not, e, s)))
    | Token.Tilde ->
        let s = span p in
        within_nesting p s "unary nesting exceeds the configured limit" (fun () ->
            ignore (bump p);
            let* e = unary p in
            Ok (Ast.Unary (Ast.Bit_not, e, s)))
    | _ -> postfix p

  and postfix p =
    let* first = primary p in
    let brackets_before_call () =
      let rec scan offset depth =
        match (peek_n p offset).kind with
        | Token.Lbracket -> scan (offset + 1) (depth + 1)
        | Token.Rbracket when depth = 1 -> (peek_n p (offset + 1)).kind = Token.Lparen
        | Token.Rbracket when depth > 1 -> scan (offset + 1) (depth - 1)
        | Token.Newline when depth > 0 -> scan (offset + 1) depth
        | Token.Eof | Token.Newline -> false
        | _ -> scan (offset + 1) depth
      in
      scan 0 0
    in
    let rec go e =
      match (peek p).kind with
      | Token.Lparen ->
          let* xs = args p in
          let finish = p.tokens.(p.pos - 1).Token.span in
          let rec callee_start = function
            | Ast.Generic_args (callee, _, _) -> callee_start callee
            | expression -> Ast.expr_span expression
          in
          let base = callee_start e in
          let call_span =
            Span.make ~file:base.Span.file ~start_offset:base.Span.start_offset
              ~end_offset:finish.Span.end_offset ~line:base.Span.line
              ~column:base.Span.column
          in
          go (Ast.Call (e, xs, call_span))
      | Token.Lbracket when brackets_before_call () ->
          let s = span p in
          let* arguments = generic_args p in
          go (Ast.Generic_args (e, arguments, s))
      | Token.Lbracket ->
          ignore (bump p);
          let* args = select_payloads p in
          let finish = p.tokens.(p.pos - 1).Token.span in
          let base = Ast.expr_span e in
          let selection_span =
            Span.make ~file:base.Span.file ~start_offset:base.Span.start_offset
              ~end_offset:finish.Span.end_offset ~line:base.Span.line
              ~column:base.Span.column
          in
          go (Ast.Select (e, args, selection_span))
      | Token.Colon when (peek_n p 1).kind = Token.Assign ->
          Error
            [
              Diag.error (span p)
                "locals are declared as `name Type = value`; Fas has no `:=`";
            ]
      | (Token.Plus | Token.Minus) as op
        when (peek_n p 1).kind = op
             && (peek p).span.Span.end_offset = (peek_n p 1).span.Span.start_offset ->
          let operator = if op = Token.Plus then "++" else "--" in
          let first = (peek p).span and last = (peek_n p 1).span in
          let operator_span =
            Span.make ~file:first.Span.file ~start_offset:first.Span.start_offset
              ~end_offset:last.Span.end_offset ~line:first.Span.line
              ~column:first.Span.column
          in
          Error
            [
              Diag.error operator_span
                (Printf.sprintf "Fas has no postfix `%s` operator" operator);
            ]
      | Token.Dot ->
          let s = span p in
          ignore (bump p);
          if at p Token.Star then (
            let star = span p in
            ignore (bump p);
            let operator_span =
              Span.make ~file:s.Span.file ~start_offset:s.Span.start_offset
                ~end_offset:star.Span.end_offset ~line:s.Span.line ~column:s.Span.column
            in
            go (Ast.C_dot_star (e, operator_span)))
          else
            let* n, field_span = field_ident p in
            go (Ast.Field (e, n, field_span))
      | _ -> Ok e
    in
    go first

  and literal_elements p =
    delimited p (fun () ->
        let rec elements acc =
          if at p Token.Rbrace then
            let* () = expected p Token.Rbrace in
            Ok (List.rev acc)
          else
            let* expression =
              if at p Token.Dot then
                let designator_span = span p in
                let* () = expected p Token.Dot in
                let* name, name_span = field_ident p in
                let* () = expected p Token.Assign in
                let* value = expr p in
                Ok (Ast.Designated_field (name, name_span, value, designator_span))
              else if at p Token.Lbracket then
                let designator_span = span p in
                let* () = expected p Token.Lbracket in
                let* index = expr p in
                let* () = expected p Token.Rbracket in
                let* () = expected p Token.Assign in
                let* value = expr p in
                Ok (Ast.Designated_index (index, value, designator_span))
              else expr p
            in
            let* () =
              if eat p Token.Comma then Ok ()
              else if at p Token.Rbrace then Ok ()
              else
                Error [ Diag.error (span p) "expected comma between literal elements" ]
            in
            elements (expression :: acc)
        in
        elements [])

  and args p =
    let* () = expected p Token.Lparen in
    delimited p (fun () ->
        if eat p Token.Rparen then Ok []
        else
          let rec go acc =
            let* x = expr p in
            if eat p Token.Comma then go (x :: acc)
            else
              let* () = expected p Token.Rparen in
              Ok (List.rev (x :: acc))
          in
          go [])

  and type_argument p =
    let* () = expected p Token.Lbracket in
    delimited p (fun () ->
        let* t = ty p in
        let* () = expected p Token.Rbracket in
        Ok t)

  and expression_argument p =
    let* () = expected p Token.Lparen in
    delimited p (fun () ->
        let* e = expr p in
        if at p Token.Comma then
          error (span p)
            "Fas has no comma operator; put each expression in its own statement"
        else
          let* () = expected p Token.Rparen in
          Ok e)

  and c_cast_type p =
    match ((peek_n p 1).kind, (peek_n p 2).kind, (peek_n p 3).kind) with
    | Token.Ident name, Token.Star, Token.Rparen when type_word name -> Some (name ^ "*")
    | Token.Ident name, Token.Rparen, _ when type_word name -> Some name
    | _ -> None

  and primary p =
    match (peek p).kind with
    | Token.Lparen when Option.is_some (c_cast_type p) ->
        let cast = Option.get (c_cast_type p) in
        let is_pointer = String.ends_with ~suffix:"*" cast || cast = "addr" in
        let first = span p in
        let last =
          (peek_n p (if String.ends_with ~suffix:"*" cast then 3 else 2)).span
        in
        let cast_span =
          Span.make ~file:first.Span.file ~start_offset:first.Span.start_offset
            ~end_offset:last.Span.end_offset ~line:first.Span.line
            ~column:first.Span.column
        in
        if String.ends_with ~suffix:"*" cast then (
          ignore (bump p);
          ignore (bump p);
          ignore (bump p);
          ignore (bump p);
          let* value = unary p in
          let type_name = String.sub cast 0 (String.length cast - 1) in
          Ok
            (Ast.C_dereference
               (value, Some ("__fas_c_pointer_cast__:" ^ type_name), cast_span)))
        else
          error
            (if is_pointer then cast_span else first)
            (if is_pointer then
               if cast = "addr" then "C cast `(addr)` is not Fas syntax"
               else
                 Printf.sprintf
                   "C pointer cast `(%s)` is not Fas syntax; `addr` is untyped" cast
             else
               Printf.sprintf
                 "C cast `(%s)` is not Fas syntax; use `zext`, `sext`, `trunc` or \
                  `bitcast`"
                 cast)
    | Token.Lbrace ->
        let s = span p in
        let* () = expected p Token.Lbrace in
        let* elements = literal_elements p in
        Ok (Ast.Array_lit (elements, s))
    | Token.Int s ->
        let sp = span p in
        ignore (bump p);
        Ok (Ast.Int_lit (s, sp))
    | Token.Float s ->
        let sp = span p in
        ignore (bump p);
        Ok (Ast.Float_lit (s, sp))
    | Token.String s ->
        let sp = span p in
        ignore (bump p);
        Ok (Ast.String_lit (false, s, sp))
    | Token.CString s ->
        let sp = span p in
        ignore (bump p);
        Ok (Ast.String_lit (true, s, sp))
    | Token.Dot
      when match ((peek_n p 1).kind, (peek_n p 2).kind) with
           | Token.Ident _, Token.Assign -> true
           | _ -> false ->
        let field =
          match (peek_n p 1).kind with Token.Ident field -> field | _ -> "field"
        in
        error (span p)
          (Printf.sprintf
             "Fas struct initializers are positional; designated initializer `.%s` is \
              not supported"
             field)
    | Token.Lparen ->
        let s = span p in
        if starts_struct_literal p then
          error s "Fas has no compound literals; declare a typed local or `const`"
        else
          let* e = expression_argument p in
          let s = { s with end_offset = p.tokens.(p.pos - 1).Token.span.end_offset } in
          Ok (Ast.Parenthesized (e, s))
    | Token.Ident n -> (
        let sp = span p in
        ignore (bump p);
        if Names.literal n = Some Names.True then Ok (Ast.Bool_lit (true, sp))
        else if Names.literal n = Some Names.False then Ok (Ast.Bool_lit (false, sp))
        else if Names.literal n = Some Names.Null then Ok (Ast.Null sp)
        else
          match (Names.parser_operation n, (peek_n p 0).kind) with
          | ( Some ((Names.Zext | Names.Sext | Names.Trunc | Names.Bitcast) as operation),
              Token.Lbracket ) ->
              let kind =
                match operation with
                | Names.Zext -> Ast.Zext
                | Names.Sext -> Ast.Sext
                | Names.Trunc -> Ast.Trunc
                | Names.Bitcast -> Ast.Bitcast
                | _ -> assert false
              in
              let* t = type_argument p in
              let* e = expression_argument p in
              Ok (Ast.Cast (kind, t, e, sp))
          | None, Token.Lbracket
            when Names.value_operation n = Some Names.Handle_from_addr ->
              let* t = type_argument p in
              let* e = expression_argument p in
              Ok (Ast.Handle_from_addr (t, e, sp))
          | Some ((Names.Sizeof | Names.Alignof | Names.Offsetof) as operation), _ ->
              let operation_name =
                match operation with
                | Names.Sizeof -> "sizeof"
                | Names.Alignof -> "alignof"
                | Names.Offsetof -> "offsetof"
                | _ -> assert false
              in
              if not (at p Token.Lbracket) then
                if operation = Names.Sizeof then
                  let operand =
                    if at p Token.Lparen then expression_argument p else expr p
                  in
                  match operand with
                  | Ok value -> Ok (Ast.Sizeof_value (value, sp))
                  | Error _ ->
                      error sp
                        (Printf.sprintf "`%s` needs a type in brackets" operation_name)
                else
                  error sp
                    (Printf.sprintf "`%s` needs a type in brackets" operation_name)
              else
                let* () = expected p Token.Lbracket in
                delimited p (fun () ->
                    let* t = ty p in
                    if operation = Names.Offsetof then
                      let* () = expected p Token.Comma in
                      let field_span = (peek p).span in
                      let* f = ident p in
                      let* () = expected p Token.Rbracket in
                      Ok (Ast.Offsetof (t, f, field_span, sp))
                    else
                      let* () = expected p Token.Rbracket in
                      Ok
                        (if operation = Names.Sizeof then Ast.Sizeof (t, sp)
                         else Ast.Alignof (t, sp)))
          | Some Names.Splat, Token.Lparen ->
              let* e = expression_argument p in
              Ok (Ast.Splat (e, sp))
          | None, Token.Lparen when n = "ptr_add" || n = "ptr_add_bytes" ->
              let message =
                if n = "ptr_add_bytes" then
                  "builtin `ptr_add_bytes` is not defined; use address arithmetic"
                else "builtin `ptr_add` is not defined; use address arithmetic"
              in
              Error [ Diag.error sp message ]
          | _ -> Ok (Ast.Ident (n, sp)))
    | t ->
        Error [ Diag.error (span p) ("expected an expression, found " ^ Token.show t) ]
end

let item_keywords = [ "const"; "var"; "struct"; "opaque"; "extern"; "fn"; "use" ]

let item_starts text =
  let n = String.length text in
  let rec loop i acc =
    if i >= n then List.rev acc
    else
      let starts_item =
        (i = 0 || text.[i - 1] = '\n')
        && (text.[i] = '@' || List.exists (word_at text i) item_keywords)
      in
      loop (i + 1) (if starts_item then i :: acc else acc)
  in
  loop 0 []

let recovery_candidates text error =
  let n = String.length text in
  let starts = item_starts text in
  let next_after x = Option.value (List.find_opt (fun s -> s > x) starts) ~default:n in
  let containing =
    List.fold_left (fun acc s -> if s <= error then Some s else acc) None starts
  in
  let previous =
    List.fold_left (fun acc s -> if s < error then Some s else acc) None starts
  in
  if List.mem error starts then
    (match previous with Some p -> [ (p, error) ] | None -> [])
    @ [ (error, next_after error) ]
  else
    match containing with
    | Some c -> [ (c, next_after c) ]
    | None -> ( match starts with [] -> [] | _ -> [ (0, next_after error) ])

let blank_text text (start, stop) =
  let buffer = Bytes.of_string text in
  blank_range buffer text start stop;
  Bytes.to_string buffer

let blanking_runs = 16

let parse ?(limits = Limits.default) source =
  match extract_containers source with
  | Error e -> Error e
  | Ok (clean, c_containers) -> (
      let cleaned = Source.create ~file:(Source.file source) ~text:clean in
      match Lexer.lex ~limits cleaned with
      | Error e -> Error e
      | Ok tokens -> (
          let p =
            {
              P.tokens = Array.of_list tokens;
              pos = 0;
              c_containers;
              limits;
              depth = 0;
              nesting = 0;
              block_expression_depth = None;
              delimited_depth = 0;
            }
          in
          match P.items p with
          | Ok items -> (
              let program = { Ast.items } in
              match Ast.check_expanded_nodes ~limits program with
              | Ok () -> Ok program
              | Error diagnostic -> Error [ diagnostic ])
          | Error e -> Error e))

let body_opening text start stop =
  let rec ident_end i =
    if i < stop && ident_char text.[i] then ident_end (i + 1) else i
  in
  let name_end = ident_end (skip_space_comments text (start + 2)) in
  let after = skip_space_comments text name_end in
  if after >= stop || text.[after] = '[' then None
  else
    let rec find i depth =
      if i >= stop then None
      else
        let j = skip_space_comments text i in
        if j > i then find j depth
        else
          match text.[i] with
          | '(' | '[' -> find (i + 1) (depth + 1)
          | ')' | ']' -> find (i + 1) (depth - 1)
          | '{' when depth = 0 -> Some i
          | _ -> find (i + 1) depth
    in
    find after 0

let keep_header text clean (start, stop) =
  if not (word_at clean start "fn") then None
  else
    match body_opening clean start stop with
    | None -> None
    | Some opening ->
        let body = opening + 1 in
        let rec closing i =
          if i < body then None
          else if clean.[i] <> '\n' then Some i
          else closing (i - 1)
        in
        Option.map
          (fun close ->
            let buffer = Bytes.of_string text in
            blank_range buffer clean body stop;
            Bytes.set buffer close '}';
            (Bytes.to_string buffer, (body, stop), opening))
          (closing (stop - 1))

type attempt = { blanked_text : string; gap : int * int; header_end : int option }

let attempts text clean candidate =
  let whole =
    { blanked_text = blank_text text candidate; gap = candidate; header_end = None }
  in
  match keep_header text clean candidate with
  | Some (blanked_text, gap, opening) ->
      [ { blanked_text; gap; header_end = Some opening }; whole ]
  | None -> [ whole ]

let fails_in_header attempt offset =
  match attempt.header_end with Some opening -> offset <= opening | None -> false

let parse_blanking ?(limits = Limits.default) source =
  match parse ~limits source with
  | Ok program -> Ok (program, [])
  | Error diagnostics -> (
      let file = Source.file source in
      let parses = ref 0 in
      let rec search text error ranges =
        match extract_containers (Source.create ~file ~text) with
        | Error _ -> None
        | Ok (clean, _) ->
            let rec try_all = function
              | [] -> None
              | candidate :: rest ->
                  let rec try_attempts = function
                    | [] -> try_all rest
                    | attempt :: others ->
                        if !parses >= blanking_runs then None
                        else (
                          incr parses;
                          let ranges = ranges @ [ attempt.gap ] in
                          match
                            parse ~limits
                              (Source.create ~file ~text:attempt.blanked_text)
                          with
                          | Ok program -> Some (program, ranges)
                          | Error ({ Diag.primary; _ } :: _)
                            when (not (Span.is_synthetic primary))
                                 && primary.Span.start_offset <> error
                                 && not
                                      (fails_in_header attempt primary.Span.start_offset)
                            -> (
                              match
                                search attempt.blanked_text primary.Span.start_offset
                                  ranges
                              with
                              | Some found -> Some found
                              | None -> try_attempts others)
                          | Error _ -> try_attempts others)
                  in
                  try_attempts (attempts text clean candidate)
            in
            try_all (recovery_candidates clean error)
      in
      match diagnostics with
      | { Diag.primary; _ } :: _ when not (Span.is_synthetic primary) -> (
          match search (Source.text source) primary.Span.start_offset [] with
          | Some found -> Ok found
          | None -> Error diagnostics)
      | _ -> Error diagnostics)
