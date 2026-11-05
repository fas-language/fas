let is_space = function ' ' | '\t' | '\r' -> true | _ -> false
let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_ident_continue c = is_ident_start c || is_digit c

let keyword = function
  | "fn" -> Token.Kw_fn
  | "return" -> Kw_return
  | "if" -> Kw_if
  | "else" -> Kw_else
  | "while" -> Kw_while
  | "break" -> Kw_break
  | "continue" -> Kw_continue
  | "const" -> Kw_const
  | "var" -> Kw_var
  | "struct" -> Kw_struct
  | "opaque" -> Kw_opaque
  | "extern" -> Kw_extern
  | "defer" -> Kw_defer
  | "use" -> Kw_use
  | "for" -> Kw_for
  | "switch" -> Kw_switch
  | "case" -> Kw_case
  | "default" -> Kw_default
  | name -> Ident name

let is_keyword name = match keyword name with Token.Ident _ -> false | _ -> true

let lex ?(limits = Limits.default) source =
  let text = Source.text source in
  let n = String.length text in
  let diagnostic ?help offset message =
    Diag.error ?help
      (Source.span source ~start_offset:offset ~end_offset:(min n (offset + 1)))
      message
  in
  let at_line_end offset =
    offset >= n
    || text.[offset] = '\n'
    || (text.[offset] = '\r' && offset + 1 < n && text.[offset + 1] = '\n')
  in
  let decode_escape i =
    if i + 1 >= n then Error [ diagnostic i "unterminated escape" ]
    else if text.[i + 1] = 'x' then
      if i + 3 >= n || (not (is_hex text.[i + 2])) || not (is_hex text.[i + 3]) then
        Error
          [
            diagnostic i "hex escape must be followed by exactly two hexadecimal digits";
          ]
      else
        let value = int_of_string ("0x" ^ String.sub text (i + 2) 2) in
        Ok (i + 4, Char.chr value)
    else
      let value =
        match text.[i + 1] with
        | 'n' -> Some '\n'
        | 't' -> Some '\t'
        | 'r' -> Some '\r'
        | '\\' -> Some '\\'
        | '\'' -> Some '\''
        | '"' -> Some '"'
        | '0' -> Some '\000'
        | _ -> None
      in
      match value with
      | None -> Error [ diagnostic i "unknown string escape" ]
      | Some value -> Ok (i + 2, value)
  in
  let rec loop offset count tokens =
    if offset >= n then
      let sp = Source.span source ~start_offset:n ~end_offset:n in
      Ok (List.rev ({ Token.kind = Token.Eof; span = sp } :: tokens))
    else
      let c = text.[offset] in
      if is_space c then loop (offset + 1) count tokens
      else if c = '/' && offset + 1 < n && text.[offset + 1] = '/' then
        let rec skip i = if i < n && text.[i] <> '\n' then skip (i + 1) else i in
        loop (skip (offset + 2)) count tokens
      else if c = '/' && offset + 1 < n && text.[offset + 1] = '*' then
        let rec close i =
          if i + 1 >= n then None
          else if text.[i] = '*' && text.[i + 1] = '/' then Some (i + 2)
          else close (i + 1)
        in
        match close (offset + 2) with
        | None -> Error [ diagnostic offset "unterminated block comment" ]
        | Some next -> loop next count tokens
      else if count >= limits.Limits.max_tokens then
        Error [ diagnostic offset "token limit exceeded" ]
      else if c = '\n' then
        let sp = Source.span source ~start_offset:offset ~end_offset:(offset + 1) in
        loop (offset + 1) (count + 1)
          ({ Token.kind = Token.Newline; span = sp } :: tokens)
      else if
        is_ident_start c && not (c = 'c' && offset + 1 < n && text.[offset + 1] = '"')
      then
        let rec ident_end i =
          if i < n && is_ident_continue text.[i] then ident_end (i + 1) else i
        in
        let stop = ident_end (offset + 1) in
        let value = String.sub text offset (stop - offset) in
        let sp = Source.span source ~start_offset:offset ~end_offset:stop in
        loop stop (count + 1) ({ Token.kind = keyword value; span = sp } :: tokens)
      else if c = '.' && offset + 1 < n && is_digit text.[offset + 1] then
        Error
          [
            diagnostic ~help:"write `0.5` with a leading zero" offset
              "floating point literals need a leading zero";
          ]
      else if is_digit c then
        let rec number_end i =
          if
            i < n
            && (is_ident_continue text.[i] || text.[i] = '_' || text.[i] = '.')
            && not (text.[i] = '.' && i + 1 < n && text.[i + 1] = '.')
          then number_end (i + 1)
          else if
            i < n
            && (text.[i] = '+' || text.[i] = '-')
            && i > offset
            && (text.[i - 1] = 'e' || text.[i - 1] = 'E')
          then number_end (i + 1)
          else i
        in
        let stop = number_end (offset + 1) in
        let raw = String.sub text offset (stop - offset) in
        let raw_prefix prefix = String.length raw >= 2 && String.sub raw 0 2 = prefix in
        let radix, digits =
          if raw_prefix "0x" || raw_prefix "0X" then
            (16, String.sub raw 2 (String.length raw - 2))
          else if raw_prefix "0b" || raw_prefix "0B" then
            (2, String.sub raw 2 (String.length raw - 2))
          else if raw_prefix "0o" || raw_prefix "0O" then
            (8, String.sub raw 2 (String.length raw - 2))
          else (10, raw)
        in
        let valid_digit c =
          match radix with
          | 2 -> c = '0' || c = '1'
          | 8 -> c >= '0' && c <= '7'
          | 10 -> is_digit c
          | 16 -> is_hex c
          | _ -> false
        in
        let digit_start = String.length raw - String.length digits in
        let misplaced_separator =
          let rec find i =
            if i >= String.length raw then false
            else if raw.[i] = '_' then
              i <= digit_start
              || i + 1 >= String.length raw
              || (not (valid_digit raw.[i - 1]))
              || (not (valid_digit raw.[i + 1]))
              || find (i + 1)
            else find (i + 1)
          in
          find 0
        in
        let clean = String.concat "" (String.split_on_char '_' raw) in
        let digits = String.concat "" (String.split_on_char '_' digits) in
        let c_suffix =
          String.length raw > 1
          &&
          match raw.[String.length raw - 1] with
          | 'u' | 'U' | 'l' | 'L' -> true
          | _ -> false
        in
        let based =
          raw_prefix "0x" || raw_prefix "0X" || raw_prefix "0b" || raw_prefix "0B"
          || raw_prefix "0o" || raw_prefix "0O"
        in
        let float_marker =
          (not based)
          &&
          let rec find i =
            if i >= String.length raw then false
            else if raw.[i] = '.' || raw.[i] = 'e' || raw.[i] = 'E' then true
            else if is_digit raw.[i] || raw.[i] = '_' then find (i + 1)
            else false
          in
          find 0
        in
        let float_syntax =
          let clean = String.concat "" (String.split_on_char '_' raw) in
          let length = String.length clean in
          let rec digits index =
            if index < length && is_digit clean.[index] then digits (index + 1)
            else index
          in
          let whole_end = digits 0 in
          if whole_end = 0 then false
          else
            let fraction_end =
              if whole_end < length && clean.[whole_end] = '.' then
                digits (whole_end + 1)
              else whole_end
            in
            let has_fraction = fraction_end > whole_end + 1 in
            let exponent_end =
              if
                fraction_end < length
                && (clean.[fraction_end] = 'e' || clean.[fraction_end] = 'E')
              then
                let start =
                  if
                    fraction_end + 1 < length
                    && (clean.[fraction_end + 1] = '+' || clean.[fraction_end + 1] = '-')
                  then fraction_end + 2
                  else fraction_end + 1
                in
                digits start
              else fraction_end
            in
            exponent_end = length
            && (has_fraction || fraction_end < length)
            && (clean.[whole_end] <> '.' || has_fraction)
            && (fraction_end >= length
               || (clean.[fraction_end] <> 'e' && clean.[fraction_end] <> 'E')
               || exponent_end > fraction_end + 1)
        in
        let misplaced_float_separator =
          let rec find i =
            if i >= String.length raw then false
            else if raw.[i] = '_' then
              i = 0
              || i + 1 >= String.length raw
              || (not (is_digit raw.[i - 1]))
              || (not (is_digit raw.[i + 1]))
              || find (i + 1)
            else find (i + 1)
          in
          find 0
        in
        if float_marker then
          let suffix =
            match raw.[String.length raw - 1] with
            | 'f' | 'F' | 'd' | 'D' -> true
            | _ -> false
          in
          if suffix then
            let unsuffixed = String.sub raw 0 (String.length raw - 1) in
            Error
              [
                diagnostic
                  ~help:(Printf.sprintf "write `%s` without a suffix" unsuffixed)
                  offset "floating point literals do not have suffixes";
              ]
          else if misplaced_float_separator then
            Error [ diagnostic offset "misplaced digit separator `_`" ]
          else if String.length raw > 0 && raw.[String.length raw - 1] = '.' then
            Error
              [
                diagnostic
                  ~help:(Printf.sprintf "write `%s0`" raw)
                  offset "floating point literals need digits after `.`";
              ]
          else if not float_syntax then
            Error [ diagnostic offset "invalid floating point literal" ]
          else
            let sp = Source.span source ~start_offset:offset ~end_offset:stop in
            loop stop (count + 1) ({ Token.kind = Token.Float raw; span = sp } :: tokens)
        else if c_suffix then
          let unsuffixed = String.sub raw 0 (String.length raw - 1) in
          Error
            [
              diagnostic offset
                (Printf.sprintf "C integer suffix `%s` is not valid in Fas; write `%s`"
                   raw unsuffixed);
            ]
        else if stop + 1 < n && text.[stop] = '.' && is_digit text.[stop + 1] then
          Error
            [
              diagnostic offset
                "float literals are not Fas syntax; use an integer literal";
            ]
        else if c_suffix then
          let unsuffixed = String.sub raw 0 (String.length raw - 1) in
          Error
            [
              diagnostic offset
                (Printf.sprintf "C integer suffix `%s` is not valid in Fas; write `%s`"
                   raw unsuffixed);
            ]
        else if misplaced_separator then
          Error [ diagnostic offset "misplaced digit separator `_`" ]
        else if digits = "" then Error [ diagnostic offset "invalid integer literal" ]
        else if not (String.for_all valid_digit digits) then
          Error
            [
              diagnostic offset
                (Printf.sprintf
                   (if radix = 2 then "invalid digit in binary integer literal %S"
                    else "invalid digit in integer literal %S")
                   raw);
            ]
        else
          let sp = Source.span source ~start_offset:offset ~end_offset:stop in
          loop stop (count + 1) ({ Token.kind = Token.Int clean; span = sp } :: tokens)
      else if c = '\'' then
        let rec character_end i buffer =
          if at_line_end i then
            Error [ diagnostic offset "unterminated character literal" ]
          else if text.[i] = '\'' then
            let value = Buffer.contents buffer in
            if value = "" then Error [ diagnostic offset "empty character literal" ]
            else if String.length value <> 1 then
              Error
                [ diagnostic offset "character literal must contain exactly one byte" ]
            else
              let stop = i + 1 in
              let sp = Source.span source ~start_offset:offset ~end_offset:stop in
              let integer = string_of_int (Char.code value.[0]) in
              loop stop (count + 1)
                ({ Token.kind = Token.Int integer; span = sp } :: tokens)
          else if text.[i] = '\\' then (
            if at_line_end (i + 1) then
              Error [ diagnostic offset "unterminated character literal" ]
            else
              match decode_escape i with
              | Error diagnostics -> Error diagnostics
              | Ok (next, value) ->
                  Buffer.add_char buffer value;
                  character_end next buffer)
          else if Char.code text.[i] < 32 || Char.code text.[i] > 126 then
            Error
              [
                diagnostic offset "character literal must be printable ASCII; use \\xNN";
              ]
          else (
            Buffer.add_char buffer text.[i];
            character_end (i + 1) buffer)
        in
        match character_end (offset + 1) (Buffer.create 1) with
        | Error diagnostics -> Error diagnostics
        | Ok _ as result -> result
      else if c = '"' || (c = 'c' && offset + 1 < n && text.[offset + 1] = '"') then
        let string_start = if c = 'c' then offset + 1 else offset in
        let rec string_end i buffer =
          if i >= n then Error [ diagnostic offset "unterminated string literal" ]
          else
            match text.[i] with
            | '"' -> Ok (i + 1, Buffer.contents buffer)
            | '\\' when i + 1 >= n -> Error [ diagnostic i "unterminated escape" ]
            | '\\' -> (
                match decode_escape i with
                | Error diagnostics -> Error diagnostics
                | Ok (next, value) ->
                    Buffer.add_char buffer value;
                    string_end next buffer)
            | value ->
                Buffer.add_char buffer value;
                string_end (i + 1) buffer
        in
        match string_end (string_start + 1) (Buffer.create 16) with
        | Error e -> Error e
        | Ok (stop, value) ->
            let sp = Source.span source ~start_offset:offset ~end_offset:stop in
            loop stop (count + 1)
              ({
                 Token.kind =
                   (if c = 'c' then Token.CString value else Token.String value);
                 span = sp;
               }
              :: tokens)
      else
        let one kind =
          let sp = Source.span source ~start_offset:offset ~end_offset:(offset + 1) in
          loop (offset + 1) (count + 1) ({ Token.kind; span = sp } :: tokens)
        in
        let two next kind1 kind2 =
          if offset + 1 < n && text.[offset + 1] = next then
            let sp = Source.span source ~start_offset:offset ~end_offset:(offset + 2) in
            loop (offset + 2) (count + 1) ({ Token.kind = kind2; span = sp } :: tokens)
          else one kind1
        in
        match c with
        | '#' when offset + 8 <= n && String.sub text offset 8 = "#include" ->
            Error [ diagnostic offset "C `#include` is not Fas syntax; use `use`" ]
        | '(' -> one Lparen
        | ')' -> one Rparen
        | '{' -> one Lbrace
        | '}' -> one Rbrace
        | '[' -> one Lbracket
        | ']' -> one Rbracket
        | ',' -> one Comma
        | ';' -> one Semi
        | '@' -> one At
        | '?' ->
            Error [ diagnostic offset "Fas has no `?:`; write `if c { a } else { b }`" ]
        | ':' -> one Colon
        | '~' -> one Tilde
        | '.' when offset + 2 < n && text.[offset + 1] = '.' && text.[offset + 2] = '.'
          ->
            let sp = Source.span source ~start_offset:offset ~end_offset:(offset + 3) in
            loop (offset + 3) (count + 1)
              ({ Token.kind = Ellipsis; span = sp } :: tokens)
        | '.' when offset + 1 < n && text.[offset + 1] = '.' ->
            let sp = Source.span source ~start_offset:offset ~end_offset:(offset + 2) in
            loop (offset + 2) (count + 1) ({ Token.kind = Dot_dot; span = sp } :: tokens)
        | '.' -> one Dot
        | '+' -> two '=' Plus Plus_eq
        | '-' -> two '=' Minus Minus_eq
        | '*' -> two '=' Star Star_eq
        | '/' -> two '=' Slash Slash_eq
        | '%' -> two '=' Percent Percent_eq
        | '=' -> two '=' Assign Eqeq
        | '!' -> two '=' Not Neq
        | '<' ->
            if offset + 1 < n && text.[offset + 1] = '<' then
              if offset + 2 < n && text.[offset + 2] = '=' then
                let sp =
                  Source.span source ~start_offset:offset ~end_offset:(offset + 3)
                in
                loop (offset + 3) (count + 1)
                  ({ Token.kind = Ltlt_eq; span = sp } :: tokens)
              else
                let sp =
                  Source.span source ~start_offset:offset ~end_offset:(offset + 2)
                in
                loop (offset + 2) (count + 1)
                  ({ Token.kind = Ltlt; span = sp } :: tokens)
            else two '=' Lt Le
        | '>' ->
            if offset + 1 < n && text.[offset + 1] = '>' then
              if offset + 2 < n && text.[offset + 2] = '=' then
                let sp =
                  Source.span source ~start_offset:offset ~end_offset:(offset + 3)
                in
                loop (offset + 3) (count + 1)
                  ({ Token.kind = Gtgt_eq; span = sp } :: tokens)
              else
                let sp =
                  Source.span source ~start_offset:offset ~end_offset:(offset + 2)
                in
                loop (offset + 2) (count + 1)
                  ({ Token.kind = Gtgt; span = sp } :: tokens)
            else two '=' Gt Ge
        | '&' ->
            if offset + 1 < n && text.[offset + 1] = '&' then two '&' Amp Andand
            else two '=' Amp Amp_eq
        | '|' ->
            if offset + 1 < n && text.[offset + 1] = '|' then two '|' Pipe Oror
            else two '=' Pipe Pipe_eq
        | '^' -> two '=' Caret Caret_eq
        | _ -> Error [ diagnostic offset (Printf.sprintf "unexpected character %C" c) ]
  in
  loop 0 0 []
