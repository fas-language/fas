let source text = Source.create ~file:"regression.fas" ~text
let checks_run = ref 0

let contains text needle =
  let rec search offset =
    offset + String.length needle <= String.length text
    && (String.sub text offset (String.length needle) = needle || search (offset + 1))
  in
  needle = "" || search 0

let normalize_vector_spacing text =
  let output = Buffer.create (String.length text) in
  let rec copy index =
    if index < String.length text then
      if index + 2 <= String.length text && String.sub text index 2 = ", " then (
        Buffer.add_char output ',';
        copy (index + 2))
      else (
        Buffer.add_char output text.[index];
        copy (index + 1))
  in
  copy 0;
  Buffer.contents output

let positions text needle =
  let rec search offset acc =
    if offset + String.length needle > String.length text then List.rev acc
    else if String.sub text offset (String.length needle) = needle then
      search (offset + 1) (offset :: acc)
    else search (offset + 1) acc
  in
  if needle = "" then [] else search 0 []

let assert_deterministic name src first second =
  let rendered diagnostics = Diag.render_all ~source:(Some src) diagnostics in
  if rendered first <> rendered second then
    failwith (name ^ ": diagnostic changed between repeated checks")

let expect_ok = function
  | Ok value -> value
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)

let c_import_fixture file =
  let cwd = Sys.getcwd () in
  let candidates =
    [
      Filename.concat cwd ("test/c_import/" ^ file);
      Filename.concat cwd ("c_import/" ^ file);
    ]
  in
  let header =
    match List.find_opt Sys.file_exists candidates with
    | Some path -> path
    | None -> failwith ("missing C import fixture " ^ file)
  in
  let source = Filename.concat (Filename.dirname header) "probe.fas" in
  let request =
    C_import.
      { spelling = Ast.C_quoted (Filename.basename header); span = Span.synthetic }
  in
  let declarations, _, _ =
    expect_ok (C_import.import ~cc:"clang-22" ~keep:false source [ request ])
  in
  (source, C_import.map_declarations ~span:Span.synthetic declarations)

let c_import_system_fixture header =
  let source = Filename.concat (Sys.getcwd ()) "test/ir_simple.fas" in
  let alloc_size_parameters = ref [] in
  let declarations, _, _ =
    expect_ok
      (C_import.import ~alloc_size_out:alloc_size_parameters ~cc:"clang-22" ~keep:false
         source
         [ C_import.{ spelling = Ast.C_system header; span = Span.synthetic } ])
  in
  ( source,
    C_import.map_declarations ~alloc_size_parameters:!alloc_size_parameters
      ~span:Span.synthetic declarations )

let c_import_system_fixtures headers =
  let source = Filename.concat (Sys.getcwd ()) "test/ir_simple.fas" in
  let declarations, _, _ =
    expect_ok
      (C_import.import ~cc:"clang-22" ~keep:false source
         (List.map
            (fun header ->
              C_import.{ spelling = Ast.C_system header; span = Span.synthetic })
            headers))
  in
  (source, C_import.map_declarations ~span:Span.synthetic declarations)

let c_import_alloc_size_fixture file =
  let cwd = Sys.getcwd () in
  let header =
    match
      List.find_opt Sys.file_exists
        [
          Filename.concat cwd ("test/c_import/" ^ file);
          Filename.concat cwd ("c_import/" ^ file);
        ]
    with
    | Some path -> path
    | None -> failwith ("missing C import fixture " ^ file)
  in
  let source = Filename.concat (Filename.dirname header) "probe.fas" in
  let alloc_size_parameters = ref [] in
  let request =
    C_import.
      { spelling = Ast.C_quoted (Filename.basename header); span = Span.synthetic }
  in
  let declarations, _, _ =
    expect_ok
      (C_import.import ~alloc_size_out:alloc_size_parameters ~cc:"clang-22" ~keep:false
         source [ request ])
  in
  ( source,
    C_import.map_declarations ~alloc_size_parameters:!alloc_size_parameters
      ~span:Span.synthetic declarations )

let c_import_container ?(macro_names = []) name fragment_text =
  let source = Filename.temp_file ("fas-" ^ name ^ "-") ".fas" in
  let span = Span.make ~file:source ~start_offset:0 ~end_offset:0 ~line:1 ~column:1 in
  let request =
    C_import.{ spelling = Ast.C_fragment { tag = "UNION"; text = fragment_text }; span }
  in
  Fun.protect
    ~finally:(fun () -> try Sys.remove source with Sys_error _ -> ())
    (fun () ->
      let declarations, _, _ =
        expect_ok
          (C_import.import ~cc:"clang-22" ~keep:false ~macro_names source [ request ])
      in
      (source, C_import.map_declarations ~span declarations))

let c_import_macro_fixture ?(c_flags = []) file macro_names =
  let cwd = Sys.getcwd () in
  let header =
    match
      List.find_opt Sys.file_exists
        [
          Filename.concat cwd ("test/c_import/" ^ file);
          Filename.concat cwd ("c_import/" ^ file);
        ]
    with
    | Some path -> path
    | None -> failwith ("missing C import fixture " ^ file)
  in
  let source = Filename.concat (Filename.dirname header) "probe.fas" in
  let request =
    C_import.
      { spelling = Ast.C_quoted (Filename.basename header); span = Span.synthetic }
  in
  let declarations, _, _ =
    expect_ok
      (C_import.import ~cc:"clang-22" ~keep:false ~c_flags ~macro_names source
         [ request ])
  in
  (source, C_import.map_declarations ~span:Span.synthetic declarations)

let replay_replayed = ref 0
let replay_template = ref 0
let replay_prose = ref 0
let replay_unmatched = ref []
let replay_failures = ref []
let replay_seen = Hashtbl.create 256

let replay_program src =
  match Parser.parse src with Ok program -> Some program | Error _ -> None

let replay_quoted help =
  List.filteri (fun index _ -> index mod 2 = 1) (String.split_on_char '`' help)

let replay_span_text text (span : Span.t) =
  String.sub text span.Span.start_offset (span.Span.end_offset - span.Span.start_offset)

let replay_plain_name name =
  not
    (Names.builtin_name name
    || List.mem name Names.primitive_type_names
    || List.mem name Names.reserved_float_type_names
    || List.mem name Names.literal_names)

let replay_tokens text =
  match Lexer.lex (Source.create ~file:"replay" ~text) with
  | Error _ -> []
  | Ok tokens -> tokens

let replay_names text =
  List.filter_map
    (fun (token : Token.t) ->
      match token.Token.kind with
      | Token.Ident name when replay_plain_name name -> Some name
      | _ -> None)
    (replay_tokens text)
  |> List.sort_uniq compare

let replay_residual text =
  List.filter_map
    (fun (token : Token.t) ->
      match token.Token.kind with
      | Token.Ident name when replay_plain_name name -> Some ("name " ^ name)
      | Token.Int digits -> Some ("int " ^ digits)
      | Token.Float digits -> Some ("float " ^ digits)
      | Token.String value | Token.CString value -> Some ("string " ^ value)
      | _ -> None)
    (replay_tokens text)
  |> List.sort compare

let replay_is_name residual =
  String.length residual > 5 && String.sub residual 0 5 = "name "

let replay_name residual = String.sub residual 5 (String.length residual - 5)

let replay_index tokens =
  let array = Array.of_list tokens in
  let table = Hashtbl.create (Array.length array) in
  Array.iteri
    (fun index (token : Token.t) ->
      Hashtbl.replace table token.Token.span.Span.start_offset index)
    array;
  (array, table)

let replay_call_extent (array, table) (span : Span.t) =
  let count = Array.length array in
  let rec close index opener closer depth =
    if index >= count then None
    else
      let kind = array.(index).Token.kind in
      let depth =
        if kind = opener then depth + 1 else if kind = closer then depth - 1 else depth
      in
      if kind = closer && depth = 0 then Some index
      else close (index + 1) opener closer depth
  in
  match Hashtbl.find_opt table span.Span.start_offset with
  | Some index
    when match array.(index).Token.kind with Token.Ident _ -> true | _ -> false -> (
      let after =
        if index + 1 < count && array.(index + 1).Token.kind = Token.Lbracket then
          Option.map
            (fun close_index -> close_index + 1)
            (close (index + 1) Token.Lbracket Token.Rbracket 0)
        else Some (index + 1)
      in
      match after with
      | Some next when next < count && array.(next).Token.kind = Token.Lparen -> (
          match close next Token.Lparen Token.Rparen 0 with
          | Some last ->
              Some
                { span with Span.end_offset = array.(last).Token.span.Span.end_offset }
          | None -> None)
      | _ -> None)
  | _ -> None

let replay_unknown_token tokens (primary : Span.t) message =
  let inside (token : Token.t) =
    primary.Span.start_offset <= token.Token.span.Span.start_offset
    && token.Token.span.Span.end_offset <= primary.Span.end_offset
  in
  if String.starts_with ~prefix:"unknown " message then
    match replay_quoted message with
    | unknown :: _ ->
        List.find_map
          (fun (token : Token.t) ->
            if token.Token.kind = Token.Ident unknown && inside token then
              Some token.Token.span
            else None)
          tokens
    | [] -> None
  else None

let replay_join (left : Span.t) (right : Span.t) =
  {
    left with
    Span.start_offset = min left.Span.start_offset right.Span.start_offset;
    end_offset = max left.Span.end_offset right.Span.end_offset;
  }

let replay_target text tokens nodes (primary : Span.t) message names =
  let inside (outer : Span.t) (inner : Span.t) =
    outer.Span.start_offset <= inner.Span.start_offset
    && inner.Span.end_offset <= outer.Span.end_offset
  in
  let size (span : Span.t) = span.Span.end_offset - span.Span.start_offset in
  let same (node : Span.t) =
    node.Span.start_offset = primary.Span.start_offset
    && node.Span.end_offset = primary.Span.end_offset
  in
  match
    nodes
    |> List.filter (fun node -> inside node primary)
    |> List.filter (fun node -> replay_names (replay_span_text text node) = names)
    |> List.stable_sort (fun left right -> compare (size right) (size left))
  with
  | node :: _ -> Some node
  | [] -> (
      match List.find_opt same nodes with
      | Some node -> Some node
      | None -> replay_unknown_token tokens primary message)

let replay_nodes index (program : Ast.program) =
  let spans = ref [] in
  let open Ast in
  let extend span =
    match replay_call_extent index span with
    | Some extended -> replay_join span extended
    | None -> span
  in
  let rec expr (value : expr) =
    let own = extend (expr_span value) in
    let inner =
      match value with
      | Int_lit _ | Float_lit _ | Bool_lit _ | Null _ | String_lit _ | Ident _
      | Sizeof _ | Alignof _ | Offsetof _ ->
          []
      | Unary (_, operand, _)
      | C_dereference (operand, _, _)
      | C_dot_star (operand, _)
      | Parenthesized (operand, _)
      | Addr_of (operand, _)
      | Field (operand, _, _)
      | Arrow_field (operand, _, _, _)
      | Splat (operand, _)
      | Sizeof_value (operand, _)
      | Cast (_, _, operand, _)
      | Handle_from_addr (_, operand, _)
      | Designated_field (_, _, operand, _) ->
          [ expr operand ]
      | Binary (_, left, right, _) | Designated_index (left, right, _) ->
          [ expr left; expr right ]
      | Ternary (first, second, third, _) -> [ expr first; expr second; expr third ]
      | Call (callee, arguments, _) -> expr callee :: List.map expr arguments
      | Generic_args (operand, arguments, _) | Select (operand, arguments, _) ->
          expr operand :: List.concat_map generic arguments
      | Array_lit (values, _) -> List.map expr values
    in
    let extent = List.fold_left replay_join own inner in
    spans := extent :: !spans;
    extent
  and generic = function Const_arg value -> [ expr value ] | _ -> []
  and target = function
    | Target_ident _ -> []
    | Target_select (operand, arguments) ->
        expr operand :: List.concat_map generic arguments
    | Target_field (operand, _, _) -> [ expr operand ]
  and run value = ignore (expr value)
  and stmt (value : stmt) =
    match value with
    | Let { init; _ } -> Option.iter run init
    | View { place; _ } -> run place
    | Assign (assigned, operand, _) | Compound_assign (assigned, _, operand, _, _) ->
        ignore (target assigned);
        run operand
    | Return (operand, _) -> Option.iter run operand
    | If (condition, body, otherwise, _) ->
        run condition;
        List.iter stmt body;
        Option.iter (List.iter stmt) otherwise
    | While (_, condition, body, _) ->
        run condition;
        List.iter stmt body
    | Break _ | Continue _ -> ()
    | Defer (body, _) | Block (body, _) | Const_copy { body; _ } -> List.iter stmt body
    | Const_for { low; high; body; _ } ->
        run low;
        run high;
        List.iter stmt body
    | Expr_stmt (operand, _, _) -> run operand
    | For (_, initial, condition, step, body, _) ->
        Option.iter stmt initial;
        Option.iter run condition;
        Option.iter stmt step;
        List.iter stmt body
    | Switch (operand, cases, default, _) ->
        run operand;
        List.iter
          (fun (values, body) ->
            List.iter run values;
            List.iter stmt body)
          cases;
        Option.iter (List.iter stmt) default
  in
  List.iter
    (function
      | Const { value; _ } -> run value
      | Global { init; _ } -> Option.iter run init
      | Func { body = Statements statements; _ } -> List.iter stmt statements
      | _ -> ())
    program.items;
  !spans

let replay_mentions_vector message =
  let rec scan index =
    index + 4 <= String.length message
    && (String.sub message index 4 = "vec[" || scan (index + 1))
  in
  scan 0

let replay_check ~run ~text ~(target : Span.t) ~code ~message ~name_only =
  let replaced =
    String.sub text 0 target.Span.start_offset
    ^ code
    ^ String.sub text target.Span.end_offset
        (String.length text - target.Span.end_offset)
  in
  let after = run replaced in
  let region_end = target.Span.start_offset + String.length code in
  let gone =
    List.for_all (fun (diagnostic : Diag.t) -> diagnostic.Diag.message <> message) after
  in
  let clear =
    List.for_all
      (fun (diagnostic : Diag.t) ->
        let start = diagnostic.primary.Span.start_offset in
        start < target.Span.start_offset
        || start >= region_end
        || name_only
           && not (String.starts_with ~prefix:"unknown " diagnostic.Diag.message))
      after
  in
  if gone && clear then None else Some (replaced, after)

let replay_diagnostics ~run ~program ~text diagnostics =
  List.iter
    (fun (diagnostic : Diag.t) ->
      match diagnostic.Diag.help with
      | None -> ()
      | Some help ->
          let primary = diagnostic.primary in
          let key =
            ( text,
              diagnostic.Diag.message,
              primary.Span.start_offset,
              primary.Span.end_offset,
              help )
          in
          if not (Hashtbl.mem replay_seen key) then (
            Hashtbl.add replay_seen key ();
            let tokens = replay_tokens text in
            let index = replay_index tokens in
            let primary_names =
              List.filter_map
                (fun (token : Token.t) ->
                  match token.Token.kind with
                  | Token.Ident name
                    when primary.Span.start_offset <= token.Token.span.Span.start_offset
                         && token.Token.span.Span.end_offset <= primary.Span.end_offset
                    ->
                      Some name
                  | _ -> None)
                tokens
            in
            let rewrites =
              List.filter
                (fun code ->
                  match replay_names code with
                  | [ name ] ->
                      not (String.trim code = name && List.mem name primary_names)
                  | _ -> true)
                (replay_quoted help)
            in
            match
              List.find_map
                (fun code ->
                  match replay_residual code with
                  | [] -> None
                  | residual -> Some (code, residual))
                rewrites
            with
            | None -> incr replay_prose
            | Some (code, residual) -> (
                let source_names = replay_names text in
                if
                  List.exists
                    (fun r ->
                      replay_is_name r && not (List.mem (replay_name r) source_names))
                    residual
                then incr replay_template
                else
                  let nodes =
                    match program with
                    | None -> []
                    | Some program -> replay_nodes index program
                  in
                  match
                    replay_target text tokens nodes primary diagnostic.Diag.message
                      (replay_names code)
                  with
                  | None ->
                      replay_unmatched :=
                        Printf.sprintf "line %d: `%s` for %s" primary.Span.line help
                          diagnostic.Diag.message
                        :: !replay_unmatched
                  | Some target -> (
                      let name_only =
                        String.starts_with ~prefix:"did you mean " help
                        && replay_names code = [ String.trim code ]
                      in
                      match
                        replay_check ~run ~text ~target ~code
                          ~message:diagnostic.Diag.message ~name_only
                      with
                      | None -> incr replay_replayed
                      | Some (_, after)
                        when List.exists
                               (fun (other : Diag.t) ->
                                 let start = other.Diag.primary.Span.start_offset in
                                 target.Span.start_offset <= start
                                 && start
                                    < target.Span.start_offset + String.length code
                                 && replay_mentions_vector other.Diag.message)
                               after ->
                          replay_unmatched :=
                            Printf.sprintf "line %d: `%s` for %s (vector operands)"
                              primary.Span.line help diagnostic.Diag.message
                            :: !replay_unmatched
                      | Some (replaced, after) ->
                          replay_failures :=
                            Printf.sprintf "line %d: `%s` for %s\n%s" primary.Span.line
                              help diagnostic.Diag.message
                              (Diag.render_all ~source:(Some (source replaced)) after)
                            :: !replay_failures))))
    diagnostics

let semantic_replay_run text =
  match Parser.parse (source text) with
  | Error diagnostics -> diagnostics
  | Ok program -> (
      match Sema.check program with Ok _ -> [] | Error diagnostics -> diagnostics)

let parse_replay_run text =
  match Parser.parse (source text) with Ok _ -> [] | Error diagnostics -> diagnostics

let c_replay_run ((file, imported) : string * C_import.mapped) text =
  match Parser.parse (Source.create ~file ~text) with
  | Error diagnostics -> diagnostics
  | Ok program -> (
      let program = { Ast.items = program.items @ imported.items } in
      match
        Sema.check ~c_aliases:imported.aliases ~c_unsupported:imported.unsupported
          ~c_nonnull_parameters:imported.nonnull_parameters
          ~c_records:imported.record_types
          ~c_string_parameters:imported.c_string_parameters
          ~c_record_calls:imported.record_calls
          ~c_alloc_size_parameters:imported.alloc_size_parameters program
      with
      | Ok _ -> []
      | Error diagnostics -> diagnostics)

let c_semantic_result ((source, imported) : string * C_import.mapped) text =
  incr checks_run;
  let src = Source.create ~file:source ~text in
  let run () =
    let program =
      match Parser.parse src with
      | Ok program -> program
      | Error diagnostics ->
          failwith (Diag.render_all ~source:None diagnostics ^ "\n" ^ text)
    in
    let program = { Ast.items = program.items @ imported.items } in
    Sema.check ~c_aliases:imported.aliases ~c_unsupported:imported.unsupported
      ~c_nonnull_parameters:imported.nonnull_parameters ~c_records:imported.record_types
      ~c_string_parameters:imported.c_string_parameters
      ~c_record_calls:imported.record_calls
      ~c_alloc_size_parameters:imported.alloc_size_parameters program
  in
  match run () with
  | Ok _ as result -> result
  | Error diagnostics as result ->
      let repeated =
        match run () with
        | Error repeated -> repeated
        | Ok _ -> failwith "C semantic result changed between repeated checks"
      in
      assert_deterministic "C semantic rejection" src diagnostics repeated;
      replay_diagnostics
        ~run:(c_replay_run (source, imported))
        ~program:(replay_program src) ~text diagnostics;
      result

let c_semantic_accept name imported text =
  match c_semantic_result imported text with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        (name ^ ": unexpected rejection: " ^ Diag.render_all ~source:None diagnostics)

let c_semantic_message name expected imported text =
  match c_semantic_result imported text with
  | Ok _ -> failwith (name ^ ": expected rejection")
  | Error diagnostics -> (
      match List.map (fun (diagnostic : Diag.t) -> diagnostic.message) diagnostics with
      | [ actual ] when actual = expected -> ()
      | actual ->
          failwith
            (name ^ ": expected [" ^ expected ^ "], got [" ^ String.concat "; " actual
           ^ "]"))

let c_semantic_pin name expected_help imported text line column width message =
  match c_semantic_result imported text with
  | Error [ diagnostic ]
    when diagnostic.Diag.primary.Span.line = line
         && diagnostic.primary.Span.column = column
         && diagnostic.primary.Span.end_offset - diagnostic.primary.Span.start_offset
            = width
         && diagnostic.message = message
         && diagnostic.help = Some expected_help ->
      ()
  | Error diagnostics ->
      failwith
        (name ^ ": unexpected diagnostic: "
        ^ Diag.render_all ~source:(Some (source text)) diagnostics)
  | Ok _ -> failwith (name ^ ": expected rejection")

let c_semantic_error name fragment imported text =
  match c_semantic_result imported text with
  | Ok _ -> failwith (name ^ ": expected rejection")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let parse_file file text = expect_ok (Parser.parse (Source.create ~file ~text))

let check_files files =
  incr checks_run;
  let items =
    files
    |> List.map (fun (file, text) -> (parse_file file text).Ast.items)
    |> List.concat
  in
  Sema.check { Ast.items }

let semantic_diagnostics text =
  incr checks_run;
  let src = source text in
  let run () =
    let program = expect_ok (Parser.parse src) in
    match Sema.check program with
    | Ok _ -> failwith "expected semantic rejection"
    | Error diagnostics -> diagnostics
  in
  let first = run () in
  let second = run () in
  assert_deterministic "semantic rejection" src first second;
  replay_diagnostics ~run:semantic_replay_run ~program:(replay_program src) ~text first;
  first

let semantic_messages text =
  List.map (fun (diagnostic : Diag.t) -> diagnostic.message) (semantic_diagnostics text)

let semantic_message name expected text =
  match semantic_diagnostics text with
  | diagnostics -> (
      let actual =
        List.map (fun (diagnostic : Diag.t) -> diagnostic.message) diagnostics
      in
      match actual with
      | [ actual ] when actual = expected -> ()
      | actual ->
          failwith
            (name ^ ": expected [" ^ expected ^ "], got [" ^ String.concat "; " actual
           ^ "]"))

let semantic_note name expected text =
  match semantic_diagnostics text with
  | [ diagnostic ]
    when List.exists (fun note -> contains note expected) diagnostic.Diag.notes ->
      ()
  | diagnostics ->
      failwith
        (name ^ ": expected note [" ^ expected ^ "], got "
        ^ Diag.render_all ~source:(Some (source text)) diagnostics)

let semantic_error name fragment text =
  let diagnostics = semantic_diagnostics text in
  let rendered = Diag.render_all ~source:None diagnostics in
  let normalized_rendered = normalize_vector_spacing rendered
  and normalized_fragment = normalize_vector_spacing fragment in
  let structured_cast =
    fragment = "illegal cast for source and destination widths"
    &&
    match diagnostics with
    | [ diagnostic ] ->
        String.starts_with ~prefix:"illegal `" diagnostic.Diag.message
        && contains diagnostic.message " from "
        && contains diagnostic.message " to "
        && contains diagnostic.message ": "
    | _ -> false
  in
  let cast_target =
    fragment = "illegal cast target type"
    &&
    match diagnostics with
    | [ diagnostic ] ->
        String.starts_with ~prefix:"illegal `" diagnostic.Diag.message
        && contains diagnostic.message " target `"
        && contains diagnostic.message ": expected "
    | _ -> false
  in
  let aggregate_diagnostic =
    (contains fragment "aggregate parameter"
    || contains fragment "aggregate result"
    || contains fragment "aggregate argument")
    &&
    match diagnostics with
    | [ diagnostic ] ->
        contains diagnostic.Diag.message "cannot be passed by value"
        || contains diagnostic.message "cannot be returned by value"
    | _ -> false
  in
  if
    not
      (contains rendered fragment
      || contains normalized_rendered normalized_fragment
      || structured_cast || cast_target || aggregate_diagnostic)
  then failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let semantic_render text =
  let src = source text in
  let diagnostics = semantic_diagnostics text in
  Diag.render_all ~source:(Some src) diagnostics

let semantic_same_error name left right =
  let diagnostic text =
    match semantic_diagnostics text with
    | [ diagnostic ] ->
        let message = diagnostic.Diag.message in
        let message =
          match positions message " is computed in " with
          | position :: _ ->
              String.sub message position (String.length message - position)
          | [] -> message
        in
        (message, Option.is_some diagnostic.help)
    | diagnostics ->
        failwith
          (name ^ ": expected one diagnostic, got "
          ^ Diag.render_all ~source:(Some (source text)) diagnostics)
  in
  if diagnostic left <> diagnostic right then
    failwith
      (name ^ ": if-expression diagnostic differed from plain-operand diagnostic\n"
     ^ semantic_render left ^ semantic_render right)

let semantic_pin name text line column width message help =
  match semantic_diagnostics text with
  | [ diagnostic ]
    when diagnostic.Diag.primary.Span.line = line
         && diagnostic.primary.Span.column = column
         && diagnostic.primary.Span.end_offset - diagnostic.primary.Span.start_offset
            = width
         && diagnostic.message = message && diagnostic.help = help ->
      ()
  | diagnostics ->
      let actual_span =
        match diagnostics with
        | [ diagnostic ] ->
            Printf.sprintf " actual=(%d,%d,%d)" diagnostic.Diag.primary.Span.line
              diagnostic.primary.Span.column
              (diagnostic.primary.Span.end_offset - diagnostic.primary.Span.start_offset)
        | _ -> ""
      in
      failwith
        (name ^ ": unexpected diagnostic: " ^ actual_span
        ^ Diag.render_all ~source:(Some (source text)) diagnostics)

let expected_diagnostic ?(line_number = 1) line column width message help =
  String.concat "\n"
    [
      Printf.sprintf "regression.fas:%d:%d: error: %s" line_number column message;
      "  " ^ line;
      "  " ^ String.make (column - 1) ' ' ^ "^" ^ String.make (width - 1) '~';
      "help: " ^ help;
    ]
  ^ "\n"

let expected_diagnostic_without_help ?(line_number = 1) line column width message =
  String.concat "\n"
    [
      Printf.sprintf "regression.fas:%d:%d: error: %s" line_number column message;
      "  " ^ line;
      "  " ^ String.make (column - 1) ' ' ^ "^" ^ String.make (width - 1) '~';
    ]
  ^ "\n"

let semantic_accept name text =
  incr checks_run;
  let program = expect_ok (Parser.parse (source text)) in
  match Sema.check program with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        (name ^ ": unexpected rejection: " ^ Diag.render_all ~source:None diagnostics)

let semantic_constant name expected text =
  incr checks_run;
  let program = expect_ok (Parser.parse (source text)) |> Sema.check |> expect_ok in
  match
    List.find_opt
      (fun (constant : Hir.const_def) -> constant.name = name)
      program.consts
  with
  | Some { bits; _ } when bits = expected -> ()
  | Some { bits; _ } ->
      failwith (Printf.sprintf "%s: expected %Ld, got %Ld" name expected bits)
  | None -> failwith ("missing constant " ^ name)

let conversion_help_twin name text original replacement help =
  match semantic_diagnostics text with
  | [ diagnostic ] when diagnostic.Diag.help = Some help ->
      if not (contains help ("`" ^ replacement ^ "`")) then
        failwith (name ^ ": expected help to contain `" ^ replacement ^ "`");
      let span_start = diagnostic.Diag.primary.Span.start_offset in
      let span_end = diagnostic.Diag.primary.Span.end_offset in
      let score start =
        if start <= span_end && start + String.length original >= span_start then 0
        else min (abs (start - span_start)) (abs (start - span_end))
      in
      let candidates = positions text original in
      let start =
        match candidates with
        | [] -> failwith (name ^ ": source expression was not found")
        | first :: rest ->
            List.fold_left
              (fun best candidate ->
                if score candidate < score best then candidate else best)
              first rest
      in
      let length = String.length original in
      let updated =
        String.sub text 0 start ^ replacement
        ^ String.sub text (start + length) (String.length text - start - length)
      in
      semantic_accept name updated
  | diagnostics ->
      failwith
        (name ^ ": unexpected conversion help: "
        ^ Diag.render_all ~source:(Some (source text)) diagnostics)

let parse_diagnostics name text =
  incr checks_run;
  let src = source text in
  let run () =
    match Parser.parse src with
    | Ok _ -> failwith (name ^ ": expected parse rejection")
    | Error diagnostics -> diagnostics
  in
  let first = run () in
  let second = run () in
  assert_deterministic name src first second;
  replay_diagnostics ~run:parse_replay_run ~program:None ~text first;
  first

let syntax_pin name text line column width message =
  match parse_diagnostics name text with
  | [ diagnostic ]
    when diagnostic.Diag.primary.Span.line = line
         && diagnostic.primary.Span.column = column
         && diagnostic.primary.Span.end_offset - diagnostic.primary.Span.start_offset
            = width
         && diagnostic.message = message ->
      ()
  | diagnostics ->
      failwith
        (name ^ ": unexpected diagnostic: "
        ^ Diag.render_all ~source:(Some (source text)) diagnostics)

let caret_render_pin name text start_offset end_offset expected =
  let span =
    Span.make ~file:"regression.fas" ~start_offset ~end_offset ~line:1
      ~column:(start_offset + 1)
  in
  let rendered =
    Diag.render_all ~source:(Some (source text)) [ Diag.error span "caret probe" ]
  in
  if rendered <> expected then
    failwith (name ^ ": unexpected caret rendering\n" ^ rendered)

let parse_pin name text line column width message help =
  match parse_diagnostics name text with
  | [ diagnostic ]
    when diagnostic.Diag.primary.Span.line = line
         && diagnostic.primary.Span.column = column
         && diagnostic.primary.Span.end_offset - diagnostic.primary.Span.start_offset
            = width
         && diagnostic.message = message && diagnostic.help = Some help ->
      ()
  | diagnostics ->
      failwith
        (name ^ ": unexpected diagnostic: "
        ^ Diag.render_all ~source:(Some (source text)) diagnostics)

let parse_error name text = ignore (parse_diagnostics name text)

let parse_messages text =
  parse_diagnostics "parse rejection" text
  |> List.map (fun (diagnostic : Diag.t) -> diagnostic.message)

let parse_message name expected text =
  match parse_messages text with
  | [ actual ] when actual = expected -> ()
  | actual ->
      failwith
        (name ^ ": expected [" ^ expected ^ "], got [" ^ String.concat "; " actual ^ "]")

let parse_error_message name fragment text =
  let diagnostics = parse_diagnostics name text in
  let rendered = Diag.render_all ~source:None diagnostics in
  if not (contains rendered fragment) then
    failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let all_errors_pin name text expected =
  incr checks_run;
  let program = expect_ok (Parser.parse (source text)) in
  let shown diagnostics =
    List.map
      (fun (diagnostic : Diag.t) -> (diagnostic.primary.Span.line, diagnostic.message))
      diagnostics
  in
  let render diagnostics =
    String.concat "; "
      (List.map
         (fun (line, message) -> string_of_int line ^ ": " ^ message)
         diagnostics)
  in
  (match Sema.check ~all_errors:true program with
  | Error diagnostics when shown diagnostics = expected -> ()
  | Error diagnostics ->
      failwith
        (name ^ ": expected [" ^ render expected ^ "], got ["
        ^ render (shown diagnostics)
        ^ "]")
  | Ok _ -> failwith (name ^ ": expected rejection"));
  match (Sema.check program, expected) with
  | Error [ diagnostic ], first :: _ when shown [ diagnostic ] = [ first ] -> ()
  | Error diagnostics, _ ->
      failwith (name ^ ": without --all-errors got [" ^ render (shown diagnostics) ^ "]")
  | Ok _, _ -> failwith (name ^ ": expected rejection without --all-errors")

let cli_error name fragment args =
  incr checks_run;
  match Cli.parse (Array.of_list ("fas" :: args)) with
  | Error message when contains message fragment -> ()
  | Error message -> failwith (name ^ ": unexpected diagnostic: " ^ message)
  | Ok _ -> failwith (name ^ ": expected CLI rejection")

let cli_run args =
  incr checks_run;
  match Cli.parse (Array.of_list ("fas" :: args)) with
  | Ok (Cli.Run config) -> config
  | Ok Cli.Help -> failwith "expected compiler invocation"
  | Ok Cli.Version -> failwith "expected compiler invocation"
  | Ok (Cli.Run_program _) -> failwith "expected compiler invocation"
  | Ok (Cli.Check _) -> failwith "expected compiler invocation"
  | Ok (Cli.Perf _) -> failwith "expected compiler invocation"
  | Ok (Cli.Lsp _) -> failwith "expected compiler invocation"
  | Error message -> failwith message

let lower_of text =
  incr checks_run;
  let program = expect_ok (Parser.parse (source text)) in
  let hir = expect_ok (Sema.check program) in
  expect_ok (Lower.lower hir)

let llvm_of text = Ir.render (lower_of text)

let lower_struct_error name fragment (struct_def : Hir.struct_def) =
  incr checks_run;
  match
    Lower.lower
      {
        Hir.structs = [ struct_def ];
        consts = [];
        const_arrays = [];
        globals = [];
        funcs = [];
        strings = [];
      }
  with
  | Ok _ -> failwith (name ^ ": malformed struct lowered without error")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let lower_function_error name fragment params body =
  incr checks_run;
  match
    Lower.lower
      {
        Hir.structs = [];
        consts = [];
        const_arrays = [];
        globals = [];
        funcs =
          [
            {
              Hir.name;
              params;
              ret = Hir.Void;
              body = Hir.Statements body;
              linkage = Hir.Internal;
              variadic = false;
              span = Span.synthetic;
              display = None;
            };
          ];
        strings = [];
      }
  with
  | Ok _ -> failwith (name ^ ": malformed HIR lowered without error")
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered fragment) then
        failwith (name ^ ": unexpected diagnostic: " ^ rendered)

let () =
  if
    C_import_json.field "name"
      (C_import_json.make_obj
         [ ("name", C_import_json.Str "first"); ("name", C_import_json.Str "last") ])
    <> Some (C_import_json.Str "first")
  then failwith "C import JSON field lookup no longer keeps the first key";
  if
    Hir.Hashtbl.find_opt (Hir.first_index [ ("name", 1); ("name", 2) ]) "name" <> Some 1
  then failwith "C import metadata lookup no longer keeps the first key";
  let long_sum = String.concat " + " (List.init 100_000 (fun _ -> "1")) in
  semantic_accept "long-arithmetic-expression"
    ("fn main() i32 { value i32 = 1\nreturn value + " ^ long_sum ^ " }\n");
  let expect_layout name expected ty =
    match Hir.layout [] ty with
    | Ok actual when actual = expected -> ()
    | Ok (size, align) ->
        failwith
          (Printf.sprintf "%s: expected layout (%d, %d), got (%d, %d)" name
             (fst expected) (snd expected) size align)
    | Error message -> failwith (name ^ ": " ^ message)
  in
  expect_layout "layout-v3i1" (1, 1) (Hir.Vec (3, Hir.Bool));
  expect_layout "layout-v9i1" (2, 2) (Hir.Vec (9, Hir.Bool));
  expect_layout "layout-v3i8" (4, 4) (Hir.Vec (3, Hir.Int Hir.U8));
  expect_layout "layout-v3i32" (16, 16) (Hir.Vec (3, Hir.Int Hir.I32));
  expect_layout "layout-v2i64" (16, 16) (Hir.Vec (2, Hir.Int Hir.I64));
  expect_layout "layout-array-v3i32" (32, 16)
    (Hir.Array (2, Hir.Vec (3, Hir.Int Hir.I32)));
  if Hir.ty_equal (Hir.Int Hir.Usize) (Hir.Int Hir.U64) then
    failwith "usize-identity: usize collapsed into u64";
  if Hir.ty_equal (Hir.Int Hir.Isize) (Hir.Int Hir.I64) then
    failwith "isize-identity: isize collapsed into i64";
  let target32 =
    { (Target_layout.current ()) with pointer_size = 4; pointer_align = 4 }
  in
  (match Hir.layout ~target:target32 [] (Hir.Int Hir.Usize) with
  | Ok (4, 4) -> ()
  | Ok (size, align) ->
      failwith (Printf.sprintf "usize-layout: expected (4, 4), got (%d, %d)" size align)
  | Error message -> failwith ("usize-layout: " ^ message));
  (match Hir.layout ~target:target32 [] (Hir.Int Hir.Isize) with
  | Ok (4, 4) -> ()
  | Ok (size, align) ->
      failwith (Printf.sprintf "isize-layout: expected (4, 4), got (%d, %d)" size align)
  | Error message -> failwith ("isize-layout: " ^ message));
  semantic_accept "usize-return-from-u64"
    "fn convert(value u64) usize { return value }\n";
  semantic_accept "u64-return-from-usize"
    "fn convert(value usize) u64 { return value }\n";
  semantic_accept "isize-return-from-i64"
    "fn convert(value i64) isize { return value }\n";
  semantic_accept "i64-return-from-isize"
    "fn convert(value isize) i64 { return value }\n";
  semantic_accept "usize-call-from-u64"
    "fn take(value usize) void { return }\n\
     fn check_case(value u64) void { take(value) }\n";
  semantic_error "named-bool-constant-keeps-type" "cannot assign `bool` to `i32`"
    "const Flag bool = true\nfn main() i32 { value i32 = Flag\n return value }\n";
  semantic_accept "named-i8-constant-local-widens"
    "const Small i8 = 7\nfn main() i32 { value i32 = Small\n return value }\n";
  semantic_error "named-negative-i8-constant-keeps-type"
    "negative values change meaning"
    "const Negative i8 = -1\nfn main() i32 { value u16 = Negative\n return 0 }\n";
  semantic_accept "named-i8-constant-return-widens"
    "const Small i8 = 7\nfn value() i32 { return Small }\n";
  semantic_accept "named-i8-constant-argument-widens"
    "const Small i8 = 7\n\
     fn take(value i32) void { return }\n\
     fn check_case() void { take(Small) }\n";
  semantic_accept "named-i8-constant-binary-widens"
    "const Small i8 = 7\nfn add(value i32) i32 { return Small + value }\n";
  semantic_accept "named-i8-constant-generic-argument-widens"
    "const Small i8 = 7\n\
     fn value[N const i32]() i32 { return N }\n\
     fn check_case() i32 { return value[Small]() }\n";
  let named_constant_explicit_conversions =
    llvm_of
      "const Negative i8 = -1\n\
       const Byte u8 = 255\n\
       fn signed_value() i32 { return sext[i32](Negative) }\n\
       fn unsigned_value() i32 { return zext[i32](Byte) }\n\
       fn byte_value() u8 { return Byte }\n\
       fn contextual_literal() i32 { return 1 }\n"
  in
  List.iter
    (fun expected ->
      if not (contains named_constant_explicit_conversions expected) then
        failwith ("named-constant-explicit-conversions: missing `" ^ expected ^ "`"))
    [
      "define internal i32 @signed_value";
      "sext i8 255 to i32";
      "define internal i32 @unsigned_value";
      "zext i8 255 to i32";
      "define internal i8 @byte_value";
      "ret i8 255\n";
      "ret i32 1\n";
    ];
  semantic_error "constant-zext-must-widen"
    "illegal cast for source and destination widths"
    "const X u8 = zext[u8](256)\nfn test() u8 { return X }\n";
  semantic_error "constant-zext-equal-width"
    "illegal cast for source and destination widths"
    "const A u8 = 1\nconst X u8 = zext[u8](A)\nfn test() u8 { return X }\n";
  semantic_error "constant-sext-equal-width"
    "illegal cast for source and destination widths"
    "const A i32 = 1\nconst X i32 = sext[i32](A)\nfn main() i32 { return X }\n";
  semantic_error "constant-trunc-equal-width"
    "illegal cast for source and destination widths"
    "const A i32 = 1\nconst X i32 = trunc[i32](A)\nfn main() i32 { return X }\n";
  semantic_error "runtime-zext-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value u8) u8 { return zext[u8](value) }\n";
  semantic_error "runtime-sext-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value i32) i32 { return sext[i32](value) }\n";
  semantic_error "runtime-trunc-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value i32) i32 { return trunc[i32](value) }\n";
  semantic_error "constant-trunc-bool-source"
    "illegal cast for source and destination widths"
    "const X u8 = trunc[u8](true)\nfn test() u8 { return X }\n";
  semantic_error "runtime-trunc-bool-source"
    "illegal cast for source and destination widths"
    "fn f(value bool) u8 { return trunc[u8](value) }\n";
  semantic_error "constant-vector-bitcast-width"
    "illegal cast for source and destination widths"
    "const A i32 = 1\n\
     const X vec[2,i32] = bitcast[vec[2,i32]](A)\n\
     fn main() i32 { return 0 }\n";
  let constant_vector_bitcasts =
    llvm_of
      "const A i64 = 1\n\
       const X vec[2,i32] = bitcast[vec[2,i32]](A)\n\
       const Y vec[4,u16] = bitcast[vec[4,u16]](X)\n\
       const Z i64 = bitcast[i64](Y)\n\
       fn x_lane() i32 { return X[1] }\n\
       fn y_lane() u16 { return Y[2] }\n\
       fn main() i32 { return trunc[i32](Z) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains constant_vector_bitcasts marker) then
        failwith ("constant-vector-bitcasts: missing `" ^ marker ^ "`"))
    [ "<i32 1, i32 0>"; "<i16 1, i16 0, i16 0, i16 0>"; "trunc i64 1 to i32" ];
  let constant_casts =
    llvm_of
      "const Byte u8 = 255\n\
       const Negative i8 = -1\n\
       const Word u16 = 258\n\
       const OddWord u16 = 259\n\
       const Zero bool = trunc[bool](Word)\n\
       const One bool = trunc[bool](OddWord)\n\
       const WideUnsigned u16 = zext[u16](Byte)\n\
       const WideSigned i16 = sext[i16](Negative)\n\
       const Narrow u8 = trunc[u8](Word)\n\
       const Bits i8 = bitcast[i8](Byte)\n\
       const SignedTruth i16 = sext[i16](true)\n\
       fn constants() i32 {\n\
      \ return zext[i32](WideUnsigned) + sext[i32](WideSigned) +zext[i32](Narrow) + \
       sext[i32](Bits) + sext[i32](SignedTruth) +zext[i32](Zero) +zext[i32](One)\n\
       }\n\
       fn runtime_sext(value bool) i16 { return sext[i16](value) }\n\
       fn runtime_low_bit(value u8) bool { return trunc[bool](value) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains constant_casts marker) then
        failwith ("constant-casts: missing `" ^ marker ^ "`"))
    [
      "zext i16 255 to i32";
      "sext i16 65535 to i32";
      "zext i8 2 to i32";
      "sext i8 255 to i32";
      "zext i1 false to i32";
      "zext i1 true to i32";
      "sext i1 ";
      "trunc i8 ";
    ];
  let target_width_integers =
    llvm_of
      "struct Pair { left u8 right u64 }\n\
       const Values arr[3,u8] = {1, 2, 3}\n\
       fn to_u64(value usize) u64 { return bitcast[u64](value) }\n\
       fn to_i64(value isize) i64 { return bitcast[i64](value) }\n\
       fn unsigned_div(left usize, right usize) usize { return left / right }\n\
       fn signed_div(left isize, right isize) isize { return left / right }\n\
       fn sizes() usize {\n\
      \ return sizeof[Pair] + alignof[Pair] + offsetof[Pair,right] + len(Values)\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains target_width_integers marker) then
        failwith ("target-width-integers: missing `" ^ marker ^ "`"))
    [
      "define internal i64 @to_u64(i64";
      "define internal i64 @to_i64(i64";
      "udiv i64";
      "sdiv i64";
    ];
  let integer_vector_bitcasts =
    llvm_of
      "fn same_shape(values vec[4,i32]) vec[4,u32] {\n\
      \ return bitcast[vec[4,u32]](values)\n\
      \ }\n\
      \ fn reshape(value u64) u64 {\n\
      \ words vec[2,u32] = bitcast[vec[2,u32]](value)\n\
      \ bytes vec[8,u8] = bitcast[vec[8,u8]](words)\n\
      \ return bitcast[u64](bytes)\n\
      \ }\n\
      \ fn flags(value vec[8,bool]) u8 { return bitcast[u8](value) }\n"
  in
  if List.length (positions integer_vector_bitcasts "bitcast ") <> 5 then
    failwith "integer-vector-bitcast: expected five mechanical LLVM bitcasts";
  List.iter
    (fun forbidden ->
      if contains integer_vector_bitcasts forbidden then
        failwith ("integer-vector-bitcast: unexpected `" ^ forbidden ^ "` lowering"))
    [ "extractelement"; "insertelement"; "shufflevector" ];
  semantic_error "integer-vector-bitcast-width"
    "illegal cast for source and destination widths"
    "fn f(value u64) vec[4,u32] { return bitcast[vec[4,u32]](value) }\n";
  semantic_error "integer-vector-bitcast-implicit"
    "is `vec[4, i32]`, expected `vec[4, u32]`"
    "fn f(value vec[4,i32]) vec[4,u32] { return value }\n";
  semantic_error "integer-vector-bitcast-array"
    "raw access cannot load an array or struct value"
    "fn f(value addr) vec[2,u32] { return bitcast[vec[2,u32]](value[arr[2,u32]]) }\n";
  semantic_error "integer-vector-bitcast-struct"
    "raw access cannot load an array or struct value"
    "struct Pair { left u32 right u32 }\n\
    \ fn f(value addr) vec[2,u32] { return bitcast[vec[2,u32]](value[Pair]) }\n";
  semantic_error "integer-vector-bitcast-pointer"
    "illegal cast for source and destination widths"
    "fn f(value addr) vec[1,u64] { return bitcast[vec[1,u64]](value) }\n";
  let integer_vector_conversions =
    llvm_of
      "fn widen_unsigned(value vec[4,u8]) vec[4,u16] {\n\
      \ return zext[vec[4,u16]](value)\n\
       }\n\
       fn widen_signed(value vec[4,i8]) vec[4,i16] {\n\
      \ return sext[vec[4,i16]](value)\n\
       }\n\
       fn narrow(value vec[4,u16]) vec[4,u8] {\n\
      \ return trunc[vec[4,u8]](value)\n\
       }\n\
       fn truth_bits(value vec[4,u8]) vec[4,bool] {\n\
      \ return trunc[vec[4,bool]](value)\n\
       }\n\
       fn widen_bool_unsigned(value vec[4,bool]) vec[4,u8] {\n\
      \ return zext[vec[4,u8]](value)\n\
       }\n\
       fn widen_bool_signed(value vec[4,bool]) vec[4,i8] {\n\
      \ return sext[vec[4,i8]](value)\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains integer_vector_conversions marker) then
        failwith ("integer-vector-conversions: missing `" ^ marker ^ "`"))
    [ "zext <4 x i8>"; "sext <4 x i8>"; "trunc <4 x i16>"; "trunc <4 x i8>" ];
  semantic_error "integer-vector-zext-lane-count"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u8]) vec[2,u16] { return zext[vec[2,u16]](value) }\n";
  semantic_error "integer-vector-zext-must-widen"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u16]) vec[4,u8] { return zext[vec[4,u8]](value) }\n";
  semantic_error "integer-vector-trunc-must-narrow"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u8]) vec[4,u16] { return trunc[vec[4,u16]](value) }\n";
  semantic_error "integer-vector-bitcast-opaque"
    "illegal cast for source and destination widths"
    "opaque Handle\nfn f(value i64) void { return bitcast[Handle](value) }\n";
  semantic_error "integer-vector-bitcast-void"
    "illegal cast for source and destination widths"
    "fn f(value i64) void { return bitcast[void](value) }\n";
  semantic_error "constant-bool-arithmetic"
    "operator `+` needs integer or integer vector operands"
    "const Invalid bool = true + true\nfn main() i32 { return 0 }\n";
  semantic_error "constant-bool-shift"
    "left operand of `<<` has type `bool`, expected an integer or integer vector"
    "const Invalid bool = true << false\nfn main() i32 { return 0 }\n";
  semantic_error "constant-dead-ternary-type" "cannot use `bool` as `i32`"
    "const Invalid i32 = if true { 1 } else { false }\n\
     fn main() i32 { return Invalid }\n";
  let const_builtin_if_source =
    "const A u8 = 1\n\
     const CLZ u32 = clz(if true { A } else { A })\n\
     const ROTR u32 = rotr(if true { A } else { A }, 1)\n\
     const SAT u32 = add_sat(if true { A } else { A }, A)\n"
  in
  semantic_constant "CLZ" 7L const_builtin_if_source;
  semantic_constant "ROTR" 128L const_builtin_if_source;
  semantic_constant "SAT" 2L const_builtin_if_source;
  semantic_accept "builtin-if-const-runtime-parity"
    (const_builtin_if_source
   ^ "fn run() u32 {\n\
      x u32 = clz(if true { A } else { A })\n\
      y u32 = rotr(if true { A } else { A }, 1)\n\
      z u32 = add_sat(if true { A } else { A }, A)\n\
      return x + y + z\n\
      }\n");
  let runtime_shift_source =
    "const A u8 = 3\nconst N u8 = 9\nfn run() u32 { return A << N }\n"
  in
  let runtime_shift_messages = semantic_messages runtime_shift_source in
  let constant_shift_sources =
    [
      "const A u8 = 3\nconst N u8 = 9\nconst K u32 = A << N\n";
      "const A u8 = 3\nconst N u8 = 9\nconst K arr[1, u32] = {A << N}\n";
      "const A u8 = 3\n\
       const N u8 = 9\n\
       fn run(x u32) i32 { switch x { case A << N: return 1 } return 0 }\n";
      "const A u8 = 3\n\
       const N u8 = 9\n\
       fn pick[K const usize]() usize { return K }\n\
       fn run() usize { return pick[A << N]() }\n";
    ]
  in
  List.iteri
    (fun index text ->
      let messages = semantic_messages text in
      if
        messages = []
        || (index <> 2 && not (contains (String.concat "\n" messages) "may wrap"))
      then
        failwith
          (Printf.sprintf "const-shift-narrow-%d: missing narrow-arithmetic error" index);
      if index = 0 && messages <> runtime_shift_messages then
        failwith "const-shift-narrow-runtime-parity: diagnostics differ")
    constant_shift_sources;
  ignore
    (llvm_of
       "const Safe i32 = if true { 7 } else { 1 / 0 }\nfn main() i32 { return Safe }\n");
  semantic_accept "if-expression-contexts"
    "var Global i32 = if true { 1 } else { 2 }\n\
     const Chosen i32 = if false { 3 } else { 4 }\n\
     fn identity(value i32) i32 { return value }\n\
     fn value(c bool, d bool) i32 {\n\
     local i32 = if c { 5 } else if d { 6 } else { Chosen }\n\
     local = if d { 7 } else { local }\n\
     local = (if c { local } else { Global }) + 1\n\
     return identity(if c { local } else { Global })\n\
     }\n\
     const Lanes vec[4,u32] = {1, 2, 3, 4}\n\
     const Selected vec[4,u32] = if false { splat(0) } else { Lanes }\n\
     fn vector(c bool) vec[4,u32] {\n\
     return if c { {4, 3, 2, 1} } else { Selected }\n\
     }\n\
     fn short_circuit(c bool) bool {\n\
     return c && (if c { true } else { false }) || (if c { false } else { true })\n\
     }\n";
  let vector_constants =
    llvm_of
      "const Bytes vec[4,u8] = splat(255)\n\
       const Signed vec[4,i8] = bitcast[vec[4,i8]](Bytes)\n\
       const WideUnsigned vec[4,u16] = zext[vec[4,u16]](Bytes)\n\
       const WideSigned vec[4,i16] = sext[vec[4,i16]](Signed)\n\
       const Narrow vec[4,u8] = trunc[vec[4,u8]](WideUnsigned)\n\
       const Flags vec[4,bool] = splat(true)\n\
       const BoolUnsigned vec[4,u8] = zext[vec[4,u8]](Flags)\n\
       const BoolSigned vec[4,i8] = sext[vec[4,i8]](Flags)\n\
       const LowBits vec[4,bool] = trunc[vec[4,bool]](Narrow)\n\
       const Added vec[4,u8] = Narrow + splat(1)\n\
       const Shifted vec[4,u8] = Added << 1\n\
       const Matches vec[4,bool] = Shifted == splat(0)\n\
       fn unsigned_lane() u16 { return WideUnsigned[2] }\n\
       fn signed_lane() i16 { return WideSigned[1] }\n\
       fn narrow_lane() u8 { return Narrow[3] }\n\
       fn bool_unsigned_lane() u8 { return BoolUnsigned[0] }\n\
       fn bool_signed_lane() i8 { return BoolSigned[0] }\n\
       fn low_bit_lane() bool { return LowBits[0] }\n\
       fn match_lane() bool { return Matches[0] }\n"
  in
  List.iter
    (fun marker ->
      if not (contains vector_constants marker) then
        failwith ("vector-constants: missing `" ^ marker ^ "`"))
    [
      "<i16 255, i16 255, i16 255, i16 255>";
      "<i16 65535, i16 65535, i16 65535, i16 65535>";
      "<i8 255, i8 255, i8 255, i8 255>";
      "<i1 1, i1 1, i1 1, i1 1>";
      "<i8 1, i8 1, i8 1, i8 1>";
    ];
  semantic_error "named-vector-constant-keeps-type"
    "is `vec[4,u8]`, expected `vec[4,u16]`"
    "const Bytes vec[4,u8] = splat(1)\n\
     fn take(value vec[4,u16]) void { return }\n\
     fn test() void { take(Bytes)\n\
    \ return }\n";
  semantic_error "constant-vector-conversion-lanes"
    "illegal cast for source and destination widths"
    "const Bytes vec[4,u8] = splat(1)\n\
     const Wide vec[2,u16] = zext[vec[2,u16]](Bytes)\n\
     fn main() i32 { return 0 }\n";
  semantic_error "constant-vector-division-by-zero"
    "division by zero in constant expression"
    "const Values vec[4,u8] = splat(8)\n\
     const Zero vec[4,u8] = splat(0)\n\
     const Invalid vec[4,u8] = Values / Zero\n\
     fn main() i32 { return 0 }\n";
  semantic_error "constant-vector-index-assignment" "cannot modify constant"
    "const Values vec[4,u8] = splat(1)\nfn main() i32 { Values[0] = 2\n return 0 }\n";
  semantic_error "constant-vector-index-compound-assignment" "cannot modify constant"
    "const Values vec[4,u8] = splat(1)\nfn main() i32 { Values[0] += 2\n return 0 }\n";
  semantic_error "constant-vector-address" "cannot take the address"
    "const Values vec[4,u8] = splat(1)\nfn main() i32 { p addr = &Values\n return 0 }\n";
  ignore
    (llvm_of
       "const Safe vec[4,u8] = if true { splat(7) } else { splat(1) / splat(0) }\n\
        fn test() u8 { return Safe[0] }\n");
  semantic_error "constant-vector-dead-ternary-type" "cannot use `bool` as `u8`"
    "const Invalid vec[4,u8] = if true { splat(7) } else { splat(true) }\n\
     fn main() i32 { return 0 }\n";
  semantic_error "runtime-logical-integer-left"
    "left operand of `&&` is `i32`, not `bool`"
    "fn f() bool { return 0 && (1 / 0 == 0) }\n";
  let integer_vector_comparisons =
    llvm_of
      "fn signed(left vec[4,i32], right vec[4,i32]) bool {\n\
      \ eq vec[4,bool] = left == right\n\
      \ ne vec[4,bool] = left != right\n\
      \ lt vec[4,bool] = left < right\n\
      \ le vec[4,bool] = left <= right\n\
      \ gt vec[4,bool] = left > right\n\
      \ ge vec[4,bool] = left >= right\n\
      \ return eq[0] && ne[0] && lt[0] && le[0] && gt[0] && ge[0]\n\
      \ }\n\
      \ fn unsigned(left vec[4,u32], right vec[4,u32]) bool {\n\
      \ lt vec[4,bool] = left < right\n\
      \ le vec[4,bool] = left <= right\n\
      \ gt vec[4,bool] = left > right\n\
      \ ge vec[4,bool] = left >= right\n\
      \ return lt[0] && le[0] && gt[0] && ge[0]\n\
      \ }\n\
      \ fn booleans(left vec[8,bool], right vec[8,bool]) bool {\n\
      \ eq vec[8,bool] = left == right\n\
      \ ne vec[8,bool] = left != right\n\
      \ return eq[0] && ne[0]\n\
      \ }\n"
  in
  List.iter
    (fun predicate ->
      if not (contains integer_vector_comparisons ("icmp " ^ predicate)) then
        failwith ("integer-vector-comparison: missing `icmp " ^ predicate ^ "`"))
    [ "eq"; "ne"; "slt"; "sle"; "sgt"; "sge"; "ult"; "ule"; "ugt"; "uge" ];
  if
    (not (contains integer_vector_comparisons "store i8"))
    || (not (contains integer_vector_comparisons "extractelement <4 x i1>"))
    || (not (contains integer_vector_comparisons "icmp eq <8 x i1>"))
    || not (contains integer_vector_comparisons "and <8 x i1>")
  then failwith "integer-vector-comparison: result vector was not preserved";
  if contains integer_vector_comparisons "store <4 x i1>" then
    failwith "integer-vector-comparison: mask store retained unused bits";
  semantic_error "integer-vector-comparison-lanes"
    "operands of `==` have different types: `vec[4,i32]` and `vec[8,i32]`"
    "fn f(left vec[4,i32], right vec[8,i32]) vec[4,bool] { return left == right }\n";
  semantic_error "integer-vector-comparison-elements"
    "operands of `==` have different types: `vec[4,i32]` and `vec[4,u32]`"
    "fn f(left vec[4,i32], right vec[4,u32]) vec[4,bool] { return left == right }\n";
  semantic_error "integer-vector-comparison-scalar"
    "operands of `==` have different types: `vec[4,i32]` and `i32`"
    "fn f(left vec[4,i32], right i32) vec[4,bool] { return left == right }\n";
  semantic_error "integer-vector-comparison-bool-order"
    "ordered comparison `<` needs an integer or integer vector, got `vec[4, bool]`"
    "fn f(left vec[4,bool], right vec[4,bool]) vec[4,bool] { return left < right }\n";
  semantic_error "integer-vector-comparison-condition"
    "condition of `if` is `vec[4, bool]`, not `bool`"
    "fn f(left vec[4,i32], right vec[4,i32]) i32 {\n\
    \ if left == right { return 1 }\n\
    \ return 0\n\
     }\n";
  semantic_error "integer-vector-comparison-no-reduction"
    "left operand of `&&` is `vec[4, bool]`, not `bool`"
    "fn f(left vec[4,i32], right vec[4,i32]) vec[4,bool] {\n\
    \ return (left == right) && (left != right)\n\
     }\n";
  let integer_vector_shift_counts =
    llvm_of
      "fn shifts(values vec[4,u32], signed_values vec[4,i32], narrow u8, equal u32, \
       wide u64) vec[4,u32] {\n\
      \ shl_narrow vec[4,u32] = values << narrow\n\
      \ shl_equal vec[4,u32] = values << equal\n\
      \ shl_wide vec[4,u32] = values << wide\n\
      \ lshr_narrow vec[4,u32] = values >> narrow\n\
      \ lshr_equal vec[4,u32] = values >> equal\n\
      \ lshr_wide vec[4,u32] = values >> wide\n\
      \ ashr_narrow vec[4,i32] = signed_values >> narrow\n\
      \ ashr_equal vec[4,i32] = signed_values >> equal\n\
      \ ashr_wide vec[4,i32] = signed_values >> wide\n\
      \ rotl_narrow vec[4,u32] = rotl(values, narrow)\n\
      \ rotl_equal vec[4,u32] = rotl(values, equal)\n\
      \ rotl_wide vec[4,u32] = rotl(values, wide)\n\
      \ rotr_narrow vec[4,u32] = rotr(values, narrow)\n\
      \ rotr_equal vec[4,u32] = rotr(values, equal)\n\
      \ rotr_wide vec[4,u32] = rotr(values, wide)\n\
      \ return rotr_wide\n\
      \ }\n"
  in
  List.iter
    (fun marker ->
      if not (contains integer_vector_shift_counts marker) then
        failwith ("integer-vector-shift-count: missing `" ^ marker ^ "`"))
    [
      "shl <4 x i32>";
      "lshr <4 x i32>";
      "ashr <4 x i32>";
      "@llvm.fshl.v4i32";
      "@llvm.fshr.v4i32";
    ];
  List.iter
    (fun (needle, expected) ->
      let actual = List.length (positions integer_vector_shift_counts needle) in
      if actual <> expected then
        failwith
          (Printf.sprintf "integer-vector-shift-count: expected %d `%s`, got %d"
             expected needle actual))
    [ ("zext i8", 5); ("trunc i64", 5); ("and i32", 15); ("shufflevector", 15) ];
  List.iter
    (fun operation ->
      semantic_error
        ("integer-vector-rotate-count-splat-" ^ operation)
        (Printf.sprintf
           "rotate count for `%s` must be a scalar integer, got `vec[4, u32]`" operation)
        (Printf.sprintf
           "fn f(values vec[4,u32]) vec[4,u32] { return %s(values, splat(1)) }\n"
           operation);
      semantic_error
        ("integer-vector-rotate-count-named-" ^ operation)
        (Printf.sprintf
           "rotate count for `%s` must be a scalar integer, got `vec[4, u32]`" operation)
        (Printf.sprintf
           "fn f(values vec[4,u32], count vec[4,u32]) vec[4,u32] { return %s(values, \
            count) }\n"
           operation))
    [ "rotl"; "rotr" ];
  List.iter
    (fun operation ->
      let source =
        Printf.sprintf
          "fn f(values vec[4,u32], count vec[4,u32]) vec[4,u32] { return %s(values, \
           count) }\n"
          operation
      in
      semantic_pin
        ("rotate-count-type-" ^ operation)
        source 1
        (String.rindex source 'c' + 1)
        5
        (Printf.sprintf
           "rotate count for `%s` must be a scalar integer, got `vec[4, u32]`" operation)
        None;
      semantic_accept
        ("rotate-count-type-twin-" ^ operation)
        (Printf.sprintf
           "fn f(values vec[4,u32], count u32) vec[4,u32] { return %s(values, count) }\n"
           operation))
    [ "rotl"; "rotr" ];
  List.iter
    (fun (operation, name) ->
      semantic_error
        ("integer-vector-shift-count-splat-" ^ name)
        "the shift count `splat(1)` has no vector type"
        (Printf.sprintf
           "fn f(values vec[4,u32]) vec[4,u32] { return values %s splat(1) }\n"
           operation))
    [ ("<<", "shl"); (">>", "lshr") ];
  semantic_error "integer-vector-shift-count-noninteger"
    "right operand of `<<` has type `bool`, expected an integer"
    "fn f(values vec[4,u32], count bool) vec[4,u32] { return values << count }\n";
  semantic_error "shift-count-scalar-value-vector-count"
    "right operand of `<<` has type `vec[4, u32]`, expected an integer"
    "fn f(x u32, count vec[4,u32]) u32 { return x << count }\n";
  semantic_error "shift-count-lane-mismatch"
    "right operand of `<<` has type `vec[2, u32]`, expected `vec[4, u32]` to match the \
     value"
    "fn f(values vec[4,u32], count vec[2,u32]) vec[4,u32] { return values << count }\n";
  semantic_error "shift-compound-lane-mismatch"
    "right operand of `<<` has type `vec[2, u32]`, expected `vec[4, u32]` to match the \
     value"
    "fn f(values vec[4,u32], count vec[2,u32]) vec[4,u32] { values <<= count\n\
    \ return values }\n";
  semantic_error "shift-compound-bool-destination"
    "left operand of `<<` has type `vec[4, bool]`, expected an integer or integer \
     vector"
    "fn f(values vec[4,bool], count u32) vec[4,bool] { values <<= count\n\
    \ return values }\n";
  semantic_error "shift-compound-scalar-value-vector-count"
    "right operand of `<<` has type `vec[4, u32]`, expected an integer"
    "fn f(x u32, count vec[4,u32]) u32 { x <<= count\n return x }\n";
  let shift_left_type_source = "fn f(value bool) bool { return value << 1 }\n" in
  semantic_pin "shift-left-type" shift_left_type_source 1
    (String.rindex shift_left_type_source 'v' + 1)
    5 "left operand of `<<` has type `bool`, expected an integer or integer vector" None;
  semantic_accept "shift-left-type-twin" "fn f(value u32) u32 { return value << 1 }\n";
  let shift_right_type_source =
    "fn f(value u32, count bool) u32 { return value << count }\n"
  in
  semantic_pin "shift-right-type" shift_right_type_source 1
    (String.rindex shift_right_type_source 'c' + 1)
    5 "right operand of `<<` has type `bool`, expected an integer" None;
  semantic_accept "shift-right-type-twin"
    "fn f(value u32, count u32) u32 { return value << count }\n";
  let shift_lane_source =
    "fn f(value vec[4,u32], count vec[2,u32]) vec[4,u32] { return value << count }\n"
  in
  semantic_pin "shift-lane-count" shift_lane_source 1
    (String.rindex shift_lane_source 'c' + 1)
    5
    "right operand of `<<` has type `vec[2, u32]`, expected `vec[4, u32]` to match the \
     value"
    None;
  semantic_accept "shift-lane-count-twin"
    "fn f(value vec[4,u32], count vec[4,u32]) vec[4,u32] { return value << count }\n";
  let constant_shift_source = "const FLAG bool = true << false\n" in
  semantic_pin "constant-shift-left-type" constant_shift_source 1 19 4
    "left operand of `<<` has type `bool`, expected an integer or integer vector" None;
  semantic_accept "constant-shift-left-type-twin" "const FLAG u32 = 7 << 1\n";
  semantic_error "shift-no-splat-lift" "is `i32`, expected `vec[4,u32]`"
    "fn f(n u32) vec[4,u32] { return 1 << n }\n";
  semantic_accept "len-returns-usize-as-u64"
    "const Values arr[3,u8] = {1, 2, 3}\nfn size() u64 { return len(Values) }\n";
  semantic_accept "sizeof-returns-usize-as-u64" "fn size() u64 { return sizeof[u8] }\n";
  ignore
    (lower_of
       "struct Measure { left u8 right u64 }\n\
        const Size usize = sizeof[Measure]\n\
        const Alignment usize = alignof[Measure]\n\
        const Offset usize = offsetof[Measure,right]\n\
        fn size() usize { return Size + Alignment + Offset }\n");
  semantic_accept "const-sizeof-usize-as-u64"
    "const Size u64 = sizeof[u8]\nfn size() u64 { return Size }\n";
  let usize_specialization =
    llvm_of
      "fn id[N const usize](value usize) usize { return value + N }\n\
       fn test() usize { return id[3](4) }\n"
  in
  if not (contains usize_specialization "N=usize:3\"") then
    failwith "usize-specialization: specialization key lost usize identity";
  let target_width_type_hir =
    expect_ok
      (Parser.parse
         (source
            "fn f[T](v T) T { return v }\n\
             fn test() usize { a usize = f[usize](1)\n\
            \ b u64 = f[u64](2)\n\
            \ return a }\n"))
    |> Sema.check |> expect_ok
  in
  let target_width_specs =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      target_width_type_hir.Hir.funcs
  in
  if List.length target_width_specs <> 2 then
    failwith "target-width-instantiation: usize/u64 instantiations were merged";
  if
    not
      (List.exists
         (fun (func : Hir.func) -> String.ends_with ~suffix:"usize" func.name)
         target_width_specs)
  then failwith "target-width-instantiation: specialization key lost usize identity";
  if
    not
      (List.exists
         (fun (func : Hir.func) -> String.ends_with ~suffix:"u64" func.name)
         target_width_specs)
  then failwith "target-width-instantiation: specialization key lost u64 identity";
  ignore (Lower.lower target_width_type_hir |> expect_ok);
  let signed_target_width_hir =
    expect_ok
      (Parser.parse
         (source
            "fn f[T](v T) T { return v }\n\
             fn test() isize { a isize = f[isize](1)\n\
            \ b i64 = f[i64](2)\n\
            \ return a }\n"))
    |> Sema.check |> expect_ok
  in
  let signed_target_width_specs =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      signed_target_width_hir.Hir.funcs
  in
  if List.length signed_target_width_specs <> 2 then
    failwith "target-width-instantiation: isize/i64 instantiations were merged";
  if
    not
      (List.exists
         (fun (func : Hir.func) -> String.ends_with ~suffix:"isize" func.name)
         signed_target_width_specs)
  then failwith "target-width-instantiation: specialization key lost isize identity";
  if
    not
      (List.exists
         (fun (func : Hir.func) -> String.ends_with ~suffix:"i64" func.name)
         signed_target_width_specs)
  then failwith "target-width-instantiation: specialization key lost i64 identity";
  ignore (Lower.lower signed_target_width_hir |> expect_ok);
  let uninitialized_field_write =
    llvm_of "struct S { x i64 }\nfn test() i64 { p S\n p.x = 1\n return 0 }\n"
  in
  if not (contains uninitialized_field_write "store i64 1") then
    failwith "place-init: field assignment on an uninitialized aggregate failed";
  let uninitialized_element_write =
    llvm_of "fn test() i64 { a arr[2, i64]\n a[0] = 1\n return 0 }\n"
  in
  if not (contains uninitialized_element_write "store i64 1") then
    failwith "place-init: element assignment on an uninitialized aggregate failed";
  ignore (llvm_of "fn test() i64 { v vec[2, i64]\n v[0] = 1\n return 0 }\n");
  let uninitialized_array_field_write =
    llvm_of
      "struct S { a arr[2, i64] }\nfn test() i64 { s S\n s.a[0] = 1\n return 0 }\n"
  in
  if not (contains uninitialized_array_field_write "store i64 1") then
    failwith "place-init: array field indexing read the whole aggregate";
  let uninitialized_array_address =
    llvm_of
      "fn take(p addr) void { return }\n\
       fn test() i64 { a arr[2, i64]\n\
       take(&a[0])\n\
       return 0 }\n"
  in
  if not (contains uninitialized_array_address "call void @take(ptr") then
    failwith "place-init: address of an uninitialized array element failed";
  let uninitialized_address =
    llvm_of
      "fn take(p addr) void { return }\nfn test() i64 { x i64\n take(&x)\n return 0 }\n"
  in
  if not (contains uninitialized_address "call void @take(ptr") then
    failwith "place-init: taking the address of an uninitialized local failed";
  semantic_error "place-init-pointer-intermediate" "use of uninitialized local `p`"
    "fn test() i64 { p addr\n p[i64] = 1\n return 0 }\n";
  semantic_error "place-init-pointer-index-write" "use of uninitialized local `p`"
    "fn test() i64 { p addr\n p[i64] = 1\n return 0 }\n";
  semantic_error "place-init-pointer-index-read" "use of uninitialized local `p`"
    "fn test() i64 { p addr\n x i64 = p[i64]\n return x }\n";
  semantic_error "place-init-pointer-index-address" "use of uninitialized local `p`"
    "fn take(p addr) void { return }\n\
     fn test() i64 { p addr\n\
     take(&p[i64])\n\
     return 0 }\n";
  semantic_error "place-init-pointer-index-compound" "use of uninitialized local `p`"
    "fn test() i64 { p addr\n p[i64] += 1\n return 0 }\n";
  semantic_error "place-init-pointer-field-write" "use of uninitialized local `s`"
    "struct S { p addr }\nfn test() i64 { s S\n s.p[i64] = 1\n return 0 }\n";
  semantic_error "place-init-pointer-field-read" "use of uninitialized local `s`"
    "struct S { p addr }\nfn test() i64 { s S\n x i64 = s.p[i64]\n return x }\n";
  semantic_error "place-init-pointer-field-address" "use of uninitialized local `s`"
    "struct S { p addr }\n\
     fn take(p addr) void { return }\n\
     fn test() i64 { s S\n\
    \ take(&s.p[i64])\n\
    \ return 0 }\n";
  semantic_error "place-init-pointer-array-element" "use of uninitialized local `a`"
    "fn test() i64 { a arr[2,addr]\n a[0][i64] = 1\n return 0 }\n";
  ignore
    (lower_of
       "struct S { p addr }\n\
        fn test() i64 { x i64\n\
       \ s S\n\
       \ s.p = &x\n\
       \ s.p[i64] = 1\n\
       \ return x }\n");
  semantic_error "place-init-whole-vector" "use of uninitialized local `v`"
    "fn f() i64 { v vec[2,i64]\n x vec[2,i64] = v\n return 0 }\n";
  ignore (lower_of "fn f() i64 { v vec[2,i64]\n v[0] = 1\n return v[0] }\n");
  semantic_error "place-init-vector-other-lane" "use of uninitialized local `v`"
    "fn f() i64 { v vec[2,i64]\n v[0] = 1\n return v[1] }\n";
  ignore
    (lower_of
       "fn f() i64 { v vec[2,i64]\n\
       \ v[0] = 1\n\
       \ v[1] = 2\n\
       \ x vec[2,i64] = v\n\
       \ return x[0] }\n");
  semantic_error "place-init-static-large-index"
    "array index `9223372036854775808` is out of bounds for length 2"
    "const N u64 = 9223372036854775808\n\
     fn f() i64 { a arr[2,i64]\n\
    \ x i64 = a[N]\n\
    \ return x }\n";
  semantic_error "place-init-static-narrow-negative-index"
    "array index `-1` is out of bounds for length 300"
    "const N i8 = -1\nfn f() i64 { a arr[300,i64]\nreturn a[N] }\n";
  semantic_error "vector-lane-address-rejected" "cannot take address of a vector lane"
    "fn take(p addr) void { return }\n\
     fn f() i64 { v vec[2,i64] = splat(1)\n\
     take(&v[0])\n\
     return 0 }\n";
  ignore
    (lower_of
       "fn read_value(v u32) u32 { return v }\n\
        fn f() u32 { x u32\n\
       \ view v = x\n\
       \ v = 5\n\
       \ v += 2\n\
       \ return read_value(v) }\n");
  ignore (lower_of "fn f(p addr) u32 { view v = p[u32]\n return v }\n");
  ignore
    (lower_of
       "fn f() u32 { a arr[2,u32]\n a[0] = 1\n view v = a[0]\n v = 3\n return v }\n");
  ignore
    (lower_of
       "fn f() u32 { x vec[2,u32] = splat(0)\n view v = x\n v[0] = 4\n return v[0] }\n");
  ignore
    (lower_of
       "struct Inner { value u32 }\n\
        fn f() u32 { s Inner\n\
       \ view outer = s\n\
       \ view inner = outer.value\n\
       \ inner = 8\n\
       \ return s.value }\n");
  ignore
    (lower_of
       "fn f() u32 { x u32 = 2\n\
       \ view v = x\n\
       \ { y u32 = 3\n\
       \ view v = y\n\
       \ v = 5 }\n\
       \ return v }\n");
  ignore
    (lower_of
       "fn at[N const u64](p addr) u32 { view v = p[u32, N]\n\
       \ return v }\n\
        fn f(p addr) u32 { return at[0](p) }\n");
  semantic_error "view-uninitialized-read" "use of uninitialized local `x`"
    "fn f() u32 { x u32\n view v = x\n return v }\n";
  semantic_error "view-element-facts-share" "use of uninitialized local `a`"
    "fn f() u32 { a arr[2,u32]\n view v = a[0]\n v = 1\n return a[1] }\n";
  let view_call =
    "fn make() u32 { return 1 }\nfn f() void { view v = make()\n return }\n"
  in
  semantic_pin "view-call-rvalue" view_call 2
    (String.length "fn f() void { view v = " + 1)
    6 "view needs a local, field, element or raw access, not a function call" None;
  let view_arithmetic = "fn f(x u32) void { view v = x + 1\n return }\n" in
  semantic_pin "view-arithmetic-rvalue" view_arithmetic 1
    (String.length "fn f(x u32) void { view v = " + 1)
    5 "view needs a local, field, element or raw access, not a computed value" None;
  let view_literal = "fn f() void { view v = 1\n return }\n" in
  semantic_pin "view-literal-rvalue" view_literal 1
    (String.length "fn f() void { view v = " + 1)
    1 "view needs a local, field, element or raw access, not an integer literal" None;
  semantic_error "view-simd-lane" "cannot create a view of a SIMD lane"
    "fn f() void { x vec[2,u32] = splat(0)\nview lane = x[0]\n return }\n";
  semantic_error "view-shadow-duplicate" "duplicate local `v`"
    "fn f(x u32) void { view v = x\n view v = x\n return }\n";
  semantic_error "view-name-reserved" "`view` is reserved and cannot be used as a name"
    "fn f(x u32) void { view view = x\n return }\n";
  parse_error "view-top-level" "view x = 1\n";
  semantic_error "view-readonly-source"
    "cannot modify read-only pointer (through view `v`)"
    "fn f() void { view v = c\"read only\"[u8]\n v = 2\n return }\n";
  ignore
    (lower_of
       "struct Inner { value u32 }\n\
        struct Pair { id u32, inner Inner }\n\
        fn take(p addr) void { return }\n\
        fn read_at[T](p addr, i usize) T { view xs = p[T, ..]\n\
       \ return xs[i] }\n\
        fn f(p addr, i usize) u32 {\n\
       \ view xs = p[u32, ..]\n\
       \ xs[i] = 4\n\
       \ xs[i] += 1\n\
       \ take(&xs[i])\n\
       \ return xs[i] + read_at[u32](p, i) }\n\
        fn fields(p addr, i usize) u32 {\n\
       \ view xs = p[Pair, ..]\n\
       \ xs[i].inner.value = 7\n\
       \ return xs[i].inner.value }\n");
  let raw_view_message = "view `xs` must be indexed" in
  List.iter
    (fun (name, body) -> semantic_message name raw_view_message body)
    [
      ( "raw-view-reject-assignment",
        "fn f(p addr) void { view xs = p[u32, ..]\n xs = null\n return }\n" );
      ( "raw-view-reject-len",
        "fn f(p addr) usize { view xs = p[u32, ..]\n return len(xs) }\n" );
      ( "raw-view-reject-address",
        "fn f(p addr) addr { view xs = p[u32, ..]\n return &xs }\n" );
      ( "raw-view-reject-passing",
        "fn take(p addr) void { return }\n\
         fn f(p addr) void { view xs = p[u32, ..]\n\
        \ take(xs)\n\
        \ return }\n" );
      ( "raw-view-reject-return",
        "fn f(p addr) addr { view xs = p[u32, ..]\n return xs }\n" );
      ( "raw-view-reject-field",
        "fn f(p addr) u32 { view xs = p[u32, ..]\n return xs.field }\n" );
      ( "raw-view-reject-typed-index",
        "fn f(p addr, i usize) u32 { view xs = p[u32, ..]\n return xs[u32, i] }\n" );
      ( "raw-view-reject-nested-view",
        "fn f(p addr) void { view xs = p[u32, ..]\n view ys = xs\n return }\n" );
    ];
  semantic_message "raw-view-constant-storage-write" "write to constant storage `K`"
    "const K arr[2,u32] = {1, 2}\n\
     fn f() void { p addr = &K[0]\n\
    \ view xs = p[u32, ..]\n\
    \ xs[0] = 3\n\
    \ return }\n";
  semantic_message "raw-selection-null-plus-offset" "access through null address"
    "fn f() void { p addr = null\n q addr = p + 4\n q[u32, 0] = 1\n return }\n";
  semantic_message "raw-view-null-plus-offset" "access through null address"
    "fn f() void { p addr = null\n\
    \ q addr = p + 4\n\
     view xs = q[u32, ..]\n\
    \ xs[0] = 1\n\
    \ return }\n";
  ignore
    (lower_of
       "fn take(p addr) void { return }\n\
        fn f() i64 { v vec[2,i64] = splat(1)\n\
       \ take(&v)\n\
       \ return 0 }\n");
  let unsigned_narrow_index =
    llvm_of
      "const N u8 = 255\n\
       fn f() i64 { a arr[256,i64]\n\
      \ a[N] = 7\n\
      \ v vec[256, u8] = splat(1)\n\
      \ v[N] = 8\n\
      \ return a[N] + zext[i64](v[N]) }\n"
  in
  if not (contains unsigned_narrow_index "zext i8 255 to i64") then
    failwith "aggregate-index-u8: narrow unsigned index was not zero-extended";
  let index_evaluation_order =
    llvm_of
      "fn base() addr { return addr_from_bits(0) }\n\
       fn index() i64 { return 0 }\n\
       fn f() i64 { base()[i64, index()] = 1\n\
      \ return 0 }\n"
  in
  let base_calls = positions index_evaluation_order "call ptr @base" in
  let index_calls = positions index_evaluation_order "call i64 @index" in
  (match (base_calls, index_calls) with
  | base :: _, index :: _ when base < index -> ()
  | _ -> failwith "aggregate-index-order: index was evaluated before its base");
  let vector_assign_alias =
    llvm_of
      "fn mutate(p addr) i64 { p[vec[2,i64]] = splat(9)\n\
      \ return 7 }\n\
       fn f() i64 { v vec[2,i64] = splat(1)\n\
      \ v[0] = mutate(&v)\n\
      \ return v[1] }\n"
  in
  let assign_call = positions vector_assign_alias "call i64 @mutate" in
  let assign_load = positions vector_assign_alias "load <2 x i64>" in
  (match (assign_call, assign_load) with
  | call :: _, load :: _ when call < load -> ()
  | _ -> failwith "vector-lane-assignment: rhs was evaluated after stale vector load");
  let vector_compound_alias =
    llvm_of
      "fn mutate(p addr) i64 { p[vec[2,i64]] = splat(9)\n\
      \ return 7 }\n\
       fn f() i64 { v vec[2,i64] = splat(1)\n\
      \ v[0] += mutate(&v)\n\
      \ return v[1] }\n"
  in
  let compound_call = positions vector_compound_alias "call i64 @mutate" in
  let compound_loads = positions vector_compound_alias "load <2 x i64>" in
  (match (compound_call, compound_loads) with
  | call :: _, first :: second :: _ when first < call && call < second -> ()
  | _ -> failwith "vector-lane-compound: rhs was not between vector loads");
  semantic_error "place-init-short-circuit-escape" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f() i64 { s S\n\
    \ b bool = true || (&s != addr_from_bits(0))\n\
    \ t S\n\
    \ copy(t, s)\n\
    \ return 0 }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p addr) void { return }\n\
        fn f() i64 { s S\n\
       \ take(&s)\n\
       \ b bool = true || false\n\
       \ t S\n\
       \ copy(t, s)\n\
       \ return 0 }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p bool) i64 { s S\n\
       \ q addr = if p { &s } else { addr_from_bits(0) }\n\
        t S\n\
        copy(t, s)\n\
       \ return 0 }\n");
  semantic_error "place-init-ternary-cross-arm" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn choose(x i64) i64 { return x }\n\
     fn f(p bool) i64 { s S\n\
    \ t i64 = if p { choose(s.x) } else { s.x }\n\
    \ return t }\n";
  semantic_error "place-init-binding-identity" "use of uninitialized local `x`"
    "fn test() i64 { { x i64 = 1 }\n { x i64\n y i64 = x\n }\n return 0 }\n";
  semantic_error "aggregate-whole-read" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n t S\n copy(t, s)\n return 0 }\n";
  semantic_error "aggregate-partial-field" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x = 1\n return s.y }\n";
  semantic_error "aggregate-partial-whole" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f() i64 { s S\n\
    \ s.x = 1\n\
    \ t S\n\
    \ copy(t, s)\n\
    \ return 0 }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f() i64 { s S\n\
       \ s.x = 1\n\
       \ s.y = 2\n\
       \ t S\n\
       \ copy(t, s)\n\
       \ return s.x }\n");
  semantic_error "aggregate-nested-field" "use of uninitialized local `o`"
    "struct I { x i64 y i64 }\n\
     struct O { i I z i64 }\n\
     fn f() i64 { o O\n\
    \ o.i.x = 1\n\
    \ return o.i.y }\n";
  semantic_error "aggregate-array-whole" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n a[0] = 1\n t arr[2,i64]\n copy(t, a)\n return 0 }\n";
  ignore
    (lower_of
       "fn f() i64 { a arr[2,i64]\n\
       \ a[0] = 1\n\
       \ x i64 = a[0]\n\
       \ a[1] = 2\n\
       \ t arr[2,i64]\n\
       \ copy(t, a)\n\
       \ return x }\n");
  let array_field_copy =
    llvm_of
      "struct S { a arr[2,i64] }\n\
       fn f() i64 { s S\n\
      \ s.a[0] = 1\n\
      \ s.a[1] = 2\n\
      \ t arr[2,i64]\n\
      \ copy(t, s.a)\n\
      \ return t[0] }\n"
  in
  if contains array_field_copy "load [2 x i64], ptr" then
    failwith "aggregate-array-field-copy: aggregate was loaded as a value";
  let nested_array_copy =
    llvm_of
      "fn f() i64 { a arr[2,arr[2,i64]]\n\
      \ a[0][0] = 1\n\
      \ a[0][1] = 2\n\
      \ t arr[2,i64]\n\
      \ copy(t, a[0])\n\
      \ return t[0] }\n"
  in
  if contains nested_array_copy "load [2 x i64], ptr" then
    failwith "aggregate-nested-array-copy: aggregate was loaded as a value";
  semantic_error "aggregate-whole-array-uninitialized" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n return a[0] }\n";
  ignore (lower_of "struct E { }\nfn f() i64 { e E\n t E\n copy(t, e)\n return 0 }\n");
  ignore
    (lower_of "fn f() i64 { a arr[0,i64]\n t arr[0,i64]\n copy(t, a)\n return 0 }\n");
  ignore
    (lower_of
       "struct E { }\n\
        struct S { e E x i64 }\n\
        fn f() i64 { s S\n\
       \ s.x = 1\n\
       \ t E\n\
       \ copy(t, s.e)\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct E { }\n\
        struct S { e E }\n\
        fn f() i64 { s S\n\
       \ t S\n\
       \ copy(t, s)\n\
       \ return 0 }\n");
  ignore
    (lower_of
       "fn f() i64 { a arr[2,i64]\n\
       \ a[0] = 1\n\
       \ a[1] = 2\n\
       \ t arr[2,i64]\n\
       \ copy(t, a)\n\
       \ return t[0] }\n");
  semantic_error "aggregate-direct-return"
    "aggregate result `S` cannot be returned by value; return `void` and take the \
     destination as an `addr` parameter"
    "struct S { x i64 }\nfn f() S { s S\nreturn s }\n";
  semantic_error "aggregate-parameter"
    "aggregate parameter `value` of type `S` cannot be passed by value; declare \
     `value` as `addr`; callers pass its address with `&`"
    "struct S { x i64 }\nfn f(value S) void { return }\n";
  semantic_error "aggregate-array-parameter"
    "aggregate parameter `value` of type `arr[4,u32]` cannot be passed by value; \
     declare `value` as `addr`; callers pass its address with `&`"
    "fn f(value arr[4,u32]) void { return }\n";
  semantic_error "aggregate-result"
    "aggregate result `S` cannot be returned by value; return `void` and take the \
     destination as an `addr` parameter"
    "struct S { x i64 }\nfn f() S { s S\nreturn s }\n";
  semantic_error "aggregate-array-result"
    "aggregate result `arr[4,u32]` cannot be returned by value; return `void` and take \
     the destination as an `addr` parameter"
    "fn f() arr[4,u32] { value arr[4,u32]\nreturn value }\n";
  let generic_aggregate_parameter_error =
    "aggregate parameter `x` of type `arr[N,u8]` cannot be passed by value; declare \
     `x` as `addr`; callers pass its address with `&`"
  in
  semantic_error "generic-array-parameter-by-value" generic_aggregate_parameter_error
    "fn take[N const usize](x arr[N,u8]) void { return }\n";
  semantic_error "generic-struct-parameter-by-value"
    "aggregate parameter `r` of type `Ring[N]` cannot be passed by value; declare `r` \
     as `addr`; callers pass its address with `&`"
    "struct Ring[N const usize] { data arr[N,u8] }\n\
     fn take[N const usize](r Ring[N]) void { return }\n";
  semantic_error "generic-known-struct-parameter-by-value"
    "aggregate parameter `value` of type `S` cannot be passed by value; declare \
     `value` as `addr`; callers pass its address with `&`"
    "struct S { value i32 }\nfn take[N const usize](value S) void { return }\n";
  semantic_error "generic-array-result-by-value"
    "aggregate result `arr[N,u8]` cannot be returned by value; return `void` and take \
     the destination as an `addr` parameter"
    "fn make[N const usize]() arr[N,u8] { value arr[N,u8]\nreturn value }\n";
  semantic_error "generic-struct-result-by-value"
    "aggregate result `Ring[N]` cannot be returned by value; return `void` and take \
     the destination as an `addr` parameter"
    "struct Ring[N const usize] { data arr[N,u8] }\n\
     fn make[N const usize]() Ring[N] { value Ring[N]\n\
     return value }\n";
  semantic_accept "generic-bare-parameter-definition"
    "fn identity[T](value T) T { return value }\n";
  semantic_error "aggregate-declaration-copy"
    "aggregate value initialization is not supported; use `copy(dst, src)`"
    "struct S { x i64 }\nfn f() void { s S = {1}\n t S = s\nreturn }\n";
  semantic_error "aggregate-argument"
    "aggregate arguments cannot be passed by value; pass `&x` as `addr` or `handle[T]`"
    "struct S { x i64 }\n\
     fn take(p addr) void { return }\n\
     fn f() void { s S = {1}\n\
     take(s)\n\
     return }\n";
  let aggregate_parameter_text =
    "struct State { value u32 }\nfn consume(state State) void { return }\n"
  in
  semantic_pin "aggregate-parameter-name" aggregate_parameter_text 2 18 5
    "aggregate parameter `state` of type `State` cannot be passed by value; declare \
     `state` as `addr`; callers pass its address with `&`"
    None;
  semantic_accept "aggregate-parameter-address-twin"
    "struct State { value u32 }\nfn consume(state addr) void { return }\n";
  semantic_pin "aggregate-result-name"
    "struct State { value u32 }\n\
     fn build() State { result State = {1}\n\
    \ return result }\n"
    2
    (String.length "fn build() " + 1)
    5
    "aggregate result `State` cannot be returned by value; return `void` and take the \
     destination as an `addr` parameter"
    None;
  semantic_accept "aggregate-result-address-twin"
    "struct State { value u32 }\nfn build(destination addr) void { return }\n";
  let aggregate_argument_text =
    "struct State { value u32 }\n\
     fn consume(destination addr) void { return }\n\
     fn run() void { snapshot State = {1}\n\
     consume(snapshot)\n\
     return }\n"
  in
  semantic_pin "aggregate-argument-name" aggregate_argument_text 4 9 8
    "aggregate argument `snapshot` of type `State` cannot be passed by value"
    (Some "pass `&snapshot` to a function that accepts `addr`");
  semantic_accept "aggregate-argument-address-twin"
    (String.concat "\n"
       [
         "struct State { value u32 }";
         "fn consume(destination addr) void { return }";
         "fn run() void { snapshot State = {1}";
         "consume(&snapshot)";
         "return }";
       ]);
  semantic_accept "variadic-address-handle-arguments"
    "opaque Token\n\
     extern \"C\" { fn consume(marker u64, ...) u64 }\n\
     fn pass(pointer addr, token handle[Token]) u64 { return consume(1, pointer, \
     token) }\n";
  let typed_variadic_call =
    llvm_of
      "extern \"C\" { fn fas_sum(marker i32, ...) i32 }\n\
       fn pass(value f64) i32 { return fas_sum(1, value) }\n"
  in
  if not (contains typed_variadic_call "call i32 (i32, ...) @fas_sum(i32 1, double")
  then failwith "variadic-call-function-type: missing fixed signature and ellipsis";
  let fixed_call =
    llvm_of
      "extern \"C\" { fn fas_fixed(value i32) i32 }\n\
       fn pass(value i32) i32 { return fas_fixed(value) }\n"
  in
  if
    (not (contains fixed_call "call i32 @fas_fixed(i32 %v"))
    || contains fixed_call "call i32 (i32) @fas_fixed"
  then failwith "nonvariadic-call-function-type: fixed call text changed";
  semantic_error "variadic-vector-argument"
    "aggregate `vec[2,u32]` cannot be passed through C varargs"
    "extern \"C\" { fn consume(marker u64, ...) void }\n\
     fn pass() void { value vec[2,u32] = splat(0)\n\
     consume(1, value)\n\
     return }\n";
  let array_vararg =
    "extern \"C\" { fn consume(marker i32, ...) void }\n\
     fn pass() void { fname arr[13,u8] = {0,0,0,0,0,0,0,0,0,0,0,0,0}\n\
     consume(1, fname)\n\
     return }\n"
  in
  let array_vararg_expected =
    expected_diagnostic ~line_number:3 "consume(1, fname)" 12 5
      "aggregate `arr[13, u8]` cannot be passed through C varargs"
      "pass `&fname`; C passes arrays by address"
  in
  let array_vararg_rendered = semantic_render array_vararg in
  if array_vararg_rendered <> array_vararg_expected then
    failwith ("variadic-array-help: unexpected diagnostic: " ^ array_vararg_rendered);
  let record_vararg =
    "struct Item { value i32 }\n\
     extern \"C\" { fn consume(marker i32, ...) void }\n\
     fn pass() void { item Item = {1}\n\
     consume(1, item)\n\
     return }\n"
  in
  let record_vararg_expected =
    expected_diagnostic_without_help ~line_number:4 "consume(1, item)" 12 4
      "aggregate `Item` cannot be passed through C varargs"
  in
  let record_vararg_rendered = semantic_render record_vararg in
  if record_vararg_rendered <> record_vararg_expected then
    failwith
      ("variadic-record-no-help: unexpected diagnostic: " ^ record_vararg_rendered);
  let array_expression_vararg =
    "extern \"C\" { fn consume(marker i32, ...) void }\n\
     fn pass() void { items arr[2,arr[2,u8]] = {{0,0},{0,0}}\n\
     consume(1, items[0])\n\
     return }\n"
  in
  let array_expression_vararg_expected =
    expected_diagnostic_without_help ~line_number:3 "consume(1, items[0])" 12 8
      "aggregate `arr[2, u8]` cannot be passed through C varargs"
  in
  let array_expression_vararg_rendered = semantic_render array_expression_vararg in
  if array_expression_vararg_rendered <> array_expression_vararg_expected then
    failwith
      ("variadic-array-expression-no-help: unexpected diagnostic: "
     ^ array_expression_vararg_rendered);
  semantic_error "aggregate-assignment"
    "aggregate assignment is not supported; use `copy(dst, src)`"
    "struct S { x i64 }\n\
     fn f() void { source S = {1}\n\
     destination S = {2}\n\
     destination = source\n\
     return }\n";
  ignore
    (lower_of
       "struct S { x i64 }\nfn f(p bool) i64 { s S\nif p { s.x = 1 }\nreturn s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p bool) i64 { s S\n\
       \ if p { s.x = 1\n\
       \ s.y = 2 } else { s.x = 3\n\
       \ s.y = 4 }\n\
       \ return s.y }\n");
  ignore
    (lower_of
       "struct S { x i64 }\n\
        fn f(n i64) i64 { s S\n\
       \ switch n {\n\
       \ case 0: { s.x = 1 }\n\
       \ default: { s.x = 2 }\n\
       \ }\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(n i64) i64 { s S\n\
       \ switch n {\n\
       \ case 0: { s.x = 1 }\n\
       \ default: { s.y = 2 }\n\
       \ }\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "fn f(i i64) i64 { a arr[2,arr[2,i64]]\n\
       \ a[0][0] = 1\n\
       \ a[0][1] = 2\n\
       \ return a[0][i] }\n");
  ignore
    (lower_of "fn f(i i64) i64 { a arr[2,arr[2,i64]]\na[0][0] = 1\nreturn a[0][i] }\n");
  ignore
    (lower_of "fn f(i i64) i64 { v vec[2,i64]\n v[0] = 1\n v[1] = 2\n return v[i] }\n");
  semantic_error "aggregate-dynamic-index" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n i i64 = 0\n return a[i] }\n";
  semantic_error "aggregate-dynamic-write-keeps-other-element-uninitialized"
    "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n a[0] = 1\n return a[1] }\n";
  semantic_error "aggregate-loop-read-before-write" "use of uninitialized local `a`"
    "fn f() i64 { a arr[2,i64]\n return a[0] }\n";
  ignore (lower_of "fn f(i i64) i64 { a arr[2,i64]\na[i] = 1\nreturn a[i] }\n");
  ignore
    (lower_of
       "fn f() u32 { a arr[4,u32]\n\
        for i u32 = 0; i < 4; i += 1 { a[i] = i }\n\
        return a[2] }\n");
  ignore
    (lower_of
       "struct S { a arr[4,u32] }\n\
        fn f() u32 { s S\n\
        for i u32 = 0; i < 4; i += 1 { s.a[i] = i }\n\
        return s.a[2] }\n");
  ignore
    (lower_of
       "fn take(p addr) void { return }\n\
        fn f() u32 { a arr[2,u32]\n\
       \ take(&a)\n\
       \ return a[1] }\n");
  ignore
    (lower_of "fn f(i i64) i64 { a arr[2,i64]\n a[0] = 1\n a[1] = 2\n return a[i] }\n");
  ignore
    (lower_of
       "struct S { a arr[2,i64] z i64 }\n\
        fn f(i i64) i64 { s S\n\
       \ s.a[0] = 1\n\
       \ s.a[1] = 2\n\
       \ return s.a[i] }\n");
  ignore
    (lower_of
       "fn f(i i64) i64 { x i64\n\
        a arr[2,addr]\n\
        a[0] = &x\n\
        a[i][i64,0] = 1\n\
        return x }\n");
  ignore
    (llvm_of
       "fn take(p addr) void { return }\nfn f() i64 { x i64\n take(&x)\n return x }\n");
  semantic_error "field-escape-preserves-sibling-initialization"
    "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn take(p addr) void { return }\n\
     fn f() i64 { s S\n\
     take(&s.x)\n\
     t S\n\
     copy(t, s)\n\
     return 0 }\n";
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p bool) i64 { s S\n\
        if p { s.x = 1 } else { s.y = 2 }\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn f(p bool) i64 { s S\n\
        if p { s.x = 1 } else { return 0 }\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 }\n\
        fn f(p bool) i64 { s S\n\
       \ while p { s.x = 1 }\n\
       \ return s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p addr) void { return }\n\
        fn f(p bool) i64 { s S\n\
       \ if p { take(&s) } else { s.x = 1 }\n\
        return s.x }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p addr) void { return }\n\
        fn f(p bool) i64 { s S\n\
       \ if p { take(&s) } else { s.x = 1 }\n\
        return s.y }\n");
  ignore
    (lower_of
       "struct S { x i64 y i64 }\n\
        fn take(p addr) void { return }\n\
        fn f(p bool) i64 { s S\n\
       \ if p { take(&s) } else { s.x = 1 }\n\
        t S\n\
        copy(t, s)\n\
       \ return 0 }\n");
  semantic_error "aggregate-compound-read" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x += 1\n return 0 }\n";
  semantic_error "place-init-after-return" "use of uninitialized local `x`"
    "fn f(p bool) i64 { x i64\n if p { return 0\n x = 1 }\n return x }\n";
  semantic_error "place-init-after-break" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n while true { break\n x = 1 }\n return x }\n";
  semantic_error "place-init-after-continue" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n while true { continue\n x = 1 }\n return x }\n";
  semantic_accept "init-in-code-after-return"
    "fn f(x i64) i64 { return x\n a i64 = x + 1\n return a }\n";
  semantic_error "uninit-in-code-after-return" "use of uninitialized local `a`"
    "fn f(x i64) i64 { a i64\n return x\n return a }\n";
  semantic_error "code-after-return-still-checked"
    "integer literal is out of range for u8: `300`"
    "fn f(x i64) i64 { return x\n y u8 = 300\n return 0 }\n";
  semantic_accept "pruned-return-skips-instance-rest"
    "fn g[C const usize](x i32) i32 { if C >= 4 { return x }\n\
    \ a arr[4, i32] = {}\n\
    \ return a[C] + x }\n\
     fn main() i32 { return g[10](2) + g[3](4) }\n";
  let pruned_dispatch =
    llvm_of
      "fn leaf[N const usize](x usize) usize { return x + N }\n\
       fn pick[N const usize, C const usize](x usize) usize {\n\
      \ if C == 1 { return leaf[N](x) }\n\
      \ if x < N + C / 2 { return pick[N, C / 2](x) }\n\
      \ return pick[N + C / 2, C / 2](x) }\n\
       fn main() i32 { return bitcast[i32](trunc[u32](pick[0, 4](3))) }\n"
  in
  if
    contains pruned_dispatch "C=usize:0"
    || not (contains pruned_dispatch "N=usize:3;1:C=usize:1")
  then failwith "pruned-return-no-dead-instances";
  all_errors_pin "all-errors-casts"
    "fn f(a u64, b u32) u8 {\n\
    \    x u32 = a\n\
    \    y u64 = b + x\n\
    \    z u64 = y\n\
    \    return z\n\
     }\n\
     fn g(a u64) u8 {\n\
    \    return a\n\
     }\n\
     fn main() i32 {\n\
    \    q i32 = f(1, 2) + g(3)\n\
    \    return q\n\
     }\n"
    [
      (2, "cannot assign `u64` to `u32`: values above 4294967295 would be lost");
      (3, "`b + x` is computed in `u32` and may wrap before it reaches `u64`");
      (5, "cannot return `u64` as `u8`: values above 255 would be lost");
      (8, "cannot return `u64` as `u8`: values above 255 would be lost");
      (11, "`f(1, 2) + g(3)` is computed in `u8` and may wrap before it reaches `i32`");
    ];
  all_errors_pin "all-errors-no-cascade"
    "fn a1(p u64) u32 {\n\
    \    x u32\n\
    \    x = p\n\
    \    return x\n\
     }\n\
     fn a2(p u64) u64 {\n\
    \    t arr[4, u64] = {}\n\
    \    i u64 = 10\n\
    \    i = p + true\n\
    \    return t[i]\n\
     }\n\
     fn a3(p u64) u64 {\n\
    \    s u64 = 0\n\
    \    for i u64 = 0; i < p; i += 1 {\n\
    \        if i == 3 {\n\
    \            v u32 = i\n\
    \            break\n\
    \        }\n\
    \        s += i\n\
    \    }\n\
    \    return s\n\
     }\n\
     fn a4(p u64) u64 {\n\
    \    defer {\n\
    \        z u8 = p\n\
    \    }\n\
    \    return p\n\
     }\n\
     fn a5(p u64) u64 {\n\
    \    if p > 1 {\n\
    \        return p + true\n\
    \    }\n\
    \    return p\n\
     }\n\
     fn main() i32 {\n\
    \    return 0\n\
     }\n"
    [
      (3, "cannot assign `u64` to `u32`: values above 4294967295 would be lost");
      (9, "operands of `+` have different types: `u64` and `bool`");
      (16, "cannot assign `u64` to `u32`: values above 4294967295 would be lost");
      (25, "cannot assign `u64` to `u8`: values above 255 would be lost");
      (31, "operands of `+` have different types: `u64` and `bool`");
    ];
  all_errors_pin "all-errors-unresolved-locals"
    "fn a(p u64) u64 {\n\
    \    w Nope = p\n\
    \    return w + q\n\
     }\n\
     fn b(p u64) u64 {\n\
    \    p u32 = 1\n\
    \    return 0\n\
     }\n\
     fn c(p u64) u8 {\n\
    \    return a(p) + 1\n\
     }\n\
     fn main() i32 {\n\
    \    return 0\n\
     }\n"
    [
      (2, "unknown type `Nope`");
      (6, "duplicate local `p`");
      (10, "cannot return `u64` as `u8`: values above 255 would be lost");
    ];
  all_errors_pin "all-errors-instances"
    "fn pick[T](x T) u8 {\n\
    \    y u8 = x\n\
    \    k u8 = 300\n\
    \    return y\n\
     }\n\
     fn main() i32 {\n\
    \    v u64 = 1\n\
    \    w u32 = 2\n\
    \    a u8 = pick(v)\n\
    \    b u8 = pick(w)\n\
    \    c u8 = pick(v)\n\
    \    return 0\n\
     }\n"
    [
      (2, "cannot assign `u64` to `u8`: values above 255 would be lost");
      (2, "cannot assign `u32` to `u8`: values above 255 would be lost");
      (3, "integer literal is out of range for u8: `300`");
    ];
  (match (cli_run [ "--all-errors"; "x.fas" ]).all_errors with
  | true -> ()
  | false -> failwith "all-errors-flag");
  (match (cli_run [ "x.fas" ]).all_errors with
  | false -> ()
  | true -> failwith "all-errors-default-off");
  let uninitialized_aggregate_llvm =
    llvm_of "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x = 1\n return s.x }\n"
  in
  if
    contains uninitialized_aggregate_llvm "memset"
    || contains uninitialized_aggregate_llvm "store %struct.S zeroinitializer"
  then failwith "place-init: aggregate declaration emitted implicit initialization";
  semantic_error "defer-does-not-initialize" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { x = 1 }\n return x }\n";
  semantic_accept "defer-value-fact-index"
    "fn p() i32 { a arr[4, i32] = {}\ni i32 = 9\n{ defer { i = 1 } }\nreturn a[i] }";
  semantic_message "defer-value-fact-index-invalid-twin"
    "array index is out of bounds for length 4"
    "fn p() i32 { a arr[4, i32] = {}\ni i32 = 9\n{ defer { i = 4 } }\nreturn a[i] }";
  semantic_accept "defer-address-fact"
    "fn p() i32 { x i32 = 0\nq addr = null\n{ defer { q = &x } }\nreturn q[i32] }";
  semantic_message "defer-address-fact-null-twin" "access through null address"
    "fn p() i32 { x i32 = 0\nq addr = &x\n{ defer { q = null } }\nreturn q[i32] }";
  semantic_accept "defer-value-fact-divisor"
    "fn p() i32 { d i32 = 0\n{ defer { d = 2 } }\nreturn 10 / d }";
  semantic_message "defer-value-fact-zero-divisor-twin"
    "division by zero is not a defined runtime operation"
    "fn p() i32 { d i32 = 2\n{ defer { d = 0 } }\nreturn 10 / d }";
  semantic_accept "defer-value-fact-continue-step"
    "fn f() void { d i32 = 0\nfor ; ; d += 10 / d { defer { d = 2 }\ncontinue } }";
  semantic_message "defer-value-fact-continue-step-invalid-twin"
    "division by zero is not a defined runtime operation"
    "fn f() void { d i32 = 2\nfor ; ; d += 10 / d { defer { d = 0 }\ncontinue } }";
  semantic_accept "defer-value-fact-labeled-continue-step"
    "fn f() void { d i32 = 0\n\
     outer: for i i32 = 0; i < 1; d += 10 / d {\n\
     for ; ; { defer { d = 2 }\n\
     continue outer }\n\
     } }";
  semantic_message "defer-value-fact-labeled-continue-step-invalid-twin"
    "division by zero is not a defined runtime operation"
    "fn f() void { d i32 = 2\n\
     outer: for i i32 = 0; i < 1; d += 10 / d {\n\
     for ; ; { defer { d = 0 }\n\
     continue outer }\n\
     } }";
  ignore
    (lower_of "fn f() i64 { x i64\n defer { observed i64 = x }\n x = 1\n return 0 }\n");
  ignore (lower_of "fn f() void { x i64\n defer { observed i64 = x }\n x = 1 }\n");
  ignore
    (lower_of
       "fn f() i64 { x i64\n\
       \ defer { observed i64 = x }\n\
       \ { defer { x = 1 } }\n\
       \ return 0 }\n");
  semantic_error "defer-normal-exit-uninitialized" "use of uninitialized local `x`"
    "fn f() void { x i64\n defer { observed i64 = x } }\n";
  semantic_error "defer-return-exit-uninitialized" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { observed i64 = x }\n return 0 }\n";
  ignore
    (lower_of
       "fn f() i64 { x i64\n defer { observed i64 = x }\n while true { }\n return 0 }\n");
  ignore
    (lower_of
       "fn f() i64 { x i64\n\
       \ defer { observed i64 = x }\n\
       \ defer { x = 1 }\n\
       \ return 0 }\n");
  semantic_error "defer-lifo-read-before-write" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { x = 1 }\n defer { observed i64 = x }\n return 0 }\n";
  ignore
    (lower_of
       "fn f() i64 { x i64\n defer { observed i64 = x }\n defer { while true { } } }\n");
  semantic_error "defer-read-before-divergence" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { while true { } }\n defer { observed i64 = x } }\n";
  ignore
    (lower_of
       "fn f() void { while true { x i64\n\
       \ defer { observed i64 = x }\n\
       \ x = 1\n\
       \ break } }\n");
  semantic_error "defer-break-uninitialized" "use of uninitialized local `x`"
    "fn f() void { while true { x i64\n defer { observed i64 = x }\n break } }\n";
  ignore
    (lower_of
       "fn f() void { index i64 = 0\n\
       \ while index < 1 { x i64\n\
       \ defer { observed i64 = x }\n\
       \ x = 1\n\
       \ index += 1\n\
       \ continue } }\n");
  semantic_error "defer-continue-uninitialized" "use of uninitialized local `x`"
    "fn f() void { while true { x i64\n defer { observed i64 = x }\n continue } }\n";
  ignore
    (lower_of
       "fn take(value i64) void { return }\n\
       \ fn f() void { for value i64; true; take(value) {\n\
       \ defer { value = 1 }\n\
       \ continue\n\
       \ } }\n");
  ignore
    (lower_of
       "fn take(value i64) void { return }\n\
       \ fn f() void { for value i64; true; take(value) {\n\
       \ defer { value = 1 }\n\
       \ } }\n");
  semantic_error "for-step-uninitialized" "use of uninitialized local `value`"
    "fn take(value i64) void { return }\n\
    \ fn f() void { for value i64; true; take(value) { continue } }\n";
  ignore
    (lower_of
       "fn take(value i64) void { return }\n\
        fn f(condition bool) void { for value i64; true; take(value) {\n\
       \ if condition { value = 1 } else { continue }\n\
       \ } }\n");
  ignore
    (lower_of
       "fn f() i64 { value i64\n while true { value = 1\n break }\n return value }\n");
  ignore
    (lower_of
       "fn f() i64 { value i64\n\
       \ while true { defer { value = 1 }\n\
       \ break }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f(condition bool) i64 { value i64\n\
       \ while condition { value = 1\n\
       \ break }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f(choice i64) i64 { value i64\n\
       \ while true { switch choice {\n\
       \ case 0: { value = 1\n\
       \ break }\n\
       \ default: { value = 2\n\
       \ break }\n\
       \ } }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f() i64 { value i64\n\
       \ while true { while true { break }\n\
       \ value = 1\n\
       \ break }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f(condition bool) i64 { value i64\n\
       \ while true { if condition { value = 1\n\
       \ break } else { continue\n\
       \ switch 0 { default: { break } } } }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f(choice i64) i64 { value i64\n\
       \ while true { switch choice {\n\
       \ case 0: { defer { value = 1 }\n\
       \ break }\n\
       \ default: { defer { value = 2 }\n\
       \ break }\n\
       \ } }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f() i64 { value i64\n\
       \ while true { while true { switch 0 { default: { break } } }\n\
       \ value = 1\n\
       \ break }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f() void { while true { continue\n\
       \ { value i64\n\
       \ defer { observed i64 = value }\n\
       \ continue } } }\n");
  ignore
    (lower_of
       "fn f(choice i64) i64 { value i64\n\
       \ while true { switch choice {\n\
       \ case 0: { value = 1\n\
       \ break }\n\
       \ default: { break }\n\
       \ } }\n\
       \ return value }\n");
  semantic_error "nested-defer" "nested defer is not allowed"
    "fn f() void { defer { defer { } } }\n";
  semantic_error "defer-return" "return is not allowed inside defer"
    "fn f() void { defer { return } }\n";
  semantic_error "defer-break" "break is not allowed inside defer"
    "fn f() void { while true { defer { break } break } }\n";
  semantic_error "defer-continue" "continue is not allowed inside defer"
    "fn f() void { while true { defer { continue } break } }\n";
  semantic_error "raw-uninitialized-array-read" "use of uninitialized local `x`"
    "fn f() u32 { x arr[4,u32]\n x[0] = 1\n return x[3] }\n";
  semantic_error "raw-uninitialized-address-read" "use of uninitialized local `p`"
    "fn f() i64 { p addr\n p[i64] = 1\n return p[i64] }\n";
  semantic_error "raw-uninitialized-array-copy" "use of uninitialized local `x`"
    "fn f() u32 { x arr[4,u32]\n t arr[4,u32]\n copy(t, x)\n return t[0] }\n";
  ignore
    (lower_of
       "fn f() u32 { x arr[4,u32]\n\
       \ for i u32 = 0; i < 4; i += 1 { x[i] = i }\n\
       \ return x[0] }\n");
  semantic_error "raw-branch-uninitialized-element" "use of uninitialized local `x`"
    "fn f(p bool) u32 { x arr[4,u32]\n if p { x[0] = 1 }\n return x[1] }\n";
  semantic_error "raw-defer-read-before-write" "use of uninitialized local `x`"
    "fn f() u64 { x u64\n defer { x = 1 }\n return x }\n";
  semantic_error "raw-uninitialized-struct-field" "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f() i64 { s S\n s.x = 1\n return s.y }\n";
  semantic_error "raw-uninitialized-vector-lane" "use of uninitialized local `v`"
    "fn f() u32 { v vec[4,u32]\n v[0] = 1\n return v[3] }\n";
  semantic_error "raw-uninitialized-scalar-read" "use of uninitialized local `x`"
    "fn f() i64 { x i64\n return x }\n";
  ignore (lower_of "fn f() i64 { x i64\n x = 5\n return x }\n");
  let uninitialized_no_zero =
    llvm_of
      "fn f() u32 { x arr[4,u32]\n\
      \ for i u32 = 0; i < 4; i += 1 { x[i] = i }\n\
      \ return x[3] }\n"
  in
  if
    contains uninitialized_no_zero "memset"
    || contains uninitialized_no_zero "zeroinitializer"
  then failwith "plain declaration emitted implicit initialization";
  parse_message "raw-declaration-removed"
    "Fas has no `= raw`; write `x i64` to declare without initializing"
    "fn f() i64 { x i64 = raw\n return 0 }\n";
  semantic_error "raw-not-a-value-return" "unknown name `raw`"
    "fn f() i64 { return raw }\n";
  semantic_error "raw-not-a-value-call" "unknown name `raw`"
    "fn g(x i64) i64 { return x }\nfn f() i64 { return g(raw) }\n";
  semantic_error "raw-not-a-value-assign" "unknown name `raw`"
    "fn f() i64 { x i64 = 1\n x = raw\n return x }\n";
  parse_message "raw-init-trailing"
    "Fas has no `= raw`; write `x i64` to declare without initializing"
    "fn f() i64 { x i64 = raw + 1\n return x }\n";
  ignore (lower_of "fn f() i64 { raw i64 = 3\n return raw }\n");
  semantic_error "lexical-scope-same-block" "duplicate local `value`"
    "fn f() i64 { value i64 = 1\n value i64 = 2\n return value }\n";
  semantic_error "lexical-scope-parameter-body" "duplicate local `value`"
    "fn f(value i64) i64 { value i64 = 2\n return value }\n";
  semantic_error "lexical-scope-type-parameter-body" "duplicate local `T`"
    "fn f[T](value T) T { T i32 = 2\n\
    \ return value }\n\
     fn test(value i32) i32 { return f[i32](value) }\n";
  semantic_error "lexical-scope-const-parameter-body" "duplicate local `N`"
    "fn f[N const i32]() i32 { N i32 = 2\n\
    \ return N }\n\
     fn main() i32 { return f[1]() }\n";
  ignore
    (lower_of
       "fn f() i64 { value i64 = 1\n { value i64 = 2\n value += 1 }\n return value }\n");
  ignore
    (lower_of "fn f(value i64) i64 { { value i64 = 2\n value += 1 }\n return value }\n");
  semantic_error "lexical-scope-independent-initialization"
    "use of uninitialized local `value`"
    "fn f() i64 { value i64\n { value i64 = 2 }\n return value }\n";
  ignore
    (lower_of
       "fn f() i64 { total i64 = 0\n\
       \ { value i64 = 2\n\
       \ total += value }\n\
       \ { value i64 = 3\n\
       \ total += value }\n\
       \ return total }\n");
  ignore
    (lower_of
       "fn f(flag bool) i64 { total i64 = 0\n\
       \ if flag { value i64 = 2\n\
       \ total += value }\n\
       \ else { value i64 = 3\n\
       \ total += value }\n\
       \ return total }\n");
  ignore
    (lower_of
       "fn f(flag bool) i64 { value i64 = 1\n\
       \ if flag { value i64 = 2\n\
       \ value += 1 }\n\
       \ return value }\n");
  ignore
    (lower_of
       "fn f() i64 { total i64 = 0\n\
       \ for index i64 = 0; index < 3; index += 1 {\n\
       \   total += index\n\
       \ }\n\
       \ return total }\n");
  semantic_error "lexical-scope-for-escape" "unknown name `index`"
    "fn f() i64 { for index i64 = 0; index < 1; index += 1 { }\n return index }\n";
  ignore
    (lower_of
       "fn delayed(out addr, value i64) void {\n\
       \ defer { out[i64] += value }\n\
       \ { value i64 = 100\n\
       \ out[i64] += value - 100 }\n\
       \ }\n");
  let lexical_const_shadow =
    llvm_of "const value i64 = 1\nfn f() i64 { value i64 = 2\n return value }\n"
  in
  if
    (not (contains lexical_const_shadow "store i64 2"))
    || not (contains lexical_const_shadow "load i64, ptr")
  then failwith "lexical-scope-const-shadow: local did not win lookup";
  let lexical_const_array_shadow =
    llvm_of
      "const values arr[1,i64] = {1}\n fn f() i64 { values i64 = 2\n return values }\n"
  in
  if
    (not (contains lexical_const_array_shadow "store i64 2"))
    || not (contains lexical_const_array_shadow "load i64, ptr")
  then failwith "lexical-scope-const-array-shadow: local did not win lookup";
  let lexical_initializer_shadow =
    llvm_of "const value i64 = 1\nfn f() i64 { value i64 = value + 1\n return value }\n"
  in
  if not (contains lexical_initializer_shadow "add i64 1, 1") then
    failwith "lexical-scope-initializer-shadow: outer binding was not visible";
  semantic_error "lexical-scope-function-call-shadow" "value, not a function"
    "fn target() i32 { return 1 }\n\
     fn check_case() i32 { target i32 = 2\n\
    \ return target() }\n";
  semantic_error "lexical-scope-generic-call-shadow" "value, not a function"
    "fn target[N const i32]() i32 { return N }\n\
     fn check_case() i32 { target i32 = 2\n\
    \ return target[1]() }\n";
  semantic_error "lexical-scope-type-shadow" "value, not a type"
    "struct Item { value i32 }\nfn check_case(Item i32) i32 { local Item\n return 0 }\n";
  semantic_error "lexical-scope-aggregate-length-shadow" "not a compile-time constant"
    "const Count usize = 2\n\
     fn check_case(Count usize) i32 { local arr[Count,i32]\n\
    \ return 0 }\n";
  ignore
    (lower_of
       "const Index i32 = 0\n\
        fn read(Index i32) i32 { values arr[2,i32]\n\
       \ values[0] = 7\n\
       \ return values[Index] }\n\
        fn main() i32 { return read(1) }\n");
  ignore
    (lower_of
       "const Index i32 = 0\n\
        fn read(Index i32) i32 { values arr[2,i32]\n\
       \ values[0] = 7\n\
       \ return values[Index + 0] }\n\
        fn main() i32 { return read(1) }\n");
  let static_global_index =
    llvm_of
      "const Index i32 = 1\n\
       fn read() i32 { values arr[2,i32]\n\
      \ values[Index] = 9\n\
      \ return values[Index] }\n"
  in
  if not (contains static_global_index "ret i32 %v") then
    failwith "static-global-index: constant index lost its initialization proof";
  let dynamic_shadowed_index =
    llvm_of
      "const Index i32 = 0\n\
       fn read(Index i32) i32 { values arr[2,i32]\n\
      \ values[0] = 7\n\
      \ values[1] = 11\n\
      \ return values[Index] }\n"
  in
  if not (contains dynamic_shadowed_index "sext i32 %") then
    failwith "dynamic-shadowed-index: parameter was replaced by the global constant";
  ignore
    (lower_of
       "const Index i32 = 1\n\
        const Hidden i32 = 0\n\
        fn read(Hidden i32) i32 { values arr[2,i32]\n\
       \ values[Index] = 9\n\
       \ return values[if true { Index } else { Hidden }] }\n");
  semantic_error "lexical-scope-declaration-before-use" "unknown assignment target"
    "fn f() i64 { value = 1\n value i64 = 2\n return value }\n";
  semantic_error "void-value-return" "void function cannot return a value"
    "extern \"C\" { fn sink() void }\nfn f() void { return sink() }\n";
  semantic_error "nonvoid-fallthrough"
    "function `f` returning `i64` may reach the end without `return`" "fn f() i64 { }\n";
  ignore (lower_of "fn spin() i32 { while true { } }\n");
  ignore (lower_of "fn spin() i32 { for ; ; { continue } }\n");
  ignore (lower_of "fn spin() i32 { while true { while true { break } } }\n");
  ignore (lower_of "fn finish() i32 { defer { while true { } } }\n");
  ignore
    (lower_of
       "fn finish(choice bool) i32 { if choice { return 1 }\n\
       \ defer { while true { } } }\n");
  ignore
    (lower_of "fn spin() i32 { while true { defer { while true { } }\n break } }\n");
  semantic_error "conditional-loop-fallthrough"
    "function `spin` returning `i32` may reach the end without `return`"
    "fn spin(condition bool) i32 { while condition { } }\n";
  semantic_error "unconditional-loop-reachable-break"
    "function `spin` returning `i32` may reach the end without `return`"
    "fn spin(condition bool) i32 { while true { if condition { break } } }\n";
  semantic_error "switch-break-exits-loop"
    "function `spin` returning `i32` may reach the end without `return`"
    "fn spin(value i32) i32 { while true { switch value {\n\
     case 0:\n\
     break\n\
     default:\n\
     continue\n\
     } } }\n";
  semantic_error "switch-duplicate-case" "duplicate case label `1`"
    "fn f(x i32) i32 { switch x { case 1: return 1; case 1: return 2 } }\n";
  semantic_error "switch-duplicate-u64-case"
    "duplicate case label `18446744073709551615`"
    "fn f(x u64) i32 { switch x { case 18446744073709551615: return 1; case \
     18446744073709551615: return 2 } }\n";
  semantic_accept "switch-multiple-case-values"
    "fn f(x i32) i32 { switch x { case 1, 2: return 3; default: return 0 } }\n";
  semantic_accept "switch-multiple-case-values-twin"
    "fn f(x i32) i32 { switch x { case 1, 2: return 3; case 4, 5: return 0 } return 0 }\n";
  semantic_accept "switch-const-generic-case-values"
    ("fn choose[N const i32](x i32) i32 { switch x { case N, N + 1: return 1; "
   ^ "default: return 0 } }\nfn run() i32 { return choose[3](4) }\n");
  let switch_duplicate_values =
    "fn f(x i32) i32 { switch x { case 1, 2: return 1; case 3, 2: return 2 } }\n"
  in
  let duplicate_diagnostics = semantic_diagnostics switch_duplicate_values in
  let first_value_column =
    List.hd (positions switch_duplicate_values "case 1, 2:")
    + String.length "case 1, " + 1
  and duplicate_value_column =
    List.hd (positions switch_duplicate_values "case 3, 2:")
    + String.length "case 3, " + 1
  in
  (match duplicate_diagnostics with
  | [ diagnostic ]
    when diagnostic.Diag.message = "duplicate case label `2`"
         && diagnostic.primary.Span.column = duplicate_value_column
         && diagnostic.notes
            = [
                Printf.sprintf "first case value is at regression.fas:1:%d"
                  first_value_column;
              ] ->
      ()
  | diagnostics ->
      failwith
        ("switch-multiple-case-values-duplicate: "
        ^ Diag.render_all ~source:(Some (source switch_duplicate_values)) diagnostics));
  let multi_switch_llvm =
    llvm_of "fn f(x i32) i32 { switch x { case 1, 2: return 3; default: return 0 } }\n"
  in
  if
    List.length (positions multi_switch_llvm "switch i32") <> 1
    || not
         (contains multi_switch_llvm
            "switch i32 %v1, label %b3 [ i32 1, label %b2 i32 2, label %b2 ]")
  then failwith "switch-multiple-case-values: expected one switch to a shared arm";
  semantic_error "switch-nonconst-case" "case label must be a compile-time constant"
    "fn f(x i32, y i32) i32 { switch x { case y: return 1 } return 0 }\n";
  semantic_message "switch-case-narrow-arithmetic"
    "`A + 1` is computed in `u32` and may wrap before it reaches `u64`"
    "const A u32 = 1\nfn f(x u64) i32 { switch x { case A + 1: return 1 } return 0 }\n";
  semantic_accept "switch-case-narrow-arithmetic-twin"
    "const A u32 = 1\n\
     fn f(x u64) i32 { switch x { case zext[u64](A) + 1: return 1 } return 0 }\n";
  semantic_message "switch-literal-case-out-of-range"
    "integer literal is out of range for u8: `300`"
    "fn f(x u8) i32 { switch x { case 300: return 1 } return 0 }\n";
  semantic_accept "switch-literal-case-range-twin"
    "fn f(x u8) i32 { switch x { case 255: return 1 } return 0 }\n";
  semantic_message "switch-constant-case-out-of-range"
    "integer literal is out of range for u8: `300`"
    "const VALUE u16 = 200 + 100\n\
     fn f(x u8) i32 { switch x { case VALUE: return 1 } return 0 }\n";
  semantic_accept "switch-constant-case-range-twin"
    "const VALUE u8 = 200 + 55\n\
     fn f(x u8) i32 { switch x { case VALUE: return 1 } return 0 }\n";
  semantic_error "switch-vec-scrutinee" "switch value must be an integer or bool"
    "fn f(v vec[2,i32]) i32 { switch v { case 1: return 1 } return 0 }\n";
  semantic_error "switch-bool-exhaustive-still-needs-return"
    "function `f` returning `i32` may reach the end without `return`"
    "fn f(x bool) i32 { switch x { case true: return 1; case false: return 2 } }\n";
  semantic_error "addr-handle-nonopaque-target"
    "handle type argument must be an opaque type"
    "struct S { x u8 }\n\
     fn f(p addr) addr { return handle_addr(handle_from_addr[S](p)) }\n";
  semantic_error "addr-handle-nonopaque-wrapper"
    "handle type argument must be an opaque type"
    "opaque O\nfn f(p addr) addr { return handle_from_addr[handle[O]](p) }\n";
  semantic_error "addr-handle-wrong-arg" "handle_from_addr argument must be an addr"
    "opaque O\n\
     fn f(n usize) usize { return addr_bits(handle_addr(handle_from_addr[O](n))) }\n";
  let handle_null_source =
    "opaque F\nfn f() handle[F] { return handle_from_addr[F](null) }\n"
  in
  let handle_null_offset = List.hd (positions handle_null_source "null") in
  (match semantic_diagnostics handle_null_source with
  | [ diagnostic ] ->
      let span = diagnostic.Diag.primary in
      if
        diagnostic.message
        <> "write `null` directly where a `handle[F]` is expected; \
            `handle_from_addr[F](null)` is not needed"
        || span.file <> "regression.fas"
        || span.start_offset <> handle_null_offset
        || span.end_offset <> handle_null_offset + 4
        || span.line <> 2
        || span.column <> handle_null_offset - String.length "opaque F\n" + 1
      then failwith "handle-from-addr-null: exact message or null span changed"
  | _ -> failwith "handle-from-addr-null: expected one diagnostic");
  semantic_accept "handle-null-local-initializer"
    "opaque F\nfn f() bool { h handle[F] = null\n return h == null }\n";
  semantic_error "addr-bits-wrong-arg" "addr_bits argument must be an addr"
    "fn f(n u32) usize { return addr_bits(n) }\n";
  semantic_accept "addr-from-bits-widening-arg"
    "fn f(n u32) addr { return addr_from_bits(n) }\n";
  semantic_error "handle-addr-wrong-arg" "handle_addr argument must be a handle"
    "fn f(p addr) addr { return handle_addr(p) }\n";
  semantic_accept "handle-roundtrip-local-address-return"
    "opaque Token\n\
     fn leak() addr {\n\
     x u32 = 58\n\
     h handle[Token] = handle_from_addr[Token](&x)\n\
     return handle_addr(h) }\n";
  semantic_error "handle-from-addr-bare"
    "`handle_from_addr` needs a type argument: `handle_from_addr[T](p)`"
    "fn f(p addr) addr { return handle_from_addr(p) }\n";
  semantic_error "addr-const-context" "expression is not compile-time constant"
    "opaque O\nconst X addr = handle_from_addr[O](addr_from_bits(0))\n";
  let addr_equality_path = llvm_of "fn f(a addr, b addr) bool { return a == b }\n" in
  List.iter
    (fun marker ->
      if not (contains addr_equality_path marker) then
        failwith ("addr-equality-path: missing `" ^ marker ^ "`"))
    [ "ptrtoint ptr"; "icmp eq i64" ];
  let condition_forms =
    llvm_of
      "fn f(x bool, y u32, z u32) void {\n\
      \  if !(x) { return }\n\
      \  if !(y < z) { return }\n\
      \  while !(y == z) { return }\n\
      \  return\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains condition_forms marker) then
        failwith ("condition-forms: missing `" ^ marker ^ "`"))
    [ "icmp ult"; "icmp eq" ];
  let nested_struct_initializer_path =
    llvm_of
      "struct S { a u32 }\n\
       fn is(x u32) bool { return x == 1 }\n\
       fn f(x u32) void {\n\
      \  literal S = { x }\n\
      \  if is(literal.a) { return }\n\
      \  return\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains nested_struct_initializer_path marker) then
        failwith ("nested-struct-initializer-path: missing `" ^ marker ^ "`"))
    [ "%struct.S"; "store i32" ];
  let null_contexts =
    llvm_of
      "opaque O\n\
       const null_addr addr = null\n\
       const null_handle handle[O] = null\n\
       const addr_is_null bool = null_addr == null\n\
       const handle_is_null bool = null == null_handle\n\
       fn return_addr() addr { return null }\n\
       fn return_handle() handle[O] { return null }\n\
       fn local_nulls() bool { p addr = null\n\
      \ h handle[O] = null\n\
      \ return p == null && h == null }\n\
       fn call_address() bool { return addr_equal(null) }\n\
       fn addr_equal(a addr) bool { return null == a }\n\
       fn handle_null_left(a handle[O]) bool { return null != a }\n\
       fn handle_equal(a handle[O], b handle[O]) bool { return a == b }\n\
       fn addr_constant() bool { return addr_is_null }\n\
       fn handle_constant() bool { return handle_is_null }\n"
  in
  List.iter
    (fun marker ->
      if not (contains null_contexts marker) then
        failwith ("null-contexts: missing `" ^ marker ^ "`"))
    [ "ret ptr null"; "icmp eq ptr"; "ret i1 true" ];
  semantic_error "handle-order-reject" "ordered comparison `<` needs an integer"
    "opaque O\nfn f(a handle[O], b handle[O]) bool { return a < b }\n";
  semantic_error "handle-arithmetic-reject"
    "operator `+` needs integer or integer vector operands"
    "opaque O\nfn f(a handle[O], b handle[O]) handle[O] { return a + b }\n";
  semantic_error "cross-handle-equality-reject"
    "operands of `==` have different types: `handle[O]` and `handle[P]`"
    "opaque O\nopaque P\nfn f(a handle[O], b handle[P]) bool { return a == b }\n";
  semantic_error "handle-null-unconstrained" "null requires an addr or handle context"
    "opaque O\nfn f() bool { return null == null }\n";
  semantic_error "addr-index-reject" "raw access on `addr` needs an element type"
    "fn f(p addr) u8 { return p[0] }\n";
  parse_error_message "removed-typed-pointer"
    "typed pointers are not Fas types; use `addr`"
    "fn f() void { p ptr[u8]\n return }\n";
  let dot_star = "fn f(p addr) addr { return p.* }\n" in
  semantic_pin "removed-pointer-dereference" dot_star 1
    (String.index dot_star '.' + 1)
    2 "Fas has no `.*`; read through an `addr` with `p[T]`" None;
  parse_error_message "removed-ptr-add"
    "builtin `ptr_add` is not defined; use address arithmetic"
    "fn f(p addr, n usize) addr { return ptr_add(p, n) }\n";
  parse_error_message "removed-ptr-add-bytes"
    "builtin `ptr_add_bytes` is not defined; use address arithmetic"
    "fn f(p addr, n usize) addr { return ptr_add_bytes(p, n) }\n";
  semantic_error "addr-bitcast-from-reject"
    "illegal cast for source and destination widths"
    "fn f(p addr) usize { return bitcast[usize](p) }\n";
  semantic_error "addr-bitcast-to-reject"
    "illegal cast for source and destination widths"
    "fn f(n usize) addr { return bitcast[addr](n) }\n";
  semantic_error "handle-field-reject" "no field `x` on `handle[O]`"
    "opaque O\nfn f(h handle[O]) usize { return h.x }\n";
  semantic_error "handle-index-reject" "cannot select through a handle"
    "opaque O\nfn f(h handle[O]) u8 { return h[0] }\n";
  semantic_error "comparison-chaining-reject" "comparisons cannot be chained"
    "fn f(a i32, b i32, c i32) bool { return a < b < c }\n";
  semantic_error "conditional-defer-divergence"
    "function `finish` returning `i32` may reach the end without `return`"
    "fn finish(choice bool) i32 { defer { if choice { while true { } } } }\n";
  semantic_error "unreached-defer-does-not-consume-break"
    "function `spin` returning `i32` may reach the end without `return`"
    "fn spin() i32 { while true { break\n defer { while true { } } } }\n";
  semantic_error "extern-c-struct-parameter"
    "C functions that take or return struct `S` by value cannot be imported"
    "struct S { x i64 }\nextern \"C\" { fn take(value S) void }\n";
  semantic_error "extern-c-array-parameter"
    "aggregate parameter `value` of type `arr[2,i64]` cannot be passed by value; \
     declare `value` as `addr`"
    "extern \"C\" { fn take(value arr[2,i64]) void }\n";
  semantic_error "extern-c-struct-return"
    "C functions that take or return struct `S` by value cannot be imported"
    "struct S { x i64 }\nextern \"C\" { fn make() S }\n";
  let extern_c_struct_parameter =
    "struct Widget { data u8 }\nextern \"C\" { fn receive(item Widget) void }\n"
  in
  semantic_pin "extern-c-struct-parameter-import-diagnostic" extern_c_struct_parameter 2
    (String.length "extern \"C\" { fn receive(item " + 1)
    6 "C functions that take or return struct `Widget` by value cannot be imported" None;
  let extern_c_struct_result =
    "struct Packet { data u8 }\nextern \"C\" { fn produce() Packet }\n"
  in
  semantic_pin "extern-c-struct-result-import-diagnostic" extern_c_struct_result 2
    (String.length "extern \"C\" { fn produce() " + 1)
    6 "C functions that take or return struct `Packet` by value cannot be imported" None;
  semantic_error "extern-c-vector-parameter"
    "cannot use `vec[4, i32]` by value; use a pointer"
    "extern \"C\" { fn take(value vec[4,i32]) void }\n";
  semantic_error "extern-c-array-return"
    "aggregate result `arr[2,i64]` cannot be returned by value; return `void` and take \
     the destination as an `addr` parameter"
    "extern \"C\" { fn make() arr[2,i64] }\n";
  semantic_error "extern-c-opaque-parameter"
    "opaque type `Handle` can only be held as `handle[Handle]`"
    "opaque Handle\nextern \"C\" { fn take(value Handle) void }\n";
  semantic_error "extern-c-definition-fallthrough"
    "function `value` returning `i64` may reach the end without `return`"
    "extern \"C\" { fn value() i64 { } }\n";
  semantic_error "extern-c-definition-struct-parameter"
    "aggregate parameter `value` of type `S` cannot be passed by value; declare \
     `value` as `addr`"
    "struct S { x i64 }\nextern \"C\" { fn take(value S) void { return } }\n";
  parse_error_message "extern-c-variadic-definition"
    "extern \"C\" function definitions cannot be variadic"
    "extern \"C\" { fn take(marker u64, ...) void { return } }\n";
  semantic_error "extern-c-const-parameter"
    "extern \"C\" functions cannot have const parameters"
    "extern \"C\" { fn value[N const usize]() usize }\n";
  let c_scalar_abi =
    llvm_of
      "extern \"C\" {\n\
       fn b(x bool) bool\n\
       fn u8_value(x u8) u8\n\
       fn i8_value(x i8) i8\n\
       fn u16_value(x u16) u16\n\
       fn i16_value(x i16) i16\n\
       }\n\
       fn main() i32 {\n\
       x bool = b(true)\n\
       a u8 = u8_value(1)\n\
       c i8 = i8_value(-1)\n\
       d u16 = u16_value(2)\n\
       e i16 = i16_value(-2)\n\
       return zext[i32](x) + zext[i32](a) + sext[i32](c) + zext[i32](d) + sext[i32](e)\n\
       }\n"
  in
  List.iter
    (fun expected ->
      if not (contains c_scalar_abi expected) then
        failwith ("extern-c-scalar-extension: missing `" ^ expected ^ "`"))
    [
      "declare zeroext i1 @b(i1 zeroext)";
      "declare zeroext i8 @u8_value(i8 zeroext)";
      "declare signext i8 @i8_value(i8 signext)";
      "declare zeroext i16 @u16_value(i16 zeroext)";
      "declare signext i16 @i16_value(i16 signext)";
      "call zeroext i1 @b(i1 zeroext true)";
      "call signext i8 @i8_value(i8 signext 255)";
    ];
  let c_definition_abi =
    llvm_of
      "extern \"C\" {\n\
       fn b_value(x bool) bool { return x }\n\
       fn u8_value(x u8) u8 { return x }\n\
       fn i8_value(x i8) i8 { return x }\n\
       fn u16_value(x u16) u16 { return x }\n\
       fn i16_value(x i16) i16 { return x }\n\
       fn empty() void { }\n\
       }\n"
  in
  List.iter
    (fun expected ->
      if not (contains c_definition_abi expected) then
        failwith ("extern-c-definition: missing `" ^ expected ^ "`"))
    [
      "define zeroext i1 @b_value(i1 zeroext";
      "define zeroext i8 @u8_value(i8 zeroext";
      "define signext i8 @i8_value(i8 signext";
      "define zeroext i16 @u16_value(i16 zeroext";
      "define signext i16 @i16_value(i16 signext";
      "define void @empty()";
    ];
  let internal_aggregate_abi =
    llvm_of
      "struct S @align(16) { x i64 y i64 }\n\
       const A arr[2,i64] = {1, 2}\n\
       fn check_struct(p addr) bool { return p[S].x == 1 && p[S].y == 2 }\n\
       fn check_array(p addr) bool { return p[i64,0] == 1 && p[i64,1] == 2 }\n\
       fn pass_vector(value vec[3,i32]) vec[3,i32] { return value }\n\
       fn main() i32 {\n\
       literal S = {1, 2}\n\
       if !check_struct(&literal) { return 1 }\n\
       a arr[2,i64]\n\
       copy(a, A)\n\
       if !check_array(&a) { return 1 }\n\
       v vec[3,i32] = pass_vector(splat(3))\n\
       return zext[i32](v[0] == 3)\n\
       }\n"
  in
  List.iter
    (fun expected ->
      if not (contains internal_aggregate_abi expected) then
        failwith ("internal-aggregate-abi: missing `" ^ expected ^ "`"))
    [
      "call i1 @check_struct(ptr";
      "call i1 @check_array(ptr";
      "call <3 x i32> @pass_vector(<3 x i32>";
    ];
  let arity_messages =
    semantic_messages
      "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
       fn test() u64 { return id[3](2, 4) }\n"
  in
  (match arity_messages with
  | [ "function `id` expects 1 argument, got 2" ] -> ()
  | _ -> failwith "const-specialization-arity: wrong message");
  semantic_error "fas-008-i64-positive-overflow"
    "integer literal is out of range for i64"
    "const X i64 = 9223372036854775808\nfn f() i64 { return X }\n";
  semantic_error "fas-008-i64-negative-underflow"
    "integer literal is out of range for i64"
    "const X i64 = -9223372036854775809\nfn f() i64 { return X }\n";
  semantic_error "fas-020-positive-narrow-overflow"
    "integer literal is out of range for i32"
    "fn f() i32 { x i32 = 18446744073709551615\n return x }\n";
  semantic_error "fas-020-negative-narrow-overflow"
    "integer literal is out of range for i32"
    "fn f() i32 { x i32 = -18446744073709551615\n return x }\n";
  semantic_error "fas-020-hex-narrow-overflow" "integer literal is out of range for i32"
    "fn f() i32 { x i32 = 0xffffffffffffffff\n return x }\n";
  let u64_max = llvm_of "fn f() u64 { return 18446744073709551615 }\n" in
  if not (contains u64_max "ret i64 -1\n") then
    failwith "fas-020: valid u64 maximum literal was rejected";
  let digit_separator_hir =
    expect_ok
      (Parser.parse
         (source
            "const Decimal u64 = 1_000\n\
             const Hex u64 = 0x0100_0193\n\
             const Binary u64 = 0b1010_0000\n"))
    |> Sema.check |> expect_ok
  in
  List.iter
    (fun (name, expected) ->
      match
        List.find_opt
          (fun (constant : Hir.const_def) -> constant.name = name)
          digit_separator_hir.consts
      with
      | Some constant when constant.bits = expected -> ()
      | Some constant ->
          failwith
            (Printf.sprintf "integer-separator-%s: expected %Ld, got %Ld" name expected
               constant.bits)
      | None -> failwith ("integer-separator-" ^ name ^ ": constant was not emitted"))
    [ ("Decimal", 1000L); ("Hex", 16777619L); ("Binary", 160L) ];
  let signed_const_eval =
    llvm_of
      "const X i8 = -4 / 2\n\
       const R i8 = -5 % 2\n\
       const A i8 = -4 >> 1\n\
       const B bool = -1 < 0\n\
       fn main() i32 { return sext[i32](X) + sext[i32](R) + sext[i32](A) + \
       sext[i32](B) }\n"
  in
  if
    (not (contains signed_const_eval "sext i8 254 to i32"))
    || (not (contains signed_const_eval "sext i8 255 to i32"))
    || not (contains signed_const_eval "sext i1 true to i32")
  then failwith "fas-001: signed constant evaluation used masked bit patterns";
  let nested_align =
    llvm_of
      "struct Inner @align(16) { x u8 y u8 }\n\
       struct Outer { p u8 i Inner }\n\
       fn main() i32 {\n\
       a arr[2,Outer]\n\
       a[0].i.y = 7\n\
       a[1].p = 9\n\
       return zext[i32](a[0].i.y)\n\
       }\n"
  in
  if
    (not
       (contains nested_align
          "%struct.Inner = type { [0 x <16 x i8>], i8, i8, [14 x i8] }"))
    || not
         (contains nested_align "%struct.Outer = type { i8, [15 x i8], %struct.Inner }")
  then failwith "fas-003: nested aligned struct layout lost internal padding";
  semantic_error "fas-003-excessive-alignment"
    "alignment exceeds target maximum of 2147483648"
    "struct S @align(4294967296) { x u8 }\n";
  semantic_error "fas-009-duplicate-opaque" "duplicate type `x`"
    "opaque x\nopaque x\nfn main() i32 { return 0 }\n";
  let neutral_named_type =
    expect_ok
      (Parser.parse
         (source "fn check_case(value handle[Handle]) i64 { return 0 }\nopaque Handle\n"))
  in
  (match neutral_named_type.Ast.items with
  | Ast.Func { params = [ { ty = Ast.Handle (Ast.Named_type ("Handle", _)); _ } ]; _ }
    :: _ ->
      ()
  | _ -> failwith "named-type-neutral-ast: parser classified a declaration name");
  ignore
    (lower_of "fn check_case(value handle[Handle]) i64 { return 0 }\nopaque Handle\n");
  let forward_struct =
    llvm_of
      "fn make() i64 { value Pair = {7, 9}\n\
      \ return value.right }\n\
       struct Pair { left i64 right i64 }\n"
  in
  if not (contains forward_struct "%struct.Pair = type { i64, i64 }") then
    failwith "forward-struct: declaration was not resolved before function checking";
  let use_file =
    ( "use.fas",
      "fn read(object handle[Handle]) i64 { value Pair = {11}\n return value.x }\n" )
  in
  let declarations_file = ("types.fas", "opaque Handle\nstruct Pair { x i64 }\n") in
  List.iter
    (fun files -> ignore (expect_ok (check_files files) |> Lower.lower |> expect_ok))
    [ [ use_file; declarations_file ]; [ declarations_file; use_file ] ];
  semantic_error "unknown-named-type" "unknown type `Missing`"
    "fn check_case(value handle[Missing]) i64 { return 0 }\n";
  parse_message "compound-literal-opaque"
    "Fas has no compound literals; declare a typed local or `const`"
    "opaque Handle\nfn check_case() i64 { (Handle){}\n return 0 }\n";
  ignore
    (lower_of
       "fn preserve(value handle[Handle]) handle[Handle] { local handle[Handle] = value\n\
       \ return local }\n\
        fn read_only(value handle[Handle]) addr { return handle_addr(value) }\n\
        fn pointer_size() usize { return sizeof[handle[Handle]] }\n\
        fn pointer_align() usize { return alignof[addr] }\n\
        opaque Handle\n");
  semantic_error "opaque-local-by-value"
    "opaque type `Handle` can only be held as `handle[Handle]`"
    "opaque Handle\nfn check_case() void { value Handle }\n";
  let opaque_struct_field = "opaque Handle\nstruct Wrapper { value Handle }\n" in
  semantic_pin "opaque-struct-field-by-value" opaque_struct_field 2
    (String.length "struct Wrapper { value " + 1)
    6 "opaque type `Handle` has no layout" (Some "write `handle[Handle]`");
  semantic_accept "opaque-struct-field-handle-twin"
    "opaque Handle\nstruct Wrapper { value handle[Handle] }\n";
  let opaque_array = "opaque Handle\nfn check_case() void { values arr[2,Handle] }\n" in
  let opaque_array_line = "fn check_case() void { values arr[2,Handle] }" in
  semantic_pin "opaque-array-by-value" opaque_array 2
    (String.index_from opaque_array_line (String.index opaque_array_line '[' + 1) 'H'
    + 1)
    6 "opaque type `Handle` can only be held as `handle[Handle]`" None;
  let opaque_vector =
    "opaque Handle\nfn check_case() void { values vec[2,Handle] }\n"
  in
  let opaque_vector_line = "fn check_case() void { values vec[2,Handle] }" in
  semantic_pin "opaque-vector-by-value" opaque_vector 2
    (String.index_from opaque_vector_line (String.index opaque_vector_line '[' + 1) 'H'
    + 1)
    6 "vector element type must be `bool` or an integer type" None;
  semantic_error "opaque-parameter-by-value"
    "opaque type `Handle` can only be held as `handle[Handle]`"
    "opaque Handle\nfn check_case(value Handle) void { return }\n";
  semantic_error "opaque-return-by-value"
    "opaque type `Handle` can only be held as `handle[Handle]`"
    "opaque Handle\nfn check_case() Handle { }\n";
  let opaque_sizeof =
    "opaque Handle\nfn check_case() usize { return sizeof[Handle] }\n"
  in
  let opaque_sizeof_line = "fn check_case() usize { return sizeof[Handle] }" in
  semantic_pin "opaque-sizeof" opaque_sizeof 2
    (String.index_from opaque_sizeof_line (String.index opaque_sizeof_line '[' + 1) 'H'
    + 1)
    6 "opaque type `Handle` has no layout" (Some "write `handle[Handle]`");
  let opaque_alignof =
    "opaque Handle\nfn check_case() usize { return alignof[Handle] }\n"
  in
  let opaque_alignof_line = "fn check_case() usize { return alignof[Handle] }" in
  semantic_pin "opaque-alignof" opaque_alignof 2
    (String.index_from opaque_alignof_line
       (String.index opaque_alignof_line '[' + 1)
       'H'
    + 1)
    6 "opaque type `Handle` has no layout" (Some "write `handle[Handle]`");
  semantic_error "opaque-implicit-erasure" "is `handle[Handle]`, expected `addr`"
    "opaque Handle\nfn check_case(value handle[Handle]) addr { return value }\n";
  ignore
    (lower_of
       "fn accept(value addr) void { return }\n\
        fn return_read_only(value addr) addr { return value }\n\
        fn choose(flag bool, mutable addr, read_only addr) addr {\n\
       \ return if flag { mutable } else { read_only }\n\
        }\n\
        fn check_case(value addr) bool {\n\
       \ read_only addr = value\n\
       \ accept(value)\n\
       \ read_only = value\n\
       \ return value == read_only\n\
        }\n");
  semantic_error "pointer-implicit-to-integer"
    "addresses convert to integers with `addr_bits`"
    "fn check_case(value addr) void { bits usize = value }\n";
  semantic_error "integer-implicit-to-pointer"
    "integers convert to addresses with `addr_from_bits`"
    "fn check_case(value usize) void { pointer addr = value }\n";
  semantic_error "pointer-bitcast-discards-const"
    "illegal cast for source and destination widths"
    "fn check_case(value addr) addr { return bitcast[addr](value) }\n";
  semantic_error "pointer-bitcast-u32-width"
    "illegal cast for source and destination widths"
    "fn check_case(value addr) u32 { return bitcast[u32](value) }\n";
  semantic_error "pointer-bitcast-i32-width"
    "illegal cast for source and destination widths"
    "fn check_case(value i32) addr { return bitcast[addr](value) }\n";
  semantic_error "const-pointer-bitcast-u32-width"
    "illegal cast for source and destination widths"
    "fn check_case(value addr) u32 { return bitcast[u32](value) }\n";
  semantic_error "integer-bitcast-const-pointer-u32-width"
    "illegal cast for source and destination widths"
    "fn check_case(value u32) addr { return bitcast[addr](value) }\n";
  let before_messages =
    semantic_messages
      "struct Stable { x i64 }\nfn check_case(value Missing) i64 { return 0 }\n"
  in
  let after_messages =
    semantic_messages
      "fn check_case(value Missing) i64 { return 0 }\nstruct Stable { x i64 }\n"
  in
  if before_messages <> after_messages then
    failwith "named-type-order-diagnostic: declaration reordering changed diagnostics";
  ignore
    (lower_of
       "fn test() u64 { values arr[2,u64]\n\
       \ values[0] = id[3](4)\n\
       \ values[1] = 5\n\
       \ return values[0] }\n\
        fn id[N const usize](value u64) u64 { return value + bitcast[u64](N) }\n");
  ignore
    (lower_of "fn choose(value bool) i64 { if (value){ return 1 } else { return 0 } }\n");
  ignore
    (lower_of
       "fn controls(value i64, condition bool) i64 {\n\
        while (condition){ break }\n\
        switch (value){ case 0: { return 0 } default: { } }\n\
        for ; condition; { break }\n\
        return value\n\
        }\n");
  semantic_error "fas-005-void-if-expression"
    "if-expression branches cannot have void type"
    "fn a() void { }\n\
     fn b() void { }\n\
     fn main() i32 {\n\
    \  c bool = true\n\
    \  value i32 = if c { a() } else { b() }\n\
    \  return 0\n\
     }\n";
  parse_error "fas-006-noalias-parameter" "fn f(x noalias addr) i32 { return 0 }\n";
  parse_error "fas-007-aligned-parameter" "fn f(x aligned[16] addr) i32 { return 0 }\n";
  let explicit_pointer_cast =
    llvm_of
      "fn take(p addr) i32 { return 0 }\n\
       fn main() i32 { x i64 = 1\n\
      \ return take(addr_from_bits(addr_bits(&x))) }\n"
  in
  if not (contains explicit_pointer_cast "call i32 @take(ptr") then
    failwith "fas-028: explicit pointer conversion was rejected";
  List.iter
    (fun (name, source) ->
      parse_error_message
        ("forbidden-attribute-" ^ name)
        ("unknown attribute `@" ^ name ^ "`")
        source)
    [
      ("inline", "@inline\nfn f() i64 { return 7 }\n");
      ("noinline", "@noinline\nfn f() i64 { return 7 }\n");
      ("kernel", "@kernel\nfn f() i64 { return 7 }\n");
      ("optimize", "@optimize\nfn f() i64 { return 7 }\n");
      ("target", "@target(\"zen3\")\nfn f() i64 { return 7 }\n");
      ("expect_asm", "@expect_asm(\"add\")\nfn f() i64 { return 7 }\n");
      ("expect_no_call", "@expect_no_call\nfn f() i64 { return 7 }\n");
      ("expect_stack_max", "@expect_stack_max(\"16\")\nfn f() i64 { return 7 }\n");
      ("unroll", "@unroll(8)\nfn f() i64 { return 7 }\n");
      ("vector_width", "@vector_width(4)\nfn f() i64 { return 7 }\n");
      ("hot", "@hot\nfn f() i64 { return 7 }\n");
      ("cold", "@cold\nfn f() i64 { return 7 }\n");
    ];
  parse_message "align-restricted-to-structs" "attribute `@align` applies to structs"
    "@align(16)\nfn f() i64 { return 7 }\n";
  syntax_pin "align-attribute-span" "@align(16)\nfn f() i64 { return 7 }\n" 1 1 6
    "attribute `@align` applies to structs";
  let misplaced_align_source = "@align(16)\nstruct P { x u8 }\n" in
  (match parse_diagnostics "align-attribute-placement" misplaced_align_source with
  | [ diagnostic ]
    when diagnostic.Diag.primary.Span.line = 1
         && diagnostic.primary.Span.column = 1
         && diagnostic.primary.Span.end_offset - diagnostic.primary.Span.start_offset
            = 6
         && diagnostic.message = "attribute `@align` applies to structs"
         && diagnostic.help = Some "write `struct P @align(16) {`" ->
      ()
  | diagnostics ->
      failwith
        ("align-attribute-placement: unexpected diagnostic: "
        ^ Diag.render_all ~source:(Some (source misplaced_align_source)) diagnostics));
  semantic_accept "align-attribute-after-struct-name" "struct P @align(16) { x u8 }\n";
  parse_error_message "struct-rejects-function-attribute" "unknown attribute `@inline`"
    "struct S @inline { x i64 }\n";
  let attribute_free_ir =
    llvm_of
      "fn id[N const i64](x i64) i64 { return x }\nfn test() i64 { return id[3](4) }\n"
  in
  List.iter
    (fun marker ->
      if contains attribute_free_ir marker then
        failwith ("optimizer attribute leaked into LLVM: " ^ marker))
    [
      "inlinehint";
      "noinline";
      "optnone";
      "\"target-cpu\"";
      "\"target-features\"";
      "noalias";
      "noundef align";
    ];
  let cli_profile = cli_run [ "-g"; "--no-inline"; "helper"; "profile.fas" ] in
  assert (cli_profile.Cli.no_inline_functions = [ "helper" ]);
  let cli_sanitizers =
    cli_run
      [
        "--sanitize=undefined,address,address";
        "--sanitize=address,undefined";
        "profile.fas";
      ]
  in
  assert (cli_sanitizers.Cli.sanitizers = [ "address"; "undefined" ]);
  let cli_profile_ordered =
    cli_run [ "-O3"; "-g"; "--no-inline"; "helper"; "profile.fas" ]
  in
  assert (cli_profile_ordered.Cli.optimization = 3);
  cli_error "no-inline-missing-name" "requires a function name" [ "-g"; "--no-inline" ];
  cli_error "no-inline-flag-name" "requires a function name"
    [ "-g"; "--no-inline"; "-O3"; "profile.fas" ];
  let cli_no_inline_without_g = cli_run [ "--no-inline"; "helper"; "profile.fas" ] in
  assert (cli_no_inline_without_g.Cli.no_inline_functions = [ "helper" ]);
  assert (cli_no_inline_without_g.Cli.optimization = 2);
  assert (
    (cli_run [ "--emit-llvm=optimized"; "profile.fas" ]).Cli.emit = Cli.Optimized_llvm);
  cli_error "emit-llvm-unknown-value" "accepted value is `optimized`"
    [ "--emit-llvm=bogus"; "profile.fas" ];
  cli_error "removed-debug-option" "unknown option `-debug`; write `-g`"
    [ "-debug"; "profile.fas" ];
  cli_error "removed-no-inline-option"
    "unknown option `-no-inline`; write `--no-inline`"
    [ "-no-inline"; "helper"; "profile.fas" ];
  assert (
    (cli_run [ "-g"; "--no-inline"; "helper"; "--no-inline"; "other"; "profile.fas" ])
      .Cli.no_inline_functions = [ "helper"; "other" ]);
  assert (
    (cli_run [ "--emit-llvm=optimized"; "--profile"; "profile.fasprof"; "profile.fas" ])
      .Cli.profile = Some "profile.fasprof");
  assert (
    (cli_run [ "--profile"; "profile.fasprof"; "profile.fas" ]).Cli.profile
    = Some "profile.fasprof");
  cli_error "profile-emit-llvm"
    "`--profile` has no effect with `--emit-llvm`; write `--emit-llvm=optimized`"
    [ "--emit-llvm"; "--profile"; "profile.fasprof"; "profile.fas" ];
  cli_error "profile-emit-header" "`--profile` has no effect with `--emit-header`"
    [ "--emit-header"; "--profile"; "profile.fasprof"; "profile.fas" ];
  cli_error "profile-emit-ir" "`--profile` has no effect with `--emit-ir`"
    [ "--emit-ir"; "--profile"; "profile.fasprof"; "profile.fas" ];
  cli_error "profile-emit-ir-json" "`--profile` has no effect with `--emit-ir-json`"
    [ "--emit-ir-json"; "--profile"; "profile.fasprof"; "profile.fas" ];
  cli_error "profile-check" "`--profile` has no effect with `fas check`"
    [ "check"; "--profile"; "profile.fasprof"; "profile.fas" ];
  cli_error "profile-lsp" "`--profile` has no effect with `fas lsp`"
    [ "lsp"; "--profile"; "profile.fasprof" ];
  cli_error "profile-duplicate" "duplicate --profile option"
    [ "--profile"; "a.fasprof"; "--profile"; "b.fasprof"; "profile.fas" ];
  cli_error "profile-missing-value" "--profile requires a profile file" [ "--profile" ];
  (match
     Cli.parse [| "fas"; "run"; "--profile"; "profile.fasprof"; "profile.fas" |]
   with
  | Ok (Cli.Run_program (config, _)) ->
      assert (config.Cli.profile = Some "profile.fasprof")
  | _ -> failwith "run-profile: expected program invocation");
  (match
     Cli.parse [| "fas"; "perf"; "--profile"; "profile.fasprof"; "profile.fas" |]
   with
  | Ok (Cli.Perf (config, _, _)) -> assert (config.Cli.profile = Some "profile.fasprof")
  | _ -> failwith "perf-profile: expected perf invocation");
  (match Cli.parse [| "fas"; "perf"; "--tune"; "profile.fas" |] with
  | Ok (Cli.Perf (_, options, _)) -> assert (options.Cli.mode = Cli.Tune None)
  | _ -> failwith "perf-tune: expected perf invocation");
  (match Cli.parse [| "fas"; "perf"; "--tune"; "--no-arena"; "profile.fas" |] with
  | Ok (Cli.Perf (_, options, _)) ->
      assert (options.Cli.no_arena && options.Cli.mode = Cli.Tune None)
  | _ -> failwith "perf-tune-no-arena: expected perf invocation");
  (match Cli.parse [| "fas"; "perf"; "--tune"; "profile.fas" |] with
  | Ok (Cli.Perf (_, options, _)) -> assert (not options.Cli.no_arena)
  | _ -> failwith "perf-tune-arena-default: expected perf invocation");
  cli_error "no-arena-without-tune" "`--no-arena` needs `--tune`"
    [ "perf"; "--no-arena"; "profile.fas" ];
  (match Cli.parse [| "fas"; "perf"; "--tune"; "profile.fas"; "--yes" |] with
  | Ok (Cli.Perf (_, options, _)) ->
      assert (options.Cli.yes && not options.Cli.no_arena)
  | _ -> failwith "perf-tune-yes: expected perf invocation");
  (match Cli.parse [| "fas"; "perf"; "--tune"; "profile.fas" |] with
  | Ok (Cli.Perf (_, options, _)) -> assert (not options.Cli.yes)
  | _ -> failwith "perf-tune-yes-default: expected perf invocation");
  cli_error "yes-without-tune" "`--yes` needs `--tune`"
    [ "perf"; "--yes"; "profile.fas" ];
  let check_with name argv expected_args expected_second =
    incr checks_run;
    match
      Cli.parse (Array.of_list ("fas" :: "perf" :: "--tune" :: "profile.fas" :: argv))
    with
    | Ok (Cli.Perf (_, options, args)) ->
        if args <> expected_args || options.Cli.check_with <> expected_second then
          failwith (name ^ ": wrong split")
    | _ -> failwith (name ^ ": expected perf invocation")
  in
  check_with "check-with-after-args"
    [ "--"; "a"; "b"; "--check-with"; "--"; "c" ]
    [ "a"; "b" ] (Some [ "c" ]);
  check_with "check-with-no-args"
    [ "--check-with"; "--"; "c"; "d" ]
    []
    (Some [ "c"; "d" ]);
  check_with "check-with-empty" [ "--"; "a"; "--check-with"; "--" ] [ "a" ] (Some []);
  check_with "check-with-not-separator"
    [ "--"; "a"; "--check-with"; "b" ]
    [ "a"; "--check-with"; "b" ] None;
  check_with "check-with-first-split"
    [ "--"; "a"; "--check-with"; "--"; "b"; "--check-with"; "--"; "c" ]
    [ "a" ]
    (Some [ "b"; "--check-with"; "--"; "c" ]);
  check_with "check-with-before-args"
    [ "--check-with"; "--"; "a"; "--check-with"; "--"; "b" ]
    []
    (Some [ "a"; "--check-with"; "--"; "b" ]);
  check_with "check-with-absent" [ "--"; "a" ] [ "a" ] None;
  cli_error "check-with-value"
    "`--check-with` must be followed by `--` and the second input's arguments"
    [ "perf"; "--tune"; "profile.fas"; "--check-with"; "x" ];
  cli_error "check-with-last"
    "`--check-with` must be followed by `--` and the second input's arguments"
    [ "perf"; "--tune"; "profile.fas"; "--check-with" ];
  cli_error "check-with-without-tune" "`--check-with` needs `--tune`"
    [ "perf"; "profile.fas"; "--check-with"; "--"; "x" ];
  (match
     Cli.parse
       [| "fas"; "perf"; "--tune"; "profile.fas"; "-o"; "out.fasprof"; "--"; "x" |]
   with
  | Ok (Cli.Perf (_, options, args)) ->
      assert (options.Cli.mode = Cli.Tune (Some "out.fasprof"));
      assert (args = [ "x" ])
  | _ -> failwith "perf-tune-output: expected perf invocation");
  cli_error "tune-static" "`--tune` cannot be combined with `--static`"
    [ "perf"; "--tune"; "--static"; "profile.fas" ];
  cli_error "tune-json" "`--tune` cannot be combined with `--json`"
    [ "perf"; "--tune"; "--json"; "profile.fas" ];
  cli_error "tune-compare" "`--tune` cannot be combined with `--compare`"
    [ "perf"; "--tune"; "--compare"; "a.fasprof"; "profile.fas" ];
  cli_error "tune-top" "`--tune` cannot be combined with `--top`"
    [ "perf"; "--tune"; "--top"; "3"; "profile.fas" ];
  cli_error "tune-asm" "`--tune` cannot be combined with `--asm`"
    [ "perf"; "--tune"; "--asm"; "profile.fas:1"; "profile.fas" ];
  cli_error "tune-profile" "`--tune` cannot be combined with `--profile`"
    [ "perf"; "--tune"; "--profile"; "a.fasprof"; "profile.fas" ];
  cli_error "tune-duplicate-output" "duplicate -o option"
    [ "perf"; "--tune"; "-o"; "a.fasprof"; "-o"; "b.fasprof"; "profile.fas" ];
  cli_error "tune-stdout-output" "-o - is not supported"
    [ "perf"; "--tune"; "-o"; "-"; "profile.fas" ];
  cli_error "tune-missing-output" "-o requires an output path"
    [ "perf"; "--tune"; "profile.fas"; "-o" ];
  let profile_source = Filename.temp_file "fas-profile-pin-" ".fas" in
  let profile_file = Filename.temp_file "fas-profile-pin-" ".fasprof" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove profile_source;
      Sys.remove profile_file)
    (fun () ->
      let write path text =
        let channel = open_out_bin path in
        Fun.protect
          ~finally:(fun () -> close_out channel)
          (fun () -> output_string channel text)
      in
      write profile_source "fn main() i32 { return 0 }\n";
      let profile_rejection ?(options = []) name path fragment =
        incr checks_run;
        match
          Driver.run (cli_run (options @ [ "--profile"; path; profile_source ]))
        with
        | Error diagnostics ->
            let rendered = Diag.render_all ~source:None diagnostics in
            if not (contains rendered fragment) then
              failwith (name ^ ": unexpected diagnostic: " ^ rendered)
        | Ok _ -> failwith (name ^ ": expected profile rejection")
      in
      profile_rejection "profile-unreadable" (profile_file ^ ".missing")
        "cannot read profile";
      write profile_file "fas profile 4\nfas 0.5.2\nllvm x\ncpu x\ncounts\n";
      profile_rejection "profile-bad-header" profile_file
        "is not a fas profile (expected `fas profile 1`, `fas profile 2` or `fas \
         profile 3` on line 1)";
      write profile_file "fas profile 1\nfas 0.5.2\nllvm x\ncpu x\nno counts here\n";
      profile_rejection "profile-no-counts" profile_file "has no `counts` section";
      write profile_file "fas profile 1\ncounts\n";
      profile_rejection "profile-early-counts" profile_file "has no `counts` section";
      write profile_file
        "fas profile 2\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice fast\n\
         align-loops def\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-unknown-choice" profile_file
        "has an unknown choice `fast`; expected default, pgo, native, native-pgo, \
         mtune or mtune-pgo";
      write profile_file
        "fas profile 2\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops 128\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-unknown-align" profile_file
        "has an unknown align-loops value `128`; expected def, 32 or 64";
      write profile_file
        "fas profile 2\nfas 0.5.2\nllvm x\ncpu x\nchoice pgo\nalign-loops def\ncounts\n";
      profile_rejection "profile-no-measured" profile_file
        "line 7 must be `measured RATIO band BAND rounds R placements K metric M`";
      write profile_file
        "fas profile 3\nfas 0.5.2\nllvm x\ncpu x\nchoice pgo\nalign-loops def\ncounts\n";
      profile_rejection "profile-no-align-functions" profile_file
        "line 7 must be `align-functions def|64`";
      write profile_file
        "fas profile 3\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions 32\n\
         options\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-unknown-align-functions" profile_file
        "has an unknown align-functions value `32`; expected def or 64";
      write profile_file
        "fas profile 3\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions def\n\
         counts\n";
      profile_rejection "profile-no-options" profile_file
        "line 8 must be `options [unroll=600] [unroll=1200] [unroll=2400] \
         [unroll=4800] [O3] [inline=1000] [interleave=4] [max-bandwidth]`";
      write profile_file
        "fas profile 3\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions def\n\
         options unroll=1200 fast\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-unknown-option" profile_file
        "has an unknown option `fast`; expected unroll=600, unroll=1200, unroll=2400, \
         unroll=4800, O3, inline=1000, interleave=4 or max-bandwidth";
      write profile_file
        "fas profile 3\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions def\n\
         options unroll=1200 unroll=1200\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-repeated-option" profile_file
        "repeats the option `unroll=1200`";
      write profile_file
        "fas profile 3\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions def\n\
         options inline=1000 inline=1000\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-repeated-inline" profile_file
        "repeats the option `inline=1000`";
      write profile_file
        "fas profile 3\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions def\n\
         options unroll=600 unroll=1200\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-two-unroll-values" profile_file "has two unroll values";
      write profile_file
        "fas profile 3\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions def\n\
         options unroll\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-bare-unroll" profile_file
        "has an unknown option `unroll`; expected unroll=600, unroll=1200, \
         unroll=2400, unroll=4800, O3, inline=1000, interleave=4 or max-bandwidth";
      write profile_file
        "fas profile 3\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions def\n\
         options unroll=3\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-unknown-unroll-value" profile_file
        "has an unknown option `unroll=3`; expected unroll=600, unroll=1200, \
         unroll=2400, unroll=4800, O3, inline=1000, interleave=4 or max-bandwidth";
      write profile_file
        "fas profile 3\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions def\n\
         options\n\
         counts\n";
      profile_rejection "profile-no-measured-3" profile_file
        "line 9 must be `measured RATIO band BAND rounds R placements K metric M`";
      write profile_file "fas profile 1\nfas 0.5.2\nllvm x\ncpu x\ncounts\n";
      profile_rejection "profile-empty-counts" profile_file
        "has no counts for a pgo choice";
      write profile_file
        "fas profile 2\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         measured 1\n\
         counts\n\n\
        \   \n";
      profile_rejection "profile-blank-counts" profile_file
        "has no counts for a pgo choice";
      write profile_file
        "fas profile 2\n\
         fas 0.5.2\n\
         llvm x\n\
         host x\n\
         choice pgo\n\
         align-loops def\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-cpu-line" profile_file "line 4 must be `cpu NAME`";
      write profile_file
        "fas profile 2\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         measured 1\n\
         bogus\n";
      profile_rejection "profile-counts-line" profile_file "line 8 must be `counts`";
      write profile_file
        "fas profile 3\n\
         fas 0.6\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops def\n\
         align-functions def\n\
         options\n\
         measured 1\n";
      profile_rejection "profile-counts-line-missing" profile_file
        "line 10 must be `counts`";
      write profile_file
        "fas profile 2\r\n\
         fas 0.5.2\r\n\
         llvm x\r\n\
         cpu x\r\n\
         choice fast\r\n\
         align-loops def\r\n\
         measured 1\r\n\
         counts\r\n";
      profile_rejection "profile-crlf-header" profile_file
        "has an unknown choice `fast`; expected default, pgo, native, native-pgo, \
         mtune or mtune-pgo";
      write profile_file
        "fas profile 2\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice pgo\n\
         align-loops\t128\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-tab-header" profile_file
        "has an unknown align-loops value `128`; expected def, 32 or 64";
      write profile_file
        "fas profile 2\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice native-pgo\n\
         align-loops def\n\
         measured 1\n\
         counts\n";
      profile_rejection ~options:[ "--target=portable" ] "profile-native-portable"
        profile_file
        "has the native choice `native-pgo`; it cannot build with `--target=portable`";
      write profile_file
        "fas profile 2\n\
         fas 0.5.2\n\
         llvm x\n\
         cpu x\n\
         choice native\n\
         align-loops def\n\
         measured 1\n\
         counts\n";
      profile_rejection "profile-native-host-cpu" profile_file
        "was tuned for the CPU `x`";
      profile_rejection ~options:[ "--target=portable" ] "profile-native-portable-plain"
        profile_file
        "has the native choice `native`; it cannot build with `--target=portable`");
  cli_error "sanitize-empty-list" "--sanitize requires a non-empty list"
    [ "--sanitize="; "profile.fas" ];
  cli_error "sanitize-unknown-name" "unknown sanitizer `memory`"
    [ "--sanitize=memory"; "profile.fas" ];
  cli_error "sanitize-conflicting-repeat" "conflicting --sanitize options"
    [ "--sanitize=address"; "--sanitize=undefined"; "profile.fas" ];
  cli_error "removed-ast-output" "unknown option: --emit-ast"
    [ "--emit-ast"; "profile.fas" ];
  cli_error "removed-release-option" "unknown option: -release"
    [ "-release"; "profile.fas" ];
  cli_error "removed-kernel-option" "unknown option: -kernel"
    [ "-kernel"; "profile.fas" ];
  cli_error "link-input-needs-executable"
    "C inputs and link flags require an executable output"
    [ "--emit-llvm"; "profile.fas"; "helper.c" ];
  let profile_path = Filename.temp_file "fas-profile-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove profile_path)
    (fun () ->
      let channel = open_out_bin profile_path in
      output_string channel
        "fn helper() i64 { return 3 }\n\
         fn other() i64 { return 4 }\n\
         fn test() i64 { return helper() }\n";
      close_out channel;
      let config =
        cli_run [ "-g"; "-O3"; "--emit-llvm"; "--no-inline"; "helper"; profile_path ]
      in
      assert (config.Cli.optimization = 3);
      let profile_ir =
        match Driver.run config with
        | Ok output -> output
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      in
      if not (contains profile_ir "@helper() noinline #0 ") then
        failwith "no-inline: selected function missing LLVM attribute";
      if contains profile_ir "@other() noinline #0 " then
        failwith "no-inline: attribute leaked to another function";
      let missing_config =
        cli_run [ "-g"; "--emit-llvm"; "--no-inline"; "missing"; profile_path ]
      in
      match Driver.run missing_config with
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          if not (contains rendered "function `missing` was not emitted") then
            failwith "no-inline: missing function diagnostic changed"
      | Ok _ -> failwith "no-inline: missing function was accepted");
  let generic_profile_source =
    "fn check_case() i64 { return identity[i64](7) }\n\
     fn identity[T](value T) T { return value }\n"
  in
  let generic_profile_hir =
    expect_ok (Parser.parse (source generic_profile_source)) |> Sema.check |> expect_ok
  in
  let generic_profile_name =
    match
      List.find_opt
        (fun (func : Hir.func) -> contains func.name "identity$spec$")
        generic_profile_hir.Hir.funcs
    with
    | Some func -> func.name
    | None -> failwith "no-inline: generic specialization was not emitted"
  in
  let generic_profile_path = Filename.temp_file "fas-profile-generic-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove generic_profile_path)
    (fun () ->
      let channel = open_out_bin generic_profile_path in
      output_string channel generic_profile_source;
      close_out channel;
      let config =
        cli_run
          [
            "-g";
            "-O3";
            "--emit-llvm";
            "--no-inline";
            generic_profile_name;
            generic_profile_path;
          ]
      in
      let output =
        match Driver.run config with
        | Ok output -> output
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      in
      if
        not
          (contains output (Ir.quote_identifier generic_profile_name)
          && contains output "noinline #0 ")
      then failwith "no-inline: generic specialization was not selected");
  let external_profile_path = Filename.temp_file "fas-profile-external-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove external_profile_path)
    (fun () ->
      let channel = open_out_bin external_profile_path in
      output_string channel
        "extern \"C\" { fn external() i64 }\nfn test() i64 { return 0 }\n";
      close_out channel;
      let config =
        cli_run
          [ "-g"; "--emit-llvm"; "--no-inline"; "external"; external_profile_path ]
      in
      match Driver.run config with
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          if not (contains rendered "not a normal definition") then
            failwith "no-inline: external declaration diagnostic changed"
      | Ok _ -> failwith "no-inline: external declaration was accepted");
  let display_source_path = Filename.temp_file "fas-display-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove display_source_path)
    (fun () ->
      let channel = open_out_bin display_source_path in
      output_string channel
        (String.concat "\n"
           [
             "fn sum[T](data addr, n usize) T {";
             "    total T = 0";
             "    for i usize = 0; i < n; i += 1 {";
             "        total += data[T, i]";
             "    }";
             "    return total";
             "}";
             "fn main() i32 {";
             "    s addr = \"abcd\"";
             "    return trunc[i32](sum[u64](s, 0))";
             "}";
             "";
           ]);
      close_out channel;
      let output =
        match Driver.run (cli_run [ "-g"; "--emit-llvm"; display_source_path ]) with
        | Ok output -> output
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      in
      if
        not
          (contains output
             "DISubprogram(name: \"sum[u64]\", linkageName: \"sum$spec$3_u64\", ")
      then failwith "debug names: generic specialization display pair changed");
  let budget_source_path = Filename.temp_file "fas-budget-source-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove budget_source_path)
    (fun () ->
      let channel = open_out_bin budget_source_path in
      output_string channel "fn f[K const u64, D const u64](x u64) u64 {\n";
      for i = 0 to 45 do
        output_string channel
          (Printf.sprintf "  x = x * 6364136223846793005 + K + %d\n" i)
      done;
      output_string channel
        "  if D == 0 { return x }\n\
        \  return f[2 * K, D - 1](x) + f[2 * K + 1, D - 1](x)\n\
         }\n\
         fn main() i32 { return bitcast[i32](trunc[u32](f[1, 11](3))) }\n";
      close_out channel;
      let expect_budget_rejection config label =
        match Driver.run config with
        | Error diagnostics ->
            let rendered = Diag.render_all ~source:None diagnostics in
            if
              not
                (contains rendered
                   "rendered LLVM text exceeds the configured limit of 32000000 bytes")
            then failwith (label ^ ": unexpected diagnostic: " ^ rendered)
        | Ok _ -> failwith (label ^ ": oversized render was accepted")
      in
      let llvm_output_path = Filename.temp_file "fas-budget-output-" ".ll" in
      Sys.remove llvm_output_path;
      Fun.protect
        ~finally:(fun () ->
          if Sys.file_exists llvm_output_path then Sys.remove llvm_output_path)
        (fun () ->
          let config =
            cli_run [ "--emit-llvm"; "-o"; llvm_output_path; budget_source_path ]
          in
          expect_budget_rejection config "budget";
          if Sys.file_exists llvm_output_path then
            failwith "budget: rejected emission wrote output");
      let asm_output_path = Filename.temp_file "fas-budget-asm-" ".s" in
      Sys.remove asm_output_path;
      Fun.protect
        ~finally:(fun () ->
          if Sys.file_exists asm_output_path then Sys.remove asm_output_path)
        (fun () ->
          let config = cli_run [ "-S"; "-o"; asm_output_path; budget_source_path ] in
          expect_budget_rejection config "budget asm";
          if Sys.file_exists asm_output_path then
            failwith "budget: rejected assembly emission wrote output"));
  semantic_error "fas-002-switch-default-init-leak" "use of uninitialized local `x`"
    "fn main() i32 {\n\
    \     x i32\n\
    \     switch 0 {\n\
    \       case 0: { }\n\
    \       default: { x = 5 }\n\
    \     }\n\
    \     return x\n\
    \   }\n";
  semantic_error "fas-002-switch-cross-arm-init-leak" "use of uninitialized local `x`"
    "fn main() i32 {\n\
    \     x i32\n\
    \     y i32 = 0\n\
    \     switch 1 {\n\
    \       case 0: { x = 7 }\n\
    \       case 1: { y = x }\n\
    \     }\n\
    \     return y\n\
    \   }\n";

  let guarded_div = llvm_of "fn div(x i64, y i64) i64 { return x / y }\n" in
  if
    (not (contains guarded_div "icmp eq i64"))
    || (not (contains guarded_div "call void @llvm.trap()"))
    || (not (contains guarded_div "-9223372036854775808"))
    || not (contains guarded_div "unreachable")
  then failwith "integer-div-trap: missing runtime divisor guard";
  let guarded_rem = llvm_of "fn rem(x i8, y i8) i8 { return x % y }\n" in
  if
    (not (contains guarded_rem "icmp eq i8"))
    || (not (contains guarded_rem "srem i8"))
    || not (contains guarded_rem "call void @llvm.trap()")
  then failwith "integer-rem-trap: missing runtime divisor guard";
  let vector_div =
    llvm_of "fn div(x vec[4,u32], y vec[4,u32]) vec[4,u32] { return x / y }\n"
  in
  if
    (not (contains vector_div "icmp eq <4 x i32>"))
    || (not (contains vector_div "@llvm.vector.reduce.or.v4i1"))
    || not (contains vector_div "udiv <4 x i32>")
  then failwith "integer-vector-div-trap: missing vector divisor guard";
  semantic_error "signed-div-constant-overflow"
    "signed division `-9223372036854775808 / -1` overflows `i64`"
    "const X i64 = -9223372036854775808 / -1\nfn test() i64 { return X }\n";
  let signed_rem_const =
    llvm_of "const X i64 = -9223372036854775808 % -1\nfn test() i64 { return X }\n"
  in
  if not (contains signed_rem_const "ret i64 0\n") then
    failwith "signed-rem-constant-overflow: expected zero remainder";

  semantic_error "fas-026-const-array-write" "cannot modify constant"
    "const K arr[2, i64] = {1, 2}\nfn main() i32 { K[0] = 9\n return 0 }\n";
  let const_array_address =
    llvm_of
      "const K arr[2,u32] = {4, 5}\n\
       fn pointer() addr { return &K }\n\
       fn test() u32 { return K[0] }\n"
  in
  if
    (not
       (contains const_array_address
          "@K = private unnamed_addr constant [2 x i32] [i32 4, i32 5]"))
    || not (contains const_array_address "getelementptr [2 x i32], ptr @K")
  then failwith "named-aggregate-constant-address: storage is not static and readonly";
  semantic_error "named-aggregate-constant-view-write" "cannot modify constant"
    "const K arr[2,u32] = {4, 5}\n\
     fn test() void { view values = K\n\
    \ values[0] = 8\n\
    \ return }\n";
  let number_head =
    "struct Cell { tag u8, weight u32 }\nconst CELLS arr[2, Cell] = {{1, 2}, {3, 4}}\n"
  in
  let number_tables_source =
    "struct Cell { tag u8, weight u32, scale f64, live bool, step u16 }\n\
     struct Row { id u8, cells arr[2, Cell] }\n\
     const CELLS arr[3, Cell] = {{1, 100, 1.5, true, 7}, {2, 200, 2.25, false, 9}, \
     {255, 4000000000, -0.5, true, 65535}}\n\
     const Grid arr[2, arr[3, i32]] = {{1, -2, 3}, {4, 5, -6}}\n\
     const Cube arr[2, arr[2, arr[2, u16]]] = {{{1, 2}, {3, 4}}, {{5, 6}, {7, 65535}}}\n\
     const Rows arr[2, Row] = {{9, {{1, 11, 2.0, true, 10}, {3, 33, 4.0, false, 20}}}, \
     {8, {{5, 55, 6.0, true, 30}, {7, 77, 8.0, true, 40}}}}\n\
     const One Cell = {42, 4242, 0.25, false, 1}\n\
     const Empty arr[3, Cell] = {}\n\
     const Named Cell = {.step = 3, .tag = 4, .weight = 5, .scale = 1.0, .live = true}\n\
     fn main() i32 { view row = Grid[1]\n\
     return row[2] }\n"
  in
  semantic_accept "const-number-tables-accept" number_tables_source;
  let number_llvm = llvm_of number_tables_source in
  if
    (not (contains number_llvm "@CELLS = private constant [72 x i8] c"))
    || not (contains number_llvm "@Grid = private constant [24 x i8] c")
  then failwith "const-number-tables-accept: storage is not static and readonly";
  semantic_accept "const-number-tables-len-const"
    (number_head ^ "const N usize = len(CELLS)\nfn main() i32 { return 0 }\n");
  semantic_accept "const-number-tables-len-var"
    (number_head ^ "var M usize = len(CELLS)\nfn main() i32 { return 0 }\n");
  semantic_accept "const-number-tables-len-size"
    (number_head ^ "fn main() i32 { buf arr[len(CELLS), i32] = {}\n return 0 }\n");
  semantic_accept "const-number-tables-view-bitcast"
    (number_head
   ^ "fn main() i32 { view c = CELLS[1]\n return bitcast[i32](c.weight) }\n");
  semantic_pin "const-number-tables-write"
    (number_head ^ "fn main() i32 { CELLS[0] = {1, 2}\n return 0 }\n")
    3 17 5 "cannot modify constant `CELLS`" None;
  semantic_pin "const-number-tables-write-field"
    (number_head ^ "fn main() i32 { CELLS[0].weight = 5\n return 0 }\n")
    3 17 8 "cannot modify constant `CELLS`" None;
  semantic_pin "const-number-tables-view-write"
    (number_head ^ "fn main() i32 { view c = CELLS[1]\n c.weight = 3\n return 0 }\n")
    4 2 1 "cannot modify constant `CELLS`" None;
  semantic_pin "const-number-tables-count"
    "struct Cell { tag u8, weight u32 }\n\
     const CELLS arr[3, Cell] = {{1, 2}, {3, 4}}\n\
     fn main() i32 { return 0 }\n"
    2 28 1 "array of 3 elements, got 2" None;
  semantic_pin "const-number-tables-range"
    "struct Cell { tag u8, weight u32 }\n\
     const CELLS arr[2, Cell] = {{1, 2}, {300, 4}}\n\
     fn main() i32 { return 0 }\n"
    2 38 3 "integer literal is out of range for u8: `300`" None;
  semantic_pin "const-number-tables-nonconstant"
    "struct Cell { tag u8, weight u32 }\n\
     var V u32 = 4\n\
     const CELLS arr[2, Cell] = {{1, 2}, {3, V}}\n\
     fn main() i32 { return 0 }\n"
    3 41 1 "global `V` is not a constant" None;
  semantic_pin "const-number-tables-slot-read"
    (number_head ^ "const X u32 = CELLS[0].weight\nfn main() i32 { return 0 }\n")
    3 15 15
    "global initializer must be a constant expression for `X`; `CELLS[0].weight` is \
     not constant"
    None;
  semantic_pin "const-number-tables-size-read"
    (number_head ^ "fn main() i32 { buf arr[CELLS[0].weight, i32] = {}\n return 0 }\n")
    3 25 15 "expression is not compile-time constant" None;
  semantic_pin "const-number-tables-whole-element"
    (number_head ^ "const ONE Cell = CELLS[0]\nfn main() i32 { return 0 }\n")
    3 18 8
    "global initializer must be a constant expression for `ONE`; `CELLS[0]` is not \
     constant"
    None;
  parse_error_message "const-number-tables-bare-slot" "expected a type, found `=`"
    (number_head ^ "const X = CELLS[0].weight\nfn main() i32 { return 0 }\n");
  let menu_head =
    "fn hit() i32 { return 1 }\n\
     struct Item { name addr, weight u32 }\n\
     const MENU arr[2, Item] = {{&hit, 2}, {&hit, 4}}\n"
  in
  semantic_message "const-number-tables-menu-slot-read"
    "global initializer must be a constant expression for `X`; `MENU[0].weight` is not \
     constant"
    (menu_head ^ "const X u32 = MENU[0].weight\nfn main() i32 { return 0 }\n");
  semantic_message "const-number-tables-menu-size-read"
    "expression is not compile-time constant"
    (menu_head ^ "fn main() i32 { buf arr[MENU[0].weight, i32] = {}\n return 0 }\n");
  semantic_error "scalar-constant-address" "constant `K` cannot be addressed"
    "const K u32 = 4\nfn test() addr { return &K }\n";
  semantic_error "fas-029-string-literal-index" "cannot modify string literal `\"x\"`"
    "fn main() i32 { \"x\"[u8] = 9\n return 0 }\n";
  semantic_error "plain-string-nul-write-name"
    "cannot modify string literal `\"ab\\0\"`"
    "fn main() i32 { \"ab\\0\"[u8] = 9\n return 0 }\n";
  let string_literals =
    llvm_of
      "extern \"C\" { fn take(p addr) void }\n\
       fn main() i32 { take(\"x\")\n\
      \ take(c\"x\")\n\
      \ take(\"\")\n\
      \ take(c\"\")\n\
      \ return 0 }\n"
  in
  if
    (not (contains string_literals "[1 x i8] c\"x\""))
    || not (contains string_literals "[2 x i8] c\"x\\00\"")
  then failwith "fas-030-string-literals: incorrect literal storage";
  if
    (not (contains string_literals "[0 x i8] c\"\""))
    || not (contains string_literals "[1 x i8] c\"\\00\"")
  then failwith "fas-030-string-literals: incorrect empty literal storage";
  let string_pointer =
    llvm_of
      "fn read(p addr) i32 { return zext[i32](p[u8]) }\n\
       fn main() i32 { return read(\"x\") }\n"
  in
  if not (contains string_pointer "call i32 @read(ptr") then
    failwith "fas-030-string-literals: read-only pointer call was rejected";
  let byte_literal_semantics =
    llvm_of
      "const ByteCount usize = len(\"a\\0b\") + len(\"\\n\") + len(\"é\")\n\
       const CByteCount usize = len(c\"fas\")\n\
       fn bytes() addr { return \"a\\0b\" }\n\
       fn cbytes() addr { return c\"fas\" }\n\
       fn test() usize { return ByteCount }\n"
  in
  if not (contains byte_literal_semantics "[3 x i8] c\"a\\00b\"") then
    failwith "fas-030-string-literals: ordinary embedded NUL was not preserved";
  if
    (not (contains byte_literal_semantics "[4 x i8] c\"fas\\00\""))
    || not (contains byte_literal_semantics "ret i64 6\n")
  then failwith "fas-030-string-literals: literal length did not count decoded bytes";
  let hexadecimal_byte_literals =
    llvm_of
      "const HexLen usize = len(\"\\x41\\x42\") + len(\"\\x414\")\n\
       fn hex_bytes() addr { return \"\\x41\\x00\\x42\" }\n\
       fn fixed_bytes() addr { return \"\\x414\" }\n\
       fn test() usize { return HexLen }\n"
  in
  if
    (not (contains hexadecimal_byte_literals "[3 x i8] c\"A\\00B\""))
    || (not (contains hexadecimal_byte_literals "[2 x i8] c\"A4\""))
    || not (contains hexadecimal_byte_literals "ret i64 4\n")
  then failwith "hex-byte-escape: decoded bytes or fixed width changed";
  let char_literal body = "'" ^ body ^ "'" in
  let slash = "\\" in
  let escaped_characters =
    List.map
      (fun (body, value) -> (char_literal body, value))
      [
        (slash ^ "n", 10);
        (slash ^ "r", 13);
        (slash ^ "t", 9);
        (slash ^ slash, 92);
        (slash ^ "'", 39);
        (slash ^ "\"", 34);
        (slash ^ "0", 0);
        (slash ^ "x41", 65);
        (slash ^ "xFF", 255);
      ]
  in
  let printable_characters =
    List.init 95 (fun index -> index + 32)
    |> List.filter (fun value -> value <> 39 && value <> 92)
    |> List.map (fun value -> (char_literal (String.make 1 (Char.chr value)), value))
  in
  let all_characters = escaped_characters @ printable_characters in
  let character_bytes =
    Printf.sprintf
      "const CharacterBytes arr[%d,u8] = {%s}\n\
       fn test() u8 { return CharacterBytes[0] }\n"
      (List.length all_characters)
      (all_characters |> List.map fst |> String.concat ", ")
  in
  semantic_accept "character-literal-all-values" character_bytes;
  let character_byte_llvm = llvm_of character_bytes in
  if
    not
      (contains character_byte_llvm
         (Printf.sprintf "[%d x i8] [%s]" (List.length all_characters)
            (String.concat ", "
               (List.map
                  (fun (_, value) -> Printf.sprintf "i8 %d" value)
                  all_characters))))
  then failwith "character-literal-values: byte values changed";
  let character_contexts =
    "const CharacterConst i32 = 'a'\n\
     fn compare_byte(value u8) bool { return value == 'a' }\n\
     fn case_byte(value u8) bool {\n\
     switch value {\n\
     case 'a': return true\n\
     default: return false\n\
     }\n\
     }\n\
     fn main() i32 {\n\
     a u8 = 'a'\n\
     b i8 = 'a'\n\
     c i32 = 'a'\n\
     d u64 = 'a'\n\
     e usize = 'a'\n\
     values arr[1,u8] = {'a'}\n\
     if compare_byte(values[0]) && case_byte(a) && CharacterConst == c && b == 97 && d \
     == 97 && e == 97 { return 0 }\n\
     return 1\n\
     }\n"
  in
  semantic_accept "character-literal-contexts" character_contexts;
  let negative_character = llvm_of "fn main() i32 { return -'a' }\n" in
  if not (contains negative_character "ret i32 4294967199\n") then
    failwith "character-literal-unary-minus: expected -97";
  semantic_error "character-literal-i8-overflow"
    "integer literal is out of range for i8"
    "const Invalid i8 = '\\xFF'\nfn test() i8 { return Invalid }\n";
  let apostrophe_escape_string =
    llvm_of
      "fn read(p addr) u8 { return p[u8,2] }\n\
       fn test() u8 { return read(\"it\\'s\") }\n"
  in
  if not (contains apostrophe_escape_string "[4 x i8] c\"it's\"") then
    failwith "string-apostrophe-escape: decoded bytes changed";
  semantic_error "fas-030-string-literal-fixed-array" "is `addr`, expected `arr[3,u8]`"
    "fn main() i32 { bytes arr[3,u8] = \"abc\"\n return 0 }\n";
  semantic_error "fas-030-c-string-literal-nul"
    "C string literal cannot contain embedded NUL"
    "fn main() i32 { v u8 = c\"a\\0b\"[0]\n return 0 }\n";
  semantic_error "fas-030-const-c-string-literal-nul"
    "C string literal cannot contain embedded NUL"
    "const N usize = len(c\"a\\0b\")\nfn test() usize { return N }\n";
  semantic_error "fas-030-runtime-len-c-string-literal-nul"
    "C string literal cannot contain embedded NUL"
    "fn test() usize { return len(c\"a\\0b\") }\n";
  semantic_error "fas-030-c-string-literal-hex-nul"
    "C string literal cannot contain embedded NUL"
    "fn main() i32 { v u8 = c\"a\\x00b\"[0]\n return 0 }\n";
  semantic_error "fas-030-const-c-string-literal-hex-nul"
    "C string literal cannot contain embedded NUL"
    "const N usize = len(c\"a\\x00b\")\nfn test() usize { return N }\n";
  semantic_error "fas-030-runtime-len-c-string-literal-hex-nul"
    "C string literal cannot contain embedded NUL"
    "fn test() usize { return len(c\"a\\x00b\") }\n";
  let raw_literal_length =
    llvm_of "fn test() i64 { return bitcast[i64](len(\"abc\")) }\n"
  in
  if not (contains raw_literal_length "ret i64 3\n") then
    failwith "fas-031-len: raw literal length is incorrect";
  let c_literal_length =
    llvm_of "fn test() i64 { return bitcast[i64](len(c\"abc\")) }\n"
  in
  if not (contains c_literal_length "ret i64 3\n") then
    failwith "fas-031-len: C literal payload length is incorrect";
  let c_literal_embedded_payload_length =
    llvm_of "fn test() i64 { return bitcast[i64](len(c\"abc\")) }\n"
  in
  if not (contains c_literal_embedded_payload_length "ret i64 3\n") then
    failwith "fas-031-len: C literal payload length included a terminator";
  let literal_const_specialization =
    llvm_of
      "fn literal_size[N const usize]() usize { return N }\n\
       fn test() usize { return literal_size[len(\"abc\")]() }\n"
  in
  if not (contains literal_const_specialization "N=usize:3\"") then
    failwith "fas-031-len: literal length was not accepted as a const argument";
  let array_length =
    llvm_of
      "const K arr[3, u8] = {1, 2, 3}\n\
       const N usize = len(\"abc\")\n\
       fn test() i64 { return bitcast[i64](len(K)) + bitcast[i64](N) }\n"
  in
  if
    (not (contains array_length "ret i64 %v"))
    || not (contains array_length "add i64 3, 3\n")
  then failwith "fas-031-len: fixed array length is incorrect";
  semantic_error "fas-031-len-pointer" "len requires a fixed array or string literal"
    "fn test() i64 { p addr = \"abc\"\n return zext[i64](len(p)) }\n";
  let const_array_value =
    llvm_of
      "const G arr[2, i64] = {7, 8}\n\
       fn take(p addr) i64 { return p[i64,0] + p[i64,1] }\n\
       fn test() i64 { a arr[2, i64]\n\
      \ copy(a, G)\n\
      \ return take(&a) }\n"
  in
  if
    contains const_array_value "load [2 x i64], ptr"
    || contains const_array_value "store [2 x i64] ptr"
  then failwith "fas-013: const array was lowered as an implicit aggregate value";

  let wide_shift =
    llvm_of
      "const C u8 = 1 << 8\n\
       fn run(x u8, n u8) u8 { return x << n }\n\
       fn main() i32 { return zext[i32](C) - zext[i32](run(1, 8)) }\n"
  in
  if not (contains wide_shift "and i8") then
    failwith "fas-010: runtime shift count was not reduced modulo width";

  let zero_bit_counts =
    llvm_of
      "const C8 u8 = ctz(0)\n\
       const L8 u8 = clz(0)\n\
       const C16 u16 = ctz(0)\n\
       const L16 u16 = clz(0)\n\
       const C32 u32 = ctz(0)\n\
       const L32 u32 = clz(0)\n\
       const C64 u64 = ctz(0)\n\
       const L64 u64 = clz(0)\n\
       fn test() i64 { return zext[i64](C8) + zext[i64](L8) +zext[i64](C16) + \
       zext[i64](L16) + zext[i64](C32) +zext[i64](L32) + bitcast[i64](C64) + \
       bitcast[i64](L64) }\n"
  in
  if
    (not (contains zero_bit_counts "zext i8 8 to i64"))
    || (not (contains zero_bit_counts "zext i16 16 to i64"))
    || (not (contains zero_bit_counts "zext i32 32 to i64"))
    || not (contains zero_bit_counts "add i64 %v11, 64")
  then failwith "fas-011: zero bit counts did not equal the integer width";
  let runtime_bit_counts =
    llvm_of
      "fn ctz32(x u32) u32 { return ctz(x) }\nfn clz32(x u32) u32 { return clz(x) }\n"
  in
  if
    (not (contains runtime_bit_counts "@llvm.cttz.i32"))
    || (not (contains runtime_bit_counts "@llvm.ctlz.i32"))
    || not (contains runtime_bit_counts "i1 false")
  then failwith "fas-011: runtime zero bit counts were not defined";
  semantic_error "fas-027-const-array-function-collision" "duplicate declaration `F`"
    "const F arr[2, i64] = {1, 2}\n\
     fn F() i64 { return 3 }\n\
     fn test() i64 { return F() }\n";
  semantic_error "scalar-const-function-collision" "duplicate declaration `F`"
    "const F i64 = 1\nfn F() i64 { return 3 }\n";
  semantic_error "function-scalar-const-collision" "duplicate declaration `F`"
    "fn F() i64 { return 3 }\nconst F i64 = 1\n";
  semantic_error "type-function-collision" "duplicate declaration `F`"
    "struct F { value i64 }\nfn F() i64 { return 3 }\n";
  semantic_error "function-type-collision" "duplicate declaration `F`"
    "fn F() i64 { return 3 }\nstruct F { value i64 }\n";
  semantic_error "type-const-collision" "duplicate declaration `F`"
    "opaque F\nconst F i64 = 1\n";
  semantic_error "const-type-collision" "duplicate declaration `F`"
    "const F i64 = 1\nopaque F\n";
  semantic_error "constant-used-as-function" "constant, not a function"
    "const F i64 = 1\nfn check_case() i64 { return F() }\n";
  semantic_error "function-used-as-value" "function, not a value"
    "fn F() i64 { return 1 }\nfn check_case() i64 { return F }\n";
  semantic_error "type-used-as-value" "type, not a value"
    "opaque F\nfn check_case() i64 { return F }\n";
  semantic_error "const-env-duplicate-const" "duplicate const `A`"
    "const A arr[2, i64] = {1, 2}\nconst A i64 = 1\n";
  semantic_error "const-env-array-length-mismatch" "array of 2 elements, got 3"
    "const A arr[2, i64] = {1, 2, 3}\n";
  semantic_error "const-env-array-element-type-mismatch"
    "cannot use `bool` as `i64`: bool and integer values use different representations"
    "const A arr[2, i64] = {1, true}\n";
  semantic_error "const-env-array-needs-brace-list"
    "const array needs a brace-list initializer" "const A arr[2, i64] = 5\n";
  semantic_error "const-env-brace-list-scalar-rejected"
    "expression is not compile-time constant" "const C i64 = {1, 2}\n";
  semantic_accept "const-env-struct-brace-list-storage"
    "struct S { x i64 y i64 }\nconst C S = {1, 2}\nfn main() i32 { return 0 }\n";

  semantic_error "fas-021-local-aggregate-limit"
    "aggregate element count exceeds the configured limit"
    "fn main() i32 { a arr[1000001,u8]\n return 0 }\n";
  semantic_error "vector-shape-cap-precedes-aggregate-limit"
    "vector lane count exceeds the portable cap of 256"
    "fn main() i32 { v vec[1000001,u8]\n return 0 }\n";
  semantic_error "fas-021-nested-aggregate-limit"
    "aggregate element count exceeds the configured limit"
    "fn main() i32 { a arr[100000,arr[20,u8]]\n return 0 }\n";

  let bool_sext =
    llvm_of "const B i64 = sext[i64](true)\nfn f() i64 { return sext[i64](true) }\n"
  in
  if not (contains bool_sext "sext i1 true to i64") then
    failwith "bool-sext: expected sign-extending lowering";

  let vector_rotate =
    llvm_of
      "fn f() i64 { x vec[4,u32] = splat(1)\n\
      \ y vec[4,u32] = rotl(x, 1)\n\
      \ return zext[i64](y[0]) }\n"
  in
  if
    (not (contains vector_rotate "@llvm.fshl.v4i32"))
    || not
         (contains vector_rotate
            "declare <4 x i32> @llvm.fshl.v4i32(<4 x i32>, <4 x i32>, <4 x i32>)")
  then failwith "vector-rotate: missing typed vector intrinsic";

  let hoisted =
    lower_of
      "fn hoist(p i64) i64 {\n\
      \ x i64 = p\n\
      \ if p > 0 { y i64 = x + 1\n\
      \ x = y }\n\
      \ while x < 3 { z i64 = x\n\
      \ x = z + 1 }\n\
      \ defer { cleanup i64 = x\n\
      \ x = cleanup }\n\
      \ return x }\n"
  in
  let hoist_fn =
    List.find (fun (func : Ir.func) -> func.name = "hoist") hoisted.Ir.funcs
  in
  let alloca_blocks =
    List.concat_map
      (fun (block : Ir.block) ->
        List.filter_map
          (function Ir.Alloca _ -> Some block.id | _ -> None)
          block.instrs)
      hoist_fn.blocks
  in
  if List.length alloca_blocks <> 5 || not (List.for_all (( = ) 0) alloca_blocks) then
    failwith "entry-alloca: lexical local storage escaped the entry block";

  let token_limits = { Limits.default with max_tokens = 1 } in
  (match Lexer.lex ~limits:token_limits (source "x /* trailing trivia */ ") with
  | Ok [ { Token.kind = Token.Ident "x"; _ }; { kind = Token.Eof; _ } ] -> ()
  | Ok _ -> failwith "token-limit: exact limit returned unexpected tokens"
  | Error diagnostics ->
      failwith
        ("token-limit: exact limit was rejected: "
        ^ Diag.render_all ~source:None diagnostics));
  (match Lexer.lex ~limits:token_limits (source "x y") with
  | Error diagnostics ->
      if
        not (contains (Diag.render_all ~source:None diagnostics) "token limit exceeded")
      then failwith "token-limit: unexpected diagnostic"
  | Ok _ -> failwith "token-limit: expected rejection");

  ignore
    (lower_of "fn f() i64 { y i64\n for i i32 = 0; i < 0; y = 5 { }\n return y }\n");

  let vec_comp =
    llvm_of
      "fn f() i32 { v vec[4,i32] = splat(1)\n      v[0] += 5\n      return v[0] }\n"
  in
  if
    (not (contains vec_comp "extractelement"))
    || not (contains vec_comp "insertelement")
  then failwith "vec-compound-assign: expected extract/insert on vec lane write";

  let _ =
    lower_of
      "fn f() i64 { b vec[4,bool] = splat(true)\n\
      \                    x bool = b[0]\n\
      \                    return 0 }\n"
  in
  let _ =
    lower_of
      "fn f(n i32) i32 {\n\
      \  x i32\n\
      \  switch n {\n\
      \    case 0: { x = 1 }\n\
      \    case 1: { x = 2 }\n\
      \    default: { x = 3 }\n\
      \  }\n\
      \  return x\n\
      \  }\n"
  in

  let shadowed_template =
    llvm_of
      "const N i64 = 100\n\
       fn f[N const i64]() i64 { return N }\n\
       fn test() i64 { return f[5]() }\n"
  in
  if not (contains shadowed_template "ret i64 5\n") then
    failwith "fas-015: global const shadowed template const parameter";

  let nested_shadowed_template =
    llvm_of
      "const N i64 = 100\n\
       fn inner[N const i64]() i64 { return N }\n\
       fn outer[N const i64]() i64 { return inner[N]() }\n\
       fn test() i64 { return outer[5]() }\n"
  in
  if not (contains nested_shadowed_template "ret i64 5\n") then
    failwith "fas-015: nested specialization used global const over template parameter";

  let signed_narrow_specialization =
    llvm_of
      "fn b8[N const i8](x i8) i32 { return sext[i32](x) + sext[i32](N) }\n\
       fn main() i32 { return b8[-5](2) }\n"
  in
  if
    (not (contains signed_narrow_specialization "add i32"))
    || not (contains signed_narrow_specialization "sext i8 251 to i32")
  then
    failwith "signed-narrow-specialization: negative i8 constant was not sign-extended";

  (let ir =
     llvm_of
       "fn f() i64 { b vec[4,bool] = splat(true)\n\
        \032             d vec[4,bool] = b & b\n\
        \032             return 0 }\n"
   in
   if not (contains ir "and <4 x i1>") then
     failwith "bool-vec-arith: splat bitand drifted");

  List.iter
    (fun name ->
      semantic_error ("reserved-builtin-" ^ name)
        (Printf.sprintf "`%s` is a builtin and cannot be used as a name" name)
        (Printf.sprintf "fn %s(x i64) i64 { return x }\n" name))
    Names.operation_names;

  (match parse_messages "use \"C\"\n" with
  | [ message ] when message = "`use \"C\"` needs a C header name" -> ()
  | messages ->
      failwith ("use-c-rejection: unexpected diagnostics " ^ String.concat "; " messages));
  incr checks_run;
  (match (parse_file "quoted-use.fas" "use \"C\" \"local.h\"\n").Ast.items with
  | [ Ast.Use { path = "C"; c_header = Some (Ast.C_quoted "local.h"); _ } ] -> ()
  | _ -> failwith "use-c-quoted: parser did not preserve the quoted header");
  incr checks_run;
  (match (parse_file "angle-use.fas" "use \"C\" <system.h>\n").Ast.items with
  | [ Ast.Use { path = "C"; c_header = Some (Ast.C_system "system.h"); _ } ] -> ()
  | _ -> failwith "use-c-angle: parser did not preserve the system header");
  incr checks_run;
  (match (parse_file "keyword-angle-use.fas" "use \"C\" <net/if.h>\n").Ast.items with
  | [ Ast.Use { path = "C"; c_header = Some (Ast.C_system "net/if.h"); _ } ] -> ()
  | _ -> failwith "use-c-keyword-angle: parser did not preserve the system header");
  incr checks_run;
  (match
     (parse_file "pkg-line-before-header.fas"
        "use pkg \"sdl2 >= 2.26\"\nuse \"C\" <system.h>\n")
       .Ast.items
   with
  | [
   Ast.Pkg { pkg = { Ast.modules = "sdl2 >= 2.26"; _ }; _ };
   Ast.Use { path = "C"; c_header = Some (Ast.C_system "system.h"); _ };
  ] ->
      ()
  | _ -> failwith "use-pkg-before-header: parser did not keep the pkg line and header");
  incr checks_run;
  (match
     (parse_file "pkg-line-after-header.fas" "use \"C\" \"local.h\"\nuse pkg \"sdl2\"\n")
       .Ast.items
   with
  | [
   Ast.Use { path = "C"; c_header = Some (Ast.C_quoted "local.h"); _ };
   Ast.Pkg { pkg = { Ast.modules = "sdl2"; _ }; _ };
  ] ->
      ()
  | _ -> failwith "use-pkg-after-header: parser did not keep the pkg line and header");
  incr checks_run;
  (match
     (parse_file "pkg-line-container.fas"
        "use pkg \"sdl2\"\nuse \"C\" <<END\nint pkg_fragment;\nEND\n")
       .Ast.items
   with
  | [
   Ast.Pkg { pkg = { Ast.modules = "sdl2"; _ }; _ };
   Ast.Use { c_header = Some (Ast.C_fragment fragment); _ };
  ]
    when fragment.text = "int pkg_fragment;\n" ->
      ()
  | _ ->
      failwith "use-pkg-container: parser did not keep the pkg line and raw container");
  parse_message "use-pkg-missing-string"
    "`pkg` must be followed by a module string, e.g. `use pkg \"sdl2\"`"
    "use pkg <stdio.h>\n";
  parse_message "use-pkg-missing-string-eol"
    "`pkg` must be followed by a module string, e.g. `use pkg \"sdl2\"`" "use pkg\n";
  parse_message "use-pkg-non-string"
    "`pkg` must be followed by a module string, e.g. `use pkg \"sdl2\"`" "use pkg 42\n";
  parse_message "use-c-inline-pkg" "`pkg` goes on its own line: `use pkg \"MODULES\"`"
    "use \"C\" pkg \"sdl2\" <stdio.h>\n";
  parse_message "use-c-inline-pkg-no-header"
    "`pkg` goes on its own line: `use pkg \"MODULES\"`" "use \"C\" pkg \"sdl2\"\n";
  parse_error_message "use-c-inline-pkg-location"
    "regression.fas:1:9: error: `pkg` goes on" "use \"C\" pkg \"sdl2\" <stdio.h>\n";
  parse_error_message "use-file-pkg"
    "regression.fas:1:17: error: expected end of statement"
    "use \"other.fas\" pkg \"sdl2\"\n";
  (match
     check_files
       [
         ( "pkg-identifier.fas",
           "fn pkg() i32 { return 1 }\n\
            var pkg_value i32 = 3\n\
            fn main() i32 { return pkg() + pkg_value }\n" );
       ]
   with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        ("pkg-identifier: unexpected diagnostics "
        ^ String.concat "; "
            (List.map (fun (diagnostic : Diag.t) -> diagnostic.message) diagnostics)));
  (match
     check_files
       [
         ( "pkg-identifier-after-c.fas",
           "use \"C\" <stdio.h>\n\
            fn pkg() i32 { return 1 }\n\
            fn main() i32 { return pkg() }\n" );
       ]
   with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        ("pkg-identifier-after-c: unexpected diagnostics "
        ^ String.concat "; "
            (List.map (fun (diagnostic : Diag.t) -> diagnostic.message) diagnostics)));
  (match
     check_files
       [
         ( "pkg-local-identifier.fas",
           "use pkg \"sdl2\"\nfn main() i32 {\npkg i32 = 2\nreturn pkg\n}\n" );
       ]
   with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        ("pkg-local-identifier: unexpected diagnostics "
        ^ String.concat "; "
            (List.map (fun (diagnostic : Diag.t) -> diagnostic.message) diagnostics)));
  let raw_container =
    parse_file "raw-container.fas"
      "  use \"C\" <<END\n/* } { */\nconst char *s = \"# Fas text\";\nEND\n"
  in
  (match raw_container.Ast.items with
  | [ Ast.Use { c_header = Some (Ast.C_fragment fragment); span; _ } ]
    when fragment.tag = "END"
         && fragment.text = "/* } { */\nconst char *s = \"# Fas text\";\n"
         && span.Span.line = 1 ->
      ()
  | _ -> failwith "use-c-container: raw text or source line was not preserved");
  let raw_crlf =
    parse_file "raw-container-crlf.fas" "use \"C\" <<E\r\nint raw_fragment;\r\nE"
  in
  (match raw_crlf.Ast.items with
  | [ Ast.Use { c_header = Some (Ast.C_fragment fragment); span; _ } ]
    when fragment.text = "int raw_fragment;\r\n" && span.Span.line = 1 ->
      ()
  | _ -> failwith "use-c-container-crlf: CRLF or EOF terminator was not accepted");
  parse_message "use-c-container-missing-terminator"
    "C container is missing terminator `END`" "use \"C\" <<END\nint value;\n";
  parse_message "use-c-container-invalid-tag"
    "C container tag must be an ASCII identifier" "use \"C\" <<9END\n9END\n";
  parse_message "use-c-container-trailing-opener-text"
    "trailing text after C container tag" "use \"C\" <<END trailing\nEND\n";
  parse_message "use-c-container-nested" "C container must be at top level"
    "fn main() i32 {\nuse \"C\" <<END\nint value;\nEND\nreturn 0 }\n";
  parse_message "use-c-container-terminator-trailing-space"
    "C container is missing terminator `END`" "use \"C\" <<END\nint value;\nEND \n";
  (match parse_messages "fn use() i32 { return 0 }\n" with
  | [ message ] when message = "expected identifier, found `use`" -> ()
  | messages ->
      failwith
        ("use-keyword-rejection: unexpected diagnostics " ^ String.concat "; " messages));
  syntax_pin "keyword-identifier-caret" "fn use() i32 { return 0 }\n" 1 4 3
    "expected identifier, found `use`";
  caret_render_pin "non-ascii-caret-after-tab" "\t\"\xc3\xa9\" bad\n" 6 9
    "regression.fas:1:7: error: caret probe\n  \t\"\xc3\xa9\" bad\n  \t    ^~~\n";
  caret_render_pin "non-ascii-caret-multibyte-width" "\t\"\xc3\xa9\" bad\n" 1 5
    "regression.fas:1:2: error: caret probe\n  \t\"\xc3\xa9\" bad\n  \t^~~\n";
  if not (Names.reserved_binding_name "use") then
    failwith "use-keyword: use is not registered as a reserved binding";
  (match Driver.use_path_error "/opt/lib.fas" with
  | Some message
    when message
         = "absolute Fas dependency path `/opt/lib.fas` is not supported; use a \
            relative path" ->
      ()
  | _ -> failwith "use-absolute-path: unexpected validation result");
  (match Driver.use_path_error "lib\000.fas" with
  | Some message when message = "Fas dependency paths cannot contain NUL bytes" -> ()
  | _ -> failwith "use-nul-path: unexpected validation result");
  (match Driver.use_path_error "lib.FAS" with
  | Some message
    when message = "Fas dependency path `lib.FAS` must end in lowercase `.fas`" ->
      ()
  | _ -> failwith "use-extension-case: unexpected validation result");
  (match Driver.use_path_error "lib.h" with
  | Some message
    when message = "Fas dependency path `lib.h` must end in lowercase `.fas`" ->
      ()
  | _ -> failwith "use-c-header-path: unexpected validation result");
  if Option.is_some (Driver.use_path_error "lib/../ops.fas") then
    failwith "use-relative-parent-path: approved relative path rejected";
  let use_limit_directory =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "fas-use-regressions-%d" (Unix.getpid ()))
  in
  Unix.mkdir use_limit_directory 0o700;
  let use_limit_root = Filename.concat use_limit_directory "root.fas" in
  let use_limit_child = Filename.concat use_limit_directory "child.fas" in
  let use_missing_root = Filename.concat use_limit_directory "missing-root.fas" in
  let use_directory = Filename.concat use_limit_directory "directory.fas" in
  let use_directory_root = Filename.concat use_limit_directory "directory-root.fas" in
  let use_duplicate_root = Filename.concat use_limit_directory "duplicate-root.fas" in
  let use_relative_root = Filename.concat use_limit_directory "relative-root.fas" in
  let use_relative_missing_root =
    Filename.concat use_limit_directory "relative-missing-root.fas"
  in
  let use_duplicate_one = Filename.concat use_limit_directory "one.fas" in
  let use_duplicate_two = Filename.concat use_limit_directory "two.fas" in
  let use_unknown_root = Filename.concat use_limit_directory "unknown-root.fas" in
  let use_unknown_child = Filename.concat use_limit_directory "unknown-child.fas" in
  let use_unknown_grandchild =
    Filename.concat use_limit_directory "unknown-grandchild.fas"
  in
  let write_use_test_file path contents =
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel contents)
  in
  let remove_use_test_path path = try Sys.remove path with Sys_error _ -> () in
  let remove_use_test_directory path =
    try Unix.rmdir path with Unix.Unix_error _ -> ()
  in
  let use_limit_root_text = "use \"child.fas\"\nfn main() i32 { return 0 }\n" in
  write_use_test_file use_limit_root use_limit_root_text;
  write_use_test_file use_limit_child "fn child() i32 { return 0 }\n";
  Fun.protect
    ~finally:(fun () ->
      List.iter remove_use_test_path
        [
          use_limit_root;
          use_limit_child;
          use_missing_root;
          Filename.concat use_limit_directory "absent.fas";
          use_directory_root;
          use_duplicate_root;
          use_duplicate_one;
          use_duplicate_two;
          use_unknown_root;
          use_unknown_child;
          use_unknown_grandchild;
          use_relative_root;
          use_relative_missing_root;
        ];
      remove_use_test_directory use_directory;
      remove_use_test_directory use_limit_directory)
    (fun () ->
      let dependency_limit_error limits =
        match Driver.load_program ~limits use_limit_root with
        | Error [ diagnostic ] -> diagnostic.Diag.message
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
        | Ok _ -> failwith "dependency-closure-limit: expected rejection"
      in
      let file_error =
        dependency_limit_error { Limits.default with max_use_files = 1 }
      in
      if
        file_error
        <> "dependency closure exceeds budget max_use_files of 1 (profile 0.1.5)"
      then failwith ("dependency-file-limit: unexpected diagnostic " ^ file_error);
      let byte_error =
        dependency_limit_error
          { Limits.default with max_use_bytes = String.length use_limit_root_text }
      in
      if
        byte_error
        <> "dependency closure exceeds budget max_use_bytes of "
           ^ string_of_int (String.length use_limit_root_text)
           ^ " (profile 0.1.5)"
      then failwith ("dependency-byte-limit: unexpected diagnostic " ^ byte_error);
      let driver_error path =
        match Driver.run (cli_run [ path ]) with
        | Error [ diagnostic ] -> diagnostic
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
        | Ok _ -> failwith "use-diagnostic-pins: expected driver rejection"
      in
      let original_cwd = Sys.getcwd () in
      write_use_test_file use_relative_root "fn f() void { if 1 { return } }\n";
      let relative_diagnostic =
        Fun.protect
          ~finally:(fun () -> Sys.chdir original_cwd)
          (fun () ->
            Sys.chdir use_limit_directory;
            driver_error "relative-root.fas")
      in
      if
        relative_diagnostic.primary.Span.file <> "relative-root.fas"
        || relative_diagnostic.notes <> []
      then failwith "relative-root-path: diagnostic path or root notes changed";
      write_use_test_file use_relative_missing_root "use \"absent.fas\"\n";
      let relative_missing =
        Fun.protect
          ~finally:(fun () -> Sys.chdir original_cwd)
          (fun () ->
            Sys.chdir use_limit_directory;
            driver_error "relative-missing-root.fas")
      in
      if
        relative_missing.primary.Span.file <> "relative-missing-root.fas"
        || relative_missing.Diag.message
           <> "cannot read Fas dependency path `absent.fas`: No such file or directory"
        || relative_missing.notes
           <> [ "include chain: relative-missing-root.fas -> absent.fas" ]
      then failwith "relative-include-path: path or include chain changed";
      write_use_test_file use_missing_root "use \"absent.fas\"\n";
      let missing = driver_error use_missing_root in
      if
        missing.Diag.message
        <> "cannot read Fas dependency path `absent.fas`: No such file or directory"
        || missing.primary.Span.file <> use_missing_root
        || missing.primary.Span.line <> 1
        || missing.primary.Span.column <> 5
        || missing.notes
           <> [
                "include chain: " ^ use_missing_root ^ " -> "
                ^ Filename.concat use_limit_directory "absent.fas";
              ]
      then failwith "use-missing-chain: diagnostic or include chain changed";
      Unix.mkdir use_directory 0o700;
      write_use_test_file use_directory_root "use \"directory.fas\"\n";
      let directory = driver_error use_directory_root in
      if
        directory.Diag.message <> "Fas dependency `directory.fas` is a directory"
        || directory.notes
           <> [ "include chain: " ^ use_directory_root ^ " -> " ^ use_directory ]
      then failwith "use-directory-chain: diagnostic or include chain changed";
      write_use_test_file use_duplicate_root "use \"one.fas\"\nuse \"two.fas\"\n";
      write_use_test_file use_duplicate_one "fn duplicate() i32 { return 1 }\n";
      write_use_test_file use_duplicate_two "fn duplicate() i64 { return 2 }\n";
      let duplicate = driver_error use_duplicate_root in
      if
        duplicate.Diag.message <> "duplicate function `duplicate`"
        || duplicate.primary.Span.file <> use_duplicate_two
        || duplicate.primary.Span.line <> 1
        || duplicate.primary.Span.column <> 4
        || duplicate.notes
           <> [
                "first definition is at " ^ use_duplicate_one ^ ":1:4";
                "include chain: " ^ use_duplicate_root ^ " -> " ^ use_duplicate_one;
                "include chain: " ^ use_duplicate_root ^ " -> " ^ use_duplicate_two;
              ]
      then failwith "use-duplicate-sites: diagnostic or include chains changed";
      write_use_test_file use_unknown_root "use \"unknown-child.fas\"\n";
      write_use_test_file use_unknown_child "use \"unknown-grandchild.fas\"\n";
      write_use_test_file use_unknown_grandchild "fn unknown() i32 { return absent }\n";
      let unknown = driver_error use_unknown_root in
      if
        unknown.Diag.message <> "unknown name `absent`"
        || unknown.primary.Span.file <> use_unknown_grandchild
        || unknown.notes
           <> [
                "include chain: " ^ use_unknown_root ^ " -> " ^ use_unknown_child
                ^ " -> " ^ use_unknown_grandchild;
              ]
      then failwith "use-unknown-name-chain: message or include chain changed");

  List.iter
    (fun name ->
      semantic_error ("reserved-type-" ^ name)
        (Printf.sprintf "`%s` is a type name and cannot be used as a name" name)
        (Printf.sprintf "struct %s { value i32 }\n" name))
    Names.primitive_type_names;

  List.iter
    (fun name ->
      semantic_error ("reserved-literal-" ^ name)
        (Printf.sprintf "`%s` is a literal and cannot be used as a name" name)
        (Printf.sprintf "const %s bool = false\n" name))
    Names.literal_names;

  List.iter
    (fun (name, fragment, text) -> semantic_error name fragment text)
    [
      ( "reserved-primitive-type",
        "`i32` is a type name and cannot be used as a name",
        "struct i32 { value i32 }\n" );
      ( "reserved-literal-const",
        "`true` is a literal and cannot be used as a name",
        "const true bool = false\n" );
      ( "reserved-generic-parameter",
        "`len` is a builtin and cannot be used as a name",
        "fn value[len](x i32) i32 { return x }\n" );
      ( "reserved-const-parameter",
        "`splat` is a builtin and cannot be used as a name",
        "fn value[splat const i32](x i32) i32 { return x }\n" );
      ( "reserved-runtime-parameter",
        "`popcount` is a builtin and cannot be used as a name",
        "fn value(popcount i32) i32 { return popcount }\n" );
      ( "reserved-local",
        "`len` is a builtin and cannot be used as a name",
        "fn value() i32 { len i32 = 1\n return len }\n" );
    ];

  let released_shift_names =
    llvm_of
      "fn shl(x i32) i32 { return x + 1 }\n\
       fn lshr(x i32) i32 { return x + 2 }\n\
       fn ashr(x i32) i32 { return x + 3 }\n\
       fn shr(x i32) i32 { return x + 4 }\n\
       fn main() i32 { return shl(1) + lshr(2) + ashr(3) + shr(4) }\n"
  in
  List.iter
    (fun name ->
      if not (contains released_shift_names ("call i32 @" ^ name)) then
        failwith ("released-shift-name: user function was not called: " ^ name))
    [ "shl"; "lshr"; "ashr"; "shr" ];

  let exact_semantic_error name expected text =
    match semantic_messages text with
    | [ message ] when message = expected -> ()
    | messages ->
        failwith
          (name ^ ": expected `" ^ expected ^ "`, got " ^ String.concat "; " messages)
  in
  let reserved_float_names = Names.reserved_float_names in
  if reserved_float_names <> [ "sqrt"; "fma"; "floor"; "ceil"; "round"; "abs"; "cvt" ]
  then failwith "reserved-float-names: registry changed";
  List.iter
    (fun (name, message) ->
      exact_semantic_error ("float-math-arity-" ^ name) message
        (Printf.sprintf "fn main() i32 { return %s() }\n" name))
    [
      ("sqrt", "builtin `sqrt` expects 1 argument");
      ("fma", "builtin `fma` expects 3 arguments");
      ("floor", "builtin `floor` expects 1 argument");
      ("ceil", "builtin `ceil` expects 1 argument");
      ("round", "builtin `round` expects 1 argument");
      ("abs", "builtin `abs` expects 1 argument");
    ];
  exact_semantic_error "reserved-float-call-cvt"
    "`cvt` needs a target type: `cvt[T](x)`" "fn main() i32 { return cvt() }\n";
  List.iter
    (fun name ->
      exact_semantic_error
        ("reserved-float-function-" ^ name)
        (Printf.sprintf "`%s` is a builtin and cannot be used as a name" name)
        (Printf.sprintf "fn %s() i32 { return 0 }\n" name);
      if not (Names.reserved_binding_name name) then
        failwith ("reserved-float-binding-registry: " ^ name))
    reserved_float_names;
  if Names.reserved_float_type_names <> [ "f32"; "f64" ] then
    failwith "reserved-float-type-names: registry changed";
  List.iter
    (fun name ->
      exact_semantic_error
        ("reserved-float-type-binding-" ^ name)
        (Printf.sprintf "`%s` is a type name and cannot be used as a name" name)
        (Printf.sprintf "fn %s() i32 { return 0 }\n" name))
    Names.reserved_float_type_names;
  List.iter
    (fun (name, binding_name, text) ->
      exact_semantic_error name
        (Printf.sprintf "`%s` is a builtin and cannot be used as a name" binding_name)
        text)
    [
      ("reserved-float-local", "abs", "fn main() i32 { abs i32 = 1\n return abs }\n");
      ("reserved-float-parameter", "cvt", "fn test(cvt i32) i32 { return cvt }\n");
      ("reserved-float-function", "sqrt", "fn sqrt() i32 { return 0 }\n");
      ("reserved-float-struct", "fma", "struct fma { value i32 }\n");
      ( "reserved-float-generic-parameter",
        "floor",
        "fn value[floor](x i32) i32 { return x }\n" );
      ( "reserved-float-const-parameter",
        "ceil",
        "fn value[ceil const u32](x i32) i32 { return x }\n" );
      ("reserved-float-const", "round", "const round i32 = 1\n");
    ];
  exact_semantic_error "reserved-float-local-round"
    "`round` is a builtin and cannot be used as a name"
    "fn main() i32 { round i32 = 1\n return round }\n";
  exact_semantic_error "reserved-float-function-f32"
    "`f32` is a type name and cannot be used as a name" "fn f32() i32 { return 0 }\n";
  List.iter
    (fun (name, expected) ->
      exact_semantic_error
        ("reserved-category-local-" ^ name)
        expected
        (Printf.sprintf "fn main() i32 { %s u32 = 3\n return 0 }\n" name);
      exact_semantic_error
        ("reserved-category-fn-" ^ name)
        expected
        (Printf.sprintf "fn %s(x u32) u32 { return x }\n" name))
    [
      ("len", "`len` is a builtin and cannot be used as a name");
      ("zext", "`zext` is a builtin and cannot be used as a name");
      ("sqrt", "`sqrt` is a builtin and cannot be used as a name");
      ("u32", "`u32` is a type name and cannot be used as a name");
      ("vec", "`vec` is a type name and cannot be used as a name");
      ("true", "`true` is a literal and cannot be used as a name");
    ];
  semantic_pin "builtin-type-position-len" "fn f(x len) i32 { return 0 }\n" 1 8 3
    "`len` is a builtin, not a type" None;
  semantic_pin "builtin-type-position-true" "fn f(x true) i32 { return 0 }\n" 1 8 4
    "`true` is a literal, not a type" None;
  semantic_pin "builtin-type-position-null" "fn f(x null) i32 { return 0 }\n" 1 8 4
    "`null` is a literal, not a type" None;
  semantic_pin "builtin-type-position-sqrt" "fn f(x sqrt) i32 { return 0 }\n" 1 8 4
    "`sqrt` is a builtin, not a type" None;
  semantic_pin "builtin-type-position-zext" "fn f(x zext) i32 { return 0 }\n" 1 8 4
    "`zext` is a builtin, not a type" None;
  semantic_pin "builtin-type-position-local" "fn f() i32 { x len = 1\nreturn 0 }\n" 1 16
    3 "`len` is a builtin, not a type" None;
  semantic_pin "builtin-type-position-generic-argument"
    "fn g[T](x T) i32 { return 0 }\nfn f() i32 { return g[len](1) }\n" 2 23 3
    "`len` is a builtin, not a type" None;
  semantic_pin "builtin-call-needs-target-zext" "fn f(a u32) u32 { return zext(a) }\n" 1
    26 4 "`zext` needs a target type: `zext[T](x)`" None;
  semantic_pin "builtin-call-needs-target-bitcast"
    "fn f(a u32) u32 { return bitcast(a) }\n" 1 26 7
    "`bitcast` needs a target type: `bitcast[T](x)`" None;
  semantic_pin "builtin-call-needs-target-cvt" "fn f(a u32) f64 { return cvt(a) }\n" 1
    26 3 "`cvt` needs a target type: `cvt[T](x)`" None;
  semantic_pin "builtin-call-needs-type-handle-from-addr"
    "fn f(p addr) addr { return handle_from_addr(p) }\n" 1 28 16
    "`handle_from_addr` needs a type argument: `handle_from_addr[T](p)`" None;
  semantic_pin "builtin-call-needs-type-gather"
    "fn f(p addr, i vec[2,u32], m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     gather(p, i, m, v) }\n"
    1 77 6 "`gather` needs a type argument: `gather[T](p, idx, m, fallback)`" None;
  semantic_pin "builtin-takes-no-type-arguments-sqrt"
    "fn f(a f64) f64 { return sqrt[u32](a) }\n" 1 26 4 "`sqrt` takes no type arguments"
    None;
  semantic_pin "builtin-takes-no-type-arguments-len"
    "fn f(a vec[2,u32]) u32 { return len[u32](a) }\n" 1 33 3
    "`len` takes no type arguments" None;
  semantic_pin "builtin-value-len" "fn f(a vec[2,u32]) u32 { x u32 = len\nreturn x }\n"
    1 34 3 "`len` is a builtin and must be called" None;
  semantic_pin "builtin-value-cvt-type-arguments"
    "fn f(a u32) u32 { x u32 = cvt[u32]\nreturn x }\n" 1 27 3
    "`cvt` is a builtin and must be called" None;
  semantic_pin "builtin-value-addr-len" "fn f(p addr) addr { y addr = len\nreturn y }\n"
    1 30 3 "`len` is a builtin and must be called" None;
  semantic_pin "builtin-value-const-len" "const K u32 = len\n" 1 15 3
    "`len` is a builtin and must be called" None;
  semantic_pin "builtin-call-const-zext" "const K u32 = zext(1)\n" 1 15 7
    "`zext` needs a target type: `zext[T](x)`" None;
  semantic_pin "builtin-takes-no-type-arguments-const-sqrt"
    "const K f64 = sqrt[u32](1.0)\n" 1 15 4 "`sqrt` takes no type arguments" None;
  semantic_accept "builtin-twin-zext-type-argument"
    "fn f(a u8) u32 { return zext[u32](a) }\n";
  semantic_accept "builtin-twin-cvt-type-argument"
    "fn f(a u32) f64 { return cvt[f64](a) }\n";
  semantic_accept "builtin-twin-sqrt-float" "fn f(x f64) f64 { return sqrt(x) }\n";
  semantic_accept "builtin-twin-sizeof-type-argument"
    "fn f() usize { return sizeof[u32] }\n";
  semantic_accept "float-types-and-storage-accept"
    "const Half f32 = 1.5\n\
     const Values arr[2, f32] = {Half, 0.1}\n\
     var Mutable f64 = 3.0\n\
     struct Pair { first f32, second f64 }\n\
     fn identity32(x f32) f32 { return x }\n\
     fn identity64(x f64) f64 { return x }\n\
     fn read(p addr, i usize) f32 { return p[f32, i] }\n\
     fn use_storage(p addr) i32 {\n\
     view values = p[f64, ..]\n\
     volatile_store[f32](p, Half)\n\
     x f32 = volatile_load[f32](p)\n\
     y f64 = Mutable\n\
     pair Pair = {x, y}\n\
     out Pair\n\
     copy(out, pair)\n\
     size usize = sizeof[f32] + sizeof[f64] + alignof[f32] + alignof[f64]\n\
     return trunc[i32](size)\n\
     }\n";
  semantic_accept "float-literal-destination-peer-typing"
    "fn accept(value f32) f32 { return value }\n\
     fn use_value() f32 { return accept(0.1) }\n";
  semantic_accept "float-literal-peer-order"
    "const A f32 = 0.1\n\
     const ADD1 f32 = A + 0.1\n\
     const ADD2 f32 = 0.1 + A\n\
     const SUB1 f32 = A - 0.1\n\
     const SUB2 f32 = 0.1 - A\n\
     const MUL1 f32 = A * -1.5\n\
     const MUL2 f32 = -1.5 * A\n\
     const DIV1 f32 = A / 2.0\n\
     const DIV2 f32 = 2.0 / A\n\
     const EQ1 bool = A == 0.1\n\
     const EQ2 bool = 0.1 == A\n\
     const NE1 bool = A != 0.1\n\
     const NE2 bool = 0.1 != A\n\
     const LT1 bool = A < 0.1\n\
     const LT2 bool = 0.1 < A\n\
     const LE1 bool = A <= 0.1\n\
     const LE2 bool = 0.1 <= A\n\
     const GT1 bool = A > 0.1\n\
     const GT2 bool = 0.1 > A\n\
     const GE1 bool = A >= 0.1\n\
     const GE2 bool = 0.1 >= A\n\
     fn branches(a f32, b bool) f32 {\n\
     x f32 = if b { a } else { 0.1 }\n\
     y f32 = if b { 0.1 } else { a }\n\
     return x + y\n\
     }\n\
     fn unary(a f32) f32 { return -1.5 * a + a * -1.5 }\n";
  semantic_message "float-literal-peer-order-widening-left"
    "`0.1 + a` is computed in `f32` and may round before it reaches `f64`"
    "fn bad(a f32) f64 { return 0.1 + a }\n";
  semantic_message "float-literal-peer-order-widening-right"
    "`a + 0.1` is computed in `f32` and may round before it reaches `f64`"
    "fn bad(a f32) f64 { return a + 0.1 }\n";
  semantic_message "float-if-vararg-peer-rejection"
    "C varargs do not accept `f32`; convert it to `f64` explicitly"
    "extern \"C\" { fn printf(format addr, ...) i32 }\n\
     fn bad(x f32, b bool) i32 { return printf(c\"%f\", if b { x } else { 1.0 }) }\n";
  semantic_accept "float-if-vararg-explicit-conversion"
    "extern \"C\" { fn printf(format addr, ...) i32 }\n\
     fn good(x f32, b bool) i32 { return printf(c\"%f\", cvt[f64](if b { x } else { \
     1.0 })) }\n";
  semantic_accept "float-comparison-branch-feasibility"
    "const X f64 = -1.0\n\
     const N f64 = 0.0 / 0.0\n\
     fn false_branches() i32 {\n\
     a arr[4, i32] = {}\n\
     i usize = 10\n\
     if X < -2.0 { return a[i] }\n\
     if N == N { return a[i] }\n\
     if -0.0 < 0.0 { return a[i] }\n\
     return 0\n\
     }\n\
     fn disjoint(c bool) u8 {\n\
     a arr[4, u8] = {10, 11, 12, 13}\n\
     i usize = if (if c { 1 } else { 2 }) != 5 { 1 } else { 9 }\n\
     return a[i]\n\
     }\n";
  semantic_message "float-comparison-proven-true-still-checks"
    "array index is out of bounds for length 4"
    "fn bad() i32 { a arr[4, i32] = {}\n\
    \ i usize = 10\n\
    \ if 1.0 < 2.0 { return a[i] }\n\
    \ return 0 }\n";
  semantic_message "float-comparison-signed-zero-proven-true-still-checks"
    "array index is out of bounds for length 4"
    "fn bad() i32 { a arr[4, i32] = {}\n\
    \ i usize = 10\n\
    \ if -0.0 == 0.0 { return a[i] }\n\
    \ return 0 }\n";
  semantic_message "float-comparison-nan-not-equal-proven-true-still-checks"
    "array index is out of bounds for length 4"
    "const N f64 = 0.0 / 0.0\n\
    \ fn bad() i32 { a arr[4, i32] = {}\n\
    \ i usize = 10\n\
    \ if N != N { return a[i] }\n\
    \ return 0 }\n";
  semantic_accept "generic-inference-checked-expressions"
    "struct S { x f32 }\n\
     fn pick[T](a T, b T, mode bool) T { return if mode { a } else { b } }\n\
     fn get() f64 { return 1.0 }\n\
     fn use_values(p addr, bits u32) i32 {\n\
     w arr[2, vec[4, u32]] = {}\n\
     s S = {}\n\
     a vec[4, u32] = pick(w[0], w[1], true)\n\
     b f32 = pick(s.x, s.x, true)\n\
     c f32 = pick(p[f32], p[f32], true)\n\
     d f64 = pick(get(), get(), true)\n\
     e f32 = pick(cvt[f32](bits), cvt[f32](bits), true)\n\
     return 0\n\
     }\n";
  semantic_message "generic-inference-checked-expression-conflict"
    "cannot infer `T` for `pick`: `x` is `f32`, `y` is `i32`"
    "fn pick[T](a T, b T, mode bool) T { return if mode { a } else { b } }\n\
     fn bad(x f32, y i32) f32 { return pick(x, y, true) }\n";
  semantic_accept "float-arithmetic-widening-and-if-expressions"
    "fn operations(a f32, b f32, wide f64, condition bool) f64 {\n\
     plain f64 = a\n\
     mixed f64 = wide + a\n\
     branch f64 = if condition { a } else { wide }\n\
     value f32 = -(a * b)\n\
     value += a\n\
     value -= b\n\
     value *= a\n\
     value /= b\n\
     return mixed + branch + plain + value\n\
     }\n\
     fn comparisons(a f32, b f32) bool {\n\
     return a == b || a != b || a < b || a <= b || a > b || a >= b\n\
     }\n";
  semantic_accept "float-constant-evaluation-accept"
    "const HALF_BITS u32 = 0x33800000\n\
     const ONE f32 = 1.0\n\
     const HALF f32 = bitcast[f32](HALF_BITS)\n\
     const ROUNDED f32 = ONE + HALF + HALF\n\
     const WIDE f64 = ROUNDED\n\
     const INTEGER i32 = cvt[i32](WIDE)\n\
     fn main() i32 { return INTEGER }\n";
  semantic_accept "float-widening-returns-and-arguments"
    "fn widen(value f32) f64 { return value }\n\
     fn identity(value f64) f64 { return value }\n\
     fn pass(value f32) f64 { return identity(value) }\n";
  semantic_pin "float-narrow-arithmetic-widening"
    "fn bad(a f32, b f32) f64 { return a * b }\n" 1 35 5
    "`a * b` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `cvt[f64](a) * b`");
  let float_vector_lane_source = "fn bad(v vec[4,f32]) f64 { return (v * v)[0] }\n" in
  semantic_pin "float-vector-lane-arithmetic-widening" float_vector_lane_source 1
    (String.length "fn bad(v vec[4,f32]) f64 { return " + 1)
    (String.length "(v * v)[0]")
    "`(v * v)[0]` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `cvt[f64]((v * v)[0])`");
  semantic_accept "float-vector-lane-arithmetic-explicit-widening"
    "fn good(v vec[4,f32]) f64 { return cvt[f64]((v * v)[0]) }\n";
  let float_vector_lane_builtin_source =
    "fn bad(v vec[4,f32]) f64 { return sqrt(v)[1] }\n"
  in
  semantic_pin "float-vector-lane-builtin-widening" float_vector_lane_builtin_source 1
    (String.length "fn bad(v vec[4,f32]) f64 { return " + 1)
    (String.length "sqrt(v)[1]")
    "`sqrt(v)[1]` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `cvt[f64](sqrt(v)[1])`");
  semantic_accept "float-vector-lane-builtin-explicit-widening"
    "fn good(v vec[4,f32]) f64 { return cvt[f64](sqrt(v)[1]) }\n";
  semantic_pin "float-narrow-mixed-operand-widening"
    "fn bad(a f32, b f32, wide f64) f64 { return wide + a * b }\n" 1 52 5
    "`a * b` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `cvt[f64](a) * b`");
  semantic_pin "float-narrow-unary-widening" "fn bad(a f32) f64 { return -a }\n" 1 28 2
    "`-a` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `-cvt[f64](a)`");
  List.iter
    (fun builtin ->
      let call = if builtin = "fma" then "fma(a, a, a)" else builtin ^ "(a)" in
      let narrow_message =
        Printf.sprintf "`%s` is computed in `f32` and may round before it reaches `f64`"
          call
      in
      List.iter
        (fun (context, source) ->
          semantic_message
            ("float-builtin-narrow-" ^ builtin ^ "-" ^ context)
            (if context = "constant" then
               Printf.sprintf
                 "`%s` is computed in `f32` and may round before it reaches `f64`"
                 (if builtin = "fma" then "fma(A, A, A)" else builtin ^ "(A)")
             else narrow_message)
            source)
        [
          ("return", "fn bad(a f32) f64 { return " ^ call ^ " }\n");
          ( "initializer",
            "fn bad(a f32) f64 { value f64 = " ^ call ^ "\n return value }\n" );
          ( "assignment",
            "fn bad(a f32, b f64) f64 { value f64 = b\n value = " ^ call
            ^ "\n return value }\n" );
          ( "argument",
            "fn take(value f64) f64 { return value }\nfn bad(a f32) f64 { return take("
            ^ call ^ ") }\n" );
          ("comparison", "fn bad(a f32, b f64) bool { return " ^ call ^ " < b }\n");
          ( "branch",
            "fn bad(a f32, b f64, c bool) f64 { return if c { " ^ call
            ^ " } else { b } }\n" );
          ( "constant",
            "const A f32 = 1.0\nconst BAD f64 = "
            ^ (if builtin = "fma" then "fma(A, A, A)" else builtin ^ "(A)")
            ^ "\nfn main() i32 { return 0 }\n" );
        ])
    [ "sqrt"; "fma"; "abs"; "floor"; "ceil"; "round" ];
  semantic_accept "float-builtin-explicit-widening"
    "fn good(a f32) f64 { return cvt[f64](sqrt(a)) }\n";
  semantic_error "float-vector-builtin-no-implicit-widening" "expected `vec[4, f64]`"
    "fn bad(a vec[4, f32]) vec[4, f64] { return sqrt(a) }\n";
  semantic_accept "float-vector-builtin-explicit-widening"
    "fn good(a vec[4, f32]) vec[4, f64] { return cvt[vec[4, f64]](sqrt(a)) }\n";
  semantic_pin "float-literal-left-widening-help"
    "fn bad(a f32) f64 { return 0.1 * a }\n" 1 28 7
    "`0.1 * a` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `0.1 * cvt[f64](a)`");
  semantic_pin "float-negative-literal-left-widening-help"
    "fn bad(a f32) f64 { return -0.5 * a }\n" 1 28 8
    "`-0.5 * a` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `-0.5 * cvt[f64](a)`");
  semantic_pin "float-parenthesized-literal-left-widening-help"
    "fn bad(a f32) f64 { return (0.1) + a }\n" 1 28 9
    "`(0.1) + a` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `(0.1) + cvt[f64](a)`");
  semantic_pin "float-compound-literal-left-widening-help"
    "fn bad(a f32) f64 { x f64 = 1.0\n x += 0.1 * a\n return x }\n" 2 7 7
    "`0.1 * a` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `0.1 * cvt[f64](a)`");
  semantic_pin "float-constant-literal-left-widening-help"
    "const A f32 = 1.0\nconst C f64 = 0.1 + A\n" 2 15 7
    "`0.1 + A` is computed in `f32` and may round before it reaches `f64`"
    (Some "write `0.1 + cvt[f64](A)`");
  semantic_accept "float-literal-left-widening-help-twins"
    "const A f32 = 1.0\n\
     const C f64 = 0.1 + cvt[f64](A)\n\
     fn good(a f32) f64 {\n\
     x f64 = 0.1 * cvt[f64](a)\n\
     y f64 = -0.5 * cvt[f64](a)\n\
     z f64 = (0.1) + cvt[f64](a)\n\
     x += 0.1 * cvt[f64](a)\n\
     return x + y + z + C }\n";
  semantic_pin "float-remainder-help" "fn bad(a f32, b f32) f32 { return a % b }\n" 1 39
    1 "remainder `%` is not defined for floating point"
    (Some "use C `fmod(a, b)` from `<math.h>`");
  semantic_pin "float-compound-remainder-help"
    "fn bad(a f32, b f32) void { a %= b\nreturn }\n" 1 31 2
    "compound assignment `%=` is not defined for `f32`" None;
  semantic_message "float-integer-operands-rejected"
    "operands of `+` have different types: `f32` and `i32`"
    "fn bad(a f32, b i32) f32 { return a + b }\n";
  semantic_message "float-bitwise-operands-rejected"
    "operator `&` needs integer or integer vector operands, got `f32`"
    "fn bad(a f32, b f32) f32 { return a & b }\n";
  semantic_message "float-shift-operands-rejected"
    "left operand of `<<` has type `f32`, expected an integer or integer vector"
    "fn bad(a f32) f32 { return a << 1 }\n";
  let float_arithmetic_llvm =
    llvm_of
      "fn calculate(a f32, b f32, wide f64) f64 {\n\
       neg f32 = -a\n\
       add f32 = a + b\n\
       sub f32 = a - b\n\
       mul f32 = a * b\n\
       div f32 = a / b\n\
       mixed f64 = wide + a\n\
       return mixed\n\
       }\n\
       fn predicates(a f64, b f64) bool {\n\
       return a == b || a != b || a < b || a <= b || a > b || a >= b\n\
       }\n\
       fn fused(a f32, b f32, c f32) f32 { return a * b + c }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun instruction ->
      if not (contains float_arithmetic_llvm instruction) then
        failwith ("float-arithmetic-lowering: missing " ^ instruction))
    [
      "fneg float";
      "fadd float";
      "fsub float";
      "fmul float";
      "fdiv float";
      "fpext float";
      "fcmp oeq double";
      "fcmp une double";
      "fcmp olt double";
      "fcmp ole double";
      "fcmp ogt double";
      "fcmp oge double";
      "fmul float";
    ];
  if contains float_arithmetic_llvm "contract" || contains float_arithmetic_llvm "fast"
  then failwith "float-arithmetic-lowering: emitted a fast-math flag";
  semantic_accept "float-conversion-types"
    "fn convert(value i64, whole u64, single f32, wide f64) f64 {\n\
     as_single f32 = cvt[f32](value)\n\
     as_wide f64 = cvt[f64](whole)\n\
     widened f64 = cvt[f64](single)\n\
     narrowed f32 = cvt[f32](wide)\n\
     signed i32 = cvt[i32](wide)\n\
     unsigned_result u16 = cvt[u16](single)\n\
     return widened + narrowed + as_single + as_wide\n\
     }\n\
     fn to_bits32(value f32) u32 { return bitcast[u32](value) }\n\
     fn from_bits64(value i64) f64 { return bitcast[f64](value) }\n";
  semantic_pin "float-cvt-integer-to-integer"
    "fn bad(x u32) i64 { return cvt[i64](x) }\n" 1 28 11
    "`cvt` does not convert `u32` to `i64`"
    (Some
       "integer width changes use `zext`, `sext`, or `trunc`; same-width values \
        already have their representation");
  semantic_pin "float-cvt-same-type" "fn bad(x f32) f32 { return cvt[f32](x) }\n" 1 28
    11 "`cvt[f32]` of an `f32` value converts nothing" (Some "write `x`");
  semantic_accept "float-cvt-same-type-applied" "fn bad(x f32) f32 { return x }\n";
  semantic_pin "int-cvt-same-type-i32" "fn bad(x i32) i32 { return cvt[i32](x) }\n" 1 28
    11 "`cvt[i32]` of an `i32` value converts nothing" (Some "write `x`");
  semantic_accept "int-cvt-same-type-i32-applied" "fn bad(x i32) i32 { return x }\n";
  semantic_pin "int-cvt-same-type-u8" "fn bad(x u8) u8 { return cvt[u8](x) }\n" 1 26 10
    "`cvt[u8]` of a `u8` value converts nothing" (Some "write `x`");
  semantic_accept "int-cvt-same-type-u8-applied" "fn bad(x u8) u8 { return x }\n";
  semantic_pin "float-cvt-same-type-f64" "fn bad(x f64) f64 { return cvt[f64](x) }\n" 1
    28 11 "`cvt[f64]` of an `f64` value converts nothing" (Some "write `x`");
  semantic_accept "float-cvt-same-type-f64-applied" "fn bad(x f64) f64 { return x }\n";
  semantic_message "float-cvt-bool-source"
    "`cvt` needs an integer or floating-point source and target, got `bool` to `f32`"
    "fn bad(x bool) f32 { return cvt[f32](x) }\n";
  semantic_message "float-cvt-bool-target"
    "`cvt` needs an integer or floating-point source and target, got `f32` to `bool`"
    "fn bad(x f32) bool { return cvt[bool](x) }\n";
  semantic_message "float-cvt-missing-type" "cvt needs one target type: cvt[T](value)"
    "fn bad(x f32) f32 { return cvt[f32, f64](x) }\n";
  let float_conversion_llvm =
    llvm_of
      "fn signed(value i64) f32 { return cvt[f32](value) }\n\
       fn unsigned(value u64) f32 { return cvt[f32](value) }\n\
       fn widen(value f32) f64 { return cvt[f64](value) }\n\
       fn narrow(value f64) f32 { return cvt[f32](value) }\n\
       fn to_signed(value f32) i64 { return cvt[i64](value) }\n\
       fn to_unsigned(value f64) u32 { return cvt[u32](value) }\n\
       fn bits32(value f32) u32 { return bitcast[u32](value) }\n\
       fn bits64(value f64) i64 { return bitcast[i64](value) }\n"
  in
  List.iter
    (fun instruction ->
      if not (contains float_conversion_llvm instruction) then
        failwith ("float-conversion-lowering: missing " ^ instruction))
    [
      "sitofp i64";
      "uitofp i64";
      "fpext float";
      "fptrunc double";
      "@llvm.fptosi.sat.i64.f32";
      "@llvm.fptoui.sat.i32.f64";
      "bitcast float";
      "bitcast double";
    ];
  semantic_constant "FloatSize32" 4L "const FloatSize32 usize = sizeof[f32]\n";
  semantic_constant "FloatAlign32" 4L "const FloatAlign32 usize = alignof[f32]\n";
  semantic_constant "FloatSize64" 8L "const FloatSize64 usize = sizeof[f64]\n";
  semantic_constant "FloatAlign64" 8L "const FloatAlign64 usize = alignof[f64]\n";
  semantic_accept "float-index-condition-and-constant-exclusions-twin"
    "fn use_index(p addr, i usize) f32 { return p[f32, i] }\n\
     fn use_switch(x i32) i32 { switch x { case 1: { return 1 } default: { return 0 } \
     } }\n";
  semantic_message "float-const-generic-type"
    "const parameter type must be a scalar integer or bool"
    "fn bad[N const f32]() i32 { return 0 }\n";
  semantic_message "float-switch-scrutinee"
    "switch value must be an integer or bool, got `f32`"
    "fn bad(x f32) i32 { switch x { case 1: { return 1 } default: { return 0 } } }\n";
  semantic_message "float-switch-case" "floating point literal cannot be used as `i32`"
    "fn bad(x i32) i32 { switch x { case 1.0: { return 1 } default: { return 0 } } }\n";
  semantic_message "float-switch-arithmetic-case"
    "floating point literal cannot be used as `i32`"
    "fn bad(x i32) i32 { switch x { case 2.0 + 1.0: { return 1 } default: { return 0 } \
     } }\n";
  semantic_message "float-array-length"
    "floating point literal cannot be used as `usize`" "var values arr[2.0, i32]\n";
  semantic_message "float-shift-count"
    "right operand of `<<` has type `f32`, expected an integer"
    "fn bad(x i32, n f32) i32 { return x << n }\n";
  semantic_message "float-index" "raw access index must be an integer, got `f32`"
    "fn bad(p addr, n f32) f32 { return p[f32, n] }\n";
  semantic_message "float-condition" "condition of `if` is `f32`, not `bool`"
    "fn bad(x f32) i32 { if x { return 1 }\n return 0 }\n";
  semantic_pin "float-condition-f32-help"
    "fn bad(x f32) i32 { if x { return 1 }\n return 0 }\n" 1 24 1
    "condition of `if` is `f32`, not `bool`"
    (Some "Fas has no implicit truth values; write `x != 0.0`");
  semantic_pin "float-condition-f64-help" "fn bad(x f64) void { while x { break } }\n" 1
    28 1 "condition of `while` is `f64`, not `bool`"
    (Some "Fas has no implicit truth values; write `x != 0.0`");
  semantic_pin "float-logical-and-help" "fn bad(x f32) bool { return x && true }\n" 1 29
    1 "left operand of `&&` is `f32`, not `bool`"
    (Some "Fas has no implicit truth values; write `x != 0.0`");
  semantic_pin "float-logical-or-help" "fn bad(x f64) bool { return true || x }\n" 1 37
    1 "right operand of `||` is `f64`, not `bool`"
    (Some "Fas has no implicit truth values; write `x != 0.0`");
  semantic_pin "float-logical-not-help" "fn bad(x f32) bool { return !x }\n" 1 30 1
    "operator `!` needs `bool` or a bool vector, got `f32`" (Some "write `x == 0.0`");
  semantic_accept "float-condition-help-f32-twin"
    "fn good(x f32) i32 { if x != 0.0 { return 1 }\n\
    \ while x != 0.0 { break }\n\
    \ if (x != 0.0) && (x != 0.0) { return 2 }\n\
    \ if !(x == 0.0) { return 3 }\n\
    \ return 0 }\n";
  semantic_accept "float-condition-help-f64-twin"
    "fn good(x f64) i32 { if x != 0.0 { return 1 }\n\
    \ while x != 0.0 { break }\n\
    \ if (x != 0.0) || (x != 0.0) { return 2 }\n\
    \ if !(x == 0.0) { return 3 }\n\
    \ return 0 }\n";
  semantic_accept "float-math-contextual-operands"
    "const Root f32 = sqrt(2.0)\n\
    \ const Fused f32 = fma(1.0, 2.0, 3.0)\n\
    \ const Down f64 = floor(-0.25)\n\
    \ const Up f64 = ceil(0.25)\n\
    \ const Nearest f64 = round(2.5)\n\
    \ const Magnitude f32 = abs(-1.5)\n\
    \ fn peer(x f32) f32 { return fma(1.0, x, 2.0) }\n\
    \ fn defaults() f64 { return sqrt(2.0) }\n";
  semantic_accept "float-math-unresolved-operands"
    "const A f32 = 0.10000001\n\
     const N f32 = fma(A, A, -(0.5))\n\
     const F f32 = fma(-(0.5), A, A)\n\
     const P f32 = fma(A, A, 0.5 * 2.0)\n\
     const I f32 = fma(A, A, if true { 0.5 } else { 1.0 })\n\
     fn shaped(a f32, c bool) f32 {\n\
     x f32 = fma(a, a, -(0.5))\n\
     y f32 = fma(-(0.5), a, a)\n\
     z f32 = fma(a, a, 0.5 * 2.0)\n\
     w f32 = fma(a, a, if c { 0.5 } else { 1.0 })\n\
     return x + y + z + w }\n";
  semantic_message "float-if-chain-vararg-peer-left"
    "C varargs do not accept `f32`; convert it to `f64` explicitly"
    "extern \"C\" { fn printf(format addr, ...) i32 }\n\
     fn bad(a f32, c bool, d bool) i32 { return printf(c\"%f\", if c { a } else if d { \
     0.2 } else { 0.3 }) }\n";
  semantic_message "float-if-chain-vararg-peer-right"
    "C varargs do not accept `f32`; convert it to `f64` explicitly"
    "extern \"C\" { fn printf(format addr, ...) i32 }\n\
     fn bad(a f32, c bool, d bool) i32 { return printf(c\"%f\", if c { 0.2 } else if d \
     { a } else { 0.3 }) }\n";
  semantic_accept "integer-if-chain-peer-shapes"
    "fn left(a u8, c bool, d bool) u8 { return if c { a } else if d { 2 } else { 3 } }\n\
     fn right(a u8, c bool, d bool) u8 { return if c { 2 } else if d { a } else { 3 } }\n";
  semantic_accept "integer-shift-unresolved-peer-shapes"
    "const X u8 = 5\n\
     const A u8 = (1 << 2) + X\n\
     const B u8 = X + (1 << 2)\n\
     const C u8 = if true { X } else { 1 << 2 }\n\
     fn left(x u8) u8 { return (1 << 2) + x }\n\
     fn right(x u8) u8 { return x + (1 << 2) }\n\
     fn conditional(x u8, c bool) u8 { return if c { x } else { 1 << 2 } }\n";
  let shift_literal_constant_source =
    "const X u8 = 16\n\
     const Y u8 = 0\n\
     const MUL_LEFT bool = (1 << 4) * X > 255\n\
     const MUL_RIGHT bool = X * (1 << 4) > 255\n\
     const ADD_LEFT bool = (1 << 9) + Y == 2\n\
     const ADD_RIGHT bool = Y + (1 << 9) == 2\n"
  in
  semantic_constant "MUL_LEFT" 0L shift_literal_constant_source;
  semantic_constant "MUL_RIGHT" 0L shift_literal_constant_source;
  semantic_constant "ADD_LEFT" 1L shift_literal_constant_source;
  semantic_constant "ADD_RIGHT" 1L shift_literal_constant_source;
  let cached_shift = String.concat " + " ("(1 << 1)" :: List.init 63 (fun _ -> "1")) in
  semantic_accept "integer-shift-unresolved-peer-cached"
    ("fn cached(x u8) u8 { return (" ^ cached_shift ^ ") + x }\n");
  semantic_message "integer-shift-peer-range-left"
    "integer literal is out of range for u8: `256`"
    "fn bad(x u8) u8 { return (256 >> 1) | x }\n";
  semantic_message "integer-shift-peer-range-right"
    "integer literal is out of range for u8: `256`"
    "fn bad(x u8) u8 { return x | (256 >> 1) }\n";
  semantic_accept "generic-dependent-vector-splat-locals"
    "fn float[N const usize](v vec[N, f32]) f32 {\n\
     x vec[N, f32] = v\n\
     y vec[N, f32] = v * splat(2.0)\n\
     x = x * splat(2.0)\n\
     x *= splat(2.0)\n\
     x = splat(3.0)\n\
     return (x * splat(2.0))[0] + y[0] }\n\
     fn integer[N const usize](v vec[N, u32]) u32 {\n\
     x vec[N, u32] = v\n\
     x += splat(2)\n\
     return x[0] }\n\
     fn main() i32 {\n\
     f vec[4, f32] = splat(1.0)\n\
     i vec[4, u32] = splat(1)\n\
     a f32 = float[4](f)\n\
     b u32 = integer[4](i)\n\
     return cvt[i32](a) + bitcast[i32](b) }\n";
  semantic_accept "generic-dependent-vector-brace-locals"
    "fn parameter[N const usize](v vec[N, f32]) f32 { return (v * {1.0, 2.0, 3.0, \
     4.0})[3] }\n\
     fn local[N const usize](v vec[N, f32]) f32 { x vec[N, f32] = v\n\
     y f32 = (x * {1.0, 2.0, 3.0, 4.0})[3]\n\
     x *= {1.0, 2.0, 3.0, 4.0}\n\
     return y + x[3] }\n\
     fn fixed(v vec[4, f32]) f32 { x vec[4, f32] = v\n\
     y f32 = (x * {1.0, 2.0, 3.0, 4.0})[3]\n\
     x *= {1.0, 2.0, 3.0, 4.0}\n\
     return y + x[3] }\n\
     fn main() i32 { v vec[4, f32] = {2.0, 3.0, 4.0, 5.0}\n\
     a f32 = parameter[4](v)\n\
     b f32 = local[4](v)\n\
     c f32 = fixed(v)\n\
     return cvt[i32](a + b + c) }\n";
  semantic_accept "generic-const-struct-literal-body"
    "struct H[N const usize] { v vec[N, f32] }\n\
     fn wrapped[N const usize](v vec[N, f32]) f32 { l H[N] = {v}\n\
     l.v *= {1.0, 2.0, 3.0, 4.0}\n\
     return l.v[0] }\n\
     fn fixed(v vec[4, f32]) f32 { l H[4] = {v}\n\
     return l.v[0] }\n\
     fn main() i32 { v vec[4, f32] = {1.0, 2.0, 3.0, 4.0}\n\
     return cvt[i32](wrapped[4](v) + fixed(v)) }\n";
  semantic_accept "dependent-vector-field-assignment"
    "struct H[K const usize] { v vec[K, u32] }\n\
     struct O[M const usize] { inner H[M] }\n\
     struct H2[N const usize, T] { v vec[N, T] }\n\
     fn nested[N const usize](v vec[N, u32]) u32 { o O[N]\n\
     o.inner.v = v\n\
     return o.inner.v[3] }\n\
     fn generic[N const usize, T](v vec[N, T]) T { o H2[N, T]\n\
     o.v = v\n\
     return o.v[3] }\n\
     fn direct[N const usize](v vec[N, u32]) u32 { o vec[N, u32]\n\
     o = v\n\
     return o[3] }\n\
     fn main() i32 { v vec[4, u32] = {2, 3, 4, 5}\n\
     return bitcast[i32](nested[4](v) + generic[4, u32](v) + direct[4](v)) }\n";
  semantic_accept "const-only-generic-struct-locals"
    "struct Box[K const usize] { v vec[K, u8] }\n\
     struct G[T] { v T }\n\
     fn box_only[M const usize]() usize { b Box[8]\n\
    \ return M }\n\
     fn box_twin[M const usize, T]() usize { b Box[8]\n\
    \ return M }\n\
     fn generic_only[M const usize]() usize { b G[u8]\n\
    \ b.v = 3\n\
    \ return M + zext[usize](b.v) }\n\
     fn generic_twin[M const usize, T]() usize { b G[u8]\n\
    \ b.v = 3\n\
    \ return M + zext[usize](b.v) }\n\
     fn main() i32 { return trunc[i32](box_only[4]() + box_twin[4, u8]() + \
     generic_only[4]() + generic_twin[4, u8]()) }\n";
  semantic_accept "const-only-generic-inference"
    "fn id[T](a T) T { return a }\n\
     fn only[M const usize]() usize { y u8 = 3\n\
    \ return M + zext[usize](id(y)) }\n\
     fn twin[M const usize, T]() usize { y u8 = 3\n\
    \ return M + zext[usize](id(y)) }\n\
     fn main() i32 { return trunc[i32](only[4]() + twin[4, u8]()) }\n";
  semantic_message "generic-dependent-vector-brace-width"
    "wrong number of vector literal lanes"
    "fn local[N const usize](v vec[N, f32]) void { x vec[N, f32] = v\n\
     x *= {1.0, 2.0, 3.0}\n\
     return }\n\
     fn main() i32 { v vec[4, f32] = {1.0, 2.0, 3.0, 4.0}\n\
     local[4](v)\n\
     return 0 }\n";
  semantic_accept "generic-inference-globals-and-generic-fields"
    "struct S { x u32 }\n\
     struct G[T] { v T }\n\
     var glob u32 = 1\n\
     var ga arr[2, u32] = {}\n\
     var gs S = {}\n\
     fn pick[T](a T, b T, mode bool) T { return if mode { a } else { b } }\n\
     fn infer() u32 {\n\
     x u32 = pick(glob, glob, true)\n\
     y u32 = pick(ga[0], 1, true)\n\
     z u32 = pick(gs.x, gs.x, true)\n\
     s G[u32] = {}\n\
     q u32 = pick(s.v, s.v, true)\n\
     return x + y + z + q }\n\
     fn main() i32 { return bitcast[i32](infer()) }\n";
  semantic_accept "float-fma-vector-splat-peers"
    "fn fused(a vec[4, f32]) vec[4, f32] {\n\
     x vec[4, f32] = fma(splat(2.0), a, a)\n\
     y vec[4, f32] = fma(a, splat(2.0), a)\n\
     return fma(a, a, splat(-0.0))\n\
     }\n\
     fn generic[N const usize](a vec[N, f32]) f32 {\n\
     return (a * splat(2.0))[0]\n\
     }\n\
     fn main() i32 { a vec[4, f32] = splat(1.0)\n\
     x f32 = generic[4](a)\n\
     return 0\n\
     }\n";
  semantic_pin "float-math-integer-help" "fn bad(x i32) f64 { return sqrt(x) }\n" 1 33 1
    "argument 1 of `sqrt` is `i32`, expected `f64`" (Some "write `sqrt(cvt[f64](x))`");
  semantic_accept "float-math-integer-help-twin"
    "fn good(x i32) f64 { return sqrt(cvt[f64](x)) }\n";
  semantic_pin "float-math-integer-literal-help" "fn bad() f32 { return floor(2) }\n" 1
    29 1 "argument 1 of `floor` is `i32`, expected `f32`" (Some "write `floor(2.0)`");
  semantic_accept "float-math-integer-literal-help-twin"
    "fn good() f32 { return floor(2.0) }\n";
  semantic_pin "float-math-hex-integer-literal-help"
    "fn bad() f32 { return floor(0xff) }\n" 1 29 4
    "argument 1 of `floor` is `i32`, expected `f32`"
    (Some "write `floor(cvt[f32](0xff))`");
  semantic_pin "float-math-binary-integer-literal-help"
    "fn bad() f32 { return floor(0b10) }\n" 1 29 4
    "argument 1 of `floor` is `i32`, expected `f32`"
    (Some "write `floor(cvt[f32](0b10))`");
  semantic_pin "float-math-octal-integer-literal-help"
    "fn bad() f32 { return floor(0o10) }\n" 1 29 4
    "argument 1 of `floor` is `i32`, expected `f32`"
    (Some "write `floor(cvt[f32](0o10))`");
  semantic_accept "float-math-based-integer-literal-help-twin"
    "fn good() f32 { return floor(cvt[f32](0xff)) + floor(cvt[f32](0b10)) + \
     floor(cvt[f32](0o10)) }\n";
  semantic_pin "float-abs-integer-result-help" "fn bad(x i32) i32 { return abs(x) }\n" 1
    32 1 "argument 1 of `abs` is `i32`, expected `f64`" None;
  semantic_pin "float-f64-to-f32-help" "fn bad(x f64) f32 { return x }\n" 1 28 1
    "cannot return `f64` as `f32`: floating-point widths need explicit conversion"
    (Some "write `cvt[f32](x)`");
  semantic_accept "float-f64-to-f32-help-twin"
    "fn good(x f64) f32 { return cvt[f32](x) }\n";
  semantic_pin "float-f32-to-i32-help" "fn bad(x f32) i32 { return x }\n" 1 28 1
    "cannot return `f32` as `i32`: floating-point values need explicit conversion to \
     integer"
    (Some "write `cvt[i32](x)`");
  semantic_accept "float-f32-to-i32-help-twin"
    "fn good(x f32) i32 { return cvt[i32](x) }\n";
  semantic_pin "float-fma-integer-help"
    "fn bad(a i32, b f32, c f32) f32 { return fma(a, b, c) }\n" 1 46 1
    "argument 1 of `fma` is `i32`, expected `f32`"
    (Some "write `fma(cvt[f32](a), b, c)`");
  semantic_accept "float-fma-integer-help-twin"
    "fn good(a i32, b f32, c f32) f32 { return fma(cvt[f32](a), b, c) }\n";
  semantic_message "float-fma-operands-must-match"
    "argument 2 of `fma` is `f64`, expected `f32`"
    "fn bad(a f32, b f64, c f32) f32 { return fma(a, b, c) }\n";
  semantic_constant "FloatSqrt32" 0x3fb504f3L "const FloatSqrt32 f32 = sqrt(2.0)\n";
  semantic_constant "FloatSqrt64" 0x3ff6a09e667f3bcdL
    "const FloatSqrt64 f64 = sqrt(2.0)\n";
  semantic_constant "FloatRoundTiesEven32" 0x40000000L
    "const FloatRoundTiesEven32 f32 = round(2.5)\n";
  semantic_constant "FloatRoundNegativeHalf32" 0x80000000L
    "const FloatRoundNegativeHalf32 f32 = round(-0.5)\n";
  semantic_constant "FloatFloor32" 0xbf800000L "const FloatFloor32 f32 = floor(-0.25)\n";
  semantic_constant "FloatCeil32" 0x80000000L "const FloatCeil32 f32 = ceil(-0.25)\n";
  semantic_constant "FloatAbsNan32" 0x7fc12345L
    "const FloatAbsNan32 f32 = abs(bitcast[f32](bitcast[u32](0x7fc12345) | 0x80000000))\n";
  semantic_constant "FloatFmaSingleRound32" 0x670239a1L
    "const A u32 = 0xd0300000\n\
    \ const B u32 = 0xd63d6b18\n\
    \ const C u32 = 0x10948057\n\
    \ const FloatFmaSingleRound32 f32 = fma(bitcast[f32](A), bitcast[f32](B), \
     bitcast[f32](C))\n";
  semantic_constant "FloatFmaSingleRound32Second" 0xe1834a61L
    "const A u32 = 0xe36967c8\n\
    \ const B u32 = 0x3d900000\n\
    \ const C u32 = 0x8f93c7c8\n\
    \ const FloatFmaSingleRound32Second f32 = fma(bitcast[f32](A), bitcast[f32](B), \
     bitcast[f32](C))\n";
  let signaling_nan_llvm =
    llvm_of
      "const N f32 = bitcast[f32](0x7f800001)\n\
       var G f32 = N\n\
       fn value() f32 { return N }\n"
  in
  if not (contains signaling_nan_llvm "float bitcast (i32 2139095041 to float)") then
    failwith "float-signaling-nan-constant-bits";
  let float_math_llvm =
    llvm_of
      "fn unary32(x f32) f32 { return sqrt(x) + floor(x) + ceil(x) + round(x) + abs(x) }\n\
      \ fn unary64(x f64) f64 { return sqrt(x) + floor(x) + ceil(x) + round(x) + \
       abs(x) }\n\
      \ fn fused32(a f32, b f32, c f32) f32 { return fma(a, b, c) }\n\
      \ fn fused64(a f64, b f64, c f64) f64 { return fma(a, b, c) }\n"
  in
  List.iter
    (fun name ->
      if not (contains float_math_llvm name) then
        failwith ("float-math-lowering: missing " ^ name))
    [
      "@llvm.sqrt.f32";
      "@llvm.sqrt.f64";
      "@llvm.fma.f32";
      "@llvm.fma.f64";
      "@llvm.floor.f32";
      "@llvm.ceil.f32";
      "@llvm.roundeven.f32";
      "@llvm.fabs.f32";
      "@llvm.floor.f64";
      "@llvm.ceil.f64";
      "@llvm.roundeven.f64";
      "@llvm.fabs.f64";
    ];
  if contains float_math_llvm "llvm.fmuladd" then
    failwith "float-math-lowering: fma must not use llvm.fmuladd";
  semantic_pin "float-integer-literal-help" "fn bad(x f32) f32 { return x * 2 }\n" 1 32
    1 "integer literals are not floating point literals" (Some "write `2.0`");
  semantic_pin "float-negative-integer-literal-help"
    "fn bad(x f32) f32 { return x + -2 }\n" 1 32 1
    "integer literals are not floating point literals" (Some "write `-2.0`");
  semantic_message "float-rounds-to-infinity"
    "floating point literal rounds to infinity for `f32`" "const X f32 = 1e39\n";
  semantic_message "float-nonzero-rounds-to-zero"
    "nonzero floating point literal rounds to zero for `f32`" "const X f32 = 1e-50\n";
  semantic_message "float64-rounds-to-infinity"
    "floating point literal rounds to infinity for `f64`" "const X f64 = 1e309\n";
  semantic_message "float64-nonzero-rounds-to-zero"
    "nonzero floating point literal rounds to zero for `f64`" "const X f64 = 1e-400\n";
  parse_pin "float-literal-trailing-dot" "fn bad() f32 { return 1. }\n" 1 23 1
    "floating point literals need digits after `.`" "write `1.0`";
  parse_pin "float-literal-leading-dot" "fn bad() f32 { return .5 }\n" 1 23 1
    "floating point literals need a leading zero" "write `0.5` with a leading zero";
  parse_pin "float-literal-f-suffix" "fn bad() f32 { return 1.0f }\n" 1 23 1
    "floating point literals do not have suffixes" "write `1.0` without a suffix";
  parse_pin "float-literal-d-suffix" "fn bad() f64 { return 1.5d }\n" 1 23 1
    "floating point literals do not have suffixes" "write `1.5` without a suffix";
  let float_llvm =
    llvm_of
      "const A f32 = 0.1\n\
       fn default_float() f64 { return 1.5 }\n\
       fn float_constant() f32 { return A }\n\
       fn main() i32 { return 0 }\n"
  in
  if not (contains float_llvm "float 0x3FB99999A0000000") then
    failwith "float-literal-f32-rounding: expected a binary32 LLVM constant";
  if not (contains float_llvm "double 0x3FF8000000000000") then
    failwith "float-literal-default-type: expected a binary64 result";
  let released_unreserved_names =
    llvm_of
      "struct Members { len i32 i32 i32 true i32 }\n\
       fn main() i32 { value Members = {1, 2, 3}\n\
       return value.len + value.i32 + value.true }\n"
  in
  if not (contains released_unreserved_names "%struct.Members = type") then
    failwith "released-unreserved-name: member labels stopped compiling";
  semantic_accept "labeled-break-accept"
    "fn f() i32 { value i32\n\
     outer: while true { while true { value = 9\n\
    \ break outer } }\n\
     return value }\n";
  semantic_accept "labeled-continue-accept"
    "fn f() void { outer: for i i32 = 0; i < 2; i += 1 {\n\
     while true { continue outer }\n\
     } }\n";
  semantic_accept "labeled-break-across-switch"
    "fn f() i32 { value i32\n\
     outer: while true { switch 0 { default: { value = 7\n\
    \ break outer } } }\n\
     return value }\n";
  semantic_accept "labeled-loop-shadow-twin"
    "fn f() void { outer: while true { inner: for ; ; { break inner } } }\n";
  semantic_message "labeled-break-unknown" "unknown loop label `missing`"
    "fn f() void { break missing }\n";
  semantic_message "labeled-continue-unknown" "unknown loop label `missing`"
    "fn f() void { continue missing }\n";
  semantic_message "labeled-break-not-enclosing"
    "`break outer` is not inside the loop labeled `outer`"
    "fn f() void { outer: while false { }\n while true { break outer } }\n";
  semantic_note "labeled-break-not-enclosing-note"
    "loop label `outer` is declared here at regression.fas:1:15"
    "fn f() void { outer: while false { }\n while true { break outer } }\n";
  semantic_message "labeled-continue-not-enclosing"
    "`continue outer` is not inside the loop labeled `outer`"
    "fn f() void { outer: while false { }\n while true { continue outer } }\n";
  semantic_message "labeled-loop-shadow"
    "loop label `outer` shadows an enclosing loop label"
    "fn f() void { outer: while true { outer: for ; ; { break } } }\n";
  parse_message "labeled-non-loop-statement"
    "loop label `block` must precede a `while` or `for` statement"
    "fn f() void { block: { } }\n";

  let hygienic_parameter_names =
    llvm_of
      "fn value_collision(v0 i32) i32 { return v0 }\n\
       fn block_collision(b0 i32) i32 { if b0 == 0 { return 1 } else { return b0 } }\n\
       fn main() i32 { return value_collision(7) + block_collision(2) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains hygienic_parameter_names marker) then
        failwith ("hygienic-parameter-names: missing `" ^ marker ^ "`"))
    [
      "define internal i32 @value_collision(i32 %a0)";
      "define internal i32 @block_collision(i32 %a0)";
      "%v0 = alloca i32";
      "b0:";
    ];
  if
    contains hygienic_parameter_names "@value_collision(i32 %v0)"
    || contains hygienic_parameter_names "@block_collision(i32 %b0)"
  then failwith "hygienic-parameter-names: source name escaped into LLVM identity";

  (match
     Parser.parse
       (source
          "fn main() i32 {\nswitch 0 {\ndefault:\nreturn 1\ndefault:\nreturn 2\n}\n}\n")
   with
  | Ok _ -> failwith "duplicate-default: expected parse rejection"
  | Error [ diagnostic ] ->
      if diagnostic.Diag.message <> "duplicate default arm" then
        failwith "duplicate-default: unexpected message";
      if diagnostic.primary.Span.line <> 5 then
        failwith "duplicate-default: wrong primary location";
      if
        not
          (List.exists (fun note -> contains note "regression.fas:3:") diagnostic.notes)
      then failwith "duplicate-default: first location missing"
  | Error _ -> failwith "duplicate-default: unexpected diagnostic count");

  let leading_zero_literals =
    llvm_of
      "fn decimal() u64 { return 000000000000000000001 }\n\
       fn hexadecimal() u64 { return 0x000000000000000000001 }\n\
       fn octal() u64 { return 0o000000000000000000001 }\n\
       fn binary() u64 { return 0b000000000000000000001 }\n\
       fn separated() u64 { return 0000_0000_0000_0001 }\n\
       fn zero() u64 { return 000000000000000000000 }\n\
       fn decimal_max() u64 { return 00018446744073709551615 }\n\
       fn hexadecimal_max() u64 { return 0x0000FFFFFFFFFFFFFFFF }\n\
       fn octal_max() u64 { return 0o00001777777777777777777777 }\n\
       fn binary_max() u64 { return \
       0b00001111111111111111111111111111111111111111111111111111111111111111 }\n\
       fn signed_minimum() i64 { return -00009223372036854775808 }\n"
  in
  if List.length (positions leading_zero_literals "ret i64 1\n") <> 5 then
    failwith "leading-zero-literals: nonzero values were not normalized";
  if not (contains leading_zero_literals "ret i64 0\n") then
    failwith "leading-zero-literals: zero-only spelling was rejected";
  if List.length (positions leading_zero_literals "ret i64 -1\n") <> 4 then
    failwith "leading-zero-literals: padded maximum values were rejected";
  if not (contains leading_zero_literals "ret i64 -9223372036854775808\n") then
    failwith "leading-zero-literals: padded signed minimum was rejected";
  List.iter
    (fun (name, literal) ->
      semantic_error name "integer literal overflows 64 bits"
        (Printf.sprintf "fn value() u64 { return %s }\n" literal))
    [
      ("leading-zero-decimal-overflow", "00018446744073709551616");
      ("leading-zero-hex-overflow", "0x000100000000000000000");
      ("leading-zero-octal-overflow", "0o00002000000000000000000000");
      ( "leading-zero-binary-overflow",
        "0b000010000000000000000000000000000000000000000000000000000000000000000" );
    ];

  let spec_count_limits = { Limits.default with max_specializations = 1 } in
  let repeated_spec_at_count_limit =
    expect_ok
      (Sema.check ~limits:spec_count_limits
         (expect_ok
            (Parser.parse
               (source
                  "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
                   fn test() u64 { return id[3](2) + id[1 + 2](3) }\n"))))
  in
  if List.length repeated_spec_at_count_limit.Hir.funcs <> 2 then
    failwith "spec-count-limit: repeated specialization was not deduplicated";
  let distinct_spec_at_count_limit =
    expect_ok
      (Parser.parse
         (source
            "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
             fn test() u64 { return id[3](2) + id[4](3) }\n"))
  in
  let count_limit_failure () =
    match Sema.check ~limits:spec_count_limits distinct_spec_at_count_limit with
    | Ok _ -> failwith "spec-count-limit: expected rejection at the count limit"
    | Error diagnostics -> Diag.render_all ~source:None diagnostics
  in
  let first_count_limit_failure = count_limit_failure () in
  let second_count_limit_failure = count_limit_failure () in
  if first_count_limit_failure <> second_count_limit_failure then
    failwith "spec-count-limit: exhaustion diagnostic was not deterministic";
  ignore
    (expect_ok
       (Sema.check ~limits:spec_count_limits
          (expect_ok
             (Parser.parse
                (source
                   "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
                    fn test() u64 { return id[3](2) + id[3](3) }\n")))));
  if
    not (contains first_count_limit_failure "const specialization count limit exceeded")
  then failwith "spec-count-limit: unexpected diagnostic";

  let spec_depth_limits = { Limits.default with max_specialization_depth = 1 } in
  let repeated_spec_at_depth_limit =
    expect_ok
      (Sema.check ~limits:spec_depth_limits
         (expect_ok
            (Parser.parse
               (source
                  "fn loop[N const i64]() i64 { return loop[N]() }\n\
                   fn test() i64 { return loop[5]() }\n"))))
  in
  if List.length repeated_spec_at_depth_limit.Hir.funcs <> 2 then
    failwith "spec-depth-limit: repeated specialization was not deduplicated";
  (match
     Sema.check ~limits:spec_depth_limits
       (expect_ok
          (Parser.parse
             (source
                "fn inner[N const i64]() i64 { return N }\n\
                 fn outer[M const i64]() i64 { return inner[M]() }\n\
                 fn test() i64 { return outer[5]() }\n")))
   with
  | Ok _ -> failwith "spec-depth-limit: expected rejection at the depth limit"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "const specialization recursion depth limit exceeded")
      then failwith "spec-depth-limit: unexpected diagnostic");
  let specialization_order_source =
    "fn leaf[N const usize]() usize { return N }\n\
     fn left[N const usize]() usize { return leaf[N + 10]() }\n\
     fn right[N const usize]() usize { return leaf[N + 20]() }\n\
     fn test() usize { return left[1]() + right[2]() }\n"
  in
  let specialization_order = llvm_of specialization_order_source in
  if specialization_order <> llvm_of specialization_order_source then
    failwith "specialization-order: generated output was not deterministic";
  let markers =
    [
      "define internal i64 @\"left$spec$4:1:N=usize:1\"()";
      "define internal i64 @\"right$spec$5:1:N=usize:2\"()";
      "define internal i64 @\"leaf$spec$4:1:N=usize:11\"()";
      "define internal i64 @\"leaf$spec$4:1:N=usize:22\"()";
    ]
  in
  let marker_positions =
    List.map
      (fun marker ->
        match positions specialization_order marker with
        | position :: _ -> position
        | [] -> failwith ("specialization-order: missing `" ^ marker ^ "`"))
      markers
  in
  if marker_positions <> List.sort compare marker_positions then
    failwith "specialization-order: work queue did not preserve discovery order";
  let bool_const_generic_llvm =
    llvm_of
      "const Enabled bool = true\n\
       fn choose[Flag const bool]() i32 {\n\
      \ if Flag { return 7 } else { return 3 }\n\
       }\n\
       fn byte[N const u8]() i32 { return zext[i32](N) }\n\
       fn main() i32 {\n\
      \ return choose[true]() + choose[false]() + choose[Enabled]() \
       +choose[trunc[bool](3)]() + byte[zext[u8](true)]()\n\
       }\n"
  in
  if
    (not
       (contains bool_const_generic_llvm
          "define internal i32 @\"choose$spec$6:4:Flag=bool:1\"()"))
    || not
         (contains bool_const_generic_llvm
            "define internal i32 @\"choose$spec$6:4:Flag=bool:0\"()")
  then failwith "const-param-bool: typed specializations were not emitted";
  if contains bool_const_generic_llvm "br i1" then
    failwith "const-param-bool: specialization condition was not pruned";
  semantic_error "const-param-bool-named-integer"
    "cannot use `u8` as `bool`: bool and integer values use different representations"
    "const One u8 = 1\n\
     fn choose[Flag const bool]() i32 { if Flag { return 1 } else { return 0 } }\n\
     fn main() i32 { return choose[One]() }\n";
  semantic_error "const-param-bool-literal-range"
    "integer literal is not a `bool`; write `true` or `false`"
    "fn choose[Flag const bool]() i32 { if Flag { return 1 } else { return 0 } }\n\
     fn main() i32 { return choose[2]() }\n";
  semantic_error "const-param-integer-bool-value"
    "cannot use `bool` as `u8`: bool and integer values use different representations"
    "fn byte[N const u8]() i32 { return zext[i32](N) }\n\
     fn main() i32 { return byte[true]() }\n";

  semantic_error "const-param-pointer-rejected"
    "const parameter type must be a scalar integer or bool"
    "fn id[N const addr](x u64) u64 { return x }\n";

  semantic_error "const-param-duplicate" "duplicate generic parameter `N`"
    "fn id[N const usize, N const usize](x u64) u64 { return x + N }\n";
  semantic_error "runtime-parameter-duplicate" "duplicate parameter `value`"
    "fn id(value i32, value i32) i32 { return value }\n";
  semantic_error "type-runtime-parameter-collision"
    "parameter `T` conflicts with a generic parameter"
    "fn id[T](T i32) i32 { return T }\n";
  semantic_error "const-runtime-parameter-collision"
    "parameter `N` conflicts with a generic parameter"
    "fn id[N const i32](N i32) i32 { return N }\n";
  ignore
    (lower_of
       "fn id[T](value T) T { { T i32 = 1 }\n\
       \ return value }\n\
        fn main() i32 { return id[i32](7) }\n");

  let generic_declarations =
    expect_ok
      (Parser.parse
         (source
            "struct Buffer[T, N const usize] { data arr[N, T] }\n\
             fn choose[T, N const usize, U](value T) U { return value }\n"))
  in
  (match generic_declarations.Ast.items with
  | [
   Ast.Struct
     {
       generic_params =
         [
           Ast.Type_param { name = "T"; _ };
           Ast.Const_param { name = "N"; ty = Ast.Int Ast.Usize; _ };
         ];
       fields = [ { ty = Ast.Array ({ text = "N"; _ }, Ast.Named_type ("T", _)); _ } ];
       _;
     };
   Ast.Func
     {
       generic_params =
         [
           Ast.Type_param { name = "T"; _ };
           Ast.Const_param { name = "N"; ty = Ast.Int Ast.Usize; _ };
           Ast.Type_param { name = "U"; _ };
         ];
       params = [ { ty = Ast.Named_type ("T", _); _ } ];
       ret = Ast.Named_type ("U", _);
       _;
     };
  ] ->
      ()
  | _ -> failwith "type-generic-declarations: parameter kinds or order were lost");
  let rendered_generic_declarations = Ast.render_program generic_declarations in
  if
    (not (contains rendered_generic_declarations "struct Buffer[T, N const usize]"))
    || not (contains rendered_generic_declarations "fn choose[T, N const usize, U]")
  then failwith "type-generic-declarations: AST rendering lost generic parameters";
  let applied_type_syntax =
    expect_ok
      (Parser.parse
         (source "fn check_case(value Mixed[T, addr, 3]) i64 { return 0 }\n"))
  in
  (match applied_type_syntax.Ast.items with
  | [
   Ast.Func
     {
       params =
         [
           {
             ty =
               Ast.Applied_type
                 ( "Mixed",
                   _,
                   [
                     Ast.Name_arg ("T", _);
                     Ast.Type_arg Ast.Addr;
                     Ast.Const_arg (Ast.Int_lit ("3", _));
                   ],
                   _ );
             _;
           };
         ];
       _;
     };
  ] ->
      ()
  | _ -> failwith "generic-argument-syntax: argument forms were not preserved");
  parse_error_message "type-generic-missing-const-type" "expected a type, found `]`"
    "fn bad[T const](value T) T { return value }\n";
  let generic_function_source =
    "fn check_case() i64 { return identity[i64](identity[i64](7)) }\n\
     fn identity[T](value T) T { return value }\n"
  in
  let generic_function_hir =
    expect_ok (Parser.parse (source generic_function_source)) |> Sema.check |> expect_ok
  in
  let identity_specializations =
    List.filter
      (fun (func : Hir.func) ->
        contains func.name "identity$spec$" && func.name <> "identity")
      generic_function_hir.Hir.funcs
  in
  if List.length identity_specializations <> 1 then
    failwith "generic-function-deduplication: expected one concrete identity function";
  (match identity_specializations with
  | [ { params = [ { ty = Hir.Int Hir.I64; _ } ]; ret = Hir.Int Hir.I64; _ } ] -> ()
  | _ -> failwith "generic-function-substitution: signature was not specialized");
  let generic_function_llvm =
    Ir.render (expect_ok (Lower.lower generic_function_hir))
  in
  if generic_function_llvm <> llvm_of generic_function_source then
    failwith "generic-function-order: generated output was not deterministic";
  if contains generic_function_llvm "define internal i64 @identity(" then
    failwith "generic-function-template: template reached LLVM output";
  if not (contains generic_function_llvm "identity$spec$") then
    failwith "generic-function-lowering: concrete specialization did not reach LLVM";
  ignore
    (expect_ok
       (Sema.check
          (expect_ok
             (Parser.parse
                (source
                   "fn first[A, B](left A, right B) A { return left }\n\
                    fn test() i64 { return first[i64, u8](7, 1) }\n")))));
  let nested_generic_function_source =
    "struct Box[T] { value T }\n\
     fn inner[T](value T) T { result Box[T] = {value}\n\
     return result.value }\n\
     fn outer[T](value T) T { return inner[T](value) }\n\
     fn check_case() u8 { return outer[u8](3) }\n"
  in
  let nested_generic_function_hir =
    expect_ok (Parser.parse (source nested_generic_function_source))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "$spec$")
         nested_generic_function_hir.Hir.funcs)
    <> 2
  then failwith "generic-function-nesting: nested specialization was not discovered";
  if
    List.length
      (List.filter
         (fun (definition : Hir.struct_def) -> contains definition.name "Box$spec$")
         nested_generic_function_hir.Hir.structs)
    <> 1
  then
    failwith
      "generic-function-struct-use: specialized body did not materialize its struct";
  let nested_generic_argument_hir =
    expect_ok
      (Parser.parse
         (source
            "fn pick[T, N const usize](v T) T { return v }\n\
             fn test() i64 { return pick[i64, 2](pick[i64, 1](7)) }\n"))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "$spec$")
         nested_generic_argument_hir.Hir.funcs)
    <> 2
  then failwith "nested-generic-argument: mixed nested call specializations incorrect";
  ignore (Lower.lower nested_generic_argument_hir |> expect_ok);
  let nested_conversion_argument_hir =
    expect_ok
      (Parser.parse
         (source
            "fn pick[T, N const usize](v T) T { return v }\n\
             fn test() i64 { return pick[i64, 2](zext[i64](pick[u8, 1](3))) }\n"))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "$spec$")
         nested_conversion_argument_hir.Hir.funcs)
    <> 2
  then
    failwith "nested-generic-argument: nested conversion call specializations incorrect";
  ignore (Lower.lower nested_conversion_argument_hir |> expect_ok);
  let nested_type_argument =
    llvm_of
      "struct Box[T] { value T }\n\
       fn sz[T]() usize { return sizeof[T] }\n\
       fn test() usize { return sz[Box[u8]]() }\n"
  in
  if not (contains nested_type_argument "ret i64 1\n") then
    failwith
      "nested-type-argument: sizeof over nested generic type argument was not evaluated";
  let sizeof_const_argument =
    llvm_of
      "fn ret[N const usize]() usize { return N }\n\
       fn test() usize { return ret[sizeof[u8]]() }\n"
  in
  if not (contains sizeof_const_argument "ret i64 1\n") then
    failwith "sizeof-const-argument: nested sizeof query was not evaluated";
  let alignof_const_argument =
    llvm_of
      "fn ret[N const usize]() usize { return N }\n\
       fn test() usize { return ret[1 + alignof[u16]]() }\n"
  in
  if not (contains alignof_const_argument "ret i64 3\n") then
    failwith "alignof-const-argument: nested alignof expression was not evaluated";
  let nested_sizeof_const_argument =
    llvm_of
      "struct Box[T] { value T }\n\
       fn ret[N const usize]() usize { return N }\n\
       fn test() usize { return ret[sizeof[Box[i64]]]() }\n"
  in
  if not (contains nested_sizeof_const_argument "ret i64 8\n") then
    failwith
      "nested-sizeof-const-argument: sizeof over nested generic type was not evaluated";
  let const_generic_layout_argument =
    llvm_of
      "struct Sized[T, N const usize] { data arr[N, T] }\n\
       fn ret[N const usize]() usize { return N }\n\
       fn test() usize { return ret[sizeof[Sized[u8, 2]]]() }\n"
  in
  if not (contains const_generic_layout_argument "ret i64 2\n") then
    failwith
      "const-generic-layout-argument: sizeof over a generic layout was not evaluated";
  let arithmetic_sizeof_const_argument =
    llvm_of
      "fn ret[N const usize]() usize { return N }\n\
       fn test() usize { return ret[sizeof[arr[3, u8]] - 2]() }\n"
  in
  if not (contains arithmetic_sizeof_const_argument "ret i64 1\n") then
    failwith
      "arithmetic-sizeof-const-argument: nested query arithmetic was not evaluated";
  semantic_error "const-argument-call-rejected"
    "call to `sz` is not a constant expression"
    "fn sz[T]() usize { return sizeof[T] }\n\
     fn pick[T, N const usize](v T) T { return v }\n\
     fn test() i64 { return pick[i64, sz[u8]()](7) }\n";
  let while_parameter_shadow =
    llvm_of
      "const N usize = 99\n\
       fn count[N const usize]() usize { i usize = 0\n\
      \ while i < N { i = i + 1 }\n\
      \ return i }\n\
       fn test() usize { return count[3]() }\n"
  in
  if not (contains while_parameter_shadow "icmp ult i64 %v1, 3\n") then
    failwith "while-const-argument: while condition did not use the const parameter";
  if contains while_parameter_shadow "99" then
    failwith "while-const-argument: while condition resolved the shadowing global";
  let defer_parameter_shadow =
    llvm_of
      "const N usize = 99\n\
       fn f[N const usize]() usize { defer { x usize = N\n\
      \ x = x + 1 }\n\
      \ return N }\n\
       fn test() usize { return f[2]() }\n"
  in
  if not (contains defer_parameter_shadow "store i64 2, ptr") then
    failwith "defer-const-argument: defer body did not use the const parameter";
  if contains defer_parameter_shadow "99" then
    failwith "defer-const-argument: defer body resolved the shadowing global";
  let statement_positions =
    llvm_of
      "const N usize = 99\n\
       fn walk[N const usize](p usize) usize { i usize = 0\n\
      \ for j usize = 0; j < N; j += 1 { i = i + N }\n\
      \ while i < N * 4 { i = i + 1 }\n\
      \ switch N { case 1: { i = i + N } case 2: { i = i + N } default: { i = 0 } }\n\
      \ if N > 1 { i = i + N }\n\
      \ defer { i = i + N }\n\
      \ i = i + N\n\
      \ return i }\n\
       fn test() usize { return walk[2](0) }\n"
  in
  if not (contains statement_positions "switch i64 2, label") then
    failwith "statement-positions: switch scrutinee did not use the const parameter";
  if not (contains statement_positions "icmp ult i64 %v3, 2\n") then
    failwith "statement-positions: for bound did not use the const parameter";
  if not (contains statement_positions "N=usize:2\"") then
    failwith "statement-positions: specialization key lost const argument identity";
  if contains statement_positions "99" then
    failwith "statement-positions: a statement position resolved the shadowing global";
  let local_index_shadow =
    "fn read[T](runtime_index usize) i32 { values arr[4,i32] = {11,22,33,44}\n\
    \ { T usize = runtime_index\n\
    \ return values[T] } }\n\
     fn main() i32 { return read[u8](1) }\n"
  in
  semantic_accept "generic-local-index-shadow" local_index_shadow;
  semantic_pin "generic-local-raw-type-shadow"
    "fn load[T](p addr, runtime_index usize) T {\n\
    \ { T usize = runtime_index\n\
    \ return p[T] } }\n\
     fn main() i32 { bytes arr[2,u8] = {11,22}\n\
    \ return zext[i32](load[u8](&bytes,1)) }\n"
    3 9 4 "raw access on `addr` needs an element type" None;
  let raw_shadow_source =
    "struct T { value u8 }\nfn read(T usize, p addr) u8 { return p[T] }\n"
  in
  semantic_pin "raw-access-shadowed-type-no-help" raw_shadow_source 2
    (String.length "fn read(T usize, p addr) u8 { return " + 1)
    4 "raw access on `addr` needs an element type" None;
  semantic_accept "raw-access-shadowed-type-explicit"
    "struct T { value u8 }\nfn read(T usize, p addr) u8 { return p[u8, T] }\n";
  semantic_error "generic-local-volatile-type-shadow" "`T` is a value, not a type"
    "fn load[T](p addr, runtime_index usize) T {\n\
    \ { T usize = runtime_index\n\
    \ return volatile_load[T](p) } }\n\
     fn main() i32 { bytes arr[2,u8] = {11,22}\n\
    \ return zext[i32](load[u8](&bytes,1)) }\n";
  semantic_error "generic-local-simd-type-shadow" "`T` is a value, not a type"
    "fn load[T](p addr, runtime_index usize) T {\n\
    \ { T usize = runtime_index\n\
    \ mask vec[1,bool] = {true}\n\
    \ fallback vec[1,u8] = {99}\n\
    \ return masked_load[T](p,mask,fallback)[0] } }\n\
     fn main() i32 { bytes arr[2,u8] = {11,22}\n\
    \ return zext[i32](load[u8](&bytes,1)) }\n";
  semantic_error "generic-struct-local-value-shadow" "`Ring` is a value, not a type"
    "struct Ring[N const usize] { value i32 }\n\
     fn main() i32 { Ring i32 = 7\n\
    \ item Ring[4] = {9}\n\
    \ return item.value }\n";
  let expr_statement_shadow =
    llvm_of
      "const N usize = 99\n\
       fn f[N const usize]() usize { v usize = N + 1\n\
      \ return N }\n\
       fn test() usize { return f[2]() }\n"
  in
  if not (contains expr_statement_shadow "add i64 2, 1\n") then
    failwith "expr-stmt-const-argument: expr statement did not use the const parameter";
  if not (contains expr_statement_shadow "N=usize:2\"") then
    failwith "expr-stmt-const-argument: specialization key lost const argument identity";
  if contains expr_statement_shadow "99" then
    failwith "expr-stmt-const-argument: expr statement resolved the shadowing global";
  let expr_statement_call =
    llvm_of
      "const N usize = 99\n\
       fn g[M const usize]() usize { return M }\n\
       fn f[N const usize]() usize { g[N]()\n\
      \ return N }\n\
       fn test() usize { return f[2]() }\n"
  in
  if not (contains expr_statement_call "M=usize:2\"") then
    failwith
      "expr-stmt-call-const-argument: nested call did not use the const parameter";
  if contains expr_statement_call "99" then
    failwith "expr-stmt-call-const-argument: nested call resolved the shadowing global";
  let mixed_bitwise_compare =
    llvm_of
      "fn f(x u64, m u64, e u64) bool { return x & m == e }\n\
       fn main() i32 { return 0 }\n"
  in
  if not (contains mixed_bitwise_compare "and i64 %v3, %v4\n") then
    failwith "ruled-precedence: bitwise operands did not group first";
  if not (contains mixed_bitwise_compare "icmp eq i64 %v5, %v6\n") then
    failwith "ruled-precedence: comparison did not apply to the bitwise result";
  let ruled_additive_shift =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn test() usize { return w[1 + 2 << 3]() }\n"
  in
  if not (contains ruled_additive_shift "ret i64 24\n") then
    failwith "ruled-precedence: additive no longer binds tighter than shift";
  let ruled_shift_bitand =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn test() usize { return w[1 << 2 & 4]() }\n"
  in
  if not (contains ruled_shift_bitand "ret i64 4\n") then
    failwith "ruled-precedence: shift no longer binds tighter than bitwise and";
  let ruled_bitand_bitxor =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn test() usize { return w[2 & 3 ^ 1]() }\n"
  in
  if not (contains ruled_bitand_bitxor "ret i64 3\n") then
    failwith "ruled-precedence: bitwise and/xor group boundary moved";
  let ruled_bitxor_bitor =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn test() usize { return w[1 ^ 3 | 1]() }\n"
  in
  if not (contains ruled_bitxor_bitor "ret i64 3\n") then
    failwith "ruled-precedence: xor/or group boundary moved";
  let ruled_shift_assoc =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn test() usize { return w[16 >> 2 >> 1]() }\n"
  in
  if not (contains ruled_shift_assoc "ret i64 2\n") then
    failwith "ruled-precedence: shift chain lost left associativity";
  let literal_hex =
    llvm_of
      "fn w[N const usize]() usize { return N }\nfn test() usize { return w[0xff]() }\n"
  in
  if not (contains literal_hex "ret i64 255\n") then
    failwith "literal-bases: hex literal did not evaluate to 255";
  let literal_binary =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn test() usize { return w[0b1010]() }\n"
  in
  if not (contains literal_binary "ret i64 10\n") then
    failwith "literal-bases: binary literal did not evaluate to 10";
  let literal_octal =
    llvm_of
      "fn w[N const usize]() usize { return N }\nfn test() usize { return w[0o17]() }\n"
  in
  if not (contains literal_octal "ret i64 15\n") then
    failwith "literal-bases: octal literal did not evaluate to 15";
  let literal_negative_hex =
    llvm_of
      "fn w[N const i64]() i64 { return N }\nfn test() i64 { return w[-0x10]() }\n"
  in
  if not (contains literal_negative_hex "ret i64 -16\n") then
    failwith "literal-bases: negative hex literal did not evaluate to -16";
  let literal_underscores =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       fn test() usize { return w[0x1_00]() }\n"
  in
  if not (contains literal_underscores "ret i64 256\n") then
    failwith "literal-bases: underscore-separated literal did not evaluate to 256";
  semantic_pin "generic-args-missing-rejected"
    "fn f[N const usize]() usize { return N }\nfn test() usize { return f() }\n" 2 26 3
    "cannot infer `N` for `f`; provide explicit generic arguments"
    (Some "write the explicit generic argument list for `f`");
  semantic_error "generic-args-extra-rejected"
    "generic function `f` expects 1 generic argument, got 2"
    "fn f[N const usize]() usize { return N }\nfn test() usize { return f[1, 2]() }\n";
  semantic_error "generic-kind-type-for-const-rejected" "expected a const argument"
    "fn f[N const usize]() usize { return N }\nfn test() usize { return f[i64]() }\n";
  semantic_error "generic-kind-const-for-type-rejected" "expected a type argument"
    "fn f[T]() usize { return 0 }\nfn test() usize { return f[1]() }\n";
  semantic_error "generic-duplicate-parameter-rejected"
    "duplicate generic parameter `T`"
    "fn f[T, T]() usize { return 0 }\nfn test() usize { return f[i64]() }\n";
  semantic_error "generic-unknown-type-argument-rejected" "unknown type `Nope`"
    "fn f[T]() usize { return 0 }\nfn test() usize { return f[Nope]() }\n";
  ignore
    (llvm_of "fn f(x u8) usize { return zext[usize](x) }\nfn main() i32 { return 0 }\n");
  ignore
    (llvm_of "fn f(x i8) isize { return sext[isize](x) }\nfn main() i32 { return 0 }\n");
  ignore
    (llvm_of "fn f(x usize) u8 { return trunc[u8](x) }\nfn main() i32 { return 0 }\n");
  let zext_const_value =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       const B u8 = 255\n\
       fn test() usize { return w[zext[usize](B)]() }\n"
  in
  if not (contains zext_const_value "ret i64 255\n") then
    failwith "target-width-conversion: zext const value mismatch";
  let sext_const_value =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -1\n\
       fn test() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains sext_const_value "ret i64 -1\n") then
    failwith "target-width-conversion: sext const value mismatch";
  let trunc_const_value =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       const B usize = 300\n\
       fn test() usize { return w[zext[usize](trunc[u8](B))]() }\n"
  in
  if not (contains trunc_const_value "ret i64 44\n") then
    failwith "target-width-conversion: trunc low-bit value mismatch";
  semantic_error "target-width-equal-cast-rejected"
    "illegal cast for source and destination widths"
    "fn f(x usize) u64 { return zext[u64](x) }\nfn main() i32 { return 0 }\n";
  semantic_error "target-width-trunc-widen-rejected"
    "illegal cast for source and destination widths"
    "fn f(x u8) i64 { return trunc[i64](x) }\nfn main() i32 { return 0 }\n";
  semantic_error "target-width-zext-shrink-rejected"
    "illegal cast for source and destination widths"
    "fn f(x i64) i8 { return zext[i8](x) }\nfn main() i32 { return 0 }\n";
  semantic_error "target-width-sext-equal-rejected"
    "illegal cast for source and destination widths"
    "fn f(x i32) i32 { return sext[i32](x) }\nfn main() i32 { return 0 }\n";
  semantic_error "target-width-sext-shrink-rejected"
    "illegal cast for source and destination widths"
    "fn f(x i64) i8 { return sext[i8](x) }\nfn main() i32 { return 0 }\n";
  let zext_signed_value =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       const B i8 = -1\n\
       fn test() usize { return w[zext[usize](B)]() }\n"
  in
  if not (contains zext_signed_value "ret i64 255\n") then
    failwith "target-width-conversion: zext of signed source must zero-fill (255)";
  let sext_unsigned_value =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const D u8 = 255\n\
       fn test() isize { return w[sext[isize](D)]() }\n"
  in
  if not (contains sext_unsigned_value "ret i64 -1\n") then
    failwith "target-width-conversion: sext of unsigned source must sign-fill (-1)";
  let call_argument_order =
    llvm_of
      "fn g() i64 { return 1 }\n\
       fn h() i64 { return 2 }\n\
       fn f(a i64, b i64) i64 { return a + b }\n\
       fn test() i64 { return f(g(), h()) }\n"
  in
  if
    not
      (contains call_argument_order "\n  %v0 = call i64 @g()\n  %v1 = call i64 @h()\n")
  then failwith "eval-order: call arguments did not evaluate left to right";
  let compound_assign_dest_once =
    llvm_of
      "fn i() usize { return 0 }\n\
       fn v() i64 { return 5 }\n\
       fn test() i64 { a arr[4, i64]\n\
      \ a[0] = 0\n\
      \ a[1] = 0\n\
      \ a[2] = 0\n\
      \ a[3] = 0\n\
      \ a[i()] += v()\n\
      \ return a[0] }\n"
  in
  let dest_calls = positions compound_assign_dest_once "call i64 @i()" in
  let rhs_calls = positions compound_assign_dest_once "call i64 @v()" in
  (match (dest_calls, rhs_calls) with
  | [ dest ], [ rhs ] when dest < rhs -> ()
  | _ ->
      failwith
        "eval-order: compound assignment did not evaluate its destination exactly once \
         before the rhs");
  let short_circuit_guard =
    llvm_of
      "fn q() bool { return true }\n\
       fn k(a bool) bool { return a && q() }\n\
       fn main() i32 { return 0 }\n"
  in
  let guard_branches = positions short_circuit_guard "br i1" in
  let guarded_calls = positions short_circuit_guard "call i1 @q()" in
  (match (guard_branches, guarded_calls) with
  | br :: _, [ call ] when br < call -> ()
  | _ ->
      failwith
        "eval-order: short-circuit rhs was not evaluated exactly once after the guard \
         branch");
  let assign_dest_once =
    llvm_of
      "fn i() usize { return 0 }\n\
       fn v() i64 { return 5 }\n\
       fn test() i64 { a arr[4, i64]\n\
      \ a[0] = 0\n\
      \ a[1] = 0\n\
      \ a[2] = 0\n\
      \ a[3] = 0\n\
      \ a[i()] = v()\n\
      \ return a[0] }\n"
  in
  let dest_calls = positions assign_dest_once "call i64 @i()" in
  let rhs_calls = positions assign_dest_once "call i64 @v()" in
  (match (dest_calls, rhs_calls) with
  | [ dest ], [ rhs ] when dest < rhs -> ()
  | _ -> failwith "eval-order: assign dest not once before rhs");
  let return_before_defer =
    llvm_of
      "fn f() i64 { x i64 = 1\n\
       defer { x = 2 }\n\
       return x }\n\
       fn test() i64 { return f() }\n"
  in
  let capture_load = positions return_before_defer "load i64" in
  let cleanup_store = positions return_before_defer "store i64 2, ptr" in
  (match (capture_load, cleanup_store) with
  | [ load ], [ store ] when load < store -> ()
  | _ -> failwith "eval-order: defer ran before return capture");
  let defer_reverse_order =
    llvm_of
      "fn f() i64 { x i64 = 0\n\
       defer { x = 1 }\n\
       defer { x = 2 }\n\
       return x }\n\
       fn test() i64 { return f() }\n"
  in
  let second_defer = positions defer_reverse_order "store i64 2, ptr" in
  let first_defer = positions defer_reverse_order "store i64 1, ptr" in
  (match (second_defer, first_defer) with
  | [ second ], [ first ] when second < first -> ()
  | _ -> failwith "eval-order: defers not reverse order");
  let lane_update_dest =
    llvm_of
      "fn i() usize { return 0 }\n\
       fn v() i64 { return 5 }\n\
       fn test() i64 { x vec[2, i64] = splat(1)\n\
      \ x[i()] = v()\n\
      \ return 0 }\n"
  in
  let lane_dest_calls = positions lane_update_dest "call i64 @i()" in
  let lane_rhs_calls = positions lane_update_dest "call i64 @v()" in
  (match (lane_dest_calls, lane_rhs_calls) with
  | [ dest ], [ rhs ] when dest < rhs -> ()
  | _ -> failwith "eval-order: lane update dest not once before rhs");
  let lane_compound_dest =
    llvm_of
      "fn i() usize { return 0 }\n\
       fn v() i64 { return 5 }\n\
       fn test() i64 { x vec[2, i64] = splat(1)\n\
      \ x[i()] += v()\n\
      \ return 0 }\n"
  in
  let compound_dest_calls = positions lane_compound_dest "call i64 @i()" in
  let compound_rhs_calls = positions lane_compound_dest "call i64 @v()" in
  (match (compound_dest_calls, compound_rhs_calls) with
  | [ dest ], [ rhs ] when dest < rhs -> ()
  | _ -> failwith "eval-order: lane compound dest not once before rhs");
  let switch_no_fallthrough =
    llvm_of
      "fn test() i64 { n i64 = 1\n\
      \ switch n {\n\
      \  case 1: { n = 2 }\n\
      \  case 2: { n = 3 }\n\
      \  default: { n = 0 }\n\
      \ }\n\
      \ return n }\n"
  in
  if
    not
      (contains switch_no_fallthrough "store i64 2, ptr %v0, align 8\n  br label %b1\n")
  then failwith "control: switch case skipped its exit branch";
  if contains switch_no_fallthrough "store i64 2, ptr %v0, align 8\n  store i64 3" then
    failwith "control: switch case fell through";
  if contains switch_no_fallthrough "store i64 3, ptr %v0, align 8\n  store i64 0" then
    failwith "control: switch case fell through";
  let condition_before_branch =
    llvm_of
      "fn c() bool { return true }\n\
       fn test() i64 { x i64 = 0\n\
      \ if c() { x = 1 }\n\
      \ return x }\n"
  in
  let cond_calls = positions condition_before_branch "call i1 @c()" in
  let cond_branches = positions condition_before_branch "br i1" in
  let body_stores = positions condition_before_branch "store i64 1, ptr" in
  (match (cond_calls, cond_branches, body_stores) with
  | [ call ], [ branch ], [ store ] when call < branch && branch < store -> ()
  | _ -> failwith "eval-order: condition or body misordered");
  let or_short_circuit =
    llvm_of
      "fn q() bool { return true }\n\
       fn k(a bool) bool { return a || q() }\n\
       fn main() i32 { return 0 }\n"
  in
  let or_guards = positions or_short_circuit "br i1" in
  let or_calls = positions or_short_circuit "call i1 @q()" in
  (match (or_guards, or_calls) with
  | [ guard ], [ call ] when guard < call -> ()
  | _ -> failwith "eval-order: or rhs not once after guard");
  let defer_reads_current =
    llvm_of
      "fn f() i64 { x i64 = 1\n\
      \ defer { x = x + 1 }\n\
      \ x = 2\n\
      \ return x }\n\
       fn test() i64 { return f() }\n"
  in
  let defer_mutation = positions defer_reads_current "store i64 2, ptr" in
  let defer_reads = positions defer_reads_current "load i64" in
  (match (defer_mutation, defer_reads) with
  | [ mutation ], read :: _ when mutation < read -> ()
  | _ -> failwith "eval-order: defer read stale values");
  let break_messages =
    semantic_messages
      "fn test() i64 { n i64 = 0\n\
      \ switch n {\n\
      \  case 0: { break }\n\
      \  default: { n = 9 }\n\
      \ }\n\
      \ return n }\n"
  in
  (match break_messages with
  | [ "break outside loop" ] -> ()
  | _ -> failwith "break-outside-loop: wrong message");
  let continue_messages = semantic_messages "fn test() i64 { continue }\n" in
  (match continue_messages with
  | [ "continue outside loop" ] -> ()
  | _ -> failwith "continue-outside-loop: wrong message");
  let void_fallthrough_defer =
    llvm_of
      "fn f() void { x i64 = 0\n\
      \ defer { x = 1 }\n\
      \ }\n\
       fn main() i32 { f()\n\
      \ return 0 }\n"
  in
  let defer_store = positions void_fallthrough_defer "store i64 1, ptr" in
  let fallthrough_exit = positions void_fallthrough_defer "ret void" in
  (match (defer_store, fallthrough_exit) with
  | [ store ], [ exit ] when store < exit -> ()
  | _ -> failwith "control: void fallthrough skipped defer");
  let mask_negation_admit =
    llvm_of
      "fn not2(a vec[2, bool]) vec[2, bool] { return !a }\nfn main() i32 { return 0 }\n"
  in
  if not (contains mask_negation_admit "define internal <2 x i1> @not2(<2 x i1>") then
    failwith "control: mask negation not admitted";
  let bitnot_messages =
    semantic_messages
      "fn not2(a vec[2, bool]) vec[2, bool] { return ~a }\nfn main() i32 { return 0 }\n"
  in
  (match bitnot_messages with
  | [ "unary operator `~` needs an integer or integer vector, got `vec[2, bool]`" ] ->
      ()
  | _ -> failwith "mask-bitnot-integer-only: wrong message");
  let unterminated_messages = parse_messages "fn test() i64 {\n" in
  (match unterminated_messages with
  | [ "unterminated block" ] -> ()
  | _ -> failwith "unterminated-block: wrong message");
  let case_messages =
    parse_messages "fn test() i64 { n i64 = 0\n switch n { foo }\n return 0 }\n"
  in
  (match case_messages with
  | [ "expected `case`, `default`, or `}`" ] -> ()
  | _ -> failwith "switch-case-parse: wrong message");
  let separator_messages =
    parse_messages "fn test() i64 { x i64 = 1 y i64 = 2\n return x }\n"
  in
  (match separator_messages with
  | [ "expected end of statement (newline or `;`)" ] -> ()
  | _ -> failwith "statement-separator-parse: wrong message");
  let extern_abi_messages = parse_messages "extern \"c\" fn f(x i64) i64\n" in
  (match extern_abi_messages with
  | [ "only extern \"C\" is supported" ] -> ()
  | _ -> failwith "extern-abi: wrong message");
  let variadic_messages = parse_messages "extern \"C\" { fn f(...) i64 }\n" in
  (match variadic_messages with
  | [ "a variadic declaration needs a fixed parameter" ] -> ()
  | _ -> failwith "variadic-fixed-param: wrong message");
  let literal_comma_messages = parse_messages "const a arr[2, i64] = { 1 2 }\n" in
  (match literal_comma_messages with
  | [ "expected comma between literal elements" ] -> ()
  | _ -> failwith "literal-comma: wrong message");
  let ellipsis_messages = parse_messages "fn f(...) i64 { return 0 }\n" in
  (match ellipsis_messages with
  | [ "`...` is legal only in extern \"C\"" ] -> ()
  | _ -> failwith "ellipsis-extern-only: wrong message");
  let block_comment_messages =
    parse_messages "fn test() i64 { return 0 }\n/* unclosed\n"
  in
  (match block_comment_messages with
  | [ "unterminated block comment" ] -> ()
  | _ -> failwith "block-comment: wrong message");
  let string_literal_messages = parse_messages "fn test() i64 { return 0 }\n\"abc\n" in
  (match string_literal_messages with
  | [ "unterminated string literal" ] -> ()
  | _ -> failwith "string-literal: wrong message");
  let character_literal_messages =
    [
      ("character-empty", "''", "empty character literal");
      ("character-two-bytes", "'ab'", "character literal must contain exactly one byte");
      ( "character-four-bytes",
        "'abcd'",
        "character literal must contain exactly one byte" );
      ( "character-non-ascii",
        "'" ^ "\195\169" ^ "'",
        "character literal must be printable ASCII; use \\xNN" );
      ( "character-raw-tab",
        "'" ^ "\t" ^ "'",
        "character literal must be printable ASCII; use \\xNN" );
      ("character-unknown-escape", "'\\q'", "unknown string escape");
      ( "character-short-hex",
        "'\\x4'",
        "hex escape must be followed by exactly two hexadecimal digits" );
      ("character-unterminated-line", "'a\n", "unterminated character literal");
      ("character-unterminated-crlf", "'a\r\n", "unterminated character literal");
      ("character-unterminated-file", "'a", "unterminated character literal");
    ]
  in
  List.iter
    (fun (name, literal, expected) ->
      let messages = parse_messages ("fn main() i32 { return 0 }\n" ^ literal) in
      match messages with
      | [ message ] when message = expected -> ()
      | _ -> failwith (name ^ ": wrong message " ^ String.concat "; " messages))
    character_literal_messages;
  let integer_literal_messages =
    parse_messages "fn test() i64 { x i64 = 0x\n return 0 }\n"
  in
  (match integer_literal_messages with
  | [ "invalid integer literal" ] -> ()
  | _ -> failwith "integer-literal: wrong message");
  let unterminated_escape_messages =
    parse_messages "fn test() i64 { return 0 }\n\"abc\\"
  in
  (match unterminated_escape_messages with
  | [ "unterminated escape" ] -> ()
  | _ -> failwith "string-escape: wrong message");
  let unknown_escape_messages =
    parse_messages "fn test() i64 { return 0 }\n\"\\q\"\n"
  in
  (match unknown_escape_messages with
  | [ "unknown string escape" ] -> ()
  | _ -> failwith "unknown-escape: wrong message");
  List.iter
    (fun (name, text) ->
      match parse_messages text with
      | [ message ]
        when message = "hex escape must be followed by exactly two hexadecimal digits"
        ->
          ()
      | messages -> failwith (name ^ ": wrong message " ^ String.concat "; " messages))
    [
      ("hex-escape-missing-digits", "fn test() i64 { return 0 }\n\"\\x\"\n");
      ("hex-escape-one-digit", "fn test() i64 { return 0 }\n\"\\xA\"\n");
      ("hex-escape-invalid-first", "fn test() i64 { return 0 }\n\"\\xG1\"\n");
      ("hex-escape-invalid-second", "fn test() i64 { return 0 }\n\"\\x0G\"\n");
    ];
  let min_sext_i8 =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -128\n\
       fn test() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains min_sext_i8 "ret i64 -128\n") then
    failwith "value: i8 min sext drifted";
  let min_sext_i16 =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i16 = -32768\n\
       fn test() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains min_sext_i16 "ret i64 -32768\n") then
    failwith "value: i16 min sext drifted";
  let min_zext_i8_fill =
    llvm_of
      "fn w[N const usize]() usize { return N }\n\
       const B i8 = -128\n\
       fn test() usize { return w[zext[usize](B)]() }\n"
  in
  if not (contains min_zext_i8_fill "ret i64 128\n") then
    failwith "value: i8 min zext fill drifted";
  let neg_hex_literal =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -0x80\n\
       fn test() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains neg_hex_literal "ret i64 -128\n") then
    failwith "value: negative hex literal drifted";
  let neg_binary_literal =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -0b10000000\n\
       fn test() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains neg_binary_literal "ret i64 -128\n") then
    failwith "value: negative binary literal drifted";
  let neg_octal_literal =
    llvm_of
      "fn w[N const isize]() isize { return N }\n\
       const B i8 = -0o200\n\
       fn test() isize { return w[sext[isize](B)]() }\n"
  in
  if not (contains neg_octal_literal "ret i64 -128\n") then
    failwith "value: negative octal literal drifted";
  let agreement_bitand_eq =
    llvm_of
      "const C bool = 4 & 2 == 2\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitand_eq "ret i64 0\n") then
    failwith
      "ruled-agreement: bitwise-and mixed form disagreed with its parenthesized twin";
  if contains agreement_bitand_eq "ret i64 1\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_bitor_eq =
    llvm_of
      "const C bool = 1 | 0 == 0\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitor_eq "ret i64 0\n") then
    failwith
      "ruled-agreement: bitwise-or mixed form disagreed with its parenthesized twin";
  if contains agreement_bitor_eq "ret i64 1\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_bitxor_eq =
    llvm_of
      "const C bool = 2 ^ 3 == 2\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitxor_eq "ret i64 0\n") then
    failwith
      "ruled-agreement: bitwise-xor mixed form disagreed with its parenthesized twin";
  if contains agreement_bitxor_eq "ret i64 1\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_bitand_zero =
    llvm_of
      "const C bool = 6 & 3 == 0\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitand_zero "ret i64 0\n") then
    failwith
      "ruled-agreement: bitwise-and equality form disagreed with its parenthesized twin";
  if contains agreement_bitand_zero "ret i64 1\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_bitand_rel =
    llvm_of
      "const C bool = 5 & 3 < 4\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains agreement_bitand_rel "ret i64 1\n") then
    failwith
      "ruled-agreement: bitwise-and relational form disagreed with its parenthesized \
       twin";
  if contains agreement_bitand_rel "ret i64 0\n" then
    failwith "ruled-agreement: const generic branch was not pruned";
  let agreement_runtime_shape =
    llvm_of "fn g(x u64) bool { return x & 3 == 1 }\nfn main() i32 { return 0 }\n"
  in
  if not (contains agreement_runtime_shape "and i64 %v1, 3\n") then
    failwith "ruled-agreement: runtime bitwise operands did not group first";
  if not (contains agreement_runtime_shape "icmp eq i64 %v2, 1\n") then
    failwith "ruled-agreement: runtime comparison did not apply to the bitwise result";
  let bool_bitand_true =
    llvm_of
      "const C bool = true & true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitand_true "ret i64 1\n") then
    failwith "bool-bitops: true & true did not evaluate to true";
  if contains bool_bitand_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitand_false =
    llvm_of
      "const C bool = true & false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitand_false "ret i64 0\n") then
    failwith "bool-bitops: true & false did not evaluate to false";
  if contains bool_bitand_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitor_false =
    llvm_of
      "const C bool = false | false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitor_false "ret i64 0\n") then
    failwith "bool-bitops: false | false did not evaluate to false";
  if contains bool_bitor_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitor_true =
    llvm_of
      "const C bool = true | false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitor_true "ret i64 1\n") then
    failwith "bool-bitops: true | false did not evaluate to true";
  if contains bool_bitor_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitxor_false =
    llvm_of
      "const C bool = true ^ true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitxor_false "ret i64 0\n") then
    failwith
      "bool-bitops: true ^ true did not evaluate to false (xor must agree with !=)";
  if contains bool_bitxor_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitxor_true =
    llvm_of
      "const C bool = true ^ false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitxor_true "ret i64 1\n") then
    failwith
      "bool-bitops: true ^ false did not evaluate to true (xor must agree with !=)";
  if contains bool_bitxor_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitand_both_false =
    llvm_of
      "const C bool = false & false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitand_both_false "ret i64 0\n") then
    failwith "bool-bitops: false & false did not evaluate to false";
  if contains bool_bitand_both_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitor_both_true =
    llvm_of
      "const C bool = true | true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitor_both_true "ret i64 1\n") then
    failwith "bool-bitops: true | true did not evaluate to true";
  if contains bool_bitor_both_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitxor_both_false =
    llvm_of
      "const C bool = false ^ false\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitxor_both_false "ret i64 0\n") then
    failwith
      "bool-bitops: false ^ false did not evaluate to false (xor must agree with !=)";
  if contains bool_bitxor_both_false "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitand_false_true =
    llvm_of
      "const C bool = false & true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitand_false_true "ret i64 0\n") then
    failwith "bool-bitops: false & true did not evaluate to false";
  if contains bool_bitand_false_true "ret i64 1\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitor_false_true =
    llvm_of
      "const C bool = false | true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitor_false_true "ret i64 1\n") then
    failwith "bool-bitops: false | true did not evaluate to true";
  if contains bool_bitor_false_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitxor_false_true =
    llvm_of
      "const C bool = false ^ true\n\
       fn r[B const bool]() usize { if B { return 1 }\n\
      \ return 0 }\n\
       fn test() usize { return r[C]() }\n"
  in
  if not (contains bool_bitxor_false_true "ret i64 1\n") then
    failwith
      "bool-bitops: false ^ true did not evaluate to true (xor must agree with !=)";
  if contains bool_bitxor_false_true "ret i64 0\n" then
    failwith "bool-bitops: const generic branch was not pruned";
  let bool_bitops_runtime =
    llvm_of
      "fn f(a bool, b bool) bool { return a & b }\n\
       fn g(a bool, b bool) bool { return a | b }\n\
       fn h(a bool, b bool) bool { return a ^ b }\n\
       fn main() i32 { return 0 }\n"
  in
  if not (contains bool_bitops_runtime " = and i1 ") then
    failwith "bool-bitops: runtime bitwise-and did not lower to one-bit and";
  if not (contains bool_bitops_runtime " = or i1 ") then
    failwith "bool-bitops: runtime bitwise-or did not lower to one-bit or";
  if not (contains bool_bitops_runtime " = xor i1 ") then
    failwith "bool-bitops: runtime bitwise-xor did not lower to one-bit xor";
  (let ir =
     llvm_of
       "fn f(a vec[2, bool], b vec[2, bool]) vec[2, bool] { return a & b }\n\
        fn main() i32 { return 0 }\n"
   in
   if not (contains ir "and <2 x i1>") then
     failwith "bool-mask-bitop: vec bitand did not lower");
  semantic_error "int-bool-bitop-rejected"
    "operands of `&` have different types: `u8` and `bool`"
    "fn f(x u8, b bool) bool { return x & b }\nfn main() i32 { return 0 }\n";
  ignore
    (llvm_of "fn f(a vec[256, u8]) u8 { return a[0] }\nfn main() i32 { return 0 }\n");
  ignore
    (llvm_of "fn f(a vec[32, u64]) u64 { return a[0] }\nfn main() i32 { return 0 }\n");
  ignore
    (llvm_of "fn f(a vec[256, bool]) bool { return a[0] }\nfn main() i32 { return 0 }\n");
  semantic_error "vector-lane-cap-rejected"
    "vector lane count exceeds the portable cap of 256"
    "fn f(a vec[257, u8]) u8 { return a[0] }\nfn main() i32 { return 0 }\n";
  semantic_error "vector-size-cap-rejected"
    "vector size exceeds the portable cap of 2048 bits"
    "fn f(a vec[33, u64]) u64 { return a[0] }\nfn main() i32 { return 0 }\n";
  semantic_pin "vector-lane-cap-caret" "fn f() void { value vec[257,u8]\nreturn }\n" 1
    (String.length "fn f() void { value " + 1)
    3 "vector lane count exceeds the portable cap of 256" None;
  semantic_pin "vector-size-cap-caret" "fn f() void { value vec[33,u64]\nreturn }\n" 1
    (String.length "fn f() void { value " + 1)
    3 "vector size exceeds the portable cap of 2048 bits" None;
  semantic_error "literal-range-const-u8-rejected"
    "integer literal is out of range for u8"
    "const C u8 = 256\n\
     fn w[N const u8]() u8 { return N }\n\
     fn test() u8 { return w[C]() }\n";
  semantic_error "literal-range-const-i8-rejected"
    "integer literal is out of range for i8"
    "const C i8 = -129\n\
     fn w[N const i8]() i8 { return N }\n\
     fn test() i8 { return w[C]() }\n";
  semantic_error "literal-range-runtime-u8-rejected"
    "integer literal is out of range for u8"
    "fn f() u8 { return 256 }\nfn main() i32 { return 0 }\n";
  semantic_error "vector-lane-cap-local-rejected"
    "vector lane count exceeds the portable cap of 256"
    "fn f() i64 { v vec[257, u8] = splat(1)\n\
    \ return 0 }\n\
     fn test() i64 { return f() }\n";
  semantic_error "vector-size-cap-const-rejected"
    "vector size exceeds the portable cap of 2048 bits"
    "const X vec[33, u64] = splat(0)\nfn test() i64 { return 0 }\n";
  let vec32_usize_legal =
    llvm_of "fn f(a vec[32, usize]) usize { return a[0] }\nfn main() i32 { return 0 }\n"
  in
  if not (contains vec32_usize_legal "extractelement <32 x i64>") then
    failwith "vector-caps: vec[32, usize] (2048 bits at max width) was not admitted";
  let vec32_isize_legal =
    llvm_of "fn f(a vec[32, isize]) isize { return a[0] }\nfn main() i32 { return 0 }\n"
  in
  if not (contains vec32_isize_legal "extractelement <32 x i64>") then
    failwith "vector-caps: vec[32, isize] (2048 bits at max width) was not admitted";
  semantic_error "vector-size-cap-usize-rejected"
    "vector size exceeds the portable cap of 2048 bits"
    "fn f(a vec[33, usize]) usize { return a[0] }\nfn main() i32 { return 0 }\n";
  semantic_error "vector-size-cap-isize-rejected"
    "vector size exceeds the portable cap of 2048 bits"
    "fn f(a vec[33, isize]) isize { return a[0] }\nfn main() i32 { return 0 }\n";
  semantic_error "dead-branch-literal-range-rejected"
    "integer literal is out of range for u8"
    "fn test() u8 { if false { return 256 }\n return 0 }\n";
  semantic_error "pruned-specialization-literal-range-rejected"
    "integer literal is out of range for u8"
    "fn r[B const bool]() u8 { if B { return 256 }\n\
    \ return 0 }\n\
     fn test() u8 { return r[false]() }\n";
  semantic_error "dead-branch-type-error-rejected"
    "bool and integer values use different representations"
    "fn test() u8 { if false { return true }\n return 0 }\n";
  semantic_error "dead-branch-unknown-name-rejected" "unknown name `nope`"
    "fn test() u8 { if false { return nope }\n return 0 }\n";
  let unused_generic_function =
    expect_ok
      (Sema.check
         (expect_ok
            (Parser.parse
               (source
                  "fn unused[T](value T) T { return value + value }\n\
                   fn test() i64 { return 0 }\n"))))
  in
  if
    List.exists
      (fun (func : Hir.func) -> contains func.name "unused")
      unused_generic_function.Hir.funcs
  then failwith "generic-function-template: unused template was emitted";
  let type_generic_failure =
    "fn bad[T](value T) T { return value + value }\n\
     fn test(value addr) addr { return bad[addr](value) }\n"
  in
  (match semantic_diagnostics type_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.message
        <> "address arithmetic for `+` has operands `addr` and `addr`; expected a \
            scalar integer offset"
      then failwith "generic-instantiation-type: root message changed";
      if
        diagnostic.primary.Span.file <> "regression.fas"
        || diagnostic.primary.Span.line <> 1
        || diagnostic.primary.Span.column <> 39
      then failwith "generic-instantiation-type: root span changed";
      if
        diagnostic.notes <> [ "while instantiating `bad[addr]` at regression.fas:2:38" ]
      then
        failwith
          ("generic-instantiation-type: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-type: expected one diagnostic");
  let const_generic_failure =
    "fn bad[N const u64](value addr) addr { return value + value }\n\
     fn test(value addr) addr {\n\
     return bad[18446744073709551615](value)\n\
     }\n"
  in
  (match semantic_diagnostics const_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [ "while instantiating `bad[18446744073709551615]` at regression.fas:3:11" ]
      then
        failwith
          ("generic-instantiation-const: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-const: expected one diagnostic");
  let mixed_generic_failure =
    "fn bad[T, N const u64](value T) T { return value + value }\n\
     fn test(value addr) addr {\n\
     return bad[addr, 18446744073709551615](value)\n\
     }\n"
  in
  (match semantic_diagnostics mixed_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `bad[addr, 18446744073709551615]` at \
              regression.fas:3:11";
           ]
      then
        failwith
          ("generic-instantiation-mixed: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-mixed: expected one diagnostic");
  let interleaved_generic_failure =
    "fn bad[A, N const u8, B, M const i8](left A, right B) A {\n\
     return left + left\n\
     }\n\
     fn test(value addr, other addr) addr {\n\
     return bad[addr, 2, addr, -3](value, other)\n\
     }\n"
  in
  (match semantic_diagnostics interleaved_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [ "while instantiating `bad[addr, 2, addr, -3]` at regression.fas:5:11" ]
      then
        failwith
          ("generic-instantiation-order: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-order: expected one diagnostic");
  let nested_generic_failure =
    "fn inner[T](value T) T { return value + value }\n\
     fn outer[T](value T) T { return inner[T](value) }\n\
     fn test(value addr) addr { return outer[addr](value) }\n"
  in
  (match semantic_diagnostics nested_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `outer[addr]` at regression.fas:3:40";
             "while instantiating `inner[addr]` at regression.fas:2:38";
           ]
      then
        failwith
          ("generic-instantiation-nested: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-nested: expected one diagnostic");
  let nested_const_generic_failure =
    "fn inner[N const usize](value addr) addr { return value + value }\n\
     fn outer[T](value T) T { return inner[4](value) }\n\
     fn test(value addr) addr { return outer[addr](value) }\n"
  in
  (match semantic_diagnostics nested_const_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes
        <> [
             "while instantiating `outer[addr]` at regression.fas:3:40";
             "while instantiating `inner[4]` at regression.fas:2:38";
           ]
      then
        failwith
          ("generic-instantiation-nested-const: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-nested-const: expected one diagnostic");
  let generic_struct_field_failure =
    "struct Bad[T] { value Missing }\nfn test(value Bad[u8]) i64 { return 0 }\n"
  in
  (match semantic_diagnostics generic_struct_field_failure with
  | [ diagnostic ] ->
      if diagnostic.message <> "unknown type `Missing`" then
        failwith "generic-instantiation-struct-field: root message changed";
      if diagnostic.notes <> [ "while instantiating `Bad[u8]` at regression.fas:2:18" ]
      then
        failwith
          ("generic-instantiation-struct-field: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-struct-field: expected one diagnostic");
  let generic_struct_layout_failure =
    "struct Bad[T] { value void }\nfn test(value Bad[u8]) i64 { return 0 }\n"
  in
  (match semantic_diagnostics generic_struct_layout_failure with
  | [ diagnostic ] ->
      if diagnostic.message <> "field `value` cannot have type `void`" then
        failwith "generic-instantiation-struct-layout: root message changed";
      if diagnostic.notes <> [ "while instantiating `Bad[u8]` at regression.fas:2:18" ]
      then
        failwith
          ("generic-instantiation-struct-layout: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-struct-layout: expected one diagnostic");
  let recursive_generic_struct_failure =
    "struct Recursive[T] { value Recursive[T] }\n\
     fn test(value Recursive[u8]) i64 { return 0 }\n"
  in
  (match semantic_diagnostics recursive_generic_struct_failure with
  | [ diagnostic ] ->
      if diagnostic.message <> "recursive by-value struct `Recursive[u8]`" then
        failwith
          ("generic-instantiation-recursive-struct: unexpected message: "
         ^ diagnostic.message);
      if
        diagnostic.notes
        <> [ "while instantiating `Recursive[u8]` at regression.fas:2:24" ]
      then
        failwith
          ("generic-instantiation-recursive-struct: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-recursive-struct: expected one diagnostic");
  let nested_struct_argument_failure =
    "struct Box[T] { value T }\n\
     fn bad[T](value T) T { return value + value }\n\
     fn test() i64 { value Box[Box[u8]] = {{1}}\n\
     bad[Box[Box[u8]]](value)\n\
     return 0\n\
     }\n"
  in
  (match semantic_diagnostics nested_struct_argument_failure with
  | [ diagnostic ] ->
      if
        diagnostic.message
        <> "aggregate parameter `value` of type `Box[Box[u8]]` cannot be passed by \
            value; declare `value` as `addr`; callers pass its address with `&`"
      then
        failwith
          ("generic-instantiation-nested-struct: unexpected message: "
         ^ diagnostic.message);
      if
        diagnostic.notes
        <> [ "while instantiating `bad[Box[Box[u8]]]` at regression.fas:4:4" ]
      then
        failwith
          ("generic-instantiation-nested-struct: unexpected notes: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-nested-struct: expected one diagnostic");
  let specialized_type_message_failure =
    "struct Box[T] { value T }\n\
     fn bad[T](value T) i64 {\n\
     local i64 = value\n\
     return 0\n\
     }\n\
     fn test() i64 { value u8 = 1\n\
    \ return bad[u8](value) }\n"
  in
  semantic_accept "generic-instantiation-lossless-local"
    specialized_type_message_failure;
  let repeated_generic_failure =
    "fn bad[T](value T) T { return value + value }\n\
     fn test(value addr) addr {\n\
     first addr = bad[addr](value)\n\
     return bad[addr](first)\n\
     }\n"
  in
  (match semantic_diagnostics repeated_generic_failure with
  | [ diagnostic ] ->
      if
        diagnostic.notes <> [ "while instantiating `bad[addr]` at regression.fas:3:17" ]
      then
        failwith
          ("generic-instantiation-cache: unexpected trace: "
          ^ String.concat " | " diagnostic.notes)
  | _ -> failwith "generic-instantiation-cache: expected one diagnostic");
  let diamond_cache_failure =
    "fn leaf[T](value T) T { return value + value }\n\
     fn left[T](value T) T { return leaf[T](value) }\n\
     fn right[T](value T) T { return leaf[T](value) }\n\
     fn root[T](value T) T {\n\
     first T = left[T](value)\n\
     return right[T](first)\n\
     }\n\
     fn test(value addr) addr { return root[addr](value) }\n"
  in
  (match semantic_diagnostics diamond_cache_failure with
  | [ diagnostic ] ->
      let rendered = Diag.render_all ~source:None [ diagnostic ] in
      if
        diagnostic.notes
        <> [
             "while instantiating `root[addr]` at regression.fas:8:39";
             "while instantiating `left[addr]` at regression.fas:5:15";
             "while instantiating `leaf[addr]` at regression.fas:2:36";
           ]
      then
        failwith
          ("generic-instantiation-diamond-cache: unexpected trace: "
          ^ String.concat " | " diagnostic.notes);
      if contains rendered "$spec$" then
        failwith "generic-instantiation-diamond-cache: internal name leaked"
  | _ -> failwith "generic-instantiation-diamond-cache: expected one diagnostic");
  let render_failure source_text =
    Diag.render_all ~source:None (semantic_diagnostics source_text)
  in
  let deterministic_failure = render_failure nested_generic_failure in
  for _ = 1 to 4 do
    if render_failure nested_generic_failure <> deterministic_failure then
      failwith "generic-instantiation-determinism: rendered output changed"
  done;
  if contains deterministic_failure "$spec$" then
    failwith "generic-instantiation-determinism: internal name leaked";
  let issue45_source =
    "fn broken[N const usize](value i64) i64 {\n\
     if true { return value + bitcast[i64](N) }\n\
     }\n\
     fn test() i64 { return broken[1](0) }\n"
  in
  let issue45_program = expect_ok (Parser.parse (source issue45_source)) in
  (match Sema.check issue45_program with
  | Ok _ -> failwith "const-generic-specialization-span: expected missing-return error"
  | Error diagnostics ->
      let diagnostic = List.hd diagnostics in
      let rendered =
        Diag.render_all ~source:(Some (source issue45_source)) diagnostics
      in
      if
        diagnostic.primary.Span.file <> "regression.fas"
        || diagnostic.primary.Span.line <> 1
        || diagnostic.primary.Span.column <> 4
      then
        failwith
          ("const-generic-specialization-span: unexpected location: "
          ^ Span.to_string diagnostic.primary);
      if
        (not (contains rendered "specialized function `broken`"))
        || (not (contains rendered "fn broken[N const usize](value i64) i64 {"))
        || (not (contains rendered "while instantiating `broken[1]`"))
        || contains rendered "$spec$"
      then
        failwith
          ("const-generic-specialization-span: missing source excerpt: " ^ rendered));
  semantic_error "generic-function-arity"
    "generic function `pair` expects 2 generic arguments, got 1"
    "fn pair[A, B](value A) A { return value }\nfn test() i64 { return pair[i64](1) }\n";
  semantic_error "generic-function-argument-kind" "expected a type argument"
    "fn identity[T](value T) T { return value }\n\
     fn test() i64 { return identity[3](1) }\n";
  semantic_error "generic-call-to-concrete" "function `identity` is not generic"
    "fn identity(value i64) i64 { return value }\n\
     fn test() i64 { return identity[i64](1) }\n";
  semantic_error "unknown-generic-function" "unknown generic function `identity`"
    "fn test() i64 { return identity[i64](1) }\n";
  semantic_pin "generic-function-missing-type-arguments"
    "fn identity[T](value T) T { return value }\nfn test() i64 { return identity(1) }\n"
    2 24 11 "cannot infer `T` for `identity` from literals only"
    (Some "write `identity[i32](1)`");
  semantic_error "generic-function-unknown-type-argument" "unknown type `Missing`"
    "fn ignore[T]() i64 { return 7 }\nfn test() i64 { return ignore[Missing]() }\n";
  semantic_error "generic-name-as-value" "`f` is a function, not a value"
    "fn f[T]() usize { return 0 }\nfn test() usize { return f }\n";
  semantic_error "generic-specialization-as-value"
    "generic function `f` needs a call after its type arguments"
    "fn f[T]() usize { return 0 }\nfn test() usize { return f[i64] }\n";
  ignore
    (expect_ok
       (Sema.check
          (expect_ok
             (Parser.parse
                (source
                   "fn inner[T, M const usize](x T) u64 { return bitcast[u64](M) }\n\
                    fn outer[N const usize](x i64) u64 { return inner[i64, 3](x) }\n\
                    fn test() u64 { return outer[5](40) }\n")))));
  semantic_error "extern-c-type-parameter"
    "extern \"C\" functions cannot have type parameters"
    "extern \"C\" { fn identity[T](value T) T }\n";
  semantic_error "generic-main" "entry point `main` cannot have generic parameters"
    "fn main[T]() i64 { return 42 }\n";
  let main_signature_error =
    "entry point `main` must have signature `fn main() i32` or `fn main(argc i32, argv \
     addr) i32`"
  in
  List.iter
    (fun (name, text) -> semantic_message name main_signature_error text)
    [
      ("main-result-u32", "fn main() u32 { return 0 }\n");
      ("main-result-void", "fn main() void { return }\n");
      ("main-one-parameter", "fn main(argc i32) i32 { return 0 }\n");
      ("main-parameter-types", "fn main(argc u32, argv addr) i32 { return 0 }\n");
      ("main-c-signature", "extern \"C\" { fn main(argc i32, argv addr) i64 }\n");
    ];
  semantic_accept "main-no-arguments" "fn main() i32 { return 0 }\n";
  semantic_accept "main-argc-argv"
    "fn main(argc i32, argv addr) i32 { return argc - argc }\n";
  semantic_message "main-internal-global"
    "global `main` cannot be the program entry point" "var main i32 = 0\n";
  semantic_message "main-exported-global"
    "global `main` cannot be the program entry point"
    "extern \"C\" { var main i32 = 0 }\n";
  let forward_constant_use =
    ( "use.fas",
      "const SIZE usize = LATER_SIZE\n\
       const FLAG bool = LATER_FLAG\n\
       fn test() usize { return add[SIZE](choose[FLAG]()) }\n" )
  in
  let forward_constant_declarations =
    ( "declarations.fas",
      "const LATER_SIZE usize = 3\n\
       const LATER_FLAG bool = true\n\
       fn add[N const usize](value usize) usize { return value + N }\n\
       fn choose[Flag const bool]() usize {\n\
      \ if Flag { return 4 } else { return 0 }\n\
      \ }\n" )
  in
  List.iter
    (fun files -> ignore (expect_ok (check_files files) |> Lower.lower |> expect_ok))
    [
      [ forward_constant_use; forward_constant_declarations ];
      [ forward_constant_declarations; forward_constant_use ];
    ];
  let cross_file_generic_decls =
    ("decls.fas", "fn add[N const usize](value usize) usize { return value + N }\n")
  in
  let cross_file_generic_use_a =
    ("use_a.fas", "fn test() usize { return add[3](1) + add[3](2) }\n")
  in
  let cross_file_generic_use_b =
    ("use_b.fas", "fn other() usize { return add[4](3) + add[3](4) }\n")
  in
  List.iter
    (fun files ->
      let hir = expect_ok (check_files files) in
      let specialized =
        List.filter (fun (func : Hir.func) -> contains func.name "$spec$") hir.Hir.funcs
      in
      if List.length specialized <> 2 then
        failwith
          "cross-file-generic-reuse: canonical specializations were not reused across \
           files";
      if
        List.length
          (List.filter
             (fun (func : Hir.func) -> String.ends_with ~suffix:"N=usize:3" func.name)
             hir.Hir.funcs)
        <> 1
      then failwith "cross-file-generic-reuse: duplicate add[3] specialization";
      if
        List.length
          (List.filter
             (fun (func : Hir.func) -> String.ends_with ~suffix:"N=usize:4" func.name)
             hir.Hir.funcs)
        <> 1
      then failwith "cross-file-generic-reuse: missing add[4] specialization";
      ignore (Lower.lower hir |> expect_ok))
    [
      [ cross_file_generic_decls; cross_file_generic_use_a; cross_file_generic_use_b ];
      [ cross_file_generic_decls; cross_file_generic_use_b; cross_file_generic_use_a ];
      [ cross_file_generic_use_a; cross_file_generic_decls; cross_file_generic_use_b ];
      [ cross_file_generic_use_a; cross_file_generic_use_b; cross_file_generic_decls ];
      [ cross_file_generic_use_b; cross_file_generic_decls; cross_file_generic_use_a ];
      [ cross_file_generic_use_b; cross_file_generic_use_a; cross_file_generic_decls ];
    ];
  (match
     check_files
       [
         ("dup_a.fas", "fn add[N const usize](value usize) usize { return value + N }\n");
         ("dup_b.fas", "fn add[N const usize](value usize) usize { return value + N }\n");
       ]
   with
  | Ok _ -> failwith "cross-file-duplicate-template: duplicate template accepted"
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      if not (contains rendered "duplicate function `add`") then
        failwith ("cross-file-duplicate-template: unexpected diagnostic: " ^ rendered));
  let duplicate_across_files name expected_message first_file second_file first_text
      second_text =
    match check_files [ (first_file, first_text); (second_file, second_text) ] with
    | Error [ diagnostic ] ->
        if diagnostic.Diag.message <> expected_message then
          failwith (name ^ ": unexpected duplicate message: " ^ diagnostic.message);
        if diagnostic.primary.Span.file <> second_file then
          failwith (name ^ ": primary location does not name the second definition");
        let first_location = "first definition is at " ^ first_file ^ ":1:" in
        if not (List.exists (fun note -> contains note first_location) diagnostic.notes)
        then failwith (name ^ ": first definition location missing from notes")
    | Error diagnostics ->
        failwith
          (name ^ ": expected one duplicate diagnostic, got "
          ^ Diag.render_all ~source:None diagnostics)
    | Ok _ -> failwith (name ^ ": duplicate declaration was accepted")
  in
  duplicate_across_files "extern-duplicate-matching-signature"
    "duplicate function `shared`" "extern_first.fas" "extern_second.fas"
    "extern \"C\" { fn shared(value i32) i32 }\n"
    "extern \"C\" { fn shared(value i32) i32 }\n";
  duplicate_across_files "extern-duplicate-mismatching-signature"
    "duplicate function `shared`" "extern_mismatch_first.fas"
    "extern_mismatch_second.fas" "extern \"C\" { fn shared(value i32) i32 }\n"
    "extern \"C\" { fn shared(value i64) i32 }\n";
  duplicate_across_files "native-extern-collision" "duplicate function `shared`"
    "native_first.fas" "extern_collision.fas"
    "fn shared(value i32) i32 { return value }\n"
    "extern \"C\" { fn shared(value i32) i32 }\n";
  ignore
    (llvm_of
       "const NARROW u8 = trunc[u8](WIDE)\n\
        const WIDE u16 = 7\n\
        fn value[N const u8]() u8 { return N }\n\
        fn test() u8 { return value[NARROW]() }\n");
  semantic_error "forward-constant-type-preservation"
    "cannot use `u16` as `u8`: values above 255 would be lost"
    "const NARROW u8 = WIDE\nconst WIDE u16 = 7\n";
  semantic_error "forward-constant-cycle" "cyclic constant dependency"
    "const LEFT usize = RIGHT\nconst RIGHT usize = LEFT\n";
  let forward_array_scalar =
    llvm_of
      "const A arr[2, i64] = {B, 0}\nconst B i64 = 1\nfn test() i64 { return A[0] }\n"
  in
  if not (contains forward_array_scalar "[2 x i64] [i64 1, i64 0]") then
    failwith "forward-array-scalar: order-independent scalar did not resolve into array";
  semantic_error "forward-array-dependency-rejected"
    "expression is not compile-time constant"
    "const A arr[2, i64] = {H[0], 0}\n\
     const H arr[2, i64] = {1, 2}\n\
     fn test() i64 { return A[0] }\n";
  ignore
    (llvm_of
       "const LEFT bool = false && RIGHT\n\
        const RIGHT bool = LEFT\n\
        fn test() bool { return RIGHT }\n");
  let mixed_generic_source =
    "const THREE usize = 3\n\
     struct Box[T] { value T }\n\
     fn stamp[T, N const usize](value T) T { seen usize = N\n\
     return value }\n\
     fn wrap[T, N const usize](value T) T { seen usize = N\n\
     result Box[T] = {stamp[T, N](value)}\n\
     return result.value }\n\
     fn test() i64 { first i64 = wrap[i64, THREE](7)\n\
     return wrap[i64, THREE](first) }\n"
  in
  let mixed_generic_hir =
    expect_ok (Parser.parse (source mixed_generic_source)) |> Sema.check |> expect_ok
  in
  let mixed_specializations =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      mixed_generic_hir.Hir.funcs
  in
  if List.length mixed_specializations <> 2 then
    failwith "mixed-generic-deduplication: expected two concrete functions";
  if
    not
      (List.for_all
         (fun (func : Hir.func) -> List.length (positions func.name "$spec$") = 1)
         mixed_specializations)
  then failwith "mixed-generic-key: final names did not use one canonical key";
  if
    not
      (List.for_all
         (fun (func : Hir.func) ->
           match func.params with [ { ty = Hir.Int Hir.I64; _ } ] -> true | _ -> false)
         mixed_specializations)
  then failwith "mixed-generic-substitution: type argument was not substituted";
  if
    List.length
      (List.filter
         (fun (definition : Hir.struct_def) -> contains definition.name "Box$spec$")
         mixed_generic_hir.Hir.structs)
    <> 1
  then failwith "mixed-generic-struct-use: concrete struct was not materialized";
  let mixed_generic_llvm = Ir.render (expect_ok (Lower.lower mixed_generic_hir)) in
  if mixed_generic_llvm <> llvm_of mixed_generic_source then
    failwith "mixed-generic-order: generated output was not deterministic";
  if contains mixed_generic_llvm "@stamp(" || contains mixed_generic_llvm "@wrap(" then
    failwith "mixed-generic-template: template reached LLVM output";
  if not (contains mixed_generic_llvm "store i64 3") then
    failwith "mixed-generic-const-substitution: const value did not reach the body";
  let mixed_layout_argument =
    llvm_of
      "struct Sized[T] { value T }\n\
       fn size[T, N const usize]() usize { return N }\n\
       fn test() usize { return size[Sized[i64], sizeof[Sized[i64]]]() }\n"
  in
  if not (contains mixed_layout_argument "ret i64 8\n") then
    failwith
      "mixed-generic-layout-argument: layout was not available to const evaluation";
  let interleaved_generic =
    expect_ok
      (Sema.check
         (expect_ok
            (Parser.parse
               (source
                  "fn sum[A, N const usize, B, M const usize](left A, right B) usize {\n\
                   return N + M }\n\
                   fn test() usize { return sum[i64, 2, u8, 3](7, 1) }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "sum$spec$")
         interleaved_generic.Hir.funcs)
    <> 1
  then failwith "mixed-generic-ordering: interleaved parameters were not preserved";
  let distinct_mixed_specializations =
    expect_ok
      (Sema.check
         (expect_ok
            (Parser.parse
               (source
                  "fn value[T, N const usize](input T) usize { return N }\n\
                   fn test() usize { return value[i64, 1](7) + value[i64, 2](7) }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "value$spec$")
         distinct_mixed_specializations.Hir.funcs)
    <> 2
  then failwith "mixed-generic-key: distinct const arguments shared a specialization";
  let mixed_specialization_limits = { Limits.default with max_specializations = 2 } in
  ignore
    (expect_ok
       (Sema.check ~limits:mixed_specialization_limits
          (expect_ok
             (Parser.parse
                (source
                   "fn value[T, N const usize](input T) usize { return N }\n\
                    fn test() usize { return value[i64, 1](7) + value[i64, 1](7) }\n")))));
  (match
     Sema.check ~limits:mixed_specialization_limits
       (expect_ok
          (Parser.parse
             (source
                "fn value[T, N const usize](input T) usize { return N }\n\
                 fn test() usize { return value[i64, 1](7) + value[i64, 2](7) }\n")))
   with
  | Ok _ -> failwith "mixed-generic-count-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "const specialization count limit exceeded")
      then failwith "mixed-generic-count-limit: unexpected diagnostic");
  semantic_error "mixed-generic-arity"
    "generic function `identity` expects 2 generic arguments, got 1"
    "fn identity[T, N const usize](value T) T { return value }\n\
     fn test() i64 { return identity[i64](1) }\n";
  semantic_error "mixed-generic-type-argument-kind" "expected a type argument"
    "fn identity[T, N const usize](value T) T { return value }\n\
     fn test() i64 { return identity[3, 1](1) }\n";
  semantic_error "mixed-generic-const-argument-kind" "expected a const argument"
    "fn identity[T, N const usize](value T) T { return value }\n\
     fn test() i64 { return identity[i64, u8](1) }\n";
  semantic_error "mixed-generic-instantiated-body-error" "address arithmetic for `+`"
    "fn bad[T, N const usize](value T) T { seen usize = N\n\
     return value + value }\n\
     fn test(value addr) addr { return bad[addr, 1](value) }\n";
  let direct_recursive_limits = { Limits.default with max_specialization_depth = 1 } in
  let direct_recursive_specialization =
    expect_ok
      (Sema.check ~limits:direct_recursive_limits
         (expect_ok
            (Parser.parse
               (source
                  "fn recurse[T](value T, count u8) T {\n\
                   if count == 0 { return value }\n\
                   return recurse[T](value, count - 1) }\n\
                   fn test() u8 { return recurse[u8](7, 2) }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "recurse$spec$")
         direct_recursive_specialization.Hir.funcs)
    <> 1
  then failwith "generic-function-recursion: expected one concrete function";
  ignore (expect_ok (Lower.lower direct_recursive_specialization));
  let mutual_recursive_limits = { Limits.default with max_specialization_depth = 2 } in
  let mutual_recursive_specializations =
    expect_ok
      (Sema.check ~limits:mutual_recursive_limits
         (expect_ok
            (Parser.parse
               (source
                  "fn left[T](value T, count u8) T {\n\
                   if count == 0 { return value }\n\
                   return right[T](value, count - 1) }\n\
                   fn right[T](value T, count u8) T {\n\
                   if count == 0 { return value }\n\
                   return left[T](value, count - 1) }\n\
                   fn test() u8 { return left[u8](7, 2) }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) ->
           contains func.name "left$spec$" || contains func.name "right$spec$")
         mutual_recursive_specializations.Hir.funcs)
    <> 2
  then failwith "generic-function-mutual-recursion: expected two concrete functions";
  ignore (expect_ok (Lower.lower mutual_recursive_specializations));
  let recursive_function_limits =
    { Limits.default with max_specialization_depth = 2 }
  in
  (match
     Sema.check ~limits:recursive_function_limits
       (expect_ok
          (Parser.parse
             (source
                "fn grow[T]() i64 { return grow[arr[2, T]]() }\n\
                 fn test() i64 { return grow[u8]() }\n")))
   with
  | Ok _ -> failwith "generic-function-depth-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "function specialization recursion depth limit exceeded")
      then failwith "generic-function-depth-limit: unexpected diagnostic");
  let one_function_limit = { Limits.default with max_specializations = 1 } in
  ignore
    (expect_ok
       (Sema.check ~limits:one_function_limit
          (expect_ok
             (Parser.parse
                (source
                   "fn identity[T](value T) T { return value }\n\
                    fn test() i64 { return identity[i64](identity[i64](1)) }\n")))));
  let canonical_type_specialization =
    expect_ok
      (Sema.check ~limits:one_function_limit
         (expect_ok
            (Parser.parse
               (source
                  "fn identity[T](pointer addr) addr { return pointer }\n\
                   fn test() usize { right arr[01, u8] = {1}\n\
                   first addr = identity[arr[01, u8]](&right)\n\
                   second addr = identity[arr[1, u8]](first)\n\
                   return sizeof[arr[1, u8]] }\n"))))
  in
  if
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name "identity$spec$")
         canonical_type_specialization.Hir.funcs)
    <> 1
  then failwith "generic-function-canonical-key: expected one concrete function";
  (match
     Sema.check ~limits:one_function_limit
       (expect_ok
          (Parser.parse
             (source
                "fn identity[T](value T) T { return value }\n\
                 fn test() i64 { a u8 = identity[u8](1)\n\
                 return identity[i64](1) }\n")))
   with
  | Ok _ -> failwith "generic-function-count-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "function specialization count limit exceeded")
      then failwith "generic-function-count-limit: unexpected diagnostic");

  let generic_struct_source =
    "struct Box[T] { value T pointer addr }\n\
     struct Pair[A, B] { first A second B }\n\
     struct Wrapper[T] { boxed Box[T] }\n\
     fn test() usize {\n\
    \ box Box[i64] = {7, addr_from_bits(0)}\n\
    \ pair64 Pair[i64, u8] = {9, 1}\n\
    \ pair32 Pair[u32, u8] = {9, 1}\n\
    \ wrapped Wrapper[u8] = {{1, addr_from_bits(0)}}\n\
    \ return sizeof[Box[i64]] + sizeof[Pair[i64, u8]]\n\
     }\n"
  in
  let generic_struct_hir =
    expect_ok (Parser.parse (source generic_struct_source)) |> Sema.check |> expect_ok
  in
  let specialization_named base (definition : Hir.struct_def) =
    let prefix = base ^ "$spec$" in
    String.length definition.name >= String.length prefix
    && String.sub definition.name 0 (String.length prefix) = prefix
  in
  let box_specializations =
    List.filter (specialization_named "Box") generic_struct_hir.Hir.structs
  in
  if List.length box_specializations <> 2 then
    failwith "generic-struct-deduplication: expected two concrete Box layouts";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           match definition.fields with
           | [ { ty = Hir.Int Hir.I64; _ }; { ty = Hir.Addr; _ } ] -> true
           | _ -> false)
         box_specializations)
  then failwith "generic-struct-substitution: field substitution failed";
  let pair_sizes =
    generic_struct_hir.Hir.structs
    |> List.filter (specialization_named "Pair")
    |> List.map (fun (definition : Hir.struct_def) -> definition.size)
    |> List.sort compare
  in
  if pair_sizes <> [ 8; 16 ] then
    failwith
      "generic-struct-layouts: distinct arguments did not produce distinct layouts";
  let generic_struct_llvm = Ir.render (expect_ok (Lower.lower generic_struct_hir)) in
  if generic_struct_llvm <> llvm_of generic_struct_source then
    failwith "generic-struct-order: generated output was not deterministic";
  if contains generic_struct_llvm "%struct.Box = type" then
    failwith "generic-struct-template: template reached LLVM output";
  if not (contains generic_struct_llvm "%struct.Box$spec$") then
    failwith "generic-struct-lowering: concrete specialization did not reach LLVM";
  let unused_generic_struct =
    expect_ok
      (Sema.check
         (expect_ok
            (Parser.parse
               (source "struct Unused[T] { value T }\nfn test() i64 { return 0 }\n"))))
  in
  if unused_generic_struct.Hir.structs <> [] then
    failwith "generic-struct-template: unused template was emitted";
  semantic_error "generic-struct-invalid-alignment"
    "alignment must be a positive power of two"
    "struct Bad[T] @align(3) { value T }\nfn test() i64 { return 0 }\n";
  semantic_error "generic-struct-bare-use"
    "generic struct `Box` requires type arguments"
    "struct Box[T] { value T }\nfn test(value Box) i64 { return 0 }\n";
  semantic_error "generic-struct-arity"
    "generic struct `Pair` expects 2 generic arguments, got 1"
    "struct Pair[A, B] { first A second B }\n\
     fn test(value Pair[i64]) i64 { return 0 }\n";
  semantic_error "generic-struct-argument-kind" "expected a type argument"
    "struct Box[T] { value T }\nfn test(value Box[3]) i64 { return 0 }\n";
  semantic_error "generic-struct-aggregate-limit"
    "aggregate element count exceeds the configured limit"
    "struct Box[T] { value T }\n\
     fn consume(value Box[arr[1000001,u8]]) i32 { return 0 }\n\
     fn main() i32 { return 0 }\n";
  semantic_error "generic-application-to-concrete" "struct `Box` is not generic"
    "struct Box { value i64 }\nfn test(value Box[i64]) i64 { return 0 }\n";
  semantic_error "unknown-generic-struct" "unknown generic struct `Missing`"
    "fn test(value Missing[i64]) i64 { return 0 }\n";
  semantic_error "generic-struct-duplicate-param" "duplicate generic parameter `T`"
    "struct Pair[T, T] { first T second T }\nfn test() i64 { return 0 }\n";

  let const_generic_struct_source =
    "struct Buffer[T, N const usize] { data arr[N, T] }\n\
     struct Bytes[N const usize] { data arr[N, u8] }\n\
     struct Lanes[T, N const usize] { data vec[N, T] }\n\
     struct Wrapped[T, N const usize] { value Buffer[T, N] }\n\
     fn test() usize {\n\
    \ return sizeof[Buffer[u8, 3]] + sizeof[Buffer[u8, 1 + 2]] + "
    ^ "sizeof[Buffer[u16, 3]] + sizeof[Buffer[u8, 4]] + "
    ^ "sizeof[Bytes[5]] + sizeof[Lanes[u32, 4]] + sizeof[Wrapped[u8, 3]]\n}\n"
  in
  let const_generic_struct_hir =
    expect_ok (Parser.parse (source const_generic_struct_source))
    |> Sema.check |> expect_ok
  in
  let buffer_specializations =
    List.filter (specialization_named "Buffer") const_generic_struct_hir.Hir.structs
  in
  if List.length buffer_specializations <> 3 then
    failwith "const-generic-struct-deduplication: expected three concrete layouts";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           match definition.fields with
           | [ { ty = Hir.Array (3, Hir.Int Hir.U8); _ } ] -> true
           | _ -> false)
         buffer_specializations)
  then failwith "const-generic-struct-substitution: array length was not substituted";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           specialization_named "Bytes" definition
           &&
           match definition.fields with
           | [ { ty = Hir.Array (5, Hir.Int Hir.U8); _ } ] -> true
           | _ -> false)
         const_generic_struct_hir.Hir.structs)
  then
    failwith "const-generic-struct-substitution: const-only layout was not materialized";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           specialization_named "Lanes" definition
           &&
           match definition.fields with
           | [ { ty = Hir.Vec (4, Hir.Int Hir.U32); _ } ] -> true
           | _ -> false)
         const_generic_struct_hir.Hir.structs)
  then failwith "const-generic-struct-substitution: vector length was not substituted";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           specialization_named "Wrapped" definition
           &&
           match definition.fields with
           | [ { ty = Hir.Struct name; _ } ] -> contains name "Buffer$spec$"
           | _ -> false)
         const_generic_struct_hir.Hir.structs)
  then failwith "const-generic-struct-nesting: outer const value was not forwarded";
  let const_generic_struct_llvm =
    Ir.render (expect_ok (Lower.lower const_generic_struct_hir))
  in
  if const_generic_struct_llvm <> llvm_of const_generic_struct_source then
    failwith "const-generic-struct-order: generated output was not deterministic";
  if
    contains const_generic_struct_llvm "%struct.Buffer = type"
    || contains const_generic_struct_llvm "%struct.Bytes = type"
    || contains const_generic_struct_llvm "%struct.Lanes = type"
    || contains const_generic_struct_llvm "%struct.Wrapped = type"
  then failwith "const-generic-struct-template: template reached LLVM output";
  let bool_const_generic_struct_hir =
    expect_ok
      (Parser.parse
         (source
            "struct Tagged[Flag const bool] { value u8 }\n\
             fn test() usize {\n\
            \ return sizeof[Tagged[true]] + sizeof[Tagged[false]] + \
             sizeof[Tagged[trunc[bool](3)]]\n\
             }\n"))
    |> Sema.check |> expect_ok
  in
  let tagged_specializations =
    List.filter
      (specialization_named "Tagged")
      bool_const_generic_struct_hir.Hir.structs
  in
  if List.length tagged_specializations <> 2 then
    failwith "const-generic-struct-bool: expected two typed specializations";
  let issue33_llvm =
    llvm_of
      "struct Bytes[N const usize] { data arr[N, u8] }\n\
       fn test() usize { value Bytes[3]\n\
      \ return sizeof[Bytes[3]] }\n"
  in
  if not (contains issue33_llvm "%\"struct.Bytes$spec$c7:usize:3\" = type { [3 x i8] }")
  then failwith "const-generic-struct-llvm-name: specialization name was not quoted";
  if not (contains issue33_llvm "alloca %\"struct.Bytes$spec$c7:usize:3\"") then
    failwith "const-generic-struct-llvm-use: local specialization type was not quoted";
  semantic_error "const-generic-struct-arity"
    "generic struct `Buffer` expects 2 generic arguments, got 1"
    "struct Buffer[T, N const usize] { data arr[N, T] }\n\
     fn test(value Buffer[u8]) i64 { return 0 }\n";
  semantic_error "const-generic-struct-argument-kind" "expected a const argument"
    "struct Buffer[T, N const usize] { data arr[N, T] }\n\
     fn test(value Buffer[u8, u16]) i64 { return 0 }\n";
  semantic_error "const-generic-struct-argument-type"
    "cannot use `usize` as `u8`: values above 255 would be lost"
    "struct Buffer[T, N const u8] { data arr[N, T] }\n\
     fn test(value Buffer[u8, sizeof[u8]]) i64 { return 0 }\n";
  semantic_message "const-generic-struct-negative-length"
    "cannot use `isize` as `usize`: negative values change meaning"
    "struct Buffer[T, N const isize] { data arr[N, T] }\n\
     fn test(value Buffer[u8, -1]) i64 { return 0 }\n";

  let global_const_generic_struct_source =
    "struct Unit { value u64 }\n\
     struct Box[T] { value T }\n\
     const THREE usize = 3\n\
     const BOX_BYTES usize = sizeof[Box[u16]]\n\
     struct Buffer[T, N const usize] { data arr[N, T] }\n\
     struct Holder { value Buffer[u8, THREE] }\n\
     fn fixed[T](pointer addr) void { result T\n\
     return }\n\
     fn test() usize { three Buffer[u8, THREE]\n\
     boxed Buffer[u8, BOX_BYTES]\n\
     unit Buffer[u8, sizeof[Unit]]\n\
     fixed[Buffer[u8, THREE]](&three)\n\
     return sizeof[Holder] + sizeof[Buffer[u8, BOX_BYTES]] + sizeof[Buffer[u8, \
     sizeof[Unit]]]\n\
     }\n"
  in
  let global_const_generic_struct_hir =
    expect_ok (Parser.parse (source global_const_generic_struct_source))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (specialization_named "Buffer")
         global_const_generic_struct_hir.Hir.structs)
    <> 3
  then
    failwith "const-generic-struct-global-const: expected three concrete Buffer layouts";
  if
    not
      (List.exists
         (fun (definition : Hir.struct_def) ->
           definition.name = "Holder"
           &&
           match definition.fields with
           | [ { ty = Hir.Struct name; _ } ] -> contains name "Buffer$spec$"
           | _ -> false)
         global_const_generic_struct_hir.Hir.structs)
  then
    failwith "const-generic-struct-global-field: named const did not resolve in a field";
  let global_const_generic_struct_llvm =
    Ir.render (expect_ok (Lower.lower global_const_generic_struct_hir))
  in
  if global_const_generic_struct_llvm <> llvm_of global_const_generic_struct_source then
    failwith "const-generic-struct-global-order: generated output was not deterministic";
  semantic_accept "const-generic-struct-lossless-argument"
    "const COUNT u8 = 3\n\
     struct Buffer[T, N const usize] { data arr[N, T] }\n\
     fn test() void { value Buffer[u8, COUNT] = {}\n\
    \ return }\n";

  let const_generic_struct_type_argument_source =
    "const THREE usize = 3\n\
     const FOUR usize = 4\n\
     struct Buffer[T, N const usize] { data arr[N, T] }\n\
     fn identity[T](pointer addr) addr { return pointer }\n\
     fn forward[T](pointer addr) addr { return identity[T](pointer) }\n\
     fn test() usize { three Buffer[u8, THREE]\n\
     four Buffer[u8, FOUR]\n\
     a addr = identity[Buffer[u8, THREE]](&three)\n\
     b addr = identity[Buffer[u8, FOUR]](&four)\n\
     forward[Buffer[u8, THREE]](a)\n\
     forward[Buffer[u8, FOUR]](b)\n\
     return sizeof[Buffer[u8, THREE]] + sizeof[Buffer[u8, FOUR]]\n\
     }\n"
  in
  let const_generic_struct_type_argument_hir =
    expect_ok (Parser.parse (source const_generic_struct_type_argument_source))
    |> Sema.check |> expect_ok
  in
  let specialized_functions name =
    List.filter
      (fun (func : Hir.func) -> contains func.name (name ^ "$spec$"))
      const_generic_struct_type_argument_hir.Hir.funcs
  in
  if List.length (specialized_functions "identity") <> 2 then
    failwith "const-generic-struct-type-key: distinct identity types were merged";
  if List.length (specialized_functions "forward") <> 2 then
    failwith "const-generic-struct-type-key: nested specializations were merged";
  if
    List.length
      (List.filter
         (specialization_named "Buffer")
         const_generic_struct_type_argument_hir.Hir.structs)
    <> 2
  then failwith "const-generic-struct-type-key: distinct layouts were merged";
  let const_generic_struct_type_argument_llvm =
    Ir.render (expect_ok (Lower.lower const_generic_struct_type_argument_hir))
  in
  if
    const_generic_struct_type_argument_llvm
    <> llvm_of const_generic_struct_type_argument_source
  then failwith "const-generic-struct-type-key: generated output was not deterministic";

  let deferred_nested_struct_source =
    "struct Inner[N const usize] { data arr[N, u8] }\n\
     struct Outer[T] { value T }\n\
     fn pass[T, N const usize](pointer addr) void {\n\
     value Outer[Inner[N]]\n\
     return\n\
     }\n\
     fn test() void { value Outer[Inner[1]]\n\
     pass[u8, 1](&value)\n\
     return\n\
     }\n"
  in
  let deferred_nested_struct_hir =
    expect_ok (Parser.parse (source deferred_nested_struct_source))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (specialization_named "Inner")
         deferred_nested_struct_hir.Hir.structs)
    <> 1
  then failwith "nested-const-struct-deferral: expected one concrete inner layout";
  if
    List.length
      (List.filter
         (specialization_named "Outer")
         deferred_nested_struct_hir.Hir.structs)
    <> 1
  then failwith "nested-const-struct-deferral: expected one concrete outer layout";
  let deferred_nested_struct_llvm =
    Ir.render (expect_ok (Lower.lower deferred_nested_struct_hir))
  in
  if
    contains deferred_nested_struct_llvm "Inner["
    || contains deferred_nested_struct_llvm "Outer["
  then failwith "nested-const-struct-deferral: unresolved type reached LLVM";

  let const_generic_function_type_source =
    "const THREE usize = 3\n\
     fn array_identity[T, N const usize](pointer addr) addr { return pointer }\n\
     fn array_outer[T, N const usize](pointer addr) addr {\n\
     return array_identity[T, N](pointer)\n\
     }\n\
     fn byte_identity[N const usize](pointer addr) addr { return pointer }\n\
     fn vector_identity[T, N const usize](value vec[N, T]) vec[N, T] {\n\
     return value\n\
     }\n\
     fn aggregate_metrics[T, N const usize]() usize {\n\
     return sizeof[arr[N, T]] + alignof[arr[N, T]] + sizeof[vec[N, T]]\n\
     }\n\
     fn test() usize { value arr[3, u8] = {1, 2, 3}\n\
     pointer addr = array_outer[u8, THREE](&value)\n\
     forwarded addr = array_outer[u8, 3](pointer)\n\
     bytes addr = byte_identity[THREE](&value)\n\
     lanes vec[4, u16] = splat(2)\n\
     same_lanes vec[4, u16] = vector_identity[u16, 4](lanes)\n\
     return len(value) + sizeof[vec[4, u16]] + aggregate_metrics[u16, 4]()\n\
     }\n"
  in
  let const_generic_function_type_hir =
    expect_ok (Parser.parse (source const_generic_function_type_source))
    |> Sema.check |> expect_ok
  in
  let function_specializations =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      const_generic_function_type_hir.Hir.funcs
  in
  if List.length function_specializations <> 5 then
    failwith "const-generic-function-type-deduplication: expected five functions";
  if
    not
      (List.exists
         (fun (func : Hir.func) ->
           contains func.name "array_identity$spec$"
           &&
           match (func.params, func.ret) with
           | [ { ty = Hir.Addr; _ } ], Hir.Addr -> true
           | _ -> false)
         function_specializations)
  then failwith "const-generic-function-type-signature: length was not substituted";
  if
    not
      (List.exists
         (fun (func : Hir.func) ->
           contains func.name "aggregate_metrics$spec$"
           && func.params = [] && func.ret = Hir.Int Hir.Usize)
         function_specializations)
  then
    failwith "const-generic-function-type-nesting: aggregate length was not substituted";
  let const_generic_function_type_llvm =
    Ir.render (expect_ok (Lower.lower const_generic_function_type_hir))
  in
  if const_generic_function_type_llvm <> llvm_of const_generic_function_type_source then
    failwith "const-generic-function-type-order: generated output was not deterministic";
  if
    contains const_generic_function_type_llvm "@array_identity("
    || contains const_generic_function_type_llvm "@array_outer("
    || contains const_generic_function_type_llvm "@byte_identity("
    || contains const_generic_function_type_llvm "@vector_identity("
    || contains const_generic_function_type_llvm "@aggregate_metrics("
  then failwith "const-generic-function-type-template: template reached LLVM output";
  semantic_error "const-generic-function-type-mismatch"
    "aggregate parameter `value` of type `arr[N,u8]` cannot be passed by value; \
     declare `value` as `addr`; callers pass its address with `&`"
    "fn identity[N const usize](value arr[N, u8]) arr[N, u8] { return value }\n";
  semantic_message "const-generic-function-negative-length"
    "cannot use `isize` as `usize`: negative values change meaning"
    "fn size[N const isize]() usize { return sizeof[arr[N,u8]] }\n\
     fn test() usize { return size[-2]() }\n";
  semantic_message "const-generic-function-machine-length"
    "array length `9223372036854775808` is too large"
    "fn size[N const u64]() usize { return sizeof[arr[N,u8]] }\n\
     fn test() usize { return size[9223372036854775808]() }\n";
  let sizeof_large_array_source =
    "fn f() usize { return sizeof[arr[9223372036854775808,u8]] }\n"
  in
  semantic_pin "sizeof-large-array-length" sizeof_large_array_source 1
    (String.index sizeof_large_array_source '9' + 1)
    19 "array length `9223372036854775808` is too large" None;
  let sizeof_negative_array_source =
    "const N isize = -2\nfn f() usize { return sizeof[arr[N,u8]] }\n"
  in
  semantic_pin "sizeof-negative-array-length" sizeof_negative_array_source 2
    (String.index_from sizeof_negative_array_source
       (String.index sizeof_negative_array_source '\n' + 1)
       'N'
    - String.index sizeof_negative_array_source '\n')
    1 "cannot use `isize` as `usize`: negative values change meaning"
    (Some
       "reinterpret the bits with `bitcast[usize](N)`, or widen with `sext`/`zext` \
        first if that is what you mean");
  let const_array_len_generic_llvm =
    llvm_of
      "const DATA arr[3, u8] = { 10, 20, 30 }\n\
       fn width[N const usize]() usize { return N }\n\
       fn test() usize { return width[len(DATA)]() }\n"
  in
  if not (contains const_array_len_generic_llvm "ret i64 3\n") then
    failwith "const-generic-array-len: array length was not used as a const argument";
  ignore
    (llvm_of
       "fn count[N const i64](value i64) i64 {\n\
       \ if N > 0 { return count[N - 1](value + 1) }\n\
       \ return value\n\
        }\n\
        fn test() i64 { return count[0](41) }\n");
  let runtime_const_generic_if =
    llvm_of
      "fn choose[N const i64](value i64, condition i64) i64 {\n\
      \ if condition > 0 { return value + N } else { return value - N }\n\
       }\n\
       fn test() i64 { return choose[2](40, 0) }\n"
  in
  if not (contains runtime_const_generic_if "br i1") then
    failwith "const-generic-runtime-if: runtime condition was pruned";
  let shadowed_specialization_condition =
    llvm_of
      "const Flag bool = false\n\
       fn choose[N const i32](Flag bool) i32 {\n\
      \ if Flag && true { return 7 }\n\
      \ return 3\n\
       }\n\
       fn main() i32 { return choose[1](true) + choose[1](false) }\n"
  in
  if not (contains shadowed_specialization_condition "br i1") then
    failwith "shadowed-specialization-condition: runtime parameter branch was pruned";
  let const_dependent_if =
    llvm_of
      "fn choose[Flag const i32]() i32 {\n\
      \ if Flag == 1 { return 7 } else { return 3 }\n\
       }\n\
       fn main() i32 { return choose[1]() + choose[0]() }\n"
  in
  if
    (not (contains const_dependent_if "ret i32 7\n"))
    || not (contains const_dependent_if "ret i32 3\n")
  then failwith "const-dependent-if: specialization branches were not selected";
  if contains const_dependent_if "br i1" then
    failwith "const-dependent-if: selected specializations retained runtime branches";
  let () =
    semantic_error "nondependent-specialization-condition" "unknown name `Missing`"
      "const Flag bool = false\n\
       fn choose[N const i32]() i32 {\n\
      \ if Flag { return Missing }\n\
      \ return 3\n\
       }\n\
       fn main() i32 { return choose[1]() }\n"
  in
  let () =
    semantic_error "unselected-specialization-unknown-value" "unknown name `Missing`"
      "fn choose[N const i32]() i32 {\n\
      \ if N == 1 { return 7 } else { return Missing }\n\
       }\n\
       fn main() i32 { return choose[1]() }\n"
  in
  let () =
    semantic_error "unselected-specialization-unknown-function"
      "unknown function `missing`"
      "fn choose[N const i32]() i32 {\n\
      \ if N == 1 { return 7 } else { return missing() }\n\
       }\n\
       fn main() i32 { return choose[1]() }\n"
  in
  semantic_error "unselected-specialization-unknown-generic"
    "unknown generic function `missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return missing[N]() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-unknown-type" "unknown type `Missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { value Missing\n\
    \ return 3 }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-unknown-target"
    "unknown assignment target `Missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { Missing = 3\n\
    \ return 3 }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-unknown-length" "unknown name `Missing`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { values arr[Missing,i32]\n\
    \ return 3 }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-type-as-value" "type, not a value"
    "opaque Item\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return Item }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-const-as-function" "value, not a function"
    "const Item i32 = 2\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return Item() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-local-shadows-function"
    "value, not a function"
    "fn Item() i32 { return 2 }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { Item i32 = 3\n\
    \ return Item() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-const-target"
    "constant `Item` is not assignable"
    "const Item i32 = 2\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { Item = 3\n\
    \ return Item }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-invalid-operation"
    "operator `+` needs integer or integer vector operands"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return true + true }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-local-type-mismatch"
    "bool and integer values use different representations"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { value i32 = true\n\
    \ return value }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-assignment-type-mismatch"
    "bool and integer values use different representations"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { value i32 = 0\n\
    \ value = true\n\
    \ return value }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-call-type-mismatch"
    "bool and integer values use different representations"
    "fn plain(value i32) i32 { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain(true) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-dependent-call-sibling"
    "bool and integer values use different representations"
    "fn plain(left i32, right i32) i32 { return left + right }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain(N, true) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-builtin-type"
    "builtin argument must be an integer"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return popcount(true) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-illegal-cast"
    "illegal cast for source and destination widths"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return zext[u8](256) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-fixed-cast-target"
    "illegal cast for source and destination widths"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return zext[void](N) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-nongeneric-application"
    "function `plain` is not generic"
    "fn plain(value i32) i32 { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[N](1) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-wrong-generic-arity"
    "generic function `plain` expects 1 generic argument, got 2"
    "fn plain[T](value T) T { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[i32, i32](1) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-wrong-generic-kind"
    "expected a type argument"
    "fn plain[T](value T) T { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[1](1) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-generic-call-type"
    "bool and integer values use different representations"
    "fn plain[T](value T) T { return value }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[i32](true) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-const-argument-range"
    "integer literal is out of range for u8"
    "fn plain[N const u8]() i32 { return 1 }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[256]() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  semantic_error "unselected-specialization-named-const-argument-type"
    "cannot use `i32` as `u8`: negative values change meaning"
    "const Wide i32 = 7\n\
     fn plain[N const u8]() i32 { return 1 }\n\
     fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { return plain[Wide]() }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  ignore
    (llvm_of
       "const Wide i32 = 7\n\
        fn plain[N const u8]() i32 { return zext[i32](N) }\n\
        fn choose[N const i32]() i32 {\n\
       \ if N == 1 { return 7 } else { return plain[trunc[u8](Wide)]() }\n\
        }\n\
        fn main() i32 { return choose[1]() }\n");
  ignore
    (llvm_of
       "fn choose[N const i32](value i32) i32 {\n\
       \ if N == 1 { return value } else { return value + N }\n\
        }\n\
        fn main() i32 { return choose[1](7) }\n");
  semantic_error "unselected-specialization-duplicate-local" "duplicate local `value`"
    "fn choose[N const i32]() i32 {\n\
    \ if N == 1 { return 7 } else { value i32 = 1\n\
    \ value i32 = 2\n\
    \ return value }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n";
  ignore
    (llvm_of
       "fn choose[N const i32](value i32) i32 {\n\
       \ prior i32 = value\n\
       \ if N == 1 { return prior } else { result i32 = prior\n\
       \ return result }\n\
        }\n\
        fn main() i32 { return choose[1](7) }\n");
  ignore
    (llvm_of
       "fn choose[N const i32]() i32 {\n\
       \ if N == 1 { return 7 } else { value i32 = 1\n\
       \ { value i32 = 2 }\n\
       \ return value }\n\
        }\n\
        fn main() i32 { return choose[1]() }\n");
  ignore
    (llvm_of
       "const One i32 = 1\n\
        fn leaf[N const i32]() i32 { return N }\n\
        fn choose[N const i32]() i32 {\n\
       \ if N == 1 { return 7 } else { return leaf[One]() }\n\
        }\n\
        fn main() i32 { return choose[1]() }\n");

  let const_generic_struct_function_source =
    "const THREE usize = 3\n\
     struct Unit { value u64 }\n\
     struct Buffer[T, N const usize] { data arr[N, T] }\n\
     struct Wrapped[T, N const usize] { value Buffer[T, N] }\n\
     fn pass[T, N const usize](pointer addr) void {\n\
     value Buffer[T, N]\n\
     return\n\
     }\n\
     fn wrap[T, N const usize](pointer addr) void {\n\
     value Buffer[T, N]\n\
     pass[T, N](&value)\n\
     wrapped Wrapped[T, N]\n\
     return\n\
     }\n\
     fn sized(pointer addr) void {\n\
     pass[u8, sizeof[Unit]](pointer)\n\
     return\n\
     }\n\
     fn test() void { value Buffer[u8, 3]\n\
     wrap[u8, THREE](&value)\n\
     sized(&value)\n\
     return\n\
     }\n"
  in
  let const_generic_struct_function_hir =
    expect_ok (Parser.parse (source const_generic_struct_function_source))
    |> Sema.check |> expect_ok
  in
  if
    List.length
      (List.filter
         (specialization_named "Buffer")
         const_generic_struct_function_hir.Hir.structs)
    <> 2
  then
    failwith "const-generic-struct-function-deduplication: expected two Buffer layouts";
  if
    List.length
      (List.filter
         (specialization_named "Wrapped")
         const_generic_struct_function_hir.Hir.structs)
    <> 1
  then failwith "const-generic-struct-function-nesting: expected one Wrapped layout";
  let struct_function_specializations =
    List.filter
      (fun (func : Hir.func) -> contains func.name "$spec$")
      const_generic_struct_function_hir.Hir.funcs
  in
  if List.length struct_function_specializations <> 3 then
    failwith
      "const-generic-struct-function-discovery: expected three concrete functions";
  if
    not
      (List.for_all
         (fun (func : Hir.func) ->
           match func.params with [ { ty = Hir.Addr; _ } ] -> true | _ -> false)
         struct_function_specializations)
  then
    failwith
      "const-generic-struct-function-signature: applied type was not materialized";
  let const_generic_struct_function_llvm =
    Ir.render (expect_ok (Lower.lower const_generic_struct_function_hir))
  in
  if const_generic_struct_function_llvm <> llvm_of const_generic_struct_function_source
  then
    failwith
      "const-generic-struct-function-order: generated output was not deterministic";

  let recursive_struct_limits = { Limits.default with max_specialization_depth = 1 } in
  ignore
    (expect_ok
       (Sema.check ~limits:recursive_struct_limits
          (expect_ok
             (Parser.parse
                (source
                   "struct Node[T] { next addr value T }\n\
                    fn test() i64 { value Node[u8]\n\
                   \ return 0 }\n")))));
  (match
     Sema.check ~limits:recursive_struct_limits
       (expect_ok
          (Parser.parse
             (source
                "struct Inner[T] { value T }\n\
                 struct Outer[T] { inner Inner[T] }\n\
                 fn test() i64 { value Outer[u8]\n\
                \ return 0 }\n")))
   with
  | Ok _ -> failwith "generic-struct-depth-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "struct specialization recursion depth limit exceeded")
      then failwith "generic-struct-depth-limit: unexpected diagnostic");
  let one_struct_limit = { Limits.default with max_specializations = 1 } in
  ignore
    (expect_ok
       (Sema.check ~limits:one_struct_limit
          (expect_ok
             (Parser.parse
                (source
                   "struct Box[T] { value T }\n\
                    fn test() i64 { left Box[u8]\n\
                   \ right Box[u8]\n\
                   \ return 0 }\n")))));
  (match
     Sema.check ~limits:one_struct_limit
       (expect_ok
          (Parser.parse
             (source
                "struct Box[T] { value T }\n\
                 fn id[N const usize]() usize { return N }\n\
                 fn test() usize { value Box[u8]\n\
                \ return id[1]() }\n")))
   with
  | Ok _ -> failwith "shared-specialization-limit: expected rejection"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "const specialization count limit exceeded")
      then failwith "shared-specialization-limit: unexpected diagnostic");

  let i8_min_specialization =
    llvm_of
      "fn b[N const i8](x i8) i32 { return sext[i32](x) + sext[i32](N) }\n\
       fn main() i32 { return b[-128](1) }\n"
  in
  if
    (not (contains i8_min_specialization "add i32"))
    || not (contains i8_min_specialization "sext i8 128 to i32")
  then failwith "const-param-i8-min: i8 minimum constant was not sign-extended";

  let u8_max_specialization =
    llvm_of
      "fn w[N const u8](x u8) i32 { return zext[i32](x) + zext[i32](N) }\n\
       fn main() i32 { return w[255](1) }\n"
  in
  if
    (not (contains u8_max_specialization "add i32"))
    || not (contains u8_max_specialization "zext i8 255 to i32")
  then failwith "const-param-u8-max: u8 maximum constant was not zero-extended";

  semantic_error "const-param-i8-overflow" "integer literal is out of range for i8"
    "fn b[N const i8](x i8) i32 { return sext[i32](x) + sext[i32](N) }\n\
     fn main() i32 { return b[128](1) }\n";

  semantic_error "const-param-u8-overflow" "integer literal is out of range for u8"
    "fn w[N const u8](x u8) i32 { return zext[i32](x) + zext[i32](N) }\n\
     fn main() i32 { return w[256](1) }\n";

  (match
     Lower.lower
       {
         Hir.structs =
           [
             {
               Hir.name = "S";
               fields =
                 [
                   {
                     Hir.name = "x";
                     ty = Hir.Opaque "X";
                     offset = 0;
                     unsupported_reason = None;
                   };
                 ];
               size = 8;
               align = 8;
               is_union = false;
               byte_storage = false;
             };
           ];
         consts = [];
         const_arrays = [];
         globals = [];
         funcs = [];
         strings = [];
       }
   with
  | Ok _ -> failwith "layout-invariant: malformed struct lowered without error"
  | Error diagnostics ->
      if not (contains (Diag.render_all ~source:None diagnostics) "internal error") then
        failwith "layout-invariant: unexpected diagnostic");

  lower_struct_error "layout-invariant-overlap" "overlaps a preceding field"
    {
      Hir.name = "Overlap";
      fields =
        [
          {
            Hir.name = "a";
            ty = Hir.Int Hir.U64;
            offset = 0;
            unsupported_reason = None;
          };
          { Hir.name = "b"; ty = Hir.Int Hir.U8; offset = 4; unsupported_reason = None };
        ];
      size = 8;
      align = 8;
      is_union = false;
      byte_storage = false;
    };
  lower_struct_error "layout-invariant-size" "size is smaller than its fields"
    {
      Hir.name = "Short";
      fields =
        [
          {
            Hir.name = "x";
            ty = Hir.Int Hir.U64;
            offset = 0;
            unsupported_reason = None;
          };
        ];
      size = 4;
      align = 8;
      is_union = false;
      byte_storage = false;
    };
  lower_struct_error "layout-invariant-alignment"
    "alignment must be a positive power of two"
    {
      Hir.name = "BadAlign";
      fields =
        [
          { Hir.name = "x"; ty = Hir.Int Hir.U8; offset = 0; unsupported_reason = None };
        ];
      size = 3;
      align = 3;
      is_union = false;
      byte_storage = false;
    };

  (match
     Lower.lower
       {
         Hir.structs = [];
         consts = [];
         const_arrays = [];
         globals = [];
         strings = [];
         funcs =
           [
             {
               Hir.name = "f";
               params = [ { Hir.name = "x"; ty = Hir.Opaque "X"; id = 0 } ];
               ret = Hir.Void;
               body = Hir.Statements [];
               linkage = Hir.Internal;
               variadic = false;
               span = Span.synthetic;
               display = None;
             };
           ];
       }
   with
  | Ok _ -> failwith "layout-invariant: opaque parameter lowered without error"
  | Error diagnostics ->
      if not (contains (Diag.render_all ~source:None diagnostics) "internal error") then
        failwith "layout-invariant: unexpected diagnostic");

  (match
     Lower.lower
       {
         Hir.structs = [];
         consts = [];
         const_arrays = [];
         globals = [];
         strings = [];
         funcs =
           [
             {
               Hir.name = "fallthrough";
               params = [];
               ret = Hir.Int Hir.I32;
               body = Hir.Statements [];
               linkage = Hir.Internal;
               variadic = false;
               span = Span.synthetic;
               display = None;
             };
           ];
       }
   with
  | Ok _ -> failwith "lower-fallthrough: malformed non-void HIR lowered"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "non-void function `fallthrough` reached lowering with fallthrough")
      then failwith "lower-fallthrough: unexpected diagnostic");

  let infinite_loop_ir = llvm_of "fn spin() i64 { while true { } }\n" in
  if not (contains infinite_loop_ir "unreachable") then
    failwith "lower-infinite-loop: proven-dead exit was not terminated";

  (match
     Lower.lower
       {
         Hir.structs = [];
         consts = [];
         const_arrays = [];
         globals = [];
         strings = [];
         funcs =
           [
             {
               Hir.name = "malformed_switch";
               params = [];
               ret = Hir.Void;
               body =
                 Hir.Statements
                   [
                     Hir.Switch
                       ( Hir.EInt (0L, Hir.Int Hir.I32, Span.synthetic),
                         [ ([ Hir.EString (0, false, Span.synthetic) ], []) ],
                         None,
                         Span.synthetic );
                   ];
               linkage = Hir.Internal;
               variadic = false;
               span = Span.synthetic;
               display = None;
             };
           ];
       }
   with
  | Ok _ -> failwith "lower-switch-case: malformed HIR lowered"
  | Error diagnostics ->
      if
        not
          (contains
             (Diag.render_all ~source:None diagnostics)
             "internal error: non-constant switch case")
      then failwith "lower-switch-case: unexpected diagnostic");

  let malformed_compound_local = { Hir.name = "x"; ty = Hir.Int Hir.I8; id = 0 } in
  lower_function_error "lower-compound-operator"
    "internal error: non-arithmetic operator reached binary lowering"
    [ malformed_compound_local ]
    [
      Hir.Compound_assign
        ( Hir.ALocal malformed_compound_local,
          Ast.And,
          Hir.EInt (1L, Hir.Int Hir.I8, Span.synthetic),
          Hir.Int Hir.I8,
          Span.synthetic );
    ];
  lower_function_error "lower-index-type"
    "internal error: index lowering received a non-integer value" []
    [
      Hir.Expr
        ( Hir.Index
            ( Hir.EVector ([ 7L ], Hir.Vec (1, Hir.Int Hir.I8), Span.synthetic),
              Hir.EBool (true, Span.synthetic),
              Hir.Int Hir.I8,
              Span.synthetic ),
          Span.synthetic );
    ];
  lower_function_error "lower-intrinsic-type"
    "internal error: integer intrinsic has a non-integer type" []
    [
      Hir.Expr
        ( Hir.Call
            ( Hir.Builtin Hir.Popcount,
              [ Hir.EBool (true, Span.synthetic) ],
              Hir.Bool,
              Span.synthetic ),
          Span.synthetic );
    ];
  lower_function_error "lower-complement-type"
    "internal error: bitwise complement has a non-integer type" []
    [
      Hir.Expr
        ( Hir.Unary
            (Ast.Bit_not, Hir.Null (Hir.Addr, Span.synthetic), Hir.Addr, Span.synthetic),
          Span.synthetic );
    ];
  lower_function_error "lower-logical-not-type"
    "internal error: logical not has a non-bool type" []
    [
      Hir.Expr
        ( Hir.Unary
            ( Ast.Not,
              Hir.EInt (1L, Hir.Int Hir.I64, Span.synthetic),
              Hir.Int Hir.I64,
              Span.synthetic ),
          Span.synthetic );
    ];
  lower_function_error "lower-condition-type"
    "internal error: condition lowering received a non-bool value" []
    [
      Hir.If (Hir.EInt (1L, Hir.Int Hir.I64, Span.synthetic), [], None, Span.synthetic);
    ];

  (match
     Lower.lower
       {
         Hir.structs = [];
         consts = [];
         const_arrays =
           [
             { Hir.name = "A"; ty = Hir.Array (2, Hir.Opaque "X"); elems = [ 0L; 0L ] };
           ];
         globals = [];
         strings = [];
         funcs = [];
       }
   with
  | Ok _ -> failwith "layout-invariant: malformed const array lowered without error"
  | Error diagnostics ->
      if not (contains (Diag.render_all ~source:None diagnostics) "internal error") then
        failwith "layout-invariant: unexpected diagnostic");

  lower_function_error "layout-invariant-local" "internal error: type `X` has no layout"
    []
    [ Hir.Let ({ Hir.name = "x"; ty = Hir.Opaque "X"; id = 0 }, None, Span.synthetic) ];
  (match
     Sema.check
       (expect_ok
          (Parser.parse
             (source
                "const MIN i64 = -9223372036854775808\n\
                 const X bool = false && (MIN / -1 == 0)\n")))
   with
  | Ok _ -> ()
  | Error diagnostics ->
      failwith
        ("const-logical-short-circuit: unexpected diagnostic: "
        ^ Diag.render_all ~source:None diagnostics));

  let runtime_short_circuit =
    llvm_of
      "const MIN i64 = -9223372036854775808\n\
       fn f() bool { return false && (MIN / -1 == 0) }\n"
  in
  if not (contains runtime_short_circuit "br") then
    failwith "runtime-logical-short-circuit: branch lowering missing";

  ignore (llvm_of "const Z bool = false && (1 / 0 == 0)\n");

  ignore (llvm_of "fn f() bool { return false && (1 / 0 == 0) }\n");

  semantic_error "const-logical-reaches-right" "division by zero in constant expression"
    "const Z bool = true && (1 / 0 == 0)\n";

  semantic_error "runtime-logical-reaches-right"
    "division by zero is not a defined runtime operation"
    "fn f() bool { return true && (1 / 0 == 0) }\n";

  semantic_error "const-logical-bool-only" "left operand of `&&` is `i32`, not `bool`"
    "const B bool = 1 && true\n";
  semantic_error "runtime-logical-bool-only" "left operand of `&&` is `i32`, not `bool`"
    "fn f() bool { return 1 && true }\n";
  semantic_error "logical-not-integer"
    "operator `!` needs `bool` or a bool vector, got `i64`"
    "fn f(value i64) bool { return !value }\n";
  semantic_error "logical-not-integer-vector"
    "operator `!` needs `bool` or a bool vector, got `vec[4, i64]`"
    "fn f(value vec[4,i64]) vec[4,bool] { return !value }\n";
  semantic_error "if-condition-bool-only" "condition of `if` is `i64`, not `bool`"
    "fn f(value i64) i64 { if value { return 1 } return 0 }\n";
  semantic_error "while-condition-bool-only"
    "condition of `while` is `addr`, not `bool`"
    "fn f(value addr) void { while value { break } }\n";
  semantic_error "for-condition-bool-only" "condition of `for` is `i32`, not `bool`"
    "fn f() void { for ; 1; { break } }\n";
  let removed_question = "fn f(c bool) i32 { return c ? 1 : 0 }" in
  syntax_pin "removed-question-token" removed_question 1
    (String.index removed_question '?' + 1)
    1 "Fas has no `?:`; write `if c { a } else { b }`";
  let missing_if_else = "fn f() i32 { return if true { 1 } }" in
  syntax_pin "if-expression-requires-else" missing_if_else 1
    (String.index missing_if_else '}' + 1)
    1 "if-expression requires an `else` branch";
  parse_error_message "if-expression-rejects-statements"
    "expected an expression, found `return`"
    "fn f() i32 { return if true { return 1 } else { 0 } }\n";
  parse_error_message "if-expression-rejects-multiple-expressions"
    "expected `}`, found `2`" "fn f() i32 { return if true { 1 2 } else { 0 } }\n";
  let unparenthesized_if = "fn f(c bool) i32 { return if c { 1 } else { 2 } + 3 }\n" in
  (match
     parse_diagnostics "if-expression-needs-parentheses-before-operator"
       unparenthesized_if
   with
  | [ diagnostic ]
    when diagnostic.Diag.message
         = "an if-expression cannot be followed by a binary operator"
         && diagnostic.help = Some "parenthesize the if-expression before this operator"
    ->
      ()
  | diagnostics ->
      failwith
        ("if-expression-needs-parentheses-before-operator: unexpected diagnostic: "
        ^ Diag.render_all ~source:(Some (source unparenthesized_if)) diagnostics));
  semantic_error "if-expression-condition-bool-only"
    "condition of `if` is `i64`, not `bool`"
    "fn f(value i64) i64 { return if value { 1 } else { 0 } }\n";
  semantic_error "constant-ternary-condition-bool-only"
    "condition of `if` is `i32`, not `bool`" "const X i64 = if 1 { 2 } else { 3 }\n";
  let logical_source = "fn f(c i32) bool { return c && true }\n" in
  let logical_expected =
    expected_diagnostic "fn f(c i32) bool { return c && true }" 27 1
      "left operand of `&&` is `i32`, not `bool`"
      "Fas has no implicit truth values; write `c != 0`"
  in
  if semantic_render logical_source <> logical_expected then
    failwith ("logical-operand-diagnostic: " ^ semantic_render logical_source);
  let raw_source = "fn f(p addr, i usize) u8 { return p[i] }\n" in
  let raw_expected =
    expected_diagnostic "fn f(p addr, i usize) u8 { return p[i] }" 35 4
      "raw access on `addr` needs an element type" "write `p[T, i]`, e.g. `p[u8, i]`"
  in
  if semantic_render raw_source <> raw_expected then
    failwith ("raw-access-diagnostic: " ^ semantic_render raw_source);
  let raw_unknown_source =
    "opaque memblock_t; fn f(p addr) i32 { x i32 = p[memblock_s, 0]; return 0 }\n"
  in
  let raw_unknown_line = String.trim raw_unknown_source in
  let raw_unknown_column =
    String.length "opaque memblock_t; fn f(p addr) i32 { x i32 = p[" + 1
  in
  let raw_unknown_expected =
    expected_diagnostic raw_unknown_line raw_unknown_column (String.length "memblock_s")
      "unknown type `memblock_s`" "did you mean `memblock_t`?"
  in
  if semantic_render raw_unknown_source <> raw_unknown_expected then
    failwith ("raw-unknown-type-diagnostic: " ^ semantic_render raw_unknown_source);
  let raw_no_match_source =
    "fn f(p addr) i32 { x i32 = p[nonexistent, 0]; return 0 }\n"
  in
  let raw_no_match_line = String.trim raw_no_match_source in
  let raw_no_match_column = String.length "fn f(p addr) i32 { x i32 = p[" + 1 in
  let raw_no_match_expected =
    expected_diagnostic_without_help raw_no_match_line raw_no_match_column
      (String.length "nonexistent") "unknown type `nonexistent`"
  in
  if semantic_render raw_no_match_source <> raw_no_match_expected then
    failwith ("raw-unknown-type-without-help: " ^ semantic_render raw_no_match_source);
  let raw_ambiguous_source =
    "opaque memblock_t; opaque memblock_u; fn f(p addr) i32 { x i32 = p[memblock_s, \
     0]; return 0 }\n"
  in
  let raw_ambiguous_line = String.trim raw_ambiguous_source in
  let raw_ambiguous_column =
    String.length "opaque memblock_t; opaque memblock_u; fn f(p addr) i32 { x i32 = p["
    + 1
  in
  let raw_ambiguous_expected =
    expected_diagnostic_without_help raw_ambiguous_line raw_ambiguous_column
      (String.length "memblock_s") "unknown type `memblock_s`"
  in
  if semantic_render raw_ambiguous_source <> raw_ambiguous_expected then
    failwith
      ("raw-unknown-type-with-ambiguous-help: " ^ semantic_render raw_ambiguous_source);
  let raw_value_source =
    "fn f(p addr, requested_index usize) u8 { return p[requested_index] }\n"
  in
  let raw_value_line = String.trim raw_value_source in
  let raw_value_fragment = "p[requested_index]" in
  let raw_value_column =
    String.length "fn f(p addr, requested_index usize) u8 { return " + 1
  in
  let raw_value_expected =
    expected_diagnostic raw_value_line raw_value_column
      (String.length raw_value_fragment)
      "raw access on `addr` needs an element type"
      "write `p[T, requested_index]`, e.g. `p[u8, requested_index]`"
  in
  if semantic_render raw_value_source <> raw_value_expected then
    failwith ("raw-value-index-help: " ^ semantic_render raw_value_source);
  let raw_expression_source =
    "fn f(p addr, requested_index usize) u8 { return p[requested_index + 1] }\n"
  in
  let raw_expression_line = String.trim raw_expression_source in
  let raw_expression_fragment = "p[requested_index + 1]" in
  let raw_expression_column =
    String.length "fn f(p addr, requested_index usize) u8 { return " + 1
  in
  let raw_expression_expected =
    expected_diagnostic raw_expression_line raw_expression_column
      (String.length raw_expression_fragment)
      "raw access on `addr` needs an element type"
      "write `p[T, requested_index + 1]`, e.g. `p[u8, requested_index + 1]`"
  in
  if semantic_render raw_expression_source <> raw_expression_expected then
    failwith ("raw-expression-index-help: " ^ semantic_render raw_expression_source);
  let ordinary_unknown_type_source =
    "opaque memblock_t; fn f(x handle[memblock_s]) void { return }\n"
  in
  let ordinary_unknown_diagnostic =
    match semantic_diagnostics ordinary_unknown_type_source with
    | [ diagnostic ] -> diagnostic
    | diagnostics ->
        failwith
          ("ordinary-unknown-type-diagnostic: "
          ^ Diag.render_all ~source:None diagnostics)
  in
  if
    ordinary_unknown_diagnostic.Diag.message <> "unknown type `memblock_s`"
    || ordinary_unknown_diagnostic.Diag.help <> Some "did you mean `memblock_t`?"
  then failwith "ordinary-unknown-type-diagnostic: missing exact similar-name help";
  ignore (llvm_of "opaque memblock_t; fn f(x handle[memblock_t]) void { return }\n");
  ignore
    (llvm_of
       "struct memblock_t { prev u8 }; fn f(base addr) u8 { return base[memblock_t, \
        0].prev }\n");
  ignore
    (llvm_of
       "fn f(p addr, requested_index usize) u8 { return p[u8, requested_index] }\n");
  ignore
    (llvm_of
       "fn f(p addr, requested_index usize) u8 { return p[u8, requested_index + 1] }\n");
  let argument_mismatch_source =
    "fn put(value u8) void { return }\nfn f(x u16) void { put(x); return }\n"
  in
  let argument_mismatch_line = "fn f(x u16) void { put(x); return }" in
  let argument_mismatch_column = String.length "fn f(x u16) void { put(" + 1 in
  let argument_mismatch_expected =
    expected_diagnostic ~line_number:2 argument_mismatch_line argument_mismatch_column 1
      "cannot pass `u16` as `u8`: values above 255 would be lost"
      "keep the low bits with `trunc[u8](x)`"
  in
  if semantic_render argument_mismatch_source <> argument_mismatch_expected then
    failwith ("argument-type-mismatch: " ^ semantic_render argument_mismatch_source);
  let initializer_mismatch_source = "fn f(x u16) u8 { value u8 = x; return 0 }\n" in
  let initializer_mismatch_line = String.trim initializer_mismatch_source in
  let initializer_mismatch_column = String.length "fn f(x u16) u8 { value u8 = " + 1 in
  let initializer_mismatch_expected =
    expected_diagnostic initializer_mismatch_line initializer_mismatch_column 1
      "cannot assign `u16` to `u8`: values above 255 would be lost"
      "keep the low bits with `trunc[u8](x)`"
  in
  if semantic_render initializer_mismatch_source <> initializer_mismatch_expected then
    failwith
      ("initializer-type-mismatch: " ^ semantic_render initializer_mismatch_source);
  semantic_accept "signed-widening-return" "fn f(x i32) i64 { return x }\n";
  semantic_accept "unsigned-widening-argument"
    "fn put(value u64) void { return }\nfn f(x u32) void { put(x); return }\n";
  semantic_accept "unsigned-to-signed-widening-return" "fn f(x u32) i64 { return x }\n";
  semantic_accept "mixed-signed-binary-widening"
    "fn f(a i32, b i64) i64 { return a + b }\n";
  semantic_accept "mixed-unsigned-binary-widening"
    "fn f(a u64, b u32) u64 { return a + b }\n";
  semantic_accept "lossless-integer-width-pairs"
    "fn f(a u8, b i8, c u32, d i32) isize {\n\
     wide_u u16 = a\n\
    \ wide_i i16 = b\n\
    \ wide_signed i16 = a\n\
     native_u usize = c\n\
    \ native_signed isize = c\n\
     native_i isize = d\n\
    \ return c\n\
     }\n";
  semantic_accept "widening-if-and-switch-values"
    "const SMALL u8 = 1\n\
     fn choose(a u8, b u16) u32 { value u32 = if true { a } else { b }; return value }\n\
     fn dispatch(value u64) u8 { switch value { case SMALL: return 1; default: return \
     0 } }\n";
  let unified_if_source =
    "fn size(c bool, l u16, o u32, off u32) usize { return zext[usize](if c { l } else \
     { o - off }) }\n"
  in
  semantic_accept "if-branch-lossless-widening" unified_if_source;
  let explicit_if_source =
    "fn size(c bool, l u16, o u32, off u32) usize { return zext[usize](if c { \
     zext[u32](l) } else { o - off }) }\n"
  in
  if llvm_of unified_if_source <> llvm_of explicit_if_source then
    failwith "if-branch-widening-llvm: implicit and explicit forms differ";
  semantic_accept "if-branch-cast-operands"
    "fn casts(c bool, u u16, U u32, x i16, X i32) void {\n\
     zero u64 = zext[u64](if c { u } else { U })\n\
     sign i64 = sext[i64](if c { x } else { X })\n\
     low u8 = trunc[u8](if c { u } else { U })\n\
     bits i32 = bitcast[i32](if c { u } else { U })\n\
     return\n\
     }\n";
  semantic_accept "if-branch-comparison-operand"
    "fn less(c bool, a u16, w u32) bool { return (if c { a } else { w }) < w }\n";
  let binary_if_operand =
    "fn bad(c bool, a u8, b u8) u32 { return (if c { a } else { b }) * a }\n"
  and binary_plain_operand = "fn bad(a u8, b u8) u32 { return b * a }\n" in
  semantic_same_error "if-binary-left-operand" binary_if_operand binary_plain_operand;
  semantic_same_error "if-binary-right-operand"
    "fn bad(c bool, a u8, b u8) u32 { return a * (if c { a } else { b }) }\n"
    binary_plain_operand;
  semantic_accept "if-binary-left-explicit-twin"
    "fn good(c bool, a u8, b u8) u32 { return zext[u32](if c { a } else { b }) * a }\n";
  semantic_accept "if-binary-right-explicit-twin"
    "fn good(c bool, a u8, b u8) u32 { return a * zext[u32](if c { a } else { b }) }\n";
  semantic_same_error "if-binary-call-argument"
    ("fn take(value u32) u32 { return value }\n"
   ^ "fn bad(c bool, a u8) u32 { return take((if c { a } else { a }) * a) }\n")
    ("fn take(value u32) u32 { return value }\n"
   ^ "fn bad(a u8) u32 { return take(a * a) }\n");
  semantic_same_error "if-binary-array-element"
    "fn bad(c bool, a u8) u32 { values arr[2,u32] = {(if c { a } else { a }) * a, 0}; \
     return values[0] }\n"
    "fn bad(a u8) u32 { values arr[2,u32] = {a * a, 0}; return values[0] }\n";
  semantic_accept "if-binary-call-explicit-twin"
    ("fn take(value u32) u32 { return value }\n"
   ^ "fn good(c bool, a u8) u32 { return take(zext[u32](if c { a } else { a }) * a) }\n"
    );
  semantic_accept "if-binary-array-explicit-twin"
    "fn good(c bool, a u8) u32 { values arr[2,u32] = {zext[u32](if c { a } else { a }) \
     * a, 0}; return values[0] }\n";
  semantic_same_error "if-unary-minus-operand"
    "fn bad(c bool, a u8) u32 { return -(if c { a } else { a }) }\n"
    "fn bad(a u8) u32 { return -a }\n";
  semantic_same_error "if-unary-bit-not-operand"
    "fn bad(c bool, a u8) u32 { return ~(if c { a } else { a }) }\n"
    "fn bad(a u8) u32 { return ~a }\n";
  semantic_accept "if-unary-minus-explicit-twin"
    "fn good(c bool, a u8) u32 { return -zext[u32](if c { a } else { a }) }\n";
  semantic_accept "if-unary-bit-not-explicit-twin"
    "fn good(c bool, a u8) u32 { return ~zext[u32](if c { a } else { a }) }\n";
  semantic_same_error "if-shift-left-operand"
    "fn bad(c bool, a u8, n u32) u32 { return (if c { a } else { a }) << n }\n"
    "fn bad(a u8, n u32) u32 { return a << n }\n";
  semantic_accept "if-shift-explicit-twin"
    "fn good(c bool, a u8, n u32) u32 { return zext[u32](if c { a } else { a }) << n }\n";
  semantic_same_error "if-mixed-literal-operand"
    "fn bad(c bool, a i8, b i8) i64 { return (if c { a } else { b }) - 100 }\n"
    "fn bad(a i8, b i8) i64 { return a - 100 }\n";
  semantic_accept "if-mixed-literal-explicit-twin"
    "fn good(c bool, a i8, b i8) i64 { return sext[i64](if c { a } else { b }) - 100 }\n";
  semantic_accept "if-addr-signed-offset"
    "fn selected(p addr, c bool, x i8) addr { return p + (if c { x } else { x }) }\n";
  semantic_accept "if-addr-signed-offset-plain-twin"
    "fn selected(p addr, x i8) addr { return p + x }\n";
  semantic_accept "if-addr-scaled-offset"
    "fn selected(p addr, c bool, x u8) addr { return p + (if c { x } else { x }) * 200 }\n";
  semantic_accept "if-addr-scaled-offset-plain-twin"
    "fn selected(p addr, x u8) addr { return p + x * 200 }\n";
  semantic_accept "if-addr-compound-no-destination"
    "fn selected(p addr, c bool, x i8) addr { q addr = p; q += if c { x } else { x }; \
     return q }\n";
  semantic_message "if-compound-binary-narrow-arithmetic"
    "`(if c { a } else { a }) * a` is computed in `u8` and may wrap before it reaches \
     `u32`"
    "fn bad(c bool, a u8) u32 { value u32 = 0; value += (if c { a } else { a }) * a; \
     return value }\n";
  semantic_accept "if-compound-binary-explicit-twin"
    "fn good(c bool, a u8) u32 { value u32 = 0; value += zext[u32](if c { a } else { a \
     }) * a; return value }\n";
  semantic_message "if-constant-binary-narrow-arithmetic"
    "`(if true { A } else { A }) + 0` is computed in `u8` and may wrap before it \
     reaches `u32`"
    "const A u8 = 200\nconst K u32 = (if true { A } else { A }) + 0\n";
  semantic_accept "if-constant-binary-explicit-widening"
    "const A u8 = 200\nconst K u32 = zext[u32](if true { A } else { A }) + 0\n";
  semantic_accept "if-branch-nested-unification"
    "fn nested(c bool, d bool, a u8, b u16, w u32) u32 { return if c { if d { a } else \
     { b } } else { w } }\n";
  semantic_accept "if-branch-order-three-arm-forward"
    "fn f(c bool, k bool, a u8, b u16, w u32) u64 { return zext[u64](if c { a } else \
     if k { b } else { w }) }\n";
  semantic_accept "if-branch-order-three-arm-reverse"
    "fn f(c bool, k bool, a u8, b u16, w u32) u64 { return zext[u64](if !c { if !k { w \
     } else { b } } else { a }) }\n";
  semantic_accept "if-branch-order-nested-else-forward"
    "fn f(c bool, k bool, a u8, b i16, w i32) i64 { return sext[i64](if c { a } else { \
     if k { w } else { b } }) }\n";
  semantic_accept "if-branch-order-nested-else-reverse"
    "fn f(c bool, k bool, a u8, b i16, w i32) i64 { return sext[i64](if !c { if !k { b \
     } else { w } } else { a }) }\n";
  semantic_accept "if-branch-order-literal-last"
    "fn f(c bool, w u32) u64 { return zext[u64](if c { w } else { 1 }) }\n";
  semantic_accept "if-branch-order-literal-first"
    "fn f(c bool, w u32) u64 { return zext[u64](if !c { 1 } else { w }) }\n";
  semantic_accept "if-branch-order-nested-then-forward"
    "fn f(c bool, k bool, a u8, b u16, w u32) u32 { return if c { if k { a } else { b \
     } } else { w } }\n";
  semantic_accept "if-branch-order-nested-then-reverse"
    "fn f(c bool, k bool, a u8, b u16, w u32) u32 { return if !c { w } else { if !k { \
     b } else { a } } }\n";
  semantic_accept "if-branch-order-signed-chain-forward"
    "fn f(c bool, k bool, a i8, b i16, w i32) i64 { return sext[i64](if c { a } else \
     if k { b } else { w }) }\n";
  semantic_accept "if-branch-order-signed-chain-reverse"
    "fn f(c bool, k bool, a i8, b i16, w i32) i64 { return sext[i64](if !c { if !k { w \
     } else { b } } else { a }) }\n";
  semantic_accept "if-branch-order-four-arm-unsigned"
    "fn f(c bool, k bool, m bool, a u8, b u16, w u32, x u16) u64 { return zext[u64](if \
     c { a } else if k { b } else if m { w } else { x }) }\n";
  semantic_accept "if-branch-order-four-arm-unsigned-reverse"
    "fn f(c bool, k bool, m bool, a u8, b u16, w u32, x u16) u64 { return zext[u64](if \
     !c { if !k { if !m { x } else { w } } else { b } } else { a }) }\n";
  semantic_accept "if-branch-order-four-arm-signed"
    "fn f(c bool, k bool, m bool, a i8, b i16, w i32, x i16) i64 { return sext[i64](if \
     c { a } else if k { b } else if m { w } else { x }) }\n";
  semantic_accept "if-branch-order-four-arm-signed-reverse"
    "fn f(c bool, k bool, m bool, a i8, b i16, w i32, x i16) i64 { return sext[i64](if \
     !c { if !k { if !m { x } else { w } } else { b } } else { a }) }\n";
  semantic_accept "if-branch-order-literal-first-chain"
    "fn f(c bool, k bool, b u8, w u32) u64 { return zext[u64](if c { 1 } else if k { b \
     } else { w }) }\n";
  semantic_accept "if-branch-order-literal-first-chain-reverse"
    "fn f(c bool, k bool, b u8, w u32) u64 { return zext[u64](if !c { if !k { w } else \
     { b } } else { 1 }) }\n";
  semantic_accept "if-branch-order-literal-middle-chain"
    "fn f(c bool, k bool, a u8, w u32) u64 { return zext[u64](if c { a } else if k { 1 \
     } else { w }) }\n";
  semantic_accept "if-branch-order-literal-middle-chain-reverse"
    "fn f(c bool, k bool, a u8, w u32) u64 { return zext[u64](if !c { if !k { w } else \
     { 1 } } else { a }) }\n";
  semantic_accept "if-branch-order-literal-last-chain"
    "fn f(c bool, k bool, a u8, b u16) u64 { return zext[u64](if c { a } else if k { b \
     } else { 1 }) }\n";
  semantic_accept "if-branch-order-literal-last-chain-reverse"
    "fn f(c bool, k bool, a u8, b u16) u64 { return zext[u64](if !c { if !k { 1 } else \
     { b } } else { a }) }\n";
  semantic_error "if-branch-order-literal-out-of-range-first"
    "integer literal is out of range for u8"
    "fn f(c bool, x u8) u16 { return zext[u16](if c { 256 } else { x }) }\n";
  semantic_error "if-branch-order-literal-out-of-range-last"
    "integer literal is out of range for u8"
    "fn f(c bool, x u8) u16 { return zext[u16](if !c { x } else { 256 }) }\n";
  semantic_error "if-branch-order-no-common-chain-forward" "no type holds both"
    "fn f(c bool, k bool, u u32, s i32, w u64) u64 { return zext[u64](if c { w } else \
     if k { u } else { s }) }\n";
  semantic_error "if-branch-order-no-common-chain-reverse" "no type holds both"
    "fn f(c bool, k bool, u u32, s i32, w u64) u64 { return zext[u64](if !c { if !k { \
     s } else { u } } else { w }) }\n";
  semantic_error "if-branch-order-narrow-first" "narrow arithmetic"
    "fn f(c bool, k bool, a u16, b u16, w u32, x u16) u64 { return zext[u64](if c { a \
     + b } else if k { w } else { x }) }\n";
  semantic_error "if-branch-order-narrow-first-reverse" "narrow arithmetic"
    "fn f(c bool, k bool, a u16, b u16, w u32, x u16) u64 { return zext[u64](if !c { \
     if !k { x } else { w } } else { a + b }) }\n";
  semantic_error "if-branch-order-narrow-middle" "narrow arithmetic"
    "fn f(c bool, k bool, a u16, b u16, w u32, x u8) u64 { return zext[u64](if c { x } \
     else if k { a + b } else { w }) }\n";
  semantic_error "if-branch-order-narrow-middle-reverse" "narrow arithmetic"
    "fn f(c bool, k bool, a u16, b u16, w u32, x u8) u64 { return zext[u64](if !c { if \
     !k { w } else { a + b } } else { x }) }\n";
  semantic_error "if-branch-order-narrow-last" "narrow arithmetic"
    "fn f(c bool, k bool, a u16, b u16, w u32, x u8) u64 { return zext[u64](if c { x } \
     else if k { w } else { a + b }) }\n";
  semantic_error "if-branch-order-narrow-last-reverse" "narrow arithmetic"
    "fn f(c bool, k bool, a u16, b u16, w u32, x u8) u64 { return zext[u64](if !c { if \
     !k { a + b } else { w } } else { x }) }\n";
  semantic_error "if-branch-no-common-type" "no type holds both"
    "fn bad(c bool, u u32, s i32) i64 { return sext[i64](if c { u } else { s }) }\n";
  semantic_accept "if-branch-no-common-type-explicit-twin"
    "fn good(c bool, u u32, s i32) i64 { return if c { zext[i64](u) } else { \
     sext[i64](s) } }\n";
  semantic_error "if-branch-narrow-arithmetic" "narrow arithmetic"
    "fn bad(c bool, a u16, b u16, w u32) u64 { return zext[u64](if c { a + b } else { \
     w }) }\n";
  semantic_accept "if-branch-narrow-arithmetic-explicit-twin"
    "fn good(c bool, a u16, b u16, w u32) u64 { return zext[u64](if c { zext[u32](a) + \
     b } else { w }) }\n";
  semantic_error "if-branch-bool-integer" "different types"
    "fn bad(c bool, w u32) u64 { return zext[u64](if c { true } else { w }) }\n";
  semantic_accept "if-branch-bool-integer-explicit-twin"
    "fn good(c bool, w u32) u64 { return zext[u64](if c { zext[u32](c) } else { w }) }\n";
  semantic_accept "if-branch-constant-unification"
    "const F bool = true\n\
     const A u8 = 0x80\n\
     const B u16 = 0x100\n\
     const K u64 = zext[u64](if F { A } else { B })\n\
     fn value() u64 { return zext[u64](if F { A } else { B }) }\n";
  semantic_accept "if-branch-constant-order-forward"
    "const F bool = true\n\
     const A u8 = 0x80\n\
     const B u16 = 0x100\n\
     const W u32 = 0x10000\n\
     const K u64 = zext[u64](if F { 1 } else if !F { B } else { W })\n";
  semantic_accept "if-branch-constant-order-reverse"
    "const F bool = true\n\
     const A u8 = 0x80\n\
     const B u16 = 0x100\n\
     const W u32 = 0x10000\n\
     const K u64 = zext[u64](if !F { if F { W } else { B } } else { 1 })\n";
  semantic_error "if-branch-constant-literal-out-of-range-first"
    "integer literal is out of range for u8"
    "const F bool = true\n\
     const X u8 = 1\n\
     const K u64 = zext[u64](if F { 256 } else { X })\n";
  semantic_error "if-branch-constant-literal-out-of-range-last"
    "integer literal is out of range for u8"
    "const F bool = true\n\
     const X u8 = 1\n\
     const K u64 = zext[u64](if !F { X } else { 256 })\n";
  let if_branch_types_source =
    "fn bad(c bool, u u32, s i32) i64 { return sext[i64](if c { u } else { s }) }\n"
  in
  semantic_pin "if-branch-no-common-type-diagnostic" if_branch_types_source 1
    (String.length
       "fn bad(c bool, u u32, s i32) i64 { return sext[i64](if c { u } else { "
    + 1)
    1 "if-expression branches are `u32` and `i32`: no type holds both" None;
  let if_branch_narrow_source =
    "fn bad(c bool, a u16, b u16, w u32) u64 { return zext[u64](if c { a + b } else { \
     w }) }\n"
  in
  let if_branch_narrow_help = "widen an operand first: `zext[u32](a) + b`" in
  semantic_pin "if-branch-narrow-arithmetic-diagnostic" if_branch_narrow_source 1
    (String.length "fn bad(c bool, a u16, b u16, w u32) u64 { return zext[u64](if c { "
    + 1)
    (String.length "a + b")
    "if-expression branches are `u16` and `u32`: narrow arithmetic may wrap before \
     widening"
    (Some if_branch_narrow_help);
  conversion_help_twin "if-branch-narrow-arithmetic-help-compiles"
    if_branch_narrow_source "a + b" "zext[u32](a) + b" if_branch_narrow_help;
  let if_branch_bool_source =
    "fn bad(c bool, w u32) u64 { return zext[u64](if c { true } else { w }) }\n"
  in
  semantic_pin "if-branch-bool-integer-diagnostic" if_branch_bool_source 1
    (String.rindex if_branch_bool_source 'w' + 1)
    1 "if-expression branches have different types: `bool` and `u32`" None;
  semantic_accept "widening-size-slot"
    "const BUFFERSIZE u16 = 0x1000\n\
     fn buffer() void { data arr[BUFFERSIZE / sizeof[i32], u8] = {}\n\
    \ return }\n";
  semantic_constant "SLOT_LITERAL_FORM" 10L
    "const SLOT_LITERAL_FORM usize = 100000 * 100000 / 1000000000\n";
  semantic_constant "SLOT_ARRAY_FORM" 10L
    "const SLOT_ARRAY_FORM usize = sizeof[arr[100000 * 100000 / 1000000000, u8]]\n";
  semantic_constant "SLOT_GENERIC_FORM" 10L
    "struct SizeBox[N const usize] { items arr[N, u8] }\n\
     const SLOT_GENERIC_FORM usize = sizeof[SizeBox[100000 * 100000 / 1000000000]]\n";
  semantic_constant "SLOT_LARGE_LITERAL" 3000000000L
    "const SLOT_LARGE_LITERAL usize = sizeof[arr[3000000000 + 0, u8]]\n";
  semantic_constant "SLOT_SHIFTED_LITERAL" 8589934592L
    "const SLOT_SHIFTED_LITERAL usize = sizeof[arr[1 << 33, u8]]\n";
  semantic_constant "SLOT_IF_FORM" 7L
    "const S u8 = 7\n\
     const SLOT_IF_FORM usize = sizeof[arr[if S > 3 { S } else { 300 }, u8]]\n";
  semantic_message "array-size-slot-narrow-arithmetic"
    "`K + 1` is computed in `u8` and may wrap before it reaches `usize`"
    "const K u8 = 255\nfn bad() void { values arr[K + 1, u8] = {}; return }\n";
  semantic_message "vector-size-slot-narrow-arithmetic"
    "`K + 1` is computed in `u8` and may wrap before it reaches `usize`"
    "const K u8 = 255\nfn bad() void { values vec[K + 1, u8] = splat(0); return }\n";
  semantic_accept "array-size-slot-explicit-widening-twin"
    "const K u8 = 255\n\
     fn good() void { values arr[zext[usize](K) + 1, u8] = {}; return }\n";
  semantic_accept "vector-size-slot-explicit-widening-twin"
    "const K u8 = 255\n\
     fn good() void { values vec[zext[usize](K) + 1, u8] = splat(0); return }\n";
  semantic_error "narrow-arithmetic-does-not-widen" "computed in `u16` and may wrap"
    "fn f(a u16, b u16) u32 { value u32 = a * b; return value }\n";
  semantic_accept "narrow-arithmetic-explicit-widening"
    "fn f(a u16, b u16) u32 { value u32 = zext[u32](a) * b; return value }\n";
  semantic_error "mixed-widths-without-common-type" "no type holds both"
    "fn f(a u32, b i32) bool { return a < b }\n";
  let widening_narrowing_source = "fn f(x u64) u32 { value u32 = x; return value }\n" in
  semantic_pin "widening-narrowing-diagnostic" widening_narrowing_source 1
    (String.length "fn f(x u64) u32 { value u32 = " + 1)
    1 "cannot assign `u64` to `u32`: values above 4294967295 would be lost"
    (Some "keep the low bits with `trunc[u32](x)`");
  semantic_accept "widening-narrowing-explicit-twin"
    "fn f(x u64) u32 { value u32 = trunc[u32](x); return value }\n";
  semantic_pin "widening-signed-narrowing-diagnostic"
    "fn f(x i64) i32 { value i32 = x; return value }\n" 1
    (String.length "fn f(x i64) i32 { value i32 = " + 1)
    1
    "cannot assign `i64` to `i32`: values outside -2147483648..2147483647 would be lost"
    (Some "keep the low bits with `trunc[i32](x)`");
  conversion_help_twin "signed-narrowing-help-compiles"
    "fn f(x i64) i32 { value i32 = x; return value }\n" "x" "trunc[i32](x)"
    "keep the low bits with `trunc[i32](x)`";
  semantic_pin "widening-unsigned-to-signed-narrowing-diagnostic"
    "fn f(x u16) i8 { return x }\n" 1
    (String.length "fn f(x u16) i8 { return " + 1)
    1 "cannot return `u16` as `i8`: values above 127 would be lost"
    (Some "keep the low bits with `trunc[i8](x)`");
  semantic_pin "widening-unsigned-to-signed-same-width-diagnostic"
    "fn f(x u32) i32 { return x }\n" 1
    (String.length "fn f(x u32) i32 { return " + 1)
    1 "cannot return `u32` as `i32`: values above 2147483647 change meaning"
    (Some "write `bitcast[i32](x)`");
  conversion_help_twin "unsigned-to-signed-help-compiles"
    "fn f(x u32) i32 { return x }\n" "x" "bitcast[i32](x)" "write `bitcast[i32](x)`";
  let widening_sign_source =
    "fn put(x u32) void { return }\nfn f(n i32) void { put(n); return }\n"
  in
  semantic_pin "widening-sign-change-diagnostic" widening_sign_source 2
    (String.length "fn f(n i32) void { put(" + 1)
    1 "cannot pass `i32` as `u32`: negative values change meaning"
    (Some
       "reinterpret the bits with `bitcast[u32](n)`, or widen with `sext`/`zext` first \
        if that is what you mean");
  conversion_help_twin "sign-change-help-compiles" widening_sign_source "n"
    "bitcast[u32](n)"
    "reinterpret the bits with `bitcast[u32](n)`, or widen with `sext`/`zext` first if \
     that is what you mean";
  semantic_accept "widening-sign-change-explicit-twin"
    "fn put(x u32) void { return }\nfn f(n i32) void { put(bitcast[u32](n)); return }\n";
  let widening_mixed_source = "fn f(a u32, b i32) bool { return a < b }\n" in
  semantic_pin "widening-mixed-types-diagnostic" widening_mixed_source 1
    (String.length "fn f(a u32, b i32) bool { return a < " + 1)
    1 "cannot compare `u32` with `i32`: no type holds both"
    (Some "widen both to `i64`: `zext[i64](a) < sext[i64](b)`");
  conversion_help_twin "mixed-widening-help-compiles" widening_mixed_source "a < b"
    "zext[i64](a) < sext[i64](b)" "widen both to `i64`: `zext[i64](a) < sext[i64](b)`";
  let comparison_narrow_message =
    "`a + 1` is computed in `u32` and may wrap before it reaches `u64`"
  in
  List.iter
    (fun (name, operator) ->
      semantic_message
        ("comparison-narrow-right-" ^ name)
        comparison_narrow_message
        (Printf.sprintf "fn bad(x u64, a u32) bool { return x %s a + 1 }\n" operator);
      semantic_message
        ("comparison-narrow-left-" ^ name)
        comparison_narrow_message
        (Printf.sprintf "fn bad(x u64, a u32) bool { return a + 1 %s x }\n" operator))
    [ ("eq", "=="); ("ne", "!="); ("lt", "<"); ("le", "<="); ("gt", ">"); ("ge", ">=") ];
  semantic_accept "comparison-narrow-peer-simple"
    "fn good(x u64, a u32) bool { return x == a }\n";
  semantic_accept "comparison-narrow-peer-explicit"
    "fn good(x u64, a u32) bool { return x == zext[u64](a) + 1 }\n";
  semantic_message "comparison-narrow-constant"
    "`A + 1` is computed in `u32` and may wrap before it reaches `u64`"
    "const A u32 = 1\nconst X u64 = 2\nconst R bool = X == A + 1\n";
  let narrow_arithmetic_source =
    "fn f(a u16, b u16) u32 { value u32 = a * b; return value }\n"
  in
  semantic_pin "widening-narrow-arithmetic-diagnostic" narrow_arithmetic_source 1
    (String.length "fn f(a u16, b u16) u32 { value u32 = " + 1)
    (String.length "a * b")
    "`a * b` is computed in `u16` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `zext[u32](a) * b`");
  let shift_widening_help_source = "fn f(x u8) u32 { return (1 << 2) + x }\n" in
  semantic_pin "shift-left-widening-help-typed-operand" shift_widening_help_source 1
    (String.length "fn f(x u8) u32 { return " + 1)
    (String.length "(1 << 2) + x")
    "`(1 << 2) + x` is computed in `u8` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `(1 << 2) + zext[u32](x)`");
  conversion_help_twin "shift-left-widening-help-compiles" shift_widening_help_source
    "(1 << 2) + x" "(1 << 2) + zext[u32](x)"
    "widen an operand first: `(1 << 2) + zext[u32](x)`";
  let literal_widening_help_source = "fn f(x u8) u32 { return 4 + x }\n" in
  semantic_pin "integer-literal-left-widening-help-typed-operand"
    literal_widening_help_source 1
    (String.length "fn f(x u8) u32 { return " + 1)
    (String.length "4 + x")
    "`4 + x` is computed in `u8` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `4 + zext[u32](x)`");
  conversion_help_twin "integer-literal-left-widening-help-compiles"
    literal_widening_help_source "4 + x" "4 + zext[u32](x)"
    "widen an operand first: `4 + zext[u32](x)`";
  let integer_vector_lane_source = "fn bad(v vec[4,u16]) u32 { return (v * v)[0] }\n" in
  semantic_pin "integer-vector-lane-arithmetic-widening" integer_vector_lane_source 1
    (String.length "fn bad(v vec[4,u16]) u32 { return " + 1)
    (String.length "(v * v)[0]")
    "`(v * v)[0]` is computed in `u16` and may wrap before it reaches `u32`"
    (Some
       "widen vector operands first: `(zext[vec[4, u32]](v) * zext[vec[4, u32]](v))[0]`");
  semantic_accept "integer-vector-lane-arithmetic-explicit-widening"
    "fn good(v vec[4,u16]) u32 { return (zext[vec[4,u32]](v) * zext[vec[4,u32]](v))[0] }\n";
  conversion_help_twin "narrow-arithmetic-help-compiles" narrow_arithmetic_source
    "a * b" "zext[u32](a) * b" "widen an operand first: `zext[u32](a) * b`";
  let nested_arithmetic_source =
    "fn f(a u16, b u16) u32 { value u32 = (a + 1) * (b - 2); return value }\n"
  in
  semantic_pin "narrow-arithmetic-parentheses" nested_arithmetic_source 1
    (String.length "fn f(a u16, b u16) u32 { value u32 = " + 1)
    (String.length "(a + 1) * (b - 2)")
    "`(a + 1) * (b - 2)` is computed in `u16` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `(zext[u32](a) + 1) * (zext[u32](b) - 2)`");
  conversion_help_twin "nested-arithmetic-help-compiles" nested_arithmetic_source
    "(a + 1) * (b - 2)" "(zext[u32](a) + 1) * (zext[u32](b) - 2)"
    "widen an operand first: `(zext[u32](a) + 1) * (zext[u32](b) - 2)`";
  let expression_span name text expression =
    let start = List.hd (positions text "return ") + String.length "return " in
    match semantic_diagnostics text with
    | [ diagnostic ]
      when diagnostic.primary.Span.start_offset = start
           && diagnostic.primary.Span.end_offset = start + String.length expression
           && contains diagnostic.message "is computed in" ->
        ()
    | diagnostics ->
        failwith
          (name ^ ": unexpected expression diagnostic: "
          ^ Diag.render_all ~source:(Some (source text)) diagnostics)
  in
  let parenthesized_operands_source = "fn f(a u8, b u8) u32 { return (a) * (b) }\n" in
  expression_span "narrow-arithmetic-parenthesized-operands"
    parenthesized_operands_source "(a) * (b)";
  let if_operand_source =
    "fn f(c bool, a u8, b u8) u32 { return a * (if c { a } else { b }) }\n"
  in
  expression_span "narrow-arithmetic-if-operand" if_operand_source
    "a * (if c { a } else { b })";
  let generic_widening_source =
    "fn n[T](c bool, a T, b T) u32 { return if c { a + b } else { a } }\n\
     fn f(x u8) u32 { return n[u8](true, x, x) }\n"
  in
  semantic_pin "generic-widening-help-omitted" generic_widening_source 1
    (String.length "fn n[T](c bool, a T, b T) u32 { return if c { " + 1)
    (String.length "a + b")
    "`a + b` is computed in `u8` and may wrap before it reaches `u32`" None;
  semantic_accept "generic-widening-help-type-parameter-twin"
    "fn n[T](c bool, a T, b T) u32 { return if c { a + b } else { a } }\n\
     fn f(x u32) u32 { return n[u32](true, x, x) }\n";
  let generic_const_widening_source =
    "fn n[N const usize](c bool, a u8, b u8) u32 { return if c { a + b } else { a } }\n\
     fn f(x u8) u32 { return n[1](true, x, x) }\n"
  in
  semantic_pin "generic-const-widening-help-kept" generic_const_widening_source 1
    (String.length "fn n[N const usize](c bool, a u8, b u8) u32 { return if c { " + 1)
    (String.length "a + b")
    "`a + b` is computed in `u8` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `zext[u32](a) + b`");
  let unary_arithmetic_source = "fn f(a u16, b u16) u32 { return -(a + b) }\n" in
  semantic_pin "narrow-unary-parentheses" unary_arithmetic_source 1
    (String.index unary_arithmetic_source '-' + 1)
    (String.length "-(a + b)")
    "`-(a + b)` is computed in `u16` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `-(zext[u32](a) + b)`");
  conversion_help_twin "unary-arithmetic-help-compiles" unary_arithmetic_source
    "-(a + b)" "-(zext[u32](a) + b)" "widen an operand first: `-(zext[u32](a) + b)`";
  let bit_not_signed_source = "fn f(s i8) i32 { value i32 = ~s; return value }\n" in
  semantic_pin "narrow-bit-not-signed" bit_not_signed_source 1
    (String.index bit_not_signed_source '~' + 1)
    (String.length "~s") "`~s` is computed in `i8` and may wrap before it reaches `i32`"
    (Some "widen an operand first: `~sext[i32](s)`");
  conversion_help_twin "narrow-bit-not-signed-help-compiles" bit_not_signed_source "~s"
    "~sext[i32](s)" "widen an operand first: `~sext[i32](s)`";
  let bit_not_mask_source =
    "fn f(s i8) i32 { value i32 = ~(s & 0x7f); return value }\n"
  in
  semantic_pin "narrow-bit-not-mask" bit_not_mask_source 1
    (String.index bit_not_mask_source '~' + 1)
    (String.length "~(s & 0x7f)")
    "`~(s & 0x7f)` is computed in `i8` and may wrap before it reaches `i32`"
    (Some "widen an operand first: `~(sext[i32](s) & 0x7f)`");
  conversion_help_twin "narrow-bit-not-mask-help-compiles" bit_not_mask_source
    "~(s & 0x7f)" "~(sext[i32](s) & 0x7f)"
    "widen an operand first: `~(sext[i32](s) & 0x7f)`";
  let bit_not_sum_source = "fn f(s i8) i32 { value i32 = ~(s + 1); return value }\n" in
  semantic_pin "narrow-bit-not-sum" bit_not_sum_source 1
    (String.index bit_not_sum_source '~' + 1)
    (String.length "~(s + 1)")
    "`~(s + 1)` is computed in `i8` and may wrap before it reaches `i32`"
    (Some "widen an operand first: `~(sext[i32](s) + 1)`");
  conversion_help_twin "narrow-bit-not-sum-help-compiles" bit_not_sum_source "~(s + 1)"
    "~(sext[i32](s) + 1)" "widen an operand first: `~(sext[i32](s) + 1)`";
  let bit_not_unsigned_source = "fn f(u u8) u32 { value u32 = ~u; return value }\n" in
  semantic_pin "narrow-bit-not-unsigned" bit_not_unsigned_source 1
    (String.index bit_not_unsigned_source '~' + 1)
    (String.length "~u") "`~u` is computed in `u8` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `~zext[u32](u)`");
  conversion_help_twin "narrow-bit-not-unsigned-help-compiles" bit_not_unsigned_source
    "~u" "~zext[u32](u)" "widen an operand first: `~zext[u32](u)`";
  let call_field_arithmetic_source =
    "struct Pair { value u16 }\n\
     fn get(value u16) u16 { return value }\n\
     fn f(a u16) u32 { pair Pair = {a}\n\
     return get(a) * pair.value }\n"
  in
  semantic_pin "narrow-arithmetic-call-field" call_field_arithmetic_source 4
    (String.length "return " + 1)
    (String.length "get(a) * pair.value")
    "`get(a) * pair.value` is computed in `u16` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `zext[u32](get(a)) * pair.value`");
  conversion_help_twin "call-field-arithmetic-help-compiles"
    call_field_arithmetic_source "get(a) * pair.value" "zext[u32](get(a)) * pair.value"
    "widen an operand first: `zext[u32](get(a)) * pair.value`";
  let left_chain_source =
    "fn f(a u16, b u16, c u16) u32 { value u32 = a * b * c; return value }\n"
  in
  semantic_pin "narrow-arithmetic-left-chain" left_chain_source 1
    (String.length "fn f(a u16, b u16, c u16) u32 { value u32 = " + 1)
    (String.length "a * b * c")
    "`a * b * c` is computed in `u16` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `zext[u32](a) * b * c`");
  conversion_help_twin "narrow-arithmetic-left-chain-help-compiles" left_chain_source
    "a * b * c" "zext[u32](a) * b * c" "widen an operand first: `zext[u32](a) * b * c`";
  let shift_count_source =
    "fn f(a u16, s u16, b u16) u32 { value u32 = a << s + b; return value }\n"
  in
  semantic_pin "narrow-arithmetic-shift-count" shift_count_source 1
    (String.length "fn f(a u16, s u16, b u16) u32 { value u32 = " + 1)
    (String.length "a << s + b")
    "`a << s + b` is computed in `u16` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `zext[u32](a) << s + b`");
  conversion_help_twin "narrow-arithmetic-shift-count-help-compiles" shift_count_source
    "a << s + b" "zext[u32](a) << s + b"
    "widen an operand first: `zext[u32](a) << s + b`";
  let right_group_source =
    "fn f(a u16, b u16, c u16) u32 { value u32 = a - (b - c); return value }\n"
  in
  semantic_pin "narrow-arithmetic-right-group" right_group_source 1
    (String.length "fn f(a u16, b u16, c u16) u32 { value u32 = " + 1)
    (String.length "a - (b - c)")
    "`a - (b - c)` is computed in `u16` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `zext[u32](a) - (zext[u32](b) - c)`");
  conversion_help_twin "narrow-arithmetic-right-group-help-compiles" right_group_source
    "a - (b - c)" "zext[u32](a) - (zext[u32](b) - c)"
    "widen an operand first: `zext[u32](a) - (zext[u32](b) - c)`";
  let bit_not_operand_source =
    "fn f(a u16, b u16) u32 { value u32 = b * ~a; return value }\n"
  in
  semantic_pin "narrow-arithmetic-bit-not-operand" bit_not_operand_source 1
    (String.length "fn f(a u16, b u16) u32 { value u32 = " + 1)
    (String.length "b * ~a")
    "`b * ~a` is computed in `u16` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `zext[u32](b) * ~zext[u32](a)`");
  conversion_help_twin "narrow-arithmetic-bit-not-operand-help-compiles"
    bit_not_operand_source "b * ~a" "zext[u32](b) * ~zext[u32](a)"
    "widen an operand first: `zext[u32](b) * ~zext[u32](a)`";
  let size_expression_printer_source =
    "fn f() void { values arr[(A + 1) * (B - 2),u8] = {}; return }\n"
  in
  (match parse_file "size-expression.fas" size_expression_printer_source with
  | {
   Ast.items =
     [
       Ast.Func
         { body = Ast.Statements [ Ast.Let { ty = Ast.Array (length, _); _ }; _ ]; _ };
     ];
  }
    when length.text = "(A + 1) * (B - 2)" ->
      ()
  | _ -> failwith "size-expression-printer: grouping was lost");
  semantic_accept "widening-narrow-arithmetic-explicit-twin"
    "fn f(a u16, b u16) u32 { value u32 = zext[u32](a) * b; return value }\n";
  semantic_accept "bit-result-widen-shift-right-unsigned"
    "fn f(a u16) i32 { x i32 = a >> 1; return x }\n";
  semantic_accept "bit-result-widen-and-literal"
    "fn f(b u8) i32 { x i32 = b & 0x7f; return x }\n";
  semantic_accept "bit-result-widen-masked-shift"
    "fn f(a u16) u32 { x u32 = (a >> 4) & 0xf; return x }\n";
  semantic_accept "bit-result-widen-unsigned-division"
    "fn f(a u32) u64 { x u64 = a / 3; return x }\n";
  semantic_accept "bit-result-widen-unsigned-remainder"
    "fn f(c u8) u32 { x u32 = c % 10; return x }\n";
  semantic_accept "bit-result-widen-signed-shift-right"
    "fn f(s i16) i32 { y i32 = s >> 2; return y }\n";
  semantic_accept "bit-result-widen-signed-and"
    "fn f(s i16, t i16) i32 { y i32 = s & t; return y }\n";
  semantic_accept "bit-result-widen-comparison-operand"
    "fn f(b u8, w u32) i32 {\n\
    \  if (b & 0x7f) == w {\n\
    \    return 1\n\
    \  }\n\
    \  return 0\n\
     }\n";
  semantic_accept "bit-result-widen-or-unsigned-division"
    "fn f(a u16, w u32) u32 { x u32 = (a / 3) | w; return x }\n";
  semantic_accept "bit-result-widen-unsigned-division-shift"
    "fn f(a u32) u64 { y u64 = (a / 3) >> 1; return y }\n";
  semantic_message "bit-result-reject-multiply"
    "`a * b` is computed in `u8` and may wrap before it reaches `u32`"
    "fn f(a u8, b u8) u32 { x u32 = a * b; return x }\n";
  semantic_message "bit-result-reject-add"
    "`a + 1` is computed in `u8` and may wrap before it reaches `u32`"
    "fn f(a u8) u32 { x u32 = a + 1; return x }\n";
  semantic_message "bit-result-reject-shift-left"
    "`a << 1` is computed in `u8` and may wrap before it reaches `u32`"
    "fn f(a u8) u32 { x u32 = a << 1; return x }\n";
  semantic_message "bit-result-reject-bit-not"
    "`~a` is computed in `u8` and may wrap before it reaches `u32`"
    "fn f(a u8) u32 { x u32 = ~a; return x }\n";
  semantic_message "bit-result-reject-signed-negate"
    "`-s` is computed in `i16` and may wrap before it reaches `i32`"
    "fn f(s i16) i32 { y i32 = -s; return y }\n";
  semantic_message "bit-result-reject-signed-division"
    "`s / t` is computed in `i16` and may wrap before it reaches `i32`"
    "fn f(s i16, t i16) i32 { y i32 = s / t; return y }\n";
  semantic_message "bit-result-reject-signed-remainder"
    "`s % t` is computed in `i16` and may wrap before it reaches `i32`"
    "fn f(s i16, t i16) i32 { y i32 = s % t; return y }\n";
  semantic_message "bit-result-reject-add-under-shift"
    "`(a + b) >> 1` is computed in `u8` and may wrap before it reaches `u32`"
    "fn f(a u8, b u8) u32 { x u32 = (a + b) >> 1; return x }\n";
  semantic_message "bit-result-reject-multiply-under-mask"
    "`(a * 3) & 0xff` is computed in `u8` and may wrap before it reaches `u32`"
    "fn f(a u8) u32 { x u32 = (a * 3) & 0xff; return x }\n";
  semantic_message "widen-nested-unsigned-division-under-signed-root"
    "`(a / 3) & s` is computed in `i32` and may wrap before it reaches `i64`"
    "fn f(a u16, s i32) i64 { y i64 = (a / 3) & s; return y }\n";
  semantic_message "bit-result-reject-signed-division-and-unsigned"
    "cannot apply `&` to `i16` and `u16`: no type holds both"
    "fn f(s i16, t i16, a u16) i64 { y i64 = (s / t) & a; return y }\n";
  semantic_message "bit-result-reject-signed-division-or-unsigned-root"
    "cannot apply `|` to `i16` and `u32`: no type holds both"
    "fn f(s i16, t i16, w u32) u64 { y u64 = (s / t) | w; return y }\n";
  semantic_message "bit-result-reject-signed-division-and-unsigned-root"
    "cannot apply `&` to `i16` and `u32`: no type holds both"
    "fn f(s i16, t i16, w u32) u32 { y u32 = (s / t) & w; return y }\n";
  semantic_message "bit-result-reject-signed-division-or-signed-root"
    "`(s / t)` is computed in `i16` and may wrap before it reaches `i32`"
    "fn f(s i16, t i16, k i32) i64 { y i64 = (s / t) | k; return y }\n";
  List.iter
    (fun (name, text, message) -> semantic_message name message text)
    [
      ( "mixed-operand-reject-add-product",
        "fn f(w u32, b u8) u32 { x u32 = w + (b * 3); return x }\n",
        "`(b * 3)` is computed in `u8` and may wrap before it reaches `u32`" );
      ( "mixed-operand-reject-product-and",
        "fn f(w u32, b u8) u32 { x u32 = (b * 3) & w; return x }\n",
        "`(b * 3)` is computed in `u8` and may wrap before it reaches `u32`" );
      ( "mixed-operand-reject-subtract-sum",
        "fn f(w u32, b u8) u32 { x u32 = w - (b + 1); return x }\n",
        "`(b + 1)` is computed in `u8` and may wrap before it reaches `u32`" );
      ( "mixed-operand-reject-shift-or",
        "fn f(w u32, b u8) u32 { x u32 = (b << 1) | w; return x }\n",
        "`(b << 1)` is computed in `u8` and may wrap before it reaches `u32`" );
      ( "mixed-operand-reject-if-expression",
        "fn f(c bool, w u32, b u8) u32 { x u32 = w + (if c { b * 3 } else { b }); \
         return x }\n",
        "`(if c { b * 3 } else { b })` is computed in `u8` and may wrap before it \
         reaches `u32`" );
      ( "mixed-operand-reject-const-initializer",
        "const W u32 = 7\n\
         const B u8 = 9\n\
         const K u32 = W + (B * 3)\n\
         fn f() u32 { return K }\n",
        "`B * 3` is computed in `u8` and may wrap before it reaches `u32`" );
    ];
  semantic_pin "mixed-operand-reject-negated-signed"
    "fn f(w i32, s i16) i32 { x i32 = w * -s; return x }\n" 1
    (String.length "fn f(w i32, s i16) i32 { x i32 = w * " + 1)
    (String.length "-s")
    "`-s` is computed in `i16` and may wrap before it reaches `i32`"
    (Some "widen an operand first: `-sext[i32](s)`");
  List.iter
    (fun (name, text) -> semantic_accept name text)
    [
      ( "mixed-operand-accept-shift-right",
        "fn f(w u32, b u8) u32 { x u32 = w + (b >> 1); return x }\n" );
      ( "mixed-operand-accept-mask",
        "fn f(w u32, b u8) u32 { x u32 = w | (b & 0x7f); return x }\n" );
      ( "mixed-operand-accept-unsigned-division",
        "fn f(w u32, c u8) u32 { x u32 = w + (c / 3); return x }\n" );
      ( "mixed-operand-accept-plain-peer",
        "fn f(w u32, b u8) u32 { x u32 = w + b; return x }\n" );
      ( "mixed-operand-accept-explicit-widening",
        "fn f(w u32, b u8) u32 { x u32 = zext[u32](b) * 3 + w; return x }\n" );
    ];
  let widening_compound_source =
    "fn f(pos u32, size u64) void { pos += size; return }\n"
  in
  semantic_pin "widening-compound-assignment-diagnostic" widening_compound_source 1
    (String.length "fn f(pos u32, size u64) void { pos += " + 1)
    (String.length "size")
    "cannot use `u64` as `u32`: values above 4294967295 would be lost"
    (Some "keep the low bits with `trunc[u32](size)`");
  conversion_help_twin "compound-narrowing-help-compiles" widening_compound_source
    "size" "trunc[u32](size)" "keep the low bits with `trunc[u32](size)`";
  semantic_accept "widening-compound-assignment-twin"
    "fn f(pos u32, size u16) void { pos += size; return }\n";
  let widening_return_source = "fn f(x u64) u32 { return x }\n" in
  semantic_pin "widening-return-diagnostic" widening_return_source 1
    (String.length "fn f(x u64) u32 { return " + 1)
    1 "cannot return `u64` as `u32`: values above 4294967295 would be lost"
    (Some "keep the low bits with `trunc[u32](x)`");
  conversion_help_twin "return-narrowing-help-compiles" widening_return_source "x"
    "trunc[u32](x)" "keep the low bits with `trunc[u32](x)`";
  semantic_accept "widening-return-twin" "fn f(x u32) u64 { return x }\n";
  let widening_if_source =
    "fn f(flag bool, value i32) u32 { return if flag { value } else { 0 } }\n"
  in
  semantic_pin "widening-if-branch-diagnostic" widening_if_source 1
    (String.length "fn f(flag bool, value i32) u32 { return if flag { " + 1)
    (String.length "value") "cannot use `i32` as `u32`: negative values change meaning"
    (Some
       "reinterpret the bits with `bitcast[u32](value)`, or widen with `sext`/`zext` \
        first if that is what you mean");
  conversion_help_twin "if-branch-help-compiles" widening_if_source "value"
    "bitcast[u32](value)"
    "reinterpret the bits with `bitcast[u32](value)`, or widen with `sext`/`zext` \
     first if that is what you mean";
  semantic_accept "widening-if-branch-twin"
    "fn f(flag bool, value u16) u32 { return if flag { value } else { 0 } }\n";
  let widening_case_source =
    "const CASE u64 = 1\n\
     fn f(value u32) u32 { switch value { case CASE: return 1; default: return 0 } }\n"
  in
  semantic_pin "widening-case-value-diagnostic" widening_case_source 2
    (String.length "fn f(value u32) u32 { switch value { case " + 1)
    (String.length "CASE")
    "cannot use `u64` as `u32`: values above 4294967295 would be lost"
    (Some "keep the low bits with `trunc[u32](CASE)`");
  conversion_help_twin "case-narrowing-help-compiles" widening_case_source "CASE"
    "trunc[u32](CASE)" "keep the low bits with `trunc[u32](CASE)`";
  semantic_accept "widening-case-value-twin"
    "const CASE u8 = 1\n\
     fn f(value u32) u32 { switch value { case CASE: return 1; default: return 0 } }\n";
  let widening_size_slot_source =
    "const A u32 = 1\n\
     const B i32 = 2\n\
     fn f() void { data arr[A + B,u8] = {}; return }\n"
  in
  semantic_pin "widening-size-slot-diagnostic" widening_size_slot_source 3
    (String.length "fn f() void { data arr[A + " + 1)
    1 "cannot apply `+` to `u32` and `i32`: no type holds both" None;
  semantic_accept "size-slot-explicit-usize-twin"
    "const A u32 = 1\n\
     const B i32 = 2\n\
     fn f() void { data arr[zext[usize](A) + zext[usize](B),u8] = {}; return }\n";
  semantic_accept "widening-size-slot-explicit-usize-twin"
    "const A u32 = 1\n\
     const B i64 = 2\n\
     fn f() void { data arr[zext[usize](A) + bitcast[usize](B),u8] = {}; return }\n";
  semantic_accept "widening-u32-i64-comparison"
    "fn f(left u32, right i64) bool { return left < right }\n";
  let widening_u64_i64_source =
    "fn f(left u64, right i64) bool { return left < right }\n"
  in
  semantic_pin "widening-u64-i64-no-common-type" widening_u64_i64_source 1
    (String.length "fn f(left u64, right i64) bool { return left < " + 1)
    (String.length "right") "cannot compare `u64` with `i64`: no type holds both" None;
  let widening_u8_i8_source =
    "fn f(left u8, right i8) bool { return left == right }\n"
  in
  semantic_pin "widening-u8-i8-no-common-type" widening_u8_i8_source 1
    (String.length "fn f(left u8, right i8) bool { return left == " + 1)
    (String.length "right") "cannot compare `u8` with `i8`: no type holds both"
    (Some "widen both to `i16`: `zext[i16](left) == sext[i16](right)`");
  let widening_integer_bool_source = "fn f(value u32) bool { return value }\n" in
  semantic_pin "widening-integer-to-bool" widening_integer_bool_source 1
    (String.length "fn f(value u32) bool { return " + 1)
    5
    "cannot return `u32` as `bool`: bool and integer values use different \
     representations"
    (Some "write `value != 0`");
  conversion_help_twin "integer-to-bool-help-compiles" widening_integer_bool_source
    "value" "value != 0" "write `value != 0`";
  let widening_vector_source = "fn f(value vec[4,u8]) vec[4,u16] { return value }\n" in
  semantic_pin "widening-vector-is-explicit" widening_vector_source 1
    (String.length "fn f(value vec[4,u8]) vec[4,u16] { return " + 1)
    5 "return value is `vec[4, u8]`, expected `vec[4, u16]`" None;
  semantic_accept "widening-vector-explicit-twin"
    "fn f(value vec[4,u8]) vec[4,u16] { return zext[vec[4,u16]](value) }\n";
  let widening_addr_to_int_source = "fn f(value addr) usize { return value }\n" in
  semantic_pin "widening-address-to-integer-is-explicit" widening_addr_to_int_source 1
    (String.length "fn f(value addr) usize { return " + 1)
    5 "cannot return `addr` as `usize`: addresses convert to integers with `addr_bits`"
    (Some "write `addr_bits(value)`");
  conversion_help_twin "address-to-usize-help-compiles" widening_addr_to_int_source
    "value" "addr_bits(value)" "write `addr_bits(value)`";
  let widening_addr_i64_source = "fn f(value addr) i64 { return value }\n" in
  semantic_pin "address-to-i64-help" widening_addr_i64_source 1
    (String.length "fn f(value addr) i64 { return " + 1)
    5 "cannot return `addr` as `i64`: addresses convert to integers with `addr_bits`"
    (Some "write `bitcast[i64](addr_bits(value))`");
  conversion_help_twin "address-to-i64-help-compiles" widening_addr_i64_source "value"
    "bitcast[i64](addr_bits(value))" "write `bitcast[i64](addr_bits(value))`";
  let widening_addr_u32_source = "fn f(value addr) u32 { return value }\n" in
  semantic_pin "address-to-u32-help" widening_addr_u32_source 1
    (String.length "fn f(value addr) u32 { return " + 1)
    5 "cannot return `addr` as `u32`: addresses convert to integers with `addr_bits`"
    (Some "write `trunc[u32](addr_bits(value))`");
  conversion_help_twin "address-to-u32-help-compiles" widening_addr_u32_source "value"
    "trunc[u32](addr_bits(value))" "write `trunc[u32](addr_bits(value))`";
  let address_call_help_source =
    "fn get() addr { return null }\nfn f() i32 { return get() }\n"
  in
  conversion_help_twin "address-call-help-compiles" address_call_help_source "get()"
    "trunc[i32](addr_bits(get()))" "write `trunc[i32](addr_bits(get()))`";
  let address_field_help_source =
    "struct Holder { pointer addr }\n\
     fn f() i32 { value Holder = {null}\n\
     return value.pointer }\n"
  in
  conversion_help_twin "address-field-help-compiles" address_field_help_source
    "value.pointer" "trunc[i32](addr_bits(value.pointer))"
    "write `trunc[i32](addr_bits(value.pointer))`";
  let record_field_conversion_source =
    "struct Pair { value u8 }\nfn f(x u16) void { pair Pair = {x}\nreturn }\n"
  in
  conversion_help_twin "record-field-conversion-help-compiles"
    record_field_conversion_source "x" "trunc[u8](x)"
    "keep the low bits with `trunc[u8](x)`";
  let vector_lane_conversion_source = "fn f(x u16) vec[2,u8] { return {x, 1} }\n" in
  conversion_help_twin "vector-lane-conversion-help-compiles"
    vector_lane_conversion_source "x" "trunc[u8](x)"
    "keep the low bits with `trunc[u8](x)`";
  semantic_accept "widening-address-explicit-twin"
    "fn f(value addr) usize { return addr_bits(value) }\n";
  let widening_handle_source =
    "opaque Left\n\
     opaque Right\n\
     fn f(value handle[Left]) handle[Right] { return value }\n"
  in
  semantic_pin "widening-handle-identity-is-explicit" widening_handle_source 3
    (String.length "fn f(value handle[Left]) handle[Right] { return " + 1)
    5 "return value is `handle[Left]`, expected `handle[Right]`" None;
  semantic_accept "widening-handle-identity-twin"
    "opaque Left\nfn f(value handle[Left]) handle[Left] { return value }\n";
  semantic_accept "widening-usize-compound-u32"
    "fn f(pos usize, size u32) usize { pos += size; return pos }\n";
  semantic_accept "widening-plain-value-not-arithmetic"
    "fn f(a u16) u32 { value u32 = a; return value }\n";
  semantic_error "widening-if-narrow-arithmetic" "computed in `u16` and may wrap"
    "fn f(flag bool, a u16, b u16) u32 { return if flag { a * b } else { a } }\n";
  semantic_accept "widening-constant-runtime-positions"
    "const Byte u8 = 255\n\
     const Wide u32 = Byte\n\
     struct Box { value u32 }\n\
     var GlobalBox Box = {Byte}\n\
     const Items arr[1,u32] = {Byte}\n\
     var Global u32 = Byte\n\
     fn take(value u32) u32 { return value }\n\
     fn const_sum() u32 { return Wide + Items[0] }\n\
     fn runtime_sum(flag bool, value u8) u32 {\n\
     local u32 = value\n\
     local = value\n\
     Boxed Box = {value}\n\
     ItemsLocal arr[1,u32] = {value}\n\
     branch u32 = if flag { value } else { value }\n\
     return take(local) + Boxed.value + ItemsLocal[0] + branch\n\
     }\n";
  semantic_accept "machine-integer-u64-usize-mixed-sum"
    "fn f(a u64, b usize) usize { return a + b }\n";
  semantic_accept "machine-integer-u64-usize-mixed-compare"
    "fn f(a u64, b usize) bool { return a < b }\n";
  semantic_accept "machine-integer-i64-isize-mixed-difference"
    "fn f(a i64, b isize) isize { return a - b }\n";
  semantic_accept "machine-integer-i64-isize-mixed-compare"
    "fn f(a i64, b isize) bool { return a >= b }\n";
  semantic_accept "machine-integer-u64-to-usize-argument"
    "fn take(value usize) usize { return value }\n\
     fn f(value u64) usize { return take(value) }\n";
  semantic_accept "machine-integer-usize-to-u64-if"
    "fn f(flag bool, a u64, b usize) u64 { return if flag { a } else { b } }\n";
  semantic_pin "machine-integer-u64-to-isize-diagnostic"
    "fn f(a u64) isize { return a }\n" 1 28 1
    "cannot return `u64` as `isize`: values above 9223372036854775807 change meaning"
    (Some "write `bitcast[isize](a)`");
  semantic_pin "machine-integer-i64-to-usize-diagnostic"
    "fn f(a i64) usize { return a }\n" 1 28 1
    "cannot return `i64` as `usize`: negative values change meaning"
    (Some
       "reinterpret the bits with `bitcast[usize](a)`, or widen with `sext`/`zext` \
        first if that is what you mean");
  semantic_pin "machine-integer-usize-to-u32-diagnostic"
    "fn f(a usize) u32 { return a }\n" 1 28 1
    "cannot return `usize` as `u32`: values above 4294967295 would be lost"
    (Some "keep the low bits with `trunc[u32](a)`");
  semantic_pin "machine-integer-mixed-sum-to-isize-diagnostic"
    "fn mixed_is(a u64, b usize) isize { return a + b }\n" 1 44 5
    "cannot return `usize` as `isize`: values above 9223372036854775807 change meaning"
    (Some "write `bitcast[isize](a + b)`");
  semantic_error "machine-integer-generic-u64-usize-conflict"
    "cannot infer `T` for `gen`: `a` is `u64`, `b` is `usize`"
    "fn gen[T](a T, b T) T { return a }\n\
     fn f(a u64, b usize) usize { return gen(a, b) }\n";
  let widening_c_import_source =
    "extern \"C\" { fn imported(value u32) void }\n\
     fn f(value i64) void { imported(value); return }\n"
  in
  semantic_pin "widening-imported-c-argument-diagnostic" widening_c_import_source 2
    (String.length "fn f(value i64) void { imported(" + 1)
    5 "cannot pass `i64` as `u32`: negative values change meaning"
    (Some "write `trunc[u32](value)` if intended");
  conversion_help_twin "imported-call-help-compiles" widening_c_import_source "value"
    "trunc[u32](value)" "write `trunc[u32](value)` if intended";
  semantic_accept "widening-imported-c-argument-twin"
    "extern \"C\" { fn imported(value u64) void }\n\
     fn f(value u32) void { imported(value); return }\n";
  let widening_bool_source = "fn f(value bool) u32 { return value }\n" in
  semantic_pin "widening-bool-integer-diagnostic" widening_bool_source 1
    (String.length "fn f(value bool) u32 { return " + 1)
    5
    "cannot return `bool` as `u32`: bool and integer values use different \
     representations"
    (Some "write `zext[u32](value)`");
  conversion_help_twin "bool-to-integer-help-compiles" widening_bool_source "value"
    "zext[u32](value)" "write `zext[u32](value)`";
  let widening_addr_source = "fn f(value u32) addr { return value }\n" in
  semantic_pin "widening-integer-address-diagnostic" widening_addr_source 1
    (String.length "fn f(value u32) addr { return " + 1)
    5
    "cannot return `u32` as `addr`: integers convert to addresses with `addr_from_bits`"
    (Some "write `addr_from_bits(zext[usize](value))`");
  let implicit_widening_llvm =
    llvm_of
      "fn unsigned_value(x u8) i16 { return x }\n\
       fn signed_value(x i8) i64 { return x }\n\
       fn mixed_value(a u32, b i64) i64 { return a + b }\n\
       fn call_value(x i32) i64 { return consume(x) }\n\
       fn consume(x i64) i64 { return x }\n"
  in
  let explicit_widening_llvm =
    llvm_of
      "fn unsigned_value(x u8) i16 { return zext[i16](x) }\n\
       fn signed_value(x i8) i64 { return sext[i64](x) }\n\
       fn mixed_value(a u32, b i64) i64 { return zext[i64](a) + b }\n\
       fn call_value(x i32) i64 { return consume(sext[i64](x)) }\n\
       fn consume(x i64) i64 { return x }\n"
  in
  if implicit_widening_llvm <> explicit_widening_llvm then
    failwith "implicit widening emitted different LLVM than explicit casts";
  let addr_bitwise_source = "fn f(p addr, n u32) addr { return p & n }\n" in
  let addr_bitwise_line = String.trim addr_bitwise_source in
  let addr_bitwise_column = String.length "fn f(p addr, n u32) addr { return " + 1 in
  let addr_bitwise_expected =
    expected_diagnostic_without_help addr_bitwise_line addr_bitwise_column 1
      "bitwise `&` is not defined for `addr`"
  in
  if semantic_render addr_bitwise_source <> addr_bitwise_expected then
    failwith ("addr-bitwise-diagnostic: " ^ semantic_render addr_bitwise_source);
  let chained_comparison_source =
    "fn f(a i32, b i32, c i32) bool { return a < b < c }\n"
  in
  let chained_comparison_line = String.trim chained_comparison_source in
  let chained_comparison_column =
    String.index_from chained_comparison_line
      (String.index chained_comparison_line '<' + 1)
      '<'
    + 1
  in
  let chained_comparison_expected =
    expected_diagnostic chained_comparison_line chained_comparison_column 1
      "comparisons cannot be chained" "write `a < b && b < c`"
  in
  if semantic_render chained_comparison_source <> chained_comparison_expected then
    failwith
      ("chained-comparison-diagnostic: " ^ semantic_render chained_comparison_source);
  let comparison_operators = [ "=="; "!="; "<"; "<="; ">"; ">=" ] in
  List.iter
    (fun operator ->
      let source =
        Printf.sprintf "fn f(a i32) bool { return a %s 2 %s 3 }\n" operator operator
      in
      let line = String.trim source in
      let first_operator =
        String.index_from line (String.index line 'a' + 1) operator.[0]
      in
      let second_operator =
        String.index_from line (first_operator + String.length operator) operator.[0]
      in
      let column = second_operator + 1 in
      let help = Printf.sprintf "write `a %s 2 && 2 %s 3`" operator operator in
      let expected =
        expected_diagnostic line column (String.length operator)
          "comparisons cannot be chained" help
      in
      if semantic_render source <> expected then
        failwith
          ("chained-comparison-literal-" ^ operator ^ ": " ^ semantic_render source))
    comparison_operators;
  List.iter
    (fun (name, first, second) ->
      let source =
        Printf.sprintf "fn f(a i32, b i32, c i32) bool { return a %s b %s c }\n" first
          second
      in
      let first_operator =
        String.index_from source (String.index source 'a' + 1) first.[0]
      in
      let second_operator =
        String.index_from source (first_operator + String.length first) second.[0]
      in
      let help = Printf.sprintf "write `a %s b && b %s c`" first second in
      semantic_pin
        ("chained-comparison-" ^ name)
        source 1 (second_operator + 1) (String.length second)
        "comparisons cannot be chained" (Some help))
    [
      ("equal-tokens", "==", "==");
      ("not-equal-tokens", "!=", "!=");
      ("mixed-upward-downward", "<", ">");
      ("mixed-downward-upward", ">=", "<=");
    ];
  let vector_name_chain =
    "fn compare(a vec[4,i32], b vec[4,i32]) vec[4,bool] { return a < b < b }\n"
  in
  let vector_name_chain_operator =
    String.index_from vector_name_chain (String.index vector_name_chain '<' + 1) '<'
  in
  semantic_pin "chained-comparison-vector-names" vector_name_chain 1
    (vector_name_chain_operator + 1)
    1 "comparisons cannot be chained" (Some "write `a < b && b < b`");
  let vector_splat_chain =
    "fn compare(a vec[4,i32]) vec[4,bool] { return a < splat(2) < splat(3) }\n"
  in
  let vector_splat_chain_operator =
    String.index_from vector_splat_chain (String.index vector_splat_chain '<' + 1) '<'
  in
  semantic_pin "chained-comparison-vector-splat" vector_splat_chain 1
    (vector_splat_chain_operator + 1)
    1 "comparisons cannot be chained" None;
  let chained_call_source =
    "fn take(value bool) void { return }\n\
     fn f(a i32) void { take(a < 2 < 3); return }\n"
  in
  let chained_call_line = "fn f(a i32) void { take(a < 2 < 3); return }" in
  let chained_call_expected =
    expected_diagnostic ~line_number:2 chained_call_line
      (String.index_from chained_call_line (String.index chained_call_line '<' + 1) '<'
      + 1)
      1 "comparisons cannot be chained" "write `a < 2 && 2 < 3`"
  in
  if semantic_render chained_call_source <> chained_call_expected then
    failwith ("chained-comparison-call: " ^ semantic_render chained_call_source);
  let chained_if_line = "fn f(a i32) i32 { if a < 2 < 3 { return 1 } return 0 }" in
  let chained_if_expected =
    expected_diagnostic chained_if_line
      (String.index_from chained_if_line (String.index chained_if_line '<' + 1) '<' + 1)
      1 "comparisons cannot be chained" "write `a < 2 && 2 < 3`"
  in
  if semantic_render (chained_if_line ^ "\n") <> chained_if_expected then
    failwith ("chained-comparison-if: " ^ semantic_render (chained_if_line ^ "\n"));
  ignore
    (llvm_of
       "fn up(a i32, b i32, c i32) bool { return a < b && b < c }\n\
        fn down(a i32, b i32, c i32) bool { return a >= b && b > c }\n");
  ignore
    (llvm_of
       "fn f(a i32, b i32) bool { return (a < 2) == (b < 3) }\n\
        fn g(a i32) bool { return (a < 2) == true }\n");
  semantic_pin "comparison-parenthesized-ordering-type"
    "fn compare(x i64, y i64, z i64) bool { return (x <= y) <= z }\n" 1
    (String.index_from "fn compare(x i64, y i64, z i64) bool { return (x <= y) <= z }"
       (String.length "fn compare(x i64, y i64, z i64) bool { return (x <= y) ")
       '<'
    + 1)
    2 "ordered comparison `<=` needs an integer or integer vector, got `bool`" None;
  semantic_pin "comparison-parenthesized-equality-type"
    "fn compare(x i64, y i64, z i32) bool { return (x != y) == z }\n" 1
    (String.length "fn compare(x i64, y i64, z i32) bool { return (x != y) == " + 1)
    1 "operands of `==` have different types: `bool` and `i32`" None;
  semantic_pin "comparison-parenthesized-ordering-bool"
    "fn compare(x i64, y i64) bool { return (x >= y) >= true }\n" 1
    (String.length "fn compare(x i64, y i64) bool { return (")
    (String.length "(x >= y)")
    "ordered comparison `>=` needs an integer or integer vector, got `bool`" None;
  semantic_pin "comparison-equality-chain-bool-right"
    "fn compare(x bool, y bool, flag bool) bool { return x == y == flag }\n" 1
    (String.index_from
       "fn compare(x bool, y bool, flag bool) bool { return x == y == flag }"
       (String.index
          "fn compare(x bool, y bool, flag bool) bool { return x == y == flag }" '='
       + 2)
       '='
    + 1)
    2 "comparisons cannot be chained" (Some "write `x == y && y == flag`");
  semantic_pin "comparison-chain-constant" "const Result bool = 4 <= 8 <= 12\n" 1
    (String.index_from "const Result bool = 4 <= 8 <= 12"
       (String.index "const Result bool = 4 <= 8 <= 12" '<' + 1)
       '<'
    + 1)
    2 "comparisons cannot be chained" (Some "write `4 <= 8 && 8 <= 12`");
  semantic_pin "comparison-chain-mismatched-types-constant"
    "const Mid i32 = 8\nconst Last i64 = 12\nconst Result bool = 4 <= Mid <= Last\n" 3
    (String.index_from "const Result bool = 4 <= Mid <= Last"
       (String.index "const Result bool = 4 <= Mid <= Last" '<' + 1)
       '<'
    + 1)
    2 "comparisons cannot be chained" (Some "write `4 <= Mid && Mid <= Last`");
  semantic_pin "comparison-chain-mismatched-types-runtime"
    "fn compare(mid i32, last i64) bool { return 4 <= mid <= last }\n" 1
    (String.index_from "fn compare(mid i32, last i64) bool { return 4 <= mid <= last"
       (String.index "fn compare(mid i32, last i64) bool { return 4 <= mid <= last" '<'
       + 1)
       '<'
    + 1)
    2 "comparisons cannot be chained" (Some "write `4 <= mid && mid <= last`");
  semantic_accept "comparison-parenthesized-equality-twin"
    "fn compare(x i64, y i64) bool { return (x != y) == true }\n";
  semantic_accept "comparison-equality-chain-bool-twin"
    "fn compare(x i32, y i32) bool { return (x == y) == true }\n";
  semantic_accept "comparison-chain-rewrite-twin"
    "fn compare(x i64, y i64, z i64) bool { return x <= y && y <= z }\n";
  semantic_accept "comparison-vector-equality-chain-twin"
    "fn compare(a vec[4,bool], b vec[4,bool], c vec[4,bool]) vec[4,bool] { return a == \
     b == c }\n";
  semantic_accept "comparison-chain-constant-rewrite-twin"
    "const Result bool = 4 <= 8 && 8 <= 12\n";
  semantic_accept "comparison-vector-equality-chain-constant-twin"
    "const A vec[4,bool] = splat(true)\n\
     const B vec[4,bool] = splat(false)\n\
     const C vec[4,bool] = splat(true)\n\
     const Result vec[4,bool] = A == B == C\n";
  semantic_error "binary-widening-narrow-destination"
    "cannot assign `i64` to `i32`: values outside -2147483648..2147483647 would be lost"
    "fn f(c i64, a i32) i32 { x i32 = c + a; return x }\n";
  semantic_accept "binary-widening-initializer"
    "fn f(c i64, a i32) i64 { x i64 = c + a; return x }\n";
  semantic_accept "binary-widening-argument"
    "fn put(value i64) void { return }\n\
     fn f(c i64, a i32) void { put(c + a); return }\n";
  semantic_accept "binary-widening-return" "fn f(c i64, a i32) i64 { return c + a }\n";
  semantic_error "constant-binary-no-common-type"
    "cannot apply `+` to `i32` and `u32`: no type holds both"
    "const Left i32 = 1\nconst Right u32 = 2\nconst Sum i64 = Left + Right\n";
  ignore (llvm_of "fn f(x i32) i64 { return sext[i64](x) }\n");
  ignore
    (llvm_of
       "fn put(value u64) void { return }\n\
        fn f(x u32) void { put(zext[u64](x)); return }\n");
  semantic_pin "array-assignment-diagnostic"
    "fn f() void { left arr[2,u8] = {1,2}\n\
    \ right arr[2,u8] = {3,4}\n\
    \ left = right\n\
    \ return }\n"
    3 2 4 "aggregate assignment is not supported for an array; use `copy(dst, src)`"
    None;
  semantic_pin "array-bounds-diagnostic"
    "fn f() u8 { elems arr[2,u8] = {1,2}\n return elems[2] }\n" 2 15 1
    "array index `2` is out of bounds for length 2" None;
  semantic_pin "aggregate-parameter-diagnostic"
    "fn consume(rows arr[3,u16]) void { return }\n" 1 17 3
    "aggregate parameter `rows` of type `arr[3, u16]` cannot be passed by value; \
     declare `rows` as `addr`; callers pass its address with `&`"
    None;
  semantic_pin "aggregate-result-diagnostic"
    "fn values() arr[3,u8] { result arr[3,u8] = {1,2,3}\n return result }\n" 1 13 3
    "aggregate result `arr[3, u8]` cannot be returned by value; return `void` and take \
     the destination as an `addr` parameter"
    None;
  semantic_pin "void-field-layout-caret" "struct Holder { value void }\n" 1
    (String.length "struct Holder { value " + 1)
    4 "field `value` cannot have type `void`" None;
  semantic_pin "opaque-field-layout-caret"
    "opaque Token\nstruct Holder { value Token }\n" 2
    (String.length "struct Holder { value " + 1)
    5 "opaque type `Token` has no layout" (Some "write `handle[Token]`");
  semantic_pin "recursive-field-layout-caret" "struct Node { next Node }\n" 1
    (String.length "struct Node { next " + 1)
    4 "recursive by-value struct `Node`" None;
  semantic_pin "unknown-type-parameter-caret"
    "struct Memory { value u8 }\nfn read(value Memroy) void { return }\n" 2
    (String.length "fn read(value " + 1)
    6 "unknown type `Memroy`" (Some "did you mean `Memory`?");
  semantic_pin "unknown-type-field-caret"
    "struct Memory { value u8 }\nstruct Cache { owner Memroy }\n" 2
    (String.length "struct Cache { owner " + 1)
    6 "unknown type `Memroy`" (Some "did you mean `Memory`?");
  semantic_pin "unknown-type-handle-caret"
    "opaque Token\n\
     struct Memory { value u8 }\n\
     fn read(value handle[Memroy]) void { return }\n"
    3
    (String.length "fn read(value handle[" + 1)
    6 "unknown type `Memroy`" (Some "did you mean `Memory`?");
  let generic_call_arity =
    "fn identity[T](value T) T { return value }\n\
     fn read() i32 { return identity[i32,u32](1) }\n"
  in
  semantic_pin "generic-call-arity-caret" generic_call_arity 2
    (String.length "fn read() i32 { return identity" + 1)
    1 "generic function `identity` expects 1 generic argument, got 2" None;
  let generic_runtime_arity =
    "fn id[N const usize](value i32) i32 { return value }\n\
     fn read() i32 { return id[3](2, 4) }\n"
  in
  semantic_pin "generic-runtime-call-arity" generic_runtime_arity 2 24 11
    "function `id` expects 1 argument, got 2" None;
  semantic_accept "generic-runtime-call-arity-twin"
    "fn id[N const usize](value i32) i32 { return value }\n\
     fn read() i32 { return id[3](2) }\n";
  let generic_struct_arity =
    "struct Pair[T] { value T }\nfn read(value Pair[i32,u32]) void { return }\n"
  in
  semantic_pin "generic-struct-arity-caret" generic_struct_arity 2
    (String.length "fn read(value Pair" + 1)
    1 "generic struct `Pair` expects 1 generic argument, got 2" None;
  let const_call_arity =
    "fn choose[N const u8]() void { return }\n\
     fn read() void { choose[1,2]()\n\
     return }\n"
  in
  semantic_pin "const-call-arity-caret" const_call_arity 2
    (String.length "fn read() void { choose" + 1)
    1 "generic function `choose` expects 1 generic argument, got 2" None;
  semantic_pin "missing-return-diagnostic"
    "fn fetch() i64 {\n if true { return 3 }\n}\n" 1 4 5
    "function `fetch` returning `i64` may reach the end without `return`" None;
  semantic_pin "void-return-value-diagnostic" "fn consume() void { return false }\n" 1
    28 5 "void function cannot return a value of type `bool`" None;
  semantic_pin "wrong-arity-diagnostic"
    "fn put(a u8, b u16) void { return }\nfn main() i32 { put(1); return 0 }\n" 2 17 6
    "function `put` expects 2 arguments, got 1" None;
  semantic_pin "wrong-singular-arity-diagnostic"
    "fn put(a u8) void { return }\nfn main() i32 { put(); return 0 }\n" 2 17 5
    "function `put` expects 1 argument, got 0" None;
  semantic_accept "wrong-singular-arity-twin"
    "fn put(a u8) void { return }\nfn main() i32 { put(1); return 0 }\n";
  semantic_pin "global-initializer-diagnostic" "var TOTAL i64 = absent_value\n" 1 17 12
    "unknown name `absent_value`" None;
  semantic_pin "constant-local-initializer-diagnostic"
    "fn f() i32 { result i32 = 2\n return result }\nconst LIMIT i32 = result\n" 3 19 6
    "constant `LIMIT` initializer uses nonconstant value `result`" None;
  let constant_array_count = "const ITEMS arr[2,u8] = {4,5,6}\n" in
  semantic_pin "constant-array-count-caret" constant_array_count 1
    (String.index constant_array_count '6' + 1)
    1 "array of 2 elements, got 3" None;
  let constant_record_count = "struct Cell { value i32 }\nconst CELL Cell = {1, 2}\n" in
  semantic_pin "constant-record-count-caret" constant_record_count 2
    (String.index "const CELL Cell = {1, 2}" '2' + 1)
    1 "record `Cell` has 1 field, got 2" None;
  let constant_brace_list = "const ITEM u8 = {7}\n" in
  semantic_pin "constant-brace-list-caret" constant_brace_list 1
    (String.index constant_brace_list '{' + 1)
    1 "expression is not compile-time constant" None;
  semantic_accept "constant-brace-list-struct-storage"
    "struct Entry { value u8 }\nconst ITEM Entry = {7}\nfn main() i32 { return 0 }\n";
  let global_opaque_type = "opaque Token\nvar ITEM Token\n" in
  semantic_pin "global-opaque-type-caret" global_opaque_type 2
    (String.length "var ITEM " + 1)
    5 "opaque type `Token` can only be held as `handle[Token]`" None;
  let global_array_length = "var ITEMS arr[18446744073709551615,u8]\n" in
  semantic_pin "global-array-length-caret" global_array_length 1
    (String.length "var ITEMS arr[" + 1)
    20 "array length `18446744073709551615` is too large" None;
  semantic_pin "constant-initializer-type-caret" "const VALUE u32 = true\n" 1 19 4
    "cannot use `bool` as `u32`: bool and integer values use different representations"
    (Some "write `zext[u32](true)`");
  let constant_address_array_write =
    "var GLOBAL u8\n\
     const PTRS arr[1,addr] = {&GLOBAL}\n\
     fn write() void { PTRS[0] = null\n\
    \ return }\n"
  in
  semantic_pin "constant-address-array-write-diagnostic" constant_address_array_write 3
    19 4 "cannot modify constant `PTRS`" None;
  semantic_accept "mutable-address-array-write-twin"
    "var GLOBAL u8\n\
     var PTRS arr[1,addr] = {&GLOBAL}\n\
     fn write() void { PTRS[0] = null\n\
    \ return }\n";
  semantic_pin "literal-overflow-diagnostic" "fn f() u8 { return 257 }\n" 1 20 3
    "integer literal is out of range for u8: `257`" None;
  semantic_pin "duplicate-constant-diagnostic"
    "const AMOUNT i64 = 1\nconst AMOUNT i64 = 2\n" 2 7 6 "duplicate const `AMOUNT`" None;
  semantic_pin "duplicate-function-diagnostic"
    "fn compute() i32 { return 1 }\nfn compute() i64 { return 2 }\n" 2 4 7
    "duplicate function `compute`" None;
  semantic_pin "duplicate-record-diagnostic"
    "struct Point { x i32 }\nstruct Point { y i32 }\n" 2 8 5 "duplicate type `Point`"
    None;
  semantic_pin "logical-integer-help"
    "fn f(left i32, right i64) bool { return left && right }\n" 1 41 4
    "left operand of `&&` is `i32`, not `bool`"
    (Some "Fas has no implicit truth values; write `left != 0 && right != 0`");
  semantic_pin "shift-condition-help"
    "fn f(x i32) i32 { if x << 2 { return 1 } return 0 }\n" 1 22 1
    "condition of `if` is `i32`, not `bool`"
    (Some "Fas has no implicit truth values; write `x << 2 != 0`");
  semantic_pin "ternary-arm-diagnostic"
    "fn f(flag bool) i64 { result i64 = if flag { 1 } else { false }\n return 0 }\n" 1
    57 5
    "cannot use `bool` as `i64`: bool and integer values use different representations"
    (Some "write `zext[i64](false)`");
  semantic_pin "raw-index-diagnostic" "fn f(p addr) u8 { return p[u8, false] }\n" 1 32 5
    "raw access index must be an integer, got `bool`" None;
  semantic_pin "unknown-record-diagnostic"
    "fn f() void { value Missing = null\n return }\n" 1 21 7 "unknown type `Missing`"
    None;
  semantic_pin "call-condition-help"
    "fn ready() i32 { return 1 }\nfn f() i32 { if ready() { return 1 } return 0 }\n" 2
    17 7 "condition of `if` is `i32`, not `bool`"
    (Some "Fas has no implicit truth values; write `ready() != 0`");
  semantic_pin "logical-not-address-help" "fn f(p addr) bool { return !p }\n" 1 29 1
    "operator `!` needs `bool` or a bool vector, got `addr`" (Some "write `p == null`");
  semantic_pin "logical-not-integer-help" "fn f(count i64) bool { return !count }\n" 1
    32 5 "operator `!` needs `bool` or a bool vector, got `i64`"
    (Some "write `count == 0`");
  let bool_vector_bitnot = "fn f(mask vec[4,bool]) vec[4,bool] { return ~mask }\n" in
  semantic_pin "bitnot-bool-vector-help" bool_vector_bitnot 1
    (String.index bool_vector_bitnot '~' + 2)
    4 "unary operator `~` needs an integer or integer vector, got `vec[4, bool]`"
    (Some "write `!mask`");
  semantic_pin "logical-or-call-help"
    "fn ready() i32 { return 1 }\n\
     fn f() i32 { if false || ready() { return 1 } return 0 }\n"
    2 26 7 "right operand of `||` is `i32`, not `bool`"
    (Some "Fas has no implicit truth values; write `ready() != 0`");
  let field_candidate_source =
    "struct Point { x i32 }\nfn f(p addr) i32 { return p[Point].xx }\n"
  in
  semantic_pin "unknown-field-near-name" field_candidate_source 2
    (String.length "fn f(p addr) i32 { return p[Point]." + 1)
    2 "record `Point` has no field `xx`" (Some "did you mean `x`?");
  let field_no_candidate_source =
    "struct Point { x i32 }\nfn f(p addr) i32 { return p[Point].radius }\n"
  in
  semantic_pin "unknown-field-no-candidate" field_no_candidate_source 2
    (String.length "fn f(p addr) i32 { return p[Point]." + 1)
    6 "record `Point` has no field `radius`" None;
  let field_ambiguous_source =
    "struct Point { x i32\n xy i32 }\nfn f(p addr) i32 { return p[Point].xx }\n"
  in
  semantic_pin "unknown-field-two-candidates" field_ambiguous_source 3
    (String.length "fn f(p addr) i32 { return p[Point]." + 1)
    2 "record `Point` has no field `xx`" None;
  let value_candidate_source = "fn f(value i32) i32 { return valeu }\n" in
  semantic_pin "unknown-value-near-name" value_candidate_source 1
    (String.length "fn f(value i32) i32 { return " + 1)
    5 "unknown name `valeu`" (Some "did you mean `value`?");
  let value_no_candidate_source = "fn f() i32 { return absent_value }\n" in
  semantic_pin "unknown-value-no-candidate" value_no_candidate_source 1
    (String.length "fn f() i32 { return " + 1)
    (String.length "absent_value")
    "unknown name `absent_value`" None;
  let uppercase_null_source = "fn pointer() addr { return NULL }\n" in
  semantic_pin "unknown-name-null-help" uppercase_null_source 1
    (String.length "fn pointer() addr { return " + 1)
    4 "unknown name `NULL`" (Some "write `null`");
  semantic_accept "unknown-name-null-help-twin" "fn pointer() addr { return null }\n";
  let value_ambiguous_source = "fn f(alpha i32, alphi i32) i32 { return alph }\n" in
  semantic_pin "unknown-value-two-candidates" value_ambiguous_source 1
    (String.length "fn f(alpha i32, alphi i32) i32 { return " + 1)
    4 "unknown name `alph`" None;
  let function_candidate_source =
    "fn compute() i32 { return 1 }\nfn f() i32 { return comptue() }\n"
  in
  semantic_pin "unknown-function-near-name" function_candidate_source 2
    (String.length "fn f() i32 { return " + 1)
    9 "unknown function `comptue`" (Some "did you mean `compute`?");
  let function_no_candidate_source = "fn f() i32 { return absent_value() }\n" in
  semantic_pin "unknown-function-no-candidate" function_no_candidate_source 1
    (String.length "fn f() i32 { return " + 1)
    14 "unknown function `absent_value`" None;
  let function_ambiguous_source =
    "fn compute() i32 { return 1 }\n\
     fn commute() i32 { return 2 }\n\
     fn f() i32 { return comute() }\n"
  in
  semantic_pin "unknown-function-two-candidates" function_ambiguous_source 3
    (String.length "fn f() i32 { return " + 1)
    (String.length "comute()") "unknown function `comute`" None;
  let address_field_source =
    "struct Point { x i32 }\nfn read(q addr) i32 { return q.x }\n"
  in
  semantic_pin "address-field-help" address_field_source 2
    (String.length "fn read(q addr) i32 { return " + 1)
    1 "no field `x` on `addr`" (Some "write `q[T].x` with the record type");
  let arrow_field_source =
    "struct Point { value i32 }\nfn read(p addr) i32 { return p->value }\n"
  in
  semantic_pin "arrow-field-help" arrow_field_source 2
    (String.index "fn read(p addr) i32 { return p->value }" '-' + 1)
    2 "Fas has no `->` operator"
    (Some "read field `value` through an `addr` as `p[Point].value`");
  semantic_accept "arrow-field-help-twin"
    "struct Point { value i32 }\nfn read(p addr) i32 { return p[Point].value }\n";
  let ambiguous_arrow_field =
    "struct Point { value i32 }\n\
     struct Other { value i32 }\n\
     fn read(p addr) i32 { return p->value }\n"
  in
  semantic_pin "arrow-field-ambiguous-help" ambiguous_arrow_field 3
    (String.index "fn read(p addr) i32 { return p->value }" '-' + 1)
    2 "Fas has no `->` operator" None;
  let array_too_many_source =
    "fn f() void { data arr[3,u16] = {2,3,5,7}\n return }\n"
  in
  semantic_pin "array-count-extra-caret" array_too_many_source 1
    (String.length "fn f() void { data arr[3,u16] = {2,3,5," + 1)
    1 "array of 3 elements, got 4" None;
  let array_too_few_source = "fn f() void { data arr[3,u16] = {2,3}\n return }\n" in
  semantic_pin "array-count-short-caret" array_too_few_source 1
    (String.length "fn f() void { data arr[3,u16] = " + 1)
    1 "array of 3 elements, got 2" None;
  let record_too_many_source =
    "struct Record { key u32 }\nfn f() void { item Record = {3, 5}\n return }\n"
  in
  semantic_pin "record-count-extra-caret" record_too_many_source 2
    (String.length "fn f() void { item Record = {3, " + 1)
    1 "record `Record` has 1 field, got 2" None;
  let record_too_few_source =
    "struct Record { key u32 value u32 }\nfn f() void { item Record = {3}\n return }\n"
  in
  semantic_pin "record-count-short-caret" record_too_few_source 2
    (String.length "fn f() void { item Record = " + 1)
    1 "record `Record` has 2 fields, got 1" None;
  ignore
    (llvm_of
       "struct Point { x i32 }\n\
        fn f(p addr) i32 { return p[Point].x }\n\
        fn value(candidate i32) i32 { return candidate }\n\
        fn compute() i32 { return 1 }\n\
        fn g() i32 { return compute() }\n");
  ignore (llvm_of "struct Point { x i32 }\nfn read(q addr) i32 { return q[Point].x }\n");
  ignore
    (llvm_of
       "fn ready() bool { return true }\n\
        fn f() i32 { if ready() { return 1 } return 0 }\n");
  ignore (llvm_of "fn f(p addr) bool { return p == null }\n");
  let not_source = "fn f(x i32) bool { return !x }\n" in
  let not_expected =
    expected_diagnostic "fn f(x i32) bool { return !x }" 28 1
      "operator `!` needs `bool` or a bool vector, got `i32`" "write `x == 0`"
  in
  if semantic_render not_source <> not_expected then
    failwith ("logical-not-diagnostic: " ^ semantic_render not_source);
  let if_line = "fn f(c i32) i32 { if c { return 1 } return 0 }" in
  let if_expected =
    expected_diagnostic if_line
      (String.rindex if_line 'c' + 1)
      1 "condition of `if` is `i32`, not `bool`"
      "Fas has no implicit truth values; write `c != 0`"
  in
  if semantic_render (if_line ^ "\n") <> if_expected then
    failwith ("if-condition-diagnostic: " ^ semantic_render (if_line ^ "\n"));
  let while_line = "fn f(p addr) void { while p { break } }" in
  let while_expected =
    expected_diagnostic while_line
      (String.rindex while_line 'p' + 1)
      1 "condition of `while` is `addr`, not `bool`"
      "Fas has no implicit truth values; write `p != null`"
  in
  if semantic_render (while_line ^ "\n") <> while_expected then
    failwith ("while-condition-diagnostic: " ^ semantic_render (while_line ^ "\n"));
  let ternary_line = "fn f(c i32) i32 { return if c { 1 } else { 0 } }" in
  let ternary_expected =
    expected_diagnostic ternary_line
      (String.rindex ternary_line 'c' + 1)
      1 "condition of `if` is `i32`, not `bool`"
      "Fas has no implicit truth values; write `c != 0`"
  in
  if semantic_render (ternary_line ^ "\n") <> ternary_expected then
    failwith ("ternary-condition-diagnostic: " ^ semantic_render (ternary_line ^ "\n"));
  ignore
    (llvm_of
       "const Explicit bool = (1 != 0) && true\n\
        fn integer(value i64) i64 {\n\
       \ if value != 0 { return if (value != 0) { 1 } else { 0 } }\n\
       \ return 0\n\
       \ }\n\
        fn pointer(value addr) bool {\n\
       \ while value != addr_from_bits(0) { break }\n\
       \ return value != addr_from_bits(0)\n\
       \ }\n\
        fn counted(value i64) i64 {\n\
       \ for ; value != 0; { break }\n\
       \ return value\n\
       \ }\n");
  let vector_logical_not =
    llvm_of
      "const Clear vec[4,bool] = splat(false)\n\
       const Set vec[4,bool] = !Clear\n\
       fn invert(value vec[4,bool]) vec[4,bool] { return !value }\n\
       fn clear() vec[4,bool] { return !splat(true) }\n\
       fn first() bool { return Set[0] }\n"
  in
  if not (contains vector_logical_not "xor <4 x i1>") then
    failwith "vector-logical-not: mask lowering missing";
  ignore
    (llvm_of
       "struct Pair[T] { left T right T }\n\
        fn test() i64 { pair Pair[i64] = {12, 4}\n\
        return pair.left }\n");

  ignore (llvm_of "fn f(x u64) bool { return 1 == x }\n");
  let context_literal_arith_left =
    llvm_of "fn f(x u64) bool { return (1 + 2) == x }\n"
  in
  if not (contains context_literal_arith_left "add i64 1, 2") then
    failwith
      "context-literal-arith-left-peer: literal subtree did not take the peer type";
  ignore (llvm_of "fn f(x u64) bool { return x == (1 + 2) }\n");
  let context_splat_left =
    llvm_of "fn f(x vec[4,u32]) vec[4,bool] { return splat(1) == x }\n"
  in
  if not (contains context_splat_left "icmp eq <4 x i32>") then
    failwith "context-splat-left-peer: splat element did not take the peer type";
  if contains context_splat_left "icmp eq <4 x i1>" then
    failwith "context-splat-left-peer: comparison result leaked into splat element";
  ignore (llvm_of "fn f(x vec[4,u32]) vec[4,bool] { return x == splat(1) }\n");
  ignore (llvm_of "fn f() bool { return (1 + 2) == 3 }\n");
  ignore (llvm_of "const C u64 = 3\nfn f() bool { return (1 + 2) == C }\n");
  let context_const_splat_destination =
    llvm_of
      "const C vec[4,u32] = splat(1)\n\
       fn f(x vec[4,u32]) vec[4,bool] { return C == x }\n"
  in
  if not (contains context_const_splat_destination "icmp eq <4 x i32>") then
    failwith
      "context-const-splat-destination: splat shape did not come from the declared type";
  ignore
    (llvm_of "const C u64 = 3\nconst D bool = (1 + 2) == C\nfn f() bool { return D }\n");
  ignore
    (llvm_of
       "const C vec[4,u32] = splat(1)\n\
        const D vec[4,bool] = splat(1) == C\n\
        const E vec[4,bool] = C == splat(1)\n\
        fn f() vec[4,bool] { return D }\n\
        fn g() vec[4,bool] { return E }\n");
  semantic_error "context-null-unconstrained" "null requires an addr or handle context"
    "fn f() bool { return null == null }\n";
  semantic_accept "context-ordered-width-comparison"
    "fn f(x u32, y u64) bool { return x == y }\n";
  semantic_accept "context-ordered-width-arithmetic"
    "fn f(x u32, y u64) u64 { return x + y }\n";
  semantic_error "context-splat-no-invented-lanes"
    "`splat` needs a vector destination or vector operand to determine its lane count"
    "fn f() usize { return len(splat(1)) }\n";
  semantic_error "context-literal-range-left" "integer literal is out of range for u8"
    "fn f(x u8) bool { return 300 == x }\n";
  semantic_error "context-literal-range-right" "integer literal is out of range for u8"
    "fn f(x u8) bool { return x == 300 }\n";
  semantic_pin "context-generic-call-needs-explicit-arguments"
    "fn id[N const u64](x u64) u64 { return x }\nfn f() u64 { return id(1) }\n" 2 21 5
    "cannot infer `N` for `id`; provide explicit generic arguments"
    (Some "write the explicit generic argument list for `id`");
  ignore (llvm_of "fn f(x u32, y u64) u64 { return zext[u64](x) + y }\n");
  ignore (llvm_of "fn f(x i32, y i64) i64 { return sext[i64](x) + y }\n");
  ignore (llvm_of "fn f(x u64) u32 { return trunc[u32](x) }\n");
  semantic_error "context-no-implicit-equal-width" "computed in `u32` and may wrap"
    "fn f(x u32) u64 { return x + 1 }\n";
  semantic_error "context-no-implicit-wrong-direction"
    "cannot return `u64` as `u32`: values above 4294967295 would be lost"
    "fn f(x u64) u32 { return x + 1 }\n";
  semantic_error "context-literal-takes-peer-not-destination"
    "computed in `u32` and may wrap" "fn f(x u32) u64 { return 1 + x }\n";
  ignore
    (llvm_of
       "fn f(p addr) bool { return p == addr_from_bits(0) }\n\
        fn g(p addr) bool { return addr_from_bits(0) == p }\n");
  let shift_unsigned_right = llvm_of "fn f(x u64, n u32) u64 { return x >> n }\n" in
  if not (contains shift_unsigned_right "lshr i64") then
    failwith "shift-unsigned-right: missing lshr";
  if contains shift_unsigned_right "ashr" then
    failwith "shift-unsigned-right: unexpected ashr";
  let shift_signed_right = llvm_of "fn f(x i64, n u32) i64 { return x >> n }\n" in
  if not (contains shift_signed_right "ashr i64") then
    failwith "shift-signed-right: missing ashr";
  if contains shift_signed_right "lshr" then
    failwith "shift-signed-right: unexpected lshr";
  let shift_left =
    llvm_of
      "fn f(x u64, n u32) u64 { return x << n }\nfn g(x u64) u64 { return x << 3 }\n"
  in
  if not (contains shift_left "shl i64") then failwith "shift-left: missing shl";
  if not (contains shift_left "and i64") then
    failwith "shift-left: missing count normalization";
  let shift_generic =
    llvm_of
      "fn shg[T](x T, n u32) T { return x << n }\n\
       fn main() i32 {\n\
      \  a u8 = shg[u8](1, 1)\n\
      \  b i32 = shg[i32](1, 1)\n\
      \  return 0\n\
       }\n"
  in
  if not (contains shift_generic "shl i8") then
    failwith "shift-generic-u8: missing shl i8";
  if not (contains shift_generic "and i8") then
    failwith "shift-generic-u8: normalization not at element width 8";
  if not (contains shift_generic "shl i32") then
    failwith "shift-generic-i32: missing shl i32";
  if not (contains shift_generic "and i32") then
    failwith "shift-generic-i32: normalization not at element width 32";
  let shift_compound =
    llvm_of "fn f(n u32) u64 { x u64 = 8\n  x >>= n\n  return x }\n"
  in
  if not (contains shift_compound "lshr i64") then
    failwith "shift-compound: missing lshr";
  let shift_vector_broadcast =
    llvm_of "fn f(v vec[4,u32], n u32) vec[4,u32] { return v << n }\n"
  in
  if contains shift_vector_broadcast "poison" || contains shift_vector_broadcast "undef"
  then failwith "shift-vector-broadcast: undefined operand in count broadcast";
  let defined_construction =
    llvm_of
      "fn test() i64 {\n\
      \  a vec[4, u32] = splat(6)\n\
      \  b vec[4, u32] = splat(2)\n\
      \  s vec[4, u32] = a << b\n\
      \  m vec[4, bool] = s == splat(24)\n\
      \  n vec[4, bool] = !m\n\
      \  q vec[4, u32] = a / b\n\
      \  z vec[4, u64] = zext[vec[4,u64]](b)\n\
      \  c vec[4, i32] = splat(-6)\n\
      \  d vec[4, i32] = splat(2)\n\
      \  e vec[4, i32] = c / d\n\
      \  f vec[4, u32] = splat(7) % splat(4)\n\
      \  g vec[4, i32] = splat(-7) % splat(4)\n\
      \  if n[0] { return 1 }\n\
      \  if !m[1] { return 2 }\n\
      \  if q[2] != 3 { return 3 }\n\
      \  if z[3] != 2 { return 4 }\n\
      \  if s[0] != 24 { return 5 }\n\
      \  if e[0] != -3 { return 6 }\n\
      \  if f[0] != 3 { return 7 }\n\
      \  if g[0] != -3 { return 8 }\n\
       return 0\n\
       }\n"
  in
  List.iter
    (fun needle ->
      if not (contains defined_construction needle) then
        failwith ("defined-construction: missing " ^ needle))
    [
      "shl <4 x i32>";
      "icmp eq <4 x i32>";
      "xor <4 x i1>";
      "zext <4 x i32>";
      "udiv <4 x i32>";
      "sdiv <4 x i32>";
      "urem <4 x i32>";
      "srem <4 x i32>";
      "insertelement <4 x i32> zeroinitializer, i32 6, i32 0\n";
      "insertelement <4 x i1> zeroinitializer, i1 true, i32 0\n";
      "insertelement <4 x i32> zeroinitializer, i32 2147483648, i32 0\n";
    ];
  if contains defined_construction "poison" || contains defined_construction "undef"
  then failwith "defined-construction: undefined seed in computed values";
  List.iter
    (fun (name, ty, a, b, op, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "fn w[N const %s]() %s { return N }\n\
              const A %s = %s\n\
              const B %s = %s\n\
              fn test() %s { return w[%s(A, B)]() }\n"
             ty ty ty a ty b ty op)
      in
      if not (contains ir needle) then failwith ("sat: " ^ name ^ " drifted"))
    [
      ("u8-add-high", "u8", "200", "100", "add_sat", "ret i8 255\n");
      ("u8-add-mid", "u8", "1", "2", "add_sat", "ret i8 3\n");
      ("u8-sub-low", "u8", "5", "10", "sub_sat", "ret i8 0\n");
      ("i8-add-high", "i8", "100", "100", "add_sat", "ret i8 127\n");
      ("i8-add-low", "i8", "-100", "-100", "add_sat", "ret i8 128\n");
      ("i8-sub-high", "i8", "127", "-1", "sub_sat", "ret i8 127\n");
      ("i8-sub-low", "i8", "-128", "1", "sub_sat", "ret i8 128\n");
      ( "i64-add-min",
        "i64",
        "-9223372036854775808",
        "-1",
        "add_sat",
        "ret i64 -9223372036854775808\n" );
      ( "i64-add-max",
        "i64",
        "9223372036854775807",
        "1",
        "add_sat",
        "ret i64 9223372036854775807\n" );
      ("usize-add", "usize", "1", "2", "add_sat", "ret i64 3\n");
      ("u64-add-wrap", "u64", "18446744073709551615", "1", "add_sat", "ret i64 -1\n");
      ("u64-sub-under", "u64", "0", "1", "sub_sat", "ret i64 0\n");
      ("isize-sub", "isize", "-2", "1", "sub_sat", "ret i64 -3\n");
      ("i8-sub-degenerate", "i8", "0", "-128", "sub_sat", "ret i8 127\n");
      ("umulh-u8-high", "u8", "255", "255", "mul_hi", "ret i8 254\n");
      ("umulh-u8-mid", "u8", "2", "3", "mul_hi", "ret i8 0\n");
      ("smulh-i8-high", "i8", "-128", "-128", "mul_hi", "ret i8 64\n");
      ("smulh-i8-negative", "i8", "-128", "127", "mul_hi", "ret i8 192\n");
      ("smulh-i8-one", "i8", "-1", "-1", "mul_hi", "ret i8 0\n");
      ( "umulh-u64-max",
        "u64",
        "18446744073709551615",
        "18446744073709551615",
        "mul_hi",
        "ret i64 -2\n" );
      ( "smulh-i64-min",
        "i64",
        "-9223372036854775808",
        "-9223372036854775808",
        "mul_hi",
        "ret i64 4611686018427387904\n" );
      ("umulh-usize", "usize", "4294967296", "4294967296", "mul_hi", "ret i64 1\n");
      ("smulh-isize", "isize", "-4294967296", "3", "mul_hi", "ret i64 -1\n");
      ( "umulh-u32-max",
        "u32",
        "4294967295",
        "4294967295",
        "mul_hi",
        "ret i32 4294967294\n" );
    ];
  let sat_vec_add =
    llvm_of
      "fn w[N const u32]() u32 { return N }\n\
       const A u32 = 16712136\n\
       const B u32 = 83886692\n\
       const AV vec[4,u8] = bitcast[vec[4,u8]](A)\n\
       const BV vec[4,u8] = bitcast[vec[4,u8]](B)\n\
       const X vec[4,u8] = add_sat(AV, BV)\n\
       fn test() u32 { return w[bitcast[u32](X)]() }\n"
  in
  if not (contains sat_vec_add "ret i32 100598783\n") then
    failwith "sat: vec add lanes drifted";
  let sat_vec_sub =
    llvm_of
      "fn w[N const u32]() u32 { return N }\n\
       const A u32 = 16712136\n\
       const B u32 = 83886692\n\
       const AV vec[4,u8] = bitcast[vec[4,u8]](A)\n\
       const BV vec[4,u8] = bitcast[vec[4,u8]](B)\n\
       const Y vec[4,u8] = sub_sat(AV, BV)\n\
       fn test() u32 { return w[bitcast[u32](Y)]() }\n"
  in
  if not (contains sat_vec_sub "ret i32 16711780\n") then
    failwith "sat: vec sub lanes drifted";
  let mul_vec =
    llvm_of
      "fn w[N const u32]() u32 { return N }\n\
       const A u32 = 197375\n\
       const B u32 = 157549567\n\
       const AV vec[4,u8] = bitcast[vec[4,u8]](A)\n\
       const BV vec[4,u8] = bitcast[vec[4,u8]](B)\n\
       const X vec[4,u8] = mul_hi(AV, BV)\n\
       fn test() u32 { return w[bitcast[u32](X)]() }\n"
  in
  if not (contains mul_vec "ret i32 65790\n") then failwith "mul_hi: vec lanes drifted";
  let sat_runtime =
    llvm_of
      "fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return add_sat(a, b) }\n\
       fn g(a i32, b i32) i32 { return sub_sat(a, b) }\n\
       fn h(a u32, b u32) u32 { return sub_sat(a, b) }\n\
       fn i(a i8, b i8) i8 { return add_sat(a, b) }\n\
       fn p(a isize, b isize) isize { return add_sat(a, b) }\n\
       fn q(a usize, b usize) usize { return sub_sat(a, b) }\n\
       fn r(a vec[3,u8], b vec[3,u8]) vec[3,u8] { return add_sat(a, b) }\n\
       fn s(a vec[1,i32], b vec[1,i32]) vec[1,i32] { return sub_sat(a, b) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains sat_runtime needle) then failwith ("sat: missing " ^ needle))
    [
      "@llvm.uadd.sat.v4i8(";
      "@llvm.ssub.sat.i32(";
      "@llvm.usub.sat.i32(";
      "@llvm.sadd.sat.i8(";
      "@llvm.sadd.sat.i64(";
      "@llvm.usub.sat.i64(";
      "@llvm.uadd.sat.v3i8(";
      "@llvm.ssub.sat.v1i32(";
    ];
  if contains sat_runtime "poison" || contains sat_runtime "undef" then
    failwith "sat: undefined value in computed results";
  let mul_runtime =
    llvm_of
      "fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return mul_hi(a, b) }\n\
       fn g(a i32, b i32) i32 { return mul_hi(a, b) }\n\
       fn h(a u32, b u32) u32 { return mul_hi(a, b) }\n\
       fn i(a vec[3,i32], b vec[3,i32]) vec[3,i32] { return mul_hi(a, b) }\n\
       fn m(a u64, b u64) u64 { return mul_hi(a, b) }\n\
       fn n(a i64, b i64) i64 { return mul_hi(a, b) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains mul_runtime needle) then failwith ("mul_hi: missing " ^ needle))
    [
      "zext <4 x i8> %";
      "mul <4 x i16> %";
      "lshr <4 x i16> %";
      "<i16 8, i16 8, i16 8, i16 8>";
      "sext i32 %";
      "mul i64 %";
      "ashr i64 %";
      "trunc i64 %";
      "zext i32 %";
      "lshr i64 %";
      "sext <3 x i32> %";
      "mul <3 x i64> %";
      "ashr <3 x i64> %";
      "trunc <3 x i64> %";
      "zext i64 %";
      "sext i64 %";
      "mul i128 %";
      "lshr i128 %";
      "ashr i128 %";
      "trunc i128 %";
    ];
  if contains mul_runtime "poison" || contains mul_runtime "undef" then
    failwith "mul_hi: undefined value in computed results";
  List.iter
    (fun (name, op, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "fn w[N const u32]() u32 { return N }\n\
              const A u32 = 197375\n\
              const AV vec[4,u8] = bitcast[vec[4,u8]](A)\n\
              const X vec[4,u8] = %s(AV)\n\
              fn test() u32 { return w[bitcast[u32](X)]() }\n"
             op)
      in
      if not (contains ir needle) then failwith ("bit-count: " ^ name ^ " drifted"))
    [
      ("vec-popcount", "popcount", "ret i32 131336\n");
      ("vec-clz", "clz", "ret i32 134612480\n");
      ("vec-ctz", "ctz", "ret i32 134217984\n");
    ];
  let bitcount_runtime =
    llvm_of
      "fn f(a vec[4,u8]) vec[4,u8] { return popcount(a) }\n\
       fn g(a vec[2,i32]) vec[2,i32] { return clz(a) }\n\
       fn h(a vec[3,i64]) vec[3,i64] { return ctz(a) }\n\
       fn i(a u32) u32 { return popcount(a) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains bitcount_runtime needle) then
        failwith ("bit-count: missing " ^ needle))
    [
      "@llvm.ctpop.v4i8("; "@llvm.ctlz.v2i32("; "@llvm.cttz.v3i64("; "@llvm.ctpop.i32(";
    ];
  if contains bitcount_runtime "poison" || contains bitcount_runtime "undef" then
    failwith "bit-count: undefined value in computed results";
  List.iter
    (fun (name, expr, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "const K1 i64 = 8589934593\n\
              const K2 i64 = 8589934594\n\
              const A vec[2,i32] = bitcast[vec[2,i32]](K1)\n\
              const B vec[2,i32] = bitcast[vec[2,i32]](K2)\n\
              const M vec[2,bool] = A == B\n\
              const N vec[2,bool] = !M\n\
              const R vec[2,bool] = %s\n\
              fn f() vec[2,bool] { return R }\n"
             expr)
      in
      if not (contains ir needle) then failwith ("mask: " ^ name ^ " drifted"))
    [
      ("const-not", "!M", "ret <2 x i1> <i1 1, i1 0>\n");
      ("const-and", "M & N", "ret <2 x i1> <i1 0, i1 0>\n");
      ("const-or", "M | (A == A)", "ret <2 x i1> <i1 1, i1 1>\n");
      ("const-xor", "M ^ (A == A)", "ret <2 x i1> <i1 1, i1 0>\n");
    ];
  List.iter
    (fun (name, expr, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "const K1 i64 = 8589934593\n\
              const K2 i64 = 8589934594\n\
              const A vec[2,i32] = bitcast[vec[2,i32]](K1)\n\
              const B vec[2,i32] = bitcast[vec[2,i32]](K2)\n\
              const M vec[2,bool] = A == B\n\
              const R %s = %s\n\
              fn f() %s { return R }\n"
             (if name = "const-any" || name = "const-all" then "bool" else "vec[2,i32]")
             expr
             (if name = "const-any" || name = "const-all" then "bool" else "vec[2,i32]"))
      in
      if not (contains ir needle) then failwith ("mask: " ^ name ^ " drifted"))
    [
      ("const-any", "any(M)", "ret i1 true\n");
      ("const-all", "all(M)", "ret i1 false\n");
      ("const-select", "select(M, A, B)", "ret <2 x i32> <i32 2, i32 2>\n");
    ];
  let mask_runtime =
    llvm_of
      "fn f(m vec[2,bool], n vec[2,bool]) vec[2,bool] {\n\
       \032 a vec[2,bool] = m & n\n\
       \032 b vec[2,bool] = m | n\n\
       \032 c vec[2,bool] = m ^ n\n\
       \032 d vec[2,bool] = !a\n\
       \032 return d\n\
       }\n\
       fn g(m vec[2,bool]) bool { return any(m) }\n\
       fn h(m vec[2,bool]) bool { return all(m) }\n\
       fn i(m vec[2,bool], a vec[2,i32], b vec[2,i32]) vec[2,i32] { return select(m, \
       a, b) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains mask_runtime needle) then failwith ("mask: missing " ^ needle))
    [ "and <2 x i1>"; "or <2 x i1>"; "xor <2 x i1>"; "select <2 x i1>" ];
  if contains mask_runtime "poison" || contains mask_runtime "undef" then
    failwith "mask: undefined value in computed results";
  let mask_reductions =
    llvm_of
      "fn g(m vec[2,bool]) bool { return any(m) }\n\
       fn h(m vec[2,bool]) bool { return all(m) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains mask_reductions needle) then failwith ("mask: missing " ^ needle))
    [ "extractelement <2 x i1>"; "or i1"; "and i1" ];
  let mask_eager =
    llvm_of
      "fn e(m vec[2,bool], a vec[2,i32], b vec[2,i32], c vec[2,i32]) vec[2,i32] {\n\
       \032 return select(m, a / b, c)\n\
       }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains mask_eager needle) then failwith ("mask: missing " ^ needle))
    [ "sdiv <2 x i32>"; "select <2 x i1>" ];
  List.iter
    (fun (name, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf
             "const KA u32 = 67305985\n\
              const KB u32 = 134678021\n\
              const A vec[4,u8] = bitcast[vec[4,u8]](KA)\n\
              const B vec[4,u8] = bitcast[vec[4,u8]](KB)\n\
              const K u32 = 117572096\n\
              const I vec[4,u8] = bitcast[vec[4,u8]](K)\n\
              const K2 u16 = 3\n\
              const I2 vec[2,u8] = bitcast[vec[2,u8]](K2)\n\
              const KS u16 = 773\n\
              const IS vec[2,i8] = bitcast[vec[2,i8]](KS)\n\
              const R %s = %s\n\
              fn f() %s { return R }\n"
             rty expr rty)
      in
      if not (contains ir needle) then failwith ("shuffle: " ^ name ^ " drifted"))
    [
      ( "const-same",
        "shuffle(A, B, I)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 3, i8 3, i8 8>\n" );
      ("const-fewer", "shuffle(A, B, I2)", "vec[2,u8]", "ret <2 x i8> <i8 4, i8 1>\n");
      ( "const-signed-positive",
        "shuffle(A, B, IS)",
        "vec[2,u8]",
        "ret <2 x i8> <i8 6, i8 4>\n" );
    ];
  let shuffle_runtime =
    llvm_of
      "const K u32 = 117572096\n\
       const I vec[4,u8] = bitcast[vec[4,u8]](K)\n\
       const K2 u16 = 3\n\
       const I2 vec[2,u8] = bitcast[vec[2,u8]](K2)\n\
       fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return shuffle(a, b, I) }\n\
       fn g(a vec[4,u8], b vec[4,u8]) vec[2,u8] { return shuffle(a, b, I2) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains shuffle_runtime needle) then
        failwith ("shuffle: missing " ^ needle))
    [
      "shufflevector <4 x i8>";
      "<4 x i32> <i32 0, i32 2, i32 2, i32 7>";
      "<2 x i32> <i32 3, i32 0>";
    ];
  if contains shuffle_runtime "poison" || contains shuffle_runtime "undef" then
    failwith "shuffle: undefined value in computed results";
  List.iter
    (fun (name, setup, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf "%sconst R %s = %s\nfn f() %s { return R }\n" setup rty expr
             rty)
      in
      if not (contains ir needle) then failwith ("permute: " ^ name ^ " drifted"))
    [
      ( "const-zero-on-invalid",
        "const KA u32 = 67305985\n\
         const A vec[4,u8] = bitcast[vec[4,u8]](KA)\n\
         const KI u32 = 117572096\n\
         const I vec[4,u8] = bitcast[vec[4,u8]](KI)\n",
        "permute(A, I)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 3, i8 3, i8 0>\n" );
      ( "const-no-truncation",
        "const KA u16 = 513\n\
         const A vec[2,u8] = bitcast[vec[2,u8]](KA)\n\
         const KI u32 = 65793\n\
         const I vec[2,u16] = bitcast[vec[2,u16]](KI)\n",
        "permute(A, I)",
        "vec[2,u8]",
        "ret <2 x i8> <i8 0, i8 2>\n" );
      ( "const-more-lanes",
        "const KA u16 = 513\n\
         const A vec[2,u8] = bitcast[vec[2,u8]](KA)\n\
         const KI u32 = 83952896\n\
         const I vec[4,u8] = bitcast[vec[4,u8]](KI)\n",
        "permute(A, I)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 0, i8 2, i8 0>\n" );
    ];
  let permute_runtime =
    llvm_of
      "fn f(a vec[4,u8], i vec[4,u8]) vec[4,u8] { return permute(a, i) }\n\
       fn g(a vec[4,u8], i vec[4,u64]) vec[4,u8] { return permute(a, i) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains permute_runtime needle) then
        failwith ("permute: missing " ^ needle))
    [
      "icmp ult i8"; "icmp ult i64"; "extractelement <4 x i8>"; "insertelement <4 x i8>";
    ];
  if contains permute_runtime "poison" || contains permute_runtime "undef" then
    failwith "permute: undefined value in computed results";
  List.iter
    (fun (name, setup, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf "%sconst R %s = %s\nfn f() %s { return R }\n" setup rty expr
             rty)
      in
      if not (contains ir needle) then failwith ("reduce: " ^ name ^ " drifted"))
    [
      ( "sum-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_sum(A)",
        "u8",
        "ret i8 153\n" );
      ( "and-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_and(A)",
        "u8",
        "ret i8 136\n" );
      ( "or-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_or(A)",
        "u8",
        "ret i8 255\n" );
      ( "xor-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_xor(A)",
        "u8",
        "ret i8 117\n" );
      ( "min-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_min(A)",
        "u8",
        "ret i8 138\n" );
      ( "max-u8",
        "const K u32 = 2348321930\nconst A vec[4,u8] = bitcast[vec[4,u8]](K)\n",
        "reduce_max(A)",
        "u8",
        "ret i8 248\n" );
      ( "min-i8-signed",
        "const K u32 = 133250186\nconst A vec[4,i8] = bitcast[vec[4,i8]](K)\n",
        "reduce_min(A)",
        "i8",
        "ret i8 138\n" );
      ( "max-i8-signed",
        "const K u32 = 133250186\nconst A vec[4,i8] = bitcast[vec[4,i8]](K)\n",
        "reduce_max(A)",
        "i8",
        "ret i8 60\n" );
      ( "sum-i8-wrap",
        "const K u32 = 133250186\nconst A vec[4,i8] = bitcast[vec[4,i8]](K)\n",
        "reduce_sum(A)",
        "i8",
        "ret i8 190\n" );
      ( "one-lane",
        "const K i32 = -5\nconst A vec[1,i32] = bitcast[vec[1,i32]](K)\n",
        "reduce_sum(A)",
        "i32",
        "ret i32 4294967291\n" );
      ( "min-u32-high",
        "const K u64 = 9223372034707292160\n\
         const A vec[2,u32] = bitcast[vec[2,u32]](K)\n",
        "reduce_min(A)",
        "u32",
        "ret i32 2147483647\n" );
      ( "max-u32-high",
        "const K u64 = 9223372034707292160\n\
         const A vec[2,u32] = bitcast[vec[2,u32]](K)\n",
        "reduce_max(A)",
        "u32",
        "ret i32 2147483648\n" );
    ];
  let reduce_runtime =
    llvm_of
      "fn f(a vec[4,u8]) u8 { return reduce_sum(a) }\n\
       fn g(a vec[2,i32]) i32 { return reduce_min(a) }\n\
       fn h(a vec[2,i32]) i32 { return reduce_max(a) }\n\
       fn i(a vec[2,u64]) u64 { return reduce_and(a) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains reduce_runtime needle) then failwith ("reduce: missing " ^ needle))
    [
      "extractelement <4 x i8>";
      "add i8";
      "icmp slt i32";
      "icmp sgt i32";
      "and i64";
      "select i1";
    ];
  if contains reduce_runtime "poison" || contains reduce_runtime "undef" then
    failwith "reduce: undefined value in computed results";
  List.iter
    (fun (name, setup, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf "%sconst R %s = %s\nfn f() %s { return R }\n" setup rty expr
             rty)
      in
      if not (contains ir needle) then failwith ("compaction: " ^ name ^ " drifted"))
    [
      ( "compress-int",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KW u32 = 134416641\n\
         const W vec[4,u8] = bitcast[vec[4,u8]](KW)\n\
         const M vec[4,bool] = V == W\n",
        "compress(V, M)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 3, i8 0, i8 0>\n" );
      ( "expand-int",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KW u32 = 134416641\n\
         const W vec[4,u8] = bitcast[vec[4,u8]](KW)\n\
         const M vec[4,bool] = V == W\n",
        "expand(V, M)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 0, i8 2, i8 0>\n" );
      ( "compress-all-false",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KA u32 = 134678021\n\
         const A vec[4,u8] = bitcast[vec[4,u8]](KA)\n\
         const M vec[4,bool] = V == A\n",
        "compress(V, M)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 0, i8 0, i8 0, i8 0>\n" );
      ( "compress-all-true",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const M vec[4,bool] = V == V\n",
        "compress(V, M)",
        "vec[4,u8]",
        "ret <4 x i8> <i8 1, i8 2, i8 3, i8 4>\n" );
      ( "compress-bool-lanes",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KW4 u32 = 100991489\n\
         const W4 vec[4,u8] = bitcast[vec[4,u8]](KW4)\n\
         const KW5 u32 = 134416641\n\
         const W5 vec[4,u8] = bitcast[vec[4,u8]](KW5)\n\
         const M5 vec[4,bool] = V == W4\n\
         const M4 vec[4,bool] = V == W5\n",
        "compress(M5, M4)",
        "vec[4,bool]",
        "ret <4 x i1> <i1 1, i1 0, i1 0, i1 0>\n" );
      ( "expand-bool-lanes",
        "const KV u32 = 67305985\n\
         const V vec[4,u8] = bitcast[vec[4,u8]](KV)\n\
         const KW4 u32 = 100991489\n\
         const W4 vec[4,u8] = bitcast[vec[4,u8]](KW4)\n\
         const KW5 u32 = 134416641\n\
         const W5 vec[4,u8] = bitcast[vec[4,u8]](KW5)\n\
         const M5 vec[4,bool] = V == W4\n\
         const M4 vec[4,bool] = V == W5\n",
        "expand(M5, M4)",
        "vec[4,bool]",
        "ret <4 x i1> <i1 1, i1 0, i1 1, i1 0>\n" );
    ];
  let compaction_runtime =
    llvm_of
      "fn f(a vec[4,u8], m vec[4,bool]) vec[4,u8] { return compress(a, m) }\n\
       fn g(a vec[4,bool], m vec[4,bool]) vec[4,bool] { return expand(a, m) }\n\
       fn h(a vec[3,i32], m vec[3,bool]) vec[3,i32] { return compress(a, m) }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains compaction_runtime needle) then
        failwith ("compaction: missing " ^ needle))
    [ "insertelement <4 x i8>"; "select i1"; "extractelement <4 x i8>" ];
  if contains compaction_runtime "poison" || contains compaction_runtime "undef" then
    failwith "compaction: undefined value in computed results";
  let lane_trap_runtime =
    llvm_of
      "fn f(a vec[4,u8], i usize) u8 { return a[i] }\n\
       fn g(a vec[4,u8], i usize) void { a[i] = 9 }\n\
       fn h(a vec[4,i32], i i64) void { a[i] += 1 }\n\
       fn main() i32 { return 0 }\n"
  in
  List.iter
    (fun needle ->
      if not (contains lane_trap_runtime needle) then
        failwith ("lane trap: missing " ^ needle))
    [
      "icmp uge i64";
      "icmp slt i64";
      "icmp sge i64";
      "llvm.trap";
      "extractelement <4 x i8>";
      "insertelement <4 x i8>";
    ];
  if contains lane_trap_runtime "poison" || contains lane_trap_runtime "undef" then
    failwith "lane trap: undefined value in computed results";
  List.iter
    (fun (name, text, expected) ->
      match semantic_messages text with
      | [ message ] when message = expected -> ()
      | messages ->
          failwith ("builtin reject: " ^ name ^ ": " ^ String.concat "; " messages))
    [
      ( "arity",
        "fn f(a u8) u8 { return add_sat(a) }\n",
        "builtin `add_sat` expects two arguments" );
      ( "bool-scalar",
        "fn f() bool { return add_sat(true, false) }\n",
        "`add_sat` needs an integer or integer vector, got `bool`" );
      ( "bool-vec",
        "fn f(m vec[2,bool]) vec[2,bool] { return add_sat(m, m) }\n",
        "`add_sat` needs an integer or integer vector, got `vec[2, bool]`" );
      ( "mixed-widths",
        "fn f(a u8, b u16) u16 { return add_sat(a, b) }\n",
        "argument 2 of `add_sat` is `u16`, expected the type of argument 1 (`u8`)" );
      ( "mixed-shape",
        "fn f(a u8, v vec[2,u8]) vec[2,u8] { return add_sat(a, v) }\n",
        "argument 2 of `add_sat` is `vec[2, u8]`, expected the type of argument 1 \
         (`u8`)" );
      ( "const-mixed",
        "const A u8 = 1\nconst B u16 = 2\nconst X u16 = add_sat(A, B)\n",
        "argument 2 of `add_sat` is `u16`, expected the type of argument 1 (`u8`)" );
      ( "const-mixed-lanes",
        "const AV vec[2,u8] = splat(1)\n\
         const BV vec[4,u8] = splat(2)\n\
         const X vec[2,u8] = add_sat(AV, BV)\n",
        "argument 2 of `add_sat` is `vec[4, u8]`, expected the type of argument 1 \
         (`vec[2, u8]`)" );
      ( "vec-const-scalar",
        "const AV vec[4,u8] = splat(1)\nconst X vec[4,u8] = add_sat(AV, 2)\n",
        "argument 2 of `add_sat` is an integer literal, expected `vec[4, u8]`" );
      ( "const-bool",
        "const X bool = add_sat(true, false)\n",
        "`add_sat` needs an integer or integer vector, got `bool`" );
      ( "vec-const-bool",
        "const M vec[2,bool] = splat(true)\nconst X vec[2,bool] = add_sat(M, M)\n",
        "`add_sat` needs an integer or integer vector, got `vec[2, bool]`" );
      ( "mul-arity",
        "fn f(a u8) u8 { return mul_hi(a) }\n",
        "builtin `mul_hi` expects two arguments" );
      ( "mul-kind",
        "fn f(m vec[2,bool]) vec[2,bool] { return mul_hi(m, m) }\n",
        "`mul_hi` needs an integer or integer vector, got `vec[2, bool]`" );
      ( "mul-mismatch",
        "fn f(a u8, b u16) u16 { return mul_hi(a, b) }\n",
        "argument 2 of `mul_hi` is `u16`, expected the type of argument 1 (`u8`)" );
      ( "ptr-add-sat",
        "fn f(p addr) u8 { return add_sat(p, p) }\n",
        "`add_sat` needs an integer or integer vector, got `addr`" );
      ( "ptr-mul-hi",
        "fn f(p addr) u8 { return mul_hi(p, p) }\n",
        "`mul_hi` needs an integer or integer vector, got `addr`" );
      ( "bool-vec-mul-hi",
        "fn f(a vec[2,bool]) vec[2,bool] { return mul_hi(a, a) }\n",
        "`mul_hi` needs an integer or integer vector, got `vec[2, bool]`" );
      ( "bitcount-bool-vec",
        "fn f(m vec[2,bool]) vec[2,bool] { return popcount(m) }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-ptr",
        "fn f(p addr) u8 { return clz(p) }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-bool-scalar",
        "fn f() bool { return popcount(true) }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-const-bool",
        "const X bool = clz(true)\nfn main() i32 { return 0 }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-const-popcount",
        "const X bool = popcount(false)\nfn main() i32 { return 0 }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-const-ctz-zero",
        "const X bool = ctz(false)\nfn main() i32 { return 0 }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "bitcount-const-ctz",
        "const X bool = ctz(true)\nfn main() i32 { return 0 }\n",
        "builtin argument must be an integer or an integer vector" );
      ( "mask-compound-bool",
        "fn f() bool {\n  a bool = true\n  a &= false\n  return a\n}\n",
        "compound assignment `&=` is not defined for `bool`" );
      ( "mask-compound-vec",
        "fn f(m vec[2,bool], n vec[2,bool]) vec[2,bool] {\n\
         \032 a vec[2,bool] = m\n\
         \032 a &= n\n\
         \032 return a\n\
         }\n",
        "compound assignment `&=` is not defined for `vec[2, bool]`" );
      ( "mask-truthiness",
        "fn f(m vec[2,bool]) i32 {\n  if m { return 1 }\n  return 0\n}\n",
        "condition of `if` is `vec[2, bool]`, not `bool`" );
      ( "select-scalar-mask",
        "fn f(a vec[2,i32], b vec[2,i32]) vec[2,i32] { return select(true, a, b) }\n",
        "argument 1 of `select` is `bool`, expected a bool vector" );
      ( "select-mismatch",
        "fn f(m vec[2,bool], a vec[2,i32], b vec[2,i64]) vec[2,i32] { return select(m, \
         a, b) }\n",
        "argument 3 of `select` is `vec[2, i64]`, expected the type of argument 2 \
         (`vec[2, i32]`)" );
      ( "select-lanes",
        "fn f(m vec[2,bool], a vec[4,i32], b vec[4,i32]) vec[4,i32] { return select(m, \
         a, b) }\n",
        "select values must be vectors with the mask lane count" );
      ( "select-scalar-values",
        "fn f(m vec[2,bool], a i32, b i32) i32 { return select(m, a, b) }\n",
        "select values must be vectors with the mask lane count" );
      ( "any-int-vec",
        "fn f(a vec[2,i32]) bool { return any(a) }\n",
        "builtin argument must be a bool vector" );
      ( "any-scalar",
        "fn f(a i32) bool { return any(a) }\n",
        "builtin argument must be a bool vector" );
      ( "select-arity",
        "fn f(m vec[2,bool], a vec[2,i32]) vec[2,i32] { return select(m, a) }\n",
        "builtin `select` expects three arguments" );
      ( "any-arity",
        "fn f(m vec[2,bool]) bool { return any(m, m) }\n",
        "builtin `any` expects one argument" );
      ( "shuffle-arity",
        "fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return shuffle(a, b) }\n",
        "builtin `shuffle` expects three arguments" );
      ( "shuffle-nonconst-indices",
        "fn f(a vec[4,u8], b vec[4,u8], i vec[4,u8]) vec[4,u8] { return shuffle(a, b, \
         i) }\n",
        "shuffle indices must be a compile-time constant integer vector, got `vec[4, \
         u8]`" );
      ( "shuffle-bool-indices",
        "const I vec[4,bool] = splat(true)\n\
         fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return shuffle(a, b, I) }\n",
        "shuffle indices must be a compile-time constant integer vector, got `vec[4, \
         bool]`" );
      ( "shuffle-index-range",
        "const K u32 = 255\n\
         const I vec[4,u8] = bitcast[vec[4,u8]](K)\n\
         fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return shuffle(a, b, I) }\n",
        "shuffle index `255` is out of range for 8 lanes" );
      ( "shuffle-neg-selector",
        "const K u8 = 199\n\
         const I vec[1,i8] = bitcast[vec[1,i8]](K)\n\
         fn f(a vec[100,u8], b vec[100,u8]) vec[100,u8] { return shuffle(a, b, I) }\n",
        "shuffle index `-57` is out of range for 200 lanes" );
      ( "shuffle-signbit-selector",
        "const K u64 = 9223372036854775808\n\
         const I vec[1,u64] = bitcast[vec[1,u64]](K)\n\
         fn f(a vec[1,u8], b vec[1,u8]) vec[1,u8] { return shuffle(a, b, I) }\n",
        "shuffle index `9223372036854775808` is out of range for 2 lanes" );
      ( "shuffle-non-vector-operands",
        "fn f(a u8, b u8, i vec[4,u8]) u8 { return shuffle(a, b, i) }\n",
        "argument 1 of `shuffle` must be an integer or bool vector, got `u8`" );
      ( "shuffle-operand-mismatch",
        "const K u32 = 117572096\n\
         const I vec[4,u8] = bitcast[vec[4,u8]](K)\n\
         fn f(a vec[4,u8], b vec[2,u8]) vec[4,u8] { return shuffle(a, b, I) }\n",
        "argument 2 of `shuffle` is `vec[2, u8]`, expected the type of argument 1 \
         (`vec[4, u8]`)" );
      ( "permute-arity",
        "fn f(a vec[4,u8]) vec[4,u8] { return permute(a) }\n",
        "builtin `permute` expects two arguments" );
      ( "permute-signed-indices",
        "fn f(a vec[4,u8], i vec[4,i32]) vec[4,u8] { return permute(a, i) }\n",
        "permute indices must be an unsigned integer vector" );
      ( "permute-non-vector-value",
        "fn f(a u8, i vec[4,u8]) u8 { return permute(a, i) }\n",
        "permute value must be a vector" );
      ( "reduce-arity-two",
        "fn f(a vec[2,u8], b vec[2,u8]) u8 { return reduce_sum(a, b) }\n",
        "builtin `reduce_sum` expects one argument" );
      ( "reduce-arity-zero",
        "fn f() u8 { return reduce_sum() }\n",
        "builtin `reduce_sum` expects one argument" );
      ( "reduce-scalar-arg",
        "fn f(a u8) u8 { return reduce_sum(a) }\n",
        "`reduce_sum` needs an integer vector, got `u8`" );
      ( "reduce-bool-vec-arg",
        "fn f(a vec[2,bool]) bool { return reduce_min(a) }\n",
        "`reduce_min` needs an integer vector, got `vec[2, bool]`" );
      ( "reduce-fold-scalar",
        "const K u8 = 5\nconst R u8 = reduce_max(K)\nfn f() u8 { return R }\n",
        "`reduce_max` needs an integer vector, got `u8`" );
      ( "reduce-fold-bool",
        "const K u32 = 67305985\n\
         const A vec[4,u8] = bitcast[vec[4,u8]](K)\n\
         const M vec[4,bool] = A == A\n\
         const R u8 = reduce_sum(M)\n\
         fn f() u8 { return R }\n",
        "`reduce_sum` needs an integer vector, got `vec[4, bool]`" );
      ( "compress-arity",
        "fn f(a vec[4,u8], m vec[4,bool]) vec[4,u8] { return compress(a) }\n",
        "builtin `compress` expects two arguments" );
      ( "expand-arity",
        "fn f(a vec[4,u8]) vec[4,u8] { return expand(a) }\n",
        "builtin `expand` expects two arguments" );
      ( "compress-non-vector-values",
        "fn f(a u8, m vec[4,bool]) u8 { return compress(a, m) }\n",
        "compress values must be a vector" );
      ( "compress-mask-not-bool-vec",
        "fn f(a vec[4,u8], m vec[4,u8]) vec[4,u8] { return compress(a, m) }\n",
        "compress mask must be a bool vector" );
      ( "compress-lane-count",
        "fn f(a vec[4,u8], m vec[3,bool]) vec[4,u8] { return compress(a, m) }\n",
        "compress values and mask must have the same lane count" );
      ( "expand-lane-count",
        "fn f(a vec[4,u8], m vec[5,bool]) vec[4,u8] { return expand(a, m) }\n",
        "expand values and mask must have the same lane count" );
      ( "vec-lane-read-oob",
        "fn f(a vec[4,u8]) u8 { return a[7] }\n",
        "array index `7` is out of bounds for length 4" );
      ( "vec-lane-write-oob",
        "fn f(a vec[4,u8]) void { a[7] = 1 }\n",
        "array index `7` is out of bounds for length 4" );
      ( "vec-lane-compound-oob",
        "fn f(a vec[4,u8]) void { a[7] += 1 }\n",
        "array index `7` is out of bounds for length 4" );
      ( "vec-lane-negative",
        "fn f(a vec[4,u8]) u8 { return a[-1] }\n",
        "array index `-1` is out of bounds for length 4" );
      ( "vec-lane-const-oob",
        "const K usize = 9\nfn f(a vec[4,u8]) u8 { return a[K] }\n",
        "array index `9` is out of bounds for length 4" );
      ( "zext-equal-width",
        "fn f(a u32) u32 { return zext[u32](a) }\n",
        "illegal `zext` from `u32` to `u32`: the value already has type `u32`" );
      ( "trunc-widening",
        "fn f(a u8) u32 { return trunc[u32](a) }\n",
        "illegal `trunc` from `u8` to `u32`: the destination must be a narrower \
         integer type" );
      ( "zext-to-bool",
        "fn f(a u8) bool { return zext[bool](a) }\n",
        "illegal `zext` from `u8` to `bool`: the source and destination must be \
         integer types" );
    ];
  List.iter
    (fun (name, expr, rty, needle) ->
      let ir =
        llvm_of
          (Printf.sprintf "const T %s = %s\nfn f() %s { return T }\n" rty expr rty)
      in
      if not (contains ir needle) then failwith ("div-edges: " ^ name ^ " drifted"))
    [
      ("trunc-negative-dividend", "-7 / 3", "i32", "ret i32 4294967294\n");
      ("trunc-negative-divisor", "7 / -3", "i32", "ret i32 4294967294\n");
      ("rem-sign-negative", "-7 % 3", "i32", "ret i32 4294967295\n");
      ("rem-sign-positive", "7 % -3", "i32", "ret i32 1\n");
      ("rem-min-minus-one", "-9223372036854775808 % -1", "i64", "ret i64 0\n");
      ("trunc-negative-i64", "-7 / 3", "i64", "ret i64 -2\n");
      ( "cond-right-assoc",
        "if false { 2 } else { if true { 3 } else { 4 } }",
        "i32",
        "ret i32 3\n" );
      ("arith-left-assoc", "8 - 3 - 2", "i32", "ret i32 3\n");
      ("bitwise-above-comparison", "(6 & 3) == 2", "bool", "ret i1 true\n");
    ];
  List.iter
    (fun (name, ir) ->
      if contains ir " nuw " || contains ir " nsw " || contains ir " exact " then
        failwith (name ^ ": unexpected shift flags"))
    [
      ("shift-unsigned-right", shift_unsigned_right);
      ("shift-signed-right", shift_signed_right);
      ("shift-left", shift_left);
      ("shift-generic", shift_generic);
      ("shift-compound", shift_compound);
      ("shift-vector-broadcast", shift_vector_broadcast);
    ];

  let lane_paired_division_guard =
    llvm_of
      "fn div(left vec[2,i64], right vec[2,i64]) vec[2,i64] { return left / right }\n"
  in
  List.iter
    (fun marker ->
      if not (contains lane_paired_division_guard marker) then
        failwith ("lane-paired-division-guard: missing `" ^ marker ^ "`"))
    [
      "and <2 x i1>";
      "or <2 x i1>";
      "icmp eq <2 x i64>";
      "sdiv <2 x i64>";
      "call void @llvm.trap()";
    ];
  if
    List.length
      (positions lane_paired_division_guard "call i1 @llvm.vector.reduce.or.v2i1")
    <> 1
  then
    failwith
      "lane-paired-division-guard: overflow pairing must collapse to one reduction";
  let scan_indices =
    List.map
      (fun offset ->
        let rec line_end index =
          if lane_paired_division_guard.[index] = '\n' then index
          else line_end (index + 1)
        in
        lane_paired_division_guard.[line_end
                                      (offset + String.length "extractelement <2 x i1> ")
                                    - 1])
      (positions lane_paired_division_guard "extractelement <2 x i1> ")
  in
  if scan_indices <> [ '0'; '1' ] then
    failwith
      "lane-paired-division-guard: trap checks must scan lanes in scalar element order";
  let bool_one_lane_bitcasts =
    llvm_of
      "fn to_bool(mask vec[1,bool]) bool { return bitcast[bool](mask) }\n\
       fn to_mask(value i32) vec[1,bool] { return \
       bitcast[vec[1,bool]](trunc[bool](value)) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains bool_one_lane_bitcasts marker) then
        failwith ("bool-one-lane-bitcasts: missing `" ^ marker ^ "`"))
    [ "bitcast <1 x i1>"; "bitcast i1 " ];
  List.iter
    (fun forbidden ->
      if contains bool_one_lane_bitcasts forbidden then
        failwith ("bool-one-lane-bitcasts: unexpected `" ^ forbidden ^ "` lowering"))
    [ "trunc <1 x i1>"; "zext i1 "; "sext i1 " ];
  let cross_shape_value_bitcasts =
    llvm_of
      "fn one_to_int(value vec[1,u64]) u64 { return bitcast[u64](value) }\n\
       fn int_to_one(value u64) vec[1,u64] { return bitcast[vec[1,u64]](value) }\n\
       fn widen(value vec[4,u32]) vec[16,u8] { return bitcast[vec[16,u8]](value) }\n\
       fn narrow(value vec[16,u8]) vec[4,u32] { return bitcast[vec[4,u32]](value) }\n\
       fn odd_reshape(value vec[3,u16]) vec[6,u8] { return bitcast[vec[6,u8]](value) }\n\
       fn odd_restore(value vec[6,u8]) vec[3,u16] {\n\
      \       return bitcast[vec[3,u16]](value)\n\
      \ }\n\
       fn pack_bool(value vec[8,bool]) u8 { return bitcast[u8](value) }\n\
       fn unpack_bool(value u8) vec[8,bool] { return bitcast[vec[8,bool]](value) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains cross_shape_value_bitcasts marker) then
        failwith ("cross-shape-value-bitcasts: missing `" ^ marker ^ "`"))
    [
      "bitcast <1 x i64>";
      "bitcast i64 ";
      "bitcast <4 x i32>";
      "bitcast <16 x i8>";
      "bitcast <3 x i16>";
      "bitcast <6 x i8>";
      "bitcast <8 x i1>";
      "bitcast i8 ";
    ];
  if List.length (positions cross_shape_value_bitcasts "bitcast ") <> 8 then
    failwith "cross-shape-value-bitcasts: expected eight mechanical LLVM bitcasts";
  let odd_lane_conversions =
    llvm_of
      "fn widen_bool(value vec[3,bool]) vec[3,u16] { return zext[vec[3,u16]](value) }\n\
       fn sign_widen(value vec[3,i8]) vec[3,i64] { return sext[vec[3,i64]](value) }\n\
       fn narrow(value vec[3,u64]) vec[3,u8] { return trunc[vec[3,u8]](value) }\n\
       fn truth_bits(value vec[3,u32]) vec[3,bool] { return trunc[vec[3,bool]](value) }\n\
       fn one_sign(value vec[1,bool]) vec[1,i64] { return sext[vec[1,i64]](value) }\n\
       fn one_zero(value vec[1,bool]) vec[1,u64] { return zext[vec[1,u64]](value) }\n\
       fn widen_unsigned(value vec[3,u8]) vec[3,u64] { return zext[vec[3,u64]](value) }\n\
       fn sign_from_unsigned(value vec[2,u8]) vec[2,u32] {\n\
      \       return sext[vec[2,u32]](value)\n\
      \ }\n"
  in
  List.iter
    (fun marker ->
      if not (contains odd_lane_conversions marker) then
        failwith ("odd-lane-conversions: missing `" ^ marker ^ "`"))
    [
      "zext <3 x i1>";
      "sext <3 x i8>";
      "trunc <3 x i64>";
      "trunc <3 x i32>";
      "sext <1 x i1>";
      "zext <1 x i1>";
      "zext <3 x i8>";
      "sext <2 x i8>";
    ];
  let scalar_width_conversions =
    llvm_of
      "fn widen(value u8) u64 { return zext[u64](value) }\n\
       fn sign_widen(value i8) i64 { return sext[i64](value) }\n\
       fn sign_from_unsigned(value u8) u32 { return sext[u32](value) }\n\
       fn low_bit(value u32) bool { return trunc[bool](value) }\n\
       fn narrow(value u64) u16 { return trunc[u16](value) }\n\
       fn widen_size(value u16) usize { return zext[usize](value) }\n\
       fn sign_size(value i16) isize { return sext[isize](value) }\n\
       fn bool_zero(value bool) u64 { return zext[u64](value) }\n\
       fn bool_sign(value bool) i64 { return sext[i64](value) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains scalar_width_conversions marker) then
        failwith ("scalar-width-conversions: missing `" ^ marker ^ "`"))
    [
      "zext i8";
      "sext i8";
      "trunc i32";
      "trunc i64";
      "zext i16";
      "sext i16";
      "zext i1";
      "sext i1";
    ];
  semantic_error "vector-zext-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u8]) vec[4,u8] { return zext[vec[4,u8]](value) }\n";
  semantic_error "vector-sext-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,i32]) vec[4,i32] { return sext[vec[4,i32]](value) }\n";
  semantic_error "vector-trunc-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u32]) vec[4,u32] { return trunc[vec[4,u32]](value) }\n";
  semantic_error "vector-zext-bool-equal-width"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,bool]) vec[4,bool] { return zext[vec[4,bool]](value) }\n";
  semantic_error "vector-sext-lane-count"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,i8]) vec[2,i16] { return sext[vec[2,i16]](value) }\n";
  semantic_error "vector-trunc-lane-count"
    "illegal cast for source and destination widths"
    "fn f(value vec[4,u16]) vec[2,u8] { return trunc[vec[2,u8]](value) }\n";
  semantic_error "scalar-zext-bool-destination"
    "illegal cast for source and destination widths"
    "fn f(value u8) bool { return zext[bool](value) }\n";
  semantic_error "scalar-sext-bool-destination"
    "illegal cast for source and destination widths"
    "fn f(value u8) bool { return sext[bool](value) }\n";
  semantic_error "bitcast-bool-padding-source"
    "illegal cast for source and destination widths"
    "fn f(value vec[3,bool]) u8 { return bitcast[u8](value) }\n";
  semantic_error "bitcast-bool-padding-destination"
    "illegal cast for source and destination widths"
    "fn f(value u8) vec[3,bool] { return bitcast[vec[3,bool]](value) }\n";
  semantic_error "bitcast-unequal-scalar-widths"
    "illegal cast for source and destination widths"
    "fn f(value u64) u32 { return bitcast[u32](value) }\n";
  semantic_error "bitcast-array-destination"
    "illegal cast for source and destination widths"
    "fn f(value vec[2,u32]) vec[2,u32] { return bitcast[arr[2,u32]](value) }\n";
  semantic_error "bitcast-struct-destination"
    "illegal cast for source and destination widths"
    "struct Pair { left u32 right u32 }\n\
    \ fn f(value vec[2,u32]) vec[2,u32] { return bitcast[Pair](value) }\n";
  semantic_error "cast-pointer-zext" "illegal cast for source and destination widths"
    "fn f(value addr) u64 { return zext[u64](value) }\n";
  semantic_error "cast-pointer-sext" "illegal cast for source and destination widths"
    "fn f(value addr) u64 { return sext[u64](value) }\n";
  semantic_error "cast-pointer-trunc" "illegal cast for source and destination widths"
    "fn f(value addr) u32 { return trunc[u32](value) }\n";
  semantic_error "cast-pointer-bitcast-bool"
    "illegal cast for source and destination widths"
    "fn f(value addr) bool { return bitcast[bool](value) }\n";
  semantic_error "cast-vector-bitcast-pointer"
    "illegal cast for source and destination widths"
    "fn f(value vec[1,u64]) addr { return bitcast[addr](value) }\n";
  semantic_error "cast-aggregate-zext" "illegal cast for source and destination widths"
    "struct Pair { left u32 right u32 }\n\
    \ fn f() u64 {\n\
    \ value Pair = {1, 2}\n\
    \ return zext[u64](value)\n\
    \ }\n";
  semantic_error "constant-vector-division-first-lane-overflow"
    "signed division `-2147483648 / -1` overflows `i32`"
    "const XA u64 = 6442450944\n\
     const XB u64 = 4294967295\n\
     const A vec[2,i32] = bitcast[vec[2,i32]](XA)\n\
     const B vec[2,i32] = bitcast[vec[2,i32]](XB)\n\
     const Q vec[2,i32] = A / B\n\
     fn main() i32 { return 0 }\n";
  semantic_error "constant-vector-division-first-lane-zero"
    "division by zero in constant expression"
    "const XC u64 = 9223372036854775809\n\
     const XD u64 = 18446744069414584320\n\
     const C vec[2,i32] = bitcast[vec[2,i32]](XC)\n\
     const D vec[2,i32] = bitcast[vec[2,i32]](XD)\n\
     const R vec[2,i32] = C / D\n\
     fn main() i32 { return 0 }\n";
  let constant_division_cross_pairs =
    llvm_of
      "const XE u64 = 19327352832\n\
       const XF u64 = 18446744069414584321\n\
       const E vec[2,i32] = bitcast[vec[2,i32]](XE)\n\
       const F vec[2,i32] = bitcast[vec[2,i32]](XF)\n\
       const S vec[2,i32] = E / F\n\
       fn low() i32 { return S[0] }\n"
  in
  if not (contains constant_division_cross_pairs "<i32 2147483648, i32 4294967292>")
  then failwith "constant-division-cross-pairs: expected paired lane result";
  let constant_remainder_edges =
    llvm_of
      "const XA u64 = 23622320128\n\
       const XB u64 = 12884901887\n\
       const A vec[2,i32] = bitcast[vec[2,i32]](XA)\n\
       const B vec[2,i32] = bitcast[vec[2,i32]](XB)\n\
       const R vec[2,i32] = A % B\n\
       fn low() i32 { return R[0] }\n"
  in
  if not (contains constant_remainder_edges "<i32 0, i32 1>") then
    failwith "constant-remainder-edges: MIN % -1 lane must yield zero";
  let constant_bool_pack =
    llvm_of
      "const A u8 = 204\n\
       const P vec[8,bool] = bitcast[vec[8,bool]](A)\n\
       const M vec[1,bool] = splat(true)\n\
       const B bool = bitcast[bool](M)\n\
       const M2 vec[1,bool] = bitcast[vec[1,bool]](true)\n\
       fn packed() vec[8,bool] { return P }\n\
       fn single() bool { return B }\n"
  in
  if
    not (contains constant_bool_pack "<i1 0, i1 0, i1 1, i1 1, i1 0, i1 0, i1 1, i1 1>")
  then failwith "constant-bool-pack: expected packed value bits";
  semantic_error "constant-bitcast-literal-parity"
    "illegal cast for source and destination widths"
    "const P vec[8,bool] = bitcast[vec[8,bool]](204)\nfn main() i32 { return 0 }\n";
  semantic_error "runtime-bitcast-literal-parity"
    "illegal cast for source and destination widths"
    "fn f() vec[8,bool] { return bitcast[vec[8,bool]](204) }\n";
  let generic_cast_path =
    llvm_of
      "fn unwrap[N const u64](mask vec[1,bool]) bool { return bitcast[bool](mask) }\n\
       fn widen_generic[N const u64](value u8) u64 { return zext[u64](value) + N }\n\
       fn check_case_it() bool { return unwrap[7](splat(true)) }\n\
       fn check_case_wide() u64 { return widen_generic[2](255) }\n"
  in
  List.iter
    (fun marker ->
      if not (contains generic_cast_path marker) then
        failwith ("generic-cast-path: missing `" ^ marker ^ "`"))
    [ "bitcast <1 x i1>"; "zext i8" ];
  semantic_error "raw-select-addr-add-both"
    "address arithmetic for `+` has operands `addr` and `addr`; expected a scalar \
     integer offset"
    "fn f(a addr, b addr) addr { return a + b }\n";
  semantic_error "raw-select-addr-sub-both"
    "address arithmetic for `-` has operands `addr` and `addr`; expected a scalar \
     integer offset"
    "fn f(a addr, b addr) addr { return a - b }\n";
  semantic_error "raw-select-int-addr-sub"
    "address arithmetic for `-` has operands `usize` and `addr`; expected a scalar \
     integer offset"
    "fn f(n usize, p addr) usize { return n - p }\n";
  semantic_error "raw-select-handle-select" "cannot select through a handle"
    "opaque O\nfn f(h handle[O]) u8 { return h[u8] }\n";
  semantic_error "raw-select-aggregate-load"
    "raw access cannot load an array or struct value"
    "struct S { a u8 }\nfn f(p addr) void { value S = p[S]\nreturn }\n";
  semantic_error "raw-select-aggregate-store"
    "raw access cannot store an array or struct value"
    "struct S { a u8 }\nfn f(p addr) void { s S = {1}\np[S] = s\nreturn }\n";
  semantic_error "raw-select-lane" "raw vector lane selection is not supported"
    "fn f(p addr) u32 { return p[vec[2,u32]][0] }\n";
  semantic_error "raw-select-lane-store" "raw vector lane selection is not supported"
    "fn f(p addr) void { p[vec[2,u32]][0] = 1 }\n";
  semantic_error "raw-select-index-nonint"
    "raw access index must be an integer, got `bool`"
    "fn f(p addr, b bool) u32 { return p[u32, b] }\n";
  semantic_error "raw-select-three-payloads"
    "raw access needs a type argument and an optional index"
    "fn f(p addr, i usize) u32 { return p[u32, i, 3] }\n";
  semantic_error "raw-select-value-payload" "raw access on `addr` needs an element type"
    "fn f(p addr, x u32) u32 { return p[x] }\n";
  semantic_error "raw-select-const-payload" "raw access on `addr` needs an element type"
    "fn f(p addr) u32 { return p[3] }\n";
  let raw_void = "fn f(p addr) void { p[void] = p[void] }\n" in
  let raw_void_index = String.index_from raw_void (String.index raw_void '[' + 1) 'v' in
  semantic_pin "raw-select-void" raw_void 1 (raw_void_index + 1) 4
    "raw access on `void` needs an element type" None;
  semantic_error "raw-select-compound-mul-addr"
    "compound assignment `*=` is not defined for `addr`"
    "fn f(p addr) void { p[addr] *= 2 }\n";
  semantic_error "raw-select-compound-addr-bad"
    "right operand of `+=` is `bool`; `addr` arithmetic needs a scalar integer offset"
    "fn f(p addr, b bool) void { p[addr] += b }\n";
  semantic_error "raw-select-field-nonstruct" "no field `x` on `u32`"
    "fn f(p addr) u32 { return p[u32].x }\n";
  semantic_error "raw-select-unknown-field" "record `S` has no field `x`"
    "struct S { a u8 }\nfn f(p addr) u8 { return p[S].x }\n";
  semantic_message "unknown-field-assignment" "record `S` has no field `missing`"
    "struct S { value i32 }\nfn f() void { item S = {1}\nitem.missing = 2\nreturn }\n";
  semantic_message "unknown-field-offsetof" "record `S` has no field `missing`"
    "struct S { value i32 }\nfn f() usize { return offsetof[S, missing] }\n";
  semantic_message "unknown-field-constant-offsetof" "record `S` has no field `missing`"
    "struct S { value i32 }\nconst Offset usize = offsetof[S, missing]\n";
  let handle_field_source = "opaque O\nfn f(h handle[O]) usize { return h.x }\n" in
  let handle_field_line = "fn f(h handle[O]) usize { return h.x }" in
  let handle_field_expected =
    expected_diagnostic_without_help ~line_number:2 handle_field_line 34 1
      "no field `x` on `handle[O]`"
  in
  if semantic_render handle_field_source <> handle_field_expected then
    failwith "handle-field-reject: receiver diagnostic span or message changed";
  let unknown_field_source =
    "struct Pair { x i32 }\nfn main() i32 {\n p Pair = { 1 }\n return p.missing\n}\n"
  in
  let unknown_field_line = " return p.missing" in
  let unknown_field_expected =
    expected_diagnostic_without_help ~line_number:4 unknown_field_line 11 7
      "record `Pair` has no field `missing`"
  in
  if semantic_render unknown_field_source <> unknown_field_expected then
    failwith "unknown-field: field-name diagnostic span or message changed";
  let raw_load_shape = llvm_of "fn f(p addr) u32 { return p[u32] }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_load_shape marker) then
        failwith ("raw-load-shape: missing `" ^ marker ^ "`"))
    [ "load i32, ptr %"; ", align 1" ];
  let raw_bool_shape = llvm_of "fn f(p addr) bool { return p[bool] }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_bool_shape marker) then
        failwith ("raw-bool-shape: missing `" ^ marker ^ "`"))
    [ "load i8, ptr %"; "icmp ne i8" ];
  let bool_byte_storage =
    llvm_of
      "struct BoolPair { first bool second bool }\n\
       var BooleanGlobal bool = false\n\
       const BooleanArray arr[2,bool] = {true, false}\n\
       var ConstantPair BoolPair = {true, false}\n\
       fn bool_storage(p addr, input bool) void {\n\
       local bool = input\n\
       values arr[2,bool] = {false, true}\n\
       fields BoolPair = {false, false}\n\
       large arr[20,bool] = {false, false, false, false, false, false, false, false, \
       false, false, false, false, false, false, false, false, false, false, false, \
       true}\n\
       copy(fields, ConstantPair)\n\
       local = true\n\
       values[0] = true\n\
       fields.second = true\n\
       BooleanGlobal = true\n\
       volatile_store[bool](p, input)\n\
       return\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if contains bool_byte_storage marker then
        failwith
          ("bool-memory-byte-storage: forbidden LLVM memory form `" ^ marker ^ "`"))
    [
      "alloca i1";
      "load i1, ptr";
      "load volatile i1, ptr";
      "store i1 ";
      "store volatile i1 ";
      "[2 x i1]";
      "[20 x i1]";
      "global i1";
      "constant i1";
    ];
  List.iter
    (fun marker ->
      if not (contains bool_byte_storage marker) then
        failwith ("bool-memory-byte-storage: missing `" ^ marker ^ "`"))
    [
      "zext i1";
      "store i8";
      "@BooleanGlobal = internal global [1 x i8]";
      "@BooleanArray = private unnamed_addr constant [2 x i8]";
      "@.literal.0 = private constant [20 x i8]";
    ];
  let raw_mask_shape = llvm_of "fn f(p addr) vec[3,bool] { return p[vec[3,bool]] }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_mask_shape marker) then
        failwith ("raw-mask-shape: missing `" ^ marker ^ "`"))
    [ "load i8, ptr %"; "bitcast i8"; "extractelement"; "insertelement" ];
  let raw_addr_shape = llvm_of "fn f(p addr, n usize) addr { return p + n }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_addr_shape marker) then
        failwith ("raw-addr-shape: missing `" ^ marker ^ "`"))
    [ "ptrtoint ptr"; "add i64"; "inttoptr i64" ];
  if contains raw_addr_shape "getelementptr i8, ptr" then
    failwith "raw-addr-shape: address arithmetic retained a GEP";
  let raw_normalize_shape = llvm_of "fn f(p addr, n u16) addr { return p - n }\n" in
  List.iter
    (fun marker ->
      if not (contains raw_normalize_shape marker) then
        failwith ("raw-normalize-shape: missing `" ^ marker ^ "`"))
    [ "zext i16"; "ptrtoint ptr"; "sub i64"; "inttoptr i64" ];
  let volatile_shapes =
    llvm_of
      "opaque Token\n\
       fn load_bool(p addr) bool { return volatile_load[bool](p) }\n\
       fn load_integer(p addr) u32 { return volatile_load[u32](p) }\n\
       fn load_address(p addr) addr { return volatile_load[addr](p) }\n\
       fn generic_load[T](p addr) T { return volatile_load[T](p) }\n\
       fn check_case_generic(p addr) u32 { return generic_load[u32](p) }\n\
       fn store_handle(p addr, value handle[Token]) void {\n\
       volatile_store[handle[Token]](p, value)\n\
       return\n\
       }\n\
       fn store_bool(p addr, value bool) void {\n\
       volatile_store[bool](p, value)\n\
       return\n\
       }\n\
       fn store_only(p addr) void {\n\
       volatile_store[u32](p, 7)\n\
       return\n\
       }\n"
  in
  List.iter
    (fun marker ->
      if not (contains volatile_shapes marker) then
        failwith ("volatile-shape: missing `" ^ marker ^ "`"))
    [
      "load volatile i8, ptr";
      "load volatile i32, ptr";
      "load volatile ptr, ptr";
      "store volatile i8";
      "store volatile ptr";
      "icmp ne i8";
      "zext i1";
      ", align 1";
    ];
  let volatile_store_only =
    llvm_of "fn store_only(p addr) void {\nvolatile_store[u32](p, 7)\nreturn\n}\n"
  in
  if not (contains volatile_store_only "store volatile i32") then
    failwith "volatile-store-no-read: missing volatile store";
  if contains volatile_store_only "load i32" then
    failwith "volatile-store-no-read: emitted a destination load";
  let volatile_vector_shapes =
    llvm_of
      "fn load_int(p addr) vec[3,u32] { return volatile_load[vec[3,u32]](p) }\n\
       fn store_int(p addr, v vec[3,u32]) void { volatile_store[vec[3,u32]](p, v)\n\
       return }\n\
       fn load_bool(p addr) vec[12,bool] { return volatile_load[vec[12,bool]](p) }\n\
       fn store_bool(p addr, v vec[12,bool]) void { volatile_store[vec[12,bool]](p, v)\n\
       return }\n"
  in
  List.iter
    (fun marker ->
      if not (contains volatile_vector_shapes marker) then
        failwith ("volatile-vector-shape: missing `" ^ marker ^ "`"))
    [
      "load volatile i32";
      "store volatile i32";
      "load volatile i16";
      "store volatile i16";
    ];
  List.iter
    (fun (needle, expected) ->
      if List.length (positions volatile_vector_shapes needle) <> expected then
        failwith ("volatile-vector-shape: wrong count for `" ^ needle ^ "`"))
    [
      ("load volatile i32", 3);
      ("store volatile i32", 3);
      ("load volatile i16", 1);
      ("store volatile i16", 1);
    ];
  let volatile_type_error =
    "volatile access type must be a scalar integer, bool, addr, handle[T], vec[N, \
     integer], or vec[N, bool]"
  in
  semantic_error "volatile-array" volatile_type_error
    "fn f(p addr) u32 { return volatile_load[arr[2,u32]](p) }\n";
  semantic_error "volatile-struct" volatile_type_error
    "struct S { value u32 }\nfn f(p addr) void { volatile_load[S](p)\nreturn }\n";
  semantic_error "volatile-store-expression" "`volatile_store` is statement-only"
    "fn f(p addr) u32 { return volatile_store[u32](p, 1) }\n";
  semantic_error "volatile-load-type-argument"
    "`volatile_load` needs a type argument: `volatile_load[T](p)`"
    "fn f(p addr) u32 { return volatile_load(p) }\n";
  semantic_error "volatile-store-arity" "expects two arguments"
    "fn f(p addr) void {\nvolatile_store[u32](p)\nreturn\n}\n";
  semantic_error "volatile-value-type"
    "bool and integer values use different representations"
    "fn f(p addr) void {\nvolatile_store[u32](p, true)\nreturn\n}\n";

  semantic_accept "place-init-ternary-escape-unknown"
    "struct S { x i64 y i64 }\n\
     fn f(p bool) i64 { s S\n\
     q addr = if p { &s } else { addr_from_bits(0) }\n\
     t S\n\
     copy(t, s)\n\
     return 0 }\n";
  semantic_accept "aggregate-branch-no-else-unknown"
    "struct S { x i64 }\nfn f(p bool) i64 { s S\nif p { s.x = 1 }\nreturn s.x }\n";
  semantic_accept "aggregate-switch-partial-merge-unknown"
    "struct S { x i64 y i64 }\n\
     fn f(n i64) i64 { s S\n\
     switch n {\n\
     case 0: { s.x = 1 }\n\
     default: { s.y = 2 }\n\
     }\n\
     return s.x }\n";
  semantic_accept "aggregate-nested-dynamic-prefix-unknown"
    "fn f(i i64) i64 { a arr[2,arr[2,i64]]\n\
     a[0][0] = 1\n\
     a[0][1] = 2\n\
     return a[0][i] }\n";
  semantic_accept "aggregate-dynamic-write-read-unknown"
    "fn f(i i64) i64 { a arr[2,i64]\na[i] = 1\nreturn a[i] }\n";
  semantic_accept "aggregate-dynamic-write-preserves-subtree"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     value.values[i] = 7\n\
     return value.values[0] }\n";
  semantic_accept "aggregate-dynamic-compound-preserves-subtree"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     value.values[0] = 3\n\
     value.values[1] = 5\n\
     value.values[i] += 1\n\
     return value.values[0] }\n";
  semantic_accept "aggregate-dynamic-copy-preserves-subtree"
    "struct S { values arr[2,arr[2,u32]] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     source arr[2,u32] = {7, 11}\n\
     copy(value.values[i], source)\n\
     return value.values[0][0] }\n";
  semantic_accept "aggregate-dynamic-nested-loop-write"
    "fn f() u32 { values arr[2,arr[2,u32]]\n\
     for j usize = 0; j < 2; j += 1 { for i usize = 0; i < 2; i += 1 {\n\
     values[j][i] = trunc[u32](j * 2 + i) } }\n\
     return values[1][1] }\n";
  semantic_accept "aggregate-dynamic-pointer-element-unknown"
    "fn f(i i64) i64 { x i64\na arr[2,addr]\na[0] = &x\na[i][i64,0] = 1\nreturn x }\n";
  semantic_accept "aggregate-address-root-remains-unknown"
    "struct S { left u32 right u32 }\n\
     fn f() u32 { value S\n\
     pointer addr = &value\n\
     pointer[S].left = 13\n\
     return value.right }\n";
  semantic_accept "aggregate-address-field-preserves-target"
    "struct S { left u32 right u32 }\n\
     fn f() u32 { value S\n\
     pointer addr = &value.left\n\
     return value.left }\n";
  semantic_accept "aggregate-view-dynamic-write-preserves-subtree"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     view values = value.values\n\
     values[i] = 17\n\
     return value.values[0] }\n";
  semantic_accept "aggregate-branch-partial-unknown"
    "struct S { x i64 y i64 }\n\
     fn f(p bool) i64 { s S\n\
     if p { s.x = 1 } else { s.y = 2 }\n\
     return s.x }\n";
  semantic_accept "aggregate-loop-only-unknown"
    "struct S { x i64 }\nfn f(p bool) i64 { s S\nwhile p { s.x = 1 }\nreturn s.x }\n";
  semantic_accept "aggregate-branch-raw-missing-field-unknown"
    "struct S { x i64 y i64 }\n\
     fn take(p addr) void { return }\n\
     fn f(p bool) i64 { s S\n\
     if p { take(&s) } else { s.x = 1 }\n\
     return s.y }\n";
  semantic_accept "aggregate-branch-raw-whole-unknown"
    "struct S { x i64 y i64 }\n\
     fn take(p addr) void { return }\n\
     fn f(p bool) i64 { s S\n\
     if p { take(&s) } else { s.x = 1 }\n\
     t S\n\
     copy(t, s)\n\
     return 0 }\n";
  semantic_accept "for-step-path-merge-unknown"
    "fn take(value i64) void { return }\n\
     fn f(condition bool) void { for value i64; true; take(value) {\n\
     if condition { value = 1 } else { continue }\n\
     } }\n";
  semantic_accept "conditional-loop-initialization-unknown"
    "fn f(condition bool) i64 { value i64\n\
     while condition { value = 1\n\
     break }\n\
     return value }\n";
  semantic_accept "switch-loop-exit-initialization-unknown"
    "fn f(choice i64) i64 { value i64\n\
     while true { switch choice {\n\
     case 0: { value = 1\n\
     break }\n\
     default: { break }\n\
     } }\n\
     return value }\n";
  semantic_accept "for-step-discard-unknown"
    "fn f() i64 { y i64\nfor i i32 = 0; i < 0; y = 5 { }\nreturn y }\n";
  semantic_accept "static-index-parameter-constant-shadow-unknown"
    "const Index i32 = 0\n\
     fn read(Index i32) i32 { values arr[2,i32]\n\
     values[0] = 7\n\
     return values[Index] }\n\
     fn main() i32 { return read(1) }\n";
  semantic_accept "static-index-nested-parameter-constant-shadow-unknown"
    "const Index i32 = 0\n\
     fn read(Index i32) i32 { values arr[2,i32]\n\
     values[0] = 7\n\
     return values[Index + 0] }\n\
     fn main() i32 { return read(1) }\n";

  semantic_error "aggregate-branch-no-else-established-uninitialized"
    "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\nfn f(p bool) i64 { s S\nif p { s.x = 1 }\nreturn s.y }\n";
  semantic_error "aggregate-dynamic-write-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     value.values[i] = 7\n\
     return value.other }\n";
  semantic_error "aggregate-dynamic-compound-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     value.values[0] = 3\n\
     value.values[1] = 5\n\
     value.values[i] += 1\n\
     return value.other }\n";
  semantic_error "aggregate-dynamic-copy-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { values arr[2,arr[2,u32]] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     source arr[2,u32] = {7, 11}\n\
     copy(value.values[i], source)\n\
     return value.other }\n";
  semantic_error "aggregate-address-field-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { left u32 right u32 }\n\
     fn f() u32 { value S\n\
     pointer addr = &value.left\n\
     return value.right }\n";
  semantic_error "aggregate-view-dynamic-write-preserves-sibling"
    "use of uninitialized local `value`"
    "struct S { values arr[2,u32] other u32 }\n\
     fn f(i usize) u32 { value S\n\
     view values = value.values\n\
     values[i] = 17\n\
     return value.other }\n";
  semantic_error "aggregate-switch-established-uninitialized"
    "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f(n i64) i64 { s S\n\
     switch n { case 0: { s.x = 1 } default: { s.x = 2 } }\n\
     return s.y }\n";
  semantic_error "aggregate-loop-established-uninitialized"
    "use of uninitialized local `s`"
    "struct S { x i64 y i64 }\n\
     fn f(p bool) i64 { s S\n\
     while p { s.x = 1 }\n\
     return s.y }\n";

  semantic_accept "constant-if-true-flow-pruning"
    "fn f() i64 { x i64\nif true { x = 1 } else { }\nreturn x }\n";
  semantic_error "constant-if-true-flow-pruning-rejects-selected-uninit"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nif true { } else { x = 1 }\nreturn x }\n";
  semantic_accept "constant-if-false-flow-pruning"
    "fn f() i64 { x i64\nif false { } else { x = 1 }\nreturn x }\n";
  semantic_error "constant-if-false-flow-pruning-rejects-selected-uninit"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nif false { x = 1 } else { }\nreturn x }\n";
  semantic_accept "constant-while-false-flow-pruning"
    "fn f() i64 { x i64 = 1\nwhile false { x = 2 }\nreturn x }\n";
  semantic_error "constant-while-false-flow-pruning-rejects-body-only-init"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nwhile false { x = 1 }\nreturn x }\n";
  semantic_accept "constant-for-false-flow-pruning"
    "fn f() i64 { x i64 = 1\nfor i i32 = 0; false; i += 1 { x = 2 }\nreturn x }\n";
  semantic_error "constant-for-false-flow-pruning-rejects-body-only-init"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nfor i i32 = 0; false; i += 1 { x = 1 }\nreturn x }\n";
  semantic_accept "constant-switch-matching-case-flow-pruning"
    "fn f() i64 { x i64\nswitch 1 { case 1: { x = 1 } default: { } }\nreturn x }\n";
  semantic_error "constant-switch-matching-case-flow-pruning-rejects-default"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nswitch 1 { case 1: { } default: { x = 1 } }\nreturn x }\n";
  semantic_accept "constant-switch-default-flow-pruning"
    "fn f() i64 { x i64\nswitch 9 { case 1: { } default: { x = 1 } }\nreturn x }\n";
  semantic_error "constant-switch-default-flow-pruning-rejects-case"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nswitch 9 { case 1: { x = 1 } default: { } }\nreturn x }\n";
  semantic_accept "constant-switch-no-match-flow-pruning-preserves-fact"
    "fn f() i64 { x i64 = 7\nswitch 9 { case 1: { x = 1 } }\nreturn x }\n";
  semantic_error "constant-switch-no-match-flow-pruning"
    "use of uninitialized local `x`"
    "fn f() i64 { x i64\nswitch 9 { case 1: { x = 1 } }\nreturn x }\n";
  semantic_accept "constant-and-short-circuit-flow-pruning"
    "fn f() bool { return false && (1 / 0 == 0) }\n";
  semantic_error "constant-and-short-circuit-reaches-rhs"
    "division by zero is not a defined runtime operation"
    "fn f() bool { return true && (1 / 0 == 0) }\n";
  semantic_accept "constant-or-short-circuit-flow-pruning"
    "fn f() bool { return true || (1 / 0 == 0) }\n";
  semantic_error "constant-or-short-circuit-reaches-rhs"
    "division by zero is not a defined runtime operation"
    "fn f() bool { return false || (1 / 0 == 0) }\n";

  semantic_accept "copy-local-struct-array-facts"
    "struct Leaf { value i32 }\n\
     struct Outer { entries arr[2,Leaf] }\n\
     fn f() i32 { source Outer\n\
     source.entries[0].value = 11\n\
     source.entries[1].value = 12\n\
     destination Outer\n\
     copy(destination, source)\n\
     copy(destination, destination)\n\
     return destination.entries[1].value }\n";
  semantic_accept "copy-field-element-place"
    "struct Cell { value i64 }\n\
     struct Box { items arr[2,Cell] }\n\
     fn f() i64 { source Box\n\
     source.items[0].value = 19\n\
     destination Box\n\
     copy(destination.items[0], source.items[0])\n\
     return destination.items[0].value }\n";
  semantic_accept "copy-view-places"
    "struct S { value i64 }\n\
     fn f() i64 { source S\n\
     source.value = 23\n\
     destination S\n\
     view source_view = source\n\
     view destination_view = destination\n\
     copy(destination_view, source_view)\n\
     return destination.value }\n";
  semantic_accept "copy-raw-struct-and-array-places"
    "struct S { value i64 }\n\
     fn f(destination addr, source addr) void {\n\
     copy(destination[S], source[S])\n\
     copy(destination[arr[2,u32]], source[arr[2,u32]])\n\
     return }\n";
  semantic_accept "copy-constant-array-source"
    "const Values arr[2,u32] = {7, 9}\n\
     fn f() u32 { destination arr[2,u32]\n\
     copy(destination, Values)\n\
     return destination[1] }\n";
  semantic_accept "copy-unknown-raw-source-marks-destination-unknown"
    "struct S { x i64 y i64 }\n\
     fn f(source addr) i64 { destination S\n\
     copy(destination, source[S])\n\
     return destination.y }\n";
  semantic_accept "copy-transfers-unknown-local-facts"
    "struct S { x i64 y i64 }\n\
     fn forget(p addr) void { return }\n\
     fn f() i64 { source S\n\
     source.x = 1\n\
     forget(&source)\n\
     destination S\n\
     copy(destination, source)\n\
     return destination.y }\n";
  semantic_error "copy-expression-is-statement-only" "copy is statement-only"
    "fn f() i32 { return copy(1, 2) }\n";
  semantic_error "copy-generic-form-rejected" "`copy` takes no type arguments"
    "fn f(destination addr, source addr) void {\n\
     copy[arr[2,u8]](destination, source)\n\
     return }\n";
  semantic_error "copy-non-aggregate-operands"
    "argument 1 of `copy` has type `i32`, expected an array or struct"
    "fn f() void { destination i32 = 1\n\
     source i32 = 2\n\
     copy(destination, source)\n\
     return }\n";
  semantic_error "copy-vector-operands"
    "argument 1 of `copy` has type `vec[2, u32]`, expected an array or struct"
    "fn f() void { destination vec[2,u32] = splat(1)\n\
     source vec[2,u32] = splat(2)\n\
     copy(destination, source)\n\
     return }\n";
  semantic_error "copy-mismatched-aggregate-types"
    "argument 2 of `copy` has type `arr[3, u32]`, expected `arr[2, u32]` to match \
     argument 1"
    "fn f() void { destination arr[2,u32]\n\
     source arr[3,u32]\n\
     copy(destination, source)\n\
     return }\n";
  let copy_ternary_destination_source =
    "struct S { value i64 }\n\
     fn f(condition bool) void { source S = {1}\n\
     copy(if condition { source } else { source }, source)\n\
     return }\n"
  in
  let copy_ternary_destination_line =
    "copy(if condition { source } else { source }, source)"
  in
  semantic_pin "copy-rvalue-destination" copy_ternary_destination_source 3
    (String.index copy_ternary_destination_line '(' + 2)
    (String.length "if condition { source } else { source")
    "argument 1 of `copy` cannot be an `if` expression; name the array or struct" None;
  let copy_ternary_source_source =
    "struct S { value i64 }\n\
     fn f(condition bool) void { destination S = {1}\n\
     copy(destination, if condition { destination } else { destination })\n\
     return }\n"
  in
  let copy_ternary_source_line =
    "copy(destination, if condition { destination } else { destination })"
  in
  semantic_pin "copy-rvalue-source" copy_ternary_source_source 3
    (String.index copy_ternary_source_line ',' + 3)
    (String.length "if condition { destination } else { destination")
    "argument 2 of `copy` cannot be an `if` expression; name the array or struct" None;
  semantic_error "copy-constant-destination" "cannot modify constant"
    "const Values arr[2,u32] = {7, 9}\n\
     fn f() void { source arr[2,u32]\n\
     copy(Values, source)\n\
     return }\n";
  semantic_error "copy-readonly-destination" "cannot modify string literal `c\"ab\"`"
    "fn f(source addr) void {\ncopy(c\"ab\"[arr[2,u8]], source[arr[2,u8]])\nreturn }\n";
  semantic_error "copy-uninitialized-source" "use of uninitialized local `source`"
    "struct S { value i64 }\n\
     fn f() void { destination S\n\
     source S\n\
     copy(destination, source)\n\
     return }\n";

  semantic_accept "construction-struct-brace-initializer"
    "struct Pair { left i32 right bool }\n\
     fn f() i32 { value Pair = {13, true}\n\
     if value.right { return value.left } else { return 0 } }\n";
  semantic_accept "construction-explicit-struct-initializer"
    "struct Pair { left i32 right i32 }\n\
     fn f() i32 { value Pair = {5, 8}\n\
     return value.left + value.right }\n";
  semantic_accept "construction-array-initializer"
    "fn f() i32 { values arr[3,i32] = {2, 3, 5}\n\
     return values[0] + values[1] + values[2] }\n";
  semantic_accept "construction-nested-array-of-struct"
    "struct Cell { x i32 y i32 }\n\
     struct Board { cells arr[2,Cell] }\n\
     fn f() i32 { board Board = {{{7, 11}, {13, 17}}}\n\
     return board.cells[1].y }\n";
  semantic_accept "construction-explicit-nested-entry"
    "struct Cell { x i32 y i32 }\n\
     struct Board { cell Cell }\n\
     fn f() i32 { board Board = {{19, 23}}\n\
     return board.cell.y }\n";
  semantic_accept "construction-explicit-array-initializer"
    "fn f() i32 { values arr[2,i32] = {19, 23}\nreturn values[1] }\n";
  semantic_accept "construction-vector-literal-shuffle-selector"
    "const indices vec[4,u32] = {3, 2, 1, 0}\n\
     fn f() u32 { a vec[4,u32] = {11, 13, 17, 19}\n\
     b vec[4,u32] = shuffle(a, a, indices)\n\
     return b[0] + b[1] + b[2] + b[3] }\n";
  semantic_accept "construction-local-facts-are-initialized"
    "struct S { left i32 right i32 }\n\
     fn f() i32 { value S = {29, 31}\n\
     return value.left + value.right }\n";
  semantic_accept "construction-empty-array-zero"
    "fn f() u8 { values arr[5,u8] = {}\nreturn values[4] }\n";
  semantic_accept "construction-empty-nested-zero"
    "fn f() i32 { values arr[2,arr[2,i32]] = {{}, {1, 2}}\n\
     return values[0][1] + values[1][1] }\n";
  semantic_error "construction-empty-scalar"
    "initializer needs an array, struct, or vector type, got `i32`"
    "fn f() void { value i32 = {}\nreturn }\n";
  List.iter
    (fun (name, text) ->
      semantic_message name "wrong number of vector literal lanes" text)
    [
      ( "construction-empty-vector-local",
        "fn f() void { value vec[3,f32] = {}\nreturn }\n" );
      ("construction-empty-vector-constant", "const V vec[3,f32] = {}\n");
      ( "construction-empty-vector-field-local",
        "struct V { n f32 value vec[3,f32] }\n\
         fn f() void { value V = {1.0, {}}\n\
         return }\n" );
      ( "construction-empty-vector-field-constant",
        "struct V { n f32 value vec[3,f32] }\nconst X V = {1.0, {}}\n" );
      ( "construction-empty-vector-designated-local",
        "struct V { value vec[3,f32] }\n\
         fn f() void { value V = {.value = {}}\n\
         return }\n" );
      ( "construction-empty-vector-designated-constant",
        "struct V { value vec[3,f32] }\nconst X V = {.value = {}}\n" );
      ( "construction-empty-vector-array-local",
        "fn f() void { value arr[2,vec[3,f32]] = {{}, {1.0, 2.0, 3.0}}\nreturn }\n" );
      ( "construction-empty-vector-array-constant",
        "const X arr[2,vec[3,f32]] = {{}, {1.0, 2.0, 3.0}}\n" );
      ( "construction-empty-vector-whole-zero-local",
        "struct V { n u8 value vec[3,f32] }\n\
         fn f() void { value V = {0, {}}\n\
         return }\n" );
      ( "construction-empty-vector-whole-zero-constant",
        "struct V { n u8 value vec[3,f32] }\nconst X V = {0, {}}\n" );
    ];
  semantic_error "construction-empty-array-nonzero-count" "array of 5 elements, got 1"
    "fn f() void { values arr[5,u8] = {0}\nreturn }\n";
  let empty_aggregate_llvm =
    llvm_of
      "struct Pair { left i32 right i32 }\n\
       var EmptyGlobal arr[5,u8] = {}\n\
       const EmptyConst arr[5,u8] = {}\n\
       fn f() i32 { scratch arr[8,i32]\n\
       scratch[4] = 71\n\
       chars arr[5,u8] = {}\n\
       nested arr[2,arr[2,i32]] = {{}, {1, 2}}\n\
       pair Pair = {}\n\
       return zext[i32](chars[4]) + nested[0][0] + pair.left }\n"
  in
  if
    (not
       (contains empty_aggregate_llvm
          "@EmptyGlobal = internal global [5 x i8] zeroinitializer"))
    || (not
          (contains empty_aggregate_llvm
             "@EmptyConst = private unnamed_addr constant [5 x i8] zeroinitializer"))
    || not (contains empty_aggregate_llvm "store i8 0")
  then failwith "empty aggregate initializers did not zero every storage byte";
  let empty_exported_global_llvm =
    llvm_of "extern \"C\" { var screens arr[5,addr] = {} }\n"
  in
  if
    not
      (contains empty_exported_global_llvm "@screens = global [40 x i8] zeroinitializer")
  then failwith "empty exported aggregate did not emit a zero definition";
  semantic_accept "construction-vector-value"
    "fn f() u32 { values vec[2,u32] = {37, 41}\nreturn values[0] + values[1] }\n";
  semantic_accept "construction-contextual-vector"
    "fn f() u32 { values vec[4,u32] = {1, 2, 3, 4}\nreturn values[3] }\n";
  semantic_accept "construction-contextual-vector-in-struct"
    "struct S { values vec[2,u32] tag u32 }\n\
     fn f() u32 { value S = {{5, 7}, 11}\n\
     return value.values[1] + value.tag }\n";
  semantic_accept "construction-contextual-array-of-vectors"
    "fn f() u32 { values arr[2,vec[2,u32]] = {{13, 17}, {19, 23}}\n\
     return values[1][0] + values[1][1] }\n";
  semantic_error "construction-array-entry-count" "array of 2 elements, got 1"
    "fn f() void { values arr[2,i32] = {1}\nreturn }\n";
  semantic_error "construction-struct-entry-count" "record `Pair` has 2 fields, got 1"
    "struct Pair { left i32 right i32 }\nfn f() void { value Pair = {1}\nreturn }\n";
  semantic_error "construction-vector-entry-count"
    "wrong number of vector literal lanes"
    "fn f() void { value vec[2,i32] = {1}\nreturn }\n";
  let scalar_literal_destination = "fn f() void { value u32 = {1}\nreturn }\n" in
  semantic_pin "construction-scalar-literal-destination" scalar_literal_destination 1
    (String.index_from scalar_literal_destination
       (String.index scalar_literal_destination '=' + 1)
       '{'
    + 1)
    1 "initializer needs an array, struct, or vector type, got `u32`" (Some "write `1`");
  semantic_accept "construction-scalar-literal-destination-twin"
    "fn f() void { value u32 = 1\nreturn }\n";
  semantic_error "construction-entry-type-mismatch"
    "bool and integer values use different representations"
    "struct S { flag i32 }\nfn f() void { value S = {true}\nreturn }\n";
  semantic_accept "designated-struct-fields"
    "struct Point { x i32 y i32 }\n\
     fn f() i32 { point Point = {.y = 11, .x = 7}\n\
     return point.x + point.y }\n";
  semantic_accept "designated-written-order"
    "struct Point { x i32 y i32 }\n\
     fn value(v i32) i32 { return v }\n\
     fn f() i32 { point Point = {.y = value(11), .x = value(7)}\n\
     return point.x + point.y }\n";
  semantic_accept "designated-nested-struct-and-array"
    "struct Point { x i32 y i32 }\n\
     struct Box { points arr[2,Point] }\n\
     fn f() i32 { box Box = {.points = {{.y = 13}, {.x = 17}}}\n\
     return box.points[0].y + box.points[1].x }\n";
  semantic_accept "designated-positional-twin"
    "struct Point { x i32 y i32 }\n\
     fn f() i32 { point Point = {7, 11}\n\
     return point.x + point.y }\n";
  let designated_unknown_field =
    "struct Point { x i32 }\nfn f() void { point Point = {.xx = 1}\nreturn }\n"
  in
  semantic_pin "designated-unknown-field" designated_unknown_field 2
    (String.index "fn f() void { point Point = {.xx = 1}" '.' + 1)
    1 "record `Point` has no field `xx`" (Some "did you mean `x`?");
  let designated_unknown_near_field =
    "struct P { b u32 }\nfn f() void { value P = {.bb = 1}\nreturn }\n"
  in
  semantic_pin "designated-unknown-near-field" designated_unknown_near_field 2
    (String.index "fn f() void { value P = {.bb = 1}" '.' + 1)
    1 "record `P` has no field `bb`" (Some "did you mean `b`?");
  semantic_accept "designated-unknown-near-field-twin"
    "struct P { b u32 }\nfn f() u32 { value P = {.b = 1}\nreturn value.b }\n";
  let designated_unknown_no_candidate =
    "struct P { b u32 }\nfn f() void { value P = {.zzzz = 1}\nreturn }\n"
  in
  semantic_pin "designated-unknown-no-candidate" designated_unknown_no_candidate 2
    (String.index "fn f() void { value P = {.zzzz = 1}" '.' + 1)
    1 "record `P` has no field `zzzz`" None;
  let designated_duplicate_field =
    "struct Point { x i32 }\nfn f() void { point Point = {.x = 1, .x = 2}\nreturn }\n"
  in
  semantic_pin "designated-duplicate-field" designated_duplicate_field 2
    (List.nth (positions "fn f() void { point Point = {.x = 1, .x = 2}" ".x") 1 + 1)
    1 "field `x` is designated more than once"
    (Some "remove the repeated field designator");
  let designated_mixed_entries =
    "struct Point { x i32 y i32 }\nfn f() void { point Point = {.x = 1, 2}\nreturn }\n"
  in
  semantic_pin "designated-mixed-entries" designated_mixed_entries 2
    (let line = "fn f() void { point Point = {.x = 1, 2}" in
     String.index_from line (String.index line '=' + 1) '{' + 1)
    1 "cannot mix designated and positional entries in one initializer"
    (Some "use only `.field = value` entries");
  let designated_array_field =
    "fn f() void { values arr[2, u8] = {.a = 1}\nreturn }\n"
  in
  semantic_pin "designated-field-on-array" designated_array_field 1
    (String.index designated_array_field '.' + 1)
    1 "field designator `.a` requires a struct type, got `arr[2, u8]`"
    (Some "initialize an array with positional entries `{a, b, ...}`");
  let designated_array_index =
    "fn f() void { values arr[2,i32] = {[0] = 1, 2}\nreturn }\n"
  in
  semantic_pin "designated-index-on-array" designated_array_index 1
    (let line = "fn f() void { values arr[2,i32] = {[0] = 1, 2}" in
     String.index_from line (String.index line '=' + 1) '[' + 1)
    1 "array designators are not supported" (Some "write positional elements");
  let designated_import = c_import_fixture "designated.h" in
  c_semantic_pin "designated-c-unknown-near-field" "did you mean `value`?"
    designated_import
    "use \"C\" \"designated.h\"\n\
     fn f() void { imported FasImported = {.valu = 1}\n\
     return }\n"
    2
    (String.index "fn f() void { imported FasImported = {.valu = 1}" '.' + 1)
    1 "record `FasImported` has no field `valu`";
  c_semantic_accept "designated-c-unknown-near-field-twin" designated_import
    "use \"C\" \"designated.h\"\n\
     fn f() u32 { imported FasImported = {.value = 1}\n\
     return imported.value }\n";
  c_semantic_pin "designated-field-on-union"
    "initialize the union with one positional value" designated_import
    "use \"C\" \"designated.h\"\n\
     fn f() void { choice FasChoice = {.value = 1}\n\
     return }\n"
    2
    (String.index "fn f() void { choice FasChoice = {.value = 1}" '.' + 1)
    1 "designated initializer is not supported for union `FasChoice`";
  let designated_scalar = "fn f() void { value i32 = {.x = 1}\nreturn }\n" in
  semantic_pin "designated-field-on-scalar" designated_scalar 1
    (String.index designated_scalar '.' + 1)
    1 "field designator `.x` requires a struct type, got `i32`" None;
  semantic_pin "designated-field-on-u32"
    "fn f() void { value u32 = {.a = 1}\nreturn }\n" 1
    (String.index "fn f() void { value u32 = {.a = 1}" '.' + 1)
    1 "field designator `.a` requires a struct type, got `u32`" None;
  semantic_accept "designated-field-on-non-struct-twins"
    "fn f() u32 { value u32 = 1\n\
     items arr[2, u8] = {3, 5}\n\
     return value + zext[u32](items[1]) }\n";
  let compound_literal_error =
    "Fas has no compound literals; declare a typed local or `const`"
  in
  List.iter
    (fun (name, text) ->
      let open_index = String.index_from text (String.index text '=' + 1) '(' in
      syntax_pin name text 1 (open_index + 1) 1 compound_literal_error)
    [
      ( "compound-literal-vector",
        "fn f() void { value vec[2,u8] = (vec[2,u8]){1, 2}\n return }\n" );
      ( "compound-literal-array",
        "fn f() void { value arr[2,u8] = (arr[2,u8]){1, 2}\n return }\n" );
      ("compound-literal-struct", "fn f() void { value Pair = (Pair){1}\n return }\n");
      ( "compound-literal-generic-instance",
        "fn f() void { value Box[u8] = (Box[u8]){1}\n return }\n" );
    ];
  semantic_accept "typed-initializer-replacements"
    "struct Pair { value u8 }\n\
     struct Box[T] { value T }\n\
     fn f(peer vec[2,u8]) vec[2,u8] {\n\
     vector vec[2,u8] = {1, 2}\n\
     array arr[2,u8] = {3, 4}\n\
     pair Pair = {5}\n\
     generic Box[u8] = {6}\n\
     return peer + vector }\n";
  semantic_accept "typed-brace-peer-replacement"
    "fn f(value vec[2,u8]) vec[2,u8] { return value + {3, 4} }\n";
  let brace_needs_destination =
    "fn consume(pointer addr) i32 { return 0 }\nfn f() i32 { return consume({43}) }\n"
  in
  semantic_pin "construction-brace-needs-destination" brace_needs_destination 2
    (String.length "fn f() i32 { return consume(" + 1)
    1 "array, struct, or vector initializer needs a destination type"
    (Some "store it in a local and pass `&local`");
  semantic_accept "construction-brace-needs-destination-twin"
    "fn consume(pointer addr) i32 { return 0 }\n\
     fn f() i32 { local arr[1,i32] = {43}\n\
     return consume(&local) }\n";
  semantic_accept "construction-typed-aggregate-destination"
    "struct S { value i32 }\n\
     fn consume(pointer addr) i32 { return 0 }\n\
     fn f() i32 { local S = {47}\n\
     return consume(&local) }\n";
  semantic_error "construction-new-name-not-in-scope" "unknown name `value`"
    "fn f() i32 { value arr[1,i32] = {value[0]}\nreturn 0 }\n";

  let copy_direct_locals =
    lower_of
      "fn f() i64 { source arr[2,i64]\n\
       source[0] = 3\n\
       source[1] = 5\n\
       destination arr[2,i64]\n\
       copy(destination, source)\n\
       return destination[1] }\n"
  in
  let copy_direct_fn =
    List.find (fun (func : Ir.func) -> func.name = "f") copy_direct_locals.Ir.funcs
  in
  let copy_array_allocas =
    List.concat_map
      (fun (block : Ir.block) ->
        List.filter_map
          (function
            | Ir.Alloca (_, Ir.Array (2, Ir.I64), _) -> Some block.id | _ -> None)
          block.instrs)
      copy_direct_fn.blocks
  in
  if
    List.length copy_array_allocas <> 2
    || not (List.for_all (( = ) 0) copy_array_allocas)
  then failwith "copy-distinct-locals: expected two local allocas and no scratch";

  semantic_accept "simd-memory-six-forms"
    "fn ml(p addr, m vec[3,bool], f vec[3,u32]) vec[3,u32] { return \
     masked_load[u32](p, m, f) }\n\
     fn ms(p addr, m vec[3,bool], v vec[3,bool]) void { masked_store[bool](p, m, v)\n\
     return }\n\
     fn ga(p addr, i vec[3,i8], m vec[3,bool], f vec[3,bool]) vec[3,bool] { return \
     gather[bool](p, i, m, f) }\n\
     fn sc(p addr, i vec[3,u64], m vec[3,bool], v vec[3,u16]) void { scatter[u16](p, \
     i, m, v)\n\
     return }\n\
     fn gb(p addr, i vec[3,u64], m vec[3,bool], f vec[3,u8]) vec[3,u8] { return \
     gather_bytes[u8](p, i, m, f) }\n\
     fn sb(p addr, i vec[3,i8], m vec[3,bool], v vec[3,u32]) void { \
     scatter_bytes[u32](p, i, m, v)\n\
     return }\n";
  semantic_accept "simd-memory-generic-specialization"
    "fn load[T](p addr, m vec[3,bool], f vec[3,T]) vec[3,T] { return masked_load[T](p, \
     m, f) }\n\
     fn check_case(p addr, m vec[3,bool], f vec[3,u32]) vec[3,u32] { return \
     load[u32](p, m, f) }\n";
  semantic_accept "simd-memory-generic-store-specialization"
    "fn store[T](p addr, i vec[3,i8], m vec[3,bool], v vec[3,T]) void { \
     scatter_bytes[T](p, i, m, v)\n\
     return }\n\
     fn check_case(p addr, i vec[3,i8], m vec[3,bool], v vec[3,u16]) void { \
     store[u16](p, i, m, v)\n\
     return }\n";
  semantic_error "simd-memory-aggregate-element"
    "`masked_load` needs an integer or bool element type, got `arr[2, u32]`"
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     masked_load[arr[2,u32]](p, m, v) }\n";
  semantic_error "simd-memory-address-element"
    "`masked_load` needs an integer or bool element type, got `addr`"
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     masked_load[addr](p, m, v) }\n";
  semantic_error "simd-memory-vector-element"
    "`masked_load` needs an integer or bool element type, got `vec[2, u32]`"
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     masked_load[vec[2,u32]](p, m, v) }\n";
  semantic_error "simd-memory-handle-element"
    "`gather` needs an integer or bool element type, got `handle[Token]`"
    "opaque Token\n\
     fn f(p addr, i vec[2,i8], m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     gather[handle[Token]](p, i, m, v) }\n";
  semantic_error "simd-memory-mask-type" "masked_load mask must be a bool vector"
    "fn f(p addr, m vec[2,u32], v vec[2,u32]) vec[2,u32] { return masked_load[u32](p, \
     m, v) }\n";
  semantic_error "simd-memory-index-type" "gather indices must be an integer vector"
    "fn f(p addr, i vec[2,bool], m vec[2,bool], v vec[2,u32]) vec[2,u32] { return \
     gather[u32](p, i, m, v) }\n";
  semantic_error "simd-memory-lane-count"
    "argument 3 of `gather` is `vec[3,bool]`, expected `vec[2,bool]`"
    "fn f(p addr, i vec[2,i8], m vec[3,bool], v vec[2,u32]) vec[2,u32] { return \
     gather[u32](p, i, m, v) }\n";
  semantic_error "simd-memory-fallback-type"
    "argument 3 of `masked_load` is `vec[3,u32]`, expected `vec[2,u32]`"
    "fn f(p addr, m vec[2,bool], v vec[3,u32]) vec[2,u32] { return masked_load[u32](p, \
     m, v) }\n";
  semantic_error "simd-memory-base-type"
    "cannot pass `u32` as `addr`: integers convert to addresses with `addr_from_bits`"
    "fn f(p u32, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return masked_load[u32](p, \
     m, v) }\n";
  semantic_error "simd-memory-load-arity" "builtin `masked_load` expects 3 arguments"
    "fn f(p addr, m vec[2,bool]) vec[2,u32] { return masked_load[u32](p, m) }\n";
  semantic_error "simd-memory-gather-arity" "builtin `gather` expects 4 arguments"
    "fn f(p addr, i vec[2,i8], m vec[2,bool]) vec[2,u32] { return gather[u32](p, i, m) }\n";
  semantic_error "simd-memory-byte-gather-arity"
    "builtin `gather_bytes` expects 4 arguments"
    "fn f(p addr, i vec[2,i8], m vec[2,bool]) vec[2,u32] { return gather_bytes[u32](p, \
     i, m) }\n";
  semantic_error "simd-memory-store-arity" "builtin `masked_store` expects 3 arguments"
    "fn f(p addr, m vec[2,bool]) void { masked_store[u32](p, m)\nreturn }\n";
  semantic_error "simd-memory-scatter-arity" "builtin `scatter` expects 4 arguments"
    "fn f(p addr, i vec[2,i8], m vec[2,bool]) void { scatter[u32](p, i, m)\nreturn }\n";
  semantic_error "simd-memory-byte-scatter-arity"
    "builtin `scatter_bytes` expects 4 arguments"
    "fn f(p addr, i vec[2,i8], m vec[2,bool]) void { scatter_bytes[u32](p, i, m)\n\
     return }\n";
  semantic_error "simd-memory-missing-type"
    "`masked_load` needs a type argument: `masked_load[T](p, m, fallback)`"
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) vec[2,u32] { return masked_load(p, m, \
     v) }\n";
  semantic_error "simd-memory-store-missing-type"
    "`masked_store` needs a type argument: `masked_store[T](p, m, values)`"
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) void { masked_store(p, m, v)\nreturn }\n";
  semantic_error "simd-memory-masked-store-expression" "masked_store is statement-only"
    "fn f(p addr, m vec[2,bool], v vec[2,u32]) u32 { return masked_store[u32](p, m, v) }\n";
  semantic_error "simd-memory-scatter-expression" "scatter is statement-only"
    "fn f(p addr, i vec[2,i8], m vec[2,bool], v vec[2,u32]) u32 { return \
     scatter[u32](p, i, m, v) }\n";
  semantic_error "simd-memory-scatter-bytes-expression"
    "scatter_bytes is statement-only"
    "fn f(p addr, i vec[2,i8], m vec[2,bool], v vec[2,u32]) u32 { return \
     scatter_bytes[u32](p, i, m, v) }\n";
  semantic_error "simd-memory-store-constant" "cannot modify constant"
    "const C arr[2,u32] = {3, 5}\n\
     fn f(m vec[2,bool], v vec[2,u32]) void { masked_store[u32](&C, m, v)\n\
     return }\n";
  semantic_error "simd-memory-store-readonly" "cannot modify string literal `c\"data\"`"
    "fn f(m vec[4,bool], v vec[4,u8]) void { masked_store[u8](c\"data\", m, v)\n\
     return }\n";
  semantic_error "simd-memory-constant-context"
    "expression is not compile-time constant"
    "const M vec[1,bool] = {true}\n\
     const F vec[1,u32] = {0}\n\
     const X u32 = masked_load[u32](null, M, F)[0]\n";
  semantic_error "simd-memory-gather-constant-context"
    "expression is not compile-time constant"
    "const I vec[1,i8] = {0}\n\
     const M vec[1,bool] = {true}\n\
     const F vec[1,u32] = {0}\n\
     const X u32 = gather[u32](null, I, M, F)[0]\n";
  semantic_error "simd-memory-gather-bytes-constant-context"
    "expression is not compile-time constant"
    "const I vec[1,i8] = {0}\n\
     const M vec[1,bool] = {true}\n\
     const F vec[1,u32] = {0}\n\
     const X u32 = gather_bytes[u32](null, I, M, F)[0]\n";

  semantic_accept "global-declaration-forms"
    "struct Pair { x i32\n\
     y i32 }\n\
     var Zero i32\n\
     var Number i32 = 7\n\
     var Values arr[2,i32] = {3, 5}\n\
     var Item Pair = {11, 13}\n\
     extern \"C\" {\n\
     var Exported i32 = 17\n\
     var Imported i32\n\
     }\n\
     fn update() i32 {\n\
     Zero = Number\n\
     Zero += Values[0]\n\
     Item.y = Zero\n\
     return Imported + Exported + Item.y}\n";
  semantic_accept "global-address-place"
    "var Number i32 = 7\nfn address() addr { return &Number }\n";
  semantic_accept "global-inner-shadow"
    "var Value i32 = 7\nfn local() i32 { Value i32 = 11\n return Value }\n";
  semantic_accept "global-handle-type"
    "opaque Token\n\
     var Current handle[Token] = null\n\
     fn current_token() handle[Token] { return Current }\n";
  let cross_file_global =
    check_files
      [
        ("globals.fas", "var Shared i32 = 19\n");
        ("reader.fas", "fn read() i32 { return Shared }\n");
      ]
  in
  ignore cross_file_global;
  semantic_message "global-duplicate" "duplicate global `Value`"
    "var Value i32\nvar Value u32\n";
  semantic_message "global-function-name-collision" "duplicate declaration `Value`"
    "var Value i32\nfn Value() i32 { return 0 }\n";
  parse_message "global-reserved-var" "expected identifier, found `var`" "var var i32\n";
  syntax_pin "keyword-identifier-caret" "var var i32\n" 1 5 3
    "expected identifier, found `var`";
  parse_message "native-struct-keyword-field-rejected" "expected identifier, found `fn`"
    "struct S { fn i32 }\n";
  semantic_message "global-initializer-not-constant"
    "global initializer must be a constant expression for `Value`; `dynamic()` is not \
     constant"
    "fn dynamic() i32 { return 1 }\nvar Value i32 = dynamic()\n";
  semantic_message "global-initializer-global-read" "global `Source` is not a constant"
    "var Source i32 = 1\nvar Value i32 = Source\n";
  semantic_accept "global-address-initializer"
    "var Target u32\nvar Pointer addr = &Target\n";
  semantic_accept "address-constants-forward-cycle" "var A addr = &B\nvar B addr = &A\n";
  semantic_accept "address-constants-c-string-table"
    "const Names arr[2,addr] = {c\"TROO\", c\"SARG\"}\n\
     fn get() addr { return Names[1] }\n";
  semantic_accept "address-constants-nested-selectors"
    "struct Pair { x i32\n\
     y arr[3,i32] }\n\
     var G Pair\n\
     const T arr[1,arr[2,addr]] = {{&G.x, &G.y[1+1]}}\n";
  semantic_accept "address-constants-forward-const-target"
    "const Links arr[1,addr] = {&Later[0]}\nconst Later arr[1,i32] = {7}\n";
  semantic_accept "address-constants-imported-target"
    "extern \"C\" { var Imported i32 }\nvar P addr = &Imported\n";
  semantic_accept "address-constants-handle-slot"
    "opaque Token\nvar G i32\nvar P handle[Token] = handle_from_addr[Token](&G)\n";
  semantic_accept "address-constants-null-table"
    "const P arr[1,addr] = {null}\nfn f() addr { return P[0] }\n";
  semantic_accept "address-constants-nested-struct-table"
    "struct Link { p addr }\n\
     struct Menu { n i32\n\
     link Link }\n\
     var G i32\n\
     const M arr[1,Menu] = {{7,{&G}}}\n\
     fn f() addr { return M[0].link.p }\n";
  semantic_accept "address-constants-specialization"
    "var G i32\n\
     const P arr[1,addr] = {&G}\n\
     fn get[T](x T) addr { return P[0] }\n\
     fn f() addr { return get[i32](0) }\n";
  semantic_accept "address-constants-c-string-length-context"
    "const N usize = len(c\"abc\")\nfn f() usize { local arr[N,i32]\nreturn N }\n";
  semantic_accept "address-constants-length-query-specialization"
    "var G i32\n\
     const P arr[2,addr] = {&G,null}\n\
     const N usize = len(P)\n\
     const A arr[1,usize] = {len(P)}\n\
     fn get[T](x T) i32 { switch N { case len(P): return 7 }\n\
     return 1 }\n\
     fn f() i32 { return get[i32](0) }\n";
  let address_length =
    llvm_of
      "var G i32\n\
       const P arr[2,addr] = {&G,null}\n\
       const N usize = len(P)\n\
       fn f() usize { return N }\n"
  in
  if not (contains address_length "ret i64 2") then
    failwith "address-constants-length-query: incorrect fixed length";
  semantic_message "address-constants-oob"
    "array index `2` is out of bounds for length 2"
    "var G arr[2,i32]\nvar P addr = &G[2]\n";
  semantic_message "address-constants-scalar-slot"
    "global initializer must be a constant expression for `P`; `&G` is not constant"
    "var G i32\nvar P i32 = &G\n";
  semantic_pin "address-constants-arithmetic" "var G arr[2,i32]\nvar P addr = &G + 1\n"
    2 14 1 "address constant `&G` cannot be used in arithmetic" (Some "write `&G[k]`");
  semantic_pin "address-constants-struct-arithmetic"
    "struct Point { x i32 }\nvar G Point\nvar P addr = &G + 1\n" 3 14 1
    "address constant `&G` cannot be used in arithmetic" (Some "write `&G.x`");
  semantic_pin "address-constants-multifield-arithmetic"
    "struct Point { x i32 y i32 }\nvar G Point\nvar P addr = &G + 1\n" 3 14 1
    "address constant `&G` cannot be used in arithmetic" None;
  semantic_pin "address-constants-scalar-arithmetic" "var G i32\nvar P addr = &G + 1\n"
    2 14 1 "address constant `&G` cannot be used in arithmetic" None;
  semantic_accept "address-constants-arithmetic-twin"
    "var G arr[2,i32]\nvar P addr = &G[1]\n";
  semantic_accept "address-constants-struct-arithmetic-twin"
    "struct Point { x i32 }\nvar G Point\nvar P addr = &G.x\n";
  semantic_message "address-constants-comparison"
    "global initializer must be a constant expression for `P`; `&G == &G` is not \
     constant"
    "var G i32\nconst P bool = &G == &G\n";
  semantic_message "address-constants-integer-conversion"
    "global initializer must be a constant expression for `P`; `zext[usize](&G)` is \
     not constant"
    "var G i32\nconst P usize = zext[usize](&G)\n";
  semantic_message "address-constants-bitcast"
    "global initializer must be a constant expression for `P`; `bitcast[usize](&G)` is \
     not constant"
    "var G i32\nconst P usize = bitcast[usize](&G)\n";
  semantic_message "address-constants-array-length"
    "array length must be an integer constant"
    "var G i32\nconst P addr = &G\nvar A arr[P,i32]\n";
  semantic_message "address-constants-switch-case"
    "address constant `P` cannot be used as a case label"
    "var G i32\n\
     const P addr = &G\n\
     fn f() i32 { switch 0 { case P: return 1 }\n\
     return 0 }\n";
  parse_message "address-constants-sizeof-value" "expected a type, found `&`"
    "var G i32\nconst P usize = sizeof[&G]\n";
  semantic_pin "address-constants-table-copy"
    "var G i32\nconst P arr[1,addr] = {&G}\nconst Q arr[1,addr] = {P[0]}\n" 3 24 4
    "`P[0]` is not an address constant; write `&G`, `&G.field`, `&G[k]`, `c\"...\"` or \
     `null`"
    None;
  semantic_pin "address-constants-read-address-constant"
    "var G i32\nconst A addr = &G\nconst B addr = A\n" 3 16 1
    "`A` is an address constant; another constant initializer cannot read it"
    (Some "write `&G`");
  semantic_pin "address-constants-read-address-constant-var"
    "var G i32\nconst A addr = &G\nvar B addr = A\n" 3 14 1
    "`A` is an address constant; another constant initializer cannot read it"
    (Some "write `&G`");
  semantic_pin "address-constants-read-address-field"
    "struct Point { x i32 y i32 }\nvar G Point\nconst A addr = &G.y\nconst B addr = A\n"
    4 16 1 "`A` is an address constant; another constant initializer cannot read it"
    (Some "write `&G.y`");
  semantic_pin "address-constants-read-address-element"
    "var G arr[2,i32]\nconst A addr = &G[1]\nconst B addr = A\n" 3 16 1
    "`A` is an address constant; another constant initializer cannot read it"
    (Some "write `&G[1]`");
  semantic_pin "address-constants-read-address-function"
    "fn f() void { return }\nconst A addr = &f\nconst B addr = A\n" 3 16 1
    "`A` is an address constant; another constant initializer cannot read it"
    (Some "write `&f`");
  semantic_pin "address-constants-read-address-table-element"
    "var G i32\nconst A addr = &G\nconst Q arr[1,addr] = {A}\n" 3 24 1
    "`A` is an address constant; another constant initializer cannot read it"
    (Some "write `&G`");
  semantic_pin "address-constants-read-address-arithmetic"
    "var G i32\nconst A addr = &G\nconst B addr = A + 1\n" 3 18 1
    "address constants can only be stored in `addr` or `handle[T]` slots" None;
  semantic_accept "address-constants-read-address-help-applied"
    "var G i32\nconst B addr = &G\n";
  semantic_accept "address-constants-read-address-field-help-applied"
    "struct Point { x i32 y i32 }\nvar G Point\nconst B addr = &G.y\n";
  semantic_accept "address-constants-read-address-element-help-applied"
    "var G arr[2,i32]\nconst B addr = &G[1]\n";
  semantic_accept "address-constants-read-address-function-help-applied"
    "fn f() void { return }\nconst B addr = &f\n";
  semantic_accept "address-constants-function-target"
    "fn f() void { return }\nvar P addr = &f\n";
  semantic_accept "address-constants-native-function-local-address"
    "fn f() void { return }\nfn run() void { p addr = &f\nreturn }\n";
  let native_function_table =
    llvm_of
      "struct State { think addr\n\
      \ action addr\n\
      \ next addr }\n\
       fn think() void { return }\n\
       fn action() void { return }\n\
       const states arr[1,State] = {{&think, &action, null}}\n"
  in
  if
    (not (contains native_function_table "@states"))
    || (not (contains native_function_table "ptr @think"))
    || not (contains native_function_table "ptr @action")
  then failwith "native function address table did not emit function relocations";
  semantic_message "address-constants-generic-function-target"
    "generic function `f` has no single address"
    "fn f[T]() void { return }\nvar P addr = &f\n";
  semantic_message "address-constants-generic-function-local-target"
    "generic function `f` has no single address"
    "fn f[T]() void { return }\nfn run() void { p addr = &f\nreturn }\n";
  semantic_accept "call-addr-unknown-callee"
    "fn invoke(p addr) i32 { return call_addr[i32](p, 1) }\n";
  semantic_accept "call-addr-generic-result"
    "fn step(value i32) i32 { return value }\n\
    \     fn invoke[T](p addr, value T) T { return call_addr[T](p, value) }\n\
    \     fn main() i32 { return invoke[i32](&step, 7) }\n";
  semantic_message "call-addr-null-target" "call_addr callee is proven null"
    "fn invalid() i32 { target addr = null\n\
     return call_addr[i32](target) }\n\
    \     fn valid(p addr) i32 { return call_addr[i32](p) }\n";
  semantic_message "call-addr-object-target"
    "call_addr callee is proven not to be a function"
    "var data i32\n\
     fn invalid() i32 { target addr = &data\n\
     return call_addr[i32](target) }\n\
    \     fn valid(p addr) i32 { return call_addr[i32](p) }\n";
  semantic_message "call-addr-string-target"
    "call_addr callee is proven not to be a function"
    "fn invalid() i32 { return call_addr[i32](c\"x\") }\n\
    \     fn valid(p addr) i32 { return call_addr[i32](p) }\n";
  semantic_message "call-addr-argument-count"
    "call_addr calls `step(i32) i32` with 0 arguments"
    "fn step(value i32) i32 { return value }\n\
    \     fn invalid() i32 { target addr = &step\n\
     return call_addr[i32](target) }\n\
    \     fn valid(p addr) i32 { return call_addr[i32](p) }\n";
  semantic_message "call-addr-argument-type"
    "call_addr argument 1 of `step(i32) i32` has type `u32`, expected `i32`"
    "fn step(value i32) i32 { return value }\n\
    \     fn invalid(value u32) i32 { target addr = &step\n\
     return call_addr[i32](target, value) }\n\
    \     fn valid(p addr, value u32) i32 { return call_addr[i32](p, value) }\n";
  semantic_message "call-addr-result-type"
    "call_addr result `u32` does not match `step(i32) i32`"
    "fn step(value i32) i32 { return value }\n\
    \     fn invalid() u32 { target addr = &step\n\
     return call_addr[u32](target, 1) }\n\
    \     fn valid(p addr) u32 { return call_addr[u32](p, 1) }\n";
  semantic_message "call-addr-variadic-target"
    "call_addr cannot call variadic C function `printf(addr, ...) i32`"
    "extern \"C\" { fn printf(format addr, ...) i32 }\n\
    \     fn invalid(format addr) i32 { target addr = &printf\n\
     return call_addr[i32](target, format) }\n\
    \     fn valid(p addr, format addr) i32 { return call_addr[i32](p, format) }\n";
  semantic_message "call-addr-const-rejection"
    "call_addr cannot be used in constant evaluation"
    "const value i32 = call_addr[i32](null)\n";
  semantic_message "call-addr-no-address" "call_addr is a builtin and has no address"
    "fn invalid() addr { return &call_addr }\n";
  semantic_accept "address-constants-c-function-target"
    "extern \"C\" { fn f() void }\nvar P addr = &f\n";
  semantic_message "address-constants-scalar-constant-target"
    "constant `G` cannot be addressed" "const G i32 = 1\nvar P addr = &G\n";
  semantic_message "address-constants-vector-constant-target"
    "cannot take the address of this expression"
    "const G vec[2,i32] = splat(1)\nvar P addr = &G\n";
  let ordinary_address_string = "var P addr = \"x\"\n" in
  semantic_pin "address-constants-ordinary-string" ordinary_address_string 1
    (String.index ordinary_address_string '"' + 1)
    3 "address constants require a C string literal" (Some "write `c\"x\"`");
  semantic_accept "address-constants-ordinary-string-twin" "var P addr = c\"x\"\n";
  semantic_message "address-constants-local-target" "unknown name `Local`"
    "fn f() void { Local i32 = 1\nreturn }\nvar P addr = &Local\n";
  semantic_message "address-constants-handle-ordinary-string"
    "address constants require a C string literal"
    "opaque Token\nvar P handle[Token] = handle_from_addr[Token](\"x\")\n";
  semantic_message "address-constants-readonly-table" "cannot modify constant `P`"
    "var G i32\nconst P arr[1,addr] = {&G}\nfn f() void { P[0] = null\nreturn }\n";
  let relocatable =
    llvm_of
      "struct Pair { x i32\n\
       p addr\n\
       y i32 }\n\
       var G arr[3,i32]\n\
       var T Pair = {7,&G[1+1],9}\n"
  in
  if
    (not (contains relocatable "@T = internal global <{ [8 x i8], ptr, [8 x i8] }>"))
    || (not (contains relocatable "getelementptr (i8, ptr @G, i64 8)"))
    || contains relocatable "inbounds"
  then failwith "address-constants-mixed-layout: invalid relocation or layout";
  semantic_message "const-global-read" "global `Value` is not a constant"
    "var Value i32 = 1\nconst Copy i32 = Value\n";
  semantic_message "global-array-length-read" "global `Count` is not a constant"
    "var Count usize = 2\nfn run() void { local arr[Count,i32]\nreturn }\n";
  semantic_message "global-generic-argument-read" "global `Count` is not a constant"
    "var Count i32 = 2\n\
     fn get[N const i32]() i32 { return N }\n\
     fn run() i32 { return get[Count]() }\n";
  semantic_message "global-case-label-read" "global `Choice` is not a constant"
    "var Choice i32 = 2\n\
     fn run() i32 { switch 0 { case Choice: return 1 }\n\
     return 0 }\n";
  semantic_message "global-array-initializer-arity" "array of 2 elements, got 1"
    "var Values arr[2,i32] = {1}\n";
  semantic_message "global-struct-initializer-arity" "record `Pair` has 2 fields, got 1"
    "struct Pair { x i32\ny i32 }\nvar Item Pair = {1}\n";
  semantic_message "constant-global-array-initializer-arity"
    "array of 3 elements, got 2" "const Values arr[3,i32] = {1, 2}\n";
  semantic_message "constant-global-record-initializer-arity"
    "record `Pair` has 1 field, got 2"
    "struct Pair { x i32 }\nconst Item Pair = {1, 2}\n";
  semantic_message "global-opaque-object"
    "opaque type `Token` can only be held as `handle[Token]`"
    "opaque Token\nvar Value Token\n";
  parse_message "global-local-var"
    "`var` declares globals; locals are declared as `name Type = value`"
    "fn run() void { var Value i32\nreturn }\n";

  let c_matrix = c_import_fixture "matrix.h" in
  let imported_c_widening =
    "fn probe(value i64) u32 { return fas_u32_echo(value) }\n"
  in
  c_semantic_pin "widening-imported-c-function-argument"
    "write `trunc[u32](value)` if intended" c_matrix imported_c_widening 1
    (String.length "fn probe(value i64) u32 { return fas_u32_echo(" + 1)
    (String.length "value") "cannot pass `i64` as `u32`: negative values change meaning";
  c_semantic_accept "widening-imported-c-function-argument-twin" c_matrix
    "fn probe(value u16) u32 { return fas_u32_echo(value) }\n";
  let c_stdlib = c_import_system_fixture "stdlib.h" in
  let c_math = c_import_system_fixture "math.h" in
  let c_select = c_import_system_fixture "sys/select.h" in
  let builtin_collision_headers =
    [
      ("stdio.h", c_import_system_fixture "stdio.h");
      ("stdlib.h", c_stdlib);
      ("string.h", c_import_system_fixture "string.h");
      ("time.h", c_import_system_fixture "time.h");
      ("zlib.h", c_import_system_fixture "zlib.h");
      ("net/if.h", c_import_system_fixture "net/if.h");
      ("math.h", c_math);
      ("sys/select.h", c_select);
      ("limits.h", c_import_system_fixture "limits.h");
      ("stdbool.h", c_import_system_fixture "stdbool.h");
      ("stdint.h", c_import_system_fixture "stdint.h");
      ("sys/stat.h", c_import_system_fixture "sys/stat.h");
    ]
  in
  let builtin_collision_sources =
    [
      ("sqrt", "fn probe(x f64) f64 { return sqrt(x) }\n");
      ("fma", "fn probe(x f64) f64 { return fma(x, x, x) }\n");
      ("floor", "fn probe(x f64) f64 { return floor(x) }\n");
      ("ceil", "fn probe(x f64) f64 { return ceil(x) }\n");
      ("round", "fn probe(x f64) f64 { return round(x) }\n");
      ("trunc", "fn probe(x vec[2,u16]) vec[2,u8] { return trunc[vec[2,u8]](x) }\n");
      ("abs", "fn probe(x f64) f64 { return abs(x) }\n");
      ( "select",
        "fn probe(mask vec[2,bool], yes vec[2,u32], no vec[2,u32]) vec[2,u32] {\n\
         return select(mask, yes, no) }\n" );
      ( "compress",
        "fn probe(values vec[2,u32], mask vec[2,bool]) vec[2,u32] {\n\
         return compress(values, mask) }\n" );
    ]
  in
  List.iter
    (fun (header, imported) ->
      List.iter
        (fun name ->
          if Option.is_some (List.assoc_opt name (snd imported).C_import.unsupported)
          then
            match List.assoc_opt name builtin_collision_sources with
            | Some source ->
                c_semantic_accept
                  ("builtin-c-header-" ^ header ^ "-" ^ name)
                  imported source
            | None when Names.builtin_name name ->
                failwith ("builtin-c-header: missing source for " ^ header ^ " " ^ name)
            | None -> ())
        Names.operation_names)
    builtin_collision_headers;
  c_semantic_message "builtin-c-header-noncall-reserved-name"
    "C declaration `sqrt` is not supported: name is a Fas builtin; call it through a C \
     container function with another name"
    c_math "fn probe() f64 { return sqrt }\n";
  (match c_semantic_result c_stdlib "fn probe(x i32) f64 { return abs(x) }\n" with
  | Error [ diagnostic ]
    when diagnostic.Diag.message = "argument 1 of `abs` is `i32`, expected `f64`"
         && diagnostic.help = Some "write `cvt[f64](labs(x))`" ->
      ()
  | Error diagnostics ->
      failwith
        ("builtin-c-header-abs-int: unexpected diagnostic: "
        ^ Diag.render_all ~source:None diagnostics)
  | Ok _ -> failwith "builtin-c-header-abs-int: accepted an integer operand");
  c_semantic_accept "builtin-c-header-abs-int-help-twin" c_stdlib
    "fn good(x i32) f64 { return cvt[f64](labs(x)) }\n";
  let c_strings = c_import_system_fixtures [ "stdio.h"; "string.h"; "stdlib.h" ] in
  let c_string_types = c_import_fixture "c_string_parameters.h" in
  let c_string_message literal function_name parameter_name =
    Printf.sprintf
      "%s has no NUL terminator, but `%s` reads parameter `%s` as a C string" literal
      function_name parameter_name
  in
  let deferred_string_source body value =
    let defer = Printf.sprintf "defer { q = %s }" value in
    let body = body defer in
    Printf.sprintf "fn probe() void { q addr = \"abc\"\n%s\nputs(q)\nreturn }\n" body
  in
  let deferred_string_bodies =
    [
      ("defer-c-string-scope", fun defer -> Printf.sprintf "{ %s }" defer);
      ( "defer-c-string-break",
        fun defer -> Printf.sprintf "while true { %s\nbreak }" defer );
      ( "defer-c-string-labeled-break",
        fun defer ->
          Printf.sprintf "outer: while true { %s\nwhile true { break outer } }" defer );
      ( "defer-c-string-for-scope",
        fun defer -> Printf.sprintf "for i i32 = 0; i < 1; i += 1 { %s }" defer );
    ]
  in
  List.iter
    (fun (name, body) ->
      c_semantic_accept name c_strings (deferred_string_source body "c\"x\""))
    deferred_string_bodies;
  List.iter
    (fun (name, body) ->
      c_semantic_message name
        (c_string_message "\"x\"" "puts" "__s")
        c_strings
        (deferred_string_source body "\"x\""))
    (List.map
       (fun (name, body) -> (name ^ "-invalid-twin", body))
       deferred_string_bodies);
  c_semantic_pin "c-string-printf" "write c\"x\"" c_strings
    "fn probe() void {\n  printf(\"x\")\n  return\n}\n" 2 10 3
    (c_string_message "\"x\"" "printf" "__format");
  c_semantic_accept "c-string-printf-terminated" c_strings
    "fn probe() void { printf(c\"x\")\nreturn }\n";
  c_semantic_accept "c-string-printf-varargs" c_strings
    "fn probe() void { printf(c\"%s\", \"x\")\nreturn }\n";
  let float_vararg =
    c_semantic_result c_strings
      "fn probe(value f32) void { printf(c\"%f\", value)\nreturn }\n"
  in
  (match float_vararg with
  | Error [ diagnostic ]
    when diagnostic.Diag.message
         = "C varargs do not accept `f32`; convert it to `f64` explicitly"
         && diagnostic.help = Some "write `cvt[f64](value)`" ->
      ()
  | Error diagnostics ->
      failwith
        ("c-float-vararg-help: unexpected diagnostic: "
        ^ Diag.render_all ~source:None diagnostics)
  | Ok _ -> failwith "c-float-vararg-help: expected rejection");
  List.iter
    (fun (name, text, help) ->
      match c_semantic_result c_strings text with
      | Error [ diagnostic ]
        when diagnostic.Diag.message
             = "C varargs do not accept `f32`; convert it to `f64` explicitly"
             && diagnostic.help = Some help ->
          ()
      | Error diagnostics ->
          failwith
            (name ^ ": unexpected diagnostic: "
            ^ Diag.render_all ~source:None diagnostics)
      | Ok _ -> failwith (name ^ ": expected rejection"))
    [
      ( "c-float-vararg-help-builtin",
        "fn probe(a f32) void { printf(c\"%f\", sqrt(a))\nreturn }\n",
        "write `cvt[f64](sqrt(a))`" );
      ( "c-float-vararg-help-if-expression",
        "fn probe(a f32, c bool) void { printf(c\"%f\", if c { a } else { 1.0 })\n\
         return }\n",
        "write `cvt[f64](if c { a } else { 1.0 })`" );
    ];
  c_semantic_accept "c-f64-printf-varargs" c_strings
    "fn probe(value f64) void { printf(c\"%.17g %a\\n\", value, value)\nreturn }\n";
  c_semantic_accept "cvt-f64-printf-varargs" c_strings
    "fn probe(value f32) void { printf(c\"%.17g %a\\n\", cvt[f64](value), \
     cvt[f64](value))\n\
     return }\n";
  c_semantic_accept "c-float-vararg-help-builtin-applied" c_strings
    "fn probe(a f32) void { printf(c\"%f\", cvt[f64](sqrt(a)))\nreturn }\n";
  c_semantic_accept "c-float-vararg-help-if-expression-applied" c_strings
    "fn probe(a f32, c bool) void { printf(c\"%f\", cvt[f64](if c { a } else { 1.0 }))\n\
     return }\n";
  c_semantic_message "c-string-puts"
    (c_string_message "\"x\"" "puts" "__s")
    c_strings "fn probe() void { puts(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-puts-terminated" c_strings
    "fn probe() void { puts(c\"x\")\nreturn }\n";
  let c_string_utf8_source = "fn probe() void { puts(\"\\xc3\\xa9\")\nreturn }\n" in
  (match c_semantic_result c_strings c_string_utf8_source with
  | Error [ diagnostic ]
    when diagnostic.Diag.message
         = "\"\\xc3\\xa9\" has no NUL terminator, but `puts` reads parameter `__s` as \
            a C string"
         && diagnostic.help = Some "write c\"\\xc3\\xa9\"" ->
      ()
  | Error diagnostics ->
      failwith
        ("c-string-utf8-diagnostic: unexpected diagnostic: "
        ^ Diag.render_all ~source:None diagnostics)
  | Ok _ -> failwith "c-string-utf8-diagnostic: expected rejection");
  let c_string_embedded_nul_source =
    "fn probe() void { puts(\"ab\\0cd\" + 3)\nreturn }\n"
  in
  (match c_semantic_result c_strings c_string_embedded_nul_source with
  | Error [ diagnostic ]
    when diagnostic.Diag.message
         = "\"ab\\0cd\" has no NUL terminator, but `puts` reads parameter `__s` as a C \
            string"
         && diagnostic.help = None ->
      ()
  | Error diagnostics ->
      failwith
        ("c-string-embedded-nul-help: unexpected diagnostic: "
        ^ Diag.render_all ~source:None diagnostics)
  | Ok _ -> failwith "c-string-embedded-nul-help: expected rejection");
  c_semantic_accept "c-string-unreachable-call" c_strings
    "fn probe() void { if false { puts(\"x\") }\nreturn }\n";
  c_semantic_message "c-string-strlen"
    (c_string_message "\"x\"" "strlen" "__s")
    c_strings "fn probe() usize { return strlen(\"x\") }\n";
  c_semantic_accept "c-string-strlen-terminated" c_strings
    "fn probe() usize { return strlen(c\"x\") }\n";
  c_semantic_message "c-string-strcmp-first"
    (c_string_message "\"x\"" "strcmp" "__s1")
    c_strings "fn probe() i32 { return strcmp(\"x\", c\"x\") }\n";
  c_semantic_message "c-string-strcmp-second"
    (c_string_message "\"x\"" "strcmp" "__s2")
    c_strings "fn probe() i32 { return strcmp(c\"x\", \"x\") }\n";
  c_semantic_accept "c-string-strcmp-terminated" c_strings
    "fn probe() i32 { return strcmp(c\"x\", c\"x\") }\n";
  c_semantic_message "c-string-fopen-first"
    (c_string_message "\"x\"" "fopen" "__filename")
    c_strings "fn probe() void { fopen(\"x\", c\"r\")\nreturn }\n";
  c_semantic_message "c-string-fopen-second"
    (c_string_message "\"r\"" "fopen" "__modes")
    c_strings "fn probe() void { fopen(c\"x\", \"r\")\nreturn }\n";
  c_semantic_accept "c-string-fopen-terminated" c_strings
    "fn probe() void { fopen(c\"x\", c\"r\")\nreturn }\n";
  c_semantic_message "c-string-getenv"
    (c_string_message "\"x\"" "getenv" "__name")
    c_strings "fn probe() addr { return getenv(\"x\") }\n";
  c_semantic_accept "c-string-getenv-terminated" c_strings
    "fn probe() addr { return getenv(c\"x\") }\n";
  c_semantic_message "c-string-atoi"
    (c_string_message "\"1\"" "atoi" "__nptr")
    c_strings "fn probe() i32 { return atoi(\"1\") }\n";
  c_semantic_accept "c-string-atoi-terminated" c_strings
    "fn probe() i32 { return atoi(c\"1\") }\n";
  c_semantic_accept "c-string-embedded-nul" c_strings
    "fn probe() usize { return strlen(\"ab\\0\") }\n";
  c_semantic_accept "c-string-strncmp-length" c_strings
    "fn probe() i32 { return strncmp(\"abcd\", c\"x\", 3) }\n";
  c_semantic_accept "c-string-strndup-length" c_strings
    "fn probe() addr { return strndup(\"abc\", 2) }\n";
  c_semantic_message "c-string-plain-literal-const"
    "expression is not compile-time constant" c_strings "const S addr = \"x\"\n";
  c_semantic_accept "c-string-c-literal-const" c_strings
    "const S addr = c\"x\"\nfn probe() void { puts(S)\nreturn }\n";
  c_semantic_accept "c-string-memcpy-void-pointer" c_strings
    "fn probe() void { dst addr = addr_from_bits(4096)\n\
     memcpy(dst, \"abc\", 3)\n\
     return }\n";
  c_semantic_message "c-string-named-binding"
    (c_string_message "\"x\"" "puts" "__s")
    c_strings "fn probe() void { S addr = \"x\"\nputs(S)\nreturn }\n";
  c_semantic_accept "c-string-named-binding-terminated" c_strings
    "fn probe() void { S addr = c\"x\"\nputs(S)\nreturn }\n";
  c_semantic_message "c-string-if-expression"
    (c_string_message "\"x\"" "puts" "__s")
    c_strings
    "fn probe(flag bool) void { puts(if flag { \"x\" } else { \"y\" })\nreturn }\n";
  c_semantic_accept "c-string-if-expression-terminated" c_strings
    "fn probe(flag bool) void { puts(if flag { c\"x\" } else { c\"y\" })\nreturn }\n";
  c_semantic_message "c-string-offset"
    (c_string_message "\"abc\"" "puts" "__s")
    c_strings "fn probe() void { puts(\"abc\" + 1)\nreturn }\n";
  c_semantic_accept "c-string-offset-terminated" c_strings
    "fn probe() void { puts(c\"abc\" + 1)\nreturn }\n";
  c_semantic_message "c-string-plain-char-pointer"
    (c_string_message "\"x\"" "fas_take_char" "value")
    c_string_types "fn probe() void { fas_take_char(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-plain-char-pointer-terminated" c_string_types
    "fn probe() void { fas_take_char(c\"x\")\nreturn }\n";
  c_semantic_message "c-string-array-parameter"
    (c_string_message "\"x\"" "fas_take_array" "value")
    c_string_types "fn probe() void { fas_take_array(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-array-parameter-terminated" c_string_types
    "fn probe() void { fas_take_array(c\"x\")\nreturn }\n";
  c_semantic_message "c-string-sized-array-parameter"
    (c_string_message "\"x\"" "fas_take_sized_array" "value")
    c_string_types "fn probe() void { fas_take_sized_array(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-sized-array-parameter-terminated" c_string_types
    "fn probe() void { fas_take_sized_array(c\"x\")\nreturn }\n";
  c_semantic_message "c-string-unnamed-parameter"
    "\"x\" has no NUL terminator, but `fas_take_unnamed` reads parameter 1 as a C \
     string"
    c_string_types "fn probe() void { fas_take_unnamed(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-unnamed-parameter-terminated" c_string_types
    "fn probe() void { fas_take_unnamed(c\"x\")\nreturn }\n";
  c_semantic_message "c-string-char-pointer-typedef"
    (c_string_message "\"x\"" "fas_take_alias" "value")
    c_string_types "fn probe() void { fas_take_alias(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-char-pointer-typedef-terminated" c_string_types
    "fn probe() void { fas_take_alias(c\"x\")\nreturn }\n";
  c_semantic_message "c-string-plain-char-alias-pointer"
    (c_string_message "\"x\"" "fas_take_plain_char" "value")
    c_string_types "fn probe() void { fas_take_plain_char(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-plain-char-alias-pointer-terminated" c_string_types
    "fn probe() void { fas_take_plain_char(c\"x\")\nreturn }\n";
  c_semantic_message "c-string-plain-char-pointer-alias"
    (c_string_message "\"x\"" "fas_take_plain_string" "value")
    c_string_types "fn probe() void { fas_take_plain_string(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-plain-char-pointer-alias-terminated" c_string_types
    "fn probe() void { fas_take_plain_string(c\"x\")\nreturn }\n";
  c_semantic_accept "c-string-char-pointer-pointer" c_string_types
    "fn probe() void { fas_take_char_pointer_pointer(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-signed-char-pointer" c_string_types
    "fn probe() void { fas_take_signed(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-unsigned-char-pointer" c_string_types
    "fn probe() void { fas_take_unsigned(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-signed-char-alias-pointer" c_string_types
    "fn probe() void { fas_take_signed_alias(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-unsigned-char-alias-pointer" c_string_types
    "fn probe() void { fas_take_unsigned_alias(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-uint8-pointer" c_string_types
    "fn probe() void { fas_take_u8(\"x\")\nreturn }\n";
  c_semantic_accept "c-string-unknown-address" c_strings
    "fn probe(p addr) void { puts(p)\nreturn }\n";
  c_semantic_accept "c-string-hand-declaration" c_strings
    "extern \"C\" { fn hand(value addr) i32 }\nfn probe() i32 { return hand(\"x\") }\n";
  let c_alloc_size_variable = c_import_alloc_size_fixture "alloc_size_variable.h" in
  c_semantic_accept "alloc-size-variable-attribute-not-function" c_alloc_size_variable
    "fn probe() void { p addr = plain_alloc(4)\np[u32, 1] = 1\nreturn }\n";
  c_semantic_message "alloc-size-function-after-variable-retains-attribute"
    "access outside object `marked_alloc(4)` (offset 4, size 4 bytes, object size 4)"
    c_alloc_size_variable
    "fn probe() void { p addr = marked_alloc(4)\np[u32, 1] = 1\nreturn }\n";
  c_semantic_accept "alloc-size-typedef-attribute-not-function"
    (c_import_alloc_size_fixture "alloc_size_typedef.h")
    "fn probe() void { p addr = plain_alloc(4)\np[u32, 1] = 1\nreturn }\n";
  c_semantic_accept "alloc-size-field-attribute-not-function"
    (c_import_alloc_size_fixture "alloc_size_field.h")
    "fn probe() void { p addr = plain_alloc(4)\np[u32, 1] = 1\nreturn }\n";
  c_semantic_accept "alloc-size-parameter-attribute-not-function"
    (c_import_alloc_size_fixture "alloc_size_parameter.h")
    "fn probe() void { p addr = wrap(f, 4)\np[u32, 1] = 1\nreturn }\n";
  let _, c_stdlib_alloc_size = c_stdlib in
  let alloc_size_positions name =
    List.assoc_opt name c_stdlib_alloc_size.alloc_size_parameters
  in
  let show_alloc_size_positions = function
    | None -> "missing"
    | Some groups ->
        groups
        |> List.map (fun indices -> String.concat "," (List.map string_of_int indices))
        |> String.concat ";"
  in
  if
    alloc_size_positions "malloc" <> Some [ [ 1 ] ]
    || alloc_size_positions "calloc" <> Some [ [ 1; 2 ] ]
    || alloc_size_positions "realloc" <> Some [ [ 2 ] ]
    || alloc_size_positions "aligned_alloc" <> Some [ [ 2 ] ]
  then
    failwith
      ("c-alloc-size-import-metadata: malloc="
      ^ show_alloc_size_positions (alloc_size_positions "malloc")
      ^ " calloc="
      ^ show_alloc_size_positions (alloc_size_positions "calloc")
      ^ " realloc="
      ^ show_alloc_size_positions (alloc_size_positions "realloc")
      ^ " aligned_alloc="
      ^ show_alloc_size_positions (alloc_size_positions "aligned_alloc"));
  c_semantic_message "alloc-size-malloc-past-end"
    "access outside object `malloc(16)` (offset 16, size 4 bytes, object size 16)"
    c_stdlib "fn probe() void { p addr = malloc(16)\np[u32, 4] = 1\nreturn }\n";
  c_semantic_accept "alloc-size-malloc-last-element" c_stdlib
    "fn probe() void { p addr = malloc(16)\np[u32, 3] = 1\nreturn }\n";
  c_semantic_accept "alloc-size-malloc-view-last-element" c_stdlib
    "fn probe() void { p addr = malloc(16)\nview xs = p[u32, ..]\nxs[3] = 1\nreturn }\n";
  c_semantic_message "alloc-size-malloc-view-past-end"
    "access outside object `malloc(16)` (offset 16, size 4 bytes, object size 16)"
    c_stdlib
    "fn probe() void { p addr = malloc(16)\nview xs = p[u32, ..]\nxs[4] = 1\nreturn }\n";
  c_semantic_pin "narrow-arithmetic-malloc-size-help"
    "widen an operand first: `zext[usize](nres) * 140 + 16`" c_stdlib
    "fn probe(nres u32) addr { return malloc(nres * 140 + 16) }\n" 1 41 15
    "`nres * 140 + 16` is computed in `u32` and may wrap before it reaches `usize`";
  c_semantic_accept "narrow-arithmetic-malloc-size-help-compiles" c_stdlib
    "fn probe(nres u32) addr { return malloc(zext[usize](nres) * 140 + 16) }\n";
  c_semantic_message "alloc-size-calloc-past-end"
    "access outside object `calloc(4, 4)` (offset 16, size 4 bytes, object size 16)"
    c_stdlib "fn probe() void { p addr = calloc(4, 4)\np[u32, 4] = 1\nreturn }\n";
  c_semantic_accept "alloc-size-calloc-last-element" c_stdlib
    "fn probe() void { p addr = calloc(4, 4)\np[u32, 3] = 1\nreturn }\n";
  c_semantic_accept "alloc-size-runtime-unknown" c_stdlib
    "fn probe(n usize) void { p addr = malloc(n)\np[u32, 400] = 1\nreturn }\n";
  c_semantic_accept "alloc-size-realloc-last-element" c_stdlib
    ("fn probe() void { p addr = malloc(16)\n"
   ^ "p = realloc(p, 32)\np[u32, 7] = 1\nreturn }\n");
  c_semantic_message "alloc-size-realloc-past-end"
    "access outside object `realloc(p, 32)` (offset 32, size 4 bytes, object size 32)"
    c_stdlib
    ("fn probe() void { p addr = malloc(16)\n"
   ^ "p = realloc(p, 32)\np[u32, 8] = 1\nreturn }\n");
  c_semantic_accept "alloc-size-aligned-last-element" c_stdlib
    ("fn probe() void { p addr = aligned_alloc(16, 64)\n" ^ "p[u32, 15] = 1\nreturn }\n");
  c_semantic_message "alloc-size-aligned-past-end"
    "access outside object `aligned_alloc(16, 64)` (offset 64, size 4 bytes, object \
     size 64)"
    c_stdlib
    "fn probe() void { p addr = aligned_alloc(16, 64)\np[u32, 16] = 1\nreturn }\n";
  c_semantic_message "alloc-size-null-check"
    "access outside object `malloc(16)` (offset 16, size 4 bytes, object size 16)"
    c_stdlib
    ("fn probe() void { p addr = malloc(16)\n"
   ^ "if p != null { p[u32, 4] = 1 }\nreturn }\n");
  c_semantic_message "alloc-size-may-return-null" "access through null address" c_stdlib
    ("fn probe() void { p addr = malloc(16)\n"
   ^ "if p == null { p[u32, 0] = 1 }\nreturn }\n");
  c_semantic_accept "alloc-size-product-overflow" c_stdlib
    ("fn probe() void { p addr = calloc(18446744073709551615, 2)\n"
   ^ "p[u8, 100] = 1\nreturn }\n");
  incr checks_run;
  let unit_path = "/tmp/fas-c-import-test.c" in
  let probe_path = "/tmp/fas-c-type-probe-test.json" in
  let digest = Digest.to_hex (Digest.string unit_path) in
  let normalized =
    C_import.normalize_import_failure ~unit_path [ probe_path ]
      (Printf.sprintf "%s:8:3: error: %s __fas_type_probe_%s_1" unit_path digest digest)
  in
  if normalized <> "<C import>:8:3: error: <hash> __fas_type_probe_<hash>_1" then
    failwith "c-import-failure-path-normalization: random importer data remained";
  let c_xmmintrin = c_import_system_fixture "xmmintrin.h" in
  ignore (c_import_system_fixture "emmintrin.h");
  c_semantic_message "c-builtin-function-unsupported"
    "C declaration `_mm_getcsr` is not supported: Clang builtin function" c_xmmintrin
    "fn probe() u32 { return _mm_getcsr() }\n";
  ignore (c_import_system_fixture "immintrin.h");
  let c_nonnull_source, _ = c_import_fixture "nonnull.h" in
  let c_nonnull_declarations, _, _ =
    expect_ok
      (C_import.import ~cc:"clang-22" ~keep:false c_nonnull_source
         [
           C_import.{ spelling = Ast.C_quoted "nonnull.h"; span = Span.synthetic };
           C_import.{ spelling = Ast.C_system "string.h"; span = Span.synthetic };
         ])
  in
  let c_nonnull =
    ( c_nonnull_source,
      C_import.map_declarations ~span:Span.synthetic c_nonnull_declarations )
  in
  c_semantic_message "c-nonnull-strlen-null"
    "null argument to nonnull parameter 1 of `strlen`" c_nonnull
    "fn probe() usize { return strlen(null) }\n";
  c_semantic_message "c-nonnull-memcpy-zero-length"
    "null argument to nonnull parameter 2 of `memcpy`" c_nonnull
    "fn probe() void { byte u8 = 0\nmemcpy(&byte, null, 0)\nreturn }\n";
  c_semantic_message "c-nonnull-strcmp-local-null"
    "null argument to nonnull parameter 2 of `strcmp`" c_nonnull
    "fn probe(first addr) i32 { second addr = null\nreturn strcmp(first, second) }\n";
  c_semantic_message "c-nonnull-no-index-pointer-parameter"
    "null argument to nonnull parameter 2 of `fas_nonnull_every`" c_nonnull
    "fn probe() void { fas_nonnull_every(c\"text\", null, 1)\nreturn }\n";
  c_semantic_message "c-nonnull-qualified-parameter"
    "null argument to nonnull parameter 1 of `fas_nonnull_qualified`" c_nonnull
    "fn probe() void { fas_nonnull_qualified(null)\nreturn }\n";
  c_semantic_accept "c-nonnull-unreachable-if-false" c_nonnull
    "fn probe() void { if false { strlen(null) }\nreturn }\n";
  c_semantic_accept "c-nonnull-unreachable-null-guard" c_nonnull
    "fn probe() void { p addr = null\nif p != null { strlen(p) }\nreturn }\n";
  c_semantic_accept "c-nonnull-unreachable-ternary-arm" c_nonnull
    "fn probe() usize { p addr = null\n\
     n usize = if p == null { 0 } else { strlen(p) }\n\
     return n }\n";
  c_semantic_accept "c-nonnull-unreachable-and-right" c_nonnull
    "fn probe() bool { p addr = null\nreturn p != null && strlen(p) == 0 }\n";
  c_semantic_accept "c-nonnull-unreachable-or-right" c_nonnull
    "fn probe() bool { p addr = null\nreturn p == null || strlen(p) == 0 }\n";
  c_semantic_accept "c-nonnull-unreachable-while-body" c_nonnull
    "fn probe() void { p addr = null\nwhile p != null { strlen(p) }\nreturn }\n";
  c_semantic_message "c-nonnull-reachable-null-guard"
    "null argument to nonnull parameter 1 of `strlen`" c_nonnull
    "fn probe() void { p addr = null\nif p == null { strlen(p) }\nreturn }\n";
  c_semantic_accept "c-nonnull-unmarked-nullable-parameter" c_matrix
    "fn probe() addr { return fas_scalar_pointer(null) }\n";
  c_semantic_accept "c-nonnull-nonzero-offset" c_nonnull
    "fn probe() usize { return strlen(addr_from_bits(0) + 1) }\n";
  c_semantic_accept "c-nonnull-null-object-join" c_nonnull
    "fn probe(flag bool) usize { pointer addr\n\
     if flag { pointer = null } else { pointer = c\"object\" }\n\
     return strlen(pointer) }\n";
  c_semantic_accept "c-nonnull-reconstructed-zero" c_nonnull
    "fn probe() usize { return strlen(addr_from_bits(0)) }\n";
  c_semantic_accept "c-nonnull-fas-wrapper" c_nonnull
    "fn wrapped() addr { return null }\nfn probe() usize { return strlen(wrapped()) }\n";
  let c_overaligned = c_import_fixture "overaligned_typedefs.h" in
  let c_overaligned_stride = c_import_fixture "overaligned_stride.h" in
  let c_record_attributes = c_import_fixture "record_attributes.h" in
  c_semantic_accept "c-import-overaligned-typedef-unused" c_overaligned
    "fn probe() i32 { return 0 }\n";
  c_semantic_accept "c-import-alignment-equals-size" c_overaligned
    "struct Holder { value FasAlignmentEqualsSize }\n\
     var stored FasAlignmentEqualsSize = 7\n\
     fn probe(value FasAlignmentEqualsSize) FasAlignmentEqualsSize { return value }\n";
  let overaligned_reason = "over-aligned typedef has alignment greater than its size" in
  c_semantic_message "c-import-overaligned-typedef-type"
    ("C declaration `FasOveraligned` is not supported: " ^ overaligned_reason)
    c_overaligned "fn probe() FasOveraligned { return 0 }\n";
  c_semantic_message "c-import-overaligned-typedef-field"
    ("C declaration `FasOveraligned` is not supported: " ^ overaligned_reason)
    c_overaligned "struct Holder { value FasOveraligned }\n";
  c_semantic_message "c-import-overaligned-typedef-global"
    ("C declaration `FasOveraligned` is not supported: " ^ overaligned_reason)
    c_overaligned "var stored FasOveraligned = 0\n";
  c_semantic_message "c-import-overaligned-typedef-parameter"
    ("C declaration `FasOveraligned` is not supported: " ^ overaligned_reason)
    c_overaligned "fn probe(value FasOveraligned) void { return }\n";
  c_semantic_message "c-import-overaligned-typedef-c-global"
    "C declaration `fas_overaligned_global` is not supported: over-aligned typedef \
     `FasOveraligned` has alignment greater than its size"
    c_overaligned "fn probe() i32 { return fas_overaligned_global }\n";
  c_semantic_message "c-import-overaligned-typedef-c-parameter"
    "C declaration `fas_overaligned_parameter` is not supported: over-aligned typedef \
     `FasOveraligned` has alignment greater than its size"
    c_overaligned "fn probe() i32 { return fas_overaligned_parameter(0) }\n";
  c_semantic_message "c-import-unrepresentable-record-stride"
    "C declaration `FasUnrepresentableStride` is not supported: record layout differs \
     from C"
    c_overaligned_stride
    "fn probe() usize { return sizeof[FasUnrepresentableStride] }\n";
  let c_matrix_source, c_matrix_imported = c_matrix in
  let stdio_declarations, _, _ =
    expect_ok
      (C_import.import ~cc:"clang-22" ~keep:false c_matrix_source
         [ C_import.{ spelling = Ast.C_system "stdio.h"; span = Span.synthetic } ])
  in
  let stdio_import =
    C_import.map_declarations ~span:Span.synthetic stdio_declarations
  in
  incr checks_run;
  if
    List.length
      (List.filter
         (function Ast.Func function_ -> function_.name = "printf" | _ -> false)
         stdio_import.items)
    <> 1
    || List.mem_assoc "printf" stdio_import.unsupported
  then failwith "implicit C builtin declaration conflicted with stdio.h printf";
  let lazy_long_double = snd (c_import_fixture "lazy_long_double.h") in
  let lazy_long_double_manifest = C_import.manifest_text lazy_long_double in
  incr checks_run;
  (match
     List.find_opt
       (fun line -> String.starts_with ~prefix:"FasLazyLongDouble\t" line)
       (String.split_on_char '\n' lazy_long_double_manifest)
   with
  | Some line
    when contains line "opaque FasLazyLongDouble"
         && contains line "unsupported=floating-point types are not supported" ->
      ()
  | _ -> failwith "lazy long-double record was not preserved as an opaque manifest type");
  let restrict_manifest =
    C_import.manifest_text c_matrix_imported
    |> String.split_on_char '\n'
    |> List.find_opt (fun line ->
        String.starts_with ~prefix:"fas_restrict_pointer\t" line)
  in
  incr checks_run;
  (match restrict_manifest with
  | Some line -> (
      match String.split_on_char '\t' line with
      | [ "fas_restrict_pointer"; spelling; signature; qualifiers; location ]
        when spelling = "void (int *restrict) fas_restrict_pointer"
             && signature = "fn(addr)->void" && qualifiers = "restrict"
             && contains location "matrix.h:" ->
          ()
      | columns ->
          failwith ("C manifest columns changed: " ^ String.concat " | " columns))
  | None -> failwith "C manifest omitted the restrict function");
  let structured_c = c_import_fixture "structured_regressions.h" in
  let _, structured_imported = structured_c in
  let structured_manifest name =
    match
      List.find_opt
        (fun line -> String.starts_with ~prefix:(name ^ "\t") line)
        structured_imported.manifest
    with
    | Some line -> String.split_on_char '\t' line
    | None -> failwith ("C manifest omitted " ^ name)
  in
  let check_structured_manifest name spelling signature qualifiers reason =
    incr checks_run;
    match structured_manifest name with
    | [ entry_name; entry_spelling; entry_signature; entry_qualifiers; location ]
      when entry_name = name && entry_spelling = spelling && entry_signature = signature
           && entry_qualifiers = qualifiers
           && contains location "structured_regressions.h:"
           && contains location reason ->
        ()
    | columns ->
        failwith ("C structured manifest changed: " ^ String.concat " | " columns)
  in
  check_structured_manifest "fas_restrict_parameter"
    "void (int *restrict) fas_restrict_parameter" "fn(addr)->void" "restrict" "";
  check_structured_manifest "fas_restrict_nested"
    "void (int *restrict *) fas_restrict_nested" "fn(addr)->void" "restrict" "";
  check_structured_manifest "fas_restrict_typedef"
    "void (fas_restrict_pointer) fas_restrict_typedef" "fn(addr)->void" "__restrict" "";
  check_structured_manifest "fas_read_function_t"
    "long (void *, char *, unsigned long) fas_read_function_t" "typedef addr" "" "";
  check_structured_manifest "fas_write_function_t"
    "long (void *, const char *, unsigned long) fas_write_function_t" "typedef addr"
    "const" "";
  check_structured_manifest "fas_complex_double" "_Complex double fas_complex_double"
    "typedef" "" "unsupported=floating-point types are not supported";
  c_semantic_accept "c-import-function-type-typedef" structured_c
    "fn probe() fas_read_function_t { return null }\n";
  c_semantic_message "c-import-complex-typedef"
    "C declaration `fas_complex_double` is not supported: floating-point types are not \
     supported"
    structured_c "fn probe() fas_complex_double { return 0 }\n";
  let provenance_dir = Filename.dirname c_matrix_source in
  let provenance_headers =
    List.map
      (fun (name, line) ->
        C_import.
          {
            spelling = Ast.C_quoted name;
            span =
              Span.make ~file:c_matrix_source ~start_offset:0 ~end_offset:0 ~line
                ~column:1;
          })
      [ ("provenance_first.h", 1); ("provenance_second.h", 2) ]
  in
  let provenance_declarations, _, _ =
    expect_ok
      (C_import.import ~cc:"clang-22" ~keep:false c_matrix_source provenance_headers)
  in
  let provenance_import =
    C_import.map_declarations ~span:Span.synthetic provenance_declarations
  in
  List.iter
    (fun (name, header, line) ->
      incr checks_run;
      let expected_file = Filename.concat provenance_dir header ^ ":" ^ line in
      match
        List.find_opt
          (fun entry -> String.starts_with ~prefix:(name ^ "\t") entry)
          provenance_import.manifest
      with
      | Some entry when contains entry expected_file -> ()
      | Some entry ->
          failwith
            (Printf.sprintf "C provenance %s: expected %s, got %s" name expected_file
               entry)
      | None -> failwith ("C provenance declaration missing: " ^ name))
    [
      ("FasProvenanceFirstType", "provenance_first.h", "1");
      ("fas_provenance_first", "provenance_first.h", "2");
      ("fas_provenance_macro", "provenance_first.h", "4");
      ("FasProvenanceSecondType", "provenance_second.h", "1");
      ("fas_provenance_second", "provenance_second.h", "2");
    ];
  incr checks_run;
  let container_source_path = Filename.temp_file "fas-container-import-" ".fas" in
  let container_local_header = container_source_path ^ ".h" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ container_source_path; container_local_header ])
    (fun () ->
      let header_channel = open_out_bin container_local_header in
      output_string header_channel "#define FAS_CONTAINER_LOCAL 2\n";
      close_out header_channel;
      let container_source =
        Printf.sprintf
          "use \"C\" <stddef.h>\n\
           use \"C\" <<FIRST\n\
           #define FAS_CONTAINER_MACRO 40\n\
           FIRST\n\
           use \"C\" <limits.h>\n\
           use \"C\" <<SECOND\n\
           #include \"%s\"\n\
           int fas_fragment_value(void) { return FAS_CONTAINER_MACRO + CHAR_BIT + \
           FAS_CONTAINER_LOCAL; }\n\
           static int fas_fragment_static(int value) { return value + 1; }\n\
           static int fas_unused_fragment_static(void) { return 3; }\n\
           SECOND\n"
          (Filename.basename container_local_header)
      in
      let parsed =
        match
          Parser.parse
            (Source.create ~file:container_source_path ~text:container_source)
        with
        | Ok parsed -> parsed
        | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      in
      let requests =
        List.filter_map
          (function
            | Ast.Use { c_header = Some spelling; span; _ } ->
                Some C_import.{ spelling; span }
            | _ -> None)
          parsed.items
      in
      let declarations, kept, _ =
        expect_ok
          (C_import.import ~cc:"clang-22" ~keep:true container_source_path requests)
      in
      let kept = Option.value ~default:[] kept in
      Fun.protect
        ~finally:(fun () ->
          List.iter (fun path -> try Sys.remove path with Sys_error _ -> ()) kept)
        (fun () ->
          match kept with
          | [ unit_path; first_fragment; second_fragment ] -> (
              let read path =
                let channel = open_in_bin path in
                Fun.protect
                  ~finally:(fun () -> close_in_noerr channel)
                  (fun () -> really_input_string channel (in_channel_length channel))
              in
              let unit = read unit_path in
              let ordered =
                [
                  C_import.find_text unit "#include <stddef.h>" 0;
                  C_import.find_text unit ("#include \"" ^ first_fragment ^ "\"") 0;
                  C_import.find_text unit "#include <limits.h>" 0;
                  C_import.find_text unit ("#include \"" ^ second_fragment ^ "\"") 0;
                ]
              in
              (match ordered with
              | [ Some first; Some second; Some third; Some fourth ]
                when first < second && second < third && third < fourth ->
                  ()
              | _ -> failwith "C unit did not preserve header and fragment source order");
              let first_line = (List.nth requests 1).C_import.span.Span.line + 1
              and second_line = (List.nth requests 3).C_import.span.Span.line + 1 in
              if
                (not
                   (String.starts_with
                      ~prefix:
                        (Printf.sprintf "#line %d %S\n" first_line container_source_path)
                      (read first_fragment)))
                || not
                     (String.starts_with
                        ~prefix:
                          (Printf.sprintf "#line %d %S\n" second_line
                             container_source_path)
                        (read second_fragment))
              then failwith "C fragment files did not start at their Fas source lines";
              let imported =
                C_import.map_declarations ~span:Span.synthetic declarations
              in
              if
                not
                  (List.exists
                     (String.starts_with ~prefix:"fas_fragment_value\t")
                     imported.manifest)
              then failwith "C function definition in a fragment was not imported";
              incr checks_run;
              List.iter
                (fun name ->
                  if
                    not
                      (List.exists
                         (fun (static : C_import.static_function) -> static.name = name)
                         imported.static_functions)
                  then
                    failwith ("static C function in fragment was not recorded: " ^ name))
                [ "fas_fragment_static"; "fas_unused_fragment_static" ];
              incr checks_run;
              match
                List.find_opt
                  (fun (static : C_import.static_function) ->
                    static.name = "fas_fragment_static")
                  imported.static_functions
              with
              | Some static -> (
                  match
                    C_import.make_adapter ~occupied:[] container_source_path static
                  with
                  | Ok adapter
                    when contains adapter.code "return fas_fragment_static(fas_arg0);"
                    ->
                      ()
                  | Ok _ ->
                      failwith "static C fragment adapter did not forward its call"
                  | Error message -> failwith message)
              | None -> failwith "static C fragment function was not recorded")
          | _ -> failwith "--keep omitted generated C units or fragment files"));
  List.iter
    (fun (name, expected) ->
      incr checks_run;
      match List.assoc_opt name c_matrix_imported.aliases with
      | Some actual when actual = expected -> ()
      | Some actual ->
          failwith
            (Printf.sprintf "C typedef %s: expected %s, got %s" name
               (Ast.type_name expected) (Ast.type_name actual))
      | None -> failwith ("C typedef was not imported: " ^ name))
    [
      ("fas_i8", Ast.Int Ast.I8);
      ("fas_u8", Ast.Int Ast.U8);
      ("fas_i16", Ast.Int Ast.I16);
      ("fas_u16", Ast.Int Ast.U16);
      ("fas_i32", Ast.Int Ast.I32);
      ("fas_u32", Ast.Int Ast.U32);
      ("fas_i64", Ast.Int Ast.I64);
      ("fas_u64", Ast.Int Ast.U64);
      ("fas_bool", Ast.Bool);
    ];
  let repeated_import =
    C_import.merge_imports [ c_matrix_imported; c_matrix_imported ]
  in
  let named_item name items =
    List.filter
      (function
        | Ast.Opaque { name = item_name; _ }
        | Ast.Const { name = item_name; _ }
        | Ast.Global { name = item_name; _ }
        | Ast.Func { name = item_name; _ } ->
            item_name = name
        | _ -> false)
      items
  in
  let nested_dir = Filename.concat (Filename.dirname c_matrix_source) "nested" in
  Unix.mkdir nested_dir 0o700;
  Fun.protect
    ~finally:(fun () -> Unix.rmdir nested_dir)
    (fun () ->
      let relative_source = Filename.concat nested_dir "probe.fas" in
      let relative_span =
        Span.make ~file:relative_source ~start_offset:0 ~end_offset:0 ~line:3 ~column:1
      in
      let relative_header =
        C_import.{ spelling = Ast.C_quoted "../matrix.h"; span = relative_span }
      in
      let relative_declarations, _, _ =
        expect_ok
          (C_import.import ~cc:"clang-22" ~keep:false relative_source
             [ relative_header ])
      in
      let relative_import =
        C_import.map_declarations ~span:relative_span relative_declarations
      in
      incr checks_run;
      if named_item "fas_i32_echo" relative_import.items = [] then
        failwith "quoted C header path was not resolved from a nested Fas file";
      let missing_span =
        Span.make ~file:relative_source ~start_offset:0 ~end_offset:0 ~line:9 ~column:4
      in
      let missing_header =
        C_import.{ spelling = Ast.C_quoted "../absent.h"; span = missing_span }
      in
      incr checks_run;
      match
        C_import.import ~cc:"clang-22" ~keep:false relative_source [ missing_header ]
      with
      | Error [ diagnostic ]
        when diagnostic.primary = missing_span
             && diagnostic.message = "C header `../absent.h` not found" ->
          ()
      | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      | Ok _ -> failwith "missing C header was accepted");
  incr checks_run;
  if List.length (named_item "fas_i32_echo" repeated_import.items) <> 1 then
    failwith "repeated C imports did not merge to one function binding";
  incr checks_run;
  if
    List.length
      (List.filter (fun (name, _) -> name = "fas_i32") repeated_import.aliases)
    <> 1
  then failwith "repeated C imports did not merge to one typedef binding";
  let matching =
    parse_file c_matrix_source
      "use \"C\" \"matrix.h\"\nextern \"C\" { fn fas_i32_echo(value i32) i32 }\n"
  in
  let matching_import =
    expect_ok (C_import.reconcile_source matching.items c_matrix_imported)
  in
  incr checks_run;
  if named_item "fas_i32_echo" matching_import.items <> [] then
    failwith "matching extern C declaration did not confirm its import";
  incr checks_run;
  (match
     Sema.check ~c_aliases:matching_import.aliases
       ~c_unsupported:matching_import.unsupported
       ~c_records:matching_import.record_types
       { Ast.items = matching.items @ matching_import.items }
   with
  | Ok _ -> ()
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics));
  let mismatching =
    parse_file c_matrix_source
      "use \"C\" \"matrix.h\"\nextern \"C\" { fn fas_i32_echo(value u32) i32 }\n"
  in
  incr checks_run;
  (match C_import.reconcile_source mismatching.items c_matrix_imported with
  | Error [ diagnostic ]
    when diagnostic.message
         = "C declaration `fas_i32_echo` has type `fn(i32)->i32`, but Fas declares \
            `fn(u32)->i32`" ->
      ()
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  | Ok _ -> failwith "mismatching extern C declaration was accepted");
  let c_conflict = snd (c_import_fixture "conflict.h") in
  let conflicted = C_import.merge_imports [ c_matrix_imported; c_conflict ] in
  c_semantic_accept "c-import-unused-cross-file-conflict" (c_matrix_source, conflicted)
    "fn main() i32 { return 0 }\n";
  c_semantic_message "c-import-cross-file-conflict"
    "C declaration `fas_i32_echo` is not supported: conflicting C declarations"
    (c_matrix_source, conflicted) "fn main() i32 { return fas_i32_echo(7) }\n";
  let split_directory =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "fas-split-regressions-%d" (Unix.getpid ()))
  in
  Unix.mkdir split_directory 0o700;
  let split_path name = Filename.concat split_directory name in
  let source_lines lines =
    String.concat "" (List.map (fun line -> line ^ "\n") lines)
  in
  let split_run ~a_header ~b_header root mini =
    let write name contents =
      let channel = open_out_bin (split_path name) in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel contents)
    in
    write "split_root.fas" root;
    write "split_mini.fas" mini;
    write "split_a.h" a_header;
    write "split_b.h" b_header;
    Driver.run (cli_run [ "--emit-llvm"; split_path "split_root.fas" ])
  in
  let split_expect_nonnull label function_name result =
    let expected =
      Printf.sprintf "null argument to nonnull parameter 1 of `%s`" function_name
    in
    match result with
    | Error diagnostics ->
        let messages =
          List.map (fun (diagnostic : Diag.t) -> diagnostic.message) diagnostics
        in
        if
          (not (List.mem expected messages))
          || List.exists (fun message -> contains message "conflicting") messages
        then
          failwith
            (label ^ ": expected the nonnull diagnostic for `" ^ function_name ^ "`")
    | Ok _ -> failwith (label ^ ": expected the nonnull diagnostic")
  in
  let split_pthread_create_root =
    source_lines
      [
        "fn main() i32 {";
        "    st i32 = pthread_create(null, null, null, null)";
        "    return st";
        "}";
      ]
  in
  let split_pthread_mutex_root =
    source_lines
      [
        "fn main() i32 {"; "    st i32 = pthread_mutex_lock(null)"; "    return st"; "}";
      ]
  in
  let split_union_root =
    source_lines [ "fn main() i32 {"; "    v_ptr(1)"; "    return 0"; "}" ]
  in
  let split_union_a_header =
    source_lines
      [
        "typedef long lng;";
        "typedef union { lng a; char b[8]; } V;";
        "V *v_ptr(int value);";
      ]
  in
  let split_union_b_header =
    source_lines [ "typedef long lng;"; "typedef union { lng a; char b[8]; } V;" ]
  in
  let split_conflict_a_header = source_lines [ "typedef struct { int x; } T;" ] in
  let split_conflict_b_header = source_lines [ "typedef struct { long x; } T;" ] in
  let split_conflict_root =
    source_lines
      [ "fn take(value T) i32 {"; "    return 0"; "}"; "fn main() i32 { return 0 }" ]
  in
  let split_with_include include_line root =
    source_lines [ include_line; "use \"split_mini.fas\"" ] ^ root
  in
  let split_expect_anonymous label root mini =
    match
      split_run ~a_header:split_union_a_header ~b_header:split_union_b_header root mini
    with
    | Ok _ -> ()
    | Error diagnostics ->
        failwith (label ^ ": " ^ Diag.render_all ~source:None diagnostics)
  in
  let split_unused_record_root =
    source_lines
      [
        "use \"C\" <pthread.h>";
        "use \"split_mini.fas\"";
        "fn main() i32 {";
        "    first pthread_t";
        "    st i32 = pthread_create(&first, null, &split_worker, null)";
        "    return st";
        "}";
      ]
  in
  let split_unused_record_mini =
    source_lines
      [
        "use \"C\" <sys/types.h>";
        "var calls i32 = 0";
        "extern \"C\" {";
        "  fn split_worker(arg addr) addr {";
        "    calls += 1";
        "    return null";
        "  }";
        "}";
        "fn split_calls() i32 {";
        "  return calls";
        "}";
      ]
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun name -> try Sys.remove (split_path name) with Sys_error _ -> ())
        [ "split_root.fas"; "split_mini.fas"; "split_a.h"; "split_b.h" ];
      try Unix.rmdir split_directory with Unix.Unix_error _ -> ())
    (fun () ->
      split_expect_nonnull "split-pthread-create-root-first" "pthread_create"
        (split_run ~a_header:"" ~b_header:""
           (split_with_include "use \"C\" <pthread.h>" split_pthread_create_root)
           (source_lines [ "use \"C\" <sys/types.h>" ]));
      split_expect_nonnull "split-pthread-create-mini-first" "pthread_create"
        (split_run ~a_header:"" ~b_header:""
           (split_with_include "use \"C\" <sys/types.h>" split_pthread_create_root)
           (source_lines [ "use \"C\" <pthread.h>" ]));
      split_expect_nonnull "split-pthread-mutex-lock" "pthread_mutex_lock"
        (split_run ~a_header:"" ~b_header:""
           (split_with_include "use \"C\" <pthread.h>" split_pthread_mutex_root)
           (source_lines [ "use \"C\" <sys/types.h>" ]));
      split_expect_anonymous "split-anonymous-typedef"
        (split_with_include "use \"C\" \"split_a.h\"" split_union_root)
        (source_lines [ "use \"C\" \"split_b.h\"" ]);
      split_expect_anonymous "split-anonymous-typedef-reversed"
        (split_with_include "use \"C\" \"split_b.h\"" split_union_root)
        (source_lines [ "use \"C\" \"split_a.h\"" ]);
      (match
         split_run ~a_header:"" ~b_header:"" split_unused_record_root
           split_unused_record_mini
       with
      | Ok llvm ->
          if contains llvm "%struct.__fsid_t" then
            failwith "unused-record-fsid: `__fsid_t` is emitted without a reference";
          if not (contains llvm "%struct.pthread_attr_t") then
            failwith "unused-record-fsid: `pthread_attr_t` is missing from the LLVM"
      | Error diagnostics ->
          failwith
            ("unused-record-fsid: unexpected rejection: "
            ^ Diag.render_all ~source:None diagnostics));
      let conflict_message =
        "C declaration `T` is not supported: conflicting C declarations"
      in
      match
        split_run ~a_header:split_conflict_a_header ~b_header:split_conflict_b_header
          (split_with_include "use \"C\" \"split_a.h\"" split_conflict_root)
          (source_lines [ "use \"C\" \"split_b.h\"" ])
      with
      | Error diagnostics
        when List.exists
               (fun (diagnostic : Diag.t) -> diagnostic.message = conflict_message)
               diagnostics ->
          ()
      | Error diagnostics ->
          failwith
            ("split-conflict-twin: unexpected rejection: "
            ^ Diag.render_all ~source:None diagnostics)
      | Ok _ -> failwith "split-conflict-twin: real conflict was accepted");
  c_semantic_accept "c-import-unused-unsupported" c_matrix
    "fn main() i32 { return 0 }\n";
  c_semantic_accept "c-import-integer-alias-matrix" c_matrix
    "fn integers(a fas_i8, b fas_u8, c fas_i16, d fas_u16, e fas_i32, f fas_u32, g \
     fas_i64, h fas_u64, flag fas_bool) void {\n\
     fas_char_echo(a)\n\
     fas_i8_echo(a)\n\
     fas_u8_echo(b)\n\
     fas_i16_echo(c)\n\
     fas_u16_echo(d)\n\
     fas_i32_echo(e)\n\
     fas_u32_echo(f)\n\
     fas_i64_echo(g)\n\
     fas_u64_echo(h)\n\
     fas_bool_echo(flag)\n\
     return }\n";
  c_semantic_accept "c-import-typedef-chain" c_matrix
    "fn chain(value fas_i8_chain) fas_i8 { return value }\n";
  let c_sizes = c_import_fixture "sizes.h" in
  List.iter
    (fun (name, expected) ->
      incr checks_run;
      match List.assoc_opt name (snd c_sizes).aliases with
      | Some actual when actual = Ast.Int expected -> ()
      | Some actual ->
          failwith
            (Printf.sprintf "C machine typedef %s: expected %s, got %s" name
               (Ast.type_name (Ast.Int expected))
               (Ast.type_name actual))
      | None -> failwith ("C machine typedef was not imported: " ^ name))
    [
      ("size_t", Ast.Usize);
      ("uintptr_t", Ast.Usize);
      ("ssize_t", Ast.Isize);
      ("ptrdiff_t", Ast.Isize);
      ("intptr_t", Ast.Isize);
      ("fas_size_alias", Ast.Usize);
      ("fas_uintptr_alias", Ast.Usize);
      ("fas_ssize_alias", Ast.Isize);
      ("fas_ptrdiff_alias", Ast.Isize);
      ("fas_intptr_alias", Ast.Isize);
    ];
  c_semantic_accept "c-import-size-functions" c_sizes
    "fn length() usize { return strlen(c\"fas\") }\n\
     fn allocation(n usize) addr { return malloc(n) }\n";
  let c_alias_specialization =
    c_semantic_result c_sizes
      "fn id[T](value T) T { return value }\n\
       fn use_aliases() usize { first usize = id[fas_size_alias](1)\n\
       return id[fas_uintptr_alias](first) }\n"
    |> expect_ok
  in
  let id_specializations =
    List.filter
      (fun (func : Hir.func) -> contains func.name "id$spec$")
      c_alias_specialization.Hir.funcs
  in
  if List.length id_specializations <> 1 then
    failwith "c-alias-specialization: resolved typedef aliases used distinct slots";
  let c_stat = c_import_fixture "stat.h" in
  let stat_second_parameter imported =
    List.find_map
      (function
        | Ast.Func { name = "stat"; params; _ } ->
            Option.map
              (fun (parameter : Ast.param) -> parameter.ty)
              (List.nth_opt params 1)
        | _ -> None)
      imported.C_import.items
  in
  incr checks_run;
  (match stat_second_parameter (snd c_stat) with
  | Some Ast.Addr -> ()
  | Some _ ->
      failwith "stat without a record typedef did not import its pointer as addr"
  | None -> failwith "sys/stat.h stat() was not imported");
  c_semantic_accept "c-import-stat-without-typedef" c_stat
    "fn call_stat(path addr) i32 { metadata i32 = 0\nreturn stat(path, &metadata) }\n";
  let stat_source = fst c_stat in
  let stat_headers =
    [
      C_import.{ spelling = Ast.C_quoted "stat.h"; span = Span.synthetic };
      C_import.
        {
          spelling =
            Ast.C_fragment
              {
                tag = "FAS_STAT_TYPEDEF";
                text = "typedef struct stat stat_base; typedef stat_base stat_t;";
              };
          span = Span.synthetic;
        };
    ]
  in
  let stat_declarations, _, _ =
    expect_ok (C_import.import ~cc:"clang-22" ~keep:false stat_source stat_headers)
  in
  let c_stat_typedef =
    C_import.map_declarations ~span:Span.synthetic stat_declarations
  in
  incr checks_run;
  (match stat_second_parameter c_stat_typedef with
  | Some (Ast.Handle (Ast.Named_type ("stat_base", _))) -> ()
  | Some _ -> failwith "stat_t did not become the imported stat() handle type"
  | None -> failwith "sys/stat.h stat() was not imported with stat_t");
  c_semantic_accept "c-import-stat-typedef-handle" (stat_source, c_stat_typedef)
    "fn call_stat(path addr, output handle[stat_base]) i32 {\n\
     st stat_base\n\
     return stat(path, output) }\n";
  let c_namespaces = c_import_fixture "namespaces.h" in
  c_semantic_accept "c-import-tag-ordinary-collisions" c_namespaces
    "fn collisions() i32 { return FasTagFunction() + FasTagGlobal + FasTagEnum }\n";
  List.iter
    (fun name ->
      incr checks_run;
      if
        List.exists
          (function Ast.Struct { name = record; _ } -> record = name | _ -> false)
          (snd c_namespaces).items
      then failwith ("ordinary C name lost to record tag " ^ name))
    [ "FasTagFunction"; "FasTagGlobal"; "FasTagEnum" ];
  if
    List.exists
      (function Ast.Struct { name; _ } -> name = "FasTagTypedef" | _ -> false)
      (snd c_namespaces).items
  then failwith "ordinary C typedef lost to record tag FasTagTypedef";
  c_semantic_accept "c-import-tag-typedef-collision" c_namespaces
    "fn collision_typedef(value FasTagTypedef) FasTagTypedef { return value }\n\
     fn collision_pointer() addr { return fas_tag_typedef_pointer() }\n";
  c_semantic_accept "c-import-enum-values-and-abi" c_matrix
    "fn enum_values() i64 {\n\
     fas_enum_arg(FAS_ENUM_NEG)\n\
     fas_enum_arg(FAS_ENUM_LARGE)\n\
     return FAS_ENUM_NEG }\n";
  c_semantic_accept "c-import-anonymous-enum-typedef-abi" c_matrix
    "fn anonymous_enum(value FasAnonymousEnum) FasAnonymousEnum {\n\
     return fas_anonymous_enum_echo(value) }\n";
  c_semantic_accept "c-import-anonymous-enum-typedef-chain" c_matrix
    "fn anonymous_enum(value FasAnonymousEnumChain) FasAnonymousEnumChain {\n\
     return fas_anonymous_enum_chain_echo(value) }\n";
  let enum_program =
    match
      c_semantic_result c_matrix "fn enum_values() i64 { return FAS_ENUM_LARGE }\n"
    with
    | Ok program -> program
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  List.iter
    (fun (name, value) ->
      match
        List.find_opt
          (fun (constant : Hir.const_def) -> constant.name = name)
          enum_program.consts
      with
      | Some { ty = Hir.Int Hir.I64; bits; _ } when bits = value -> ()
      | Some { ty; bits; _ } ->
          failwith
            (Printf.sprintf "C enum %s: expected i64 %Ld, got %s %Ld" name value
               (Hir.ty_name ty) bits)
      | None -> failwith ("C enum constant not imported: " ^ name))
    [ ("FAS_ENUM_NEG", -3L); ("FAS_ENUM_LARGE", 0xffffffffL) ];
  let implicit_enum = c_import_fixture "enums.h" in
  let implicit_enum_program =
    match
      c_semantic_result implicit_enum
        "fn enum_values() i32 { return FAS_IMPLICIT_INT_MAX }\n"
    with
    | Ok program -> program
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  List.iter
    (fun (name, value) ->
      match
        List.find_opt
          (fun (constant : Hir.const_def) -> constant.name = name)
          implicit_enum_program.consts
      with
      | Some { ty = Hir.Int Hir.I32; bits; _ } when bits = value -> ()
      | Some { ty; bits; _ } ->
          failwith
            (Printf.sprintf "C implicit enum %s: expected i32 %Ld, got %s %Ld" name
               value (Hir.ty_name ty) bits)
      | None -> failwith ("C implicit enum constant not imported: " ^ name))
    [
      ("FAS_IMPLICIT_FIRST", 0L);
      ("FAS_IMPLICIT_EXPLICIT", 7L);
      ("FAS_IMPLICIT_AFTER_EXPLICIT", 8L);
      ("FAS_IMPLICIT_NEGATIVE", 4294967291L);
      ("FAS_IMPLICIT_AFTER_NEGATIVE", 4294967292L);
      ("FAS_IMPLICIT_NEAR_MAX", 2147483646L);
      ("FAS_IMPLICIT_INT_MAX", 2147483647L);
    ];
  let c_enum_types = c_import_fixture "enum_types.h" in
  List.iter
    (fun (name, expected) ->
      incr checks_run;
      match List.assoc_opt name (snd c_enum_types).aliases with
      | Some actual when actual = Ast.Int expected -> ()
      | Some actual ->
          failwith
            (Printf.sprintf "C enum type %s: expected %s, got %s" name
               (Ast.type_name (Ast.Int expected))
               (Ast.type_name actual))
      | None -> failwith ("C enum type was not imported: " ^ name))
    [
      ("ammo_t", Ast.I32);
      ("state_t", Ast.I32);
      ("ammo_chain_t", Ast.I32);
      ("packed_t", Ast.U8);
      ("wide_t", Ast.I64);
      ("unsigned_enum_t", Ast.U32);
      ("FasTaggedEnumType", Ast.I32);
      ("FasEarlierEnumType", Ast.I32);
    ];
  c_semantic_accept "c-import-enum-repro-and-abi" c_enum_types
    "const constant_tab arr[2,i32] = { S_X, S_Y }\n\
     extern \"C\" {\n\
     var tab arr[1,info_t] = { {am_b, S_Y} }\n\
     fn enum_result() i32 { return S_Y }\n\
     fn enum_roundtrip(value state_t) state_t { return c_enum_roundtrip(value) }\n\
     fn packed_roundtrip(value packed_t) packed_t { return c_packed_roundtrip(value) }\n\
     fn wide_roundtrip(value wide_t) wide_t { return c_wide_roundtrip(value) }}\n";
  let c_enum_program =
    match
      c_semantic_result c_enum_types
        "const constant_tab arr[2,i32] = { S_X, S_Y }\n\
         fn enum_result() i32 { return S_Y }\n"
    with
    | Ok program -> program
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  List.iter
    (fun (name, ty, value) ->
      match
        List.find_opt
          (fun (constant : Hir.const_def) -> constant.name = name)
          c_enum_program.consts
      with
      | Some { ty = actual; bits; _ } when actual = ty && bits = value -> ()
      | Some { ty = actual; bits; _ } ->
          failwith
            (Printf.sprintf "C enum constant %s: expected %s %Ld, got %s %Ld" name
               (Hir.ty_name ty) value (Hir.ty_name actual) bits)
      | None -> failwith ("C enum constant not imported: " ^ name))
    [
      ("am_a", Hir.Int Hir.I32, 0L);
      ("am_b", Hir.Int Hir.I32, 1L);
      ("S_X", Hir.Int Hir.I32, 0L);
      ("S_Y", Hir.Int Hir.I32, 1L);
      ("PACKED_MIN", Hir.Int Hir.I32, 0L);
      ("PACKED_MAX", Hir.Int Hir.I32, 255L);
      ("WIDE_NEG", Hir.Int Hir.I64, -3L);
      ("WIDE_LARGE", Hir.Int Hir.I64, 4294967295L);
      ("UNSIGNED_ZERO", Hir.Int Hir.U32, 0L);
      ("UNSIGNED_MAX", Hir.Int Hir.U32, 4294967295L);
      ("FAS_TAGGED_FIRST", Hir.Int Hir.I32, 0L);
      ("FAS_TAGGED_LAST", Hir.Int Hir.I32, 1L);
      ("FAS_EARLIER_FIRST", Hir.Int Hir.I32, 0L);
      ("FAS_EARLIER_LAST", Hir.Int Hir.I32, 1L);
    ];
  let c_records = c_import_fixture "records.h" in
  c_semantic_message "extern-c-record-parameter-diagnostic"
    "aggregate parameter `value` of type `FasAnonymousRecord` cannot be passed by \
     value; declare `value` as `addr` or `handle[FasAnonymousRecord]`"
    c_records
    "extern \"C\" { fn consume_record(value FasAnonymousRecord) void { return } }\n";
  c_semantic_message "extern-c-record-result-diagnostic"
    "aggregate result `FasAnonymousRecord` cannot be returned by value; return `void` \
     and take the destination as an `addr` or `handle[FasAnonymousRecord]` parameter"
    c_records "extern \"C\" { fn produce_record() FasAnonymousRecord { return {1} } }\n";
  c_semantic_accept "c-import-typedef-anonymous-record-handle" c_records
    "fn probe(value handle[FasAnonymousRecord]) handle[FasAnonymousRecord] {\n\
    \     return fas_anonymous_record(value) }\n";
  c_semantic_message "c-import-record-without-typedef"
    "C declaration `fas_untyped_record` is not supported: struct and union values are \
     not supported"
    c_records "fn probe() i32 { return fas_untyped_record }\n";
  c_semantic_accept "c-import-pointer-matrix" c_matrix
    "fn pointers(p addr, bytes addr, record handle[FasRecord], other \
     handle[FasOtherRecord], same handle[FasSameRecord]) void {\n\
     fas_void_pointer(p)\n\
     fas_scalar_pointer(bytes)\n\
     fas_record_pointer(record)\n\
     fas_record_alias_pointer(record)\n\
     fas_other_pointer(other)\n\
     fas_same_record(same)\n\
     fas_pointer_output(p)\n\
     fas_variadic(1, true, 2)\n\
     return }\n";
  c_semantic_error "c-import-distinct-record-identities"
    "argument 1 of `fas_other_pointer` is `handle[FasRecord]`, expected \
     `handle[FasOtherRecord]`"
    c_matrix
    "fn wrong(record handle[FasRecord]) void {\nfas_other_pointer(record)\nreturn }\n";
  c_semantic_accept "c-import-global-places" c_matrix
    "fn globals() i32 {\n\
     value i32 = fas_mutable_global\n\
     fas_mutable_global = value\n\
     fas_mutable_pointer_global = null\n\
     fas_mutable_typedef_pointer_global = null\n\
     return fas_readonly_global }\n";
  c_semantic_message "c-import-readonly-global"
    "global `fas_readonly_global` is read-only" c_matrix
    "fn write() void { fas_readonly_global = 1\nreturn }\n";
  c_semantic_message "c-import-readonly-pointer-global"
    "global `fas_readonly_pointer_global` is read-only" c_matrix
    "fn write() void { fas_readonly_pointer_global = null\nreturn }\n";
  c_semantic_message "c-import-readonly-typedef-global"
    "global `fas_readonly_alias_global` is read-only" c_matrix
    "fn write() void { fas_readonly_alias_global = 1\nreturn }\n";
  c_semantic_message "c-import-readonly-typedef-pointer-global"
    "global `fas_readonly_typedef_pointer_global` is read-only" c_matrix
    "fn write() void { fas_readonly_typedef_pointer_global = null\nreturn }\n";
  let unsupported name reason expression =
    c_semantic_message
      ("c-import-unsupported-" ^ name)
      (Printf.sprintf "C declaration `%s` is not supported: %s" name reason)
      c_matrix
      ("fn probe() i32 {\n" ^ expression ^ "\nreturn 0 }\n")
  in
  c_semantic_accept "c-import-float-functions" c_matrix
    "fn float_call(value f32) f32 { return fas_float_value(value) }\n\
     fn double_call(value f64) f64 { return fas_double_value(value) }\n\
     fn static_float_call(value f32) f32 { return fas_static_float(value) }\n\
     fn aliases(value fas_float) fas_double {\n\
     return fas_double_value(fas_float_value(value)) }\n";
  let float_manifest = C_import.manifest_text (snd c_matrix) in
  if
    (not
       (contains float_manifest
          "fas_float_value\tfloat (float) fas_float_value\tfn(f32)->f32"))
    || (not
          (contains float_manifest
             "fas_double_value\tdouble (double) fas_double_value\tfn(f64)->f64"))
    || not
         (contains float_manifest
            "fas_array_float\tfas_float[4] fas_array_float\tarr[4, f32]")
  then failwith "C manifest did not record imported float types";
  unsupported "fas_long_double_value" "floating-point types are not supported"
    "fas_long_double_value(1)";
  unsupported "fas_float16_value" "floating-point types are not supported"
    "fas_float16_value(1)";
  unsupported "fas_fp16_global" "floating-point types are not supported"
    "fas_fp16_global = 1";
  unsupported "fas_float128_value" "floating-point types are not supported"
    "fas_float128_value(1)";
  unsupported "fas_int128_value" "`__int128` has no Fas type" "fas_int128_value(1)";
  unsupported "fas_uint128_value" "`__int128` has no Fas type" "fas_uint128_value(1)";
  unsupported "fas_bitint_value" "`_BitInt` has no Fas type" "fas_bitint_value(1)";
  unsupported "fas_variadic_by_value"
    "variadic functions with struct and union values are not supported"
    "fas_variadic_by_value(0)";
  c_semantic_accept "c-record-value-declaration" c_matrix
    "fn probe() i32 {\n\
     v FasByValue = {3}\n\
     r FasByValue = fas_struct_by_value(v)\n\
     return r.field }\n";
  c_semantic_accept "c-record-value-union" c_matrix
    "fn probe() i32 {\n\
     u FasUnionByValue\n\
     u.integer = 1\n\
     r FasUnionByValue = fas_union_by_value(u)\n\
     return r.integer }\n";
  c_semantic_accept "c-record-value-places" c_matrix
    "struct Pair { left FasByValue, right FasByValue }\n\
     fn probe() i32 {\n\
     xs arr[2,FasByValue] = {{1}, {2}}\n\
     xs[1] = fas_struct_by_value(xs[0])\n\
     p Pair = {{4}, {5}}\n\
     p.left = fas_struct_by_value(p.right)\n\
     fas_direct_record_array[1] = fas_struct_by_value(fas_direct_record_array[0])\n\
     return xs[1].field + p.left.field }\n";
  c_semantic_message "c-record-value-literal-argument"
    "argument 1 of `fas_struct_by_value` passes `FasByValue` by value; pass a \
     variable, field or array element of type `FasByValue`"
    c_matrix
    "fn probe() i32 {\nr FasByValue = fas_struct_by_value({3})\nreturn r.field }\n";
  c_semantic_message "c-record-value-call-argument"
    "argument 1 of `fas_struct_by_value` passes `FasByValue` by value; pass a \
     variable, field or array element of type `FasByValue`"
    c_matrix
    "fn probe() i32 {\n\
     v FasByValue = {3}\n\
     r FasByValue = fas_struct_by_value(fas_struct_by_value(v))\n\
     return r.field }\n";
  c_semantic_message "c-record-value-wrong-argument"
    "argument 1 of `fas_struct_by_value` passes `FasByValue` by value; pass a \
     variable, field or array element of type `FasByValue`, got `FasUnionByValue`"
    c_matrix
    "fn probe() i32 {\n\
     u FasUnionByValue\n\
     u.integer = 1\n\
     r FasByValue = fas_struct_by_value(u)\n\
     return r.field }\n";
  c_semantic_message "c-record-value-discarded-result"
    "C function `fas_struct_by_value` returns `FasByValue` by value; use the result \
     only as `x FasByValue = fas_struct_by_value(...)` or `place = \
     fas_struct_by_value(...)`"
    c_matrix
    "fn probe() i32 {\nv FasByValue = {3}\nfas_struct_by_value(v)\nreturn 0 }\n";
  c_semantic_message "c-record-value-field-of-result"
    "C function `fas_struct_by_value` returns `FasByValue` by value; use the result \
     only as `x FasByValue = fas_struct_by_value(...)` or `place = \
     fas_struct_by_value(...)`"
    c_matrix
    "fn probe() i32 {\nv FasByValue = {3}\nreturn fas_struct_by_value(v).field }\n";
  c_semantic_message "c-record-value-wrong-destination"
    "C function `fas_struct_by_value` returns `FasByValue`, but the destination is \
     `FasUnionByValue`"
    c_matrix
    "fn probe() i32 {\n\
     v FasByValue = {3}\n\
     u FasUnionByValue\n\
     u = fas_struct_by_value(v)\n\
     return 0 }\n";
  let c_record_value_calls = c_import_fixture "record_value_calls.h" in
  c_semantic_accept "c-record-value-nested-calls" c_record_value_calls
    "fn probe() i32 {\n\
     q FasPoint = {3, 4}\n\
     p FasPoint = fas_point_make(fas_point_sum(q), fas_point_sum(q))\n\
     p = fas_point_make(fas_point_sum(p), 1)\n\
     return p.x }\n";
  c_semantic_message "c-record-value-result-arity"
    "function `fas_point_make` expects 2 arguments, got 1" c_record_value_calls
    "fn probe() i32 {\np FasPoint = fas_point_make(1)\nreturn p.x }\n";
  c_semantic_message "c-record-value-result-nonnull"
    "null argument to nonnull parameter 2 of `fas_point_named`" c_record_value_calls
    "fn probe() i32 {\np FasPoint = fas_point_named(1, null)\nreturn p.x }\n";
  c_semantic_message "c-record-value-result-c-string"
    "\"ab\" has no NUL terminator, but `fas_point_named` reads parameter `name` as a C \
     string"
    c_record_value_calls
    "fn probe() i32 {\np FasPoint = fas_point_named(1, \"ab\")\nreturn p.x }\n";
  c_semantic_accept "c-record-value-result-c-string-literal" c_record_value_calls
    "fn probe() i32 {\np FasPoint = fas_point_named(1, c\"ab\")\nreturn p.x }\n";
  c_semantic_message "c-record-value-shadowed-declaration"
    "the declaration of `v` cannot use `v` in the arguments of `fas_struct_by_value`; \
     give the result another name"
    c_matrix
    "fn probe() i32 {\n\
     v FasByValue = {3}\n\
     if true {\n\
     v FasByValue = fas_struct_by_value(v)\n\
     return v.field }\n\
     return 0 }\n";
  c_semantic_message "c-record-value-address"
    "C function `fas_struct_by_value` passes a struct or union by value; Fas calls it \
     through an adapter and cannot take its address"
    c_matrix "fn probe() i32 {\np addr = &fas_struct_by_value\nreturn 0 }\n";
  unsupported "fas_bitfield_global" "bit-fields are not supported"
    "fas_bitfield_global = null";
  c_semantic_accept "c-import-array-global" c_matrix
    "fn probe() i32 { fas_array_global[3] = 9; return fas_array_global[0] }\n";
  c_semantic_accept "c-import-array-nested" c_matrix
    "fn probe() i8 { fas_array_names[3][7] = 65; return fas_array_names[0][0] }\n";
  c_semantic_accept "c-import-array-parameter-adjustment" c_matrix
    "fn probe() i32 { return fas_array_parameter(&fas_array_global) }\n";
  c_semantic_accept "c-import-array-mutable-pointer-elements" c_matrix
    "fn probe() void { fas_array_pointer_elements[0] = null }\n";
  c_semantic_accept "c-import-record-array-global" c_matrix
    "fn probe() i32 { return fas_direct_record_array[0].field }\n";
  c_semantic_accept "c-import-typedef-record-array-global" c_matrix
    "fn probe() i32 { return fas_alias_record_array[0].field }\n";
  unsupported "fas_array_unknown" "arrays of unknown size are not supported"
    "v i32 = fas_array_unknown[0]";
  c_semantic_accept "c-import-float-global-array" c_matrix
    "fn probe() f32 { fas_array_float[2] = 1.5\nreturn fas_array_float[0] }\n";
  c_semantic_accept "c-import-float-globals" c_matrix
    "fn globals() f64 { fas_float_global = 2.5\nreturn fas_double_global }\n";
  List.iter
    (fun name ->
      c_semantic_message
        ("c-import-array-readonly-" ^ name)
        (Printf.sprintf "cannot modify read-only C array `%s`" name)
        c_matrix
        ("fn probe() void { " ^ name ^ "[0] = null }\n"))
    [ "fas_array_readonly_pointer_elements" ];
  List.iter
    (fun name ->
      c_semantic_message
        ("c-import-array-readonly-" ^ name)
        (Printf.sprintf "cannot modify read-only C array `%s`" name)
        c_matrix
        ("fn probe() void { " ^ name ^ "[0] = 1 }\n"))
    [ "fas_array_readonly"; "fas_array_readonly_alias" ];
  c_semantic_message "c-import-array-bounds"
    "array index `4` is out of bounds for length 4" c_matrix
    "fn probe() i32 { return fas_array_global[4] }\n";
  c_semantic_message "c-import-array-nested-bounds"
    "array index `8` is out of bounds for length 8" c_matrix
    "fn probe() i8 { return fas_array_names[0][8] }\n";
  c_semantic_message "c-import-array-readonly-view"
    "cannot modify read-only C array `fas_array_readonly` (through view `values`)"
    c_matrix "fn probe() void { view values = fas_array_readonly; values[0] = 1 }\n";
  c_semantic_message "c-import-array-readonly-copy"
    "cannot modify read-only C array `fas_array_readonly`" c_matrix
    "fn probe() void { copy(fas_array_readonly, fas_array_global) }\n";
  c_semantic_accept "c-import-array-readonly-pointer-target" c_matrix
    "fn probe() void { fas_array_readonly_pointer_elements[0][i32] = 1 }\n";
  c_semantic_accept "c-import-function-pointer-global-is-addr" c_matrix
    "fn read() addr { return fas_function_pointer }\n";
  c_semantic_message "c-import-function-pointer-global-not-callable"
    "unknown function `fas_function_pointer`" c_matrix
    "fn probe() i32 { return fas_function_pointer(1) }\n";
  c_semantic_accept "c-import-function-pointer-parameter-is-addr" c_matrix
    "fn probe() i32 { return fas_function_pointer_arg(null) }\n";
  c_semantic_accept "c-import-nested-function-pointer-is-addr" c_matrix
    "fn probe() i32 { return fas_function_pointer_nested(null) }\n";
  let raw_declarators = c_import_fixture "raw_declarators.h" in
  let raw_declarators_source, raw_declarators_imported = raw_declarators in
  c_semantic_accept "c-import-raw-function-pointer-declarators" raw_declarators
    "fn get_callback() addr { return fas_raw_get() }\n\
     fn call_raw(values addr) i32 {\n\
     return fas_raw_apply(fas_raw_get(), 1) + fas_raw_apply(fas_raw_global, 1) + \
     fas_raw_array(values) }\n\
     fn read_callback(value addr) addr {\n\
     return value[FasRawCallbackRecord].callback }\n";
  (match
     List.find_opt
       (function
         | Ast.Func { name = "fas_raw_get"; ret = Ast.Addr; _ } -> true | _ -> false)
       raw_declarators_imported.items
   with
  | Some _ -> ()
  | None -> failwith "raw C function-pointer result did not map to addr");
  (match
     List.find_opt
       (function
         | Ast.Global { name = "fas_raw_global"; ty = Ast.Addr; _ } -> true | _ -> false)
       raw_declarators_imported.items
   with
  | Some _ -> ()
  | None -> failwith "raw C function-pointer global did not map to addr");
  (match
     List.find_map
       (function
         | Ast.Struct { name = "FasRawCallbackRecord"; fields; _ } -> Some fields
         | _ -> None)
       raw_declarators_imported.items
   with
  | Some [ { Ast.name = "callback"; ty = Ast.Addr; _ } ] -> ()
  | _ -> failwith "raw C function-pointer field did not map to addr");
  List.iter
    (fun (name, expected) ->
      match
        List.find_opt
          (fun (static : C_import.static_function) -> static.name = name)
          raw_declarators_imported.static_functions
      with
      | None -> failwith ("raw C adapter source was not recorded for " ^ name)
      | Some static -> (
          match C_import.make_adapter ~occupied:[] raw_declarators_source static with
          | Ok adapter when contains adapter.code expected -> ()
          | Ok adapter ->
              failwith
                ("raw C adapter declarator missing for " ^ name ^ ": " ^ adapter.code)
          | Error message -> failwith message))
    [
      ("fas_raw_get", "fas_raw_get_result");
      ("fas_raw_apply", "int (*fas_arg0)(int)");
      ("fas_raw_array", "int (*fas_arg0)[4]");
    ];
  unsupported "fas_vector_value" "vector types are not supported by value"
    "fas_vector_value(null)";
  unsupported "fas_address_space" "C address spaces are not supported"
    "fas_address_space(null)";
  unsupported "fas_nondefault_abi" "non-default calling conventions are not supported"
    "fas_nondefault_abi(1)";
  c_semantic_accept "c-import-static-inline" c_matrix
    "fn static_inline_call() i32 { return fas_static_inline(4) }\n";
  c_semantic_accept "c-import-static-function-address" c_matrix
    "fn static_inline_address() addr { return &fas_static_inline }\n\
     const FasStaticInlineAddresses arr[1, addr] = {&fas_static_inline}\n";
  if
    not
      (List.exists
         (fun (static : C_import.static_function) ->
           static.name = "fas_static_inline" && not static.variadic)
         c_matrix_imported.static_functions)
  then failwith "static inline function was not recorded for adapter generation";
  c_semantic_accept "c-import-static-inline-handle" c_matrix
    "fn static_rect_call(rect handle[FasRect]) i32 { return fas_rect_empty(rect) }\n";
  let rect_adapter =
    match
      List.find_opt
        (fun (static : C_import.static_function) -> static.name = "fas_rect_empty")
        c_matrix_imported.static_functions
    with
    | Some static -> (
        match C_import.make_adapter ~occupied:[] c_matrix_source static with
        | Ok adapter -> adapter
        | Error message -> failwith message)
    | None -> failwith "static inline handle function was not recorded"
  in
  if
    not
      (contains rect_adapter.code "const struct FasRect * fas_arg0"
      && contains rect_adapter.code "return fas_rect_empty(fas_arg0);")
  then failwith "static inline handle adapter did not preserve its C signature";
  incr checks_run;
  if not (String.starts_with ~prefix:"__fas_c_adapter_" rect_adapter.symbol) then
    failwith "static adapter symbol omitted its fixed prefix";
  incr checks_run;
  (match
     List.find_opt
       (fun (static : C_import.static_function) -> static.name = "fas_rect_empty")
       c_matrix_imported.static_functions
   with
  | Some static -> (
      match
        C_import.make_adapter ~occupied:[ rect_adapter.symbol ] c_matrix_source static
      with
      | Ok adapter when adapter.symbol <> rect_adapter.symbol -> ()
      | Ok _ -> failwith "static adapter symbol collision was not avoided"
      | Error message -> failwith message)
  | None -> failwith "static inline handle function was not recorded");
  c_semantic_accept "c-import-static-float" c_matrix
    "fn probe(value f32) f32 { return fas_static_float(value) }\n";
  let static_collision_header = Filename.temp_file "fas-static-collision-" ".h" in
  Fun.protect
    ~finally:(fun () -> Sys.remove static_collision_header)
    (fun () ->
      let channel = open_out_bin static_collision_header in
      output_string channel
        "static int fas_shared_static(int value) { return value; }\n";
      close_out channel;
      let import_static source =
        let span =
          Span.make ~file:source ~start_offset:0 ~end_offset:0 ~line:1 ~column:1
        in
        let declarations, _, _ =
          expect_ok
            (C_import.import ~cc:"clang-22" ~keep:false source
               [ C_import.{ spelling = Ast.C_quoted static_collision_header; span } ])
        in
        C_import.map_declarations ~span declarations
      in
      let static_left =
        Filename.concat (Filename.dirname static_collision_header) "left.fas"
      and static_right =
        Filename.concat (Filename.dirname static_collision_header) "right.fas"
      in
      let collided =
        C_import.merge_imports [ import_static static_left; import_static static_right ]
      in
      c_semantic_message "c-import-static-name-collision"
        "C declaration `fas_shared_static` is not supported: conflicting C declarations"
        (static_left, collided) "fn call_static() i32 { return fas_shared_static(1) }\n");
  c_semantic_accept "c-import-anonymous-record-typedef-by-value" c_matrix
    "fn anonymous() i32 { value FasAnonymous = { 1 }\nreturn value.field }\n";
  let record_import_cases = c_import_fixture "record_import_cases.h" in
  let collision_header =
    Filename.concat (Filename.dirname (fst record_import_cases)) "record_import_cases.h"
  in
  let collision_source =
    Filename.concat (Filename.dirname collision_header) "probe.fas"
  in
  let collision_request =
    C_import.
      {
        spelling = Ast.C_quoted (Filename.basename collision_header);
        span = Span.synthetic;
      }
  in
  let collision_declarations, _, _ =
    expect_ok
      (C_import.import ~cc:"clang-22" ~keep:false collision_source [ collision_request ])
  in
  let rec duplicate_flattened_field_name active = function
    | C_import_json.Arr values ->
        C_import_json.Arr (List.map (duplicate_flattened_field_name active) values)
    | C_import_json.Obj fields as node ->
        let kind = Option.bind (C_import_json.field "kind" node) C_import_json.string in
        let target_record =
          kind = Some "RecordDecl"
          && C_import_json.field "name" node
             = Some (C_import_json.Str "FasAnonymousCollisionRecord")
        in
        let active = active || target_record in
        let fields =
          Array.map
            (fun (key, value) ->
              let value =
                if
                  key = "name" && active && kind = Some "FieldDecl"
                  && value = C_import_json.Str "second"
                then C_import_json.Str "first"
                else duplicate_flattened_field_name active value
              in
              (key, value))
            fields
        in
        C_import_json.Obj fields
    | value -> value
  in
  let collision_declarations =
    List.map (duplicate_flattened_field_name false) collision_declarations
  in
  let collision_imported =
    C_import.map_declarations ~span:Span.synthetic collision_declarations
  in
  let collision_fixture = (collision_source, collision_imported) in
  c_semantic_message "c-import-anonymous-member-name-collision"
    "C declaration `FasAnonymousCollisionRecord` is not supported: anonymous member \
     field names collide"
    collision_fixture
    "fn read(value FasAnonymousCollisionRecord) i32 { return value.first }\n";
  let union_container =
    c_import_container "union-container"
      "struct FasContainerNested { unsigned short first; unsigned int second; };\n\
       union FasContainerUnion { unsigned int word; unsigned char bytes[8];\n\
       struct FasContainerNested nested; };\n"
  in
  let time_record_handles = c_import_fixture "time_record_handles.h" in
  let pointer_enum_types = c_import_fixture "pointer_enum_macro_types.h" in
  let pointer_enum_aliases = snd pointer_enum_types in
  let require_c_alias name expected =
    incr checks_run;
    match List.assoc_opt name pointer_enum_aliases.aliases with
    | Some actual when actual = expected -> ()
    | Some actual ->
        failwith
          (Printf.sprintf "C alias %s: expected %s, got %s" name
             (Ast.type_name expected) (Ast.type_name actual))
    | None -> failwith ("C alias was not imported: " ^ name)
  in
  require_c_alias "PointerRecordPointer"
    (Ast.Handle (Ast.Named_type ("PointerRecord", Span.synthetic)));
  require_c_alias "PointerUnionPointer"
    (Ast.Handle (Ast.Named_type ("PointerUnion", Span.synthetic)));
  require_c_alias "AnonymousRecordPointer" Ast.Addr;
  require_c_alias "PointerCollision" Ast.Addr;
  require_c_alias "IncompletePointer"
    (Ast.Handle (Ast.Named_type ("IncompletePointerTarget", Span.synthetic)));
  c_semantic_accept "c-import-record-pointer-typedefs" pointer_enum_types
    "fn pointers() bool { return pointer_record_value(null) == 0 && \
     pointer_union_value(null) == 0 && anonymous_pointer_is_null(null) != 0 && \
     pointer_collision_value(null) == 0 && incomplete_pointer() == null }\n";
  c_semantic_accept "c-import-anonymous-record-pointer-fields" pointer_enum_types
    "fn pointers() bool { return anonymous_pointer_fields.pointer == null && \
     anonymous_pointer_fields.pointers[0] == null && \
     anonymous_pointer_fields.union_pointer == null }\n";
  c_semantic_accept "c-import-anonymous-record-pointer-parameter" pointer_enum_types
    "fn read(value addr) i32 { return read_anonymous_parameter(value) }\n";
  c_semantic_accept "c-import-anonymous-enum-values" pointer_enum_types
    "fn values() bool { return anonymous_enum_global == 42 && \
     anonymous_enum_field.kind == AnonymousFieldHigh }\n";
  c_semantic_accept "c-import-float-field-offset" pointer_enum_types
    "const ValuesOffset usize = offsetof[OffsetFloatStorage, values]\n\
     const TailOffset usize = offsetof[OffsetFloatStorage, tail]\n";
  c_semantic_accept "c-import-record-address-handle-contexts" time_record_handles
    "fn read_clock() i32 { ts timespec\n\
     return clock_gettime(1, &ts) }\n\
     fn assign_handle() handle[timespec] {\n\
     ts timespec\n\
     result handle[timespec] = null\n\
     result = &ts\n\
     return result }\n\
     fn init_handle() handle[timespec] {\n\
     ts timespec\n\
     result handle[timespec] = &ts\n\
     return result }\n\
     fn return_handle() handle[timespec] { ts timespec\n\
     return &ts }\n\
     fn peer_handle(other handle[timespec]) bool {\n\
     ts timespec\n\
     return (&ts == other) && (other == &ts) }\n\
     struct NativeTimespec { value i32 }\n\
     fn address_default() addr { return addr_from_bits(0) }\n\
     var NativeTimespecStorage NativeTimespec\n\
     fn native_address() addr { return &NativeTimespecStorage }\n";
  c_semantic_message "c-import-record-address-different-handle"
    "argument 2 of `clock_gettime` is `addr` to `FasOtherTimespec`, expected \
     `handle[timespec]`"
    time_record_handles
    "fn wrong_record() i32 { ts FasOtherTimespec\nreturn clock_gettime(1, &ts) }\n";
  let record_import = snd record_import_cases in
  let imported_struct name =
    List.find_map
      (function
        | Ast.Struct { name = found; fields; align; _ } when found = name ->
            Some (fields, align)
        | _ -> None)
      record_import.items
  in
  let require_struct name =
    match imported_struct name with
    | Some result -> result
    | None -> failwith ("missing admitted C record " ^ name)
  in
  let tag_fields, _ = require_struct "FasTagRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) tag_fields
    <> [ ("first", Ast.Int Ast.I32); ("second", Ast.Int Ast.U8) ]
  then failwith "C tag record fields were not imported in order";
  let const_pointer_fields, _ = require_struct "FasConstPointerRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) const_pointer_fields
    <> [ ("value", Ast.Addr) ]
  then failwith "const pointee field was not admitted";
  let anonymous_fields, _ = require_struct "FasAnonymousRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) anonymous_fields
    <> [ ("byte", Ast.Int Ast.U8); ("word", Ast.Int Ast.I32) ]
  then failwith "anonymous C typedef record fields were not imported";
  let alias_fields, _ = require_struct "FasAliasRecord" in
  if
    List.length alias_fields <> 1
    || List.length
         (List.filter
            (function Ast.Struct { name = "FasAliasRecord"; _ } -> true | _ -> false)
            record_import.items)
       <> 1
    || List.assoc_opt "FasAlias" record_import.aliases
       <> Some (Ast.Named_type ("FasAliasRecord", Span.synthetic))
  then failwith "tag and typedef did not preserve one C record identity";
  let nested_fields, _ = require_struct "FasNestedRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) nested_fields
    <> [
         ("inner", Ast.Named_type ("FasInnerRecord", Span.synthetic));
         ( "values",
           Ast.Array
             ( Ast.aggregate_length (Ast.Int_lit ("2", Span.synthetic)) Span.synthetic,
               Ast.Int Ast.I32 ) );
       ]
  then failwith "nested record or array field type was not imported";
  let self_fields, _ = require_struct "FasSelfRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) self_fields
    <> [
         ("next", Ast.Handle (Ast.Named_type ("FasSelfRecord", Span.synthetic)));
         ("value", Ast.Int Ast.I32);
       ]
  then failwith "self-referential record pointer did not map to a handle";
  let _, aligned = require_struct "FasAlignedRecord" in
  if aligned <> Some 16 then failwith "aligned C record did not map to @align(16)";
  List.iter
    (fun (name, reason) ->
      if
        imported_struct name <> None
        || (not (List.mem_assoc name record_import.unsupported))
        || List.assoc name record_import.unsupported <> reason
      then failwith ("unsupported C record reason was not retained for " ^ name))
    [
      ("FasBitfieldRecord", "bit-fields are not supported");
      ("FasFlexibleRecord", "flexible array members are not supported");
      ("FasAnonymousUnsupportedRecord", "transparent unions are not supported");
      ("FasConstFieldRecord", "const fields are not supported");
      ("FasNestedConstFieldRecord", "const fields are not supported");
    ];
  (match c_semantic_result record_import_cases "fn noop() void { return }\n" with
  | Ok program ->
      let layout name =
        match
          List.find_opt
            (fun (record : Hir.struct_def) -> record.name = name)
            program.structs
        with
        | Some record -> record
        | None -> failwith ("missing checked record layout " ^ name)
      in
      List.iter
        (fun (name, size, align, offsets) ->
          let record = layout name in
          if
            record.size <> size || record.align <> align
            || List.map
                 (fun (field : Hir.field) -> (field.name, field.offset))
                 record.fields
               <> offsets
          then failwith ("unexpected imported record layout for " ^ name))
        [
          ("FasTagRecord", 8, 4, [ ("first", 0); ("second", 4) ]);
          ("FasPackedRecord", 5, 1, [ ("byte", 0); ("word", 1) ]);
          ("FasAnonymousRecord", 8, 4, [ ("byte", 0); ("word", 4) ]);
          ("FasNestedRecord", 16, 4, [ ("inner", 0); ("values", 8) ]);
          ("FasSelfRecord", 16, 8, [ ("next", 0); ("value", 8) ]);
          ("FasAlignedRecord", 16, 16, [ ("value", 0) ]);
          ("FasUnionRecord", 4, 4, [ ("value", 0); ("byte", 0) ]);
          ("FasAnonymousMemberRecord", 4, 4, [ ("integer", 0); ("byte", 0) ]);
          ( "FasAnonymousNestedRecord",
            12,
            4,
            [ ("outer", 0); ("nested", 4); ("raw", 4); ("tail", 8) ] );
          ("FasAnonymousStructUnion", 4, 4, [ ("low", 0); ("high", 2); ("word", 0) ]);
          ("FasFloatRecord", 12, 4, [ ("before", 0); ("value", 4); ("after", 8) ]);
          ("FasNestedFloatRecord", 20, 4, [ ("before", 0); ("inner", 4); ("after", 16) ]);
          ("FasFloatArrayRecord", 64, 8, [ ("before", 0); ("data", 8); ("after", 56) ]);
        ];
      if not (layout "FasUnionRecord").is_union then
        failwith "imported union lost its union layout"
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics));
  let record_manifest = C_import.manifest_text record_import in
  if
    not
      (contains record_manifest
         "FasNestedRecord\tstruct FasNestedRecord\tstruct FasNestedRecord {inner \
          FasInnerRecord, values arr[2, i32]}")
  then failwith "C record manifest did not list admitted fields";
  if
    not
      (contains record_manifest
         "FasUnionRecord\tunion FasUnionRecord\tunion FasUnionRecord size=4 align=4 \
          {value i32 @0, byte u8 @0}")
  then failwith "C union manifest did not list its layout and fields";
  let callback_fields, _ = require_struct "FasFunctionPointerRecord" in
  if
    List.map (fun (field : Ast.field) -> (field.name, field.ty)) callback_fields
    <> [ ("callback", Ast.Addr) ]
  then failwith "C function-pointer field did not map to addr";
  if List.assoc_opt "FasCallback" record_import.aliases <> Some Ast.Addr then
    failwith "C function-pointer typedef did not map to addr";
  let callback_function name =
    List.find_map
      (function
        | Ast.Func { name = found; params; ret; _ } when found = name ->
            Some (params, ret)
        | _ -> None)
      record_import.items
  in
  (match callback_function "fas_callback_parameter" with
  | Some ([ { ty = Ast.Addr; _ } ], Ast.Int Ast.I32) -> ()
  | _ -> failwith "C function-pointer parameter did not map to addr");
  (match callback_function "fas_callback_result" with
  | Some ([], Ast.Addr) -> ()
  | _ -> failwith "C function-pointer result did not map to addr");
  (match
     List.find_opt
       (function
         | Ast.Global { name = "fas_callback_global"; ty = Ast.Addr; _ } -> true
         | _ -> false)
       record_import.items
   with
  | Some _ -> ()
  | None -> failwith "C function-pointer global did not map to addr");
  let callback_manifest =
    record_manifest |> String.split_on_char '\n'
    |> List.find_opt (String.starts_with ~prefix:"FasFunctionPointerRecord\t")
  in
  (match callback_manifest with
  | Some line when contains line "callback addr (C int (*)(int))" -> ()
  | _ -> failwith "C manifest omitted the function-pointer field signature");
  c_semantic_accept "c-import-record-handle-and-field-access" record_import_cases
    "fn read(value handle[FasSelfRecord]) i32 {\n\
     return handle_addr(value)[FasSelfRecord].value }\n\
     fn cast(pointer addr) handle[FasTagRecord] {\n\
     return handle_from_addr[FasTagRecord](pointer) }\n";
  c_semantic_accept "c-import-view-through-complete-handle" record_import_cases
    "fn fields(value handle[FasNestedRecord]) i32 {\n\
     view r = value\n\
     r.inner.right = 17\n\
     r.values[1] = 9\n\
     return r.inner.right + r.values[1] }\n";
  let handle_view_ir text =
    match c_semantic_result record_import_cases text with
    | Ok program -> Ir.render (Lower.lower program |> expect_ok)
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  let handle_view_shorthand =
    handle_view_ir
      "fn pinned(value handle[FasNestedRecord]) i32 {\n\
       view r = value\n\
       r.inner.right = 17\n\
       return r.inner.right }\n"
  and handle_view_explicit =
    handle_view_ir
      "fn pinned(value handle[FasNestedRecord]) i32 {\n\
       view r = handle_addr(value)[FasNestedRecord]\n\
       r.inner.right = 17\n\
       return r.inner.right }\n"
  in
  if handle_view_shorthand <> handle_view_explicit then
    failwith "c-import-view-through-handle: LLVM differs from explicit handle_addr view";
  c_semantic_message "c-import-view-through-opaque-handle"
    "cannot view `Token` through a handle: its layout is unknown" record_import_cases
    "opaque Token\nfn probe(value handle[Token]) void { view r = value\n return }\n";
  c_semantic_message "c-import-view-through-null-handle" "access through null address"
    record_import_cases
    "fn probe() i32 { value handle[FasNestedRecord] = null\n\
     view r = value\n\
     return r.inner.right }\n";
  c_semantic_message "c-import-raw-record-through-null-address"
    "access through null address" record_import_cases
    "fn probe() i32 { value addr = null\nreturn value[FasNestedRecord].inner.right }\n";
  c_semantic_accept "c-import-view-through-nonnull-handle" record_import_cases
    "fn probe() i32 { record FasNestedRecord\n\
     record.inner.right = 29\n\
     value handle[FasNestedRecord] = &record\n\
     view r = value\n\
     return r.inner.right }\n";
  c_semantic_accept "c-import-record-fas-field-names" record_import_cases
    "fn fields(value handle[FasFieldNamesRecord]) i32 {\n\
     raw addr = handle_addr(value)\n\
     raw[FasFieldNamesRecord].handle = 1\n\
     raw[FasFieldNamesRecord].len = 2\n\
     raw[FasFieldNamesRecord].view = 3\n\
     raw[FasFieldNamesRecord].i32 = 4\n\
     return raw[FasFieldNamesRecord].handle + raw[FasFieldNamesRecord].len + \
     raw[FasFieldNamesRecord].view + raw[FasFieldNamesRecord].i32 }\n";
  c_semantic_accept "c-import-record-typedef-global-initializer" record_import_cases
    "var AliasValue FasAlias = {7}\n";
  c_semantic_accept "address-constants-imported-record-handles" record_import_cases
    "var Direct handle[FasSelfRecord] = &fas_address_self\n\
     const Nested arr[1,arr[1,handle[FasInnerRecord]]] = {{&fas_address_nested.inner}}\n\
     var Indexed arr[1,handle[FasSelfRecord]] = {&fas_address_self_array[1]}\n";
  c_semantic_message "address-constants-different-imported-record"
    (Printf.sprintf "constant initializer has type `%s`, expected `%s`" "addr"
       "handle[FasTagRecord]")
    record_import_cases "var P handle[FasTagRecord] = &fas_address_self\n";
  c_semantic_message "address-constants-native-record-handle"
    "constant initializer has type `addr`, expected `handle[FasSelfRecord]`"
    record_import_cases
    "struct NativeAddressRecord { value i32 }\n\
     var G NativeAddressRecord = {1}\n\
     var P handle[FasSelfRecord] = &G\n";
  c_semantic_message "address-constants-scalar-record-handle"
    "constant initializer has type `addr`, expected `handle[FasSelfRecord]`"
    record_import_cases "var G i32 = 1\nvar P handle[FasSelfRecord] = &G\n";
  let imported_record_addresses =
    match
      c_semantic_result record_import_cases
        "var Direct handle[FasSelfRecord] = &fas_address_self\n\
         const Nested arr[1,arr[1,handle[FasInnerRecord]]] = \
         {{&fas_address_nested.inner}}\n\
         var Indexed arr[1,handle[FasSelfRecord]] = {&fas_address_self_array[1]}\n"
    with
    | Ok program -> Ir.render (expect_ok (Lower.lower program))
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  if
    (not (contains imported_record_addresses "@fas_address_self"))
    || (not (contains imported_record_addresses "@fas_address_nested"))
    || (not (contains imported_record_addresses "@fas_address_self_array"))
    || (not
          (contains imported_record_addresses
             "getelementptr (i8, ptr @fas_address_self_array, i64 16)"))
    || contains imported_record_addresses "inbounds"
  then failwith "address-constants-imported-record-handles: invalid relocation";
  c_semantic_accept "c-import-unsupported-record-remains-handle" record_import_cases
    "fn retain(value handle[FasUnionRecord]) handle[FasUnionRecord] { return value }\n";
  c_semantic_accept "c-import-packed-record-layout" record_import_cases
    "fn read(value handle[FasPackedRecord]) i32 {\n\
     return handle_addr(value)[FasPackedRecord].word }\n";
  c_semantic_accept "c-import-trailing-record-attributes" c_record_attributes
    "fn packed_layout() usize { return sizeof[FasPackedTrailing] + \
     alignof[FasPackedTrailing] + offsetof[FasPackedTrailing,value] }\n\
     fn aligned_layout() usize { return sizeof[FasAlignedTrailing] + \
     alignof[FasAlignedTrailing] + offsetof[FasAlignedTrailing,value] }\n\
     fn typedef_layout() usize { return sizeof[FasAlignedTypedef] + \
     alignof[FasAlignedTypedef] + offsetof[FasAlignedTypedef,bytes] }\n";
  c_semantic_message "c-import-overaligned-enum"
    "C declaration `FasAlignedEnum` is not supported: over-aligned C enums are not \
     supported"
    c_record_attributes "fn enum_layout() usize { return alignof[FasAlignedEnum] }\n";
  c_semantic_accept "c-import-union-record" record_import_cases
    "fn read(value handle[FasUnionRecord]) i32 {\n\
     return handle_addr(value)[FasUnionRecord].value }\n\
     fn local() i32 { value FasUnionRecord = {7}\n\
    \ return value.value }\n\
     fn return_handle() handle[FasUnionRecord] {\n\
     value FasUnionRecord = {1}\n\
    \ return &value }\n\
     fn via_view(value handle[FasUnionRecord]) i32 {\n\
     view record = handle_addr(value)[FasUnionRecord]\n\
     record.value = 9\n\
    \ return record.value }\n\
     fn pass_address() void { value FasUnionRecord = {1}\n\
     fas_union_pointer(&value)\n\
    \ return }\n\
     fn c_handle_result() handle[FasUnionRecord] {\n\
     return fas_union_pointer_result() }\n\
     var union_global FasUnionRecord = {3}\n\
     const union_constant FasUnionRecord = {4}\n";
  let container_union_program =
    match
      c_semantic_result union_container
        "fn nested(value handle[FasContainerUnion]) u32 {\n\
         raw addr = handle_addr(value)\n\
         raw[FasContainerUnion].nested.second = 17\n\
         return raw[FasContainerUnion].nested.second }\n\
         const initial FasContainerUnion = {29}\n"
    with
    | Ok program -> program
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  let container_union_layout =
    List.find_opt
      (fun (record : Hir.struct_def) -> record.name = "FasContainerUnion")
      container_union_program.structs
    |> Option.get
  in
  if
    container_union_layout.size <> 8
    || container_union_layout.align <> 4
    || (not container_union_layout.is_union)
    || List.map
         (fun (field : Hir.field) -> (field.name, field.offset))
         container_union_layout.fields
       <> [ ("word", 0); ("bytes", 0); ("nested", 0) ]
  then failwith "container union layout mismatch";
  ignore (expect_ok (Lower.lower container_union_program));
  if
    not
      (contains
         (C_import.manifest_text (snd union_container))
         "FasContainerUnion\tunion FasContainerUnion\tunion FasContainerUnion size=8 \
          align=4 {word u32 @0, bytes arr[8, u8] @0, nested FasContainerNested @0}")
  then failwith "container union manifest omitted its layout or fields";
  c_semantic_message "c-import-union-by-value-parameter"
    "argument 1 of `fas_union_by_value` passes `FasUnionRecord` by value; pass a \
     variable, field or array element of type `FasUnionRecord`"
    record_import_cases "fn probe() i32 { return fas_union_by_value(null) }\n";
  c_semantic_accept "c-record-value-union-argument" record_import_cases
    "fn probe() i32 {\nu FasUnionRecord\nu.value = 2\nreturn fas_union_by_value(u) }\n";
  c_semantic_accept "c-import-float-union-field" record_import_cases
    "fn read_bits(value handle[FasFloatUnion]) u32 {\n\
     raw addr = handle_addr(value)\n\
     raw[FasFloatUnion].bits = 9\n\
     return raw[FasFloatUnion].bits }\n";
  c_semantic_accept "c-import-float-union-member" record_import_cases
    "fn read_float(value handle[FasFloatUnion]) f32 {\n\
     raw addr = handle_addr(value)\n\
     raw[FasFloatUnion].value = 3.5\n\
     return raw[FasFloatUnion].value }\n";
  c_semantic_message "c-import-bitfield-union-member"
    "C declaration `FasBitfieldUnion` is not supported: bit-fields are not supported"
    record_import_cases
    "fn read_bits(value FasBitfieldUnion) u32 { return value.word }\n";
  parse_message "native-union-declaration" "expected a top-level item, found `union`"
    "union NativeUnion { value i32 }\n";
  c_semantic_message "c-import-bitfield-record-reason"
    "C declaration `FasBitfieldRecord` is not supported: bit-fields are not supported"
    record_import_cases "fn read(value FasBitfieldRecord) i32 { return value.value }\n";
  c_semantic_message "c-import-flexible-record-reason"
    "C declaration `FasFlexibleRecord` is not supported: flexible array members are \
     not supported"
    record_import_cases "fn read(value FasFlexibleRecord) i32 { return value.length }\n";
  c_semantic_accept "c-import-anonymous-members" record_import_cases
    "fn write(value handle[FasAnonymousMemberRecord]) i32 {\n\
     raw addr = handle_addr(value)\n\
     raw[FasAnonymousMemberRecord].integer = 18\n\
     raw[FasAnonymousMemberRecord].byte = 7\n\
     return raw[FasAnonymousMemberRecord].integer }\n\
     fn nested(value handle[FasAnonymousNestedRecord]) u32 {\n\
     raw addr = handle_addr(value)\n\
     raw[FasAnonymousNestedRecord].nested = 21\n\
     raw[FasAnonymousNestedRecord].tail = 8\n\
     return raw[FasAnonymousNestedRecord].nested + raw[FasAnonymousNestedRecord].tail }\n\
     fn union_struct(value handle[FasAnonymousStructUnion]) u16 {\n\
     raw addr = handle_addr(value)\n\
     raw[FasAnonymousStructUnion].high = 12\n\
     return raw[FasAnonymousStructUnion].high }\n";
  c_semantic_message "c-import-anonymous-unsupported-reason"
    "C declaration `FasAnonymousUnsupportedRecord` is not supported: transparent \
     unions are not supported"
    record_import_cases
    "fn read(value FasAnonymousUnsupportedRecord) i32 { return value.value }\n";
  c_semantic_accept "c-import-float-fields" record_import_cases
    "fn read(value handle[FasFloatRecord]) f32 {\n\
     raw addr = handle_addr(value)\n\
     return raw[FasFloatRecord].value }\n\
     fn write(value handle[FasFloatRecord]) void {\n\
     raw addr = handle_addr(value)\n\
     raw[FasFloatRecord].value = 1.25\n\
     return }\n\
     fn address(value handle[FasFloatRecord]) addr {\n\
     raw addr = handle_addr(value)\n\
     return &raw[FasFloatRecord].value }\n\
     fn nested(value handle[FasNestedFloatRecord]) f32 {\n\
     raw addr = handle_addr(value)\n\
     return raw[FasNestedFloatRecord].inner.value }\n\
     fn array_element(value handle[FasFloatArrayRecord], index usize) f64 {\n\
     raw addr = handle_addr(value)\n\
     return raw[FasFloatArrayRecord].data[index] }\n\
     fn initialize() f32 { value FasFloatRecord = {1, 2.5, 3}\n\
     return value.value }\n";
  c_semantic_accept "c-import-empty-zero-union" record_import_cases
    "fn zero() i32 { value FasUnionRecord = {}\nreturn value.value }\n";
  c_semantic_accept "c-import-empty-zero-float-record" record_import_cases
    "fn zero() i32 { value FasFloatRecord = {}\nreturn value.after }\n";
  c_semantic_accept "c-import-empty-zero-union-global" record_import_cases
    "extern \"C\" { var empty_union FasUnionRecord = {} }\n";
  c_semantic_accept "c-import-empty-zero-float-record-global" record_import_cases
    "extern \"C\" { var empty_float FasFloatRecord = {} }\n";
  c_semantic_accept "c-import-empty-zero-nested-record-array-global" record_import_cases
    "extern \"C\" { var empty_records arr[4,FasNestedRecord] = {} }\n";
  List.iter
    (fun (name, field) ->
      c_semantic_message
        ("c-import-unsupported-float-field-" ^ name)
        (Printf.sprintf
           "C declaration `%s` is not supported: floating-point types are not supported"
           name)
        record_import_cases
        (Printf.sprintf "fn probe(value %s) i32 { return value.%s }\n" name field))
    [
      ("FasLongDoubleRecord", "value");
      ("FasComplexRecord", "value");
      ("FasFloat16Record", "value");
      ("FasFp16Record", "value");
      ("FasFloat128Record", "value");
    ];
  let imported_float_llvm =
    match
      c_semantic_result record_import_cases
        "fn read(value handle[FasFloatRecord]) f32 {\n\
         raw addr = handle_addr(value)\n\
         raw[FasFloatRecord].value = 2.5\n\
         return raw[FasFloatRecord].value }\n\
         fn read_wide(value handle[FasFloatArrayRecord]) f64 {\n\
         raw addr = handle_addr(value)\n\
         return raw[FasFloatArrayRecord].data[0] }\n"
    with
    | Ok program -> Ir.render (expect_ok (Lower.lower program))
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  if not (contains imported_float_llvm "float" && contains imported_float_llvm "double")
  then failwith "imported C float fields omitted LLVM floating-point types";
  c_semantic_accept "c-import-function-pointer-field-record" record_import_cases
    "fn read(value addr) addr { return value[FasFunctionPointerRecord].callback }\n";
  let keyword_fields, _ = require_struct "FasKeywordFieldRecord" in
  if
    List.map (fun (field : Ast.field) -> field.name) keyword_fields
    <> [ "opaque"; "fn"; "var" ]
  then failwith "C keyword-named fields were not imported";
  c_semantic_accept "c-import-keyword-field-record-access" record_import_cases
    "fn fields(raw addr) i32 {\n\
     raw[FasKeywordFieldRecord].opaque = 1\n\
     raw[FasKeywordFieldRecord].fn = 2\n\
     raw[FasKeywordFieldRecord].var = 3\n\
     return raw[FasKeywordFieldRecord].opaque + raw[FasKeywordFieldRecord].fn + \
     raw[FasKeywordFieldRecord].var }\n";
  c_semantic_accept "c-import-keyword-field-record-remains-handle" record_import_cases
    "fn retain(value handle[FasKeywordFieldRecord]) handle[FasKeywordFieldRecord] { \
     return value }\n";
  let function_addresses =
    "extern \"C\" {\n\
     fn fas_address_c_callback(value i32) i32 { return value }\n\
     fn fas_address_c_declaration() i32 }\n\
     fn fas_return_imported_address() addr { return &fas_callback_result }\n\
     fn fas_return_exported_address() addr { return &fas_address_c_callback }\n\
     fn fas_return_declared_address() addr { return &fas_address_c_declaration }\n\
     const FasCallbacks arr[1,FasFunctionPointerRecord] = {{&fas_callback_result}}\n"
  in
  let function_address_ir =
    match c_semantic_result record_import_cases function_addresses with
    | Ok program -> Ir.render (expect_ok (Lower.lower program))
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  if
    (not (contains function_address_ir "@fas_callback_result"))
    || (not (contains function_address_ir "@fas_address_c_callback"))
    || (not (contains function_address_ir "@fas_address_c_declaration"))
    || (not (contains function_address_ir "ptr @fas_callback_result"))
    || contains function_address_ir "getelementptr (i8, ptr @fas_callback_result"
    || contains function_address_ir "dso_local"
  then failwith "C function addresses did not lower to plain function relocations";
  semantic_error "addr-handle-c-record-native-still-rejected"
    "handle type argument must be an opaque type"
    "struct NativeRecord { value i32 }\n\
     fn f(pointer addr) handle[NativeRecord] {\n\
     return handle_from_addr[NativeRecord](pointer) }\n";
  let definition_header = Filename.temp_file "fas-import-definition-" ".h" in
  let definition_source =
    Filename.concat (Filename.dirname definition_header) "import-definition.fas"
  in
  Fun.protect
    ~finally:(fun () -> Sys.remove definition_header)
    (fun () ->
      let output = open_out_bin definition_header in
      output_string output
        "extern int fas_imported_global;\n\
         int fas_imported_function(int value);\n\
         extern int fas_incomplete[];\n\
         typedef struct FasIncompleteNamedRecord { int value; } \
         FasIncompleteNamedRecord;\n\
         extern FasIncompleteNamedRecord fas_incomplete_named[];\n";
      close_out output;
      let imported =
        let span =
          Span.make ~file:definition_source ~start_offset:0 ~end_offset:0 ~line:1
            ~column:1
        in
        let declarations, _, _ =
          expect_ok
            (C_import.import ~cc:"clang-22" ~keep:false definition_source
               [ C_import.{ spelling = Ast.C_quoted definition_header; span } ])
        in
        C_import.map_declarations ~span declarations
      in
      let definitions =
        parse_file definition_source
          "extern \"C\" { var fas_imported_global i32 = 7\n\
           fn fas_imported_function(value i32) i32 { return value }\n\
           var fas_incomplete arr[3,i32] = {1,2,3}\n\
           var fas_incomplete_named arr[3,FasIncompleteNamedRecord] = {{1},{2},{3}} }\n"
      in
      let reconciled =
        expect_ok (C_import.reconcile_source definitions.items imported)
      in
      ignore
        (expect_ok (Sema.check { Ast.items = definitions.items @ reconciled.items }));
      if
        List.exists
          (function
            | Ast.Global { name; _ } | Ast.Func { name; _ } ->
                List.mem name [ "fas_imported_global"; "fas_imported_function" ]
            | _ -> false)
          reconciled.items
        || List.mem_assoc "fas_incomplete" reconciled.unsupported
        || List.mem_assoc "fas_incomplete_named" reconciled.unsupported
      then failwith "matching C definitions retained conflicting imports";
      let mismatching =
        parse_file definition_source
          "extern \"C\" { fn fas_imported_function(value u32) i32 {return 0 } }\n"
      in
      (match C_import.reconcile_source mismatching.items imported with
      | Error [ diagnostic ]
        when diagnostic.message
             = "C declaration `fas_imported_function` has type `fn(i32)->i32`, but Fas \
                declares `fn(u32)->i32`" ->
          ()
      | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      | Ok _ -> failwith "mismatching imported function definition was accepted");
      let wrong_element =
        parse_file definition_source
          "extern \"C\" { var fas_incomplete arr[3,u32] = {1,2,3} }\n"
      in
      match C_import.reconcile_source wrong_element.items imported with
      | Error [ diagnostic ]
        when diagnostic.message
             = "C declaration `fas_incomplete` has type `arr[?, i32]`, but Fas \
                declares `arr[?, u32]`" ->
          ()
      | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
      | Ok _ -> failwith "incomplete array with the wrong element type was accepted");
  c_semantic_message "c-import-reserved-name"
    "C declaration `addr` is not supported: name is a Fas type name; call it through a \
     C container function with another name"
    c_matrix "fn probe() i32 { return addr(1) }\n";
  c_semantic_message "c-import-macro-is-foreign-only" "unknown name `FAS_MACRO_ONLY`"
    c_matrix "fn probe() i32 { return FAS_MACRO_ONLY }\n";
  let c_macros =
    c_import_macro_fixture "macros.h"
      [
        "EOF";
        "SEEK_END";
        "FAS_MACRO_UHEX";
        "FAS_MACRO_LONG";
        "FAS_MACRO_ENUM";
        "FAS_MACRO_CHAIN";
        "FAS_MACRO_STRING";
        "FAS_MACRO_FUNCTION";
        "FAS_MACRO_ERRNO";
        "stdout";
        "FAS_MACRO_POINTER";
        "FAS_MACRO_EMPTY";
        "addr";
      ]
  in
  let machine_size_macros =
    c_import_macro_fixture "pointer_enum_macro_types.h"
      [
        "SIZE_T_MACRO";
        "PTRDIFF_MACRO";
        "UINTPTR_MACRO";
        "INTPTR_MACRO";
        "CUSTOM_SIZE_MACRO";
        "SIZE_MAX";
        "INT64_MAX";
      ]
  in
  let machine_size_program =
    match c_semantic_result machine_size_macros "fn probe() i32 { return 0 }\n" with
    | Ok program -> program
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  List.iter
    (fun (name, expected_ty, expected_bits) ->
      incr checks_run;
      match
        List.find_opt
          (fun (constant : Hir.const_def) -> constant.name = name)
          machine_size_program.consts
      with
      | Some { ty; bits; _ } when ty = expected_ty && bits = expected_bits -> ()
      | Some { ty; bits; _ } ->
          failwith
            (Printf.sprintf "C macro %s: expected %s %Ld, got %s %Ld" name
               (Hir.ty_name expected_ty) expected_bits (Hir.ty_name ty) bits)
      | None -> failwith ("C macro was not imported: " ^ name))
    [
      ("SIZE_T_MACRO", Hir.Int Hir.Usize, 4L);
      ("PTRDIFF_MACRO", Hir.Int Hir.Isize, -5L);
      ("UINTPTR_MACRO", Hir.Int Hir.Usize, 7L);
      ("INTPTR_MACRO", Hir.Int Hir.Isize, -6L);
      ("CUSTOM_SIZE_MACRO", Hir.Int Hir.Usize, 4L);
      ("SIZE_MAX", Hir.Int Hir.U64, -1L);
      ("INT64_MAX", Hir.Int Hir.I64, Int64.max_int);
    ];
  let macro_program =
    match c_semantic_result c_macros "fn macro_probe() i32 { return 0 }\n" with
    | Ok program -> program
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  List.iter
    (fun (name, ty, bits) ->
      incr checks_run;
      let bits = if bits < 0L then Int64.logand bits 0xffffffffL else bits in
      match
        List.find_opt
          (fun (constant : Hir.const_def) -> constant.name = name)
          macro_program.consts
      with
      | Some { ty = actual; bits = actual_bits; _ }
        when actual = ty && actual_bits = bits ->
          ()
      | Some { ty = actual; bits = actual_bits; _ } ->
          failwith
            (Printf.sprintf "C macro %s: expected %s %Ld, got %s %Ld" name
               (Hir.ty_name ty) bits (Hir.ty_name actual) actual_bits)
      | None -> failwith ("C integer macro was not imported: " ^ name))
    [
      ("EOF", Hir.Int Hir.I32, -1L);
      ("SEEK_END", Hir.Int Hir.I32, 2L);
      ("FAS_MACRO_UHEX", Hir.Int Hir.U32, 3735928559L);
      ("FAS_MACRO_LONG", Hir.Int Hir.I64, 19L);
      ("FAS_MACRO_ENUM", Hir.Int Hir.I32, 37L);
      ("FAS_MACRO_CHAIN", Hir.Int Hir.I32, 41L);
    ];
  if
    not
      (contains
         (C_import.manifest_text (snd c_macros))
         "FAS_MACRO_UHEX\tmacro FAS_MACRO_UHEX\tmacro u32 3735928559")
  then failwith "integer macro was omitted from the C manifest";
  List.iter
    (fun name ->
      c_semantic_message
        ("c-import-invisible-macro-" ^ name)
        ("unknown name `" ^ name ^ "`")
        c_macros
        ("fn probe() i32 { return " ^ name ^ " }\n"))
    [
      "FAS_MACRO_STRING";
      "FAS_MACRO_FUNCTION";
      "FAS_MACRO_ERRNO";
      "FAS_MACRO_POINTER";
      "FAS_MACRO_EMPTY";
    ];
  let undeclared_macros =
    c_import_macro_fixture "macro_undeclared.h" [ "FAS_BAD_INT"; "FAS_BAD_FLOAT" ]
  in
  c_semantic_message "c-macro-undeclared-integer-is-invisible"
    "unknown name `FAS_BAD_INT`" undeclared_macros
    "fn probe() i32 { return FAS_BAD_INT }\n";
  c_semantic_message "c-macro-undeclared-float-is-invisible"
    "unknown name `FAS_BAD_FLOAT`" undeclared_macros
    "fn probe() f64 { return FAS_BAD_FLOAT }\n";
  let vector_pointer = c_import_macro_fixture "vector_pointer.h" [] in
  c_semantic_accept "c-vector-pointer-is-address" vector_pointer
    "fn probe(value addr) addr { return fas_vector_pointer(value) }\n";
  c_semantic_accept "c-import-stdio-self-macro" c_macros
    "fn probe() i32 { return fputs(c\"hello\", stdout) }\n";
  c_semantic_message "c-import-reserved-macro"
    "C declaration `addr` is not supported: name is a Fas type name; call it through a \
     C container function with another name"
    c_macros "fn probe() i32 { return addr(1) }\n";
  let c_from_d =
    c_import_macro_fixture ~c_flags:[ "-DFAS_FROM_D=19" ] "macros.h" [ "FAS_FROM_D" ]
  in
  c_semantic_message "c-import-command-line-macro-is-invisible"
    "unknown name `FAS_FROM_D`" c_from_d "fn probe() i32 { return FAS_FROM_D }\n";
  let float_macros =
    c_import_macro_fixture
      ~c_flags:[ "-D_GNU_SOURCE"; "-DFAS_FLOAT_FROM_D=1.25" ]
      "float_macros.h"
      [
        "FAS_FLOAT_DECIMAL";
        "FAS_FLOAT_F_SUFFIX";
        "FAS_FLOAT_UPPER_F_SUFFIX";
        "FAS_FLOAT_TRAILING_DOT";
        "FAS_FLOAT_LEADING_DOT";
        "FAS_FLOAT_EXPONENT";
        "FAS_FLOAT_EXPONENT_F";
        "FAS_FLOAT_PARENS";
        "FAS_FLOAT_NEGATIVE";
        "FAS_FLOAT_NEGATIVE_PARENS";
        "FAS_FLOAT_NEGATIVE_ZERO";
        "FAS_FLOAT_HEX";
        "FAS_FLOAT_HEX_MIN";
        "FAS_FLOAT_CHAIN_A";
        "FAS_FLOAT_CHAIN_B";
        "FAS_FLOAT_PREDEFINED_CHAIN";
        "FAS_FLOAT_DOUBLE_ROUND";
        "FAS_FLOAT_LONG_DOUBLE";
        "FAS_FLOAT_LONG_DOUBLE_LOWER";
        "FAS_FLOAT_OVERFLOW";
        "FAS_FLOAT_UNDERFLOW";
        "FAS_FLOAT_HEX_UNDERFLOW";
        "FAS_FLOAT_DIVISION";
        "FAS_FLOAT_ADD";
        "FAS_FLOAT_DOUBLE_NEGATIVE";
        "FAS_FLOAT_PLUS";
        "FAS_FLOAT_CAST";
        "FAS_FLOAT_FUNCTION";
        "FAS_FLOAT_STRING";
        "FAS_FLOAT_EMPTY";
        "HUGE_VAL";
        "INFINITY";
        "NAN";
        "M_PIl";
        "M_Ef16";
        "FAS_FLOAT_FROM_D";
      ]
  in
  let float_macro_program =
    match
      c_semantic_result float_macros
        ("const FAS_FLOAT_CONST f64 = FAS_FLOAT_DECIMAL + FAS_FLOAT_HEX\n"
       ^ "fn float_macro_all() f64 { return FAS_FLOAT_DECIMAL + FAS_FLOAT_F_SUFFIX + "
       ^ "FAS_FLOAT_UPPER_F_SUFFIX + FAS_FLOAT_TRAILING_DOT + FAS_FLOAT_LEADING_DOT + "
       ^ "FAS_FLOAT_EXPONENT + FAS_FLOAT_EXPONENT_F + FAS_FLOAT_NEGATIVE + "
       ^ "FAS_FLOAT_NEGATIVE_PARENS + FAS_FLOAT_HEX_MIN + FAS_FLOAT_CHAIN_A + "
       ^ "FAS_FLOAT_PREDEFINED_CHAIN + FAS_FLOAT_DOUBLE_ROUND }\n"
       ^ "fn float_macro_vector() vec[4, f32] { return splat(FAS_FLOAT_F_SUFFIX) + "
       ^ "{FAS_FLOAT_CHAIN_A, FAS_FLOAT_PARENS, FAS_FLOAT_DOUBLE_ROUND, "
       ^ "FAS_FLOAT_HEX_MIN} }\n"
       ^ "fn float_macro_convert() f64 { return cvt[f64](FAS_FLOAT_F_SUFFIX) }\n"
       ^ "fn float_macro_bits() u32 { return bitcast[u32](FAS_FLOAT_F_SUFFIX) }\n"
       ^ "fn float_macro_compare() bool { return FAS_FLOAT_DECIMAL < FAS_FLOAT_HEX }\n"
        )
    with
    | Ok program -> program
    | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)
  in
  List.iter
    (fun (name, ty, bits) ->
      incr checks_run;
      match
        List.find_opt
          (fun (constant : Hir.const_def) -> constant.name = name)
          float_macro_program.consts
      with
      | Some { ty = actual_ty; bits = actual_bits; _ }
        when actual_ty = ty && actual_bits = bits ->
          ()
      | Some { ty = actual_ty; bits = actual_bits; _ } ->
          failwith
            (Printf.sprintf "C float macro %s: expected %s 0x%Lx, got %s 0x%Lx" name
               (Hir.ty_name ty) bits (Hir.ty_name actual_ty) actual_bits)
      | None -> failwith ("C float macro was not imported: " ^ name))
    [
      ("FAS_FLOAT_DECIMAL", Hir.Float Hir.F64, 0x3ff8000000000000L);
      ("FAS_FLOAT_F_SUFFIX", Hir.Float Hir.F32, 0x40000000L);
      ("FAS_FLOAT_UPPER_F_SUFFIX", Hir.Float Hir.F32, 0x40400000L);
      ("FAS_FLOAT_TRAILING_DOT", Hir.Float Hir.F64, 0x3ff0000000000000L);
      ("FAS_FLOAT_LEADING_DOT", Hir.Float Hir.F64, 0x3fe0000000000000L);
      ("FAS_FLOAT_NEGATIVE", Hir.Float Hir.F64, 0xbff8000000000000L);
      ("FAS_FLOAT_NEGATIVE_PARENS", Hir.Float Hir.F64, 0xbff8000000000000L);
      ("FAS_FLOAT_NEGATIVE_ZERO", Hir.Float Hir.F64, 0x8000000000000000L);
      ("FAS_FLOAT_HEX", Hir.Float Hir.F64, 0x4008000000000000L);
      ("FAS_FLOAT_HEX_MIN", Hir.Float Hir.F32, 1L);
      ("FAS_FLOAT_CHAIN_A", Hir.Float Hir.F32, 0x40200000L);
      ("FAS_FLOAT_PREDEFINED_CHAIN", Hir.Float Hir.F32, 0x7f7fffffL);
      ("FAS_FLOAT_DOUBLE_ROUND", Hir.Float Hir.F32, 0x3f800001L);
    ];
  if
    not
      (List.exists
         (fun line ->
           String.starts_with
             ~prefix:"FAS_FLOAT_F_SUFFIX\tmacro FAS_FLOAT_F_SUFFIX\tmacro f32 2.0\t\t"
             line
           && String.ends_with ~suffix:"test/c_import/float_macros.h:3" line)
         (String.split_on_char '\n' (C_import.manifest_text (snd float_macros))))
  then failwith "C float macro manifest line changed";
  c_semantic_message "c-float-macro-long-double"
    "C declaration `FAS_FLOAT_LONG_DOUBLE` is not supported: long double types are not \
     supported"
    float_macros "fn probe() f64 { return FAS_FLOAT_LONG_DOUBLE }\n";
  c_semantic_message "c-float-macro-overflow"
    "C declaration `FAS_FLOAT_OVERFLOW` is not supported: floating point literal \
     rounds to infinity for `f64`"
    float_macros "fn probe() f64 { return FAS_FLOAT_OVERFLOW }\n";
  c_semantic_message "c-float-macro-underflow"
    "C declaration `FAS_FLOAT_UNDERFLOW` is not supported: nonzero floating point \
     literal rounds to zero for `f32`"
    float_macros "fn probe() f32 { return FAS_FLOAT_UNDERFLOW }\n";
  c_semantic_message "c-float-macro-hex-underflow"
    "C declaration `FAS_FLOAT_HEX_UNDERFLOW` is not supported: nonzero floating point \
     literal rounds to zero for `f64`"
    float_macros "fn probe() f64 { return FAS_FLOAT_HEX_UNDERFLOW }\n";
  c_semantic_message "c-float-macro-math-long-double"
    "C declaration `M_PIl` is not supported: long double types are not supported"
    float_macros "fn probe() f64 { return M_PIl }\n";
  c_semantic_message "c-float-macro-reserved"
    "`addr` is a type name and cannot be used as a name"
    (c_import_macro_fixture ~c_flags:[ "-D_GNU_SOURCE" ] "float_macros.h" [ "addr" ])
    "fn probe() f64 { return addr }\n";
  List.iter
    (fun name ->
      c_semantic_message
        ("c-float-macro-invisible-" ^ name)
        ("unknown name `" ^ name ^ "`")
        float_macros
        ("fn probe() f64 { return " ^ name ^ " }\n"))
    [
      "FAS_FLOAT_DIVISION";
      "FAS_FLOAT_ADD";
      "FAS_FLOAT_DOUBLE_NEGATIVE";
      "FAS_FLOAT_PLUS";
      "FAS_FLOAT_CAST";
      "FAS_FLOAT_FUNCTION";
      "FAS_FLOAT_STRING";
      "FAS_FLOAT_EMPTY";
      "HUGE_VAL";
      "INFINITY";
      "NAN";
      "M_Ef16";
      "FAS_FLOAT_FROM_D";
    ];
  let first_float_macro =
    c_import_macro_fixture "float_macros_conflict_first.h"
      [ "FAS_FLOAT_MACRO_CONFLICT" ]
  and second_float_macro =
    c_import_macro_fixture "float_macros_conflict_second.h"
      [ "FAS_FLOAT_MACRO_CONFLICT" ]
  in
  let float_macro_conflict =
    ( fst first_float_macro,
      C_import.merge_imports [ snd first_float_macro; snd second_float_macro ] )
  in
  c_semantic_message "c-float-macro-conflict"
    "C declaration `FAS_FLOAT_MACRO_CONFLICT` is not supported: conflicting C \
     declarations"
    float_macro_conflict "fn probe() f64 { return FAS_FLOAT_MACRO_CONFLICT }\n";
  let first_macro =
    c_import_macro_fixture "macros_conflict_first.h" [ "FAS_MACRO_CONFLICT" ]
  and second_macro =
    c_import_macro_fixture "macros_conflict_second.h" [ "FAS_MACRO_CONFLICT" ]
  in
  let macro_conflict =
    (fst first_macro, C_import.merge_imports [ snd first_macro; snd second_macro ])
  in
  c_semantic_message "c-import-macro-conflict"
    "C declaration `FAS_MACRO_CONFLICT` is not supported: conflicting C declarations"
    macro_conflict "fn probe() i32 { return FAS_MACRO_CONFLICT }\n";
  List.iter
    (fun (name, output, file, line, column, notes) ->
      incr checks_run;
      let fallback =
        Span.make ~file:"import.fas" ~start_offset:0 ~end_offset:0 ~line:7 ~column:1
      in
      let diagnostic = C_import.compilation_error fallback output in
      if
        diagnostic.message <> "C compilation failed: expected expression"
        || diagnostic.primary.file <> file
        || diagnostic.primary.line <> line
        || diagnostic.primary.column <> column
        || diagnostic.notes <> notes
      then failwith (name ^ ": " ^ Diag.render_all ~source:None [ diagnostic ]))
    [
      ( "c-diagnostic-fas",
        "m.fas:2:12: error: expected expression\n",
        "m.fas",
        2,
        12,
        [] );
      ( "c-diagnostic-fatal",
        "header.h:3:9: fatal error: expected expression\n",
        "import.fas",
        7,
        1,
        [ "header.h:3:9" ] );
      ( "c-diagnostic-header",
        "header.h:3:9: error: expected expression\n",
        "import.fas",
        7,
        1,
        [ "header.h:3:9" ] );
    ];
  semantic_accept "static-named-sizes"
    "const N usize = 3\n\
     var V arr[N,i32] = {1,2,3}\n\
     const T arr[N,i32] = {1,2,3}\n\
     struct P { d arr[N,u8] }\n\
     const S arr[3,u8] = {1,2,3}\n\
     const K usize = len(S)\n\
     const L usize = len(c\"abc\")\n\
     var W arr[L,u8]\n\
     var X arr[K,u8]\n\
     var Q vec[N,u8]\n";
  semantic_accept "static-forward-named-sizes"
    "var V arr[N,i32]\nstruct P { d arr[N,u8] }\nconst N usize = 3\n";
  semantic_message "static-size-self-cycle" "cyclic constant dependency involving `N`"
    "const N usize = len(A)\nconst A arr[N,u8] = {1}\n";
  semantic_message "static-size-mutual-cycle" "cyclic constant dependency involving `N`"
    "const N usize = len(B)\n\
     const M usize = len(A)\n\
     const A arr[N,u8] = {1}\n\
     const B arr[M,u8] = {2}\n";
  semantic_message "generic-layout-query-cycle"
    "cyclic constant dependency involving `WIDTH`"
    "const WIDTH usize = sizeof[Bytes[WIDTH]]\n\
     struct Bytes[N const usize] { data arr[N, u8] }\n\
     fn test() usize { return WIDTH }\n";
  semantic_message "layout-sizeof-cycle" "cyclic constant dependency involving `S`"
    "struct S { d arr[sizeof[S], u8] }\nvar X arr[1, S]\n";
  semantic_message "layout-alignof-cycle" "cyclic constant dependency involving `S`"
    "struct S { d arr[alignof[S], u8] }\nvar X arr[1, S]\n";
  semantic_message "layout-offsetof-cycle" "cyclic constant dependency involving `S`"
    "struct S { a u8, d arr[offsetof[S, a] + 1, u8] }\nvar X arr[1, S]\n";
  semantic_message "layout-mutual-query-cycle"
    "cyclic constant dependency involving `T2`"
    "struct S { d arr[sizeof[T2], u8] }\nstruct T2 { s arr[1, S] }\nvar X arr[1, S]\n";
  semantic_message "layout-generic-query-cycle"
    "cyclic constant dependency involving `S`"
    "struct S[T] { d arr[sizeof[S[T]], u8] }\nvar X S[u8]\n";
  semantic_message "layout-growing-generic-query-cycle"
    "cyclic constant dependency involving `S`"
    "struct S[N const usize] { d arr[sizeof[S[N + 1]], u8] }\nvar X S[1]\n";
  semantic_message "const-sizeof-struct-layout-cycle"
    "cyclic constant dependency involving `N`"
    "const N usize = sizeof[P]\nstruct P { x arr[N, u8] }\n";
  semantic_message "const-offsetof-struct-layout-cycle"
    "cyclic constant dependency involving `N`"
    "const N usize = offsetof[P, x]\nstruct P { x arr[N, u8] }\n";
  semantic_accept "const-layout-query-cycle-twin"
    "struct Leaf { value u8 }\n\
     const N usize = sizeof[Leaf]\n\
     struct P { x arr[N, u8] }\n";
  semantic_pin "struct-layout-addition-overflow-field"
    "const N usize = 4611686018427387904\n\
     struct P { a arr[N - 1, u8] b u8 c arr[N - 1, u8] d u16 }\n"
    2 29 1 "aggregate size overflows" None;
  semantic_pin "struct-layout-padding-overflow-field"
    "struct P { a arr[4611686018427387903, u8] b u16 }\n" 1 43 1
    "aggregate size overflows" None;
  semantic_accept "layout-query-other-struct"
    "struct Leaf { value u32 }\n\
     struct S { data arr[sizeof[Leaf], u8] }\n\
     var X arr[1, S]\n";
  let generic_layout_constant name expected text =
    incr checks_run;
    let hir = expect_ok (Parser.parse (source text)) |> Sema.check |> expect_ok in
    match
      List.find_opt (fun (constant : Hir.const_def) -> constant.name = name) hir.consts
    with
    | Some { bits; _ } when bits = expected -> ()
    | Some { bits; _ } ->
        failwith (Printf.sprintf "%s: expected %Ld, got %Ld" name expected bits)
    | None -> failwith ("missing constant " ^ name)
  in
  generic_layout_constant "SIZE" 5L
    "const N usize = 3\n\
     struct Pair[N const usize, M const usize] { first arr[N, u8]\n\
     second arr[M, u8] }\n\
     const SIZE usize = sizeof[Pair[2, N]]\n\
     fn main() i32 { return trunc[i32](SIZE) }\n";
  generic_layout_constant "SIZE" 16L
    "struct Inner[T, U] { first T, second U }\n\
     struct Outer[T] { inner Inner[u8, T] }\n\
     const SIZE usize = sizeof[Outer[u64]]\n\
     fn main() i32 { return trunc[i32](SIZE) }\n";
  generic_layout_constant "SIZE" 3L
    "const N usize = 3\n\
     struct Box[T] { value T }\n\
     const SIZE usize = sizeof[Box[arr[N, u8]]]\n\
     fn main() i32 { return trunc[i32](SIZE) }\n";
  semantic_message "generic-layout-local-constant-shadow"
    "`N` is not a compile-time constant"
    "struct Bytes[N const usize] { data arr[N, u8] }\n\
     fn probe[N const usize]() usize { { N usize = 2\n\
     return sizeof[Bytes[N]] } }\n\
     fn main() i32 { return trunc[i32](probe[3]()) }\n";
  semantic_message "generic-array-type-local-constant-shadow"
    "`N` is not a compile-time constant"
    "const N usize = 3\n\
     fn size_of[T]() usize { return sizeof[T] }\n\
     fn main() i32 { N usize = 2\n\
     return trunc[i32](size_of[arr[N, u8]]()) }\n";
  semantic_message "generic-layout-array-local-constant-shadow"
    "`N` is not a compile-time constant"
    "const N usize = 3\n\
     fn main() i32 { N usize = 2\n\
     return trunc[i32](sizeof[arr[N, u8]]) }\n";
  semantic_accept "size-expression-forms"
    "const N usize = 12\n\
     const K usize = 3\n\
     const TABLE arr[5, u8] = {1,2,3,4,5}\n\
     const BUFFERSIZE u32 = 64\n\
     var bufferseg arr[zext[usize](BUFFERSIZE) / sizeof[i32], i32]\n\
     var divided arr[N / 4, u8]\n\
     var parenthesized arr[(N / 4), u8]\n\
     var arithmetic arr[N * 2 + 1, u8]\n\
     var shifted arr[1 << K, u8]\n\
     var from_table arr[len(TABLE) - 1, u8]\n\
     var selected arr[if true { 3 } else { 5 }, u8]\n\
     var nested arr[N / 4, arr[2 * 2, u8]]\n\
     var lanes vec[1 << K, u8]\n";
  semantic_accept "size-expression-generic-layout"
    "struct Bytes[T] { data arr[sizeof[T] * 4, u8] }\n\
     const SIZE usize = sizeof[Bytes[u16]]\n\
     fn main() i32 { return trunc[i32](SIZE) }\n";
  semantic_accept "generic-const-argument-sizeof-type"
    "struct ByteBlock[N const usize] { data arr[N, u8] }\n\
     struct TypeBox[T] { value T }\n\
     struct TypeMatrix[T] { direct ByteBlock[sizeof[T]]\n\
     doubled ByteBlock[sizeof[T] * 2]\n\
     wrapped ByteBlock[sizeof[TypeBox[T]]]\n\
     nested arr[sizeof[ByteBlock[sizeof[TypeBox[T]]]], u8] }\n\
     fn type_probe[T]() usize { direct ByteBlock[sizeof[T]] = {}\n\
     doubled ByteBlock[sizeof[T] * 2] = {}\n\
     wrapped ByteBlock[sizeof[TypeBox[T]]] = {}\n\
     return len(direct.data) + len(doubled.data) * 10 + len(wrapped.data) * 100 \
     +sizeof[ByteBlock[sizeof[TypeBox[T]]]] * 1000 + sizeof[TypeMatrix[T]] * 10000 }\n\
     fn main() i32 { if type_probe[u16]() != 102242 { return 1 }\n\
     if type_probe[u32]() != 204484 { return 2 }\n\
     if sizeof[TypeMatrix[u16]] != 10 { return 3 }\n\
     if sizeof[TypeMatrix[u32]] != 20 { return 4 }\n\
     return 0 }\n";
  semantic_accept "size-expression-const-generic-arguments"
    "const N usize = 3\n\
     struct Ring[M const usize] { data arr[M, u8] }\n\
     var R Ring[N * 2]\n\
     fn main() i32 { return 0 }\n";
  semantic_accept "size-expression-type-identity"
    "fn main() i32 { a arr[4, u8] = {1,2,3,4}\n\
     b arr[2 * 2, u8]\n\
     copy(b, a)\n\
     return zext[i32](b[3]) }\n";
  semantic_accept "size-expression-negative-explicit-twin"
    "const POS isize = 1\nvar A arr[bitcast[usize](POS) + 1, u8]\n";
  semantic_message "size-expression-negative"
    "cannot use `isize` as `usize`: negative values change meaning"
    "const NEG isize = -1\nvar A arr[NEG + 0, u8]\n";
  semantic_accept "size-expression-too-large-twin"
    "const SMALL usize = 3\nvar A arr[SMALL + 1, u8]\n";
  semantic_message "size-expression-too-large"
    "array length `9223372036854775808` is too large"
    "const BIG usize = 9223372036854775808\nvar A arr[BIG + 0, u8]\n";
  semantic_accept "size-expression-address-twin" "var G i32\nvar A arr[2, u8]\n";
  semantic_message "size-expression-address" "expression is not compile-time constant"
    "var G i32\nvar A arr[&G, u8]\n";
  semantic_accept "size-expression-runtime-twin"
    "const N usize = 3\nfn probe() void { A arr[N, u8] = {} }\n";
  semantic_message "size-expression-runtime" "`n` is not a compile-time constant"
    "fn probe(n usize) void { A arr[n, u8] = {} }\n";
  semantic_pin "size-expression-cycle"
    "const N usize = len(A)\nconst A arr[N + 1, u8] = {1}\n" 2 13 5
    "cyclic constant dependency involving `N`" None;
  semantic_accept "size-expression-cycle-twin"
    "const N usize = 3\nconst A arr[N + 1, u8] = {1,2,3,4}\n";
  semantic_pin "static-size-direct-len-cycle"
    "const A arr[len(B),u8] = {1}\nconst B arr[len(A),u8] = {2}\n" 1 13 6
    "cyclic constant dependency involving `B`" None;
  Printf.printf "regression checks: %d passed\n" !checks_run

let () =
  let hir =
    expect_ok
      (Sema.check
         (parse_file "exports.fas"
            "opaque Token\n\
             struct Inner { x u16 }\n\
             struct Outer @align(16) { inner Inner, lanes vec[3,u8], mask vec[8,bool], \
             ptr handle[Token], data arr[2,arr[3,i32]] }\n\
             extern \"C\" {\n\
             var state Outer = {{2},splat(1),splat(true),null,{{1,2,3},{4,5,6}}}\n\
             var a i8 = 1\n\
             fn z(p addr, t handle[Token], n usize, s isize, b bool) i32 { return 2 }\n\
             fn imported() i32\n\
             }\n\
             fn private() i32 { return 1 }\n"))
  in
  let actual, headers, errors = C_exports.declarations [] hir in
  let expected =
    "struct Inner {\n\
    \  alignas(2) uint16_t x;\n\
     };\n\
     typedef struct Inner Inner;\n\
     static_assert(sizeof(struct Inner) == 2, \"struct Inner size\");\n\
     static_assert(alignof(struct Inner) == 2, \"struct Inner alignment\");\n\
     static_assert(offsetof(struct Inner, x) == 0, \"Inner.x offset\");\n\
     typedef uint8_t fas_vec_3_u8 __attribute__((ext_vector_type(3)));\n\
     static_assert(sizeof(fas_vec_3_u8) == 4, \"fas_vec_3_u8 size\");\n\
     static_assert(alignof(fas_vec_3_u8) == 4, \"fas_vec_3_u8 alignment\");\n\
     typedef bool fas_vec_8_bool __attribute__((ext_vector_type(8)));\n\
     static_assert(sizeof(fas_vec_8_bool) == 1, \"fas_vec_8_bool size\");\n\
     static_assert(alignof(fas_vec_8_bool) == 1, \"fas_vec_8_bool alignment\");\n\
     struct Token;\n\
     struct Outer {\n\
    \  alignas(16) struct Inner inner;\n\
    \  fas_vec_3_u8 lanes;\n\
    \  fas_vec_8_bool mask;\n\
    \  struct Token * ptr;\n\
    \  int32_t data[2][3];\n\
     };\n\
     typedef struct Outer Outer;\n\
     static_assert(sizeof(struct Outer) == 48, \"struct Outer size\");\n\
     static_assert(alignof(struct Outer) == 16, \"struct Outer alignment\");\n\
     static_assert(offsetof(struct Outer, inner) == 0, \"Outer.inner offset\");\n\
     static_assert(offsetof(struct Outer, lanes) == 4, \"Outer.lanes offset\");\n\
     static_assert(offsetof(struct Outer, mask) == 8, \"Outer.mask offset\");\n\
     static_assert(offsetof(struct Outer, ptr) == 16, \"Outer.ptr offset\");\n\
     static_assert(offsetof(struct Outer, data) == 24, \"Outer.data offset\");\n\
     extern int8_t a;\n\
     extern struct Outer state;\n\
     int32_t z(void * p, struct Token * t, size_t n, ptrdiff_t s, bool b);\n"
  in
  if actual <> expected then failwith ("c-export-matrix:\n" ^ actual);
  assert (headers = [] && errors = []);
  print_endline "C export declaration matrix: passed"

let () =
  let generate text =
    C_exports.declarations [] (expect_ok (Sema.check (parse_file "exports.fas" text)))
  in
  let widths =
    [
      ("i8", "int8_t");
      ("u8", "uint8_t");
      ("i16", "int16_t");
      ("u16", "uint16_t");
      ("i32", "int32_t");
      ("u32", "uint32_t");
      ("i64", "int64_t");
      ("u64", "uint64_t");
      ("isize", "ptrdiff_t");
      ("usize", "size_t");
      ("bool", "bool");
      ("addr", "void *");
    ]
  in
  List.iter
    (fun (fas, c) ->
      incr checks_run;
      let value =
        if fas = "bool" then "true" else if fas = "addr" then "null" else "1"
      in
      let text, _, errors =
        generate
          (Printf.sprintf
             "extern \"C\" {\nvar value %s = %s\nfn echo(x %s) %s { return x }\n}\n" fas
             value fas fas)
      in
      assert (errors = []);
      assert (text = Printf.sprintf "%s echo(%s x);\nextern %s value;\n" c c c))
    widths;
  let text, _, errors = generate "extern \"C\" { fn done() void {} }\n" in
  assert (text = "void done(void);\n" && errors = []);
  List.iter
    (fun (source, expected) ->
      incr checks_run;
      let text, _, errors = generate source in
      assert (text = "extern int32_t good;\n" && errors = [ expected ]))
    [
      ( "struct Empty {}\n\
         extern \"C\" {\n\
         var bad arr[2,Empty] = {{},{}}\n\
         var good i32 = 1\n\
         }\n",
        "export `bad` has no C declaration: arr[2, Empty] contains an empty struct" );
      ( "struct Zero { x arr[0,u8] }\n\
         extern \"C\" {\n\
         var bad Zero = {{}}\n\
         var good i32 = 1\n\
         }\n",
        "export `bad` has no C declaration: Zero contains a zero-length array" );
    ];
  let _, _, errors =
    generate
      "struct Empty {}\nextern \"C\" {\nvar z Empty = {}\nvar a arr[0,u8] = {}\n}\n"
  in
  assert (
    List.hd errors
    = "export `a` has no C declaration: arr[0, u8] contains a zero-length array");
  let text, _, errors =
    generate
      "extern \"C\" {\nvar fas_vec_3_u8 i32 = 5\nvar lanes vec[3,u8] = splat(1)\n}\n"
  in
  assert (
    errors = []
    && contains text
         "typedef uint8_t fas_vec_3_u8_ __attribute__((ext_vector_type(3)));\n"
    && contains text "extern fas_vec_3_u8_ lanes;\n");
  let header =
    C_exports.header ~name:"my-api.h" ~headers:[ "#include <api.h>\n" ]
      "void done(void);\n"
  in
  assert (
    header
    = "#ifndef FAS_MY_API_H\n\
       #define FAS_MY_API_H\n\
       #include <api.h>\n\
       #ifdef __cplusplus\n\
       extern \"C\" {\n\
       #endif\n\
       void done(void);\n\
       #ifdef __cplusplus\n\
       }\n\
       #endif\n\
       #endif\n");
  let plain_header =
    C_exports.header ~name:"plain.h" ~headers:[] "void print(void * text);\n"
  in
  assert (
    plain_header
    = "#ifndef FAS_PLAIN_H\n\
       #define FAS_PLAIN_H\n\
       #ifdef __cplusplus\n\
       extern \"C\" {\n\
       #endif\n\
       void print(void * text);\n\
       #ifdef __cplusplus\n\
       }\n\
       #endif\n\
       #endif\n");
  let scalar_declarations, _, errors =
    generate
      "extern \"C\" {\n\
       fn echo(x i32) i32 { return x }\n\
       var flag bool = true\n\
       var count usize = 1\n\
       }\n"
  in
  assert (errors = []);
  let scalar_header =
    C_exports.header ~name:"scalars.h" ~headers:[] scalar_declarations
  in
  assert (
    scalar_header
    = "#ifndef FAS_SCALARS_H\n\
       #define FAS_SCALARS_H\n\
       #include <stdbool.h>\n\
       #include <stdint.h>\n\
       #include <stddef.h>\n\
       #ifdef __cplusplus\n\
       extern \"C\" {\n\
       #endif\n\
       extern size_t count;\n\
       int32_t echo(int32_t x);\n\
       extern bool flag;\n\
       #ifdef __cplusplus\n\
       }\n\
       #endif\n\
       #endif\n");
  let struct_declarations, _, errors =
    generate "struct Pair { x i32\n y i32 }\nextern \"C\" { var pair Pair = {1, 2} }\n"
  in
  assert (errors = []);
  let struct_header = C_exports.header ~name:"pair.h" ~headers:[] struct_declarations in
  assert (
    struct_header
    = String.concat "\n"
        [
          "#ifndef FAS_PAIR_H";
          "#define FAS_PAIR_H";
          "#include <stdalign.h>";
          "#include <assert.h>";
          "#include <stdint.h>";
          "#include <stddef.h>";
          "#ifdef __cplusplus";
          "extern \"C\" {";
          "#endif";
          "struct Pair {";
          "  alignas(4) int32_t x;";
          "  int32_t y;";
          "};";
          "typedef struct Pair Pair;";
          "static_assert(sizeof(struct Pair) == 8, \"struct Pair size\");";
          "static_assert(alignof(struct Pair) == 4, \"struct Pair alignment\");";
          "static_assert(offsetof(struct Pair, x) == 0, \"Pair.x offset\");";
          "static_assert(offsetof(struct Pair, y) == 4, \"Pair.y offset\");";
          "extern struct Pair pair;";
          "#ifdef __cplusplus";
          "}";
          "#endif";
          "#endif";
        ]
      ^ "\n");
  let records =
    [
      C_exports.{ name = "Tag"; spelling = "struct Tag"; header = None };
      C_exports.
        { name = "Alias"; spelling = "Alias"; header = Some "#include <alias.h>\n" };
    ]
  in
  let hir =
    expect_ok
      (Sema.check
         (parse_file "handles.fas"
            "opaque Tag\n\
             opaque Alias\n\
             extern \"C\" { fn get(x handle[Tag], y handle[Alias]) handle[Alias] { \
             return y } }\n"))
  in
  let text, headers, errors = C_exports.declarations records hir in
  assert (
    text = "struct Tag;\nAlias * get(struct Tag * x, Alias * y);\n"
    && headers = [ "#include <alias.h>\n" ]
    && errors = []);
  let root = Filename.temp_file "fas-export-pins-" ".fas" in
  let h = root ^ ".h" and out = root ^ ".api.h" in
  let dir = root ^ ".dir" in
  Unix.mkdir dir 0o700;
  let angle = Filename.concat dir "types.h" in
  let inner = Filename.concat dir "inner.h" in
  let write path text =
    let ch = open_out_bin path in
    output_string ch text;
    close_out ch
  in
  let read path =
    let ch = open_in_bin path in
    let text = really_input_string ch (in_channel_length ch) in
    close_in ch;
    text
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun p -> try Sys.remove p with Sys_error _ -> ())
        [ root; h; out; angle; inner ];
      Unix.rmdir dir)
    (fun () ->
      List.iter
        (fun source ->
          write root source;
          match Driver.run (cli_run [ "--emit-header"; root ]) with
          | Error [ d ] ->
              assert (
                d.Diag.message
                = "export `bad` has no C declaration: Empty contains an empty struct")
          | _ -> failwith "c-export-empty-header: expected exact rejection")
        [ "struct Empty {}\nextern \"C\" { var bad Empty = {} }\n" ];
      write root "extern \"C\" { var bad arr[0,u8] = {} }\n";
      (match Driver.run (cli_run [ "--emit-header"; root ]) with
      | Error [ d ] ->
          assert (
            d.Diag.message
            = "export `bad` has no C declaration: arr[0, u8] contains a zero-length \
               array")
      | _ -> failwith "c-export-zero-header: expected exact rejection");
      List.iter
        (fun (source, expected) ->
          write root source;
          match Driver.run (cli_run [ "--emit-header"; root ]) with
          | Error [ d ] -> assert (d.Diag.message = expected)
          | _ -> failwith "c-export-generic-header: expected exact rejection")
        [
          ( "struct B[N const usize] { value arr[N,u8] }\n\
             extern \"C\" { var item B[2 * 2] = {{1,2,3,4}} }\n",
            "export `item` has no C declaration: generic struct instance `B[4]`" );
          ( "struct G[T] { value T }\nextern \"C\" { var item G[u16] = {3} }\n",
            "export `item` has no C declaration: generic struct instance `G[u16]`" );
          ( "struct G[T] { value T }\n\
             struct Outer { items arr[2,G[u16]] }\n\
             extern \"C\" { var item Outer = {{{3},{4}}} }\n",
            "export `item` has no C declaration: generic struct instance `G[u16]`" );
          ( "struct B[N const usize] { value arr[N,u8] }\n\
             extern \"C\" { fn take(x B[4]) i32 { return 0 } }\n",
            "aggregate parameter `x` of type `B[4]` cannot be passed by value; declare \
             `x` as `addr`" );
          ( "struct B[N const usize] { value arr[N,u8] }\n\
             extern \"C\" { fn make() B[4] { return {1,2,3,4} } }\n",
            "aggregate result `B[4]` cannot be returned by value; return `void` and \
             take the destination as an `addr` parameter" );
        ];
      write root
        "use \"C\" <<C\n\
         int access_item(void) { return item.value[0]; }\n\
         C\n\
         struct B[N const usize] { value arr[N,u8] }\n\
         extern \"C\" { var item B[4] = {{1,2,3,4}} }\n";
      (match Driver.run (cli_run [ "--emit-llvm"; root ]) with
      | Error [ d ] ->
          assert (
            d.Diag.message = "C compilation failed: use of undeclared identifier 'item'"
            && d.primary.line = 2)
      | _ -> failwith "c-export-generic-container: expected omitted export");
      write root
        "struct B4 { value arr[4,u8] }\nextern \"C\" { var item B4 = {{1,2,3,4}} }\n";
      let non_generic_header =
        expect_ok (Driver.run (cli_run [ "--emit-header"; root ]))
      in
      assert (contains non_generic_header "extern struct B4 item;\n");
      write root
        "use \"C\" <<C\n\
         int exported(int x);\n\
         int invoke(void) { return exported(2); }\n\
         C\n\
         extern \"C\" { fn exported(x i32) i32 { return x } }\n";
      ignore (expect_ok (Driver.run (cli_run [ "--emit-header"; root ])));
      write root
        "use \"C\" <<C\n\
         long exported(int x);\n\
         C\n\
         extern \"C\" { fn exported(x i32) i32 { return x } }\n";
      (match Driver.run (cli_run [ "--emit-header"; root ]) with
      | Error [ d ] ->
          assert (
            d.Diag.message
            = "C declaration `exported` has type `fn(i32)->i64`, but Fas declares \
               `fn(i32)->i32`"
            && d.primary.line = 4 && d.primary.file = root)
      | _ -> failwith "c-export-incompatible-prototype: expected signature rejection");
      write root
        "use \"C\" <<C\n\
         void *missing(void) { return &bad; }\n\
         C\n\
         extern \"C\" { var bad arr[0,u8] = {} }\n";
      (match Driver.run (cli_run [ "--emit-ir"; root ]) with
      | Error [ d ] ->
          assert (
            d.Diag.message = "C compilation failed: use of undeclared identifier 'bad'"
            && d.primary.line = 2)
      | _ -> failwith "c-export-omitted-container: expected mapped Clang rejection");
      List.iter
        (fun (source, expected) ->
          write root ("use \"C\" <<C\nint sentinel(void) { return 1; }\nC\n" ^ source);
          ignore (expect_ok (Driver.run (cli_run [ "--emit-ir"; root ])));
          match Driver.run (cli_run [ "--emit-header"; root ]) with
          | Error [ d ] -> assert (d.Diag.message = expected)
          | _ -> failwith "c-export-third-run: expected exact rejection")
        [
          ( "use \"C\" <<T\n\
             typedef struct { int x; } Alias;\n\
             T\n\
             extern \"C\" { fn echo(x handle[Alias]) handle[Alias] { return x } }\n",
            "export `echo` has no C declaration: `Alias` is declared only in a C \
             container" );
          ( "extern \"C\" { fn unsigned(new i32) i32 { return new } }\n",
            "export `unsigned` has no C declaration: `unsigned` is a reserved C or C++ \
             identifier" );
          ( "struct S { char u8\nclass i32 }\nextern \"C\" { var value S = {1,2} }\n",
            "export `value` has no C declaration: `char` is a reserved C or C++ \
             identifier" );
          ( "struct Inner { class i32 }\n\
             struct Outer { x arr[1,Inner] }\n\
             extern \"C\" { var value Outer = {{{1}}} }\n",
            "export `value` has no C declaration: `class` is a reserved C or C++ \
             identifier" );
          ( "struct size_t { x i32 }\nextern \"C\" { var value size_t = {1} }\n",
            "export `value` has no C declaration: `size_t` is a reserved C or C++ \
             identifier" );
        ];
      write root
        "extern \"C\" { fn echo(new i32, assert i32) i32 { return new + assert } }\n";
      let header = expect_ok (Driver.run (cli_run [ "--emit-header"; root ])) in
      assert (contains header "int32_t echo(int32_t , int32_t );\n");
      write root
        "use \"C\" <<C\n\
         struct Tag;\n\
         C\n\
         extern \"C\" { fn echo(x handle[Tag]) handle[Tag] { return x } }\n";
      let header = expect_ok (Driver.run (cli_run [ "--emit-header"; root ])) in
      assert (contains header "struct Tag * echo(struct Tag * x);\n");
      write h
        "#ifndef EXPORT_TYPES_H\n\
         #define EXPORT_TYPES_H\n\
         typedef struct { int x; } Alias;\n\
         struct Tag;\n\
         #endif\n";
      let exports =
        "extern \"C\" {\n\
         fn exported(x handle[Alias], y handle[Tag]) handle[Alias] { return x }\n\
         }\n"
      in
      write root (Printf.sprintf "use \"C\" %S\n%s" (Filename.basename h) exports);
      ignore (expect_ok (Driver.run (cli_run [ "--emit-header"; "-o"; out; root ])));
      let header = read out in
      assert (contains header (Printf.sprintf "#include %S\n" (Filename.basename h)));
      assert (
        contains header "struct Tag;\nAlias * exported(Alias * x, struct Tag * y);\n");
      assert (
        String.starts_with
          ~prefix:("#ifndef FAS_" ^ C_exports.guard (Filename.basename out) ^ "_H\n")
          header);
      write angle (read h);
      write root (Printf.sprintf "use \"C\" <%s>\n%s" "types.h" exports);
      let header =
        expect_ok (Driver.run (cli_run [ "--emit-header"; "-I"; dir; root ]))
      in
      assert (contains header ("#include <" ^ "types.h" ^ ">\n"));
      write inner "typedef struct { int x; } NestedAlias;\n";
      write angle "#include \"inner.h\"\n";
      write root
        "use \"C\" <types.h>\n\
         use \"C\" <<C\n\
         NestedAlias *wrapper(NestedAlias *x) { return echo(x); }\n\
         C\n\
         extern \"C\" { fn echo(x handle[NestedAlias]) handle[NestedAlias] { return x \
         } }\n";
      let nested =
        expect_ok (Driver.run (cli_run [ "--emit-header"; "-I"; dir; root ]))
      in
      assert (
        contains nested "#include <types.h>\n"
        && contains nested "NestedAlias * echo(NestedAlias * x);\n");
      write angle "typedef struct { ZRecord embedded; int other; } ARecord;\n";
      write inner
        "#ifndef EXPORT_INNER_H\n\
         #define EXPORT_INNER_H\n\
         typedef struct fas_z_record { int value; } ZRecord;\n\
         #endif\n";
      write root
        "use \"C\" <inner.h>\n\
         use \"C\" <types.h>\n\
         extern \"C\" { fn accept_nested(value handle[ARecord]) i32 { return 1 } }\n";
      let ordered =
        expect_ok (Driver.run (cli_run [ "--emit-header"; "-I"; dir; root ]))
      in
      if
        not
          (positions ordered "#include <inner.h>\n"
           < positions ordered "#include <types.h>\n"
          && contains ordered "int32_t accept_nested(ARecord * value);\n")
      then failwith "c-export-nested-record-headers: dependency include order changed";

      write root "struct S { char u8 }\nextern \"C\" { var value S = {1} }\n";
      (match Driver.run (cli_run [ "--emit-header"; root ]) with
      | Error [ d ] ->
          assert (
            d.Diag.message
            = "export `value` has no C declaration: `char` is a reserved C or C++ \
               identifier"
            && d.primary.Span.file = root && d.primary.Span.line = 2
            && d.primary.Span.column = 14
            && d.notes
               = [
                   Printf.sprintf "offending field `char` is declared here: %s:1:12"
                     root;
                 ])
      | _ -> failwith "c-export-field-span: expected exact rejection location");

      assert (
        String.starts_with
          ~prefix:
            ("#ifndef FAS_"
            ^ C_exports.guard (Filename.remove_extension (Filename.basename root))
            ^ "_H\n")
          header));
  semantic_accept "struct-fields-adjacent" "struct V { x i32 y i32 z i32 }\n";
  semantic_accept "struct-fields-comma-separated" "struct V { x i32, y i32 }\n";
  semantic_accept "struct-fields-one-per-line" "struct V {\nx i32\ny i32\n}\n";
  semantic_accept "struct-field-nested-struct-on-one-line"
    "struct V { x i32 }\nstruct B { lo V hi V }\n";
  semantic_accept "struct-field-multiline-array-type"
    "struct S { values arr[\n2,\ni32\n] }\n";
  let split_struct_field = "struct S { a\ni32 }\n" in
  incr checks_run;
  (match Parser.parse (source split_struct_field) with
  | Ok _ -> failwith "struct-field-newline-before-type: expected parse rejection"
  | Error [ diagnostic ] ->
      let span = diagnostic.Diag.primary in
      if
        diagnostic.message <> "expected a type, found newline"
        || span.Span.file <> "regression.fas"
        || span.start_offset <> 12 || span.end_offset <> 13 || span.line <> 1
        || span.column <> 13
      then
        failwith "struct-field-newline-before-type: diagnostic message or span changed"
  | Error _ -> failwith "struct-field-newline-before-type: expected one diagnostic");
  semantic_accept "newline-whitespace-in-delimited-lists"
    "struct Shape {\n\
     value i32\n\
     }\n\
     const values arr[2,i32] = {\n\
     1,\n\
     2\n\
     }\n\
     fn identity[T](\n\
     value T\n\
     ) T { return value }\n\
     fn combine(\n\
     left i32,\n\
     right i32\n\
     ) i32 { return (left +\n\
     right) }\n\
     fn f() i32 {\n\
     items arr[\n\
     2,\n\
     i32\n\
     ] = {\n\
     3,\n\
     4\n\
     }\n\
     return identity[\n\
     i32\n\
     ](combine(\n\
     items[\n\
     0\n\
     ],\n\
     values[\n\
     1\n\
     ]))\n\
     }\n";
  let literal =
    "fn main() i32 { a arr[20,u32] = {"
    ^ String.concat "," (List.init 20 (fun i -> string_of_int i ^ " + 1"))
    ^ "}\n return if a[19] == 20 { 0 } else { 1 } }"
  in
  semantic_accept "constant-local-literal-expressions" literal;
  if not (contains (llvm_of literal) "@.literal.") then
    failwith "constant local literal must use immutable storage";
  semantic_message "constant-local-literal-width" "array of 20 elements, got 2"
    "fn main() i32 { a arr[20,u32] = {1,2}\n return 0 }";
  semantic_accept "large-zero-array-loop"
    "fn main() i32 { a arr[65536,u32] = {}\n\
    \ i usize = 0\n\
    \ s u32 = 0\n\
    \ while i < 4 { s = s + a[i]\n\
    \ i = i + 1 }\n\
    \ return bitcast[i32](s) }";
  semantic_message "partial-array-loop-uninitialized" "use of uninitialized local `a`"
    "fn main() i32 { a arr[65536,u32]\n\
    \ a[0] = 0\n\
    \ i usize = 0\n\
    \ while i < 4 { x u32 = a[1]\n\
    \ i = i + 1 }\n\
    \ return 0 }";
  let large_path = Filename.temp_file "fas-large-zero-" ".fas" in
  let large_output = Filename.temp_file "fas-large-zero-" ".exe" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove large_path;
      Sys.remove large_output)
    (fun () ->
      let channel = open_out_bin large_path in
      output_string channel
        "fn main() i32 { a arr[65536,u32] = {}\n\
        \ i usize = 0\n\
        \ s u32 = 0\n\
        \ while i < 4 { s = s + a[i]\n\
        \ i = i + 1 }\n\
        \ return bitcast[i32](s) }";
      close_out channel;
      ignore
        (expect_ok (Driver.run (cli_run [ "-O2"; "-o"; large_output; large_path ])));
      if Sys.command large_output <> 0 then failwith "large zero initialization value");
  let assembly_path = Filename.temp_file "fas-assembly-unit-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove assembly_path)
    (fun () ->
      let channel = open_out_bin assembly_path in
      output_string channel
        "use \"asm\" <<ASM\n\
         .text\n\
         .globl unit_answer\n\
         unit_answer: movl $42, %eax; ret\n\
         .section .note.GNU-stack,\"\",@progbits\n\
         ASM\n\
         extern \"C\" { fn unit_answer() i32 }\n\
         fn main() i32 { return unit_answer() - 42 }\n";
      close_out channel;
      let llvm = expect_ok (Driver.run (cli_run [ "--emit-llvm"; assembly_path ])) in
      if contains llvm "movl" then failwith "assembly unit leaked into Fas LLVM");
  parse_message "removed-asm-function"
    "asm fn was removed in v0.3; use a use \"asm\" unit and an extern \"C\" declaration"
    "asm fn old(x i32) i32 { movl $1, %eax; ret }\n";
  semantic_accept "asm-is-an-ordinary-binding" "fn f() i32 { asm i32 = 7\n return asm }";
  parse_message "assembly-unit-missing-terminator"
    "assembly unit is missing terminator `END`" "use \"asm\" <<END\n.text\n";
  parse_message "assembly-unit-nested-container" "assembly unit must be at top level"
    "fn f() void {\nuse \"asm\" <<END\n.text\nEND\n}\n";
  parse_message "assembly-unit-nested-path" "assembly unit must be at top level"
    "fn f() void { use \"asm\" \"file.s\" }\n";
  let assembly_root = Filename.temp_file "fas-assembly-pins-" ".fas" in
  let assembly_dependency = assembly_root ^ ".dependency.fas" in
  let assembly_cpp = assembly_root ^ ".S" in
  let assembly_plain = assembly_root ^ ".s" in
  Fun.protect
    ~finally:(fun () ->
      List.iter Sys.remove
        [ assembly_root; assembly_dependency; assembly_cpp; assembly_plain ])
    (fun () ->
      let write path text =
        let channel = open_out_bin path in
        output_string channel text;
        close_out channel
      in
      write assembly_cpp
        "#if VALUE != 42\n\
         #error missing preprocessor flag\n\
         #endif\n\
         .text\n\
         .globl unit_cpp\n\
         unit_cpp: movl $VALUE, %eax; ret\n\
         .section .note.GNU-stack,\"\",@progbits\n";
      write assembly_plain
        ".set VALUE, 41\n\
         .text\n\
         .globl unit_plain\n\
         unit_plain: movl $VALUE, %eax; ret\n\
         .section .note.GNU-stack,\"\",@progbits\n";
      write assembly_dependency
        (Printf.sprintf "use \"asm\" %S\n" (Filename.basename assembly_plain));
      write assembly_root
        (Printf.sprintf
           "use \"asm\" %S\n\
            use %S\n\
            extern \"C\" { fn unit_cpp() i32\n\
            fn unit_plain() i32 }\n\
            fn main() i32 { return unit_cpp() + unit_plain() - 83 }\n"
           (Filename.basename assembly_cpp)
           (Filename.basename assembly_dependency));
      ignore
        (expect_ok (Driver.run (cli_run [ "--emit-ir"; "-DVALUE=42"; assembly_root ])));
      write assembly_root
        (Printf.sprintf "use \"asm\" %S\nfn f() void {}\n"
           (Filename.basename assembly_plain));
      ignore
        (expect_ok (Driver.run (cli_run [ "--emit-ir"; "-DVALUE=42"; assembly_root ])));
      write assembly_root
        "use \"asm\" <<END\n.text\ninvalid_opcode %rax\nEND\nfn f() void {}\n";
      (match Driver.run (cli_run [ "--emit-ir"; assembly_root ]) with
      | Error [ diagnostic ]
        when diagnostic.Diag.message
             = "assembly failed: invalid instruction mnemonic 'invalid_opcode'"
             && diagnostic.primary.Span.file = assembly_root
             && diagnostic.primary.Span.line = 3 ->
          ()
      | _ -> failwith "assembly container error location or text changed");
      write assembly_cpp "invalid_opcode %rax\n";
      write assembly_root
        (Printf.sprintf "\nuse \"asm\" %S\n" (Filename.basename assembly_cpp));
      (match Driver.run (cli_run [ "--emit-ir"; assembly_root ]) with
      | Error [ diagnostic ]
        when diagnostic.Diag.message
             = "assembly failed: invalid instruction mnemonic 'invalid_opcode'"
             && diagnostic.primary.Span.file = assembly_cpp
             && diagnostic.primary.Span.line = 1
             && diagnostic.notes = [] ->
          ()
      | _ -> failwith "assembly .S error location changed");
      write assembly_root
        (Printf.sprintf "use \"asm\" %S\nfn f() void {}\n" assembly_plain);
      ignore (expect_ok (Driver.run (cli_run [ "--emit-ir"; assembly_root ])));
      let missing = assembly_root ^ ".missing.S" in
      write assembly_root
        (Printf.sprintf "use \"asm\" %S\n" (Filename.basename missing));
      match Driver.run (cli_run [ "--emit-ir"; assembly_root ]) with
      | Error [ diagnostic ]
        when diagnostic.Diag.message
             = Printf.sprintf
                 "cannot read assembly input `%s`: No such file or directory"
                 (Filename.basename missing) ->
          ()
      | _ -> failwith "assembly missing file diagnostic changed");
  print_endline "C export spelling, omission, diagnostics and header pins: passed"

let () =
  let source = Filename.temp_file "fas-feature-macro-" ".fas" in
  let block line body =
    C_import.
      {
        spelling = Ast.C_fragment { tag = "C"; text = body };
        span = Span.make ~file:source ~start_offset:0 ~end_offset:0 ~line ~column:1;
      }
  in
  let system = C_import.{ spelling = Ast.C_system "stdio.h"; span = Span.synthetic } in
  let import ?(c_flags = []) headers =
    C_import.import ~cc:"clang-22" ~keep:false ~c_flags source headers
  in
  let feature_api =
    "#include <pthread.h>\n\
     #include <sched.h>\n\
     static inline int feature_zero(cpu_set_t *set) { CPU_ZERO(set); return 0; }\n"
  in
  let expect_late name macro flag headers =
    match import headers with
    | Error [ diagnostic ] ->
        let message =
          "C block defines `" ^ macro
          ^ "` after earlier C imports already read system headers"
        and help =
          "remove this line and pass `-D" ^ flag
          ^ "` on the command line, or move this block before the other C imports"
        in
        if
          diagnostic.Diag.message <> message
          || diagnostic.help <> Some help
          || diagnostic.primary.Span.file <> source
          || diagnostic.primary.Span.line <> 3
          || diagnostic.primary.Span.column <> 1
        then
          failwith
            (name ^ ": unexpected diagnostic: "
            ^ Diag.render_all ~source:None [ diagnostic ])
    | Error diagnostics ->
        failwith
          (name ^ ": unexpected diagnostics: "
          ^ Diag.render_all ~source:None diagnostics)
    | Ok _ -> failwith (name ^ ": expected rejection")
  in
  Fun.protect
    ~finally:(fun () -> try Sys.remove source with Sys_error _ -> ())
    (fun () ->
      expect_late "c-feature-macro-late-repro" "_GNU_SOURCE" "_GNU_SOURCE"
        [ system; block 2 ("#define _GNU_SOURCE\n" ^ feature_api) ];
      expect_late "c-feature-macro-late-file-offset" "_FILE_OFFSET_BITS"
        "_FILE_OFFSET_BITS=64"
        [ system; block 2 "#define _FILE_OFFSET_BITS 64\n" ];
      (match
         import ~c_flags:[ "-D_FILE_OFFSET_BITS=32" ]
           [ system; block 2 "#define _FILE_OFFSET_BITS 64\n" ]
       with
      | Error [ { Diag.message; help = None; primary; _ } ]
        when message
             = "C block defines `_FILE_OFFSET_BITS` as `64` but the command line sets \
                `32`"
             && primary.Span.line = 3 && primary.Span.column = 1 ->
          ()
      | _ -> failwith "c-feature-macro-value-mismatch: unexpected result");
      ignore
        (expect_ok
           (import ~c_flags:[ "-D_GNU_SOURCE" ] [ system; block 2 feature_api ]));
      ignore
        (expect_ok
           (import ~c_flags:[ "-D_GNU_SOURCE" ]
              [ system; block 2 ("#define _GNU_SOURCE\n" ^ feature_api) ]));
      ignore
        (expect_ok
           (import ~c_flags:[ "-D_GNU_SOURCE=1" ]
              [ system; block 2 ("#define _GNU_SOURCE\n" ^ feature_api) ]));
      ignore
        (expect_ok
           (import ~c_flags:[ "-D"; "_GNU_SOURCE=1" ]
              [ system; block 2 ("#define _GNU_SOURCE 1\n" ^ feature_api) ]));
      ignore
        (expect_ok (import [ block 2 ("#define _GNU_SOURCE\n" ^ feature_api); system ]));
      ignore
        (expect_ok
           (import
              [
                system;
                block 2
                  "#include <stdint.h>\n\
                   static inline int plain_value(void) { return 7; }\n";
              ])))

let () =
  let fixture =
    c_import_container "anonymous-container"
      "typedef struct { int id; union { int x; unsigned y; }; } user_t;\n\
       typedef union { unsigned word; unsigned char bytes[4]; } word_t;\n\
       extern user_t users[2];\n"
  in
  c_semantic_accept "c-import-container-anonymous-typedef-records" fixture
    "fn f() i32 { u user_t = {3, 7, 7}\n\
     w word_t = {9}\n\
     users[1].id = u.id\n\
     return users[1].id + u.x + bitcast[i32](w.word) }\n";
  c_semantic_message "c-import-container-anonymous-typedef-field-error"
    "record `user_t` has no field `missing`" fixture
    "fn f() i32 { u user_t = {}\nreturn u.missing }\n"

let () =
  let anonymous_field_container =
    c_import_container "anonymous-field-container"
      "struct FasAnonField { struct { int x; } inner; int tail; };\n\
       union FasAnonUnionField { struct { short s; } inner; int word; };\n\
       struct FasAnonArrayField { struct { int w; } items[2]; int tail; };\n\
       struct FasAnonDeepField { struct { struct { int y; } deep; }; int tail; };\n\
       typedef struct { struct { int y; } inner; int tail; } FasAnonTypedefField;\n"
  in
  c_semantic_message "c-import-anonymous-record-field"
    "C declaration `FasAnonField` is not supported: anonymous record type of field \
     `inner` in `struct FasAnonField` is not supported"
    anonymous_field_container "fn f() i32 { a FasAnonField = {}\nreturn a.tail }\n";
  c_semantic_message "c-import-anonymous-union-record-field"
    "C declaration `FasAnonUnionField` is not supported: anonymous record type of \
     field `inner` in `union FasAnonUnionField` is not supported"
    anonymous_field_container "fn f() i32 { a FasAnonUnionField = {}\nreturn a.word }\n";
  c_semantic_message "c-import-anonymous-array-record-field"
    "C declaration `FasAnonArrayField` is not supported: anonymous record type of \
     field `items` in `struct FasAnonArrayField` is not supported"
    anonymous_field_container "fn f() i32 { a FasAnonArrayField = {}\nreturn a.tail }\n";
  c_semantic_message "c-import-anonymous-deep-record-field"
    "C declaration `FasAnonDeepField` is not supported: anonymous record type of field \
     `deep` is not supported"
    anonymous_field_container "fn f() i32 { a FasAnonDeepField = {}\nreturn a.tail }\n";
  c_semantic_message "c-import-anonymous-typedef-record-field"
    "C declaration `FasAnonTypedefField` is not supported: anonymous record type of \
     field `inner` is not supported"
    anonymous_field_container
    "fn f() i32 { a FasAnonTypedefField = {}\nreturn a.tail }\n";
  let anonymous_variable_container =
    c_import_container "anonymous-variable-container"
      "struct { int x; } fas_anon_variable;\n"
  in
  c_semantic_message "c-import-anonymous-record-variable"
    "C declaration `fas_anon_variable` is not supported: struct and union values are \
     not supported"
    anonymous_variable_container "fn f() i32 { return fas_anon_variable }\n"

let () =
  let nested_container =
    c_import_container "nested-tag-container"
      "struct NestedOuter { struct NestedInner { int x; } pos; int tag; };\n\
       union NestedUnion { struct NestedRec { short s; char c; } rec; unsigned word; };\n\
       struct NestedDeep { struct NestedMid { struct NestedLeaf { int v; } leaf; int \
       m; } mid; int d; };\n\
       int nested_read_inner(struct NestedInner *value);\n\
       int nested_local_probe(void) { struct NestedLocal { int w; } v; v.w = 3; return \
       v.w; }\n"
  in
  c_semantic_accept "c-import-nested-tag-records" nested_container
    "fn g(p handle[NestedInner]) i32 { return nested_read_inner(p) }\n\
     fn f() i32 { o NestedOuter = {}\n\
     o.pos.x = 5\n\
     o.tag = 7\n\
     u NestedUnion = {}\n\
     u.rec.s = 3\n\
     d NestedDeep = {}\n\
     d.mid.leaf.v = 11\n\
     i NestedInner = {}\n\
     r NestedRec = {}\n\
     m NestedMid = {}\n\
     l NestedLeaf = {}\n\
     l.v = 2\n\
     return o.pos.x + o.tag + u.rec.s + d.mid.leaf.v + i.x + r.s + m.m + l.v }\n";
  c_semantic_message "c-import-nested-tag-function-local-invisible"
    "unknown type `NestedLocal`" nested_container
    "fn f() i32 { u NestedLocal = {}\nreturn 0 }\n"

let () =
  let fixture =
    c_import_container ~macro_names:[ "counter" ] "stdio-self-macro"
      "extern int counter;\n#define counter counter\n"
  in
  c_semantic_accept "c-import-self-macro-preserves-global" fixture
    "fn f() i32 { return counter }\n";
  c_semantic_message "c-import-self-macro-global-type"
    "cannot return `i32` as `u8`: negative values change meaning" fixture
    "fn f() u8 { return counter }\n"

let () =
  semantic_accept "brace-vector-static-contexts"
    "struct S { v vec[4,u32] }\n\
     const V vec[4,u32] = {3, 2, 1, 0}\n\
     var W vec[4,u32] = {4, 5, 6, 7}\n\
     const T S = {{8, 9, 10, 11}}\n\
     const A arr[2,vec[4,u32]] = {{12, 13, 14, 15}, {16, 17, 18, 19}}\n\
     fn f() u32 { return V[0] + W[1] + T.v[2] + A[1][3] }\n";
  semantic_message "brace-vector-constant-width" "wrong number of vector literal lanes"
    "const V vec[4,u32] = {1, 2}\n"

let () =
  List.iter
    (fun name ->
      semantic_accept
        ("builtin-peer-literal-" ^ name)
        ("fn f(k u32) u32 { return " ^ name ^ "(k, 2) + " ^ name ^ "(2, k) }\n");
      semantic_message
        ("builtin-peer-range-" ^ name)
        "integer literal is out of range for u8: `256`"
        ("fn f(k u8) u8 { return " ^ name ^ "(k, 256) }\n"))
    [ "add_sat"; "sub_sat"; "mul_hi" ];
  semantic_accept "builtin-peer-default-literals"
    "fn f() i32 { return add_sat(1, 2) + sub_sat(3, 1) + mul_hi(4, 5) }\n";
  semantic_accept "builtin-store-literal-context"
    "fn f(p addr, m vec[4,bool]) void { volatile_store[u32](p, 37)\n\
     masked_store[u32](p, m, splat(41))\n\
     return }\n"

let () =
  List.iter
    (fun name ->
      semantic_accept
        ("builtin-rotate-literal-" ^ name)
        ("fn f(k u32, v vec[4,u32]) u32 { return " ^ name ^ "(k, 1) + " ^ name
       ^ "(v, 1)[0] }\n"))
    [ "rotl"; "rotr" ];
  List.iter
    (fun (name, arguments) ->
      semantic_accept
        ("builtin-destination-literal-" ^ name)
        ("fn f() u32 { return " ^ name ^ "(" ^ arguments ^ ") }\n"))
    [
      ("rotl", "0x80000001, 1");
      ("rotr", "0x80000001, 1");
      ("popcount", "0xffffffff");
      ("clz", "1");
      ("ctz", "0x80000000");
      ("add_sat", "0xfffffff0, 37");
      ("sub_sat", "0, 1");
      ("mul_hi", "0xffffffff, 0xffffffff");
    ];
  semantic_accept "builtin-destination-literal-contexts"
    "const C u32 = rotl(0x80000001, 1)\n\
     var G u32 = popcount(0xffffffff)\n\
     fn accept(value u32) u32 { return value }\n\
     fn from_return() u32 { return clz(1) }\n\
     fn from_case(value u32) u32 { switch value {\n\
     case popcount(0xffffffff): return 1\n\
     default: return 0 } }\n\
     fn from_branch(condition bool) u32 { return if condition { ctz(0x80000000) } else \
     { 1 } }\n\
     fn from_assignment() u32 { x u32 = 0\n\
     x = add_sat(0xfffffff0, 37)\n\
     x = accept(popcount(0xffffffff))\n\
     return x }\n\
     fn from_initializer() u32 { x u32 = sub_sat(0, 1)\n\
     return x }\n\
     fn typed_peer(value u32) u32 { return add_sat(value, 1) }\n\
     fn defaults() i32 { return popcount(1) }\n";
  semantic_message "builtin-destination-literal-range"
    "integer literal is out of range for u8: `256`"
    "fn f() u8 { return popcount(256) }\n";
  semantic_accept "builtin-vector-typed-operands"
    "const indices vec[4,u8] = {0, 1, 2, 3}\n\
     fn f(v vec[4,u32], m vec[4,bool], p addr) u32 {\n\
     w vec[4,u32] = {1, 2, 3, 4}\n\
     x vec[4,u32] = add_sat(v, w) + sub_sat(v, w) + mul_hi(v, w)\n\
     y vec[4,u32] = select(m, x, w)\n\
     z vec[4,u32] = shuffle(y, w, indices)\n\
     volatile_store[vec[4,u32]](p, z)\n\
     masked_store[u32](p, m, z)\n\
     return z[0] }\n";
  semantic_message "builtin-store-literal-range"
    "integer literal is out of range for u8: `256`"
    "fn f(p addr) void { volatile_store[u8](p, 256)\nreturn }\n";
  let select_runtime_type_mismatch =
    "fn f(m vec[4,bool], a vec[4,u8], b vec[4,u32]) vec[4,u8] { return select(m, a, b) }\n"
  in
  semantic_pin "builtin-select-type-mismatch" select_runtime_type_mismatch 1
    (String.length
       "fn f(m vec[4,bool], a vec[4,u8], b vec[4,u32]) vec[4,u8] { return select(m, a, "
    + 1)
    1
    "argument 3 of `select` is `vec[4, u32]`, expected the type of argument 2 (`vec[4, \
     u8]`)"
    None;
  let select_runtime_type_twin =
    "fn f(m vec[4,bool], a vec[4,u8], b vec[4,u8]) vec[4,u8] { return select(m, a, b) }\n"
  in
  semantic_accept "builtin-select-type-mismatch-twin" select_runtime_type_twin;
  let shuffle_runtime_type_mismatch =
    "const indices vec[4,u8] = {0, 1, 2, 3}\n\
     fn f(a vec[4,u8], b vec[4,u32]) vec[4,u8] { return shuffle(a, b, indices) }\n"
  in
  semantic_pin "builtin-shuffle-type-mismatch" shuffle_runtime_type_mismatch 2
    (String.length "fn f(a vec[4,u8], b vec[4,u32]) vec[4,u8] { return shuffle(a, " + 1)
    1
    "argument 2 of `shuffle` is `vec[4, u32]`, expected the type of argument 1 \
     (`vec[4, u8]`)"
    None;
  semantic_accept "builtin-shuffle-type-mismatch-twin"
    "const indices vec[4,u8] = {0, 1, 2, 3}\n\
     fn f(a vec[4,u8], b vec[4,u8]) vec[4,u8] { return shuffle(a, b, indices) }\n";
  let select_constant_mask_type =
    "const MASK vec[2,i32] = {0, 1}\n\
     const LEFT vec[2,i32] = {1, 2}\n\
     const RIGHT vec[2,i32] = {3, 4}\n\
     const RESULT vec[2,i32] = select(MASK, LEFT, RIGHT)\n"
  in
  semantic_pin "builtin-select-constant-mask-type" select_constant_mask_type 4 34 4
    "argument 1 of `select` is `vec[2, i32]`, expected a bool vector" None;
  semantic_accept "builtin-select-constant-mask-twin"
    "const MASK vec[2,bool] = {true, false}\n\
     const LEFT vec[2,i32] = {1, 2}\n\
     const RIGHT vec[2,i32] = {3, 4}\n\
     const RESULT vec[2,i32] = select(MASK, LEFT, RIGHT)\n";
  let select_constant_value_type =
    "const MASK vec[2,bool] = {true, false}\n\
     const LEFT vec[2,i32] = {1, 2}\n\
     const RIGHT vec[2,i64] = {3, 4}\n\
     const RESULT vec[2,i32] = select(MASK, LEFT, RIGHT)\n"
  in
  semantic_pin "builtin-select-constant-value-type" select_constant_value_type 4 46 5
    "argument 3 of `select` is `vec[2, i64]`, expected the type of argument 2 (`vec[2, \
     i32]`)"
    None;
  semantic_accept "builtin-select-constant-value-twin"
    "const MASK vec[2,bool] = {true, false}\n\
     const LEFT vec[2,i32] = {1, 2}\n\
     const RIGHT vec[2,i32] = {3, 4}\n\
     const RESULT vec[2,i32] = select(MASK, LEFT, RIGHT)\n"

let () =
  semantic_accept "bitcast-literal-default-i32"
    "const V vec[4,u8] = bitcast[vec[4,u8]](0x00010203)\n\
     const W vec[2,u16] = bitcast[vec[2,u16]](-1)\n\
     fn f() u8 { return bitcast[vec[4,u8]](0x00010203)[0] + V[1] }\n";
  semantic_message "bitcast-literal-equal-bits"
    "illegal `bitcast` from integer literal `1` to `u64`: the source is 32 bits and \
     the destination is 64 bits"
    "fn f() u64 { return bitcast[u64](1) }\n";
  semantic_message "zext-literal-source"
    "illegal `zext` from integer literal `256` to `u8`: the destination must be a \
     wider integer type"
    "fn f() u8 { return zext[u8](256) }\n";
  semantic_message "bitcast-vector-literal-source"
    "illegal `bitcast` from integer literal `204` to `vec[8, bool]`: the source is 32 \
     bits and the destination is 8 bits"
    "fn f() vec[8,bool] { return bitcast[vec[8,bool]](204) }\n";
  semantic_message "generic-cast-target-template"
    "illegal `zext` from `i32` to `void`: the source and destination must be integer \
     types"
    "fn choose[N const i32]() i32 {\n\
     if N == 1 { return 7 } else { return zext[void](N) }\n\
     }\n\
     fn main() i32 { return choose[1]() }\n"

let () =
  semantic_accept "vector-peer-context-all-slots"
    "const indices vec[4,u8] = {0, 1, 2, 3}\n\
     fn f(v vec[4,u32], m vec[4,bool], ok bool) vec[4,u32] {\n\
     a vec[4,u32] = add_sat(v, splat(37)) + sub_sat(splat(37), v)\n\
     b vec[4,u32] = mul_hi(v, {1, 2, 3, 4})\n\
     c vec[4,bool] = v == {1, 2, 3, 4}\n\
     d vec[4,bool] = {1, 2, 3, 4} == v\n\
     e vec[4,u32] = select(m, splat(0), v)\n\
     g vec[4,u32] = select(m, v, {1, 2, 3, 4})\n\
     h vec[4,u32] = shuffle(splat(0), v, indices)\n\
     i vec[4,u32] = if ok { splat(0) } else { v }\n\
     j vec[4,u32] = if ok { v } else { {1, 2, 3, 4} }\n\
     return a + b + e + g + h + i + j }\n";
  semantic_message "vector-peer-no-typed-peer"
    "`splat` needs a vector destination or vector operand to determine its lane count"
    "fn f() void { add_sat(splat(1), splat(2))\nreturn }\n";
  semantic_message "vector-result-does-not-infer-splat"
    "`splat` needs a vector destination or vector operand to determine its lane count"
    "fn f() vec[4,u32] { return add_sat(splat(1), splat(2)) }\n";
  semantic_message "vector-peer-scalar-peer"
    "`splat` needs a vector destination or vector operand to determine its lane count"
    "fn f(k u32) void { add_sat(k, splat(2))\nreturn }\n";
  semantic_message "splat-rejects-type-argument" "`splat` takes no type arguments"
    "fn f() u8 { return splat[u8](1) }\n";
  let shift_splat_source =
    "fn f(values vec[4,u32]) vec[4,u32] { return values << splat(1) }\n"
  in
  semantic_pin "shift-splat-vector-count" shift_splat_source 1
    (String.length "fn f(values vec[4,u32]) vec[4,u32] { return values << " + 1)
    5 "the shift count `splat(1)` has no vector type" (Some "write `values << 1`");
  let const_shift_splat_source =
    "const VALUES vec[4,u32] = {1, 2, 3, 4}\n\
     const RESULT vec[4,u32] = VALUES << splat(1)\n"
  in
  semantic_pin "constant-shift-splat-vector-count" const_shift_splat_source 2
    (String.length "const RESULT vec[4,u32] = VALUES << " + 1)
    5 "the shift count `splat(1)` has no vector type" (Some "write `VALUES << 1`");
  semantic_message "logical-not-vector-type"
    "operator `!` needs `bool` or a bool vector, got `vec[4, i64]`"
    "fn f(value vec[4,i64]) vec[4,bool] { return !value }\n";
  semantic_message "ordered-comparison-vector-type"
    "ordered comparison `<` needs an integer or integer vector, got `vec[4, bool]`"
    "fn f(value vec[4,bool]) vec[4,bool] { return value < value }\n";
  semantic_message "addition-vector-type"
    "operator `+` needs integer or integer vector operands, got `vec[4, bool]`"
    "fn f(value vec[4,bool]) vec[4,bool] { return value + value }\n";
  semantic_message "vector-peer-wrong-width" "wrong number of vector literal lanes"
    "fn f(v vec[4,u32]) vec[4,bool] { return v == {1, 2} }\n";
  semantic_accept "vector-peer-folding-reverse-arms"
    "const V vec[4,u32] = {1, 2, 3, 4}\n\
     const MASK vec[4,bool] = {true, false, true, false}\n\
     const A vec[4,u32] = select(MASK, splat(0), V)\n\
     const B vec[4,u32] = if true { splat(0) } else { V }\n\
     const D vec[4,u32] = if false { {0, 0, 0, 0} } else { V }\n\
     fn f() u32 { return A[1] + B[2] + D[3] }\n"

let () =
  semantic_accept "shuffle-brace-width-and-peer"
    "const V vec[4,u32] = {1, 2, 3, 4}\n\
     const R vec[2,u32] = shuffle(V, V, {7, 0})\n\
     fn f(v vec[4,u32]) vec[6,u32] { return shuffle(v, splat(0), {3, 2, 1, 0, 4, 7}) }\n";
  List.iter
    (fun selector ->
      semantic_message
        ("shuffle-brace-range-" ^ selector)
        ("shuffle index `" ^ selector ^ "` is out of range for 8 lanes")
        ("fn f(v vec[4,u32]) vec[1,u32] { return shuffle(v, v, {" ^ selector ^ "}) }\n"))
    [ "-1"; "8" ];
  semantic_message "shuffle-brace-nonconstant"
    "shuffle indices must be a compile-time constant integer vector"
    "fn f(v vec[4,u32], i u32) vec[2,u32] { return shuffle(v, v, {0, i}) }\n";
  let shuffle_missing_const =
    "const V vec[4,u32] = {1, 2, 3, 4}\n\
     const R vec[2,u32] = shuffle(V, V, {0, missing})\n"
  in
  semantic_pin "shuffle-brace-const-nonconstant" shuffle_missing_const 2 40 7
    "unknown name `missing`" None;
  let reduce_runtime_scalar =
    "fn maximum(value i16) i16 { return reduce_max(value) }\n"
  in
  semantic_pin "reduce-scalar-runtime-agreement" reduce_runtime_scalar 1
    (String.length "fn maximum(value i16) i16 { return reduce_max(" + 1)
    5 "`reduce_max` needs an integer vector, got `i16`" None;
  semantic_accept "reduce-scalar-runtime-agreement-twin"
    "fn maximum(value vec[4,i16]) i16 { return reduce_max(value) }\n";
  let reduce_constant_scalar =
    "const SAMPLE i16 = 9\nconst MAXIMUM i16 = reduce_max(SAMPLE)\n"
  in
  semantic_pin "reduce-scalar-constant-agreement" reduce_constant_scalar 2 32 6
    "`reduce_max` needs an integer vector, got `i16`" None;
  semantic_accept "reduce-scalar-constant-agreement-twin"
    "const SAMPLE vec[4,i16] = {1, 2, 3, 4}\nconst MAXIMUM i16 = reduce_max(SAMPLE)\n";
  let add_sat_runtime_scalar =
    "fn saturate(value vec[4,u8]) vec[4,u8] { return add_sat(value, 2) }\n"
  in
  semantic_pin "add-sat-vector-scalar-runtime-agreement" add_sat_runtime_scalar 1
    (String.length "fn saturate(value vec[4,u8]) vec[4,u8] { return add_sat(value, " + 1)
    1 "argument 2 of `add_sat` is an integer literal, expected `vec[4, u8]`"
    (Some "write `splat(2)`");
  semantic_accept "add-sat-vector-scalar-runtime-agreement-twin"
    "fn saturate(value vec[4,u8]) vec[4,u8] { return add_sat(value, splat(2)) }\n";
  let add_sat_constant_scalar =
    "const INPUT vec[4,u8] = splat(1)\nconst OUTPUT vec[4,u8] = add_sat(INPUT, 2)\n"
  in
  semantic_pin "add-sat-vector-scalar-constant-agreement" add_sat_constant_scalar 2 41 1
    "argument 2 of `add_sat` is an integer literal, expected `vec[4, u8]`"
    (Some "write `splat(2)`");
  semantic_accept "add-sat-vector-scalar-constant-agreement-twin"
    "const INPUT vec[4,u8] = splat(1)\n\
     const OUTPUT vec[4,u8] = add_sat(INPUT, splat(2))\n";
  let shuffle_missing_index =
    "fn read(value vec[4,u32]) vec[2,u32] { return shuffle(value, value, {0, \
     absent_index}) }\n"
  in
  semantic_pin "shuffle-inline-unknown-index" shuffle_missing_index 1
    (String.length
       "fn read(value vec[4,u32]) vec[2,u32] { return shuffle(value, value, {0, "
    + 1)
    12 "unknown name `absent_index`" None;
  semantic_accept "shuffle-inline-unknown-index-twin"
    "const PRESENT_INDEX i64 = 1\n\
     fn read(value vec[4,u32]) vec[2,u32] { return shuffle(value, value, {0, \
     PRESENT_INDEX}) }\n";
  semantic_accept "shuffle-inline-const-index"
    "const SLOT i64 = 2\n\
     fn read(value vec[4,u32]) vec[2,u32] { return shuffle(value, value, {0, SLOT}) }\n"

let () =
  let bool_vector_compound =
    "fn combine(left vec[2,bool], right vec[2,bool]) void { left &= right\nreturn }\n"
  in
  semantic_pin "bool-vector-compound-assignment" bool_vector_compound 1
    (String.index bool_vector_compound '&' + 1)
    2 "compound assignment `&=` is not defined for `vec[2, bool]`" None

let () =
  let bool_compound =
    "fn combine(left bool, right bool) bool { left &= right\nreturn left }\n"
  in
  semantic_pin "bool-compound-help" bool_compound 1
    (String.index bool_compound '&' + 1)
    2 "compound assignment `&=` is not defined for `bool`"
    (Some "write `left = left & right`");
  semantic_accept "bool-compound-help-twin"
    "fn combine(left bool, right bool) bool { left = left & right\nreturn left }\n"

let () =
  let dir = Filename.dirname (fst (c_import_fixture "container_followup.h")) in
  let file = Filename.concat dir "container_order_cases.fas" in
  let span = Span.make ~file ~start_offset:0 ~end_offset:0 ~line:30 ~column:1 in
  let requests =
    C_import.
      [
        {
          spelling =
            Ast.C_fragment
              { tag = "C"; text = "typedef struct { int before; } BeforeContainer;\n" };
          span;
        };
        { spelling = Ast.C_quoted "container_followup.h"; span };
      ]
  in
  let nodes, _, _ =
    expect_ok (C_import.import ~cc:"clang-22" ~keep:false file requests)
  in
  let fixture = (file, C_import.map_declarations ~span nodes) in
  c_semantic_accept "c-import-anonymous-header-after-container" fixture
    "fn f() i32 { a AfterContainer = {}\n\
     b BeforeContainer = {}\n\
     return a.after + b.before }\n";
  c_semantic_message "c-import-anonymous-header-after-container-field"
    "record `AfterContainer` has no field `missing`" fixture
    "fn f() i32 { a AfterContainer = {}\nreturn a.missing }\n"

let () =
  let prefix = "struct Ring[N const usize] { head u32\nitems arr[N,u32] }\n" in
  semantic_accept "generic-struct-raw-type-slots"
    (prefix
   ^ "fn f[N const usize](p addr) u32 { view r = p[Ring[N], 0]\n\
     \ return r.head }\n\
     \ fn g(p addr) u32 { return p[Ring[4]].head + f[4](p) }\n\
     \ fn h[T](p addr) u32 { return p[T].head }\n\
     \ fn j(p addr) u32 { return h[Ring[4]](p) }\n");
  semantic_accept "aggregate-raw-type-slots"
    "fn f(p addr) u32 { view a = p[arr[4,u32]]\n\
    \ view v = p[vec[4,u32], 1]\n\
    \ return a[0] + v[0] }\n";
  semantic_accept "nested-index-and-type-shadow"
    (prefix
   ^ "fn f() u32 { a arr[4,u32] = {}\n\
     \ Ring arr[1,u32] = {1}\n\
     \ return a[Ring[0]] + a[Ring[0] + 1] }\n");
  semantic_accept "generic-nested-index-shadow"
    (prefix
   ^ "fn f[N const usize](p addr) u32 { view a = p[arr[4,u32]]\n\
     \ view Ring = p[arr[1,u32]]\n\
     \ return a[Ring[0]] }\n\
     \ fn g(p addr) u32 { return f[4](p) }");
  semantic_message "nested-index-bounds" "array index `1` is out of bounds for length 1"
    "fn f() u32 { a arr[4,u32] = {}\n i arr[1,u32] = {0}\n return a[i[1]] }";
  semantic_message "nested-index-noninteger"
    "array index must be an integer, got `bool`"
    "fn f() u32 { a arr[4,u32] = {}\n i arr[1,bool] = {true}\n return a[i[0]] }";
  List.iter
    (fun ty ->
      let body call = prefix ^ "fn f(p addr) void { " ^ call ^ "\nreturn }" in
      List.iter
        (fun name ->
          let args = if name = "volatile_load" then "p" else "p, 0" in
          semantic_message
            ("generic-slot-" ^ name ^ "-" ^ ty)
            "volatile access type must be a scalar integer, bool, addr, handle[T], \
             vec[N, integer], or vec[N, bool]"
            (body (name ^ "[" ^ ty ^ "](" ^ args ^ ")")))
        [ "volatile_load"; "volatile_store" ];
      List.iter
        (fun name ->
          semantic_message
            ("generic-slot-" ^ name ^ "-" ^ ty)
            (Printf.sprintf "`%s` needs an integer or bool element type, got `%s`" name
               (String.split_on_char ',' ty |> String.concat ", "))
            (body (name ^ "[" ^ ty ^ "](p, 0, 0, 0)")))
        [
          "masked_load";
          "masked_store";
          "gather";
          "scatter";
          "gather_bytes";
          "scatter_bytes";
        ];
      if ty = "arr[4,u32]" then
        semantic_message ("generic-slot-handle-" ^ ty)
          "handle type argument must be an opaque type"
          (body ("h handle[" ^ ty ^ "] = handle_from_addr[" ^ ty ^ "](p)"));
      List.iter
        (fun name ->
          let reason =
            if name = "bitcast" then
              "both types must be bool, integer, or integer-vector types of equal width"
            else "the source and destination must be integer types"
          in
          let printed_ty = String.split_on_char ',' ty |> String.concat ", " in
          semantic_message
            ("generic-slot-conversion-" ^ name ^ "-" ^ ty)
            (Printf.sprintf "illegal `%s` from integer literal `1` to `%s`: %s" name
               printed_ty reason)
            (body ("v " ^ ty ^ " = " ^ name ^ "[" ^ ty ^ "](1)")))
        [ "bitcast"; "zext"; "sext"; "trunc" ])
    [ "Ring[4]"; "arr[4,u32]" ];
  semantic_accept "generic-slot-handle-generic-struct"
    (prefix
   ^ "fn f(p addr) void { h handle[Ring[4]] = handle_from_addr[Ring[4]](p)\nreturn }");
  semantic_accept "vector-volatile-type-slot"
    "fn f(p addr) void { volatile_store[vec[4,u32]](p, splat(7))\n\
    \ volatile_load[vec[4,u32]](p)\n\
     return }";
  semantic_accept "vector-conversion-type-slot"
    "fn f() vec[4,u8] { return bitcast[vec[4,u8]](0x00010203) }";
  semantic_accept "aggregate-explicit-call-type-slots"
    (prefix
   ^ "fn f[T](p addr) usize { return sizeof[T] }\n\
     \ fn g(p addr) usize { return f[Ring[4]](p) + f[arr[4,u32]](p) + f[vec[4,u32]](p) \
      }")

let () =
  semantic_accept "generic-raw-independent-struct-instance"
    "struct Ring[N const usize] { head u32 }\n\
    \ fn f[N const usize](p addr) u32 { return p[Ring[4]].head }\n\
    \ fn g(p addr) u32 { return f[4](p) }";
  semantic_message "generic-type-slot-value-index" "expected a type argument"
    "fn f[T]() void { return }\n\
    \ fn g() void { i arr[1,u32] = {0}\n\
    \ f[i[0]]()\n\
    \ return }"

let () =
  semantic_pin "c-size_t-parameter" "fn f(p size_t) usize { return 0 }\n" 1 8 6
    "`size_t` is not a Fas type; parameters are declared as `name Type`, e.g. `p usize`"
    None;
  semantic_pin "c-int-parameter" "fn f(p int) i32 { return 0 }\n" 1 8 3
    "`int` is not a Fas type; parameters are declared as `name Type`, e.g. `p i32`" None;
  semantic_pin "c-char-parameter" "fn f(p char) i32 { return 0 }\n" 1 8 4
    "`char` is not a Fas type; parameters are declared as `name Type`, e.g. `p i8`" None;
  semantic_pin "c-double-parameter" "fn f(p double) i32 { return 0 }\n" 1 8 6
    "`double` is not a Fas type; parameters are declared as `name Type`, e.g. `p f64`"
    None;
  semantic_pin "c-size_t-array-local" "fn f() usize { xs size_t[4]\nreturn 0 }\n" 1 19 6
    "`size_t` is not a Fas type; locals are declared as `name Type`, e.g. `xs \
     arr[4,usize]`"
    None;
  semantic_pin "c-char-array-local-generic"
    "fn f[N const usize]() usize { xs char[N]\n\
     return 0 }\n\
     fn g() usize { return f[4]() }\n"
    1 34 4
    "`char` is not a Fas type; locals are declared as `name Type`, e.g. `xs arr[N,i8]`"
    None;
  semantic_pin "user-generic-array-local" "fn f() usize { xs Foo[4]\nreturn 0 }\n" 1 19
    3 "unknown generic struct `Foo`" None;
  semantic_pin "c-size_t-array-parameter" "fn f(p size_t[4]) usize { return 0 }\n" 1 8 6
    "`size_t` is not a Fas type; parameters are declared as `name Type`, e.g. `p \
     arr[4,usize]`"
    None;
  semantic_pin "c-size_t-local-type-caret" "fn f() usize { p size_t = 0\nreturn 0 }\n" 1
    18 6
    "`size_t` is not a Fas type; locals are declared as `name Type = value`, e.g. `p \
     usize`"
    None;
  semantic_pin "c-uint8_t-field" "struct S { p uint8_t }\n" 1 14 7
    "`uint8_t` is not a Fas type; fields are declared as `name Type`, e.g. `p u8`" None;
  semantic_pin "c-int-field" "struct S { p int }\n" 1 14 3
    "`int` is not a Fas type; fields are declared as `name Type`, e.g. `p i32`" None;
  semantic_pin "c-size_t-array-field" "struct S { p size_t[4] }\n" 1 14 6
    "`size_t` is not a Fas type; fields are declared as `name Type`, e.g. `p \
     arr[4,usize]`"
    None;
  semantic_pin "c-size_t-field" "struct S { p size_t }\n" 1 14 6
    "`size_t` is not a Fas type; fields are declared as `name Type`, e.g. `p usize`"
    None;
  semantic_accept "user-struct-size_t-field"
    "struct size_t { x i32 }\nstruct S { p size_t }\nfn main() i32 { return 0 }\n";
  semantic_pin "user-struct-size_t-parameter"
    "struct size_t { x i32 }\nfn f(p size_t) i32 { return p.x }\n" 2 8 6
    "aggregate parameter `p` of type `size_t` cannot be passed by value; declare `p` \
     as `addr`; callers pass its address with `&`"
    None;

  let pin body token expected generic =
    let header =
      if generic then "fn f[N const usize]() void { " else "fn f() void { "
    in
    let text =
      header ^ body ^ "\nreturn }"
      ^ if generic then "\nfn g() void { f[4]()\nreturn }" else ""
    in
    let result =
      match Parser.parse (source text) with
      | Error diagnostics -> Error diagnostics
      | Ok program -> Sema.check program
    in
    incr checks_run;
    match result with
    | Error [ diagnostic ] ->
        let offset = String.length header + List.hd (positions body token) in
        if
          diagnostic.Diag.message <> expected
          || diagnostic.primary.start_offset <> offset
          || diagnostic.primary.end_offset <> offset + String.length token
        then
          failwith
            ("declaration diagnostic/span: " ^ body ^ ": "
            ^ Diag.render_all ~source:None [ diagnostic ])
    | _ -> failwith ("expected declaration rejection: " ^ body)
  in
  List.iter
    (fun generic ->
      List.iter
        (fun word ->
          pin (word ^ " x = 5") word
            ("locals are declared as `name Type = value`; `" ^ word
           ^ "` is not a Fas keyword")
            generic)
        [ "let"; "auto"; "mut" ];
      List.iter
        (fun (word, equivalent) ->
          pin (word ^ " x = 5") word
            ("`" ^ word
           ^ "` is not a Fas type; locals are declared as `name Type = value`, e.g. `x "
           ^ equivalent ^ "`")
            generic)
        [
          ("int", "i32");
          ("char", "i8");
          ("short", "i16");
          ("long", "i64");
          ("unsigned", "u32");
          ("signed", "i32");
          ("float", "f32");
          ("double", "f64");
        ];
      List.iter
        (fun (word, equivalent) ->
          pin (word ^ " x = 5") word
            ("`" ^ word
           ^ "` is not a Fas type; locals are declared as `name Type = value`, e.g. `x "
           ^ equivalent ^ "`")
            generic)
        [
          ("size_t", "usize");
          ("ssize_t", "isize");
          ("ptrdiff_t", "isize");
          ("intptr_t", "isize");
          ("uintptr_t", "usize");
          ("int8_t", "i8");
          ("int16_t", "i16");
          ("int32_t", "i32");
          ("int64_t", "i64");
          ("uint8_t", "u8");
          ("uint16_t", "u16");
          ("uint32_t", "u32");
          ("uint64_t", "u64");
        ];
      pin "let x i32 = 5" "let"
        "locals are declared as `name Type = value`; `let` is not a Fas keyword" generic;
      pin "auto x i32 = 5" "auto"
        "locals are declared as `name Type = value`; `auto` is not a Fas keyword"
        generic;
      pin "mut x i32 = 5" "mut"
        "locals are declared as `name Type = value`; `mut` is not a Fas keyword" generic;
      pin "char *s = 0" "char *" "C type `char*` is not a Fas type; use `addr`" generic;
      pin "int *p = 0" "int *"
        "C type `int*` is not a Fas type; use `addr`, or `handle[T]` when `T` is an \
         `opaque` type"
        generic;
      pin "void *v = 0" "void *" "C type `void*` is not a Fas type; use `addr`" generic;
      pin "int (*fp)(int) = 0" "int"
        "C function pointer declarations are not Fas syntax; carry a function address \
         in an `addr` and call it with `call_addr[R](p, args...)`"
        generic;
      pin "u32 x = 5" "u32" "locals are declared as `name Type = value`; write `x u32`"
        generic;
      pin "var x i32 = 5" "var"
        "`var` declares globals; locals are declared as `name Type = value`" generic;
      pin "x := 5" ":" "locals are declared as `name Type = value`; Fas has no `:=`"
        generic;
      pin "x++" "++" "Fas has no postfix `++` operator; write `x += 1`" generic;
      pin "x--" "--" "Fas has no postfix `--` operator; write `x -= 1`" generic)
    [ false; true ];
  List.iter
    (fun word ->
      semantic_accept
        ("declaration-word-binding-" ^ word)
        ("fn f() i32 { " ^ word ^ " i32 = 5\n return " ^ word ^ " }");
      semantic_accept
        ("declaration-word-valid-user-type-" ^ word)
        ("struct x { value i32 }\n fn f() i32 { " ^ word ^ " x = {5}\n return " ^ word
       ^ ".value }");
      semantic_accept
        ("declaration-word-generic-binding-" ^ word)
        ("fn f[T](value T) T { " ^ word ^ " T = value\n return " ^ word
       ^ " }\n fn g() i32 { return f[i32](5) }"))
    [ "let"; "auto"; "mut" ];
  List.iter
    (fun equivalent ->
      semantic_accept
        ("fixed-width-local-twin-" ^ equivalent)
        ("fn f() " ^ equivalent ^ " { n " ^ equivalent ^ " = 0\n return n }"))
    [ "usize"; "isize"; "i8"; "i16"; "i32"; "i64"; "u8"; "u16"; "u32"; "u64" ];
  semantic_accept "separated-plus-unary-keeps-syntax"
    "fn f(x i32) i32 { return x + -1 }";
  pin "for var x i32 = 0; x < 4; x += 1 {}" "var"
    "`var` declares globals; locals are declared as `name Type = value`" false

let () =
  List.iter
    (fun name ->
      semantic_message ("vector-type-slot-" ^ name)
        (Printf.sprintf "`%s` needs an integer or bool element type, got `vec[4, u32]`"
           name)
        ("fn f(p addr) void { " ^ name ^ "[vec[4,u32]](p, 0, 0, 0)\nreturn }"))
    [
      "masked_load";
      "masked_store";
      "gather";
      "scatter";
      "gather_bytes";
      "scatter_bytes";
    ];
  semantic_message "vector-handle-type-slot"
    "handle type argument must be an opaque type"
    "fn f(p addr) void { h handle[vec[4,u32]] = handle_from_addr[vec[4,u32]](p)\n\
     return }";
  List.iter
    (fun name ->
      semantic_message
        ("vector-conversion-rejection-" ^ name)
        (Printf.sprintf "illegal `%s` from integer literal `1` to `vec[4, u32]`: %s"
           name
           (if name = "trunc" then
              "the destination must be a narrower integer type with the same vector \
               lane count"
            else
              "the destination must be a wider integer type with the same vector lane \
               count"))
        ("fn f() void { v vec[4,u32] = " ^ name ^ "[vec[4,u32]](1)\nreturn }"))
    [ "zext"; "sext"; "trunc" ];
  List.iter
    (fun name ->
      semantic_accept
        ("peer-vector-constructor-" ^ name)
        ("fn f(v vec[4,u32]) vec[4,u32] { return " ^ name ^ "(v, splat(2)) + " ^ name
       ^ "({1,2,3,4}, v) }");
      semantic_message ("peer-vector-range-" ^ name)
        "integer literal is out of range for u8: `256`"
        ("fn f(v vec[4,u8]) vec[4,u8] { return " ^ name ^ "(v, splat(256)) }"))
    [ "add_sat"; "sub_sat"; "mul_hi" ];
  List.iter
    (fun name ->
      semantic_message ("peer-rotate-range-" ^ name)
        "integer literal is out of range for i32: `4294967296`"
        ("fn f(k u8) u8 { return " ^ name ^ "(k, 4294967296) }"))
    [ "rotl"; "rotr" ];
  semantic_message "peer-masked-store-range"
    "integer literal is out of range for u8: `256`"
    "fn f(p addr, m vec[4,bool]) void { masked_store[u8](p, m, splat(256))\nreturn }";
  semantic_message "peer-shuffle-constructor-range"
    "integer literal is out of range for u8: `256`"
    "fn f(v vec[4,u8]) vec[4,u8] { return shuffle(v, splat(256), {0,1,2,3}) }";
  semantic_message "peer-select-constructor-range"
    "integer literal is out of range for u8: `256`"
    "fn f(m vec[4,bool], v vec[4,u8]) vec[4,u8] { return select(m, v, splat(256)) }";
  semantic_accept "generic-index-arithmetic"
    "fn f[N const usize](p addr) u32 { view a = p[arr[4,u32]]\n\
    \ view i = p[arr[1,u32]]\n\
    \ return a[i[0] + 1] }\n\
    \ fn g(p addr) u32 { return f[4](p) }"

let () =
  semantic_accept "rotate-count-independent-width"
    "fn f(k u8) u8 { return rotl(k, 256) + rotr(k, -256) }"

let () =
  semantic_accept "generic-brace-local-index-shadow"
    "struct Ring[N const usize] { head u32 }\n\
    \ fn f[N const usize]() u32 { a arr[4,u32] = {4,5,6,7}\n\
    \ Ring arr[1,u32] = {1}\n\
    \ return a[Ring[0]] }\n\
    \ fn g() u32 { return f[4]() }";
  semantic_message "generic-brace-local-width" "array of 4 elements, got 2"
    "fn f[N const usize]() u32 { a arr[4,u32] = {4,5}\n\
     return 0 }\n\
    \ fn g() u32 { return f[4]() }"

let () =
  semantic_message "value-fact-index-arithmetic"
    "array index is out of bounds for length 4"
    "fn f() u8 { values arr[4,u8] = {0,1,2,3}\n\
     index usize = 2 + 2\n\
     return values[index] }";
  semantic_message "value-fact-branch-joined-index"
    "array index is out of bounds for length 4"
    "fn f(flag bool) u8 { values arr[4,u8] = {0,1,2,3}\n\
     index usize = 0\n\
     if flag { index = 4 } else { index = 4 }\n\
     return values[index] }";
  semantic_accept "value-fact-branch-hull-keeps-unknown"
    "fn f(flag bool) u8 { values arr[4,u8] = {0,1,2,3}\n\
     index usize = 0\n\
     if flag { index = 4 } else { index = 1 }\n\
     return values[index] }";
  semantic_message "value-fact-view-index" "array index is out of bounds for length 4"
    "fn f() u8 { values arr[4,u8] = {0,1,2,3}\n\
     view row = values\n\
     index usize = 4\n\
     return row[index] }";
  semantic_message "value-fact-global-index" "array index is out of bounds for length 2"
    "var Values arr[2,u8] = {1,2}\nfn f() u8 { index usize = 2\nreturn Values[index] }";
  semantic_accept "value-fact-address-escape-keeps-unknown"
    "fn f() u8 { values arr[4,u8] = {0,1,2,3}\n\
     index usize = 4\n\
     pointer addr = &index\n\
     return values[index] }";
  semantic_accept "value-fact-wrapping-update-keeps-unknown"
    "fn f() u8 { values arr[4,u8] = {0,1,2,3}\n\
     index u8 = 255\n\
     index += 1\n\
     return values[index] }";
  semantic_accept "value-fact-specialized-false-guard"
    "fn at[N const usize]() u8 { values arr[4,u8] = {0,1,2,3}\n\
     if N < 4 { return values[N] }\n\
     return 0 }\n\
     fn instantiate() u8 { return at[7]() }";
  semantic_accept "value-fact-local-false-guard"
    "fn f() u8 { values arr[4,u8] = {0,1,2,3}\n\
     index usize = 7\n\
     if index < 4 { return values[index] }\n\
     return 0 }";
  semantic_message "value-fact-comparison-refined-index"
    "array index is out of bounds for length 4"
    "fn f(index usize) u8 { values arr[4,u8] = {0,1,2,3}\n\
     if index >= 4 { return values[index] }\n\
     return 0 }";
  semantic_message "value-fact-generic-index"
    "array index is out of bounds for length 2"
    "fn at[N const usize]() u8 { values arr[N,u8] = {}\n\
     index usize = N\n\
     return values[index] }\n\
     fn instantiate() u8 { return at[2]() }";
  semantic_message "value-fact-vector-lane" "array index is out of bounds for length 2"
    "fn f() u32 { values vec[2,u32] = {7,9}\nlane usize = 1 + 1\nreturn values[lane] }";
  semantic_accept "value-fact-valid-vector-lane"
    "fn f(lane usize) u32 { values vec[2,u32] = {7,9}\n\
     if lane < 2 { return values[lane] }\n\
     return 0 }";
  semantic_accept "value-fact-generic-valid-specialization"
    "fn at[N const usize](index usize) u8 { values arr[N,u8] = {}\n\
     return values[index] }\n\
     fn instantiate(index usize) u8 { return at[4](index) }";
  semantic_accept "value-fact-unknown-index"
    "fn f(index usize) u8 { values arr[4,u8] = {0,1,2,3}\nreturn values[index] }";
  semantic_accept "value-fact-data-dependent-branch-index"
    "fn f(flag bool) u8 { values arr[4,u8] = {0,1,2,3}\n\
     index usize = 1\n\
     if flag { index = 4 }\n\
     return values[index] }";
  semantic_message "value-fact-loop-inclusive-index"
    "array index is out of bounds for length 4"
    "fn f() u8 { values arr[4,u8] = {0,1,2,3}\n\
     result u8 = 0\n\
     for index usize = 0; index <= 4; index += 1 {\n\
     result = values[index]\n\
     }\n\
     return result }";
  semantic_accept "value-fact-loop-exclusive-index"
    "fn f() u8 { values arr[4,u8] = {0,1,2,3}\n\
     result u8 = 0\n\
     for index usize = 0; index < 4; index += 1 {\n\
     result = values[index]\n\
     }\n\
     return result }";
  semantic_message "value-fact-loop-negative-index"
    "array index is out of bounds for length 4"
    "fn f() i32 { values arr[4,i32] = {0,1,2,3}\n\
     result i32 = 0\n\
     for index i32 = 0; index >= -1; index -= 1 {\n\
     result = values[index]\n\
     }\n\
     return result }";
  semantic_accept "value-fact-loop-break-before-index"
    "fn f() u8 { values arr[4,u8] = {0,1,2,3}\n\
     index usize = 4\n\
     for i usize = 0; i <= 4; i += 1 {\n\
     break\n\
     v u8 = values[index]\n\
     }\n\
     return 0 }";
  semantic_accept "value-fact-labeled-break-index"
    "fn f() i32 { values arr[4,i32] = {}\n\
     result i32 = 0\n\
     outer: for i i32 = 0; i < 2; i += 1 {\n\
     for index i32 = 0; index < 10; index += 1 {\n\
     if index == 4 { break outer }\n\
     result += values[index] + index\n\
     }\n\
     }\n\
     return result }";
  semantic_accept "value-fact-labeled-break-twin"
    "fn f() i32 { values arr[4,i32] = {}\n\
     result i32 = 0\n\
     outer: for i i32 = 0; i < 2; i += 1 {\n\
     for index i32 = 0; index < 10; index += 1 {\n\
     if index == 4 { break }\n\
     result += values[index] + index\n\
     }\n\
     }\n\
     return result }";
  semantic_accept "value-fact-labeled-continue-index"
    "fn f() i32 { values arr[4,i32] = {}\n\
     result i32 = 0\n\
     outer: for i i32 = 0; i < 2; i += 1 {\n\
     for index i32 = 0; index < 10; index += 1 {\n\
     if index == 4 { continue outer }\n\
     result += values[index]\n\
     }\n\
     }\n\
     return result }";
  semantic_accept "value-fact-labeled-continue-twin"
    "fn f() i32 { values arr[4,i32] = {}\n\
     result i32 = 0\n\
     outer: for i i32 = 0; i < 2; i += 1 {\n\
     for index i32 = 0; index < 5; index += 1 {\n\
     if index == 4 { continue }\n\
     result += values[index]\n\
     }\n\
     }\n\
     return result }";
  semantic_accept "value-fact-labeled-break-while-index"
    "fn f() i32 { values arr[4,i32] = {}\n\
     result i32 = 0\n\
     outer: while true {\n\
     for index i32 = 0; index < 10; index += 1 {\n\
     if index == 4 { break outer }\n\
     result += values[index]\n\
     }\n\
     }\n\
     return result }";
  semantic_accept "value-fact-labeled-break-while-twin"
    "fn f() i32 { values arr[4,i32] = {}\n\
     result i32 = 0\n\
     outer: while true {\n\
     for index i32 = 0; index < 10; index += 1 {\n\
     if index == 4 { break }\n\
     result += values[index]\n\
     }\n\
     break outer\n\
     }\n\
     return result }";
  semantic_accept "value-fact-loop-unknown-bound"
    "fn f(bound usize) u8 { values arr[4,u8] = {0,1,2,3}\n\
     result u8 = 0\n\
     for index usize = 0; index <= bound; index += 1 {\n\
     result = values[index]\n\
     }\n\
     return result }";
  semantic_message "value-fact-divide-by-zero"
    "division by zero is not a defined runtime operation"
    "fn f() i32 { divisor i32 = 9\ndivisor -= 9\nreturn 81 / divisor }";
  semantic_message "value-fact-remainder-by-zero"
    "division by zero is not a defined runtime operation"
    "fn f() i32 { divisor i32 = 3 - 3\nreturn 81 % divisor }";
  semantic_accept "value-fact-unknown-divisor"
    "fn f(divisor i32) i32 { return 81 / divisor }";
  semantic_message "value-fact-min-div-minus-one"
    "signed division `-2147483648 / -1` overflows `i32`"
    "fn f() i32 { value i32 = -2147483648\ndivisor i32 = -1\nreturn value / divisor }";
  semantic_accept "value-fact-min-rem-minus-one-is-defined"
    "fn f() i32 { value i32 = -2147483648\ndivisor i32 = -1\nreturn value % divisor }";
  semantic_accept "value-fact-min-div-minus-two"
    "fn f() i32 { value i32 = -2147483648\ndivisor i32 = -2\nreturn value / divisor }"

let () =
  semantic_message "value-fact-proven-index-out-of-range"
    "array index is out of bounds for length 5"
    "fn f() i32 { values arr[5,i32] = {1,2,3,4,5}\nk i32 = 5\nreturn values[k] }";
  semantic_message "value-fact-loop-condition-proven-index"
    "array index is out of bounds for length 5"
    "fn f() i32 { values arr[5,i32] = {1,2,3,4,5}\n\
     k i32 = 5\n\
     while values[k] != 0 { k += 1 }\n\
     return k }";
  semantic_accept "value-fact-loop-condition-advancing-index"
    "fn f() i32 { values arr[5,i32] = {1,2,3,4,5}\n\
     k i32 = 0\n\
     while values[k] != 3 { k += 1 }\n\
     return k }";
  semantic_accept "value-fact-view-range-index-accepted"
    "struct P { x i32, y i32, z i32 }\n\
     var POS arr[8, P]\n\
     fn pick() i32 { return 3 }\n\
     fn f() i32 { channel i32 = pick()\n\
     if channel < 0 { return 0 }\n\
     view p = POS[channel - 1]\n\
     p.x = 5\n\
     return p.x }";
  semantic_accept "value-fact-view-joined-index-accepted"
    "struct P { x i32, y i32, z i32 }\n\
     var POS arr[8, P]\n\
     fn pick() i32 { return 3 }\n\
     fn f() i32 { channel i32 = pick()\n\
     if channel == 0 { channel = 1 }\n\
     view p = POS[channel - 1]\n\
     p.x = 5\n\
     return p.x }";
  semantic_accept "value-fact-raw-range-index-accepted"
    "struct P { x i32, y i32, z i32 }\n\
     var POS arr[8, P]\n\
     fn pick() i32 { return 3 }\n\
     fn f() i32 { channel i32 = pick()\n\
     if channel < 0 { return 0 }\n\
     p addr = &POS\n\
     p[i32, channel - 1] = 5\n\
     return 0 }";
  semantic_accept "value-fact-addr-offset-range-accepted"
    "struct P { x i32, y i32, z i32 }\n\
     var POS arr[8, P]\n\
     fn pick() i32 { return 3 }\n\
     fn f() i32 { channel i32 = pick()\n\
     if channel < 0 { return 0 }\n\
     q addr = &POS + (channel - 1)\n\
     q[i32] = 5\n\
     return 0 }";
  semantic_message "value-fact-view-exact-index-past-end"
    "array index is out of bounds for length 8"
    "struct P { x i32, y i32, z i32 }\n\
     var POS arr[8, P]\n\
     fn f() i32 { k i32 = 9\n\
     view p = POS[k]\n\
     p.x = 5\n\
     return p.x }";
  semantic_message "value-fact-raw-exact-offset-past-end"
    "access outside object `x` (offset 16, size 4 bytes, object size 4)"
    "fn f() i32 { x u32 = 1\nk i32 = 0\np addr = &x\np[u32, k + 4] = 2\nreturn 0 }";
  semantic_message "value-fact-addr-exact-offset-past-end"
    "access outside object `x` (offset 4, size 4 bytes, object size 4)"
    "fn f() i32 { x u32 = 1\nk i32 = 4\np addr = &x + k\np[u32] = 2\nreturn 0 }"

let () =
  semantic_accept "value-fact-init-return-before-divide"
    "fn main() i32 { divisor i32 = 7\n\
     if divisor == 7 { divisor = 0 }\n\
     if divisor == 0 { return 0 }\n\
     quotient i32 = 91 / divisor\n\
     return if quotient == 13 { 0 } else { 1 } }";
  semantic_accept "value-fact-init-for-reaches-initializer"
    "fn main() i32 { x u32\n\
     for i usize = 0; i < 2; i += 1 { if i == 1 { x = 3 } }\n\
     value u32 = x\n\
     return 0 }";
  semantic_accept "value-fact-init-for-unconditional-initializer"
    "fn main() i32 { x u32\n\
     for i usize = 0; i < 1; i += 1 { x = 3 }\n\
     return bitcast[i32](x) - 3 }";
  semantic_accept "value-fact-init-for-initializes-both-elements"
    "fn main() i32 { xs arr[2,u32]\n\
     for i usize = 0; i < 2; i += 1 { xs[i] = 8 }\n\
     value u32 = xs[1]\n\
     return 0 }";
  semantic_accept "value-fact-init-for-fill-last-element"
    "fn main() i32 { xs arr[2,u32]\n\
     for i usize = 0; i < 1; i += 1 { xs[i] = 8 }\n\
     xs[1] = 9\n\
     return bitcast[i32](xs[1]) - 9 }";
  semantic_accept "value-fact-init-while-all-lanes"
    "fn main() i32 { values vec[128,u16]\n\
     lane usize = 0\n\
     while lane < 128 { values[lane] = 0\n\
     lane += 1 }\n\
     (&values)[u16, 0] = 13\n\
     return if reduce_sum(values) == 13 { 0 } else { 1 } }";
  semantic_accept "value-fact-init-while-fallback-lanes"
    "fn main() i32 { values arr[4,u32] = {1, 2, 3, 4}\n\
     fallback vec[4,u32]\n\
     lane usize = 0\n\
     while lane < 4 { fallback[lane] = 0\n\
     lane += 1 }\n\
     mask vec[4,bool] = {true, true, true, true}\n\
     loaded vec[4,u32] = masked_load[u32](&values, mask, fallback)\n\
     return if loaded[0] == 1 && loaded[3] == 4 { 0 } else { 1 } }"

let () =
  semantic_accept "value-fact-not-equal-if-edge"
    "fn f() i32 { divisor i32 = 0\n\
     value i32 = 7\n\
     if divisor != 0 { value = value / divisor }\n\
     return value }";
  semantic_accept "value-fact-not-equal-reversed-edge"
    "fn f() i32 { divisor i32 = 0\n\
     value i32 = 7\n\
     if 0 != divisor { value = value / divisor }\n\
     return value }";
  semantic_accept "value-fact-not-equal-and-edges"
    "fn t() bool { return true }\n\
     fn f() i32 { divisor i32 = 0\n\
     value i32 = 7\n\
     if t() && divisor != 0 { value = value / divisor }\n\
     if divisor != 0 && t() { value = value / divisor }\n\
     return value }";
  semantic_accept "value-fact-not-equal-or-edge"
    "fn f() i32 { divisor i32 = 0\n\
     value i32 = 7\n\
     if divisor != 0 || divisor != 0 { value = value / divisor }\n\
     return value }";
  semantic_accept "value-fact-not-equal-ternary-edge"
    "fn f() i32 { divisor i32 = 0\n\
     value i32 = if divisor != 0 { 7 / divisor } else { 7 }\n\
     return value }";
  semantic_accept "value-fact-not-equal-while-edge"
    "fn f() i32 { divisor i32 = 0\n\
     value i32 = 7\n\
     while divisor != 0 { value = value / divisor }\n\
     return value }";
  semantic_accept "value-fact-not-equal-for-edge"
    "fn f() i32 { divisor i32 = 0\n\
     value i32 = 7\n\
     for i usize = 0; divisor != 0; i += 1 { value = value / divisor }\n\
     return value }";
  semantic_accept "value-fact-comparison-infeasible-edges"
    "fn f() i32 { value i32 = 7\n\
     zero i32 = 0\n\
     if zero < 0 { value = value / zero }\n\
     if zero <= -1 { value = value / zero }\n\
     if zero > 0 { value = value / zero }\n\
     if zero >= 1 { value = value / zero }\n\
     if zero == 1 { value = value / zero }\n\
     if zero != 0 { value = value / zero }\n\
     return value }";
  semantic_message "value-fact-not-equal-true-edge"
    "division by zero is not a defined runtime operation"
    "fn f() i32 { zero i32 = 0\n\
     value i32 = 7\n\
     if zero != 1 { value = value / zero }\n\
     return value }";
  semantic_accept "value-fact-loop-written-bound"
    "fn f() i32 { values arr[4,i32] = {}\n\
     bound usize = 8\n\
     for index usize = 0; index < bound; index += 1 {\n\
     bound = 4\n\
     values[index] = 1\n\
     }\n\
     return values[3] - 1 }";
  semantic_accept "value-fact-loop-written-inclusive-bound"
    "fn f() i32 { values arr[4,i32] = {}\n\
     bound usize = 8\n\
     for index usize = 0; index <= bound; index += 1 {\n\
     values[index] = 1\n\
     bound = 2\n\
     }\n\
     return values[2] - 1 }";
  semantic_accept "value-fact-loop-addressed-bound"
    "fn shrink(pointer addr) void { pointer[usize] = 4 }\n\
     fn f() i32 { values arr[4,i32] = {}\n\
     bound usize = 8\n\
     for index usize = 0; index < bound; index += 1 {\n\
     shrink(&bound)\n\
     values[index] = 1\n\
     }\n\
     return values[3] - 1 }";
  semantic_accept "value-fact-loop-written-step"
    "fn f() i32 { values arr[4,i32] = {}\n\
     step usize = 2\n\
     for index usize = 0; index <= 4; index += step {\n\
     step = 3\n\
     values[index] = 1\n\
     }\n\
     return values[3] - 1 }";
  semantic_accept "value-fact-stride-u32-up-lt6"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i u32 = 0; i < 10; i += 2 {\n\
     if i < 6 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     if sum == 9 { return 0 }\n\
     return 1 }";
  semantic_accept "value-fact-stride-u32-up-le5"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i u32 = 0; i < 10; i += 2 {\n\
     if i <= 5 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     if sum == 9 { return 0 }\n\
     return 1 }";
  semantic_accept "value-fact-stride-i32-up-lt6"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i i32 = 0; i < 10; i += 2 {\n\
     if i < 6 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     if sum == 9 { return 0 }\n\
     return 1 }";
  semantic_accept "value-fact-stride-i32-up-le5"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i i32 = 0; i < 10; i += 2 {\n\
     if i <= 5 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     if sum == 9 { return 0 }\n\
     return 1 }";
  semantic_accept "value-fact-stride-u32-down-lt6"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i u32 = 8; i > 0; i -= 2 {\n\
     if i < 6 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     if sum == 8 { return 0 }\n\
     return 1 }";
  semantic_accept "value-fact-stride-i32-down-lt6"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i i32 = 8; i > 0; i -= 2 {\n\
     if i < 6 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     if sum == 8 { return 0 }\n\
     return 1 }";
  semantic_accept "value-fact-stride-i32-low-snap"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i i32 = 0; i < 10; i += 2 {\n\
     if i > -1 {\n\
     if i < 6 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     }\n\
     if sum == 9 { return 0 }\n\
     return 1 }";
  semantic_message "value-fact-stride-u32-up-lt7"
    "array index is out of bounds for length 5"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i u32 = 0; i < 10; i += 2 {\n\
     if i < 7 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     return 1 }";
  semantic_message "value-fact-stride-u32-up-le6"
    "array index is out of bounds for length 5"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i u32 = 0; i < 10; i += 2 {\n\
     if i <= 6 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     return 1 }";
  semantic_message "value-fact-stride-i32-up-lt7"
    "array index is out of bounds for length 5"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i i32 = 0; i < 10; i += 2 {\n\
     if i < 7 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     return 1 }";
  semantic_message "value-fact-stride-u32-step1-lt6"
    "array index is out of bounds for length 5"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i u32 = 0; i < 6; i += 1 {\n\
     sum = sum + t[i]\n\
     }\n\
     return 1 }";
  semantic_message "value-fact-stride-u32-step1-cond-lt6"
    "array index is out of bounds for length 5"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i u32 = 0; i < 10; i += 1 {\n\
     if i < 6 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     return 1 }";
  semantic_message "value-fact-stride-u32-down-odd-lt6"
    "array index is out of bounds for length 5"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i u32 = 9; i > 1; i -= 2 {\n\
     if i < 6 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     return 1 }";
  semantic_message "value-fact-stride-i32-down-odd-lt6"
    "array index is out of bounds for length 5"
    "fn f() i32 { t arr[5,u32] = {1, 2, 3, 4, 5}\n\
     sum u32 = 0\n\
     for i i32 = 9; i > 1; i -= 2 {\n\
     if i < 6 {\n\
     sum = sum + t[i]\n\
     }\n\
     }\n\
     return 1 }"

let () =
  semantic_message "null-raw-write" "access through null address"
    "fn f() void { p addr = null\np[u32] = 1\nreturn }";
  semantic_message "null-offset-raw-read" "access through null address"
    "fn f() u32 { p addr = null\nq addr = p + 8\nreturn q[u32] }";
  semantic_message "null-masked-load" "access through null address"
    "fn f() vec[2,u32] {\n\
     mask vec[2,bool] = {true, true}\n\
     fallback vec[2,u32] = {0, 0}\n\
     return masked_load[u32](null, mask, fallback) }";
  semantic_accept "null-masked-load-empty-mask"
    "fn f() vec[2,u32] {\n\
     mask vec[2,bool] = {false, false}\n\
     fallback vec[2,u32] = {7, 9}\n\
     return masked_load[u32](null, mask, fallback) }";
  semantic_accept "null-masked-load-dead-local-condition"
    "fn f() i32 { pointer addr = null\n\
     active bool = false\n\
     mask vec[2,bool] = {true, false}\n\
     fallback vec[2,u32] = {0, 0}\n\
     if active { loaded vec[2,u32] = masked_load[u32](pointer, mask, fallback) }\n\
     return 0 }";
  semantic_accept "null-unknown-from-bits"
    "fn f() void { p addr = addr_from_bits(0)\np[u32] = 1\nreturn }";
  semantic_accept "null-object-join-is-unknown"
    "fn f(flag bool) void {\n\
     x u32 = 1\n\
     p addr\n\
     if flag { p = null } else { p = &x }\n\
     p[u32] = 2\n\
     return }";
  semantic_message "object-raw-index-past-end"
    "access outside object `x` (offset 4, size 4 bytes, object size 4)"
    "fn f() void { x u32 = 1\np addr = &x\np[u32, 1] = 2\nreturn }";
  semantic_accept "object-raw-byte-last"
    "fn f() void { x u32 = 1\np addr = &x\np[u8, 3] = 2\nreturn }";
  semantic_accept "object-pointer-moves-out-and-back"
    "fn f() u32 { x u32 = 7\np addr = &x + 8\nq addr = p - 8\nreturn q[u32] }";
  semantic_accept "object-array-element-address"
    "fn f() void { x arr[2,u32] = {1, 2}\np addr = &x[1]\np[u32] = 3\nreturn }";
  semantic_accept "object-field-address"
    "struct Pair { left u32\n\
     right u32 }\n\
     fn f() void { x Pair\n\
     p addr = &x.right\n\
     p[u32] = 3\n\
     return }";
  semantic_accept "object-nested-array-field-address"
    "struct Pair { bytes arr[2,u8] }\n\
     fn f() void { x Pair\n\
     p addr = &x.bytes[1]\n\
     p[u8] = 3\n\
     return }";
  semantic_message "object-field-footprint-past-end"
    "access outside object `pair` (offset 8, size 4 bytes, object size 8)"
    "struct Pair { left u32\n\
     right u32 }\n\
     fn f() void { pair Pair\n\
     p addr = &pair.right\n\
     p[u32, 1] = 3\n\
     return }";
  semantic_message "object-footprint-straddles-end"
    "access outside object `x` (offset 6, size 4 bytes, object size 8)"
    "fn f() void { x arr[2,u32] = {1, 2}\np addr = &x[1] + 2\np[u32] = 3\nreturn }";
  semantic_message "constant-raw-write" "write to constant storage `TABLE`"
    "const TABLE arr[1,u32] = {4}\nfn f() void { p addr = &TABLE\np[u32] = 5\nreturn }";
  semantic_accept "constant-raw-read"
    "const TABLE arr[1,u32] = {4}\nfn f() u32 { p addr = &TABLE\nreturn p[u32] }";
  semantic_message "constant-volatile-write" "write to constant storage `TABLE`"
    "const TABLE arr[1,u32] = {4}\n\
     fn f() void { volatile_store[u32](&TABLE, 5)\n\
     return }";
  semantic_message "constant-scatter-write" "write to constant storage `TABLE`"
    "const TABLE arr[2,u32] = {4, 6}\n\
     fn f() void { indices vec[2,i32] = {0, 1}\n\
     mask vec[2,bool] = {true, false}\n\
     values vec[2,u32] = {5, 7}\n\
     scatter[u32](&TABLE, indices, mask, values)\n\
     return }";
  semantic_accept "constant-masked-store-empty-mask"
    "const TABLE arr[1,u32] = {4}\n\
     fn f() void { mask vec[2,bool] = {false, false}\n\
     values vec[2,u32] = {5, 7}\n\
     masked_store[u32](&TABLE, mask, values)\n\
     return }";
  semantic_accept "constant-address-join-unknown-write"
    "const TABLE arr[1,u32] = {4}\n\
     fn f(flag bool) void { mutable u32 = 1\n\
     p addr\n\
     if flag { p = &TABLE } else { p = &mutable }\n\
     p[u32] = 5\n\
     return }";
  semantic_message "lifetime-return-local" "returns address of local `value`"
    "fn f() addr { value u32 = 1\nreturn &value }";
  semantic_message "lifetime-return-parameter" "returns address of local `value`"
    "fn f(value u32) addr { return &value }";
  semantic_accept "lifetime-global-store"
    "var G addr\nfn f() void { value u32 = 1\nG = &value\nreturn }";
  semantic_accept "lifetime-global-use-replace"
    "var saved addr\n\
     var stable u32 = 9\n\
     fn f() u32 { x u32 = 7\n\
     saved = &x\n\
     value u32 = saved[u32]\n\
     saved = &stable\n\
     return value }";
  semantic_accept "lifetime-global-record-use-replace"
    "struct Saved { value addr }\n\
     var stable u32 = 9\n\
     var saved Saved = {null}\n\
     fn f() u32 { x u32 = 7\n\
     saved.value = &x\n\
     value u32 = saved.value[u32]\n\
     saved.value = &stable\n\
     return value }";
  semantic_accept "lifetime-global-inner-block-use-replace"
    "var saved addr\n\
     fn f() u32 { stable u32 = 9\n\
     { x u32 = 7\n\
     saved = &x\n\
     value u32 = saved[u32]\n\
     saved = &stable }\n\
     return stable }";
  semantic_message "lifetime-access-after-block"
    "access to local `value` after its block ended"
    "fn f() void { p addr\n{ value u32 = 1\np = &value }\np[u32] = 2\nreturn }";
  semantic_accept "lifetime-hold-and-compare-after-block"
    "fn f() bool { p addr\n\
     { value u32 = 1\n\
     p = &value }\n\
     other addr = null\n\
     return p != other }";
  semantic_accept "lifetime-outer-local-used-in-inner-block"
    "fn f() u32 { value u32 = 1\np addr = &value\n{ p[u32] = 2 }\nreturn value }";
  semantic_accept "sanitize-accept-oob-helper"
    "fn write(p addr, index i32) void { p[i32, index] = 19 }\n\
     fn main() i32 { values arr[2, i32] = {0, 0}\n\
     write(&values[0], 2)\n\
     return 0 }";
  semantic_accept "sanitize-accept-use-after-scope-helper"
    "fn hold(p addr) addr { return p }\n\
     fn read_value(p addr) i32 { return p[i32, 0] }\n\
     fn main() i32 { escaped addr = null\n\
     { value i32 = 5\n\
     escaped = hold(&value) }\n\
     return read_value(escaped) - 5 }";
  semantic_accept "sanitize-accept-stack-return-helper"
    "fn hold(p addr) addr { return p }\n\
     fn read_value(p addr) i32 { return p[i32, 0] }\n\
     fn make() addr { value i32 = 5\n\
     return hold(&value) }\n\
     fn main() i32 { return read_value(make()) - 5 }";
  semantic_accept "sanitize-accept-heap-overrun-helper"
    "extern \"C\" { fn alloc_bytes(size usize) addr }\n\
     fn write(p addr, index usize) void { p[u8, index] = 68 }\n\
     fn main() i32 { p addr = alloc_bytes(1)\n\
     write(p, 1)\n\
     return 0 }";
  semantic_accept "sanitize-accept-c-undefined-overflow"
    "extern \"C\" { fn fas_signed_overflow() i32 }\n\
     fn main() i32 { fas_signed_overflow()\n\
     return 0 }";
  semantic_accept "sanitize-accept-wrapping-arithmetic"
    "fn main() i32 { value i32 = 2147483647\n\
     value += 1\n\
     if value < 0 { return 0 }\n\
     return 1 }"

let () =
  let pin name text line prefix width message =
    syntax_pin name text line (String.length prefix + 1) width message
  in
  let type_order = "fn f() void { u16 samples[3]\n return }\n" in
  pin "c-array-type-order" type_order 1 "fn f() void { u16 " 7
    "C array declaration `u16 samples[3]` is not Fas syntax; write `samples arr[3,u16]`";
  let c_array = "fn f() void { int items[4]\n return }\n" in
  pin "c-array-declaration" c_array 1 "fn f() void { " 3
    "C array declaration `int items[4]` is not Fas syntax; write `items arr[4,i32]`";
  let char_array = "fn f() void { char buf[16]\n return }\n" in
  pin "c-char-array-declaration" char_array 1 "fn f() void { " 4
    "C array declaration `char buf[16]` is not Fas syntax; write `buf arr[16,i8]`";
  let missing_element = "fn f() void { items arr[3] }\n" in
  pin "array-missing-element-type" missing_element 1 "fn f() void { items arr[3" 1
    "array type `arr` needs an element type after its length";
  let sizeof_type = "fn f() usize { return sizeof i32 }\n" in
  semantic_pin "sizeof-type-operand-help" sizeof_type 1
    (String.length "fn f() usize { return " + 1)
    6 "`sizeof` needs a type in brackets" (Some "write `sizeof[i32]`");
  semantic_accept "sizeof-type-operand-twin" "fn f() usize { return sizeof[i32] }\n";
  parse_message "sizeof-needs-type" "`sizeof` needs a type in brackets"
    "fn f() usize { return sizeof }\n";
  let sizeof_name = "fn f() usize { a arr[4,u8]\nreturn sizeof a }\n" in
  semantic_pin "sizeof-variable-type-help" sizeof_name 2 8 6
    "`sizeof` needs a type in brackets" (Some "write `sizeof[arr[4, u8]]`");
  semantic_accept "sizeof-variable-type-twin"
    "fn f() usize { a arr[4,u8]\nreturn sizeof[arr[4,u8]] }\n";
  let sizeof_parenthesized =
    "fn f() usize { amount i16 = 0\nreturn sizeof(amount) }\n"
  in
  semantic_pin "sizeof-parenthesized-type-help" sizeof_parenthesized 2 8 6
    "`sizeof` needs a type in brackets" (Some "write `sizeof[i16]`");
  semantic_accept "sizeof-parenthesized-type-twin"
    "fn f() usize { amount i16 = 0\nreturn sizeof[i16] }\n";
  pin "c-header-needs-delimiters" "use \"C\" math.h\n" 1 "use \"C\" " 4
    "C header path `math.h` needs quotes or angle brackets";
  syntax_pin "include-directive" "#include <stdint.h>\n" 1 1 1
    "C `#include` is not Fas syntax; use `use`";
  syntax_pin "c-function-prototype" "u16 sum(u16 left, u16 right);\n" 1 1 3
    "C `u16` function prototypes are not Fas syntax; use `fn` declarations";
  syntax_pin "c-extern-function" "extern int write(char* data);\n" 1 8 3
    "C extern function prototypes starting with `int` are not Fas syntax; use `extern \
     \"C\"` and `fn`";
  syntax_pin "c-extern-global" "extern int count;\n" 1 8 3
    "C extern global declarations starting with `int` are not Fas syntax; use `extern \
     \"C\"` and `var`";
  let char_pointer = "fn accept(data char*) void { return }\n" in
  syntax_pin "c-char-pointer-type-caret" char_pointer 1
    (String.length "fn accept(data " + 1)
    5 "C type `char*` is not a Fas type; use `addr`";
  pin "vector-addr-element-caret" "fn f(v vec[2,addr]) usize { return 0 }\n" 1
    "fn f(v vec[2," 4 "vector element type must be `bool` or an integer type";
  pin "c-char-pointer-type" char_pointer 1 "fn accept(data " 5
    "C type `char*` is not a Fas type; use `addr`";
  let typed_pointer = "typed pointers are not Fas types; use `addr`" in
  pin "typed-pointer-param-u32-star" "fn accept(p u32 *) u32 { return 0 }\n" 1
    "fn accept(p " 5 typed_pointer;
  pin "typed-pointer-param-point-star"
    "struct Point { x i32 }\nfn accept(p Point *) u32 { return 0 }\n" 2 "fn accept(p " 7
    typed_pointer;
  pin "typed-pointer-param-u8-double-star" "fn accept(p u8 **) u32 { return 0 }\n" 1
    "fn accept(p " 5 typed_pointer;
  pin "typed-pointer-local-u32-star" "fn f() u32 {\n    x u32 * = 0\n    return 0\n}\n"
    2 "    x " 5 typed_pointer;
  pin "typed-pointer-local-point-star"
    "struct Point { x i32 }\nfn f() u32 {\n    x Point * = 0\n    return 0\n}\n" 3
    "    x " 7 typed_pointer;
  pin "typed-pointer-local-u8-double-star"
    "fn f() u32 {\n    x u8 ** = 0\n    return 0\n}\n" 2 "    x " 5 typed_pointer;
  pin "typed-pointer-field-u32-star" "struct S {\n    p u32 *\n}\n" 2 "    p " 5
    typed_pointer;
  pin "typed-pointer-field-point-star"
    "struct Point { x i32 }\nstruct S {\n    p Point *\n}\n" 3 "    p " 7 typed_pointer;
  pin "typed-pointer-field-u8-double-star" "struct S {\n    p u8 **\n}\n" 2 "    p " 5
    typed_pointer;
  pin "typed-pointer-param-addr-u32" "fn accept(p addr[u32]) u32 { return 0 }\n" 1
    "fn accept(p " 9 typed_pointer;
  pin "typed-pointer-param-addr-point"
    "struct Point { x i32 }\nfn accept(p addr[Point]) u32 { return 0 }\n" 2
    "fn accept(p " 11 typed_pointer;
  pin "typed-pointer-local-addr-u32"
    "fn f() u32 {\n    x addr[u32] = null\n    return 0\n}\n" 2 "    x " 9 typed_pointer;
  pin "typed-pointer-local-addr-point"
    "struct Point { x i32 }\nfn f() u32 {\n    x addr[Point] = null\n    return 0\n}\n"
    3 "    x " 11 typed_pointer;
  pin "typed-pointer-field-addr-u32" "struct S {\n    p addr[u32]\n}\n" 2 "    p " 9
    typed_pointer;
  pin "typed-pointer-field-addr-point"
    "struct Point { x i32 }\nstruct S {\n    p addr[Point]\n}\n" 3 "    p " 11
    typed_pointer;
  pin "typed-pointer-generic-addr-u32"
    "struct Box[T] { v T }\nfn accept(b Box[addr[u32]]) u32 { return 0 }\n" 2
    "fn accept(b Box[" 9 typed_pointer;
  pin "typed-pointer-return-u32-star" "fn f() u32 * {\n    return 0\n}\n" 1 "fn f() " 5
    typed_pointer;
  semantic_accept "typed-pointer-mul-decl-twin"
    "fn f(a u32, b u32) u32 {\n    x u32 = a * b\n    return x\n}\n";
  semantic_accept "typed-pointer-mul-no-space-twin"
    "fn f(a u32, b u32) u32 {\n    y u32 = a *b\n    return y\n}\n";
  semantic_accept "typed-pointer-index-mul-twin"
    "fn f(p addr) u32 {\n    n u32 = p[u32] * 2\n    return n\n}\n";
  semantic_accept "typed-pointer-handle-twin"
    "opaque FILE\nfn f(h handle[FILE]) u32 { return 0 }\n";
  semantic_accept "typed-pointer-addr-twin" "fn f(p addr) u32 { return 0 }\n";
  let arrow_record =
    "struct Pair { x i32 }\nfn read() i32 { value Pair = {1}\n return value->x }\n"
  in
  semantic_pin "c-arrow-record-field-name" arrow_record 3
    (String.length " return value" + 1)
    2 "Fas has no `->`; access field `x` with `.`" None;
  semantic_accept "c-arrow-record-dot-twin"
    "struct Pair { x i32 }\nfn read() i32 { value Pair = {1}\n return value.x }\n";
  let parameter_order = "fn combine(i64 first, i64 second) i64 { return first }\n" in
  pin "c-parameter-order" parameter_order 1 "fn combine(" 3
    "C parameter order puts `i64` before the name; Fas parameters put the name first";
  let missing_result = "fn empty() { return 0 }\n" in
  pin "function-result-type-required" missing_result 1 "fn empty() " 1
    "function result type is required before the body";
  let return_type_first = "fn u32 read() { return 0 }\n" in
  pin "function-return-type-first" return_type_first 1 "fn u32 " 4
    "Fas function result types follow the parameters; `fn u32 name` puts the type first";
  let void_parameter = "fn vacant(void) void { return }\n" in
  pin "c-void-parameter" void_parameter 1 "fn vacant(" 4
    "C `void` parameter spelling is not Fas syntax; use an empty parameter list";
  syntax_pin "c-static-declaration" "static i32 counter = 1\n" 1 1 6
    "C `static` is not a Fas keyword; use `fn` for a function or `var` for a global";
  syntax_pin "c-const-global" "const int TOTAL = 1\n" 1 7 3
    "C `const int` globals are not Fas syntax; Fas puts the name before its type";
  syntax_pin "c-const-local" "fn f() void { const int value = 1\n return }\n" 1 15 5
    "C `const int` locals are not Fas syntax; Fas locals put the name before the type";
  pin "global-type-required" "var LIMIT = 1\n" 1 "var LIMIT " 1
    "global `var` declarations need an explicit type before `=`";
  let record_variable = "fn f() void { struct Pair point\n return }\n" in
  pin "c-record-variable" record_variable 1 "fn f() void { " 6
    "C `struct Pair point` declarations are not Fas syntax; write `point Pair`";
  syntax_pin "c-struct-fields" "struct Point { int x; }\n" 1 1 6
    "C struct field declarations are not Fas syntax; fields put the name before the \
     type";
  pin "c-for-loop" "fn f() i32 { for (;;) { return 1 } return 0 }\n" 1
    "fn f() i32 { for (" 1
    "C `for (;;)` syntax has parentheses; Fas writes `for ; ;` with an optional \
     condition";
  pin "c-do-while" "fn f() void { do { break } while (true)\n return }\n" 1
    "fn f() void { do " 1 "C `do`/`while` loops are not Fas syntax; use `while`";
  pin "c-goto" "fn f() void { goto finish\n return }\n" 1 "fn f() void { " 4
    "Fas has no `goto` labels; use `break` or `continue`, optionally with a loop label";
  let arrow = "fn read(p addr) i32 { return p->value }\n" in
  semantic_pin "c-arrow-field" arrow 1
    (String.index arrow '-' + 1)
    2 "Fas has no `->` operator" None;
  semantic_accept "c-arrow-field-typed-twin"
    "struct Point { value i32 }\nfn read(p addr) i32 { return p[Point].value }\n";
  let cast = "fn widen(p addr) u8 { return (u8*)p }\n" in
  semantic_pin "c-cast-value" cast 1
    (String.length "fn widen(p addr) u8 { return " + 1)
    5 "C pointer cast `(u8*)` is not Fas syntax; `addr` is untyped"
    (Some "write `p[u8]`");
  semantic_accept "c-cast-value-twin" "fn widen(p addr) u8 { return p[u8] }\n";
  let cast_address = "fn keep(p addr) addr { q addr = (u8*)p\nreturn q }\n" in
  semantic_pin "c-cast-address" cast_address 1
    (String.length "fn keep(p addr) addr { q addr = " + 1)
    5 "C pointer cast `(u8*)` is not Fas syntax; `addr` is untyped" (Some "write `p`");
  semantic_accept "c-cast-address-twin"
    "fn keep(p addr) addr { q addr = p\nreturn q }\n";
  let addr_cast = "fn keep(p addr) addr { return (addr)p }\n" in
  syntax_pin "c-addr-cast" addr_cast 1
    (String.length "fn keep(p addr) addr { return " + 1)
    6 "C cast `(addr)` is not Fas syntax";
  let integer_cast = "fn reinterpret(value i32) u32 { return (u32)value }\n" in
  pin "c-integer-cast" integer_cast 1 "fn reinterpret(value i32) u32 { return " 1
    "C cast `(u32)` is not Fas syntax; use `zext`, `sext`, `trunc` or `bitcast`";
  pin "c-fixed-width-cast-int8"
    "fn reinterpret(value i32) i8 { return (int8_t)value }\n" 1
    "fn reinterpret(value i32) i8 { return " 1
    "C cast `(int8_t)` is not Fas syntax; use `zext`, `sext`, `trunc` or `bitcast`";
  pin "c-fixed-width-cast-size"
    "fn reinterpret(value i32) usize { return (size_t)value }\n" 1
    "fn reinterpret(value i32) usize { return " 1
    "C cast `(size_t)` is not Fas syntax; use `zext`, `sext`, `trunc` or `bitcast`";
  let dereference = "fn fetch(address addr) u8 { return *address }\n" in
  semantic_pin "c-star-dereference" dereference 1
    (String.index dereference '*' + 1)
    1 "Fas has no unary `*`; read through an `addr` with `p[T]`"
    (Some "write `address[u8]`");
  semantic_accept "c-star-dereference-typed-twin"
    "fn fetch(address addr) u8 { return address[u8] }\n";
  let dereference_assignment =
    "fn store(address addr) void { *address = 1\n return }\n"
  in
  syntax_pin "c-star-dereference-assignment" dereference_assignment 1
    (String.index dereference_assignment '*' + 1)
    1 "Fas has no unary `*`; read through an `addr` with `p[T]`";
  let typed_dereference = "fn fetch(address addr) i32 { return *(i32*)address }\n" in
  semantic_pin "c-typed-star-dereference" typed_dereference 1
    (String.index typed_dereference '*' + 1)
    1 "Fas has no unary `*`; read through an `addr` with `p[T]`"
    (Some "write `address[i32]`");
  semantic_accept "c-typed-star-dereference-twin"
    "fn fetch(address addr) i32 { return address[i32] }\n";
  let dot_star_read = "fn fetch(address addr) i32 { return address.* }\n" in
  semantic_pin "c-dot-star-dereference" dot_star_read 1
    (String.index dot_star_read '.' + 1)
    2 "Fas has no `.*`; read through an `addr` with `p[T]`"
    (Some "write `address[i32]`");
  semantic_accept "c-dot-star-dereference-twin"
    "fn fetch(address addr) i32 { return address[i32] }\n";
  let dot_star_assignment =
    "fn store(address addr) void { address.* = 1\n return }\n"
  in
  syntax_pin "c-dot-star-dereference-assignment" dot_star_assignment 1
    (String.index dot_star_assignment '.' + 1)
    2 "Fas has no `.*`; read through an `addr` with `p[T]`";
  let constant_dereference = "var address addr = null\nconst VALUE u8 = *address\n" in
  semantic_pin "constant-c-star-dereference" constant_dereference 2
    (String.index "const VALUE u8 = *address" '*' + 1)
    1 "Fas has no unary `*`; read through an `addr` with `p[T]`" None;
  let prefix_increment = "fn increment(value i32) i32 { return ++value }\n" in
  pin "c-prefix-increment" prefix_increment 1 "fn increment(value i32) i32 { return " 2
    "Fas has no prefix `++` operator";
  let prefix_decrement = "fn decrement(value i32) i32 { return --value }\n" in
  pin "c-prefix-decrement" prefix_decrement 1 "fn decrement(value i32) i32 { return " 2
    "Fas has no prefix `--` operator";
  let for_postfix = "fn f() void { i i32 = 0\nfor ; ; i++ { break }\nreturn }\n" in
  syntax_pin "for-step-postfix-increment" for_postfix 2
    (String.length "for ; ; i" + 1)
    2 "Fas has no postfix `++` operator; write `i += 1`";
  semantic_accept "for-step-postfix-increment-twin"
    "fn f() void { i i32 = 0\nfor ; ; i += 1 { break }\nreturn }\n";
  let for_postfix_decrement =
    "fn f() void { i i32 = 0\nfor ; ; i-- { break }\nreturn }\n"
  in
  syntax_pin "for-step-postfix-decrement" for_postfix_decrement 2
    (String.length "for ; ; i" + 1)
    2 "Fas has no postfix `--` operator; write `i -= 1`";
  semantic_accept "for-step-postfix-decrement-twin"
    "fn f() void { i i32 = 0\nfor ; ; i -= 1 { break }\nreturn }\n";
  let postfix_expression = "fn increment(value i32) i32 { return value++ }\n" in
  pin "c-postfix-increment-expression" postfix_expression 1
    "fn increment(value i32) i32 { return value" 2 "Fas has no postfix `++` operator";
  let postfix_decrement_expression =
    "fn decrement(value i32) i32 { return value-- }\n"
  in
  pin "c-postfix-decrement-expression" postfix_decrement_expression 1
    "fn decrement(value i32) i32 { return value" 2 "Fas has no postfix `--` operator";
  let prefix_statement = "fn increment(value i32) void { ++value\n return }\n" in
  pin "c-prefix-increment-statement" prefix_statement 1
    "fn increment(value i32) void { " 2
    "Fas has no prefix `++` operator; write `value += 1`";
  let prefix_decrement_statement =
    "fn decrement(value i32) void { --value\n return }\n"
  in
  pin "c-prefix-decrement-statement" prefix_decrement_statement 1
    "fn decrement(value i32) void { " 2
    "Fas has no prefix `--` operator; write `value -= 1`";
  semantic_accept "increment-statement-help-twin"
    "fn increment(value i32) void { value += 1\nreturn }";
  semantic_accept "decrement-statement-help-twin"
    "fn decrement(value i32) void { value -= 1\nreturn }";
  let postfix_increment = "fn increment(value i32) void { value++\n return }\n" in
  pin "c-postfix-increment" postfix_increment 1 "fn increment(value i32) void { value" 2
    "Fas has no postfix `++` operator; write `value += 1`";
  let chain =
    "fn f() void { left i32 = 0\n right i32 = 1\n left = right = 2\n return }\n"
  in
  pin "c-chained-assignment" chain 3 " left = right " 1
    "Fas assignments are statements, not chained assignment expressions";
  let comma = "fn f(left i32, right i32) i32 { return (left, right) }\n" in
  pin "c-comma-operator" comma 1 "fn f(left i32, right i32) i32 { return (left" 1
    "Fas has no comma operator; put each expression in its own statement";
  pin "unsigned-integer-suffix" "fn f() u32 { return 10u }\n" 1 "fn f() u32 { return " 1
    "C integer suffix `10u` is not valid in Fas; write `10`";
  pin "long-integer-suffix" "fn f() i64 { return 10L }\n" 1 "fn f() i64 { return " 1
    "C integer suffix `10L` is not valid in Fas; write `10`";
  pin "binary-invalid-digit" "fn f() i32 { return 0b102 }\n" 1 "fn f() i32 { return " 1
    "invalid digit in binary integer literal \"0b102\"";
  ignore (llvm_of "fn samples() void { values arr[3,u16]\n return }\n");
  ignore (llvm_of "fn count_bytes() usize { return sizeof[i32] }\n");
  ignore (llvm_of "use \"C\" \"stdint.h\"\nfn f() void { return }\n");
  ignore (llvm_of "fn sum(left u16, right u16) u16 { return left + right }\n");
  ignore (llvm_of "extern \"C\" { fn puts(text addr) i32 }\nfn f() void { return }\n");
  ignore (llvm_of "extern \"C\" { var count i32 }\nfn f() void { return }\n");
  ignore (llvm_of "fn accept(data addr) void { return }\n");
  ignore (llvm_of "fn combine(first i64, second i64) i64 { return first + second }\n");
  ignore (llvm_of "fn empty() i32 { return 0 }\n");
  ignore (llvm_of "fn read() u32 { return 0 }\n");
  ignore (llvm_of "fn vacant() void { return }\n");
  ignore (llvm_of "var TOTAL i32 = 1\nfn f() void { value i32 = 1\n return }\n");
  ignore (llvm_of "const TOTAL i32 = 1\nfn f() void { value i32 = 1\n return }\n");
  ignore (llvm_of "fn counter() i32 { return 1 }\nvar count i32 = 1\n");
  ignore (llvm_of "struct Pair { value i32 }\nfn f() void { point Pair\n return }\n");
  ignore (llvm_of "fn f() i32 { for ; ; { return 1 } return 0 }\n");
  ignore (llvm_of "fn f() void { while true { break }\n return }\n");
  ignore (llvm_of "fn g() void { while true { continue }\n return }\n");
  ignore
    (llvm_of
       "struct Point { value i32 }\nfn read(p addr) i32 { return p[Point].value }\n");
  ignore (llvm_of "fn widen(value u8) u32 { return zext[u32](value) }\n");
  ignore (llvm_of "fn widen_signed(value i8) i32 { return sext[i32](value) }\n");
  ignore (llvm_of "fn narrow(value i32) u8 { return trunc[u8](value) }\n");
  ignore (llvm_of "fn increment(value i32) i32 { value += 1\n return value }\n");
  ignore
    (llvm_of "fn f() void { left i32 = 0\n right i32 = 1\n left = right\n return }\n");
  ignore (llvm_of "fn f() i32 { return 1 }\n");
  ignore (llvm_of "fn f() i32 { return 10 }\n");
  ignore (llvm_of "fn f() i32 { return 0b101 }\n");
  let address_to_integer =
    "fn bits(pointer addr) usize { return bitcast[usize](pointer) }\n"
  in
  semantic_pin "bitcast-address-to-integer" address_to_integer 1
    (String.length "fn bits(pointer addr) usize { return " + 1)
    7
    "illegal `bitcast` from `addr` to `usize`: address-to-integer conversion uses \
     `addr_bits`"
    (Some "write `addr_bits(pointer)`");
  semantic_accept "bitcast-address-to-integer-twin"
    "fn bits(pointer addr) usize { return addr_bits(pointer) }\n";
  let integer_to_address =
    "fn pointer(bits usize) addr { return bitcast[addr](bits) }\n"
  in
  semantic_pin "bitcast-integer-to-address" integer_to_address 1
    (String.length "fn pointer(bits usize) addr { return " + 1)
    7
    "illegal `bitcast` from `usize` to `addr`: integer-to-address conversion uses \
     `addr_from_bits`"
    (Some "write `addr_from_bits(bits)`");
  semantic_accept "bitcast-integer-to-address-twin"
    "fn pointer(bits usize) addr { return addr_from_bits(bits) }\n";
  let trunc_wider = "fn widen(value u8) u32 { return trunc[u32](value) }\n" in
  semantic_pin "trunc-wider-help" trunc_wider 1
    (String.length "fn widen(value u8) u32 { return " + 1)
    5
    "illegal `trunc` from `u8` to `u32`: the destination must be a narrower integer \
     type"
    (Some "write `zext[u32](value)`");
  let zext_narrower = "fn narrow(value u32) u8 { return zext[u8](value) }\n" in
  semantic_pin "zext-narrower-help" zext_narrower 1
    (String.length "fn narrow(value u32) u8 { return " + 1)
    4 "illegal `zext` from `u32` to `u8`: the destination must be a wider integer type"
    (Some "write `trunc[u8](value)`");
  let usize_zext_u64 = "fn widen(value usize) u64 { return zext[u64](value) }\n" in
  semantic_pin "zext-usize-u64-help" usize_zext_u64 1
    (String.length "fn widen(value usize) u64 { return " + 1)
    4
    "illegal `zext` from `usize` to `u64`: `usize` and `u64` have the same width and \
     convert implicitly; no conversion is needed"
    (Some "write `value`");
  semantic_accept "zext-usize-u64-applied"
    "fn widen(value usize) u64 { return value }\n";
  let same_width_pin name kind source destination =
    let prefix = Printf.sprintf "fn widen(value %s) %s { return " source destination in
    semantic_pin name
      (prefix ^ Printf.sprintf "%s[%s](value) }\n" kind destination)
      1
      (String.length prefix + 1)
      (String.length kind)
      (Printf.sprintf
         "illegal `%s` from `%s` to `%s`: `%s` and `%s` have the same width and \
          convert implicitly; no conversion is needed"
         kind source destination source destination)
      (Some "write `value`");
    semantic_accept (name ^ "-applied")
      (Printf.sprintf "fn widen(value %s) %s { return value }\n" source destination)
  in
  same_width_pin "trunc-u64-usize-help" "trunc" "u64" "usize";
  same_width_pin "zext-u64-usize-help" "zext" "u64" "usize";
  same_width_pin "sext-isize-i64-help" "sext" "isize" "i64";
  same_width_pin "trunc-i64-isize-help" "trunc" "i64" "isize";
  same_width_pin "sext-i64-isize-help" "sext" "i64" "isize";
  semantic_pin "zext-i64-usize-mixed-sign"
    "fn widen(value i64) usize { return zext[usize](value) }\n" 1
    (String.length "fn widen(value i64) usize { return " + 1)
    4
    "illegal `zext` from `i64` to `usize`: the source and destination have the same \
     width; use `bitcast`"
    (Some "write `bitcast[usize](value)`");
  semantic_accept "zext-i64-usize-mixed-sign-applied"
    "fn widen(value i64) usize { return bitcast[usize](value) }\n";
  semantic_pin "trunc-u64-isize-mixed-sign"
    "fn narrow(value u64) isize { return trunc[isize](value) }\n" 1
    (String.length "fn narrow(value u64) isize { return " + 1)
    5
    "illegal `trunc` from `u64` to `isize`: the source and destination have the same \
     width; use `bitcast`"
    (Some "write `bitcast[isize](value)`");
  semantic_accept "trunc-u64-isize-mixed-sign-applied"
    "fn narrow(value u64) isize { return bitcast[isize](value) }\n";
  semantic_pin "zext-i32-u32-mixed-sign"
    "fn widen(value i32) u32 { return zext[u32](value + 1) }\n" 1
    (String.length "fn widen(value i32) u32 { return " + 1)
    4
    "illegal `zext` from `i32` to `u32`: the source and destination have the same \
     width; use `bitcast`"
    (Some "write `bitcast[u32](value + 1)`");
  semantic_accept "zext-i32-u32-mixed-sign-applied"
    "fn widen(value i32) u32 { return bitcast[u32](value + 1) }\n";
  semantic_pin "sext-i16-u16-mixed-sign"
    "fn widen(value i16) u16 { return sext[u16](value) }\n" 1
    (String.length "fn widen(value i16) u16 { return " + 1)
    4
    "illegal `sext` from `i16` to `u16`: the source and destination have the same \
     width; use `bitcast`"
    (Some "write `bitcast[u16](value)`");
  semantic_accept "sext-i16-u16-mixed-sign-applied"
    "fn widen(value i16) u16 { return bitcast[u16](value) }\n";
  let same_width_expression =
    "fn widen(a usize, b usize) u64 { return zext[u64](a + b) }\n"
  in
  semantic_pin "zext-usize-u64-expression-help" same_width_expression 1
    (String.length "fn widen(a usize, b usize) u64 { return " + 1)
    4
    "illegal `zext` from `usize` to `u64`: `usize` and `u64` have the same width and \
     convert implicitly; no conversion is needed"
    (Some "write `a + b`");
  semantic_accept "zext-usize-u64-expression-applied"
    "fn widen(a usize, b usize) u64 { return a + b }\n";
  let zext_address = "fn bits(pointer addr) usize { return zext[usize](pointer) }\n" in
  semantic_pin "zext-address-help" zext_address 1
    (String.length "fn bits(pointer addr) usize { return " + 1)
    4
    "illegal `zext` from `addr` to `usize`: address-to-integer conversion uses \
     `addr_bits`"
    (Some "write `addr_bits(pointer)`");
  let trunc_address = "fn pointer(bits usize) addr { return trunc[addr](bits) }\n" in
  semantic_pin "trunc-address-help" trunc_address 1
    (String.length "fn pointer(bits usize) addr { return " + 1)
    5
    "illegal `trunc` from `usize` to `addr`: integer-to-address conversion uses \
     `addr_from_bits`"
    (Some "write `addr_from_bits(bits)`");
  let void_cast = "fn invalid(value i32) void { return zext[void](value) }\n" in
  semantic_pin "cast-void-target" void_cast 1
    (String.length "fn invalid(value i32) void { return " + 1)
    4
    "illegal `zext` from `i32` to `void`: the source and destination must be integer \
     types"
    None

let () =
  semantic_accept "generic-inference-plain-and-handle-type"
    "opaque O\n\
     fn pass[T](value T) T { return value }\n\
     fn keep[T](value handle[T]) handle[T] { return value }\n\
     fn run(value handle[O]) handle[O] { return keep(value) }\n\
     fn plain(value u8) u8 { return pass(value) }\n";
  semantic_accept "generic-inference-visible-scope-types"
    "fn id[T](value T) T { return value }\n\
     fn scopes() u32 { y u8 = 200\n\
     { y u32 = 1000 }\n\
     k u32 = 0\n\
     loop: while k < 1 { y u32 = 1000\n\
     k += 1\n\
     break loop }\n\
     outer: for y u32 = 0; y < 1; y += 1 { break outer }\n\
     defer { y u32 = 1000 }\n\
     switch 0 { case 0: { y u32 = 1000 } default: {} }\n\
     if true { y u32 = 1000 }\n\
     return zext[u32](id(y) << 4) >> 4 }\n\
     fn shadow() u32 { y u32 = 1000\n\
     { y u8 = 200\n\
     x u32 = zext[u32](id(y) << 4) >> 4 }\n\
     return id(y) }\n";
  semantic_accept "generic-view-inference-bindings"
    "fn id[T](value T) T { return value }\n\
     var global u32 = 1\n\
     fn nested[T](source T) T { outer u32 = 1\n\
     { view outer = source\n\
     return id(outer) } }\n\
     fn parameter(outer u32, source u8) u8 {\n\
     { view outer = source\n\
     return id(outer) } }\n\
     fn global_view(source u8) u8 {\n\
     { view global = source\n\
     return id(global) } }\n\
     fn reverse() u32 { outer u8 = 1\n\
     source u32 = 65541\n\
     { view outer = source\n\
     return id(outer) } }\n\
     fn for_view(source u8) u8 {\n\
     for view value = source; true; { return id(value) }\n\
     return 0 }\n\
     fn switch_view(source u8) u8 {\n\
     switch 0 { case 0: { view value = source\n\
     return id(value) } default: { return 0 } } }\n\
     fn const_parameter[N const usize]() usize { return id(N) }\n";
  semantic_message "generic-view-inference-wrap-rejected"
    "`id[u8](outer) + id[u8](outer)` is computed in `u8` and may wrap before it \
     reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     fn bad() u32 { outer u32 = 1\n\
     source u8 = 200\n\
     { view outer = source\n\
     result u32 = id(outer) + id(outer) }\n\
     return 0 }\n";
  semantic_message "generic-view-inference-wrap-explicit-twin"
    "`id[u8](outer) + id[u8](outer)` is computed in `u8` and may wrap before it \
     reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     fn bad() u32 { outer u32 = 1\n\
     source u8 = 200\n\
     { view outer = source\n\
     result u32 = id[u8](outer) + id[u8](outer) }\n\
     return 0 }\n";
  semantic_accept "generic-view-inference-call-result-local-shadow"
    "fn id[T](value T) T { return value }\n\
     fn at(pointer addr) addr { return pointer }\n\
     fn local() u32 { value u32 = 1\n\
     pointer addr = null\n\
     { view value = at(pointer)[u8, 1]\n\
     return zext[u32](id(value)) }\n\
     return 0 }\n";
  semantic_accept "generic-view-inference-call-result-parameter-shadow"
    "fn id[T](value T) T { return value }\n\
     fn at(pointer addr) addr { return pointer }\n\
     fn parameter(value u32, pointer addr) u32 {\n\
     { view value = at(pointer)[u8, 1]\n\
     return zext[u32](id(value)) } }\n";
  semantic_accept "generic-view-inference-call-result-global-shadow"
    "fn id[T](value T) T { return value }\n\
     fn at(pointer addr) addr { return pointer }\n\
     var value u32 = 1\n\
     fn global(pointer addr) u32 {\n\
     { view value = at(pointer)[u8, 1]\n\
     return zext[u32](id(value)) } }\n";
  semantic_accept "generic-view-inference-unknown-explicit"
    "fn id[T](value T) T { return value }\n\
     fn at(pointer addr) addr { return pointer }\n\
     fn explicit(pointer addr) u32 {\n\
     { view value = at(pointer)[u8, 1]\n\
     return zext[u32](id[u8](value)) } }\n";
  semantic_message "generic-view-inference-dependent-wrap"
    "`id[u8](value) + id[u8](value)` is computed in `u8` and may wrap before it \
     reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     fn bad[T](pointer addr) u32 { view value = pointer[T, 1]\n\
     result u32 = id(value) + id(value)\n\
     return 0 }\n\
     fn main() i32 { values arr[2, u8] = {}\n\
     return bitcast[i32](bad[u8](&values)) }\n";
  semantic_message "generic-view-inference-dependent-wrap-explicit-twin"
    "`id[u8](value) + id[u8](value)` is computed in `u8` and may wrap before it \
     reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     fn bad[T](pointer addr) u32 { view value = pointer[T, 1]\n\
     result u32 = id[T](value) + id[T](value)\n\
     return 0 }\n\
     fn main() i32 { values arr[2, u8] = {}\n\
     return bitcast[i32](bad[u8](&values)) }\n";
  semantic_accept "generic-view-inference-global-const-shadow"
    "fn id[T](value T) T { return value }\n\
     const value u32 = 1\n\
     fn main() i32 { values arr[4, u8] = {0, 200, 0, 0}\n\
     pointer addr = &values\n\
     view value = (pointer + 1)[u8]\n\
     return trunc[i32](zext[u64](id(value) << 4) >> 4) }\n";
  semantic_accept "generic-view-inference-global-const-call-shadow"
    "fn id[T](value T) T { return value }\n\
     const value u32 = 1\n\
     fn at(pointer addr) addr { return pointer }\n\
     fn main() i32 { values arr[4, u8] = {0, 200, 0, 0}\n\
     pointer addr = &values\n\
     view value = at(pointer)[u8, 1]\n\
     return trunc[i32](zext[u64](id(value) << 4) >> 4) }\n";
  semantic_message "generic-view-inference-global-const-loop-shadow"
    "`id[u8](value) << 4` is computed in `u8` and may wrap before it reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     const value u32 = 1\n\
     fn main() i32 { values arr[4, u8] = {0, 200, 0, 0}\n\
     pointer addr = &values\n\
     for view value = (pointer + 1)[u8]; true; {\n\
     result u32 = id(value) << 4\n\
     return bitcast[i32](result >> 4) }\n\
     return 0 }\n";
  semantic_message "generic-view-inference-global-const-loop-shadow-explicit-twin"
    "`id[u8](value) << 4` is computed in `u8` and may wrap before it reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     const value u32 = 1\n\
     fn main() i32 { values arr[4, u8] = {0, 200, 0, 0}\n\
     pointer addr = &values\n\
     for view value = (pointer + 1)[u8]; true; {\n\
     result u32 = id[u8](value) << 4\n\
     return bitcast[i32](result >> 4) }\n\
     return 0 }\n";
  semantic_message "generic-view-inference-global-const-wrap"
    "`id[u8](value) << 4` is computed in `u8` and may wrap before it reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     const value u32 = 1\n\
     fn bad(pointer addr) u32 { view value = (pointer + 1)[u8]\n\
     result u32 = id(value) << 4\n\
     return result }\n";
  semantic_message "generic-view-inference-global-const-wrap-explicit-twin"
    "`id[u8](value) << 4` is computed in `u8` and may wrap before it reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     const value u32 = 1\n\
     fn bad(pointer addr) u32 { view value = (pointer + 1)[u8]\n\
     result u32 = id[u8](value) << 4\n\
     return result }\n";
  semantic_message "generic-view-inference-global-const-generic-wrap"
    "`id[u8](value) << 4` is computed in `u8` and may wrap before it reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     const value u32 = 1\n\
     fn bad[T](pointer addr) u32 { view value = (pointer + 1)[T]\n\
     result u32 = id(value) << 4\n\
     return result }\n\
     fn main() i32 { values arr[2, u8] = {}\n\
     return bitcast[i32](bad[u8](&values)) }\n";
  semantic_message "generic-view-inference-global-const-generic-wrap-explicit-twin"
    "`id[u8](value) << 4` is computed in `u8` and may wrap before it reaches `u32`"
    "fn id[T](value T) T { return value }\n\
     const value u32 = 1\n\
     fn bad[T](pointer addr) u32 { view value = (pointer + 1)[T]\n\
     result u32 = id[T](value) << 4\n\
     return result }\n\
     fn main() i32 { values arr[2, u8] = {}\n\
     return bitcast[i32](bad[u8](&values)) }\n";
  semantic_accept "generic-view-inference-field-const-shadow"
    "fn id[T](value T) T { return value }\n\
     struct B { field u8 }\n\
     const item u32 = 1\n\
     fn main() i32 { b B = {200}\n\
     pointer addr = &b\n\
     view item = pointer[B]\n\
     return trunc[i32](zext[u64](id(item.field) << 4) >> 4) }\n";
  let view_wrap_source declarations argument explicit =
    Printf.sprintf
      "fn id[T](value T) T { return value }\n\
       fn at(pointer addr) addr { return pointer }\n\
       struct Sample { value u8 }\n\
       struct Packet { values arr[2, u8] }\n\
       fn bad(source addr) u32 { %s\n\
       result u32 = id%s(%s) << 4\n\
       return result }\n"
      declarations
      (if explicit then "[u8]" else "")
      argument
  in
  let view_wrap_cases =
    [
      ("field", "view selected = source[Sample]", "selected.value");
      ("call", "view selected = at(source)[u8, 1]", "selected");
      ("element", "view selected = source[u8, ..]", "selected[1]");
      ("call-field", "view selected = at(source)[Sample]", "selected.value");
      ( "field-element",
        "view packet = source[Packet]\nview selected = packet.values",
        "selected[1]" );
      ("call-element", "view selected = at(source)[u8, ..]", "selected[1]");
    ]
  in
  List.iter
    (fun (name, declarations, argument) ->
      let expected =
        Printf.sprintf
          "`id[u8](%s) << 4` is computed in `u8` and may wrap before it reaches `u32`"
          argument
      in
      let messages explicit =
        try semantic_messages (view_wrap_source declarations argument explicit)
        with Failure message ->
          failwith ("generic-view-place-wrap-" ^ name ^ ": " ^ message)
      in
      let inferred = messages false and explicit = messages true in
      if inferred <> [ expected ] || explicit <> inferred then
        failwith
          ("generic-view-place-wrap-" ^ name
         ^ ": inferred and explicit diagnostics differ"))
    view_wrap_cases;
  semantic_accept "generic-const-local-types-in-scopes"
    "fn nested[M const usize](condition u32) u32 { total u32 = 0\n\
     { lanes vec[M, u8] = splat(11)\n\
     total += zext[u32](lanes[0]) }\n\
     if condition > 0 { values arr[M, u8] = {}\n\
     values[0] = 13\n\
     total += zext[u32](values[0]) }\n\
     return total }\n\
     fn main() i32 { return bitcast[i32](nested[3](1)) - 24 }\n";
  let closed_scope_inference =
    "fn id[T](value T) T { return value }\n\
     fn f[T](x u32) u32 { y u8 = 200\n\
     k u32 = 0\n\
     { y u32 = 1000\n\
     x += id(y) }\n\
     r u32 = id(y) + id(y)\n\
     return r + x }\n\
     fn main() i32 { return bitcast[i32](f[u8](10)) }\n"
  in
  semantic_message "generic-inference-closed-scope-narrow-result"
    "`id[u8](y) + id[u8](y)` is computed in `u8` and may wrap before it reaches `u32`"
    closed_scope_inference;
  semantic_accept "generic-inference-unresolved-literal-shapes"
    "fn pick[T](a T, b T, m bool) T { return if m { a } else { b } }\n\
     fn integers(seed i64, c bool) i64 {\n\
     a i64 = pick(seed, 1 + 2, false)\n\
     b i64 = pick(~0, seed, true)\n\
     d i64 = pick(seed, 1 << 40, false)\n\
     e i64 = pick(seed, if c {1} else {2}, false)\n\
     return a + b + d + e\n\
     }\n\
     fn floats(seed f32, c bool) f32 {\n\
     a f32 = pick(seed, 0.5 * 3.0, false)\n\
     b f32 = pick(seed, if c {0.5} else {1.5}, false)\n\
     return a + b\n\
     }\n";
  semantic_accept "generic-inference-generic-field-places"
    "struct G[T] { v T, w arr[2,T] }\n\
     fn pick[T](a T, b T, m bool) T { return if m {a} else {b} }\n\
     fn id[T](a T) T { return a }\n\
     var gg G[u16] = {13, {17, 19}}\n\
     fn main() i32 {\n\
     a u16 = pick(gg.v, gg.v, true)\n\
     b u16 = id(gg.v)\n\
     c u16 = pick(gg.w[0], 1, true)\n\
     g G[u32] = {23, {29, 31}}\n\
     d u32 = id(g.w[1])\n\
     return 0\n\
     }\n";
  semantic_accept "generic-inference-nested-applied-field-places"
    "struct G[T] { v T, w arr[2, T] }\n\
     struct N[U] { inner G[U], k U }\n\
     struct P { g G[u16] }\n\
     var gn N[u8]\n\
     var gp P\n\
     fn id[T](a T) T { return a }\n\
     fn outer[U](x U) U { return id(gn.inner.v) + x }\n\
     fn indexed() u16 { i usize = 0\n\
     return id(gp.g.w[i]) }\n\
     fn raw(p addr) u16 { return id(p[G[u16]].v) }\n";
  let nested_literal_range =
    "struct G[T] { v T }\n\
     struct N[U] { inner G[U] }\n\
     fn pick[T](a T, b T, mode bool) T { return if mode { a } else { b } }\n\
     var n N[u8]\n\
     fn bad() u8 { return pick(n.inner.v, 300 - 299, true) }\n"
  in
  semantic_pin "generic-inference-nested-u8-literal-range" nested_literal_range 5
    (String.length "fn bad() u8 { return pick(n.inner.v, " + 1)
    3 "integer literal is out of range for u8: `300`" None;
  let conflicting_fields =
    "struct P { a u16 }\n\
     struct N { k i8 }\n\
     var gp P\n\
     var gn N\n\
     fn pick[T](a T, b T, mode bool) T { return a }\n\
     fn bad() i32 { return zext[i32](pick(gp.a, gn.k, true)) }\n"
  in
  semantic_pin "generic-inference-conflicting-field-types" conflicting_fields 6
    (String.length "fn bad() i32 { return zext[i32](" + 1)
    (String.length "pick(gp.a, gn.k, true)")
    "cannot infer `T` for `pick`: `gp.a` is `u16`, `gn.k` is `i8`" None;
  let conflicting =
    "fn max[T](a T, b T) T { return a }\nfn f(a u8, b u32) u32 { return max(a, b) }\n"
  in
  semantic_pin "generic-inference-conflicting-types" conflicting 2
    (String.length "fn f(a u8, b u32) u32 { return " + 1)
    (String.length "max(a, b)") "cannot infer `T` for `max`: `a` is `u8`, `b` is `u32`"
    (Some "write `max[u32](a, b)`");
  let signed_unsigned_conflict =
    "fn max[T](a T, b T) T { return a }\nfn f(a i32, b u32) i32 { return max(a, b) }\n"
  in
  semantic_pin "generic-inference-no-rejected-help-i32-u32" signed_unsigned_conflict 2
    (String.length "fn f(a i32, b u32) i32 { return " + 1)
    (String.length "max(a, b)") "cannot infer `T` for `max`: `a` is `i32`, `b` is `u32`"
    None;
  let wide_signed_unsigned_conflict =
    "fn max[T](a T, b T) T { return a }\nfn f(a u64, b i64) u64 { return max(a, b) }\n"
  in
  semantic_pin "generic-inference-no-rejected-help-u64-i64"
    wide_signed_unsigned_conflict 2
    (String.length "fn f(a u64, b i64) u64 { return " + 1)
    (String.length "max(a, b)") "cannot infer `T` for `max`: `a` is `u64`, `b` is `i64`"
    None;
  semantic_accept "generic-inference-explicit-widening-twin"
    "fn max[T](a T, b T) T { return a }\n\
     fn f(a u8, b u32) u32 { return max[u32](a, b) }\n";
  let literals =
    "fn max[T](a T, b T) T { return a }\nfn f() i32 { return max(1, 2) }\n"
  in
  semantic_pin "generic-inference-literals-only" literals 2
    (String.length "fn f() i32 { return " + 1)
    (String.length "max(1, 2)") "cannot infer `T` for `max` from literals only"
    (Some "write `max[i32](1, 2)`");
  let float_literals =
    "fn max[T](a T, b T) T { return a }\nfn f() f64 { return max(1.0, 2.0) }\n"
  in
  semantic_pin "generic-inference-float-literals-only" float_literals 2
    (String.length "fn f() f64 { return " + 1)
    (String.length "max(1.0, 2.0)")
    "cannot infer `T` for `max` from literals only" (Some "write `max[f64](1.0, 2.0)`");
  semantic_accept "generic-inference-float-literals-only-help-twin"
    "fn max[T](a T, b T) T { return a }\nfn f() f64 { return max[f64](1.0, 2.0) }\n";
  semantic_pin "generic-inference-literal-parameter-with-unrelated-peer"
    "fn choose[T](value T, count u32) T { return value }\n\
     fn run(count u32) i32 { return choose(1, count) }\n"
    2
    (String.length "fn run(count u32) i32 { return " + 1)
    16 "cannot infer `T` for `choose` from literals only"
    (Some "write `choose[i32](1, count)`");
  let undetermined =
    "fn choose[N const usize, T](value T) T { return value }\n\
     fn run(value u32) u32 { return choose(value) }\n"
  in
  semantic_pin "generic-inference-undetermined-parameter" undetermined 2
    (String.length "fn run(value u32) u32 { return " + 1)
    (String.length "choose(value)")
    "cannot infer `N` for `choose`; provide explicit generic arguments"
    (Some "write the explicit generic argument list for `choose`");
  semantic_pin "generic-inference-no-result-inference"
    "fn zero[T]() T { return 0 }\nfn run() u32 { return zero() }\n" 2
    (String.length "fn run() u32 { return " + 1)
    (String.length "zero()")
    "cannot infer `T` for `zero`; provide explicit generic arguments"
    (Some "write the explicit generic argument list for `zero`");
  let const_conflict =
    "fn sc2[N const usize](v vec[N, u16], w vec[N, u16]) void { return }\n\
     fn f(v vec[2, u16], w vec[3, u16]) void { sc2(v, w)\n\
     return }\n"
  in
  semantic_pin "generic-inference-conflicting-const" const_conflict 2
    (String.length "fn f(v vec[2, u16], w vec[3, u16]) void { " + 1)
    (String.length "sc2(v, w)")
    "cannot infer `N` for `sc2`: `v` gives `2`, `w` gives `3`" None;
  let const_type_position_conflict =
    "struct Blob[N const usize] { values arr[N, u8] }\n\
     fn sc2[N const usize](v vec[N, u8], b handle[Blob[N]]) void { return }\n\
     fn f(v vec[2, u8], b handle[Blob[3]]) void { sc2(v, b)\n\
     return }\n"
  in
  semantic_pin "generic-inference-const-type-position-conflict"
    const_type_position_conflict 3
    (String.length "fn f(v vec[2, u8], b handle[Blob[3]]) void { " + 1)
    (String.length "sc2(v, b)")
    "cannot infer `N` for `sc2`: `v` gives `2`, `b` gives `3`" None;
  semantic_accept "generic-inference-const-type-position-twin"
    "struct Blob[N const usize] { values arr[N, u8] }\n\
     fn sc2[N const usize](v vec[N, u8], b handle[Blob[N]]) void { return }\n\
     fn f(v vec[2, u8], b handle[Blob[2]]) void { sc2(v, b)\n\
     return }\n";
  semantic_message "generic-inference-generic-function-address"
    "generic function `f` has no single address"
    "fn f[T](value T) T { return value }\nvar P addr = &f\n"

let () =
  let reduce_sum = "fn bad(v vec[4,f32]) f32 { return reduce_sum(v) }\n" in
  semantic_pin "float-vector-reduce-sum" reduce_sum 1
    (String.length "fn bad(v vec[4,f32]) f32 { return reduce_sum(" + 1)
    1 "`reduce_sum` is not defined for floating-point vectors"
    (Some "the order is not defined; write the additions explicitly");
  semantic_accept "float-vector-reduce-sum-integer-twin"
    "fn good(v vec[4,i32]) i32 { return reduce_sum(v) }\n";
  let reduce_min = "fn bad(v vec[4,f32]) f32 { return reduce_min(v) }\n" in
  semantic_pin "float-vector-reduce-min" reduce_min 1
    (String.length "fn bad(v vec[4,f32]) f32 { return reduce_min(" + 1)
    1 "`reduce_min` is not defined for floating-point vectors" None;
  semantic_accept "float-vector-reduce-min-integer-twin"
    "fn good(v vec[4,i32]) i32 { return reduce_min(v) }\n";
  let rotl = "fn bad(v vec[4,f32]) vec[4,f32] { return rotl(v, 1) }\n" in
  semantic_pin "float-vector-rotl" rotl 1
    (String.length "fn bad(v vec[4,f32]) vec[4,f32] { return rotl(" + 1)
    1 "`rotl` value must be an integer or integer vector, got `vec[4, f32]`" None;
  semantic_accept "float-vector-rotl-integer-twin"
    "fn good(v vec[4,u32]) vec[4,u32] { return rotl(v, 1) }\n";
  let popcount = "fn bad(v vec[4,f32]) vec[4,f32] { return popcount(v) }\n" in
  semantic_pin "float-vector-popcount" popcount 1
    (String.length "fn bad(v vec[4,f32]) vec[4,f32] { return " + 1)
    (String.length "popcount(v)")
    "builtin argument must be an integer or an integer vector" None;
  semantic_accept "float-vector-popcount-integer-twin"
    "fn good(v vec[4,u32]) vec[4,u32] { return popcount(v) }\n";
  let add_sat =
    "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return add_sat(a,b) }\n"
  in
  semantic_pin "float-vector-add-sat" add_sat 1
    (String.length "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return add_sat(" + 1)
    1 "`add_sat` needs an integer or integer vector, got `vec[4, f32]`" None;
  semantic_accept "float-vector-add-sat-integer-twin"
    "fn good(a vec[4,u32], b vec[4,u32]) vec[4,u32] { return add_sat(a,b) }\n";
  let remainder = "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return a % b }\n" in
  semantic_pin "float-vector-remainder" remainder 1
    (String.length "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return a % " + 1)
    1 "remainder `%` is not defined for floating-point vectors" None;
  semantic_accept "float-vector-remainder-integer-twin"
    "fn good(a vec[4,i32], b vec[4,i32]) vec[4,i32] { return a % b }\n";
  let bitwise = "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return a & b }\n" in
  semantic_pin "float-vector-bitwise" bitwise 1
    (String.length "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return " + 1)
    1 "operator `&` needs integer or integer vector operands, got `vec[4, f32]`" None;
  semantic_accept "float-vector-bitwise-integer-twin"
    "fn good(a vec[4,u32], b vec[4,u32]) vec[4,u32] { return a & b }\n";
  let shift = "fn bad(a vec[4,f32]) vec[4,f32] { return a << 1 }\n" in
  semantic_pin "float-vector-shift" shift 1
    (String.length "fn bad(a vec[4,f32]) vec[4,f32] { return " + 1)
    1
    "left operand of `<<` has type `vec[4, f32]`, expected an integer or integer vector"
    None;
  semantic_accept "float-vector-shift-integer-twin"
    "fn good(a vec[4,u32]) vec[4,u32] { return a << 1 }\n";
  let widening = "fn bad(a vec[4,f32]) vec[4,f64] { return a }\n" in
  semantic_pin "float-vector-no-widening" widening 1
    (String.length "fn bad(a vec[4,f32]) vec[4,f64] { return " + 1)
    1 "return value is `vec[4, f32]`, expected `vec[4, f64]`" None;
  semantic_accept "float-vector-explicit-widening-twin"
    "fn good(a vec[4,f32]) vec[4,f64] { return cvt[vec[4,f64]](a) }\n";
  let cvt_lanes = "fn bad(a vec[4,f32]) vec[8,f64] { return cvt[vec[8,f64]](a) }\n" in
  semantic_pin "float-vector-cvt-lane-count" cvt_lanes 1
    (String.length "fn bad(a vec[4,f32]) vec[8,f64] { return " + 1)
    (String.length "cvt[vec[8,f64]](a)")
    "`cvt` needs an integer or floating-point source and target, got `vec[4, f32]` to \
     `vec[8, f64]`"
    None;
  semantic_accept "float-vector-cvt-lane-count-twin"
    "fn good(a vec[4,f32]) vec[4,f64] { return cvt[vec[4,f64]](a) }\n";
  semantic_error "extern-c-float-vector-parameter"
    "cannot use `vec[4, f32]` by value; use a pointer"
    "extern \"C\" { fn take(value vec[4,f32]) void }\n";
  semantic_accept "float-vector-lane-and-constant-twins"
    "const V vec[4,f32] = {1.0, 2.0, 3.0, 4.0}\n\
     fn f(x vec[4,f32], i usize) f32 { y vec[4,f32] = x + splat(x[i])\n\
     return y[i] }\n"

let () =
  let reductions = [ "reduce_max"; "reduce_and"; "reduce_or"; "reduce_xor" ] in
  List.iter
    (fun name ->
      let source = Printf.sprintf "fn bad(v vec[4,f32]) f32 { return %s(v) }\n" name in
      semantic_pin ("float-vector-" ^ name) source 1
        (String.length ("fn bad(v vec[4,f32]) f32 { return " ^ name ^ "(") + 1)
        1
        (Printf.sprintf "`%s` is not defined for floating-point vectors" name)
        None;
      semantic_accept
        ("float-vector-" ^ name ^ "-integer-twin")
        (Printf.sprintf "fn good(v vec[4,i32]) i32 { return %s(v) }\n" name))
    reductions;
  let reduce_sum_f64 = "fn bad(v vec[2,f64]) f64 { return reduce_sum(v) }\n" in
  semantic_pin "float-vector-reduce-sum-f64-help" reduce_sum_f64 1
    (String.length "fn bad(v vec[2,f64]) f64 { return reduce_sum(" + 1)
    1 "`reduce_sum` is not defined for floating-point vectors"
    (Some "the order is not defined; write the additions explicitly");
  List.iter
    (fun (name, operation) ->
      let source =
        Printf.sprintf "fn bad(v vec[4,f32]) vec[4,f32] { return %s(v) }\n" name
      in
      semantic_pin ("float-vector-" ^ name) source 1
        (String.length "fn bad(v vec[4,f32]) vec[4,f32] { return " + 1)
        (String.length (name ^ "(v)"))
        operation None;
      semantic_accept
        ("float-vector-" ^ name ^ "-integer-twin")
        (Printf.sprintf "fn good(v vec[4,u32]) vec[4,u32] { return %s(v) }\n" name))
    [
      ("clz", "builtin argument must be an integer or an integer vector");
      ("ctz", "builtin argument must be an integer or an integer vector");
    ];
  List.iter
    (fun name ->
      let source =
        Printf.sprintf
          "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return %s(a,b) }\n" name
      in
      semantic_pin ("float-vector-" ^ name) source 1
        (String.length
           (Printf.sprintf "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return %s("
              name)
        + 1)
        1
        (Printf.sprintf "`%s` needs an integer or integer vector, got `vec[4, f32]`"
           name)
        None;
      semantic_accept
        ("float-vector-" ^ name ^ "-integer-twin")
        (Printf.sprintf
           "fn good(a vec[4,u32], b vec[4,u32]) vec[4,u32] { return %s(a,b) }\n" name))
    [ "sub_sat"; "mul_hi" ];
  List.iter
    (fun (operator, name) ->
      let source =
        Printf.sprintf
          "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return a %s b }\n" operator
      in
      semantic_pin
        ("float-vector-bitwise-" ^ name)
        source 1
        (String.length "fn bad(a vec[4,f32], b vec[4,f32]) vec[4,f32] { return " + 1)
        1
        (Printf.sprintf
           "operator `%s` needs integer or integer vector operands, got `vec[4, f32]`"
           operator)
        None;
      semantic_accept
        ("float-vector-bitwise-" ^ name ^ "-integer-twin")
        (Printf.sprintf
           "fn good(a vec[4,u32], b vec[4,u32]) vec[4,u32] { return a %s b }\n" operator))
    [ ("|", "or"); ("^", "xor") ];
  List.iter
    (fun (operator, name) ->
      let source =
        Printf.sprintf "fn bad(a vec[4,f32]) vec[4,f32] { return a %s 1 }\n" operator
      in
      semantic_pin
        ("float-vector-shift-" ^ name)
        source 1
        (String.length "fn bad(a vec[4,f32]) vec[4,f32] { return " + 1)
        1
        (Printf.sprintf
           "left operand of `%s` has type `vec[4, f32]`, expected an integer or \
            integer vector"
           operator)
        None;
      semantic_accept
        ("float-vector-shift-" ^ name ^ "-integer-twin")
        (Printf.sprintf "fn good(a vec[4,u32]) vec[4,u32] { return a %s 1 }\n" operator))
    [ (">>", "right"); ("<<", "left") ];
  let bit_not = "fn bad(v vec[4,f32]) vec[4,f32] { return ~v }\n" in
  semantic_pin "float-vector-bit-not" bit_not 1
    (String.length "fn bad(v vec[4,f32]) vec[4,f32] { return " + 2)
    1 "unary operator `~` needs an integer or integer vector, got `vec[4, f32]`" None;
  semantic_accept "float-vector-bit-not-integer-twin"
    "fn good(v i32) i32 { return ~v }\n";
  let compound_source operator =
    Printf.sprintf "fn bad(a vec[4,f32], b vec[4,f32]) void { a %s b }\n" operator
  in
  List.iter
    (fun (operator, name) ->
      let source = compound_source operator in
      let prefix = "fn bad(a vec[4,f32], b vec[4,f32]) void { a " in
      semantic_pin
        ("float-vector-compound-" ^ name)
        source 1
        (String.length prefix + 1)
        (String.length operator)
        (Printf.sprintf "compound assignment `%s` is not defined for `vec[4, f32]`"
           operator)
        None;
      semantic_accept
        ("float-vector-compound-" ^ name ^ "-integer-twin")
        (Printf.sprintf "fn good(a vec[4,u32], b vec[4,u32]) void { a %s b }\n" operator))
    [ ("%=", "remainder"); ("&=", "and"); ("|=", "or"); ("^=", "xor") ]

let () =
  List.iter
    (fun (operator, name) ->
      semantic_accept ("line-continuation-" ^ name)
        (Printf.sprintf "fn good(a i32, b i32) i32 {\n  return a %s\n  b\n}\n" operator))
    [
      ("+", "add");
      ("-", "sub");
      ("*", "mul");
      ("/", "div");
      ("%", "rem");
      ("&", "bit-and");
      ("|", "bit-or");
      ("^", "bit-xor");
      ("<<", "shl");
      (">>", "shr");
    ];
  List.iter
    (fun (operator, name) ->
      semantic_accept ("line-continuation-" ^ name)
        (Printf.sprintf
           "fn good(a i32, b i32) i32 {\n\
           \  if a %s\n\
           \  b {\n\
           \    return 1\n\
           \  }\n\
           \  return 0\n\
            }\n"
           operator))
    [ ("==", "eq"); ("!=", "ne"); ("<", "lt"); ("<=", "le"); (">", "gt"); (">=", "ge") ];
  semantic_accept "line-continuation-and"
    "fn good(a i32, b i32) i32 {\n\
    \  if a > 0 &&\n\
    \  b > 0 {\n\
    \    return 1\n\
    \  }\n\
    \  return 0\n\
     }\n";
  semantic_accept "line-continuation-or"
    "fn good(a i32, b i32) i32 {\n\
    \  if a > 0 ||\n\
    \  b > 0 {\n\
    \    return 1\n\
    \  }\n\
    \  return 0\n\
     }\n";
  semantic_accept "line-continuation-while-condition"
    "fn good(n i32) i32 {\n\
    \  i i32 = 0\n\
    \  while i <\n\
    \  n {\n\
    \    i += 1\n\
    \  }\n\
    \  return i\n\
     }\n";
  semantic_accept "line-continuation-for-condition"
    "fn good(n i32) i32 {\n\
    \  total i32 = 0\n\
    \  for i i32 = 0; i <\n\
    \  n &&\n\
    \  total >=\n\
    \  0; i += 1 {\n\
    \    total += i\n\
    \  }\n\
    \  return total\n\
     }\n";
  semantic_accept "line-continuation-call-argument"
    "fn add(a i32, b i32) i32 {\n\
    \  return a + b\n\
     }\n\
     fn good(a i32, b i32) i32 {\n\
    \  return add(a +\n\
    \  b, 1)\n\
     }\n";
  semantic_accept "line-continuation-initializer"
    "fn good(a i32, b i32) i32 {\n  x i32 = a +\n  b\n  return x\n}\n";
  semantic_accept "line-continuation-assignment"
    "fn good(a i32, b i32) i32 {\n  x i32 = 0\n  x = a *\n  b\n  return x\n}\n";
  semantic_accept "line-continuation-return-chain"
    "fn good(a i32, b i32, c i32) i32 {\n  return a +\n  b +\n  c\n}\n";
  semantic_accept "line-continuation-blank-and-comment-lines"
    "fn good(a i32, b i32) i32 {\n  return a *\n\n  // a comment\n\n  b\n}\n";
  syntax_pin "line-continuation-operator-before-closing-brace"
    "fn bad(a i32) i32 {\n  x i32 = a +\n}\n" 2 14 1
    "expected an expression, found newline";
  syntax_pin "line-continuation-operator-before-blank-and-comment-lines"
    "fn bad(a i32) i32 {\n  x i32 = a +\n\n  // note\n}\n" 2 14 1
    "expected an expression, found newline";
  syntax_pin "line-continuation-operator-before-closing-paren"
    "fn bad(a i32) i32 {\n  return (a +\n)\n}\n" 3 1 1
    "expected an expression, found `)`";
  syntax_pin "line-continuation-assignment-stays-unjoined"
    "fn bad(a i32) i32 {\n  x i32 = 0\n  x =\n  a\n  return x\n}\n" 3 6 1
    "expected an expression, found newline";
  syntax_pin "line-continuation-while-operator-before-brace"
    "fn bad(n i32) i32 {\n  while n <\n}\n" 2 12 1
    "expected an expression, found newline"

let () =
  let c_matrix = c_import_fixture "matrix.h" in
  semantic_accept "expression-statement-void-call"
    "fn g() void { }\nfn f() i32 {\n  g()\n  return 0\n}\n";
  semantic_accept "expression-statement-value-call"
    "fn h() i32 { return 1 }\nfn f() i32 {\n  h()\n  return 0\n}\n";
  semantic_accept "expression-statement-builtin-store"
    "fn f(p addr) void {\n  volatile_store[u32](p, 7)\n  return\n}\n";
  c_semantic_accept "expression-statement-c-call" c_matrix
    "fn probe(value u32) u32 {\n  fas_u32_echo(value)\n  return value\n}\n";
  semantic_accept "expression-statement-parenthesized-call"
    "fn g() i32 { return 1 }\nfn f() i32 {\n  (g())\n  return 0\n}\n";
  semantic_pin "expression-statement-address-of"
    "fn f(x i32) i32 {\n  &x\n  return 0\n}\n" 2 3 2 "`&x` is not used"
    (Some "to continue the previous line, end it with `&`");
  semantic_pin "expression-statement-leading-and"
    "fn f(a u32, b u32) u32 {\n  y u32 = a\n  & b\n  return y\n}\n" 3 3 3
    "`&b` is not used" (Some "to continue the previous line, end it with `&`");
  semantic_pin "expression-statement-leading-minus"
    "fn f(a u32, b u32) u32 {\n  y u32 = a\n  - b\n  return y\n}\n" 3 3 3
    "`-b` is not used" (Some "to continue the previous line, end it with `-`");
  semantic_pin "expression-statement-identifier"
    "fn f(i i32) i32 {\n  i\n  return 0\n}\n" 2 3 1 "`i` is not used" None;
  semantic_pin "expression-statement-not" "fn f(b bool) i32 {\n  !b\n  return 0\n}\n" 2
    3 2 "`!b` is not used" None;
  semantic_pin "expression-statement-binary-operator"
    "fn f(x i32) i32 {\n  x + 1\n  return 0\n}\n" 2 3 5 "`x + 1` is not used" None;
  semantic_pin "expression-statement-comparison"
    "fn f(a i32, b i32) i32 {\n  a == b\n  return 0\n}\n" 2 3 6 "`a == b` is not used"
    None;
  semantic_pin "expression-statement-cast"
    "fn f(x u32) u32 {\n  zext[u32](x)\n  return 0\n}\n" 2 3 4
    "illegal `zext` from `u32` to `u32`: the value already has type `u32`" None;
  semantic_pin "expression-statement-star-dereference"
    "fn f(p addr) i32 {\n  *p\n  return 0\n}\n" 2 3 1
    "Fas has no unary `*`; read through an `addr` with `p[T]`" None;
  semantic_pin "expression-statement-arrow-field"
    "fn f(p addr) i32 {\n  p->value\n  return 0\n}\n" 2 4 2 "Fas has no `->` operator"
    None;
  semantic_pin "expression-statement-sizeof-without-type"
    "fn f(i i32) i32 {\n  sizeof(i)\n  return 0\n}\n" 2 3 6
    "`sizeof` needs a type in brackets" (Some "write `sizeof[i32]`");
  semantic_pin "expression-statement-for-step"
    "fn f(value u32) u32 {\n  for i u32 = 0; i < 4; (value) {\n  }\n  return 0\n}\n" 2
    25 7 "`(value)` is not used" None

let widening_help_destinations = [ "i32"; "u32"; "i64"; "u64" ]
let widening_help_operands = [ "i8"; "i16"; "u8"; "u16" ]

let widening_help_operators =
  [ "+"; "-"; "*"; "/"; "%"; "<<"; ">>"; "&"; "|"; "^"; "unary -"; "unary ~" ]

let widening_help_cases =
  [
    ("+", "s", "t", "s + t");
    ("-", "s", "t", "s - t");
    ("*", "s", "t", "s * t");
    ("/", "s", "t", "s / t");
    ("%", "s", "t", "s % t");
    ("<<", "s", "t", "s << t");
    (">>", "s", "t", "s >> t");
    ("&", "s", "t", "s & t");
    ("|", "s", "t", "s | t");
    ("^", "s", "t", "s ^ t");
    ("unary -", "s", "t", "-s");
    ("unary ~", "s", "t", "~s");
    (">>", "s", "t", "(s / t) >> 1");
    (">>", "a", "b", "(a + b) >> 1");
    ("&", "a", "b", "(a + b) & 1");
    ("|", "a", "b", "(a + b) | 1");
    ("^", "a", "b", "(a + b) ^ 1");
  ]

let widening_help_program first second operand destination expression =
  Printf.sprintf "fn f(%s %s, %s %s) %s {\n  y %s = %s\n  return y\n}\n" first operand
    second operand destination destination expression

let widening_help_replacement help =
  match positions help ": `" with
  | index :: _
    when String.length help > index + 4 && help.[String.length help - 1] = '`' ->
      let start = index + 3 in
      Some (String.sub help start (String.length help - start - 1))
  | _ -> None

let () =
  let helped = Hashtbl.create 16 in
  List.iter
    (fun (label, first, second, expression) ->
      List.iter
        (fun operand ->
          List.iter
            (fun destination ->
              let text =
                widening_help_program first second operand destination expression
              in
              incr checks_run;
              match Sema.check (expect_ok (Parser.parse (source text))) with
              | Ok _ -> ()
              | Error [ { Diag.help = Some help; _ } ] -> (
                  match widening_help_replacement help with
                  | None -> ()
                  | Some replacement -> (
                      Hashtbl.replace helped label ();
                      let fixed =
                        widening_help_program first second operand destination
                          replacement
                      in
                      match Sema.check (expect_ok (Parser.parse (source fixed))) with
                      | Ok _ -> ()
                      | Error diagnostics ->
                          failwith
                            ("widening help does not compile: " ^ replacement ^ "\n"
                            ^ Diag.render_all ~source:(Some (source fixed)) diagnostics
                            )))
              | Error _ -> ())
            widening_help_destinations)
        widening_help_operands)
    widening_help_cases;
  List.iter
    (fun label ->
      if not (Hashtbl.mem helped label) then
        failwith ("no widening help produced for operator " ^ label))
    widening_help_operators

let () =
  let signed_narrow_source =
    "fn f(s i16, t i16) u32 {\n  y u32 = (s / t) >> 1\n  return y\n}\n"
  in
  semantic_pin "widening-help-signed-narrow-shift" signed_narrow_source 2 11 12
    "`(s / t) >> 1` is computed in `i16` and may wrap before it reaches `u32`"
    (Some "widen an operand first: `bitcast[u32](sext[i32](s) / t >> 1)`");
  conversion_help_twin "widening-help-signed-narrow-shift-compiles" signed_narrow_source
    "(s / t) >> 1" "bitcast[u32](sext[i32](s) / t >> 1)"
    "widen an operand first: `bitcast[u32](sext[i32](s) / t >> 1)`";
  let vector_lane_source =
    "fn f(v vec[4, i16]) u32 {\n  y u32 = (v * v)[0]\n  return y\n}\n"
  in
  semantic_pin "widening-help-vector-lane" vector_lane_source 2 11 10
    "`(v * v)[0]` is computed in `i16` and may wrap before it reaches `u32`"
    (Some
       "widen vector operands first: `bitcast[u32]((sext[vec[4, i32]](v) * sext[vec[4, \
        i32]](v))[0])`");
  conversion_help_twin "widening-help-vector-lane-compiles" vector_lane_source
    "(v * v)[0]" "bitcast[u32]((sext[vec[4, i32]](v) * sext[vec[4, i32]](v))[0])"
    "widen vector operands first: `bitcast[u32]((sext[vec[4, i32]](v) * sext[vec[4, \
     i32]](v))[0])`"

let () =
  let mixed_leaf_division_source =
    "fn f(a u16, s i32) i64 { value i64 = (a / 3) & s; return value }\n"
  in
  semantic_pin "widening-help-mixed-leaf-division" mixed_leaf_division_source 1
    (String.length "fn f(a u16, s i32) i64 { value i64 = " + 1)
    (String.length "(a / 3) & s")
    "`(a / 3) & s` is computed in `i32` and may wrap before it reaches `i64`"
    (Some "widen an operand first: `zext[i64](a) / 3 & s`");
  conversion_help_twin "widening-help-mixed-leaf-division-compiles"
    mixed_leaf_division_source "(a / 3) & s" "zext[i64](a) / 3 & s"
    "widen an operand first: `zext[i64](a) / 3 & s`";
  let mixed_leaf_product_source =
    "fn f(a u16, s i32) i64 { value i64 = (a * s) + 1; return value }\n"
  in
  semantic_pin "widening-help-mixed-leaf-product" mixed_leaf_product_source 1
    (String.length "fn f(a u16, s i32) i64 { value i64 = " + 1)
    (String.length "(a * s) + 1")
    "`(a * s) + 1` is computed in `i32` and may wrap before it reaches `i64`"
    (Some "widen an operand first: `zext[i64](a) * s + 1`");
  conversion_help_twin "widening-help-mixed-leaf-product-compiles"
    mixed_leaf_product_source "(a * s) + 1" "zext[i64](a) * s + 1"
    "widen an operand first: `zext[i64](a) * s + 1`";
  let mixed_leaf_const_source =
    "const A u16 = 40000\n\
     const S i32 = -1\n\
     const Y i64 = (A * S) + 1\n\
     fn f() i64 { return Y }\n"
  in
  semantic_pin "widening-help-mixed-leaf-const-declaration" mixed_leaf_const_source 3
    (String.length "const Y i64 = (" + 1)
    (String.length "A * S")
    "`A * S` is computed in `i32` and may wrap before it reaches `i64`"
    (Some "widen an operand first: `zext[i64](A) * S`");
  conversion_help_twin "widening-help-mixed-leaf-const-declaration-compiles"
    mixed_leaf_const_source "A * S" "zext[i64](A) * S"
    "widen an operand first: `zext[i64](A) * S`";
  let mixed_leaf_switch_source =
    "const A u16 = 40000\n\
     const S i32 = -1\n\
     fn f(y i64) i64 {\n\
    \  switch y {\n\
    \    case (A * S) + 1: return 1\n\
    \    case 2: return 2\n\
    \  }\n\
    \  return 0\n\
     }\n"
  in
  semantic_pin "widening-help-mixed-leaf-switch-case" mixed_leaf_switch_source 5
    (String.length "    case (" + 1)
    (String.length "A * S")
    "`A * S` is computed in `i32` and may wrap before it reaches `i64`"
    (Some "widen an operand first: `zext[i64](A) * S`");
  conversion_help_twin "widening-help-mixed-leaf-switch-case-compiles"
    mixed_leaf_switch_source "A * S" "zext[i64](A) * S"
    "widen an operand first: `zext[i64](A) * S`";
  let mixed_leaf_generic_source =
    "fn g[N const i64]() i64 { return N }\n\
     const A u16 = 40000\n\
     const S i32 = -1\n\
     fn f() i64 { return g[(A * S) + 1]() }\n"
  in
  semantic_pin "widening-help-mixed-leaf-generic-argument" mixed_leaf_generic_source 4
    (String.length "fn f() i64 { return g[(" + 1)
    (String.length "A * S")
    "`A * S` is computed in `i32` and may wrap before it reaches `i64`"
    (Some "widen an operand first: `zext[i64](A) * S`");
  conversion_help_twin "widening-help-mixed-leaf-generic-argument-compiles"
    mixed_leaf_generic_source "A * S" "zext[i64](A) * S"
    "widen an operand first: `zext[i64](A) * S`"

let () =
  semantic_accept "const-loop-array-size-and-generic-argument"
    "fn scale[K const usize](x usize) usize { return x * K }\n\
     fn f() usize {\n\
    \  total usize = 0\n\
    \  for const i usize in 0..4 {\n\
    \    a arr[i + 1, u8] = {}\n\
    \    total += scale[i](len(a))\n\
    \  }\n\
    \  return total\n\
     }\n";
  semantic_accept "const-loop-prunes-per-copy"
    "fn f(x u32) u32 {\n\
    \  for const i u32 in 0..3 {\n\
    \    if i != 0 { x += 10 / i }\n\
    \  }\n\
    \  return x\n\
     }\n";
  semantic_accept "const-loop-nested"
    "fn f(x u64) u64 {\n\
    \  for const i u64 in 0..3 {\n\
    \    for const j u64 in i..3 {\n\
    \      if j == 1 { continue }\n\
    \      x = x * 3 + i * j\n\
    \    }\n\
    \  }\n\
    \  return x\n\
     }\n";
  semantic_accept "const-loop-copy-scopes"
    "fn f() u32 {\n\
    \  t u32 = 0\n\
    \  for const i u32 in 0..3 {\n\
    \    y u32 = i\n\
    \    t += y\n\
    \  }\n\
    \  return t\n\
     }\n";
  semantic_accept "const-loop-empty-range"
    "fn f() u32 {\n\
    \  for const i u32 in 3..3 { return 1 }\n\
    \  for const i i32 in 2..-2 { return 2 }\n\
    \  return 0\n\
     }\n";
  semantic_accept "const-loop-budget-boundary"
    "fn f() void {\n  for const i u32 in 0..4096 { }\n}\n";
  let generic_loop =
    expect_ok
      (Parser.parse
         (source
            "fn scale[K const u32](x u32) u32 { return x * K }\n\
             fn sum[N const u32](x u32) u32 {\n\
            \  t u32 = 0\n\
            \  for const i u32 in 0..N { t += scale[i + 1](x) }\n\
            \  return t\n\
             }\n\
             fn f(x u32) u32 { return sum[3](x) + sum[5](x) }\n"))
    |> Sema.check |> expect_ok
  in
  let instances name =
    List.length
      (List.filter
         (fun (func : Hir.func) -> contains func.name (name ^ "$spec$"))
         generic_loop.Hir.funcs)
  in
  if instances "scale" <> 5 || instances "sum" <> 2 then
    failwith "const-loop-generic-bounds: unexpected instance counts";
  semantic_pin "const-loop-break"
    "fn f() void {\n  for const i u32 in 0..4 { break }\n}\n" 2 29 5
    "`break` cannot leave a `for const` loop; its copies are straight-line code" None;
  syntax_pin "const-loop-label"
    "fn f() void {\n  outer: for const i u32 in 0..4 { }\n}\n" 2 3 5
    "a `for const` loop cannot have a label";
  syntax_pin "const-loop-missing-in" "fn f() void {\n  for const i u32 0..4 { }\n}\n" 2
    19 1 "expected `in` after `for const i u32`";
  semantic_pin "const-loop-budget"
    "fn f() void {\n  for const i u32 in 0..4097 { }\n}\n" 2 3 3
    "`for const` loop has 4097 iterations: budget max_const_loop_iterations of 4096 \
     (profile 0.1.5)"
    None;
  let third =
    "fn f(x u32) u32 {\n\
    \  for const i u32 in 0..5 {\n\
    \    if i == 3 { x = x / 0 }\n\
    \  }\n\
    \  return x\n\
     }\n"
  in
  semantic_message "const-loop-iteration-error"
    "division by zero is not a defined runtime operation" third;
  semantic_note "const-loop-iteration-note" "in iteration `i = 3`" third;
  semantic_accept "const-loop-half-open"
    "fn f(x u8) u8 {\n\
    \  for const i u8 in 0..3 {\n\
    \    if i == 3 { x = x / 0 }\n\
    \  }\n\
    \  return x\n\
     }\n";
  semantic_note "const-loop-first-value" "in iteration `i = 0`"
    "fn f(x u8) u8 {\n\
    \  for const i u8 in 0..3 {\n\
    \    if i == 0 { x = x / 0 }\n\
    \  }\n\
    \  return x\n\
     }\n";
  semantic_note "const-loop-signed-note" "in iteration `i = -2`"
    "fn f(x i32) i32 {\n\
    \  for const i i32 in -3..0 {\n\
    \    if i == -2 { x = x / 0 }\n\
    \  }\n\
    \  return x\n\
     }\n";
  all_errors_pin "const-loop-independent-error-once"
    "fn f() void {\n  for const i u32 in 0..4 {\n    x u32 = 1 + true\n  }\n}\n"
    [ (3, "integer literal is not a `bool`; write `true` or `false`") ];
  semantic_pin "const-loop-shadows-local"
    "fn f() void {\n  i u32 = 0\n  for const i u32 in 0..2 { }\n}\n" 3 3 3
    "`for const` index `i` shadows a local of the same name" None;
  semantic_message "const-loop-shadows-index"
    "`for const` index `i` shadows a constant of the same name"
    "fn f() void {\n  for const i u32 in 0..2 { for const i u32 in 0..2 { } }\n}\n";
  semantic_message "const-loop-shadows-generic-constant"
    "`for const` index `N` shadows a constant of the same name"
    "fn g[N const u32]() void {\n\
    \  for const N u32 in 0..2 { }\n\
     }\n\
     fn f() void { g[1]() }\n";
  semantic_pin "const-loop-float-index"
    "fn f() void {\n  for const i f32 in 0..2 { }\n}\n" 2 15 3
    "a `for const` index must have an integer type" None;
  semantic_message "const-loop-bound-out-of-range"
    "integer literal is out of range for u8: `256`"
    "fn f() void {\n  for const i u8 in 0..256 { }\n}\n";
  semantic_pin "const-loop-runtime-bound"
    "fn f(n u32) void {\n  for const i u32 in 0..n { }\n}\n" 2 25 1
    "`for const` bounds must be compile-time constants" None;
  semantic_message "const-loop-index-redeclared" "duplicate local `i`"
    "fn f() void {\n  for const i u8 in 0..2 { i u8 = 1 }\n}\n";
  let node_limits = { Limits.default with max_ast_nodes = 1000 } in
  let node_check count =
    Sema.check ~limits:node_limits
      (expect_ok
         (Parser.parse
            (source
               (Printf.sprintf
                  "fn f() u32 {\n\
                  \  x u32 = 0\n\
                  \  for const i u32 in 0..%d { x += i }\n\
                  \  return x\n\
                   }\n"
                  count))))
  in
  ignore (expect_ok (node_check 10));
  (match node_check 1000 with
  | Error [ diagnostic ]
    when diagnostic.Diag.message
         = "cumulative expanded AST nodes exceed budget max_ast_nodes of 1000 (profile \
            0.1.5)" ->
      ()
  | Error diagnostics ->
      failwith
        ("const-loop-node-budget: unexpected diagnostic: "
        ^ Diag.render_all ~source:None diagnostics)
  | Ok _ -> failwith "const-loop-node-budget: expected rejection");
  semantic_message "const-loop-index-scope" "unknown name `i`"
    "fn f() u8 {\n  for const i u8 in 0..2 { }\n  return i\n}\n"

let () =
  let literal_render name source expected =
    incr checks_run;
    let rendered = semantic_render source in
    if rendered <> expected then failwith (name ^ ": " ^ rendered)
  in
  let unsigned_help digits =
    "an unsigned type has no negative values; write " ^ digits ^ " for all ones"
  in
  let not_bool = "integer literal is not a `bool`; write `true` or `false`" in
  literal_render "literal-unsigned-negative-one-u32"
    "fn f() u32 { x u32 = -1\n return x }\n"
    (expected_diagnostic "fn f() u32 { x u32 = -1" 22 1
       "integer literal is out of range for u32: `-1`" (unsigned_help "0xFFFFFFFF"));
  literal_render "literal-unsigned-negative-255-u8"
    "fn f() u8 { x u8 = -255\n return x }\n"
    (expected_diagnostic_without_help "fn f() u8 { x u8 = -255" 20 1
       "integer literal is out of range for u8: `-255`");
  literal_render "literal-const-unsigned-negative-one-u64"
    "const X u64 = -1\nfn f() u64 { return 0 }\n"
    (expected_diagnostic "const X u64 = -1" 15 1
       "integer literal is out of range for u64: `-1`"
       (unsigned_help "0xFFFFFFFFFFFFFFFF"));
  let generic_call =
    "fn f[T](v T) T { return v }\n\
     fn g() u32 { return f[u32](-1) }\n\
     fn main() i32 { return 0 }\n"
  in
  literal_render "literal-generic-argument-unsigned-negative-one" generic_call
    (expected_diagnostic "fn g() u32 { return f[u32](-1) }"
       (String.index "fn g() u32 { return f[u32](-1) }" '-' + 1)
       ~line_number:2 1 "integer literal is out of range for u32: `-1`"
       (unsigned_help "0xFFFFFFFF"));
  literal_render "literal-vector-lane-unsigned-negative-one"
    "fn f() vec[4,u32] { v vec[4,u32] = {-1, 0, 0, 0}\n return v }\n"
    (expected_diagnostic "fn f() vec[4,u32] { v vec[4,u32] = {-1, 0, 0, 0}" 37 1
       "integer literal is out of range for u32: `-1`" (unsigned_help "0xFFFFFFFF"));
  literal_render "literal-bool-one" "fn f() bool { b bool = 1\n return b }\n"
    (expected_diagnostic_without_help "fn f() bool { b bool = 1" 24 1 not_bool);
  literal_render "literal-bool-zero" "fn f() bool { b bool = 0\n return b }\n"
    (expected_diagnostic_without_help "fn f() bool { b bool = 0" 24 1 not_bool);
  literal_render "literal-bool-vector-lane"
    "fn f() vec[4,bool] { v vec[4,bool] = {1, 0, 0, 0}\n return v }\n"
    (expected_diagnostic_without_help
       "fn f() vec[4,bool] { v vec[4,bool] = {1, 0, 0, 0}" 39 1 not_bool);
  literal_render "literal-const-bool-one" "const B bool = 1\nfn f() i32 { return 0 }\n"
    (expected_diagnostic_without_help "const B bool = 1" 16 1 not_bool);
  semantic_accept "literal-unsigned-negative-zero"
    "fn f() u32 { x u32 = -0\n return x }\n";
  semantic_accept "literal-signed-negative-min" "fn f() i8 { x i8 = -128\n return x }\n";
  semantic_accept "literal-signed-negative-one" "fn f() i32 { x i32 = -1\n return x }\n";
  semantic_accept "literal-bitcast-negative-one"
    "fn f() u32 { return bitcast[u32](-1) }\n"

let replay_unmatched_bound = 30

let () =
  let unmatched = List.rev !replay_unmatched in
  let failures = List.rev !replay_failures in
  let unmatched_count = List.length unmatched in
  Printf.printf
    "help replay: %d replayed, %d template, %d prose, %d unmatched (bound %d)\n"
    !replay_replayed !replay_template !replay_prose unmatched_count
    replay_unmatched_bound;
  if unmatched_count > replay_unmatched_bound then
    List.iter (Printf.printf "help replay unmatched: %s\n") unmatched;
  List.iter (Printf.printf "help replay failed: %s\n") failures;
  if unmatched_count > replay_unmatched_bound || failures <> [] then
    failwith "help replay failed"

let () = Printf.printf "all regression checks: %d passed\n" !checks_run
