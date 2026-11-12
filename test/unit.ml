(* These tests run against the hidden LLVM test oracle (removal A3); the
   native-default CLI is checked in regressions.ml with the variable unset. *)
let () = Unix.putenv "FAS_ORACLE" "llvm"

let source text = Source.create ~file:"test.fas" ~text

let expect_ok = function
  | Ok value -> value
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics)

let contains text needle =
  let n = String.length text and m = String.length needle in
  let rec go i = i + m <= n && (String.sub text i m = needle || go (i + 1)) in
  m = 0 || go 0

(* An expected text kept in test/[name]. With FAS_UNIT_BLESS=<dir> the
   actual text is written to <dir>/[name] instead (copy it into test/ after
   reviewing it) *)
let expected_file name actual =
  match Sys.getenv_opt "FAS_UNIT_BLESS" with
  | Some dir ->
      let channel = open_out_bin (Filename.concat dir name) in
      output_string channel actual;
      close_out channel;
      actual
  | None ->
      let channel = open_in_bin name in
      let text = really_input_string channel (in_channel_length channel) in
      close_in channel;
      text

(* Small Fir modules built through Lower_emit, the way lower.ml builds them;
   [lay] gives named structs' (size, align) *)
let fir_data_module ?(lay = []) add =
  let table : Lower_emit.layouts = Hashtbl.create 4 in
  List.iter (fun (name, l) -> Hashtbl.replace table name l) lay;
  let mc = Lower_emit.create_module ~triple:(Target_layout.current ()).triple table in
  add mc;
  mc.Lower_emit.m

let fir_bytes_global mc name bytes =
  let g = Lower_emit.base_global name (String.length bytes) 1 in
  g.Fir.init <- Some [ Fir.Bytes bytes ];
  Fir.add_global mc.Lower_emit.m g

(* a void function [name] whose body [body fn] must end with a terminator *)
let fir_void_function mc name body =
  let fn =
    Lower_emit.start mc ~name ~linkage:Fir.Global ~params:[] ~ret:Low.Void
      ~ret_extension:Low.No_extension ~variadic:false ~display:None ~source:None
  in
  body fn;
  match Lower_emit.finish fn with Ok () -> () | Error b -> failwith (Printf.sprintf "b%d open" b)

let () =
  assert (Limits.budget_version_name Limits.default_budget_version = "0.1.5");
  assert (Limits.for_budget_version Limits.V0_15 = Limits.default);
  assert (Limits.default.max_tokens = 1_000_000);
  assert (Limits.default.max_nesting = 128);
  assert (Limits.default.max_interned_string_bytes = 4_000_000);
  assert (Limits.default.max_rendered_ir_bytes = 32_000_000);
  assert (Limits.default.max_specializations = 10_000);
  assert (Limits.default.max_specialization_depth = 64);
  assert (Limits.default.max_aggregate_elements = 1_000_000);
  assert (Limits.default.max_object_alignment = 1_048_576);
  assert (Limits.default.max_object_size = 1_073_741_824);
  assert (Limits.default.max_use_files = 10_000);
  assert (Limits.default.max_use_bytes = 1_073_741_824);
  let handle_view_type = Ast.Handle (Ast.Named_type ("Opaque", Span.synthetic)) in
  assert (
    Sema_substitution.view_source_type (Some handle_view_type) = Some handle_view_type);
  assert (Target_layout.pointer_integer_bits (Target_layout.current ()) = Ok 64);
  assert (
    Sema_types.source_ty [] (Ast.Applied_type ("G", Span.synthetic, [], Span.synthetic))
    = Error "internal error: generic type application reached ordinary type checking");
  assert (
    Target_layout.pointer_integer_bits
      { (Target_layout.current ()) with pointer_size = 4; pointer_align = 4 }
    = Ok 32);
  assert (
    Target_layout.pointer_integer_bits
      { (Target_layout.current ()) with pointer_size = 16; pointer_align = 16 }
    = Error "unsupported pointer size: 16");
  let layout_declarations =
    [
      ("Leaf", [ ("value", Hir.Int Hir.U8) ], None);
      ("Left", [ ("leaf", Hir.Struct "Leaf") ], None);
      ("Right", [ ("leaf", Hir.Struct "Leaf") ], None);
      ("Root", [ ("left", Hir.Struct "Left"); ("right", Hir.Struct "Right") ], None);
    ]
  in
  let layout_cache = Hir.struct_layout_cache layout_declarations in
  let root_layout =
    match Hir.compute_struct_cached layout_cache "Root" with
    | Ok definition -> definition
    | Error message -> failwith message
  in
  assert (root_layout.size = 2);
  assert (Hir.Hashtbl.length layout_cache.definitions = 4);
  ignore
    (match Hir.compute_struct_cached layout_cache "Root" with
    | Ok definition -> definition
    | Error message -> failwith message);
  assert (Hir.Hashtbl.length layout_cache.definitions = 4);
  let pointer_declarations = [ ("Address", [ ("value", Hir.Addr) ], None) ] in
  let target32 =
    { (Target_layout.current ()) with pointer_size = 4; pointer_align = 4 }
  in
  let pointer64 =
    match
      Hir.compute_struct_cached (Hir.struct_layout_cache pointer_declarations) "Address"
    with
    | Ok definition -> definition
    | Error message -> failwith message
  in
  let pointer32 =
    match
      Hir.compute_struct_cached
        (Hir.struct_layout_cache ~target:target32 pointer_declarations)
        "Address"
    with
    | Ok definition -> definition
    | Error message -> failwith message
  in
  assert (pointer64.size = 8);
  assert (pointer32.size = 4);
  let recursive_declarations =
    [
      ("First", [ ("second", Hir.Struct "Second") ], None);
      ("Second", [ ("first", Hir.Struct "First") ], None);
    ]
  in
  let recursive_cache = Hir.struct_layout_cache recursive_declarations in
  (match Hir.compute_struct_cached recursive_cache "First" with
  | Error message -> assert (message = "recursive by-value struct `First`")
  | Ok _ -> assert false);
  assert (Hir.Hashtbl.length recursive_cache.definitions = 0);
  let overflowing_declarations =
    [
      ( "Overflowing",
        [ ("large", Hir.Array (max_int, Hir.Int Hir.U8)); ("tail", Hir.Int Hir.U8) ],
        None );
    ]
  in
  let overflowing_cache = Hir.struct_layout_cache overflowing_declarations in
  (match Hir.compute_struct_cached overflowing_cache "Overflowing" with
  | Error message -> assert (message = "aggregate size overflows")
  | Ok _ -> assert false);
  assert (Hir.Hashtbl.length overflowing_cache.definitions = 0);
  let overflowing_program =
    expect_ok
      (Parser.parse
         (source
            (Printf.sprintf
               "struct Overflowing { large arr[%d,u8] tail u8 }\n\
                fn f() void { return }\n"
               max_int)))
  in
  (match
     Sema.check
       ~limits:
         {
           Limits.default with
           max_aggregate_elements = max_int;
           max_object_size = max_int;
         }
       overflowing_program
   with
  | Error diagnostics ->
      assert (
        contains (Diag.render_all ~source:None diagnostics) "aggregate size overflows")
  | Ok _ -> assert false);
  let padding_overflow_declarations =
    [
      ( "PaddingOverflow",
        [ ("large", Hir.Array (max_int - 1, Hir.Int Hir.U8)) ],
        Some (1 lsl 31) );
    ]
  in
  let padding_overflow_cache = Hir.struct_layout_cache padding_overflow_declarations in
  (match Hir.compute_struct_cached padding_overflow_cache "PaddingOverflow" with
  | Error message -> assert (message = "aggregate size overflows")
  | Ok _ -> assert false);
  assert (Hir.Hashtbl.length padding_overflow_cache.definitions = 0);
  let object_size_program =
    expect_ok (Parser.parse (source "fn f() void { value arr[16,u8]\n return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_object_size = 16 }
          object_size_program));
  (match
     Sema.check ~limits:{ Limits.default with max_object_size = 15 } object_size_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "object size exceeds budget max_object_size of 15 (profile 0.1.5)")
  | Ok _ -> assert false);
  let generic_object_size_program =
    expect_ok
      (Parser.parse
         (source
            "struct Box[T] { value T }\n\
             fn f() void { value Box[arr[16,u8]]\n\
            \ return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_object_size = 16 }
          generic_object_size_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_object_size = 15 }
       generic_object_size_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "object size exceeds budget max_object_size of 15 (profile 0.1.5)")
  | Ok _ -> assert false);
  let aligned_program =
    expect_ok
      (Parser.parse
         (source "struct Aligned @align(16) { value u8 }\nfn f() void { return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_object_alignment = 16 }
          aligned_program));
  (match
     Sema.check ~limits:{ Limits.default with max_object_alignment = 8 } aligned_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "alignment exceeds budget max_object_alignment of 8 (profile 0.1.5)")
  | Ok _ -> assert false);
  let target_alignment_program =
    expect_ok (Parser.parse (source "struct Invalid @align(4294967296) { value u8 }\n"))
  in
  (match
     Sema.check
       ~limits:{ Limits.default with max_object_alignment = 8 }
       target_alignment_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "alignment exceeds target maximum of 2147483648")
  | Ok _ -> assert false);
  let natural_alignment_program =
    expect_ok (Parser.parse (source "fn f() void { value vec[16,u8]\n return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_object_alignment = 16 }
          natural_alignment_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_object_alignment = 8 }
       natural_alignment_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "alignment exceeds budget max_object_alignment of 8 (profile 0.1.5)")
  | Ok _ -> assert false);
  let generic_alignment_program =
    expect_ok
      (Parser.parse
         (source
            "struct Box[T] { value T }\n\
             fn f() void { value Box[vec[16,u8]]\n\
            \ return }\n"))
  in
  (match
     Sema.check
       ~limits:{ Limits.default with max_object_alignment = 8 }
       generic_alignment_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "alignment exceeds budget max_object_alignment of 8 (profile 0.1.5)")
  | Ok _ -> assert false);
  let aggregate_budget_program =
    expect_ok
      (Parser.parse
         (source
            "struct Pair { left arr[3,u8] right arr[3,u8] }\n\
             fn f() void { value Pair\n\
            \ return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_aggregate_elements = 6 }
          aggregate_budget_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_aggregate_elements = 5 }
       aggregate_budget_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "aggregate element count exceeds the configured limit")
  | Ok _ -> assert false);
  let aggregate_substitution_program =
    expect_ok
      (Parser.parse
         (source
            "struct Pair[T] { left T right T }\n\
             fn f() void { value Pair[arr[3,u8]]\n\
            \ return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_aggregate_elements = 6 }
          aggregate_substitution_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_aggregate_elements = 5 }
       aggregate_substitution_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "aggregate element count exceeds the configured limit")
  | Ok _ -> assert false);
  let nested_vector_budget_program =
    expect_ok
      (Parser.parse
         (source
            "struct Wrapped { values arr[2,vec[3,u8]] }\n\
             fn f() void { value Wrapped\n\
            \ return }\n"))
  in
  ignore
    (expect_ok
       (Sema.check
          ~limits:{ Limits.default with max_aggregate_elements = 6 }
          nested_vector_budget_program));
  (match
     Sema.check
       ~limits:{ Limits.default with max_aggregate_elements = 5 }
       nested_vector_budget_program
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "aggregate element count exceeds the configured limit")
  | Ok _ -> assert false);
  let diagnostic_source = Source.create ~file:"first.fas" ~text:"wrong line\n" in
  let foreign_span =
    Span.make ~file:"second.fas" ~start_offset:0 ~end_offset:1 ~line:1 ~column:1
  in
  let foreign_diagnostic = Diag.error foreign_span "failure" in
  let rendered = Diag.render ~source:(Some diagnostic_source) foreign_diagnostic in
  assert (not (contains rendered "wrong line"));
  let mapped_source = Source.create ~file:"mapped.fas" ~text:"head\n  bad\n" in
  let mapped_span =
    Span.make ~file:"mapped.fas" ~start_offset:0 ~end_offset:0 ~line:2 ~column:3
  in
  assert (
    Diag.render ~source:(Some mapped_source) (Diag.error mapped_span "failure")
    = "mapped.fas:2:3: error: failure\n    bad\n    ^\n");
  let tokens = expect_ok (Lexer.lex (source "fn main() i64 { return 0x2a + 1\n }\n")) in
  assert (List.exists (fun token -> token.Token.kind = Token.Kw_fn) tokens);
  assert (List.exists (fun token -> token.Token.kind = Token.Int "0x2a") tokens);
  List.iter
    (fun literal ->
      match Lexer.lex (source literal) with
      | Error [ diagnostic ] ->
          assert (diagnostic.Diag.message = "misplaced digit separator `_`")
      | _ -> failwith ("integer separator should be rejected: " ^ literal))
    [ "0_x10"; "0x_FF"; "0b_1"; "1_"; "1__2"; "0o_7" ];
  List.iter
    (fun (literal, expected) ->
      match Lexer.lex (source literal) with
      | Ok [ { Token.kind = Token.Int actual; _ }; { Token.kind = Token.Eof; _ } ] ->
          assert (actual = expected)
      | _ -> failwith ("integer separator should be accepted: " ^ literal))
    [ ("1_000", "1000"); ("0x0100_0193", "0x01000193"); ("0b1010_0000", "0b10100000") ];
  (match Lexer.lex (source "0x") with
  | Ok _ -> assert false
  | Error diagnostics ->
      let rendered = Diag.render_all ~source:None diagnostics in
      assert (contains rendered "invalid integer literal");
      assert (not (contains rendered "invalid digit in integer literal")));
  let program =
    expect_ok
      (Parser.parse
         (source
            "struct S { x i64 }\n\
             const C i64 = 4\n\
             fn f(a i64) i64 { y i64 = a + C\n\
            \ return y }\n"))
  in
  assert (List.length program.Ast.items = 3);
  let typed = expect_ok (Sema.check program) in
  assert (List.length typed.Hir.structs = 1 && List.length typed.Hir.funcs = 1);
  let bad = expect_ok (Parser.parse (source "fn bad() i64 { x i64\n return x }\n")) in
  (match Sema.check bad with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let opaque =
    expect_ok
      (Parser.parse
         (source "opaque Ctx\nfn bad(p handle[Ctx]) i64 { return p[u64] }\n"))
  in
  (match Sema.check opaque with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let generic =
    expect_ok
      (Parser.parse
         (source
            "fn id[N const usize](x u64) u64 { return x + bitcast[u64](N) }\n\
             fn test() u64 { return id[3](2) }\n"))
  in
  let specialized = expect_ok (Sema.check generic) in
  assert (List.length specialized.Hir.funcs = 2);
  let lowered = expect_ok (Lower.lower specialized) in
  assert (Fir.Vec.length lowered.Fir.funcs = 2);
  (* lowering and rendering are deterministic *)
  assert (
    Fir_llvm.render lowered = Fir_llvm.render (expect_ok (Lower.lower specialized)));
  (* Hand-built Fir modules, made through Lower_emit as lower.ml makes
     them. (The old Ir's validator tests went with it in v0.6; Fir_verify
     has its own in fir_test.ml.) *)
  let fir_mc () =
    Lower_emit.create_module ~triple:(Target_layout.current ()).triple (Hashtbl.create 1)
  in
  let fir_fn ?(params = []) ?(ret = Low.Void) ?source mc name =
    Lower_emit.start mc ~name ~linkage:Fir.Local ~params ~ret
      ~ret_extension:Low.No_extension ~variadic:false ~display:None ~source
  in
  let fir_finish fn = assert (Lower_emit.finish fn = Ok ()) in
  let fir_module (mc : Lower_emit.modctx) =
    assert (Fir_verify.verify mc.m = []);
    mc.m
  in
  let fir_declare ?(params = []) ?(ret = Low.Void) ?(ret_extension = Low.No_extension)
      ?(variadic = false) mc name =
    Lower_emit.declare mc ~name ~params ~ret ~ret_extension ~variadic ~display:None;
    if variadic then Hashtbl.replace mc.Lower_emit.variadic name (List.length params)
  in
  (* a function that only returns *)
  let fir_void mc name =
    let fn = fir_fn mc name in
    Lower_emit.ret fn None;
    fir_finish fn
  in
  (* C ABI extensions and variadic calls in the rendered text *)
  let call_module ~variadic args =
    let mc = fir_mc () in
    fir_declare mc "callee" ~params:[ (Low.I8, Low.Zero_extension) ] ~ret:Low.I16
      ~ret_extension:Low.Sign_extension ~variadic;
    let fn = fir_fn mc "caller" in
    Lower_emit.call fn (Some 0) Low.Sign_extension Low.I16 "callee" args;
    Lower_emit.ret fn None;
    fir_finish fn;
    Fir_llvm.render (fir_module mc)
  in
  let seven = (Low.I8, Low.Zero_extension, Low.Const (Low.I8, 7L)) in
  let valid_call_text =
    call_module ~variadic:true [ seven; (Low.I64, Low.No_extension, Low.Const (Low.I64, 9L)) ]
  in
  assert (contains valid_call_text "call signext i16 (i8 zeroext, ...) @callee(i8 zeroext 7, i64 9)");
  let fixed_call_text = call_module ~variadic:false [ seven ] in
  assert (contains fixed_call_text "call signext i16 @callee(i8 zeroext 7)");
  let unresolved_template_call =
    {
      Hir.structs = [];
      consts = [];
      const_arrays = [];
      globals = [];
      funcs =
        [
          {
            Hir.name = "main";
            params = [];
            ret = Hir.Int Hir.I64;
            body =
              Hir.Statements
                [
                  Hir.Return
                    ( Some
                        (Hir.Call
                           (Hir.User "identity", [], Hir.Int Hir.I64, Span.synthetic)),
                      Span.synthetic );
                ];
            linkage = Hir.Internal;
            variadic = false;
            span = Span.synthetic;
            display = None;
          };
        ];
      strings = [];
    }
  in
  (match Lower.lower unresolved_template_call with
  | Ok _ -> assert false
  | Error [ diagnostic ] ->
      assert (diagnostic.message = "unknown function `identity` reached lowering")
  | Error _ -> assert false);
  let golden_source = source "fn test() i64 { x i64 = 2\n return x + 3\n }\n" in
  let golden_ast = expect_ok (Parser.parse golden_source) in
  let golden_hir = expect_ok (Sema.check golden_ast) in
  let golden_ir = Fir_llvm.render (expect_ok (Lower.lower golden_hir)) in
  assert (golden_ir = expected_file "ir_simple.expected" golden_ir);
  (match Process.run [| "/bin/printf"; "process-ok" |] with
  | Ok (out, _) -> assert (out = "process-ok")
  | Error _ -> assert false);
  let builtin =
    expect_ok (Parser.parse (source "fn f(p addr) usize { return addr_bits(p) }\n"))
  in
  assert (List.length builtin.Ast.items = 1);
  let rendered_module =
    let parsed =
      expect_ok
        (Parser.parse
           (source "fn test() i64 { s addr = \"a\\n\\t\\\\\\\"z\\0\"\n return 0\n }\n"))
    in
    let hir = expect_ok (Sema.check parsed) in
    expect_ok (Lower.lower hir)
  in
  let rendered_text = Fir_llvm.render rendered_module in
  let rendered_bytes = String.length rendered_text in
  assert (contains rendered_text "c\"a\\0A\\09\\5C\\22z\\00\"");
  let expect_render_ok budget =
    match Fir_llvm.render_bounded ~budget rendered_module with
    | Ok text -> assert (text = rendered_text)
    | Error _ -> assert false
  in
  let expect_render_error budget =
    match Fir_llvm.render_bounded ~budget rendered_module with
    | Error message ->
        assert (
          message
          = Printf.sprintf "rendered LLVM text exceeds the configured limit of %d bytes"
              budget)
    | Ok _ -> assert false
  in
  expect_render_ok rendered_bytes;
  expect_render_error (rendered_bytes - 1);
  expect_render_error 0;
  expect_render_ok rendered_bytes;
  assert (
    Fir_llvm.render_bounded ~budget:rendered_bytes rendered_module
    = Fir_llvm.render_bounded ~budget:rendered_bytes rendered_module);
  (match Fir_llvm.render_bounded ~budget:(-1) rendered_module with
  | Error message -> assert (message = "rendered LLVM text budget must not be negative")
  | Ok _ -> assert false);
  (match Fir_llvm.render_bounded ~budget:min_int rendered_module with
  | Error message -> assert (message = "rendered LLVM text budget must not be negative")
  | Ok _ -> assert false);
  let multi_block =
    let mc = fir_mc () in
    let fn = fir_fn mc "flow" ~ret:Low.I32 in
    let b1 = Lower_emit.new_block fn and b2 = Lower_emit.new_block fn in
    Lower_emit.cond_br fn (Low.Const (Low.I1, 1L)) b1 b2;
    Lower_emit.enter fn b1 None;
    Lower_emit.br fn b2;
    Lower_emit.enter fn b2 None;
    Lower_emit.ret fn (Some (Low.I32, Low.Const (Low.I32, 0L)));
    fir_finish fn;
    fir_module mc
  in
  let multi_block_expected =
    expected_file "fir_llvm_multi_block.expected" (Fir_llvm.render multi_block)
  in
  assert (Fir_llvm.render multi_block = multi_block_expected);
  assert (
    Fir_llvm.render_bounded ~budget:(String.length multi_block_expected) multi_block
    = Ok multi_block_expected);
  let verify_path = Filename.temp_file "fas-render-verify-" ".ll" in
  Fun.protect
    ~finally:(fun () -> Sys.remove verify_path)
    (fun () ->
      let channel = open_out_bin verify_path in
      output_string channel multi_block_expected;
      close_out channel;
      match
        Process.run [| "opt-22"; "-passes=verify"; "-disable-output"; verify_path |]
      with
      | Ok _ -> ()
      | Error failure ->
          failwith
            ("opt-22 rejected rendered multi-block LLVM: " ^ failure.Process.stderr));
  let debug_marked = Debug_fixture.module_ ~markers:true in
  let debug_plain = Debug_fixture.module_ ~markers:false in
  assert (Fir_verify.verify debug_marked = []);
  assert (Fir_llvm.render debug_marked = Fir_llvm.render debug_plain);
  assert (Fir_llvm.render ~debug_info:false debug_marked = Fir_llvm.render debug_plain);
  assert (Fir_limits.count_lowered_nodes debug_marked = Fir_limits.count_lowered_nodes debug_plain);
  let nodes = Fir_limits.count_lowered_nodes debug_plain in
  let node_budget n = { Limits.default with max_ir_nodes = n } in
  assert (Fir_limits.check_lowered_nodes ~limits:(node_budget nodes) debug_marked = Ok ());
  assert (Fir_limits.check_lowered_nodes ~limits:(node_budget nodes) debug_plain = Ok ());
  assert (
    Result.is_error
      (Fir_limits.check_lowered_nodes ~limits:(node_budget (nodes - 1)) debug_marked));
  let debug_expected =
    expected_file "fir_llvm_debug_info.expected"
      (Fir_llvm.render ~debug_info:true debug_marked)
  in
  assert (Fir_llvm.render ~debug_info:true debug_marked = debug_expected);
  let debug_counted = String.length (Fir_llvm.render debug_marked) in
  assert (debug_counted < String.length debug_expected);
  assert (
    Fir_llvm.render_bounded ~debug_info:true ~budget:debug_counted debug_marked
    = Ok debug_expected);
  (match
     Fir_llvm.render_bounded ~debug_info:true ~budget:(debug_counted - 1) debug_marked
   with
  | Error message ->
      assert (
        message
        = Printf.sprintf "rendered LLVM text exceeds the configured limit of %d bytes"
            (debug_counted - 1))
  | Ok _ -> assert false);
  (match
     Fir_llvm.render_bounded ~debug_info:true
       ~redirect:(fun name -> name ^ "_sym")
       ~budget:max_int debug_marked
   with
  | Ok text -> assert (contains text "linkageName: \"helper_sym\"")
  | Error _ -> assert false);
  let relative_module =
    let mc = fir_mc () in
    let fn = fir_fn mc "control_flow" ~source:("rel/x.fas", 1) in
    Lower_emit.ret fn None;
    fir_finish fn;
    fir_module mc
  in
  assert (
    contains
      (Fir_llvm.render ~debug_info:true relative_module)
      (Printf.sprintf "!DIFile(filename: \"x.fas\", directory: \"%s/rel\")"
         (Sys.getcwd ())));
  let wide_module =
    let mc = fir_mc () in
    fir_declare mc "wide" ~params:(List.init 100_000 (fun _ -> (Low.I8, Low.No_extension)));
    let fn = fir_fn mc "caller" in
    Lower_emit.call fn None Low.No_extension Low.Void "wide"
      (List.init 100_000 (fun _ -> (Low.I8, Low.No_extension, Low.Const (Low.I8, 1L))));
    Lower_emit.ret fn None;
    fir_finish fn;
    fir_module mc
  in
  (match Fir_llvm.render_bounded ~budget:1024 wide_module with
  | Error message ->
      assert (message = "rendered LLVM text exceeds the configured limit of 1024 bytes")
  | Ok _ -> assert false);
  let wide_text = Fir_llvm.render wide_module in
  (match Fir_llvm.render_bounded ~budget:(String.length wide_text) wide_module with
  | Ok text -> assert (text = wide_text)
  | Error _ -> assert false);
  let expect_budget_rejection module_ =
    match Fir_llvm.render_bounded ~budget:256 module_ with
    | Error message ->
        assert (message = "rendered LLVM text exceeds the configured limit of 256 bytes")
    | Ok _ -> assert false
  in
  let one_function name =
    let mc = fir_mc () in
    fir_void mc name;
    fir_module mc
  in
  expect_budget_rejection (one_function (String.make 1_000_000 'a'));
  let long_plain_global =
    let mc = fir_mc () in
    Lower_emit.string_global mc 0 (String.make 1_000_000 'A');
    fir_void mc "control_flow";
    fir_module mc
  in
  expect_budget_rejection long_plain_global;
  (* LLVM's escapes in a quoted name: \0A for a newline *)
  assert (contains (Fir_llvm.render (one_function "a\nb")) "@\"a\\0Ab\"");
  expect_budget_rejection (one_function (String.make 1_000_000 '\n'));
  let ast_source text = Source.create ~file:"ast.fas" ~text in
  let ast_expected = "fn f(a i64) i64 {\n  y i64 = a + 1\n  return y\n}\n" in
  let ast_program =
    expect_ok
      (Parser.parse (ast_source "fn f(a i64) i64 { y i64 = a + 1\n return y }\n"))
  in
  assert (Ast.render_program ast_program = ast_expected);
  let func_string_ids (func : Hir.func) =
    let from_expr = function Hir.EString (id, _, _) -> [ id ] | _ -> [] in
    match func.Hir.body with
    | Hir.Statements statements ->
        List.concat_map
          (function
            | Hir.Let (_, Some value, _) -> from_expr value
            | Hir.Assign (_, value, _) -> from_expr value
            | Hir.Return (Some value, _) -> from_expr value
            | Hir.Expr (value, _) -> from_expr value
            | _ -> [])
          statements
    | _ -> []
  in
  let expect_budget_error name ~line ~column ~message ~notes outcome =
    match outcome with
    | Error [ diagnostic ] ->
        assert (diagnostic.Diag.message = message);
        assert (diagnostic.Diag.primary.Span.file = "test.fas");
        assert (diagnostic.Diag.primary.Span.line = line);
        assert (diagnostic.Diag.primary.Span.column = column);
        assert (diagnostic.Diag.notes = notes)
    | _ -> failwith (name ^ ": expected a single budget diagnostic")
  in
  let string_literal_program =
    expect_ok (Parser.parse (source "fn f() void { s addr = \"abc\"\n return }\n"))
  in
  expect_budget_error "string single" ~line:1 ~column:24
    ~message:
      "string literal bytes exceed budget max_interned_string_bytes of 2 (profile \
       0.1.5)"
    ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 2 }
       string_literal_program);
  let boundary_strings =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 3 }
         string_literal_program)
  in
  assert (boundary_strings.Hir.strings = [ "abc" ]);
  let c_string_program =
    expect_ok (Parser.parse (source "fn f() void { s addr = c\"abc\"\n return }\n"))
  in
  expect_budget_error "c-string single" ~line:1 ~column:24
    ~message:
      "string literal bytes exceed budget max_interned_string_bytes of 3 (profile \
       0.1.5)"
    ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 3 }
       c_string_program);
  let nul_charged_strings =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 4 }
         c_string_program)
  in
  assert (nul_charged_strings.Hir.strings = [ "abc\000" ]);
  let duplicated_strings =
    expect_ok
      (Parser.parse
         (source "fn f() void { a addr = \"abc\"\n b addr = \"abc\"\n return }\n"))
  in
  let deduplicated =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 3 }
         duplicated_strings)
  in
  assert (deduplicated.Hir.strings = [ "abc" ]);
  let function_named name hir =
    List.find (fun (func : Hir.func) -> func.Hir.name = name) hir.Hir.funcs
  in
  assert (func_string_ids (function_named "f" deduplicated) = [ 0; 0 ]);
  let cross_function_strings =
    expect_ok
      (Parser.parse
         (source
            "fn f() void { a addr = \"ab\"\n\
            \ return }\n\
             fn g() void { b addr = \"ab\"\n\
            \ c addr = \"cd\"\n\
            \ return }\n"))
  in
  expect_budget_error "cross-function cumulative" ~line:4 ~column:11
    ~message:
      "cumulative interned string bytes exceed budget max_interned_string_bytes of 3 \
       (profile 0.1.5)"
    ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 3 }
       cross_function_strings);
  let shared_strings =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 4 }
         cross_function_strings)
  in
  assert (shared_strings.Hir.strings = [ "ab"; "cd" ]);
  assert (func_string_ids (function_named "f" shared_strings) = [ 0 ]);
  assert (func_string_ids (function_named "g" shared_strings) = [ 0; 1 ]);
  let ordering_strings =
    expect_ok
      (Parser.parse
         (source
            "fn f() void { a addr = \"ab\"\n\
            \ return }\n\
             fn g() void { b addr = \"cde\"\n\
            \ return }\n"))
  in
  expect_budget_error "ordering cumulative" ~line:3 ~column:24
    ~message:
      "cumulative interned string bytes exceed budget max_interned_string_bytes of 4 \
       (profile 0.1.5)"
    ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 4 }
       ordering_strings);
  let ordered_strings =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 5 }
         ordering_strings)
  in
  assert (ordered_strings.Hir.strings = [ "ab"; "cde" ]);
  assert (func_string_ids (function_named "f" ordered_strings) = [ 0 ]);
  assert (func_string_ids (function_named "g" ordered_strings) = [ 1 ]);
  let specialized_strings =
    expect_ok
      (Parser.parse
         (source
            "fn echo[T](v T) addr { return \"abc\" }\n\
             fn test() void { a addr = echo[u8](1)\n\
            \ b addr = echo[i64](2)\n\
            \ return }\n"))
  in
  let specialization_shared =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 3 }
         specialized_strings)
  in
  assert (specialization_shared.Hir.strings = [ "abc" ]);
  let specializations =
    List.filter
      (fun (func : Hir.func) -> contains func.Hir.name "$spec$")
      specialization_shared.Hir.funcs
  in
  assert (List.length specializations = 2);
  assert (List.for_all (fun func -> func_string_ids func = [ 0 ]) specializations);
  let legality_single_strings =
    expect_ok
      (Parser.parse
         (source
            "fn big[N const u64](v addr) addr { return \"toolongstr\" }\n\
             fn test(v addr) void { d addr = big[1](v)\n\
            \ return }\n"))
  in
  expect_budget_error "legality single" ~line:1 ~column:43
    ~message:
      "string literal bytes exceed budget max_interned_string_bytes of 8 (profile \
       0.1.5)"
    ~notes:[ "while instantiating `big[1]` at test.fas:2:36" ]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 8 }
       legality_single_strings);
  let legality_cumulative_strings =
    expect_ok
      (Parser.parse
         (source
            "fn pair[N const u64](v addr) addr {\n\
            \ a addr = \"aa\"\n\
            \ b addr = \"bb\"\n\
            \ return v }\n\
             fn test(v addr) void { d addr = pair[1](v)\n\
            \ return }\n"))
  in
  expect_budget_error "legality cumulative" ~line:3 ~column:11
    ~message:
      "cumulative interned string bytes exceed budget max_interned_string_bytes of 3 \
       (profile 0.1.5)"
    ~notes:[ "while instantiating `pair[1]` at test.fas:5:37" ]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 3 }
       legality_cumulative_strings);
  expect_budget_error "fresh state failure repeat" ~line:4 ~column:11
    ~message:
      "cumulative interned string bytes exceed budget max_interned_string_bytes of 3 \
       (profile 0.1.5)"
    ~notes:[]
    (Sema.check
       ~limits:{ Limits.default with max_interned_string_bytes = 3 }
       cross_function_strings);
  let refreshed =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 5 }
         ordering_strings)
  in
  assert (refreshed.Hir.strings = [ "ab"; "cde" ]);
  let after_failure =
    expect_ok
      (Sema.check
         ~limits:{ Limits.default with max_interned_string_bytes = 4 }
         cross_function_strings)
  in
  assert (after_failure.Hir.strings = [ "ab"; "cd" ]);
  (match Lexer.lex (source "/* unterminated") with
  | Ok _ -> assert false
  | Error _ -> ());
  (match Parser.parse (source "fn broken( i64) void {}") with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  (match Parser.parse (source "const K arr[2, u8] = { 1 2 }\n") with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let overflow =
    expect_ok (Parser.parse (source "const X u64 = 18446744073709551616\n"))
  in
  (match Sema.check overflow with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let divzero = expect_ok (Parser.parse (source "const X i64 = 1 / 0\n")) in
  (match Sema.check divzero with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let bad_cmp =
    expect_ok (Parser.parse (source "fn bad() bool { return true < false }\n"))
  in
  (match Sema.check bad_cmp with
  | Ok _ -> assert false
  | Error diagnostics -> assert (diagnostics <> []));
  let expect_cli = function
    | Ok (Cli.Run value) -> value
    | Ok Cli.Help -> failwith "expected compiler invocation"
    | Ok Cli.Version -> failwith "expected compiler invocation"
    | Ok (Cli.Run_program _) -> failwith "expected compiler invocation"
    | Ok (Cli.Check _) -> failwith "expected compiler invocation"
    | Ok (Cli.Lsp _) -> failwith "expected compiler invocation"
    | Error message -> failwith message
  in
  let cli = expect_cli (Cli.parse [| "fas"; "--emit-ir"; "-o"; "out"; "one.fas" |]) in
  assert (
    cli.Cli.emit = Cli.Ir && cli.output = "out" && cli.output_explicit
    && cli.input = "one.fas" && cli.link_inputs = []);
  let cli_links =
    expect_cli
      (Cli.parse [| "fas"; "-L"; "lib"; "prog.fas"; "helper.c"; "-lm"; "obj.o" |])
  in
  assert (
    cli_links.Cli.input = "prog.fas"
    && cli_links.link_inputs = [ "-L"; "lib"; "helper.c"; "-lm"; "obj.o" ]);
  let cli_sanitizers =
    expect_cli
      (Cli.parse
         [|
           "fas";
           "--sanitize=undefined,undefined";
           "--sanitize=undefined";
           "prog.fas";
         |])
  in
  assert (cli_sanitizers.Cli.sanitizers = [ "undefined" ]);
  (* removal A6: trap mode, no runtime *)
  assert (
    Cli.c_sanitizer_flags cli_sanitizers
    = [ "-fsanitize=undefined"; "-fsanitize-trap=all" ]);
  List.iter
    (fun mode ->
      let cli =
        expect_cli
          (Cli.parse
             (Array.of_list [ "fas"; "--sanitize=undefined"; mode; "prog.fas" ]))
      in
      assert (cli.Cli.sanitizers = [ "undefined" ]))
    [ "--emit-header"; "--emit-ir"; "--emit-llvm"; "-S"; "-c" ];
  List.iter
    (fun (option, message) ->
      match Cli.parse [| "fas"; option; "prog.fas" |] with
      | Error actual -> assert (actual = message)
      | Ok _ -> assert false)
    [
      ("--sanitize=", "--sanitize requires a non-empty list");
      ("--sanitize=memory", "unknown sanitizer `memory`");
      ("--sanitize=thread", "unknown sanitizer `thread`");
      ("--sanitize=foo", "unknown sanitizer `foo`");
      ( "--sanitize=address",
        "`--sanitize=address` was removed; it needed LLVM's AddressSanitizer runtime" );
      ( "--sanitize=undefined,address",
        "`--sanitize=address` was removed; it needed LLVM's AddressSanitizer runtime" );
    ];
  let cli_c_flags =
    expect_cli
      (Cli.parse
         [|
           "fas";
           "-I";
           "include dir";
           "-Isecond";
           "-isystem";
           "system dir";
           "-isystemthird";
           "-D";
           "FEATURE=7";
           "-DOTHER=9";
           "prog.fas";
           "helper.c";
         |])
  in
  assert (
    cli_c_flags.Cli.c_flags
    = [
        "-I";
        "include dir";
        "-Isecond";
        "-isystem";
        "system dir";
        "-isystemthird";
        "-D";
        "FEATURE=7";
        "-DOTHER=9";
      ]
    && cli_c_flags.link_inputs = [ "helper.c" ]);
  (match Cli.parse [| "fas"; "--emit-llvm"; "prog.fas"; "helper.c" |] with
  | Error message ->
      assert (message = "C inputs and link flags require an executable output")
  | Ok _ -> assert false);
  (match Cli.parse [| "fas"; "one.fas"; "two.fas" |] with
  | Error message
    when message
         = "multiple input files are not supported; use \"path.fas\" for dependencies"
    ->
      ()
  | Error message -> failwith ("multiple input files: unexpected message " ^ message)
  | Ok _ -> failwith "multiple input files: expected rejection");
  let header = expect_cli (Cli.parse [| "fas"; "--emit-header"; "root.fas" |]) in
  assert (header.emit = Cli.Header && not header.output_explicit);
  let debug = expect_cli (Cli.parse [| "fas"; "-g"; "root.fas" |]) in
  assert (debug.debug && debug.optimization = 2);
  let debug_ordered = expect_cli (Cli.parse [| "fas"; "-O3"; "-g"; "root.fas" |]) in
  assert (debug_ordered.debug && debug_ordered.optimization = 3);
  List.iter
    (fun mode ->
      List.iter
        (fun args ->
          match Cli.parse (Array.of_list (("fas" :: args) @ [ "root.fas" ])) with
          | Error message ->
              assert (
                message = "--emit-header cannot be combined with other output modes")
          | Ok _ -> assert false)
        [ [ "--emit-header"; mode ]; [ mode; "--emit-header" ] ])
    [ "--emit-ir"; "--emit-llvm"; "-S"; "--emit-asm"; "-c"; "--emit-obj" ];
  (match Cli.parse [| "fas"; "--emit-header"; "root.fas"; "extra.c" |] with
  | Error message ->
      assert (message = "C inputs and link flags require an executable output")
  | Ok _ -> assert false);
  assert (contains Cli.usage "--emit-header");
  let cli_obj = expect_cli (Cli.parse [| "fas"; "-c"; "path/prog.fas" |]) in
  assert (
    cli_obj.Cli.emit = Cli.Obj && cli_obj.output = "prog.o"
    && not cli_obj.output_explicit);
  let cli_asm = expect_cli (Cli.parse [| "fas"; "-S"; "path/prog.fas" |]) in
  assert (cli_asm.Cli.emit = Cli.Asm && cli_asm.output = "prog.s");
  let cli_named_obj =
    expect_cli (Cli.parse [| "fas"; "-c"; "-o"; "artifact"; "prog.fas" |])
  in
  assert (
    cli_named_obj.Cli.emit = Cli.Obj
    && cli_named_obj.output = "artifact"
    && cli_named_obj.output_explicit);
  assert (
    (expect_cli (Cli.parse [| "fas"; "-c"; "prog.fas" |])).Cli.target = Cli.Portable);
  assert (
    (expect_cli (Cli.parse [| "fas"; "--target=force-native"; "prog.fas" |])).Cli.target
    = Cli.Force_native);
  assert (
    (expect_cli (Cli.parse [| "fas"; "--target=portable"; "prog.fas" |])).Cli.target
    = Cli.Portable);
  (match Cli.parse [| "fas"; "--target=bogus"; "prog.fas" |] with
  | Error message ->
      assert (
        message
        = "unknown --target value `bogus`; accepted values are `portable` and \
           `force-native`")
  | Ok _ -> assert false);
  (* removal A3: --target is oracle-only now, so help no longer lists it *)
  assert (not (contains Cli.usage "--target="));
  (match Cli.parse [| "fas"; "--help" |] with
  | Ok Cli.Help ->
      assert (contains Cli.usage "-g");
      assert (not (contains Cli.usage "inline"));
      assert (contains Cli.usage "-I DIR, -isystem DIR, -D NAME[=VALUE]");
      assert (contains Cli.usage "--sanitize=undefined");
      assert (not (contains Cli.usage "address"))
  | Ok (Cli.Run _)
  | Ok (Cli.Run_program _)
  | Ok (Cli.Check _)
  | Ok (Cli.Lsp _)
  | Ok Cli.Version
  | Error _ ->
      assert false);
  let input_path = Filename.temp_file "fas-driver-" ".fas" in
  Fun.protect
    ~finally:(fun () -> Sys.remove input_path)
    (fun () ->
      let channel = open_out_bin input_path in
      output_string channel "fn test() i64 { return 3 }\n";
      close_out channel;
      let stdout_config =
        expect_cli (Cli.parse [| "fas"; "--emit-llvm"; input_path |])
      in
      assert (not stdout_config.output_explicit);
      (match Driver.run stdout_config with
      | Ok output -> assert (output <> "")
      | Error _ -> assert false);
      List.iter
        (fun flag ->
          let output_path = Filename.temp_file "fas-driver-output-" ".txt" in
          Fun.protect
            ~finally:(fun () -> Sys.remove output_path)
            (fun () ->
              let config =
                expect_cli (Cli.parse [| "fas"; flag; "-o"; output_path; input_path |])
              in
              assert config.output_explicit;
              (match Driver.run config with
              | Ok output -> assert (output = "")
              | Error _ -> assert false);
              let channel = open_in_bin output_path in
              let contents = really_input_string channel (in_channel_length channel) in
              close_in channel;
              assert (contents <> "")))
        [ "--emit-ir"; "--emit-llvm" ]);
  (match Cli.parse [| "fas"; "--emit-ast"; "one.fas" |] with
  | Error message -> assert (message = "unknown option: --emit-ast")
  | Ok _ -> assert false);
  (match Cli.parse [| "fas"; "-o"; "-"; "one.fas" |] with
  | Error message -> assert (message = "-o - is not supported")
  | Ok _ -> assert false);
  (match Cli.parse [| "fas"; "--unknown" |] with Ok _ -> assert false | Error _ -> ());
  List.iter
    (fun (option, expected) ->
      match Cli.parse [| "fas"; option; "root.fas" |] with
      | Error message -> assert (message = expected)
      | Ok _ -> assert false)
    [
      ("-debug", "unknown option `-debug`; write `-g`");
      ("--no-inline", "`--no-inline` was removed; Fas has no inliner yet");
    ];
  (match Cli.parse [| "fas"; "check"; "-O3"; "--sanitize=undefined"; "one.fas" |] with
  | Ok (Cli.Check config) ->
      assert (config.Cli.emit = Cli.Ir && config.Cli.optimization = 3);
      assert (config.Cli.sanitizers = [ "undefined" ] && config.Cli.diagnostics = Cli.Text)
  | Ok _ | Error _ -> assert false);
  (match Cli.parse [| "fas"; "check"; "--diagnostics=json"; "one.fas" |] with
  | Ok (Cli.Check config) -> assert (config.Cli.diagnostics = Cli.Json)
  | Ok _ | Error _ -> assert false);
  List.iter
    (fun (args, expected) ->
      match Cli.parse (Array.of_list ("fas" :: args)) with
      | Error message -> assert (message = expected)
      | Ok _ -> assert false)
    [
      ( [ "check"; "-lm"; "one.fas" ],
        "fas check checks one Fas file; `-lm` is not accepted" );
      ( [ "check"; "helper.c"; "one.fas" ],
        "fas check checks one Fas file; `helper.c` is not accepted" );
      ( [ "check"; "--emit-ir"; "one.fas" ],
        "fas check checks one Fas file; `--emit-ir` is not accepted" );
      ( [ "check"; "--diagnostics=xml"; "one.fas" ],
        "unknown --diagnostics value `xml`; accepted values are `text` and `json`" );
      ([ "check" ], "no input files");
    ];
  let json_diagnostics =
    [
      Diag.error
        (Span.make ~file:"a.fas" ~start_offset:3 ~end_offset:4 ~line:1 ~column:4)
        "bad \"x\"\\\n\tcaf\xc3\xa9 \001 \255 \b\012";
      Diag.warning ~notes:[ "n1" ] ~help:"h"
        (Span.make ~file:"a.fas" ~start_offset:5 ~end_offset:6 ~line:2 ~column:1)
        "w";
      Diag.error Span.synthetic "synthetic";
    ]
  in
  assert (Diag.render_json [] = {|{"diagnostics":[]}|});
  assert (
    Diag.render_json json_diagnostics
    = {|{"diagnostics":[{"severity":"error","file":"a.fas","line":1,"column":4,"start_offset":3,"end_offset":4,"message":"bad \"x\"\\\n\tcafé \u0001 \u00ff \b\f","notes":[],"help":null},{"severity":"warning","file":"a.fas","line":2,"column":1,"start_offset":5,"end_offset":6,"message":"w","notes":["n1"],"help":"h"},{"severity":"error","file":null,"line":null,"column":null,"start_offset":null,"end_offset":null,"message":"synthetic","notes":[],"help":null}]}|});
  let sema_error ?message text =
    let program = expect_ok (Parser.parse (source text)) in
    match Sema.check program with
    | Ok _ -> assert false
    | Error diagnostics -> (
        match message with
        | None -> ()
        | Some fragment ->
            assert (contains (Diag.render_all ~source:None diagnostics) fragment))
  in
  sema_error "fn f(a i64, a i64) i64 { return a }\n";
  sema_error "fn f() i64 { break\n return 0 }\n";
  sema_error "fn f() i64 { switch 1 { case 1: { } case 1: { } } return 0 }\n";
  sema_error "fn f(x i64) i64 { switch x { case x: { } } return 0 }\n";
  sema_error ~message:"duplicate local `x`"
    "fn f() i64 { x i64 = 1\n x i64 = 2\n return x }\n";
  ignore
    (expect_ok
       (Sema.check
          (expect_ok
             (Parser.parse
                (source "fn f() i64 { x i64 = 1\n { x i64 = 2 } return x }\n")))));
  sema_error "fn f() i64 { x vec[0,u64]\n return 0 }\n";
  sema_error ~message:"use of uninitialized local `x`"
    "fn f() i64 { x i64\n defer { x = 1 }\n return x }\n";
  sema_error ~message:"nested defer is not allowed"
    "fn f() void { defer { defer { } } }\n";
  sema_error ~message:"void function cannot return a value"
    "extern \"C\" { fn sink() void }\nfn f() void { return sink() }\n";
  sema_error ~message:"function `f` returning `i64` may reach the end without `return`"
    "fn f() i64 { }\n";
  let edge_program =
    expect_ok
      (Parser.parse
         (source
            "const B i64 = sext[i64](true)\n\
             fn rem(x i8) i8 { return x % -1 }\n\
             fn vecrot() i64 { x vec[4,u32] = splat(1)\n\
            \ y vec[4,u32] = rotl(x, 1)\n\
            \ return sext[i64](true) }\n"))
  in
  let edge_hir = expect_ok (Sema.check edge_program) in
  let edge_ir = expect_ok (Lower.lower edge_hir) in
  let edge_llvm = Fir_llvm.render edge_ir in
  assert (contains edge_llvm "call void @llvm.trap()");
  assert (contains edge_llvm "@llvm.fshl.v4i32");
  assert (contains edge_llvm "declare <4 x i32> @llvm.fshl.v4i32");
  assert (contains edge_llvm "sext i1 true to i64");
  let edge_debug = Fir_print.module_to_string edge_ir in
  assert (contains edge_debug "func @rem (i8) -> (i8)");
  assert (contains edge_debug "trap");
  (* the Fir text parses back to the same module text *)
  (match Fir_parse.parse_module edge_debug with
  | again -> assert (Fir_print.module_to_string again = edge_debug)
  | exception Fir_parse.Error (line, message) ->
      failwith (Printf.sprintf "edge Fir text does not parse: %d: %s" line message));
  assert (edge_debug <> edge_llvm);
  let token_limits = { Limits.default with max_tokens = 1 } in
  (match Lexer.lex ~limits:token_limits (source "x /* trailing trivia */ ") with
  | Ok [ { Token.kind = Token.Ident "x"; _ }; { kind = Token.Eof; _ } ] -> ()
  | Ok _ | Error _ -> assert false);
  (match Lexer.lex ~limits:token_limits (source "x y") with
  | Error diagnostics ->
      assert (contains (Diag.render_all ~source:None diagnostics) "token limit exceeded")
  | Ok _ -> assert false);
  let type_nesting_limits = { Limits.default with max_nesting = 2 } in
  ignore
    (expect_ok
       (Parser.parse ~limits:type_nesting_limits
          (source "fn f(value addr) void { return }\n")));
  (match
     Parser.parse ~limits:type_nesting_limits
       (source "fn f(value arr[2, arr[2, u8]]) void { return }\n")
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "type nesting exceeds the configured limit")
  | Ok _ -> assert false);
  let unary_nesting_limits = { Limits.default with max_nesting = 3 } in
  ignore
    (expect_ok
       (Parser.parse ~limits:unary_nesting_limits
          (source "fn f(value bool) bool { return !value }\n")));
  (match
     Parser.parse ~limits:unary_nesting_limits
       (source "fn f(value bool) bool { return !!value }\n")
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "unary nesting exceeds the configured limit")
  | Ok _ -> assert false);
  let switch_nesting_limits = { Limits.default with max_nesting = 3 } in
  ignore
    (expect_ok
       (Parser.parse ~limits:switch_nesting_limits
          (source "fn f(value i64) void { switch value { default: { return } } }\n")));
  (match
     Parser.parse ~limits:switch_nesting_limits
       (source
          "fn f(value i64) void { switch value { default: switch value { default: \
           return } } }\n")
   with
  | Error diagnostics ->
      assert (
        contains
          (Diag.render_all ~source:None diagnostics)
          "nesting exceeds the configured limit")
  | Ok _ -> assert false);
  (match Process.run [||] with Error _ -> () | Ok _ -> assert false);
  (match Process.run [| "/definitely/missing/fas-tool" |] with
  | Error _ -> ()
  | Ok _ -> assert false);
  let parity_text =
    "extern \"C\" { fn printf(fmt addr, ...) i32 }\n\
     struct Pair { a i64 b i64 }\n\
     const K arr[2,u32] = { 1, 2 }\n\
     fn first(p addr, x i64) i64 { defer { printf(\"d\") } if p != addr_from_bits(0) { \
     y Pair = {x, 2}\n\
    \ return y.a } return if x == 0 { 3 } else { 4 } }\n\
     fn second() i64 { printf(\"s\")\n\
    \ return zext[i64](K[1]) }\n"
  in
  let pp = expect_ok (Parser.parse (source parity_text)) in
  let ph = expect_ok (Sema.check pp) in
  assert (List.length ph.Hir.strings = 2);
  let pir = Fir_llvm.render (expect_ok (Lower.lower ph)) in
  List.iter
    (fun needle -> assert (contains pir needle))
    [
      "target datalayout";
      (* Fir's LLVM keeps memory as bytes: Pair is a 16-byte slot, K is
         8 bytes of data *)
      "alloca [16 x i8], align 8";
      "@K = private constant [8 x i8]";
      "...";
      "phi i64";
      "alloca [8 x i8], align 8";
    ];
  assert (not (contains pir "noalias"));
  assert (not (contains pir "align 16"));
  let run_budget_tests () =
    assert (Limits.default.Limits.max_ast_nodes = 4_000_000);
    assert (Limits.default.Limits.max_ir_nodes = 4_000_000);
    assert (Limits.default.Limits.max_static_data_bytes = 1_073_741_824);
    assert (Limits.budget_profile_name Limits.default = "0.1.5");
    let expect_diag needles result =
      match result with
      | Ok _ -> assert false
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          List.iter (fun needle -> assert (contains rendered needle)) needles
    in
    let expect_ir_diag needles offender result =
      match result with
      | Ok () -> assert false
      | Error (actual, message) ->
          assert (actual = offender);
          List.iter (fun needle -> assert (contains message needle)) needles
    in
    let ast_unit_text = "fn f(a i64) i64 { x i64 = a + 1\n return x }\n" in
    let ast_program = expect_ok (Parser.parse (source ast_unit_text)) in
    let ast_nodes = Ast.count_expanded_nodes ast_program in
    assert (ast_nodes > 8);
    let ast_exact = { Limits.default with max_ast_nodes = ast_nodes } in
    let ast_over = { Limits.default with max_ast_nodes = ast_nodes - 1 } in
    assert (
      Ast.render_program
        (expect_ok (Parser.parse ~limits:ast_exact (source ast_unit_text)))
      = Ast.render_program ast_program);
    (match Parser.parse ~limits:ast_over (source ast_unit_text) with
    | Ok _ -> assert false
    | Error [ diagnostic ] ->
        assert (contains diagnostic.Diag.message "max_ast_nodes");
        assert (contains diagnostic.Diag.message (string_of_int (ast_nodes - 1)));
        assert (contains diagnostic.Diag.message "0.1.5");
        assert (diagnostic.Diag.primary.Span.file = "test.fas")
    | Error _ -> assert false);
    let ast_over_first =
      Parser.parse ~limits:ast_over (source ast_unit_text)
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    let ast_over_second =
      Parser.parse ~limits:ast_over (source ast_unit_text)
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    assert (ast_over_first = ast_over_second);
    ignore
      (expect_ok
         (Parser.parse
            ~limits:{ Limits.default with max_ast_nodes = max_int }
            (source ast_unit_text)));
    expect_diag
      [ "budget max_ast_nodes must not be negative"; "0.1.5" ]
      (Parser.parse
         ~limits:{ Limits.default with max_ast_nodes = min_int }
         (source ast_unit_text));
    ignore
      (expect_ok
         (Parser.parse ~limits:{ Limits.default with max_ast_nodes = 0 } (source "")));
    expect_diag [ "max_ast_nodes"; "0.1.5" ]
      (Parser.parse
         ~limits:{ Limits.default with max_ast_nodes = 0 }
         (source "opaque Q\n"));
    let many_functions =
      String.concat ""
        (List.init 12 (fun i -> Printf.sprintf "fn f%d() i64 { return %d }\n" i i))
    in
    let many_nodes =
      Ast.count_expanded_nodes (expect_ok (Parser.parse (source many_functions)))
    in
    let one_nodes =
      Ast.count_expanded_nodes
        (expect_ok (Parser.parse (source "fn f0() i64 { return 0 }\n")))
    in
    let many_over = { Limits.default with max_ast_nodes = many_nodes - 1 } in
    assert (one_nodes < many_nodes - 1);
    ignore
      (expect_ok (Parser.parse ~limits:many_over (source "fn f0() i64 { return 0 }\n")));
    expect_diag [ "max_ast_nodes"; "0.1.5" ]
      (Parser.parse ~limits:many_over (source many_functions));
    let unit_a =
      expect_ok
        (Parser.parse
           (Source.create ~file:"unit_a.fas" ~text:"fn a() i64 { return 1 }\n"))
    in
    let unit_b =
      expect_ok
        (Parser.parse
           (Source.create ~file:"unit_b.fas" ~text:"fn b() i64 { return 2 }\n"))
    in
    let combined = { Ast.items = unit_a.Ast.items @ unit_b.Ast.items } in
    let combined_nodes = Ast.count_expanded_nodes combined in
    assert (
      Ast.check_expanded_nodes
        ~limits:{ Limits.default with max_ast_nodes = combined_nodes }
        combined
      = Ok ());
    (match
       Ast.check_expanded_nodes
         ~limits:{ Limits.default with max_ast_nodes = combined_nodes - 1 }
         combined
     with
    | Ok () -> assert false
    | Error diagnostic ->
        assert (contains diagnostic.Diag.message "max_ast_nodes");
        assert (contains diagnostic.Diag.message "0.1.5");
        assert (diagnostic.Diag.primary.Span.file = "unit_b.fas"));
    assert (
      Ast.check_expanded_nodes
        ~limits:{ Limits.default with max_ast_nodes = 0 }
        { Ast.items = [] }
      = Ok ());
    (match Ast.item_span_by_name ast_program "f" with
    | Some span -> assert (span.Span.file = "test.fas")
    | None -> assert false);
    assert (Ast.item_span_by_name ast_program "missing" = None);
    let medium_functions =
      String.concat ""
        (List.init 8 (fun i ->
             Printf.sprintf
               "fn m%d(x i64) i64 { a i64 = x + %d\n\
               \ b i64 = a * 2\n\
               \ c i64 = b - x\n\
               \ return c }\n"
               i i))
    in
    let medium_ir =
      expect_ok
        (Lower.lower
           (expect_ok (Sema.check (expect_ok (Parser.parse (source medium_functions))))))
    in
    let rendered_before = Fir_llvm.render medium_ir in
    let medium_nodes = Fir_limits.count_lowered_nodes medium_ir in
    assert (medium_nodes > 8);
    assert (
      Fir_limits.check_lowered_nodes
        ~limits:{ Limits.default with max_ir_nodes = medium_nodes }
        medium_ir
      = Ok ());
    assert (Fir_llvm.render medium_ir = rendered_before);
    let per_function_nodes =
      List.map
        Fir_limits.func_nodes
        (Fir.Vec.to_list medium_ir.Fir.funcs)
    in
    let ir_over = { Limits.default with max_ir_nodes = medium_nodes - 1 } in
    assert (List.for_all (fun n -> n < medium_nodes - 1) per_function_nodes);
    expect_ir_diag
      [ "max_ir_nodes"; string_of_int (medium_nodes - 1); "0.1.5"; "at function `m7`" ]
      (Some "m7")
      (Fir_limits.check_lowered_nodes ~limits:ir_over medium_ir);
    let ir_over_first = Fir_limits.check_lowered_nodes ~limits:ir_over medium_ir in
    let ir_over_second = Fir_limits.check_lowered_nodes ~limits:ir_over medium_ir in
    assert (ir_over_first = ir_over_second);
    assert (
      Fir_limits.check_lowered_nodes
        ~limits:{ Limits.default with max_ir_nodes = max_int }
        medium_ir
      = Ok ());
    expect_ir_diag
      [ "budget max_ir_nodes must not be negative"; "0.1.5" ]
      None
      (Fir_limits.check_lowered_nodes
         ~limits:{ Limits.default with max_ir_nodes = min_int }
         medium_ir);
    let static_small =
      fir_data_module (fun mc ->
          fir_bytes_global mc "s0" "ok";
          Lower_emit.array_global mc ~name:"a0" ~elem_ty:Low.I32 ~elems:[ 1L; 2L; 3L; 4L ]
            ~align:4)
    in
    assert (
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 18 }
        static_small
      = Ok ());
    assert (
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = max_int }
        static_small
      = Ok ());
    expect_ir_diag
      [ "max_static_data_bytes"; "17"; "0.1.5"; "at global `a0`" ]
      (Some "a0")
      (Fir_limits.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 17 }
         static_small);
    expect_ir_diag
      [ "budget max_static_data_bytes must not be negative"; "0.1.5" ]
      None
      (Fir_limits.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = min_int }
         static_small);
    let six = String.make 6 's' in
    let static_cumulative =
      fir_data_module (fun mc -> List.iter (fun n -> fir_bytes_global mc n six) [ "g0"; "g1"; "g2" ])
    in
    assert (
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 18 }
        static_cumulative
      = Ok ());
    expect_ir_diag
      [ "max_static_data_bytes"; "10"; "0.1.5"; "at global `g1`" ]
      (Some "g1")
      (Fir_limits.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 10 }
         static_cumulative);
    (* (A global too big for an int is now a lowering error: Fir sizes are
       ints. The old Ir's overflow tests went with it.) *)
    let static_over_first =
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 17 }
        static_small
    in
    let static_over_second =
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 17 }
        static_small
    in
    assert (static_over_first = static_over_second);
    (* struct P { i8; [3 x i8]; i32; i16 } + 2 bytes tail padding: 12 bytes;
       Q { [0 x <8 x i8>]; P; [4 x i8]; i64 }: 24. 2 Ps and a Q are 48. *)
    let static_padded =
      fir_data_module ~lay:[ ("P", (12, 4, [])); ("Q", (24, 8, [])) ] (fun mc ->
          Lower_emit.array_global mc ~name:"padded" ~elem_ty:(Low.Struct "P") ~elems:[ 0L; 0L ]
            ~align:4;
          Lower_emit.array_global mc ~name:"nested" ~elem_ty:(Low.Struct "Q") ~elems:[ 0L ]
            ~align:8)
    in
    assert (
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 48 }
        static_padded
      = Ok ());
    expect_ir_diag
      [ "max_static_data_bytes"; "47"; "0.1.5"; "at global `nested`" ]
      (Some "nested")
      (Fir_limits.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 47 }
         static_padded);
    let static_padded_first =
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 47 }
        static_padded
    in
    let static_padded_second =
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 47 }
        static_padded
    in
    assert (static_padded_first = static_padded_second)
  in
  run_budget_tests ();
  let run_type_node_budget_tests () =
    assert (Limits.default.Limits.max_type_nodes = 4_000_000);
    let expect_type_diag needles result =
      match result with
      | Ok _ -> assert false
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          List.iter (fun needle -> assert (contains rendered needle)) needles
    in
    let two_use_text =
      "fn two[T](x0 T, x1 T) i64 { return 0 }\n\
       fn use0(a addr) i64 { return two[addr](a, a) }\n"
    in
    let two_program = expect_ok (Parser.parse (source two_use_text)) in
    let exact_two =
      expect_ok
        (Sema.check ~limits:{ Limits.default with max_type_nodes = 3 } two_program)
    in
    assert (List.length exact_two.Hir.funcs = 2);
    assert (
      Fir_llvm.render (expect_ok (Lower.lower exact_two))
      = Fir_llvm.render (expect_ok (Lower.lower (expect_ok (Sema.check two_program)))));
    (match
       Sema.check
         ~limits:{ Limits.default with max_type_nodes = 2 }
         (expect_ok (Parser.parse (source two_use_text)))
     with
    | Ok _ -> assert false
    | Error [ diagnostic ] ->
        assert (contains diagnostic.Diag.message "max_type_nodes");
        assert (contains diagnostic.Diag.message "of 2 (profile 0.1.5)");
        assert (contains diagnostic.Diag.message "two");
        assert (diagnostic.Diag.primary.Span.file = "test.fas")
    | Error _ -> assert false);
    let two_over_first =
      Sema.check
        ~limits:{ Limits.default with max_type_nodes = 2 }
        (expect_ok (Parser.parse (source two_use_text)))
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    let two_over_second =
      Sema.check
        ~limits:{ Limits.default with max_type_nodes = 2 }
        (expect_ok (Parser.parse (source two_use_text)))
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    assert (two_over_first = two_over_second);
    ignore
      (expect_ok
         (Sema.check
            ~limits:{ Limits.default with max_type_nodes = max_int }
            (expect_ok (Parser.parse (source two_use_text)))));
    expect_type_diag
      [ "budget max_type_nodes must not be negative"; "0.1.5" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = min_int }
         (expect_ok (Parser.parse (source two_use_text))));
    ignore
      (expect_ok
         (Sema.check
            ~limits:{ Limits.default with max_type_nodes = 0 }
            (expect_ok (Parser.parse (source "")))));
    expect_type_diag [ "max_type_nodes"; "0.1.5" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = 0 }
         (expect_ok (Parser.parse (source two_use_text))));
    let cumulative_text =
      "fn two[T](x0 T, x1 T) i64 { return 0 }\n\
       fn use0(a addr, b usize) i64 { return two[addr](a, a) + two[usize](b, b) }\n"
    in
    let cumulative_program = expect_ok (Parser.parse (source cumulative_text)) in
    let exact_cumulative =
      expect_ok
        (Sema.check
           ~limits:{ Limits.default with max_type_nodes = 6 }
           cumulative_program)
    in
    assert (List.length exact_cumulative.Hir.funcs = 3);
    expect_type_diag
      [ "max_type_nodes"; "of 5 (profile 0.1.5)"; "test.fas" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = 5 }
         (expect_ok (Parser.parse (source cumulative_text))));
    let box_text =
      "struct Box[T] { a T, b T }\nfn use0() i64 { x Box[addr]\n return 0 }\n"
    in
    let box_program = expect_ok (Parser.parse (source box_text)) in
    let exact_box =
      expect_ok
        (Sema.check ~limits:{ Limits.default with max_type_nodes = 2 } box_program)
    in
    assert (List.length exact_box.Hir.structs = 1);
    (match
       Sema.check
         ~limits:{ Limits.default with max_type_nodes = 1 }
         (expect_ok (Parser.parse (source box_text)))
     with
    | Ok _ -> assert false
    | Error [ diagnostic ] ->
        assert (contains diagnostic.Diag.message "struct specialization");
        assert (contains diagnostic.Diag.message "Box");
        assert (contains diagnostic.Diag.message "of 1 (profile 0.1.5)");
        assert (diagnostic.Diag.primary.Span.file = "test.fas")
    | Error _ -> assert false);
    let pick_text =
      "fn pick[N const i64](x i64) i64 { return x + N }\n\
       fn use0(a i64) i64 { return pick[1](a) }\n"
    in
    let pick_program = expect_ok (Parser.parse (source pick_text)) in
    let exact_pick =
      expect_ok
        (Sema.check ~limits:{ Limits.default with max_type_nodes = 3 } pick_program)
    in
    assert (List.length exact_pick.Hir.funcs = 2);
    expect_type_diag
      [ "function specialization"; "pick"; "of 2 (profile 0.1.5)"; "test.fas" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = 2 }
         (expect_ok (Parser.parse (source pick_text))));
    let widen_text =
      "fn widen[T](x0 addr, x1 addr, x2 addr, x3 addr) i64 {\n\
       a T\n\
      \ b T\n\
      \ c T\n\
      \ d T\n\
      \ return 0 }\n\
       fn use0(a addr) i64 { return widen[arr[2,arr[2,u8]]](a, a, a, a) }\n\
       fn use1(b addr) i64 { return widen[arr[3,arr[2,u8]]](b, b, b, b) }\n"
    in
    let widen_program = expect_ok (Parser.parse (source widen_text)) in
    let exact_widen =
      expect_ok
        (Sema.check ~limits:{ Limits.default with max_type_nodes = 34 } widen_program)
    in
    assert (List.length exact_widen.Hir.funcs = 4);
    expect_type_diag
      [ "max_type_nodes"; "of 33 (profile 0.1.5)"; "widen" ]
      (Sema.check
         ~limits:{ Limits.default with max_type_nodes = 33 }
         (expect_ok (Parser.parse (source widen_text))));
    let widen_over_first =
      Sema.check
        ~limits:{ Limits.default with max_type_nodes = 33 }
        (expect_ok (Parser.parse (source widen_text)))
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    let widen_over_second =
      Sema.check
        ~limits:{ Limits.default with max_type_nodes = 33 }
        (expect_ok (Parser.parse (source widen_text)))
      |> Result.map_error (fun diagnostics -> Diag.render_all ~source:None diagnostics)
    in
    assert (widen_over_first = widen_over_second)
  in
  run_type_node_budget_tests ();
  let run_diagnostic_naming_tests () =
    let expect_naming needles ~limits text =
      match Sema.check ~limits (expect_ok (Parser.parse (source text))) with
      | Ok _ -> assert false
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          List.iter (fun needle -> assert (contains rendered needle)) needles
    in
    expect_naming
      [
        "string literal bytes exceed budget max_interned_string_bytes of 2 (profile \
         0.1.5)";
      ]
      ~limits:{ Limits.default with max_interned_string_bytes = 2 }
      "fn f() void { s addr = \"abc\"\n return }\n";
    expect_naming
      [
        "cumulative interned string bytes exceed budget max_interned_string_bytes of 3 \
         (profile 0.1.5)";
      ]
      ~limits:{ Limits.default with max_interned_string_bytes = 3 }
      "fn f() void { a addr = \"ab\"\n b addr = \"cd\"\n return }\n";
    expect_naming
      [ "object size exceeds budget max_object_size of 15 (profile 0.1.5)" ]
      ~limits:{ Limits.default with max_object_size = 15 }
      "fn f() void { value arr[16,u8]\n return }\n";
    expect_naming
      [ "alignment exceeds budget max_object_alignment of 8 (profile 0.1.5)" ]
      ~limits:{ Limits.default with max_object_alignment = 8 }
      "struct Aligned @align(16) { value u8 }\nfn f() void { return }\n";
    expect_naming
      [
        "aggregate element count exceeds the configured limit";
        "budget max_aggregate_elements of 99 (profile 0.1.5)";
      ]
      ~limits:{ Limits.default with max_aggregate_elements = 99 }
      "fn f() void { value arr[100,u8]\n return }\n";
    expect_naming
      [
        "const specialization count limit exceeded";
        "budget max_specializations of 1 (profile 0.1.5)";
      ]
      ~limits:{ Limits.default with max_specializations = 1 }
      "fn id[N const i64]() i64 { return N }\n\
       fn a() i64 { return id[1]() }\n\
       fn b() i64 { return id[2]() }\n";
    expect_naming
      [
        "const specialization recursion depth limit exceeded";
        "budget max_specialization_depth of 1 (profile 0.1.5)";
      ]
      ~limits:{ Limits.default with max_specialization_depth = 1 }
      "fn inner[N const i64]() i64 { return N }\n\
       fn outer[M const i64]() i64 { return inner[M]() }\n\
       fn test() i64 { return outer[5]() }\n";
    expect_naming
      [
        "function specialization count limit exceeded";
        "budget max_specializations of 1 (profile 0.1.5)";
      ]
      ~limits:{ Limits.default with max_specializations = 1 }
      "fn identity[T](value T) T { return value }\n\
       fn test() i64 { a u8 = identity[u8](1)\n\
      \ return identity[i64](1) }\n";
    expect_naming
      [
        "struct specialization recursion depth limit exceeded";
        "budget max_specialization_depth of 1 (profile 0.1.5)";
      ]
      ~limits:{ Limits.default with max_specialization_depth = 1 }
      "struct Inner[T] { value T }\n\
       struct Outer[T] { inner Inner[T] }\n\
       fn test() i64 { value Outer[u8]\n\
      \ return 0 }\n"
  in
  let run_specialization_span_tests () =
    let program =
      expect_ok
        (Parser.parse
           (Source.create ~file:"spans.fas"
              ~text:
                "fn first() u64 { return 1 }\n\
                 fn second[T](value T) T { return value }\n\
                 fn test() i64 { return first() }\n"))
    in
    let first_span, template_span =
      match program.Ast.items with
      | [ Ast.Func { span = first; _ }; Ast.Func { span = template; _ }; _ ] ->
          (first, template)
      | _ -> failwith "unexpected span-mapping program shape"
    in
    assert (Ast.item_span_by_name program "first" = Some first_span);
    assert (Ast.item_span_by_name program "second" = Some template_span);
    assert (Ast.item_span_by_name program "missing" = None);
    let type_mangled =
      Sema_specialization.mangle_type_specialization "second" [ Ast.Bool ]
    in
    assert (Ast.item_span_by_name program type_mangled = Some template_span);
    assert (
      Ast.item_span_by_name program ("second" ^ Ast.specialization_name_delimiter)
      = Some template_span);
    assert (
      Ast.item_span_by_name program
        ("missing" ^ Ast.specialization_name_delimiter ^ "4_bool")
      = None);
    assert (
      Ast.item_span_by_name program
        ("first" ^ Ast.specialization_name_delimiter ^ "named6_nope$spec$4_bool")
      = Some first_span)
  in
  let run_debug_ir_budget_tests () =
    let empty = fir_data_module ignore in
    (match Fir_limits.render_text ~limits:Limits.default empty with
    | Ok text -> assert (text = Fir_print.module_to_string empty)
    | Error _ -> assert false);
    let small =
      fir_data_module (fun mc ->
          fir_bytes_global mc "s" "abc";
          fir_void_function mc "control_flow" (fun fn -> Lower_emit.ret fn None))
    in
    let full =
      match
        Fir_limits.render_text
          ~limits:{ Limits.default with max_rendered_ir_bytes = max_int }
          small
      with
      | Ok text -> text
      | Error _ -> assert false
    in
    let length = String.length full in
    (match
       Fir_limits.render_text
         ~limits:{ Limits.default with max_rendered_ir_bytes = length }
         small
     with
    | Ok text -> assert (text = full)
    | Error _ -> assert false);
    (match
       Fir_limits.render_text
         ~limits:{ Limits.default with max_rendered_ir_bytes = length - 1 }
         small
     with
    | Error message ->
        assert (
          contains message
            "rendered debug IR bytes exceed budget max_rendered_ir_bytes of");
        assert (contains message "(profile 0.1.5)")
    | Ok _ -> assert false);
    (match
       Fir_limits.render_text
         ~limits:{ Limits.default with max_rendered_ir_bytes = min_int }
         small
     with
    | Error message ->
        assert (contains message "budget max_rendered_ir_bytes must not be negative")
    | Ok _ -> assert false);
    let over =
      match
        Fir_limits.render_text
          ~limits:{ Limits.default with max_rendered_ir_bytes = length - 1 }
          small
      with
      | Error message -> message
      | Ok _ -> assert false
    in
    match
      Fir_limits.render_text
        ~limits:{ Limits.default with max_rendered_ir_bytes = length - 1 }
        small
    with
    | Error message -> assert (message = over)
    | Ok _ -> assert false
  in
  (* a function [control_flow] with one alloca of each [slots] type, each in
     its own block *)
  let scratch_module slots =
    fir_data_module (fun mc ->
        fir_void_function mc "control_flow" (fun fn ->
            List.iteri
              (fun i (t, align) ->
                if i > 0 then (
                  let b = Lower_emit.new_block fn in
                  Lower_emit.br fn b;
                  Lower_emit.enter fn b None);
                Lower_emit.alloca fn i t align)
              slots;
            Lower_emit.ret fn None))
  in
  let run_scratch_budget_tests () =
    let one = scratch_module [ (Low.I32, 4); (Low.I64, 8) ] in
    assert (
      Fir_limits.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 12 }
        one
      = Ok ());
    assert (
      Fir_limits.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = max_int }
        one
      = Ok ());
    assert (
      Fir_limits.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 0 }
        (fir_data_module ignore)
      = Ok ());
    (match
       Fir_limits.check_stack_scratch_bytes
         ~limits:{ Limits.default with max_stack_scratch_bytes = 11 }
         one
     with
    | Error (offender, message) ->
        assert (offender = Some "control_flow");
        assert (
          contains message
            "cumulative alloca scratch bytes exceed budget max_stack_scratch_bytes of \
             11");
        assert (contains message "(profile 0.1.5)");
        assert (contains message "at function `control_flow`")
    | Ok () -> assert false);
    (match
       Fir_limits.check_stack_scratch_bytes
         ~limits:{ Limits.default with max_stack_scratch_bytes = min_int }
         one
     with
    | Error (offender, message) ->
        assert (offender = None);
        assert (contains message "budget max_stack_scratch_bytes must not be negative")
    | Ok () -> assert false);
    (* (The old Ir's overflowing [max_int x [max_int x i8]] slot is gone:
       Lower_emit sizes a slot as an int when it emits the alloca.) *)
    let over_first =
      Fir_limits.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 11 }
        one
    in
    let over_second =
      Fir_limits.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 11 }
        one
    in
    assert (over_first = over_second)
  in
  let run_vector_size_tests () =
    let check_scratch budget slot_ty =
      Fir_limits.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = budget }
        (scratch_module [ (slot_ty, 1) ])
    in
    assert (check_scratch 8 (Low.Vector (3, Low.I16)) = Ok ());
    assert (check_scratch 1 (Low.Vector (8, Low.I1)) = Ok ());
    assert (check_scratch 16 (Low.Vector (3, Low.I32)) = Ok ());
    assert (check_scratch 4 (Low.Vector (3, Low.I8)) = Ok ());
    assert (check_scratch 8 (Low.Array (2, Low.Vector (3, Low.I8))) = Ok ());
    let over budget slot_ty =
      match check_scratch budget slot_ty with
      | Error (offender, message) ->
          assert (offender = Some "control_flow");
          assert (
            contains message
              "cumulative alloca scratch bytes exceed budget max_stack_scratch_bytes")
      | Ok () -> assert false
    in
    over 7 (Low.Vector (3, Low.I16));
    over 0 (Low.Vector (8, Low.I1));
    over 15 (Low.Vector (3, Low.I32));
    over 3 (Low.Vector (3, Low.I8));
    over 7 (Low.Array (2, Low.Vector (3, Low.I8)));
    let vec_global elem_ty =
      fir_data_module (fun mc ->
          Lower_emit.array_global mc ~name:"vg" ~elem_ty ~elems:[ 0L ] ~align:4)
    in
    assert (
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 4 }
        (vec_global (Low.Vector (3, Low.I8)))
      = Ok ());
    (match
       Fir_limits.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 3 }
         (vec_global (Low.Vector (3, Low.I8)))
     with
    | Error (offender, _) -> assert (offender = Some "vg")
    | Ok () -> assert false);
    (* struct W { [2 x <3 x i8>] }: the front end's layout gives 8 bytes *)
    let w_size = Lower_emit.size_of (Hashtbl.create 1) (Low.Array (2, Low.Vector (3, Low.I8))) in
    assert (w_size = 8);
    let wrapped =
      fir_data_module ~lay:[ ("W", (w_size, 4, [])) ] (fun mc ->
          Lower_emit.array_global mc ~name:"w" ~elem_ty:(Low.Struct "W") ~elems:[ 0L ]
            ~align:4)
    in
    assert (
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 8 }
        wrapped
      = Ok ());
    (match
       Fir_limits.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 7 }
         wrapped
     with
    | Error (offender, _) -> assert (offender = Some "w")
    | Ok () -> assert false);
    let source_program =
      expect_ok
        (Parser.parse
           (Source.create ~file:"vec_size.fas"
              ~text:"fn f(x vec[3,u16]) void { return }\n"))
    in
    let source_hir = expect_ok (Sema.check source_program) in
    let source_ir = expect_ok (Lower.lower source_hir) in
    assert (
      Fir_limits.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 8 }
        source_ir
      = Ok ());
    match
      Fir_limits.check_stack_scratch_bytes
        ~limits:{ Limits.default with max_stack_scratch_bytes = 7 }
        source_ir
    with
    | Error (offender, message) ->
        assert (offender = Some "f");
        assert (
          contains message
            "cumulative alloca scratch bytes exceed budget max_stack_scratch_bytes")
    | Ok () -> assert false
  in
  let run_layout_accounting_parity_tests () =
    let hir_size ty =
      match Hir.layout [] ty with
      | Ok (size, _) -> size
      | Error message -> failwith message
    in
    (* Lower_emit's slot sizes must agree with the front end's layouts *)
    let static_type_bytes ty =
      match Lower_emit.size_of (Hashtbl.create 1) ty with
      | n -> Ok n
      | exception Lower_emit.Unsupported _ -> Error ()
    in
    let assert_pair low_ty hir_ty =
      match static_type_bytes low_ty with
      | Ok bytes -> assert (bytes = hir_size hir_ty)
      | Error () -> assert false
    in
    let elems =
      [
        (Low.I1, Hir.Bool);
        (Low.I8, Hir.Int Hir.U8);
        (Low.I16, Hir.Int Hir.U16);
        (Low.I32, Hir.Int Hir.U32);
        (Low.I64, Hir.Int Hir.U64);
      ]
    in
    List.iter (fun (low_e, hir_e) -> assert_pair low_e hir_e) elems;
    assert_pair (Low.Pointer Low.I8) Hir.Addr;
    List.iter
      (fun lanes ->
        List.iter
          (fun (low_e, hir_e) ->
            assert_pair (Low.Vector (lanes, low_e)) (Hir.Vec (lanes, hir_e)))
          elems)
      [ 1; 2; 3; 4; 5; 7; 8; 9; 16; 31; 64 ];
    List.iter
      (fun count ->
        List.iter
          (fun lanes ->
            List.iter
              (fun (low_e, hir_e) ->
                assert_pair
                  (Low.Array (count, Low.Vector (lanes, low_e)))
                  (Hir.Array (count, Hir.Vec (lanes, hir_e))))
              [ (Low.I1, Hir.Bool); (Low.I8, Hir.Int Hir.U8); (Low.I16, Hir.Int Hir.U16) ])
          [ 1; 3; 8 ])
      [ 0; 1; 2; 3 ];
    assert (static_type_bytes (Low.Vector (2, Low.Array (2, Low.I8))) = Error ());
    (match Hir.layout [] (Hir.Vec (2, Hir.Array (2, Hir.Int Hir.U8))) with
    | Error _ -> ()
    | Ok _ -> assert false);
    assert (static_type_bytes (Low.Vector (2, Low.Void)) = Error ());
    (match Hir.layout [] (Hir.Vec (2, Hir.Void)) with
    | Error _ -> ()
    | Ok _ -> assert false);
    assert (static_type_bytes (Low.Vector (2, Low.Struct "s")) = Error ());
    (match Hir.layout [] (Hir.Vec (2, Hir.Struct "s")) with
    | Error _ -> ()
    | Ok _ -> assert false);
    assert (static_type_bytes (Low.Vector (2, Low.Vector (2, Low.I8))) = Error ());
    match Hir.layout [] (Hir.Vec (2, Hir.Vec (2, Hir.Int Hir.U8))) with
    | Error _ -> ()
    | Ok _ -> assert false
  in
  let run_top_level_order_tests () =
    let source_program =
      expect_ok
        (Parser.parse
           (Source.create ~file:"top_level_order.fas"
              ~text:
                "const a u32 = 1\n\
                 const b u32 = 2\n\
                 const g0 arr[2,u8] = { 1, 2 }\n\
                 const c u32 = 3\n\
                 const g1 arr[2,u8] = { 3, 4 }\n\
                 const d u32 = 4\n\
                 const w0 i64 = 7\n\
                 const v0 vec[2,i32] = bitcast[vec[2,i32]](w0)\n\
                 const l0 i64 = bitcast[i64](v0)\n\
                 const w1 i64 = 9\n\
                 const v1 vec[2,i32] = bitcast[vec[2,i32]](w1)\n\
                 const l1 i64 = bitcast[i64](v1)\n\
                 fn f() void { return }\n"))
    in
    let hir = expect_ok (Sema.check source_program) in
    assert (
      List.map (fun (c : Hir.const_def) -> c.Hir.name) hir.Hir.consts
      = [ "a"; "b"; "c"; "d"; "w0"; "w1"; "l0"; "l1" ]);
    assert (
      List.map (fun (c : Hir.const_arr_def) -> c.Hir.name) hir.Hir.const_arrays
      = [ "g0"; "g1" ])
  in
  let run_phase_invariant_tests () =
    let ok = function Ok () -> () | Error message -> failwith message in
    let rejected needle = function
      | Ok () -> failwith ("phase invariant accepted invalid input: " ^ needle)
      | Error message ->
          if not (contains message needle) then
            failwith ("phase invariant wrong diagnostic: " ^ message)
    in
    ok (Sema_invariants.check_declarations [ (0, "a"); (1, "b"); (2, "c") ]);
    rejected "not dense" (Sema_invariants.check_declarations [ (0, "a"); (0, "b") ]);
    rejected "collected twice"
      (Sema_invariants.check_declarations [ (0, "a"); (1, "a") ]);
    ok
      (Sema_invariants.check_const_environment ~declared:[ "a"; "g0"; "b"; "g1" ]
         ~early:[ "a"; "b" ] ~consts:[ "a"; "b" ] ~arrays:[ "g0"; "g1" ]);
    ok
      (Sema_invariants.check_const_environment ~declared:[ "a"; "g0"; "c" ]
         ~early:[ "a" ] ~consts:[ "a"; "c" ] ~arrays:[ "g0" ]);
    rejected "do not match declared"
      (Sema_invariants.check_const_environment ~declared:[ "a"; "b" ] ~early:[ "a" ]
         ~consts:[ "a" ] ~arrays:[]);
    rejected "emitted twice"
      (Sema_invariants.check_const_environment ~declared:[ "a" ] ~early:[ "a" ]
         ~consts:[ "a"; "a" ] ~arrays:[]);
    rejected "emitted twice"
      (Sema_invariants.check_const_environment ~declared:[ "g0" ] ~early:[] ~consts:[]
         ~arrays:[ "g0"; "g0" ]);
    rejected "do not match declared"
      (Sema_invariants.check_const_environment ~declared:[ "a" ] ~early:[ "a" ]
         ~consts:[ "a" ] ~arrays:[ "a" ]);
    rejected "array and vector constants are not in declaration order"
      (Sema_invariants.check_const_environment ~declared:[ "g0"; "g1" ] ~early:[]
         ~consts:[] ~arrays:[ "g1"; "g0" ]);
    rejected "early-resolved set"
      (Sema_invariants.check_const_environment ~declared:[ "a"; "b" ] ~early:[ "b" ]
         ~consts:[ "a"; "b" ] ~arrays:[]);
    rejected "early-resolved scalar constants are not in declaration order"
      (Sema_invariants.check_const_environment ~declared:[ "a"; "b" ]
         ~early:[ "b"; "a" ] ~consts:[ "b"; "a" ] ~arrays:[]);
    rejected "late scalar constants are not in declaration order"
      (Sema_invariants.check_const_environment ~declared:[ "a"; "x"; "y" ]
         ~early:[ "a" ] ~consts:[ "a"; "y"; "x" ] ~arrays:[]);
    ok (Sema_invariants.check_materialization ~pending:0 ~functions:[ "f"; "g" ]);
    rejected "remain pending"
      (Sema_invariants.check_materialization ~pending:2 ~functions:[]);
    rejected "emitted twice"
      (Sema_invariants.check_materialization ~pending:0 ~functions:[ "f"; "f" ])
  in
  run_diagnostic_naming_tests ();
  run_specialization_span_tests ();
  run_debug_ir_budget_tests ();
  run_scratch_budget_tests ();
  run_vector_size_tests ();
  run_layout_accounting_parity_tests ();
  let run_cross_budget_tests () =
    let one_diag name outcome =
      match outcome with
      | Error [ diagnostic ] -> diagnostic.Diag.message
      | Error diagnostics ->
          failwith
            (name ^ ": actual queue: "
            ^ String.concat " | " (List.map (fun d -> d.Diag.message) diagnostics))
      | Ok _ -> failwith (name ^ ": accepted")
    in
    let fails_alone name budget_needle limits text =
      match Sema.check ~limits (expect_ok (Parser.parse (source text))) with
      | Error diagnostics ->
          let rendered = Diag.render_all ~source:None diagnostics in
          if not (contains rendered budget_needle) then
            failwith (name ^ ": single budget " ^ budget_needle ^ " did not fire")
      | Ok _ -> failwith (name ^ ": single budget " ^ budget_needle ^ " accepted")
    in
    let pair name (limits_a, needle_a) (limits_b, needle_b) limits_both winner text =
      fails_alone name needle_a limits_a text;
      fails_alone name needle_b limits_b text;
      let run () =
        one_diag name
          (Sema.check ~limits:limits_both (expect_ok (Parser.parse (source text))))
      in
      let message = run () in
      let again = run () in
      let third = run () in
      if again <> message || third <> message then
        failwith (name ^ ": nondeterministic budget winner");
      if message <> winner then failwith (name ^ ": winner drifted: " ^ message)
    in
    pair "aggregate-vs-object"
      ({ Limits.default with max_aggregate_elements = 50 }, "max_aggregate_elements")
      ({ Limits.default with max_object_size = 8 }, "max_object_size")
      { Limits.default with max_aggregate_elements = 50; max_object_size = 8 }
      "object size exceeds budget max_object_size of 8 (profile 0.1.5)"
      "struct S { big arr[100,u8] }\nfn f() void { s S\n return }\n";
    pair "strings-vs-aggregate"
      ( { Limits.default with max_interned_string_bytes = 4 },
        "max_interned_string_bytes" )
      ({ Limits.default with max_aggregate_elements = 50 }, "max_aggregate_elements")
      { Limits.default with max_interned_string_bytes = 4; max_aggregate_elements = 50 }
      "aggregate element count exceeds the configured limit: budget \
       max_aggregate_elements of 50 (profile 0.1.5)"
      "struct S { big arr[100,u8] }\n\
       fn f() void { s S\n\
       q addr = \"hello world\"\n\
      \ return }\n";
    pair "typenodes-vs-specializations"
      ({ Limits.default with max_type_nodes = 2 }, "max_type_nodes")
      ({ Limits.default with max_specializations = 0 }, "max_specializations")
      { Limits.default with max_type_nodes = 2; max_specializations = 0 }
      "function specialization count limit exceeded: budget max_specializations of 0 \
       (profile 0.1.5)"
      "fn two[T](x0 T, x1 T) i64 { return 0 }\n\
       fn use0(a addr) i64 { return two[addr](a, a) }\n";
    let sd_text = "const A arr[4,u8] = { 1, 2, 3, 4 }\nfn f() void { return }\n" in
    let sd_program = expect_ok (Parser.parse (source sd_text)) in
    (match
       Fir_limits.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 3 }
         (expect_ok (Lower.lower (expect_ok (Sema.check sd_program))))
     with
    | Ok () ->
        failwith "staticdata-vs-object: single budget max_static_data_bytes accepted"
    | Error _ -> ());
    (match
       Sema.check
         ~limits:{ Limits.default with max_object_size = 2 }
         (expect_ok (Parser.parse (source sd_text)))
     with
    | Ok _ -> failwith "staticdata-vs-object: single budget max_object_size accepted"
    | Error _ -> ());
    let sd_run () =
      one_diag "staticdata-vs-object"
        (Sema.check
           ~limits:
             { Limits.default with max_static_data_bytes = 3; max_object_size = 2 }
           (expect_ok (Parser.parse (source sd_text))))
    in
    let sd_message = sd_run () in
    let sd_again = sd_run () in
    let sd_third = sd_run () in
    if sd_again <> sd_message || sd_third <> sd_message then
      failwith "staticdata-vs-object: nondeterministic budget winner";
    if sd_message <> "object size exceeds budget max_object_size of 2 (profile 0.1.5)"
    then failwith ("staticdata-vs-object: winner drifted: " ^ sd_message);
    let global_data_program =
      (* an all-zero initializer is zero-fill storage (bss) and costs no
         static data, so the initialized global holds a 1 *)
      expect_ok (Parser.parse (source "var Zero u32\nvar Initialized u32 = 1\n"))
    in
    let global_data_ir =
      expect_ok (Sema.check global_data_program) |> Lower.lower |> expect_ok
    in
    assert (
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 4 }
        global_data_ir
      = Ok ());
    (match
       Fir_limits.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 3 }
         global_data_ir
     with
    | Error (Some "Initialized", _) -> ()
    | Error (offender, _) ->
        failwith
          ("initialized global budget reported the wrong object: "
          ^ Option.value offender ~default:"none")
    | Ok () -> failwith "initialized global was omitted from static-data budget");
    let address_data_program =
      expect_ok
        (Parser.parse
           (source "var Target i32\nconst Table arr[2,addr] = {&Target,&Target}\n"))
    in
    let address_data_ir =
      expect_ok (Sema.check address_data_program) |> Lower.lower |> expect_ok
    in
    assert (
      Fir_limits.check_static_data_bytes
        ~limits:{ Limits.default with max_static_data_bytes = 16 }
        address_data_ir
      = Ok ());
    (match
       Fir_limits.check_static_data_bytes
         ~limits:{ Limits.default with max_static_data_bytes = 15 }
         address_data_ir
     with
    | Error (Some "Table", _) -> ()
    | _ -> failwith "address slots must each count eight static-data bytes");
    pair "specializations-vs-aggregate"
      ({ Limits.default with max_specializations = 0 }, "max_specializations")
      ({ Limits.default with max_aggregate_elements = 50 }, "max_aggregate_elements")
      { Limits.default with max_specializations = 0; max_aggregate_elements = 50 }
      "struct specialization count limit exceeded: budget max_specializations of 0 \
       (profile 0.1.5)"
      "struct Box[T] { a T, b T }\n\
       struct S { big arr[100,u8] }\n\
       fn use0() i64 { x Box[addr]\n\
      \ return 0 }\n\
       fn f() void { s S\n\
      \ return }\n";
    pair "strings-vs-typenodes"
      ( { Limits.default with max_interned_string_bytes = 4 },
        "max_interned_string_bytes" )
      ({ Limits.default with max_type_nodes = 2 }, "max_type_nodes")
      { Limits.default with max_interned_string_bytes = 4; max_type_nodes = 2 }
      "cumulative expanded type nodes exceed budget max_type_nodes of 2 (profile \
       0.1.5) at function specialization `two$spec$4_addr`"
      "fn two[T](x0 T, x1 T) i64 { return 0 }\n\
       fn use0(a addr, s addr) i64 { q addr = \"hello world\"\n\
       return two[addr](a, a) }\n"
  in
  run_top_level_order_tests ();
  run_cross_budget_tests ();
  run_phase_invariant_tests ();
  print_endline "frontend unit tests: ok"

let () =
  assert (C_exports.guard "checksum.h" = "CHECKSUM");
  assert (C_exports.guard "/tmp/path/my-api.h" = "MY_API");
  assert (C_exports.guard "checksum" = "CHECKSUM")

let () =
  let open Json in
  let j s = String.map (fun c -> if c = '~' then Char.chr 92 else c) s in
  let parse s = parse (j s) in
  assert (
    parse {|{"a":[1,{"b":null,"c":[true,false]}],"d":"x"}|}
    = Ok
        (Obj
           [
             ( "a",
               Arr
                 [ Num "1"; Obj [ ("b", Null); ("c", Arr [ Bool true; Bool false ]) ] ]
             );
             ("d", Str "x");
           ]));
  assert (
    parse {|"~"~~~/~b~f~n~r~t~u0041~u00e9"|} = Ok (Str "\"\\/\b\012\n\r\tA\xc3\xa9"));
  assert (parse {|"~uD83D~uDE00"|} = Ok (Str "\xf0\x9f\x98\x80"));
  assert (parse {|"~uD83D"|} = Error "unpaired surrogate at byte 7");
  assert (parse {|"~uDE00"|} = Error "unpaired surrogate at byte 7");
  assert (parse {|"~uD83D~u0041"|} = Error "unpaired surrogate at byte 13");
  assert (parse {|"~x"|} = Error "bad escape at byte 3");
  assert (
    parse {|[1e3,-2.5E+10,0.5]|} = Ok (Arr [ Num "1e3"; Num "-2.5E+10"; Num "0.5" ]));
  assert (parse {|[1,]|} = Error "unexpected character at byte 3");
  assert (parse {|{"a":1,}|} = Error "expected '\"' at byte 7");
  assert (parse {|1 x|} = Error "trailing text at byte 2");
  assert (parse {|"abc|} = Error "unterminated string at byte 4");
  assert (parse "\"a\n\"" = Error "control character in string at byte 3");
  assert (field "x" (Obj [ ("x", Null) ]) = Null);
  assert (field "y" (Obj [ ("x", Null) ]) = Null);
  assert (field "x" (Arr []) = Null)

let () =
  let char_at text i =
    let length = String.length text in
    let byte k = Char.code text.[k] in
    let within lo hi k = k < length && byte k >= lo && byte k <= hi in
    let tail k = within 0x80 0xBF k in
    let lead = byte i in
    if lead < 0x80 then (1, 1)
    else if lead >= 0xC2 && lead <= 0xDF && tail (i + 1) then (2, 1)
    else if
      lead >= 0xE0 && lead <= 0xEF
      && within
           (if lead = 0xE0 then 0xA0 else 0x80)
           (if lead = 0xED then 0x9F else 0xBF)
           (i + 1)
      && tail (i + 2)
    then (3, 1)
    else if
      lead >= 0xF0 && lead <= 0xF4
      && within
           (if lead = 0xF0 then 0x90 else 0x80)
           (if lead = 0xF4 then 0x8F else 0xBF)
           (i + 1)
      && tail (i + 2)
      && tail (i + 3)
    then (4, 2)
    else (1, 1)
  in
  let scan_position text offset =
    let offset = max 0 (min offset (String.length text)) in
    let rec line_start i line start =
      if i >= offset then (line, start)
      else if text.[i] = '\n' then line_start (i + 1) (line + 1) (i + 1)
      else line_start (i + 1) line start
    in
    let line, start = line_start 0 0 0 in
    let rec units i acc =
      if i >= offset then acc
      else
        let width, size = char_at text i in
        units (i + width) (acc + size)
    in
    (line, units start 0)
  in
  let scan_offset text (line, character) =
    let length = String.length text in
    let rec find_line i current =
      if current = line then Some i
      else
        match String.index_from_opt text i '\n' with
        | None -> None
        | Some j -> find_line (j + 1) (current + 1)
    in
    match if line < 0 then None else find_line 0 0 with
    | None -> length
    | Some start ->
        let at_break i =
          i >= length
          || text.[i] = '\n'
          || (text.[i] = '\r' && i + 1 < length && text.[i + 1] = '\n')
        in
        let rec walk i units =
          if at_break i then i
          else
            let width, size = char_at text i in
            if units + size > character then i else walk (i + width) (units + size)
        in
        walk start 0
  in
  let pieces =
    [|
      "a";
      "bc";
      "\t";
      "\n";
      "\r\n";
      "\r";
      "\xc3\xa9";
      "\xe2\x82\xac";
      "\xf0\x9f\x98\x80";
      "\xff";
      "\x80";
      "\xed\xa0\x80";
      "\xc3";
      "\xf4\x90\x80\x80";
      "\xe0\x80\x80";
      "\xe0\xa0\x80";
    |]
  in
  let state = Random.State.make [| 20261017; 7 |] in
  for _ = 1 to 400 do
    let count = Random.State.int state 48 in
    let text =
      String.concat ""
        (List.init count (fun _ ->
             pieces.(Random.State.int state (Array.length pieces))))
    in
    let index = Lsp_position.index text in
    for offset = -2 to String.length text + 2 do
      let expected = scan_position text offset in
      assert (Lsp_position.position index offset = expected);
      assert (Lsp_position.position_of_offset text offset = expected)
    done;
    let lines = List.length (String.split_on_char '\n' text) in
    for line = -1 to lines + 1 do
      for character = -1 to 12 do
        let expected = scan_offset text (line, character) in
        assert (Lsp_position.offset index (line, character) = expected);
        assert (Lsp_position.offset_of_position text (line, character) = expected)
      done
    done
  done;
  let sample = "x\r\n\t\"\xc3\xa9\"\n\xf0\x9f\x98\x80z" in
  let indexed = Lsp_position.index sample in
  assert (Lsp_position.position indexed 0 = (0, 0));
  assert (Lsp_position.position indexed 3 = (1, 0));
  assert (Lsp_position.position indexed 8 = (1, 4));
  assert (Lsp_position.position indexed 9 = (2, 0));
  assert (Lsp_position.offset indexed (1, 5) = 8);
  assert (Lsp_position.offset indexed (0, 2) = 1);
  assert (Lsp_position.offset indexed (2, 0) = 9);
  assert (Lsp_position.offset indexed (2, 2) = 13);
  assert (Lsp_position.offset indexed (3, 0) = String.length sample)

let () =
  let text = "a\u{e9}\u{1F600}\nb" in
  let position = Lsp_position.position_of_offset text in
  let offset = Lsp_position.offset_of_position text in
  assert (position 0 = (0, 0));
  assert (position 1 = (0, 1));
  assert (position 3 = (0, 2));
  assert (position 7 = (0, 4));
  assert (position 8 = (1, 0));
  assert (position 9 = (1, 1));
  assert (position 100 = (1, 1));
  assert (offset (0, 2) = 3);
  assert (offset (0, 3) = 3);
  assert (offset (0, 4) = 7);
  assert (offset (0, 100) = 7);
  assert (offset (1, 0) = 8);
  assert (offset (1, 9) = 9);
  assert (offset (2, 0) = 9);
  assert (offset (-1, 0) = 9);
  List.iter (fun o -> assert (offset (position o) = o)) [ 0; 1; 3; 7; 8; 9 ];
  let crlf = "a\r\nb" in
  assert (Lsp_position.offset_of_position crlf (0, 5) = 1);
  assert (Lsp_position.position_of_offset crlf 3 = (1, 0));
  assert (Lsp_position.offset_of_position crlf (1, 0) = 3);
  assert (Lsp_position.position_of_offset "\xffa" 2 = (0, 2));
  assert (Lsp_position.offset_of_position "\xffa" (0, 1) = 1);
  let malformed = "missing or invalid Content-Length header" in
  assert (Lsp_frame.take_message "" = Lsp_frame.Incomplete);
  assert (Lsp_frame.take_message "Content-Length: 2\r\n\r" = Lsp_frame.Incomplete);
  assert (Lsp_frame.take_message "Content-Length: 5\r\n\r\nab" = Lsp_frame.Incomplete);
  assert (
    Lsp_frame.take_message "Content-Length: 2\r\n\r\nabcd"
    = Lsp_frame.Message ("ab", "cd"));
  assert (
    Lsp_frame.take_message "content-length:  3 \r\n\r\nxyzW"
    = Lsp_frame.Message ("xyz", "W"));
  assert (
    Lsp_frame.take_message "Content-Length: 2\r\nContent-Type: x\r\n\r\nok"
    = Lsp_frame.Message ("ok", ""));
  assert (
    Lsp_frame.take_message "X-Other: 1\r\n\r\nrest"
    = Lsp_frame.Malformed (malformed, "rest"));
  assert (
    Lsp_frame.take_message "Content-Length: -1\r\n\r\n"
    = Lsp_frame.Malformed (malformed, ""));
  assert (
    Lsp_frame.take_message "Content-Length: 1234567890\r\n\r\n"
    = Lsp_frame.Malformed (malformed, ""));
  assert (
    Lsp_frame.take_message "Content-Length: 1x\r\n\r\n"
    = Lsp_frame.Malformed (malformed, ""));
  let pair = "Content-Length: 1\r\n\r\na" ^ "Content-Length: 2\r\n\r\nbc" in
  assert (
    Lsp_frame.take_message pair = Lsp_frame.Message ("a", "Content-Length: 2\r\n\r\nbc"));
  assert (
    Lsp_frame.take_message "Content-Length: 2\r\n\r\nbc" = Lsp_frame.Message ("bc", ""));
  let split = "Content-Length: 3\r\n\r\nab" in
  assert (Lsp_frame.take_message split = Lsp_frame.Incomplete);
  assert (Lsp_frame.take_message (split ^ "c") = Lsp_frame.Message ("abc", ""));
  assert (
    Lsp_frame.resynchronise "{}Content-Length: 2\r\n\r\nok"
    = (true, "Content-Length: 2\r\n\r\nok"));
  assert (Lsp_frame.resynchronise "CONTENT-LENGTH:1" = (true, "CONTENT-LENGTH:1"));
  assert (Lsp_frame.resynchronise "{}" = (false, ""));
  assert (Lsp_frame.resynchronise "xxConte" = (false, "Conte"));
  assert (Lsp_frame.resynchronise "Content-Length" = (false, "Content-Length"));
  assert (Lsp_frame.resynchronise "" = (false, ""))

let () =
  let directory = Filename.temp_dir "fas-index-" "" in
  let files = [ "probe.h"; "extra.h"; "shapes.fas"; "main.fas" ] in
  let cleanup () =
    List.iter
      (fun name ->
        let path = Filename.concat directory name in
        if Sys.file_exists path then Sys.remove path)
      files;
    Sys.rmdir directory
  in
  Fun.protect ~finally:cleanup @@ fun () ->
  let write name text =
    let channel = open_out_bin (Filename.concat directory name) in
    output_string channel text;
    close_out channel
  in
  write "probe.h"
    "#ifndef PROBE_H\n\
     #define PROBE_H\n\
     #define PROBE_WIDTH 3\n\
     typedef unsigned int probe_word;\n\
     struct probe_box { int count; };\n\
     int probe_add(int a, int b);\n\
     extern int probe_count;\n\
     #endif\n";
  write "extra.h"
    "#include \"probe.h\"\ntypedef int extra_word;\nint extra_twice(int value);\n";
  write "shapes.fas"
    "const SCALE u32 = 4\n\
     const LIMIT u32 = SCALE * 2\n\n\
     struct Point {\n\
    \  x i32\n\
    \  y i32\n\
     }\n\n\
     struct Pair[T] {\n\
    \  left T\n\
    \  right T\n\
     }\n\n\
     fn twice[T](value T) T { return value + value }\n\
     fn scaled[N const u32](value u32) u32 { return value * N }\n\
     fn unused[T](value T) T {\n\
    \  copy T = value\n\
    \  return copy\n\
     }\n\n\
     const GAP usize = offsetof[Point, y]\n";
  write "main.fas"
    "use \"shapes.fas\"\n\
     use \"C\" \"probe.h\"\n\
     use \"C\" \"extra.h\"\n\n\
     var total u32 = LIMIT\n\
     const TABLE arr[2, u8] = {7, 9}\n\n\
     fn helper() i32 { return 0 }\n\n\
     fn main() i32 {\n\
    \  point Point = {. x = 1, .y = 2}\n\
    \  view p = point\n\
    \  p.x = probe_add(p.y, PROBE_WIDTH)\n\
    \  small u8 = 3\n\
    \  wide u32 = 5\n\
    \  pair Pair [u8] = {small, twice(small)}\n\
    \  word probe_word = twice[u32](wide)\n\
    \  total = scaled[2](word) + SCALE\n\
    \  view q = pair\n\
    \  q.left = TABLE[1]\n\
    \  where addr = &helper\n\
    \  box probe_box = {probe_count}\n\
    \  box.count = 2\n\
    \  doubled extra_word = extra_twice(box.count)\n\
    \  offset usize = offsetof[Point, x] + GAP\n\
    \  return p.x\n\
     }\n";
  let main = Filename.concat directory "main.fas" in
  let config =
    match Cli.parse [| "fas"; "check"; main |] with
    | Ok (Cli.Check config) -> config
    | _ -> failwith "expected check invocation"
  in
  let index = Sema_index.create () in
  (match Driver.check ~index config with
  | Ok checked -> Driver.release checked
  | Error diagnostics -> failwith (Diag.render_all ~source:None diagnostics));
  let span (s : Span.t) =
    Printf.sprintf "%s:%d:%d-%d" (Filename.basename s.file) s.line s.column
      (s.column + s.end_offset - s.start_offset)
  in
  let declaration (d : Sema_index.declaration) =
    Printf.sprintf "%s %s %s [%s] %s %s" (Sema_index.kind_name d.kind) d.name
      (span d.name_span) (span d.item_span)
      (Option.value ~default:"-" d.container)
      (match d.c_origin with
      | Some (file, line) -> Printf.sprintf "%s:%d" (Filename.basename file) line
      | None -> "-")
  in
  let expect label expected actual =
    if actual <> expected then (
      List.iter (fun line -> prerr_endline (label ^ " " ^ line)) actual;
      failwith ("semantic index " ^ label ^ " records differ"))
  in
  expect "declaration"
    [
      "global total main.fas:5:5-10 [main.fas:5:1-22] - -";
      "const array TABLE main.fas:6:7-12 [main.fas:6:1-32] - -";
      "function helper main.fas:8:4-10 [main.fas:8:1-29] - -";
      "function main main.fas:10:4-8 [main.fas:10:1-455] - -";
      "local point main.fas:11:3-8 [main.fas:10:1-455] main -";
      "view p main.fas:12:8-9 [main.fas:10:1-455] main -";
      "local small main.fas:14:3-8 [main.fas:10:1-455] main -";
      "local wide main.fas:15:3-7 [main.fas:10:1-455] main -";
      "local pair main.fas:16:3-7 [main.fas:10:1-455] main -";
      "local word main.fas:17:3-7 [main.fas:10:1-455] main -";
      "view q main.fas:19:8-9 [main.fas:10:1-455] main -";
      "local where main.fas:21:3-8 [main.fas:10:1-455] main -";
      "local box main.fas:22:3-6 [main.fas:10:1-455] main -";
      "local doubled main.fas:24:3-10 [main.fas:10:1-455] main -";
      "local offset main.fas:25:3-9 [main.fas:10:1-455] main -";
      "const SCALE shapes.fas:1:7-12 [shapes.fas:1:1-20] - -";
      "const LIMIT shapes.fas:2:7-12 [shapes.fas:2:1-28] - -";
      "struct Point shapes.fas:4:8-13 [shapes.fas:4:1-33] - -";
      "field x shapes.fas:5:3-4 [shapes.fas:4:1-33] Point -";
      "field y shapes.fas:6:3-4 [shapes.fas:4:1-33] Point -";
      "struct Pair shapes.fas:9:8-12 [shapes.fas:9:1-38] - -";
      "type parameter T shapes.fas:9:13-14 [shapes.fas:9:1-38] Pair -";
      "field left shapes.fas:10:3-7 [shapes.fas:9:1-38] Pair -";
      "field right shapes.fas:11:3-8 [shapes.fas:9:1-38] Pair -";
      "function twice shapes.fas:14:4-9 [shapes.fas:14:1-48] - -";
      "type parameter T shapes.fas:14:10-11 [shapes.fas:14:1-48] twice -";
      "parameter value shapes.fas:14:13-18 [shapes.fas:14:1-48] twice -";
      "function scaled shapes.fas:15:4-10 [shapes.fas:15:1-59] - -";
      "const parameter N shapes.fas:15:11-12 [shapes.fas:15:1-59] scaled -";
      "parameter value shapes.fas:15:24-29 [shapes.fas:15:1-59] scaled -";
      "function unused shapes.fas:16:4-10 [shapes.fas:16:1-59] - -";
      "type parameter T shapes.fas:16:11-12 [shapes.fas:16:1-59] unused -";
      "parameter value shapes.fas:16:14-19 [shapes.fas:16:1-59] unused -";
      "local copy shapes.fas:17:3-7 [shapes.fas:16:1-59] unused -";
      "const GAP shapes.fas:21:7-10 [shapes.fas:21:1-37] - -";
      "const PROBE_WIDTH <generated>:1:1-1 [<generated>:1:1-1] - probe.h:3";
      "field count <generated>:1:1-1 [<generated>:1:1-1] probe_box probe.h:5";
      "function extra_twice <generated>:1:1-1 [<generated>:1:1-1] - extra.h:3";
      "type alias extra_word <generated>:1:1-1 [<generated>:1:1-1] - extra.h:2";
      "function probe_add <generated>:1:1-1 [<generated>:1:1-1] - probe.h:6";
      "struct probe_box <generated>:1:1-1 [<generated>:1:1-1] - probe.h:5";
      "global probe_count <generated>:1:1-1 [<generated>:1:1-1] - probe.h:7";
      "type alias probe_word <generated>:1:1-1 [<generated>:1:1-1] - probe.h:4";
    ]
    (List.map declaration (Sema_index.declarations index));
  expect "reference"
    [
      "main.fas:5:17-22 -> const LIMIT shapes.fas:2:7-12 [shapes.fas:2:1-28] - -";
      "main.fas:11:9-14 -> struct Point shapes.fas:4:8-13 [shapes.fas:4:1-33] - -";
      "main.fas:11:20-21 -> field x shapes.fas:5:3-4 [shapes.fas:4:1-33] Point -";
      "main.fas:11:28-29 -> field y shapes.fas:6:3-4 [shapes.fas:4:1-33] Point -";
      "main.fas:12:12-17 -> local point main.fas:11:3-8 [main.fas:10:1-455] main -";
      "main.fas:13:3-4 -> view p main.fas:12:8-9 [main.fas:10:1-455] main -";
      "main.fas:13:5-6 -> field x shapes.fas:5:3-4 [shapes.fas:4:1-33] Point -";
      "main.fas:13:9-18 -> function probe_add <generated>:1:1-1 [<generated>:1:1-1] - \
       probe.h:6";
      "main.fas:13:19-20 -> view p main.fas:12:8-9 [main.fas:10:1-455] main -";
      "main.fas:13:21-22 -> field y shapes.fas:6:3-4 [shapes.fas:4:1-33] Point -";
      "main.fas:13:24-35 -> const PROBE_WIDTH <generated>:1:1-1 [<generated>:1:1-1] - \
       probe.h:3";
      "main.fas:16:8-12 -> struct Pair shapes.fas:9:8-12 [shapes.fas:9:1-38] - -";
      "main.fas:16:21-26 -> local small main.fas:14:3-8 [main.fas:10:1-455] main -";
      "main.fas:16:28-33 -> function twice shapes.fas:14:4-9 [shapes.fas:14:1-48] - -";
      "main.fas:16:34-39 -> local small main.fas:14:3-8 [main.fas:10:1-455] main -";
      "main.fas:17:8-18 -> type alias probe_word <generated>:1:1-1 [<generated>:1:1-1] \
       - probe.h:4";
      "main.fas:17:21-26 -> function twice shapes.fas:14:4-9 [shapes.fas:14:1-48] - -";
      "main.fas:17:32-36 -> local wide main.fas:15:3-7 [main.fas:10:1-455] main -";
      "main.fas:18:3-8 -> global total main.fas:5:5-10 [main.fas:5:1-22] - -";
      "main.fas:18:11-17 -> function scaled shapes.fas:15:4-10 [shapes.fas:15:1-59] - -";
      "main.fas:18:21-25 -> local word main.fas:17:3-7 [main.fas:10:1-455] main -";
      "main.fas:18:29-34 -> const SCALE shapes.fas:1:7-12 [shapes.fas:1:1-20] - -";
      "main.fas:19:12-16 -> local pair main.fas:16:3-7 [main.fas:10:1-455] main -";
      "main.fas:20:3-4 -> view q main.fas:19:8-9 [main.fas:10:1-455] main -";
      "main.fas:20:5-9 -> field left shapes.fas:10:3-7 [shapes.fas:9:1-38] Pair -";
      "main.fas:20:12-17 -> const array TABLE main.fas:6:7-12 [main.fas:6:1-32] - -";
      "main.fas:21:17-23 -> function helper main.fas:8:4-10 [main.fas:8:1-29] - -";
      "main.fas:22:7-16 -> struct probe_box <generated>:1:1-1 [<generated>:1:1-1] - \
       probe.h:5";
      "main.fas:22:20-31 -> global probe_count <generated>:1:1-1 [<generated>:1:1-1] - \
       probe.h:7";
      "main.fas:23:3-6 -> local box main.fas:22:3-6 [main.fas:10:1-455] main -";
      "main.fas:23:7-12 -> field count <generated>:1:1-1 [<generated>:1:1-1] probe_box \
       probe.h:5";
      "main.fas:24:11-21 -> type alias extra_word <generated>:1:1-1 \
       [<generated>:1:1-1] - extra.h:2";
      "main.fas:24:24-35 -> function extra_twice <generated>:1:1-1 [<generated>:1:1-1] \
       - extra.h:3";
      "main.fas:24:36-39 -> local box main.fas:22:3-6 [main.fas:10:1-455] main -";
      "main.fas:24:40-45 -> field count <generated>:1:1-1 [<generated>:1:1-1] \
       probe_box probe.h:5";
      "main.fas:25:27-32 -> struct Point shapes.fas:4:8-13 [shapes.fas:4:1-33] - -";
      "main.fas:25:34-35 -> field x shapes.fas:5:3-4 [shapes.fas:4:1-33] Point -";
      "main.fas:25:39-42 -> const GAP shapes.fas:21:7-10 [shapes.fas:21:1-37] - -";
      "main.fas:26:10-11 -> view p main.fas:12:8-9 [main.fas:10:1-455] main -";
      "main.fas:26:12-13 -> field x shapes.fas:5:3-4 [shapes.fas:4:1-33] Point -";
      "shapes.fas:2:19-24 -> const SCALE shapes.fas:1:7-12 [shapes.fas:1:1-20] - -";
      "shapes.fas:10:8-9 -> type parameter T shapes.fas:9:13-14 [shapes.fas:9:1-38] \
       Pair -";
      "shapes.fas:11:9-10 -> type parameter T shapes.fas:9:13-14 [shapes.fas:9:1-38] \
       Pair -";
      "shapes.fas:14:19-20 -> type parameter T shapes.fas:14:10-11 \
       [shapes.fas:14:1-48] twice -";
      "shapes.fas:14:22-23 -> type parameter T shapes.fas:14:10-11 \
       [shapes.fas:14:1-48] twice -";
      "shapes.fas:14:33-38 -> parameter value shapes.fas:14:13-18 [shapes.fas:14:1-48] \
       twice -";
      "shapes.fas:14:41-46 -> parameter value shapes.fas:14:13-18 [shapes.fas:14:1-48] \
       twice -";
      "shapes.fas:15:48-53 -> parameter value shapes.fas:15:24-29 [shapes.fas:15:1-59] \
       scaled -";
      "shapes.fas:15:56-57 -> const parameter N shapes.fas:15:11-12 \
       [shapes.fas:15:1-59] scaled -";
      "shapes.fas:21:28-33 -> struct Point shapes.fas:4:8-13 [shapes.fas:4:1-33] - -";
      "shapes.fas:21:35-36 -> field y shapes.fas:6:3-4 [shapes.fas:4:1-33] Point -";
    ]
    (List.map
       (fun (r : Sema_index.reference) ->
         span r.use_span ^ " -> " ^ declaration r.declaration)
       (Sema_index.references index));
  expect "type"
    [
      "main.fas:5:17-22 [] u32 8";
      "main.fas:6:17-18 [] usize 2";
      "main.fas:6:27-28 [] u8 7";
      "main.fas:6:30-31 [] u8 9";
      "main.fas:8:26-27 [] i32 -";
      "main.fas:11:17-18 [] Point -";
      "main.fas:11:24-25 [] i32 -";
      "main.fas:11:32-33 [] i32 -";
      "main.fas:12:12-17 [] Point -";
      "main.fas:13:3-4 [] Point -";
      "main.fas:13:3-6 [] i32 -";
      "main.fas:13:9-36 [] i32 -";
      "main.fas:13:19-20 [] Point -";
      "main.fas:13:19-22 [] i32 -";
      "main.fas:13:24-35 [] i32 3";
      "main.fas:14:14-15 [] u8 -";
      "main.fas:15:14-15 [] u32 -";
      "main.fas:16:20-21 [] Pair[u8] -";
      "main.fas:16:21-26 [] u8 -";
      "main.fas:16:28-40 [] u8 -";
      "main.fas:16:34-39 [] u8 -";
      "main.fas:17:21-37 [] u32 -";
      "main.fas:17:32-36 [] u32 -";
      "main.fas:18:11-26 [] u32 -";
      "main.fas:18:11-34 [] u32 -";
      "main.fas:18:18-19 [] u32 2";
      "main.fas:18:21-25 [] u32 -";
      "main.fas:18:29-34 [] u32 4";
      "main.fas:19:12-16 [] Pair[u8] -";
      "main.fas:20:3-4 [] Pair[u8] -";
      "main.fas:20:3-9 [] u8 -";
      "main.fas:20:12-17 [] arr[2, u8] -";
      "main.fas:20:12-20 [] u8 -";
      "main.fas:20:18-19 [] i32 -";
      "main.fas:21:16-17 [] addr -";
      "main.fas:22:19-20 [] probe_box -";
      "main.fas:22:20-31 [] i32 -";
      "main.fas:23:3-6 [] probe_box -";
      "main.fas:23:3-12 [] i32 -";
      "main.fas:23:15-16 [] i32 -";
      "main.fas:24:24-46 [] i32 -";
      "main.fas:24:36-39 [] probe_box -";
      "main.fas:24:36-45 [] i32 -";
      "main.fas:25:18-26 [] usize -";
      "main.fas:25:18-42 [] usize -";
      "main.fas:25:39-42 [] usize 4";
      "main.fas:26:10-11 [] Point -";
      "main.fas:26:10-13 [] i32 -";
      "shapes.fas:1:19-20 [] u32 4";
      "shapes.fas:2:19-24 [] u32 4";
      "shapes.fas:2:19-28 [] u32 8";
      "shapes.fas:2:27-28 [] u32 2";
      "shapes.fas:14:33-38 [u32] u32 -";
      "shapes.fas:14:33-38 [u8] u8 -";
      "shapes.fas:14:33-46 [u32] u32 -";
      "shapes.fas:14:33-46 [u8] u8 -";
      "shapes.fas:14:41-46 [u32] u32 -";
      "shapes.fas:14:41-46 [u8] u8 -";
      "shapes.fas:15:48-53 [2] u32 -";
      "shapes.fas:15:48-57 [2] u32 -";
      "shapes.fas:15:56-57 [2] u32 2";
      "shapes.fas:21:19-27 [] usize 4";
    ]
    (List.map
       (fun (t : Sema_index.expression_type) ->
         Printf.sprintf "%s [%s] %s %s" (span t.span)
           (String.concat "," t.instance)
           t.ty
           (Option.value ~default:"-" t.value))
       (Sema_index.expression_types index));
  expect "generic call"
    [
      "main.fas:16:28-33 [] u8 false";
      "main.fas:17:21-26 [] u32 true";
      "main.fas:18:11-17 [] 2 true";
    ]
    (List.map
       (fun (g : Sema_index.generic_call) ->
         Printf.sprintf "%s [%s] %s %b" (span g.callee_span)
           (String.concat "," g.instance)
           (String.concat "," g.arguments)
           g.explicit)
       (Sema_index.generic_calls index))

let () =
  let base = "a\nb\nc" in
  assert (Lsp_map.to_current (Lsp_map.create ~analyzed:base ~current:base) 1 = Some 1);
  assert (Lsp_map.to_analyzed (Lsp_map.create ~analyzed:base ~current:base) 2 = Some 2);
  let inserted = "x\ny\na\nb\nc" in
  let above = Lsp_map.create ~analyzed:base ~current:inserted in
  assert (Lsp_map.to_current above 0 = Some 2);
  assert (Lsp_map.to_current above 1 = Some 3);
  assert (Lsp_map.to_analyzed above 3 = Some 1);
  assert (Lsp_map.to_analyzed above 1 = None);
  let deleted = "a\nb" in
  let removed = "x\na\nb" in
  let above_delete = Lsp_map.create ~analyzed:removed ~current:deleted in
  assert (Lsp_map.to_current above_delete 1 = Some 0);
  assert (Lsp_map.to_current above_delete 2 = Some 1);
  assert (Lsp_map.to_current above_delete 0 = None);
  let edited = "a\nB\nc" in
  let region = Lsp_map.create ~analyzed:base ~current:edited in
  assert (Lsp_map.to_current region 1 = None);
  assert (Lsp_map.to_current region 0 = Some 0);
  assert (Lsp_map.to_current region 2 = Some 2);
  assert (Lsp_map.to_analyzed region 1 = None);
  let below = "a\nb\nc\nd\ne" in
  let broken = "a\nB\nC\nd\ne" in
  let below_region = Lsp_map.create ~analyzed:below ~current:broken in
  assert (Lsp_map.to_current below_region 1 = None);
  assert (Lsp_map.to_current below_region 2 = None);
  assert (Lsp_map.to_current below_region 3 = Some 3);
  assert (Lsp_map.to_current below_region 4 = Some 4);
  assert (Lsp_map.to_analyzed below_region 2 = None);
  assert (Lsp_map.to_analyzed below_region 3 = Some 3);
  assert (Lsp_map.to_analyzed below_region 4 = Some 4);
  let shifted = "x\ny\nalpha\nb\nc\nd\ne" in
  let shifted_map = Lsp_map.create ~analyzed:below ~current:shifted in
  assert (Lsp_map.to_current shifted_map 1 = Some 3);
  assert (Lsp_map.to_current shifted_map 4 = Some 6);
  assert (Lsp_map.to_current shifted_map 9 = None)

let () =
  let before = Build_limit.get_soft () in
  let three_gib = 3 * 1024 * 1024 * 1024 in
  Build_limit.set_soft three_gib;
  assert (Build_limit.get_soft () = three_gib);
  Build_limit.set_soft before;
  assert (Build_limit.get_soft () = before);
  let expected = if before < 0 then Build_limit.cap else min Build_limit.cap before in
  Build_limit.with_cap (fun () -> assert (Build_limit.get_soft () = expected));
  assert (Build_limit.get_soft () = before);
  (try Build_limit.with_cap (fun () -> failwith "probe") with Failure _ -> ());
  assert (Build_limit.get_soft () = before)
