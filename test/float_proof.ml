external unary : bool -> int -> int64 -> int64 = "proof_unary"
external binary : bool -> int -> int64 -> int64 -> int64 = "proof_binary"
external fused : bool -> int64 -> int64 -> int64 -> int64 = "proof_fma"
external to_int : bool -> int -> bool -> int64 -> int64 = "proof_to_int"
external from_int : bool -> int -> bool -> int64 -> int64 = "proof_from_int"
external literal_cases : bool -> int64 -> (string * int64) array = "proof_literals"
external load_runtime : string -> unit = "proof_load"

external runtime : int -> int64 array -> int64 array -> int64 array -> int64 array
  = "proof_runtime"

external boundaries : bool -> (string * int64) array = "proof_boundaries"
external parse_literal : bool -> string -> int64 = "proof_parse_literal"

let full = Sys.getenv_opt "FULL" = Some "1"

let nan f32 bits =
  let exponent, fraction =
    if f32 then (0x7f800000L, 0x7fffffL) else (0x7ff0000000000000L, 0xfffffffffffffL)
  in
  Int64.logand bits exponent = exponent && Int64.logand bits fraction <> 0L

let quiet f32 bits =
  Int64.logand bits (if f32 then 0x400000L else 0x8000000000000L) <> 0L

let check ?(allow_nan = false) f32 label input actual expected =
  if
    (allow_nan && nan f32 actual && not (quiet f32 actual))
    || actual <> expected
       && not
            (allow_nan && nan f32 actual && nan f32 expected && quiet f32 actual
           && quiet f32 expected)
  then
    failwith
      (Printf.sprintf "%s input=%016Lx actual=%016Lx expected=%016Lx" label input actual
         expected)

let ty f32 = Hir.Float (if f32 then Hir.F32 else Hir.F64)
let mask32 x = Int64.logand x 0xffffffffL
let worker = Domain.DLS.new_key (fun () -> 0)
let workers = if full then 6 else 1

let state_key =
  Domain.DLS.new_key (fun () ->
      ref
        (Int64.logxor 0x9e3779b97f4a7c15L
           (Int64.mul 0xd1b54a32d192ed03L (Int64.of_int (Domain.DLS.get worker)))))

let random () =
  let state = Domain.DLS.get state_key in
  let x = Int64.logxor !state (Int64.shift_left !state 13) in
  let x = Int64.logxor x (Int64.shift_right_logical x 7) in
  let x = Int64.logxor x (Int64.shift_left x 17) in
  state := x;
  x

let edges32 =
  [|
    0L;
    0x80000000L;
    1L;
    0x80000001L;
    0x7fffffL;
    0x807fffffL;
    0x800000L;
    0x80800000L;
    0x7f7fffffL;
    0xff7fffffL;
    0x7f800000L;
    0xff800000L;
    0x7f800001L;
    0xff812345L;
    0x7fc00001L;
    0xffc12345L;
    0x3f000000L;
    0xbf000000L;
    0x3f800000L;
    0xbf800000L;
    0x3fc00000L;
    0x4b000001L;
    0x4b800001L;
    0x4f000000L;
    0xcf000000L;
    0x4f800000L;
    0x5f000000L;
    0xdf000000L;
    0x5f800000L;
    0x43000000L;
    0xc3000000L;
    0x43800000L;
    0x47000000L;
    0xc7000000L;
    0x47800000L;
  |]

let edges64 =
  [|
    0L;
    Int64.min_int;
    1L;
    0x8000000000000001L;
    0xfffffffffffffL;
    0x800fffffffffffffL;
    0x10000000000000L;
    0x8010000000000000L;
    0x7fefffffffffffffL;
    0xffefffffffffffffL;
    0x7ff0000000000000L;
    0xfff0000000000000L;
    0x7ff0000000000001L;
    0xfff0123456789abcL;
    0x7ff8000000000001L;
    0xfff8123456789abcL;
    0x3fe0000000000000L;
    0xbfe0000000000000L;
    0x3ff0000000000000L;
    0xbff0000000000000L;
    0x3ff8000000000000L;
    0x4330000000000001L;
    0x4340000000000001L;
    0x3ff0000010000000L;
    0x3ff0000030000000L;
    0x3690000000000000L;
    0x47effffff0000000L;
    0x43e0000000000000L;
    0xc3e0000000000000L;
    0x43f0000000000000L;
    0x4060000000000000L;
    0xc060000000000000L;
    0x4070000000000000L;
    0x40e0000000000000L;
    0xc0e0000000000000L;
    0x40f0000000000000L;
    0x41e0000000000000L;
    0xc1e0000000000000L;
    0x41f0000000000000L;
  |]

let sample f32 edge =
  let raw = random () in
  let raw = if f32 then mask32 raw else raw in
  if not edge then raw
  else
    let edges = if f32 then edges32 else edges64 in
    let base =
      edges.(Int64.to_int (Int64.logand (random ()) 0x7fffffffL) mod Array.length edges)
    in
    let offset = Int64.sub (Int64.logand raw 15L) 8L in
    let result = Int64.add base offset in
    if f32 then mask32 result else result

let span = Span.synthetic
let ident name = Ast.Ident (name, span)
let ast_float f32 = Ast.Float (if f32 then Ast.F32 else Ast.F64)

let cvt target =
  Ast.Call
    (Ast.Generic_args (ident "cvt", [ Ast.Type_arg target ], span), [ ident "a" ], span)

let evaluate values expression =
  match Sema_constants.const_expr values None expression with
  | Ok (Hir.Float Hir.F32, value) -> mask32 value
  | Ok (_, value) -> value
  | Error errors -> failwith (Diag.render_all ~source:None errors)

let unary_expr f32 =
  Array.init 7 (fun op ->
      match op with
      | 5 -> Ast.Unary (Ast.Neg, ident "a", span)
      | 6 -> cvt (ast_float (not f32))
      | _ ->
          Ast.Call
            ( ident [| "sqrt"; "floor"; "ceil"; "round"; "abs" |].(op),
              [ ident "a" ],
              span ))

let unary32 = unary_expr true
let unary64 = unary_expr false

let expression_unary f32 op raw =
  evaluate [ ("a", ty f32, raw) ] (if f32 then unary32.(op) else unary64.(op))

let binary_expr =
  Array.map
    (fun op -> Ast.Binary (op, ident "a", ident "b", span))
    [|
      Ast.Add; Ast.Sub; Ast.Mul; Ast.Div; Ast.Eq; Ast.Ne; Ast.Lt; Ast.Le; Ast.Gt; Ast.Ge;
    |]

let expression_binary f32 op a b =
  evaluate [ ("a", ty f32, a); ("b", ty f32, b) ] binary_expr.(op)

let fma_expr = Ast.Call (ident "fma", [ ident "a"; ident "b"; ident "c" ], span)

let expression_fma f32 a b c =
  evaluate [ ("a", ty f32, a); ("b", ty f32, b); ("c", ty f32, c) ] fma_expr

let const_unary f32 op raw =
  match op with
  | 0 | 1 | 2 | 3 | 4 -> Float_parse.float_unary_bits op f32 raw
  | 5 -> Int64.logxor raw (if f32 then 0x80000000L else Int64.min_int)
  | _ when f32 ->
      Sema_types.widen_integer_value Ast.Float_widen (ty true) (ty false) raw
  | _ -> mask32 (Int64.of_int32 (Int32.bits_of_float (Int64.float_of_bits raw)))

let operations =
  [|
    Ast.Add; Ast.Sub; Ast.Mul; Ast.Div; Ast.Eq; Ast.Ne; Ast.Lt; Ast.Le; Ast.Gt; Ast.Ge;
  |]

let const_binary f32 op a b =
  let operation = operations.(op) in
  if op < 4 then Sema_constants.float_binary_bits (ty f32) operation a b
  else Sema_constants.float_comparison_bits (ty f32) operation a b

let const_fma f32 a b c = Float_parse.float_fma_bits f32 a b c

let int_types =
  [
    (8, false, Ast.U8, Hir.U8);
    (8, true, Ast.I8, Hir.I8);
    (16, false, Ast.U16, Hir.U16);
    (16, true, Ast.I16, Hir.I16);
    (32, false, Ast.U32, Hir.U32);
    (32, true, Ast.I32, Hir.I32);
    (64, false, Ast.U64, Hir.U64);
    (64, true, Ast.I64, Hir.I64);
  ]

let unary_case f32 raw =
  for op = 0 to 6 do
    check
      ~allow_nan:(op < 4 || (op = 6 && not f32))
      (if op = 6 then not f32 else f32)
      [| "sqrt"; "floor"; "ceil"; "round"; "abs"; "neg"; "format" |].(op)
      raw (const_unary f32 op raw) (unary f32 op raw)
  done;
  List.iter
    (fun (width, signed, _, _) ->
      check f32 "float-to-int" raw
        (Float_parse.float_to_int_bits raw (if f32 then 32 else 64) width signed)
        (to_int f32 width signed raw))
    int_types;
  List.iter
    (fun b ->
      for op = 4 to 9 do
        check f32 "comparison" raw (const_binary f32 op raw b) (binary f32 op raw b)
      done)
    (if f32 then [ 0L; 0x3f800000L; 0xff800000L ]
     else [ 0L; 0x3ff0000000000000L; 0xfff0000000000000L ])

let integer_case width raw =
  List.iter
    (fun (bits, signed, _, _) ->
      if bits = width then
        List.iter
          (fun f32 ->
            check f32 "int-to-float" raw
              (Float_parse.int_to_float_bits raw width signed f32)
              (from_int f32 width signed raw))
          [ true; false ])
    int_types

let timed name count run =
  let id = Domain.DLS.get worker in
  let first = count * id / workers and last = count * (id + 1) / workers in
  let start = Unix.gettimeofday () in
  run first last;
  Printf.printf "%s worker=%d count=%d seconds=%.3f\n%!" name id (last - first)
    (Unix.gettimeofday () -. start)

let progress name first current =
  if full && current > first && current land 0xfffff = 0 then
    Printf.printf "%s worker=%d completed=%d\n%!" name (Domain.DLS.get worker)
      (current - first)

let expression_pins () =
  List.iter
    (fun f32 ->
      Array.iter
        (fun a ->
          for op = 0 to 6 do
            check
              ~allow_nan:(op < 4 || (op = 6 && not f32))
              (if op = 6 then not f32 else f32)
              "expression/kernel unary" a (expression_unary f32 op a)
              (const_unary f32 op a)
          done;
          Array.iter
            (fun b ->
              for op = 0 to 9 do
                check ~allow_nan:(op < 4) f32 "expression/kernel binary" a
                  (expression_binary f32 op a b)
                  (const_binary f32 op a b)
              done;
              check ~allow_nan:true f32 "expression/kernel fma" a
                (expression_fma f32 a b a) (const_fma f32 a b a))
            (if f32 then edges32 else edges64))
        (if f32 then edges32 else edges64))
    [ true; false ]

let unary_run () =
  Array.iter (unary_case true) edges32;
  Array.iter (unary_case false) edges64;
  let count = if full then 1 lsl 32 else 10_000 in
  timed "f32 unary patterns" count (fun first last ->
      for i = first to last - 1 do
        progress "f32 unary" first i;
        unary_case true (if full then Int64.of_int i else sample true (i land 1 = 0))
      done);
  timed "u32/i32 conversions" count (fun first last ->
      for i = first to last - 1 do
        progress "u32/i32" first i;
        integer_case 32 (if full then Int64.of_int i else random ())
      done);
  timed "16-bit conversions" 65536 (fun first last ->
      for i = first to last - 1 do
        integer_case 16 (Int64.of_int i)
      done);
  timed "8-bit conversions" 256 (fun first last ->
      for i = first to last - 1 do
        integer_case 8 (Int64.of_int i)
      done)

let hard_cases () =
  let a = 0x3ff0000000000000L and b = 0x3ca0000008000000L in
  check false "extended precision double rounding sum" a (const_binary false 0 a b)
    0x3ff0000000000001L;
  check false "extended precision double rounding fma" a (const_fma false a a b)
    0x3ff0000000000001L;
  List.iter
    (fun (a, b, c) ->
      let expected = fused true a b c in
      check true "fma double rounding" a (const_fma true a b c) expected;
      let a64 = Int32.float_of_bits (Int64.to_int32 a) in
      let b64 = Int32.float_of_bits (Int64.to_int32 b) in
      let c64 = Int32.float_of_bits (Int64.to_int32 c) in
      let rounded =
        mask32 (Int64.of_int32 (Int32.bits_of_float ((a64 *. b64) +. c64)))
      in
      if expected = rounded then
        failwith "fma hard case does not distinguish double rounding")
    [
      (0xd0300000L, 0xd63d6b18L, 0x10948057L);
      (0xe36967c8L, 0x3d900000L, 0x8f93c7c8L);
      (0x3a6549a8L, 0xf9500000L, 0xd35fe9a8L);
      (0x53008000L, 0x23149180L, 0x95bdeb52L);
    ];
  let ties =
    [
      0x8000008000000000L;
      0x8000008000000001L;
      0x8000007fffffffffL;
      0x7fffff4000000000L;
      0x7fffff4000000001L;
      0x20000000000001L;
    ]
  in
  List.iter (integer_case 64) ties;
  List.iter
    (fun signed ->
      if
        not
          (List.exists
             (fun raw ->
               let direct = from_int true 64 signed raw in
               let twice =
                 mask32
                   (Int64.of_int32
                      (Int32.bits_of_float
                         (Int64.float_of_bits (from_int false 64 signed raw))))
               in
               direct <> twice)
             ties)
      then failwith "integer hard cases do not distinguish double rounding")
    [ true; false ]

let binary_run () =
  hard_cases ();
  List.iter
    (fun f32 ->
      let count = if full then if f32 then 200_000_000 else 50_000_000 else 10_000 in
      timed
        (if f32 then "f32 binary per operation" else "f64 binary per operation")
        count
        (fun first last ->
          for i = first to last - 1 do
            progress (if f32 then "f32 binary/fma" else "f64 binary/fma") first i;
            let a = sample f32 (i land 1 = 0) and b = sample f32 (i land 1 = 0) in
            for op = 0 to 9 do
              check ~allow_nan:(op < 4) f32
                [| "add"; "sub"; "mul"; "div"; "eq"; "ne"; "lt"; "le"; "gt"; "ge" |].(op)
                a (const_binary f32 op a b) (binary f32 op a b)
            done
          done);
      let count = if full then 50_000_000 else 10_000 in
      timed
        (if f32 then "f32 fma" else "f64 fma/unary/conversion")
        count
        (fun first last ->
          for i = first to last - 1 do
            progress (if f32 then "f32 binary/fma" else "f64 binary/fma") first i;
            let a = sample f32 (i land 1 = 0) and b = sample f32 (i land 1 = 0) in
            let c =
              if i land 3 = 0 then
                Int64.logxor (const_binary f32 2 a b)
                  (if f32 then 0x80000000L else Int64.min_int)
              else sample f32 (i land 1 = 0)
            in
            check ~allow_nan:true f32 "fma" a (const_fma f32 a b c) (fused f32 a b c);
            if not f32 then (
              unary_case false a;
              List.iter (fun width -> integer_case width (random ())) [ 8; 16; 32; 64 ])
          done))
    [ true; false ]

let finite f32 bits =
  let mask = if f32 then 0x7f800000L else 0x7ff0000000000000L in
  Int64.logand bits mask <> mask

let literal_case f32 raw =
  if finite f32 raw then
    Array.iteri
      (fun i (text, expected) ->
        let magnitude =
          Int64.logand expected (if f32 then 0x7fffffffL else Int64.max_int)
        in
        let nonzero =
          String.exists
            (fun c -> c >= '1' && c <= '9')
            (List.hd (String.split_on_char 'e' text))
        in
        let rejected = (not (finite f32 expected)) || (magnitude = 0L && nonzero) in
        let negative = text.[0] = '-' in
        let positive =
          if negative then String.sub text 1 (String.length text - 1) else text
        in
        match Sema_numeric.parse_float_literal (ty f32) positive with
        | Error _ when rejected -> ()
        | Ok actual when not rejected ->
            let parsed =
              if negative then
                Int64.logxor actual (if f32 then 0x80000000L else Int64.min_int)
              else actual
            in
            check f32 "decimal parser" raw parsed expected;
            if i < 3 then check f32 "decimal round trip" raw parsed raw
        | Error message ->
            failwith ("unexpected literal rejection: " ^ text ^ ": " ^ message)
        | Ok _ -> failwith ("missing literal rejection: " ^ text))
      (literal_cases f32 raw)

let literal_run ?(formats = [ true; false ]) ?(offset = 0) ?limit () =
  List.iter
    (fun f32 ->
      List.iter
        (fun text ->
          let expected = parse_literal f32 text in
          let rejected = (not (finite f32 expected)) || expected = 0L in
          match Sema_numeric.parse_float_literal (ty f32) text with
          | Ok actual when not rejected ->
              check f32 "long and extreme literal" 0L actual expected
          | Error _ when rejected -> ()
          | Error message -> failwith message
          | Ok _ -> failwith "long literal rejection missing")
        [
          "1." ^ String.make 798 '0' ^ "1";
          "9." ^ String.make 798 '9' ^ "e-1";
          "1.0e999999";
          "1.0e-999999";
          "1.0e-45";
          "1.0e-323";
        ];
      Array.iter
        (fun (text, expected) ->
          let parsed =
            if f32 then Float_parse.f32_bits text else Float_parse.f64_bits text
          in
          check f32 "literal exact boundary" 0L parsed expected;
          let reject = (not (finite f32 expected)) || expected = 0L in
          match Sema_numeric.parse_float_literal (ty f32) text with
          | Error message when reject ->
              let wanted =
                Printf.sprintf
                  (if expected = 0L then
                     "nonzero floating point literal rounds to zero for `%s`"
                   else "floating point literal rounds to infinity for `%s`")
                  (Hir.ty_name (ty f32))
              in
              if message <> wanted then
                failwith ("unexpected boundary diagnostic: " ^ message)
          | Ok actual when not reject ->
              check f32 "boundary accepted bits" 0L actual expected
          | Error message -> failwith message
          | Ok _ -> failwith "boundary rejection missing")
        (boundaries f32);
      Array.iter (literal_case f32) (if f32 then edges32 else edges64);
      let count = if full then if f32 then 2 * 0x7f800000 else 100_000_000 else 1000 in
      let count =
        match limit with
        | None -> count
        | Some requested
          when offset >= 0 && requested > 0 && requested <= count - offset ->
            requested
        | Some _ -> invalid_arg "literal range outside requested coverage"
      in
      let state = Domain.DLS.get state_key in
      state := Int64.logxor !state (Int64.of_int offset);
      timed
        (if f32 then "f32 finite literals and neighbours"
         else "f64 literals and neighbours")
        count
        (fun first last ->
          for i = first to last - 1 do
            progress (if f32 then "f32 literal" else "f64 literal") first i;
            let i = offset + i in
            let raw =
              if full && f32 then
                if i < 0x7f800000 then Int64.of_int i
                else Int64.logor 0x80000000L (Int64.of_int (i - 0x7f800000))
              else
                let rec pick () =
                  let x = sample f32 (i land 1 = 0) in
                  if finite f32 x && (f32 || finite false (Int64.add x 1L)) then x
                  else pick ()
                in
                pick ()
            in
            literal_case f32 raw
          done))
    formats

let integer_lanes f32 width a =
  let source_bytes = if f32 then 4 else 8 in
  Array.init (Array.length a) (fun lane ->
      let raw = ref 0L in
      for byte = 0 to (width / 8) - 1 do
        let offset = (lane * (width / 8)) + byte in
        let element = offset / source_bytes in
        let shift = 8 * (offset mod source_bytes) in
        let part =
          if element < Array.length a then
            Int64.logand (Int64.shift_right_logical a.(element) shift) 255L
          else 0L
        in
        raw := Int64.logor !raw (Int64.shift_left part (8 * byte))
      done;
      !raw)

let constant_results f32 scalar lanes a b c =
  let shape t = if scalar then t else Hir.Vec (lanes, t) in
  let ast_shape t =
    if scalar then t else Ast.Vec (Ast.int_aggregate_length lanes span, t)
  in
  let bindings =
    [ ("a", shape (ty f32), a); ("b", shape (ty f32), b); ("c", shape (ty f32), c) ]
  in
  let evaluate bindings expression =
    if scalar then
      [|
        evaluate
          (List.map (fun (name, t, values) -> (name, t, values.(0))) bindings)
          expression;
      |]
    else
      match
        Sema_constants.vector_const_expr
          ~arrays:
            (List.map
               (fun (name, t, values) -> (name, t, Array.to_list values))
               bindings)
          [] None expression
      with
      | Ok (Hir.Vec (_, Hir.Float Hir.F32), values) ->
          Array.of_list (List.map mask32 values)
      | Ok (_, values) -> Array.of_list values
      | Error errors -> failwith (Diag.render_all ~source:None errors)
  in
  let expressions =
    Array.to_list (Array.sub binary_expr 0 4)
    @ Array.to_list (Array.sub (if f32 then unary32 else unary64) 0 6)
    @ [ fma_expr ]
    @ Array.to_list (Array.sub binary_expr 4 6)
  in
  let results = List.map (evaluate bindings) expressions in
  let results =
    if lanes <= 32 then
      results @ [ evaluate bindings (cvt (ast_shape (ast_float (not f32)))) ]
    else results
  in
  let results =
    List.fold_left
      (fun results (width, signed, ast_kind, hir_kind) ->
        if lanes * width > 2048 then results
        else
          let integers = integer_lanes f32 width a in
          let integer_ty = Hir.Int hir_kind in
          let integers =
            if signed then
              Array.map (Sema_numeric.sign_extend_value integer_ty) integers
            else integers
          in
          results
          @ [
              evaluate bindings (cvt (ast_shape (Ast.Int ast_kind)));
              evaluate
                [ ("a", shape integer_ty, integers) ]
                (cvt (ast_shape (ast_float f32)));
            ])
      results int_types
  in
  Array.of_list results

let runtime_run group =
  List.iter
    (fun f32 ->
      let maximum = if f32 then 64 else 32 in
      for shape_index = 0 to maximum do
        let scalar = shape_index = 0 in
        let lanes = max 1 shape_index in
        if
          (if f32 then 0 else 4) + ((lanes - 1) / 16) = group
          && (full || List.mem lanes [ 1; 2; 3; 4; 8; 16; 32; 64 ])
        then
          let cases = if full then (1_000_000 + lanes - 1) / lanes else 8 in
          timed
            (Printf.sprintf "runtime f%d %s=%d"
               (if f32 then 32 else 64)
               (if scalar then "scalar" else "lanes")
               lanes)
            cases
            (fun first last ->
              for i = if first = 0 then -1 else first to last - 1 do
                let a = Array.init lanes (fun _ -> sample f32 true) in
                let b = Array.init lanes (fun _ -> sample f32 true) in
                let c = Array.init lanes (fun _ -> sample f32 true) in
                let edges = if f32 then edges32 else edges64 in
                if i = -1 then
                  List.iteri
                    (fun index values ->
                      Array.iteri
                        (fun lane _ ->
                          values.(lane) <-
                            edges.((lane + (index * 7)) mod Array.length edges))
                        values)
                    [ a; b; c ];
                let encoded = (if f32 then 0 else 100) + shape_index in
                let actual =
                  runtime (if i = -1 then -1000 - encoded else encoded) a b c
                in
                let constants = constant_results f32 scalar lanes a b c in
                let check_result operation lane target allow expected =
                  check ~allow_nan:allow target "const expression versus kernel/C"
                    a.(lane)
                    constants.(operation).(lane)
                    expected;
                  check ~allow_nan:allow target "runtime versus const/C" a.(lane)
                    actual.((operation * lanes) + lane)
                    expected
                in
                for lane = 0 to lanes - 1 do
                  for op = 0 to 3 do
                    let expected = const_binary f32 op a.(lane) b.(lane) in
                    check ~allow_nan:true f32 "const/C binary" a.(lane) expected
                      (binary f32 op a.(lane) b.(lane));
                    check_result op lane f32 true expected
                  done;
                  for op = 0 to 5 do
                    let expected = const_unary f32 op a.(lane) in
                    check ~allow_nan:(op < 4) f32 "const/C unary" a.(lane) expected
                      (unary f32 op a.(lane));
                    check_result (4 + op) lane f32 (op < 4) expected
                  done;
                  let expected = const_fma f32 a.(lane) b.(lane) c.(lane) in
                  check ~allow_nan:true f32 "const/C fma" a.(lane) expected
                    (fused f32 a.(lane) b.(lane) c.(lane));
                  check_result 10 lane f32 true expected;
                  for op = 4 to 9 do
                    let expected = const_binary f32 op a.(lane) b.(lane) in
                    check f32 "const/C comparison" a.(lane) expected
                      (binary f32 op a.(lane) b.(lane));
                    check_result (11 + op - 4) lane f32 false expected
                  done;
                  let operation = ref 17 in
                  if lanes <= 32 then (
                    let expected = const_unary f32 6 a.(lane) in
                    check ~allow_nan:(not f32) (not f32) "const/C format conversion"
                      a.(lane) expected
                      (unary f32 6 a.(lane));
                    check_result !operation lane (not f32) (not f32) expected;
                    incr operation);
                  List.iter
                    (fun (width, signed, _, _) ->
                      if lanes * width <= 2048 then (
                        let expected =
                          Float_parse.float_to_int_bits a.(lane)
                            (if f32 then 32 else 64)
                            width signed
                        in
                        check f32 "const/C saturation" a.(lane) expected
                          (to_int f32 width signed a.(lane));
                        check_result !operation lane f32 false expected;
                        incr operation;
                        let raw = (integer_lanes f32 width a).(lane) in
                        let expected =
                          Float_parse.int_to_float_bits raw width signed f32
                        in
                        check f32 "const/C integer conversion" raw expected
                          (from_int f32 width signed raw);
                        check_result !operation lane f32 false expected;
                        incr operation))
                    int_types
                done
              done)
      done)
    [ true; false ]

let () =
  expression_pins ();
  ignore (Float_parse.f32_bits "1.0");
  let section = if Array.length Sys.argv > 1 then Sys.argv.(1) else "all" in
  if section = "runtime" then load_runtime Sys.argv.(2);
  let run () =
    match section with
    | "unary" -> unary_run ()
    | "binary" -> binary_run ()
    | "literal" -> literal_run ()
    | "literal32" | "literal64" ->
        if not full then invalid_arg "literal ranges require FULL=1";
        literal_run
          ~formats:[ section = "literal32" ]
          ~offset:(int_of_string Sys.argv.(2))
          ~limit:(int_of_string Sys.argv.(3))
          ()
    | "runtime" -> runtime_run (int_of_string Sys.argv.(3))
    | "all" ->
        unary_run ();
        binary_run ();
        literal_run ()
    | _ ->
        failwith "expected unary, binary, literal, literal32, literal64, runtime or all"
  in
  let domains =
    List.init (workers - 1) (fun i ->
        Domain.spawn (fun () ->
            Domain.DLS.set worker (i + 1);
            run ()))
  in
  run ();
  List.iter Domain.join domains
