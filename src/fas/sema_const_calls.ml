(* Constants from calls (roadmap v0.4, survey D2): a top-level constant whose
   initializer calls a Fas function, and a top-level `const assert` whose
   condition does, are evaluated before the main semantic check by the Fir
   interpreter.

   For each such constant `const X T = init`, in dependency order:
   1. take the items `init` reaches by name (functions, constants, globals),
      plus every type, error, `use` and body-less function declaration;
   2. add `fn __fas_const_X() T { return init }`;
   3. check and lower that reduced program like any other, then run the
      function in the interpreter under its compile-time configuration (no C
      calls, no system calls, no `var` globals, a step limit);
   4. replace `init` with the literal result.

   A constant the reached code itself uses is a cycle and an error. A failed
   `assert`, a trap, a C call, a system call, touching a `var` and running out
   of steps are compile errors at the constant. The rest of the program then
   sees only literals, so every later phase is unchanged. Programs with no
   such constant skip all of this.

   A `const NAME T = const { BODY }` fill runs the same way, with two changes:
   the reduced program declares `var NAME T` (zeroed) instead of the constant,
   and the function is `fn __fas_const_NAME() void { BODY }`. The interpreter
   lets that one global be read and written, and its bytes after the run
   become an aggregate literal of type T. BODY may use NAME (directly or
   through the functions it calls) to read entries it filled already. *)

module String_set = Set.Make (String)

let synthetic_prefix = "__fas_const_"

(* Functions with a Fas body: calling one makes an initializer a call
   constant. Builtins (`trunc`, `sizeof`, ...) are not items, and C functions
   have no body. *)
let fas_functions items =
  List.fold_left
    (fun names item ->
      match item with
      | Ast.Func { name; body = Ast.Statements _; _ } -> String_set.add name names
      | _ -> names)
    String_set.empty items

let calls_fas_function functions expression =
  let found = ref false in
  let rec head = function
    | Ast.Ident (name, _) -> Some name
    | Ast.Generic_args (callee, _, _) | Ast.Parenthesized (callee, _) -> head callee
    | _ -> None
  in
  let mapper =
    {
      Ast.identity_mapper with
      on_expr =
        (fun expression ->
          (match expression with
          | Ast.Call (callee, _, _) -> (
              match head callee with
              | Some name when String_set.mem name functions -> found := true
              | _ -> ())
          | _ -> ());
          None);
    }
  in
  ignore (Ast.map_expr mapper expression);
  !found

(* the names an item uses, by identifier *)
let item_uses item = Ast.used_names { Ast.items = [ item ] }

let item_name = function
  | Ast.Const { name; _ }
  | Ast.Global { name; _ }
  | Ast.Func { name; _ }
  | Ast.Struct { name; _ }
  | Ast.Opaque { name; _ }
  | Ast.Enum { name; _ } ->
      Some name
  | Ast.Use _ | Ast.Pkg _ | Ast.Error_decl _ | Ast.Const_assert _ -> None

(* items whose presence never depends on reachability: declarations without
   a Fas body or initializer to evaluate *)
let always_kept = function
  | Ast.Use _ | Ast.Pkg _ | Ast.Error_decl _ -> true
  | Ast.Func { body = Ast.Declaration; _ } -> true
  | Ast.Struct _ | Ast.Opaque _ | Ast.Enum _ -> true
  | Ast.Const _ | Ast.Global _ | Ast.Func _ | Ast.Const_assert _ -> false

(* the literal for a scalar's bits, by its type; [None] for a type a scalar
   constant cannot have *)
(* [nonfinite kind bits] spells a NaN or an infinity, which has no float
   literal: the caller declares the bits as an integer constant and returns
   `bitcast[f..](that constant)`, so every payload bit survives *)
let literal_of_scalar ~nonfinite span (ty : Hir.ty) bits =
  let integer kind bits =
    let width = Sema_numeric.int_bits kind in
    let signed = not (Sema_numeric.is_unsigned (Hir.Int kind)) in
    let value =
      if width = 64 then bits
      else if signed then
        Int64.shift_right (Int64.shift_left bits (64 - width)) (64 - width)
      else Int64.logand bits (Int64.pred (Int64.shift_left 1L width))
    in
    if signed && value < 0L then
      if value = Int64.min_int then
        (* -9223372036854775808 has no positive literal to negate *)
        Ast.Binary
          ( Ast.Sub,
            Ast.Unary (Ast.Neg, Ast.Int_lit (Int64.to_string Int64.max_int, span), span),
            Ast.Int_lit ("1", span),
            span )
      else
        Ast.Unary (Ast.Neg, Ast.Int_lit (Int64.to_string (Int64.neg value), span), span)
    else Ast.Int_lit (Sema_numeric.unsigned_int64_to_string value, span)
  in
  match ty with
  | Hir.Bool -> Some (Ok (Ast.Bool_lit (Int64.logand bits 1L <> 0L, span)))
  | Hir.Int kind -> Some (Ok (integer kind bits))
  | Hir.Enum (enum, kind) ->
      Some
        (let width = Sema_numeric.int_bits kind in
         let member_bits =
           if width = 64 then bits
           else Int64.logand bits (Int64.pred (Int64.shift_left 1L width))
         in
         match Sema_constants.enum_member_of enum member_bits with
         | Some member -> Ok (Ast.Field (Ast.Ident (enum, span), member, span))
         | None ->
             Error
               (Printf.sprintf "it produces %Ld, which is no member of `%s`" member_bits
                  enum))
  | Hir.Float kind ->
      Some
        (let value =
           match kind with
           | Hir.F64 -> Int64.float_of_bits bits
           | Hir.F32 -> Int32.float_of_bits (Int64.to_int32 bits)
         in
         if Float.is_nan value || Float.abs value = Float.infinity then
           Ok (nonfinite kind bits)
         else
           (* "%h" is exact; the sign is a separate negation so -0.0 keeps it *)
           let magnitude =
             Ast.Float_lit (Printf.sprintf "%h" (Float.abs value), span)
           in
           Ok
             (if Float.sign_bit value then Ast.Unary (Ast.Neg, magnitude, span)
              else magnitude))
  | _ -> None

(* the literal for a value the interpreter returned, by the constant's type *)
let literal_of_value ~nonfinite span (ty : Hir.ty) (value : Fir_interp.v array) =
  match value with
  | [| [| Fir_interp.Bits limbs |] |] when Array.length limbs > 0 -> (
      match literal_of_scalar ~nonfinite span ty limbs.(0) with
      | Some result -> result
      | None ->
          Error
            (Printf.sprintf
               "a constant computed by a call must have an integer, bool, float or \
                enum type, not `%s`"
               (Hir.ty_name ty)))
  | [| [| Fir_interp.Poison |] |] -> Error "it produces an undefined (poison) value"
  | _ -> Error "it does not produce a single scalar value"

(* the literal for the bytes of a filled aggregate (little-endian), by its
   type: arrays and vectors as `{e0, e1, ...}`, structs as their fields in
   order. Addresses other than null and error values cannot be data. *)
let literal_of_bytes ~nonfinite span (structs : Hir.struct_def list) (ty : Hir.ty)
    (bytes : int option array) =
  let ( let* ) = Result.bind in
  let pointer_size = (Target_layout.current ()).pointer_size in
  let rec size_of = function
    | Hir.Bool | Hir.Int (Hir.U8 | Hir.I8) -> Ok 1
    | Hir.Int (Hir.U16 | Hir.I16) -> Ok 2
    | Hir.Int (Hir.U32 | Hir.I32) | Hir.Float Hir.F32 | Hir.Err -> Ok 4
    | Hir.Int (Hir.U64 | Hir.I64) | Hir.Float Hir.F64 -> Ok 8
    | Hir.Enum (_, kind) -> size_of (Hir.Int kind)
    | Hir.Addr | Hir.Handle _ -> Ok pointer_size
    | Hir.Array (n, e) | Hir.Vec (n, e) ->
        let* size = size_of e in
        Ok (n * size)
    | Hir.Struct name -> (
        match List.find_opt (fun (d : Hir.struct_def) -> d.name = name) structs with
        | Some d -> Ok d.size
        | None -> Error (Printf.sprintf "its type `%s` has no known layout" name))
    | (Hir.Opaque _ | Hir.Void) as ty ->
        Error (Printf.sprintf "`%s` cannot be constant data" (Hir.ty_name ty))
  in
  let scalar ty off size =
    let rec bits k acc =
      if k < 0 then Ok acc
      else
        match bytes.(off + k) with
        | None ->
            Error
              (Printf.sprintf
                 "the block leaves byte %d undefined (it stored a poison value)"
                 (off + k))
        | Some b -> bits (k - 1) (Int64.logor (Int64.shift_left acc 8) (Int64.of_int b))
    in
    let* value = bits (size - 1) 0L in
    match ty with
    | Hir.Addr | Hir.Handle _ ->
        if value = 0L then Ok (Ast.Null span)
        else
          Error
            (Printf.sprintf
               "byte %d holds an address, which compile-time data cannot hold" off)
    | Hir.Err -> Error "an `error` value cannot be compile-time data yet"
    | _ -> (
        match literal_of_scalar ~nonfinite span ty value with
        | Some result ->
            Result.map_error
              (fun message -> Printf.sprintf "at byte %d %s" off message)
              result
        | None -> Error (Printf.sprintf "`%s` cannot be constant data" (Hir.ty_name ty))
        )
  in
  let rec go ty off =
    match ty with
    | Hir.Vec (_, Hir.Bool) -> Error "a `vec` of `bool` cannot be filled yet"
    | Hir.Array (n, e) | Hir.Vec (n, e) ->
        let* size = size_of e in
        let rec elements k acc =
          if k < 0 then Ok acc
          else
            let* element = go e (off + (k * size)) in
            elements (k - 1) (element :: acc)
        in
        let* xs = elements (n - 1) [] in
        Ok (Ast.Array_lit (xs, span))
    | Hir.Struct name -> (
        match List.find_opt (fun (d : Hir.struct_def) -> d.name = name) structs with
        | Some { is_union = true; _ } | Some { byte_storage = true; _ } ->
            Error
              (Printf.sprintf "`%s` is a C union or record, which cannot be filled yet"
                 name)
        | Some d ->
            let* fields =
              List.fold_right
                (fun (f : Hir.field) acc ->
                  let* acc = acc in
                  let* value = go f.ty (off + f.offset) in
                  Ok (value :: acc))
                d.fields (Ok [])
            in
            Ok (Ast.Array_lit (fields, span))
        | None -> Error (Printf.sprintf "its type `%s` has no known layout" name))
    | ty ->
        let* size = size_of ty in
        scalar ty off size
  in
  go ty 0

(* the first `return` in a `const { }` body: the block is not a function *)
let rec find_return statements =
  let in_expression expression =
    let found = ref None in
    let mapper =
      {
        Ast.identity_mapper with
        on_expr =
          (function
          | Ast.Catch (_, Ast.Catch_block (_, body), _) ->
              if !found = None then found := find_return body;
              None
          | _ -> None);
      }
    in
    ignore (Ast.map_expr mapper expression);
    !found
  in
  let in_statement statement =
    let nested =
      match statement with
      | Ast.Return (_, span) -> Some span
      | Ast.If (_, yes, no, _) -> (
          match find_return yes with
          | Some span -> Some span
          | None -> Option.bind no find_return)
      | Ast.While (_, _, body, _)
      | Ast.Defer (body, _)
      | Ast.Block (body, _)
      | Ast.Const_for { body; _ }
      | Ast.Const_copy { body; _ }
      | Ast.Errdefer (_, body, _)
      | Ast.Catch_stmt { body; _ } ->
          find_return body
      | Ast.For (_, init, _, step, body, _) ->
          List.find_map
            (fun x -> x)
            [
              Option.bind init (fun s -> find_return [ s ]);
              Option.bind step (fun s -> find_return [ s ]);
              find_return body;
            ]
      | Ast.Switch (_, cases, default, _) -> (
          match List.find_map (fun (_, body) -> find_return body) cases with
          | Some span -> Some span
          | None -> Option.bind default find_return)
      | _ -> None
    in
    match nested with
    | Some span -> Some span
    | None ->
        (* `catch { }` blocks inside the statement's expressions *)
        let expressions = ref [] in
        let mapper =
          {
            Ast.identity_mapper with
            on_expr =
              (fun e ->
                expressions := e :: !expressions;
                Some e);
          }
        in
        ignore (Ast.map_stmt mapper statement);
        List.find_map in_expression !expressions
  in
  List.find_map in_statement statements

let trap_text = function
  | Fir_interp.Trap_div -> "divides by zero or overflows a division"
  | Fir_interp.Trap_insn -> "reaches a trap"

(* [sema items specializations] runs the main semantic check on a reduced
   program; the driver supplies it so the C import context is the same. *)
let resolve ~sema (program : Ast.program) =
  let ( let* ) = Result.bind in
  let functions = fas_functions program.items in
  let is_call_const = function
    | Ast.Const { fill = Some _; _ } -> true
    | Ast.Const { value; _ } -> calls_fas_function functions value
    | Ast.Const_assert { cond; _ } -> calls_fas_function functions cond
    | _ -> false
  in
  if not (List.exists is_call_const program.items) then Ok program
  else
    let pending =
      List.fold_left
        (fun set item ->
          match item with
          | Ast.Const { name; _ } when is_call_const item -> String_set.add name set
          | _ -> set)
        String_set.empty program.items
    in
    let pending = ref pending in
    (* current items: resolved constants are replaced in place *)
    let items = ref program.items in
    (* NaN and infinity results: `const __fas_const_bits_N uNN = 0x...`,
       added to the program, and `bitcast[fNN](__fas_const_bits_N)` *)
    let bits_counter = ref 0 in
    let nonfinite span kind bits =
      incr bits_counter;
      let name = Printf.sprintf "%sbits_%d" synthetic_prefix !bits_counter in
      let int_ty, float_ty, bits =
        match kind with
        | Hir.F64 -> (Ast.Int Ast.U64, Ast.Float Ast.F64, bits)
        | Hir.F32 -> (Ast.Int Ast.U32, Ast.Float Ast.F32, Int64.logand bits 0xFFFF_FFFFL)
      in
      items :=
        !items
        @ [
            Ast.const_item name span int_ty
              (Ast.Int_lit (Printf.sprintf "0x%LX" bits, span))
              span;
          ];
      Ast.Cast (Ast.Bitcast, float_ty, Ast.Ident (name, span), span)
    in
    let named () =
      List.fold_left
        (fun table item ->
          match item_name item with Some name -> (name, item) :: table | None -> table)
        [] !items
    in
    (* the named items [seeds] reach, transitively *)
    let reach seeds =
      let table = named () in
      let rec go seen = function
        | [] -> seen
        | name :: rest when String_set.mem name seen -> go seen rest
        | name :: rest -> (
            match List.assoc_opt name table with
            | None -> go seen rest
            | Some item -> go (String_set.add name seen) (item_uses item @ rest))
      in
      go String_set.empty seeds
    in
    let evaluate ?fill ~what ~name ~ty ~ty_span ~value ~span () =
      let synthetic = synthetic_prefix ^ name in
      let ret, body =
        match fill with
        | Some body -> (Ast.Void, body)
        | None -> (ty, [ Ast.Return (Some value, span) ])
      in
      let func =
        Ast.Func
          {
            name = synthetic;
            name_span = span;
            ret_span = ty_span;
            params = [];
            ret;
            fails = false;
            body = Ast.Statements body;
            linkage = Ast.Internal;
            variadic = false;
            generic_params = [];
            span;
            extent = span;
            body_span = span;
          }
      in
      let reached = reach (item_uses func) in
      let kept =
        List.filter
          (fun item ->
            always_kept item
            ||
            match item_name item with
            | Some n when fill <> None && n = name -> false
            | Some name -> String_set.mem name reached
            | None -> false)
          !items
      in
      (* a fill writes `var NAME T`, which starts zeroed *)
      let kept =
        match fill with
        | None -> kept
        | Some _ ->
            Ast.Global
              {
                name;
                name_span = span;
                ty_span;
                ty;
                init = None;
                linkage = Ast.Internal_global;
                span;
                extent = span;
              }
            :: kept
      in
      let note = Printf.sprintf "while evaluating %s at compile time" what in
      let with_note diagnostics =
        List.map
          (fun (d : Diag.t) -> { d with Diag.notes = d.notes @ [ note ] })
          diagnostics
      in
      let fail message =
        Error [ Diag.error (Ast.expr_span value) (what ^ " " ^ message) ]
      in
      let returns = Option.bind fill find_return in
      match returns with
      | Some at ->
          Error
            [
              Diag.error at
                (Printf.sprintf
                   "a `const { }` block cannot `return`; the block of `%s` ends at its \
                    closing brace"
                   name);
            ]
      | None -> (
          match
            sema { Ast.items = kept @ [ func ] } (Sema_specialization.create ())
          with
          | Error diagnostics -> Error (with_note diagnostics)
          | Ok (hir : Hir.program) -> (
              match
                List.find_opt (fun (f : Hir.func) -> f.name = synthetic) hir.funcs
              with
              | None -> fail "cannot be evaluated: its function was not compiled"
              | Some f -> (
                  match Lower.lower hir with
                  | Error diagnostics -> Error (with_note diagnostics)
                  | Ok modul when fill <> None -> (
                      let global_ty =
                        List.find_map
                          (fun (g : Hir.global) ->
                            if g.name = name then Some g.ty else None)
                          hir.globals
                      in
                      match global_ty with
                      | None -> fail "cannot be evaluated: its storage was not compiled"
                      | Some global_ty -> (
                          match Fir_interp.eval_fill [ modul ] ~fill:name synthetic with
                          | Fir_interp.Fill_bytes bytes -> (
                              match
                                literal_of_bytes ~nonfinite:(nonfinite span) span
                                  hir.structs global_ty bytes
                              with
                              | Ok literal -> Ok literal
                              | Error message ->
                                  fail ("cannot be compile-time data: " ^ message))
                          | Fir_interp.Fill_failed
                              (Fir_interp.Const_trapped (kind, stderr)) ->
                              let stderr = String.trim stderr in
                              if stderr <> "" then
                                fail ("fails at compile time: " ^ stderr)
                              else
                                fail
                                  ("fails at compile time: its evaluation "
                                 ^ trap_text kind)
                          | Fir_interp.Fill_failed (Fir_interp.Const_error message) ->
                              fail
                                ("cannot be evaluated at compile time: its evaluation "
                               ^ message)
                          | Fir_interp.Fill_failed (Fir_interp.Const_value _) ->
                              fail "cannot be evaluated: the fill returned a value"))
                  | Ok modul -> (
                      match Fir_interp.eval_const [ modul ] synthetic with
                      | Fir_interp.Const_value values -> (
                          match
                            literal_of_value ~nonfinite:(nonfinite span) span f.ret
                              values
                          with
                          | Ok literal -> Ok literal
                          | Error message -> fail ("cannot be a constant: " ^ message))
                      | Fir_interp.Const_trapped (kind, stderr) ->
                          let stderr = String.trim stderr in
                          if stderr <> "" then fail ("fails at compile time: " ^ stderr)
                          else
                            fail
                              ("fails at compile time: its evaluation " ^ trap_text kind)
                      | Fir_interp.Const_error message ->
                          fail
                            ("cannot be evaluated at compile time: its evaluation "
                           ^ message)))))
    in
    let replace name literal =
      items :=
        List.map
          (function
            | Ast.Const c when c.name = name ->
                Ast.Const { c with value = literal; fill = None }
            | item -> item)
          !items;
      pending := String_set.remove name !pending
    in
    (* resolve [name] after the pending constants it reaches; [visiting] holds
       the chain that led here, for the cycle error *)
    let rec resolve_const visiting name =
      if not (String_set.mem name !pending) then Ok ()
      else
        match
          List.find_opt
            (function Ast.Const { name = n; _ } -> n = name | _ -> false)
            !items
        with
        | Some (Ast.Const { ty; ty_span; value; fill; span; name_span; _ } as item) ->
            if List.mem name visiting then
              Error
                [
                  Diag.error name_span
                    (Printf.sprintf
                       "constant `%s` depends on itself through the functions its \
                        initializer calls"
                       name);
                ]
            else
              let reached =
                match fill with
                | Some _ ->
                    (* a fill may use its own name: it reads what it filled *)
                    String_set.remove name
                      (reach (Ast.used_names { Ast.items = [ item ] }))
                | None ->
                    reach
                      (Ast.used_names
                         {
                           Ast.items =
                             [ Ast.Const_assert { cond = value; message = None; span } ];
                         })
              in
              let* () =
                String_set.fold
                  (fun dependency result ->
                    let* () = result in
                    if dependency = name then
                      Error
                        [
                          Diag.error name_span
                            (Printf.sprintf
                               "constant `%s` depends on itself through the functions \
                                its initializer calls"
                               name);
                        ]
                    else if String_set.mem dependency !pending then
                      resolve_const (name :: visiting) dependency
                    else Ok ())
                  reached (Ok ())
              in
              let* literal =
                evaluate ?fill
                  ~what:(Printf.sprintf "constant `%s`" name)
                  ~name ~ty ~ty_span ~value ~span ()
              in
              replace name literal;
              Ok ()
        | _ -> Ok ()
    in
    let* () =
      String_set.fold
        (fun name result ->
          let* () = result in
          resolve_const [] name)
        !pending (Ok ())
    in
    (* top-level `const assert`s with calls: evaluate the condition as a bool
       constant once every call constant is known, and leave a literal for the
       main check to report with the assertion's own message *)
    let counter = ref 0 in
    let* resolved =
      List.fold_left
        (fun result item ->
          let* acc = result in
          match item with
          | Ast.Const_assert { cond; message; span }
            when calls_fas_function functions cond ->
              incr counter;
              let* literal =
                evaluate ~what:"this const assert"
                  ~name:(Printf.sprintf "assert_%d" !counter)
                  ~ty:Ast.Bool ~ty_span:span ~value:cond ~span ()
              in
              Ok (Ast.Const_assert { cond = literal; message; span } :: acc)
          | item -> Ok (item :: acc))
        (Ok []) !items
    in
    Ok { Ast.items = List.rev resolved }
