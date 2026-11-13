open Sema_constants
open Sema_flow
open Sema_numeric
open Sema_specialization
open Sema_types
open Sema_context
module Int64_map = Map.Make (Int64)

let error ?help span message = Error [ Diag.error ?help span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x
let literal_condition_truth = function Hir.EBool (value, _) -> Some value | _ -> None

let rec statement_call = function
  | Ast.Call _ -> true
  | Ast.Parenthesized (e, _) -> statement_call e
  (* `try f()` and `f() catch DEFAULT` stand alone like the call *)
  | Ast.Try (e, _) | Ast.Catch (e, Ast.Catch_value _, _) -> statement_call e
  | _ -> false

let index_type_error label expression ty =
  error (Ast.expr_span expression)
    (Printf.sprintf "%s must be an integer, got `%s`" label
       (Sema_types.diagnostic_ty_name ty))

(* a C function's struct or union result (removal B1): an aggregate value
   that may initialize or be assigned to a place, unlike other aggregate
   values, which need `copy` *)
let c_struct_result = function
  | Hir.Call (Hir.User _, _, Hir.Struct _, _) -> true
  | _ -> false

let visible_value_names c =
  Sema_flow.local_names c.flow
  @ List.filter_map
      (fun binding ->
        if binding.declaration_kind = Top_const || binding.declaration_kind = Top_global
        then Some binding.declaration_name
        else None)
      c.top_level_bindings
  @ List.map (fun (name, _, _) -> name) c.consts
  @ List.map (fun (name, _, _) -> name) c.arrays

let unknown_name_error visible kind span name =
  error
    ?help:
      (if kind = "name" && name = "NULL" then Some "write `null`"
       else similar_name_help visible name)
    span
    ("unknown " ^ kind ^ " `" ^ name ^ "`")

let unknown_value_error visible span name =
  match Names.value_misuse_message name with
  | Some message -> error span message
  | None -> unknown_name_error visible "name" span name

let missing_field_error ?record_name ?base c span name ty =
  let help =
    match (ty, base) with
    | Hir.Addr, Some (Ast.Ident (base_name, _)) ->
        Some (Printf.sprintf "write `%s[T].%s` with the record type" base_name name)
    | Hir.Addr, _ -> None
    | Hir.Struct record_name, _ ->
        c.structs
        |> List.find_opt (fun (record : Hir.struct_def) -> record.name = record_name)
        |> Option.map (fun (record : Hir.struct_def) ->
            similar_name_help
              (List.map (fun (field : Hir.field) -> field.name) record.fields)
              name)
        |> Option.join
    | _ -> None
  in
  error ?help span (missing_field_message ?record_name name ty)

let is_comparison_operator = function
  | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> true
  | _ -> false

let assignment_context = function
  | Hir.ALocal local -> Printf.sprintf "value for `%s`" local.name
  | Hir.AGlobal (name, _) -> Printf.sprintf "value for `%s`" name
  | Hir.AField (base, name, _) -> (
      match Hir.expr_ty base with
      | Hir.Struct record ->
          Printf.sprintf "value for field `%s` of record `%s`" name record
      | _ -> Printf.sprintf "value for field `%s`" name)
  | Hir.ARaw _ | Hir.AIndex _ -> "assigned value"

let compound_operator = function
  | Ast.Add -> "+="
  | Ast.Sub -> "-="
  | Ast.Mul -> "*="
  | Ast.Div -> "/="
  | Ast.Rem -> "%="
  | Ast.Bit_and -> "&="
  | Ast.Bit_or -> "|="
  | Ast.Bit_xor -> "^="
  | Ast.Shl -> "<<="
  | Ast.Shr -> ">>="
  | Ast.And -> "&&="
  | Ast.Or -> "||="
  | Ast.Eq -> "=="
  | Ast.Ne -> "!="
  | Ast.Lt -> "<"
  | Ast.Le -> "<="
  | Ast.Gt -> ">"
  | Ast.Ge -> ">="

let literal_expression_truth = function
  | Hir.EBool (value, _) -> Some value
  | Hir.EInt (value, _, _) -> Some (value <> 0L)
  | _ -> None

let fact_order ty =
  match ty with
  | Hir.Int (Hir.U8 | U16 | U32 | U64) -> Int64.unsigned_compare
  | _ -> Int64.compare

let fact_bounds ty =
  match ty with
  | Hir.Int kind ->
      let bits = int_bits kind in
      if is_unsigned ty then
        ( 0L,
          if bits = 64 then Int64.minus_one else Int64.pred (Int64.shift_left 1L bits)
        )
      else if bits = 64 then (Int64.min_int, Int64.max_int)
      else
        let minimum = Int64.neg (Int64.shift_left 1L (bits - 1)) in
        (minimum, Int64.pred (Int64.shift_left 1L (bits - 1)))
  | _ -> (0L, 0L)

let fact_in_domain ty value =
  let low, high = fact_bounds ty in
  fact_order ty value low >= 0 && fact_order ty value high <= 0

let fact_exact ty value : value_fact =
  let value = sign_extend_value ty value in
  { low = value; high = value; induction = None }

let fact_single ty fact = fact_order ty fact.low fact.high = 0

let fact_add ty left right =
  let value = Int64.add left right in
  let valid =
    if is_unsigned ty then Int64.unsigned_compare value left >= 0
    else Int64.logand (Int64.logxor left value) (Int64.logxor right value) >= 0L
  in
  if valid && fact_in_domain ty value then Some value else None

let fact_sub ty left right =
  let value = Int64.sub left right in
  let valid =
    if is_unsigned ty then Int64.unsigned_compare left right >= 0
    else Int64.logand (Int64.logxor left right) (Int64.logxor left value) >= 0L
  in
  if valid && fact_in_domain ty value then Some value else None

let fact_mul ty left right =
  let _, maximum = fact_bounds ty in
  if is_unsigned ty then
    if right = 0L || Int64.unsigned_compare left (Int64.unsigned_div maximum right) <= 0
    then
      let value = Int64.mul left right in
      if fact_in_domain ty value then Some value else None
    else None
  else if left = 0L || right = 0L then Some 0L
  else if (left = Int64.min_int && right = -1L) || (right = Int64.min_int && left = -1L)
  then None
  else
    let value = Int64.mul left right in
    if Int64.div value right = left && fact_in_domain ty value then Some value else None

let fact_min ty left right = if fact_order ty left right <= 0 then left else right
let fact_max ty left right = if fact_order ty left right >= 0 then left else right

let combine_facts op ty left right =
  let induction =
    match
      (left.induction, right.induction, fact_single ty left, fact_single ty right)
    with
    | Some id, _, false, true | _, Some id, true, false -> Some id
    | _ -> None
  in
  let endpoints =
    match op with
    | Ast.Add -> (fact_add ty left.low right.low, fact_add ty left.high right.high)
    | Ast.Sub -> (fact_sub ty left.low right.high, fact_sub ty left.high right.low)
    | Ast.Mul ->
        let products =
          [
            fact_mul ty left.low right.low;
            fact_mul ty left.low right.high;
            fact_mul ty left.high right.low;
            fact_mul ty left.high right.high;
          ]
        in
        if List.exists Option.is_none products then (None, None)
        else
          let values = List.map Option.get products in
          ( Some (List.fold_left (fact_min ty) (List.hd values) (List.tl values)),
            Some (List.fold_left (fact_max ty) (List.hd values) (List.tl values)) )
    | _ -> (None, None)
  in
  match endpoints with
  | Some low, Some high -> Some { low; high; induction }
  | _ -> None

let rec value_fact (c : context) = function
  | Hir.EInt (value, ty, _) -> Some (fact_exact ty value)
  | Hir.Local (binding, _) -> Sema_flow.value_of c.flow binding
  | Hir.Unary (Ast.Neg, value, ty, _) -> (
      match value_fact c value with
      | Some fact when is_int ty && not (is_unsigned ty) ->
          if fact.low = fst (fact_bounds ty) then None
          else
            Some
              {
                low = Int64.neg fact.high;
                high = Int64.neg fact.low;
                induction = fact.induction;
              }
      | Some fact when fact.low = 0L && fact.high = 0L -> Some fact
      | _ -> None)
  | Hir.Binary (((Ast.Add | Ast.Sub | Ast.Mul) as op), left, right, ty, _) -> (
      match (value_fact c left, value_fact c right) with
      | Some left, Some right -> combine_facts op ty left right
      | _ -> None)
  | Hir.Ternary (condition, yes, no, ty, _) -> (
      match condition_truth c condition with
      | Some true -> value_fact c yes
      | Some false -> value_fact c no
      | None -> (
          match (value_fact c yes, value_fact c no) with
          | Some yes, Some no ->
              Some
                {
                  low = fact_min ty yes.low no.low;
                  high = fact_max ty yes.high no.high;
                  induction =
                    (if yes.induction = no.induction then yes.induction else None);
                }
          | _ -> None))
  | _ -> None

and fact_for_condition c expression =
  match value_fact c expression with
  | Some fact -> Some fact
  | None when is_int (Hir.expr_ty expression) ->
      let low, high = fact_bounds (Hir.expr_ty expression) in
      Some { low; high; induction = None }
  | None -> None

and comparison_truth op ty left right =
  let compare = fact_order ty in
  let exact = fact_single ty left && fact_single ty right in
  match op with
  | Ast.Lt when compare left.high right.low < 0 -> Some true
  | Ast.Lt when compare left.low right.high >= 0 -> Some false
  | Ast.Le when compare left.high right.low <= 0 -> Some true
  | Ast.Le when compare left.low right.high > 0 -> Some false
  | Ast.Gt when compare left.low right.high > 0 -> Some true
  | Ast.Gt when compare left.high right.low <= 0 -> Some false
  | Ast.Ge when compare left.low right.high >= 0 -> Some true
  | Ast.Ge when compare left.high right.low < 0 -> Some false
  | Ast.Eq when exact && compare left.low right.low = 0 -> Some true
  | Ast.Eq when compare left.high right.low < 0 || compare right.high left.low < 0 ->
      Some false
  | Ast.Ne when exact && compare left.low right.low <> 0 -> Some true
  | Ast.Ne when exact && compare left.low right.low = 0 -> Some false
  | Ast.Ne when compare left.high right.low < 0 || compare right.high left.low < 0 ->
      Some true
  | _ -> None

and condition_truth c = function
  | Hir.EBool (value, _) -> Some value
  | Hir.EInt (value, _, _) -> Some (value <> 0L)
  | Hir.Local (binding, _) when binding.ty = Hir.Bool -> (
      match Sema_flow.mask_of c.flow binding with
      | Some [ value ] -> Some value
      | _ -> None)
  | Hir.Unary (Ast.Not, value, _, _) -> Option.map not (condition_truth c value)
  | Hir.Binary (Ast.And, left, right, _, _) -> (
      match condition_truth c left with
      | Some false -> Some false
      | Some true -> condition_truth c right
      | None -> (
          match condition_truth c right with Some false -> Some false | _ -> None))
  | Hir.Binary (Ast.Or, left, right, _, _) -> (
      match condition_truth c left with
      | Some true -> Some true
      | Some false -> condition_truth c right
      | None -> (
          match condition_truth c right with Some true -> Some true | _ -> None))
  | Hir.Binary
      (((Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge) as op), left, right, _, _)
    -> (
      let ty = Hir.expr_ty left in
      match ty with
      | Hir.Float _ -> (
          match (float_constant_bits left, float_constant_bits right) with
          | Some left, Some right ->
              Some (Sema_constants.float_comparison_bits ty op left right <> 0L)
          | _ -> None)
      | _ -> (
          match (fact_for_condition c left, fact_for_condition c right) with
          | Some left, Some right -> comparison_truth op ty left right
          | _ -> None))
  | _ -> None

and float_constant_bits = function
  | Hir.EFloat (bits, _, _) -> Some bits
  | Hir.Unary (Ast.Neg, value, Hir.Float Hir.F32, _) ->
      Option.map (fun bits -> Int64.logxor bits 0x80000000L) (float_constant_bits value)
  | Hir.Unary (Ast.Neg, value, Hir.Float Hir.F64, _) ->
      Option.map
        (fun bits -> Int64.logxor bits Int64.min_int)
        (float_constant_bits value)
  | _ -> None

let exact_address_offset c expression =
  let rec exact = function
    | Hir.EInt (value, ty, _) -> Some (sign_extend_value ty value)
    | Hir.Cast (kind, value, ty, _) -> (
        match exact value with
        | None -> None
        | Some value ->
            let converted =
              match kind with Ast.Trunc -> mask_value ty value | _ -> value
            in
            if is_unsigned ty && Int64.compare converted 0L < 0 then None
            else Some (sign_extend_value ty converted))
    | Hir.Binary (((Ast.Add | Ast.Sub | Ast.Mul) as op), left, right, ty, _) -> (
        match (exact left, exact right) with
        | Some left, Some right ->
            let result =
              match op with
              | Ast.Add ->
                  if
                    (right > 0L && left > Int64.sub Int64.max_int right)
                    || (right < 0L && left < Int64.sub Int64.min_int right)
                  then None
                  else Some (Int64.add left right)
              | Ast.Sub ->
                  if
                    (right < 0L && left > Int64.add Int64.max_int right)
                    || (right > 0L && left < Int64.add Int64.min_int right)
                  then None
                  else Some (Int64.sub left right)
              | Ast.Mul ->
                  if left = 0L || right = 0L then Some 0L
                  else if
                    (left = Int64.min_int && right = -1L)
                    || (right = Int64.min_int && left = -1L)
                  then None
                  else
                    let product = Int64.mul left right in
                    if Int64.div product right = left then Some product else None
              | _ -> None
            in
            Option.bind result (fun result ->
                if is_unsigned ty && result < 0L then None
                else Some (sign_extend_value ty result))
        | _ -> None)
    | Hir.Local (binding, _) -> (
        match Sema_flow.value_of c.flow binding with
        | Some fact when fact_single binding.ty fact ->
            Some (sign_extend_value binding.ty fact.low)
        | _ -> None)
    | _ -> None
  in
  match exact expression with
  | Some value -> Some value
  | None -> (
      match value_fact c expression with
      | Some fact when fact_single (Hir.expr_ty expression) fact ->
          let value = fact.low in
          if is_unsigned (Hir.expr_ty expression) && Int64.compare value 0L < 0 then
            None
          else Some (sign_extend_value (Hir.expr_ty expression) value)
      | _ -> None)

let shift_address op address delta =
  let shift offset =
    match op with
    | Ast.Add ->
        let value = Int64.add offset delta in
        if Int64.logand (Int64.logxor offset value) (Int64.logxor delta value) < 0L then
          None
        else Some value
    | Ast.Sub ->
        let value = Int64.sub offset delta in
        if Int64.logand (Int64.logxor offset delta) (Int64.logxor offset value) < 0L
        then None
        else Some value
    | _ -> None
  in
  match address with
  | Sema_flow.Null_address offset ->
      Option.map (fun offset -> Sema_flow.Null_address offset) (shift offset)
  | Sema_flow.Object_address ({ offset; _ } as address) ->
      Option.map
        (fun offset -> Sema_flow.Object_address { address with offset })
        (shift offset)
  | Sema_flow.Function_address _ -> None
  | Sema_flow.Dead_local_address _ -> Some address

let rec address_fact c = function
  | Hir.Null _ -> Some (Sema_flow.Null_address 0L)
  | Hir.Local (binding, _) when binding.ty = Hir.Addr ->
      if Sema_flow.is_view c.flow binding then
        Option.bind (Sema_flow.view_origin c.flow binding) (fun (root, _) ->
            Sema_flow.address_of c.flow root)
      else Sema_flow.address_of c.flow binding
  | Hir.Local (binding, _)
    when c.handle_view_expansion
         && match binding.ty with Hir.Handle _ -> true | _ -> false ->
      Sema_flow.address_of c.flow binding
  | Hir.Call (Hir.Builtin Hir.Handle_addr, [ handle ], Hir.Addr, _) ->
      address_fact c handle
  | Hir.Call (Hir.Builtin (Hir.Handle_from_addr _), [ pointer ], Hir.Handle _, _) ->
      address_fact c pointer
  | Hir.EString _ as expression -> object_address c expression
  | Hir.Address (place, _, _) -> object_address c place
  | Hir.Function_address (name, _) -> Some (Sema_flow.Function_address name)
  | Hir.Call (Hir.User _, _, Hir.Addr, span) ->
      Option.map
        (fun (_, name, extent) ->
          Sema_flow.Object_address
            {
              identity = "allocation:" ^ Span.to_string span;
              name;
              owner_name = None;
              writable = true;
              nullable = true;
              owner = None;
              extent;
              offset = 0L;
            })
        (Sema_flow.call_object c.flow span)
  | Hir.Binary (((Ast.Add | Ast.Sub) as op), left, right, Hir.Addr, _) -> (
      match (address_fact c left, exact_address_offset c right) with
      | Some address, Some offset -> shift_address op address offset
      | _ -> (
          match (op, exact_address_offset c left, address_fact c right) with
          | Ast.Add, Some offset, Some address -> shift_address Ast.Add address offset
          | _ -> None))
  | Hir.Ternary (condition, yes, no, _, _) -> (
      match condition_truth c condition with
      | Some true -> address_fact c yes
      | Some false -> address_fact c no
      | None -> (
          match (address_fact c yes, address_fact c no) with
          | Some yes, Some no when yes = no -> Some yes
          | _ -> None))
  | _ -> None

and object_address c expression =
  let object_size ty =
    match Hir.layout c.structs ty with Ok (size, _) -> Some size | Error _ -> None
  in
  let root binding ty =
    Option.map
      (fun extent ->
        Sema_flow.Object_address
          {
            identity = Printf.sprintf "local:%d" binding.id;
            name = binding.name;
            owner_name = Some binding.name;
            writable = true;
            nullable = false;
            owner = Some binding.id;
            extent = Int64.of_int extent;
            offset = 0L;
          })
      (object_size ty)
  in
  let rec from_path address ty = function
    | [] -> Some address
    | Sema_flow.Field name :: rest -> (
        match ty with
        | Hir.Struct struct_name -> (
            match field_info c.structs struct_name name with
            | Some field -> (
                match (address, object_size field.ty) with
                | Sema_flow.Object_address base, Some _ ->
                    let address =
                      Sema_flow.Object_address
                        {
                          base with
                          offset = Int64.add base.offset (Int64.of_int field.offset);
                        }
                    in
                    from_path address field.ty rest
                | _ -> None)
            | None -> None)
        | _ -> None)
    | Sema_flow.Element index :: rest -> (
        match ty with
        | Hir.Array (_, element) | Hir.Vec (_, element) -> (
            match (address, object_size element) with
            | Sema_flow.Object_address base, Some stride ->
                let step = Int64.mul (Int64.of_int index) (Int64.of_int stride) in
                from_path
                  (Sema_flow.Object_address
                     { base with offset = Int64.add base.offset step })
                  element rest
            | _ -> None)
        | _ -> None)
  in
  let rec locate = function
    | Hir.Local (binding, _) when binding.ty = Hir.Addr -> None
    | Hir.Local (binding, _) when Sema_flow.is_view c.flow binding -> (
        match Sema_flow.view_origin c.flow binding with
        | Some (root_binding, Sema_flow.Exact path) ->
            Option.bind (root root_binding root_binding.ty) (fun address ->
                from_path address root_binding.ty path)
        | _ -> None)
    | Hir.Local (binding, _) -> root binding binding.ty
    | Hir.Global (name, ty, _) ->
        Option.map
          (fun extent ->
            let writable =
              match lookup_top_level name c.top_level_bindings with
              | Some { declaration_kind = Top_const; _ } -> false
              | _ -> (
                  match lookup_global name c.globals with
                  | Some (_, _, Ast.Import_const_c) -> false
                  | _ -> true)
            in
            Sema_flow.Object_address
              {
                identity = "global:" ^ name;
                name;
                owner_name = None;
                writable;
                nullable = false;
                owner = None;
                extent = Int64.of_int extent;
                offset = 0L;
              })
          (object_size ty)
    | Hir.Const_array (name, ty, _) ->
        Option.map
          (fun extent ->
            Sema_flow.Object_address
              {
                identity = "constant:" ^ name;
                name;
                owner_name = None;
                writable = false;
                nullable = false;
                owner = None;
                extent = Int64.of_int extent;
                offset = 0L;
              })
          (object_size ty)
    | Hir.EString (id, is_c, _) ->
        let strings = List.rev c.string_pool.reversed in
        Option.map
          (fun value ->
            let extent = String.length value in
            let value =
              if is_c then String.sub value 0 (String.length value - 1) else value
            in
            let name = Ast.string_literal_name is_c value in
            Sema_flow.Object_address
              {
                identity = "string:" ^ string_of_int id;
                name;
                owner_name = None;
                writable = false;
                nullable = false;
                owner = None;
                extent = Int64.of_int extent;
                offset = 0L;
              })
          (List.nth_opt strings id)
    | Hir.Field (base_expr, _, _, field_offset, _) -> (
        match locate base_expr with
        | Some (Sema_flow.Object_address base) ->
            Some
              (Sema_flow.Object_address
                 {
                   base with
                   offset = Int64.add base.offset (Int64.of_int field_offset);
                 })
        | _ -> None)
    | Hir.Index (base, index, ty, _) -> (
        match (locate base, exact_address_offset c index, object_size ty) with
        | Some (Sema_flow.Object_address base), Some index, Some stride ->
            let index = Int64.mul index (Int64.of_int stride) in
            Some
              (Sema_flow.Object_address
                 { base with offset = Int64.add base.offset index })
        | _ -> None)
    | Hir.Raw_select (base, offset, _, _) -> (
        match (address_fact c base, exact_address_offset c offset) with
        | Some address, Some offset -> shift_address Ast.Add address offset
        | _ -> None)
    | _ -> None
  in
  locate expression

let address_equal c left right =
  match (address_fact c left, address_fact c right) with
  | ( Some (Sema_flow.Object_address { nullable = true; _ }),
      Some (Sema_flow.Null_address _) )
  | ( Some (Sema_flow.Null_address _),
      Some (Sema_flow.Object_address { nullable = true; _ }) ) ->
      None
  | Some left, Some right -> Some (left = right)
  | _ -> None

let condition_truth c expression =
  match expression with
  | Hir.Binary (((Ast.Eq | Ast.Ne) as op), left, right, _, _)
    when match Hir.expr_ty left with Hir.Addr | Hir.Handle _ -> true | _ -> false ->
      Option.map
        (fun equal -> if op = Ast.Eq then equal else not equal)
        (address_equal c left right)
  | _ -> condition_truth c expression

let rec unterminated_string_literal c = function
  | Hir.Ternary (condition, yes, no, _, _) -> (
      match condition_truth c condition with
      | Some true -> unterminated_string_literal c yes
      | Some false -> unterminated_string_literal c no
      | None -> (
          match
            (unterminated_string_literal c yes, unterminated_string_literal c no)
          with
          | (Some _ as literal), _ | None, (Some _ as literal) -> literal
          | None, None -> None))
  | value -> (
      match address_fact c value with
      | Some (Sema_flow.Object_address { identity; _ }) -> (
          match String.split_on_char ':' identity with
          | [ "string"; id ] ->
              Option.bind (int_of_string_opt id) (fun id ->
                  match List.nth_opt (List.rev c.string_pool.reversed) id with
                  (* Any pointer into a string literal without the c prefix is
                     rejected, whatever its offset and whatever NUL bytes it holds
                     (removal C2). The pool shares bytes between literals, so a plain
                     literal that already ends in a NUL byte is the same object as the
                     c literal and is accepted. *)
                  | Some bytes when not (String.ends_with ~suffix:"\000" bytes) ->
                      Some bytes
                  | _ -> None)
          | _ -> None)
      | _ -> None)

let c_string_help bytes =
  if String.contains bytes '\000' then None
  else Some ("write " ^ Ast.string_literal_name true bytes)

let check_c_string_arguments c name parameters values arguments =
  if not (Sema_flow.proof_checks_enabled c.flow) then Ok ()
  else
    Result_list.iter
      (fun (index, parameter_name) ->
        match (List.nth_opt values (index - 1), List.nth_opt arguments (index - 1)) with
        | Some value, Some argument -> (
            match unterminated_string_literal c value with
            | None -> Ok ()
            | Some bytes ->
                let literal = Ast.string_literal_name false bytes in
                let parameter =
                  match parameter_name with
                  | Some name when name <> "" -> "parameter `" ^ name ^ "`"
                  | _ -> "parameter " ^ string_of_int index
                in
                error ?help:(c_string_help bytes) (Ast.expr_span argument)
                  (Printf.sprintf
                     "%s has no NUL terminator, but `%s` reads %s as a C string" literal
                     name parameter))
        | _ -> Ok ())
      parameters

let address_checks_enabled c = Sema_flow.proof_checks_enabled c.flow

let check_address_access c span ~write fact size =
  if not (address_checks_enabled c) then Ok ()
  else
    match fact with
    | Some (Sema_flow.Null_address offset) when Int64.compare offset 4096L < 0 ->
        error span "access through null address"
    | Some (Sema_flow.Object_address address) when write && not address.writable ->
        if String.starts_with ~prefix:"string:" address.identity then
          error span (Printf.sprintf "cannot modify string literal `%s`" address.name)
        else error span (Printf.sprintf "write to constant storage `%s`" address.name)
    | Some (Sema_flow.Object_address address) ->
        let past = Int64.add address.offset (Int64.of_int size) in
        if
          address.offset < 0L
          || Int64.compare past address.offset < 0
          || Int64.unsigned_compare past address.extent > 0
        then
          error span
            (Printf.sprintf
               "access outside object `%s` (offset %Ld, size %d bytes, object size %Ld)"
               address.name address.offset size address.extent)
        else Ok ()
    | Some (Sema_flow.Dead_local_address name) ->
        error span (Printf.sprintf "access to local `%s` after its block ended" name)
    | _ -> Ok ()

let remember_alloc_size_object c name arguments parameters span =
  let constant_size index =
    match (List.nth_opt arguments (index - 1), List.nth_opt parameters (index - 1)) with
    | Some expression, Some (_, expected) -> (
        let visible name = Option.is_none (lookup_local name c) in
        let consts = List.filter (fun (name, _, _) -> visible name) c.consts
        and arrays = List.filter (fun (name, _, _) -> visible name) c.arrays in
        match
          const_expr ~structs:c.structs ~named_types:c.named_types
            ~generic_structs:c.generic_structs ~arrays
            ~array_lengths:(static_array_lengths c.top_level_bindings c.globals)
            ~globals:(List.map (fun (global, _, _) -> global) c.globals)
            consts (Some expected) ~validate_dead:false expression
        with
        | Ok (actual, value)
          when Hir.ty_equal actual expected && is_int actual
               && (is_unsigned actual || value >= 0L) ->
            Some value
        | _ -> None)
    | _ -> None
  in
  let product indices =
    match List.map constant_size indices with
    | [ Some size ] -> Some size
    | [ Some left; Some right ]
      when right = 0L
           || Int64.unsigned_compare left (Int64.unsigned_div Int64.minus_one right)
              <= 0 ->
        Some (Int64.mul left right)
    | _ -> None
  in
  match List.assoc_opt name c.c_alloc_size_parameters with
  | None -> ()
  | Some attributes -> (
      match List.find_map product attributes with
      | None -> ()
      | Some extent ->
          let name =
            name ^ "(" ^ String.concat ", " (List.map Ast.expr_name arguments) ^ ")"
          in
          Sema_flow.set_call_object c.flow span name extent)

let access_footprint c ty =
  match ty with
  | Hir.Vec (lanes, Hir.Bool) -> Some ((lanes + 7) / 8)
  | Hir.Vec (lanes, element) -> (
      match Hir.layout c.structs element with
      | Ok (size, _) -> Some (size * lanes)
      | Error _ -> None)
  | _ -> (
      match Hir.layout c.structs ty with Ok (size, _) -> Some size | Error _ -> None)

let check_place_access c span ~write expression =
  match access_footprint c (Hir.expr_ty expression) with
  | None -> Ok ()
  | Some size -> check_address_access c span ~write (object_address c expression) size

let raw_view_index_error span (binding : Sema_flow.binding) =
  error span (Printf.sprintf "view `%s` must be indexed" binding.name)

let static_vector_lanes c = function
  | Hir.EBool (value, _) -> Some [ value ]
  | Hir.Local (binding, _) when binding.ty = Hir.Bool ->
      Sema_flow.mask_of c.flow binding
  | Hir.Local (binding, _) -> Sema_flow.mask_of c.flow binding
  | Hir.EVector (values, Hir.Vec (_, Hir.Bool), _) ->
      Some (List.map (fun value -> value <> 0L) values)
  | Hir.Vector_lit (values, Hir.Vec (_, Hir.Bool), _) ->
      let facts = List.map (condition_truth c) values in
      if List.for_all Option.is_some facts then Some (List.map Option.get facts)
      else None
  | Hir.Splat (Hir.EBool (value, _), Hir.Vec (lanes, Hir.Bool), _) ->
      Some (List.init lanes (fun _ -> value))
  | _ -> None

let static_vector_integers c = function
  | Hir.EVector (values, Hir.Vec (_, (Hir.Int _ as element)), _) ->
      Some (List.map (sign_extend_value element) values)
  | Hir.Vector_lit (values, Hir.Vec (_, (Hir.Int _ as element)), _) ->
      let values =
        List.map
          (fun value ->
            match value_fact c value with
            | Some fact when fact_single (Hir.expr_ty value) fact ->
                Some (sign_extend_value element fact.low)
            | _ -> None)
          values
      in
      if List.for_all Option.is_some values then Some (List.map Option.get values)
      else None
  | _ -> None

let check_simd_access c span ~write name access_ty args =
  let indexed =
    name = "gather" || name = "gather_bytes" || name = "scatter"
    || name = "scatter_bytes"
  in
  let base, indices, mask =
    if indexed then (List.nth args 0, Some (List.nth args 1), List.nth args 2)
    else (List.nth args 0, None, List.nth args 1)
  in
  match static_vector_lanes c mask with
  | None -> Ok ()
  | Some active ->
      let base_fact = address_fact c base in
      let element_size =
        match Hir.layout c.structs access_ty with Ok (size, _) -> size | Error _ -> 0
      in
      let lanes =
        match indices with
        | None -> Some (List.mapi (fun lane _ -> Int64.of_int lane) active)
        | Some indices -> static_vector_integers c indices
      in
      let scale =
        if String.ends_with ~suffix:"_bytes" name then 1L else Int64.of_int element_size
      in
      let rec check lane = function
        | [] -> Ok ()
        | false :: rest -> check (lane + 1) rest
        | true :: rest ->
            let* () =
              match base_fact with
              | Some (Sema_flow.Dead_local_address name) ->
                  check_address_access c span ~write
                    (Some (Sema_flow.Dead_local_address name)) element_size
              | Some (Sema_flow.Object_address address)
                when write && (not address.writable)
                     && Sema_flow.proof_checks_enabled c.flow ->
                  error span
                    (Printf.sprintf "write to constant storage `%s`" address.name)
              | _ -> Ok ()
            in
            let lane_offset =
              match lanes with
              | Some offsets when lane < List.length offsets ->
                  Some (Int64.mul (List.nth offsets lane) scale)
              | _ -> None
            in
            let* () =
              match (base_fact, lane_offset) with
              | Some fact, Some offset -> (
                  match shift_address Ast.Add fact offset with
                  | Some fact ->
                      check_address_access c span ~write (Some fact) element_size
                  | None -> Ok ())
              | _ -> Ok ()
            in
            check (lane + 1) rest
      in
      check 0 active

let reverse_comparison = function
  | Ast.Lt -> Ast.Gt
  | Ast.Le -> Ast.Ge
  | Ast.Gt -> Ast.Lt
  | Ast.Ge -> Ast.Le
  | op -> op

let grid_residue ty stride value =
  if is_unsigned ty then Int64.unsigned_rem value stride
  else
    let residue = Int64.rem value stride in
    if residue < 0L then Int64.add residue stride else residue

let grid_ge ty left right =
  if is_unsigned ty then Int64.unsigned_compare left right >= 0
  else Int64.compare left right >= 0

let snap_low ty (start, stride) low =
  let p = grid_residue ty stride start and q = grid_residue ty stride low in
  let up = if grid_ge ty p q then Int64.sub p q else Int64.sub stride (Int64.sub q p) in
  fact_add ty low up

let snap_high ty (start, stride) high =
  let p = grid_residue ty stride start and q = grid_residue ty stride high in
  let down =
    if grid_ge ty q p then Int64.sub q p else Int64.sub stride (Int64.sub p q)
  in
  fact_sub ty high down

let constrain_fact ?grid ty fact op constant truth =
  let low, high = (fact.low, fact.high) in
  let lower_bound value = fact_max ty low value
  and upper_bound value = fact_min ty high value in
  let predecessor = fact_sub ty constant 1L and successor = fact_add ty constant 1L in
  let interval =
    match (op, truth) with
    | Ast.Lt, true -> Option.map (fun high -> (low, upper_bound high)) predecessor
    | Ast.Lt, false -> Some (lower_bound constant, high)
    | Ast.Le, true -> Some (low, upper_bound constant)
    | Ast.Le, false -> Option.map (fun low -> (lower_bound low, high)) successor
    | Ast.Gt, true -> Option.map (fun low -> (lower_bound low, high)) successor
    | Ast.Gt, false -> Some (low, upper_bound constant)
    | Ast.Ge, true -> Some (lower_bound constant, high)
    | Ast.Ge, false -> Option.map (fun high -> (low, upper_bound high)) predecessor
    | Ast.Eq, true -> Some (lower_bound constant, upper_bound constant)
    | Ast.Ne, false -> Some (lower_bound constant, upper_bound constant)
    | Ast.Eq, false | Ast.Ne, true -> Some (low, high)
    | _ -> None
  in
  let interval =
    match (grid, fact.induction, interval) with
    | Some grid, Some _, Some (low, high) ->
        Option.bind (snap_low ty grid low) (fun low ->
            Option.map (fun high -> (low, high)) (snap_high ty grid high))
    | _, _, interval -> interval
  in
  match interval with
  | Some (low, high) when fact_order ty low high <= 0 ->
      let induction =
        if grid <> None || (low, high) = (fact.low, fact.high) then fact.induction
        else None
      in
      Some { low; high; induction }
  | _ -> None

let refine_condition (c : context) expression truth =
  let set_local binding op constant truth =
    let fact =
      match Sema_flow.value_of c.flow binding with
      | Some fact -> fact
      | None ->
          let low, high = fact_bounds binding.ty in
          { low; high; induction = None }
    in
    match fact with
    | { induction = Some id; _ }
      when Sema_flow.induction_valid c.flow id && (op = Ast.Eq || op = Ast.Ne) ->
        ()
    | fact ->
        let grid =
          match fact.induction with
          | Some id when id = binding.id -> Sema_flow.induction_grid c.flow id
          | _ -> None
        in
        Sema_flow.set_value c.flow binding
          (constrain_fact ?grid binding.ty fact op constant truth)
  in
  let rec apply expression truth =
    match expression with
    | Hir.Unary (Ast.Not, inner, _, _) -> apply inner (not truth)
    | Hir.Binary (Ast.And, left, right, _, _) when truth ->
        apply left true;
        apply right true
    | Hir.Binary (Ast.Or, left, right, _, _) when not truth ->
        apply left false;
        apply right false
    | Hir.Binary (((Ast.Eq | Ast.Ne) as op), Hir.Local (binding, _), right, _, _)
      when match binding.ty with Hir.Addr | Hir.Handle _ -> true | _ -> false ->
        let equal = op = Ast.Eq = truth in
        if equal then Sema_flow.set_address c.flow binding (address_fact c right)
    | Hir.Binary (((Ast.Eq | Ast.Ne) as op), left, Hir.Local (binding, _), _, _)
      when match binding.ty with Hir.Addr | Hir.Handle _ -> true | _ -> false ->
        let equal = op = Ast.Eq = truth in
        if equal then Sema_flow.set_address c.flow binding (address_fact c left)
    | Hir.Binary
        ( ((Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge) as op),
          Hir.Local (binding, _),
          right,
          _,
          _ ) -> (
        match value_fact c right with
        | Some constant when fact_single (Hir.expr_ty right) constant ->
            set_local binding op constant.low truth
        | _ -> ())
    | Hir.Binary
        ( ((Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge) as op),
          left,
          Hir.Local (binding, _),
          _,
          _ ) -> (
        match value_fact c left with
        | Some constant when fact_single (Hir.expr_ty left) constant ->
            set_local binding (reverse_comparison op) constant.low truth
        | _ -> ())
    | _ -> ()
  in
  apply expression truth

let aggregate_construction ?(zero_padding = false) ty entries span =
  let rec zero = function
    | Hir.Init_zero _ | Hir.Init_omitted _ -> true
    | Hir.Init_value (Hir.EInt (0L, _, _) | Hir.EBool (false, _) | Hir.Null _) -> true
    | Hir.Init_aggregate (_, _, children, _) -> List.for_all zero children
    | _ -> false
  in
  let rec bytewise_zero = function
    | Hir.Init_zero (_, true, _) -> true
    | Hir.Init_aggregate (_, _, children, _) -> List.exists bytewise_zero children
    | _ -> false
  in
  if (not zero_padding) && List.for_all zero entries then
    let zero = Hir.zero_initializer ty in
    Hir.Init_zero
      ( zero,
        List.exists bytewise_zero entries
        || (entries = [] && Option.is_none (Hir.zero_integer_array_values zero)),
        span )
  else Hir.Init_aggregate (ty, zero_padding, entries, span)

let c_unsupported c span name =
  Option.map
    (fun reason ->
      error span (Printf.sprintf "C declaration `%s` is not supported: %s" name reason))
    (List.assoc_opt name c.c_unsupported)

let expression_mentions_name_ref : (string -> Ast.expr -> bool) ref =
  ref (fun _ _ -> false)

let rec source_ty_in_context c span = function
  | Ast.Named_type (name, type_span) when Option.is_some (lookup_local name c) ->
      error type_span (Printf.sprintf "`%s` is a value, not a type" name)
  | Ast.Named_type (name, type_span) -> (
      match lookup_top_level name c.top_level_bindings with
      | Some { declaration_kind = Top_const; _ } ->
          error type_span (Printf.sprintf "`%s` is a constant, not a type" name)
      | Some { declaration_kind = Top_function; _ } ->
          error type_span (Printf.sprintf "`%s` is a function, not a type" name)
      | Some { declaration_kind = Top_global; _ } ->
          error type_span (Printf.sprintf "`%s` is a global, not a type" name)
      | Some { declaration_kind = Top_type; _ } | None ->
          source_ty_diag c.named_types type_span (Ast.Named_type (name, type_span)))
  | Ast.Array (length, ty) ->
      source_aggregate_in_context c "array" length (fun n t -> Hir.Array (n, t)) ty
  | Ast.Vec (length, element_ty) -> (
      let* result =
        source_aggregate_in_context c "vector" length
          (fun n t -> Hir.Vec (n, t))
          element_ty
      in
      match result with
      | Hir.Vec (n, element) -> (
          match Sema_types.vec_cap_error n element with
          | Some message -> error (Sema_types.source_type_span span element_ty) message
          | None -> Ok result)
      | _ -> Ok result)
  | ty -> source_ty_diag c.named_types span ty

and source_aggregate_in_context c kind length_info make element =
  let shadowed =
    List.find_opt
      (fun name -> !expression_mentions_name_ref name length_info.Ast.expression)
      (Sema_flow.local_names c.flow)
  in
  let* length =
    match shadowed with
    | Some name ->
        error length_info.span
          (Printf.sprintf "`%s` is not a compile-time constant" name)
    | None ->
        let evaluated =
          const_expr ~structs:c.structs ~named_types:c.named_types
            ~generic_structs:c.generic_structs ~arrays:c.arrays
            ~array_lengths:
              (Sema_context.static_array_lengths c.top_level_bindings c.globals)
            ~globals:(List.map (fun (name, _, _) -> name) c.globals)
            c.consts
            (aggregate_length_expected length_info.expression)
            length_info.expression
        in
        let evaluated = remap_length_cycle length_info.span evaluated in
        let* ty, value = evaluated in
        aggregate_length_value kind length_info.span ty value
  in
  let* element = source_ty_in_context c length_info.span element in
  Ok (make length element)

let intern_string c span s =
  let pool = c.string_pool in
  match Hashtbl.find_opt pool.index s with
  | Some id -> Ok id
  | None ->
      let size = String.length s in
      let budget = pool.budget in
      if size > budget then
        error span
          (Printf.sprintf
             "string literal bytes exceed budget max_interned_string_bytes of %d \
              (profile %s)"
             budget pool.budget_profile)
      else if pool.bytes_used > budget - size then
        error span
          (Printf.sprintf
             "cumulative interned string bytes exceed budget max_interned_string_bytes \
              of %d (profile %s)"
             budget pool.budget_profile)
      else
        let id = pool.next_id in
        pool.next_id <- id + 1;
        pool.reversed <- s :: pool.reversed;
        pool.bytes_used <- pool.bytes_used + size;
        Hashtbl.add pool.index s id;
        Ok id

(* names the locals of value struct brace lists *)
let construct_counter = ref 0

let with_dead_check c dead check = Sema_flow.with_dead_check c.flow dead check
(* arrays and structs other than value structs: these are not values *)
let aggregate_value_type = function
  | Hir.Array _ -> true
  | Hir.Struct _ as ty -> not (Hir.is_value_struct ty)
  | _ -> false

let imported_record_type c name =
  List.exists
    (function _, C_record_name (record, None) when record = name -> true | _ -> false)
    c.named_types

let address_type_for_expected c expected place =
  match (expected, Hir.expr_ty place) with
  | Some (Hir.Handle record as ty), Hir.Struct actual
    when record = actual && imported_record_type c record ->
      ty
  | _ -> Hir.Addr

let rec rooted_in_constant c = function
  | Hir.Const_array _ | Hir.EVector _ -> true
  | Hir.Global (name, _, _) -> (
      match lookup_top_level name c.top_level_bindings with
      | Some { declaration_kind = Top_const; _ } -> true
      | _ -> false)
  | Hir.Index (a, _, _, _)
  | Hir.Field (a, _, _, _, _)
  | Hir.Address (a, _, _)
  | Hir.Cast (_, a, _, _) ->
      rooted_in_constant c a
  | Hir.Ternary (_, a, b, _, _) -> rooted_in_constant c a || rooted_in_constant c b
  | _ -> false

let rec rooted_in_string_literal = function
  | Hir.EString _ -> true
  | Hir.Index (a, _, _, _)
  | Hir.Field (a, _, _, _, _)
  | Hir.Address (a, _, _)
  | Hir.Cast (_, a, _, _) ->
      rooted_in_string_literal a
  | Hir.Ternary (_, a, b, _, _) ->
      rooted_in_string_literal a || rooted_in_string_literal b
  | _ -> false

let rec rooted_in_readonly_storage c = function
  | Hir.EString _ -> true
  | Hir.Raw_select (a, _, _, _) | Hir.Index (a, _, _, _) | Hir.Field (a, _, _, _, _) ->
      rooted_in_string_literal a || rooted_in_readonly_storage c a
  | Hir.Address (a, _, _) | Hir.Cast (_, a, _, _) ->
      rooted_in_string_literal a || rooted_in_constant c a
      || rooted_in_readonly_storage c a
  | Hir.Ternary (_, a, b, _, _) ->
      rooted_in_readonly_storage c a || rooted_in_readonly_storage c b
  | _ -> false

let rec inherited_view_access c = function
  | Hir.Local (binding, _) -> view_access c.flow binding
  | Hir.Index (base, _, _, _)
  | Hir.Field (base, _, _, _, _)
  | Hir.Raw_select (base, _, _, _)
  | Hir.Address (base, _, _)
  | Hir.Cast (_, base, _, _) ->
      inherited_view_access c base
  | Hir.Ternary (_, yes, no, _, _) ->
      let yes = inherited_view_access c yes and no = inherited_view_access c no in
      if yes = Constant_access || no = Constant_access then Constant_access
      else if yes = Readonly_access || no = Readonly_access then Readonly_access
      else Mutable_access
  | _ -> Mutable_access

let rec readonly_global_place c = function
  | Hir.Global (name, _, _) -> (
      match lookup_global name c.globals with
      | Some (_, _, Ast.Import_const_c) -> true
      | _ -> false)
  | Hir.Index (base, _, _, _) when aggregate_value_type (Hir.expr_ty base) ->
      readonly_global_place c base
  | Hir.Field (base, _, _, _, _) -> readonly_global_place c base
  | _ -> false

let rec readonly_imported_constant_name c = function
  | Hir.Global (name, ty, _) when aggregate_value_type ty -> (
      match lookup_global name c.globals with
      | Some (_, _, Ast.Import_const_c) -> Some name
      | _ -> None)
  | Hir.Index (base, _, _, _) when aggregate_value_type (Hir.expr_ty base) ->
      readonly_imported_constant_name c base
  | Hir.Field (base, _, _, _, _) -> readonly_imported_constant_name c base
  | Hir.Local (binding, _)
    when Sema_flow.is_view c.flow binding
         || Option.is_some (Sema_flow.raw_view_type c.flow binding) ->
      Sema_flow.view_readonly_name c.flow binding
  | Hir.Ternary (_, yes, no, _, _) -> (
      match
        (readonly_imported_constant_name c yes, readonly_imported_constant_name c no)
      with
      | Some yes, Some no when yes = no -> Some yes
      | _ -> None)
  | _ -> None

let rec readonly_storage_name c = function
  | Hir.Const_array (name, _, _) -> Some name
  | Hir.Address (base, _, _) | Hir.Cast (_, base, _, _) -> readonly_storage_name c base
  | expression -> readonly_imported_constant_name c expression

let readonly_string_source = function
  | Ast.String_lit _ as source -> Some source
  | Ast.Select ((Ast.String_lit _ as source), _, _) -> Some source
  | _ -> None

let rec readonly_view_name c = function
  | Hir.Local (binding, _)
    when is_view c.flow binding
         || Option.is_some (Sema_flow.raw_view_type c.flow binding) ->
      Some binding.name
  | Hir.Index (base, _, _, _) | Hir.Address (base, _, _) -> readonly_view_name c base
  | _ -> None

let is_fas_constant c name =
  List.exists
    (fun b -> b.declaration_name = name && b.declaration_kind = Top_const)
    c.top_level_bindings

let readonly_write_error ?source_expression c span expression =
  let storage_name =
    match (readonly_storage_name c expression, source_expression) with
    | Some name, _ -> Some name
    | None, Some (Ast.Ident (name, _)) when is_fas_constant c name -> Some name
    | _ -> None
  in
  let through_view =
    Option.fold ~none:""
      ~some:(fun n -> " (through view `" ^ n ^ "`)")
      (readonly_view_name c expression)
  in
  let message =
    match (Option.bind source_expression readonly_string_source, storage_name) with
    | Some source, _ ->
        Printf.sprintf "cannot modify string literal `%s`" (Ast.expr_name source)
    | None, Some name ->
        if is_fas_constant c name then Printf.sprintf "cannot modify constant `%s`" name
        else Printf.sprintf "cannot modify read-only C array `%s`" name
    | _ -> "cannot modify read-only pointer"
  in
  error span (message ^ through_view)

let view_access_of_expr c expression =
  if rooted_in_constant c expression then Constant_access
  else if rooted_in_readonly_storage c expression || readonly_global_place c expression
  then Readonly_access
  else inherited_view_access c expression

let rec expression_uses_view c = function
  | Hir.Local (binding, _) ->
      is_view c.flow binding || Option.is_some (Sema_flow.raw_view_type c.flow binding)
  | Hir.Index (base, _, _, _)
  | Hir.Field (base, _, _, _, _)
  | Hir.Raw_select (base, _, _, _)
  | Hir.Address (base, _, _)
  | Hir.Cast (_, base, _, _) ->
      expression_uses_view c base
  | Hir.Ternary (_, yes, no, _, _) ->
      expression_uses_view c yes || expression_uses_view c no
  | _ -> false

let unresolved_shape_key expression =
  let span = Ast.expr_span expression in
  (span.Span.file, span.Span.start_offset, span.Span.end_offset)

let shape_children = function
  | Ast.Parenthesized (operand, _) -> [ operand ]
  | Ast.Unary ((Ast.Neg | Ast.Bit_not), operand, _) -> [ operand ]
  | Ast.Binary
      ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
        | Ast.Bit_xor | Ast.Shl | Ast.Shr ),
        left,
        right,
        _ ) ->
      [ left; right ]
  | Ast.Ternary (_, yes, no, _) -> [ yes; no ]
  | _ -> []

let unresolved_shape_cache_worthwhile expression =
  let pending = ref [ expression ] in
  let operators = ref 0 in
  while !operators < 64 && !pending <> [] do
    match !pending with
    | current :: rest -> (
        pending := rest;
        match current with
        | Ast.Parenthesized (operand, _) -> pending := operand :: !pending
        | Ast.Unary ((Ast.Neg | Ast.Bit_not), operand, _) ->
            incr operators;
            pending := operand :: !pending
        | Ast.Binary
            ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and
              | Ast.Bit_or | Ast.Bit_xor | Ast.Shl | Ast.Shr ),
              left,
              right,
              _ ) ->
            incr operators;
            pending := left :: right :: !pending
        | Ast.Ternary (_, yes, no, _) ->
            incr operators;
            pending := yes :: no :: !pending
        | _ -> ())
    | [] -> ()
  done;
  !operators = 64

let build_unresolved_shapes expression =
  let shapes = Hashtbl.create 64 in
  let seen = Hashtbl.create 64 in
  let unique = ref true in
  let stack = ref [ (expression, false) ] in
  let cached_shape expression =
    Option.value ~default:None
      (Hashtbl.find_opt shapes (unresolved_shape_key expression))
  in
  while !stack <> [] do
    match !stack with
    | (current, true) :: rest ->
        stack := rest;
        let shape =
          match current with
          | Ast.Parenthesized (operand, _) -> cached_shape operand
          | Ast.Int_lit _ -> Some Unresolved_int
          | Ast.Float_lit _ -> Some Unresolved_float
          | Ast.Null _ -> Some Unresolved_null
          | Ast.Unary (Ast.Neg, operand, _) -> (
              match cached_shape operand with
              | Some Unresolved_int -> Some Unresolved_int
              | Some Unresolved_float -> Some Unresolved_float
              | Some Unresolved_vector -> Some Unresolved_vector
              | _ -> None)
          | Ast.Unary (Ast.Bit_not, operand, _) -> (
              match cached_shape operand with
              | Some Unresolved_int -> Some Unresolved_int
              | _ -> None)
          | Ast.Binary
              ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and
                | Ast.Bit_or | Ast.Bit_xor | Ast.Shl | Ast.Shr ),
                left,
                right,
                _ ) -> (
              match (cached_shape left, cached_shape right) with
              | Some left_shape, Some right_shape when left_shape = right_shape ->
                  Some left_shape
              | _ -> None)
          | Ast.Ternary (_, yes, no, _) -> (
              match (cached_shape yes, cached_shape no) with
              | Some yes_shape, Some no_shape when yes_shape = no_shape ->
                  Some yes_shape
              | _ -> None)
          | Ast.Splat _ | Ast.Array_lit _ -> Some Unresolved_vector
          | _ -> None
        in
        Hashtbl.replace shapes (unresolved_shape_key current) shape
    | (current, false) :: rest ->
        stack := (current, true) :: rest;
        let key = unresolved_shape_key current in
        if Hashtbl.mem seen key then unique := false else Hashtbl.add seen key ();
        List.iter
          (fun child -> stack := (child, false) :: !stack)
          (shape_children current)
    | [] -> ()
  done;
  (shapes, !unique)

let rec unresolved_shape_uncached expression =
  let combined left right =
    match (unresolved_shape_uncached left, unresolved_shape_uncached right) with
    | Some left_shape, Some right_shape when left_shape = right_shape -> Some left_shape
    | _ -> None
  in
  match expression with
  | Ast.Parenthesized (operand, _) -> unresolved_shape_uncached operand
  | Ast.Int_lit _ -> Some Unresolved_int
  | Ast.Float_lit _ -> Some Unresolved_float
  | Ast.Null _ -> Some Unresolved_null
  | Ast.Unary (Ast.Neg, operand, _) -> (
      match unresolved_shape_uncached operand with
      | Some (Unresolved_int | Unresolved_float | Unresolved_vector) as shape -> shape
      | _ -> None)
  | Ast.Unary (Ast.Bit_not, operand, _) -> (
      match unresolved_shape_uncached operand with
      | Some Unresolved_int -> Some Unresolved_int
      | _ -> None)
  | Ast.Binary
      ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
        | Ast.Bit_xor | Ast.Shl | Ast.Shr ),
        left,
        right,
        _ ) ->
      combined left right
  | Ast.Ternary (_, yes, no, _) -> combined yes no
  | Ast.Splat _ | Ast.Array_lit _ -> Some Unresolved_vector
  | _ -> None

let unresolved_shape_of (c : context) expression =
  if c.unresolved_shapes_unique then
    match Hashtbl.find_opt c.unresolved_shapes (unresolved_shape_key expression) with
    | Some shape -> shape
    | None -> unresolved_shape_uncached expression
  else unresolved_shape_uncached expression

let rec builtin_literal_operand = function
  | Ast.Parenthesized (expression, _) -> builtin_literal_operand expression
  | Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _) -> true
  | Ast.Array_lit (entries, _) -> List.for_all builtin_literal_operand entries
  | _ -> false

let unresolved_float_operand c expression =
  match unresolved_shape_of c expression with
  | Some (Unresolved_float | Unresolved_vector) -> true
  | _ -> false

let rec unresolved_vector_elements c expression =
  match expression with
  | Ast.Splat (element, _) -> (
      match unresolved_shape_of c element with
      | Some Unresolved_int -> true
      | _ -> false)
  | Ast.Binary
      ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
        | Ast.Bit_xor ),
        left,
        right,
        _ ) ->
      unresolved_vector_elements c left && unresolved_vector_elements c right
  | Ast.Binary ((Ast.Shl | Ast.Shr), _, _, _) -> false
  | _ -> false

let rec is_if_operand = function
  | Ast.Ternary _ -> true
  | Ast.Parenthesized (expression, _) -> is_if_operand expression
  | _ -> false

let operand_type_hint c operation expected left right =
  let expected = if is_if_operand left || is_if_operand right then None else expected in
  match operation with
  | Ast.Add | Ast.Sub -> (
      match expected with Some Hir.Addr -> Some (Hir.Int Hir.U64) | e -> e)
  | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or | Ast.Bit_xor -> expected
  | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
      match expected with
      | Some (Hir.Vec (lanes, _))
        when unresolved_vector_elements c left && unresolved_vector_elements c right ->
          Some (Hir.Vec (lanes, Hir.Int Hir.I32))
      | _ -> None)
  | Ast.And | Ast.Or -> None
  | Ast.Shl | Ast.Shr -> None

let contextual_peer_type c operation peer operand =
  if is_if_operand operand then None
  else
    match (peer, unresolved_shape_of c operand, operation) with
    | Hir.Addr, Some Unresolved_null, (Ast.Eq | Ast.Ne) -> Some Hir.Addr
    | Hir.Addr, _, _ -> Some (Hir.Int Hir.U64)
    | Hir.Handle _, Some Unresolved_int, _ -> None
    | peer, _, _ -> Some peer

let select_value_arg span payload =
  match payload with
  | Ast.Const_arg e -> Ok e
  | Ast.Name_arg (name, name_span) -> Ok (Ast.Ident (name, name_span))
  | Ast.Type_or_index t -> Ok (Ast.index_expression t)
  | Ast.Type_arg _ -> error span "index payload must be a value expression"

let select_type_arg named_types span payload =
  match payload with
  | Ast.Type_arg (Ast.Named_type ("void", _))
  | Ast.Type_or_index (Ast.Named_type ("void", _)) ->
      Ok Hir.Void
  | Ast.Type_arg t | Ast.Type_or_index t -> source_ty_diag named_types span t
  | Ast.Name_arg (name, name_span) ->
      source_ty_diag named_types name_span (Ast.Named_type (name, name_span))
  | Ast.Const_arg e -> error (Ast.expr_span e) "raw access requires a type argument"

let select_type_payload_span fallback = function
  | Ast.Type_arg (Ast.Named_type ("void", span))
  | Ast.Type_or_index (Ast.Named_type ("void", span)) ->
      span
  | _ -> fallback

let rec view_source_span expression =
  let span =
    match expression with
    | Ast.Parenthesized (_, span) -> span
    | _ -> Ast.expr_span expression
  in
  let children =
    match expression with
    | Ast.Unary (_, value, _)
    | Ast.C_dereference (value, _, _)
    | Ast.C_dot_star (value, _)
    | Ast.Parenthesized (value, _)
    | Ast.Cast (_, _, value, _)
    | Ast.Addr_of (value, _)
    | Ast.Handle_from_addr (_, value, _)
    | Ast.Sizeof_value (value, _)
    | Ast.Splat (value, _) ->
        [ value ]
    | Ast.Binary (_, left, right, _) -> [ left; right ]
    | Ast.Generic_args (callee, _, _) -> [ callee ]
    | Ast.Ternary (condition, yes, no, _) -> [ condition; yes; no ]
    | Ast.Array_lit (values, _) -> values
    | _ -> []
  in
  let spans = span :: List.map view_source_span children in
  let first =
    List.fold_left
      (fun best item ->
        if item.Span.start_offset < best.Span.start_offset then item else best)
      span spans
  in
  let last_offset =
    List.fold_left
      (fun best item -> max best item.Span.end_offset)
      span.Span.end_offset spans
  in
  { first with Span.end_offset = last_offset }

let normalize_offset_expr structs span (e : Hir.expr) =
  match Hir.expr_ty e with
  | Hir.Int k ->
      let* usize_size, _ = layout_diag span structs (Hir.Int Hir.U64) in
      let target_bits = usize_size * 8 in
      let bits =
        match k with
        | Hir.U8 | Hir.I8 -> 8
        | Hir.U16 | Hir.I16 -> 16
        | Hir.U32 | Hir.I32 -> 32
        | Hir.U64 | Hir.I64 -> 64
      in
      let signed =
        match k with
        | Hir.I8 | Hir.I16 | Hir.I32 | Hir.I64 -> true
        | _ -> false
      in
      if bits = target_bits then Ok e
      else if bits < target_bits then
        Ok
          (Hir.Cast ((if signed then Ast.Sext else Ast.Zext), e, Hir.Int Hir.U64, span))
      else Ok (Hir.Cast (Ast.Trunc, e, Hir.Int Hir.U64, span))
  | _ -> error span "offset must be an integer"

let query_layout_in_context (c : context) span ty =
  let globals = List.map (fun (name, _, _) -> name) c.globals in
  let evaluate consts expression expected =
    const_expr
      ~array_lengths:(static_array_lengths c.top_level_bindings c.globals)
      ~structs:c.structs ~named_types:c.named_types ~generic_structs:c.generic_structs
      ~arrays:c.arrays ~globals consts expected expression
  in
  query_layout ~structs:c.structs ~named_types:c.named_types
    ~generic_structs:c.generic_structs ~globals ~evaluate c.consts span ty

let rec check_place_inner (c : context) expr =
  let visible_consts =
    List.filter (fun (name, _, _) -> Option.is_none (lookup_local name c)) c.consts
  in
  let static_index source =
    match
      const_expr
        ~array_lengths:(static_array_lengths c.top_level_bindings c.globals)
        ~structs:c.structs ~named_types:c.named_types ~generic_structs:c.generic_structs
        ~arrays:c.arrays
        ~globals:(List.map (fun (name, _, _) -> name) c.globals)
        visible_consts None ~validate_dead:false source
    with
    | Ok (ty, value) -> Known (ty, value)
    | Error _ -> Dynamic
  in
  let index_outside ty fact length =
    let count = Int64.of_int length in
    let unsigned = is_unsigned ty in
    let all_outside =
      if unsigned then Int64.unsigned_compare fact.low count >= 0
      else fact.high < 0L || fact.low >= count
    in
    let induction_outside =
      match fact.induction with
      | Some id when Sema_flow.induction_valid c.flow id ->
          if unsigned then Int64.unsigned_compare fact.high count >= 0
          else fact.low < 0L || fact.high >= count
      | _ -> false
    in
    all_outside || induction_outside
  in
  match expr with
  | Ast.Ident ("call_addr", s) -> error s "call_addr is a builtin and has no address"
  | Ast.Ident (n, s)
    when Option.is_none (lookup_local n c)
         && Option.is_some (List.assoc_opt n c.c_unsupported) ->
      Option.get (c_unsupported c s n)
  | Ast.Ident (n, s) -> (
      match lookup_local n c with
      | Some b when Option.is_some (Sema_flow.raw_view_type c.flow b) ->
          raw_view_index_error s b
      | Some b ->
          let root, path =
            match view_origin c.flow b with
            | Some (root, path) -> (Some root, Some path)
            | None -> (None, None)
          in
          Ok { expr = Hir.Local (b, s); root; path }
      | None -> (
          match lookup n c.arrays with
          | Some (_, (Hir.Array _ as t), _) ->
              Ok { expr = Hir.Const_array (n, t, s); root = None; path = None }
          | Some (_, (Hir.Vec _ as t), values) ->
              Ok { expr = Hir.EVector (values, t, s); root = None; path = None }
          | Some _ -> error s (Printf.sprintf "constant `%s` cannot be addressed" n)
          | None -> (
              match lookup_top_level n c.top_level_bindings with
              | Some { declaration_kind = Top_const; _ } -> (
                  match lookup_global n c.globals with
                  | Some (_, ((Hir.Array _ | Hir.Struct _) as ty), _) ->
                      Ok { expr = Hir.Global (n, ty, s); root = None; path = None }
                  | _ -> error s (Printf.sprintf "constant `%s` cannot be addressed" n))
              | Some { declaration_kind = Top_type; _ } ->
                  error s (Printf.sprintf "type `%s` cannot be used as a value" n)
              | Some { declaration_kind = Top_function; _ } ->
                  if List.mem n c.external_c_functions then
                    Ok { expr = Hir.Function_address (n, s); root = None; path = None }
                  else
                    error s
                      (Printf.sprintf "function `%s` must be called to produce a value"
                         n)
              | Some { declaration_kind = Top_global; _ } -> (
                  match lookup_global n c.globals with
                  | Some (_, ty, _) ->
                      Ok { expr = Hir.Global (n, ty, s); root = None; path = None }
                  | None -> error s "internal error: global declaration is missing")
              | None -> unknown_value_error (visible_value_names c) s n)))
  | Ast.Select (a, args, s) -> (
      match a with
      | Ast.Ident (name, name_span)
        when Option.is_some (lookup_local name c)
             && Option.is_some
                  (Sema_flow.raw_view_type c.flow (Option.get (lookup_local name c)))
        -> (
          let binding = Option.get (lookup_local name c) in
          match (Sema_flow.raw_view_type c.flow binding, args) with
          | Some element, [ payload ] ->
              let* offset = raw_offset_expr c s element (Some payload) in
              Ok
                {
                  expr =
                    Hir.Raw_select (Hir.Local (binding, name_span), offset, element, s);
                  root = None;
                  path = None;
                }
          | _ -> raw_view_index_error s binding)
      | Ast.Ident (name, _)
        when match List.assoc_opt name c.templates with
             | Some (Ast.Func _) -> true
             | _ -> false ->
          error s
            (Printf.sprintf
               "generic function `%s` needs a call after its type arguments" name)
      | _ -> (
          let* base = check_place c a in
          match (Hir.expr_ty base.expr, args) with
          | Hir.Vec _, [ _ ]
            when match base.expr with Hir.Raw_select _ -> true | _ -> false ->
              error s "raw vector lane selection is not supported"
          | Hir.Addr, [ Ast.Const_arg index ] ->
              Error
                [
                  Sema_types.raw_access_needs_type_error ~index:(Ast.expr_name index) a
                    s;
                ]
          | Hir.Addr, [ Ast.Name_arg (name, _) ]
            when Option.is_some (lookup_local name c)
                 ||
                 match lookup_top_level name c.top_level_bindings with
                 | Some { declaration_kind = Top_const | Top_global; _ } -> true
                 | _ -> false ->
              Error
                [
                  Sema_types.raw_access_needs_type_error
                    ~allow_help:
                      (not
                         (Option.is_some (lookup_local name c)
                         && List.mem_assoc name c.named_types))
                    ~index:name a s;
                ]
          | Hir.Addr, [ Ast.Name_arg (name, name_span) ]
            when Result.is_error
                   (Sema_types.source_ty c.named_types
                      (Ast.Named_type (name, name_span))) -> (
              match
                Sema_types.source_ty c.named_types (Ast.Named_type (name, name_span))
              with
              | Ok _ -> Error [ Sema_types.raw_access_needs_type_error a s ]
              | Error message -> (
                  match Sema_types.unknown_type_name message with
                  | Some _ ->
                      Error
                        [
                          Sema_types.unknown_type_error (List.map fst c.named_types)
                            name_span name;
                        ]
                  | None -> Error [ Sema_types.raw_access_needs_type_error a s ]))
          | (Hir.Array (length, e) | Hir.Vec (length, e)), [ payload ] -> (
              let* i = select_value_arg s payload in
              let* checked_index = check_expr c None i in
              if not (is_int (Hir.expr_ty checked_index)) then
                index_type_error "array index" i (Hir.expr_ty checked_index)
              else
                match static_index i with
                | Known (ty, value)
                  when let value = sign_extend_value ty value in
                       (value < 0L || value >= Int64.of_int length)
                       && Sema_flow.proof_checks_enabled c.flow ->
                    error (Ast.expr_span i)
                      (Printf.sprintf "array index `%s` is out of bounds for length %d"
                         (if is_unsigned ty then
                            Printf.sprintf "%Lu" (sign_extend_value ty value)
                          else Int64.to_string (sign_extend_value ty value))
                         length)
                | Known (ty, value) ->
                    let value = sign_extend_value ty value in
                    let index = Int64.to_int value in
                    let path =
                      match (base.root, base.path) with
                      | Some _, Some (Exact path) ->
                          Some (Exact (path @ [ Element index ]))
                      | Some _, Some (Dynamic_prefix path) -> Some (Dynamic_prefix path)
                      | _ -> None
                    in
                    Ok
                      {
                        expr =
                          Hir.Index
                            (base.expr, Hir.EInt (value, ty, Ast.expr_span i), e, s);
                        root = base.root;
                        path;
                      }
                | Dynamic ->
                    let fact = value_fact c checked_index in
                    if
                      Sema_flow.proof_checks_enabled c.flow
                      && fact <> None
                      && index_outside (Hir.expr_ty checked_index) (Option.get fact)
                           length
                    then
                      error (Ast.expr_span i)
                        (Printf.sprintf "array index is out of bounds for length %d"
                           length)
                    else
                      let known =
                        match fact with
                        | Some fact when fact_single (Hir.expr_ty checked_index) fact ->
                            true
                        | _ -> false
                      in
                      let value_path =
                        match (base.root, base.path, known) with
                        | Some _, Some (Exact path), true ->
                            Some
                              (Exact
                                 (path
                                 @ [ Element (Int64.to_int (Option.get fact).low) ]))
                        | Some _, Some (Exact path), false -> Some (Dynamic_prefix path)
                        | Some _, Some (Dynamic_prefix path), _ ->
                            Some (Dynamic_prefix path)
                        | _ -> None
                      in
                      Ok
                        {
                          expr = Hir.Index (base.expr, checked_index, e, s);
                          root = base.root;
                          path = value_path;
                        })
          | (Hir.Array _ | Hir.Vec _), _ -> error s "array index takes one expression"
          | Hir.Addr, [ type_payload ] ->
              let* () =
                match (base.root, base.path) with
                | Some binding, Some path -> require_place_state binding path c s
                | _ -> Ok ()
              in
              let* t = select_type_arg c.named_types s type_payload in
              let* () =
                match t with
                | Hir.Void ->
                    error
                      (select_type_payload_span s type_payload)
                      "raw access on `void` needs an element type"
                | _ -> Ok ()
              in
              let* offset = raw_offset_expr c s t None in
              Ok
                {
                  expr = Hir.Raw_select (base.expr, offset, t, s);
                  root = base.root;
                  path = base.path;
                }
          | Hir.Addr, [ type_payload; index_payload ] ->
              let* () =
                match (base.root, base.path) with
                | Some binding, Some path -> require_place_state binding path c s
                | _ -> Ok ()
              in
              let* t = select_type_arg c.named_types s type_payload in
              let* () =
                match t with
                | Hir.Void ->
                    error
                      (select_type_payload_span s type_payload)
                      "raw access on `void` needs an element type"
                | _ -> Ok ()
              in
              let* offset = raw_offset_expr c s t (Some index_payload) in
              Ok
                {
                  expr = Hir.Raw_select (base.expr, offset, t, s);
                  root = base.root;
                  path = base.path;
                }
          | Hir.Addr, _ ->
              error s "raw access needs a type argument and an optional index"
          | Hir.Handle _, _ -> error s "cannot select through a handle"
          | _ -> error s "cannot index this type"))
  | Ast.Field (a, n, s) -> (
      let* base = check_place c a in
      match base.expr with
      | Hir.Raw_select (baddr, off, Hir.Struct sn, _) -> (
          match field_info c.structs sn n with
          | Some { unsupported_reason = Some reason; _ } -> error s reason
          | Some f ->
              index_field c sn n s;
              Ok
                {
                  expr =
                    Hir.Raw_select
                      ( baddr,
                        Hir.Binary
                          ( Ast.Add,
                            off,
                            Hir.EInt (Int64.of_int f.offset, Hir.Int Hir.U64, s),
                            Hir.Int Hir.U64,
                            s ),
                        f.ty,
                        s );
                  root = base.root;
                  path = base.path;
                }
          | None -> missing_field_error ~base:a c s n (Hir.Struct sn))
      | Hir.Raw_select (_, _, access_ty, _) ->
          missing_field_error ~base:a c (Ast.expr_span a) n access_ty
      | _ -> (
          match Hir.expr_ty base.expr with
          | Hir.Struct sn -> (
              match field_info c.structs sn n with
              | Some { unsupported_reason = Some reason; _ } -> error s reason
              | Some f ->
                  index_field c sn n s;
                  Ok
                    {
                      expr = Hir.Field (base.expr, n, f.ty, f.offset, s);
                      root = base.root;
                      path =
                        (match base.path with
                        | Some (Exact path) -> Some (Exact (path @ [ Field n ]))
                        | Some (Dynamic_prefix path) -> Some (Dynamic_prefix path)
                        | None -> None);
                    }
              | None -> missing_field_error ~base:a c s n (Hir.Struct sn))
          | actual -> missing_field_error ~base:a c (Ast.expr_span a) n actual))
  | Ast.Arrow_field (base, field, operator_span, _) -> (
      let* value = check_expr c None base in
      match Hir.expr_ty value with
      | Hir.Addr ->
          let help =
            let records =
              List.filter
                (fun (record : Hir.struct_def) ->
                  List.exists
                    (fun (candidate : Hir.field) ->
                      candidate.name = field
                      && Option.is_none candidate.unsupported_reason)
                    record.fields)
                c.structs
            in
            match records with
            | [ record ] ->
                Some
                  (Printf.sprintf "read field `%s` through an `addr` as `%s[%s].%s`"
                     field (Ast.expr_name base) record.name field)
            | _ -> None
          in
          error ?help operator_span "Fas has no `->` operator"
      | Hir.Struct _ ->
          error operator_span
            (Printf.sprintf "Fas has no `->`; access field `%s` with `.`" field)
      | _ ->
          error operator_span
            (Printf.sprintf "Fas has no `->`; field `%s` needs a record or `addr`" field)
      )
  | e ->
      let* checked = check_expr c None e in
      Ok { expr = checked; root = None; path = None }

and check_place (c : context) expr =
  let result = check_place_inner c expr in
  (match (c.index, expr, result) with
  | Some _, Ast.Ident (name, span), Ok place ->
      index_ident c name span;
      index_type c span (Hir.expr_ty place.expr) None
  | Some _, _, Ok place ->
      index_type c (Ast.expr_span expr) (Hir.expr_ty place.expr) None
  | _ -> ());
  result

and raw_offset_expr c s access_ty index_payload =
  match index_payload with
  | None -> Ok (Hir.EInt (0L, Hir.Int Hir.U64, s))
  | Some payload ->
      let* i = select_value_arg s payload in
      let* idx = check_expr c None i in
      if not (is_int (Hir.expr_ty idx)) then
        index_type_error "raw access index" i (Hir.expr_ty idx)
      else
        let* norm = normalize_offset_expr c.structs s idx in
        let* size, _ = layout_diag s c.structs access_ty in
        Ok
          (Hir.Binary
             ( Ast.Mul,
               norm,
               Hir.EInt (Int64.of_int size, Hir.Int Hir.U64, s),
               Hir.Int Hir.U64,
               s ))

and check_expr ?destination ?(allow_widen = true) (c : context) expected expression =
  let previous_depth = c.expression_depth in
  if previous_depth = 0 then c.unresolved_shapes_unique <- false;
  if previous_depth = 0 && unresolved_shape_cache_worthwhile expression then (
    let shapes, unique = build_unresolved_shapes expression in
    c.unresolved_shapes <- shapes;
    c.unresolved_shapes_unique <- unique);
  c.expression_depth <- previous_depth + 1;
  let result = check_expr_inner ?destination c expected expression in
  c.expression_depth <- previous_depth;
  index_expression c expression result;
  let result =
    match (allow_widen, expected, result) with
    | true, Some target, Ok value -> (
        match
          Sema_types.convert_expected_kind ~expression (Hir.expr_ty value) target
        with
        | Some kind -> Ok (Hir.Cast (kind, value, target, Ast.expr_span expression))
        | None -> Ok value)
    | _ -> result
  in
  match (expression, result) with
  | Ast.Binary (op, left, right, span), Error _ -> (
      match Sema_types.comparison_chain_diagnostic span op left right with
      | Some diagnostic -> Error [ diagnostic ]
      | None -> result)
  | _ -> result

and check_expr_inner ?destination (c : context) expected expression =
  let peer_handle_type expression =
    let before = Sema_flow.snapshot c.flow in
    let checked = check_expr c None expression in
    Sema_flow.restore c.flow before;
    match checked with
    | Ok value -> (
        match Hir.expr_ty value with Hir.Handle _ as ty -> Some ty | _ -> None)
    | Error _ -> None
  in
  let vector_literal literal_type lanes element entries span =
    if List.length entries <> lanes then
      error span "wrong number of vector literal lanes"
    else
      let rec go index acc = function
        | [] -> Ok (List.rev acc)
        | entry :: rest ->
            let* value = check_expr c (Some element) entry in
            let* () =
              let context =
                match destination with
                | Some name ->
                    Printf.sprintf "element %d of vector `%s`" (index + 1) name
                | None -> Printf.sprintf "lane %d of vector literal" (index + 1)
              in
              ensure_expected ~checked_expression:value ~context ~expression:entry
                (Hir.expr_ty value) element (Ast.expr_span entry)
            in
            go (index + 1) (value :: acc) rest
      in
      let* entries = go 0 [] entries in
      Ok (Hir.Vector_lit (entries, literal_type, span))
  in
  match expression with
  | Ast.Parenthesized (value, _) -> check_expr_inner ?destination c expected value
  | Ast.Ident ("call_addr", s) -> error s "call_addr is a builtin and has no address"
  | (Ast.C_dereference _ | Ast.C_dot_star _) as expression ->
      Error [ Sema_types.c_pointer_selection_diagnostic expected expression ]
  | Ast.Int_lit (raw, s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error s m ]) in
      let ty = Option.value ~default:(Hir.Int Hir.I32) expected in
      if fits_literal ty v then Ok (Hir.EInt (mask_value ty v, ty, s))
      else
        Error
          [
            Sema_types.integer_literal_error s ty ~negative:false ~text:raw ~magnitude:v;
          ]
  | Ast.Float_lit (raw, s) -> (
      let ty = Option.value ~default:(Hir.Float Hir.F64) expected in
      match ty with
      | Hir.Float _ ->
          let* bits =
            parse_float_literal ty raw
            |> Result.map_error (fun message -> [ Diag.error s message ])
          in
          Ok (Hir.EFloat (bits, ty, s))
      | _ -> Error [ Sema_types.float_literal_type_error ty s ])
  | Ast.Bool_lit (v, s) -> Ok (Hir.EBool (v, s))
  | Ast.Null s -> (
      match expected with
      | Some (Hir.Addr | Hir.Handle _) as ty -> Ok (Hir.Null (Option.get ty, s))
      | _ -> error s "null requires an addr or handle context")
  | Ast.String_lit (cstr, v, s) ->
      if cstr && String.contains v '\000' then
        error s "C string literal cannot contain embedded NUL"
      else
        let value = if cstr then v ^ "\000" else v in
        let* id = intern_string c s value in
        Ok (Hir.EString (id, cstr, s))
  | Ast.Ident (n, s)
    when Option.is_none (lookup_local n c)
         && Option.is_some (List.assoc_opt n c.c_unsupported) ->
      Option.get (c_unsupported c s n)
  | Ast.Ident (n, s) -> (
      match lookup_local n c with
      | Some b when Option.is_some (Sema_flow.raw_view_type c.flow b) ->
          raw_view_index_error s b
      | Some b ->
          let* () =
            match view_origin c.flow b with
            | Some (root, path) -> require_place_state root path c s
            | None -> Ok ()
          in
          let* () =
            if Sema_flow.is_view c.flow b then
              check_place_access c s ~write:false (Hir.Local (b, s))
            else Ok ()
          in
          Ok (Hir.Local (b, s))
      | None -> (
          match lookup_top_level n c.top_level_bindings with
          | Some { declaration_kind = Top_type; _ } ->
              error s (Printf.sprintf "`%s` is a type, not a value" n)
          | Some { declaration_kind = Top_function; _ } ->
              error s (Printf.sprintf "`%s` is a function, not a value" n)
          | Some { declaration_kind = Top_global; _ } -> (
              match lookup_global n c.globals with
              | Some (_, ty, _) -> Ok (Hir.Global (n, ty, s))
              | None -> error s "internal error: global declaration is missing")
          | (Some { declaration_kind = Top_const; _ } | None)
            when Option.is_some (lookup_global n c.globals) ->
              let _, ty, _ = Option.get (lookup_global n c.globals) in
              Ok (Hir.Global (n, ty, s))
          | Some { declaration_kind = Top_const; _ } | None -> (
              match lookup n c.consts with
              | Some (_, ((Hir.Addr | Hir.Handle _) as t), 0L) -> Ok (Hir.Null (t, s))
              | Some (_, (Hir.Addr | Hir.Handle _), _) ->
                  error s "address and handle constants must be null"
              | Some (_, (Hir.Float _ as t), v) -> Ok (Hir.EFloat (v, t, s))
              | Some (_, t, v) -> Ok (Hir.EInt (v, t, s))
              | None -> (
                  match lookup n c.arrays with
                  | Some (_, (Hir.Array _ as t), _) -> Ok (Hir.Const_array (n, t, s))
                  | Some (_, (Hir.Vec _ as t), values) ->
                      Ok (Hir.EVector (values, t, s))
                  | Some _ -> unknown_value_error (visible_value_names c) s n
                  | None -> unknown_value_error (visible_value_names c) s n))))
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, is), s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error is m ]) in
      let t = Option.value ~default:(Hir.Int Hir.I32) expected in
      let allowed =
        match t with
        | Hir.Int ((Hir.I8 | I16 | I32 | I64) as k) ->
            let b = int_bits k in
            (b = 64 && v = Int64.min_int) || (b < 64 && v = Int64.shift_left 1L (b - 1))
        | _ -> false
      in
      if fits_negative_literal t v || allowed then
        Ok (Hir.EInt (mask_value t (Int64.neg v), t, s))
      else
        Error
          [
            Sema_types.integer_literal_error s t ~negative:true ~text:("-" ^ raw)
              ~magnitude:v;
          ]
  | Ast.Unary (op, e, s) -> (
      let* te =
        check_expr ~allow_widen:false c
          (match (op, expected) with
          | (Ast.Neg | Ast.Bit_not), _ -> if is_if_operand e then None else expected
          | Ast.Not, Some (Hir.Vec (_, Hir.Bool)) -> expected
          | Ast.Not, _ -> None)
          e
      in
      match op with
      | Ast.Neg | Ast.Bit_not ->
          let floating =
            match Hir.expr_ty te with Hir.Vec (_, Hir.Float _) -> true | _ -> false
          in
          if
            (not (is_numeric (Hir.expr_ty te)))
            && not (op = Ast.Neg && (is_float (Hir.expr_ty te) || floating))
          then
            let operator = if op = Ast.Neg then "-" else "~" in
            let help =
              match (op, e, Hir.expr_ty te) with
              | Ast.Bit_not, Ast.Ident (name, _), Hir.Vec (_, Hir.Bool) ->
                  Some (Printf.sprintf "write `!%s`" name)
              | _ -> None
            in
            let expectation =
              if op = Ast.Bit_not then "an integer or integer vector"
              else "a number or a numeric vector"
            in
            Error
              [
                Diag.error ?help (Ast.expr_span e)
                  (Printf.sprintf "unary operator `%s` needs %s, got `%s`" operator
                     expectation
                     (Sema_types.diagnostic_ty_name (Hir.expr_ty te)));
              ]
          else Ok (Hir.Unary (op, te, Hir.expr_ty te, s))
      | Ast.Not ->
          let result_ty = Hir.expr_ty te in
          if
            result_ty = Hir.Bool
            || match result_ty with Hir.Vec (_, Hir.Bool) -> true | _ -> false
          then Ok (Hir.Unary (op, te, result_ty, s))
          else Error [ Sema_types.logical_not_error e result_ty ])
  | Ast.Binary (op, l, r, s) -> (
      if op = Ast.And || op = Ast.Or then (
        let* a = check_expr c None l in
        let after_left = Sema_flow.snapshot c.flow in
        let left_truth = condition_truth c a in
        let literal_dead =
          match (op, a) with
          | Ast.And, Hir.EBool (false, _) | Ast.Or, Hir.EBool (true, _) -> true
          | Ast.And, Hir.EInt (0L, _, _) -> true
          | Ast.Or, Hir.EInt (value, _, _) when value <> 0L -> true
          | _ -> false
        in
        let fact_dead =
          (op = Ast.And && left_truth = Some false)
          || (op = Ast.Or && left_truth = Some true)
        in
        (match op with
        | Ast.And -> refine_condition c a true
        | Ast.Or -> refine_condition c a false
        | _ -> ());
        let* b = with_dead_check c fact_dead (fun () -> check_expr c None r) in
        let after_right = Sema_flow.snapshot c.flow in
        let initialized =
          if literal_dead then after_left else merge_maps c after_left after_right
        in
        let value_paths =
          match left_truth with
          | Some false when op = Ast.And -> [ after_left ]
          | Some true when op = Ast.Or -> [ after_left ]
          | Some true when op = Ast.And -> [ after_right ]
          | Some false when op = Ast.Or -> [ after_right ]
          | _ -> [ after_left; after_right ]
        in
        Sema_flow.restore c.flow
          (Sema_flow.merge_values_into c.flow initialized value_paths);
        let logical_help =
          match (Hir.expr_ty a, Hir.expr_ty b) with
          | Hir.Int _, Hir.Int _ ->
              Some
                (Printf.sprintf
                   "Fas has no implicit truth values; write `%s != 0 %s %s != 0`"
                   (Ast.expr_name l)
                   (if op = Ast.And then "&&" else "||")
                   (Ast.expr_name r))
          | _ -> None
        in
        let logical_operand_error side expression ty =
          let operation = if op = Ast.And then "&&" else "||" in
          match logical_help with
          | Some help ->
              Sema_types.logical_operand_error ~help operation side expression ty
          | None -> Sema_types.logical_operand_error operation side expression ty
        in
        if Hir.expr_ty a = Hir.Bool && Hir.expr_ty b = Hir.Bool then
          Ok (Hir.Binary (op, a, b, Hir.Bool, s))
        else if Hir.expr_ty a <> Hir.Bool then
          Error [ logical_operand_error "left" l (Hir.expr_ty a) ]
        else Error [ logical_operand_error "right" r (Hir.expr_ty b) ])
      else if op = Ast.Shl || op = Ast.Shr then
        let* a =
          check_expr ~allow_widen:false c
            (if is_if_operand l then None
             else match expected with Some (Hir.Int _) -> expected | _ -> None)
            l
        in
        let at = Hir.expr_ty a in
        let* () =
          match at with
          | Hir.Int _ | Hir.Vec (_, Hir.Int _) -> Ok ()
          | _ -> Error [ Sema_types.shift_value_error op (Ast.expr_span l) at ]
        in
        match (at, r) with
        | Hir.Vec _, Ast.Splat _ -> Error [ Sema_types.shift_count_splat_error op l r ]
        | _ ->
            let* b = check_expr c None r in
            let* () =
              match (at, Hir.expr_ty b) with
              | Hir.Vec (lanes, _), Hir.Vec (count_lanes, (Hir.Int _ as count_element))
                ->
                  if lanes = count_lanes then Ok ()
                  else
                    Error
                      [
                        Sema_types.shift_count_lanes_error op (Ast.expr_span r)
                          (Hir.expr_ty b)
                          (Hir.Vec (lanes, count_element));
                      ]
              | Hir.Vec _, Hir.Int _ -> Ok ()
              | Hir.Int _, Hir.Int _ -> Ok ()
              | _, (Hir.Vec _ as count_ty) | _, (Hir.Bool as count_ty) ->
                  Error [ Sema_types.shift_count_error op (Ast.expr_span r) count_ty ]
              | _ ->
                  Error
                    [
                      Sema_types.shift_count_error op (Ast.expr_span r) (Hir.expr_ty b);
                    ]
            in
            Ok (Hir.Binary (op, a, b, at, s))
      else
        let comparison_chain =
          is_comparison_operator op
          &&
          match l with
          | Ast.Binary (inner, _, _, _) -> is_comparison_operator inner
          | _ -> false
        in
        let parenthesized_comparison =
          match l with
          | Ast.Parenthesized (Ast.Binary (inner, _, _, _), _) ->
              is_comparison_operator inner
          | _ -> false
        in
        let address_peer_type =
          match (expected, l, r) with
          | None, Ast.Addr_of _, Ast.Addr_of _ | Some _, _, _ -> None
          | None, Ast.Addr_of _, _ ->
              Option.map (fun ty -> `Left ty) (peer_handle_type r)
          | None, _, _ -> None
        in
        let* a, b =
          match (unresolved_shape_of c l, unresolved_shape_of c r) with
          | Some _, None ->
              let* b =
                check_expr ~allow_widen:false c (operand_type_hint c op expected l r) r
              in
              let* a =
                check_expr ~allow_widen:false c
                  (contextual_peer_type c op (Hir.expr_ty b) l)
                  l
              in
              Ok (a, b)
          | _ ->
              let left_expected =
                match address_peer_type with
                | Some (`Left ty) -> Some ty
                | _ -> operand_type_hint c op expected l r
              in
              let* a = check_expr ~allow_widen:false c left_expected l in
              let right_expected =
                if comparison_chain || parenthesized_comparison then None
                else contextual_peer_type c op (Hir.expr_ty a) r
              in
              let* b = check_expr ~allow_widen:false c right_expected r in
              Ok (a, b)
        in
        let at = Hir.expr_ty a in
        let bt = Hir.expr_ty b in
        let comparison_chain_rewrite_valid =
          match (op, l) with
          | ( (Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge),
              Ast.Binary
                (((Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge) as inner), first, middle, _) ) ->
              let snapshot = Sema_flow.snapshot c.flow in
              let check_pair operation left right =
                match (unresolved_shape_of c left, unresolved_shape_of c right) with
                | Some _, None ->
                    let* right_value = check_expr c None right in
                    let* left_value =
                      check_expr c
                        (contextual_peer_type c operation (Hir.expr_ty right_value) left)
                        left
                    in
                    Ok (left_value, right_value)
                | _ ->
                    let* left_value = check_expr c None left in
                    let* right_value =
                      check_expr c
                        (contextual_peer_type c operation (Hir.expr_ty left_value) right)
                        right
                    in
                    Ok (left_value, right_value)
              in
              let rewrite_result =
                let* first_value, middle_value = check_pair inner first middle in
                let* _ =
                  binary_result_type ~left_expression:first ~right_expression:middle
                    ~left_checked:first_value ~right_checked:middle_value s inner
                    (Hir.expr_ty first_value) (Hir.expr_ty middle_value)
                in
                let* last_value =
                  check_expr c
                    (contextual_peer_type c op (Hir.expr_ty middle_value) r)
                    r
                in
                let* _ =
                  binary_result_type ~left_expression:middle ~right_expression:r
                    ~left_checked:middle_value ~right_checked:last_value s op
                    (Hir.expr_ty middle_value) (Hir.expr_ty last_value)
                in
                Ok ()
              in
              Sema_flow.restore c.flow snapshot;
              Result.is_ok rewrite_result
          | _ -> true
        in
        match (op, at, bt) with
        | (Ast.Add | Ast.Sub), Hir.Addr, Hir.Int _ ->
            let* off = normalize_offset_expr c.structs s b in
            Ok (Hir.Binary (op, a, off, Hir.Addr, s))
        | Ast.Add, Hir.Int _, Hir.Addr ->
            let* off = normalize_offset_expr c.structs s a in
            Ok (Hir.Binary (Ast.Add, b, off, Hir.Addr, s))
        | (Ast.Add | Ast.Sub), Hir.Addr, Hir.Addr ->
            error (Ast.expr_span r)
              (Printf.sprintf
                 "address arithmetic for `%s` has operands `addr` and `addr`; expected \
                  a scalar integer offset"
                 (Sema_types.binary_operator_name op))
        | (Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge), Hir.Addr, Hir.Addr ->
            Ok (Hir.Binary (op, a, b, Hir.Bool, s))
        | _ ->
            let* result_ty =
              binary_result_type ~left_expression:l ~right_expression:r ~left_checked:a
                ~right_checked:b ?result_expected:expected
                ~comparison_chain_rewrite_valid s op at bt
            in
            let a, b, at, bt =
              match
                if (is_int at && is_int bt) || (is_float at && is_float bt) then
                  Sema_types.common_numeric_type at bt
                else None
              with
              | Some common ->
                  let widen value =
                    match
                      Sema_types.implicit_numeric_widen (Hir.expr_ty value) common
                    with
                    | Some kind -> Hir.Cast (kind, value, common, Hir.expr_span value)
                    | None -> value
                  in
                  let a = widen a and b = widen b in
                  (a, b, Hir.expr_ty a, Hir.expr_ty b)
              | None -> (a, b, at, bt)
            in
            let facts_enabled = Sema_flow.proof_checks_enabled c.flow in
            let divisor_fact = value_fact c b in
            let division_by_zero =
              facts_enabled && is_int bt
              && (op = Ast.Div || op = Ast.Rem)
              &&
              match divisor_fact with
              | Some fact -> fact_single bt fact && fact.low = 0L
              | None -> false
            in
            let minimum_division_overflow =
              facts_enabled && op = Ast.Div && is_int at
              && (not (is_unsigned at))
              &&
              match (value_fact c a, divisor_fact) with
              | Some numerator, Some denominator ->
                  fact_single at numerator && fact_single bt denominator
                  && numerator.low = fst (fact_bounds at)
                  && denominator.low = -1L
              | _ -> false
            in
            if division_by_zero then
              error s "division by zero is not a defined runtime operation"
            else if minimum_division_overflow then
              error s
                (Sema_types.signed_division_overflow_message at
                   (Option.get (value_fact c a)).low (Option.get divisor_fact).low)
            else Ok (Hir.Binary (op, a, b, result_ty, s)))
  | Ast.Call (fn, args, s) -> check_call c expected fn args s
  | Ast.Try (call, s) ->
      if Sema_flow.in_defer c.flow then
        error s "`try` is not allowed inside defer or errdefer"
      else if not c.fn_fails then
        Error
          [
            Diag.error
              ~help:"mark the function `fails`, or handle the error here with `catch`"
              s "`try` passes the error up, so it needs a `fails` function";
          ]
      else
        let* inner = check_handled_call c expected "try" call in
        Ok (Hir.Try (inner, Hir.expr_ty inner, s))
  | Ast.Catch (call, Ast.Catch_value value, s) ->
      let* inner = check_handled_call c expected "catch" call in
      let ty = Hir.expr_ty inner in
      if ty = Hir.Void then
        error (Ast.expr_span value)
          "the call has no result, so `catch` has no value to use; write `catch { ... }`"
      else
        let* checked = check_expr c (Some ty) value in
        let* () =
          ensure_expected ~checked_expression:checked ~context:"catch value"
            ~expression:value (Hir.expr_ty checked) ty (Ast.expr_span value)
        in
        Ok (Hir.Catch_value (inner, checked, ty, s))
  | Ast.Catch (_, Ast.Catch_block _, s) ->
      Error
        [
          Diag.error ~help:"use `catch VALUE` inside an expression" s
            "a `catch` block must be a whole statement, a declaration's value or an \
             assignment's value";
        ]
  | Ast.Generic_args (Ast.Ident ("call_addr", _), _, s) ->
      error s "call_addr is a builtin and has no address"
  | Ast.Generic_args (Ast.Ident ("enum_count", _), generic_args, s)
    when Option.is_none (lookup_local "enum_count" c) ->
      let* enum, _ =
        Sema_constants.enum_type_argument (source_ty_in_context c) "enum_count"
          generic_args s
      in
      let count = Int64.of_int (List.length (Option.get (Hir.enum_def enum)).Hir.members) in
      let ty = match expected with Some (Hir.Int _ as t) -> t | _ -> Hir.Int Hir.U64 in
      if Sema_numeric.fits_literal ty count then Ok (Hir.EInt (count, ty, s))
      else
        error s
          (Printf.sprintf "enum_count[%s] does not fit `%s`" enum
             (Sema_types.diagnostic_ty_name ty))
  | Ast.Handle_from_addr (t, e, s) -> (
      let* opaque_name =
        handle_target c.named_types t
        |> Result.map_error (fun message -> [ Diag.error s message ])
      in
      match e with
      | Ast.Null null_span ->
          error null_span
            (Printf.sprintf
               "write `null` directly where a `handle[%s]` is expected; \
                `handle_from_addr[%s](null)` is not needed"
               opaque_name opaque_name)
      | _ -> (
          let* a = check_expr c None e in
          match Hir.expr_ty a with
          | Hir.Addr ->
              Ok
                (Hir.Call
                   ( Hir.Builtin (Hir.Handle_from_addr opaque_name),
                     [ a ],
                     Hir.Handle opaque_name,
                     s ))
          | _ -> error s "handle_from_addr argument must be an addr"))
  | Ast.Generic_args (_fn, _, s) ->
      error s "generic specialization is not available in this context"
  | Ast.Cast (k, t, e, s) ->
      let* t = source_ty_in_context c s t in
      let* x = check_expr c None e in
      let from = Hir.expr_ty x in
      if cast_legal k from t then Ok (Hir.Cast (k, x, t, s))
      else Error [ Sema_types.cast_error ~expression:e k from t s ]
  | Ast.Select (a, args, s) ->
      let* place = check_place c (Ast.Select (a, args, s)) in
      let* () =
        match place.expr with
        | Hir.Raw_select (_, _, (Hir.Struct _ as t), _) when Hir.is_value_struct t ->
            Ok ()
        | Hir.Raw_select (_, _, (Hir.Struct _ | Hir.Array _), _) ->
            error s "raw access cannot load an array or struct value"
        | _ -> Ok ()
      in
      let* () =
        match (place.root, place.path) with
        | Some binding, Some path -> require_place_state binding path c s
        | _ -> Ok ()
      in
      let* () =
        if
          (match place.expr with Hir.Raw_select _ -> true | _ -> false)
          || expression_uses_view c place.expr
        then check_place_access c s ~write:false place.expr
        else Ok ()
      in
      Ok place.expr
  | Ast.Field (a, n, s) ->
      let* place = check_place c (Ast.Field (a, n, s)) in
      let* () =
        match (place.root, place.path) with
        | Some binding, Some path -> require_place_state binding path c s
        | _ -> Ok ()
      in
      let* () =
        if
          (match place.expr with Hir.Raw_select _ -> true | _ -> false)
          || expression_uses_view c place.expr
        then check_place_access c s ~write:false place.expr
        else Ok ()
      in
      Ok place.expr
  | Ast.Arrow_field (base, field, operator_span, field_span) ->
      let* place =
        check_place c (Ast.Arrow_field (base, field, operator_span, field_span))
      in
      Ok place.expr
  | Ast.Addr_of (Ast.Ident (name, _), s)
    when Option.is_none (lookup_local name c)
         && Option.value ~default:false
              (Option.map
                 (fun binding -> binding.declaration_kind = Top_function)
                 (lookup_top_level name c.top_level_bindings))
         && not (List.mem name c.external_c_functions) ->
      if
        List.exists
          (function
            | Ast.Func { name = generic_name; generic_params = _ :: _; _ } ->
                generic_name = name
            | _ -> false)
          c.generic_structs
      then error s (Printf.sprintf "generic function `%s` has no single address" name)
      else Ok (Hir.Function_address (name, s))
  | Ast.Addr_of (e, s) -> (
      let* place = check_place c e in
      (match (place.root, place.path) with
      | Some binding, Some (Exact path) ->
          set_state c binding path Unknown;
          Sema_flow.forget_value c.flow binding;
          Sema_flow.forget_address c.flow binding;
          Sema_flow.forget_mask c.flow binding
      | Some binding, Some (Dynamic_prefix path) ->
          set_state c binding path Unknown;
          Sema_flow.forget_value c.flow binding;
          Sema_flow.forget_address c.flow binding;
          Sema_flow.forget_mask c.flow binding
      | _ -> ());
      match place.expr with
      | Hir.Function_address _ -> Ok place.expr
      | Hir.Index (base, _, _, _)
        when match Hir.expr_ty base with Hir.Vec _ -> true | _ -> false ->
          error s "cannot take address of a vector lane"
      | Hir.Local _ | Hir.Global _ | Hir.Index _ | Hir.Field _ | Hir.Const_array _
      | Hir.Raw_select _ ->
          Ok
            (Hir.Address (place.expr, address_type_for_expected c expected place.expr, s))
      | _ -> error s "cannot take the address of this expression")
  | Ast.Sizeof_value (value, span) -> (
      match value with
      | Ast.Ident (name, _)
        when List.mem name Names.scalar_type_names
             || List.mem name Names.reserved_float_type_names
             ||
             match List.assoc_opt name c.named_types with
             | Some
                 ( Struct_name | Opaque_name | C_record_name _ | Alias_name _
                 | Unsupported_name _ ) ->
                 true
             | _ -> false ->
          error
            ~help:(Printf.sprintf "write `sizeof[%s]`" name)
            span "`sizeof` needs a type in brackets"
      | _ ->
          let inferred_type =
            match check_place c value with
            | Ok place -> Some (Hir.expr_ty place.expr)
            | Error _ -> None
          in
          let help =
            Option.map
              (fun ty ->
                Printf.sprintf "write `sizeof[%s]`" (Sema_types.diagnostic_ty_name ty))
              inferred_type
          in
          error ?help span "`sizeof` needs a type in brackets")
  | Ast.Sizeof (source_type, s) ->
      let* t, structs = query_layout_in_context c s source_type in
      let type_span =
        Option.value ~default:s
          (Sema_types.opaque_source_span c.named_types [] source_type)
      in
      let* size, _ = layout_diag type_span structs t in
      Ok (Hir.Sizeof (t, size, s))
  | Ast.Alignof (source_type, s) ->
      let* t, structs = query_layout_in_context c s source_type in
      let type_span =
        Option.value ~default:s
          (Sema_types.opaque_source_span c.named_types [] source_type)
      in
      let* _, a = layout_diag type_span structs t in
      Ok (Hir.Alignof (t, a, s))
  | Ast.Offsetof (source_ty, n, field_span, s) -> (
      let* t, structs = query_layout_in_context c s source_ty in
      match t with
      | Hir.Struct sn -> (
          match field_info structs sn n with
          | Some { unsupported_reason = Some reason; _ } -> error s reason
          | Some f ->
              index_field c (Sema_constants.layout_source sn) n field_span;
              Ok (Hir.Offsetof (t, n, f.offset, s))
          | None ->
              missing_field_error ~record_name:(Ast.type_name source_ty) c s n
                (Hir.Struct sn))
      | _ -> error s "offsetof requires a struct type")
  | Ast.Splat (e, s) -> (
      match expected with
      | Some (Hir.Vec (n, elem)) ->
          let* x = check_expr c (Some elem) e in
          if equal (Hir.expr_ty x) elem then Ok (Hir.Splat (x, Hir.Vec (n, elem), s))
          else error s "splat element type mismatch"
      | _ ->
          error s
            "`splat` needs a vector destination or vector operand to determine its \
             lane count")
  | Ast.Ternary (q, a, b, s) ->
      let* tq = check_expr c None q in
      if Hir.expr_ty tq <> Hir.Bool then
        Error [ Sema_types.condition_error "if" q (Hir.expr_ty tq) ]
      else
        let before_arms = Sema_flow.snapshot c.flow in
        let condition = condition_truth c tq
        and init_condition = literal_expression_truth tq in
        let* expected =
          if
            expected = None
            && unresolved_shape_of c a = Some Unresolved_vector
            && unresolved_shape_of c b = None
          then (
            let* peer = check_expr c None b in
            Sema_flow.restore c.flow before_arms;
            Ok (match Hir.expr_ty peer with Hir.Vec _ as ty -> Some ty | _ -> None))
          else Ok expected
        in
        refine_condition c tq true;
        let left_shape = unresolved_shape_of c a
        and right_shape = unresolved_shape_of c b in
        let* a_expected =
          if
            Option.is_none expected
            && List.mem left_shape [ Some Unresolved_int; Some Unresolved_float ]
            && Option.is_none right_shape
          then (
            Sema_flow.restore c.flow before_arms;
            refine_condition c tq false;
            let* peer =
              with_dead_check c (condition = Some true) (fun () -> check_expr c None b)
            in
            Sema_flow.restore c.flow before_arms;
            refine_condition c tq true;
            Ok
              (match (left_shape, Hir.expr_ty peer) with
              | Some Unresolved_int, (Hir.Int _ as ty)
              | Some Unresolved_float, (Hir.Float _ as ty) ->
                  Some ty
              | _ -> None))
          else Ok expected
        in
        let* ta =
          with_dead_check c (condition = Some false) (fun () ->
              check_expr c a_expected a)
        in
        let after_a = Sema_flow.snapshot c.flow in
        Sema_flow.restore c.flow before_arms;
        refine_condition c tq false;
        let b_expected =
          match (expected, right_shape, Hir.expr_ty ta) with
          | Some _, _, _ -> expected
          | None, Some Unresolved_int, (Hir.Int _ as ty)
          | None, Some Unresolved_float, (Hir.Float _ as ty)
          | None, Some Unresolved_vector, (Hir.Vec _ as ty) ->
              Some ty
          | _ -> None
        in
        let* tb =
          with_dead_check c (condition = Some true) (fun () ->
              check_expr c b_expected b)
        in
        let after_b = Sema_flow.snapshot c.flow in
        let initialized =
          match init_condition with
          | Some true -> after_a
          | Some false -> after_b
          | None -> merge_maps c after_a after_b
        in
        let value_paths =
          match condition with
          | Some true -> [ after_a ]
          | Some false -> [ after_b ]
          | None -> [ after_a; after_b ]
        in
        Sema_flow.restore c.flow
          (Sema_flow.merge_values_into c.flow initialized value_paths);
        let at = Hir.expr_ty ta and bt = Hir.expr_ty tb in
        let widening_help =
          not
            (List.exists
               (fun frame ->
                 List.exists
                   (function Diagnostic_type_argument _ -> true | _ -> false)
                   frame.arguments)
               c.spec_trace)
        in
        let* result_type, left_widen, right_widen =
          if Option.is_none expected then
            unify_if_branches ~left_checked:ta ~right_checked:tb a at b bt
            |> Result.map_error (fun diagnostic -> [ diagnostic ])
          else
            let* () =
              match expected with
              | Some target when scalar_conversion_pair at target ->
                  let* () =
                    ensure_expected ~checked_expression:ta ~widening_help
                      ~context:"if-expression branch" ~expression:a at target
                      (Ast.expr_span a)
                  in
                  ensure_expected ~checked_expression:tb ~widening_help
                    ~context:"if-expression branch" ~expression:b bt target
                    (Ast.expr_span b)
              | _ -> Ok ()
            in
            let result_ty =
              if equal at bt then Some at
              else if compatible at bt then Some bt
              else if compatible bt at then Some at
              else None
            in
            match result_ty with
            | None ->
                error (Ast.expr_span b)
                  (Printf.sprintf
                     "if-expression branches have different types: `%s` and `%s`"
                     (Sema_types.diagnostic_ty_name at)
                     (Sema_types.diagnostic_ty_name bt))
            | Some _ when at = Hir.Void || bt = Hir.Void ->
                error s "if-expression branches cannot have void type"
            | Some ty -> Ok (ty, None, None)
        in
        let widen expression value = function
          | Some kind -> Hir.Cast (kind, value, result_type, Ast.expr_span expression)
          | None -> value
        in
        Ok
          (Hir.Ternary
             (tq, widen a ta left_widen, widen b tb right_widen, result_type, s))
  | Ast.Array_lit (_, s) as literal
    when match expected with Some t -> Hir.is_value_struct t | None -> false -> (
      (* a brace list as a value struct value (`return {a, b}`, an argument):
         built in a fresh local *)
      let t = Option.get expected in
      let* initialized = check_initializer c t literal in
      match initialized with
      | `Aggregate (ty, entries, construction_span, zero_padding) ->
          incr construct_counter;
          let* local =
            add_local ("value#" ^ string_of_int !construct_counter) ty c s
          in
          mark_init local c;
          index_type c s ty None;
          Ok
            (Hir.Construct
               ( local,
                 aggregate_construction ~zero_padding ty entries construction_span,
                 s ))
      | `Value value -> Ok value)
  | Ast.Array_lit (entries, s) -> (
      match expected with
      | Some (Hir.Vec (lanes, element) as literal_type) -> (
          match
            List.find_opt
              (function
                | Ast.Designated_field _ | Ast.Designated_index _ -> true | _ -> false)
              entries
          with
          | Some entry ->
              Error [ Sema_types.invalid_designator_error literal_type entry ]
          | None -> vector_literal literal_type lanes element entries s)
      | _ ->
          error
            ?help:
              (match expected with
              | Some Hir.Addr -> Some "store it in a local and pass `&local`"
              | _ -> None)
            s "array, struct, or vector initializer needs a destination type")
  | (Ast.Designated_field _ | Ast.Designated_index _) as expression ->
      Error
        [
          Sema_types.invalid_designator_error
            (Option.value expected ~default:Hir.Void)
            expression;
        ]

and check_same_operands ?expected c left right =
  let contextual expression = Option.is_some (unresolved_shape_of c expression) in
  let hint peer expression =
    match (unresolved_shape_of c expression, Hir.expr_ty peer) with
    | Some Unresolved_int, (Hir.Int _ as ty) -> Some ty
    | Some Unresolved_vector, (Hir.Vec _ as ty) -> Some ty
    | _ -> None
  in
  if
    contextual left && contextual right && Option.is_some expected
    && builtin_literal_operand left && builtin_literal_operand right
  then
    let* left = check_expr c expected left in
    let* right = check_expr c expected right in
    Ok (left, right)
  else if contextual left && not (contextual right) then
    let* right = check_expr c None right in
    let* left = check_expr c (hint right left) left in
    Ok (left, right)
  else
    let* left = check_expr c None left in
    let* right = check_expr c (hint left right) right in
    Ok (left, right)

and check_initializer ?(constant = false) ?destination c expected expression =
  let vector_value () =
    let* value = check_expr ?destination c (Some expected) expression in
    if not constant then Ok (`Value value)
    else
      let consts =
        List.filter (fun (name, _, _) -> Option.is_none (lookup_local name c)) c.consts
      in
      let arrays =
        List.filter (fun (name, _, _) -> Option.is_none (lookup_local name c)) c.arrays
      in
      match
        vector_const_expr ~structs:c.structs ~named_types:c.named_types
          ~generic_structs:c.generic_structs ~arrays consts (Some expected) expression
      with
      | Ok (actual, lanes)
        when Hir.ty_equal actual expected && Hir.ty_equal actual (Hir.expr_ty value) ->
          Ok (`Value (Hir.EVector (lanes, expected, Ast.expr_span expression)))
      | _ -> Ok (`Value value)
  in
  let scalar_initializer_error ty entries span =
    let help =
      let scalar = match ty with Hir.Bool | Hir.Int _ -> true | _ -> false in
      match entries with
      | [ entry ]
        when scalar
             &&
             match entry with
             | Ast.Int_lit _ | Ast.Bool_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _) ->
                 true
             | _ -> false -> (
          let before = Sema_flow.snapshot c.flow in
          let result = check_expr c (Some ty) entry in
          Sema_flow.restore c.flow before;
          match result with
          | Ok value when Hir.ty_equal (Hir.expr_ty value) ty ->
              Some (Printf.sprintf "write `%s`" (Ast.expr_name entry))
          | _ -> None)
      | _ -> None
    in
    error ?help span
      (Printf.sprintf "initializer needs an array, struct, or vector type, got `%s`"
         (Sema_types.diagnostic_ty_name ty))
  in
  let aggregate_entries ty entries span =
    match (ty, entries) with
    | (Hir.Array _ | Hir.Struct _), [] -> Ok (`Aggregate (ty, [], span, false))
    | _ ->
        let* typed_entries, zero_padding =
          match ty with
          | Hir.Array (length, element) ->
              if
                List.exists
                  (function
                    | Ast.Designated_field _ | Ast.Designated_index _ -> true
                    | _ -> false)
                  entries
              then
                let entry =
                  List.find
                    (function
                      | Ast.Designated_field _ | Ast.Designated_index _ -> true
                      | _ -> false)
                    entries
                in
                Error [ Sema_types.invalid_designator_error ty entry ]
              else if List.length entries <> length then
                error
                  (Sema_types.aggregate_count_error_span span length entries)
                  (Sema_types.array_element_count_message length (List.length entries))
              else
                Ok
                  ( List.mapi
                      (fun index entry ->
                        ( element,
                          Some entry,
                          Printf.sprintf "element %d of array%s" (index + 1)
                            (match destination with
                            | Some name -> Printf.sprintf " `%s`" name
                            | None -> "") ))
                      entries,
                    false )
          | Hir.Struct name -> (
              match
                List.find_opt
                  (fun (definition : Hir.struct_def) -> definition.name = name)
                  c.structs
              with
              | None -> error span (Printf.sprintf "unknown struct `%s`" name)
              | Some definition -> (
                  let* designated =
                    Sema_types.designated_record_entries name definition.is_union
                      definition.fields entries span
                  in
                  if Option.is_some designated then
                    index_designated_fields c name entries;
                  match designated with
                  | Some fields -> (
                      match
                        List.find_opt
                          (fun (field : Hir.field) ->
                            Option.is_some field.unsupported_reason)
                          definition.fields
                      with
                      | Some { unsupported_reason = Some reason; _ } ->
                          error span reason
                      | _ ->
                          Ok
                            ( List.map
                                (fun ((field : Hir.field), value) ->
                                  ( field.ty,
                                    value,
                                    Printf.sprintf "field `%s` of record `%s`"
                                      field.name name ))
                                fields,
                              true ))
                  | None when definition.is_union -> (
                      match (definition.fields, entries) with
                      | { unsupported_reason = Some reason; _ } :: _, _ ->
                          error span reason
                      | field :: _, [ entry ] ->
                          Ok
                            ( [
                                ( field.ty,
                                  Some entry,
                                  Printf.sprintf "field `%s` of record `%s`" field.name
                                    name );
                              ],
                              false )
                      | _ ->
                          error
                            (Sema_types.aggregate_count_error_span span 1 entries)
                            (Sema_types.record_field_count_message name 1
                               (List.length entries)))
                  | None when List.length entries <> List.length definition.fields ->
                      error
                        (Sema_types.aggregate_count_error_span span
                           (List.length definition.fields)
                           entries)
                        (Sema_types.record_field_count_message name
                           (List.length definition.fields)
                           (List.length entries))
                  | None
                    when List.exists
                           (fun (field : Hir.field) ->
                             Option.is_some field.unsupported_reason)
                           definition.fields ->
                      error span
                        (List.find_map
                           (fun (field : Hir.field) -> field.unsupported_reason)
                           definition.fields
                        |> Option.get)
                  | None ->
                      Ok
                        ( List.map2
                            (fun (field : Hir.field) entry ->
                              ( field.ty,
                                Some entry,
                                Printf.sprintf "field `%s` of record `%s`" field.name
                                  name ))
                            definition.fields entries,
                          false )))
          | _ -> (
              match
                List.find_opt
                  (function
                    | Ast.Designated_field _ | Ast.Designated_index _ -> true
                    | _ -> false)
                  entries
              with
              | Some entry -> Error [ Sema_types.invalid_designator_error ty entry ]
              | None -> scalar_initializer_error ty entries span)
        in
        let rec check acc = function
          | [] -> Ok (List.rev acc)
          | (entry_ty, entry, context) :: rest -> (
              match entry with
              | None ->
                  check
                    (Hir.Init_omitted (Hir.zero_initializer entry_ty, span) :: acc)
                    rest
              | Some entry ->
                  let* initialized =
                    check_initializer ~constant:true ?destination c entry_ty entry
                  in
                  let* () =
                    let actual =
                      match initialized with
                      | `Value value -> Hir.expr_ty value
                      | `Aggregate (ty, _, _, _) -> ty
                    in
                    if
                      (match initialized with
                        | `Value value -> not (c_struct_result value)
                        | `Aggregate _ -> false)
                      && aggregate_value_type entry_ty && aggregate_value_type actual
                    then
                      error (Ast.expr_span entry)
                        "aggregate value initialization is not supported; use \
                         `copy(dst, src)`"
                    else
                      let checked_expression =
                        match initialized with
                        | `Value value -> Some value
                        | `Aggregate _ -> None
                      in
                      ensure_expected ?checked_expression ~context ~expression:entry
                        actual entry_ty (Ast.expr_span entry)
                  in
                  let child =
                    match initialized with
                    | `Value value -> Hir.Init_value value
                    | `Aggregate (ty, children, child_span, zero_padding) ->
                        aggregate_construction ~zero_padding ty children child_span
                  in
                  check (child :: acc) rest)
        in
        let* entries = check [] typed_entries in
        Ok (`Aggregate (ty, entries, span, zero_padding))
  in
  match expression with
  | Ast.Array_lit (entries, span) -> (
      match expected with
      | Hir.Vec _ -> (
          match
            List.find_opt
              (function
                | Ast.Designated_field _ | Ast.Designated_index _ -> true | _ -> false)
              entries
          with
          | Some entry -> Error [ Sema_types.invalid_designator_error expected entry ]
          | None -> vector_value ())
      | _ -> aggregate_entries expected entries span)
  | _ when match expected with Hir.Vec _ -> true | _ -> false -> vector_value ()
  | _ ->
      let* value = check_expr c (Some expected) expression in
      if not constant then Ok (`Value value)
      else
        let consts =
          List.filter
            (fun (name, _, _) -> Option.is_none (lookup_local name c))
            c.consts
        in
        let arrays =
          List.filter
            (fun (name, _, _) -> Option.is_none (lookup_local name c))
            c.arrays
        in
        let value =
          match expected with
          | Hir.Bool | Hir.Int _ | Hir.Float _ -> (
              match
                const_expr ~structs:c.structs ~named_types:c.named_types
                  ~generic_structs:c.generic_structs ~arrays consts (Some expected)
                  ~validate_dead:false expression
              with
              | Ok (actual, bits)
                when Hir.ty_equal actual expected
                     && Hir.ty_equal actual (Hir.expr_ty value) ->
                  if expected = Hir.Bool then
                    Hir.EBool (bits <> 0L, Ast.expr_span expression)
                  else if match expected with Hir.Float _ -> true | _ -> false then
                    Hir.EFloat (bits, expected, Ast.expr_span expression)
                  else Hir.EInt (bits, expected, Ast.expr_span expression)
              | _ -> value)
          | _ -> value
        in
        Ok (`Value value)

and generic_const_argument span = function
  | Ast.Const_arg expression -> Ok expression
  | Ast.Name_arg (name, span) -> Ok (Ast.Ident (name, span))
  | Ast.Type_or_index ty -> Ok (Ast.index_expression ty)
  | Ast.Type_arg (Ast.Applied_type (name, _, [ argument ], _)) ->
      let* index = generic_const_argument span argument in
      Ok (Ast.Select (Ast.Ident (name, span), [ Ast.Const_arg index ], span))
  | Ast.Type_arg _ -> error span "expected a const argument"

(* atomic_<op>[T, ordering](...) and atomic_fence[ordering](). T is an
   integer or addr (bitwise and arithmetic ops: integers only); the access is
   naturally aligned, so the pointer must be aligned to sizeof(T) at run
   time. Atomics are synchronisation points: like an opaque call they drop
   every remembered address, value and mask fact. *)
and atomic_ordering_name = function
  | Ast.Name_arg (n, span) -> Some (n, span)
  | Ast.Type_arg (Ast.Named_type (n, span)) | Ast.Type_or_index (Ast.Named_type (n, span))
    ->
      Some (n, span)
  | _ -> None

and check_atomic c name generic_args span args s =
  let op =
    match name with
    | "atomic_load" -> Hir.Atomic_load
    | "atomic_store" -> Hir.Atomic_store
    | "atomic_add" -> Hir.Atomic_add
    | "atomic_sub" -> Hir.Atomic_sub
    | "atomic_and" -> Hir.Atomic_and
    | "atomic_or" -> Hir.Atomic_or
    | "atomic_xor" -> Hir.Atomic_xor
    | "atomic_xchg" -> Hir.Atomic_xchg
    | "atomic_cas" -> Hir.Atomic_cas
    | _ -> Hir.Atomic_fence
  in
  let ordering argument =
    match atomic_ordering_name argument with
    | Some ("relaxed", _) -> Ok Hir.Relaxed
    | Some ("acquire", _) -> Ok Hir.Acquire
    | Some ("release", _) -> Ok Hir.Release
    | Some ("acq_rel", _) -> Ok Hir.Acq_rel
    | Some ("seq_cst", _) -> Ok Hir.Seq_cst
    | Some (other, other_span) ->
        error other_span
          (Printf.sprintf
             "unknown memory ordering `%s`; expected relaxed, acquire, release, acq_rel               or seq_cst"
             other)
    | None ->
        error span
          "expected a memory ordering: relaxed, acquire, release, acq_rel or seq_cst"
  in
  let* access_ty, order =
    match (op, generic_args) with
    | Hir.Atomic_fence, [ o ] ->
        let* order = ordering o in
        Ok (Hir.Void, order)
    | Hir.Atomic_fence, _ ->
        error span "`atomic_fence` expects one ordering: `atomic_fence[seq_cst]()`"
    | _, [ t; o ] ->
        let* access_ty =
          match t with
          | Ast.Type_arg ty | Ast.Type_or_index ty -> source_ty_in_context c span ty
          | Ast.Name_arg (n, name_span) ->
              source_ty_in_context c name_span (Ast.Named_type (n, name_span))
          | Ast.Const_arg _ -> error span (Printf.sprintf "`%s` expects a type first" name)
        in
        let* order = ordering o in
        Ok (access_ty, order)
    | _ ->
        error span
          (Printf.sprintf "`%s` expects a type and an ordering: `%s[T, seq_cst]`" name name)
  in
  let* () =
    match (op, access_ty) with
    | Hir.Atomic_fence, _ -> Ok ()
    | _, Hir.Int _ -> Ok ()
    | (Hir.Atomic_load | Hir.Atomic_store | Hir.Atomic_xchg | Hir.Atomic_cas), Hir.Addr ->
        Ok ()
    | _ ->
        error span
          (Printf.sprintf "`%s` needs an integer%s type, got `%s`" name
             (match op with
             | Hir.Atomic_load | Hir.Atomic_store | Hir.Atomic_xchg | Hir.Atomic_cas ->
                 " or addr"
             | _ -> "")
             (Sema_types.diagnostic_ty_name access_ty))
  in
  let* () =
    match (op, order) with
    | Hir.Atomic_load, (Hir.Release | Hir.Acq_rel) ->
        error span "an atomic load cannot have release or acq_rel ordering"
    | Hir.Atomic_store, (Hir.Acquire | Hir.Acq_rel) ->
        error span "an atomic store cannot have acquire or acq_rel ordering"
    | Hir.Atomic_fence, Hir.Relaxed -> error span "a fence cannot be relaxed"
    | _ -> Ok ()
  in
  let arity =
    match op with
    | Hir.Atomic_fence -> 0
    | Hir.Atomic_load -> 1
    | Hir.Atomic_cas -> 3
    | _ -> 2
  in
  if List.length args <> arity then
    error s (Printf.sprintf "builtin `%s` expects %d argument%s" name arity
               (if arity = 1 then "" else "s"))
  else
    let* checked =
      Result_list.map
        (fun (i, arg) ->
          let expected = if i = 0 then Hir.Addr else access_ty in
          let* x = check_expr c (Some expected) arg in
          let* () =
            ensure_expected
              ~context:(Printf.sprintf "argument %d of `%s`" (i + 1) name)
              ~checked_expression:x ~expression:arg (Hir.expr_ty x) expected
              (Ast.expr_span arg)
          in
          Ok x)
        (List.mapi (fun i a -> (i, a)) args)
    in
    let* () =
      match (op, checked) with
      | Hir.Atomic_fence, _ | _, [] -> Ok ()
      | _, pointer :: _ ->
          let* size =
            match access_footprint c access_ty with
            | Some size -> Ok size
            | None -> error s "atomic access type has no storage layout"
          in
          check_address_access c s ~write:(op <> Hir.Atomic_load) (address_fact c pointer)
            size
    in
    Sema_flow.forget_all_addresses c.flow;
    Sema_flow.forget_all_values c.flow;
    Sema_flow.forget_all_masks c.flow;
    let result_ty =
      match op with Hir.Atomic_store | Hir.Atomic_fence -> Hir.Void | _ -> access_ty
    in
    Ok (Hir.Call (Hir.Builtin (Hir.Atomic (op, order, access_ty)), checked, result_ty, s))

and volatile_access_type c name span arguments =
  let* access_ty =
    match arguments with
    | [ Ast.Type_arg ty ] -> source_ty_in_context c span ty
    | [ Ast.Name_arg (name, name_span) ] ->
        source_ty_in_context c name_span (Ast.Named_type (name, name_span))
    | _ -> error span (Printf.sprintf "builtin `%s` expects one type argument" name)
  in
  match access_ty with
  | Hir.Bool | Hir.Int _ | Hir.Float _ | Hir.Addr | Hir.Handle _
  | Hir.Vec (_, (Hir.Int _ | Hir.Bool)) ->
      Ok access_ty
  | _ ->
      error span
        "volatile access type must be a scalar integer, bool, addr, handle[T], vec[N, \
         integer], or vec[N, bool]"

and simd_memory_access_type c name span arguments =
  let* access_ty =
    match arguments with
    | [ Ast.Type_arg ty ] -> source_ty_in_context c span ty
    | [ Ast.Name_arg (name, name_span) ] ->
        source_ty_in_context c name_span (Ast.Named_type (name, name_span))
    | _ -> error span (Printf.sprintf "builtin `%s` expects one type argument" name)
  in
  match access_ty with
  | Hir.Bool | Hir.Int _ | Hir.Float _ -> Ok access_ty
  | _ ->
      error span
        (Printf.sprintf "`%s` needs an integer or bool element type, got `%s`" name
           (Sema_types.diagnostic_ty_name access_ty))

and check_simd_memory_args c name access_ty args span =
  let indexed =
    name = "gather" || name = "scatter" || name = "gather_bytes"
    || name = "scatter_bytes"
  in
  let arity = if indexed then 4 else 3 in
  if List.length args <> arity then
    error span (Printf.sprintf "builtin `%s` expects %d arguments" name arity)
  else
    let base_arg = List.nth args 0 in
    let* base = check_expr c (Some Hir.Addr) base_arg in
    let* () =
      ensure_expected
        ~context:(Printf.sprintf "argument 1 of `%s`" name)
        ~expression:base_arg (Hir.expr_ty base) Hir.Addr (Ast.expr_span base_arg)
    in
    if not indexed then
      let mask_arg = List.nth args 1 in
      let* mask = check_expr c None mask_arg in
      match Hir.expr_ty mask with
      | Hir.Vec (lanes, Hir.Bool) ->
          let value_arg = List.nth args 2 in
          let value_ty = Hir.Vec (lanes, access_ty) in
          let* value = check_expr c (Some value_ty) value_arg in
          let* () =
            ensure_expected
              ~context:(Printf.sprintf "argument 3 of `%s`" name)
              ~checked_expression:value ~expression:value_arg (Hir.expr_ty value)
              value_ty (Ast.expr_span value_arg)
          in
          Ok ([ base; mask; value ], lanes)
      | _ -> error (Ast.expr_span mask_arg) (name ^ " mask must be a bool vector")
    else
      let index_arg = List.nth args 1 in
      let* indices = check_expr c None index_arg in
      match Hir.expr_ty indices with
      | Hir.Vec (lanes, Hir.Int _) ->
          let mask_arg = List.nth args 2 in
          let mask_ty = Hir.Vec (lanes, Hir.Bool) in
          let* mask = check_expr c (Some mask_ty) mask_arg in
          let* () =
            ensure_expected
              ~context:(Printf.sprintf "argument 3 of `%s`" name)
              ~expression:mask_arg (Hir.expr_ty mask) mask_ty (Ast.expr_span mask_arg)
          in
          let value_arg = List.nth args 3 in
          let value_ty = Hir.Vec (lanes, access_ty) in
          let* value = check_expr c (Some value_ty) value_arg in
          let* () =
            ensure_expected
              ~context:(Printf.sprintf "argument 4 of `%s`" name)
              ~checked_expression:value ~expression:value_arg (Hir.expr_ty value)
              value_ty (Ast.expr_span value_arg)
          in
          Ok ([ base; indices; mask; value ], lanes)
      | _ ->
          error (Ast.expr_span index_arg) (name ^ " indices must be an integer vector")

and check_handle_from_addr c name opaque_name args s =
  if List.length args <> 1 then
    error s (Printf.sprintf "builtin `%s` expects one argument" name)
  else
    match List.hd args with
    | Ast.Null null_span ->
        error null_span
          (Printf.sprintf
             "write `null` directly where a `handle[%s]` is expected; \
              `handle_from_addr[%s](null)` is not needed"
             opaque_name opaque_name)
    | arg -> (
        let* a = check_expr c (Some Hir.Addr) arg in
        match Hir.expr_ty a with
        | Hir.Addr ->
            Ok
              (Hir.Call
                 ( Hir.Builtin (Hir.Handle_from_addr opaque_name),
                   [ a ],
                   Hir.Handle opaque_name,
                   s ))
        | _ -> error s "handle_from_addr argument must be an addr")

and check_float_math c expected operation name args s =
  let arity = if operation = Names.Fma then 3 else 1 in
  if List.length args <> arity then
    error s
      (Printf.sprintf "builtin `%s` expects %d argument%s" name arity
         (if arity = 1 then "" else "s"))
  else
    let peer_types =
      List.filter_map
        (fun argument ->
          if unresolved_float_operand c argument then None
          else
            let before = Sema_flow.snapshot c.flow in
            let result = check_expr c None argument in
            Sema_flow.restore c.flow before;
            match result with
            | Ok value -> (
                match Hir.expr_ty value with
                | (Hir.Float _ | Hir.Vec (_, Hir.Float _)) as ty -> Some ty
                | _ -> None)
            | Error _ -> None)
        args
    in
    let operand_ty =
      match peer_types with
      | ty :: _ -> ty
      | [] -> (
          match expected with
          | Some ((Hir.Float _ | Hir.Vec (_, Hir.Float _)) as ty) -> ty
          | _ -> Hir.Float Hir.F64)
    in
    let* checked =
      Result_list.map
        (fun argument ->
          check_expr c
            (if unresolved_float_operand c argument then Some operand_ty else None)
            argument)
        args
    in
    let indexed = List.mapi (fun index value -> (index, value)) checked in
    let float_type = function
      | Hir.Float _ | Hir.Vec (_, Hir.Float _) -> true
      | _ -> false
    in
    let rec first_bad = function
      | [] -> None
      | (index, value) :: rest ->
          if Hir.expr_ty value = operand_ty && float_type (Hir.expr_ty value) then
            first_bad rest
          else Some (index, value)
    in
    match first_bad indexed with
    | Some (index, value) ->
        let actual = Hir.expr_ty value in
        let help =
          match actual with
          | Hir.Int _ ->
              if name = "abs" && Option.is_some (List.assoc_opt name c.c_unsupported)
              then
                match expected with
                | Some (Hir.Float _ as ty) ->
                    Some
                      (Printf.sprintf "write `cvt[%s](labs(%s))`"
                         (Sema_types.diagnostic_ty_name ty)
                         (Ast.expr_name (List.nth args index)))
                | _ -> None
              else if
                name = "abs"
                && Option.fold ~none:false
                     ~some:(function Hir.Int _ -> true | _ -> false)
                     expected
              then None
              else
                let converted =
                  List.mapi
                    (fun argument_index argument ->
                      if
                        match Hir.expr_ty argument with Hir.Int _ -> true | _ -> false
                      then
                        let source = List.nth args argument_index in
                        let rec literal_float = function
                          | Ast.Parenthesized (value, _) -> literal_float value
                          | Ast.Int_lit (raw, _) ->
                              if
                                String.length raw >= 2
                                && raw.[0] = '0'
                                && List.mem
                                     (Char.lowercase_ascii raw.[1])
                                     [ 'x'; 'b'; 'o' ]
                              then None
                              else Some (raw ^ ".0")
                          | Ast.Unary (Ast.Neg, value, _) ->
                              Option.map (( ^ ) "-") (literal_float value)
                          | _ -> None
                        in
                        Option.value
                          ~default:
                            (Printf.sprintf "cvt[%s](%s)"
                               (Sema_types.diagnostic_ty_name operand_ty)
                               (Ast.expr_name source))
                          (literal_float source)
                      else Ast.expr_name (List.nth args argument_index))
                    checked
                in
                Some
                  (Printf.sprintf "write `%s(%s)`" name (String.concat ", " converted))
          | _ -> None
        in
        Error
          [
            Diag.error ?help
              (Ast.expr_span (List.nth args index))
              (Printf.sprintf "argument %d of `%s` is `%s`, expected `%s`" (index + 1)
                 name
                 (Sema_types.diagnostic_ty_name actual)
                 (Sema_types.diagnostic_ty_name operand_ty));
          ]
    | None ->
        let builtin =
          match operation with
          | Names.Sqrt -> Hir.Float_sqrt
          | Names.Fma -> Hir.Float_fma
          | Names.Floor -> Hir.Float_floor
          | Names.Ceil -> Hir.Float_ceil
          | Names.Round -> Hir.Float_round
          | Names.Abs -> Hir.Float_abs
        in
        Ok (Hir.Call (Hir.Builtin (Hir.Float_math builtin), checked, operand_ty, s))

and check_call c expected fn args s =
  (* a call to a `fails` function is the operand of `try` or `catch`, which
     names it in `handled_call`; see check_handled_call *)
  let handled = c.handled_call = Some s in
  if handled then c.handled_call <- None;
  let fails = callee_fails c fn in
  let* () =
    if fails && not handled then
      Error
        [
          Diag.error ~help:"handle it with `try` or `catch`" s
            (Printf.sprintf "`%s` can fail and the error is not handled"
               (Ast.expr_name fn));
        ]
    else if handled && not fails then
      error s
        (Printf.sprintf
           "`%s` does not fail; `try` and `catch` need a call to a `fails` function"
           (Ast.expr_name fn))
    else Ok ()
  in
  let result = check_call_inner c expected fn args s in
  index_call c fn result;
  result

and callee_fails c fn =
  let template_fails name =
    match List.assoc_opt name c.templates with
    | Some (Ast.Func { fails; _ }) -> fails
    | _ -> false
  in
  match fn with
  | Ast.Ident (name, _) when Option.is_none (lookup_local name c) -> (
      match List.assoc_opt name c.signatures with
      | Some (signature : Sema_context.signature) -> signature.fails
      | None -> template_fails name)
  | Ast.Generic_args (Ast.Ident ("call_addr", _), arguments, _) ->
      List.exists (function Ast.Name_arg ("fails", _) -> true | _ -> false) arguments
  | Ast.Generic_args (Ast.Ident (name, _), _, _) when Option.is_none (lookup_local name c)
    ->
      template_fails name
  | Ast.Parenthesized (fn, _) -> callee_fails c fn
  | _ -> false

(* the operand of `try` or `catch`: a call to a `fails` function, checked
   with its value type *)
and check_handled_call c expected keyword call =
  let rec strip = function Ast.Parenthesized (e, _) -> strip e | e -> e in
  match strip call with
  | Ast.Call (_, _, call_span) as call ->
      let previous = c.handled_call in
      c.handled_call <- Some call_span;
      let result = check_expr c expected call in
      c.handled_call <- previous;
      result
  | other ->
      error (Ast.expr_span other)
        (Printf.sprintf "`%s` needs a call to a `fails` function" keyword)

and check_call_inner c expected fn args s =
  match fn with
  | Ast.Ident ("errno_err", _) when Option.is_none (lookup_local "errno_err" c) ->
      if args <> [] then error s "builtin `errno_err` takes no arguments"
      else Ok (Hir.Call (Hir.Builtin Hir.Errno_err, [], Hir.Err, s))
  | Ast.Ident ("err_name", _) when Option.is_none (lookup_local "err_name" c) -> (
      match args with
      | [ value ] ->
          let* checked = check_expr c (Some Hir.Err) value in
          if Hir.expr_ty checked <> Hir.Err then
            error (Ast.expr_span value)
              (Printf.sprintf "`err_name` needs an `error`, got `%s`"
                 (Sema_types.diagnostic_ty_name (Hir.expr_ty checked)))
          else Ok (Hir.Call (Hir.Builtin Hir.Err_name, [ checked ], Hir.Addr, s))
      | _ -> error s "builtin `err_name` expects one argument")
  | Ast.Ident ("enum_value", _) when Option.is_none (lookup_local "enum_value" c) -> (
      match args with
      | [ value ] -> (
          let* checked = check_expr c None value in
          match Hir.expr_ty checked with
          | Hir.Enum (_, kind) -> (
              match checked with
              | Hir.EInt (v, _, _) -> Ok (Hir.EInt (v, Hir.Int kind, s))
              | _ ->
                  Ok (Hir.Call (Hir.Builtin Hir.Enum_value, [ checked ], Hir.Int kind, s)))
          | other ->
              error (Ast.expr_span value)
                (Printf.sprintf "`enum_value` needs an enum, got `%s`"
                   (Sema_types.diagnostic_ty_name other)))
      | _ -> error s "builtin `enum_value` expects one argument")
  | Ast.Ident ("enum_name", _) when Option.is_none (lookup_local "enum_name" c) -> (
      match args with
      | [ value ] -> (
          let* checked = check_expr c None value in
          match Hir.expr_ty checked with
          | Hir.Enum (enum, _) ->
              Ok (Hir.Call (Hir.Builtin (Hir.Enum_name enum), [ checked ], Hir.Addr, s))
          | other ->
              error (Ast.expr_span value)
                (Printf.sprintf "`enum_name` needs an enum, got `%s`"
                   (Sema_types.diagnostic_ty_name other)))
      | _ -> error s "builtin `enum_name` expects one argument")
  | Ast.Generic_args
      (Ast.Ident ((("enum_from" | "enum_is") as builtin), _), generic_args, application_span)
    when Option.is_none (lookup_local builtin c) -> (
      let* enum, kind =
        Sema_constants.enum_type_argument (source_ty_in_context c) builtin generic_args
          application_span
      in
      match args with
      | [ value ] -> (
          let* checked = check_expr c (Some (Hir.Int kind)) value in
          let* () =
            Sema_constants.enum_operand_type builtin kind (Hir.expr_ty checked)
              (Ast.expr_span value)
          in
          match checked with
          | Hir.EInt (v, _, _) -> (
              (* a constant operand is checked now *)
              let member = Sema_constants.enum_member_of enum v in
              if builtin = "enum_is" then Ok (Hir.EBool (member <> None, s))
              else
                match member with
                | Some _ -> Ok (Hir.EInt (v, Hir.Enum (enum, kind), s))
                | None ->
                    error s
                      (Printf.sprintf "%s is not a value of enum `%s`"
                         (Int64.to_string v) enum))
          | _ ->
              if builtin = "enum_is" then
                Ok (Hir.Call (Hir.Builtin (Hir.Enum_is enum), [ checked ], Hir.Bool, s))
              else
                Ok
                  (Hir.Call
                     (Hir.Builtin (Hir.Enum_from enum), [ checked ], Hir.Enum (enum, kind), s)))
      | _ -> error s (Printf.sprintf "builtin `%s` expects one argument" builtin))
  | Ast.Generic_args (Ast.Ident ("call_addr", _), generic_args, application_span) -> (
      let generic_args =
        List.filter
          (function Ast.Name_arg ("fails", _) -> false | _ -> true)
          generic_args
      in
      let* result_ty =
        match generic_args with
        | [] when callee_fails c fn -> Ok Hir.Void
        | [ (Ast.Type_arg source | Ast.Type_or_index source) ] ->
            source_ty_in_context c application_span source
        | [ Ast.Name_arg (name, name_span) ] ->
            source_ty_in_context c name_span (Ast.Named_type (name, name_span))
        | _ -> error application_span "call_addr needs a result type: call_addr[R](...)"
      in
      if aggregate_value_type result_ty then
        error application_span
          (Printf.sprintf "aggregate result `%s` cannot be returned by value"
             (Sema_types.diagnostic_ty_name result_ty))
      else
        match args with
        | [] -> error s "call_addr expects a callee address"
        | callee :: actuals ->
            let* checked_callee = check_expr c (Some Hir.Addr) callee in
            let target_fact = address_fact c checked_callee in
            if Hir.expr_ty checked_callee <> Hir.Addr then
              error (Ast.expr_span callee)
                (Printf.sprintf "call_addr callee must be `addr`, got `%s`"
                   (Sema_types.diagnostic_ty_name (Hir.expr_ty checked_callee)))
            else
              let* checked_args = check_indirect_actuals c actuals in
              let* () = check_indirect_target c target_fact checked_args result_ty s in
              Sema_flow.forget_all_addresses c.flow;
              Sema_flow.forget_all_values c.flow;
              Sema_flow.forget_all_masks c.flow;
              Ok (Hir.Indirect_call (checked_callee, checked_args, result_ty, s)))
  | Ast.Ident (name, span)
    when Option.is_none (lookup_local name c)
         && (not (Names.builtin_name name))
         && Option.is_some (List.assoc_opt name c.c_unsupported) ->
      Option.get (c_unsupported c span name)
  | Ast.Generic_args (Ast.Ident (name, span), _, _)
    when Option.is_none (lookup_local name c)
         && (not (Names.builtin_name name))
         && Option.is_some (List.assoc_opt name c.c_unsupported) ->
      Option.get (c_unsupported c span name)
  | Ast.Generic_args (Ast.Ident ("cvt", _), generic_args, application_span) ->
      let* target =
        match generic_args with
        | [ Ast.Type_arg source ] -> source_ty_in_context c application_span source
        | [ Ast.Name_arg (name, name_span) ] ->
            source_ty_in_context c name_span (Ast.Named_type (name, name_span))
        | _ -> error application_span "cvt needs one target type: cvt[T](value)"
      in
      if List.length args <> 1 then error s "builtin `cvt` expects one argument"
      else
        let value = List.hd args in
        let* checked = check_expr c None value in
        let source = Hir.expr_ty checked in
        let integer = function Hir.Int _ -> true | _ -> false in
        let floating = function Hir.Float _ -> true | _ -> false in
        if
          (integer source && floating target)
          || (floating source && integer target)
          || (floating source && floating target && source <> target)
          ||
          match (source, target) with
          | ( Hir.Vec (source_lanes, source_element),
              Hir.Vec (target_lanes, target_element) )
            when source_lanes = target_lanes ->
              (integer source_element && floating target_element)
              || (floating source_element && integer target_element)
              || floating source_element && floating target_element
                 && source_element <> target_element
          | _ -> false
        then Ok (Hir.Call (Hir.Builtin (Hir.Cvt target), [ checked ], target, s))
        else if Hir.ty_equal source target then
          let name = Sema_types.diagnostic_ty_name target in
          let article =
            match name.[0] with 'a' | 'e' | 'f' | 'i' | 'o' -> "an" | _ -> "a"
          in
          Error
            [
              Diag.error
                ~help:(Printf.sprintf "write `%s`" (Ast.expr_name value))
                s
                (Printf.sprintf "`cvt[%s]` of %s `%s` value converts nothing" name
                   article name);
            ]
        else if integer source && integer target then
          let help =
            "integer width changes use `zext`, `sext`, or `trunc`; same-width values \
             already have their representation"
          in
          Error
            [
              Diag.error ~help s
                (Printf.sprintf "`cvt` does not convert `%s` to `%s`"
                   (Sema_types.diagnostic_ty_name source)
                   (Sema_types.diagnostic_ty_name target));
            ]
        else
          error s
            (Printf.sprintf
               "`cvt` needs an integer or floating-point source and target, got `%s` \
                to `%s`"
               (Sema_types.diagnostic_ty_name source)
               (Sema_types.diagnostic_ty_name target))
  | Ast.Ident (name, _) when Option.is_some (Names.float_math name) ->
      check_float_math c expected (Option.get (Names.float_math name)) name args s
  | Ast.Ident (name, name_span)
    when Option.is_some (Names.missing_type_arguments_message name) ->
      error name_span (Option.get (Names.missing_type_arguments_message name))
  | Ast.Generic_args (Ast.Ident (name, _), generic_args, application_span)
    when name = "handle_from_addr" -> (
      match generic_args with
      | [ Ast.Type_arg type_arg ] ->
          let* opaque_name =
            handle_target c.named_types type_arg
            |> Result.map_error (fun message -> [ Diag.error application_span message ])
          in
          check_handle_from_addr c name opaque_name args s
      | [ Ast.Name_arg (type_name, name_span) ] ->
          let* opaque_name =
            handle_target c.named_types (Ast.Named_type (type_name, name_span))
            |> Result.map_error (fun message -> [ Diag.error name_span message ])
          in
          check_handle_from_addr c name opaque_name args s
      | _ -> error s (Printf.sprintf "builtin `%s` expects a type argument" name))
  | Ast.Generic_args (Ast.Ident ("copy", _), _, _) ->
      error s "copy is statement-only and takes no type arguments"
  | Ast.Generic_args (Ast.Ident (name, _), _, _)
    when name = "masked_store" || name = "scatter" || name = "scatter_bytes" ->
      error s (name ^ " is statement-only")
  | Ast.Generic_args (Ast.Ident (name, name_span), _, _)
    when Option.is_some (Names.takes_no_type_arguments_message name) ->
      error name_span (Option.get (Names.takes_no_type_arguments_message name))
  | Ast.Generic_args (Ast.Ident (name, _), generic_args, application_span)
    when name = "masked_load" || name = "gather" || name = "gather_bytes" ->
      let* access_ty = simd_memory_access_type c name application_span generic_args in
      let* checked, lanes = check_simd_memory_args c name access_ty args s in
      let* () = check_simd_access c s ~write:false name access_ty checked in
      let kind =
        match name with
        | "masked_load" -> Hir.Masked
        | "gather" -> Hir.Gather
        | _ -> Hir.Gather_bytes
      in
      Ok
        (Hir.Call
           ( Hir.Builtin (Hir.Simd_load (access_ty, kind)),
             checked,
             Hir.Vec (lanes, access_ty),
             s ))
  | Ast.Generic_args (Ast.Ident (name, _), generic_args, application_span)
    when List.mem name Names.atomic_names ->
      check_atomic c name generic_args application_span args s
  | Ast.Generic_args (Ast.Ident ("volatile_load", _), generic_args, application_span) ->
      let* access_ty =
        volatile_access_type c "volatile_load" application_span generic_args
      in
      if List.length args <> 1 then
        error s "builtin `volatile_load` expects one argument"
      else
        let pointer_arg = List.hd args in
        let* pointer = check_expr c (Some Hir.Addr) pointer_arg in
        let* () =
          ensure_expected ~context:"argument 1 of `volatile_load`"
            ~expression:pointer_arg (Hir.expr_ty pointer) Hir.Addr
            (Ast.expr_span pointer_arg)
        in
        let* size =
          match access_footprint c access_ty with
          | Some size -> Ok size
          | None -> error s "volatile access type has no storage layout"
        in
        let* () = check_address_access c s ~write:false (address_fact c pointer) size in
        Ok
          (Hir.Call
             (Hir.Builtin (Hir.Volatile_load access_ty), [ pointer ], access_ty, s))
  | Ast.Generic_args (Ast.Ident ("volatile_store", _), _, _) ->
      error s "`volatile_store` is statement-only"
  | Ast.Generic_args (Ast.Ident (name, _), generic_args, application_span) -> (
      match lookup_local name c with
      | Some _ -> error s (Printf.sprintf "`%s` is a value, not a function" name)
      | None -> (
          match List.assoc_opt name c.templates with
          | None ->
              if
                Option.is_some (lookup name c.consts)
                || Option.is_some (lookup name c.arrays)
              then error s (Printf.sprintf "`%s` is a constant, not a function" name)
              else if Option.is_some (List.assoc_opt name c.named_types) then
                error s (Printf.sprintf "`%s` is a type, not a function" name)
              else error s (Printf.sprintf "unknown generic function `%s`" name)
          | Some (Ast.Func { generic_params; _ } as item) ->
              let const_params = const_params generic_params in
              if has_type_params generic_params then
                error s "type-generic call reached ordinary type checking"
              else if List.length generic_args <> List.length const_params then
                error s
                  (generic_arity_message "generic function" name
                     (List.length const_params) (List.length generic_args))
              else
                let* cargs = Result_list.map (generic_const_argument s) generic_args in
                let rec eval acc cps actual =
                  match (cps, actual) with
                  | [], [] -> Ok (List.rev acc)
                  | cp :: cs, a :: rest ->
                      let* ct =
                        source_ty_diag c.named_types (Ast.expr_span a) cp.Ast.ty
                      in
                      let* vt, v =
                        const_expr
                          ~array_lengths:
                            (static_array_lengths c.top_level_bindings c.globals)
                          ~structs:c.structs ~named_types:c.named_types
                          ~generic_structs:c.generic_structs
                          ~globals:(List.map (fun (name, _, _) -> name) c.globals)
                          ~arrays:c.arrays c.consts (Some ct) a
                      in
                      if equal vt ct then eval ((cp.name, ct, v) :: acc) cs rest
                      else error (Ast.expr_span a) "const argument type mismatch"
                  | _ -> error s "const argument arity mismatch"
                in
                let* values = eval [] const_params cargs in
                let* staged =
                  staged_specialization_identity c.specializations name values
                in
                let* mangled, key =
                  match staged with
                  | None ->
                      let* declaration_id =
                        specialization_declaration_id c.top_level_bindings Top_function
                          s name
                      in
                      Ok
                        ( mangle_specialization name values,
                          function_specialization_key declaration_id values )
                  | Some (origin_id, origin_name, arguments, _, _) ->
                      Ok
                        ( mangle_mixed_specialization origin_name arguments,
                          (Function_specialization, origin_id, arguments) )
                in
                let frame_name, frame_arguments, frame_span =
                  match staged with
                  | None ->
                      ( name,
                        List.map
                          (fun (_, ty, value) -> Diagnostic_const_argument (ty, value))
                          values,
                        application_span )
                  | Some (_, origin_name, _, arguments, span) ->
                      (origin_name, arguments, span)
                in
                let frame =
                  {
                    template_name = frame_name;
                    arguments = frame_arguments;
                    application_span = frame_span;
                  }
                in
                let spec =
                  {
                    key;
                    name = mangled;
                    depth = c.spec_depth;
                    payload =
                      Function_payload
                        { item; substitutions = []; values; staged_args = None };
                    trace = c.spec_trace @ [ frame ];
                    pending_frame = None;
                  }
                in
                let* specialization =
                  Sema_specialization.request c.specializations ~limits:c.limits
                    ~depth:c.spec_depth ~span:s ~description:"const specialization" spec
                in
                let params, ret =
                  match specialization.payload with
                  | Function_payload
                      { item = Ast.Func { params; ret; _ }; substitutions = []; _ } ->
                      (params, ret)
                  | _ -> assert false
                in
                let* ps =
                  Result_list.map
                    (fun (p : Ast.param) ->
                      let* t = source_ty_with_values c.named_types values p.span p.ty in
                      Ok (p.name, t))
                    params
                in
                let* rt = source_ty_with_values c.named_types values s ret in
                let* checked = check_actuals ~callee:name c Reject s ps args in
                Ok (Hir.Call (Hir.User specialization.name, checked, rt, s))
          | Some _ -> error s "const-generic symbol is not a function"))
  | Ast.Ident ("copy", _) -> error s "copy is statement-only"
  | Ast.Ident (name, _) when Names.value_operation name = Some Names.Len ->
      if List.length args <> 1 then error s "builtin `len` expects one argument"
      else
        let argument = List.hd args in
        let* n =
          match argument with
          | Ast.String_lit (cstr, value, _) ->
              if cstr && String.contains value '\000' then
                error s "C string literal cannot contain embedded NUL"
              else Ok (String.length value)
          | _ -> (
              (* `len` only needs the type, so a place (`digits`, `s.buf`)
                 may still be uninitialized: look it up as a place, which
                 does not read it, before falling back to a value *)
              let place_length =
                match argument with
                | Ast.Ident _ | Ast.Field _ | Ast.Arrow_field _ | Ast.Select _ -> (
                    let before = Sema_flow.snapshot c.flow in
                    let place = check_place c argument in
                    Sema_flow.restore c.flow before;
                    match place with
                    | Ok place -> (
                        match Hir.expr_ty place.expr with
                        | Hir.Array (n, _) -> Some n
                        | _ -> None)
                    | Error _ -> None)
                | _ -> None
              in
              match place_length with
              | Some n -> Ok n
              | None -> (
                  let* value = check_expr c None argument in
                  match Hir.expr_ty value with
                  | Hir.Array (n, _) -> Ok n
                  | _ -> error s "len requires a fixed array or string literal"))
        in
        Ok (Hir.EInt (Int64.of_int n, Hir.Int Hir.U64, s))
  | Ast.Ident (name, _) -> (
      let builtin =
        match Names.value_operation name with
        | Some Names.Rotl -> Some Hir.Rotl
        | Some Names.Rotr -> Some Rotr
        | Some Names.Popcount -> Some Popcount
        | Some Names.Ctz -> Some Ctz
        | Some Names.Clz -> Some Clz
        | Some Names.Add_sat -> Some Hir.Add_sat
        | Some Names.Sub_sat -> Some Hir.Sub_sat
        | Some Names.Mul_hi -> Some Hir.Mul_hi
        | Some Names.Any -> Some Hir.Any
        | Some Names.All -> Some Hir.All
        | Some Names.Select -> Some Hir.Select
        | Some Names.Shuffle -> Some Hir.Shuffle
        | Some Names.Permute -> Some Hir.Permute
        | Some Names.Reduce_sum -> Some Hir.Reduce_sum
        | Some Names.Reduce_min -> Some Hir.Reduce_min
        | Some Names.Reduce_max -> Some Hir.Reduce_max
        | Some Names.Reduce_and -> Some Hir.Reduce_and
        | Some Names.Reduce_or -> Some Hir.Reduce_or
        | Some Names.Reduce_xor -> Some Hir.Reduce_xor
        | Some Names.Compress -> Some Hir.Compress
        | Some Names.Expand -> Some Hir.Expand
        | Some Names.Addr_bits -> Some Hir.Addr_bits
        | Some Names.Syscall -> Some Hir.Syscall
        | Some Names.Addr_from_bits -> Some Hir.Addr_from_bits
        | Some Names.Handle_addr -> Some Hir.Handle_addr
        | Some Names.Handle_from_addr -> Some (Hir.Handle_from_addr "")
        | Some Names.Len | None -> None
      in
      let check_builtin b =
        let result_context =
          match expected with
          | Some (Hir.Int _ | Hir.Vec (_, Hir.Int _)) -> expected
          | _ -> None
        in
        let literal_context expression =
          if builtin_literal_operand expression then result_context else None
        in
        match b with
        | Hir.Popcount | Hir.Ctz | Hir.Clz ->
            if List.length args <> 1 then
              error s (Printf.sprintf "builtin `%s` expects one argument" name)
            else
              let* a = check_expr c (literal_context (List.hd args)) (List.hd args) in
              let valid_operand =
                is_int (Hir.expr_ty a)
                ||
                match Hir.expr_ty a with
                | Hir.Vec (_, Hir.Int _) -> true
                | _ -> false
              in
              if valid_operand then
                Ok (Hir.Call (Hir.Builtin b, [ a ], Hir.expr_ty a, s))
              else error s "builtin argument must be an integer or an integer vector"
        | Hir.Add_sat | Hir.Sub_sat | Hir.Mul_hi ->
            if List.length args <> 2 then
              error s (Printf.sprintf "builtin `%s` expects two arguments" name)
            else
              let* a, b2 =
                check_same_operands ?expected:result_context c (List.hd args)
                  (List.hd (List.tl args))
              in
              let valid_operand =
                is_int (Hir.expr_ty a)
                ||
                match Hir.expr_ty a with
                | Hir.Vec (_, Hir.Int _) -> true
                | _ -> false
              in
              if not valid_operand then
                error
                  (Ast.expr_span (List.nth args 0))
                  (Printf.sprintf "`%s` needs an integer or integer vector, got `%s`"
                     name
                     (Sema_types.diagnostic_ty_name (Hir.expr_ty a)))
              else if Hir.expr_ty b2 <> Hir.expr_ty a then
                match
                  Sema_types.integer_literal_vector_argument_error name
                    (List.nth args 1) (Hir.expr_ty a)
                with
                | Some diagnostic -> Error [ diagnostic ]
                | None ->
                    error
                      (Ast.expr_span (List.nth args 1))
                      (Printf.sprintf
                         "argument 2 of `%s` is `%s`, expected the type of argument 1 \
                          (`%s`)"
                         name
                         (Sema_types.diagnostic_ty_name (Hir.expr_ty b2))
                         (Sema_types.diagnostic_ty_name (Hir.expr_ty a)))
              else Ok (Hir.Call (Hir.Builtin b, [ a; b2 ], Hir.expr_ty a, s))
        | Hir.Any | Hir.All -> (
            if List.length args <> 1 then
              error s (Printf.sprintf "builtin `%s` expects one argument" name)
            else
              let* a = check_expr c None (List.hd args) in
              match Hir.expr_ty a with
              | Hir.Vec (_, Hir.Bool) ->
                  Ok (Hir.Call (Hir.Builtin b, [ a ], Hir.Bool, s))
              | _ -> error s "builtin argument must be a bool vector")
        | Hir.Reduce_sum | Hir.Reduce_min | Hir.Reduce_max | Hir.Reduce_and
        | Hir.Reduce_or | Hir.Reduce_xor -> (
            if List.length args <> 1 then
              error s (Printf.sprintf "builtin `%s` expects one argument" name)
            else
              let* a = check_expr c None (List.hd args) in
              match Hir.expr_ty a with
              | Hir.Vec (_, (Hir.Int _ as e)) ->
                  Ok (Hir.Call (Hir.Builtin b, [ a ], e, s))
              | Hir.Vec (_, Hir.Float _) when b = Hir.Reduce_sum ->
                  Error
                    [
                      Diag.error
                        ~help:"the order is not defined; write the additions explicitly"
                        (Ast.expr_span (List.hd args))
                        "`reduce_sum` is not defined for floating-point vectors";
                    ]
              | Hir.Vec (_, Hir.Float _) ->
                  error
                    (Ast.expr_span (List.hd args))
                    (Printf.sprintf "`%s` is not defined for floating-point vectors"
                       name)
              | actual ->
                  error
                    (Ast.expr_span (List.hd args))
                    (Printf.sprintf "`%s` needs an integer vector, got `%s`" name
                       (Sema_types.diagnostic_ty_name actual)))
        | Hir.Compress | Hir.Expand -> (
            if List.length args <> 2 then
              error s (Printf.sprintf "builtin `%s` expects two arguments" name)
            else
              let* v = check_expr c None (List.nth args 0) in
              let* m = check_expr c None (List.nth args 1) in
              match (Hir.expr_ty v, Hir.expr_ty m) with
              | ( Hir.Vec (n, ((Hir.Int _ | Hir.Bool | Hir.Float _) as e)),
                  Hir.Vec (mcount, Hir.Bool) ) ->
                  if n <> mcount then
                    error s
                      (Printf.sprintf "%s values and mask must have the same lane count"
                         name)
                  else Ok (Hir.Call (Hir.Builtin b, [ v; m ], Hir.Vec (n, e), s))
              | Hir.Vec (_, _), _ ->
                  error s (Printf.sprintf "%s mask must be a bool vector" name)
              | _ -> error s (Printf.sprintf "%s values must be a vector" name))
        | Hir.Select -> (
            if List.length args <> 3 then
              error s (Printf.sprintf "builtin `%s` expects three arguments" name)
            else
              let* m = check_expr c None (List.nth args 0) in
              let* y, z =
                check_same_operands ?expected:result_context c (List.nth args 1)
                  (List.nth args 2)
              in
              let mask_lanes =
                match Hir.expr_ty m with Hir.Vec (n, Hir.Bool) -> Some n | _ -> None
              in
              match mask_lanes with
              | None ->
                  error
                    (Ast.expr_span (List.nth args 0))
                    (Printf.sprintf
                       "argument 1 of `select` is `%s`, expected a bool vector"
                       (Sema_types.diagnostic_ty_name (Hir.expr_ty m)))
              | Some n -> (
                  if Hir.expr_ty y <> Hir.expr_ty z then
                    error
                      (Ast.expr_span (List.nth args 2))
                      (Printf.sprintf
                         "argument 3 of `select` is `%s`, expected the type of \
                          argument 2 (`%s`)"
                         (Sema_types.diagnostic_ty_name (Hir.expr_ty z))
                         (Sema_types.diagnostic_ty_name (Hir.expr_ty y)))
                  else
                    match Hir.expr_ty y with
                    | Hir.Vec (n2, (Hir.Int _ | Hir.Bool | Hir.Float _)) when n2 = n ->
                        Ok (Hir.Call (Hir.Builtin b, [ m; y; z ], Hir.expr_ty y, s))
                    | _ ->
                        error s "select values must be vectors with the mask lane count"
                  ))
        | Hir.Shuffle -> (
            if List.length args <> 3 then
              error s (Printf.sprintf "builtin `%s` expects three arguments" name)
            else
              let* a, b2 = check_same_operands c (List.nth args 0) (List.nth args 1) in
              let at = Hir.expr_ty a in
              let ok_vec =
                match at with
                | Hir.Vec (_, (Hir.Int _ | Hir.Bool | Hir.Float _)) -> true
                | _ -> false
              in
              if not ok_vec then
                error
                  (Ast.expr_span (List.nth args 0))
                  (Printf.sprintf
                     "argument 1 of `shuffle` must be an integer or bool vector, got \
                      `%s`"
                     (Sema_types.diagnostic_ty_name at))
              else if Hir.expr_ty b2 <> at then
                error
                  (Ast.expr_span (List.nth args 1))
                  (Printf.sprintf
                     "argument 2 of `shuffle` is `%s`, expected the type of argument 1 \
                      (`%s`)"
                     (Sema_types.diagnostic_ty_name (Hir.expr_ty b2))
                     (Sema_types.diagnostic_ty_name at))
              else
                let visible_consts =
                  List.filter
                    (fun (name, _, _) -> Option.is_none (lookup_local name c))
                    c.consts
                in
                let selector = List.nth args 2 in
                match
                  vector_const_expr
                    ~array_lengths:(static_array_lengths c.top_level_bindings c.globals)
                    ~structs:c.structs ~named_types:c.named_types
                    ~generic_structs:c.generic_structs
                    ~globals:(List.map (fun (name, _, _) -> name) c.globals)
                    ~arrays:c.arrays visible_consts
                    (match selector with
                    | Ast.Array_lit (entries, _) ->
                        Some (Hir.Vec (List.length entries, Hir.Int Hir.I64))
                    | _ -> None)
                    selector
                with
                | Ok ((Hir.Vec (m, (Hir.Int _ as sel_elem)) as sty), values) ->
                    let n = match at with Hir.Vec (n, _) -> n | _ -> 0 in
                    let in_range = shuffle_indices_in_range sel_elem n values in
                    if not in_range then
                      let bad_index =
                        List.find_opt
                          (fun value ->
                            let value =
                              if
                                match sel_elem with
                                | Hir.Int
                                    (Hir.I8 | Hir.I16 | Hir.I32 | Hir.I64)
                                  ->
                                    true
                                | _ -> false
                              then sign_extend_value sel_elem value
                              else value
                            in
                            Int64.compare value 0L < 0
                            || Int64.compare value (Int64.of_int (2 * n)) >= 0)
                          values
                      in
                      error
                        (Ast.expr_span (List.nth args 2))
                        (Printf.sprintf
                           "shuffle index `%s` is out of range for %d lanes"
                           (match bad_index with
                           | Some value -> (
                               match sel_elem with
                               | Hir.Int
                                   (Hir.I8 | Hir.I16 | Hir.I32 | Hir.I64) ->
                                   Int64.to_string (sign_extend_value sel_elem value)
                               | _ -> Printf.sprintf "%Lu" value)
                           | None -> "?")
                           (2 * n))
                    else
                      let elem = match at with Hir.Vec (_, e) -> e | _ -> Hir.Bool in
                      let result_ty = Hir.Vec (m, elem) in
                      let* _ =
                        Sema_limits.validate_object c.limits c.structs s result_ty
                      in
                      Ok
                        (Hir.Call
                           ( Hir.Builtin b,
                             [ a; b2; Hir.EVector (values, sty, s) ],
                             result_ty,
                             s ))
                | Error diagnostics -> (
                    match check_expr c None selector with
                    | Ok value ->
                        error
                          (Ast.expr_span (List.nth args 2))
                          (Printf.sprintf
                             "shuffle indices must be a compile-time constant integer \
                              vector, got `%s`"
                             (Sema_types.diagnostic_ty_name (Hir.expr_ty value)))
                    | Error _ ->
                        let local_value name =
                          Option.is_some (lookup_local name c)
                          && List.exists
                               (fun (diagnostic : Diag.t) ->
                                 diagnostic.message
                                 = Printf.sprintf "unknown name `%s`" name)
                               diagnostics
                        in
                        if List.exists local_value (Sema_flow.local_names c.flow) then
                          error
                            (Ast.expr_span (List.nth args 2))
                            "shuffle indices must be a compile-time constant integer \
                             vector"
                        else Error diagnostics)
                | Ok (actual, _) ->
                    error
                      (Ast.expr_span (List.nth args 2))
                      (Printf.sprintf
                         "shuffle indices must be a compile-time constant integer \
                          vector, got `%s`"
                         (Sema_types.diagnostic_ty_name actual)))
        | Hir.Permute ->
            if List.length args <> 2 then
              error s (Printf.sprintf "builtin `%s` expects two arguments" name)
            else
              let* a = check_expr c None (List.hd args) in
              let* b2 = check_expr c None (List.hd (List.tl args)) in
              let ok_value =
                match Hir.expr_ty a with
                | Hir.Vec (_, (Hir.Int _ | Hir.Bool | Hir.Float _)) -> true
                | _ -> false
              in
              let ok_indices =
                match Hir.expr_ty b2 with
                | Hir.Vec (_, Hir.Int (Hir.U8 | Hir.U16 | Hir.U32 | Hir.U64))
                  ->
                    true
                | _ -> false
              in
              if not ok_value then error s "permute value must be a vector"
              else if not ok_indices then
                error s "permute indices must be an unsigned integer vector"
              else
                let elem =
                  match Hir.expr_ty a with Hir.Vec (_, e) -> e | _ -> Hir.Bool
                in
                let m = match Hir.expr_ty b2 with Hir.Vec (m, _) -> m | _ -> 1 in
                let result_ty = Hir.Vec (m, elem) in
                let* _ = Sema_limits.validate_object c.limits c.structs s result_ty in
                Ok (Hir.Call (Hir.Builtin b, [ a; b2 ], result_ty, s))
        | Hir.Addr_bits -> (
            if List.length args <> 1 then
              error s (Printf.sprintf "builtin `%s` expects one argument" name)
            else
              let* a = check_expr c None (List.hd args) in
              match Hir.expr_ty a with
              | Hir.Addr -> Ok (Hir.Call (Hir.Builtin b, [ a ], Hir.Int Hir.U64, s))
              | _ -> error s "addr_bits argument must be an addr")
        | Hir.Syscall ->
            let count = List.length args in
            if count < 1 || count > 7 then
              error s
                (Printf.sprintf "builtin `%s` expects one to seven arguments" name)
            else if (Target_layout.current ()).Target_layout.os <> Target_layout.Linux
            then error s "builtin `syscall` is only available on Linux x86-64"
            else
              let* checked =
                Result_list.map
                  (fun a ->
                    let* a = check_expr c None a in
                    match Hir.expr_ty a with
                    | Hir.Int _ | Hir.Addr -> Ok a
                    | _ -> error s "syscall arguments must be integers or addrs")
                  args
              in
              Ok (Hir.Call (Hir.Builtin b, checked, Hir.Int Hir.I64, s))
        | Hir.Addr_from_bits -> (
            if List.length args <> 1 then
              error s (Printf.sprintf "builtin `%s` expects one argument" name)
            else
              let* a = check_expr c (Some (Hir.Int Hir.U64)) (List.hd args) in
              match Hir.expr_ty a with
              | Hir.Int Hir.U64 -> Ok (Hir.Call (Hir.Builtin b, [ a ], Hir.Addr, s))
              | _ -> error s "addr_from_bits argument must be a usize")
        | Hir.Handle_addr -> (
            if List.length args <> 1 then
              error s (Printf.sprintf "builtin `%s` expects one argument" name)
            else
              let* a = check_expr c None (List.hd args) in
              match Hir.expr_ty a with
              | Hir.Handle _ -> Ok (Hir.Call (Hir.Builtin b, [ a ], Hir.Addr, s))
              | _ -> error s "handle_addr argument must be a handle")
        | Hir.Handle_from_addr "" ->
            error s (Printf.sprintf "builtin `%s` expects a type argument" name)
        | Hir.Handle_from_addr opaque_name -> (
            if List.length args <> 1 then
              error s (Printf.sprintf "builtin `%s` expects one argument" name)
            else
              let* a = check_expr c None (List.hd args) in
              match Hir.expr_ty a with
              | Hir.Addr ->
                  Ok
                    (Hir.Call
                       ( Hir.Builtin (Hir.Handle_from_addr opaque_name),
                         [ a ],
                         Hir.Handle opaque_name,
                         s ))
              | _ -> error s "handle_from_addr argument must be an addr")
        | _ ->
            if List.length args <> 2 then
              error s (Printf.sprintf "builtin `%s` expects two arguments" name)
            else
              let* a = check_expr c (literal_context (List.hd args)) (List.hd args) in
              let valid_operand =
                is_int (Hir.expr_ty a)
                ||
                match Hir.expr_ty a with
                | Hir.Vec (_, Hir.Int _) -> true
                | _ -> false
              in
              if not valid_operand then
                Error
                  [
                    Sema_types.rotate_value_error name
                      (Ast.expr_span (List.hd args))
                      (Hir.expr_ty a);
                  ]
              else
                let count = List.hd (List.tl args) in
                let count_expected =
                  match count with Ast.Splat _ -> Some (Hir.expr_ty a) | _ -> None
                in
                let* b2 = check_expr c count_expected count in
                if is_int (Hir.expr_ty b2) then
                  Ok (Hir.Call (Hir.Builtin b, [ a; b2 ], Hir.expr_ty a, s))
                else
                  let operation =
                    if name = "rotl" || name = "rotr" then "rotate" else "shift"
                  in
                  Error
                    [
                      (if operation = "rotate" then
                         Sema_types.rotate_count_error name (Ast.expr_span count)
                           (Hir.expr_ty b2)
                       else
                         Diag.error (Ast.expr_span count)
                           (Printf.sprintf
                              "shift count for `%s` must be a scalar integer, got `%s`"
                              name
                              (Sema_types.diagnostic_ty_name (Hir.expr_ty b2))));
                    ]
      in
      match builtin with
      | Some b
        when Names.reserved_binding_name name
             || Option.is_none (lookup_local name c)
                && Option.is_none (lookup_sig name c) ->
          check_builtin b
      | Some _ | None -> (
          match lookup_local name c with
          | Some _ -> error s (Printf.sprintf "`%s` is a value, not a function" name)
          | None -> (
              match lookup_sig name c with
              | None -> (
                  match List.assoc_opt name c.templates with
                  | Some item -> (
                      match item with
                      | Ast.Func { generic_params; _ } ->
                          error s
                            (generic_arity_message "generic function" name
                               (List.length generic_params) 0)
                      | _ ->
                          error s
                            "internal error: non-function in generic function template \
                             table")
                  | None ->
                      if
                        Option.is_some (lookup name c.consts)
                        || Option.is_some (lookup name c.arrays)
                      then
                        error s
                          (Printf.sprintf "`%s` is a constant, not a function" name)
                      else if Option.is_some (List.assoc_opt name c.named_types) then
                        error s (Printf.sprintf "`%s` is a type, not a function" name)
                      else
                        unknown_name_error
                          (List.map fst c.signatures @ List.map fst c.templates)
                          "function" s name)
              | Some sig_ ->
                  if
                    ((not sig_.variadic) && List.length args <> List.length sig_.params)
                    || (sig_.variadic && List.length args < List.length sig_.params)
                  then
                    error s
                      (function_arity_message name
                         (List.length sig_.params)
                         (List.length args))
                  else
                    let policy = if sig_.variadic then Promote_variadic else Reject in
                    let* xs = check_actuals ~callee:name c policy s sig_.params args in
                    let* () =
                      match List.assoc_opt name c.c_string_parameters with
                      | None -> Ok ()
                      | Some parameters ->
                          check_c_string_arguments c name parameters xs
                            args
                    in
                    let* () =
                      match
                        ( Sema_flow.proof_checks_enabled c.flow,
                          List.assoc_opt name c.c_nonnull_parameters )
                      with
                      | false, _ -> Ok ()
                      | true, None -> Ok ()
                      | true, Some indices ->
                          Result_list.iter
                            (fun index ->
                              match
                                ( List.nth_opt xs (index - 1),
                                  List.nth_opt args (index - 1) )
                              with
                              | Some value, Some argument
                                when address_fact c value
                                     = Some (Sema_flow.Null_address 0L) ->
                                  error (Ast.expr_span argument)
                                    (Printf.sprintf
                                       "null argument to nonnull parameter %d of `%s`"
                                       index name)
                              | _ -> Ok ())
                            indices
                    in
                    remember_alloc_size_object c name args
                      sig_.params s;
                    Ok (Hir.Call (Hir.User name, xs, sig_.ret, s)))))
  | _ -> error s "call target must be a function name"

and check_indirect_actuals c actuals =
  let rec check index acc = function
    | [] -> Ok (List.rev acc)
    | expression :: rest ->
        let* value = check_expr c None expression in
        let* () =
          match Hir.expr_ty value with
          | Hir.Void ->
              error (Ast.expr_span expression)
                (Printf.sprintf "argument %d of `call_addr` has type `void`" index)
          | ty when aggregate_value_type ty ->
              error (Ast.expr_span expression)
                (Printf.sprintf
                   "aggregate argument %d of `call_addr` cannot be passed by value"
                   index)
          | _ -> Ok ()
        in
        check (index + 1) (value :: acc) rest
  in
  check 1 [] actuals

and check_indirect_target c fact arguments result_ty span =
  let signature name (sig_ : Sema_context.signature) =
    Printf.sprintf "%s(%s) %s" name
      (String.concat ", "
         (List.map (fun (_, ty) -> Sema_types.diagnostic_ty_name ty) sig_.params)
      ^ if sig_.variadic then if sig_.params = [] then "..." else ", ..." else "")
      (Sema_types.diagnostic_ty_name sig_.ret)
  in
  let bad message = error span message in
  match fact with
  | Some (Sema_flow.Null_address 0L) -> bad "call_addr callee is proven null"
  | Some (Sema_flow.Object_address _ | Sema_flow.Dead_local_address _) ->
      bad "call_addr callee is proven not to be a function"
  | Some (Sema_flow.Function_address name) -> (
      match List.assoc_opt name c.signatures with
      | None -> bad (Printf.sprintf "internal error: missing signature for `%s`" name)
      | Some sig_ ->
          let named_signature = signature name sig_ in
          if sig_.variadic then
            bad
              (Printf.sprintf "call_addr cannot call variadic C function `%s`"
                 named_signature)
          else if List.length arguments <> List.length sig_.params then
            bad
              (Printf.sprintf "call_addr calls `%s` with %d arguments" named_signature
                 (List.length arguments))
          else
            let rec compare index actuals formals =
              match (actuals, formals) with
              | [], [] ->
                  if Hir.ty_equal result_ty sig_.ret then Ok ()
                  else
                    bad
                      (Printf.sprintf "call_addr result `%s` does not match `%s`"
                         (Sema_types.diagnostic_ty_name result_ty)
                         named_signature)
              | actual :: actuals, (_, expected) :: formals ->
                  let actual_ty = Hir.expr_ty actual in
                  if Hir.ty_equal actual_ty expected then
                    compare (index + 1) actuals formals
                  else
                    bad
                      (Printf.sprintf
                         "call_addr argument %d of `%s` has type `%s`, expected `%s`"
                         index named_signature
                         (Sema_types.diagnostic_ty_name actual_ty)
                         (Sema_types.diagnostic_ty_name expected))
              | _ -> Ok ()
            in
            compare 1 arguments sig_.params)
  | Some (Sema_flow.Null_address _) | None -> Ok ()

and check_actuals ?callee c policy span formals actuals =
  let expected_count = List.length formals and actual_count = List.length actuals in
  let arity_error () =
    match callee with
    | Some name -> error span (function_arity_message name expected_count actual_count)
    | None -> error span "wrong number of arguments"
  in
  let rec loop index checked formals actuals =
    match (formals, actuals) with
    | [], rest ->
        let* trailing =
          match policy with
          | Reject -> if rest = [] then Ok [] else arity_error ()
          | Promote_variadic ->
              Result_list.map
                (fun expression ->
                  let* value = check_expr c None expression in
                  if Hir.expr_ty value = Hir.Float Hir.F32 then
                    Error
                      [
                        Diag.error
                          ~help:
                            (Printf.sprintf "write `cvt[f64](%s)`"
                               (Ast.expr_name expression))
                          (Ast.expr_span expression)
                          "C varargs do not accept `f32`; convert it to `f64` \
                           explicitly";
                      ]
                  else if
                    is_scalar (Hir.expr_ty value)
                    ||
                    match Hir.expr_ty value with
                    | Hir.Addr | Hir.Handle _ -> true
                    | _ -> false
                  then Ok (variadic_promote value)
                  else if (match Hir.expr_ty value with Hir.Struct _ -> true | _ -> false)
                  then
                    (* a struct or union through C varargs: the call passes
                       it with its agg ABI, as C does (removal B1) *)
                    Ok value
                  else
                    let help =
                      match (expression, Hir.expr_ty value) with
                      | Ast.Ident (name, _), Hir.Array _ ->
                          Some
                            (Printf.sprintf "pass `&%s`; C passes arrays by address"
                               name)
                      | _ -> None
                    in
                    Error
                      [
                        Diag.error ?help (Ast.expr_span expression)
                          (Printf.sprintf
                             "aggregate `%s` cannot be passed through C varargs"
                             (Sema_types.diagnostic_ty_name (Hir.expr_ty value)));
                      ])
                rest
        in
        Ok (List.rev_append checked trailing)
    | (_, expected) :: formal_rest, expression :: actual_rest ->
        let* value = check_expr c (Some expected) expression in
        let* () =
          if
            aggregate_value_type (Hir.expr_ty value)
            && (match expected with Hir.Struct _ -> false | _ -> true)
          then
            let name, help =
              match expression with
              | Ast.Ident (name, _) ->
                  ( name,
                    Some
                      (Printf.sprintf "pass `&%s` to a function that accepts `addr`"
                         name) )
              | _ -> (Ast.expr_name expression, None)
            in
            Error
              [
                Diag.error ?help (Ast.expr_span expression)
                  (Printf.sprintf
                     "aggregate argument `%s` of type `%s` cannot be passed by value"
                     name
                     (Sema_types.diagnostic_ty_name (Hir.expr_ty value)));
              ]
          else
            let context =
              match callee with
              | Some name -> Printf.sprintf "argument %d of `%s`" index name
              | None -> Printf.sprintf "argument %d" index
            in
            ensure_expected ~context ~expression ~checked_expression:value
              (Hir.expr_ty value) expected (Ast.expr_span expression)
        in
        loop (index + 1) (value :: checked) formal_rest actual_rest
    | _ -> arity_error ()
  in
  loop 1 [] formals actuals

let rec check_target (c : context) target =
  let result = check_target_inner c target in
  (match (target, result) with
  | Ast.Target_ident (name, span), Ok _ -> index_ident c name span
  | _ -> ());
  result

and check_target_inner (c : context) = function
  | Ast.Target_ident (n, span) -> (
      match lookup_local n c with
      | Some b when Option.is_some (Sema_flow.raw_view_type c.flow b) ->
          raw_view_index_error span b
      | Some b -> (
          match view_access c.flow b with
          | Readonly_access | Constant_access ->
              readonly_write_error c span (Hir.Local (b, span))
          | Mutable_access ->
              let root, path =
                match view_origin c.flow b with
                | Some (root, path) -> (Some root, Some path)
                | None -> (None, None)
              in
              let* () =
                if is_view c.flow b then
                  check_place_access c span ~write:true (Hir.Local (b, span))
                else Ok ()
              in
              Ok { target = Hir.ALocal b; root; path; through_view = is_view c.flow b })
      | None -> (
          match List.assoc_opt n c.c_unsupported with
          | Some _ -> Option.get (c_unsupported c span n)
          | None -> (
              match lookup_top_level n c.top_level_bindings with
              | Some { declaration_kind = Top_const; _ } ->
                  error span (Printf.sprintf "constant `%s` is not assignable" n)
              | Some { declaration_kind = Top_type; _ } ->
                  error span (Printf.sprintf "type `%s` is not assignable" n)
              | Some { declaration_kind = Top_function; _ } ->
                  error span (Printf.sprintf "function `%s` is not assignable" n)
              | Some { declaration_kind = Top_global; _ } -> (
                  match lookup_global n c.globals with
                  | Some (_, _, Ast.Import_const_c) ->
                      error span (Printf.sprintf "global `%s` is read-only" n)
                  | Some (_, ty, _) ->
                      Ok
                        {
                          target = Hir.AGlobal (n, ty);
                          root = None;
                          path = None;
                          through_view = false;
                        }
                  | None -> error span "internal error: global declaration is missing")
              | None -> error span (Printf.sprintf "unknown assignment target `%s`" n)))
      )
  | Ast.Target_select (a, args) -> (
      let* place = check_place c (Ast.Select (a, args, Ast.expr_span a)) in
      let x = place.expr in
      let access = view_access_of_expr c x in
      match x with
      | Hir.Raw_select (_, _, (Hir.Struct _ | Hir.Array _ as t), _)
        when not (Hir.is_value_struct t) ->
          error (Ast.expr_span a) "raw access cannot store an array or struct value"
      | Hir.Raw_select (base, off, t, _) ->
          let* () = check_place_access c (Ast.expr_span a) ~write:true x in
          if access <> Mutable_access then
            readonly_write_error ~source_expression:a c (Ast.expr_span a) x
          else
            Ok
              {
                target = Hir.ARaw (base, off, t);
                root = None;
                path = None;
                through_view = expression_uses_view c x;
              }
      | Hir.Index (base, index, _, _) -> (
          if access <> Mutable_access then
            readonly_write_error ~source_expression:a c (Ast.expr_span a) x
          else
            match Hir.expr_ty base with
            | Hir.Array (_, _) | Hir.Vec _ ->
                Ok
                  {
                    target = Hir.AIndex (base, index);
                    root = place.root;
                    path = place.path;
                    through_view = expression_uses_view c x;
                  }
            | _ -> error (Ast.expr_span a) "index assignment requires aggregate")
      | _ -> error (Ast.expr_span a) "index assignment requires aggregate")
  | Ast.Target_field (a, n, field_span) -> (
      let* place = check_place c (Ast.Field (a, n, field_span)) in
      let x = place.expr in
      let access = view_access_of_expr c x in
      match x with
      | Hir.Raw_select (_, _, (Hir.Struct _ | Hir.Array _ as t), _)
        when not (Hir.is_value_struct t) ->
          error (Ast.expr_span a) "raw access cannot store an array or struct value"
      | Hir.Raw_select (base, off, t, _) ->
          let* () = check_place_access c (Ast.expr_span a) ~write:true x in
          if access <> Mutable_access then readonly_write_error c (Ast.expr_span a) x
          else
            Ok
              {
                target = Hir.ARaw (base, off, t);
                root = None;
                path = None;
                through_view = expression_uses_view c x;
              }
      | Hir.Field (base, _, _, _, _) -> (
          if access <> Mutable_access then readonly_write_error c (Ast.expr_span a) x
          else
            match Hir.expr_ty base with
            | Hir.Struct sn -> (
                match field_info c.structs sn n with
                | Some { unsupported_reason = Some reason; _ } ->
                    error (Ast.expr_span a) reason
                | Some f ->
                    Ok
                      {
                        target = Hir.AField (base, n, f.offset);
                        root = place.root;
                        path = place.path;
                        through_view = expression_uses_view c x;
                      }
                | None ->
                    missing_field_error ~base:a c (Ast.expr_span a) n (Hir.Struct sn))
            | actual -> missing_field_error ~base:a c (Ast.expr_span a) n actual)
      | _ -> missing_field_error ~base:a c (Ast.expr_span a) n (Hir.expr_ty x))

let target_ty c = function
  | Hir.ALocal binding -> Some binding.ty
  | Hir.AGlobal (_, ty) -> Some ty
  | Hir.ARaw (_, _, t) -> Some t
  | Hir.AIndex (expression, _) -> (
      match Hir.expr_ty expression with
      | Hir.Array (_, t) | Hir.Vec (_, t) -> Some t
      | _ -> None)
  | Hir.AField (expression, name, _) -> (
      match Hir.expr_ty expression with
      | Hir.Struct struct_name ->
          Option.map
            (fun (field : Hir.field) -> field.ty)
            (field_info c.structs struct_name name)
      | _ -> None)

let rec existing_place = function
  | Hir.Local _ | Hir.Global _ | Hir.Const_array _ | Hir.Raw_select _ -> true
  | Hir.Index (base, _, _, _) -> (
      match Hir.expr_ty base with Hir.Array _ -> existing_place base | _ -> false)
  | Hir.Field (base, _, _, _, _) -> existing_place base
  | _ -> false

let rec contains_raw_place = function
  | Hir.Raw_select _ -> true
  | Hir.Index (base, _, _, _) | Hir.Field (base, _, _, _, _) -> contains_raw_place base
  | _ -> false

let known_local_path place =
  if contains_raw_place place.expr then None
  else
    match (place.root, place.path) with
    | Some binding, Some (Exact path) -> Some (binding, path)
    | _ -> None

let local_path place =
  if contains_raw_place place.expr then None
  else
    match (place.root, place.path) with
    | Some binding, Some path -> Some (binding, path)
    | _ -> None

let copy_is_aggregate = function Hir.Array _ | Hir.Struct _ -> true | _ -> false

let rec copy_ternary_span = function
  | Ast.Ternary (_, _, alternate, start) ->
      let first = start and last = Ast.expr_span alternate in
      Some
        (Span.make ~file:first.file ~start_offset:first.start_offset
           ~end_offset:last.end_offset ~line:first.line ~column:first.column)
  | Ast.Parenthesized (expression, _) -> copy_ternary_span expression
  | _ -> None

let rec expression_mentions_name name = function
  | Ast.Ident (found, _) -> found = name
  | Ast.Unary (_, value, _)
  | Ast.C_dereference (value, _, _)
  | Ast.C_dot_star (value, _)
  | Ast.Parenthesized (value, _)
  | Ast.Cast (_, _, value, _)
  | Ast.Field (value, _, _)
  | Ast.Arrow_field (value, _, _, _)
  | Ast.Addr_of (value, _)
  | Ast.Handle_from_addr (_, value, _)
  | Ast.Splat (value, _) ->
      expression_mentions_name name value
  | Ast.Binary (_, left, right, _) ->
      expression_mentions_name name left || expression_mentions_name name right
  | Ast.Call (callee, args, _) ->
      expression_mentions_name name callee
      || List.exists (expression_mentions_name name) args
  | Ast.Generic_args (value, args, _) | Ast.Select (value, args, _) ->
      expression_mentions_name name value
      || List.exists
           (function
             | Ast.Const_arg value -> expression_mentions_name name value | _ -> false)
           args
  | Ast.Ternary (condition, yes, no, _) ->
      expression_mentions_name name condition
      || expression_mentions_name name yes
      || expression_mentions_name name no
  | Ast.Array_lit (values, _) -> List.exists (expression_mentions_name name) values
  | Ast.Designated_field (_, _, value, _) -> expression_mentions_name name value
  | Ast.Designated_index (index, value, _) ->
      expression_mentions_name name index || expression_mentions_name name value
  | Ast.Int_lit _ | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _
  | Ast.Sizeof _ | Ast.Alignof _ | Ast.Offsetof _ ->
      false
  | Ast.Sizeof_value (value, _) -> expression_mentions_name name value
  | Ast.Try (call, _) | Ast.Catch (call, Ast.Catch_block _, _) ->
      expression_mentions_name name call
  | Ast.Catch (call, Ast.Catch_value value, _) ->
      expression_mentions_name name call || expression_mentions_name name value

let () = expression_mentions_name_ref := expression_mentions_name

let rec expression_takes_name_address name = function
  | Ast.Addr_of (value, _) ->
      expression_mentions_name name value || expression_takes_name_address name value
  | Ast.Unary (_, value, _)
  | Ast.C_dereference (value, _, _)
  | Ast.C_dot_star (value, _)
  | Ast.Parenthesized (value, _)
  | Ast.Cast (_, _, value, _)
  | Ast.Field (value, _, _)
  | Ast.Arrow_field (value, _, _, _)
  | Ast.Handle_from_addr (_, value, _)
  | Ast.Splat (value, _) ->
      expression_takes_name_address name value
  | Ast.Binary (_, left, right, _) ->
      expression_takes_name_address name left
      || expression_takes_name_address name right
  | Ast.Call (callee, args, _) ->
      expression_takes_name_address name callee
      || List.exists (expression_takes_name_address name) args
  | Ast.Generic_args (value, args, _) | Ast.Select (value, args, _) ->
      expression_takes_name_address name value
      || List.exists
           (function
             | Ast.Const_arg value -> expression_takes_name_address name value
             | _ -> false)
           args
  | Ast.Ternary (condition, yes, no, _) ->
      expression_takes_name_address name condition
      || expression_takes_name_address name yes
      || expression_takes_name_address name no
  | Ast.Array_lit (values, _) -> List.exists (expression_takes_name_address name) values
  | Ast.Designated_field (_, _, value, _) -> expression_takes_name_address name value
  | Ast.Designated_index (index, value, _) ->
      expression_takes_name_address name index
      || expression_takes_name_address name value
  | Ast.Int_lit _ | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _
  | Ast.Ident _ | Ast.Sizeof _ | Ast.Alignof _ | Ast.Offsetof _ ->
      false
  | Ast.Sizeof_value (value, _) -> expression_takes_name_address name value
  | Ast.Try (call, _) | Ast.Catch (call, Ast.Catch_block _, _) ->
      expression_takes_name_address name call
  | Ast.Catch (call, Ast.Catch_value value, _) ->
      expression_takes_name_address name call
      || expression_takes_name_address name value

let target_takes_name_address name = function
  | Ast.Target_ident _ -> false
  | Ast.Target_select (value, args) ->
      expression_takes_name_address name value
      || List.exists
           (function
             | Ast.Const_arg value -> expression_takes_name_address name value
             | _ -> false)
           args
  | Ast.Target_field (value, _, _) -> expression_takes_name_address name value

let rec statement_changes_name name = function
  | Ast.Let { init; _ } ->
      Option.fold ~none:false ~some:(expression_takes_name_address name) init
  | Ast.View { place; _ } -> expression_mentions_name name place
  | Ast.Assign (Ast.Target_ident (found, _), value, _)
  | Ast.Compound_assign (Ast.Target_ident (found, _), _, value, _, _) ->
      found = name || expression_takes_name_address name value
  | Ast.Assign (target, value, _) | Ast.Compound_assign (target, _, value, _, _) ->
      target_takes_name_address name target || expression_takes_name_address name value
  | Ast.Return (value, _) ->
      Option.fold ~none:false ~some:(expression_takes_name_address name) value
  | Ast.If (condition, yes, no, _) ->
      expression_takes_name_address name condition
      || List.exists (statement_changes_name name) yes
      || Option.fold ~none:false ~some:(List.exists (statement_changes_name name)) no
  | Ast.While (_, condition, body, _) ->
      expression_takes_name_address name condition
      || List.exists (statement_changes_name name) body
  | Ast.Defer (body, _) | Ast.Block (body, _) | Ast.Const_copy { body; _ } ->
      List.exists (statement_changes_name name) body
  | Ast.Const_for { name = index; low; high; body; _ } ->
      expression_takes_name_address name low
      || expression_takes_name_address name high
      || (index <> name && List.exists (statement_changes_name name) body)
  | Ast.Expr_stmt (expression, _, _) -> expression_takes_name_address name expression
  | Ast.For (_, init, condition, step, body, _) ->
      Option.fold ~none:false ~some:(statement_changes_name name) init
      || Option.fold ~none:false ~some:(expression_takes_name_address name) condition
      || Option.fold ~none:false ~some:(statement_changes_name name) step
      || List.exists (statement_changes_name name) body
  | Ast.Switch (value, arms, default, _) ->
      expression_takes_name_address name value
      || List.exists
           (fun (cases, body) ->
             List.exists (expression_takes_name_address name) cases
             || List.exists (statement_changes_name name) body)
           arms
      || Option.fold ~none:false
           ~some:(List.exists (statement_changes_name name))
           default
  | Ast.Break _ | Ast.Continue _ -> false
  | Ast.Fail (value, _) -> expression_takes_name_address name value
  | Ast.Errdefer (_, body, _) -> List.exists (statement_changes_name name) body
  | Ast.Catch_stmt { target; call; body; _ } -> (
      expression_takes_name_address name call
      || List.exists (statement_changes_name name) body
      ||
      match target with
      | Ast.Catch_assign (Ast.Target_ident (found, _)) -> found = name
      | Ast.Catch_assign target -> target_takes_name_address name target
      | Ast.Catch_let _ | Ast.Catch_discard -> false)

let stable_loop_operand body = function
  | Hir.EInt _ -> true
  | Hir.Local (binding, _) ->
      not (List.exists (statement_changes_name binding.name) body)
  | _ -> false

let loop_induction c init condition step body =
  match (init, condition, step) with
  | ( Some (Hir.Let (binding, Some initial, _)),
      Some condition,
      Some
        (Ast.Compound_assign
           (Ast.Target_ident (name, _), ((Ast.Add | Ast.Sub) as step_op), rhs, _, _)) )
    when name = binding.name && is_int binding.ty -> (
      let comparison =
        match condition with
        | Hir.Binary
            (((Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge) as op), Hir.Local (b, _), bound, _, _)
          when b.id = binding.id ->
            Some (op, bound)
        | Hir.Binary
            (((Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge) as op), bound, Hir.Local (b, _), _, _)
          when b.id = binding.id ->
            Some (reverse_comparison op, bound)
        | _ -> None
      in
      match (value_fact c initial, comparison) with
      | Some start, Some (op, bound) when fact_single binding.ty start -> (
          let before = Sema_flow.snapshot c.flow in
          let checked_step = check_expr c (Some binding.ty) rhs in
          let amount = Option.bind (Result.to_option checked_step) (value_fact c) in
          let stable_step =
            match Result.to_option checked_step with
            | Some expression -> stable_loop_operand body expression
            | None -> false
          in
          let stable_bound = stable_loop_operand body bound in
          Sema_flow.restore c.flow before;
          match (amount, value_fact c bound) with
          | Some amount, Some bound
            when stable_step && stable_bound && fact_single binding.ty amount
                 && fact_single binding.ty bound -> (
              let raw_step = amount.low in
              let movement =
                if is_unsigned binding.ty then
                  if raw_step = 0L then None else Some (step_op = Ast.Add, raw_step)
                else if step_op = Ast.Add then
                  if raw_step = 0L then None
                  else if raw_step > 0L then Some (true, raw_step)
                  else if raw_step = Int64.min_int then None
                  else Some (false, Int64.neg raw_step)
                else if raw_step = 0L || raw_step = Int64.min_int then None
                else if raw_step < 0L then Some (true, Int64.neg raw_step)
                else Some (false, raw_step)
              in
              match movement with
              | Some (forward, stride)
                when (if forward then op = Ast.Lt || op = Ast.Le
                      else op = Ast.Gt || op = Ast.Ge)
                     && comparison_truth op binding.ty start bound = Some true -> (
                  let distance =
                    if forward then fact_sub binding.ty bound.low start.low
                    else fact_sub binding.ty start.low bound.low
                  in
                  let strict = op = Ast.Lt || op = Ast.Gt in
                  let distance =
                    Option.bind distance (fun distance ->
                        if strict then fact_sub binding.ty distance 1L
                        else Some distance)
                  in
                  let quotient =
                    Option.map
                      (fun distance ->
                        if is_unsigned binding.ty then
                          Int64.unsigned_div distance stride
                        else Int64.div distance stride)
                      distance
                  in
                  let last =
                    Option.bind quotient (fun quotient ->
                        let offset = fact_mul binding.ty quotient stride in
                        Option.bind offset (fun offset ->
                            if forward then fact_add binding.ty start.low offset
                            else fact_sub binding.ty start.low offset))
                  in
                  let valid_step =
                    match last with
                    | Some last ->
                        if forward then Option.is_some (fact_add binding.ty last stride)
                        else Option.is_some (fact_sub binding.ty last stride)
                    | None -> false
                  in
                  match last with
                  | Some last when valid_step ->
                      Some
                        ( binding,
                          {
                            low = fact_min binding.ty start.low last;
                            high = fact_max binding.ty start.low last;
                            induction = Some binding.id;
                          },
                          (start.low, stride) )
                  | _ -> None)
              | _ -> None)
          | _ -> None)
      | _ -> None)
  | _ -> None

let check_copy c args span =
  if List.length args <> 2 then error span "builtin `copy` expects two arguments"
  else
    let destination_arg = List.nth args 0 and source_arg = List.nth args 1 in
    let reject_ternary argument_index expression =
      match copy_ternary_span expression with
      | Some ternary_span ->
          error ternary_span
            (Printf.sprintf
               "argument %d of `copy` cannot be an `if` expression; name the array or \
                struct"
               argument_index)
      | None -> Ok ()
    in
    let* () = reject_ternary 1 destination_arg in
    let* () = reject_ternary 2 source_arg in
    let* destination = check_place c destination_arg in
    let* source = check_place c source_arg in
    let* () =
      if existing_place destination.expr && existing_place source.expr then Ok ()
      else if not (existing_place destination.expr) then
        error
          (Ast.expr_span destination_arg)
          "argument 1 of `copy` must be an existing array or struct"
      else
        error (Ast.expr_span source_arg)
          "argument 2 of `copy` must be an existing array or struct"
    in
    let destination_ty = Hir.expr_ty destination.expr
    and source_ty = Hir.expr_ty source.expr in
    let* () =
      if copy_is_aggregate destination_ty then
        if copy_is_aggregate source_ty then Ok ()
        else
          error (Ast.expr_span source_arg)
            (Printf.sprintf
               "argument 2 of `copy` has type `%s`, expected an array or struct"
               (Sema_types.diagnostic_ty_name source_ty))
      else
        error
          (Ast.expr_span destination_arg)
          (Printf.sprintf
             "argument 1 of `copy` has type `%s`, expected an array or struct"
             (Sema_types.diagnostic_ty_name destination_ty))
    in
    let* () =
      if Hir.ty_equal destination_ty source_ty then Ok ()
      else
        error (Ast.expr_span source_arg)
          (Printf.sprintf
             "argument 2 of `copy` has type `%s`, expected `%s` to match argument 1"
             (Sema_types.diagnostic_ty_name source_ty)
             (Sema_types.diagnostic_ty_name destination_ty))
    in
    let* () =
      match view_access_of_expr c destination.expr with
      | Mutable_access -> Ok ()
      | _ ->
          readonly_write_error ~source_expression:destination_arg c
            (Ast.expr_span destination_arg)
            destination.expr
    in
    let* () = check_place_access c span ~write:true destination.expr in
    let* () = check_place_access c span ~write:false source.expr in
    Sema_flow.forget_addresses_on_write c.flow;
    let* () =
      match local_path source with
      | Some (binding, path) -> require_place_state binding path c span
      | None -> Ok ()
    in
    let scratch_free =
      match (local_path destination, local_path source) with
      | Some (destination_root, _), Some (source_root, _) ->
          destination_root.id <> source_root.id
      | _ -> false
    in
    (match (known_local_path destination, known_local_path source) with
    | Some (destination_root, destination_path), Some (source_root, source_path) ->
        copy_state c.flow destination_root destination_path source_root source_path
    | Some (destination_root, destination_path), None ->
        set_state c destination_root destination_path Unknown
    | None, _ -> (
        match local_path destination with
        | Some (destination_root, Exact path)
        | Some (destination_root, Dynamic_prefix path) ->
            set_state c destination_root path Unknown
        | _ -> ()));
    Ok (Hir.Copy (destination.expr, source.expr, destination_ty, scratch_free, span))

let stmt_terminates = Ast.stmt_terminates
let catch_counter = ref 0

let rec check_block (c : context) stmts =
  push c;
  let rec go acc = function
    | [] ->
        let out = List.rev acc in
        let* () = Sema_flow.finish_block_scope c.flow in
        Ok out
    | s :: rest -> (
        let before = Sema_flow.snapshot c.flow in
        let saved =
          if Sema_flow.recovering c.flow then
            Some
              (Sema_flow.checkpoint c.flow, c.expression_depth, c.handle_view_expansion)
          else None
        in
        let checked =
          match s with
          | Ast.Catch_stmt _ -> check_catch_stmt c s
          | _ -> Result.map (fun x -> [ x ]) (check_stmt c s)
        in
        match (checked, saved) with
        | Ok xs, _ ->
            Sema_flow.finish_statement c.flow ~before ~terminates:(stmt_terminates s);
            go (List.rev_append xs acc) rest
        | Error diagnostics, Some (checkpoint, depth, expansion)
          when List.for_all Sema_flow.recoverable diagnostics ->
            Sema_flow.record c.flow diagnostics;
            Sema_flow.rewind c.flow checkpoint;
            c.expression_depth <- depth;
            c.handle_view_expansion <- expansion;
            let continues = recover_stmt c s in
            Sema_flow.finish_statement c.flow ~before ~terminates:(stmt_terminates s);
            let acc = match placeholder s with Some x -> x :: acc | None -> acc in
            if continues then go acc rest else go acc []
        | Error diagnostics, _ -> Error diagnostics)
  in
  go [] stmts

(* `[target =] CALL catch [e] { BODY }` (see "Fas error handling"):
     err#N error                         // the call's error, 0 on success
     tmp#N T = <call, error into err#N>  // the value
     if bitcast[u32](err#N) != 0 { e error = err#N; BODY }
     target = tmp#N                      // a declaration or an assignment
   The '#' names cannot be written in source. BODY must leave unless the call
   has no result. *)
and check_catch_stmt c = function
  | Ast.Catch_stmt { target; call; binding; body; span } ->
      let* expected =
        match target with
        | Ast.Catch_let { ty; ty_span; _ } ->
            let* t = source_ty_in_context c ty_span ty in
            Ok (Some t)
        | Ast.Catch_assign target -> (
            match check_target c target with
            | Ok checked -> Ok (target_ty c checked.target)
            | Error _ -> Ok None)
        | Ast.Catch_discard -> Ok None
      in
      let* inner = check_handled_call c expected "catch" call in
      let ty = Hir.expr_ty inner in
      let* () =
        match target with
        | Ast.Catch_discard -> Ok ()
        | _ when ty = Hir.Void ->
            error (Ast.expr_span call) "the call has no result to store"
        | _ -> Ok ()
      in
      incr catch_counter;
      let n = string_of_int !catch_counter in
      let err_name = "err#" ^ n and tmp_name = "tmp#" ^ n in
      let* err_local = add_local err_name Hir.Err c span in
      let first = Hir.Let (err_local, None, span) in
      let* value_stmt =
        if ty = Hir.Void then Ok (Hir.Expr (Hir.Capture (inner, err_local, ty, span), span))
        else
          let* tmp_local = add_local tmp_name ty c span in
          mark_init tmp_local c;
          Ok (Hir.Let (tmp_local, Some (Hir.Capture (inner, err_local, ty, span)), span))
      in
      mark_init err_local c;
      let handler_body =
        (match binding with
        | Some (name, name_span) ->
            [
              Ast.Let
                {
                  name;
                  ty = Ast.Named_type ("error", name_span);
                  ty_span = name_span;
                  init = Some (Ast.Ident (err_name, name_span));
                  span = name_span;
                };
            ]
        | None -> [])
        @ body
      in
      let condition =
        Ast.Binary
          ( Ast.Ne,
            Ast.Cast (Ast.Bitcast, Ast.Int Ast.U32, Ast.Ident (err_name, span), span),
            Ast.Int_lit ("0", span),
            span )
      in
      let* handler = check_stmt c (Ast.If (condition, handler_body, None, span)) in
      let* () =
        match handler with
        | Hir.If (_, yes, _, _) when ty <> Hir.Void && (Hir.block_flow yes).falls_through
          ->
            Error
              [
                Diag.error
                  ~help:
                    "leave with `return`, `fail`, `break` or `continue`, or use `catch \
                     VALUE` to substitute a value"
                  span "this `catch` block can fall through, but the call's value is used";
              ]
        | _ -> Ok ()
      in
      (* the handler left or did nothing: the flow continues as after the call *)
      Sema_flow.set_falls_through c.flow true;
      let* rest =
        match target with
        | Ast.Catch_discard -> Ok []
        | Ast.Catch_let { name; ty = source; ty_span } ->
            let* x =
              check_stmt c
                (Ast.Let
                   {
                     name;
                     ty = source;
                     ty_span;
                     init = Some (Ast.Ident (tmp_name, span));
                     span;
                   })
            in
            Ok [ x ]
        | Ast.Catch_assign target ->
            let* x = check_stmt c (Ast.Assign (target, Ast.Ident (tmp_name, span), span)) in
            Ok [ x ]
      in
      Ok ([ first; value_stmt; handler ] @ rest)
  | statement -> Result.map (fun x -> [ x ]) (check_stmt c statement)

and recover_stmt c s =
  Sema_flow.forget_facts c.flow;
  List.iter
    (fun name ->
      match lookup_local name c with Some binding -> mark_init binding c | None -> ())
    (Ast.stmt_names s);
  match s with
  | Ast.Let { name; ty; ty_span; span; _ } -> (
      match source_ty_in_context c ty_span ty with
      | Error _ -> false
      | Ok t -> (
          match ensure_new_local name c span with
          | Error _ -> true
          | Ok () -> (
              match add_local name t c span with
              | Ok binding ->
                  mark_init binding c;
                  true
              | Error _ -> false)))
  | Ast.View _ -> false
  | _ -> true

and placeholder = function
  | Ast.Break (target, s) -> Some (Hir.Break (Option.map fst target, s))
  | Ast.Continue (target, s) -> Some (Hir.Continue (Option.map fst target, s))
  | s when stmt_terminates s -> Some (Hir.Return (None, Ast.stmt_span s))
  | _ -> None

and check_stmt (c : context) = function
  | Ast.Let { name; ty; ty_span; init; span } -> (
      let* () = ensure_new_local name c span in
      let* t = source_ty_in_context c ty_span ty in
      let* _ =
        Sema_limits.validate_object
          ?opaque_span:(Sema_types.opaque_source_span c.named_types [] ty)
          ?vector_element_span:(Sema_types.vector_element_source_span ty_span ty)
          c.limits c.structs ty_span t
      in
      let* x =
        match init with
        | None -> Ok None
        | Some e ->
            let* initialized = check_initializer ~destination:name c t e in
            let* () =
              let actual =
                match initialized with
                | `Value value -> Hir.expr_ty value
                | `Aggregate (ty, _, _, _) -> ty
              in
              if
                (match initialized with
                  | `Value value -> not (c_struct_result value)
                  | `Aggregate _ -> false)
                && aggregate_value_type t && aggregate_value_type actual
              then
                error (Ast.expr_span e)
                  "aggregate value initialization is not supported; use `copy(dst, \
                   src)`"
              else
                let checked_expression =
                  match initialized with
                  | `Value value -> Some value
                  | `Aggregate _ -> None
                in
                ensure_expected ?checked_expression
                  ~context:(Printf.sprintf "value for `%s`" name)
                  ~expression:e actual t (Ast.expr_span e)
            in
            Ok (Some initialized)
      in
      let* binding = add_local name t c span in
      index_local c binding span;
      (match (init, x) with
      | Some e, Some (`Aggregate (ty, _, _, _)) ->
          index_type c (Ast.expr_span e) ty None
      | _ -> ());
      (match x with
      | Some (`Value value) ->
          mark_init binding c;
          Sema_flow.set_value c.flow binding (value_fact c value);
          Sema_flow.set_address c.flow binding (address_fact c value);
          Sema_flow.set_mask c.flow binding (static_vector_lanes c value)
      | Some (`Aggregate _) -> mark_init binding c
      | None -> ());
      match x with
      | None -> Ok (Hir.Let (binding, None, span))
      | Some (`Value value) -> Ok (Hir.Let (binding, Some value, span))
      | Some (`Aggregate (ty, entries, construction_span, zero_padding)) ->
          Ok
            (Hir.Let_construct
               ( binding,
                 aggregate_construction ~zero_padding ty entries construction_span,
                 span )))
  | Ast.View { name; name_span; place; span } -> (
      let* () = ensure_new_local name c span in
      match place with
      | Ast.Select (base, [ type_payload; Ast.Name_arg ("..", _) ], select_span) ->
          let* element = select_type_arg c.named_types select_span type_payload in
          let* () =
            if element = Hir.Void then
              error
                (select_type_payload_span select_span type_payload)
                "raw access on `void` needs an element type"
            else Ok ()
          in
          let* pointer = check_expr c (Some Hir.Addr) base in
          if Hir.expr_ty pointer <> Hir.Addr then
            error (Ast.expr_span base) "unknown-length view base must have type `addr`"
          else
            let address = address_fact c pointer in
            let access = view_access_of_expr c pointer in
            let readonly_name = readonly_storage_name c pointer in
            let* binding = add_local name Hir.Addr c span in
            index_local c binding name_span;
            mark_init binding c;
            Sema_flow.set_address c.flow binding address;
            Sema_flow.bind_raw_view c.flow binding element access readonly_name;
            Ok (Hir.Let (binding, Some pointer, span))
      | _ -> (
          let check_regular_view place =
            let* place_info = check_place c place in
            let rec addressable = function
              | Hir.Local _ | Hir.Global _ | Hir.Const_array _ | Hir.Raw_select _ ->
                  true
              | Hir.Index (base, _, _, _) -> (
                  match Hir.expr_ty base with
                  | Hir.Array _ -> addressable base
                  | Hir.Vec _ -> false
                  | _ -> false)
              | Hir.Field (base, _, _, _, _) -> addressable base
              | _ -> false
            in
            let* () =
              if
                match place_info.expr with
                | Hir.Index (base, _, _, _) -> (
                    match Hir.expr_ty base with Hir.Vec _ -> true | _ -> false)
                | _ -> false
              then error (Ast.expr_span place) "cannot create a view of a SIMD lane"
              else if addressable place_info.expr then Ok ()
              else
                let source_kind =
                  match place with
                  | Ast.Call _ -> "a function call"
                  | Ast.Binary _ | Ast.Ternary _ | Ast.Unary _ -> "a computed value"
                  | Ast.Int_lit _ -> "an integer literal"
                  | Ast.Bool_lit _ -> "a bool literal"
                  | Ast.Null _ -> "a null literal"
                  | Ast.String_lit _ -> "a string literal"
                  | Ast.Array_lit _ -> "an aggregate literal"
                  | Ast.Splat _ -> "a splat"
                  | Ast.Cast _ -> "a cast"
                  | Ast.Addr_of _ -> "an address value"
                  | _ -> "an expression"
                in
                error (view_source_span place)
                  (Printf.sprintf
                     "view needs a local, field, element or raw access, not %s"
                     source_kind)
            in
            let* () = check_place_access c span ~write:false place_info.expr in
            let* binding = add_local name (Hir.expr_ty place_info.expr) c span in
            index_local c binding name_span;
            let readonly_name = readonly_storage_name c place_info.expr in
            let root, path =
              match place_info.expr with
              | Hir.Raw_select _ -> (None, None)
              | _ -> (place_info.root, place_info.path)
            in
            bind_view c.flow binding root path
              (view_access_of_expr c place_info.expr)
              readonly_name;
            Ok (Hir.View (binding, place_info.expr, span))
          in
          let check_handle_view place =
            let previous = c.handle_view_expansion in
            c.handle_view_expansion <- true;
            let result = check_regular_view place in
            c.handle_view_expansion <- previous;
            result
          in
          match place with
          | Ast.Ident (handle_name, handle_span) -> (
              match lookup_local handle_name c with
              | Some { ty = Hir.Handle record; _ } ->
                  if imported_record_type c record then
                    let handle_call =
                      Ast.Call
                        ( Ast.Ident ("handle_addr", handle_span),
                          [ Ast.Ident (handle_name, handle_span) ],
                          span )
                    in
                    check_handle_view
                      (Ast.Select
                         (handle_call, [ Ast.Name_arg (record, handle_span) ], span))
                  else
                    error handle_span
                      (Printf.sprintf
                         "cannot view `%s` through a handle: its layout is unknown"
                         record)
              | _ -> check_regular_view place)
          | _ -> check_regular_view place))
  | Ast.Assign (t, e, span) ->
      let* checked_target = check_target c t in
      let target = checked_target.target in
      let* expected =
        match target_ty c target with
        | Some t -> Ok t
        | None -> error span "invalid assignment target"
      in
      let* v = check_expr c (Some expected) e in
      let* () =
        if
          aggregate_value_type expected
          && aggregate_value_type (Hir.expr_ty v)
          && not (c_struct_result v)
        then
          error span
            (if match expected with Hir.Array _ -> true | _ -> false then
               "aggregate assignment is not supported for an array; use `copy(dst, \
                src)`"
             else "aggregate assignment is not supported; use `copy(dst, src)`")
        else
          ensure_expected ~checked_expression:v ~context:(assignment_context target)
            ~expression:e (Hir.expr_ty v) expected (Ast.expr_span e)
      in
      (match target with
      | Hir.ALocal binding when binding.ty = Hir.Addr ->
          let target_binding =
            match Sema_flow.view_origin c.flow binding with
            | Some (root, Sema_flow.Exact []) when root.ty = Hir.Addr -> root
            | _ -> binding
          in
          Sema_flow.set_address c.flow target_binding (address_fact c v)
      | Hir.ARaw _ -> Sema_flow.forget_addresses_on_write c.flow
      | _ -> ());
      (match (checked_target.root, checked_target.path) with
      | Some binding, Some (Exact path) -> set_state c binding path Full
      | Some binding, Some (Dynamic_prefix path) -> set_state c binding path Unknown
      | _ -> ());
      (match (checked_target.root, checked_target.path) with
      | Some binding, Some (Exact []) when is_int binding.ty ->
          Sema_flow.note_binding_write c.flow binding.id;
          Sema_flow.set_value c.flow binding (value_fact c v)
      | Some binding, Some (Exact []) ->
          Sema_flow.note_binding_write c.flow binding.id;
          Sema_flow.set_address c.flow binding (address_fact c v);
          Sema_flow.set_mask c.flow binding (static_vector_lanes c v)
      | Some binding, _ -> Sema_flow.note_binding_write c.flow binding.id
      | _ -> ());
      Ok (Hir.Assign (target, v, span))
  | Ast.Compound_assign (t, op, e, span, operator_span) ->
      let target_span =
        match t with
        | Ast.Target_ident (_, target_span) | Ast.Target_field (_, _, target_span) ->
            target_span
        | Ast.Target_select (expression, _) -> Ast.expr_span expression
      in
      let* checked_target = check_target c t in
      let target = checked_target.target in
      let* et =
        match target_ty c target with
        | Some t -> Ok t
        | None -> error span "invalid compound assignment target"
      in
      let* () =
        match (checked_target.root, checked_target.path) with
        | Some binding, Some path -> require_place_state binding path c span
        | _ -> Ok ()
      in
      let is_shift = op = Ast.Shl || op = Ast.Shr in
      let is_addr_step =
        et = Hir.Addr && (op = Ast.Add || op = Ast.Sub) && not is_shift
      in
      let* () =
        if et = Hir.Addr && not is_addr_step then
          error operator_span
            (Printf.sprintf "compound assignment `%s` is not defined for `%s`"
               (compound_operator op)
               (Sema_types.diagnostic_ty_name et))
        else Ok ()
      in
      let* v =
        check_expr c
          (if is_shift || is_addr_step || is_if_operand e then None else Some et)
          e
      in
      let* () =
        if is_shift then
          let* () =
            match et with
            | Hir.Int _ | Hir.Vec (_, Hir.Int _) -> Ok ()
            | _ -> Error [ Sema_types.shift_value_error op target_span et ]
          in
          match (et, Hir.expr_ty v) with
          | Hir.Vec (lanes, _), Hir.Vec (count_lanes, (Hir.Int _ as count_element)) ->
              if lanes = count_lanes then Ok ()
              else
                Error
                  [
                    Sema_types.shift_count_lanes_error op (Ast.expr_span e)
                      (Hir.expr_ty v)
                      (Hir.Vec (lanes, count_element));
                  ]
          | Hir.Vec _, Hir.Int _ -> Ok ()
          | Hir.Int _, Hir.Int _ -> Ok ()
          | _, (Hir.Vec _ as count_ty) | _, (Hir.Bool as count_ty) ->
              Error [ Sema_types.shift_count_error op (Ast.expr_span e) count_ty ]
          | _ ->
              Error
                [ Sema_types.shift_count_error op (Ast.expr_span e) (Hir.expr_ty v) ]
        else if is_addr_step then
          match Hir.expr_ty v with
          | Hir.Int _ -> Ok ()
          | actual ->
              error (Ast.expr_span e)
                (Printf.sprintf
                   "right operand of `%s` is `%s`; `addr` arithmetic needs a scalar \
                    integer offset"
                   (compound_operator op)
                   (Sema_types.diagnostic_ty_name actual))
        else
          ensure_expected ~checked_expression:v
            ~context:(Printf.sprintf "right operand of `%s`" (compound_operator op))
            ~expression:e (Hir.expr_ty v) et (Ast.expr_span e)
      in
      let* v = if is_addr_step then normalize_offset_expr c.structs span v else Ok v in
      let float_arithmetic =
        (is_float et || match et with Hir.Vec (_, Hir.Float _) -> true | _ -> false)
        && List.mem op [ Ast.Add; Ast.Sub; Ast.Mul; Ast.Div ]
      in
      if (not (is_numeric et)) && (not float_arithmetic) && not is_addr_step then
        let help =
          match (et, op, t) with
          | Hir.Bool, Ast.Bit_and, Ast.Target_ident (name, _) ->
              Some (Printf.sprintf "write `%s = %s & %s`" name name (Ast.expr_name e))
          | _ -> None
        in
        error ?help operator_span
          (Printf.sprintf "compound assignment `%s` is not defined for `%s`"
             (compound_operator op)
             (Sema_types.diagnostic_ty_name et))
      else (
        (match (target, is_addr_step) with
        | Hir.ALocal binding, true when binding.ty = Hir.Addr ->
            let current = Hir.Local (binding, span) in
            Sema_flow.set_address c.flow binding
              (address_fact c (Hir.Binary (op, current, v, Hir.Addr, span)))
        | Hir.ARaw _, _ -> Sema_flow.forget_addresses_on_write c.flow
        | _ -> ());
        (match (checked_target.root, checked_target.path) with
        | Some binding, Some (Exact path) -> set_state c binding path Full
        | Some binding, Some (Dynamic_prefix path) -> set_state c binding path Unknown
        | _ -> ());
        (match (checked_target.root, checked_target.path) with
        | Some binding, Some (Exact []) when is_int binding.ty ->
            Sema_flow.note_binding_write c.flow binding.id;
            let result =
              match (op, value_fact c (Hir.Local (binding, span)), value_fact c v) with
              | ((Ast.Add | Ast.Sub | Ast.Mul) as op), Some left, Some right ->
                  combine_facts op et left right
              | _ -> None
            in
            Sema_flow.set_value c.flow binding result
        | Some binding, _ -> Sema_flow.note_binding_write c.flow binding.id
        | _ -> ());
        Ok (Hir.Compound_assign (target, op, v, et, span)))
  | Ast.Expr_stmt (Ast.Call (Ast.Ident ("copy", _), args, call_span), _, _) ->
      check_copy c args call_span
  | Ast.Expr_stmt
      (Ast.Call (Ast.Generic_args (Ast.Ident ("copy", _), _, _), _, call_span), _, _) ->
      error call_span "copy takes no type arguments"
  | Ast.Return (e, span) ->
      let* () = Sema_flow.validate_return c.flow span in
      let* x =
        match (e, c.ret_ty) with
        | None, Hir.Void -> Ok None
        | Some e, Hir.Void ->
            let* value = check_expr c None e in
            error (Ast.expr_span e)
              (Printf.sprintf "void function cannot return a value of type `%s`"
                 (Sema_types.diagnostic_ty_name (Hir.expr_ty value)))
        | Some e, t ->
            let* v = check_expr c (Some t) e in
            let* () =
              ensure_expected ~checked_expression:v ~context:"return value"
                ~expression:e (Hir.expr_ty v) t (Ast.expr_span e)
            in
            let* () =
              if t = Hir.Addr && Sema_flow.proof_checks_enabled c.flow then
                match address_fact c v with
                | Some (Sema_flow.Object_address { owner = Some _; owner_name; _ }) ->
                    error span
                      (Printf.sprintf "returns address of local `%s`"
                         (Option.value ~default:"" owner_name))
                | Some (Sema_flow.Dead_local_address name) ->
                    error span (Printf.sprintf "returns address of local `%s`" name)
                | _ -> Ok ()
              else Ok ()
            in
            Ok (Some v)
        | None, _ ->
            error span ("return value required (expected " ^ ty_name c.ret_ty ^ ")")
      in
      let* () =
        if Sema_flow.falls_through c.flow then validate_exit_defers c 0 else Ok ()
      in
      Sema_flow.invalidate_induction_on_return c.flow;
      Ok (Hir.Return (x, span))
  | Ast.Expr_stmt
      ( Ast.Call
          ( Ast.Generic_args
              (Ast.Ident ("volatile_store", _), generic_args, application_span),
            args,
            call_span ),
        span,
        _ ) ->
      let* access_ty =
        volatile_access_type c "volatile_store" application_span generic_args
      in
      if List.length args <> 2 then
        error call_span "builtin `volatile_store` expects two arguments"
      else
        let pointer_arg = List.nth args 0 and value_arg = List.nth args 1 in
        let* pointer = check_expr c (Some Hir.Addr) pointer_arg in
        let* () =
          ensure_expected ~context:"argument 1 of `volatile_store`"
            ~expression:pointer_arg (Hir.expr_ty pointer) Hir.Addr
            (Ast.expr_span pointer_arg)
        in
        let* value = check_expr c (Some access_ty) value_arg in
        let* () =
          ensure_expected ~context:"argument 2 of `volatile_store`"
            ~checked_expression:value ~expression:value_arg (Hir.expr_ty value)
            access_ty (Ast.expr_span value_arg)
        in
        let* size =
          match layout_diag call_span c.structs access_ty with
          | Ok (size, _) -> Ok size
          | Error diagnostics -> Error diagnostics
        in
        let* () =
          check_address_access c call_span ~write:true (address_fact c pointer) size
        in
        Sema_flow.forget_addresses_on_write c.flow;
        Ok (Hir.Volatile_store (access_ty, pointer, value, span))
  | Ast.Expr_stmt
      ( Ast.Call
          ( Ast.Generic_args (Ast.Ident (name, _), generic_args, application_span),
            args,
            call_span ),
        span,
        _ )
    when name = "masked_store" || name = "scatter" || name = "scatter_bytes" ->
      let* access_ty = simd_memory_access_type c name application_span generic_args in
      let* checked, _ = check_simd_memory_args c name access_ty args call_span in
      let* () = check_simd_access c call_span ~write:true name access_ty checked in
      let mask_lanes =
        static_vector_lanes c
          (List.nth checked (if List.length checked = 3 then 1 else 2))
      in
      let no_active_lanes =
        match mask_lanes with
        | Some lanes -> not (List.exists Fun.id lanes)
        | None -> false
      in
      let* () =
        if no_active_lanes then Ok ()
        else
          match view_access_of_expr c (List.hd checked) with
          | Mutable_access -> Ok ()
          | _ ->
              readonly_write_error ~source_expression:(List.hd args) c call_span
                (List.hd checked)
      in
      (match no_active_lanes with
      | true -> ()
      | _ -> Sema_flow.forget_addresses_on_write c.flow);
      let kind =
        match name with
        | "masked_store" -> Hir.Masked_store
        | "scatter" -> Hir.Scatter
        | _ -> Hir.Scatter_bytes
      in
      Ok (Hir.Simd_store (access_ty, kind, checked, span))
  | Ast.Expr_stmt (e, s, finish) ->
      let* x = check_expr c None e in
      (* a C call's struct or union result may be discarded, as in C (removal B1) *)
      if aggregate_value_type (Hir.expr_ty x) && not (c_struct_result x) then
        error s "aggregate values cannot be used by value; use `copy(dst, src)`"
      else if statement_call e then Ok (Hir.Expr (x, s))
      else
        let span =
          Span.make ~file:s.Span.file ~start_offset:s.Span.start_offset
            ~end_offset:finish.Span.end_offset ~line:s.Span.line ~column:s.Span.column
        in
        error
          ?help:
            (Option.map
               (fun op -> "to continue the previous line, end it with `" ^ op ^ "`")
               (Ast.expr_leading_operator e))
          span
          (Printf.sprintf "`%s` is not used" (Ast.expr_name e))
  | Ast.Block (xs, s) ->
      let* x = check_block c xs in
      Ok (Hir.Block (x, s))
  | Ast.Const_for { span; _ } ->
      error span "internal error: `for const` loop was not expanded"
  | Ast.Const_copy { name; ty; value; note; body; span } ->
      let* () =
        match lookup_local name c with
        | Some _ ->
            error span
              (Printf.sprintf "`for const` index `%s` shadows a local of the same name"
                 name)
        | None -> Ok ()
      in
      let* index_ty = source_ty_in_context c span ty in
      let copy = { c with consts = (name, index_ty, value) :: c.consts } in
      let statement =
        if Ast.copy_continues body then
          let label = Printf.sprintf "for const %s %d" name span.Span.start_offset in
          Ast.While
            ( Some { Ast.name = label; span },
              Ast.Bool_lit (true, span),
              Ast.continues_to_breaks label body
              @ [ Ast.Break (Some (label, span), span) ],
              span )
        else Ast.Block (body, span)
      in
      let annotate (diagnostic : Diag.t) =
        if List.mem note diagnostic.notes then diagnostic
        else { diagnostic with notes = diagnostic.notes @ [ note ] }
      in
      let recorded = Sema_flow.recovered_count c.flow in
      let result = check_stmt copy statement |> Result.map_error (List.map annotate) in
      Sema_flow.map_recovered_since c.flow recorded annotate;
      result
  | Ast.If (q, a, b, s) ->
      let* tq = check_expr c None q in
      if Hir.expr_ty tq <> Hir.Bool then
        Error [ Sema_types.condition_error "if" q (Hir.expr_ty tq) ]
      else
        let condition = condition_truth c tq
        and init_condition = literal_condition_truth tq in
        let before = Sema_flow.snapshot c.flow in
        let before_falls = Sema_flow.falls_through c.flow in
        refine_condition c tq true;
        Sema_flow.set_falls_through c.flow before_falls;
        let* ta =
          with_dead_check c (condition = Some false) (fun () -> check_block c a)
        in
        let ia = Sema_flow.snapshot c.flow in
        let fa = Sema_flow.falls_through c.flow in
        Sema_flow.restore c.flow before;
        refine_condition c tq false;
        Sema_flow.set_falls_through c.flow before_falls;
        let* tb =
          match b with
          | None -> Ok None
          | Some xs ->
              let* x =
                with_dead_check c (condition = Some true) (fun () -> check_block c xs)
              in
              Ok (Some x)
        in
        let ib = Sema_flow.snapshot c.flow in
        let fb = Sema_flow.falls_through c.flow in
        let initialized, initialization_falls_through =
          match init_condition with
          | Some true -> (ia, before_falls && fa)
          | Some false -> (ib, before_falls && fb)
          | None ->
              ( (match (fa, fb) with
                | true, true -> merge_maps c ia ib
                | true, false -> ia
                | false, true -> ib
                | false, false -> before),
                before_falls && (fa || fb) )
        in
        let value_paths =
          match condition with
          | Some true -> if fa then [ ia ] else []
          | Some false -> if fb then [ ib ] else []
          | None -> (
              match (fa, fb) with
              | true, true -> [ ia; ib ]
              | true, false -> [ ia ]
              | false, true -> [ ib ]
              | false, false -> [ before ])
        in
        Sema_flow.restore c.flow
          (Sema_flow.merge_values_into c.flow initialized value_paths);
        Sema_flow.set_falls_through c.flow initialization_falls_through;
        Ok (Hir.If (tq, ta, tb, s))
  | Ast.While (label, q, b, s) ->
      let* () =
        match label with
        | None -> Ok ()
        | Some label -> Sema_flow.check_loop_label c.flow (label.name, label.span)
      in
      let* tq = check_expr c None q in
      if Hir.expr_ty tq <> Hir.Bool then
        Error [ Sema_types.condition_error "while" q (Hir.expr_ty tq) ]
      else
        let condition = condition_truth c tq in
        let init_condition = literal_condition_truth tq in
        let loop =
          Sema_flow.begin_loop
            ?label:(Option.map (fun (label : Ast.loop_label) -> label.name) label)
            c.flow
        in
        Sema_flow.forget_all_values c.flow;
        if condition <> Some false then refine_condition c tq true;
        let checked =
          with_dead_check c (condition = Some false) (fun () -> check_block c b)
        in
        Sema_flow.end_loop c.flow;
        let* tb = checked in
        Sema_flow.finish_while c.flow loop
          ~condition_is_true:(init_condition = Some true)
          ~condition_is_false:(init_condition = Some false);
        Ok
          (Hir.While
             (Option.map (fun (label : Ast.loop_label) -> label.name) label, tq, tb, s))
  | Ast.For (label, i, q, step, b, s) ->
      let* () =
        match label with
        | None -> Ok ()
        | Some label -> Sema_flow.check_loop_label c.flow (label.name, label.span)
      in
      push c;
      let checked =
        let* ti =
          match i with
          | None -> Ok None
          | Some x ->
              let* y = check_stmt c x in
              Ok (Some y)
        in
        let* tq =
          match q with
          | None -> Ok None
          | Some x ->
              let* y = check_expr c None x in
              if Hir.expr_ty y = Hir.Bool then Ok (Some y)
              else Error [ Sema_types.condition_error "for" x (Hir.expr_ty y) ]
        in
        let induction = loop_induction c ti tq step b in
        let condition =
          match tq with
          | None -> Some true
          | Some condition -> condition_truth c condition
        in
        let loop =
          Sema_flow.begin_loop
            ?label:(Option.map (fun (label : Ast.loop_label) -> label.name) label)
            ?induction_binding:
              (Option.map (fun (binding, _, _) -> binding.id) induction)
            ?induction_grid:(Option.map (fun (_, _, grid) -> grid) induction)
            c.flow
        in
        Sema_flow.forget_all_values c.flow;
        Option.iter
          (fun (binding, fact, _) -> Sema_flow.set_value c.flow binding (Some fact))
          induction;
        let init_condition =
          match tq with
          | None -> Some true
          | Some condition -> literal_condition_truth condition
        in
        let single_iteration =
          match induction with
          | Some (_, fact, _) -> fact.low = fact.high
          | None -> false
        in
        (match (tq, induction) with
        | Some condition_expr, None when condition <> Some false ->
            refine_condition c condition_expr true
        | _ -> ());
        let body_result =
          with_dead_check c (condition = Some false) (fun () -> check_block c b)
        in
        let* tb = body_result in
        Sema_flow.prepare_for_step c.flow loop
          ~body_falls_through:(Hir.block_flow tb).falls_through;
        let* ts =
          match step with
          | None -> Ok None
          | Some x ->
              let* y =
                with_dead_check c (condition = Some false) (fun () -> check_stmt c x)
              in
              Ok (Some y)
        in
        Sema_flow.finish_for c.flow loop ~unconditional:(init_condition = Some true)
          ~condition_is_false:(init_condition = Some false) ~single_iteration;
        Sema_flow.end_loop c.flow;
        Ok
          (Hir.For
             ( Option.map (fun (label : Ast.loop_label) -> label.name) label,
               ti,
               tq,
               ts,
               tb,
               s ))
      in
      pop c;
      checked
  | Ast.Switch (e, arms, d, s) ->
      let* te = check_expr c None e in
      let et = Hir.expr_ty te in
      let enum_switch = match et with Hir.Enum (enum, _) -> Some enum | _ -> None in
      if not (is_int et || et = Hir.Bool || et = Hir.Err || enum_switch <> None) then
        error s
          (Printf.sprintf
             "switch value must be an integer, bool, enum or error, got `%s`"
             (Sema_types.diagnostic_ty_name et))
      else if et = Hir.Err && d = None then
        error s
          "a switch on an error needs a `default` arm: the set of errors is open"
      else
        let before = Sema_flow.snapshot c.flow
        and seen = ref Int64_map.empty
        and branch_states = ref []
        and selected_case = ref None
        and selected_default = ref None in
        let switch_value =
          match te with
          | Hir.EInt (value, _, _) -> Some value
          | Hir.EBool (value, _) -> Some (if value then 1L else 0L)
          | _ -> None
        in
        let before_falls = Sema_flow.falls_through c.flow in
        let branch_falls = ref [] in
        let evaluate_case =
          const_expr
            ~array_lengths:(static_array_lengths c.top_level_bindings c.globals)
            ~structs:c.structs ~named_types:c.named_types
            ~generic_structs:c.generic_structs
            ~globals:(List.map (fun (name, _, _) -> name) c.globals)
            ~arrays:c.arrays c.consts
        in
        let case_value_fits source value =
          let value = Sema_numeric.sign_extend_value source value in
          match et with
          | Hir.Int _ ->
              Sema_numeric.fits_literal et value
              || (not (Sema_numeric.is_unsigned et))
                 && value < 0L
                 && (not (Sema_numeric.is_unsigned source))
                 && Sema_numeric.sign_extend_bits et value = value
          | _ -> true
        in
        let case_value_text source value =
          match source with
          | Hir.Enum (enum, _) -> (
              match Sema_constants.enum_member_of enum value with
              | Some member -> enum ^ "." ^ member
              | None -> Int64.to_string value)
          | _ ->
          if Sema_numeric.is_unsigned source then
            Sema_numeric.unsigned_int64_to_string value
          else Int64.to_string (Sema_numeric.sign_extend_value source value)
        in
        let rec float_case_literal = function
          | Ast.Float_lit _ -> true
          | Ast.Parenthesized (value, _) | Ast.Unary (Ast.Neg, value, _) ->
              float_case_literal value
          | Ast.Binary (_, left, right, _) ->
              float_case_literal left || float_case_literal right
          | Ast.Ternary (_, yes, no, _) ->
              float_case_literal yes || float_case_literal no
          | _ -> false
        in
        let rec case_values acc selected = function
          | [] -> Ok (List.rev acc, selected)
          | k :: rest -> (
              let* kt, kv =
                evaluate_case (Some et) k
                |> Result.map_error (function
                  | _ when float_case_literal k ->
                      [ Sema_types.float_literal_type_error et (Ast.expr_span k) ]
                  | [ diagnostic ]
                    when String.starts_with ~prefix:"global `" diagnostic.Diag.message
                         && String.ends_with ~suffix:" is not a constant"
                              diagnostic.Diag.message -> (
                      let prefix = "global `" in
                      let first = String.length prefix in
                      let stop =
                        String.index_from_opt diagnostic.Diag.message first '`'
                      in
                      match stop with
                      | Some stop -> (
                          let name =
                            String.sub diagnostic.message first (stop - first)
                          in
                          match
                            ( lookup_top_level name c.top_level_bindings,
                              lookup_global name c.globals )
                          with
                          | ( Some { declaration_kind = Top_const; _ },
                              Some (_, Hir.Addr, _) ) ->
                              [
                                Diag.error diagnostic.primary
                                  (Printf.sprintf
                                     "address constant `%s` cannot be used as a case \
                                      label"
                                     name);
                              ]
                          | _ -> [ diagnostic ])
                      | None -> [ diagnostic ])
                  | [ diagnostic ]
                    when String.starts_with ~prefix:"cannot use `"
                           diagnostic.Diag.message -> (
                      match evaluate_case None k with
                      | Ok ((Hir.Int _ as source), value)
                        when not (case_value_fits source value) ->
                          [
                            Diag.error (Ast.expr_span k)
                              (Printf.sprintf
                                 "integer literal is out of range for %s: `%s`"
                                 (Sema_types.diagnostic_ty_name et)
                                 (case_value_text source value));
                          ]
                      | _ -> [ diagnostic ])
                  | [ diagnostic ]
                    when String.starts_with ~prefix:"integer literal is "
                           diagnostic.Diag.message
                         || String.starts_with ~prefix:"`" diagnostic.Diag.message ->
                      [ diagnostic ]
                  | _ ->
                      [
                        Diag.error (Ast.expr_span k)
                          "case label must be a compile-time constant";
                      ])
              in
              let* () =
                ensure_expected ~context:"case value" ~expression:k kt et
                  (Ast.expr_span k)
              in
              match Int64_map.find_opt kv !seen with
              | Some first_span ->
                  Error
                    [
                      Diag.error
                        ~notes:[ "first case value is at " ^ Span.to_string first_span ]
                        (Ast.expr_span k)
                        ("duplicate case label `" ^ case_value_text et kv ^ "`");
                    ]
              | None ->
                  seen := Int64_map.add kv (Ast.expr_span k) !seen;
                  let tk =
                    match et with
                    | Hir.Bool -> Hir.EBool (kv <> 0L, Ast.expr_span k)
                    | _ -> Hir.EInt (mask_value et kv, et, Ast.expr_span k)
                  in
                  case_values (tk :: acc) (selected || switch_value = Some kv) rest)
        in
        let rec ar acc = function
          | [] -> Ok (List.rev acc)
          | (ks, b) :: xs ->
              let* tks, selected = case_values [] false ks in
              Sema_flow.restore c.flow before;
              Sema_flow.set_falls_through c.flow before_falls;
              let* tb = check_block c b in
              let state = Sema_flow.snapshot c.flow
              and falls_through = Sema_flow.falls_through c.flow in
              if falls_through then branch_states := state :: !branch_states;
              branch_falls := falls_through :: !branch_falls;
              if selected then selected_case := Some (state, falls_through);
              ar ((tks, tb) :: acc) xs
        in
        let result = ar [] arms in
        let* ta = result in
        Sema_flow.restore c.flow before;
        Sema_flow.set_falls_through c.flow before_falls;
        (* a switch on an enum without `default` names every member; a value
           that is no member (only from memory C wrote) traps *)
        let* () =
          match (enum_switch, d) with
          | Some enum, None -> (
              let members = (Option.get (Hir.enum_def enum)).Hir.members in
              let missing =
                List.filter
                  (fun (_, v) -> not (Int64_map.mem (mask_value et v) !seen))
                  members
              in
              match missing with
              | [] -> Ok ()
              | _ ->
                  Error
                    [
                      Diag.error ~help:"add the missing cases, or a `default` arm" s
                        (Printf.sprintf "switch on enum `%s` does not handle %s" enum
                           (String.concat ", "
                              (List.map (fun (m, _) -> "`" ^ enum ^ "." ^ m ^ "`") missing)));
                    ])
          | _ -> Ok ()
        in
        let* td =
          match d with
          | None when enum_switch <> None ->
              Ok
                (Some
                   [ Hir.Expr (Hir.Call (Hir.Builtin Hir.Trap, [], Hir.Void, s), s) ])
          | None -> Ok None
          | Some x ->
              let* y = check_block c x in
              let state = Sema_flow.snapshot c.flow
              and falls_through = Sema_flow.falls_through c.flow in
              if falls_through then branch_states := state :: !branch_states;
              branch_falls := falls_through :: !branch_falls;
              selected_default := Some (state, falls_through);
              Ok (Some y)
        in
        (match d with
        | None when enum_switch <> None -> ()
        | None ->
            branch_states := before :: !branch_states;
            branch_falls := true :: !branch_falls
        | Some _ -> ());
        (match switch_value with
        | Some _ ->
            let state, falls_through =
              match (!selected_case, !selected_default) with
              | Some selected, _ -> selected
              | None, Some selected -> selected
              | None, None -> (before, true)
            in
            Sema_flow.restore c.flow state;
            Sema_flow.set_falls_through c.flow (before_falls && falls_through)
        | None ->
            Sema_flow.restore c.flow
              (match !branch_states with
              | [] -> before
              | first :: rest -> List.fold_left (merge_maps c) first rest);
            Sema_flow.set_falls_through c.flow
              (before_falls && List.exists (fun value -> value) !branch_falls));
        Ok (Hir.Switch (te, ta, td, s))
  | Ast.Break (target, s) ->
      let* () = Sema_flow.record_break c.flow target s in
      Ok (Hir.Break (Option.map fst target, s))
  | Ast.Continue (target, s) ->
      let* () = Sema_flow.record_continue c.flow target s in
      Ok (Hir.Continue (Option.map fst target, s))
  | Ast.Catch_stmt { span; _ } as statement ->
      let* xs = check_catch_stmt c statement in
      Ok (Hir.Block (xs, span))
  | Ast.Fail (e, s) ->
      if Sema_flow.in_defer c.flow then
        error s "`fail` is not allowed inside defer or errdefer"
      else if not c.fn_fails then
        Error
          [
            Diag.error ~help:"mark the function `fails`" s
              "`fail` needs a `fails` function";
          ]
      else
        let* v = check_expr c (Some Hir.Err) e in
        let* () =
          ensure_expected ~checked_expression:v ~context:"fail" ~expression:e
            (Hir.expr_ty v) Hir.Err (Ast.expr_span e)
        in
        let* () =
          if Sema_flow.falls_through c.flow then validate_exit_defers c 0 else Ok ()
        in
        Sema_flow.invalidate_induction_on_return c.flow;
        Ok (Hir.Fail (v, s))
  | Ast.Errdefer (binding, xs, s) ->
      if not c.fn_fails then
        Error
          [
            Diag.error ~help:"mark the function `fails`, or use `defer`" s
              "`errdefer` needs a `fails` function";
          ]
      else
        let* capture = Sema_flow.begin_defer c.flow s in
        push c;
        let checked =
          let* local =
            match binding with
            | None -> Ok None
            | Some (name, name_span) ->
                let* () = ensure_new_local name c name_span in
                let* local = add_local name Hir.Err c name_span in
                mark_init local c;
                Ok (Some local)
          in
          let* body = check_block c xs in
          Ok (local, body)
        in
        pop c;
        let* local, body =
          Sema_flow.finish_defer ~errdefer:true c.flow capture checked
            ~falls_through:(fun _ -> true)
        in
        Ok (Hir.Errdefer (local, body, s))
  | Ast.Defer (xs, s) ->
      let* capture = Sema_flow.begin_defer c.flow s in
      let checked = check_block c xs in
      let* body =
        Sema_flow.finish_defer c.flow capture checked ~falls_through:(fun body ->
            (Hir.block_flow body).falls_through)
      in
      Ok (Hir.Defer (body, s))
