open Sema_constants
open Sema_context
open Sema_types
module String_set = Set.Make (String)

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let collect ?(global_names = []) ?(array_lengths = []) ?(generic_structs = [])
    ~source_obj ~structs ~named_types ~scalar_consts (program : Ast.program) =
  let consts = ref (List.rev scalar_consts) and arrays = ref [] in
  let consts_names =
    ref (String_set.of_list (List.map (fun (n, _, _) -> n) scalar_consts))
  and arrays_names = ref String_set.empty in
  let const_value name expression result =
    match result with
    | Error [ { Diag.issue = Diag.Not_constant; primary; _ } ]
    | Error
        [
          { Diag.message = "constant expression requires a known constant"; primary; _ };
        ] ->
        error primary
          (Printf.sprintf "constant `%s` initializer uses nonconstant value `%s`" name
             (Ast.expr_name expression))
    | result -> result
  in
  let eval_const_item = function
    | Ast.Const { name; name_span; ty_span; ty; value; span; extent = _ } ->
        if String_set.mem name !consts_names then Ok ()
        else if String_set.mem name !arrays_names then
          error name_span (Printf.sprintf "duplicate const `%s`" name)
        else
          let* t = source_obj ty_span ty in
          let result =
            match (t, value) with
            | Hir.Array (_, (Hir.Bool | Hir.Int _)), Ast.Array_lit ([], _) -> (
                let zero = Hir.zero_initializer t in
                match Hir.zero_integer_array_values zero with
                | Some values ->
                    arrays := (name, t, values) :: !arrays;
                    arrays_names := String_set.add name !arrays_names;
                    Ok ()
                | None -> error span "internal error: invalid zero array initializer")
            | Hir.Array (n, elem), Ast.Array_lit (xs, _) ->
                if List.length xs <> n then
                  error
                    (Sema_types.aggregate_count_error_span (Ast.expr_span value) n xs)
                    (Sema_types.array_element_count_message n (List.length xs))
                else
                  let rec values acc = function
                    | [] -> Ok (List.rev acc)
                    | x :: rest ->
                        let* vt, v =
                          const_expr ~array_lengths ~structs ~named_types
                            ~generic_structs ~globals:global_names ~arrays:!arrays
                            !consts (Some elem) x
                        in
                        if equal vt elem then values (v :: acc) rest
                        else
                          error (Ast.expr_span x)
                            (constant_array_element_type_message vt elem)
                  in
                  let* vs = values [] xs in
                  arrays := (name, t, vs) :: !arrays;
                  arrays_names := String_set.add name !arrays_names;
                  Ok ()
            | Hir.Array _, _ ->
                error (Ast.expr_span value) "const array needs a brace-list initializer"
            | (Hir.Vec _ as vector_ty), _ ->
                let* actual_ty, values =
                  vector_const_expr ~array_lengths ~structs ~named_types
                    ~generic_structs ~globals:global_names ~arrays:!arrays !consts
                    (Some vector_ty) value
                in
                if equal actual_ty vector_ty then (
                  arrays := (name, vector_ty, values) :: !arrays;
                  arrays_names := String_set.add name !arrays_names;
                  Ok ())
                else
                  error (Ast.expr_span value)
                    (constant_initializer_type_message actual_ty vector_ty)
            | _, _ ->
                let* vt, v =
                  const_value name value
                    (const_expr ~array_lengths ~structs ~named_types ~generic_structs
                       ~globals:global_names ~arrays:!arrays !consts (Some t) value)
                in
                if equal vt t then (
                  consts := (name, t, v) :: !consts;
                  consts_names := String_set.add name !consts_names;
                  Ok ())
                else
                  error (Ast.expr_span value) (constant_initializer_type_message vt t)
          in
          result
    | _ -> Ok ()
  in
  let* () = Result_list.iter eval_const_item program.items in
  let consts_ordered = List.rev !consts and arrays_ordered = List.rev !arrays in
  Ok (consts_ordered, arrays_ordered, List.map (fun (name, _, _) -> name) arrays_ordered)
