let ( let* ) result next =
  match result with Ok value -> next value | Error diagnostics -> Error diagnostics

let error span message = Error [ Diag.error span message ]

let rec object_type (structs : Hir.struct_def list) = function
  | Hir.Void -> Error "void is not an object type"
  | Hir.Opaque name ->
      Error ("opaque type `" ^ name ^ "` can only be held as `handle[" ^ name ^ "]`")
  | Hir.Bool | Hir.Int _ | Hir.Float _ | Hir.Addr | Hir.Handle _ -> Ok ()
  | Hir.Array (length, element) ->
      if length < 0 then Error "negative array length" else object_type structs element
  | Hir.Vec (length, element) when length > 0 -> (
      match element with
      | Hir.Int _ | Hir.Bool | Hir.Float _ -> Ok ()
      | _ -> Error "vector element type must be `bool` or an integer type")
  | Hir.Vec _ -> Error "vector lane count must be positive"
  | Hir.Struct name ->
      if
        List.exists
          (fun (definition : Hir.struct_def) -> definition.name = name)
          structs
      then Ok ()
      else Error ("unknown struct `" ^ name ^ "`")

let aggregate_within_limit limits structs ty =
  let max_elements = limits.Limits.max_aggregate_elements in
  let rec count visiting = function
    | Hir.Array (length, element) | Hir.Vec (length, element) -> (
        if length = 0 then Some 0
        else
          match count visiting element with
          | Some elements when elements = 0 || length <= max_elements / elements ->
              Some (length * elements)
          | Some _ | None -> None)
    | Hir.Struct name -> (
        if List.mem name visiting then None
        else
          let visiting = name :: visiting in
          match
            List.find_opt
              (fun (definition : Hir.struct_def) -> definition.name = name)
              structs
          with
          | Some definition ->
              List.fold_left
                (fun total (field : Hir.field) ->
                  match (total, count visiting field.ty) with
                  | Some total, Some field_count
                    when field_count <= max_elements - total ->
                      Some (total + field_count)
                  | Some _, Some _ | None, _ | _, None -> None)
                (Some 0) definition.fields
          | None -> Some 1)
    | _ -> if max_elements >= 1 then Some 1 else None
  in
  Option.is_some (count [] ty)

let validate_object ?opaque_span ?vector_element_span limits structs span ty =
  let* () =
    object_type structs ty
    |> Result.map_error (fun message ->
        let primary =
          if String.starts_with ~prefix:"opaque type " message then
            Option.value ~default:span opaque_span
          else if String.starts_with ~prefix:"vector element type" message then
            Option.value ~default:span vector_element_span
          else span
        in
        [ Diag.error primary message ])
  in
  let* size, alignment =
    Hir.layout structs ty
    |> Result.map_error (fun message -> [ Diag.error span message ])
  in
  let* () =
    if alignment <= limits.Limits.max_object_alignment then Ok ()
    else
      error span
        (Printf.sprintf
           "alignment exceeds budget max_object_alignment of %d (profile %s)"
           limits.Limits.max_object_alignment
           (Limits.budget_profile_name limits))
  in
  let* () =
    if size <= limits.Limits.max_object_size then Ok ()
    else
      error span
        (Printf.sprintf "object size exceeds budget max_object_size of %d (profile %s)"
           limits.Limits.max_object_size
           (Limits.budget_profile_name limits))
  in
  if aggregate_within_limit limits structs ty then Ok ty
  else
    error span
      (Printf.sprintf
         "aggregate element count exceeds the configured limit: budget \
          max_aggregate_elements of %d (profile %s)"
         limits.Limits.max_aggregate_elements
         (Limits.budget_profile_name limits))

let validate_struct_alignment limits span = function
  | Some alignment ->
      let* () =
        Target_layout.validate_type_alignment (Target_layout.current ()) alignment
        |> Result.map_error (fun message -> [ Diag.error span message ])
      in
      if alignment <= limits.Limits.max_object_alignment then Ok ()
      else
        error span
          (Printf.sprintf
             "alignment exceeds budget max_object_alignment of %d (profile %s)"
             limits.Limits.max_object_alignment
             (Limits.budget_profile_name limits))
  | None -> Ok ()
