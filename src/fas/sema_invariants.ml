let split_at count xs =
  let rec go remaining acc = function
    | rest when remaining = 0 -> (List.rev acc, rest)
    | [] -> (List.rev acc, [])
    | x :: rest -> go (remaining - 1) (x :: acc) rest
  in
  go count [] xs

let ordered_subsequence ~within ~sub =
  let rec go within sub =
    match (within, sub) with
    | _, [] -> true
    | [], _ -> false
    | w :: within', s :: sub' when String.equal w s -> go within' sub'
    | _ :: within', sub -> go within' sub
  in
  go within sub

let duplicate names =
  let seen = Hashtbl.create 16 in
  List.find_opt
    (fun name ->
      if Hashtbl.mem seen name then true
      else (
        Hashtbl.add seen name ();
        false))
    names

let check_declarations bindings =
  let seen = Hashtbl.create 16 in
  let rec go index = function
    | [] -> Ok ()
    | (id, name) :: rest ->
        if id <> index then
          Error (Printf.sprintf "declaration id %d is not dense at index %d" id index)
        else if Hashtbl.mem seen name then
          Error (Printf.sprintf "declaration `%s` is collected twice" name)
        else (
          Hashtbl.add seen name ();
          go (index + 1) rest)
  in
  go 0 bindings

let check_const_environment ~declared ~early ~consts ~arrays =
  let names = consts @ arrays in
  match duplicate consts with
  | Some name -> Error (Printf.sprintf "constant `%s` is emitted twice" name)
  | None -> (
      match duplicate arrays with
      | Some name -> Error (Printf.sprintf "constant `%s` is emitted twice" name)
      | None ->
          if List.sort String.compare declared <> List.sort String.compare names then
            Error "emitted constants do not match declared constants exactly"
          else if not (ordered_subsequence ~within:declared ~sub:arrays) then
            Error "array and vector constants are not in declaration order"
          else
            let prefix, suffix = split_at (List.length early) consts in
            if prefix <> early then
              Error "emitted scalar constants do not start with the early-resolved set"
            else if not (ordered_subsequence ~within:declared ~sub:early) then
              Error "early-resolved scalar constants are not in declaration order"
            else if not (ordered_subsequence ~within:declared ~sub:suffix) then
              Error "late scalar constants are not in declaration order"
            else Ok ())

let check_materialization ~pending ~functions =
  if pending <> 0 then
    Error
      (Printf.sprintf "%d specializations remain pending after materialization" pending)
  else
    match duplicate functions with
    | Some name -> Error (Printf.sprintf "function `%s` is emitted twice" name)
    | None -> Ok ()
