(* Filesystem paths used for linking only. Interpreter and runtime search strings
   belong to the target and must never acquire this host-side root prefix. *)
let validate path =
  if String.length path > 65536 || String.contains path '\000' then
    Error.fail "invalid or oversized sysroot/search path"

let normalize path =
  validate path;
  if path = "" then None
  else
    let path =
      if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
    in
    let path =
      try Unix.realpath path
      with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
        let rec simplify found = function
          | [] -> "/" ^ String.concat "/" (List.rev found)
          | ("" | ".") :: rest -> simplify found rest
          | ".." :: rest ->
              simplify (match found with [] -> [] | _ :: tail -> tail) rest
          | item :: rest -> simplify (item :: found) rest
        in
        simplify [] (String.split_on_char '/' path)
    in
    validate path;
    Some path

let target ~root path =
  validate path;
  let result = match root with None -> path | Some root -> root ^ path in
  validate result;
  result

let prefixed path =
  if String.starts_with ~prefix:"=" path then
    Some (String.sub path 1 (String.length path - 1))
  else if String.starts_with ~prefix:"$SYSROOT" path then
    Some (String.sub path 8 (String.length path - 8))
  else None

let file ~root path =
  match prefixed path with
  | None ->
      validate path;
      None
  | Some suffix -> Some (target ~root suffix)

let directory ~root path =
  validate path;
  match prefixed path with
  | None -> path
  | Some suffix ->
      let result =
        match root with
        | None -> suffix
        | Some root ->
            let rec first index =
              if index < String.length suffix && suffix.[index] = '/' then
                first (index + 1)
              else index
            in
            let index = first 0 in
            Filename.concat root
              (String.sub suffix index (String.length suffix - index))
      in
      validate result;
      result

let contains ~root canonical =
  match root with
  | None -> false
  | Some "/" -> true
  | Some root -> canonical = root || String.starts_with ~prefix:(root ^ "/") canonical
