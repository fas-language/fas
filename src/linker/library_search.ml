let find ?(exists = Sys.file_exists) ~dynamic ~directories name =
  if name = "" then Error.fail "empty library name";
  let files =
    if String.starts_with ~prefix:":" name then
      [ String.sub name 1 (String.length name - 1) ]
    else if dynamic then [ "lib" ^ name ^ ".so"; "lib" ^ name ^ ".a" ]
    else [ "lib" ^ name ^ ".a" ]
  in
  if List.exists (fun file -> file = "" || Filename.basename file <> file) files then
    Error.fail "invalid library name %s" name;
  let rec search = function
    | [] -> Error.fail "cannot find -l%s in explicit -L directories" name
    | directory :: rest -> (
        match List.find_opt exists (List.map (Filename.concat directory) files) with
        | Some path -> path
        | None -> search rest)
  in
  search directories
