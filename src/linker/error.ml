exception Error of string

let fail fmt = Printf.ksprintf (fun message -> raise (Error message)) fmt

let io f =
  try f () with
  | Sys_error message -> fail "%s" message
  | Unix.Unix_error (code, call, path) ->
      fail "%s: %s: %s" call path (Unix.error_message code)
