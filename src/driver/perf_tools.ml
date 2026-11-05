let ( let* ) = Result.bind
let hex text = int_of_string_opt ("0x" ^ text)
let lines text = String.split_on_char '\n' text

let read_file path =
  try Some (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error _ -> None

let split_at c text =
  Option.map
    (fun i ->
      (String.sub text 0 i, String.sub text (i + 1) (String.length text - i - 1)))
    (String.index_opt text c)

let before c text =
  match String.index_opt text c with Some i -> String.sub text 0 i | None -> text

let after c text = match split_at c text with Some (_, rest) -> rest | None -> text

let drop_prefix prefix text =
  if String.starts_with ~prefix text then
    let n = String.length prefix in
    Some (String.sub text n (String.length text - n))
  else None

let debuginfod_free argv = Array.append [| "env"; "DEBUGINFOD_URLS=" |] argv
let run_env argv = Process.run (debuginfod_free argv)

let tool_output name argv =
  match run_env argv with
  | Ok (out, _) -> Ok out
  | Error failure ->
      let detail = String.trim failure.Process.stderr in
      Error
        (Printf.sprintf "fas perf: %s failed%s" name
           (if detail = "" then "" else ": " ^ detail))

type insn = { addr : int; op : string; arg : string }
type item = Label of string | Instr of int * string

type loop = {
  start : int;
  latch : int;
  stop : int;
  instrs : int;
  calls : string list;
  trap : bool;
  timeable : bool;
  header : string;
  body : item list;
}

let parse_insn line =
  Option.bind (split_at ':' line) (fun (address, rest) ->
      let spaced = String.map (fun c -> if c = '\t' then ' ' else c) rest in
      match
        (hex address, List.filter (( <> ) "") (String.split_on_char ' ' spaced))
      with
      | Some addr, op :: args -> Some { addr; op; arg = String.concat " " args }
      | _ -> None)

let header_of line =
  match split_at ' ' line with
  | Some (address, name) when String.ends_with ~suffix:">:" line ->
      Option.map (fun addr -> (addr, name)) (hex address)
  | _ -> None

let routines text =
  let finish (insns, labels) =
    if insns = [] then None else Some (Array.of_list (List.rev insns), List.rev labels)
  in
  let step (done_, (insns, labels)) raw =
    let line = String.trim raw in
    if Option.is_some (header_of line) then (finish (insns, labels) :: done_, ([], []))
    else if String.starts_with ~prefix:"<" line && String.ends_with ~suffix:">:" line
    then
      let label = (String.sub line 0 (String.length line - 1), List.length insns) in
      (done_, (insns, label :: labels))
    else
      match parse_insn line with
      | Some insn -> (done_, (insn :: insns, labels))
      | None -> (done_, (insns, labels))
  in
  let done_, last = List.fold_left step ([], ([], [])) (lines text) in
  List.filter_map Fun.id (List.rev (finish last :: done_))

let symbol_starts text =
  List.filter_map
    (fun raw ->
      match header_of (String.trim raw) with
      | Some (addr, name) when String.starts_with ~prefix:"<" name ->
          Some (String.sub name 1 (String.length name - 3), addr)
      | _ -> None)
    (lines text)

let callee arg = before '+' (before '>' (after '<' arg))

let unbracket text =
  String.of_seq (Seq.filter (fun c -> c <> '<' && c <> '>') (String.to_seq text))

let is_jump insn = String.starts_with ~prefix:"j" insn.op
let is_call insn = String.starts_with ~prefix:"call" insn.op

let assembly insn =
  let arg = before '#' insn.arg in
  let arg =
    if is_jump insn || is_call insn then unbracket arg
    else
      let operands = before '<' arg in
      if String.contains operands ',' then
        String.concat ","
          (List.map
             (fun piece -> if String.trim piece = "" then "(%rip)" else piece)
             (String.split_on_char ',' operands))
      else operands
  in
  String.trim (insn.op ^ " " ^ arg)

let loops_of (insns, labels) =
  let n = Array.length insns in
  let target i =
    if is_jump insns.(i) then List.assoc_opt insns.(i).arg labels else None
  in
  let ud2 k = k < n && insns.(k).op = "ud2" in
  let back i = match target i with Some h -> h <= i | None -> false in
  let traps i = ud2 i || match target i with Some t -> ud2 t | None -> false in
  let items i =
    List.filter_map
      (fun (name, at) -> if at = i then Some (Label (unbracket name)) else None)
      labels
    @ [ Instr (insns.(i).addr, assembly insns.(i)) ]
  in
  List.filter_map
    (fun latch ->
      match target latch with
      | Some header when header <= latch ->
          let range = List.init (latch - header + 1) (( + ) header) in
          let calls = List.filter (fun i -> is_call insns.(i)) range in
          Some
            {
              start = insns.(header).addr;
              latch = insns.(latch).addr;
              stop =
                (if latch + 1 < n then insns.(latch + 1).addr
                 else insns.(latch).addr + 1);
              instrs = latch - header + 1;
              calls = List.map (fun i -> callee insns.(i).arg) calls;
              trap = List.exists traps range;
              timeable =
                calls = [] && not (List.exists (fun i -> i <> latch && back i) range);
              header = unbracket insns.(latch).arg;
              body = List.concat_map items range;
            }
      | _ -> None)
    (List.init n Fun.id)

let instructions (loop : loop) =
  List.filter_map (function Instr (a, t) -> Some (a, t) | Label _ -> None) loop.body

let disassemble exe =
  let objdump = Driver.tool "FAS_OBJDUMP" "llvm-objdump-22" in
  let opts =
    [ "-d"; "--no-show-raw-insn"; "--symbolize-operands"; "--no-debuginfod" ]
  in
  tool_output objdump (Array.of_list ((objdump :: opts) @ [ exe ]))

type place = {
  file : string;
  line : int;
  func : string;
  caller : (string * int) option;
  host : string;
}

let blocks text =
  let rec go acc current = function
    | [] -> List.rev acc
    | "" :: rest -> go (List.rev current :: acc) [] rest
    | line :: rest -> go acc (line :: current) rest
  in
  Array.of_list (go [] [] (lines text))

let parse_location text =
  match List.rev (String.split_on_char ':' text) with
  | _ :: line :: (_ :: _ as file) ->
      Option.map
        (fun line -> (String.concat ":" (List.rev file), line))
        (int_of_string_opt line)
  | _ -> None

let rec outermost = function
  | [ name; _ ] -> name
  | _ :: _ :: rest -> outermost rest
  | _ -> ""

let locate = function
  | func :: location :: rest as frames -> (
      match parse_location location with
      | Some (file, line) when List.exists (Filename.check_suffix file) [ ".fas"; ".c" ]
        ->
          let caller =
            match rest with
            | caller :: caller_location :: _ ->
                Option.map (fun (_, l) -> (caller, l)) (parse_location caller_location)
            | _ -> None
          in
          Some { file; line; func; caller; host = outermost frames }
      | _ -> None)
  | _ -> None

let rec call_chain = function
  | f :: l :: rest -> (f, Option.map snd (parse_location l)) :: call_chain rest
  | _ -> []

let rec chunks size = function
  | [] -> []
  | list ->
      List.filteri (fun i _ -> i < size) list
      :: chunks size (List.filteri (fun i _ -> i >= size) list)

let frames_of exe addresses =
  let symbolizer = Driver.tool "FAS_SYMBOLIZER" "llvm-symbolizer-22" in
  let flags = [ "--obj=" ^ exe; "--inlining"; "--relativenames"; "--no-debuginfod" ] in
  let table = Hashtbl.create 64 in
  let query batch =
    let* text =
      tool_output symbolizer
        (Array.of_list ((symbolizer :: flags) @ List.map (Printf.sprintf "0x%x") batch))
    in
    let blocks = blocks text in
    List.iteri
      (fun i address ->
        Hashtbl.replace table address
          (if i < Array.length blocks then Some blocks.(i) else None))
      batch;
    Ok ()
  in
  let* () =
    List.fold_left
      (fun result batch -> Result.bind result (fun () -> query batch))
      (Ok ())
      (chunks 4096 (List.sort_uniq compare addresses))
  in
  Ok (fun address -> Option.join (Hashtbl.find_opt table address))

let display_names llvm =
  List.filter_map
    (fun line ->
      match String.split_on_char '"' line with
      | prefix :: display :: sep :: linkage :: _
        when String.ends_with ~suffix:"DISubprogram(name: " prefix
             && String.ends_with ~suffix:"linkageName: " sep ->
          Some (linkage, display)
      | _ -> None)
    (lines llvm)

let display_symbol names symbol =
  match List.assoc_opt symbol names with
  | Some display -> display
  | None -> Option.value ~default:symbol (C_import.adapter_c_name symbol)

let display_text names text =
  String.split_on_char ' ' text
  |> List.map (fun word ->
      let n = String.length word in
      if n > 2 && word.[0] = '<' && word.[n - 1] = '>' then
        "<" ^ display_symbol names (String.sub word 1 (n - 2)) ^ ">"
      else display_symbol names word)
  |> String.concat " "

let display_place names (p : place) =
  let shown = display_symbol names in
  {
    p with
    func = shown p.func;
    caller = Option.map (fun (f, n) -> (shown f, n)) p.caller;
    host = shown p.host;
  }

let symbolize names exe addresses =
  let* frame = frames_of exe addresses in
  Ok (fun addr -> Option.map (display_place names) (Option.bind (frame addr) locate))

type remark = {
  pass : string;
  name : string;
  file : string;
  line : int;
  host : string;
  args : (string * string) list;
}

let unquote text =
  let n = String.length text in
  if n < 2 || text.[0] <> '\'' || text.[n - 1] <> '\'' then text
  else
    let rec collapse = function
      | '\'' :: '\'' :: rest -> '\'' :: collapse rest
      | c :: rest -> c :: collapse rest
      | [] -> []
    in
    let body = List.of_seq (String.to_seq (String.sub text 1 (n - 2))) in
    String.of_seq (List.to_seq (collapse body))

let pair text =
  Option.map (fun (a, b) -> (String.trim a, String.trim b)) (split_at ':' text)

let location text =
  let flow = String.map (fun c -> if c = '{' || c = '}' then ' ' else c) text in
  let fields = List.filter_map pair (String.split_on_char ',' flow) in
  match
    ( List.assoc_opt "File" fields,
      Option.bind (List.assoc_opt "Line" fields) int_of_string_opt )
  with
  | Some file, Some line -> Some (Filename.basename file, line)
  | _ -> None

let parse_remark lines =
  let field key =
    List.find_map
      (fun l ->
        if String.starts_with ~prefix:(key ^ ":") l then Option.map snd (pair l)
        else None)
      lines
  in
  let arg l =
    Option.map
      (fun (key, value) -> (key, unquote value))
      (Option.bind (drop_prefix "  - " l) pair)
  in
  match (field "Pass", field "Name", Option.bind (field "DebugLoc") location) with
  | Some pass, Some name, Some (file, line) ->
      Some
        {
          pass;
          name;
          file;
          line;
          host = unquote (Option.value ~default:"" (field "Function"));
          args = List.filter_map arg lines;
        }
  | _ -> None

let parse_remarks text =
  let docs, last =
    List.fold_left
      (fun (docs, current) line ->
        if String.starts_with ~prefix:"--- " line then (List.rev current :: docs, [])
        else (docs, line :: current))
      ([], []) (lines text)
  in
  List.filter_map parse_remark (List.rev (List.rev last :: docs))

let read_remarks path = Option.fold ~none:[] ~some:parse_remarks (read_file path)
let message (r : remark) = String.concat "" (List.map snd r.args)
let int_arg (r : remark) key = Option.bind (List.assoc_opt key r.args) int_of_string_opt

type timing = { per_iter : float; limit : string option }

let scan line format build =
  try Some (Scanf.sscanf line format build)
  with Scanf.Scan_failure _ | End_of_file | Failure _ -> None

let mca_timing text =
  let lines = List.map String.trim (lines text) in
  let first format = List.find_map (fun line -> scan line format Fun.id) lines in
  let num format = Option.value ~default:0. (first format) in
  let busiest =
    List.fold_left
      (fun best (n, p) -> if Some p > Option.map snd best then Some (n, p) else best)
      None
      (List.filter_map (fun line -> scan line "- %s [ %f%%" (fun n p -> (n, p))) lines)
  in
  match (first "Iterations: %d", first "Total Cycles: %d") with
  | Some iterations, Some total when iterations > 0 ->
      let bound =
        if
          first "Resource Pressure [ %f%%" = None
          && first "Data Dependencies: [ %f%%" = None
        then None
        else if num "Resource Pressure [ %f%%" >= num "Data Dependencies: [ %f%%" then
          Option.map fst busiest
        else if
          num "- Register Dependencies [ %f%%" >= num "- Memory Dependencies [ %f%%"
        then Some "register dependency"
        else Some "memory dependency"
      in
      Some { per_iter = float_of_int total /. float_of_int iterations; limit = bound }
  | _ -> None

let mca () = Driver.tool "FAS_MCA" "llvm-mca-22"
let mca_available () = Result.is_ok (run_env [| mca (); "--version" |])

let mca_source (loop : loop) =
  String.concat "\n" ((loop.header ^ ":") :: List.map snd (instructions loop)) ^ "\n"

let estimate (loop : loop) =
  match Filename.temp_file "fas-mca-" ".s" with
  | exception Sys_error _ -> None
  | path ->
      Fun.protect
        ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
        (fun () ->
          Out_channel.with_open_bin path (fun channel ->
              output_string channel (mca_source loop));
          match
            run_env
              [|
                mca (); "-mcpu=native"; "-iterations=100"; "-bottleneck-analysis"; path;
              |]
          with
          | Ok (text, _) -> mca_timing text
          | Error _ -> None)

let named names op =
  List.exists
    (fun name ->
      op = name
      || String.length op = String.length name + 1
         && String.starts_with ~prefix:name op
         && String.contains "bwlq" op.[String.length op - 1])
    names

let split_operands text =
  let depth = ref 0 in
  let pieces, last =
    String.fold_left
      (fun (pieces, current) c ->
        match c with
        | ',' when !depth = 0 -> (current :: pieces, "")
        | _ ->
            if c = '(' then incr depth else if c = ')' then decr depth;
            (pieces, current ^ String.make 1 c))
      ([], "") text
  in
  List.rev (last :: pieces)

let pure_write op =
  List.exists
    (fun prefix -> String.starts_with ~prefix op)
    [ "mov"; "vmov"; "set"; "stos"; "fst"; "fist" ]

let rec memory_access text =
  let op, arg =
    match split_at ' ' text with
    | Some (op, arg) -> (op, String.trim arg)
    | None -> (text, "")
  in
  let memory piece = String.contains piece '(' in
  if op = "lock" then memory_access arg
  else if
    named [ "lea"; "push"; "pop"; "call"; "ret" ] op
    || String.starts_with ~prefix:"nop" op
    || String.starts_with ~prefix:"prefetch" op
  then (false, false)
  else
    match List.rev (if arg = "" then [] else split_operands arg) with
    | [] -> (false, false)
    | [ only ] ->
        if not (memory only) then (false, false)
        else if pure_write op then (false, true)
        else if named [ "mul"; "imul"; "div"; "idiv"; "jmp" ] op then (true, false)
        else (true, true)
    | dst :: sources ->
        let source_memory = List.exists memory sources in
        if named [ "xchg" ] op && List.exists memory (dst :: sources) then (true, true)
        else if memory dst && named [ "cmp"; "test"; "bt" ] op then (true, false)
        else if memory dst then (source_memory || not (pure_write op), true)
        else (source_memory, false)

type sample = {
  time : float;
  event : string;
  period : int;
  symbol : (string * int) option;
  dso : string;
}

let cycles_event = "cycles:u"
let events = "cycles:u,instructions:u,branches:u,branch-misses:u,cache-misses:u"

let split_word text =
  match split_at ' ' text with
  | Some (word, rest) -> (word, String.trim rest)
  | None -> (text, "")

let symbol_offset text =
  match String.rindex_opt text '+' with
  | Some i ->
      Option.map
        (fun offset -> (String.sub text 0 i, offset))
        (int_of_string_opt (String.sub text (i + 1) (String.length text - i - 1)))
  | None -> None

let trim_colon text =
  if String.ends_with ~suffix:":" text then String.sub text 0 (String.length text - 1)
  else text

let parse_sample line =
  let stamp, rest = split_word (String.trim line) in
  let period, rest = split_word rest in
  let event, rest = split_word rest in
  let _ip, rest = split_word rest in
  let symbol, dso = split_word rest in
  let unparen text =
    if String.length text >= 2 then String.sub text 1 (String.length text - 2) else text
  in
  match
    ( float_of_string_opt (trim_colon stamp),
      int_of_string_opt period,
      String.ends_with ~suffix:":" event )
  with
  | Some time, Some period, true ->
      Some
        {
          time;
          event = trim_colon event;
          period;
          symbol = symbol_offset symbol;
          dso = unparen dso;
        }
  | _ -> None

let samples text = List.filter_map parse_sample (lines text)

let record ~perf ~data exe args =
  let recording =
    [ "record"; "-q"; "-N"; "-e"; events; "-F"; "4000"; "-o"; data; "--"; exe ]
  in
  let argv = debuginfod_free (Array.of_list ((perf :: recording) @ args)) in
  let started =
    match Unix.access (Driver.executable_path perf) [ Unix.X_OK ] with
    | () -> Process.run_program "env" argv
    | exception Unix.Unix_error (code, _, _) -> Error (Unix.error_message code)
  in
  match started with
  | Error message -> Error (Printf.sprintf "cannot run %s: %s" perf message)
  | Ok status -> (
      let none =
        Printf.sprintf "perf recorded no cycle samples (exit status %d)" status
      in
      let script =
        [| perf; "script"; "-i"; data; "-F"; "time,event,period,ip,sym,symoff,dso" |]
      in
      if not (Sys.file_exists data) then Error none
      else
        match run_env script with
        | Error failure ->
            let detail = String.trim failure.Process.stderr in
            Error (if detail = "" then "perf script failed" else detail)
        | Ok (text, _) ->
            if List.exists (fun s -> s.event = cycles_event) (samples text) then
              Ok (status, text)
            else Error none)
