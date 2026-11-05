let ( let* ) = Result.bind

type candidate = {
  name : string;
  choice : Driver.choice;
  counts : string;
  input : string;
  options : string list;
}

type outcome = {
  head : int;
  challenger : int;
  comparison : Tune_decide.comparison;
  held : Tune_decide.comparison option;
  selection : Tune_decide.selection;
  data : int -> Tune_decide.side array;
  select : int -> Tune_decide.side array;
}

type placement = { layout : int option; functions : int option; offset : int option }

type run = {
  wall : float;
  cycles : float option;
  status : string;
  stdout : string;
  stderr : string;
}

type session = {
  runner : string;
  arena : string;
  cpu : int;
  dir : string;
  log : string option;
  argv0 : string;
  args : string list;
}

exception Stop of int
exception Interrupted of int

let rounds = 7
let layouts = [ None; Some 32; Some 64 ]
let function_alignments = [ None; Some 64 ]
let offsets = [ 0; 65536; 589824 ]

let layout_pairs =
  List.concat_map
    (fun layout -> List.map (fun functions -> (layout, functions)) function_alignments)
    layouts

let placements_for pairs no_arena =
  List.concat_map
    (fun (layout, functions) ->
      if no_arena then [ { layout; functions; offset = None } ]
      else List.map (fun offset -> { layout; functions; offset = Some offset }) offsets)
    pairs

let placements no_arena =
  placements_for (List.map (fun layout -> (layout, None)) layouts) no_arena

let layout_placements no_arena = placements_for layout_pairs no_arena

let placement_name p =
  Driver.align_loops_name p.layout
  ^ (match p.functions with Some n -> "+f" ^ string_of_int n | None -> "")
  ^ match p.offset with Some n -> "+" ^ string_of_int n | None -> ""

let read path = In_channel.with_open_bin path In_channel.input_all

let test_values variable parse =
  match Sys.getenv_opt variable with
  | None -> []
  | Some text ->
      String.split_on_char ',' text
      |> List.filter_map (fun item ->
          match String.rindex_opt item '=' with
          | Some i ->
              let name = String.sub item 0 i
              and value = String.sub item (i + 1) (String.length item - i - 1) in
              Option.map (fun value -> (name, value)) (parse value)
          | None -> None)

let text_limit = 64 * 1024 * 1024

let elf_field ic pos width =
  seek_in ic pos;
  let raw = really_input_string ic width in
  let value = ref 0 in
  for i = width - 1 downto 0 do
    value := (!value lsl 8) lor Char.code raw.[i]
  done;
  !value

let text_bytes path =
  try
    In_channel.with_open_bin path (fun ic ->
        if really_input_string ic 6 <> "\127ELF\002\001" then None
        else
          let shoff = elf_field ic 0x28 8
          and shentsize = elf_field ic 0x3A 2
          and shnum = elf_field ic 0x3C 2
          and shstrndx = elf_field ic 0x3E 2 in
          let strings = shoff + (shstrndx * shentsize) in
          let names_at = elf_field ic (strings + 0x18) 8
          and names_size = elf_field ic (strings + 0x20) 8 in
          seek_in ic names_at;
          let names = really_input_string ic names_size in
          let name_at offset =
            let stop = String.index_from names offset '\000' in
            String.sub names offset (stop - offset)
          in
          let rec find index =
            if index >= shnum then None
            else
              let section = shoff + (index * shentsize) in
              if name_at (elf_field ic section 4) = ".text" then
                Some (elf_field ic (section + 0x20) 8)
              else find (index + 1)
          in
          find 0)
  with Sys_error _ | End_of_file | Invalid_argument _ | Failure _ | Not_found -> None

let rec remove_tree path =
  match Sys.is_directory path with
  | true ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Sys.rmdir path
  | false -> Sys.remove path
  | exception Sys_error _ -> ()

let mkdir path = if not (Sys.file_exists path) then Sys.mkdir path 0o700

let parse_cpu_list text =
  String.split_on_char ',' (String.trim text)
  |> List.concat_map (fun part ->
      match String.split_on_char '-' part with
      | [ a ] -> Option.to_list (int_of_string_opt a)
      | [ a; b ] -> (
          match (int_of_string_opt a, int_of_string_opt b) with
          | Some a, Some b when a <= b -> List.init (b - a + 1) (fun i -> a + i)
          | _ -> [])
      | _ -> [])

let allowed_cpus () =
  match
    read "/proc/self/status" |> String.split_on_char '\n'
    |> List.find_map (fun line ->
        if String.starts_with ~prefix:"Cpus_allowed_list:" line then
          Some (String.sub line 18 (String.length line - 18))
        else None)
  with
  | Some list -> parse_cpu_list list
  | None | (exception Sys_error _) -> []

let cpu_times () =
  match read "/proc/stat" with
  | exception Sys_error _ -> []
  | text ->
      String.split_on_char '\n' text
      |> List.filter_map (fun line ->
          match String.split_on_char ' ' line |> List.filter (( <> ) "") with
          | name :: fields
            when String.length name > 3 && String.starts_with ~prefix:"cpu" name -> (
              match
                ( int_of_string_opt (String.sub name 3 (String.length name - 3)),
                  List.map int_of_string_opt fields )
              with
              | Some cpu, values when List.for_all Option.is_some values ->
                  let values = List.map Option.get values in
                  let total = List.fold_left ( + ) 0 values in
                  let idle =
                    match values with
                    | _ :: _ :: _ :: idle :: iowait :: _ -> idle + iowait
                    | _ -> 0
                  in
                  Some (cpu, (total, idle))
              | _ -> None)
          | _ -> None)

let siblings cpu =
  match
    read
      (Printf.sprintf "/sys/devices/system/cpu/cpu%d/topology/thread_siblings_list" cpu)
  with
  | text -> ( match parse_cpu_list text with [] -> [ cpu ] | list -> list)
  | exception Sys_error _ -> [ cpu ]

let choose_cpu () =
  let allowed = allowed_cpus () in
  let isolated =
    match read "/sys/devices/system/cpu/isolated" with
    | text -> parse_cpu_list text
    | exception Sys_error _ -> []
  in
  match List.find_opt (fun cpu -> List.mem cpu allowed) isolated with
  | Some cpu -> cpu
  | None -> (
      let before = cpu_times () in
      Unix.sleepf 0.2;
      let after = cpu_times () in
      let busy cpu =
        match (List.assoc_opt cpu before, List.assoc_opt cpu after) with
        | Some (t0, i0), Some (t1, i1) -> t1 - t0 - (i1 - i0)
        | _ -> max_int / 4
      in
      let cores =
        List.filter
          (fun cpu ->
            let group = siblings cpu in
            List.hd group = cpu && List.for_all (fun c -> List.mem c allowed) group)
          allowed
      in
      let score cpu = List.fold_left (fun acc c -> acc + busy c) 0 (siblings cpu) in
      match List.sort (fun a b -> compare (score a, a) (score b, b)) cores with
      | cpu :: _ -> cpu
      | [] -> ( match allowed with cpu :: _ -> cpu | [] -> -1))

let log_line session text =
  match session.log with
  | None -> ()
  | Some path ->
      Out_channel.with_open_gen [ Open_append; Open_creat ] 0o644 path (fun channel ->
          output_string channel (text ^ "\n"))

let signals = [ (Sys.sigint, 130); (Sys.sigterm, 143); (Sys.sighup, 129) ]
let run_counter = ref 0
let child = ref None

let execute session ?(env = Unix.environment ()) ?(arena = true) ~label placement
    program =
  incr run_counter;
  let out = Filename.concat session.dir (Printf.sprintf "run-%d" !run_counter) in
  mkdir out;
  let arena_path, offset =
    match (arena, placement) with
    | true, Some { offset = Some offset; _ } -> (session.arena, string_of_int offset)
    | _ -> ("-", "0")
  in
  log_line session label;
  let argv =
    Array.of_list
      ([
         session.runner;
         string_of_int session.cpu;
         arena_path;
         offset;
         out;
         program;
         "--";
         session.argv0;
       ]
      @ session.args)
  in
  let failed message =
    prerr_endline ("fas: cannot run the tune runner: " ^ message);
    raise (Stop 1)
  in
  (match
     Fun.protect
       ~finally:(fun () ->
         ignore (Unix.sigprocmask Unix.SIG_UNBLOCK (List.map fst signals)))
       (fun () ->
         ignore (Unix.sigprocmask Unix.SIG_BLOCK (List.map fst signals));
         let pid =
           Unix.create_process_env session.runner argv env Unix.stdin Unix.stdout
             Unix.stderr
         in
         child := Some pid;
         pid)
   with
  | exception Unix.Unix_error (code, _, _) -> failed (Unix.error_message code)
  | pid -> (
      let rec wait () =
        match Unix.waitpid [] pid with
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
        | _, status -> status
      in
      let status = wait () in
      child := None;
      match status with Unix.WEXITED 0 -> () | _ -> failed "it did not finish"));
  let result = read (Filename.concat out "result") in
  let fields = String.split_on_char ' ' (String.trim result) in
  let run =
    match fields with
    | [ wall; cycles; _; status; _ ] ->
        {
          wall = float_of_string wall;
          cycles = float_of_string_opt cycles;
          status;
          stdout = read (Filename.concat out "stdout");
          stderr = read (Filename.concat out "stderr");
        }
    | _ -> failed ("malformed result " ^ result)
  in
  remove_tree out;
  run

let same a b = a.status = b.status && a.stdout = b.stdout && a.stderr = b.stderr

let status_message = function
  | "0" -> None
  | status when String.starts_with ~prefix:"sig:" status ->
      Some
        (Printf.sprintf "error: the program was killed by signal %s; no profile written"
           (String.sub status 4 (String.length status - 4)))
  | status ->
      Some
        (Printf.sprintf "error: the program exited with status %s; no profile written"
           status)

let build ~report (config : Cli.t) ?profile ?(instrument = false)
    ?(target = config.target) ~input output =
  Build_limit.with_cap (fun () ->
      match
        Driver.run ~instrument
          {
            config with
            Cli.input;
            output;
            output_explicit = true;
            target_explicit = false;
            profile;
            target;
            emit = Cli.Executable;
          }
      with
      | Ok _ -> ()
      | Error diagnostics ->
          report diagnostics;
          raise (Stop 1))

let train ~report session config ~target name =
  let dir = Filename.concat session.dir ("train-" ^ name) in
  mkdir dir;
  let program = Filename.concat dir "program" in
  build ~report config ~instrument:true ~target ~input:config.Cli.input program;
  let env =
    Array.of_list
      (("LLVM_PROFILE_FILE=" ^ Filename.concat dir "fas-%p-%m.profraw")
      :: (Array.to_list (Unix.environment ())
         |> List.filter (fun entry ->
             not (String.starts_with ~prefix:"LLVM_PROFILE_FILE=" entry))))
  in
  let run = execute session ~env ~arena:false ~label:("train " ^ name) None program in
  (match status_message run.status with
  | Some message ->
      prerr_string run.stderr;
      prerr_endline message;
      raise (Stop 1)
  | None -> ());
  let profraw =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".profraw")
    |> List.sort String.compare
    |> List.map (Filename.concat dir)
  in
  if profraw = [] then (
    prerr_endline
      "error: the program wrote no profile (it must return from `main` or call `exit`)";
    raise (Stop 1));
  match Driver.merge_profraw ~scratch:dir ~profraw with
  | Ok counts -> (counts, run.wall)
  | Error diagnostics ->
      report diagnostics;
      raise (Stop 1)

let align_functions_name = function None -> "def" | Some n -> string_of_int n

let profile_text ~header ~(candidate : candidate) ~functions ~layout ~measured =
  String.concat "\n"
    (header
    @ [
        "choice " ^ Driver.choice_name candidate.choice;
        "align-loops " ^ Driver.align_loops_name layout;
        "align-functions " ^ align_functions_name functions;
        String.concat " " ("options" :: candidate.options);
        "measured " ^ measured;
        "counts";
      ])
  ^ "\n" ^ candidate.counts

let write ~report output text =
  match Driver.write_profile_file output text with
  | Ok () -> ()
  | Error diagnostics ->
      report diagnostics;
      raise (Stop 1)

let percent band = Printf.sprintf "%.1f%%" (band *. 100.)

let second_input_text (held : Tune_decide.comparison list) =
  match held with
  | [] -> ""
  | _ ->
      let geomean =
        List.fold_left
          (fun acc (h : Tune_decide.comparison) -> acc *. h.geomean)
          1. held
      in
      let band =
        List.fold_left (fun acc (h : Tune_decide.comparison) -> acc +. h.band) 0. held
      in
      let passes = List.for_all (fun (h : Tune_decide.comparison) -> h.passes) held in
      Printf.sprintf "; second input %.3f×, band %s%s" geomean (percent band)
        (if passes then "" else ", slower at some placement")

let test_root () = Option.value (Sys.getenv_opt "FAS_TUNE_TEST_ROOT") ~default:""
let state_path () = test_root () ^ "/run/fas-tune-isolate"
let interactive () = Unix.isatty Unix.stdin && Unix.isatty Unix.stderr

let ask question ~default =
  prerr_string (question ^ " ");
  flush stderr;
  match In_channel.input_line stdin with
  | None ->
      prerr_newline ();
      false
  | Some answer -> (
      match String.lowercase_ascii (String.trim answer) with
      | "" -> default
      | "y" | "yes" -> true
      | _ -> false)

let cpu_list cpus = String.concat "," (List.map string_of_int cpus)

let sudo_argv mode cpu =
  [|
    "sudo";
    "--";
    "sh";
    "-c";
    Tune_tools.isolate_source;
    mode;
    string_of_int cpu;
    string_of_int (Unix.getpid ());
    test_root ();
  |]

let rec wait_for pid =
  match Unix.waitpid [] pid with
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_for pid
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> Unix.WEXITED 255
  | _, status -> status

let script_line prefix line =
  match line with
  | Some line when String.starts_with ~prefix:(prefix ^ " ") line ->
      Some
        (String.sub line
           (String.length prefix + 1)
           (String.length line - String.length prefix - 1))
  | _ -> None

type held = { pid : int; input : Unix.file_descr; output : in_channel }

let held = ref None

let start_sudo mode cpu =
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let out_read, out_write = Unix.pipe ~cloexec:true () in
  match
    Unix.create_process "sudo" (sudo_argv mode cpu) stdin_read out_write Unix.stderr
  with
  | exception Unix.Unix_error (code, _, _) ->
      List.iter Unix.close [ stdin_read; stdin_write; out_read; out_write ];
      Error (Unix.error_message code)
  | pid ->
      Unix.close stdin_read;
      Unix.close out_write;
      Ok { pid; input = stdin_write; output = Unix.in_channel_of_descr out_read }

let finish_sudo process =
  (try Unix.close process.input with Unix.Unix_error _ -> ());
  let rec last line =
    match In_channel.input_line process.output with
    | Some next -> last (Some next)
    | None | (exception Sys_error _) -> line
  in
  let line = last None in
  In_channel.close process.output;
  (line, wait_for process.pid)

let isolate cpu =
  match start_sudo "hold" cpu with
  | Error message ->
      Printf.eprintf "fas perf: cannot run sudo (%s); measuring without isolation\n%!"
        message
  | Ok process -> (
      match script_line "isolated" (In_channel.input_line process.output) with
      | Some cpus ->
          held := Some process;
          Printf.eprintf "fas perf: isolated CPUs %s for the measurement\n%!" cpus
      | None ->
          ignore (finish_sudo process);
          prerr_endline
            "fas perf: the CPU was not isolated; measuring without isolation")

let release () =
  match !held with
  | None -> ()
  | Some process -> (
      held := None;
      match finish_sudo process with
      | line, Unix.WEXITED 0 when Option.is_some (script_line "released" line) ->
          Printf.eprintf "fas perf: restored CPUs %s\n%!"
            (Option.get (script_line "released" line))
      | _ ->
          Printf.eprintf
            "fas perf: restoring the isolated CPU failed; `%s` lists the saved values\n\
             %!"
            (state_path ()))

let stale_state () =
  match In_channel.with_open_bin (state_path ()) In_channel.input_line with
  | exception Sys_error _ -> None
  | None -> Some (None, None)
  | Some line -> (
      match String.split_on_char ' ' line with
      | [ "#"; "cpu"; cpu; "pid"; pid ] ->
          Some (int_of_string_opt cpu, int_of_string_opt pid)
      | _ -> Some (None, None))

let alive pid = pid > 0 && Sys.file_exists ("/proc/" ^ string_of_int pid)

let restore_stale ~prompts =
  match stale_state () with
  | None -> true
  | Some (_, Some pid) when alive pid && pid <> Unix.getpid () ->
      Printf.eprintf
        "fas perf: another tune session (process %d) holds an isolated CPU; measuring \
         without isolation\n\
         %!"
        pid;
      false
  | Some (None, _) ->
      Printf.eprintf
        "fas perf: `%s` is not a tune state file; measuring without isolation\n%!"
        (state_path ());
      false
  | Some (Some cpu, _) ->
      Printf.eprintf "fas perf: an earlier tune session left CPU %d isolated (%s)\n%!"
        cpu (state_path ());
      if prompts && ask "restore it now with sudo? [Y/n]" ~default:true then (
        match start_sudo "off" cpu with
        | Ok process -> (
            match finish_sudo process with
            | line, Unix.WEXITED 0 when Option.is_some (script_line "released" line) ->
                Printf.eprintf "fas perf: restored CPUs %s\n%!"
                  (Option.get (script_line "released" line));
                true
            | _ ->
                prerr_endline "fas perf: restoring failed; measuring without isolation";
                false)
        | Error message ->
            Printf.eprintf "fas perf: cannot run sudo (%s)\n%!" message;
            false)
      else (
        if not prompts then
          prerr_endline
            "fas perf: run `fas perf --tune` on a terminal without `--yes` to restore \
             it";
        false)

let duration seconds =
  if seconds < 1. then "under a second"
  else if seconds < 90. then Printf.sprintf "about %.0f s" seconds
  else if seconds < 5400. then Printf.sprintf "about %.0f min" (seconds /. 60.)
  else
    let minutes = Float.to_int (Float.round (seconds /. 60.)) in
    Printf.sprintf "about %d h %d min" (minutes / 60) (minutes mod 60)

let announce ~input ~cpu ~trained ~candidates ~placements ~layout_placements ~second
    ~wall =
  let selection = candidates * placements * (1 + rounds)
  and confirmation = 2 * placements * rounds in
  let held = if second then 2 + (candidates * placements) + confirmation else 0 in
  let option_stage ~fresh ~measured =
    (fresh * placements)
    + (measured * placements * rounds)
    + confirmation
    + if second then (fresh * placements) + confirmation else 0
  in
  let stage1 = List.length Driver.tune_option_names in
  let kinds = List.length Driver.tune_option_kinds in
  let later = List.init (kinds - 1) (fun i -> kinds - 1 - i) in
  let options =
    option_stage ~fresh:stage1 ~measured:(stage1 + 1)
    + List.fold_left
        (fun total c -> total + option_stage ~fresh:c ~measured:(c + 1))
        0 later
  in
  let layout_runs = layout_placements * rounds in
  let runs = trained + 2 + selection + confirmation + held + options + layout_runs in
  let builds = candidates * List.length layouts in
  let option_builds = (stage1 + List.fold_left ( + ) 0 later) * List.length layouts in
  let others = List.filter (( <> ) cpu) (siblings cpu) in
  Printf.eprintf "fas perf: tuning %s for this machine\n" input;
  Printf.eprintf
    "  runs: %d training, 2 reference, %d selection (%d builds x %d placements x %d), \
     %d confirmation (2 x %d x %d); %d builds\n"
    trained selection candidates placements (1 + rounds) confirmation placements rounds
    builds;
  if second then
    Printf.eprintf
      "  second input: 2 reference, %d checks (%d builds x %d placements), %d \
       confirmation (2 x %d x %d)\n"
      (candidates * placements) candidates placements confirmation placements rounds;
  Printf.eprintf "  options: up to %d more runs (all option stages, up to %d builds)\n"
    options option_builds;
  Printf.eprintf
    "  layout: %d runs (%d placements x %d), %d builds with align-functions 64\n"
    layout_runs layout_placements rounds (List.length layouts);
  Printf.eprintf
    "  time: %s for the runs, plus the builds (one training run took %.3f s)\n"
    (duration (wall *. float_of_int runs *. 1.1 /. 1e9))
    (wall /. 1e9);
  Printf.eprintf "  CPU %d%s\n" cpu
    (match others with
    | [] -> ""
    | others -> Printf.sprintf " (its sibling %s gets no work)" (cpu_list others));
  prerr_endline
    "  Leave the machine alone until it finishes. The program run after `--` should do \
     the\n\
    \  work it really does and finish quickly (well under a second is ideal).";
  flush stderr

let offer cpu =
  let cpus = cpu_list (siblings cpu) in
  let others = List.filter (( <> ) cpu) (siblings cpu) in
  Printf.eprintf
    "For more accurate results, fas can isolate CPU %d%s for the\n\
     measurement. This needs sudo and is used only to:\n\
    \  - keep other processes off CPUs %s (runtime cpuset of system.slice, init.scope \
     and\n\
    \    session scopes; CPU affinity of your own processes)\n\
    \  - move interrupts and kernel workqueues off CPUs %s\n\
    \  - set the performance governor and disable idle states deeper than 1 us on CPUs \
     %s\n\
    \  - turn off the NMI watchdog\n\
     Every change is undone when tuning ends, also on Ctrl-C.\n"
    cpu
    (match others with
    | [] -> ""
    | others -> Printf.sprintf " (and its sibling %s)" (cpu_list others))
    cpus cpus cpus;
  ask "use sudo? [y/N]" ~default:false

let tune ~report (config : Cli.t) ~output ~no_arena ~yes ~check_with ~may_isolate ~dir
    ~argv0 args =
  let cc = Driver.tool "FAS_CC" "clang-22" in
  let tools = Filename.concat dir "tools" in
  mkdir tools;
  let runner, arena =
    match Tune_tools.build ~cc tools with
    | Ok tools -> tools
    | Error message ->
        prerr_endline ("fas: " ^ message);
        raise (Stop 1)
  in
  let session =
    {
      runner;
      arena;
      cpu = choose_cpu ();
      dir;
      log = Sys.getenv_opt "FAS_TUNE_LOG";
      argv0;
      args;
    }
  in
  let header = Driver.tune_header () in
  let native_only = config.target = Cli.Force_native in
  let first_counts, wall =
    train ~report session config ~target:config.target
      (if native_only then "native" else "portable")
  in
  if interactive () then (
    announce ~input:config.input ~cpu:session.cpu
      ~trained:(if native_only then 1 else 2)
      ~candidates:(if native_only then 2 else 6)
      ~placements:(List.length (placements no_arena))
      ~layout_placements:(List.length (layout_placements no_arena))
      ~second:(Option.is_some check_with) ~wall;
    if not yes then (
      if not (ask "continue? [Y/n]" ~default:true) then (
        prerr_endline "fas perf: stopped; no profile written";
        raise (Stop 0));
      if may_isolate && session.cpu >= 0 && offer session.cpu then isolate session.cpu));
  let native_counts =
    if native_only then first_counts
    else fst (train ~report session config ~target:Cli.Force_native "native")
  in
  let test_input = Sys.getenv_opt "FAS_TUNE_TEST_CANDIDATE" in
  let test_option_input = Sys.getenv_opt "FAS_TUNE_TEST_OPTION_CANDIDATE" in
  let scales = test_values "FAS_TUNE_TEST_SCALE" float_of_string_opt in
  let build_seconds = test_values "FAS_TUNE_TEST_BUILD_SECONDS" float_of_string_opt in
  let text_hook = test_values "FAS_TUNE_TEST_TEXT_BYTES" int_of_string_opt in
  let failing_build = Sys.getenv_opt "FAS_TUNE_TEST_FAIL_BUILD" in
  let baseline_index = 0 in
  let base_seconds = ref 0. in
  let skipped = ref [] in
  let pool = ref [||] in
  let get index : candidate = !pool.(index) in
  let builds = Filename.concat dir "builds" in
  mkdir builds;
  let build_name (c : candidate) functions layout =
    c.name ^ "-"
    ^ Driver.align_loops_name layout
    ^ match functions with Some n -> "-f" ^ string_of_int n | None -> ""
  in
  let program_with (c : candidate) functions layout =
    Filename.concat builds (build_name c functions layout)
  in
  let program c layout = program_with c None layout in
  let unmeasured = "none" in
  let build_program (c : candidate) functions layout =
    let profile = program_with c functions layout ^ ".fasprof" in
    write ~report profile
      (profile_text ~header ~candidate:c ~functions ~layout ~measured:unmeasured);
    build ~report config ~profile ~target:Cli.Portable ~input:c.input
      (program_with c functions layout)
  in
  let build_candidate (c : candidate) =
    if failing_build = Some c.name then (
      if c.options = [] then Printf.eprintf "error: build of %s failed\n%!" c.name;
      raise (Stop 1));
    List.iter (fun layout -> build_program c None layout) layouts
  in
  let add ~name ~choice ~counts ~options =
    let index = Array.length !pool in
    let input =
      match (test_input, test_option_input) with
      | Some input, _ when index = 1 -> input
      | _, Some input when options <> [] -> input
      | _ -> config.input
    in
    let c = { name; choice; counts; input; options } in
    let skip reason =
      skipped := options @ !skipped;
      Printf.eprintf "fas perf: options: %s skipped (%s)\n%!" name reason;
      None
    in
    let keep () =
      pool := Array.append !pool [| c |];
      Some index
    in
    let started = Unix.gettimeofday () in
    let built =
      if options = [] then (
        build_candidate c;
        true)
      else
        try
          build_candidate c;
          true
        with Stop _ | Failure _ | Sys_error _ | Out_of_memory | Unix.Unix_error _ ->
          false
    in
    let measured = Unix.gettimeofday () -. started in
    if not built then skip "build failed"
    else
      let seconds =
        Option.value ~default:measured (List.assoc_opt name build_seconds)
      in
      if index = baseline_index then base_seconds := seconds;
      if options = [] then keep ()
      else if seconds > Float.max (10. *. !base_seconds) 30. then
        skip (Printf.sprintf "build took %.0fs, base %.0fs" seconds !base_seconds)
      else
        let text =
          match List.assoc_opt name text_hook with
          | Some bytes -> bytes
          | None ->
              List.fold_left
                (fun acc layout ->
                  max acc (Option.value ~default:0 (text_bytes (program c layout))))
                0 layouts
        in
        if text > text_limit then skip (Printf.sprintf ".text %d MiB" (text / 1048576))
        else keep ()
  in
  let choices =
    if native_only then
      [
        ("default", Driver.Native, native_counts);
        ("pgo", Driver.Native_pgo, native_counts);
      ]
    else
      [
        ("default", Driver.Default, first_counts);
        ("pgo", Driver.Pgo, first_counts);
        ("native", Driver.Native, first_counts);
        ("native-pgo", Driver.Native_pgo, native_counts);
        ("mtune", Driver.Mtune, first_counts);
        ("mtune-pgo", Driver.Mtune_pgo, first_counts);
      ]
  in
  let all =
    List.filter_map
      (fun (name, choice, counts) -> add ~name ~choice ~counts ~options:[])
      choices
  in
  let placements = Array.of_list (placements no_arena) in
  let measured_layouts = Array.of_list (layout_placements no_arena) in
  let second = Option.map (fun args -> { session with args }) check_with in
  let suffix = if Option.is_none second then " (measured on one input)" else "" in
  let default_profile reason =
    prerr_endline ("fas perf: " ^ reason ^ "; the profile keeps the default build");
    write ~report output
      (profile_text ~header ~candidate:(get baseline_index) ~functions:None ~layout:None
         ~measured:unmeasured);
    Printf.eprintf "fas perf: wrote %s\n%!" output;
    raise (Stop 0)
  in
  let baseline = get baseline_index in
  let unstable input =
    "the program's output differs between two runs of the same build"
    ^ match input with `First -> "" | `Second -> " on the second input"
  in
  let reference input session prefix =
    let run () =
      execute session ~arena:false
        ~label:(prefix ^ " " ^ baseline.name)
        None (program baseline None)
    in
    let first = run () in
    let second = run () in
    if not (same first second) then default_profile (unstable input);
    first
  in
  let first_reference = reference `First session "reference" in
  let second_reference =
    Option.map (fun session -> reference `Second session "reference-with") second
  in
  let check input index p run =
    let expected =
      match input with
      | `First -> first_reference
      | `Second -> Option.get second_reference
    in
    if not (same expected run) then
      if index = baseline_index then
        match p.offset with
        | Some offset ->
            prerr_endline
              (Printf.sprintf
                 "error: the program behaves differently under the measuring allocator \
                  (arena offset %d%s); rerun with --no-arena"
                 offset
                 (match input with `First -> "" | `Second -> ", second input"));
            raise (Stop 1)
        | None -> default_profile (unstable input)
      else (
        prerr_endline
          (Printf.sprintf
             "internal error: a tuned build changed the program's output (%s, %s%s); \
              kept builds in %s"
             (get index).name
             (match p.offset with
             | Some n -> "arena offset " ^ string_of_int n
             | None -> "no arena")
             (match input with `First -> "" | `Second -> ", second input")
             builds);
        raise (Stop 2))
  in
  let metric run = match run.cycles with Some c when c > 0. -> c | _ -> run.wall in
  let scale_for input name =
    let key = match input with `First -> name | `Second -> name ^ "@with" in
    match List.assoc_opt key scales with
    | Some factor -> factor
    | None -> Option.value ~default:1. (List.assoc_opt name scales)
  in
  let wall_metric = ref false in
  let scale_of input (c : candidate) p =
    scale_for input c.name *. scale_for input (build_name c p.functions p.layout)
  in
  let measure_once input session step index p =
    let c = get index in
    let run =
      execute session
        ~label:(Printf.sprintf "%s %s %s" step c.name (placement_name p))
        (Some p)
        (program_with c p.functions p.layout)
    in
    check input index p run;
    (match run.cycles with Some c when c > 0. -> () | _ -> wall_metric := true);
    metric run *. scale_of input c p
  in
  let measure_in pls input session step indices =
    let pairs =
      Array.of_list
        (List.concat_map
           (fun index -> List.init (Array.length pls) (fun p -> (index, p)))
           indices)
    in
    let samples = Hashtbl.create 64 in
    for round = 0 to rounds - 1 do
      let n = Array.length pairs in
      for k = 0 to n - 1 do
        let index, p = pairs.((round + k) mod n) in
        let value = measure_once input session step index pls.(p) in
        Hashtbl.replace samples (index, p)
          (value :: Option.value ~default:[] (Hashtbl.find_opt samples (index, p)))
      done
    done;
    fun index ->
      Array.init (Array.length pls) (fun p ->
          Tune_decide.summarize (Array.of_list (Hashtbl.find samples (index, p))))
  in
  let measure input session step indices =
    measure_in placements input session step indices
  in
  let check_all input session step indices =
    List.iter
      (fun index ->
        Array.iter
          (fun p -> ignore (measure_once input session step index p))
          placements)
      indices
  in
  let stage indices fresh =
    check_all `First session "check" fresh;
    Option.iter (fun session -> check_all `Second session "check-with" fresh) second;
    let head = List.hd indices and challengers = List.tl indices in
    let select_data = measure `First session "select" indices in
    let selection =
      Tune_decide.select ~rounds ~baseline:(select_data head)
        ~candidates:(Array.of_list (List.map select_data challengers))
    in
    let make index comparison held selection data =
      {
        head;
        challenger = index;
        comparison;
        held;
        selection;
        data;
        select = select_data;
      }
    in
    match selection with
    | Tune_decide.Selected (i, _) ->
        let index = List.nth challengers i in
        let confirmation = measure `First session "confirm" [ head; index ] in
        let comparison =
          Tune_decide.confirm ~rounds ~baseline:(confirmation head)
            ~candidate:(confirmation index)
        in
        let held =
          Option.map
            (fun session ->
              let data = measure `Second session "confirm-with" [ head; index ] in
              Tune_decide.confirm ~rounds ~baseline:(data head) ~candidate:(data index))
            second
        in
        Some (make index comparison held selection confirmation)
    | Tune_decide.Rejected best ->
        Option.map
          (fun (i, comparison) ->
            make (List.nth challengers i) comparison None selection select_data)
          best
  in
  let kept_by_selection (o : outcome) =
    Tune_decide.kept ?held:o.held o.selection o.comparison
  in
  let winner (result : outcome option) =
    match result with Some o when kept_by_selection o -> Some o | _ -> None
  in
  let choice = stage all all in
  let base =
    match winner choice with Some o -> o.challenger | None -> baseline_index
  in
  let derive options =
    let b = get base in
    let options =
      List.filter (fun name -> List.mem name options) Driver.tune_option_names
    in
    add
      ~name:(String.concat "+" (b.name :: options))
      ~choice:b.choice ~counts:b.counts ~options
  in
  let same_binary a b =
    List.for_all
      (fun layout -> read (program (get a) layout) = read (program (get b) layout))
      layouts
  in
  let distinct_builds head candidates =
    List.rev
      (List.fold_left
         (fun kept index ->
           match
             List.find_opt (fun other -> same_binary other index) (head :: kept)
           with
           | Some other ->
               Printf.eprintf
                 "fas perf: options: %s builds the same binary as %s; not measured\n%!"
                 (get index).name (get other).name;
               kept
           | None -> index :: kept)
         [] candidates)
  in
  let option_round head kept =
    match kept with [] -> None | _ -> stage (head :: kept) kept
  in
  let fresh =
    List.filter_map (fun option -> derive [ option ]) Driver.tune_option_names
  in
  let kept1 = distinct_builds base fresh in
  let option_stage1 = option_round base kept1 in
  let kind_name (first : outcome) (kind, names) =
    match names with
    | [ name ] -> if List.mem name !skipped then None else Some name
    | _ ->
        let baseline = first.select base in
        List.fold_left
          (fun best index ->
            match (get index).options with
            | [ name ] when Driver.option_kind name = Some kind -> (
                let geomean =
                  (Tune_decide.compare ~tests:(Array.length placements) ~rounds
                     ~baseline ~candidate:(first.select index))
                    .geomean
                in
                match best with
                | Some (_, best_geomean) when best_geomean >= geomean -> best
                | _ -> Some (name, geomean))
            | _ -> best)
          None kept1
        |> Option.map fst
  in
  let next_names set first =
    List.filter_map
      (fun (kind, names) ->
        if List.exists (fun name -> Driver.option_kind name = Some kind) set then None
        else kind_name first (kind, names))
      Driver.tune_option_kinds
  in
  let rec option_stages first (previous : outcome) =
    if not (kept_by_selection previous) then []
    else
      let set = (get previous.challenger).options in
      let candidates =
        List.filter_map (fun name -> derive (set @ [ name ])) (next_names set first)
      in
      match
        option_round previous.challenger
          (distinct_builds previous.challenger candidates)
      with
      | None -> []
      | Some o -> o :: option_stages first o
  in
  let option_runs =
    match option_stage1 with
    | None -> []
    | Some first -> first :: option_stages first first
  in
  if !wall_metric then
    prerr_endline "fas perf: cycle counters are not available; measured wall time";
  let metric_name = if !wall_metric then "wall" else "cycles" in
  Printf.eprintf "fas perf: measured on CPU %d, %d rounds\n" session.cpu rounds;
  let by_group (data : Tune_decide.side array) =
    Array.of_list
      (List.map
         (fun pair ->
           Array.of_list
             (List.filter_map Fun.id
                (List.mapi
                   (fun p (placement : placement) ->
                     if (placement.layout, placement.functions) = pair then
                       Some data.(p)
                     else None)
                   (Array.to_list measured_layouts))))
         layout_pairs)
  in
  let table (o : outcome) =
    let before = o.data o.head and after = o.data o.challenger in
    Printf.eprintf "%-14s %14s %14s %8s\n" "placement" (get o.head).name
      (get o.challenger).name "ratio";
    Array.iteri
      (fun p placement ->
        Printf.eprintf "%-14s %14.0f %14.0f %8.3f\n" (placement_name placement)
          before.(p).median after.(p).median o.comparison.ratios.(p))
      placements
  in
  let option_line ~more (o : outcome) =
    let held = second_input_text (Option.to_list o.held) in
    if kept_by_selection o then
      Printf.eprintf "fas perf: kept options %s (%.3f×, band %s%s)%s\n%!"
        (String.concat " " (get o.challenger).options)
        o.comparison.geomean (percent o.comparison.band) held suffix
    else
      let added =
        List.filter
          (fun name -> not (List.mem name (get o.head).options))
          (get o.challenger).options
      in
      Printf.eprintf "fas perf: kept no %soptions (best %s %.3f×, band %s%s)%s\n%!"
        (if more then "more " else "")
        (String.concat " " added) o.comparison.geomean (percent o.comparison.band) held
        suffix
  in
  Option.iter table choice;
  List.iteri
    (fun i (o : outcome) ->
      table o;
      option_line ~more:(i > 0) o)
    option_runs;
  let kept_stages =
    List.filter_map winner (choice :: List.map Option.some option_runs)
  in
  let final = match List.rev kept_stages with last :: _ -> Some last | [] -> None in
  let ratio =
    List.fold_left (fun acc (o : outcome) -> acc *. o.comparison.geomean) 1. kept_stages
  in
  let band =
    match (kept_stages, choice) with
    | [], Some o -> o.comparison.band
    | [], None -> 0.
    | _ ->
        List.fold_left
          (fun acc (o : outcome) -> acc +. o.comparison.band)
          0. kept_stages
  in
  let chosen_index =
    match final with Some o -> o.challenger | None -> baseline_index
  in
  let chosen = get chosen_index in
  List.iter (fun layout -> build_program chosen (Some 64) layout) layouts;
  let layout_data =
    measure_in measured_layouts `First session "layout" [ chosen_index ]
  in
  let groups = by_group (layout_data chosen_index) in
  let fallback =
    match choice with
    | None -> None
    | Some (o : outcome) ->
        let kept = match winner choice with Some w -> w.challenger | None -> o.head in
        let data = o.select kept in
        let loop_groups =
          Array.of_list
            (List.map
               (fun layout ->
                 Array.of_list
                   (List.filter_map Fun.id
                      (List.mapi
                         (fun p (placement : placement) ->
                           if placement.layout = layout then Some data.(p) else None)
                         (Array.to_list placements))))
               layouts)
        in
        Option.map
          (fun i -> List.nth layouts i)
          (Tune_decide.best_layout ~rounds ~layouts:loop_groups)
  in
  let picked =
    match Tune_decide.best_layout ~rounds ~layouts:groups with
    | Some i -> i
    | None -> (
        match fallback with
        | Some layout ->
            Option.value ~default:0
              (List.find_index (fun pair -> pair = (layout, None)) layout_pairs)
        | None -> 0)
  in
  let layout, functions = List.nth layout_pairs picked in
  let layout_comparison =
    Tune_decide.compare
      ~tests:((Array.length groups - 1) * Array.length groups.(0))
      ~rounds ~baseline:groups.(0) ~candidate:groups.(picked)
  in
  let loops_text =
    match layout with
    | Some n -> [ Printf.sprintf "loops aligned to %d" n ]
    | None -> []
  in
  let functions_text =
    match functions with
    | Some n -> [ Printf.sprintf "functions aligned to %d" n ]
    | None -> []
  in
  let layout_parts =
    String.concat "" (List.map (fun text -> ", " ^ text) (loops_text @ functions_text))
  in
  let second_text =
    match final with
    | Some _ ->
        second_input_text (List.filter_map (fun (o : outcome) -> o.held) kept_stages)
    | None ->
        second_input_text
          (Option.to_list (Option.bind choice (fun (o : outcome) -> o.held)))
  in
  (match (final, choice) with
  | Some _, _ ->
      let options =
        match chosen.options with
        | [] -> ""
        | opts -> ", options " ^ String.concat " " opts
      in
      Printf.eprintf "fas perf: kept the profile (%s %.3f×, band %s%s%s%s)%s\n"
        (get base).name ratio (percent band) layout_parts options second_text suffix
  | None, _ when (layout, functions) <> (None, None) ->
      Printf.eprintf "fas perf: kept the profile (%s %.3f×, band %s%s%s)%s\n"
        (get base).name layout_comparison.geomean
        (percent layout_comparison.band)
        layout_parts second_text suffix
  | None, Some o ->
      Printf.eprintf "fas perf: kept the default build (best %s %.3f×, band %s%s)%s\n"
        (get o.challenger).name o.comparison.geomean (percent o.comparison.band)
        second_text suffix
  | None, None -> Printf.eprintf "fas perf: kept the default build%s\n" suffix);
  (match chosen.choice with
  | Driver.Native | Driver.Native_pgo ->
      Printf.eprintf "fas perf: tuned for %s; the binary may not run on other CPUs\n"
        (Driver.llc_host_cpu ())
  | Driver.Default | Driver.Pgo | Driver.Mtune | Driver.Mtune_pgo -> ());
  let measured =
    Printf.sprintf "%.4f band %.4f rounds %d placements %d metric %s" ratio band rounds
      (Array.length placements) metric_name
  in
  write ~report output
    (profile_text ~header ~candidate:chosen ~functions ~layout ~measured);
  Printf.eprintf "fas perf: wrote %s (%d functions)\n%!" output
    (Driver.count_profile_functions chosen.counts);
  0

let last_reason message =
  match List.rev (String.split_on_char ':' message) with
  | last :: _ -> String.trim last
  | [] -> message

let output_problem output =
  if Sys.file_exists output && Sys.is_directory output then Some "is a directory"
  else
    match
      Filename.temp_file ~temp_dir:(Filename.dirname output) "fas-perf-" ".probe"
    with
    | exception Sys_error message -> Some (last_reason message)
    | probe ->
        (try Sys.remove probe with Sys_error _ -> ());
        None

let run ~report (config : Cli.t) ~output ~no_arena ~yes ~check_with ~argv0 args =
  match Driver.check_clang () with
  | Error diagnostics ->
      report diagnostics;
      1
  | Ok () -> (
      match output_problem output with
      | Some reason ->
          prerr_endline
            (Printf.sprintf "fas perf: cannot write `%s` (%s)" output reason);
          1
      | None -> (
          match Sys.getenv_opt "FAS_TUNE_TEST_TOOLS" with
          | Some dir -> (
              match Tune_tools.build ~cc:(Driver.tool "FAS_CC" "clang-22") dir with
              | Ok _ -> 0
              | Error message ->
                  prerr_endline ("fas: " ^ message);
                  1)
          | None -> (
              match Filename.temp_dir "fas-tune-" "" with
              | exception Sys_error message ->
                  prerr_endline ("fas: " ^ message);
                  1
              | dir ->
                  let previous =
                    List.map
                      (fun (signal, code) ->
                        ( signal,
                          Sys.signal signal
                            (Sys.Signal_handle (fun _ -> raise (Interrupted code))) ))
                      signals
                  in
                  let status =
                    try
                      let may_isolate =
                        if Sys.file_exists (state_path ()) then
                          restore_stale ~prompts:(interactive () && not yes)
                        else true
                      in
                      tune ~report config ~output ~no_arena ~yes ~check_with
                        ~may_isolate ~dir ~argv0 args
                    with
                    | Stop status -> status
                    | Interrupted code | Fun.Finally_raised (Interrupted code) ->
                        List.iter
                          (fun (signal, _) -> Sys.set_signal signal Sys.Signal_ignore)
                          signals;
                        Option.iter
                          (fun pid ->
                            try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
                          !child;
                        Option.iter (fun pid -> ignore (wait_for pid)) !child;
                        child := None;
                        prerr_endline "fas perf: interrupted; no profile written";
                        code
                  in
                  List.iter
                    (fun (signal, _) -> Sys.set_signal signal Sys.Signal_ignore)
                    signals;
                  release ();
                  List.iter
                    (fun (signal, behavior) -> Sys.set_signal signal behavior)
                    previous;
                  if not (config.keep || status = 2) then remove_tree dir;
                  if status = 2 then 1 else status)))
