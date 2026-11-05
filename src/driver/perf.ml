open Perf_tools

type row = { place : place; loop : loop }
type target = { other : row list; other_notes : remark list }

type program = {
  input : string;
  exe : string;
  names : (string * string) list;
  notes : remark list;
  rows : row list;
  starts : (string * int) list;
  target : target option;
}

type view = Summary | Asm of { copies : row list; copy : int option }

let addresses loop = List.map fst (instructions loop)

let row_of names snap frame (loop : loop) =
  let placed (address, text) =
    Option.bind (frame address) (fun frames ->
        match locate frames with
        | Some place when place.line > 0 ->
            let key = (place.func, call_chain (List.tl (List.tl frames))) in
            Some (address, text, List.length frames / 2, key, place)
        | _ -> None)
  in
  let considered = List.filter_map placed (instructions loop) in
  let depth = List.fold_left (fun m (_, _, d, _, _) -> min m d) max_int considered in
  match List.filter (fun (_, _, d, _, _) -> d = depth) considered with
  | [] -> None
  | (_, _, _, key, first) :: _ as shallow ->
      let place =
        match List.find_opt (fun (a, _, _, _, _) -> a = loop.latch) shallow with
        | Some (_, _, _, _, p) -> p
        | None ->
            let same = List.filter (fun (_, _, _, k, _) -> k = key) shallow in
            let rank (_, t, _, _, (p : place)) =
              (not (String.starts_with ~prefix:"j" t), p.line)
            in
            { first with line = snd (List.hd (List.sort compare (List.map rank same))) }
      in
      Some { place = display_place names (snap place); loop }

let bodies = function
  | Ast.If (_, body, other, _) -> body :: Option.to_list other
  | Ast.While (_, _, body, _)
  | Ast.For (_, _, _, _, body, _)
  | Ast.Block (body, _)
  | Ast.Defer (body, _) ->
      [ body ]
  | Ast.Switch (_, arms, default, _) -> List.map snd arms @ Option.to_list default
  | _ -> []

let rec last_line stmts =
  match List.rev stmts with
  | [] -> 0
  | last :: _ ->
      List.fold_left
        (fun m b -> max m (last_line b))
        (Ast.stmt_span last).Span.line (bodies last)

let rec loop_extents stmts =
  List.concat_map
    (fun stmt ->
      let inner = List.concat_map loop_extents (bodies stmt) in
      match stmt with
      | Ast.While (_, _, body, span) | Ast.For (_, _, _, _, body, span) ->
          (span.Span.line, last_line body) :: inner
      | _ -> inner)
    stmts

let file_loops path =
  match
    Option.map
      (fun text -> Parser.parse (Source.create ~file:path ~text))
      (read_file path)
  with
  | Some (Ok program) ->
      List.concat_map
        (function Ast.Func { body = Ast.Statements s; _ } -> loop_extents s | _ -> [])
        program.Ast.items
  | _ -> []

let snap memo ~input (p : place) =
  if not (Filename.check_suffix p.file ".fas") then p
  else
    let path = Filename.concat (Filename.dirname input) p.file in
    if not (Hashtbl.mem memo path) then Hashtbl.add memo path (file_loops path);
    match
      List.filter (fun (s, e) -> s <= p.line && p.line <= e) (Hashtbl.find memo path)
    with
    | [] -> p
    | found -> { p with line = List.fold_left (fun m (s, _) -> max m s) 0 found }

let located ~input names exe =
  let* disassembly = disassemble exe in
  let loops = List.concat_map loops_of (routines disassembly) in
  let* frame = frames_of exe (List.concat_map addresses loops) in
  let memo = Hashtbl.create 4 in
  let rows = List.filter_map (row_of names (snap memo ~input) frame) loops in
  let key (r : row) = (r.place.file, r.place.line, r.loop.latch) in
  Ok (List.sort (fun a b -> compare (key a) (key b)) rows, symbol_starts disassembly)

let on_line file line (r : row) =
  r.place.line = line && Filename.basename r.place.file = Filename.basename file

let select asm copy rows =
  match asm with
  | None -> Ok Summary
  | Some (file, line) -> (
      match List.filter (on_line file line) rows with
      | [] ->
          let loops =
            List.map
              (fun (file, line) -> Printf.sprintf "%s:%d" file line)
              (List.sort_uniq compare
                 (List.map
                    (fun (r : row) -> (Filename.basename r.place.file, r.place.line))
                    rows))
          in
          Error
            (Printf.sprintf "fas perf: no loop at %s:%d; loops: %s" file line
               (if loops = [] then "none" else String.concat ", " loops))
      | copies -> (
          let count = List.length copies in
          match copy with
          | Some n when n > count ->
              Error
                (Printf.sprintf "fas perf: %s:%d has %d copies; use @1..@%d" file line
                   count count)
          | _ -> Ok (Asm { copies; copy })))

let remarks_for names notes (row : row) =
  List.filter
    (fun (r : remark) ->
      r.line = row.place.line
      && r.file = Filename.basename row.place.file
      && display_symbol names r.host = row.place.host)
    notes

type vector = Vectorized of int * int | Missed of string

let vector_fact (r : remark) =
  if r.pass <> "loop-vectorize" then None
  else if r.name = "Vectorized" then
    match (int_arg r "VectorizationFactor", int_arg r "InterleaveCount") with
    | Some factor, Some interleave -> Some (Vectorized (factor, interleave))
    | _ -> None
  else
    Option.map
      (fun reason -> Missed reason)
      (drop_prefix "loop not vectorized: " (message r))

let wins remarks =
  List.sort_uniq compare
    (List.filter_map
       (fun r ->
         match vector_fact r with Some (Vectorized (f, i)) -> Some (f, i) | _ -> None)
       remarks)

let vectorization mine =
  let reason =
    List.find_map
      (fun r -> match vector_fact r with Some (Missed r) -> Some r | _ -> None)
      mine
  in
  match (wins mine, reason) with
  | [ win ], _ -> (Some win, reason)
  | [], Some reason -> (None, Some reason)
  | _ -> (None, None)

let unroll_fact (r : remark) =
  match (r.pass, r.name, List.assoc_opt "UnrollCount" r.args) with
  | "loop-unroll", "FullyUnrolled", _ -> Some "fully unrolled"
  | "loop-unroll", "PartialUnrolled", Some n -> Some ("unrolled x" ^ n)
  | _ -> None

let unroll_count mine =
  let factors =
    List.filter_map
      (fun (r : remark) ->
        match (r.pass, r.name) with
        | "loop-unroll", "PartialUnrolled" -> Some (int_arg r "UnrollCount")
        | "loop-unroll", "FullyUnrolled" -> Some None
        | _ -> None)
      mine
  in
  match List.sort_uniq compare factors with
  | [] -> Some 1
  | [ Some n ] -> Some n
  | _ -> None

type shape = { elements : int; vector : (int * int) option }

let shape_of names notes (row : row) =
  let mine = remarks_for names notes row in
  match (vectorization mine, unroll_count mine) with
  | (Some (factor, interleave), None), Some unroll ->
      Some
        { elements = factor * interleave * unroll; vector = Some (factor, interleave) }
  | (None, Some _), Some unroll -> Some { elements = unroll; vector = None }
  | _ -> None

type native =
  | Differs of { vector : (int * int) option }
  | Predicted of { cycles : float option; here : shape; there : shape }

let copies_on (rows : row list) (r : row) =
  List.sort
    (fun (a : row) (b : row) -> compare a.loop.latch b.loop.latch)
    (List.filter (on_line r.place.file r.place.line) rows)

let earlier (r : row) rows =
  List.length (List.filter (fun (x : row) -> x.loop.latch < r.loop.latch) rows)

let native_win ~portable ~native =
  match native with [ win ] when not (List.mem win portable) -> Some win | _ -> None

let native_of p target (r : row) =
  let mine = copies_on p.rows r and theirs = copies_on target.other r in
  let wins notes rows = wins (List.concat_map (remarks_for p.names notes) rows) in
  if List.length mine <> List.length theirs then
    Some
      (Differs
         {
           vector =
             native_win ~portable:(wins p.notes mine)
               ~native:(wins target.other_notes theirs);
         })
  else
    let twin = List.nth theirs (earlier r mine) in
    match (shape_of p.names p.notes r, shape_of p.names target.other_notes twin) with
    | Some here, Some there ->
        let cycles =
          if r.loop.timeable && twin.loop.timeable then
            Option.map (fun (t : timing) -> t.per_iter) (estimate twin.loop)
          else None
        in
        Some (Predicted { cycles; here; there })
    | _ -> (
        match
          native_win ~portable:(wins p.notes mine)
            ~native:(wins target.other_notes theirs)
        with
        | Some _ as vector -> Some (Differs { vector })
        | None -> None)

let because text =
  let key = " because " in
  let n = String.length key in
  let rec find i =
    if i + n > String.length text then text
    else if String.sub text i n = key then
      String.sub text (i + n) (String.length text - i - n)
    else find (i + 1)
  in
  find 0

let kept_call remarks callee =
  List.find_map
    (fun (r : remark) ->
      if
        r.pass = "inline" && r.name <> "Inlined"
        && List.assoc_opt "Callee" r.args = Some callee
      then Some (because (message r))
      else None)
    remarks

type kept = { callee : string; reason : string; count : int }

type facts = {
  instrs : int;
  loads : int;
  stores : int;
  spill : (int * int) option;
  vectorized : (int * int) option;
  not_vectorized : string option;
  unroll : string option;
  cycles_per_iter : float option;
  native : native option;
  bound : string option;
  calls : (string * int) list;
  calls_kept : kept list;
  trap : bool;
}

let facts names remarks timing native (row : row) =
  let mine = remarks_for names remarks row in
  let vectorized, not_vectorized = vectorization mine in
  let spill =
    match
      List.filter
        (fun (r : remark) -> r.pass = "regalloc" && r.name = "LoopSpillReloadCopies")
        mine
    with
    | [ r ] -> (
        match (int_arg r "NumSpills", int_arg r "NumReloads") with
        | Some spills, Some reloads when spills > 0 || reloads > 0 ->
            Some (spills, reloads)
        | _ -> None)
    | _ -> None
  in
  let access = List.map (fun (_, text) -> memory_access text) (instructions row.loop) in
  let count pick = List.length (List.filter pick access) in
  let callees = List.sort_uniq compare row.loop.calls in
  let calls_kept, calls =
    List.partition_map
      (fun callee ->
        let n = List.length (List.filter (( = ) callee) row.loop.calls) in
        let shown = display_symbol names callee in
        match kept_call remarks callee with
        | Some reason -> Either.Left { callee = shown; reason; count = n }
        | None -> Either.Right (shown, n))
      callees
  in
  {
    instrs = row.loop.instrs;
    loads = count fst;
    stores = count snd;
    spill;
    vectorized;
    not_vectorized;
    unroll = List.find_map unroll_fact mine;
    cycles_per_iter = Option.map (fun (t : timing) -> t.per_iter) timing;
    native;
    bound = Option.bind timing (fun (t : timing) -> t.limit);
    calls;
    calls_kept;
    trap = row.loop.trap;
  }

type tally = { hits : int; total : int }
type counts = { branch : tally; branch_miss : tally; cache_miss : tally; instr : tally }

let no_tally = { hits = 0; total = 0 }

let no_counts =
  { branch = no_tally; branch_miss = no_tally; cache_miss = no_tally; instr = no_tally }

let add_tally a b = { hits = a.hits + b.hits; total = a.total + b.total }

let add_counts a b =
  {
    branch = add_tally a.branch b.branch;
    branch_miss = add_tally a.branch_miss b.branch_miss;
    cache_miss = add_tally a.cache_miss b.cache_miss;
    instr = add_tally a.instr b.instr;
  }

let add_sample c (s : sample) =
  let one = { hits = 1; total = s.period } in
  match s.event with
  | "branches:u" -> { c with branch = add_tally c.branch one }
  | "branch-misses:u" -> { c with branch_miss = add_tally c.branch_miss one }
  | "cache-misses:u" -> { c with cache_miss = add_tally c.cache_miss one }
  | "instructions:u" -> { c with instr = add_tally c.instr one }
  | _ -> c

let enough (t : tally) = t.hits >= 20

let branch_rate c =
  if enough c.branch && enough c.branch_miss then
    Some (float_of_int c.branch_miss.total /. float_of_int c.branch.total)
  else None

let cache_per_kilo c =
  if enough c.cache_miss && enough c.instr then
    Some (1000. *. float_of_int c.cache_miss.total /. float_of_int c.instr.total)
  else None

type kind = Loop | Function | Outside | Rest

type code =
  | Label_line of string
  | Insn of { share : float option; text : string; where : string option }

type entry = {
  kind : kind;
  weight : int;
  place : place option;
  source : string option;
  symbol : string option;
  dso : string option;
  places : int option;
  facts : facts option;
  code : code list;
  copies : entry list;
  copy_of : (int * int) option;
  counts : counts option;
}

type run = {
  seconds : float;
  cycles : int;
  instructions : int;
  branches : int;
  branch_misses : int;
  cache_misses : int;
  samples : int;
  status : int;
}

type report = { title : string; run : run option; entries : entry list }

let blank kind weight =
  {
    kind;
    weight;
    place = None;
    source = None;
    symbol = None;
    dso = None;
    places = None;
    facts = None;
    code = [];
    copies = [];
    copy_of = None;
    counts = None;
  }

let rec merge_runs same merge = function
  | [] -> []
  | x :: rest -> (
      match List.partition (same x) rest with
      | [], _ -> x :: merge_runs same merge rest
      | others_same, others -> merge (x :: others_same) :: merge_runs same merge others)

let group entries =
  let key (e : entry) =
    Option.map (fun (p : place) -> (Filename.basename p.file, p.line)) e.place
  in
  let merge all =
    let counts = List.filter_map (fun (e : entry) -> e.counts) all in
    {
      (List.hd all) with
      weight = List.fold_left (fun acc (f : entry) -> acc + f.weight) 0 all;
      copies = all;
      copy_of = None;
      counts =
        (if counts = [] then None else Some (List.fold_left add_counts no_counts counts));
    }
  in
  merge_runs
    (fun (e : entry) (f : entry) -> e.kind = Loop && f.kind = Loop && key f = key e)
    merge entries

let place_entry p kind ~weight (place : place) =
  let path =
    if Filename.check_suffix place.file ".fas" then
      Filename.concat (Filename.dirname p.input) place.file
    else place.file
  in
  let source =
    match read_file path with
    | Some text when place.line >= 1 ->
        Option.map String.trim (List.nth_opt (lines text) (place.line - 1))
    | _ -> None
  in
  { (blank kind weight) with place = Some place; source }

let loop_entry p ?(weight = 0) ?counts ~among (row : row) =
  let timing = if row.loop.timeable then estimate row.loop else None in
  let native = Option.bind p.target (fun target -> native_of p target row) in
  let copies = copies_on among row in
  {
    (place_entry p Loop ~weight row.place) with
    facts = Some (facts p.names p.notes timing native row);
    copy_of =
      (if List.length copies > 1 then Some (earlier row copies + 1, List.length copies)
       else None);
    counts;
  }

let code_lines p ~where ~shares (row : row) =
  let loop = row.loop in
  let share =
    match shares with
    | None -> fun _ -> None
    | Some table ->
        let inside =
          Hashtbl.fold
            (fun address n acc ->
              if loop.start <= address && address < loop.stop then acc + n else acc)
            table 0
        in
        fun address ->
          if inside = 0 then None
          else
            Option.map
              (fun n -> 100. *. float_of_int n /. float_of_int inside)
              (Hashtbl.find_opt table address)
  in
  let at (q : place) =
    if q.line >= 1 then Some (Printf.sprintf "%s:%d" (Filename.basename q.file) q.line)
    else None
  in
  List.map
    (function
      | Label name -> Label_line (display_symbol p.names name)
      | Instr (address, text) ->
          Insn
            {
              share = share address;
              text = display_text p.names text;
              where = Option.bind (where address) at;
            })
    loop.body

let report p run entries = { title = Filename.basename p.input; run; entries }

let static_report p view =
  match view with
  | Summary -> Ok (report p None (group (List.map (loop_entry p ~among:p.rows) p.rows)))
  | Asm { copies; copy } ->
      let row = List.nth copies (Option.fold ~none:0 ~some:pred copy) in
      let* where = symbolize p.names p.exe (addresses row.loop) in
      let entry = loop_entry p ~among:copies row in
      Ok (report p None [ { entry with code = code_lines p ~where ~shares:None row } ])

let file_address p (sample : sample) =
  match sample.symbol with
  | Some (name, offset) when Filename.basename sample.dso = Filename.basename p.exe ->
      Option.map (fun start -> (name, start + offset)) (List.assoc_opt name p.starts)
  | _ -> None

let innermost (rows : row array) address =
  let width i = rows.(i).loop.stop - rows.(i).loop.start in
  List.fold_left
    (fun best i ->
      let loop = rows.(i).loop in
      if loop.start <= address && address < loop.stop then
        match best with Some j when width j <= width i -> best | _ -> Some i
      else best)
    None
    (List.init (Array.length rows) Fun.id)

type spot = Elsewhere | In_code of string * int | In_loop of int * int

let totals pairs =
  let table = Hashtbl.create 16 in
  List.iter
    (fun (key, n) ->
      Hashtbl.replace table key
        (n + Option.value ~default:0 (Hashtbl.find_opt table key)))
    pairs;
  table

let run_of ~status samples =
  let sum event =
    List.fold_left
      (fun acc s -> if s.event = event then acc + s.period else acc)
      0 samples
  in
  let times = List.map (fun s -> s.time) samples in
  {
    seconds =
      List.fold_left Float.max neg_infinity times
      -. List.fold_left Float.min infinity times;
    cycles = sum cycles_event;
    instructions = sum "instructions:u";
    branches = sum "branches:u";
    branch_misses = sum "branch-misses:u";
    cache_misses = sum "cache-misses:u";
    samples = List.length (List.filter (fun s -> s.event = cycles_event) samples);
    status;
  }

type candidate = { score : int; key : string * int; entry : entry Lazy.t }

let merge_candidates candidates =
  merge_runs
    (fun c d -> d.key = c.key)
    (fun same ->
      let ordered = List.stable_sort (fun a b -> compare b.score a.score) same in
      {
        (List.hd same) with
        score = List.fold_left (fun acc d -> acc + d.score) 0 ordered;
        entry = lazy (List.hd (group (List.map (fun d -> Lazy.force d.entry) ordered)));
      })
    candidates

let functions placed =
  List.fold_left
    (fun acc ((name, (p : place)), weight) ->
      match List.assoc_opt name acc with
      | None -> (name, (weight, p, weight)) :: acc
      | Some (sum, best, best_weight) ->
          let best, best_weight =
            if weight > best_weight then (p, weight) else (best, best_weight)
          in
          (name, (sum + weight, best, best_weight)) :: List.remove_assoc name acc)
    [] placed

let summary p ~top ~total ~place ~weight_of ~counts_of cycles =
  let unplaced, placed =
    List.partition_map
      (fun ((s : sample), name, address) ->
        match place address with
        | Some where -> Either.Right ((name, where), s.period)
        | None -> Either.Left s)
      (List.rev
         (List.filter_map
            (function
              | s, In_code (name, address) -> Some (s, name, address) | _ -> None)
            cycles))
  in
  let outside =
    totals
      (List.map
         (fun (s : sample) ->
           ((Option.map fst s.symbol, Filename.basename s.dso), s.period))
         (List.filter_map (function s, Elsewhere -> Some s | _ -> None) cycles
         @ unplaced))
  in
  let loops =
    List.filter_map
      (fun (row : row) ->
        let weight = weight_of row in
        if weight = 0 then None
        else
          Some
            {
              score = weight;
              key = (row.place.file, row.place.line);
              entry =
                lazy (loop_entry p ~weight ~counts:(counts_of row) ~among:p.rows row);
            })
      p.rows
  in
  let functions =
    List.map
      (fun (_, (weight, (place : place), _)) ->
        {
          score = weight;
          key = (place.file, place.line);
          entry = lazy (place_entry p Function ~weight place);
        })
      (functions (List.sort compare (Hashtbl.to_seq (totals placed) |> List.of_seq)))
  in
  let outside =
    Hashtbl.fold
      (fun (raw, dso) weight acc ->
        let symbol = Option.map (display_symbol p.names) raw in
        let shown = Option.value ~default:"?" symbol in
        {
          score = weight;
          key = (Printf.sprintf "outside Fas: %s (%s)" shown dso, 0);
          entry = lazy { (blank Outside weight) with symbol; dso = Some dso };
        }
        :: acc)
      outside []
  in
  let percent weight = 100. *. float_of_int weight /. float_of_int total in
  let ranked =
    List.stable_sort
      (fun a b -> compare (b.score, a.key) (a.score, b.key))
      (merge_candidates loops @ functions @ outside)
  in
  let shown =
    List.filteri
      (fun i _ -> i < top)
      (List.filter (fun c -> percent c.score >= 2.) ranked)
  in
  let rest = List.filter (fun c -> not (List.memq c shown)) ranked in
  let rest_weight = List.fold_left (fun acc c -> acc + c.score) 0 rest in
  List.map (fun c -> Lazy.force c.entry) shown
  @
  if rest = [] || Float.round (percent rest_weight) < 1. then []
  else [ { (blank Rest rest_weight) with places = Some (List.length rest) } ]

let build p view ~top ~status ~place ~where samples =
  let rows = Array.of_list p.rows in
  let spot s =
    match file_address p s with
    | None -> Elsewhere
    | Some (name, address) -> (
        match innermost rows address with
        | Some i -> In_loop (rows.(i).loop.latch, address)
        | None -> In_code (name, address))
  in
  let spotted = List.map (fun s -> (s, spot s)) samples in
  let cycles = List.filter (fun ((s : sample), _) -> s.event = cycles_event) spotted in
  let loop_weight =
    totals
      (List.filter_map
         (function
           | (s : sample), In_loop (latch, _) -> Some (latch, s.period) | _ -> None)
         cycles)
  in
  let weight_of (row : row) =
    Option.value ~default:0 (Hashtbl.find_opt loop_weight row.loop.latch)
  in
  let counts_of (row : row) =
    List.fold_left
      (fun counts ((s : sample), spot) ->
        match spot with
        | In_loop (latch, _) when latch = row.loop.latch -> add_sample counts s
        | _ -> counts)
      no_counts spotted
  in
  let run = run_of ~status samples in
  match view with
  | Asm { copies; copy } ->
      let _, _, heaviest =
        List.fold_left max (-1, 0, 0)
          (List.mapi (fun i row -> (weight_of row, -i, i)) copies)
      in
      let row = List.nth copies (Option.fold ~none:heaviest ~some:pred copy) in
      let shares =
        totals
          (List.filter_map
             (function
               | (s : sample), (In_loop (_, address) | In_code (_, address)) ->
                   Some (address, s.period)
               | _ -> None)
             cycles)
      in
      let entry =
        loop_entry p ~weight:(weight_of row) ~counts:(counts_of row) ~among:copies row
      in
      report p (Some run)
        [ { entry with code = code_lines p ~where ~shares:(Some shares) row } ]
  | Summary ->
      report p (Some run)
        (summary p ~top ~total:run.cycles ~place ~weight_of ~counts_of cycles)

let profile p view ~top ~status text =
  let samples = samples text in
  let rows = Array.of_list p.rows in
  let in_code address = innermost rows address = None in
  let unplaced, copies =
    match view with
    | Asm { copies; _ } -> ([], copies)
    | Summary ->
        ( List.sort_uniq compare
            (List.filter_map
               (fun s ->
                 match file_address p s with
                 | Some (_, address) when s.event = cycles_event && in_code address ->
                     Some address
                 | _ -> None)
               samples),
          [] )
  in
  let* place = symbolize p.names p.exe unplaced in
  let* where =
    symbolize p.names p.exe (List.concat_map (fun (r : row) -> addresses r.loop) copies)
  in
  Ok (build p view ~top ~status ~place ~where samples)
