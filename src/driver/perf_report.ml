open Perf_tools
open Perf

let arrow = " \u{2192} "
let times n = if n > 1 then Printf.sprintf " ×%d" n else ""
let percent total weight = 100. *. float_of_int weight /. float_of_int total

let distinct items =
  List.rev
    (List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] items)

let code_points s =
  String.fold_left (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1) 0 s

let take s k =
  let rec go i seen =
    if i >= String.length s then i
    else if Char.code s.[i] land 0xC0 = 0x80 then go (i + 1) seen
    else if seen = k then i
    else go (i + 1) (seen + 1)
  in
  String.sub s 0 (go 0 0)

let clip s = if code_points s <= 40 then s else take s 39 ^ "…"
let pad s width = s ^ String.make (max 1 (width - code_points s)) ' '

let header (report : report) =
  match report.run with
  | None -> Printf.sprintf "%s  static (no run)" report.title
  | Some run ->
      let ratio scale fmt a b =
        if b = 0 then "n/a"
        else Printf.sprintf fmt (scale *. float_of_int a /. float_of_int b)
      in
      Printf.sprintf
        "%s  %.2f s  %.2f G cycles  IPC %s  branch miss %s  cache miss %s  %d \
         samples%s%s"
        report.title run.seconds
        (float_of_int run.cycles /. 1e9)
        (ratio 1. "%.1f" run.instructions run.cycles)
        (ratio 100. "%.1f%%" run.branch_misses run.branches)
        (ratio 1000. "%.1f/1k instr" run.cache_misses run.instructions)
        run.samples
        (if run.status = 0 then "" else Printf.sprintf "  exit %d" run.status)
        (if run.samples < 1000 then "  short run: numbers are rough" else "")

let vector_text (factor, interleave) =
  Printf.sprintf "x%d%s" factor
    (if interleave = 1 then "" else Printf.sprintf " interleave %d" interleave)

let vector_form win = "vectorized " ^ vector_text win

let native_ratio (f : facts) =
  match (f.cycles_per_iter, f.native) with
  | Some p, Some (Predicted { cycles = Some n; here; there }) when p > 0. ->
      Some
        ((n *. float_of_int here.elements /. (float_of_int there.elements *. p)) -. 1.)
  | _ -> None

let native_differs_text = function
  | Some win -> vector_form win ^ ", loop shape differs"
  | None -> "loop shape differs"

let native_value (f : facts) =
  let shape (s : shape) =
    Printf.sprintf "%s (%d element%s)"
      (Option.fold ~none:"not vectorized" ~some:vector_form s.vector)
      s.elements
      (if s.elements = 1 then "" else "s")
  in
  match (f.native, native_ratio f) with
  | Some (Differs { vector }), _ -> Some (native_differs_text vector)
  | Some (Predicted { cycles = Some n; here; there }), Some r when Float.abs r >= 0.05
    ->
      let change =
        Printf.sprintf "%s%d%%"
          (if r < 0. then "\u{2212}" else "+")
          (int_of_float (Float.round (100. *. Float.abs r)))
      in
      if here.elements = there.elements then
        Some (Printf.sprintf "%.1f cycles/iter (%s)" n change)
      else
        Some
          (Printf.sprintf "%s, %.1f cycles/iter (%s per element)" (shape there) n change)
  | _ -> None

let calls_text (f : facts) =
  String.concat ", " (List.map (fun (c, n) -> c ^ times n) f.calls)

let kept_text (k : kept) = Printf.sprintf "%s%s (%s)" k.callee (times k.count) k.reason

let fact_strings ~rates (f : facts) =
  let some = Option.to_list in
  let elements =
    Option.fold ~none:""
      ~some:(fun (factor, interleave) ->
        Printf.sprintf " (%d elements)" (factor * interleave))
      f.vectorized
  in
  some (Option.map vector_form f.vectorized)
  @ some (Option.map (( ^ ) "not vectorized: ") f.not_vectorized)
  @ some f.unroll
  @ [
      Printf.sprintf "%d instr/iter%s" f.instrs elements;
      Printf.sprintf "loads %d stores %d /iter" f.loads f.stores;
    ]
  @ some
      (Option.map
         (fun (spills, reloads) -> Printf.sprintf "spills %d reloads %d" spills reloads)
         f.spill)
  @ some (Option.map (Printf.sprintf "%.1f cycles/iter") f.cycles_per_iter)
  @ some (Option.map (( ^ ) "native: ") (native_value f))
  @ rates
  @ some (Option.map (( ^ ) "bound: ") f.bound)
  @ List.map (fun k -> "call kept: " ^ kept_text k) f.calls_kept
  @ (if f.calls = [] then [] else [ "calls: " ^ calls_text f ])
  @ if f.trap then [ "trap check" ] else []

let shown_items (f : facts) =
  let vector =
    match (f.vectorized, f.not_vectorized) with
    | Some win, None -> vector_text win
    | Some win, Some reason -> vector_text win ^ "; not vectorized: " ^ reason
    | None, Some reason -> "not vectorized: " ^ reason
    | None, None -> "not vectorized"
  in
  let or_none = Option.value ~default:"none" in
  let number = Option.map string_of_int in
  let listed show = function [] -> "none" | items -> show items in
  [
    ("vector", "", vector);
    ("unroll", "", or_none f.unroll);
    ("instrs", "instr/iter", string_of_int f.instrs);
    ("loads", "loads", string_of_int f.loads);
    ("stores", "stores", string_of_int f.stores);
    ("spills", "spills", or_none (number (Option.map fst f.spill)));
    ("reloads", "reloads", or_none (number (Option.map snd f.spill)));
    ( "cycles",
      "cycles/iter",
      or_none (Option.map (Printf.sprintf "%.1f") f.cycles_per_iter) );
    ("native", "native:", or_none (native_value f));
    ("bound", "bound:", or_none f.bound);
    ("calls", "calls:", listed (fun _ -> calls_text f) f.calls);
    ( "call_kept",
      "call kept:",
      listed (fun kept -> String.concat "; " (List.map kept_text kept)) f.calls_kept );
    ("trap", "", if f.trap then "trap check" else "none");
  ]

let raw_keys = function
  | "vector" -> [ "vectorized"; "not_vectorized" ]
  | "cycles" -> [ "cycles_per_iter" ]
  | "native" -> [ "native_cycles_per_iter"; "native_elements"; "native_shape_differs" ]
  | "call_kept" -> [ "calls_kept" ]
  | ( "unroll" | "instrs" | "loads" | "stores" | "spills" | "reloads" | "bound"
    | "calls" | "trap" ) as key ->
      [ key ]
  | _ -> []

let rates (e : entry) =
  match e.counts with
  | None -> []
  | Some c ->
      Option.to_list
        (Option.map
           (fun r -> Printf.sprintf "branch miss %.1f%%" (100. *. r))
           (branch_rate c))
      @ Option.to_list
          (Option.map (Printf.sprintf "cache miss %.1f/1k instr") (cache_per_kilo c))

let members (e : entry) = match e.copies with [] -> [ e ] | copies -> copies

let native_counts (report : report) =
  let ratios =
    List.filter_map
      (fun (c : entry) -> Option.bind c.facts native_ratio)
      (List.concat_map members report.entries)
  in
  let count p = List.length (List.filter p ratios) in
  let same r = Float.abs r < 0.05 in
  if ratios = [] then None
  else
    Some
      ( count (fun r -> (not (same r)) && r < 0.),
        count (fun r -> (not (same r)) && r >= 0.),
        count same )

let site (p : place) =
  match p.caller with
  | Some (caller, line) -> Printf.sprintf "%s:%d" caller line
  | None -> p.func

let copy_lines ~rates (copies : entry list) =
  let number (e : entry) = Option.fold ~none:0 ~some:fst e.copy_of in
  let copies = List.stable_sort (fun a b -> compare (number a) (number b)) copies in
  let facts =
    List.map
      (fun (e : entry) -> Option.fold ~none:[] ~some:(fact_strings ~rates) e.facts)
      copies
  in
  let shared =
    match facts with
    | first :: _ -> List.filter (fun s -> List.for_all (List.mem s) facts) first
    | [] -> []
  in
  let own = List.map (List.filter (fun s -> not (List.mem s shared))) facts in
  let sites = List.filter_map (fun (c : entry) -> Option.map site c.place) copies in
  let label (c : entry) =
    match c.place with
    | None -> ""
    | Some p ->
        let s = site p in
        if List.length (List.filter (( = ) s) sites) > 1 then
          Printf.sprintf "%s (copy %d)" s (number c)
        else s
  in
  let groups = if List.for_all (( = ) []) own then [] else distinct own in
  (if shared = [] then [] else [ "      " ^ String.concat "   " shared ])
  @ List.map
      (fun strings ->
        let labels =
          List.filter_map
            (fun (c, f) -> if f = strings then Some (label c) else None)
            (List.combine copies own)
        in
        Printf.sprintf "      in %s: %s" (String.concat ", " labels)
          (if strings = [] then "nothing else" else String.concat "   " strings))
      groups

let owner_text (e : entry) =
  let places = List.filter_map (fun (c : entry) -> c.place) (members e) in
  let sites = distinct (List.map site places) in
  let shown = List.filteri (fun i _ -> i < 2) sites in
  let more = List.length sites - List.length shown in
  let inlined = List.exists (fun (p : place) -> Option.is_some p.caller) places in
  String.concat ", " (distinct (List.map (fun (p : place) -> p.func) places))
  ^
  if inlined then
    Printf.sprintf ", inlined into %s%s" (String.concat ", " shown)
      (if more > 0 then Printf.sprintf " (+%d more)" more else "")
  else ""

let code_text = function
  | Label_line name -> name ^ ":"
  | Insn { share; text; where } ->
      let share =
        Option.fold ~none:(String.make 6 ' ') ~some:(Printf.sprintf "%5.1f%%") share
      in
      "  " ^ share ^ "  "
      ^ Option.fold ~none:text ~some:(fun w -> pad text 36 ^ w) where

let entry_lines (e : entry) =
  let head owner =
    match e.place with
    | Some p ->
        pad (Printf.sprintf "%s:%d" (Filename.basename p.file) p.line) 14
        ^ pad (clip (Option.value ~default:"" e.source)) 41
        ^ owner
    | None -> owner
  in
  match e.kind with
  | Loop ->
      let copy =
        Option.fold ~none:""
          ~some:(fun (n, k) -> Printf.sprintf "   copy %d of %d" n k)
          e.copy_of
      in
      head (owner_text e ^ copy)
      :: (copy_lines ~rates:(rates e) (members e) @ List.map code_text e.code)
  | Function -> [ head (owner_text e ^ " (no loop)") ]
  | Outside ->
      [
        Printf.sprintf "outside Fas: %s (%s)"
          (Option.value ~default:"?" e.symbol)
          (Option.value ~default:"" e.dso);
      ]
  | Rest ->
      let n = Option.value ~default:0 e.places in
      [ Printf.sprintf "rest (%d %s)" n (if n = 1 then "place" else "places") ]

type baseline = { label : string; tree : Json.t }

type change =
  | Added
  | Changed of { items : string list; raw : (string * Json.t * Json.t) list }

type diff = {
  label : string;
  changes : change option list;
  gone : string list;
  facts_only : bool;
}

let changes_of diff (report : report) =
  match diff with
  | Some d -> d.changes
  | None -> List.map (fun _ -> None) report.entries

let text_lines ?diff (report : report) =
  let change_line = function
    | Added -> "    new row"
    | Changed { items; _ } -> "    change: " ^ String.concat "   " items
  in
  let lines (e : entry) change =
    match (report.run, entry_lines e, Option.map change_line change) with
    | Some run, first :: more, change ->
        Printf.sprintf "%3.0f%%  %s" (percent run.cycles e.weight) first
        :: (Option.to_list change @ more)
    | None, first :: more, change -> first :: (Option.to_list change @ more)
    | _, [], change -> Option.to_list change
  in
  let body =
    match (report.run, report.entries) with
    | None, [] -> [ "no loops in Fas code" ]
    | _ -> List.concat (List.map2 lines report.entries (changes_of diff report))
  in
  let native =
    match native_counts report with
    | None -> []
    | Some (faster, slower, same) ->
        [
          Printf.sprintf
            "native target (predicted by llvm-mca): faster on %d loops, slower on %d, \
             same on %d"
            faster slower same;
        ]
  in
  match diff with
  | None -> (header report :: native) @ body
  | Some d ->
      (header report ^ "  compared with " ^ d.label
      ^ if d.facts_only then "  (facts only: one side is static)" else "")
      :: (native @ body)
      @ List.map (fun g -> "gone: " ^ g) d.gone

let jint n = Json.Num (string_of_int n)
let jfloat x = Json.Num (Printf.sprintf "%.6g" x)
let jopt f = Option.fold ~none:Json.Null ~some:f
let jstr s = Json.Str s

let facts_json (f : facts) =
  Json.Obj
    [
      ("instrs", jint f.instrs);
      ("loads", jint f.loads);
      ("stores", jint f.stores);
      ("spills", jopt (fun (s, _) -> jint s) f.spill);
      ("reloads", jopt (fun (_, r) -> jint r) f.spill);
      ( "vectorized",
        jopt
          (fun (factor, interleave) ->
            Json.Obj [ ("factor", jint factor); ("interleave", jint interleave) ])
          f.vectorized );
      ("not_vectorized", jopt jstr f.not_vectorized);
      ("unroll", jopt jstr f.unroll);
      ("cycles_per_iter", jopt jfloat f.cycles_per_iter);
      ("bound", jopt jstr f.bound);
      ("calls", Json.Arr (List.map (fun (c, _) -> jstr c) f.calls));
      ( "calls_kept",
        Json.Arr
          (List.map
             (fun (k : kept) ->
               Json.Obj [ ("callee", jstr k.callee); ("reason", jstr k.reason) ])
             f.calls_kept) );
      ("trap", Json.Bool f.trap);
    ]

let run_json (run : run) =
  Json.Obj
    [
      ("seconds", jfloat run.seconds);
      ("cycles", jint run.cycles);
      ("instructions", jint run.instructions);
      ("branches", jint run.branches);
      ("branch_misses", jint run.branch_misses);
      ("cache_misses", jint run.cache_misses);
      ("samples", jint run.samples);
      ("exit", jint run.status);
    ]

let shown_json (f : facts) =
  Json.Obj
    (List.map
       (fun (key, label, value) -> (key, Json.Arr [ jstr label; jstr value ]))
       (shown_items f))

let native_fields (f : facts) =
  let fields cycles elements differs =
    [
      ("native_cycles_per_iter", jopt jfloat cycles);
      ("native_elements", jopt jint elements);
      ("native_shape_differs", Json.Bool differs);
    ]
  in
  match f.native with
  | None -> []
  | Some (Predicted { cycles; there; _ }) -> fields cycles (Some there.elements) false
  | Some (Differs { vector }) ->
      fields None (Option.map (fun (f, i) -> f * i) vector) true

let row_fields run (e : entry) =
  let at f = jopt f e.place in
  let kind = function
    | Loop -> "loop"
    | Function -> "function"
    | Outside -> "outside"
    | Rest -> "rest"
  in
  [
    ( "share",
      jopt
        (fun (r : run) -> jfloat (float_of_int e.weight /. float_of_int r.cycles))
        run );
    ("kind", jstr (kind e.kind));
    ("file", at (fun (p : place) -> jstr (Filename.basename p.file)));
    ("line", at (fun (p : place) -> jint p.line));
    ("source", jopt jstr e.source);
    ("function", at (fun (p : place) -> jstr p.func));
    ( "inlined_into",
      jopt
        (fun (name, line) -> Json.Obj [ ("function", jstr name); ("line", jint line) ])
        (Option.bind e.place (fun (p : place) -> p.caller)) );
    ("symbol", jopt jstr e.symbol);
    ("dso", jopt jstr e.dso);
    ("places", jopt jint e.places);
    ("facts", jopt facts_json e.facts);
    ("shown", jopt shown_json e.facts);
  ]
  @ Option.fold ~none:[] ~some:native_fields e.facts
  @
  if run = None then []
  else
    [
      ("branch_miss_rate", jopt jfloat (Option.bind e.counts branch_rate));
      ("cache_miss_per_kilo", jopt jfloat (Option.bind e.counts cache_per_kilo));
    ]

let change_json = function
  | None -> Json.Null
  | Some Added -> jstr "new"
  | Some (Changed { raw; _ }) ->
      Json.Obj
        (List.map (fun (key, before, after) -> (key, Json.Arr [ before; after ])) raw)

let json_report ?diff (report : report) =
  let row (e : entry) change =
    let copies = List.map (fun c -> Json.Obj (row_fields report.run c)) e.copies in
    Json.Obj
      (row_fields report.run e
      @ [ ("copies", Json.Arr copies); ("change", change_json change) ])
  in
  let gone = match diff with Some d -> d.gone | None -> [] in
  let native (faster, slower, same) =
    ( "native_summary",
      Json.Obj [ ("faster", jint faster); ("slower", jint slower); ("same", jint same) ]
    )
  in
  Json.to_string
    (Json.Obj
       ([
          ("format", jstr "fas-perf");
          ("version", Json.Num "1");
          ("file", jstr report.title);
          ("run", jopt run_json report.run);
          ("rows", Json.Arr (List.map2 row report.entries (changes_of diff report)));
          ("gone", Json.Arr (List.map jstr gone));
        ]
       @ Option.to_list (Option.map native (native_counts report))))

let lacks_row_text tree =
  match Json.field "rows" tree with
  | Json.Arr rows ->
      List.exists
        (fun row ->
          match (Json.field "facts" row, Json.field "shown" row) with
          | Json.Obj _, Json.Obj _ -> false
          | Json.Obj _, _ -> true
          | _ -> false)
        rows
  | _ -> false

let load_baseline path =
  match In_channel.with_open_bin path In_channel.input_all with
  | exception Sys_error message -> Error ("--compare: " ^ message)
  | text -> (
      match Json.parse text with
      | Error message ->
          Error (Printf.sprintf "--compare: %s is not valid JSON: %s" path message)
      | Ok tree
        when Json.field "format" tree <> Json.Str "fas-perf"
             || Json.field "version" tree <> Json.Num "1" ->
          Error (Printf.sprintf "--compare: %s is not fas perf JSON version 1" path)
      | Ok tree when lacks_row_text tree ->
          Error
            (Printf.sprintf "--compare: %s lacks row text; rerun it with this fas" path)
      | Ok tree -> Ok ({ label = Filename.basename path; tree } : baseline))

let shown_text tree key =
  match Json.field key (Json.field "shown" tree) with
  | Json.Arr [ _; Json.Str value ] -> value
  | _ -> "none"

let raw_value tree key =
  if String.starts_with ~prefix:"native_" key then Json.field key tree
  else Json.field key (Json.field "facts" tree)

let shown_change prefix before after =
  if before = "none" then prefix ^ after ^ " (new)"
  else if after = "none" then prefix ^ before ^ " (gone)"
  else prefix ^ before ^ arrow ^ after

let vector_side tree =
  let facts = Json.field "facts" tree in
  let reason =
    match Json.field "not_vectorized" facts with Json.Str r -> Some r | _ -> None
  in
  let win = Json.field "vectorized" facts in
  let number key =
    match Json.field key win with Json.Num n -> int_of_string_opt n | _ -> None
  in
  let shown =
    match (number "factor", number "interleave") with
    | Some factor, Some interleave ->
        Some
          (vector_text (factor, interleave)
          ^ Option.fold ~none:"" ~some:(( ^ ) "; not vectorized: ") reason)
    | _ -> None
  in
  (shown, reason)

let vector_change old row =
  match (vector_side old, vector_side row) with
  | (Some before, _), (Some after, _) ->
      if before = after then None else Some (before ^ arrow ^ after)
  | (Some before, _), (None, _) -> Some (before ^ arrow ^ "not vectorized")
  | (None, _), (Some after, _) -> Some ("not vectorized" ^ arrow ^ after)
  | (None, before), (None, after) ->
      if before = after then None
      else
        let or_none = Option.value ~default:"none" in
        Some ("not vectorized: " ^ shown_change "" (or_none before) (or_none after))

let fact_changes old row =
  match (Json.field "facts" old, Json.field "facts" row, Json.field "shown" row) with
  | Json.Obj _, Json.Obj _, Json.Obj fields ->
      List.filter_map
        (fun (key, value) ->
          let before = shown_text old key and after = shown_text row key in
          let prefix =
            match value with
            | Json.Arr [ Json.Str label; _ ] when label <> "" -> label ^ " "
            | _ -> ""
          in
          let raw =
            List.filter_map
              (fun k ->
                let before = raw_value old k and after = raw_value row k in
                if before = after then None else Some (k, before, after))
              (raw_keys key)
          in
          let text =
            if key = "vector" then Option.map (( ^ ) prefix) (vector_change old row)
            else if before = after then None
            else Some (shown_change prefix before after)
          in
          Option.map (fun text -> (text, raw)) text)
        fields
  | Json.Obj _, Json.Obj _, _ -> []
  | before, after, _ when before = after -> []
  | before, after, _ ->
      let text = if before = Json.Null then "facts (new)" else "facts (gone)" in
      [ (text, [ ("facts", before, after) ]) ]

let share_change old row =
  match (Json.field "share" old, Json.field "share" row) with
  | Json.Num before, Json.Num after ->
      let delta = (float_of_string after -. float_of_string before) *. 100. in
      if Float.abs delta >= 1.0 then
        [
          ( Printf.sprintf "share %+.1f pts" delta,
            [ ("share", Json.Num before, Json.Num after) ] );
        ]
      else []
  | _ -> []

let row_change old row =
  match share_change old row @ fact_changes old row with
  | [] -> None
  | items ->
      Some (Changed { items = List.map fst items; raw = List.concat_map snd items })

let same_row old row =
  List.for_all
    (fun key -> Json.field key old = Json.field key row)
    [ "kind"; "file"; "line"; "function"; "symbol" ]

let rec take_match row = function
  | [] -> (None, [])
  | old :: rest when same_row old row -> (Some old, rest)
  | old :: rest ->
      let found, rest = take_match row rest in
      (found, old :: rest)

let gone_label old =
  let raw key =
    match Json.field key old with Json.Str s | Json.Num s -> s | _ -> "?"
  in
  match Json.field "kind" old with
  | Json.Str "loop" -> Printf.sprintf "loop %s:%s" (raw "file") (raw "line")
  | Json.Str "function" -> "function " ^ raw "function"
  | Json.Str "outside" -> "outside " ^ raw "symbol"
  | _ -> "rest"

let compare_with (baseline : baseline) (report : report) =
  let rows tree = match Json.field "rows" tree with Json.Arr rows -> rows | _ -> [] in
  match Json.parse (json_report report) with
  | Error message -> Error ("internal error: perf JSON: " ^ message)
  | Ok current ->
      let gone, changes =
        List.fold_left_map
          (fun olds row ->
            match take_match row olds with
            | None, olds -> (olds, Some Added)
            | Some old, olds -> (olds, row_change old row))
          (rows baseline.tree) (rows current)
      in
      let facts_only =
        Json.field "run" baseline.tree = Json.Null <> Option.is_none report.run
      in
      Ok
        { label = baseline.label; changes; gone = List.map gone_label gone; facts_only }
