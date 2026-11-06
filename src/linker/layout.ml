module B = Binary
module E = Elf_reader

let base = 0x400000
let page = 4096
let max_output = 512 * 1024 * 1024
let max_image = 1024 * 1024 * 1024

let initialization_name = function
  | 14 -> Some ".init_array"
  | 15 -> Some ".fini_array"
  | 16 -> Some ".preinit_array"
  | _ -> None

let combined_name (s : E.section) =
  if s.flags land 0x400 <> 0 then None
  else
    match initialization_name s.kind with
    | Some name -> Some name
    | None when Section_registry.eligible s -> Some s.name
    (* Debuggers and Valgrind expect a single conventional .text/.data/.bss
       range. Keep input placements separate for relocations and GC, but give
       compatible contributions one output header. Function sections belong
       to the same text range; unrelated named sections retain their identity. *)
    | None when s.kind = 1 && s.flags land 6 = 6
                && (s.name = ".text" || String.starts_with ~prefix:".text." s.name) ->
        Some ".text"
    | None when s.flags land 2 <> 0
                && List.mem s.kind [ 1; 8 ]
                && List.mem s.name [ ".data"; ".rodata"; ".bss"; ".sdata"; ".sbss" ] ->
        Some s.name
    | None when List.mem s.name [ ".init"; ".fini"; ".eh_frame" ] -> Some s.name
    | _ -> None

let initialization_priority (s : E.section) =
  match initialization_name s.kind with
  | Some base when String.length s.name > String.length base ->
      int_of_string
        (String.sub s.name
           (String.length base + 1)
           (String.length s.name - String.length base - 1))
  | _ -> 65535

let readonly_data (s : E.section) =
  s.kind = 1 && s.flags land 7 = 2
  && (s.name = ".rodata" || String.starts_with ~prefix:".rodata." s.name)

type placement = { output_index : int; offset : int; address : int }
type chunk = { file : int; section : E.section; placement : placement }

type section = {
  index : int;
  name : string;
  kind : int;
  flags : int;
  offset : int;
  address : int;
  size : int;
  alignment : int;
  entsize : int;
}

type segment = {
  flags : int;
  offset : int;
  file_size : int;
  memory_size : int;
  alignment : int;
}

type t = {
  placements : placement option array array;
  content : chunk list;
  sections : section list;
  segments : segment list;
  cursor : int;
  tls : segment option;
  relro : segment option;
}

(* A layout address needs a regular section index in .dynsym so that the loader
   adds the image's load bias. Prefer its containing section, including one-past
   boundaries. Header/padding/empty-array addresses can precede every section;
   anchor those to the first allocatable section without changing their value. *)
let image_symbol_section sections value =
  let first = ref None and containing = ref None in
  List.iter
    (fun (s : section) ->
      if s.flags land 2 <> 0 && s.flags land 0x400 = 0 then begin
        if !first = None then first := Some s.index;
        let start = Int64.of_int s.address in
        let ending = Int64.add start (Int64.of_int s.size) in
        if value >= start && value <= ending then containing := Some s.index
      end)
    sections;
  match (!containing, !first) with
  | Some index, _ | None, Some index -> index
  | None, None -> Error.fail "image-relative symbol has no allocatable output section"

let make ~base ~relro ~kept ~aliases objects =
  let add = B.add ~context:"output layout" ~limit:max_output in
  let align value alignment =
    B.align ~context:"output layout" ~limit:max_output value alignment
  in
  let placements =
    Array.map (fun (o : E.t) -> Array.make (Array.length o.sections) None) objects
  in
  let content = ref [] and headers = ref [] and count = ref 0 in
  let cursor = ref page in
  let header (s : E.section) ~offset ~address ~size ~alignment =
    incr count;
    if !count + 4 >= 0xff00 then
      Error.fail "output requires unsupported extended section numbering";
    headers :=
      {
        index = !count;
        name = s.name;
        kind = s.kind;
        flags = s.flags land lnot 0x200;
        offset;
        address;
        size;
        alignment;
        entsize = s.entsize;
      }
      :: !headers;
    !count
  in
  let chunk file (s : E.section) ~output_index ~offset ~address =
    let placement = { output_index; offset; address } in
    placements.(file).(s.index) <- Some placement;
    content := { file; section = s; placement } :: !content
  in
  let add_section file (s : E.section) offset address =
    let output_index = header s ~offset ~address ~size:s.size ~alignment:s.alignment in
    chunk file s ~output_index ~offset ~address
  in
  let select predicate =
    let selected = ref [] in
    Array.iteri
      (fun file (o : E.t) ->
        Array.iter
          (fun (s : E.section) ->
            if
              (List.mem s.kind [ 1; 7; 8; 14; 15; 16 ]
              || s.flags land 2 <> 0
                 && List.mem s.kind
                      [ 3; 4; 5; 6; 11; 0x6fffffff; 0x6ffffffe; 0x6ffffffd; 0x6ffffff6 ]
              )
              && kept.(file).(s.index)
              && aliases.(file).(s.index) = None
              && s.name <> ".note.GNU-stack" && predicate s
            then selected := (file, s) :: !selected)
          o.sections)
      objects;
    List.rev !selected
  in
  let segments = ref [] in
  let tls = ref None in
  let relro_segment = ref None in
  let is_relro (s : E.section) =
    relro
    && s.flags land 3 = 3
    && s.flags land 0x400 = 0
    && s.kind <> 8
    && (List.mem s.name [ ".got"; ".got.plt"; ".dynamic" ]
       || List.mem s.kind [ 14; 15; 16 ]
       || s.name = ".data.rel.ro"
       || String.starts_with ~prefix:".data.rel.ro." s.name)
  in
  List.iter
    (fun (wanted, flags) ->
      let sections =
        select (fun (s : E.section) ->
            s.flags land 2 <> 0
            && (if s.flags land 4 <> 0 then 1 else if s.flags land 1 <> 0 then 2 else 0)
               = wanted)
        |> List.stable_sort (fun (_, a) (_, b) ->
            (* Older Valgrind aggregates .rodata* ranges only when adjacent.
               Keep their individual merge/string metadata, but place the
               ranges together rather than interleaving notes or unwind data. *)
            compare (not (is_relro a), not (readonly_data a))
              (not (is_relro b), not (readonly_data b)))
      in
      if sections <> [] then begin
        let alignment =
          List.fold_left (fun a (_, (s : E.section)) -> max a s.alignment) page sections
        in
        let start = align !cursor alignment in
        cursor := start;
        let has_relro = List.exists (fun (_, s) -> is_relro s) sections in
        let end_relro () =
          if has_relro && !relro_segment = None then begin
            cursor := align !cursor page;
            relro_segment :=
              Some
                {
                  flags = 4;
                  offset = start;
                  file_size = !cursor - start;
                  memory_size = !cursor - start;
                  alignment = 1;
                }
          end
        in
        let key (s : E.section) name =
          (* CRT and compiler array contributions can disagree on sh_entsize
             (notably GNU CRT versus Clang). They still form one lifecycle
             array; splitting it lets another array fall inside its bounds. *)
          ( name,
            if Section_registry.eligible s
               || List.mem name [ ".text"; ".data"; ".rodata"; ".bss"; ".sdata"; ".sbss" ]
            then Section_registry.signature s
            else (0, 0, 0) )
        in
        let groups = Hashtbl.create ~random:true 16 in
        List.iter
          (fun ((_, s) as member) ->
            Option.iter
              (fun name ->
                let key = key s name in
                let previous = Option.value (Hashtbl.find_opt groups key) ~default:[] in
                Hashtbl.replace groups key (member :: previous))
              (combined_name s))
          sections;
        let combined = Hashtbl.create ~random:true 8 in
        List.iter
          (fun (file, (s : E.section)) ->
            if not (is_relro s) then end_relro ();
            match (s.kind = 8, combined_name s) with
            | true, _ -> ()
            | false, Some name ->
                begin if not (Hashtbl.mem combined (key s name)) then begin
                  Hashtbl.add combined (key s name) ();
                  let members =
                    List.rev (Hashtbl.find groups (key s name))
                    |> List.stable_sort (fun (_, a) (_, b) ->
                        compare (initialization_priority a) (initialization_priority b))
                  in
                  let alignment =
                    List.fold_left
                      (fun a (_, (member : E.section)) -> max a member.alignment)
                      1 members
                  in
                  cursor := align !cursor alignment;
                  let beginning = !cursor and output_index = !count + 1 in
                  List.iter
                    (fun (file, (member : E.section)) ->
                      cursor := align !cursor member.alignment;
                      chunk file member ~output_index ~offset:!cursor
                        ~address:(base + !cursor);
                      cursor := add !cursor member.size)
                    members;
                  (* normalize strips per-input unwind terminators so they do
                     not stop readers before later contributions. Legacy
                     __register_frame_info still scans to a zero length; keep
                     one final terminator inside the combined output section. *)
                  if name = ".eh_frame" then cursor := add !cursor 4;
                  ignore
                    (header { s with name } ~offset:beginning
                       ~address:(base + beginning) ~size:(!cursor - beginning)
                       ~alignment)
                end
                end
            | false, None ->
                if s.kind <> 8 && s.flags land 0x400 = 0 then begin
                  cursor := align !cursor s.alignment;
                  add_section file s !cursor (base + !cursor);
                  cursor := add !cursor s.size
                end)
          sections;
        end_relro ();
        let tls_sections =
          List.filter (fun (_, (s : E.section)) -> s.flags land 0x400 <> 0) sections
        in
        let tls_alignment =
          List.fold_left
            (fun a (_, (s : E.section)) -> max a s.alignment)
            1 tls_sections
        in
        if tls_sections <> [] then cursor := align !cursor tls_alignment;
        let tls_start = !cursor in
        List.iter
          (fun (file, (s : E.section)) ->
            if s.kind <> 8 then begin
              cursor := align !cursor s.alignment;
              add_section file s !cursor (base + !cursor);
              cursor := add !cursor s.size
            end)
          tls_sections;
        let file_end = !cursor and memory_end = ref !cursor in
        let nobits =
          List.filter (fun (_, (s : E.section)) -> s.kind = 8) sections
          |> List.stable_sort (fun (_, (a : E.section)) (_, (b : E.section)) ->
              compare (a.flags land 0x400 = 0) (b.flags land 0x400 = 0))
        in
        List.iter
          (fun (file, (s : E.section)) ->
            match combined_name s with
            | Some name ->
                if not (Hashtbl.mem combined (key s name)) then begin
                  Hashtbl.add combined (key s name) ();
                  let members = List.rev (Hashtbl.find groups (key s name)) in
                  let alignment =
                    List.fold_left
                      (fun value (_, (s : E.section)) -> max value s.alignment)
                      1 members
                  in
                  memory_end :=
                    B.align ~context:"registry BSS layout" ~limit:max_image !memory_end
                      alignment;
                  let beginning = !memory_end and output_index = !count + 1 in
                  List.iter
                    (fun (file, (member : E.section)) ->
                      memory_end :=
                        B.align ~context:"registry BSS layout" ~limit:max_image
                          !memory_end member.alignment;
                      chunk file member ~output_index ~offset:!memory_end
                        ~address:(base + !memory_end);
                      memory_end :=
                        B.add ~context:"registry BSS layout" ~limit:max_image
                          !memory_end member.size)
                    members;
                  ignore
                    (header s ~offset:beginning ~address:(base + beginning)
                       ~size:(!memory_end - beginning) ~alignment)
                end
            | None -> begin
                memory_end :=
                  B.align ~context:"BSS layout" ~limit:max_image !memory_end s.alignment;
                add_section file s !memory_end (base + !memory_end);
                memory_end :=
                  B.add ~context:"BSS layout" ~limit:max_image !memory_end s.size;
                if s.flags land 0x400 <> 0 then
                  tls :=
                    Some
                      {
                        flags = 4;
                        offset = tls_start;
                        file_size = file_end - tls_start;
                        memory_size = !memory_end - tls_start;
                        alignment = tls_alignment;
                      }
              end)
          nobits;
        if tls_sections <> [] && !tls = None then
          tls :=
            Some
              {
                flags = 4;
                offset = tls_start;
                file_size = file_end - tls_start;
                memory_size = file_end - tls_start;
                alignment = tls_alignment;
              };
        segments :=
          {
            flags;
            offset = start;
            file_size = file_end - start;
            memory_size = !memory_end - start;
            alignment;
          }
          :: !segments;
        cursor := if wanted = 2 then file_end else !memory_end
      end)
    [ (1, 5); (0, 4); (2, 6) ];
  (* Debug relocations refer to offsets inside the combined output debug section,
     not file offsets or virtual addresses. A contribution's address below is
     its offset in that section; the output section's sh_addr remains zero. *)
  let groups = Hashtbl.create ~random:true 16 and order = ref [] in
  List.iter
    (fun (file, (s : E.section)) ->
      let key = (s.name, s.kind, s.flags land lnot 0x200, s.entsize) in
      match Hashtbl.find_opt groups key with
      | Some members -> members := (file, s) :: !members
      | None ->
          let members = ref [ (file, s) ] in
          Hashtbl.add groups key members;
          order := members :: !order)
    (select (fun (s : E.section) -> s.flags land 2 = 0));
  List.iter
    (fun members ->
      let members = List.rev !members in
      let alignment =
        List.fold_left (fun a (_, (s : E.section)) -> max a s.alignment) 1 members
      in
      cursor := align !cursor alignment;
      let start = !cursor and output_index = !count + 1 in
      List.iter
        (fun (file, (s : E.section)) ->
          cursor := align !cursor s.alignment;
          chunk file s ~output_index ~offset:!cursor ~address:(!cursor - start);
          cursor := add !cursor s.size)
        members;
      ignore
        (header
           (snd (List.hd members))
           ~offset:start ~address:0 ~size:(!cursor - start) ~alignment))
    (List.rev !order);
  Array.iteri
    (fun file sections ->
      Array.iteri
        (fun index canonical ->
          Option.iter
            (fun (source_file, source_section) ->
              placements.(file).(index) <- placements.(source_file).(source_section))
            canonical)
        sections)
    aliases;
  {
    placements;
    content = List.rev !content;
    sections = List.rev !headers;
    segments = List.rev !segments;
    cursor = !cursor;
    tls = !tls;
    relro = !relro_segment;
  }
