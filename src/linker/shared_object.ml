module B = Binary
module Intervals = Map.Make (Int)

type symbol = {
  name : string;
  kind : int;
  binding : int;
  size : int64;
  version : string option;
  default : bool;
}

type reference = {
  name : string;
  undefined : bool;
  binding : int;
  kind : int;
  version : string option;
}

type version_requirement = { provider : string; version : string; weak : bool }

(* Storage metadata is separate from lookup descriptors: only an executable's
   COPY planner needs section bounds, alignment and preemption constraints. *)
type location = {
  section : int;
  value : int64;
  size : int64;
  start : int64;
  length : int;
  kind : int;
  flags : int64;
  alignment : int64;
  visibility : int;
  read_only : bool;
}

type t = {
  path : string;
  needed : string;
  dependencies : string array;
  rpath : string option;
  runpath : string option;
  defined_versions : string array;
  version_requirements : version_requirement array;
  exports : symbol array;
  references : reference array;
  locations : (string * string option, location) Hashtbl.t;
}

type section = {
  kind : int;
  offset : int;
  size : int;
  address : int64;
  link : int;
  info : int;
  entsize : int;
}

let is_shared data =
  String.length data >= 18 && String.sub data 0 4 = "\127ELF" && B.u16 data 16 = 3

let parse ~path data =
  let fail fmt =
    Printf.ksprintf (fun text -> Error.fail "%s: shared object: %s" path text) fmt
  in
  let length = String.length data in
  let span what offset size =
    B.span ~context:(path ^ ": shared " ^ what) ~length offset size
  in
  let index what limit value =
    if value < 0L || value > Int64.of_int limit then
      fail "%s exceeds supported bounds" what;
    Int64.to_int value
  in
  span "header" 0 64;
  if
    (not (is_shared data))
    || String.sub data 4 3 <> "\002\001\001"
    || B.u16 data 18 <> 62
    || B.u32 data 20 <> 1L
  then fail "expected little-endian ELF64 x86-64 ET_DYN";
  if
    (B.u8 data 7 <> 0 && B.u8 data 7 <> 3)
    || B.u8 data 8 <> 0
    || B.u32 data 48 <> 0L
    || B.u16 data 52 <> 64
    || B.u16 data 54 <> 56
    || B.u16 data 58 <> 64
  then fail "unsupported ELF ABI or header fields";
  let count = B.u16 data 60 and phnum = B.u16 data 56 in
  if count = 0 || count >= 0xff00 || phnum = 0 || phnum = 0xffff then
    fail "missing or extended header numbering is unsupported";
  let shoff = index "section table offset" length (B.u64 data 40) in
  let phoff = index "program table offset" length (B.u64 data 32) in
  span "section table" shoff (count * 64);
  span "program table" phoff (phnum * 56);
  let sections =
    Array.init count (fun n ->
        let at = shoff + (n * 64) in
        let kind = index "section type" max_int (B.u32 data (at + 4)) in
        let size = index "section size" (1024 * 1024 * 1024) (B.u64 data (at + 32)) in
        let offset =
          if kind = 8 then 0 else index "section offset" length (B.u64 data (at + 24))
        in
        if kind <> 8 then span "section contents" offset size;
        {
          kind;
          offset;
          size;
          address = B.u64 data (at + 16);
          link = index "section link" (count - 1) (B.u32 data (at + 40));
          info = index "section info" max_int (B.u32 data (at + 44));
          entsize = index "section entry size" length (B.u64 data (at + 56));
        })
  in
  let unique kind description =
    let found = Array.to_list sections |> List.filter (fun s -> s.kind = kind) in
    match found with [ s ] -> s | _ -> fail "expected one %s section" description
  in
  let optional kind description =
    match Array.to_list sections |> List.filter (fun s -> s.kind = kind) with
    | [] -> None
    | [ s ] -> Some s
    | _ -> fail "multiple %s sections" description
  in
  let symbols = unique 11 "dynamic symbol" in
  let symbol_index = Option.get (Array.find_index (fun s -> s == symbols) sections) in
  if symbols.entsize <> 24 || symbols.size = 0 || symbols.size mod 24 <> 0 then
    fail "invalid dynamic symbol table size";
  let symbol_count = symbols.size / 24 in
  if symbol_count > 1_000_000 then fail "dynamic symbol count exceeds 1000000";
  let strings = sections.(symbols.link) in
  if
    strings.kind <> 3 || strings.size = 0
    || B.u8 data strings.offset <> 0
    || B.u8 data (strings.offset + strings.size - 1) <> 0
  then fail "invalid dynamic string table";
  let decoded = ref 0 in
  let name value =
    let at = index "dynamic string index" (strings.size - 1) value in
    let result =
      B.string_at ~context:(path ^ ": dynamic name") data ~offset:strings.offset
        ~size:strings.size at
    in
    decoded :=
      B.add ~context:"shared decoded names"
        ~limit:(64 * 1024 * 1024)
        !decoded (String.length result);
    result
  in
  let dynamic = unique 6 "dynamic" in
  if dynamic.entsize <> 16 || dynamic.size mod 16 <> 0 then
    fail "invalid dynamic entry size";
  let tags = Hashtbl.create 32 and terminated = ref false in
  let dependencies = ref [] in
  for n = 0 to (dynamic.size / 16) - 1 do
    let at = dynamic.offset + (n * 16) in
    if not !terminated then begin
      let tag = B.u64 data at and value = B.u64 data (at + 8) in
      if tag = 0L then terminated := true
      else if tag = 1L then begin
        let dependency = name value in
        if dependency = "" || String.length dependency > 65535 then
          fail "invalid DT_NEEDED name";
        dependencies := dependency :: !dependencies
      end
      else begin
        if Hashtbl.mem tags tag then fail "repeated singleton dynamic tag 0x%Lx" tag;
        Hashtbl.add tags tag value
      end
    end
  done;
  if not !terminated then fail "dynamic table has no terminator";
  let tag n = Hashtbl.find_opt tags (Int64.of_int n) in
  let expect n value =
    if tag n <> Some value then
      fail "dynamic tag 0x%x disagrees with section metadata" n
  in
  expect 5 strings.address;
  expect 6 symbols.address;
  expect 10 (Int64.of_int strings.size);
  expect 11 24L;
  let mapped (s : section) =
    let present = ref false in
    for n = 0 to phnum - 1 do
      let at = phoff + (n * 56) in
      if B.u32 data at = 1L then begin
        let offset = index "load offset" length (B.u64 data (at + 8)) in
        let size = index "load file size" length (B.u64 data (at + 32)) in
        span "load" offset size;
        if B.u64 data (at + 40) < Int64.of_int size then fail "invalid load memory size";
        let address = B.u64 data (at + 16) in
        if
          s.offset >= offset
          && s.size <= size - (s.offset - offset)
          && s.address = Int64.add address (Int64.of_int (s.offset - offset))
        then present := true
      end
    done;
    if not !present then fail "dynamic metadata is not mapped consistently"
  in
  List.iter mapped [ symbols; strings; dynamic ];
  let dynamic_header = ref false in
  for n = 0 to phnum - 1 do
    let at = phoff + (n * 56) in
    if B.u32 data at = 2L then begin
      if
        !dynamic_header
        || B.u64 data (at + 8) <> Int64.of_int dynamic.offset
        || B.u64 data (at + 16) <> dynamic.address
        || B.u64 data (at + 32) <> Int64.of_int dynamic.size
      then fail "PT_DYNAMIC disagrees with dynamic section";
      dynamic_header := true
    end
  done;
  if not !dynamic_header then fail "missing PT_DYNAMIC";
  if Int64.logand (Option.value (tag 0x6ffffffb) ~default:0L) 0x08000000L <> 0L then
    fail "PIE executables cannot be used as shared libraries";
  let version_records = ref 0 in
  let claim intervals description at width =
    incr version_records;
    if !version_records > 1_000_000 then fail "version record count exceeds 1000000";
    (match Intervals.find_last_opt (fun start -> start < at + width) !intervals with
    | Some (_, ending) when ending > at -> fail "overlapping %s" description
    | _ -> ());
    intervals := Intervals.add at (at + width) !intervals
  in
  let versions = Hashtbl.create 32 in
  (match (optional 0x6ffffffd "version definition", tag 0x6ffffffc, tag 0x6ffffffd) with
  | None, None, None -> ()
  | Some s, Some address, Some amount when address = s.address ->
      mapped s;
      if s.link <> symbols.link then fail "version definition string table differs";
      let amount = index "version definition count" 32766 amount in
      if amount <> s.info || amount = 0 then fail "invalid version definition count";
      let cursor = ref s.offset and ending = s.offset + s.size in
      let intervals = ref Intervals.empty in
      let bounded at width =
        if at < s.offset || at > ending || width > ending - at then
          fail "truncated version definition";
        claim intervals "version definitions" at width
      in
      for n = 0 to amount - 1 do
        bounded !cursor 20;
        let id = B.u16 data (!cursor + 4) and aux_count = B.u16 data (!cursor + 6) in
        if
          B.u16 data !cursor <> 1
          || id = 0 || id >= 0x8000 || Hashtbl.mem versions id || aux_count = 0
        then fail "invalid version definition";
        let aux = index "version auxiliary offset" s.size (B.u32 data (!cursor + 12)) in
        if aux < 20 then fail "overlapping version auxiliary record";
        let at = ref (!cursor + aux) and version = ref "" in
        for j = 0 to aux_count - 1 do
          bounded !at 8;
          let text = name (B.u32 data !at) in
          if j = 0 then version := text;
          let next = index "version auxiliary link" s.size (B.u32 data (!at + 4)) in
          if j = aux_count - 1 then
            begin if next <> 0 then fail "unterminated version auxiliary list"
            end
          else begin
            if next < 8 then fail "cyclic version auxiliary list";
            at := !at + next
          end
        done;
        if !version = "" || Symbol_hash.sysv !version <> B.u32 data (!cursor + 8) then
          fail "invalid version name or hash";
        Hashtbl.add versions id !version;
        let next = index "version definition link" s.size (B.u32 data (!cursor + 16)) in
        if n = amount - 1 then
          begin if next <> 0 then fail "unterminated version definition list"
          end
        else begin
          if next < 20 then fail "overlapping version definitions";
          cursor := !cursor + next
        end
      done
  | _ -> fail "inconsistent version definition metadata");
  let required_versions = Hashtbl.create 32 in
  let version_requirements = ref [] in
  (match
     (optional 0x6ffffffe "version requirement", tag 0x6ffffffe, tag 0x6fffffff)
   with
  | None, None, None -> ()
  | Some s, Some address, Some amount when address = s.address ->
      mapped s;
      if s.link <> symbols.link then fail "version requirement string table differs";
      let amount = index "version requirement count" 32766 amount in
      if amount = 0 || amount <> s.info then fail "invalid version requirement count";
      let cursor = ref s.offset and ending = s.offset + s.size in
      let intervals = ref Intervals.empty in
      let bounded at width =
        if at < s.offset || at > ending || width > ending - at then
          fail "truncated version requirement";
        claim intervals "version requirements" at width
      in
      for n = 0 to amount - 1 do
        bounded !cursor 16;
        let count = B.u16 data (!cursor + 2) in
        if B.u16 data !cursor <> 1 || count = 0 then fail "invalid version requirement";
        let provider = name (B.u32 data (!cursor + 4)) in
        if provider = "" then fail "empty version requirement provider";
        let aux =
          index "version requirement auxiliary offset" s.size (B.u32 data (!cursor + 8))
        in
        if aux < 16 then fail "overlapping version requirement auxiliary record";
        let at = ref (!cursor + aux) in
        for j = 0 to count - 1 do
          bounded !at 16;
          let id = B.u16 data (!at + 6) land 0x7fff in
          let version = name (B.u32 data (!at + 8)) in
          if
            id < 2
            || Hashtbl.mem required_versions id
            || Hashtbl.mem versions id
            || B.u16 data (!at + 4) land lnot 2 <> 0
            || version = ""
            || Symbol_hash.sysv version <> B.u32 data !at
          then fail "invalid version requirement auxiliary record";
          Hashtbl.add required_versions id version;
          version_requirements :=
            { provider; version; weak = B.u16 data (!at + 4) land 2 <> 0 }
            :: !version_requirements;
          let next =
            index "version requirement auxiliary link" s.size (B.u32 data (!at + 12))
          in
          if j = count - 1 then
            begin if next <> 0 then
              fail "unterminated version requirement auxiliary list"
            end
          else begin
            if next < 16 then fail "cyclic version requirement auxiliary list";
            at := !at + next
          end
        done;
        let next =
          index "version requirement link" s.size (B.u32 data (!cursor + 12))
        in
        if n = amount - 1 then
          begin if next <> 0 then fail "unterminated version requirement list"
          end
        else begin
          if next < 16 then fail "overlapping version requirements";
          cursor := !cursor + next
        end
      done
  | _ -> fail "inconsistent version requirement metadata");
  let versym = optional 0x6fffffff "version symbol" in
  (match (versym, tag 0x6ffffff0) with
  | None, None -> ()
  | Some s, Some address when address = s.address ->
      if s.size <> symbol_count * 2 || s.link <> symbol_index || s.entsize <> 2 then
        fail "invalid version symbol table";
      mapped s
  | _ -> fail "inconsistent version symbol metadata");
  for n = 0 to 23 do
    if B.u8 data (symbols.offset + n) <> 0 then fail "invalid null dynamic symbol"
  done;
  let raw =
    Array.init symbol_count (fun n ->
        let at = symbols.offset + (n * 24) in
        let info = B.u8 data (at + 4) in
        ( name (B.u32 data at),
          info lsr 4,
          info land 15,
          B.u8 data (at + 5),
          B.u16 data (at + 6),
          B.u64 data (at + 16) ))
  in
  let hash = optional 5 "SysV hash" and gnu_hash = optional 0x6ffffff6 "GNU hash" in
  let validate_hash s address minimum =
    if s.address <> address || s.size < minimum then fail "inconsistent hash metadata";
    mapped s
  in
  let coverage seen =
    Array.iteri
      (fun n (name, binding, _, visibility, section, _) ->
        let id =
          match versym with
          | None -> 1
          | Some s -> B.u16 data (s.offset + (n * 2)) land 0x7fff
        in
        if
          n <> 0 && binding <> 0 && section <> 0
          && visibility land 3 <> 1
          && visibility land 3 <> 2
          && id <> 0
          && not seen.(n)
        then fail "export %s is missing from dynamic hash" name)
      raw
  in
  (match (hash, tag 4) with
  | None, None -> ()
  | Some s, Some address ->
      validate_hash s address 8;
      let buckets = index "hash bucket count" 1_000_000 (B.u32 data s.offset) in
      let chains = index "hash chain count" 1_000_000 (B.u32 data (s.offset + 4)) in
      if buckets = 0 || chains <> symbol_count || 8 + (4 * (buckets + chains)) > s.size
      then fail "invalid SysV hash size";
      let seen = Array.make symbol_count false in
      for bucket = 0 to buckets - 1 do
        let current =
          ref
            (index "hash bucket" (symbol_count - 1)
               (B.u32 data (s.offset + 8 + (bucket * 4))))
        in
        while !current <> 0 do
          if seen.(!current) then fail "cyclic or overlapping SysV hash chains";
          seen.(!current) <- true;
          let text, _, _, _, _, _ = raw.(!current) in
          if
            Int64.rem (Symbol_hash.sysv text) (Int64.of_int buckets)
            <> Int64.of_int bucket
          then fail "SysV hash name mismatch";
          current :=
            index "hash chain" (symbol_count - 1)
              (B.u32 data (s.offset + 8 + ((buckets + !current) * 4)))
        done
      done;
      coverage seen
  | _ -> fail "inconsistent SysV hash metadata");
  (match (gnu_hash, tag 0x6ffffef5) with
  | None, None -> ()
  | Some s, Some address ->
      validate_hash s address 16;
      let buckets = index "GNU bucket count" 1_000_000 (B.u32 data s.offset) in
      let first = index "GNU first symbol" symbol_count (B.u32 data (s.offset + 4)) in
      let bloom = index "GNU bloom size" 1_000_000 (B.u32 data (s.offset + 8)) in
      let shift = index "GNU bloom shift" 63 (B.u32 data (s.offset + 12)) in
      if
        buckets = 0 || bloom = 0
        || bloom land (bloom - 1) <> 0
        || 16 + (bloom * 8) + (buckets * 4) + ((symbol_count - first) * 4) > s.size
      then fail "invalid GNU hash size";
      let seen = Array.make symbol_count false in
      for bucket = 0 to buckets - 1 do
        let current =
          ref
            (index "GNU hash bucket" (symbol_count - 1)
               (B.u32 data (s.offset + 16 + (bloom * 8) + (bucket * 4))))
        in
        let finished = ref (!current = 0) in
        while not !finished do
          if !current < first || !current >= symbol_count || seen.(!current) then
            fail "invalid or overlapping GNU hash chain";
          seen.(!current) <- true;
          let text, _, _, _, _, _ = raw.(!current) in
          let hash = Symbol_hash.gnu text in
          let chain =
            B.u32 data
              (s.offset + 16 + (bloom * 8) + (buckets * 4) + ((!current - first) * 4))
          in
          let word =
            B.u64 data
              (s.offset + 16 + (Int64.to_int (Int64.div hash 64L) land (bloom - 1) * 8))
          in
          let bit n = Int64.shift_left 1L (Int64.to_int (Int64.logand n 63L)) in
          let bits =
            Int64.logor (bit hash) (bit (Int64.shift_right_logical hash shift))
          in
          if
            Int64.logand chain 0xfffffffeL <> Int64.logand hash 0xfffffffeL
            || Int64.rem hash (Int64.of_int buckets) <> Int64.of_int bucket
            || Int64.logand word bits <> bits
          then fail "GNU hash name or bloom mismatch";
          finished := Int64.logand chain 1L <> 0L;
          incr current
        done
      done;
      coverage seen
  | _ -> fail "inconsistent GNU hash metadata");
  if hash = None && gnu_hash = None then fail "missing dynamic hash table";
  let locations = Hashtbl.create ~random:true 32 in
  let relro = ref [] in
  for n = 0 to phnum - 1 do
    let at = phoff + (n * 56) in
    if B.u32 data at = 0x6474e552L then
      relro := (B.u64 data (at + 16), B.u64 data (at + 40)) :: !relro
  done;
  if List.length !relro > 16 then fail "GNU RELRO segment count exceeds 16";
  let exports =
    Array.to_list raw
    |> List.mapi (fun n (name, binding, kind, visibility, section, size) ->
        if
          n = 0 || binding = 0 || section = 0
          || visibility land 3 = 1
          || visibility land 3 = 2
        then None
        else begin
          if
            (binding <> 1 && binding <> 2 && binding <> 10)
            || (not (List.mem kind [ 0; 1; 2; 6; 10 ]))
            || visibility > 3 || name = ""
          then fail "unsupported dynamic symbol %s" name;
          if section >= count && section <> 0xfff1 then
            fail "dynamic symbol %s has invalid section" name;
          let version_id =
            match versym with None -> 1 | Some s -> B.u16 data (s.offset + (n * 2))
          in
          let id = version_id land 0x7fff in
          let version =
            if id < 2 then None
            else
              match Hashtbl.find_opt versions id with
              | Some version -> Some version
              | None -> fail "symbol %s references missing version definition" name
          in
          if id = 0 then None
          else begin
            if section < count then begin
              let source = sections.(section) in
              let at = shoff + (section * 64) in
              let value = B.u64 data (symbols.offset + (n * 24) + 8) in
              let flags = B.u64 data (at + 8) in
              let read_only =
                Int64.logand flags 1L = 0L
                || List.exists
                     (fun (start, length) ->
                       let delta = Int64.sub value start in
                       delta >= 0L && length >= delta && size >= 0L
                       && size <= Int64.sub length delta)
                     !relro
              in
              Hashtbl.replace locations (name, version)
                {
                  section;
                  value;
                  size;
                  start = source.address;
                  length = source.size;
                  kind = source.kind;
                  flags;
                  alignment = B.u64 data (at + 48);
                  visibility;
                  read_only;
                }
            end;
            Some
              {
                name;
                kind;
                binding;
                size;
                version;
                default = version_id land 0x8000 = 0;
              }
          end
        end)
    |> List.filter_map Fun.id |> Array.of_list
  in
  let needed = match tag 14 with None -> path | Some value -> name value in
  if needed = "" || String.length needed > 65535 then fail "invalid shared library name";
  let references =
    Array.to_list raw
    |> List.mapi (fun n (name, binding, kind, visibility, section, _) ->
        if binding = 10 && section = 0 then fail "undefined GNU unique symbol %s" name;
        if name = "" || binding = 0 || visibility land 3 = 1 || visibility land 3 = 2
        then None
        else begin
          let id =
            match versym with
            | None -> 1
            | Some s -> B.u16 data (s.offset + (n * 2)) land 0x7fff
          in
          let version =
            if section <> 0 || id < 2 then None
            else
              match Hashtbl.find_opt required_versions id with
              | Some version -> Some version
              | None -> fail "symbol %s references missing version requirement" name
          in
          Some { name; undefined = section = 0; binding; kind; version }
        end)
    |> List.filter_map Fun.id |> Array.of_list
  in
  {
    path;
    needed;
    exports;
    references;
    locations;
    dependencies = Array.of_list (List.rev !dependencies);
    rpath = Option.map name (tag 15);
    runpath = Option.map name (tag 29);
    defined_versions =
      Array.of_list (Hashtbl.fold (fun _ name names -> name :: names) versions []);
    version_requirements = Array.of_list (List.rev !version_requirements);
  }
