module E = Elf_reader
module B = Binary

type frame = {
  file : int;
  section : int;
  offset : int;
  initial : int;
  encoding : int;
  relocation : int;
  range : int64;
  metadata : int array;
}

type t = { frames : frame array; header_file : int option }

let normalize ?stats (resolved : Resolver.t) =
  let frames = ref [] in
  let objects =
    Array.mapi
      (fun file (o : E.t) ->
        let appended = Buffer.create 128 in
        let changed = ref false in
        let sections = Array.copy o.sections in
        Array.iter
          (fun (s : E.section) ->
            if s.name = ".eh_frame" then begin
              changed := true;
              let fail fmt =
                Printf.ksprintf
                  (fun text -> Error.fail "%s: .eh_frame: %s" o.path text)
                  fmt
              in
              let data = String.sub o.data s.offset s.size in
              let byte at ending =
                if at >= ending then fail "truncated CIE operand" else B.u8 data at
              in
              let leb at ending =
                let value = ref 0L and shift = ref 0 and more = ref true in
                while !more do
                  if !shift >= 70 then fail "oversized LEB128 operand";
                  let b = byte !at ending in
                  incr at;
                  if !shift = 63 && b land 0x7e <> 0 && b land 0x7f <> 0x7f then
                    fail "LEB128 overflow";
                  value :=
                    Int64.logor !value
                      (Int64.shift_left (Int64.of_int (b land 127)) !shift);
                  shift := !shift + 7;
                  more := b land 128 <> 0
                done;
                !value
              in
              let relocated = Hashtbl.create 16 in
              Array.iteri
                (fun index (r : E.relocation) ->
                  if r.target = s.index && r.kind <> 0 then
                    Hashtbl.add relocated r.offset index)
                o.relocations;
              let cies = Hashtbl.create 8 in
              let consumed = Hashtbl.create 16 in
              let augmentation_end cursor ending =
                let size = leb cursor ending in
                if size < 0L || size > Int64.of_int (ending - !cursor) then
                  fail "invalid augmentation length";
                !cursor + Int64.to_int size
              in
              let pointer ~indirect encoding cursor ending =
                let plain = encoding land 0x7f in
                if (not indirect) && encoding land 0x80 <> 0 then
                  fail "unsupported indirect pointer encoding 0x%x" encoding;
                let width, expected =
                  match plain with
                  | 0 -> (8, 1)
                  | 3 -> (4, 10)
                  | 4 | 12 -> (8, 1)
                  | 11 -> (4, 11)
                  | 0x1b -> (4, 2)
                  | 0x1c -> (8, 24)
                  | _ -> fail "unsupported augmentation pointer encoding 0x%x" encoding
                in
                if width > ending - !cursor then fail "truncated augmentation pointer";
                let offset = !cursor in
                cursor := offset + width;
                match Hashtbl.find_opt relocated offset with
                | Some index ->
                    if o.relocations.(index).kind <> expected then
                      fail "augmentation relocation does not match its pointer encoding";
                    Hashtbl.add consumed offset ();
                    Some index
                | None ->
                    let value =
                      if width = 4 then B.u32 data offset else B.u64 data offset
                    in
                    if value <> 0L then fail "augmentation pointer needs a relocation";
                    None
              in
              let at = ref 0 and last = ref None and finished = ref false in
              while !at < s.size && not !finished do
                if s.size - !at < 4 then fail "truncated record length";
                let length = B.u32 data !at in
                if length = 0L then begin
                  for index = !at to s.size - 1 do
                    if B.u8 data index <> 0 then
                      fail "nonzero bytes after unwind terminator"
                  done;
                  finished := true
                end
                else begin
                  if length = 0xffffffffL then
                    fail "64-bit DWARF record lengths are unsupported";
                  if
                    length < 4L
                    || length > Int64.of_int (s.size - !at - 4)
                    || Int64.rem length 4L <> 0L
                  then fail "invalid record length at 0x%x" !at;
                  let ending = !at + 4 + Int64.to_int length in
                  let id = B.u32 data (!at + 4) in
                  if id = 0L then begin
                    let cursor = ref (!at + 8) in
                    let version = byte !cursor ending in
                    incr cursor;
                    if version <> 1 && version <> 3 && version <> 4 then
                      fail "unsupported CIE version %d" version;
                    let augmentation_start = !cursor in
                    while byte !cursor ending <> 0 do
                      if !cursor - augmentation_start >= 32 then
                        fail "oversized CIE augmentation string";
                      incr cursor
                    done;
                    let augmentation =
                      String.sub data augmentation_start (!cursor - augmentation_start)
                    in
                    incr cursor;
                    if augmentation <> "" && augmentation.[0] <> 'z' then
                      fail "unsupported CIE augmentation %S" augmentation;
                    if version = 4 then begin
                      if byte !cursor ending <> 8 || byte (!cursor + 1) ending <> 0 then
                        fail "unsupported CIE address or segment size";
                      cursor := !cursor + 2
                    end;
                    ignore (leb cursor ending);
                    ignore (leb cursor ending);
                    if version = 1 then begin
                      ignore (byte !cursor ending);
                      incr cursor
                    end
                    else ignore (leb cursor ending);
                    let encoding = ref 0 and lsda = ref 0xff and metadata = ref [] in
                    if augmentation <> "" then begin
                      let ending = augmentation_end cursor ending in
                      let seen = Hashtbl.create 4 in
                      String.iteri
                        (fun n code ->
                          if n <> 0 then begin
                            if Hashtbl.mem seen code then
                              fail "duplicate CIE augmentation";
                            Hashtbl.add seen code ();
                            match code with
                            | 'P' ->
                                let value = byte !cursor ending in
                                incr cursor;
                                Option.iter
                                  (fun index -> metadata := index :: !metadata)
                                  (pointer ~indirect:true value cursor ending)
                            | 'L' ->
                                lsda := byte !cursor ending;
                                incr cursor;
                                if
                                  not
                                    (List.mem !lsda
                                       [ 0xff; 0; 3; 4; 11; 12; 0x1b; 0x1c ])
                                then fail "unsupported LSDA pointer encoding 0x%x" !lsda
                            | 'R' ->
                                encoding := byte !cursor ending;
                                incr cursor
                            | 'S' -> ()
                            | _ -> fail "unsupported CIE augmentation %S" augmentation
                          end)
                        augmentation;
                      if !cursor <> ending then fail "invalid CIE augmentation length"
                    end;
                    if !encoding <> 0 && !encoding <> 0x1b && !encoding <> 0x1c then
                      fail "unsupported FDE pointer encoding 0x%x" !encoding;
                    Hashtbl.add cies !at
                      (!encoding, augmentation <> "", !lsda, !metadata)
                  end
                  else begin
                    let cie = Int64.sub (Int64.of_int (!at + 4)) id in
                    if cie < 0L || cie > Int64.of_int !at then
                      fail "invalid FDE CIE pointer";
                    let encoding, augmented, lsda, cie_metadata =
                      match Hashtbl.find_opt cies (Int64.to_int cie) with
                      | Some cie -> cie
                      | None -> fail "FDE references a missing CIE"
                    in
                    let width = if encoding = 0x1b then 4 else 8 in
                    let initial = !at + 8 in
                    if width * 2 > ending - initial then fail "truncated FDE fields";
                    let relocation =
                      match Hashtbl.find_opt relocated initial with
                      | Some index -> index
                      | None -> fail "FDE initial location needs a relocation"
                    in
                    let expected =
                      if encoding = 0 then 1 else if encoding = 0x1b then 2 else 24
                    in
                    if o.relocations.(relocation).kind <> expected then
                      fail "FDE relocation does not match its pointer encoding";
                    Hashtbl.add consumed initial ();
                    let range =
                      if width = 4 then B.u32 data (initial + width)
                      else B.u64 data (initial + width)
                    in
                    if range < 0L then fail "FDE address range exceeds supported limit";
                    let metadata = ref cie_metadata in
                    if augmented then begin
                      let cursor = ref (initial + (width * 2)) in
                      let ending = augmentation_end cursor ending in
                      if lsda <> 0xff then
                        Option.iter
                          (fun index -> metadata := index :: !metadata)
                          (pointer ~indirect:false lsda cursor ending);
                      if !cursor <> ending then fail "invalid FDE augmentation length"
                    end;
                    frames :=
                      {
                        file;
                        section = s.index;
                        offset = !at;
                        initial;
                        encoding;
                        relocation;
                        range;
                        metadata = Array.of_list !metadata;
                      }
                      :: !frames
                  end;
                  last := Some (!at, Int64.to_int length);
                  at := ending
                end
              done;
              Array.iter
                (fun (r : E.relocation) ->
                  if
                    r.target = s.index && r.kind <> 0
                    && not (Hashtbl.mem consumed r.offset)
                  then fail "unsupported relocation outside an unwind pointer field")
                o.relocations;
              let padding = (8 - (!at mod 8)) mod 8 in
              let normalized = Bytes.make (!at + padding) '\000' in
              Bytes.blit_string data 0 normalized 0 !at;
              Option.iter
                (fun (offset, length) ->
                  B.set_int normalized offset 4 (length + padding))
                !last;
              Array.iter
                (fun (symbol : E.symbol) ->
                  if
                    symbol.section = s.index
                    && Int64.add symbol.value symbol.size
                       > Int64.of_int (Bytes.length normalized)
                  then
                    fail "symbol %s extends into removed unwind terminator" symbol.name)
                o.symbols;
              sections.(s.index) <-
                {
                  s with
                  offset = String.length o.data + Buffer.length appended;
                  size = Bytes.length normalized;
                  alignment = 8;
                };
              Buffer.add_bytes appended normalized
            end)
          o.sections;
        if not !changed then o
        else { o with sections; data = o.data ^ Buffer.contents appended })
      resolved.objects
  in
  Stats.add stats "unwind_fdes" (List.length !frames);
  ({ resolved with objects }, Array.of_list (List.rev !frames))

let prepare ?stats ~kept ~frames (resolved : Resolver.t) =
  let active (frame : frame) =
    if not kept.(frame.file).(frame.section) then false
    else
      let o = resolved.objects.(frame.file) in
      let relocation = o.relocations.(frame.relocation) in
      let s = o.symbols.(relocation.symbol) in
      let definition =
        if s.binding = 0 then Some (frame.file, s)
        else
          Option.map
            (fun (d : Resolver.resolved) ->
              (d.file, resolved.objects.(d.file).symbols.(d.symbol)))
            (Hashtbl.find_opt resolved.definitions s.name)
      in
      match definition with
      | None -> s.binding <> 2
      | Some (_, s) when s.section = 0xfff1 -> frame.range <> 0L
      | Some (file, s) ->
          if s.section = 0 || (not kept.(file).(s.section)) || frame.range = 0L then
            false
          else begin
            let target = resolved.objects.(file).sections.(s.section) in
            let offset = Int64.add s.value relocation.addend in
            if
              target.flags land 6 <> 6
              || offset < 0L
              || offset > Int64.of_int target.size
              || frame.range > Int64.sub (Int64.of_int target.size) offset
            then
              Error.fail "%s: .eh_frame: FDE code range exceeds executable section"
                o.path;
            true
          end
  in
  let frames = Array.of_list (Array.to_list frames |> List.filter active) in
  if Array.length frames = 0 then (resolved, kept, { frames; header_file = None })
  else begin
    let size = 12 + (Array.length frames * 8) in
    let section : E.section =
      {
        index = 1;
        name = ".eh_frame_hdr";
        kind = 1;
        flags = 2;
        offset = 0;
        size;
        link = 0;
        info = 0;
        alignment = 4;
        entsize = 0;
      }
    in
    let null : E.symbol =
      {
        name = "";
        binding = 0;
        kind = 0;
        visibility = 0;
        section = 0;
        value = 0L;
        size = 0L;
      }
    in
    let object_ : E.t =
      {
        path = "<unwind header>";
        data = String.make size '\000';
        sections =
          [|
            { section with index = 0; name = ""; kind = 0; flags = 0; size = 0 };
            section;
          |];
        symbols = [| null |];
        relocations = [||];
        groups = [||];
      }
    in
    let file = Array.length resolved.objects in
    Stats.add stats "indexed_unwind_fdes" (Array.length frames);
    ( {
        resolved with
        objects = Array.append resolved.objects [| object_ |];
        discarded = Array.append resolved.discarded [| [| false; false |] |];
      },
      Array.append kept [| [| false; true |] |],
      { frames; header_file = Some file } )
  end

let emit ~out ~placements ~objects unwind =
  Option.iter
    (fun file ->
      let header : Layout.placement = Option.get placements.(file).(1) in
      let entries =
        Array.map
          (fun (frame : frame) ->
            let placement : Layout.placement =
              Option.get placements.(frame.file).(frame.section)
            in
            let field = placement.offset + frame.initial in
            let raw =
              if frame.encoding = 0x1b then
                Int64.of_int32
                  (Int64.to_int32 (B.u32 (Bytes.unsafe_to_string out) field))
              else B.u64 (Bytes.unsafe_to_string out) field
            in
            let address =
              if frame.encoding = 0 then raw
              else Int64.add raw (Int64.of_int (placement.address + frame.initial))
            in
            (address, placement.address + frame.offset))
          unwind.frames
      in
      Array.sort (fun (a, _) (b, _) -> Int64.unsigned_compare a b) entries;
      let relative at value place =
        X86_64.apply ~context:"unwind header" out ~offset:at ~kind:2 ~symbol:value
          ~addend:0L ~place:(Int64.of_int place)
      in
      Bytes.blit_string "\001\027\003\059" 0 out header.offset 4;
      let first = ref max_int in
      Array.iteri
        (fun file (o : E.t) ->
          Array.iter
            (fun (s : E.section) ->
              if s.name = ".eh_frame" then
                Option.iter
                  (fun (p : Layout.placement) -> first := min !first p.address)
                  placements.(file).(s.index))
            o.sections)
        objects;
      relative (header.offset + 4) (Int64.of_int !first) (header.address + 4);
      B.set_int out (header.offset + 8) 4 (Array.length entries);
      Array.iteri
        (fun index (address, fde) ->
          relative (header.offset + 12 + (index * 8)) address header.address;
          relative (header.offset + 16 + (index * 8)) (Int64.of_int fde) header.address)
        entries)
    unwind.header_file
