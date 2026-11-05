let name = function
  | 0 -> "R_X86_64_NONE"
  | 1 -> "R_X86_64_64"
  | 2 -> "R_X86_64_PC32"
  | 4 -> "R_X86_64_PLT32"
  | 9 -> "R_X86_64_GOTPCREL"
  | 17 -> "R_X86_64_DTPOFF64"
  | 19 -> "R_X86_64_TLSGD"
  | 20 -> "R_X86_64_TLSLD"
  | 21 -> "R_X86_64_DTPOFF32"
  | 22 -> "R_X86_64_GOTTPOFF"
  | 23 -> "R_X86_64_TPOFF32"
  | 24 -> "R_X86_64_PC64"
  | 26 -> "R_X86_64_GOTPC32"
  | 29 -> "R_X86_64_GOTPC64"
  | 34 -> "R_X86_64_GOTPC32_TLSDESC"
  | 35 -> "R_X86_64_TLSDESC_CALL"
  | 41 -> "R_X86_64_GOTPCRELX"
  | 42 -> "R_X86_64_REX_GOTPCRELX"
  | 10 -> "R_X86_64_32"
  | 11 -> "R_X86_64_32S"
  | n -> Printf.sprintf "x86-64 relocation %d" n

let apply_lazy ~context bytes ~offset ~kind ~symbol ~addend ~place =
  match Elf_reader.relocation_width kind with
  | None -> Error.fail "%s: unsupported %s" (context ()) (name kind)
  | Some width ->
      let length = Bytes.length bytes in
      if offset < 0 || offset > length || width > length - offset then
        Binary.span ~context:(context ()) ~length offset width;
      if kind <> 0 && kind <> 35 then begin
        let value = Int64.add symbol addend in
        let value =
          if
            kind = 2 || kind = 4 || kind = 24 || Got.relocation kind
            || Got.base_relocation kind
          then Int64.sub value place
          else value
        in
        let fits =
          match kind with
          | 1 | 17 | 24 | 29 -> true
          | 10 -> value >= 0L && value <= 0xffff_ffffL
          | _ -> value >= -0x8000_0000L && value <= 0x7fff_ffffL
        in
        if not fits then
          Error.fail "%s: %s overflow: result 0x%Lx" (context ()) (name kind) value;
        Binary.set bytes offset width value
      end

let apply ~context = apply_lazy ~context:(fun () -> context)
