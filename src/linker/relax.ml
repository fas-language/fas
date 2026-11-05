(* Static PIE CRT entry runs before its GOT has been relocated. Convert only
   marked x86-64 GOTPCRELX instructions whose local target is known at link time.
   Keep instruction lengths and call return addresses unchanged. *)
module E = Elf_reader

let static_pie ?stats ~kept (resolved : Resolver.t) =
  let objects =
    Array.mapi
      (fun file (object_ : E.t) ->
        let intervals =
          Array.to_list object_.relocations
          |> List.filter_map (fun (r : E.relocation) ->
              match E.relocation_width r.kind with
              | Some size when size > 0 -> Some (r.target, r.offset, r.offset + size)
              | _ -> None)
          |> Array.of_list
        in
        Array.sort compare intervals;
        let protected section offset =
          let rec search low high =
            if low >= high then low
            else
              let middle = low + ((high - low) / 2) in
              let target, start, _ = intervals.(middle) in
              if (target, start) <= (section, offset) then search (middle + 1) high
              else search low middle
          in
          let at = search 0 (Array.length intervals) - 1 in
          at >= 0
          &&
          let target, start, finish = intervals.(at) in
          target = section && start <= offset && offset < finish
        in
        let data = ref None in
        let patch offset value =
          let bytes =
            match !data with
            | Some bytes -> bytes
            | None ->
                let bytes = Bytes.of_string object_.data in
                data := Some bytes;
                bytes
          in
          Bytes.set bytes offset (Char.chr value)
        in
        let relocations =
          Array.map
            (fun (r : E.relocation) ->
              let section = object_.sections.(r.target) in
              let symbol = object_.symbols.(r.symbol) in
              let definition =
                if symbol.binding = 0 then Some symbol
                else
                  Option.map
                    (fun (d : Resolver.resolved) ->
                      resolved.objects.(d.file).symbols.(d.symbol))
                    (Hashtbl.find_opt resolved.definitions symbol.name)
              in
              let local_address =
                match definition with
                | Some s ->
                    s.section > 0 && s.section < 0xff00 && s.kind <> 6 && s.kind <> 10
                | None -> false
              in
              let byte relative =
                Char.code object_.data.[section.offset + r.offset + relative]
              in
              if
                (r.kind = 41 || r.kind = 42)
                && r.addend = -4L
                && kept.(file).(r.target)
                && section.flags land 4 <> 0
                && local_address && r.offset >= 2
                && (not (protected r.target (r.offset - 2)))
                && not (protected r.target (r.offset - 1))
              then
                let at = section.offset + r.offset in
                if
                  r.kind = 42 && r.offset >= 3
                  && (not (protected r.target (r.offset - 3)))
                  && byte (-3) land 0xf8 = 0x48
                  && byte (-2) = 0x8b
                  && byte (-1) land 0xc7 = 5
                then begin
                  patch (at - 2) 0x8d;
                  Stats.add stats "gotpcrelx_relaxations" 1;
                  { r with kind = 2 }
                end
                else if r.kind = 41 && byte (-2) = 0xff && byte (-1) = 0x15 then begin
                  patch (at - 2) 0x67;
                  patch (at - 1) 0xe8;
                  Stats.add stats "gotpcrelx_relaxations" 1;
                  { r with kind = 2 }
                end
                else r
              else r)
            object_.relocations
        in
        match !data with
        | None -> object_
        | Some data -> { object_ with data = Bytes.unsafe_to_string data; relocations })
      resolved.objects
  in
  { resolved with objects }
