module E = Elf_reader

(* Whole-section equality is a conservative first merge pass. Offsets remain
   linear, including section-symbol addends and interior/end pointers. Relocated
   and writable contents are retained separately; equal bytes alone cannot prove
   that their values at run time are equal. *)
let plan ?stats ~kept objects =
  let aliases =
    Array.map (fun (o : E.t) -> Array.make (Array.length o.sections) None) objects
  in
  let seen = Hashtbl.create ~random:true 64 in
  Array.iteri
    (fun file (o : E.t) ->
      let relocated = Array.make (Array.length o.sections) false in
      Array.iter
        (fun (r : E.relocation) -> if r.kind <> 0 then relocated.(r.target) <- true)
        o.relocations;
      Array.iter
        (fun (s : E.section) ->
          if
            kept.(file).(s.index)
            && s.kind = 1
            && s.flags land 0x11 = 0x10
            && s.size <> 0
            (* Registry iteration observes each contribution, even equal bytes. *)
            && (not (Section_registry.eligible s))
            && not relocated.(s.index)
          then begin
            let key =
              ( s.name,
                s.flags land lnot 0x200,
                s.entsize,
                s.alignment,
                String.sub o.data s.offset s.size )
            in
            match Hashtbl.find_opt seen key with
            | None -> Hashtbl.add seen key (file, s.index)
            | Some canonical ->
                aliases.(file).(s.index) <- Some canonical;
                Stats.add stats "merged_sections" 1;
                Stats.add stats "merged_bytes" s.size
          end)
        o.sections)
    objects;
  aliases
