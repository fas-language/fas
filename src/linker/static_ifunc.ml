(* Static executables have no ELF interpreter. Their startup code must apply
   IRELATIVE records before calling an IFUNC or using its canonical IPLT address. *)
module E = Elf_reader
module B = Binary

type t = {
  file : int;
  symbols : (int * int) array;
  indices : (int * int, int) Hashtbl.t;
}

let prepare ?stats ~(options : Options.t) ~kept (resolved : Resolver.t) =
  if options.mode <> Options.Static then (resolved, kept, None)
  else begin
    let symbols = ref [] in
    Array.iteri
      (fun file (o : E.t) ->
        Array.iteri
          (fun index (s : E.symbol) ->
            if s.kind = 10 && s.section >= 0xff00 then
              Error.fail
                "%s: IFUNC resolver %s must be defined in an executable section" o.path
                s.name;
            if
              s.kind = 10 && s.section <> 0 && s.section < 0xff00
              && kept.(file).(s.section)
              && Got.definition resolved file index = Some (file, index)
            then begin
              let section = o.sections.(s.section) in
              if section.flags land 6 <> 6 || s.value >= Int64.of_int section.size then
                Error.fail "%s: IFUNC resolver %s must be in executable code" o.path
                  s.name;
              symbols := (file, index) :: !symbols
            end)
          o.symbols)
      resolved.objects;
    let referenced = Hashtbl.create ~random:true 32 in
    (* Most static links have no IFUNC definitions. Do not scan their relocation
       arrays merely to produce empty resolver tables. *)
    if !symbols <> [] then
      Array.iteri
        (fun file (o : E.t) ->
          Array.iter
            (fun (r : E.relocation) ->
              if
                r.kind <> 0
                && kept.(file).(r.target)
                && o.sections.(r.target).flags land 2 <> 0
                && o.sections.(r.target).name <> ".eh_frame"
                && Got.ifunc resolved file r.symbol
              then
                Option.iter
                  (fun key -> Hashtbl.replace referenced key ())
                  (Got.definition resolved file r.symbol))
            o.relocations)
        resolved.objects;
    let symbols =
      List.rev !symbols |> List.filter (Hashtbl.mem referenced) |> Array.of_list
    in
    let count = Array.length symbols in
    if count = 0 then (resolved, kept, None)
    else begin
      let indices = Hashtbl.create ~random:true count in
      Array.iteri (fun index key -> Hashtbl.add indices key index) symbols;
      let bytes =
        B.add ~context:"static IFUNC tables" ~limit:(Layout.max_output / 48) 0 count
        * 48
      in
      let section index name kind flags offset size alignment entsize : E.section =
        {
          index;
          name;
          kind;
          flags;
          offset;
          size;
          alignment;
          entsize;
          link = 0;
          info = 0;
        }
      in
      let object_ : E.t =
        {
          path = "<static IFUNC>";
          data = String.make bytes '\000';
          sections =
            [|
              section 0 "" 0 0 0 0 1 0;
              section 1 ".iplt" 1 6 0 (16 * count) 16 0;
              section 2 ".got.iplt" 1 3 (16 * count) (8 * count) 8 8;
              section 3 ".rela.iplt" 4 2 (24 * count) (24 * count) 8 24;
            |];
          symbols =
            [|
              {
                E.name = "";
                binding = 0;
                kind = 0;
                visibility = 0;
                section = 0;
                value = 0L;
                size = 0L;
              };
            |];
          relocations = [||];
          groups = [||];
        }
      in
      let file = Array.length resolved.objects in
      Stats.add stats "static_ifunc_resolvers" count;
      ( {
          resolved with
          objects = Array.append resolved.objects [| object_ |];
          discarded =
            Array.append resolved.discarded [| [| false; false; false; false |] |];
        },
        Array.append kept [| [| false; true; true; true |] |],
        Some { file; symbols; indices } )
    end
  end

let stub ~placements ~resolved t file symbol =
  Option.bind t (fun t ->
      Option.bind (Got.definition resolved file symbol) (fun key ->
          Option.map
            (fun index ->
              let p : Layout.placement = Option.get placements.(t.file).(1) in
              p.address + (index * 16))
            (Hashtbl.find_opt t.indices key)))

let boundary ~placements t last =
  match t with
  | None -> 0
  | Some t ->
      let p : Layout.placement = Option.get placements.(t.file).(3) in
      p.address + if last then Array.length t.symbols * 24 else 0

let emit ~out ~placements ~values t =
  Option.iter
    (fun t ->
      let p section : Layout.placement = Option.get placements.(t.file).(section) in
      Array.iteri
        (fun index (file, symbol) ->
          let stub = (p 1).offset + (index * 16) in
          let address = (p 1).address + (index * 16) in
          Bytes.blit_string "\243\015\030\250\255\037" 0 out stub 6;
          X86_64.apply ~context:"static IPLT" out ~offset:(stub + 6) ~kind:2
            ~symbol:(Int64.of_int ((p 2).address + (index * 8)))
            ~addend:(-4L)
            ~place:(Int64.of_int (address + 6));
          Bytes.blit_string "\102\015\031\068\000\000" 0 out (stub + 10) 6;
          let relocation = (p 3).offset + (index * 24) in
          B.set_int out relocation 8 ((p 2).address + (index * 8));
          B.set_int out (relocation + 8) 8 37;
          match values.(file).(symbol) with
          | Some resolver -> B.set out (relocation + 16) 8 resolver
          | None -> Error.fail "static IFUNC resolver has no live address")
        t.symbols)
    t
