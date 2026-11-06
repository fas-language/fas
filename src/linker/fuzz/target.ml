(* Only expected input errors are swallowed: uncaught runtime exceptions must be
   visible to AFL. Script fuzzing parses names without opening arbitrary paths. *)
open Fas_linker

let copy_link path data =
  let library = Shared_object.parse ~path data in
  if Array.length library.exports <= 65536 then begin
    let data_symbols =
      Array.to_list library.exports
      |> List.filter (fun (s : Shared_object.symbol) ->
          s.kind = 1 && s.size >= 0L && s.size <= 1048576L)
      |> List.to_seq |> Seq.take 32 |> Array.of_seq
    in
    let key (s : Shared_object.symbol) =
      match s.version with
      | Some version when not s.default -> s.name ^ "@" ^ version
      | _ -> s.name
    in
    let code = Bytes.make (4 + (4 * Array.length data_symbols)) '\000' in
    Bytes.set code 0 '\195';
    let symbol name kind definition : Elf_object.symbol =
      { name; kind; definition; binding = 1; visibility = 0; size = 0L }
    in
    let consumer =
      Elf_object.write
        {
          sections =
            [| { name = ".text"; flags = 6; alignment = 4; contents = Data code } |];
          symbols =
            Array.append
              [| symbol "_start" 2 (Section (0, 0L)) |]
              (Array.map (fun s -> symbol (key s) 1 Undefined) data_symbols);
          relocations =
            Array.mapi
              (fun n _ ->
                {
                  Elf_object.section = 0;
                  offset = 4 + (n * 4);
                  symbol = n + 1;
                  kind = 2;
                  addend = -4L;
                })
              data_symbols;
        }
      |> Bytes.to_string
    in
    List.iter
      (fun pie ->
        let options =
          {
            Options.default with
            mode = Dynamic { pie; interpreter = "/unused/fuzz-loader" };
          }
        in
        let resolved =
          Resolver.resolve ~options ~entry:"_start"
            [ Input.Object { path = "<COPY consumer>"; data = consumer } ]
        in
        Array.iter
          (fun (s : Shared_object.symbol) ->
            Hashtbl.replace resolved.imports (key s)
              { Resolver.library = Some 0; symbol = s; visibility = 0 })
          data_symbols;
        let resolved = { resolved with shared = [| library |] } in
        let kept =
          Array.map
            (fun (o : Elf_reader.t) -> Array.map (fun _ -> true) o.sections)
            resolved.objects
        in
        let resolved, kept, copies = Copy_relocation.prepare ~options ~kept resolved in
        let resolved, kept, got = Got.prepare ~kept resolved in
        ignore (Dynamic.prepare ~options ~kept ~got ~module_base:None ~copies resolved))
      [ false; true ]
  end

let () =
  if Array.length Sys.argv <> 3 then invalid_arg "target MODE FILE";
  let data =
    let channel = open_in_bin Sys.argv.(2) in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let length = in_channel_length channel in
        if length > 1024 * 1024 then exit 0;
        really_input_string channel length)
  in
  let path = "fuzz-input" in
  try
    match Sys.argv.(1) with
    | "zlib" ->
        if String.length data >= 4 then begin
          let size = Binary.u32 data 0 in
          if size <= 1048576L then
            ignore
              (Zlib_decode.decode ~context:path ~size:(Int64.to_int size) data ~offset:4
                 ~length:(String.length data - 4))
        end
    | "elf" -> ignore (Elf_reader.parse ~path data)
    | "shared" -> ignore (Shared_object.parse ~path data)
    | "copy-link" -> copy_link path data
    | "dependency" ->
        let library = Shared_object.parse ~path data in
        if Array.length library.exports + Array.length library.references <= 65536 then begin
          let names = Hashtbl.create 16 in
          Hashtbl.add names (library.path, library.needed) library;
          Array.iter
            (fun name -> Hashtbl.replace names (library.path, name) library)
            library.dependencies;
          Shared_dependencies.validate
            ~defined:(fun _ _ -> None)
            { libraries = [| library |]; names }
        end
    | "archive" ->
        let archive = Archive.parse ~path data in
        Array.iter
          (fun (member : Archive.member) ->
            if member.external_member = None then
              ignore
                (Elf_reader.parse ~path (String.sub data member.offset member.size)))
          archive.members
    | "script" -> ignore (Linker_script.parse ~path data)
    | "version" ->
        let script = Version_script.parse ~path data in
        let policy = Version_script.policy script in
        List.iter
          (fun name -> ignore (policy name))
          [ "answer"; "datum"; "private_symbol" ];
        ignore
          (Static_link.link_inputs
             ~options:
               { Options.default with mode = Shared; version_script = Some script }
             [
               Input.Object
                 {
                   path = "empty-object";
                   data =
                     Bytes.to_string
                       (Elf_object.write
                          { sections = [||]; symbols = [||]; relocations = [||] });
                 };
             ])
    | "link" | "shared-link" | "symver-link" | "metadata-link" | "partial-link"
    | "static-pie-link" ->
        (* Constrain synthetic section sizes before allocating output. Parser
           fuzzing above retains the production limits without this restriction. *)
        let obj = Elf_reader.parse ~path data in
        if
          Array.for_all
            (fun (section : Elf_reader.section) -> section.size <= 1024 * 1024)
            obj.sections
          && Array.length obj.sections <= 256
          && Array.for_all
               (fun (symbol : Elf_reader.symbol) ->
                 symbol.section <> 0xfff2 || symbol.size <= 1048576L)
               obj.symbols
        then
          if Sys.argv.(1) = "partial-link" then
            List.iter
              (fun options ->
                ignore
                  (Partial_link.link_inputs ~options [ Input.Object { path; data } ]))
              [ Options.default;
                { Options.default with undefined = [ "__fas_fuzz_required_root" ] } ]
          else
            List.iter
              (fun options ->
                ignore
                  (Static_link.link_inputs ~options [ Input.Object { path; data } ]))
              (if Sys.argv.(1) = "metadata-link" then
                 let version_script =
                   Some
                     (Version_script.parse ~path:"<fuzz versions>"
                        "FAS_1 {}; FAS_2 {} FAS_1;")
                 in
                 List.concat_map
                   (fun hash_style ->
                     [
                       {
                         Options.default with
                         mode = Shared;
                         version_script;
                         hash_style;
                         build_id = Sha1;
                       };
                       {
                         Options.default with
                         mode = Shared;
                         version_script;
                         hash_style;
                         build_id = Md5;
                         gc_sections = true;
                       };
                     ])
                   [ Options.Gnu; Both ]
               else if Sys.argv.(1) = "symver-link" then
                 let version_script =
                   Some
                     (Version_script.parse ~path:"<fuzz versions>"
                        "FAS_1 {}; FAS_2 {} FAS_1;")
                 in
                 [
                   { Options.default with mode = Shared; version_script };
                   {
                     Options.default with
                     mode = Shared;
                     version_script;
                     gc_sections = true;
                   };
                 ]
               else if Sys.argv.(1) = "static-pie-link" then
                 [
                   { Options.default with mode = Static_pie };
                   { Options.default with mode = Static_pie; gc_sections = true };
                 ]
               else if Sys.argv.(1) = "shared-link" then
                 [
                   { Options.default with mode = Shared };
                   { Options.default with mode = Shared; gc_sections = true };
                 ]
               else [ Options.default; { Options.default with gc_sections = true } ])
    | _ -> invalid_arg "unknown fuzz mode"
  with Error.Error _ -> ()
