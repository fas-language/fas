module E = Elf_reader

let prepare ~(options : Options.t) ~kept (resolved : Resolver.t) =
  match options.build_id with
  | No_id -> (resolved, kept, None)
  | (Sha1 | Md5) as style ->
      let size = if style = Options.Sha1 then 20 else 16 in
      let data = Bytes.make (16 + size) '\000' in
      Binary.set_int data 0 4 4;
      Binary.set_int data 4 4 size;
      Binary.set_int data 8 4 3;
      Bytes.blit_string "GNU\000" 0 data 12 4;
      let section : E.section =
        {
          index = 1;
          name = ".note.gnu.build-id";
          kind = 7;
          flags = 2;
          offset = 0;
          size = Bytes.length data;
          link = 0;
          info = 0;
          alignment = 4;
          entsize = 0;
        }
      in
      let object_ : E.t =
        {
          path = "<build identifier>";
          data = Bytes.unsafe_to_string data;
          sections =
            [|
              { section with index = 0; name = ""; kind = 0; flags = 0; size = 0 };
              section;
            |];
          symbols = [||];
          relocations = [||];
          groups = [||];
        }
      in
      let file = Array.length resolved.objects in
      ( {
          resolved with
          objects = Array.append resolved.objects [| object_ |];
          discarded = Array.append resolved.discarded [| [| false; false |] |];
        },
        Array.append kept [| [| false; true |] |],
        Some file )

let emit ~style ~offset output =
  let digest =
    match style with
    | Options.No_id -> assert false
    | Sha1 -> Sha1.digest output
    | Md5 -> Digest.string (Bytes.unsafe_to_string output)
  in
  Bytes.blit_string digest 0 output (offset + 16) (String.length digest)
