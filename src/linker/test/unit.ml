open Fas_linker

let rejected f =
  match f () with
  | () -> failwith "expected a linker diagnostic"
  | exception Error.Error _ -> ()

let relocation kind symbol addend place expected =
  let bytes = Bytes.make 12 '\165' in
  X86_64.apply ~context:"unit" bytes ~offset:2 ~kind ~symbol ~addend ~place;
  assert (Bytes.sub_string bytes 2 (String.length expected) = expected);
  assert (Bytes.sub_string bytes 0 2 = "\165\165");
  for index = 2 + String.length expected to 11 do
    assert (Bytes.get bytes index = '\165')
  done

let overflow kind symbol addend place =
  let bytes = Bytes.make 8 '\165' in
  rejected (fun () ->
      X86_64.apply ~context:"unit" bytes ~offset:0 ~kind ~symbol ~addend ~place);
  assert (bytes = Bytes.make 8 '\165')

let () =
  List.iter
    (fun (length, expected) ->
      let digest = Sha1.digest (Bytes.make length 'a') in
      let hex =
        String.to_seq digest
        |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
        |> List.of_seq |> String.concat ""
      in
      assert (hex = expected))
    [
      (0, "da39a3ee5e6b4b0d3255bfef95601890afd80709");
      (1, "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8");
      (55, "c1c8bbdc22796e28c0e15163d20899b65621d65a");
      (56, "c2db330f6083854c99d4b5bfb6e8f29f201be699");
      (63, "03f09f5b158a7a8cdad920bddc29b81c18a551f5");
      (64, "0098ba824b5c16427bd7a1122a5a442a25ec644d");
      (65, "11655326c708d70319be2610e8a57d9a5b959d3b");
      (1_000_000, "34aa973cd4c4daa4f61eeb2bdbad27316534016f");
    ];
  (* Independent hashlib vectors: all byte values, wrapping additions and
     both sides of SHA-1's one/two-block padding boundaries. *)
  List.iter
    (fun (length, expected) ->
      let input =
        Bytes.init length (fun i ->
            Char.chr (((i * 197) + (i / 7 * 31) + 128) land 255))
      in
      let digest = Sha1.digest input in
      let hex =
        String.to_seq digest
        |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
        |> List.of_seq |> String.concat ""
      in
      assert (hex = expected))
    [
      (2, "f8556eaf16ebf49f8c35467a7515d9e86713b712");
      (55, "67c99092ae33f8fe6f758d86ffae3e3c03de7657");
      (56, "871678b25b88d2c2002c17a162f2ee578a0e94bf");
      (63, "2b4854216054db8e9debc9e905db32cdaefb2d0e");
      (64, "d13c70a561483b8417b46b2a3147d8fa2612fba6");
      (65, "c68f2017c573094d4978d0795fd8d2e2b86ba845");
      (119, "f6d5a51b78f1df628c3609ee2903af7720afafd9");
      (120, "7444e27a57d6bf2c588510b64c25b2ea37415e22");
      (127, "09128d67047a5d173569242be722161c754a8166");
      (128, "61605dac973c67cce695bc284a9ddbc946b0d2ce");
      (129, "32b0d6f84f80e7451005db7e1fb0012ca718e7b8");
      (256, "bd65ecc1ceddfa0c0e13275a5b3ac6917c75fef5");
      (4097, "7e9ad3cc14deb00e468aa7b4ce309b1908e7126d");
    ];
  relocation 1 0x1122334455667788L 0L 0L "\136\119\102\085\068\051\034\017";
  relocation 1 0L (-1L) 0L "\255\255\255\255\255\255\255\255";
  relocation 1 (-1L) 1L 0L "\000\000\000\000\000\000\000\000";
  relocation 10 0xffff_ffffL 0L 0L "\255\255\255\255";
  relocation 10 17L (-17L) 0L "\000\000\000\000";
  overflow 10 0x1_0000_0000L 0L 0L;
  overflow 10 0L (-1L) 0L;
  relocation 11 0x7fff_ffffL 0L 0L "\255\255\255\127";
  relocation 11 (-0x8000_0000L) 0L 0L "\000\000\000\128";
  overflow 11 0x8000_0000L 0L 0L;
  overflow 11 (-0x8000_0001L) 0L 0L;
  List.iter
    (fun kind ->
      relocation kind 0x8000_0fffL 0L 0x1000L "\255\255\255\127";
      relocation kind 0L 0L 0x8000_0000L "\000\000\000\128";
      relocation kind 0x401020L (-4L) 0x401001L "\027\000\000\000";
      overflow kind 0x8000_1000L 0L 0x1000L;
      overflow kind 0L (-1L) 0x8000_0000L)
    [ 2; 4 ];
  relocation 0 99L 17L 100L "";
  rejected (fun () ->
      X86_64.apply ~context:"bounds" (Bytes.create 4) ~offset:1 ~kind:2 ~symbol:0L
        ~addend:0L ~place:0L);
  rejected (fun () ->
      X86_64.apply ~context:"unsupported" (Bytes.create 8) ~offset:0 ~kind:99 ~symbol:0L
        ~addend:0L ~place:0L);
  rejected (fun () -> ignore (Binary.index ~context:"index" ~limit:max_int (-1L)));
  rejected (fun () -> ignore (Binary.add ~context:"add" ~limit:max_int max_int 1));
  rejected (fun () ->
      ignore (Binary.align ~context:"align" ~limit:max_int max_int 4096));
  rejected (fun () -> ignore (Binary.align ~context:"align" ~limit:4096 1 3));
  assert (Binary.align ~context:"align" ~limit:4096 1 4096 = 4096);
  rejected (fun () ->
      ignore (Binary.string_at ~context:"string" "\000abc" ~offset:0 ~size:4 1));
  rejected (fun () ->
      ignore (Binary.string_at ~context:"string" "abc\000" ~offset:0 ~size:3 0));
  List.iter
    (fun s -> rejected (fun () -> ignore (Elf_reader.parse ~path:"fixture" s)))
    [ ""; "\127ELF"; String.make 64 '\000' ];
  print_endline "linker unit tests: relocation limits, arithmetic, and bounds passed"
