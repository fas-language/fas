open Fas_linker
module O = Elf_object

let symbol ?(binding = 1) ?(kind = 0) ?(size = 0L) name definition : O.symbol =
  { name; binding; kind; visibility = 0; definition; size }

let start : O.t =
  {
    sections =
      [|
        {
          name = ".text";
          flags = 6;
          alignment = 16;
          contents =
            Data
              (Bytes.of_string
                 "\232\000\000\000\000\137\199\184\060\000\000\000\015\005");
        };
        {
          name = ".data";
          flags = 3;
          alignment = 8;
          contents = Data (Bytes.make 24 '\000');
        };
        { name = ".bss"; flags = 3; alignment = 128; contents = Bss 128 };
        {
          name = ".note.GNU-stack";
          flags = 0;
          alignment = 1;
          contents = Data Bytes.empty;
        };
      |];
    symbols =
      [|
        symbol ~kind:2 ~size:14L "_start" (Section (0, 0L));
        symbol "answer" Undefined;
        symbol ~binding:0 ~kind:1 ~size:8L "local" (Section (1, 0L));
        symbol ~kind:1 ~size:128L "scratch" (Section (2, 0L));
        symbol "negative" (Absolute (-7L));
      |];
    relocations =
      [|
        { section = 0; offset = 1; symbol = 1; kind = 2; addend = -4L };
        { section = 1; offset = 0; symbol = 1; kind = 1; addend = 3L };
        { section = 1; offset = 8; symbol = 3; kind = 1; addend = 4L };
        { section = 1; offset = 16; symbol = 4; kind = 1; addend = 7L };
      |];
  }

let support : O.t =
  {
    sections =
      [|
        {
          name = ".text";
          flags = 6;
          alignment = 16;
          contents = Data (Bytes.of_string "\184\042\000\000\000\195");
        };
        {
          name = ".note.GNU-stack";
          flags = 0;
          alignment = 1;
          contents = Data Bytes.empty;
        };
      |];
    symbols = [| symbol ~kind:2 ~size:6L "answer" (Section (0, 0L)) |];
    relocations = [||];
  }

let () =
  let directory = Sys.argv.(1) in
  let write name data =
    let channel = open_out_bin (Filename.concat directory name) in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_bytes channel data)
  in
  let start_bytes = O.write start and support_bytes = O.write support in
  write "emitted-start.o" start_bytes;
  write "emitted-support.o" support_bytes;
  let inputs =
    [
      Input.Object { path = "<start>"; data = Bytes.to_string start_bytes };
      Input.Object { path = "<support>"; data = Bytes.to_string support_bytes };
    ]
  in
  Static_link.link_input_files
    ~output:(Filename.concat directory "emitted-memory")
    inputs;
  let expected = Static_link.link_inputs inputs in
  let check () = assert (Static_link.link_inputs inputs = expected) in
  List.init 2 (fun _ -> Domain.spawn check) |> List.iter Domain.join;
  List.iter
    (fun invalid ->
      match O.write invalid with
      | _ -> failwith "invalid emitted object accepted"
      | exception Error.Error _ -> ())
    [
      { start with relocations = [| { (start.relocations.(0)) with offset = 1000 } |] };
      { start with relocations = [| { (start.relocations.(0)) with symbol = 1000 } |] };
      { start with relocations = [| { (start.relocations.(0)) with section = -1 } |] };
      {
        start with
        symbols = [| symbol "bad" (Section (0, 1000L)) |];
        relocations = [||];
      };
    ];
  print_endline
    "ELF object writer: checked emission and concurrent in-memory links passed"
