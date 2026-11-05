open Fas_linker

let word () =
  let value = ref 0 in
  for i = 0 to 3 do
    value := !value lor (input_byte stdin lsl (i * 8))
  done;
  !value

let output_word n =
  for i = 0 to 3 do
    output_byte stdout ((n lsr (i * 8)) land 255)
  done

let () =
  try
    while true do
      let size = word () in
      let length = word () in
      if length > 1024 * 1024 then failwith "test stream too large";
      let data = really_input_string stdin length in
      try
        let decoded = Zlib_decode.decode ~context:"test" ~size data ~offset:0 ~length in
        output_word (String.length decoded);
        output_string stdout decoded
      with Error.Error _ -> output_word 0xffff_ffff
    done
  with End_of_file -> ()
