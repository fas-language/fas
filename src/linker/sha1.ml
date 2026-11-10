(* SHA-1 for deterministic GNU build identifiers, not authentication.
   On the supported 64-bit host, 32-bit words fit in immediate OCaml integers.
   Mask additions and rotations to avoid allocating boxed Int32 words in the
   message schedule and compression loop. Five-word sums fit in 35 bits. *)
let digest bytes =
  let mask = 0xffff_ffff in
  let rotate x n = (x lsl n) lor (x lsr (32 - n)) land mask in
  let state = [| 0x67452301; 0xefcdab89; 0x98badcfe; 0x10325476; 0xc3d2e1f0 |] in
  let length = Bytes.length bytes in
  let blocks = (length + 9 + 63) / 64 in
  let padded = blocks * 64 in
  let bit_length = Int64.mul (Int64.of_int length) 8L in
  let byte n =
    if n < length then Char.code (Bytes.get bytes n)
    else if n = length then 128
    else if n < padded - 8 then 0
    else
      Int64.to_int
        (Int64.logand 255L
           (Int64.shift_right_logical bit_length ((padded - 1 - n) * 8)))
  in
  let words = Array.make 80 0 in
  for block = 0 to blocks - 1 do
    if block < length / 64 then
      (* Complete input blocks need neither padding decisions nor four byte
         loads per word. Only the final one or two blocks use [byte] below. *)
      for n = 0 to 15 do
        words.(n) <-
          Int32.to_int (Bytes.get_int32_be bytes ((block * 64) + (n * 4))) land mask
      done
    else
      for n = 0 to 15 do
        let at = (block * 64) + (n * 4) in
        words.(n) <-
          (byte at lsl 24)
          lor (byte (at + 1) lsl 16)
          lor (byte (at + 2) lsl 8)
          lor byte (at + 3)
      done;
    for n = 16 to 79 do
      words.(n) <-
        rotate
          (words.(n - 3) lxor words.(n - 8) lxor words.(n - 14) lxor words.(n - 16))
          1
    done;
    let a = ref state.(0)
    and b = ref state.(1)
    and c = ref state.(2)
    and d = ref state.(3)
    and e = ref state.(4) in
    for n = 0 to 79 do
      let f, k =
        if n < 20 then (!b land !c lor (lnot !b land !d), 0x5a827999)
        else if n < 40 then (!b lxor !c lxor !d, 0x6ed9eba1)
        else if n < 60 then (!b land !c lor (!b land !d) lor (!c land !d), 0x8f1bbcdc)
        else (!b lxor !c lxor !d, 0xca62c1d6)
      in
      let next = (rotate !a 5 + f + !e + k + words.(n)) land mask in
      e := !d;
      d := !c;
      c := rotate !b 30;
      b := !a;
      a := next
    done;
    state.(0) <- (state.(0) + !a) land mask;
    state.(1) <- (state.(1) + !b) land mask;
    state.(2) <- (state.(2) + !c) land mask;
    state.(3) <- (state.(3) + !d) land mask;
    state.(4) <- (state.(4) + !e) land mask
  done;
  let result = Bytes.create 20 in
  Array.iteri
    (fun n word ->
      for i = 0 to 3 do
        Bytes.set result ((n * 4) + i) (Char.chr ((word lsr ((3 - i) * 8)) land 255))
      done)
    state;
  Bytes.unsafe_to_string result
