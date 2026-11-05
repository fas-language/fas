(* SHA-1 for deterministic GNU build identifiers, not authentication. *)
let digest bytes =
  let open Int32 in
  let rotate x n = logor (shift_left x n) (shift_right_logical x (32 - n)) in
  let state = [| 0x67452301l; 0xefcdab89l; 0x98badcfel; 0x10325476l; 0xc3d2e1f0l |] in
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
  let words = Array.make 80 0l in
  for block = 0 to blocks - 1 do
    for n = 0 to 15 do
      let at = (block * 64) + (n * 4) in
      words.(n) <-
        logor
          (shift_left (of_int (byte at)) 24)
          (logor
             (shift_left (of_int (byte (at + 1))) 16)
             (logor (shift_left (of_int (byte (at + 2))) 8) (of_int (byte (at + 3)))))
    done;
    for n = 16 to 79 do
      words.(n) <-
        rotate
          (logxor
             words.(n - 3)
             (logxor words.(n - 8) (logxor words.(n - 14) words.(n - 16))))
          1
    done;
    let a = ref state.(0)
    and b = ref state.(1)
    and c = ref state.(2)
    and d = ref state.(3)
    and e = ref state.(4) in
    for n = 0 to 79 do
      let f, k =
        if n < 20 then (logor (logand !b !c) (logand (lognot !b) !d), 0x5a827999l)
        else if n < 40 then (logxor !b (logxor !c !d), 0x6ed9eba1l)
        else if n < 60 then
          (logor (logand !b !c) (logor (logand !b !d) (logand !c !d)), 0x8f1bbcdcl)
        else (logxor !b (logxor !c !d), 0xca62c1d6l)
      in
      let next = add (rotate !a 5) (add f (add !e (add k words.(n)))) in
      e := !d;
      d := !c;
      c := rotate !b 30;
      b := !a;
      a := next
    done;
    Array.iteri
      (fun n value -> state.(n) <- add state.(n) value)
      [| !a; !b; !c; !d; !e |]
  done;
  let result = Bytes.create 20 in
  Array.iteri
    (fun n word ->
      for i = 0 to 3 do
        Bytes.set result
          ((n * 4) + i)
          (Char.chr (to_int (logand 255l (shift_right_logical word ((3 - i) * 8)))))
      done)
    state;
  Bytes.unsafe_to_string result
