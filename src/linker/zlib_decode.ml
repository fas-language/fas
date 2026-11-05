(* RFC 1950 / RFC 1951 decoder for bounded ELF debug sections. No native zlib
   dependency. All state belongs to the call; the caller bounds decoded size. *)
type tree = {
  counts : int array;
  first : int array;
  starts : int array;
  symbols : int array;
}

let decode ~context ~size input ~offset ~length =
  let fail message = Error.fail "%s: zlib: %s" context message in
  Binary.span ~context ~length:(String.length input) offset length;
  if size < 0 || size > 128 * 1024 * 1024 then fail "decoded size exceeds limit";
  if length < 6 then fail "truncated header or checksum";
  let byte i = Char.code input.[i] in
  let cmf = byte offset and flg = byte (offset + 1) in
  if cmf land 15 <> 8 || cmf lsr 4 > 7 then fail "unsupported compression method/window";
  if (cmf lsl 8) lor flg mod 31 <> 0 then fail "invalid header check";
  if flg land 32 <> 0 then fail "preset dictionaries are unsupported";
  let window = 1 lsl ((cmf lsr 4) + 8) in
  let ending = offset + length - 4 in
  let bit = ref ((offset + 2) * 8) in
  let bits n =
    if n > (ending * 8) - !bit then fail "truncated deflate stream";
    let value = ref 0 in
    for i = 0 to n - 1 do
      value := !value lor (((byte (!bit lsr 3) lsr (!bit land 7)) land 1) lsl i);
      incr bit
    done;
    !value
  in
  let tree ~complete lengths =
    let counts = Array.make 16 0 in
    Array.iter
      (fun n ->
        if n < 0 || n > 15 then fail "invalid Huffman code length";
        if n <> 0 then counts.(n) <- counts.(n) + 1)
      lengths;
    let first = Array.make 16 0 and starts = Array.make 16 0 in
    let left = ref 1 and total = ref 0 in
    for n = 1 to 15 do
      left := (!left lsl 1) - counts.(n);
      if !left < 0 then fail "oversubscribed Huffman tree";
      first.(n) <- (first.(n - 1) + counts.(n - 1)) lsl 1;
      starts.(n) <- !total;
      total := !total + counts.(n)
    done;
    if !left <> 0 && (complete || (!total <> 0 && not (!total = 1 && counts.(1) = 1)))
    then fail "incomplete Huffman tree";
    let symbols = Array.make !total 0 and next = Array.copy starts in
    Array.iteri
      (fun symbol n ->
        if n <> 0 then begin
          symbols.(next.(n)) <- symbol;
          next.(n) <- next.(n) + 1
        end)
      lengths;
    { counts; first; starts; symbols }
  in
  let symbol t =
    let code = ref 0 and found = ref (-1) and n = ref 1 in
    while !found < 0 && !n <= 15 do
      code := (!code lsl 1) lor bits 1;
      let index = !code - t.first.(!n) in
      if index >= 0 && index < t.counts.(!n) then
        found := t.symbols.(t.starts.(!n) + index);
      incr n
    done;
    if !found < 0 then fail "invalid Huffman code";
    !found
  in
  let output = Bytes.create size and cursor = ref 0 in
  let write value =
    if !cursor >= size then fail "decoded data exceeds declared size";
    Bytes.set output !cursor (Char.chr value);
    incr cursor
  in
  let length_base =
    [|
      3;
      4;
      5;
      6;
      7;
      8;
      9;
      10;
      11;
      13;
      15;
      17;
      19;
      23;
      27;
      31;
      35;
      43;
      51;
      59;
      67;
      83;
      99;
      115;
      131;
      163;
      195;
      227;
      258;
    |]
  in
  let length_extra =
    [|
      0;
      0;
      0;
      0;
      0;
      0;
      0;
      0;
      1;
      1;
      1;
      1;
      2;
      2;
      2;
      2;
      3;
      3;
      3;
      3;
      4;
      4;
      4;
      4;
      5;
      5;
      5;
      5;
      0;
    |]
  in
  let distance_base =
    [|
      1;
      2;
      3;
      4;
      5;
      7;
      9;
      13;
      17;
      25;
      33;
      49;
      65;
      97;
      129;
      193;
      257;
      385;
      513;
      769;
      1025;
      1537;
      2049;
      3073;
      4097;
      6145;
      8193;
      12289;
      16385;
      24577;
    |]
  in
  let distance_extra =
    [|
      0;
      0;
      0;
      0;
      1;
      1;
      2;
      2;
      3;
      3;
      4;
      4;
      5;
      5;
      6;
      6;
      7;
      7;
      8;
      8;
      9;
      9;
      10;
      10;
      11;
      11;
      12;
      12;
      13;
      13;
    |]
  in
  let final = ref false in
  while not !final do
    final := bits 1 <> 0;
    let kind = bits 2 in
    if kind = 0 then begin
      bit := (!bit + 7) land lnot 7;
      let n = bits 16 in
      let complement = bits 16 in
      if n lxor complement <> 0xffff then fail "invalid stored block length";
      if n > size - !cursor then fail "stored block exceeds declared size";
      for _ = 1 to n do
        write (bits 8)
      done
    end
    else if kind = 3 then fail "reserved block type"
    else begin
      let literals, distances =
        if kind = 1 then
          ( tree ~complete:true
              (Array.init 288 (fun n ->
                   if n < 144 then 8
                   else if n < 256 then 9
                   else if n < 280 then 7
                   else 8)),
            tree ~complete:true (Array.make 32 5) )
        else begin
          let literal_count = bits 5 + 257 in
          let distance_count = bits 5 + 1 in
          let code_count = bits 4 + 4 in
          if literal_count > 286 then fail "reserved literal/length alphabet size";
          let order =
            [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15 |]
          in
          let codes = Array.make 19 0 in
          for n = 0 to code_count - 1 do
            codes.(order.(n)) <- bits 3
          done;
          let code_tree = tree ~complete:true codes in
          let lengths = Array.make (literal_count + distance_count) 0 in
          let n = ref 0 in
          while !n < Array.length lengths do
            let code = symbol code_tree in
            if code < 16 then begin
              lengths.(!n) <- code;
              incr n
            end
            else begin
              let value, repeats =
                match code with
                | 16 ->
                    if !n = 0 then fail "repeat without previous code";
                    (lengths.(!n - 1), bits 2 + 3)
                | 17 -> (0, bits 3 + 3)
                | 18 -> (0, bits 7 + 11)
                | _ -> fail "invalid code-length symbol"
              in
              if repeats > Array.length lengths - !n then
                fail "code-length repeat exceeds alphabet";
              Array.fill lengths !n repeats value;
              n := !n + repeats
            end
          done;
          if lengths.(256) = 0 then fail "missing end-of-block code";
          ( tree ~complete:false (Array.sub lengths 0 literal_count),
            tree ~complete:false (Array.sub lengths literal_count distance_count) )
        end
      in
      let finished = ref false in
      while not !finished do
        let code = symbol literals in
        if code < 256 then write code
        else if code = 256 then finished := true
        else begin
          if code > 285 then fail "reserved length code";
          let i = code - 257 in
          let n = length_base.(i) + bits length_extra.(i) in
          let d = symbol distances in
          if d > 29 then fail "reserved distance code";
          let distance = distance_base.(d) + bits distance_extra.(d) in
          if distance > !cursor || distance > window then
            fail "invalid back-reference distance";
          if n > size - !cursor then fail "back-reference exceeds declared size";
          for _ = 1 to n do
            write (Char.code (Bytes.get output (!cursor - distance)))
          done
        end
      done
    end
  done;
  if (!bit + 7) / 8 <> ending then fail "trailing bytes after deflate stream";
  if !cursor <> size then fail "decoded size differs from declared size";
  let a = ref 1 and b = ref 0 in
  Bytes.iter
    (fun c ->
      a := (!a + Char.code c) mod 65521;
      b := (!b + !a) mod 65521)
    output;
  let expected = ref 0 in
  for i = 0 to 3 do
    expected := (!expected lsl 8) lor byte (ending + i)
  done;
  if (!b lsl 16) lor !a <> !expected then fail "Adler-32 checksum mismatch";
  Bytes.unsafe_to_string output
