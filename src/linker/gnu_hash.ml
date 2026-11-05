(* Stable counting sort makes each GNU hash bucket a contiguous dynsym chain.
   Return old-to-new symbol indices so relocations and versions follow it. *)
let build ~first names =
  let total = Array.length names in
  let count = total - first in
  let buckets = max 1 (count / 4) in
  let hashes = Array.init count (fun n -> Symbol_hash.gnu names.(first + n)) in
  let bucket hash = Int64.to_int (Int64.rem hash (Int64.of_int buckets)) in
  let counts = Array.make buckets 0 in
  Array.iter
    (fun hash ->
      let b = bucket hash in
      counts.(b) <- counts.(b) + 1)
    hashes;
  let starts = Array.make buckets 0 and cursor = ref first in
  Array.iteri
    (fun b size ->
      starts.(b) <- !cursor;
      cursor := !cursor + size)
    counts;
  let next = Array.copy starts in
  let indices = Array.init total Fun.id in
  Array.iteri
    (fun n hash ->
      let b = bucket hash in
      indices.(first + n) <- next.(b);
      next.(b) <- next.(b) + 1)
    hashes;
  let rec power n = if n >= max 1 ((count + 7) / 8) then n else power (n * 2) in
  let bloom = power 1 in
  let bucket_start = 16 + (bloom * 8) in
  let chain_start = bucket_start + (buckets * 4) in
  let bytes = Bytes.make (chain_start + (count * 4)) '\000' in
  Binary.set_int bytes 0 4 buckets;
  Binary.set_int bytes 4 4 first;
  Binary.set_int bytes 8 4 bloom;
  Binary.set_int bytes 12 4 6;
  Array.iteri
    (fun b size ->
      Binary.set_int bytes
        (bucket_start + (b * 4))
        4
        (if size = 0 then 0 else starts.(b)))
    counts;
  Array.iteri
    (fun n hash ->
      let word = Int64.to_int (Int64.div hash 64L) land (bloom - 1) in
      let mask =
        Int64.logor
          (Int64.shift_left 1L (Int64.to_int (Int64.logand hash 63L)))
          (Int64.shift_left 1L
             (Int64.to_int (Int64.logand (Int64.shift_right_logical hash 6) 63L)))
      in
      let at = 16 + (word * 8) in
      Binary.set bytes at 8
        (Int64.logor (Binary.u64 (Bytes.unsafe_to_string bytes) at) mask);
      let index = indices.(first + n) in
      let b = bucket hash in
      Binary.set bytes
        (chain_start + ((index - first) * 4))
        4
        (Int64.logor
           (Int64.logand hash 0xffff_fffeL)
           (if index + 1 = starts.(b) + counts.(b) then 1L else 0L)))
    hashes;
  (indices, Bytes.unsafe_to_string bytes)
