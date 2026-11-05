let sysv name =
  let hash = ref 0L in
  String.iter
    (fun c ->
      hash := Int64.add (Int64.shift_left !hash 4) (Int64.of_int (Char.code c));
      let high = Int64.logand !hash 0xf0000000L in
      hash :=
        Int64.logand
          (Int64.logxor !hash (Int64.shift_right_logical high 24))
          (Int64.lognot high))
    name;
  !hash

let gnu name =
  String.fold_left
    (fun hash c ->
      Int64.logand 0xffffffffL
        (Int64.add (Int64.mul hash 33L) (Int64.of_int (Char.code c))))
    5381L name
