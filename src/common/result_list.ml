let rec map f = function
  | [] -> Ok []
  | x :: xs ->
      let ( let* ) = Result.bind in
      let* y = f x in
      let* ys = map f xs in
      Ok (y :: ys)

let rec iter f = function
  | [] -> Ok ()
  | x :: xs ->
      let ( let* ) = Result.bind in
      let* () = f x in
      iter f xs
