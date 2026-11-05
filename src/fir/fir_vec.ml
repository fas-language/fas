(* Growable arrays: the storage behind every Fir table. Elements live in
   [data.(0) .. data.(len - 1)]; [dummy] fills unused capacity so the array
   never holds stale references the GC would keep alive. *)

type 'a t = { mutable data : 'a array; mutable len : int; dummy : 'a }

let create ?(capacity = 16) dummy = { data = Array.make (max 1 capacity) dummy; len = 0; dummy }
let length v = v.len

let get v i =
  if i < 0 || i >= v.len then invalid_arg "Fir_vec.get";
  Array.unsafe_get v.data i

let set v i x =
  if i < 0 || i >= v.len then invalid_arg "Fir_vec.set";
  Array.unsafe_set v.data i x

(* Appends [x] and returns its index *)
let push v x =
  if v.len = Array.length v.data then begin
    let bigger = Array.make (2 * v.len) v.dummy in
    Array.blit v.data 0 bigger 0 v.len;
    v.data <- bigger
  end;
  Array.unsafe_set v.data v.len x;
  v.len <- v.len + 1;
  v.len - 1

let clear v =
  Array.fill v.data 0 v.len v.dummy;
  v.len <- 0

let iter f v =
  for i = 0 to v.len - 1 do
    f (Array.unsafe_get v.data i)
  done

let iteri f v =
  for i = 0 to v.len - 1 do
    f i (Array.unsafe_get v.data i)
  done

let fold f acc v =
  let acc = ref acc in
  for i = 0 to v.len - 1 do
    acc := f !acc (Array.unsafe_get v.data i)
  done;
  !acc

let to_array v = Array.sub v.data 0 v.len
let to_list v = Array.to_list (to_array v)

let of_list dummy l =
  let v = create ~capacity:(List.length l) dummy in
  List.iter (fun x -> ignore (push v x)) l;
  v
