(* The SysV x86-64 C calling convention (psABI 3.2.3) as both backends see
   it: how a vector crosses a call (as C passes a GNU vector, i.e. as an agg
   holding just that vector) and the classification of an agg into
   eightbytes. The native backend (Mir_isel) and the LLVM path (Fir_llvm)
   both use it, so the two agree on every signature. SPEC.md §5. *)

module F = Fir
open Fir

let pow2_ge n =
  let rec go p = if p >= n then p else go (2 * p) in
  go 1

(* the bytes a vector takes in memory (bool vectors bit packed) *)
let vec_bytes = function Vec (n, Int 1) -> (n + 7) / 8 | t -> size_of t

(* How a vector crosses a call: as C passes a GNU vector, whose size is a
   power of 2 (a 3 x float is 16 bytes) and also its alignment, i.e. as a
   struct holding just the vector *)
let vec_agg t =
  let size = pow2_ge (vec_bytes t) in
  Agg { size; align = size; fields = [ (0, t) ] }

let abi_of = function Scalar (Vec _ as t) | Ext ((Vec _ as t), _) -> vec_agg t | a -> a

(* The C size of a vector type: GNU and ext vectors round their size up to
   a power of 2 (a 3 x float is 16 bytes); bool vectors are bit packed *)
let vec_abi_size (t : F.ty) =
  match t with
  | Vec _ -> pow2_ge (vec_bytes t)
  | t -> F.size_of t

(* An agg passed in registers is a list of parts, each one register: an
   INTEGER or SSE eightbyte, or an SSE eightbyte with its SSEUP neighbour
   (16 bytes of a vector in one xmm register). Eightbytes of class NO_CLASS
   (padding only) take no register. *)
type eightbyte = Eint | Esse | Esse16
type agg_class = Regs of (int * eightbyte) list  (** byte offset, kind *) | Memory

(* the psABI (3.2.3) class of one eightbyte *)
type ebclass = Cnone | Cint | Csse | Csseup | Cx87 | Cx87up | Cmem

let merge a b =
  if a = b then a
  else
    match (a, b) with
    | Cnone, x | x, Cnone -> x
    | Cmem, _ | _, Cmem -> Cmem
    | Cint, _ | _, Cint -> Cint
    | (Cx87 | Cx87up), _ | _, (Cx87 | Cx87up) -> Cmem
    | _ -> Csse

(* the alignment a field needs (else the agg has an unaligned field and is
   MEMORY) and the classes of the eightbytes it covers, from its first.
   Vectors as gcc and the psABI have them without AVX: up to 4 bytes
   INTEGER, 8 bytes SSE (except <1 x double>, which gcc passes in memory),
   16 bytes SSE + SSEUP, wider MEMORY. A 65..128-bit integer is two
   INTEGER eightbytes (needing only 8-byte alignment, as _BitInt(128) has) *)
let field_classes (t : F.ty) =
  match t with
  | Int n when n <= 64 -> (F.size_of t, [ Cint ])
  | Int n when n <= 128 -> (8, [ Cint; Cint ])
  | Int _ -> (8, [ Cmem ])
  | Ptr _ -> (8, [ Cint ])
  | F16 | F32 | F64 -> (F.size_of t, [ Csse ])
  | F80 -> (16, [ Cx87; Cx87up ])
  | Vec (_, Int 1) ->
      let s = vec_abi_size t in
      (min s 8, if s <= 8 then [ Cint ] else if s <= 16 then [ Cint; Cint ] else [ Cmem ])
  | Vec (n, e) ->
      let s = vec_abi_size t in
      if s <= 4 then (s, [ Cint ])
      else if s = 8 then (8, if n = 1 && e = F.F64 then [ Cmem ] else [ Csse ])
      else if s = 16 then (16, [ Csse; Csseup ])
      else (16, [ Cmem ])

(* SysV classification of an agg (psABI 3.2.3). An agg with an f80 is
   MEMORY here; [plan_ret] returns the x87 forms first. An agg without
   fields is MEMORY unless it is empty (size 0: nothing is passed) *)
let classify ~size ~align:_ ~fields =
  if fields = [] then if size = 0 then Regs [] else Memory
  else if size > 16 then Memory
  else begin
    let n = (size + 7) / 8 in
    let cls = Array.make n Cnone in
    let mem = ref false in
    List.iter
      (fun (o, t) ->
        let al, cs = field_classes t in
        if o < 0 || o mod al <> 0 then mem := true
        else
          List.iteri (fun k c -> let i = (o / 8) + k in if i < n then cls.(i) <- merge cls.(i) c else mem := true) cs)
      fields;
    if !mem || Array.exists (fun c -> c = Cmem || c = Cx87 || c = Cx87up) cls then Memory
    else
      let rec parts i acc =
        if i >= n then List.rev acc
        else
          match cls.(i) with
          | Cnone -> parts (i + 1) acc
          | Cint -> parts (i + 1) ((8 * i, Eint) :: acc)
          | Csse when i + 1 < n && cls.(i + 1) = Csseup -> parts (i + 2) ((8 * i, Esse16) :: acc)
          | _ -> parts (i + 1) ((8 * i, Esse) :: acc)
      in
      Regs (parts 0 [])
  end

let count_parts l =
  List.fold_left (fun (g, x) (_, eb) -> if eb = Eint then (g + 1, x) else (g, x + 1)) (0, 0) l

