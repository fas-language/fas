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
  Agg { size; align = size; fields = [ (0, Leaf t) ] }

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
   fields is MEMORY unless it is empty (size 0: nothing is passed).

   A nested agg (a struct or union member, an array element) is classified
   on its own first, as gcc does: if its cleanup makes it MEMORY, the whole
   is MEMORY; otherwise its eightbyte classes merge into the whole's at its
   offset. This is how `union { int; __int128; union { long double; long;
   } }` is MEMORY (the inner union's eightbytes are INTEGER, X87UP: an
   X87UP not after an X87) while the same members in one union are
   INTEGER, INTEGER. *)
let classify ~size ~align:_ ~fields =
  if fields = [] then if size = 0 then Regs [] else Memory
  else if size > 16 then Memory
  else begin
    let n = (size + 7) / 8 in
    let mem = ref false in
    (* the classes of [fields] placed at byte [base] of the whole *)
    let rec place base fields =
      let cls = Array.make n Cnone in
      List.iter
        (fun (o, f) ->
          let o = base + o in
          match f with
          | Leaf t ->
              let al, cs = field_classes t in
              if o < 0 || o mod al <> 0 then mem := true
              else
                List.iteri
                  (fun k c -> let i = (o / 8) + k in if i < n then cls.(i) <- merge cls.(i) c else mem := true)
                  cs
          | Sub (Agg { size = ssize; align = salign; fields = sfields }) ->
              if o < 0 || o mod max 1 salign <> 0 || o + ssize > size then mem := true
              else if ssize > 0 then begin
                let sub = place o sfields in
                (* the nested agg's own cleanup (gcc's classify_argument):
                   an X87UP not after an X87 makes it MEMORY; an SSEUP not
                   after an SSE or SSEUP becomes SSE *)
                for i = o / 8 to (o + ssize - 1) / 8 do
                  if sub.(i) = Cx87up && (i = o / 8 || sub.(i - 1) <> Cx87) then mem := true;
                  if sub.(i) = Csseup && (i = o / 8 || (sub.(i - 1) <> Csse && sub.(i - 1) <> Csseup)) then
                    sub.(i) <- Csse;
                  cls.(i) <- merge cls.(i) sub.(i)
                done
              end
          | Sub (Scalar _ | Ext _) -> mem := true
          (* a complex f80 (COMPLEX_X87) is MEMORY as an argument; as a
             result alone [x87_result] has it first. Other complexes are
             their two parts *)
          | Complex F80 | Sub (Cplx F80) -> mem := true
          | Complex t | Sub (Cplx t) ->
              List.iter
                (fun o ->
                  let al, cs = field_classes t in
                  if o < 0 || o mod al <> 0 then mem := true
                  else
                    List.iteri
                      (fun k c -> let i = (o / 8) + k in if i < n then cls.(i) <- merge cls.(i) c else mem := true)
                      cs)
                [ o; o + F.size_of t ])
        fields;
      cls
    in
    let cls = place 0 fields in
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

(* an agg C returns in x87 registers: one long double (alone, or the same
   one several times in a union) in st0; the number of registers. A C
   _Complex long double comes back in st0 and st1, but it is the [Cplx F80]
   ABI, not an agg: a struct holding one (or two long doubles) is 32 bytes,
   MEMORY, and returned in memory (gcc and clang agree) *)
let x87_result ~size ~fields =
  let l = F.leaves_of_fields fields in
  if size = 16 && l <> [] && List.for_all (( = ) (0, F.F80)) l then Some 1
  else None

let count_parts l =
  List.fold_left (fun (g, x) (_, eb) -> if eb = Eint then (g + 1, x) else (g, x + 1)) (0, 0) l


(* ---------- several results (Fir's own convention, SPEC §5) ----------

   A signature with two or more results returns them in order: an integer
   of up to 64 bits or a pointer in the next of rax, rdx; an f32 or f64 in
   the next of xmm0, xmm1; every other result (f16, f80, an agg, which
   includes vectors and wide integers), and any result once its registers
   are used up, in a block the caller provides. The block's address is a
   hidden first argument (rdi, like sret) and is not returned; each result
   in it sits at the next offset aligned to its alignment, as a C struct of
   those results would. One result keeps the C rules above. *)

type mpart = Mgpr of int  (** rax, rdx *) | Mxmm of int  (** xmm0, xmm1 *) | Mmem of int  (** offset *)

(* the part of each result, and the block's size and alignment if any *)
let multi_results (abis : abi list) =
  let g = ref 0 and x = ref 0 and off = ref 0 and al = ref 1 and mem = ref false in
  let in_mem size align =
    mem := true;
    al := max !al align;
    let o = (!off + align - 1) / align * align in
    off := o + size;
    Mmem o
  in
  let parts =
    List.map
      (fun (a : abi) ->
        match a with
        | (Scalar (Int n) | Ext (Int n, _)) when n <= 64 && !g < 2 ->
            incr g;
            Mgpr (!g - 1)
        | Scalar (Ptr _) when !g < 2 ->
            incr g;
            Mgpr (!g - 1)
        | Scalar (F32 | F64) when !x < 2 ->
            incr x;
            Mxmm (!x - 1)
        | Scalar F80 -> in_mem 16 16
        | Scalar t | Ext (t, _) -> in_mem (size_of t) (size_of t)
        | Agg { size; align; _ } -> in_mem size (max 1 align)
        | Cplx t -> in_mem (2 * size_of t) (size_of t))
      abis
  in
  (parts, if !mem then Some ((!off + !al - 1) / !al * !al, !al) else None)

(* Win64 passes records of these sizes as one integer. Other records
   travel by address, including a hidden result pointer when returned. *)
let ms_reg_size = function 1 | 2 | 4 | 8 -> true | _ -> false
