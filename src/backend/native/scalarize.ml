(* Vector scalarization for the native backend. Every value of type
   `<n x t>` becomes n scalar lane values before promotion, so the rest of the
   backend sees scalars only.

   - A vector definition [d] gets n consecutive fresh lane ids, assigned up
     front so that phis can refer to lanes defined later. Lanes that are
     existing values (constant-index extracts, shuffles, inserts) go through a
     substitution applied at the end.
   - A vector parameter `p` becomes the parameters `p.0` .. `p.(n-1)`; a
     function returning a vector gets a hidden last parameter `vret.ptr` and
     returns void, storing its lanes there.
   - Vector memory keeps its layout: lane i of a non-bool vector is at
     i * sizeof(t), accessed with `gep [k x t], p, 0, i` so that SROA can
     promote vector allocas (now allocas of `[k x t]`). Bool vectors are bit
     packed (lane i is bit i), as LLVM stores `<n x i1>`, and are accessed as
     one integer.
   - Bitcasts between vectors and scalars pack lanes into 64-bit words and
     unpack them again. *)

open Ir
module Hashtbl = Itbl
module Ptbl = Stdlib.Hashtbl

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt
let is_vector = function Vector _ -> true | _ -> false

let lane_bits = function
  | I1 -> 1
  | I8 -> 8
  | I16 -> 16
  | I32 | F32 -> 32
  | I64 | F64 | Pointer _ -> 64
  | _ -> unsupported "vector lane type"

let int_ty = function
  | 1 -> I1
  | 8 -> I8
  | 16 -> I16
  | 32 -> I32
  | 64 -> I64
  | _ -> unsupported "integer width"

let lane_name name i = name ^ "." ^ string_of_int i
let vret = "vret.ptr"
let zero t = match t with Pointer _ -> Null t | _ -> Const (t, 0L)

type st = {
  structs : struct_def list;
  base : int Hashtbl.t; (* vector definition -> first lane id *)
  subst : value Hashtbl.t; (* id -> the value it stands for *)
  mutable next : int;
  mutable out : instr list; (* reversed *)
  mutable entry : instr list; (* allocas for the entry block, reversed *)
  chunk : bool;
      (* chunk mode: a vector is split into 128-bit pieces `<c x t>` that stay
         vectors (SSE2 registers) instead of into lanes *)
  split : value array Hashtbl.t; (* chunk mode: vector alloca -> piece allocas *)
  ppieces : (string, value array) Ptbl.t; (* chunk mode: vector param -> pieces *)
  ins0 : value Hashtbl.t; (* insert at lane 0 -> the inserted scalar *)
  dyn : (ty, value) Ptbl.t;
      (* chunk mode: per vector type, a stack slot for lane accesses at a
         variable index *)
  pairs : unit Hashtbl.t;
      (* i128 values split into two i64 words, see [i128_pairs] *)
  slot_of : value Hashtbl.t;
      (* lane mode, current block: vector definition -> a stack slot that
         holds its lanes, see [slot_lane] *)
  users : (int * bool) list Hashtbl.t;
      (* vector definition -> per use, its block and whether the use is a
         variable-index extract or insert reading it from a slot *)
  mutable cur : int; (* index of the block being rewritten *)
  const_slot : (value, value) Ptbl.t; (* like [slot_of], for constants *)
  guarded : (int, ty * value * value * value) Ptbl.t;
      (* variable insert whose only use is `select c (insert v i x) v`:
         lowered with the select, as a conditional store into v's slot *)
}

let fresh st =
  let i = st.next in
  st.next <- i + 1;
  i

let emit st ins = st.out <- ins :: st.out

let bytes st ty =
  match static_type_bytes st.structs [] ty with
  | Ok n -> n
  | Error () -> unsupported "vector type with no static size"

(* The array type with the same layout as a non-bool vector type *)
let lane_array st = function
  | Vector (_, t) as vt -> Array (bytes st vt / (lane_bits t / 8), t)
  | _ -> invalid_arg "lane_array"

(* The type holding a bool vector in memory: one integer up to 64 lanes,
   an array of i64 words beyond (lane i is bit i mod 64 of word i / 64) *)
let packed_ty st vt =
  match bytes st vt with
  | 1 -> I8
  | 2 -> I16
  | 4 -> I32
  | 8 -> I64
  | n when n mod 8 = 0 -> Array (n / 8, I64)
  | _ -> unsupported "bool vector of %d bytes" (bytes st vt)

(* Lanes per piece, the type of a piece and the number of pieces *)
let per st t = if st.chunk then 128 / lane_bits t else 1

let piece_ty st = function
  | Vector (_, t) -> if st.chunk then Vector (per st t, t) else t
  | t -> t

let npieces st = function Vector (n, t) -> n / per st t | _ -> 1

let lid st d i =
  match Hashtbl.find_opt st.base d with
  | Some b -> b + i
  | None -> unsupported "vector value without a definition"

(* The pieces of a vector value: its lanes, or its 128-bit chunks *)
let lanes st v =
  match v with
  | Local (id, (Vector _ as vt)) ->
      let b = lid st id 0 and pt = piece_ty st vt in
      Array.init (npieces st vt) (fun i -> Local (b + i, pt))
  | Local (id, I128) when Hashtbl.mem st.pairs id ->
      let b = lid st id 0 in
      [| Local (b, I64); Local (b + 1, I64) |]
  | Param (name, _) when Ptbl.mem st.ppieces name -> Ptbl.find st.ppieces name
  | Param (name, Vector (n, t)) -> Array.init n (fun i -> Param (lane_name name i, t))
  | Const_vector ((Vector (n, t) as vt), xs) ->
      if List.length xs <> n then unsupported "vector constant";
      if st.chunk then
        let c = per st t and a = Array.of_list xs in
        Array.init (n / c) (fun i ->
            Const_vector (piece_ty st vt, Array.to_list (Array.sub a (i * c) c)))
      else Array.of_list (List.map (fun x -> Const (t, x)) xs)
  | Zero (Vector (n, t) as vt) ->
      if st.chunk then Array.make (npieces st vt) (Zero (piece_ty st vt) : value)
      else Array.make n (zero t)
  | Undef (Vector (n, t) as vt) ->
      if st.chunk then Array.make (npieces st vt) (Undef (piece_ty st vt))
      else Array.make n (Undef t)
  | _ -> unsupported "vector operand"

let set st d i v = Hashtbl.replace st.subst (lid st d i) v

(* Small instruction builders that return their result *)
let bin st op t a b =
  let d = fresh st in
  emit st (Bin (d, op, t, a, b));
  Local (d, t)

let cast st kind from v target =
  if from = target then v
  else
    let d = fresh st in
    emit st (Cast (d, kind, from, v, target));
    Local (d, target)

let cmp st c t a b =
  let d = fresh st in
  emit st (Cmp (d, c, t, a, b));
  Local (d, I1)

let select st c a b =
  let d = fresh st in
  emit st (Select (d, c, a, b));
  Local (d, value_ty a)

(* A lane as an integer of its width, and back *)
let to_int st t v =
  match t with
  | F32 -> cast st "bitcast" F32 v I32
  | F64 -> cast st "bitcast" F64 v I64
  | Pointer _ -> cast st "ptrtoint" t v I64
  | _ -> v

let of_int st t v =
  match t with
  | F32 -> cast st "bitcast" I32 v F32
  | F64 -> cast st "bitcast" I64 v F64
  | Pointer _ -> cast st "inttoptr" I64 v t
  | _ -> v

(* Pack lanes of type [t] into 64-bit words, lane i at bit i * width *)
let pack st t (ls : value array) =
  let w = lane_bits t in
  let nwords = ((Array.length ls * w) + 63) / 64 in
  let words = Array.make nwords None in
  Array.iteri
    (fun i v ->
      let x = to_int st t v in
      let pos = i * w in
      let k = pos / 64 and off = pos mod 64 in
      let part =
        if w = 64 then x
        else
          let z = cast st "zext" (int_ty w) x I64 in
          if off = 0 then z else bin st Shl I64 z (Const (I64, Int64.of_int off))
      in
      words.(k) <-
        Some (match words.(k) with None -> part | Some acc -> bin st Or I64 acc part))
    ls;
  Array.map (function Some v -> v | None -> Const (I64, 0L)) words

let unpack st (words : value array) n t =
  let w = lane_bits t in
  Array.init n (fun j ->
      let pos = j * w in
      let k = pos / 64 and off = pos mod 64 in
      let word = if k < Array.length words then words.(k) else Const (I64, 0L) in
      let x =
        if w = 64 then word
        else
          let s =
            if off = 0 then word
            else bin st Lshr I64 word (Const (I64, Int64.of_int off))
          in
          cast st "trunc" I64 s (int_ty w)
      in
      of_int st t x)

(* Words of any vector or scalar value *)
let words_of st v =
  match value_ty v with
  | Vector (_, t) -> pack st t (lanes st v)
  | (I1 | I8 | I16 | I32 | I64 | F32 | F64 | Pointer _) as t ->
      let x = to_int st t v in
      [|
        (if lane_bits t = 64 then x else cast st "zext" (int_ty (lane_bits t)) x I64);
      |]
  | I128 -> lanes st v
  | _ -> unsupported "bitcast of this type"

let scalar_of_words st words t =
  let w = lane_bits t in
  let x = words.(0) in
  of_int st t (if w = 64 then x else cast st "trunc" I64 x (int_ty w))

let lane_ptr st vt p i =
  let at = lane_array st vt in
  let g = fresh st in
  emit st (Gep (g, at, p, [ Zero; Index (Const (I64, Int64.of_int i)) ]));
  Local (g, Pointer at)

let lane_align al t =
  let e = lane_bits t / 8 in
  if al <= 0 then e else min al e

(* Lanes of a vector loaded from [p] *)
let load_lanes st ~volatile vt p al =
  match vt with
  | Vector (n, I1) -> (
      match packed_ty st vt with
      | Array (k, _) as at ->
          let words =
            Array.init k (fun i ->
                let q = fresh st and d = fresh st in
                emit st (Gep (q, at, p, [ Zero; Index (Const (I64, Int64.of_int i)) ]));
                let q = Local (q, Pointer at) and al = if al <= 0 then 8 else min al 8 in
                emit st
                  (if volatile then Load_volatile (d, I64, q, al) else Load (d, I64, q, al));
                Local (d, I64))
          in
          unpack st words n I1
      | it ->
          let d = fresh st in
          emit st (if volatile then Load_volatile (d, it, p, al) else Load (d, it, p, al));
          let w = cast st "zext" it (Local (d, it)) I64 in
          unpack st [| w |] n I1)
  | Vector (_, t) when st.chunk ->
      let pt = piece_ty st vt and c = per st t in
      Array.init (npieces st vt) (fun i ->
          let q = lane_ptr st vt p (i * c) in
          let d = fresh st in
          let al = if al <= 0 then lane_bits t / 8 else min al 16 in
          emit st
            (if volatile then Load_volatile (d, pt, q, al) else Load (d, pt, q, al));
          Local (d, pt))
  | Vector (n, t) ->
      Array.init n (fun i ->
          let q = lane_ptr st vt p i in
          let d = fresh st in
          let al = lane_align al t in
          emit st (if volatile then Load_volatile (d, t, q, al) else Load (d, t, q, al));
          Local (d, t))
  | _ -> invalid_arg "load_lanes"

let store_lanes st ~volatile vt (ls : value array) p al =
  match vt with
  | Vector (_, I1) -> (
      match packed_ty st vt with
      | Array (_, _) as at ->
          Array.iteri
            (fun i w ->
              let q = fresh st in
              emit st (Gep (q, at, p, [ Zero; Index (Const (I64, Int64.of_int i)) ]));
              let q = Local (q, Pointer at) and al = if al <= 0 then 8 else min al 8 in
              emit st
                (if volatile then Store_volatile (I64, w, q, al) else Store (I64, w, q, al)))
            (pack st I1 ls)
      | it ->
          let w = (pack st I1 ls).(0) in
          let x = cast st "trunc" I64 w it in
          emit st (if volatile then Store_volatile (it, x, p, al) else Store (it, x, p, al)))
  | Vector (_, t) when st.chunk ->
      let pt = piece_ty st vt and c = per st t in
      Array.iteri
        (fun i v ->
          let q = lane_ptr st vt p (i * c) in
          let al = if al <= 0 then lane_bits t / 8 else min al 16 in
          emit st
            (if volatile then Store_volatile (pt, v, q, al) else Store (pt, v, q, al)))
        ls
  | Vector (_, t) ->
      Array.iteri
        (fun i v ->
          let q = lane_ptr st vt p i in
          let al = lane_align al t in
          emit st
            (if volatile then Store_volatile (t, v, q, al) else Store (t, v, q, al)))
        ls
  | _ -> invalid_arg "store_lanes"

(* Storage for a vector in memory: an alloca of the same size *)
let storage_ty st vt =
  match vt with Vector (_, I1) -> packed_ty st vt | _ -> lane_array st vt

let index_const = function Const (_, k) -> Some (Int64.to_int k) | _ -> None

(* "llvm.ctpop.v8i64" -> "llvm.ctpop.i64" *)
let scalar_intrinsic name =
  String.split_on_char '.' name
  |> List.map (fun c ->
      let n = String.length c in
      if n >= 2 && c.[0] = 'v' && c.[1] >= '0' && c.[1] <= '9' then begin
        let j = ref 1 in
        while !j < n && c.[!j] >= '0' && c.[!j] <= '9' do
          incr j
        done;
        String.sub c !j (n - !j)
      end
      else c)
  |> String.concat "."

let expand_args st args =
  List.concat_map
    (fun ((t, e, v) as a) ->
      match t with
      | Vector (n, lt) when st.chunk ->
          let ps = lanes st v and c = per st lt and pt = piece_ty st t in
          List.init n (fun j ->
              let d = fresh st in
              emit st (Extract (d, pt, ps.(j / c), Const (I32, Int64.of_int (j mod c))));
              (lt, No_extension, Local (d, lt)))
      | Vector (_, lt) ->
          Array.to_list (Array.map (fun l -> (lt, No_extension, l)) (lanes st v))
      | _ ->
          ignore e;
          [ a ])
    args

(* Lane mode: an extract or insert at a variable index on a wide vector goes
   through a stack slot (store the lanes once, then one indexed access)
   instead of a compare and select per lane, which is quadratic for the
   permute / compress / expand loops on 256 lanes. *)
let slot_lane st = function
  | Vector (n, t) -> (not st.chunk) && n >= 16 && n land (n - 1) = 0 && t <> I1
  | _ -> false

let slot_new st vt =
  let sty = lane_array st vt and tmp = fresh st in
  st.entry <- Alloca (tmp, sty, 16) :: st.entry;
  Local (tmp, Pointer sty)

(* A slot holding [v]: the one it was put in earlier in this block, or a new
   one it is stored to now *)
let slot_with st vt v =
  match v with
  | Local (id, _) when Hashtbl.mem st.slot_of id -> Hashtbl.find st.slot_of id
  | (Const_vector _ | Zero _) when Ptbl.mem st.const_slot v -> Ptbl.find st.const_slot v
  | _ ->
      let p = slot_new st vt in
      store_lanes st ~volatile:false vt (lanes st v) p 16;
      (match v with
      | Local (id, _) -> Hashtbl.replace st.slot_of id p
      | Const_vector _ | Zero _ -> Ptbl.replace st.const_slot v p
      | _ -> ());
      p

let slot_elem st vt p idx =
  let n = match vt with Vector (n, _) -> n | _ -> invalid_arg "slot_elem" in
  let i = match value_ty idx with I64 -> idx | it -> cast st "zext" it idx I64 in
  (* out of range is poison; the mask keeps the access inside the slot *)
  let i = bin st And I64 i (Const (I64, Int64.of_int (n - 1))) in
  let g = fresh st in
  emit st (Gep (g, lane_array st vt, p, [ Zero; Index i ]));
  Local (g, Pointer (lane_array st vt))

let uses_of st id = Option.value (Hashtbl.find_opt st.users id) ~default:[]

(* The slot [d] (a vector made from [v]) is written in: [v]'s own when [d]
   is the only thing left reading [v] ([own] uses), else a copy *)
let slot_for_update st vt v own =
  match v with
  | Local (id, _) when Hashtbl.mem st.slot_of id && List.length (uses_of st id) = own ->
      Hashtbl.find st.slot_of id
  | _ ->
      let p = slot_new st vt in
      store_lanes st ~volatile:false vt (lanes st v) p 16;
      p

(* [d] now lives in slot [p]; its lanes are loaded only when something
   other than slot accesses in this block reads it *)
let slot_result st vt d p =
  Hashtbl.replace st.slot_of d p;
  if List.exists (fun (b, ok) -> b <> st.cur || not ok) (uses_of st d) then
    Array.iteri (fun i l -> set st d i l) (load_lanes st ~volatile:false vt p 16)

let instr_slot st ins =
  match ins with
  | Insert (d, _, _, _, _) when Ptbl.mem st.guarded d -> true
  | Select (d, c, Local (a, _), _) when Ptbl.mem st.guarded a ->
      let vt, v, idx, x = Ptbl.find st.guarded a in
      let t = match vt with Vector (_, t) -> t | _ -> assert false in
      (* v is read by the insert and by the select *)
      let p = slot_for_update st vt v 2 in
      let q = slot_elem st vt p idx and al = lane_bits t / 8 in
      let old = fresh st in
      emit st (Load (old, t, q, al));
      emit st (Store (t, select st c x (Local (old, t)), q, al));
      slot_result st vt d p;
      true
  | Extract (d, (Vector (_, t) as vt), v, idx)
    when slot_lane st vt && index_const idx = None ->
      let p = slot_with st vt v in
      let al = lane_bits t / 8 in
      emit st (Load (d, t, slot_elem st vt p idx, al));
      true
  | Insert (d, (Vector (_, t) as vt), v, idx, x)
    when slot_lane st vt && index_const idx = None ->
      let p = slot_for_update st vt v 1 in
      emit st (Store (t, x, slot_elem st vt p idx, lane_bits t / 8));
      slot_result st vt d p;
      true
  | _ -> false

let instr_lanes st ins =
  match ins with
  | Bin (d, op, (Vector _ as vt), a, b) ->
      let la = lanes st a and lb = lanes st b and t = piece_ty st vt in
      for i = 0 to npieces st vt - 1 do
        emit st (Bin (lid st d i, op, t, la.(i), lb.(i)))
      done
  | Fneg (d, Vector (n, t), a) ->
      let la = lanes st a in
      for i = 0 to n - 1 do
        emit st (Fneg (lid st d i, t, la.(i)))
      done
  | Cmp (d, c, Vector (n, t), a, b) ->
      let la = lanes st a and lb = lanes st b in
      for i = 0 to n - 1 do
        emit st (Cmp (lid st d i, c, t, la.(i), lb.(i)))
      done
  | Select (d, c, a, b) when is_vector (value_ty a) ->
      let la = lanes st a and lb = lanes st b in
      let n = Array.length la in
      let lc = if is_vector (value_ty c) then lanes st c else Array.make n c in
      for i = 0 to n - 1 do
        emit st (Select (lid st d i, lc.(i), la.(i), lb.(i)))
      done
  | Cast (d, "bitcast", from, v, target) when is_vector from || is_vector target -> (
      if from = target then
        match target with
        | Vector _ ->
            let lv = lanes st v in
            for i = 0 to npieces st target - 1 do
              set st d i lv.(i)
            done
        | _ -> Hashtbl.replace st.subst d v
      else
        let words = words_of st v in
        match target with
        | Vector (n, t) -> Array.iteri (fun i x -> set st d i x) (unpack st words n t)
        | t -> Hashtbl.replace st.subst d (scalar_of_words st words t))
  | Cast (d, kind, Vector (n, t), v, Vector (m, t2)) when n = m ->
      let lv = lanes st v in
      for i = 0 to n - 1 do
        emit st (Cast (lid st d i, kind, t, lv.(i), t2))
      done
  | Cast (_, kind, from, _, target) when is_vector from || is_vector target ->
      unsupported "vector cast %s" kind
  | Extract (d, Vector (n, t), v, idx) -> (
      let lv = lanes st v in
      match index_const idx with
      | Some k when k >= 0 && k < n -> Hashtbl.replace st.subst d lv.(k)
      | Some _ -> Hashtbl.replace st.subst d (Undef t)
      | None ->
          let it = value_ty idx in
          let r = ref lv.(0) in
          for i = 1 to n - 1 do
            let c = cmp st Eq it idx (Const (it, Int64.of_int i)) in
            r := select st c lv.(i) !r
          done;
          Hashtbl.replace st.subst d !r)
  | Insert (d, Vector (n, _), v, idx, x) -> (
      let lv = lanes st v in
      match index_const idx with
      | Some k ->
          for i = 0 to n - 1 do
            set st d i (if i = k then x else lv.(i))
          done
      | None ->
          let it = value_ty idx in
          for i = 0 to n - 1 do
            let c = cmp st Eq it idx (Const (it, Int64.of_int i)) in
            emit st (Select (lid st d i, c, x, lv.(i)))
          done)
  | Shuffle_zero (d, Vector (n, _), v) ->
      let l0 = (lanes st v).(0) in
      for i = 0 to n - 1 do
        set st d i l0
      done
  | Shufflevector (d, Vector (m, t), a, b, mask) ->
      let la = lanes st a and lb = lanes st b in
      let na = Array.length la in
      let ms =
        match mask with
        | Const_vector (_, xs) -> Array.of_list (List.map Int64.to_int xs)
        | Zero _ -> Array.make m 0
        | Undef _ -> Array.make m (-1)
        | _ -> unsupported "shuffle with a variable mask"
      in
      for i = 0 to m - 1 do
        let k = ms.(i) in
        set st d i
          (if k < 0 then Undef t
           else if k < na then la.(k)
           else if k - na < Array.length lb then lb.(k - na)
           else Undef t)
      done
  | Load (d, (Vector (n, _) as vt), p, al)
  | Load_volatile (d, (Vector (n, _) as vt), p, al) ->
      let volatile = match ins with Load_volatile _ -> true | _ -> false in
      let ls = load_lanes st ~volatile vt p al in
      for i = 0 to Array.length ls - 1 do
        ignore n;
        set st d i ls.(i)
      done
  | Store ((Vector _ as vt), v, p, al) | Store_volatile ((Vector _ as vt), v, p, al) ->
      let volatile = match ins with Store_volatile _ -> true | _ -> false in
      store_lanes st ~volatile vt (lanes st v) p al
  | Alloca (d, (Vector _ as vt), al) -> emit st (Alloca (d, storage_ty st vt, al))
  | Gep (d, (Vector (_, t) as vt), p, idx) -> (
      match (t, idx) with
      | I1, [ _ ] -> emit st (Gep (d, packed_ty st vt, p, idx))
      | I1, _ -> unsupported "address of a bool vector lane"
      | _ -> emit st (Gep (d, lane_array st vt, p, idx)))
  | Phi (d, (Vector _ as vt), incoming) ->
      let ins = List.map (fun (v, b) -> (lanes st v, b)) incoming in
      let t = piece_ty st vt in
      for i = 0 to npieces st vt - 1 do
        emit st (Phi (lid st d i, t, List.map (fun (l, b) -> (l.(i), b)) ins))
      done
  | Call (dst, ext, rty, name, args)
    when is_vector rty || List.exists (fun (t, _, _) -> is_vector t) args ->
      if String.starts_with ~prefix:"llvm.vector.reduce.or." name then
        begin match (dst, args) with
        | Some d, [ (_, _, v) ] ->
            let lv = lanes st v in
            let r = ref lv.(0) in
            for i = 1 to Array.length lv - 1 do
              r := bin st Or I1 !r lv.(i)
            done;
            Hashtbl.replace st.subst d !r
        | _ -> unsupported "intrinsic %s" name
        end
      else if String.starts_with ~prefix:"llvm.masked.store." name then
        begin match args with
        | [ ((Vector (_, t) as vt), _, v); (_, _, p); _; (_, _, m) ] ->
            let lv = lanes st v and lm = lanes st m in
            Array.iteri
              (fun i x ->
                let q = lane_ptr st vt p i in
                emit st
                  (Call
                     ( None,
                       No_extension,
                       Void,
                       "llvm.fas.store_if",
                       [
                         (I1, No_extension, lm.(i));
                         (t, No_extension, x);
                         (value_ty q, No_extension, q);
                       ] )))
              lv
        | _ -> unsupported "intrinsic %s" name
        end
      else if String.starts_with ~prefix:"llvm.masked.load." name then
        begin match (dst, args) with
        | Some d, [ (_, _, p); _; (_, _, m); ((Vector (n, t) as vt), _, pass) ] ->
            let lp = lanes st pass and lm = lanes st m in
            for i = 0 to n - 1 do
              let q = lane_ptr st vt p i in
              emit st
                (Call
                   ( Some (lid st d i),
                     No_extension,
                     t,
                     "llvm.fas.load_if",
                     [
                       (I1, No_extension, lm.(i));
                       (value_ty q, No_extension, q);
                       (t, No_extension, lp.(i));
                     ] ))
            done
        | _ -> unsupported "intrinsic %s" name
        end
      else if
        String.starts_with ~prefix:"llvm.masked." name
        || String.starts_with ~prefix:"llvm.experimental." name
      then unsupported "intrinsic %s" name
      else if String.starts_with ~prefix:"llvm." name then
        begin match (dst, rty) with
        | Some d, Vector (n, t) ->
            let sname = scalar_intrinsic name in
            let largs =
              List.map
                (fun (at, e, v) ->
                  match at with
                  | Vector (_, lt) -> `L (lt, lanes st v)
                  | _ -> `S (at, e, v))
                args
            in
            for i = 0 to n - 1 do
              let a =
                List.map
                  (function `L (lt, l) -> (lt, No_extension, l.(i)) | `S x -> x)
                  largs
              in
              emit st (Call (Some (lid st d i), ext, t, sname, a))
            done
        | _ -> unsupported "intrinsic %s" name
        end
      else begin
        let args = expand_args st args in
        match rty with
        | Vector _ -> (
            let sty = storage_ty st rty in
            let tmp = fresh st in
            st.entry <- Alloca (tmp, sty, 32) :: st.entry;
            let p = Local (tmp, Pointer sty) in
            emit st
              (Call
                 ( None,
                   No_extension,
                   Void,
                   name,
                   args @ [ (Pointer I8, No_extension, p) ] ));
            match dst with
            | Some d -> (
                let ls = load_lanes st ~volatile:false rty p 32 in
                match rty with
                | Vector _ ->
                    for i = 0 to Array.length ls - 1 do
                      set st d i ls.(i)
                    done
                | _ -> ())
            | None -> ())
        | _ -> emit st (Call (dst, ext, rty, name, args))
      end
  | Call_indirect (_, _, rty, _, args)
    when is_vector rty || List.exists (fun (t, _, _) -> is_vector t) args ->
      unsupported "indirect call with vector values"
  | Syscall (_, args) when List.exists (fun v -> is_vector (value_ty v)) args ->
      unsupported "vector syscall argument"
  | ins -> emit st ins

(* A vector of [vt]'s piece type with scalar [x] in every lane *)
let splat st vt x =
  let pt = piece_ty st vt in
  match x with
  | Const (_, k) ->
      Const_vector
        (pt, List.init (match pt with Vector (c, _) -> c | _ -> 1) (fun _ -> k))
  | Undef _ -> Undef pt
  | _ ->
      let i = fresh st in
      emit st (Insert (i, pt, Undef pt, Const (I32, 0L), x));
      let s = fresh st in
      emit st (Shuffle_zero (s, pt, Local (i, pt)));
      Local (s, pt)

(* Lane 0 of [v], when it is known as a scalar *)
let lane0 st (v : value) =
  match v with
  | Local (i, _) -> Hashtbl.find_opt st.ins0 i
  | Const_vector (Vector (_, t), k :: _) -> Some (Const (t, k))
  | Zero (Vector (_, t)) -> Some (zero t)
  | Undef (Vector (_, t)) -> Some (Undef t)
  | _ -> None

let zero_mask : value -> bool = function
  | Zero _ -> true
  | Const_vector (_, xs) -> List.for_all (fun x -> x = 0L) xs
  | _ -> false

(* Chunk mode: the operations whose pieces are not independent *)
(* Chunk mode, variable lane index: spill the pieces to a stack slot and
   address the lane there. The index is masked into range, since an
   out-of-range lane is poison but must not touch other memory. *)
let dyn_lane st vt pieces idx =
  let n, t = match vt with Vector (n, t) -> (n, t) | _ -> invalid_arg "dyn_lane" in
  if n land (n - 1) <> 0 then unsupported "vector of %d lanes" n;
  let sty = storage_ty st vt in
  let p =
    match Ptbl.find_opt st.dyn vt with
    | Some p -> p
    | None ->
        let tmp = fresh st in
        st.entry <- Alloca (tmp, sty, 16) :: st.entry;
        let p = Local (tmp, Pointer sty) in
        Ptbl.replace st.dyn vt p;
        p
  in
  store_lanes st ~volatile:false vt pieces p 16;
  let i = match value_ty idx with I64 -> idx | it -> cast st "zext" it idx I64 in
  let i = bin st And I64 i (Const (I64, Int64.of_int (n - 1))) in
  let g = fresh st in
  emit st (Gep (g, sty, p, [ Zero; Index i ]));
  (p, Local (g, Pointer sty), lane_bits t / 8)

let instr_chunk st ins =
  match ins with
  | Extract (d, (Vector (_, t) as vt), v, idx) when index_const idx = None ->
      let _, q, al = dyn_lane st vt (lanes st v) idx in
      emit st (Load (d, t, q, al))
  | Insert (d, (Vector (_, t) as vt), v, idx, x) when index_const idx = None ->
      let p, q, al = dyn_lane st vt (lanes st v) idx in
      emit st (Store (t, x, q, al));
      let ls = load_lanes st ~volatile:false vt p 16 in
      Array.iteri (fun i l -> set st d i l) ls
  | Shufflevector (d, (Vector _ as vt), a, _, mask) when zero_mask mask ->
      let v =
        match lane0 st a with
        | Some x -> splat st vt x
        | None ->
            let pt = piece_ty st vt and s = fresh st in
            emit st (Shuffle_zero (s, pt, (lanes st a).(0)));
            Local (s, pt)
      in
      for i = 0 to npieces st vt - 1 do
        set st d i v
      done
  | Shuffle_zero (d, (Vector _ as vt), a) ->
      let v =
        match lane0 st a with
        | Some x -> splat st vt x
        | None ->
            let pt = piece_ty st vt and s = fresh st in
            emit st (Shuffle_zero (s, pt, (lanes st a).(0)));
            Local (s, pt)
      in
      for i = 0 to npieces st vt - 1 do
        set st d i v
      done
  | Extract (d, (Vector (n, t) as vt), v, idx) -> (
      match index_const idx with
      | Some k when k >= 0 && k < n ->
          let c = per st t in
          emit st
            (Extract
               ( d,
                 piece_ty st vt,
                 (lanes st v).(k / c),
                 Const (I32, Int64.of_int (k mod c)) ))
      | _ -> Hashtbl.replace st.subst d (Undef t))
  | Insert (d, (Vector (n, t) as vt), v, idx, x) -> (
      let lv = lanes st v in
      match index_const idx with
      | Some k when k >= 0 && k < n ->
          let c = per st t and pt = piece_ty st vt in
          if k = 0 then Hashtbl.replace st.ins0 d x;
          Array.iteri
            (fun i p ->
              if i = k / c then
                emit st
                  (Insert (lid st d i, pt, p, Const (I32, Int64.of_int (k mod c)), x))
              else set st d i p)
            lv
      | _ -> Array.iteri (fun i p -> set st d i p) lv)
  | Cast (d, "bitcast", (Vector _ as from), v, (Vector _ as target)) when from <> target
    ->
      let lv = lanes st v and pf = piece_ty st from and pt = piece_ty st target in
      Array.iteri (fun i p -> emit st (Cast (lid st d i, "bitcast", pf, p, pt))) lv
  | Alloca (d, (Vector _ as vt), al) when Hashtbl.mem st.split d ->
      let pt = piece_ty st vt in
      Array.iter
        (function
          | Local (q, _) -> emit st (Alloca (q, pt, if al <= 0 then 16 else min al 16))
          | _ -> ())
        (Hashtbl.find st.split d)
  | Load (d, (Vector _ as vt), Local (p, _), al) when Hashtbl.mem st.split p ->
      let pt = piece_ty st vt in
      Array.iteri
        (fun i q ->
          emit st (Load (lid st d i, pt, q, if al <= 0 then 16 else min al 16)))
        (Hashtbl.find st.split p)
  | Store ((Vector _ as vt), v, Local (p, _), al) when Hashtbl.mem st.split p ->
      let pt = piece_ty st vt and lv = lanes st v in
      Array.iteri
        (fun i q -> emit st (Store (pt, lv.(i), q, if al <= 0 then 16 else min al 16)))
        (Hashtbl.find st.split p)
  | ins -> instr_lanes st ins

(* i128 pairs: the low word at [p], the high word at [p + 8] *)
let pair_ptr st p i =
  if i = 0 then p
  else
    let q = fresh st in
    emit st (Gep (q, Array (2, I64), p, [ Zero; Index (Const (I64, Int64.of_int i)) ]));
    Local (q, Pointer (Array (2, I64)))

let is_pair st = function Local (id, I128) -> Hashtbl.mem st.pairs id | _ -> false

let instr_pair st ins =
  match ins with
  | (Load (d, I128, p, al) | Load_volatile (d, I128, p, al)) when Hashtbl.mem st.pairs d ->
      let al = if al <= 0 then 8 else min al 8 in
      for i = 0 to 1 do
        let q = pair_ptr st p i in
        emit st
          (match ins with
          | Load_volatile _ -> Load_volatile (lid st d i, I64, q, al)
          | _ -> Load (lid st d i, I64, q, al))
      done;
      true
  | (Store (I128, v, p, al) | Store_volatile (I128, v, p, al)) when is_pair st v ->
      let al = if al <= 0 then 8 else min al 8 in
      Array.iteri
        (fun i w ->
          let q = pair_ptr st p i in
          emit st
            (match ins with
            | Store_volatile _ -> Store_volatile (I64, w, q, al)
            | _ -> Store (I64, w, q, al)))
        (lanes st v);
      true
  | Cast (d, "bitcast", Vector _, v, I128) when Hashtbl.mem st.pairs d ->
      Array.iteri (fun i w -> set st d i w) (words_of st v);
      true
  | Cast (d, "bitcast", I128, v, Vector (n, t)) when is_pair st v ->
      Array.iteri (fun i x -> set st d i x) (unpack st (lanes st v) n t);
      true
  | _ -> false

let instr st ins =
  if not (instr_pair st ins || instr_slot st ins) then
    if st.chunk then instr_chunk st ins else instr_lanes st ins

(* Chunk mode applies to a function whose vectors all have integer lanes and a
   multiple of 128 bits, and whose vector operations all have SSE2 forms. *)
let simd = Sys.getenv_opt "FAS_NATIVE_NOSIMD" <> Some "1"
let chunk_lane = function I8 | I16 | I32 | I64 -> true | _ -> false

let chunk_ty = function
  | Vector (n, t) -> chunk_lane t && n * lane_bits t mod 128 = 0
  | Struct _ | Array _ | Pointer _ | I1 | I8 | I16 | I32 | I64 | I128 | F32 | F64 | Void
    ->
      true

let types_of ins =
  let ops = ref [] in
  Regalloc.iter_operands (fun v -> ops := value_ty v :: !ops) ins;
  let own =
    match ins with
    | Store (t, _, _, _)
    | Store_volatile (t, _, _, _)
    | Alloca (_, t, _)
    | Gep (_, t, _, _) ->
        [ t ]
    | Call (_, _, t, _, args) -> t :: List.map (fun (t, _, _) -> t) args
    | Call_indirect (_, _, t, _, args) -> t :: List.map (fun (t, _, _) -> t) args
    | Phi (_, t, incoming) -> t :: List.map (fun (v, _) -> value_ty v) incoming
    | _ -> []
  in
  (match instruction_result ins with Some (_, t) -> [ t ] | None -> []) @ own @ !ops

let chunkable (f : func) =
  simd
  &&
  let splats = Hashtbl.create 16 in
  List.iter
    (fun (b : block) ->
      List.iter
        (function
          | Shuffle_zero (d, _, _) -> Hashtbl.replace splats d ()
          | Shufflevector (d, _, _, _, m) when zero_mask m ->
              Hashtbl.replace splats d ()
          | _ -> ())
        b.instrs)
    f.blocks;
  let uniform : value -> bool = function
    | Const_vector (_, x :: xs) -> List.for_all (fun y -> y = x) xs
    | Zero _ | Undef _ -> true
    | Local (i, _) -> Hashtbl.mem splats i
    | _ -> false
  in
  let ok ins =
    let tys = types_of ins in
    List.for_all chunk_ty tys
    && ((not (List.exists is_vector tys))
       ||
       match ins with
       | Bin (_, op, Vector (_, t), _, y) -> (
           match op with
           | Add | Sub | And | Or | Xor -> true
           | Mul -> t = I64 || t = I16
           | Shl | Lshr -> t <> I8 && uniform y
           | Ashr -> (t = I32 || t = I16) && uniform y
           | _ -> false)
       | Extract (_, Vector (n, _), _, idx) | Insert (_, Vector (n, _), _, idx, _) ->
           index_const idx <> None || n land (n - 1) = 0
       | Shuffle_zero _ -> true
       | Shufflevector (_, _, _, _, m) -> zero_mask m
       | Load _ | Store _ | Alloca _ | Gep _ | Phi _ -> true
       | Cast (_, "bitcast", Vector _, _, Vector _) -> true
       | Call (_, _, _, name, _) -> not (String.starts_with ~prefix:"llvm." name)
       | _ -> false)
  in
  let debug = Sys.getenv_opt "FAS_SIMD_DEBUG" = Some "1" in
  let ok ins =
    let r = ok ins in
    if (not r) && debug then
      Printf.eprintf "%s: no chunk mode: %s\n" f.name
        (match ins with
        | Bin (_, op, _, _, _) -> (
            match op with
            | Mul -> "mul"
            | Shl -> "shl"
            | Lshr -> "lshr"
            | Ashr -> "ashr"
            | Udiv | Sdiv | Urem | Srem -> "div"
            | _ -> "bin")
        | Cmp _ -> "cmp"
        | Select _ -> "select"
        | Cast (_, k, _, _, _) -> "cast " ^ k
        | Call (_, _, _, n, _) -> "call " ^ n
        | Shufflevector _ -> "shufflevector"
        | Extract _ -> "extract"
        | Insert _ -> "insert"
        | _ -> "other");
    r
  in
  List.for_all (fun (p : param) -> chunk_ty p.ty) f.params
  && chunk_ty f.ret
  && List.for_all (fun (b : block) -> List.for_all ok b.instrs) f.blocks

(* Chunk mode: vector allocas used only by whole loads and stores of their
   own type become one alloca per piece *)
let splittable (f : func) =
  let cand = Hashtbl.create 8 in
  List.iter
    (fun (b : block) ->
      List.iter
        (function
          | Alloca (d, (Vector _ as t), _) -> Hashtbl.replace cand d t | _ -> ())
        b.instrs)
    f.blocks;
  let reject = function Local (i, _) -> Hashtbl.remove cand i | _ -> () in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          match ins with
          | Load (_, t, Local (p, _), _) when Hashtbl.find_opt cand p = Some t -> ()
          | Store (t, v, Local (p, _), _) when Hashtbl.find_opt cand p = Some t ->
              reject v
          | ins -> Regalloc.iter_operands reject ins)
        b.instrs;
      Regalloc.iter_term_operands reject b.terminator)
    f.blocks;
  cand

(* Does [f] mention vectors at all? *)
(* Instructions that expand into [per] scalar instructions for each lane,
   lane after lane, each reading only the same lane of its vector operands.
   Plain loads and stores of a vector alloca of the function qualify: every
   access to it inside a run has the same lane layout (any other pointer ends
   the run), so lane order cannot reorder overlapping accesses. *)
let lane_local st allocas = function
  | Bin (_, _, (Vector _ as vt), _, _) -> Some (npieces st vt, 1)
  | Fneg (_, Vector (n, _), _) | Cmp (_, _, Vector (n, _), _, _) -> Some (n, 1)
  | Select (_, _, a, _) -> (
      match value_ty a with Vector (n, _) -> Some (n, 1) | _ -> None)
  | Cast (_, kind, Vector (n, _), _, Vector (m, _)) when n = m && kind <> "bitcast" ->
      Some (n, 1)
  | Load (_, (Vector (_, t) as vt), Local (p, _), _)
  | Store ((Vector (_, t) as vt), _, Local (p, _), _)
    when t <> I1 && Hashtbl.mem allocas p ->
      Some (npieces st vt, if Hashtbl.mem st.split p then 1 else 2)
  | _ -> None

(* An instruction that emits nothing but makes lane i of its result (or a
   scalar) stand for another lane's value: the pending runs must be flushed
   first, as a later lane i may read a lane that is still in a later bucket *)
let moves_lanes = function
  | Shuffle_zero _ | Alloca _ -> false
  | Shufflevector (_, _, _, _, m) -> not (zero_mask m)
  | Insert (_, _, _, idx, _) -> index_const idx = None
  | Cast (_, "bitcast", from, _, target) -> from <> target
  | ins -> List.exists is_vector (types_of ins)

let lane_major st allocas scalars instrs =
  let buckets = ref [||] in
  let flush () =
    (* each bucket is reversed, like [st.out] *)
    Array.iter (fun l -> st.out <- l @ st.out) !buckets;
    buckets := [||]
  in
  List.iter
    (fun ins ->
      match lane_local st allocas ins with
      | Some (n, per) when n > 1 ->
          let saved = st.out in
          st.out <- [];
          instr st ins;
          let emitted = st.out in
          st.out <- saved;
          if List.length emitted <> n * per then begin
            flush ();
            st.out <- emitted @ st.out
          end
          else begin
            if Array.length !buckets <> n then begin
              flush ();
              buckets := Array.make n []
            end;
            (* [emitted] is reversed (lane n-1 first); walk it in program
               order so each bucket stays reversed, also within a lane's
               [per] instructions (a gep before its store) *)
            List.iteri
              (fun k x ->
                let i = k / per in
                !buckets.(i) <- x :: !buckets.(i))
              (List.rev emitted)
          end
      | _ -> (
          let saved = st.out in
          st.out <- [];
          instr st ins;
          let emitted = st.out in
          st.out <- saved;
          match ins with
          | Load (_, _, Local (p, _), _) when Hashtbl.mem scalars p ->
              (* a scalar alloca is not touched by the pending lanes, so its
                 load goes ahead of them and the run continues *)
              st.out <- emitted @ st.out
          | _ ->
              if emitted <> [] || moves_lanes ins then begin
                flush ();
                st.out <- emitted @ st.out
              end))
    instrs;
  flush ()

(* A size for the inliner that leaves out what promotion and scalarization
   remove: allocas, loads and stores of allocas, and splats *)
let vector_size (f : func) =
  let allocas = Hashtbl.create 16 in
  List.iter
    (fun (b : block) ->
      List.iter
        (function Alloca (d, _, _) -> Hashtbl.replace allocas d () | _ -> ())
        b.instrs)
    f.blocks;
  let local = function Local (i, _) -> Hashtbl.mem allocas i | _ -> false in
  List.fold_left
    (fun n (b : block) ->
      List.fold_left
        (fun n ins ->
          match ins with
          | Alloca _ | Location _ | Shuffle_zero _ -> n
          | Insert (_, _, (Zero _ | Undef _), _, _) -> n
          | Shufflevector (_, _, _, _, Zero _) -> n
          | (Load (_, _, p, _) | Store (_, _, p, _)) when local p -> n
          | _ -> n + 1)
        n b.instrs)
    0 f.blocks

(* Bool vectors over 64 lanes reach memory through i128: the front end
   bitcasts each <128 x i1> chunk to i128 and loads or stores that. The native
   backend has no i128 values, so such an i128 becomes a pair of i64 words,
   when every use of it is a store of it or a bitcast back to a vector. *)
let i128_pairs (f : func) =
  let cand = Hashtbl.create 8 in
  List.iter
    (fun (b : block) ->
      List.iter
        (function
          | Load (d, I128, _, _) | Load_volatile (d, I128, _, _)
          | Cast (d, "bitcast", Vector _, _, I128) ->
              Hashtbl.replace cand d ()
          | _ -> ())
        b.instrs)
    f.blocks;
  if Hashtbl.length cand > 0 then begin
    let bad v =
      (match v with Local (i, _) -> Hashtbl.remove cand i | _ -> ());
      v
    in
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match ins with
            | Store (I128, Local _, p, _) | Store_volatile (I128, Local _, p, _) ->
                ignore (bad p)
            | Cast (_, "bitcast", I128, Local _, Vector _) -> ()
            | _ -> ignore (Ir_fold.map_instr bad ins))
          b.instrs;
        ignore (Ir_fold.map_term bad b.terminator))
      f.blocks
  end;
  cand

let has_vectors (f : func) =
  is_vector f.ret
  || List.exists (fun (p : param) -> is_vector p.ty) f.params
  || List.exists
       (fun (b : block) ->
         List.exists
           (fun ins ->
             (match instruction_result ins with
               | Some (_, t) -> is_vector t
               | None -> false)
             ||
             match ins with
             | Store (t, _, _, _)
             | Store_volatile (t, _, _, _)
             | Alloca (_, t, _)
             | Gep (_, t, _, _) ->
                 is_vector t
             | Call (_, _, _, _, args) ->
                 List.exists (fun (t, _, _) -> is_vector t) args
             | _ -> false)
           b.instrs
         || match b.terminator with Ret (Some (t, _)) -> is_vector t | _ -> false)
       f.blocks

let func structs (f : func) =
  if not (has_vectors f) then f
  else begin
    let st =
      {
        structs;
        base = Hashtbl.create 16;
        subst = Hashtbl.create 64;
        next = 0;
        out = [];
        entry = [];
        chunk = chunkable f;
        split = Hashtbl.create 8;
        ppieces = Ptbl.create 4;
        ins0 = Hashtbl.create 16;
        dyn = Ptbl.create 2;
        pairs = i128_pairs f;
        slot_of = Hashtbl.create 8;
        users = Hashtbl.create 64;
        cur = 0;
        const_slot = Ptbl.create 4;
        guarded = Ptbl.create 16;
      }
    in
    (* uses of vector definitions, for the slot lowering *)
    if not st.chunk then begin
      (* variable inserts used once, by `select c (insert v i x) v` in the
         same block *)
      let var_ins = Hashtbl.create 16 and count = Hashtbl.create 64 in
      let tally v =
        (match v with
        | Local (id, _) ->
            Hashtbl.replace count id (1 + Option.value (Hashtbl.find_opt count id) ~default:0)
        | _ -> ());
        v
      in
      List.iter
        (fun (b : block) ->
          List.iter
            (fun ins ->
              (match ins with
              | Insert (d, vt, v, idx, x) when slot_lane st vt && index_const idx = None ->
                  Hashtbl.replace var_ins d (vt, v, idx, x)
              | _ -> ());
              ignore (Ir_fold.map_instr tally ins))
            b.instrs;
          ignore (Ir_fold.map_term tally b.terminator))
        f.blocks;
      List.iter
        (fun (b : block) ->
          let here = Hashtbl.create 16 in
          List.iter
            (fun ins ->
              match ins with
              | Insert (d, _, _, _, _) when Hashtbl.mem var_ins d -> Hashtbl.replace here d ()
              | Select (_, c, Local (a, _), w)
                when Hashtbl.mem here a
                     && Hashtbl.find_opt count a = Some 1
                     && (not (is_vector (value_ty c)))
                     && (let _, v, _, _ = Hashtbl.find var_ins a in v = w) ->
                  Ptbl.replace st.guarded a (Hashtbl.find var_ins a)
              | _ -> ())
            b.instrs)
        f.blocks
    end;
    if not st.chunk then
      List.iteri
        (fun bi (b : block) ->
          let note ok v =
            (match v with
            | Local (id, Vector _) ->
                Hashtbl.replace st.users id ((bi, ok) :: uses_of st id)
            | _ -> ());
            v
          in
          List.iter
            (fun ins ->
              match ins with
              | Extract (_, vt, (Local _ as v), idx)
                when slot_lane st vt && index_const idx = None ->
                  ignore (note true v);
                  ignore (note false idx)
              | Insert (_, vt, (Local _ as v), idx, x)
                when slot_lane st vt && index_const idx = None ->
                  ignore (note true v);
                  ignore (note false idx);
                  ignore (note false x)
              | Select (_, c, Local (a, _), w) when Ptbl.mem st.guarded a ->
                  ignore (note false c);
                  ignore (note true w)
              | _ -> ignore (Ir_fold.map_instr (note false) ins))
            b.instrs;
          ignore (Ir_fold.map_term (note false) b.terminator))
        f.blocks;
    let top = ref 0 in
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match instruction_result ins with
            | Some (i, _) -> if i > !top then top := i
            | None -> ())
          b.instrs)
      f.blocks;
    st.next <- !top + 1;
    let allocas = Hashtbl.create 8 and scalars = Hashtbl.create 8 in
    List.iter
      (fun (b : block) ->
        List.iter
          (function
            | Alloca (d, Vector _, _) -> Hashtbl.replace allocas d ()
            | Alloca (d, _, _) -> Hashtbl.replace scalars d ()
            | _ -> ())
          b.instrs)
      f.blocks;
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match instruction_result ins with
            | Some (i, (Vector _ as vt)) ->
                Hashtbl.replace st.base i st.next;
                st.next <- st.next + npieces st vt
            | Some (i, I128) when Hashtbl.mem st.pairs i ->
                Hashtbl.replace st.base i st.next;
                st.next <- st.next + 2
            | _ -> ())
          b.instrs)
      f.blocks;
    (* chunk mode: piece allocas, and pieces built from the lanes of vector
       parameters at entry *)
    let prologue =
      if not st.chunk then []
      else begin
        Hashtbl.iter
          (fun d vt ->
            let pt = piece_ty st vt in
            Hashtbl.replace st.split d
              (Array.init (npieces st vt) (fun _ -> Local (fresh st, Pointer pt))))
          (splittable f);
        st.out <- [];
        List.iter
          (fun (p : param) ->
            match p.ty with
            | Vector (_, t) as vt ->
                let pt = piece_ty st vt and c = per st t in
                Ptbl.replace st.ppieces p.name
                  (Array.init (npieces st vt) (fun i ->
                       let acc = ref (Undef pt) in
                       for j = 0 to c - 1 do
                         let d = fresh st in
                         emit st
                           (Insert
                              ( d,
                                pt,
                                !acc,
                                Const (I32, Int64.of_int j),
                                Param (lane_name p.name ((i * c) + j), t) ));
                         acc := Local (d, pt)
                       done;
                       !acc))
            | _ -> ())
          f.params;
        List.rev st.out
      end
    in
    let blocks =
      List.mapi
        (fun bi (b : block) ->
          st.out <- [];
          st.cur <- bi;
          (* a slot filled in another block may not dominate this one *)
          Hashtbl.reset st.slot_of;
          Ptbl.reset st.const_slot;
          if bi = 0 then st.out <- List.rev prologue;
          lane_major st allocas scalars b.instrs;
          let terminator =
            match b.terminator with
            | Ret (Some ((Vector _ as vt), v)) ->
                store_lanes st ~volatile:false vt (lanes st v)
                  (Param (vret, Pointer I8))
                  32;
                Ret None
            | t -> t
          in
          { b with instrs = List.rev st.out; terminator })
        f.blocks
    in
    let blocks =
      match (blocks, st.entry) with
      | _, [] -> blocks
      | b :: rest, allocas ->
          { b with instrs = List.rev_append allocas b.instrs } :: rest
      | [], _ -> blocks
    in
    let blocks =
      if Hashtbl.length st.subst = 0 then blocks
      else
        let rec resolve v =
          match v with
          | Local (i, _) -> (
              match Hashtbl.find_opt st.subst i with Some w -> resolve w | None -> v)
          | v -> v
        in
        List.map
          (fun (b : block) ->
            {
              b with
              instrs = List.map (Ir_fold.map_instr resolve) b.instrs;
              terminator = Ir_fold.map_term resolve b.terminator;
            })
          blocks
    in
    let params =
      List.concat_map
        (fun (p : param) ->
          match p.ty with
          | Vector (n, t) ->
              List.init n (fun i ->
                  { name = lane_name p.name i; ty = t; extension = No_extension })
          | _ -> [ p ])
        f.params
    in
    let params, ret =
      if is_vector f.ret then
        (params @ [ { name = vret; ty = Pointer I8; extension = No_extension } ], Void)
      else (params, f.ret)
    in
    { f with blocks; params; ret }
  end

(* `trunc (lshr (mul (zext a) (zext b)) 64)` on i128 — how `mul_hi` of 64-bit
   values is lowered — becomes the pseudo-intrinsic `llvm.fas.umulh` (`smulh`
   for sext/ashr), one `mul` on x86. The i128 instructions become dead. *)
let mul_high (f : func) =
  let uses_i128 =
    List.exists
      (fun (b : block) ->
        List.exists (function Bin (_, Mul, I128, _, _) -> true | _ -> false) b.instrs)
      f.blocks
  in
  if not uses_i128 then f
  else begin
    let defs = Hashtbl.create 16 in
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match ins with
            | Cast (d, _, _, _, I128) | Bin (d, _, I128, _, _) ->
                Hashtbl.replace defs d ins
            | _ -> ())
          b.instrs)
      f.blocks;
    let find = function Local (i, _) -> Hashtbl.find_opt defs i | _ -> None in
    let widened kind v =
      match find v with
      | Some (Cast (_, k, I64, x, I128)) when k = kind -> Some x
      | _ -> None
    in
    let rewrite = function
      | Cast (d, "trunc", I128, h, I64) as ins -> (
          match find h with
          | Some (Bin (_, ((Lshr | Ashr) as sh), I128, p, Const (_, 64L))) -> (
              match find p with
              | Some (Bin (_, Mul, I128, x, y)) -> (
                  let kind, name =
                    if sh = Lshr then ("zext", "llvm.fas.umulh")
                    else ("sext", "llvm.fas.smulh")
                  in
                  match (widened kind x, widened kind y) with
                  | Some a, Some b ->
                      Call
                        ( Some d,
                          No_extension,
                          I64,
                          name,
                          [ (I64, No_extension, a); (I64, No_extension, b) ] )
                  | _ -> ins)
              | _ -> ins)
          | _ -> ins)
      | ins -> ins
    in
    {
      f with
      blocks =
        List.map
          (fun (b : block) -> { b with instrs = List.map rewrite b.instrs })
          f.blocks;
    }
  end

let module_ (m : module_) =
  let m =
    if not (List.exists has_vectors m.funcs) then m
    else { m with funcs = List.map (func m.structs) m.funcs }
  in
  { m with funcs = List.map mul_high m.funcs }
