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

(* The integer type holding a bool vector in memory *)
let packed_ty st vt =
  match bytes st vt with
  | 1 -> I8
  | 2 -> I16
  | 4 -> I32
  | 8 -> I64
  | _ -> unsupported "bool vector wider than 64 lanes"

let lid st d i =
  match Hashtbl.find_opt st.base d with
  | Some b -> b + i
  | None -> unsupported "vector value without a definition"

let lanes st v =
  match v with
  | Local (id, Vector (n, t)) ->
      let b = lid st id 0 in
      Array.init n (fun i -> Local (b + i, t))
  | Param (name, Vector (n, t)) -> Array.init n (fun i -> Param (lane_name name i, t))
  | Const_vector (Vector (n, t), xs) ->
      if List.length xs <> n then unsupported "vector constant";
      Array.of_list (List.map (fun x -> Const (t, x)) xs)
  | Zero (Vector (n, t)) -> Array.make n (zero t)
  | Undef (Vector (n, t)) -> Array.make n (Undef t)
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
  | Vector (n, I1) ->
      let it = packed_ty st vt in
      let d = fresh st in
      emit st (if volatile then Load_volatile (d, it, p, al) else Load (d, it, p, al));
      let w = cast st "zext" it (Local (d, it)) I64 in
      unpack st [| w |] n I1
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
  | Vector (_, I1) ->
      let it = packed_ty st vt in
      let w = (pack st I1 ls).(0) in
      let x = cast st "trunc" I64 w it in
      emit st (if volatile then Store_volatile (it, x, p, al) else Store (it, x, p, al))
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
      | Vector (_, lt) ->
          Array.to_list (Array.map (fun l -> (lt, No_extension, l)) (lanes st v))
      | _ ->
          ignore e;
          [ a ])
    args

let instr st ins =
  match ins with
  | Bin (d, op, Vector (n, t), a, b) ->
      let la = lanes st a and lb = lanes st b in
      for i = 0 to n - 1 do
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
        | Vector (n, _) ->
            let lv = lanes st v in
            for i = 0 to n - 1 do
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
      for i = 0 to n - 1 do
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
  | Phi (d, Vector (n, t), incoming) ->
      let ins = List.map (fun (v, b) -> (lanes st v, b)) incoming in
      for i = 0 to n - 1 do
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
                | Vector (n, _) ->
                    for i = 0 to n - 1 do
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

(* Does [f] mention vectors at all? *)
(* Instructions that expand into [per] scalar instructions for each lane,
   lane after lane, each reading only the same lane of its vector operands.
   Plain loads and stores of a vector alloca of the function qualify: every
   access to it inside a run has the same lane layout (any other pointer ends
   the run), so lane order cannot reorder overlapping accesses. *)
let lane_local allocas = function
  | Bin (_, _, Vector (n, _), _, _)
  | Fneg (_, Vector (n, _), _)
  | Cmp (_, _, Vector (n, _), _, _) ->
      Some (n, 1)
  | Select (_, _, a, _) -> (
      match value_ty a with Vector (n, _) -> Some (n, 1) | _ -> None)
  | Cast (_, kind, Vector (n, _), _, Vector (m, _)) when n = m && kind <> "bitcast" ->
      Some (n, 1)
  | Load (_, Vector (n, t), Local (p, _), _)
  | Store (Vector (n, t), _, Local (p, _), _)
    when t <> I1 && Hashtbl.mem allocas p ->
      Some (n, 2)
  | _ -> None

let lane_major st allocas instrs =
  let buckets = ref [||] in
  let flush () =
    (* each bucket is reversed, like [st.out] *)
    Array.iter (fun l -> st.out <- l @ st.out) !buckets;
    buckets := [||]
  in
  List.iter
    (fun ins ->
      match lane_local allocas ins with
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
            (* [emitted] is reversed: lane n-1 first *)
            List.iteri
              (fun k x ->
                let i = n - 1 - (k / per) in
                !buckets.(i) <- x :: !buckets.(i))
              emitted
          end
      | _ ->
          let saved = st.out in
          st.out <- [];
          instr st ins;
          let emitted = st.out in
          st.out <- saved;
          if emitted <> [] then begin
            flush ();
            st.out <- emitted @ st.out
          end)
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
      }
    in
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
    let allocas = Hashtbl.create 8 in
    List.iter
      (fun (b : block) ->
        List.iter
          (function Alloca (d, Vector _, _) -> Hashtbl.replace allocas d () | _ -> ())
          b.instrs)
      f.blocks;
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match instruction_result ins with
            | Some (i, Vector (n, _)) ->
                Hashtbl.replace st.base i st.next;
                st.next <- st.next + n
            | _ -> ())
          b.instrs)
      f.blocks;
    let blocks =
      List.map
        (fun (b : block) ->
          st.out <- [];
          lane_major st allocas b.instrs;
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
