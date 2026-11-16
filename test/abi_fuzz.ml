(* ABI fuzzing, Fir <-> C in both directions (roadmap v0.7 hardening).

   Each seed makes a C file and a Fir module with several cases. A case is
   one random C signature: scalars of every SysV class (_Bool, narrow and
   wide integers, __int128, _Float16, float, double, long double,
   pointers), GNU vectors, structs (nested, with arrays, packed, or
   over-aligned) and unions, and optionally a variadic tail. For case K:

   - c_callee_K (C) and fir_callee_K (Fir) both hash every argument they
     receive, leaf by leaf, and build their result from the hash;
   - ref_K (C) calls c_callee_K: the reference, C to C;
   - c2f_K (C) calls fir_callee_K: C to Fir;
   - f2c_K (Fir) calls c_callee_K: Fir to C;
   - f2f_K (Fir) calls fir_callee_K: Fir to Fir.

   Each caller hashes the result it gets back, so all four must print the
   same number. The C side is compiled by gcc (the native backend follows
   gcc where gcc and clang disagree, SPEC §5); the Fir module is built by
   the native backend and, as a cross-check, through LLVM. When only the
   native build disagrees with C, the native backend is wrong; when both
   do, the shared classifier (Fir_abi) or this generator is.

   The value of a leaf is made from a 64-bit number the same way on both
   sides (see [c_make] and [fir_make]), so no float is ever a NaN and every
   conversion is exact. *)

(* ---------- C types ---------- *)

type sc = Bool | Int of int * bool (* bits, signed *) | I128 of bool | F16 | F32 | F64 | F80 | Ptr

type cty =
  | Sc of sc
  | Vec of int * sc  (* lanes of Int or F32/F64; GNU vector_size *)
  | Struct of int * cty list * attr  (* id, members *)
  | Union of int * cty list
  | Arr of cty * int  (* only as a member *)
  | Cplx of sc  (* _Complex of F16/F32/F64/F80: `complex T` in an agg *)

and attr = Plain | Packed | Aligned of int

let sc_size = function
  | Bool -> 1
  | Int (w, _) -> w / 8
  | I128 _ -> 16
  | F16 -> 2
  | F32 -> 4
  | F64 | Ptr -> 8
  | F80 -> 16

let sc_align = sc_size

let rec size_align = function
  | Sc s -> (sc_size s, sc_align s)
  | Vec (n, e) -> (n * sc_size e, n * sc_size e)
  | Cplx e -> (2 * sc_size e, sc_size e)
  | Arr (t, n) ->
      let s, a = size_align t in
      (s * n, a)
  | Union (_, ms) ->
      let s, a = List.fold_left (fun (s, a) m -> let s', a' = size_align m in (max s s', max a a')) (0, 1) ms in
      ((s + a - 1) / a * a, a)
  | Struct (_, ms, attr) ->
      let off = ref 0 and al = ref 1 in
      List.iter
        (fun m ->
          let s, a = size_align m in
          let a = if attr = Packed then 1 else a in
          off := (!off + a - 1) / a * a + s;
          al := max !al a)
        ms;
      let al = match attr with Aligned n -> max n !al | _ -> !al in
      ((!off + al - 1) / al * al, al)

(* member offsets of a struct *)
let offsets ms attr =
  let off = ref 0 in
  List.map
    (fun m ->
      let s, a = size_align m in
      let a = if attr = Packed then 1 else a in
      let o = (!off + a - 1) / a * a in
      off := o + s;
      o)
    ms

(* The scalar leaves of a type, with their byte offsets and a C access
   path suffix. [all] (for the ABI field list) includes every union
   member; otherwise only a union's first member, the one that is set and
   hashed. Vector lanes are leaves of the element type. *)
let rec leaves ~all t path off =
  match t with
  | Sc s -> [ (off, s, path) ]
  | Vec (n, e) -> List.init n (fun i -> (off + (i * sc_size e), e, Printf.sprintf "%s[%d]" path i))
  | Cplx e -> [ (off, e, path ^ "@re"); (off + sc_size e, e, path ^ "@im") ]
  | Arr (t, n) ->
      let s, _ = size_align t in
      List.concat (List.init n (fun i -> leaves ~all t (Printf.sprintf "%s[%d]" path i) (off + (i * s))))
  | Struct (_, ms, attr) ->
      List.concat (List.mapi (fun i (m, o) -> leaves ~all m (Printf.sprintf "%s.f%d" path i) (off + o)) (List.combine ms (offsets ms attr)))
  | Union (_, ms) ->
      let ms = if all then ms else [ List.hd ms ] in
      List.concat (List.mapi (fun i m -> leaves ~all m (Printf.sprintf "%s.f%d" path i) off) ms)

(* the C lvalue of leaf [path] of [base]: a complex part is the last step
   of a path, `@re` or `@im`, and is reached with __real__ / __imag__ *)
let c_path base path =
  let part suffix op =
    if String.ends_with ~suffix path then
      Some (Printf.sprintf "%s (%s%s)" op base (String.sub path 0 (String.length path - 3)))
    else None
  in
  match part "@re" "__real__" with
  | Some e -> e
  | None -> ( match part "@im" "__imag__" with Some e -> e | None -> base ^ path)

(* ---------- type text ---------- *)

let c_sc = function
  | Bool -> "_Bool"
  | Int (w, true) -> Printf.sprintf "int%d_t" w
  | Int (w, false) -> Printf.sprintf "uint%d_t" w
  | I128 true -> "__int128"
  | I128 false -> "unsigned __int128"
  | F16 -> "_Float16"
  | F32 -> "float"
  | F64 -> "double"
  | F80 -> "long double"
  | Ptr -> "void *"

let vec_name n e = Printf.sprintf "v%d_%s" n (match e with Int (w, _) -> "i" ^ string_of_int w | F32 -> "f32" | F64 -> "f64" | _ -> assert false)

let c_ty = function
  | Sc s -> c_sc s
  | Vec (n, e) -> vec_name n e
  | Struct (id, _, _) -> Printf.sprintf "struct S%d" id
  | Union (id, _) -> Printf.sprintf "union U%d" id
  | Cplx e -> "_Complex " ^ c_sc e
  | Arr _ -> assert false

let fir_sc = function
  | Bool -> "i8" (* in memory; a scalar parameter is zext i1 *)
  | Int (w, _) -> "i" ^ string_of_int w
  | I128 _ -> "i128"
  | F16 -> "f16"
  | F32 -> "f32"
  | F64 -> "f64"
  | F80 -> "f80"
  | Ptr -> "ptr"

let fir_vec n e = Printf.sprintf "<%d x %s>" n (fir_sc e)

(* the ABI field list: every scalar, vector fields whole *)
let rec abi_fields t off =
  match t with
  | Sc s -> [ Printf.sprintf "%d: %s" off (fir_sc s) ]
  | Vec (n, e) -> [ Printf.sprintf "%d: %s" off (fir_vec n e) ]
  | Cplx e -> [ Printf.sprintf "%d: complex %s" off (fir_sc e) ]
  | Arr (t, n) ->
      let s, _ = size_align t in
      List.concat (List.init n (fun i -> abi_fields t (off + (i * s))))
  | Struct _ | Union _ -> [ Printf.sprintf "%d: %s" off (agg_text t) ]

(* a struct or union as a Fir agg: its members' fields, a nested struct or
   union as a nested agg (SPEC §5; gcc classifies each on its own) *)
and agg_text t =
  let s, a = size_align t in
  let fs =
    match t with
    | Struct (_, ms, attr) -> List.concat (List.map2 (fun m o -> abi_fields m o) ms (offsets ms attr))
    | Union (_, ms) -> List.concat (List.map (fun m -> abi_fields m 0) ms)
    | _ -> assert false
  in
  Printf.sprintf "agg(%d, %d, [%s])" s a (String.concat ", " fs)

(* passed as a Fir agg: a struct, a union, or a complex number on its own
   (the `complex T` ABI, which is an agg(2S, S, [0: complex T]) except that a
   `complex f80` result comes back in st0/st1 where a struct holding one is
   memory) *)
let is_agg = function Struct _ | Union _ | Cplx _ -> true | _ -> false

let fir_abi t =
  match t with
  | Sc Bool -> "zext i1"
  | Sc (Int (w, s)) when w < 32 -> (if s then "sext " else "zext ") ^ fir_sc (Int (w, s))
  | Sc s -> fir_sc s
  | Vec (n, e) -> fir_vec n e
  | Struct _ | Union _ -> agg_text t
  | Cplx e -> "complex " ^ fir_sc e
  | Arr _ -> assert false

(* the entry-block parameter type *)
let fir_param_ty = function
  | Sc Bool -> "i1"
  | Sc s -> fir_sc s
  | Vec (n, e) -> fir_vec n e
  | _ -> "ptr"

(* ---------- generation ---------- *)

type case = { params : cty list; nfixed : int; ret : cty option; args : int64 list list }
(* [args]: per parameter, one number per leaf (as [leaves ~all:false]) *)

let rs = ref (Random.State.make [| 0 |])
let int n = Random.State.int !rs n
let chance p = Random.State.float !rs 1.0 < p
let choose l = List.nth l (int (List.length l))
let next_id = ref 0

let scalars = [ Bool; Int (8, true); Int (8, false); Int (16, true); Int (16, false); Int (32, true); Int (32, false); Int (64, true); Int (64, false); I128 true; I128 false; F16; F32; F64; F80; Ptr ]

let vecs =
  [ (4, Int (8, true)); (2, Int (16, true)); (8, Int (8, false)); (4, Int (16, true)); (2, Int (32, true)); (2, F32);
    (16, Int (8, true)); (8, Int (16, false)); (4, Int (32, true)); (2, Int (64, true)); (4, F32); (2, F64);
    (8, F32); (4, Int (64, false)); (4, F64) ]

let cplx = [ F16; F32; F64; F80 ]

let rec gen_member depth ~packed =
  match int 10 with
  | 0 | 1 when depth > 0 -> gen_agg (depth - 1) ~packed
  | 2 when depth > 0 ->
      (* gcc 15 drops the elements of an array of _Complex _Float16 that
         cross an eightbyte, even with gcc on both sides (a struct
         {_Complex _Float16 a; _Complex _Float16 b[3];} arrives as a, b[0]
         and zeros); clang and the psABI put them in xmm0/xmm1 *)
      let rec cf16 = function
        | Cplx F16 -> true
        | Arr (t, _) -> cf16 t
        | Struct (_, ms, _) | Union (_, ms) -> List.exists cf16 ms
        | _ -> false
      in
      let m = gen_member (depth - 1) ~packed in
      let m = if cf16 m then Cplx F32 else m in
      (* an array of a packed struct whose size breaks a member's natural
         alignment puts that member misaligned in the later elements: clang
         (and Fir, and the psABI's "unaligned fields") make the whole agg
         MEMORY, gcc does not, as it checks alignment only inside the
         packed struct. One element keeps every member aligned *)
      let s, _ = size_align m in
      let misaligned = List.exists (fun (o, e, _) -> (s + o) mod sc_align e <> 0) (leaves ~all:true m "" 0) in
      Arr (m, if misaligned then 1 else 1 + int 4)
  | 3 when not packed ->
      let n, e = choose vecs in
      Vec (n, e)
  | 4 when not packed && chance 0.4 -> Cplx (choose cplx)
  | _ ->
      (* weighted to the small classes, where the eightbyte merging is *)
      let s = choose (scalars @ [ F32; F32; F64; Int (32, true); Int (8, false); F16 ]) in
      Sc (if packed && s = F80 then F64 else s)

and gen_agg depth ~packed =
  incr next_id;
  let id = !next_id in
  if chance 0.12 then Union (id, List.init (2 + int 2) (fun _ -> gen_member depth ~packed))
  else
    let packed = packed || chance 0.08 in
    let attr = if packed then Packed else if chance 0.08 then Aligned (choose [ 16; 32 ]) else Plain in
    Struct (id, List.init (1 + int 5) (fun _ -> gen_member depth ~packed), attr)

let gen_param () =
  match int 10 with
  | 0 | 1 | 2 | 3 -> gen_agg 2 ~packed:false
  | 4 ->
      let n, e = choose vecs in
      Vec (n, e)
  | 5 when chance 0.4 -> Cplx (choose cplx)
  | _ -> Sc (choose scalars)

(* through `...`, only what the default argument promotions leave alone *)
let gen_va () =
  match int 10 with
  | 0 | 1 | 2 -> gen_agg 2 ~packed:false
  | 3 ->
      let n, e = choose (List.filter (fun (n, e) -> n * sc_size e >= 8) vecs) in
      Vec (n, e)
  | 4 when chance 0.4 -> Cplx (choose [ F32; F64; F80 ])
  | _ -> Sc (choose [ Int (32, true); Int (64, true); Int (64, false); I128 true; F64; F80; Ptr ])

let leaf_numbers t =
  List.map (fun _ -> if chance 0.3 then choose [ 0L; 1L; -1L; Int64.min_int; Int64.max_int; 0x80L; 0x8000L; 0x80000000L ] else Random.State.bits64 !rs) (leaves ~all:false t "" 0)

let gen_case () =
  let nfixed = int 13 in
  let nva = if nfixed > 0 && chance 0.25 then 1 + int 6 else 0 in
  let params = List.init nfixed (fun _ -> gen_param ()) @ List.init nva (fun _ -> gen_va ()) in
  let ret = if chance 0.1 then None else Some (gen_param ()) in
  { params; nfixed; ret; args = List.map leaf_numbers params }

(* every struct, union and vector type in a case, inner ones first *)
let rec collect acc t =
  match t with
  | Struct (_, ms, _) | Union (_, ms) -> List.fold_left collect acc ms @ [ t ]
  | Arr (t, _) -> collect acc t
  | Vec _ -> acc @ [ t ]
  | Sc _ | Cplx _ -> acc

(* ---------- C text ---------- *)

let h0 = "0xcbf29ce484222325ULL"

(* the 64-bit number of leaf [k] of a result, from the hash [h] *)
let leaf_mult k = Int64.of_int ((2 * k) + 3)
let leaf_add k = Int64.mul (Int64.of_int k) 0x9e3779b97f4a7c15L

(* a leaf's value from the C expression [v] of type uint64_t *)
let c_make s v =
  match s with
  | Bool -> Printf.sprintf "(_Bool)((%s) & 1)" v
  | Int _ -> Printf.sprintf "(%s)(%s)" (c_sc s) v
  | I128 _ ->
      Printf.sprintf "(%s)(((unsigned __int128)(%s) << 64) | (unsigned __int128)((%s) ^ 0x5555555555555555ULL))" (c_sc s) v v
  | F16 -> Printf.sprintf "(_Float16)(int8_t)(%s)" v
  | F32 -> Printf.sprintf "(float)(int32_t)(%s)" v
  | F64 -> Printf.sprintf "(double)(int32_t)(%s)" v
  | F80 -> Printf.sprintf "(long double)(int64_t)(%s)" v
  | Ptr -> Printf.sprintf "(void *)(uintptr_t)(%s)" v

(* statements mixing leaf [x] into h *)
let c_hash s x =
  let mix e = Printf.sprintf "h = (h ^ (uint64_t)(%s)) * 0x100000001b3ULL;" e in
  match s with
  | Bool | Int (_, false) -> mix x
  | Int (_, true) -> mix (Printf.sprintf "(int64_t)(%s)" x)
  | I128 _ -> mix x ^ " " ^ mix (Printf.sprintf "(unsigned __int128)(%s) >> 64" x)
  | F16 -> mix (Printf.sprintf "bits16(%s)" x)
  | F32 -> mix (Printf.sprintf "bits32(%s)" x)
  | F64 -> mix (Printf.sprintf "bits64(%s)" x)
  | F80 -> mix (Printf.sprintf "f80lo(%s)" x) ^ " " ^ mix (Printf.sprintf "f80hi(%s)" x)
  | Ptr -> mix (Printf.sprintf "(uintptr_t)(%s)" x)

let c_lit x = Printf.sprintf "0x%LxULL" x

(* a C initializer for [t] whose leaves take the numbers [nums] *)
let rec c_init t nums =
  match t with
  | Sc s -> (
      match nums with x :: rest -> (c_make s (c_lit x), rest) | [] -> assert false)
  | Vec (n, e) ->
      let rec take k nums acc = if k = 0 then (List.rev acc, nums) else match nums with x :: r -> take (k - 1) r (c_make e (c_lit x) :: acc) | [] -> assert false in
      let es, rest = take n nums [] in
      (Printf.sprintf "(%s){%s}" (vec_name n e) (String.concat ", " es), rest)
  | Cplx e -> (
      match nums with
      | x :: y :: rest ->
          (Printf.sprintf "__builtin_complex(%s, %s)" (c_make e (c_lit x)) (c_make e (c_lit y)), rest)
      | _ -> assert false)
  | Arr (t, n) ->
      let rec go k nums acc = if k = 0 then (List.rev acc, nums) else let e, r = c_init t nums in go (k - 1) r (e :: acc) in
      let es, rest = go n nums [] in
      ("{" ^ String.concat ", " es ^ "}", rest)
  | Struct (_, ms, _) ->
      let rec go ms nums acc = match ms with [] -> (List.rev acc, nums) | m :: ms -> let e, r = c_init m nums in go ms r (e :: acc) in
      let es, rest = go ms nums [] in
      (Printf.sprintf "(%s){%s}" (c_ty t) (String.concat ", " es), rest)
  | Union (_, ms) ->
      let e, rest = c_init (List.hd ms) nums in
      (Printf.sprintf "(%s){.f0 = %s}" (c_ty t) e, rest)

let c_typedefs b (cases : case list) =
  let seen = Hashtbl.create 16 in
  List.iter
    (fun (c : case) ->
      List.iter
        (fun t ->
          List.iter
            (fun t ->
              let key = match t with Vec (n, e) -> vec_name n e | _ -> c_ty t in
              if not (Hashtbl.mem seen key) then begin
                Hashtbl.add seen key ();
                match t with
                | Vec (n, e) ->
                    Printf.bprintf b "typedef %s %s __attribute__((vector_size(%d)));\n" (c_sc e) (vec_name n e) (n * sc_size e)
                | Struct (_, ms, _) | Union (_, ms) ->
                    let kw = match t with Struct _ -> "struct" | _ -> "union" in
                    let id = match t with Struct (id, _, _) | Union (id, _) -> id | _ -> 0 in
                    Printf.bprintf b "%s %s%d {" kw (if kw = "struct" then "S" else "U") id;
                    List.iteri
                      (fun i m ->
                        match m with
                        | Arr (t, n) ->
                            (* nested arrays as arrays of arrays *)
                            let rec dims t acc = match t with Arr (t, n) -> dims t (acc ^ Printf.sprintf "[%d]" n) | t -> (t, acc) in
                            let et, ds = dims t (Printf.sprintf "[%d]" n) in
                            Printf.bprintf b " %s f%d%s;" (c_ty et) i ds
                        | m -> Printf.bprintf b " %s f%d;" (c_ty m) i)
                      ms;
                    Printf.bprintf b " }%s;\n"
                      (match t with
                      | Struct (_, _, Packed) -> " __attribute__((packed))"
                      | Struct (_, _, Aligned n) -> Printf.sprintf " __attribute__((aligned(%d)))" n
                      | _ -> "")
                | _ -> ()
              end)
            (collect [] t))
        (Option.to_list c.ret @ c.params))
    cases

let c_proto k (c : case) name =
  let ps = List.mapi (fun i t -> Printf.sprintf "%s a%d" (c_ty t) i) (List.filteri (fun i _ -> i < c.nfixed) c.params) in
  let ps = if List.length c.params > c.nfixed then ps @ [ "..." ] else if ps = [] then [ "void" ] else ps in
  Printf.sprintf "%s %s_%d(%s)" (match c.ret with None -> "void" | Some t -> c_ty t) name k (String.concat ", " ps)

let c_file (cases : case list) =
  let b = Buffer.create 8192 in
  Buffer.add_string b
    {|#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
static uint64_t bits16(_Float16 x) { uint16_t u; memcpy(&u, &x, 2); return u; }
static uint64_t bits32(float x) { uint32_t u; memcpy(&u, &x, 4); return u; }
static uint64_t bits64(double x) { uint64_t u; memcpy(&u, &x, 8); return u; }
static uint64_t f80lo(long double x) { uint64_t u; memcpy(&u, &x, 8); return u; }
static uint64_t f80hi(long double x) { uint16_t u; memcpy(&u, (char *)&x + 8, 2); return u; }
/* a void callee's hash */
uint64_t abi_sink;
|};
  c_typedefs b cases;
  List.iteri
    (fun k (c : case) ->
      (* the callee *)
      Printf.bprintf b "%s {\n  uint64_t h = %s;\n" (c_proto k c "c_callee") h0;
      let va = List.length c.params > c.nfixed in
      if va then Printf.bprintf b "  va_list ap; va_start(ap, a%d);\n" (c.nfixed - 1);
      List.iteri
        (fun i t ->
          if i >= c.nfixed then Printf.bprintf b "  %s a%d = va_arg(ap, %s);\n" (c_ty t) i (c_ty t);
          List.iter (fun (_, s, path) -> Printf.bprintf b "  %s\n" (c_hash s (c_path (Printf.sprintf "a%d" i) path))) (leaves ~all:false t "" 0))
        c.params;
      if va then Buffer.add_string b "  va_end(ap);\n";
      (match c.ret with
      | None -> Buffer.add_string b "  abi_sink = h;\n"
      | Some t ->
          Printf.bprintf b "  %s r;\n  memset(&r, 0, sizeof r);\n" (c_ty t);
          List.iteri
            (fun j (_, s, path) ->
              Printf.bprintf b "  %s = %s;\n" (c_path "r" path)
                (c_make s (Printf.sprintf "h * %LdULL + %s" (leaf_mult j) (c_lit (leaf_add j)))))
            (leaves ~all:false t "" 0);
          Buffer.add_string b "  return r;\n");
      Buffer.add_string b "}\n";
      Printf.bprintf b "%s;\n" (c_proto k c "fir_callee");
      (* the C callers: reference and C to Fir *)
      let args =
        String.concat ", "
          (List.map2 (fun t nums -> let e, rest = c_init t nums in assert (rest = []); e) c.params c.args)
      in
      List.iter
        (fun (name, callee) ->
          Printf.bprintf b "uint64_t %s_%d(void) {\n" name k;
          (match c.ret with
          | None -> Printf.bprintf b "  abi_sink = 0;\n  %s_%d(%s);\n  return abi_sink;\n" callee k args
          | Some t ->
              Printf.bprintf b "  %s r = %s_%d(%s);\n  uint64_t h = %s;\n" (c_ty t) callee k args h0;
              List.iter (fun (_, s, path) -> Printf.bprintf b "  %s\n" (c_hash s (c_path "r" path))) (leaves ~all:false t "" 0);
              Buffer.add_string b "  return h;\n");
          Buffer.add_string b "}\n")
        [ ("ref", "c_callee"); ("c2f", "fir_callee") ];
      Printf.bprintf b "uint64_t f2c_%d(void);\nuint64_t f2f_%d(void);\n" k k)
    cases;
  Buffer.add_string b "int main(void) {\n  int bad = 0;\n";
  List.iteri
    (fun k _ ->
      Printf.bprintf b
        "  { uint64_t r = ref_%d(), a = c2f_%d(), c = f2c_%d(), f = f2f_%d();\n\
        \    if (a != r) { printf(\"case %d: C to Fir %%016llx, C to C %%016llx\\n\", (unsigned long long)a, (unsigned long long)r); bad = 1; }\n\
        \    if (c != r) { printf(\"case %d: Fir to C %%016llx, C to C %%016llx\\n\", (unsigned long long)c, (unsigned long long)r); bad = 1; }\n\
        \    if (f != r) { printf(\"case %d: Fir to Fir %%016llx, C to C %%016llx\\n\", (unsigned long long)f, (unsigned long long)r); bad = 1; } }\n"
        k k k k k k k)
    cases;
  Buffer.add_string b "  if (!bad) printf(\"ok\\n\");\n  return bad;\n}\n";
  Buffer.contents b

(* ---------- Fir text ---------- *)

type fb = { b : Buffer.t; mutable n : int }

let fresh fb =
  fb.n <- fb.n + 1;
  Printf.sprintf "%%t%d" fb.n

let ln fb fmt = Printf.ksprintf (fun s -> Buffer.add_string fb.b ("  " ^ s ^ "\n")) fmt

let fir_sig (c : case) =
  let ps = List.map fir_abi (List.filteri (fun i _ -> i < c.nfixed) c.params) in
  let ps = if List.length c.params > c.nfixed then ps @ [ "..." ] else ps in
  Printf.sprintf "(%s) -> (%s)" (String.concat ", " ps) (match c.ret with None -> "" | Some t -> fir_abi t)

(* mix i64 [x] into the hash value [h]; returns the new one *)
let mix fb h x =
  let a = fresh fb and m = fresh fb in
  ln fb "%s = xor i64 %s, %s" a h x;
  ln fb "%s = mul i64 %s, i64 1099511628211" m a;
  m

(* mix scalar [v] of kind [s] (a Fir value; Bool as i1 or i8) into [h].
   [tmp] is a 16-byte slot for f80. *)
let hash_val fb tmp h s v =
  let ext k =
    let r = fresh fb in
    ln fb "%s = %s i64 %s" r k v;
    r
  in
  match s with
  | Int (64, _) -> mix fb h v
  | Bool | Int (_, false) -> mix fb h (ext "zext")
  | Int (_, true) -> mix fb h (ext "sext")
  | I128 _ ->
      let lo = fresh fb and s1 = fresh fb and hi = fresh fb in
      ln fb "%s = trunc i64 %s" lo v;
      ln fb "%s = lshr i128 %s, i128 64" s1 v;
      ln fb "%s = trunc i64 %s" hi s1;
      mix fb (mix fb h lo) hi
  | F16 | F32 ->
      let bi = fresh fb and z = fresh fb in
      ln fb "%s = bitcast %s %s" bi (if s = F16 then "i16" else "i32") v;
      ln fb "%s = zext i64 %s" z bi;
      mix fb h z
  | F64 ->
      let bi = fresh fb in
      ln fb "%s = bitcast i64 %s" bi v;
      mix fb h bi
  | F80 ->
      let lo = fresh fb and q = fresh fb and hi = fresh fb and z = fresh fb in
      ln fb "store %s, %s, align 16" v tmp;
      ln fb "%s = load i64 %s, align 16" lo tmp;
      ln fb "%s = ptradd ptr %s, i64 0, 1, 8" q tmp;
      ln fb "%s = load i16 %s, align 8" hi q;
      ln fb "%s = zext i64 %s" z hi;
      mix fb (mix fb h lo) z
  | Ptr ->
      let i = fresh fb in
      ln fb "%s = ptrtoint i64 %s" i v;
      mix fb h i

(* the leaves of [t] in memory at [p] *)
let hash_mem fb tmp h t p =
  List.fold_left
    (fun h (off, s, _) ->
      let q = fresh fb and v = fresh fb in
      ln fb "%s = ptradd ptr %s, i64 0, 1, %d" q p off;
      ln fb "%s = load %s %s, align 1" v (fir_sc s) q;
      hash_val fb tmp h s v)
    h (leaves ~all:false t "" 0)

(* hash a value of C type [t] held as its Fir form [v] (agg: a ptr) *)
let hash_any fb tmp h t v =
  match t with
  | Struct _ | Union _ | Cplx _ -> hash_mem fb tmp h t v
  | Vec (n, e) ->
      let h = ref h in
      for i = 0 to n - 1 do
        let x = fresh fb in
        ln fb "%s = extract %s %s, i32 %d" x (fir_sc e) v i;
        h := hash_val fb tmp !h e x
      done;
      !h
  | Sc s -> hash_val fb tmp h s v
  | Arr _ -> assert false

(* a leaf's value from i64 [x] (see c_make); [mem]: Bool as i8 *)
let fir_make fb ~mem s x =
  let r = fresh fb in
  (match s with
  | Bool ->
      let a = fresh fb in
      ln fb "%s = and i64 %s, i64 1" a x;
      ln fb "%s = trunc %s %s" r (if mem then "i8" else "i1") a
  | Int (64, _) -> ln fb "%s = or i64 %s, i64 0" r x
  | Int (w, _) -> ln fb "%s = trunc i%d %s" r w x
  | I128 _ ->
      let a = fresh fb and s1 = fresh fb and c = fresh fb and z = fresh fb in
      ln fb "%s = zext i128 %s" a x;
      ln fb "%s = shl i128 %s, i128 64" s1 a;
      ln fb "%s = xor i64 %s, i64 6148914691236517205" c x;
      ln fb "%s = zext i128 %s" z c;
      ln fb "%s = or i128 %s, %s" r s1 z
  | F16 ->
      let a = fresh fb in
      ln fb "%s = trunc i8 %s" a x;
      ln fb "%s = sitofp f16 %s" r a
  | F32 | F64 ->
      let a = fresh fb in
      ln fb "%s = trunc i32 %s" a x;
      ln fb "%s = sitofp %s %s" r (fir_sc s) a
  | F80 -> ln fb "%s = sitofp f80 %s" r x
  | Ptr -> ln fb "%s = inttoptr ptr %s" r x);
  r

(* a value of C type [t] whose leaves come from the i64 values [xs]:
   a vector by inserts, an agg in the zeroed slot [slot] (returned) *)
let build fb t slot xs =
  match t with
  | Sc s -> fir_make fb ~mem:false s (List.hd xs)
  | Vec (n, e) ->
      let v = ref (Printf.sprintf "%s zero" (fir_vec n e)) in
      List.iteri
        (fun i x ->
          let l = fir_make fb ~mem:false e x in
          let r = fresh fb in
          ln fb "%s = insert %s %s, %s, i32 %d" r (fir_vec n e) !v l i;
          v := r)
        xs;
      ignore n;
      !v
  | Struct _ | Union _ | Cplx _ ->
      let s, _ = size_align t in
      ln fb "memset %s, i8 0, i64 %d" slot s;
      List.iter2
        (fun (off, sc, _) x ->
          let l = fir_make fb ~mem:true sc x in
          let q = fresh fb in
          ln fb "%s = ptradd ptr %s, i64 0, 1, %d" q slot off;
          ln fb "store %s, %s, align 1" l q)
        (leaves ~all:false t "" 0) xs;
      slot
  | Arr _ -> assert false

(* a slot per agg, in the entry block (all functions here are one block) *)
let slot fb t =
  let s, a = size_align t in
  let p = fresh fb in
  ln fb "%s = alloca ptr i64 %d, align %d" p (max s 1) (max a 16);
  p

let fir_file (cases : case list) =
  let fb = { b = Buffer.create 8192; n = 0 } in
  Buffer.add_string fb.b "triple \"x86_64-unknown-linux-gnu\"\nglobal @abi_sink size 8 align 8 global = extern\n\n";
  List.iteri
    (fun k (c : case) ->
      Printf.bprintf fb.b "declare @c_callee_%d %s global\n" k (fir_sig c);
      (* the callee: hash every argument, build the result *)
      let fixed = List.filteri (fun i _ -> i < c.nfixed) c.params in
      Printf.bprintf fb.b "func @fir_callee_%d %s global {\n^e(%s):\n" k (fir_sig c)
        (String.concat ", " (List.mapi (fun i t -> Printf.sprintf "%%a%d: %s" i (fir_param_ty t)) fixed));
      let tmp = fresh fb in
      ln fb "%s = alloca ptr i64 16, align 16" tmp;
      let ap = fresh fb in
      ln fb "%s = alloca ptr i64 24, align 8" ap;
      let rslot = match c.ret with Some t when is_agg t -> Some (slot fb t) | _ -> None in
      if List.length c.params > c.nfixed then ln fb "va_start %s" ap;
      let h =
        List.fold_left
          (fun h (i, t) ->
            let v =
              if i < c.nfixed then Printf.sprintf "%%a%d" i
              else begin
                let v = fresh fb in
                (match t with
                | Struct _ | Union _ | Cplx _ -> ln fb "%s = va_arg %s %s" v (fir_abi t) ap
                | _ -> ln fb "%s = va_arg %s %s" v (fir_param_ty t) ap);
                v
              end
            in
            hash_any fb tmp h t v)
          "i64 -3750763034362895579" (List.mapi (fun i t -> (i, t)) c.params)
      in
      (match c.ret with
      | None ->
          ln fb "store %s, ptr @abi_sink, align 8" h;
          ln fb "ret"
      | Some t ->
          let xs =
            List.mapi
              (fun j _ ->
                let m = fresh fb and a = fresh fb in
                ln fb "%s = mul i64 %s, i64 %Ld" m h (leaf_mult j);
                ln fb "%s = add i64 %s, i64 %Ld" a m (leaf_add j);
                a)
              (leaves ~all:false t "" 0)
          in
          let r = build fb t (match rslot with Some p -> p | None -> "") xs in
          ln fb "ret %s" r);
      Buffer.add_string fb.b "}\n\n";
      (* the Fir callers *)
      List.iter
        (fun (name, callee) ->
          Printf.bprintf fb.b "func @%s_%d () -> (i64) global {\n^e:\n" name k;
          let tmp = fresh fb in
          ln fb "%s = alloca ptr i64 16, align 16" tmp;
          let slots = List.map (fun t -> if is_agg t then slot fb t else "") c.params in
          let args =
            List.map2
              (fun (i, t) (sl, nums) ->
                let v = build fb t sl (List.map (fun x -> Printf.sprintf "i64 %Ld" x) nums) in
                if i >= c.nfixed && is_agg t then fir_abi t ^ " " ^ v
                else
                  match (t, v) with
                  (* a constant operand names its type *)
                  | Vec _, _ when String.ends_with ~suffix:" zero" v -> v
                  | _ -> v)
              (List.mapi (fun i t -> (i, t)) c.params)
              (List.combine slots c.args)
          in
          let call = Printf.sprintf "call %s @%s_%d(%s)" (fir_sig c) callee k (String.concat ", " args) in
          (match c.ret with
          | None ->
              ln fb "store i64 0, ptr @abi_sink, align 8";
              ln fb "%s" call;
              let r = fresh fb in
              ln fb "%s = load i64 ptr @abi_sink, align 8" r;
              ln fb "ret %s" r
          | Some t ->
              let r = fresh fb in
              ln fb "%s = %s" r call;
              let h = hash_any fb tmp "i64 -3750763034362895579" t r in
              ln fb "ret %s" h);
          Buffer.add_string fb.b "}\n\n")
        [ ("f2c", "c_callee"); ("f2f", "fir_callee") ])
    cases;
  Buffer.contents fb.b

(* ---------- harness ---------- *)

let write path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let read path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let keep = ref (Filename.get_temp_dir_name ())
let cc = ref "gcc"
let llvm_cc = ref (Option.value (Sys.getenv_opt "FAS_CC") ~default:"clang-22")
let verbose = ref false
let ncases = ref 6
let do_reduce = ref false

type outcome = Same | Differ of string | Broken of string

let sh fmt = Printf.ksprintf (fun s -> Sys.command s) fmt
let q = Filename.quote

let check_cases tmp name cases =
  let ctext = c_file cases and ftext = fir_file cases in
  let save () =
    let base = Filename.concat !keep name in
    write (base ^ ".c") ctext;
    write (base ^ ".fir") ftext;
    base
  in
  let cfile = Filename.concat tmp "c.c" and co = Filename.concat tmp "c.o" in
  write cfile ctext;
  if sh "%s -O1 -w -c %s -o %s 2>%s" !cc (q cfile) (q co) (q (Filename.concat tmp "cc.err")) <> 0 then
    Broken ("C: " ^ read (Filename.concat tmp "cc.err") ^ " (" ^ save () ^ ")")
  else
    let parse () =
      let m = Fir_parse.parse_module ftext in
      match Fir_verify.verify m with [] -> m | errs -> failwith ("verifier: " ^ String.concat "; " errs)
    in
    match parse () with
    | exception Fir_parse.Error (l, msg) -> Broken (Printf.sprintf "parse line %d: %s (%s)" l msg (save ()))
    | exception Failure msg -> Broken (msg ^ " (" ^ save () ^ ")")
    | m -> (
        (* each side linked with the C object and run *)
        let run_with name obj =
          let exe = Filename.concat tmp name in
          if sh "%s %s %s -o %s -lm 2>%s" !cc (q co) (q obj) (q exe) (q (Filename.concat tmp "ld.err")) <> 0 then
            Error ("link: " ^ read (Filename.concat tmp "ld.err"))
          else
            let out = Filename.concat tmp (name ^ ".out") in
            let st = sh "timeout 10 %s > %s 2>&1" (q exe) (q out) in
            if st = 0 then Ok () else Error (Printf.sprintf "status %d: %s" st (String.trim (read out)))
        in
        let llvm =
          match Fir_llvm.render_bounded ~budget:max_int m with
          | Error msg -> Error ("fir -> llvm: " ^ msg)
          | Ok ll ->
              let llf = Filename.concat tmp "m.ll" and lo = Filename.concat tmp "l.o" in
              write llf ll;
              if sh "%s -O1 -w -c -x ir %s -o %s 2>%s" !llvm_cc (q llf) (q lo) (q (Filename.concat tmp "ll.err")) <> 0 then
                Error ("clang: " ^ read (Filename.concat tmp "ll.err"))
              else run_with "l" lo
        in
        let native =
          match
            let m = parse () in
            Fir_wide.run m;
            Mir_emit.compile_object m
          with
          | exception e -> Error ("native compile: " ^ Printexc.to_string e)
          | Error msg -> Error ("native compile: " ^ msg)
          | Ok obj ->
              let no = Filename.concat tmp "n.o" in
              write no (Bytes.to_string obj);
              run_with "n" no
        in
        match (native, llvm) with
        | Ok (), Ok () -> Same
        | Error n, Ok () -> Differ ("native only: " ^ n ^ " (" ^ save () ^ ")")
        | Ok (), Error l -> Differ ("LLVM only: " ^ l ^ " (" ^ save () ^ ")")
        | Error n, Error l -> Differ ("both (Fir_abi or generator): native " ^ n ^ "; llvm " ^ l ^ " (" ^ save () ^ ")"))

(* A smaller failing input: the first failing case alone, then without
   each parameter and the result while it still fails the same way (the
   same side). Returns the reduced cases. *)
let kind = function Same -> "same" | Broken _ -> "broken" | Differ m -> String.sub m 0 (min 10 (String.length m))

let reduce tmp seed cases first =
  let fails cs = kind (check_cases tmp "reduce" cs) = kind first in
  let cur = ref cases in
  let i = ref (List.length cases - 1) in
  while !i >= 0 do
    let cs = List.filteri (fun j _ -> j <> !i) !cur in
    if cs <> [] && fails cs then cur := cs;
    decr i
  done;
  (match !cur with
  | [ c ] ->
      let c = ref c in
      let progress = ref true in
      while !progress do
        progress := false;
        (if !c.ret <> None then
           let c' = { !c with ret = None } in
           if fails [ c' ] then (c := c'; progress := true));
        let n = List.length !c.params in
        let i = ref (n - 1) in
        while !i >= 0 do
          let drop l = List.filteri (fun j _ -> j <> !i) l in
          let nfixed = if !i < !c.nfixed then !c.nfixed - 1 else !c.nfixed in
          (* a variadic tail needs a fixed parameter before it *)
          if nfixed > 0 || nfixed = List.length !c.params - 1 then begin
            let c' = { params = drop !c.params; nfixed; ret = !c.ret; args = drop !c.args } in
            if fails [ c' ] then (c := c'; progress := true)
          end;
          decr i
        done
      done;
      cur := [ !c ]
  | _ -> ());
  (* the reduced pair is kept like any failing one *)
  let ctext = c_file !cur and ftext = fir_file !cur in
  let base = Filename.concat !keep (Printf.sprintf "abi_fuzz_%d_min" seed) in
  write (base ^ ".c") ctext;
  write (base ^ ".fir") ftext

let check_seed tmp seed =
  rs := Random.State.make [| seed |];
  next_id := 0;
  let cases = List.init (1 + int !ncases) (fun _ -> gen_case ()) in
  let r = check_cases tmp (Printf.sprintf "abi_fuzz_%d" seed) cases in
  (match r with Same -> () | _ -> if !do_reduce then reduce tmp seed cases r);
  r

let () =
  let count = ref 100 and first = ref 1 in
  Arg.parse
    [
      ("-n", Arg.Set_int count, "COUNT number of seeds (default 100)");
      ("-seed", Arg.Set_int first, "FIRST first seed (default 1)");
      ("-keep", Arg.Set_string keep, "DIR where failing cases are written");
      ("-cc", Arg.Set_string cc, "CC the C compiler for the C side (default gcc)");
      ("-llvm-cc", Arg.Set_string llvm_cc, "CLANG clang for the LLVM build of the Fir side");
      ("-cases", Arg.Set_int ncases, "N up to N+1 cases per seed (default 6)");
      ("-v", Arg.Set verbose, " print every seed");
      ("-reduce", Arg.Set do_reduce, " also write a reduced abi_fuzz_SEED_min pair for each failure");
    ]
    (fun _ -> ())
    "abi_fuzz [-n COUNT] [-seed FIRST] [-keep DIR]";
  let tmp = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "abi_fuzz.%d" (Unix.getpid ())) in
  Sys.mkdir tmp 0o700;
  let same = ref 0 and differ = ref 0 and broken = ref 0 in
  for seed = !first to !first + !count - 1 do
    match check_seed tmp seed with
    | Same ->
        incr same;
        if !verbose then Printf.printf "seed %d: same\n%!" seed
    | Differ msg ->
        incr differ;
        Printf.printf "seed %d: DIFFER %s\n%!" seed msg
    | Broken msg ->
        incr broken;
        Printf.printf "seed %d: generator problem: %s\n%!" seed msg
  done;
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (q tmp)));
  Printf.printf "abi fuzz: %d same, %d differ, %d broken\n" !same !differ !broken;
  exit (if !differ + !broken = 0 then 0 else 1)
