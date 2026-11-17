(* Differential IR fuzzer.

   Each seed generates a random Fir module as text. The module is parsed,
   verified, then compiled twice:
   - by the native backend (Fir_wide, then Mir_emit.compile: Mir_isel,
     Mir_ra, Mir_check; -mir is accepted for old command lines);
   - by LLVM: Fir_llvm, then clang -O2.
   Both programs run and must print the same bytes and exit with the same
   status.

   The generated programs are free of undefined behaviour by construction:
   - shift counts are masked below the width;
   - divisors are forced odd and positive;
   - float-to-int conversions saturate;
   - memory comes from one zeroed stack buffer, indexed with a mask;
   - loops have constant trip counts;
   - calls only go to earlier functions, so there is no recursion.
   So any difference is a bug in Fir_llvm, in the native backend, or
   (rarely) in this generator.

   Usage: fir_fuzz [-n COUNT] [-seed FIRST] [-keep DIR] [-cc CLANG] [-v] [-file M.fir]
   A failing seed's .fir file is written to DIR (default: the current
   directory). Rerun just that seed with `-seed S -n 1`. *)

let count = ref 200
let first = ref 1
let keep = ref "."
let cc = ref (Option.value (Sys.getenv_opt "CC") ~default:"clang-22")
let verbose = ref false

(* ---------- generator ---------- *)

(* The types: integers, the four float widths, and vectors of integers or
   f32/f64 (no f16/f80 lanes, no bool vectors except as conditions and
   masks) *)
type ty = I of int | F16 | F32 | F64 | F80 | V of int * ty

let rec ty_text = function
  | I w -> "i" ^ string_of_int w
  | F16 -> "f16"
  | F32 -> "f32"
  | F64 -> "f64"
  | F80 -> "f80"
  | V (n, e) -> Printf.sprintf "<%d x %s>" n (ty_text e)

(* the bytes a store writes *)
let rec store_size = function
  | I w -> max 1 (w / 8)
  | F16 -> 2
  | F32 -> 4
  | F64 -> 8
  | F80 -> 10
  | V (n, e) -> n * store_size e

(* a buffer slot's size and alignment *)
let slot_size = function F80 -> 16 | t -> store_size t
let align_of = function F80 -> 16 | V (_, e) -> store_size e | t -> store_size t
let is_float = function F16 | F32 | F64 | F80 -> true | _ -> false
let width = function I w -> w | F16 -> 16 | F32 -> 32 | F64 -> 64 | F80 -> 80 | V (n, e) -> n * store_size e * 8

type fn_sig = { fname : string; params : (ty * string) list; ret : ty }
(* the string is the ABI prefix: "", "sext " or "zext " *)

type g = {
  rs : Random.State.t;
  buf : Buffer.t;
  mutable next : int;  (* value and block counter *)
  mutable env : (string * ty) list;  (* values that dominate the insertion point *)
  mem : string;  (* the function's zeroed 64-byte buffer *)
  callees : fn_sig list;  (* functions this one may call *)
}

let int g n = Random.State.int g.rs n
let chance g p = Random.State.float g.rs 1.0 < p
let choose g l = List.nth l (int g (List.length l))
let line g fmt = Printf.ksprintf (fun s -> Buffer.add_string g.buf ("  " ^ s ^ "\n")) fmt

let fresh g =
  g.next <- g.next + 1;
  Printf.sprintf "%%v%d" g.next

let label g =
  g.next <- g.next + 1;
  Printf.sprintf "^b%d" g.next

(* i128 has no register in the native backend: Fir_wide splits it into
   two i64 limbs there, while LLVM (the oracle) keeps it whole *)
let int_tys = [ I 8; I 16; I 32; I 64; I 128 ]
let float_tys = [ F16; F32; F64; F80 ]

(* up to 32 bytes; odd lane counts exercise the legalizer's padding *)
let vec_tys =
  [ V (16, I 8); V (8, I 16); V (4, I 32); V (2, I 64); V (3, I 32); V (8, I 32); V (4, I 64); V (5, I 16);
    V (4, F32); V (2, F64); V (8, F32); V (4, F64); V (3, F32) ]

(* a random type: integers most often *)
let any_ty g =
  match int g 8 with 0 | 1 | 2 | 3 -> choose g int_tys | 4 | 5 -> choose g float_tys | _ -> choose g vec_tys

let f16_pool = [ 0x0000; 0x8000; 0x3c00; 0xbe00; 0x3400; 0x4a80; 0x7bff; 0x0001; 0x5640; 0x3800; 0xfbff ]

let f80_pool =
  [ "0x0"; "0x80000000000000000000"; "0x3fff8000000000000000"; "0xbfffc000000000000000"; "0x3ffe8000000000000000";
    "0x4000d000000000000000"; "0x4000c90fdaa22168c235"; "0x7ffeffffffffffffffff"; "0x1"; "0x40209502f90000000000";
    "0xbffb8f5c28f5c28f5c29" ]

(* An interesting constant of type [t], as Fir text with its type. *)
let rec const g t =
  match t with
  | I 1 -> if chance g 0.5 then "i1 1" else "i1 0"
  | I w ->
      let pool = [ 0L; 1L; 2L; 3L; 7L; -1L; -2L; 255L; 256L; 65535L; 0x7fffffffL; 0x80000000L; Int64.max_int; Int64.min_int; 12345L; -77L ] in
      let x = if chance g 0.6 then choose g pool else Random.State.bits64 g.rs in
      if w = 128 && chance g 0.4 then
        (* both limbs random, or a limb boundary value *)
        let hi = if chance g 0.5 then Random.State.bits64 g.rs else choose g pool in
        Printf.sprintf "i128 0x%016Lx%016Lx" hi x
      else
      let x = if w >= 64 then x else Int64.shift_right (Int64.shift_left x (64 - w)) (64 - w) in
      Printf.sprintf "i%d %Ld" w x
  | F64 ->
      let x = choose g [ 0.0; -0.0; 1.0; -1.5; 0.1; 3.25; 1e10; -7e-5; 123456.789; 0.5 ] in
      Printf.sprintf "f64 0x%Lx" (Int64.bits_of_float x)
  | F32 ->
      let x = choose g [ 0.0; -0.0; 1.0; -1.5; 0.25; 3.25; 1e6; -7e-3; 1234.5; 0.5 ] in
      Printf.sprintf "f32 0x%lx" (Int32.bits_of_float x)
  | F16 -> Printf.sprintf "f16 0x%04x" (choose g f16_pool)
  | F80 -> "f80 " ^ choose g f80_pool
  | V (n, e) ->
      if chance g 0.2 then ty_text t ^ " zero"
      else Printf.sprintf "%s [%s]" (ty_text t) (String.concat ", " (List.init n (fun _ -> const g e)))

(* the vector constant with every lane [k] *)
let vsplat n w k = Printf.sprintf "<%d x i%d> [%s]" n w (String.concat ", " (List.init n (fun _ -> Printf.sprintf "i%d %d" w k)))

let qnan = function
  | F16 -> "f16 0x7e00"
  | F32 -> "f32 0x7fc00000"
  | F64 -> "f64 0x7ff8000000000000"
  | F80 -> "f80 0x7fffc000000000000000"
  | _ -> assert false

(* Fir text for an operand of type [t]: usually a value in scope *)
let pick g t =
  let have = List.filter (fun (_, t') -> t' = t) g.env in
  if have <> [] && chance g 0.85 then fst (choose g have) else const g t

let def g t name = g.env <- (name, t) :: g.env

(* values in scope whose type satisfies [p] *)
let having g p = List.filter (fun (_, t) -> p t) g.env

(* [v] with any NaN replaced by the default quiet NaN, lane-wise: a NaN
   computed by an operation has an unspecified sign and payload (SPEC §6),
   so its bits are never observed *)
let canon g t v =
  match t with
  | F80 ->
      (* an f80 loaded from bits that are not a canonical encoding (a
         pseudo-denormal: exponent 0 with the integer bit set) holds its
         value in an unspecified encoding (SPEC §6): x87 keeps the bits,
         LLVM's constant folding rewrites them. A multiply by 1 encodes it
         canonically on both and keeps -0 *)
      let m = fresh g and u = fresh g and c = fresh g in
      line g "%s = fmul f80 %s, f80 0x3fff8000000000000000" m v;
      line g "%s = fcmp une i1 %s, %s" u m m;
      line g "%s = select f80 %s, %s, %s" c u (qnan F80) m;
      c
  | F16 | F32 | F64 ->
      let u = fresh g and c = fresh g in
      line g "%s = fcmp une i1 %s, %s" u v v;
      line g "%s = select %s %s, %s, %s" c (ty_text t) u (qnan t) v;
      c
  | V (n, e) when is_float e ->
      let u = fresh g and c = fresh g in
      line g "%s = fcmp une <%d x i1> %s, %s" u n v v;
      line g "%s = select %s %s, %s [%s], %s" c (ty_text t) u (ty_text t) (String.concat ", " (List.init n (fun _ -> qnan e))) v;
      c
  | _ -> v

(* An integer value of type [I w], converting from another width if useful *)
let rec int_of g w =
  let srcs = List.filter (function _, I w' -> w' <> w && w' > 1 | _ -> false) g.env in
  if srcs <> [] && chance g 0.25 then begin
    let s, st = choose g srcs in
    let sw = match st with I x -> x | _ -> assert false in
    let r = fresh g in
    let k = if sw > w then "trunc" else if chance g 0.5 then "zext" else "sext" in
    line g "%s = %s i%d %s" r k w s;
    def g (I w) r;
    r
  end
  else pick g (I w)

(* A bool vector of [n] lanes: a lane-wise compare of two vectors with [n]
   lanes in scope (or of constants) *)
and vcond g n =
  let srcs = having g (function V (m, _) -> m = n | _ -> false) in
  let c = fresh g in
  (match if srcs <> [] && chance g 0.8 then Some (choose g srcs) else None with
  | Some (a, (V (_, e) as t)) when is_float e ->
      line g "%s = fcmp %s <%d x i1> %s, %s" c
        (choose g [ "oeq"; "ogt"; "oge"; "olt"; "ole"; "one"; "ord"; "ueq"; "ugt"; "uge"; "ult"; "ule"; "une"; "uno" ])
        n a (pick g t)
  | Some (a, t) ->
      line g "%s = icmp %s <%d x i1> %s, %s" c
        (choose g [ "eq"; "ne"; "slt"; "sle"; "sgt"; "sge"; "ult"; "ule"; "ugt"; "uge" ])
        n a (pick g t)
  | None ->
      let t = V (n, I 32) in
      line g "%s = icmp %s <%d x i1> %s, %s" c (choose g [ "eq"; "slt"; "ugt" ]) n (const g t) (const g t));
  c

and gen_int_op g w =
  let t = Printf.sprintf "i%d" w in
  let a = int_of g w and b = pick g (I w) in
  let r = fresh g in
  let fallback () = line g "%s = %s %s %s, %s" r (choose g [ "add"; "xor" ]) t a b in
  (match int g 16 with
  | 9 -> line g "%s = %s %s %s, %s" r (choose g [ "uadd_sat"; "sadd_sat"; "usub_sat"; "ssub_sat" ]) t a b
  | 10 ->
      (* funnel shifts take the count modulo the width *)
      line g "%s = %s %s %s, %s, %s" r (choose g [ "fshl"; "fshr" ]) t a b (pick g (I w))
  | 0 | 1 ->
      (* no i128 mul-high: the oracle's Ir has no i256 to compute it in
         (test/fas_fir/c/wide covers it against clang instead) *)
      let ops = [ "add"; "sub"; "mul" ] @ if w = 128 then [] else [ "umulh"; "smulh" ] in
      line g "%s = %s %s %s, %s" r (choose g ops) t a b
  | 2 -> line g "%s = %s %s %s, %s" r (choose g [ "and"; "or"; "xor" ]) t a b
  | 3 ->
      let s = fresh g in
      line g "%s = and %s %s, %s %d" s t b t (w - 1);
      line g "%s = %s %s %s, %s" r (choose g [ "shl"; "lshr"; "ashr" ]) t a s
  | 4 ->
      let d0 = fresh g and d = fresh g in
      (* divisor in [1, 63]: never 0 and never -1 (sdiv MIN, -1 is UB).
         63 fits every width; don't compute 2^(w-1)-1, which overflows
         OCaml's 63-bit int at w = 64. *)
      line g "%s = and %s %s, %s 63" d0 t b t;
      line g "%s = or %s %s, %s 1" d t d0 t;
      line g "%s = %s %s %s, %s" r (choose g [ "udiv"; "urem"; "sdiv"; "srem" ]) t a d
  | 5 -> line g "%s = %s %s %s, %s" r (choose g [ "smin"; "smax"; "umin"; "umax" ]) t a b
  | 6 ->
      let ops = [ "popcnt"; "ctz"; "clz"; "bitrev"; "abs"; "freeze" ] @ if w mod 16 = 0 then [ "bswap" ] else [] in
      line g "%s = %s %s %s" r (choose g ops) t a
  | 7 ->
      let c = gen_cond g in
      line g "%s = select %s %s, %s, %s" r t c a b
  | 8 ->
      (* from a float, saturating *)
      let ft = choose g float_tys in
      line g "%s = %s %s %s" r (choose g [ "fptosi_sat"; "fptoui_sat" ]) t (pick g ft)
  | 11 ->
      (* the wrapped value or, on overflow, b *)
      let ops = [ "sadd_ovf"; "uadd_ovf"; "ssub_ovf"; "usub_ovf" ] @ if w = 128 then [] else [ "smul_ovf"; "umul_ovf" ] in
      let v = fresh g and o = fresh g in
      line g "%s, %s = %s (%s, i1) %s, %s" v o (choose g ops) t a b;
      line g "%s = select %s %s, %s, %s" r t o b v
  | 12 -> (
      (* a lane or a reduction of an integer vector *)
      match having g (function V (_, I w') -> w' = w | _ -> false) with
      | [] -> fallback ()
      | srcs -> (
          let v, vt = choose g srcs in
          let n = match vt with V (n, _) -> n | _ -> assert false in
          if chance g 0.5 then line g "%s = extract %s %s, i32 %d" r t v (int g n)
          else
            line g "%s = reduce %s %s %s" r
              (choose g [ "add"; "mul"; "and"; "or"; "xor"; "smin"; "smax"; "umin"; "umax" ])
              t v))
  | 13 -> (
      (* the bits of a float or a vector of the same width *)
      match having g (fun t' -> t' <> I w && t' <> F80 && width t' = w && not (match t' with I _ -> true | _ -> false)) with
      | [] -> fallback ()
      | srcs ->
          let v, vt = choose g srcs in
          line g "%s = bitcast %s %s" r t (canon g vt v))
  | 14 -> line g "%s = %s" r (gen_ct g w)
  | _ -> gen_asm g w r a b);
  def g (I w) r

(* A constant-time region: a secret copy of [a], a few allowed operations
   on it (SPEC §8), and the declassified result (as `freeze`'s operand,
   for the caller's "%r = ..." line). Tainted values never enter the
   environment, so nothing else branches on them, divides by them or
   indexes with them. *)
and gen_ct g w =
  let t = Printf.sprintf "i%d" w in
  let s = fresh g in
  line g "%s = secret %s %s" s t (int_of g w);
  let tainted = ref [ s ] in
  for _ = 0 to int g 6 do
    let a = choose g !tainted in
    let b = if chance g 0.5 then choose g !tainted else pick g (I w) in
    let r = fresh g in
    (match int g 9 with
    | 0 | 1 -> line g "%s = %s %s %s, %s" r (choose g [ "add"; "sub"; "mul"; "and"; "or"; "xor" ]) t a b
    | 2 ->
        let c = fresh g in
        line g "%s = and %s %s, %s %d" c t b t (w - 1);
        line g "%s = %s %s %s, %s" r (choose g [ "shl"; "lshr"; "ashr" ]) t a c
    | 3 -> line g "%s = %s %s %s, %s, %s" r (choose g [ "fshl"; "fshr" ]) t a b (choose g !tainted)
    | 4 -> line g "%s = %s %s %s, %s" r (choose g [ "smin"; "smax"; "umin"; "umax"; "uadd_sat"; "ssub_sat" ]) t a b
    | 5 ->
        (* a select on a secret condition: a cmov *)
        let c = fresh g in
        line g "%s = icmp %s i1 %s, %s" c (choose g [ "eq"; "ult"; "slt"; "uge" ]) a b;
        line g "%s = select %s %s, %s, %s" r t c a b
    | 6 -> line g "%s = %s %s %s" r (choose g ("abs" :: (if w mod 16 = 0 then [ "bswap" ] else []))) t a
    | 7 when w < 128 -> line g "%s = %s %s %s, %s" r (choose g [ "umulh"; "smulh" ]) t a b
    | _ ->
        (* through another width and back *)
        let w2 = choose g (List.filter (fun x -> x <> w) [ 8; 16; 32; 64; 128 ]) in
        let m = fresh g in
        line g "%s = %s i%d %s" m (if w2 < w then "trunc" else choose g [ "zext"; "sext" ]) w2 a;
        line g "%s = %s %s %s" r (if w2 < w then choose g [ "zext"; "sext" ] else "trunc") t m);
    tainted := r :: !tainted
  done;
  let d = fresh g in
  line g "%s = declassify %s %s" d t (List.hd !tainted);
  Printf.sprintf "freeze %s %s" t d

(* GNU inline asm on 64-bit values (AT&T syntax, as LLVM reads it); other
   widths take an ordinary operation *)
and gen_asm g w r a b =
  if w <> 64 then line g "%s = sub i%d %s, %s" r w a b
  else
    match int g 5 with
    | 0 -> line g "%s = asm \"%s %%1, %%0\" out(\"+r\" i64 %s) in(\"r\" i64 %s) clobber(\"cc\")" r (choose g [ "addq"; "subq"; "xorq"; "andq"; "orq"; "imulq" ]) a b
    | 1 -> line g "%s = asm \"rolq $%d, %%0\" out(\"+r\" i64 %s) clobber(\"cc\")" r (1 + int g 63) a
    | 2 -> line g "%s = asm \"bswapq %%0\" out(\"+r\" i64 %s)" r a
    | 3 -> line g "%s = asm \"leaq %d(%%1,%%2,%d), %%0\" out(\"=r\" i64) in(\"r\" i64 %s, \"r\" i64 %s)" r (int g 100) (choose g [ 1; 2; 4; 8 ]) a b
    | _ ->
        (* a value through memory: an "m" operand *)
        let p = fresh g in
        line g "%s = ptradd ptr %s, i64 %d, 8, 0" p g.mem (int g 8);
        line g "store %s, %s, align 8" a p;
        line g "%s = asm \"movq %%1, %%0\\0a\\09addq $1, %%0\" out(\"=&r\" i64) in(\"m\" ptr %s) clobber(\"cc\")" r p

(* An i1 condition *)
and gen_cond g =
  let c = fresh g in
  (if chance g 0.75 then begin
     let t = choose g int_tys in
     let w = match t with I w -> w | _ -> 64 in
     line g "%s = icmp %s i1 %s, %s" c
       (choose g [ "eq"; "ne"; "slt"; "sle"; "sgt"; "sge"; "ult"; "ule"; "ugt"; "uge" ])
       (int_of g w) (pick g t)
   end
   else begin
     let t = choose g float_tys in
     line g "%s = fcmp %s i1 %s, %s" c
       (choose g
          [ "oeq"; "ogt"; "oge"; "olt"; "ole"; "one"; "ord"; "ueq"; "ugt"; "uge"; "ult"; "ule"; "une"; "uno"; "true"; "false" ])
       (pick g t) (pick g t)
   end);
  c

let gen_float_op g t =
  let tt = ty_text t in
  let r = fresh g in
  let bin () = line g "%s = %s %s %s, %s" r (choose g [ "fadd"; "fsub"; "fmul"; "fdiv"; "frem"; "fmin"; "fmax" ]) tt (pick g t) (pick g t) in
  (match int g 9 with
  | 0 when t <> F16 && int g 3 = 0 ->
      (* not f16: LLVM computes it in f32 and rounds twice *)
      line g "%s = fma %s %s, %s, %s" r tt (pick g t) (pick g t) (pick g t)
  | 0 | 1 -> bin ()
  | 2 ->
      (* the sign of a computed NaN is unspecified: never copy it *)
      let b = canon g t (pick g t) in
      line g "%s = copysign %s %s, %s" r tt (pick g t) b
  | 3 -> line g "%s = %s %s %s" r (choose g [ "fneg"; "fabs"; "sqrt"; "floor"; "ceil"; "ftrunc"; "roundeven" ]) tt (pick g t)
  | 4 ->
      (* f16 only from 16 bits or fewer: LLVM converts wider integers
         through f32 and rounds twice *)
      let w = choose g (if t = F16 then [ 8; 16 ] else [ 8; 16; 32; 64; 128 ]) in
      line g "%s = %s %s %s" r (choose g [ "sitofp"; "uitofp" ]) tt (int_of g w)
  | 5 ->
      let other = choose g (List.filter (fun x -> x <> t) float_tys) in
      line g "%s = %s %s %s" r (if width other > width t then "fptrunc" else "fpext") tt (pick g other)
  | 6 -> (
      match having g (function V (_, e) -> e = t | _ -> false) with
      | [] -> bin ()
      | srcs ->
          let v, vt = choose g srcs in
          let n = match vt with V (n, _) -> n | _ -> assert false in
          if chance g 0.6 then line g "%s = extract %s %s, i32 %d" r tt v (int g n)
          else line g "%s = reduce %s %s %s" r (choose g [ "fmin"; "fmax"; "fadd_ordered" ]) tt v)
  | 7 when t <> F80 ->
      (* from integer bits: any pattern, NaNs included *)
      line g "%s = bitcast %s %s" r tt (int_of g (width t))
  | _ ->
      let c = gen_cond g in
      line g "%s = select %s %s, %s, %s" r tt c (pick g t) (pick g t));
  def g t r

(* vectors of integers *)
let gen_vint_op g n w =
  let t = V (n, I w) in
  let tt = ty_text t in
  let a = pick g t and b = pick g t in
  let r = fresh g in
  let bin ops = line g "%s = %s %s %s, %s" r (choose g ops) tt a b in
  (match int g 16 with
  | 0 -> bin [ "add"; "sub"; "mul" ]
  | 1 -> bin [ "and"; "or"; "xor" ]
  | 2 ->
      let s = fresh g in
      line g "%s = and %s %s, %s" s tt b (vsplat n w (w - 1));
      line g "%s = %s %s %s, %s" r (choose g [ "shl"; "lshr"; "ashr" ]) tt a s
  | 3 ->
      let d0 = fresh g and d = fresh g in
      line g "%s = and %s %s, %s" d0 tt b (vsplat n w 63);
      line g "%s = or %s %s, %s" d tt d0 (vsplat n w 1);
      line g "%s = %s %s %s, %s" r (choose g [ "udiv"; "urem"; "sdiv"; "srem" ]) tt a d
  | 4 -> bin [ "smin"; "smax"; "umin"; "umax"; "umulh"; "smulh" ]
  | 5 -> bin [ "uadd_sat"; "sadd_sat"; "usub_sat"; "ssub_sat" ]
  | 6 ->
      let ops = [ "popcnt"; "ctz"; "clz"; "bitrev"; "abs" ] @ if w >= 16 then [ "bswap" ] else [] in
      line g "%s = %s %s %s" r (choose g ops) tt a
  | 7 -> line g "%s = select %s %s, %s, %s" r tt (vcond g n) a b
  | 8 -> line g "%s = splat %s %s" r tt (int_of g w)
  | 9 -> line g "%s = insert %s %s, %s, i32 %d" r tt a (int_of g w) (int g n)
  | 10 -> (
      (* lanes of two vectors of any lane count with the same elements *)
      match having g (function V (_, I w') -> w' = w | _ -> false) with
      | [] -> bin [ "add" ]
      | srcs ->
          let x, xt = choose g srcs in
          let m = match xt with V (m, _) -> m | _ -> assert false in
          line g "%s = shuffle %s %s, %s, [%s]" r tt x (pick g xt)
            (String.concat ", " (List.init n (fun _ -> string_of_int (int g (2 * m))))))
  | 11 -> line g "%s = %s %s %s, %s" r (choose g [ "compress"; "expand" ]) tt a (vcond g n)
  | 12 -> (
      (* indices of any width; out of range gives a zero lane *)
      let src = having g (function V (_, I w') -> w' = w | _ -> false) in
      let x, xt = if src <> [] && chance g 0.7 then choose g src else (a, t) in
      let m = match xt with V (m, _) -> m | _ -> assert false in
      let iw = choose g [ 8; 16; 32; 64 ] in
      match having g (fun t' -> t' = V (n, I iw)) with
      | (ix, _) :: _ when chance g 0.5 -> line g "%s = permute %s %s, %s" r tt x ix
      | _ ->
          line g "%s = permute %s %s, <%d x i%d> [%s]" r tt x n iw
            (String.concat ", " (List.init n (fun _ -> Printf.sprintf "i%d %d" iw (int g (m + 2))))))
  | 13 -> (
      match having g (function V (m, I w') -> m = n && w' <> w | _ -> false) with
      | [] -> bin [ "xor" ]
      | srcs ->
          let x, xt = choose g srcs in
          let w' = match xt with V (_, I w') -> w' | _ -> assert false in
          line g "%s = %s %s %s" r (if w' > w then "trunc" else choose g [ "zext"; "sext" ]) tt x)
  | 14 -> (
      match having g (fun t' -> t' <> t && t' <> F80 && width t' = width t) with
      | [] -> bin [ "sub" ]
      | srcs ->
          let x, xt = choose g srcs in
          line g "%s = bitcast %s %s" r tt (canon g xt x))
  | _ -> bin [ "add"; "mul" ]);
  def g t r

(* vectors of f32 or f64 *)
let gen_vfloat_op g n e =
  let t = V (n, e) in
  let tt = ty_text t in
  let r = fresh g in
  (match int g 11 with
  | 0 | 1 -> line g "%s = %s %s %s, %s" r (choose g [ "fadd"; "fsub"; "fmul"; "fdiv"; "frem"; "fmin"; "fmax" ]) tt (pick g t) (pick g t)
  | 2 -> line g "%s = %s %s %s" r (choose g [ "fneg"; "fabs"; "sqrt"; "floor"; "ceil"; "ftrunc"; "roundeven" ]) tt (pick g t)
  | 3 -> line g "%s = fma %s %s, %s, %s" r tt (pick g t) (pick g t) (pick g t)
  | 4 -> line g "%s = select %s %s, %s, %s" r tt (vcond g n) (pick g t) (pick g t)
  | 5 -> line g "%s = splat %s %s" r tt (pick g e)
  | 6 -> line g "%s = insert %s %s, %s, i32 %d" r tt (pick g t) (pick g e) (int g n)
  | 7 -> (
      match having g (function V (m, I _) -> m = n | _ -> false) with
      | [] -> line g "%s = fadd %s %s, %s" r tt (pick g t) (pick g t)
      | srcs ->
          let x, _ = choose g srcs in
          line g "%s = %s %s %s" r (choose g [ "sitofp"; "uitofp" ]) tt x)
  | 8 -> (
      match having g (function V (m, e') -> m = n && e' <> e && is_float e' | _ -> false) with
      | [] -> line g "%s = fsub %s %s, %s" r tt (pick g t) (pick g t)
      | srcs ->
          let x, _ = choose g srcs in
          line g "%s = %s %s %s" r (if e = F32 then "fptrunc" else "fpext") tt x)
  | 9 -> line g "%s = %s %s %s, %s" r (choose g [ "compress"; "expand" ]) tt (pick g t) (vcond g n)
  | _ ->
      line g "%s = shuffle %s %s, %s, [%s]" r tt (pick g t) (pick g t)
        (String.concat ", " (List.init n (fun _ -> string_of_int (int g (2 * n))))));
  def g t r

let gen_op g t =
  match t with
  | I w -> gen_int_op g w
  | F16 | F32 | F64 | F80 -> gen_float_op g t
  | V (n, I w) -> gen_vint_op g n w
  | V (n, e) -> gen_vfloat_op g n e

(* A pointer into the buffer, aligned for [t], with room for it *)
let slot g t =
  let s = slot_size t and a = align_of t in
  let room = (64 - s) / a in
  let rec mask k = if 2 * k <= room + 1 then mask (2 * k) else k - 1 in
  let ix = fresh g and p = fresh g in
  line g "%s = and i64 %s, i64 %d" ix (int_of g 64) (mask 1);
  line g "%s = ptradd ptr %s, %s, %d, 0" p g.mem ix a;
  if chance g 0.1 then begin
    (* through an integer and back *)
    let i = fresh g and q = fresh g in
    line g "%s = ptrtoint i64 %s" i p;
    line g "%s = inttoptr ptr %s" q i;
    q
  end
  else p

(* a byte position in the buffer below [lim] *)
let pos g base lim =
  let ix = fresh g and p = fresh g in
  line g "%s = and i64 %s, i64 %d" ix (int_of g 64) (lim - 1);
  line g "%s = ptradd ptr %s, %s, 1, %d" p g.mem ix base;
  p

(* memmove of 1..32 bytes between two places in the buffer (overlap
   allowed), memcpy between its two halves, memset *)
let gen_move g =
  match int g 3 with
  | 0 ->
      let d = pos g 0 32 in
      let src = pos g 0 32 in
      line g "memmove %s, %s, i64 %d" d src (1 + int g 32)
  | 1 ->
      let lo = pos g 0 16 and hi = pos g 32 16 in
      let d, s = if chance g 0.5 then (lo, hi) else (hi, lo) in
      line g "memcpy %s, %s, i64 %d" d s (1 + int g 16)
  | _ ->
      let d = pos g 0 32 in
      line g "memset %s, %s, i64 %d" d (int_of g 8) (1 + int g 32)

let rmw_ops = function
  | I _ -> [ "xchg"; "add"; "sub"; "and"; "or"; "xor"; "min"; "max"; "umin"; "umax" ]
  | _ -> [ "xchg"; "fadd"; "fsub" ]

(* atomics on the buffer, single-threaded: their values must match *)
let gen_atomic g =
  let t = choose g [ I 8; I 16; I 32; I 64; I 128; F32; F64 ] in
  let tt = ty_text t in
  let p = slot g t in
  let a = align_of t in
  match int g 5 with
  | 0 ->
      let r = fresh g in
      line g "%s = atomic_rmw %s %s %s, %s, align %d %s" r (choose g (rmw_ops t)) tt p (pick g t) a
        (choose g [ "relaxed"; "acquire"; "release"; "acq_rel"; "seq_cst" ]);
      def g t r
  | 1 when (match t with I _ -> true | _ -> false) ->
      let old = fresh g and ok = fresh g and r = fresh g in
      let ord, fail = choose g [ ("seq_cst", "seq_cst"); ("acq_rel", "acquire"); ("relaxed", "relaxed"); ("acquire", "relaxed") ] in
      (* the expected value is often what the slot holds *)
      let e =
        if chance g 0.5 then begin
          let cur = fresh g in
          line g "%s = load %s %s, align %d" cur tt p a;
          cur
        end
        else pick g t
      in
      line g "%s, %s = cmpxchg %s(%s, i1) %s, %s, %s, align %d %s, fail %s" old ok
        (if chance g 0.3 && t <> I 128 then "weak " else "")
        tt p e (pick g t) a ord fail;
      def g t old;
      line g "%s = select %s %s, %s, %s" r tt ok (pick g t) old;
      def g t r
  | 2 ->
      let r = fresh g in
      line g "%s = load %s %s, align %d %s" r tt p a (choose g [ "relaxed"; "acquire"; "seq_cst" ]);
      def g t r
  | 3 -> line g "store %s, %s, align %d %s" (pick g t) p a (choose g [ "relaxed"; "release"; "seq_cst" ])
  | _ -> line g "fence %s" (choose g [ "acquire"; "release"; "acq_rel"; "seq_cst" ])

(* a dynamic alloca between stacksave and stackrestore *)
let gen_stack g =
  let s = fresh g and n0 = fresh g and n = fresh g and p = fresh g in
  line g "%s = stacksave" s;
  line g "%s = and i64 %s, i64 63" n0 (int_of g 64);
  line g "%s = add i64 %s, i64 8" n n0;
  line g "%s = alloca ptr %s, align %d" p n (choose g [ 8; 16; 32 ]);
  line g "memset %s, i8 0, %s" p n;
  let t = choose g [ I 8; I 32; I 64; F64 ] in
  line g "store %s, %s, align 8" (pick g t) p;
  let r = fresh g in
  line g "%s = load %s %s, align 8" r (ty_text t) p;
  def g t r;
  line g "stackrestore %s" s

let gen_mem g =
  match int g 20 with
  | 0 | 1 | 2 -> gen_move g
  | 3 | 4 | 5 -> gen_atomic g
  | 6 -> gen_stack g
  | _ -> (
      let t = any_ty g in
      let p = slot g t in
      let a = align_of t in
      match t with
      | V (n, _) when chance g 0.3 ->
          if chance g 0.5 then line g "masked_store %s, %s, %s, align %d" (pick g t) p (vcond g n) a
          else begin
            let r = fresh g in
            line g "%s = masked_load %s %s, %s, %s, align %d" r (ty_text t) p (vcond g n) (pick g t) a;
            def g t r
          end
      | _ ->
          if chance g 0.5 then line g "store %s, %s, align %d" (pick g t) p a
          else begin
            let r = fresh g in
            line g "%s = load %s %s, align %d" r (ty_text t) p a;
            def g t r
          end)

let sig_text (s : fn_sig) =
  Printf.sprintf "(%s) -> (%s)"
    (String.concat ", " (List.map (fun (t, abi) -> abi ^ ty_text t) s.params))
    (ty_text s.ret)

(* the variadic helper every module defines (gen_module): @va(n, ...) adds
   n unnamed arguments, i64 at even positions and f64 (saturated to an
   integer) at odd ones *)
let va_helper =
  {|func @va (i32, ...) -> (i64) local {
^e(%n: i32):
  %ap = alloca ptr i64 24, align 8
  va_start %ap
  jump ^loop(i32 0, i64 0)
^loop(%i: i32, %acc: i64):
  %done = icmp uge i1 %i, %n
  br %done, ^exit, ^body
^body:
  %odd0 = and i32 %i, i32 1
  %odd = icmp ne i1 %odd0, i32 0
  br %odd, ^f, ^int
^int:
  %x = va_arg i64 %ap
  %a1 = mul i64 %acc, i64 31
  %a2 = add i64 %a1, %x
  jump ^next(%a2)
^f:
  %y = va_arg f64 %ap
  %yi = fptosi_sat i64 %y
  %a3 = mul i64 %acc, i64 37
  %a4 = xor i64 %a3, %yi
  jump ^next(%a4)
^next(%acc2: i64):
  %i2 = add i32 %i, i32 1
  jump ^loop(%i2, %acc2)
^exit:
  ret %acc
}

|}

let gen_call g =
  if chance g 0.2 then begin
    let n = int g 14 in
    let args = List.init n (fun k -> if k mod 2 = 0 then int_of g 64 else pick g F64) in
    let r = fresh g in
    line g "%s = call (i32, ...) -> (i64) @va(i32 %d%s)" r n (String.concat "" (List.map (fun a -> ", " ^ a) args));
    def g (I 64) r
  end
  else if g.callees <> [] then begin
    let s = choose g g.callees in
    let args = List.map (fun (t, _) -> match t with I w -> int_of g w | _ -> pick g t) s.params in
    let r = fresh g in
    if chance g 0.3 then begin
      (* through a pointer: the SysV path for function pointers *)
      let fp = fresh g in
      line g "%s = select ptr %s, ptr @%s, ptr @%s" fp (gen_cond g) s.fname s.fname;
      line g "%s = call %s %s(%s)" r (sig_text s) fp (String.concat ", " args)
    end
    else line g "%s = call %s @%s(%s)" r (sig_text s) s.fname (String.concat ", " args);
    def g s.ret r
  end

let rec gen_stmts g depth n =
  for _ = 1 to n do
    match int g 20 with
    | 0 | 1 when depth > 0 -> gen_if g depth
    | 2 when depth > 0 -> gen_loop g depth
    | 6 when depth > 0 -> gen_switch g depth
    | 3 | 4 -> gen_mem g
    | 5 -> gen_call g
    | _ -> gen_op g (any_ty g)
  done

(* if/else diamond merging one value of a random type *)
and gen_if g depth =
  let t = any_ty g in
  let c = gen_cond g in
  let yes = label g and no = label g and join = label g in
  line g "br %s, %s, %s" c yes no;
  let saved = g.env in
  let arm l =
    Buffer.add_string g.buf (l ^ ":\n");
    gen_stmts g (depth - 1) (1 + int g 4);
    let v = pick g t in
    line g "jump %s(%s)" join v;
    g.env <- saved
  in
  arm yes;
  arm no;
  let m = fresh g in
  Buffer.add_string g.buf (Printf.sprintf "%s(%s: %s):\n" join m (ty_text t));
  def g t m

(* switch on a small value, every arm merging one value *)
and gen_switch g depth =
  let t = any_ty g in
  let w = choose g [ 8; 16; 32; 64; 128 ] in
  let x = fresh g in
  line g "%s = and i%d %s, i%d 7" x w (int_of g w) w;
  let n = 1 + int g 4 in
  let arms = List.init n (fun _ -> label g) in
  let default = label g and join = label g in
  let keys = List.sort_uniq compare (List.init n (fun _ -> int g 8)) in
  let cases = List.mapi (fun k key -> Printf.sprintf "%d: %s" key (List.nth arms (k mod n))) keys in
  line g "switch %s, %s, [%s]" x default (String.concat ", " cases);
  let saved = g.env in
  List.iter
    (fun l ->
      Buffer.add_string g.buf (l ^ ":\n");
      gen_stmts g (depth - 1) (int g 3);
      line g "jump %s(%s)" join (pick g t);
      g.env <- saved)
    (arms @ [ default ]);
  let m = fresh g in
  Buffer.add_string g.buf (Printf.sprintf "%s(%s: %s):\n" join m (ty_text t));
  def g t m

(* counted loop carrying one accumulator *)
and gen_loop g depth =
  let t = any_ty g in
  let trip = int g 7 in
  let head = label g and body = label g and exit = label g in
  let init = pick g t in
  line g "jump %s(i32 0, %s)" head init;
  let saved = g.env in
  let i = fresh g and acc = fresh g in
  Buffer.add_string g.buf
    (Printf.sprintf "%s(%s: i32, %s: %s):\n" head i acc (ty_text t));
  def g (I 32) i;
  def g t acc;
  let c = fresh g in
  line g "%s = icmp ult i1 %s, i32 %d" c i trip;
  line g "br %s, %s, %s(%s)" c body exit acc;
  Buffer.add_string g.buf (body ^ ":\n");
  gen_stmts g (depth - 1) (1 + int g 5);
  gen_op g t;
  let acc2 = fst (List.find (fun (_, t') -> t' = t) g.env) in
  let i2 = fresh g in
  line g "%s = add i32 %s, i32 1" i2 i;
  line g "jump %s(%s, %s)" head i2 acc2;
  g.env <- saved;
  let r = fresh g in
  Buffer.add_string g.buf (Printf.sprintf "%s(%s: %s):\n" exit r (ty_text t));
  def g t r

let gen_func rs buf callees (s : fn_sig) =
  let g = { rs; buf; next = 0; env = []; mem = "%mem"; callees } in
  let ps = List.mapi (fun k (t, _) -> (Printf.sprintf "%%p%d" k, t)) s.params in
  Buffer.add_string buf
    (Printf.sprintf "func @%s %s local {\n^entry(%s):\n" s.fname (sig_text s)
       (String.concat ", " (List.map (fun (n, t) -> n ^ ": " ^ ty_text t) ps)));
  g.env <- List.rev ps;
  line g "%%mem = alloca ptr i64 64, align 32";
  line g "memset %%mem, i8 0, i64 64";
  gen_stmts g 2 (3 + int g 10);
  line g "ret %s" (pick g s.ret);
  Buffer.add_string buf "}\n\n"

let gen_module seed =
  let rs = Random.State.make [| seed |] in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "triple \"x86_64-unknown-linux-gnu\"\n\n";
  Buffer.add_string buf va_helper;
  let nf = 1 + Random.State.int rs 4 in
  let g0 = { rs; buf; next = 0; env = []; mem = ""; callees = [] } in
  let sigs =
    List.init nf (fun k ->
        let np = Random.State.int rs 11 in
        let params =
          List.init np (fun _ ->
              let t = any_ty g0 in
              let abi =
                match t with
                | I w when w < 32 && Random.State.bool rs ->
                    if Random.State.bool rs then "sext " else "zext "
                | _ -> ""
              in
              (t, abi))
        in
        let ret = any_ty g0 in
        { fname = "f" ^ string_of_int k; params; ret })
  in
  List.iteri (fun k s -> gen_func rs buf (List.filteri (fun j _ -> j < k) sigs) s) sigs;
  (* main: call every function with arguments derived from argc and
     constants, and write each result's bytes (NaNs made canonical) *)
  let g = { rs; buf; next = 0; env = []; mem = "%out"; callees = [] } in
  Buffer.add_string buf "func @main (i32, ptr) -> (i32) global {\n^entry(%argc: i32, %argv: ptr):\n";
  g.env <- [ ("%argc", I 32) ];
  line g "%%out = alloca ptr i64 64, align 32";
  List.iter
    (fun s ->
      let args =
        List.map
          (fun (t, _) ->
            match t with
            | I w when chance g 0.3 ->
                let a = int_of g w in
                let r = fresh g in
                line g "%s = add i%d %s, %s" r w a (const g (I w));
                r
            | _ -> const g t)
          s.params
      in
      let r = fresh g in
      line g "%s = call %s @%s(%s)" r (sig_text s) s.fname (String.concat ", " args);
      let v = canon g s.ret r in
      line g "store %s, %%out, align %d" v (align_of s.ret);
      let pi = fresh g and w = fresh g in
      line g "%s = ptrtoint i64 %%out" pi;
      line g "%s = syscall i64 i64 1, i64 1, %s, i64 %d" w pi (store_size s.ret))
    sigs;
  line g "ret i32 0";
  Buffer.add_string buf "}\n";
  Buffer.contents buf

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

let run exe out =
  let status = Sys.command (Printf.sprintf "timeout 10 %s > %s 2>/dev/null" (Filename.quote exe) (Filename.quote out)) in
  (status, read out)

type outcome = Same | Differ of string | Broken of string

let file = ref ""

(* The generator's random CFGs can leave blocks no path reaches; SPEC 0.1
   requires every block reachable, so they are dropped as the front end
   does. *)
let parse_module text =
  let m = Fir_parse.parse_module text in
  Fir.Vec.iter
    (fun (f : Fir.func) ->
      if f.defined && not (Fir.blocks_pinned m f) then ignore (Fir.remove_unreachable_blocks f))
    m.funcs;
  m

let check_seed tmp seed =
  let text = if !file <> "" then read !file else gen_module seed in
  let save () =
    let path = Filename.concat !keep (Printf.sprintf "fir_fuzz_%d.fir" seed) in
    write path text;
    path
  in
  match parse_module text with
  | exception Fir_parse.Error (l, msg) -> Broken (Printf.sprintf "parse line %d: %s (%s)" l msg (save ()))
  | m -> (
      match Fir_verify.verify m with
      | _ :: _ as errs -> Broken ("verifier: " ^ String.concat "; " errs ^ " (" ^ save () ^ ")")
      | [] -> (
          match Fir_llvm.render_bounded ~budget:max_int m with
          | Error msg -> Broken ("fir -> llvm: " ^ msg ^ " (" ^ save () ^ ")")
          | Ok llvm_text -> (
              let nexe = Filename.concat tmp "n" and lexe = Filename.concat tmp "l" in
              let ll = Filename.concat tmp "m.ll" in
              write ll llvm_text;
              (* -lm: without SSE4.1, LLVM lowers floor/ceil/... to libm calls *)
              let lstatus =
                Sys.command
                  (Printf.sprintf "%s -O2 -w -x ir %s -lm -latomic -o %s 2>%s" !cc (Filename.quote ll) (Filename.quote lexe)
                     (Filename.quote (Filename.concat tmp "cc.err")))
              in
              if lstatus <> 0 then
                Broken ("clang: " ^ read (Filename.concat tmp "cc.err") ^ " (" ^ save () ^ ")")
              else
                (* the native backend gets its own copy, legalized *)
                let native () =
                  let m = parse_module text in
                  Fir_wide.run m;
                  match Fir_verify.verify m with
                  | [] -> Mir_emit.compile m
                  | errs -> failwith ("verifier after Fir_wide: " ^ String.concat "; " errs)
                in
                match (try native () with e -> Error (Printexc.to_string e)) with
                | Error msg -> Differ ("native compile: " ^ msg ^ " (" ^ save () ^ ")")
                | Ok image ->
                    let oc = open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] 0o755 nexe in
                    output_bytes oc image;
                    close_out oc;
                    let ls, lo = run lexe (Filename.concat tmp "l.out") in
                    let ns, no = run nexe (Filename.concat tmp "n.out") in
                    (* the interpreter is the third side: it must agree with
                       LLVM too (Sys.command's status is the exit code) *)
                    let interp =
                      match Fir_interp.run_program ~step_limit:50_000_000 [ parse_module text ] with
                      | r -> Ok (r.status, r.stdout)
                      | exception Fir_interp.Error msg -> Error ("interpreter: " ^ msg)
                      | exception e -> Error ("interpreter: " ^ Printexc.to_string e)
                    in
                    let interp_differs =
                      match interp with
                      | Ok (is, io) when is = ls && io = lo -> None
                      | Ok (is, io) -> Some (Printf.sprintf "interpreter status %d, stdout %S" is io)
                      (* f80 arithmetic and inline asm are outside the
                         interpreter (fir_interp.ml): LLVM and native alone *)
                      | Error msg
                        when List.exists
                               (fun k ->
                                 let n = String.length k in
                                 let rec at i = i + n <= String.length msg && (String.sub msg i n = k || at (i + 1)) in
                                 at 0)
                               [ "not supported by the interpreter"; "which the interpreter cannot do" ] ->
                          None
                      | Error msg -> Some msg
                    in
                    if ls = ns && lo = no && interp_differs = None then Same
                    else if ls = ns && lo = no then
                      Differ (Printf.sprintf "llvm and native agree (status %d), %s (%s)" ls
                                (Option.get interp_differs) (save ()))
                    else begin
                      (* keep both programs next to the module, for disassembly *)
                      let base = Filename.concat !keep (Printf.sprintf "fir_fuzz_%d" seed) in
                      ignore
                        (Sys.command
                           (Printf.sprintf "cp %s %s.native && cp %s %s.ll" (Filename.quote nexe) (Filename.quote base)
                              (Filename.quote ll) (Filename.quote base)));
                      let hex s = String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s))) in
                      Differ
                        (Printf.sprintf "llvm status %d, native status %d\n    llvm   %s\n    native %s\n    (%s)" ls ns
                           (hex lo) (hex no) (save ()))
                    end)))

let () =
  Arg.parse
    [
      ("-n", Arg.Set_int count, "COUNT number of seeds (default 200)");
      ("-seed", Arg.Set_int first, "FIRST first seed (default 1)");
      ("-keep", Arg.Set_string keep, "DIR where failing modules are written");
      ("-cc", Arg.Set_string cc, "CLANG clang used for the LLVM side");
      ("-v", Arg.Set verbose, " print every seed");
      ("-mir", Arg.Unit ignore, " no effect (the native side is always the machine IR)");
      ("-file", Arg.String (fun f -> file := f; count := 1), "PATH check this .fir module instead of generating");
    ]
    (fun _ -> ())
    "fir_fuzz [-n COUNT] [-seed FIRST] [-keep DIR]";
  let tmp = Filename.temp_dir "fir_fuzz" "" in
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
        Printf.printf "seed %d: generator/converter problem: %s\n%!" seed msg
  done;
  ignore (Sys.command ("rm -rf " ^ Filename.quote tmp));
  Printf.printf "fir fuzz: %d same, %d differ, %d broken\n" !same !differ !broken;
  if !differ + !broken > 0 then exit 1
