(* Fir core tests: text round-trip, verifier acceptance and rejection. *)

open Fir

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let parse name text =
  try Some (Fir_parse.parse_module text)
  with Fir_parse.Error (line, msg) ->
    incr failures;
    Printf.printf "FAIL %s: parse error line %d: %s\n%!" name line msg;
    None

(* print (parse text) must verify and be a fixed point of parse+print *)
let roundtrip name text =
  match parse name text with
  | None -> ()
  | Some m -> (
      (match Fir_verify.verify m with
      | [] -> ()
      | es ->
          incr failures;
          Printf.printf "FAIL %s: verifier:\n  %s\n%!" name (String.concat "\n  " es));
      let p1 = Fir_print.module_to_string m in
      match parse (name ^ " (reparse)") p1 with
      | None -> print_string p1
      | Some m2 ->
          let p2 = Fir_print.module_to_string m2 in
          if p1 <> p2 then begin
            incr failures;
            Printf.printf "FAIL %s: round trip differs\n--- first\n%s--- second\n%s\n%!" name p1 p2
          end)

(* the verifier must reject [text] with a message containing [needle] *)
let rejects name needle text =
  match parse name text with
  | None -> ()
  | Some m ->
      let es = Fir_verify.verify m in
      let contains s =
        let n = String.length needle and l = String.length s in
        let rec go i = i + n <= l && (String.sub s i n = needle || go (i + 1)) in
        go 0
      in
      if not (List.exists contains es) then begin
        incr failures;
        Printf.printf "FAIL %s: expected an error containing %S, got:\n  %s\n%!" name needle
          (String.concat "\n  " es)
      end

let sum =
  {|
triple "x86_64-unknown-linux-gnu"
loc 1 "sum.fas" 4 3
global @counter size 8 align 8 local = zero
global @msg size 5 align 1 local readonly = {bytes "hi\0a", zeros 2}
global @table size 16 align 8 global = {reloc @counter w8, reloc @msg+2 w8}
global @tls size 4 align 4 global hidden thread_local = zero
global @ext size 0 align 1 global = extern

declare @puts (ptr) -> (i32) global

func @sum (ptr, i64) -> (i64) global {
^entry(%p: ptr, %n: i64):
  jump ^loop(i64 0, i64 0)
^loop(%i: i64, %acc: i64):
  %c = icmp ult i1 %i, %n
  br %c, ^body, ^done(%acc), weight 900
^body:
  %a = ptradd inbounds ptr %p, %i, 8, 0
  %x = load i64 %a, align 8 !loc 1
  %acc2 = add i64 %acc, %x
  %i2 = add nuw nsw i64 %i, i64 1
  jump ^loop(%i2, %acc2)
^done(%r: i64):
  ret %r
}
|}

let everything =
  {|
triple "x86_64-unknown-linux-gnu"
strict_aliasing false
loc 1 "a.fas" 3 7
alias_tag 1 "int" 0

func @ops (i32, f64, ptr, <4 x i32>, i128) -> (i32) local noinline cold display "ops" {
^0(%a: i32, %f: f64, %p: ptr, %v: <4 x i32>, %w: i128):
  %1 = sub nsw i32 %a, i32 -5
  %2 = sdiv exact i32 %1, i32 3
  %3 = or disjoint i32 %2, i32 64
  %4 = ctz zero_poison i32 %3
  %5 = abs poison_min i32 %4
  %6 = bswap i32 %5
  %7 = fadd contract f64 %f, f64 0x3ff0000000000000
  %8 = ftrunc f64 %7
  %9 = fma f64 %8, %8, %f
  %10 = fshl i32 %a, %a, i32 7
  %11, %12 = sadd_ovf (i32, i1) %10, %a
  %13 = fcmp olt fast i1 %9, %f
  %14 = select i32 %13, %11, %6
  %15 = freeze i32 %14
  %16 = sext i64 %15
  %17 = sitofp f32 %16
  %18 = fpext f64 %17
  %19 = fptosi_sat i32 %18
  %20 = ptrtoint i64 %p
  %21 = inttoptr ptr %20
  %22 = alloca ptr i64 32, align 16
  store %19, %22, align 4 volatile !alias 1 !noalias [1, 2]
  %23 = load i32 %22, align 4 acquire
  %24 = atomic_rmw add i32 %22, %23, align 4 seq_cst
  %25, %26 = cmpxchg weak (i32, i1) %22, %24, i32 0, align 4 acq_rel, fail acquire
  fence seq_cst
  memcpy %22, %21, i64 16
  memset volatile %22, i8 0, i64 16
  %27 = extract i32 %v, i64 1
  %28 = insert <4 x i32> %v, %27, i64 0
  %29 = shuffle <2 x i32> %28, %v, [0, -1]
  %30 = splat <4 x i32> %25
  %31 = reduce umax i32 %30
  %32 = icmp eq <4 x i1> %28, <4 x i32> [i32 1, i32 2, i32 3, i32 4]
  %33 = masked_load <4 x i32> %p, %32, <4 x i32> zero, align 4
  masked_store %33, %p, %32, align 4
  %34 = compress <4 x i32> %33, %32
  %35 = call nounwind (ptr, ...) -> (i32) @printf(%p, %34, i128 0x1ffffffffffffffff)
  %36 = call (i32) -> (sext i8) %21(%35)
  %37 = syscall i64 i64 60, i64 0
  %38 = umulh i128 %w, i128 -1
  %39 = trunc i32 %38
  dbg_value 3, %39
  switch %39, ^d, [0: ^d, -1: ^e(%39), 7: ^e(i32 7)] !loc 1
^d:
  %40 = sext i32 %36
  ret %40
^e(%41: i32):
  trap
  unreachable
}
|}

(* forward reference: ^b is printed before the block defining %x *)
let forward =
  {|
func @fwd (i1) -> (i64) global {
^entry(%c: i1):
  jump ^def
^use:
  %y = add i64 %x, i64 1
  ret %y
^def:
  %x = add i64 i64 2, i64 3
  jump ^use
}
|}

let () =
  roundtrip "sum" sum;
  roundtrip "everything" everything;
  roundtrip "forward" forward;
  rejects "dominance" "does not dominate"
    {|
func @bad (i1) -> (i64) global {
^entry(%c: i1):
  br %c, ^a, ^b
^a:
  %x = add i64 i64 1, i64 2
  jump ^b
^b:
  ret %x
}
|};
  rejects "type" "rhs is i32"
    {|
func @bad (i64, i32) -> (i64) global {
^entry(%a: i64, %b: i32):
  %c = add i64 %a, %b
  ret %c
}
|};
  rejects "args" "takes 1 arguments"
    {|
func @bad () -> () global {
^entry:
  jump ^x
^x(%v: i8):
  ret
}
|};
  rejects "flags" "flags not allowed"
    {|
func @bad (i8) -> (i8) global {
^entry(%a: i8):
  %b = xor nsw i8 %a, %a
  ret %b
}
|};
  rejects "ordering" "release"
    {|
func @bad (ptr) -> (i8) global {
^entry(%p: ptr):
  %b = load i8 %p, align 1 release
  ret %b
}
|};
  (* constants are interned per function *)
  let f = new_func "k" { param_abis = []; result_abis = []; variadic = false } in
  check "intern" (iconst f i64 5L = iconst f i64 5L);
  check "intern width" (iconst f i32 5L <> iconst f i64 5L);
  check "neg limbs" (const_of f (iconst f i128 (-1L)) = Some (Cint (i128, [| -1L; -1L |])));
  check "i7 normalize" (const_of f (iconst f (Int 7) (-1L)) = Some (Cint (Int 7, [| 127L |])));
  if Sys.getenv_opt "FIR_SHOW" <> None then (match Fir_parse.parse_module everything with m -> print_string (Fir_print.module_to_string m));
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1) else print_endline "fir: ok"
