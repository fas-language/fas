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
  var "a" 0 arg 1
  var "s" 0 at %p
  var "t" 0
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

(* debug variables (roadmap v0.8): types, lexical scopes, variables *)
let debugvars =
  {|
triple "x86_64-unknown-linux-gnu"
loc 1 "v.fas" 1 1
loc 2 "v.fas" 2 3 !scope 1
loc 3 "v.fas" 3 5 !scope 2
scope 1 parent 0
scope 2 parent 1
dtype 1 base "i64" signed 8
dtype 2 pointer 1
dtype 3 struct "P" size 16 {"x" 1 0, "y" 1 8}
dtype 4 union "U" declared
dtype 5 array 1 count 4
dtype 6 vector 1 count 2
dtype 7 typedef "word" 1
dtype 8 enum "E" 1 {"A" 0, "B" -1}
dtype 9 base "bool" boolean 1

func @f (i64) -> (i64) global {
  var "n" 1 arg 1 !loc 1 at %n.addr
  var "x" 1 scope 1 !loc 2 at %x
  var "x" 3 scope 2 !loc 3
  var "w" 7 scope 2
^entry(%n: i64):
  %n.addr = alloca ptr i64 8, align 8
  store %n, %n.addr, align 8
  %x = alloca ptr i64 8, align 8 !loc 2
  store %n, %x, align 8 !loc 2
  ret %n !loc 3
}
|}

let () =
  roundtrip "sum" sum;
  roundtrip "debugvars" debugvars;
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
  rejects "var type" "type 9 out of range"
    {|
func @bad () -> () global {
  var "x" 9
^entry:
  ret
}
|};
  rejects "var home" "home must be a pointer"
    {|
func @bad (i64) -> () global {
  var "x" 0 at %a
^entry(%a: i64):
  ret
}
|};
  rejects "var arg" "arg 2 out of range"
    {|
func @bad (i64) -> () global {
  var "x" 0 arg 2
^entry(%a: i64):
  ret
}
|};
  rejects "dtype member" "member type 7 out of range"
    {|
dtype 1 struct "S" size 8 {"a" 7 0}
|};
  rejects "scope owner" "scope 1 is used by both"
    {|
loc 1 "a" 1 1 !scope 1
scope 1 parent 0
func @a () -> () global {
^entry:
  ret !loc 1
}
func @b () -> () global {
  var "x" 0 scope 1
^entry:
  ret
}
|};
  rejects "dbg_value range" "dbg_value of variable 2, the function has 1"
    {|
func @bad (i64) -> () global {
  var "x" 0
^entry(%a: i64):
  dbg_value 2, %a
  ret
}
|};
  rejects "dbg_value home" "which lives in memory"
    {|
func @bad (ptr) -> () global {
  var "x" 0 at %p
^entry(%p: ptr):
  dbg_value 1, %p
  ret
}
|};
  (* constants are interned per function *)
  let f = new_func "k" { param_abis = []; result_abis = []; variadic = false } in
  check "intern" (iconst f i64 5L = iconst f i64 5L);
  check "intern width" (iconst f i32 5L <> iconst f i64 5L);
  check "neg limbs" (const_of f (iconst f i128 (-1L)) = Some (Cint (i128, [| -1L; -1L |])));
  check "i7 normalize" (const_of f (iconst f (Int 7) (-1L)) = Some (Cint (Int 7, [| 127L |])));
  (* ELF sh_type for .init_array / .fini_array / .preinit_array sections *)
  (match
     parse "elf_lifecycle_sections"
       {|
triple "x86_64-unknown-linux-gnu"
func @ctor1 () -> () local {
^entry:
  ret
}
func @ctor2 () -> () local {
^entry:
  ret
}
func @dtor1 () -> () local {
^entry:
  ret
}
func @pre1 () -> () local {
^entry:
  ret
}
global @.Lctor.0 size 8 align 8 local section ".init_array" = { reloc @ctor1 w8 }
global @.Lctor.1 size 8 align 8 local section ".init_array.00101" = { reloc @ctor2 w8 }
global @.Ldtor.0 size 8 align 8 local section ".fini_array" = { reloc @dtor1 w8 }
global @.Lpre.0 size 8 align 8 local section ".preinit_array" = { reloc @pre1 w8 }
|}
   with
  | None -> ()
  | Some m -> (
      match Mir_driver.compile_object ~optimization:0 m with
      | Error e ->
          incr failures;
          Printf.printf "FAIL elf_lifecycle_sections: compile_object: %s\n%!" e
      | Ok bytes ->
          let obj = Fas_linker.Elf_reader.parse ~path:"<elf_lifecycle_sections>" (Bytes.unsafe_to_string bytes) in
          let find_kind name =
            Array.to_list obj.sections
            |> List.find_opt (fun (s : Fas_linker.Elf_reader.section) -> s.name = name)
            |> Option.map (fun (s : Fas_linker.Elf_reader.section) -> (s.kind, s.flags))
          in
          check "sh_type .init_array" (find_kind ".init_array" = Some (14, 3));
          check "sh_type .init_array.00101" (find_kind ".init_array.00101" = Some (14, 3));
          check "sh_type .fini_array" (find_kind ".fini_array" = Some (15, 3));
          check "sh_type .preinit_array" (find_kind ".preinit_array" = Some (16, 3))));
  (* IFUNC: parse/print, verifier, interp, native ELF STT_GNU_IFUNC, and LLVM IR *)
  let ifunc_src =
    {|
triple "x86_64-unknown-linux-gnu"
func @impl (i32) -> (i32) local {
^entry(%x: i32):
  %r = add i32 %x, i32 40
  ret %r
}
func @res0 () -> (ptr) local {
^entry:
  ret ptr @impl
}
func @res1 (i64) -> (ptr) local {
^entry(%hw: i64):
  ret ptr @impl
}
func @res2 (i64, ptr) -> (ptr) local {
^entry(%hw: i64, %p: ptr):
  ret ptr @impl
}
ifunc @magic global = @res0
ifunc @magic_loc local = @res1
ifunc @magic_hid global hidden = @res2
func @main () -> (i32) global {
^entry:
  %a = call (i32) -> (i32) @magic(i32 2)
  %b = call (i32) -> (i32) ptr @magic_loc(i32 2)
  %c = call (i32) -> (i32) @magic_hid(i32 2)
  %s1 = add i32 %a, %b
  %s2 = add i32 %s1, %c
  ret %s2
}
|}
  in
  (match parse "ifunc_ok" ifunc_src with
  | None -> ()
  | Some m -> (
      let printed = Fir_print.module_to_string m in
      (match parse "ifunc_roundtrip" printed with None -> () | Some _ -> ());
      let pr = Fir_interp.run_program [ m ] in
      check "ifunc interp status" (pr.status = 126 && pr.trapped = None);
      let ll = Fir_llvm.render m in
      let has_sub s sub =
        let n = String.length s and k = String.length sub in
        let rec loop i = i + k <= n && (String.sub s i k = sub || loop (i + 1)) in
        loop 0
      in
      check "ifunc llvm global" (has_sub ll "@magic = ifunc void (), ptr @res0");
      check "ifunc llvm local" (has_sub ll "@magic_loc = internal ifunc void (), ptr @res1");
      check "ifunc llvm hidden" (has_sub ll "@magic_hid = hidden ifunc void (), ptr @res2");
      match Mir_driver.compile_object ~optimization:0 m with
      | Error e ->
          incr failures;
          Printf.printf "FAIL ifunc_ok: compile_object: %s\n%!" e
      | Ok bytes ->
          let obj = Fas_linker.Elf_reader.parse ~path:"<ifunc_ok>" (Bytes.unsafe_to_string bytes) in
          let find_sym name =
            Array.to_list obj.symbols
            |> List.find_opt (fun (s : Fas_linker.Elf_reader.symbol) -> s.name = name)
          in
          let sym_attr name =
            find_sym name |> Option.map (fun (s : Fas_linker.Elf_reader.symbol) -> (s.kind, s.binding, s.visibility))
          in
          check "ifunc elf magic" (sym_attr "magic" = Some (10, 1, 0));
          check "ifunc elf magic_loc" (sym_attr "magic_loc" = Some (10, 0, 0));
          check "ifunc elf magic_hid" (sym_attr "magic_hid" = Some (10, 1, 2))));
  rejects "ifunc dup name" "symbol @dup defined twice"
    {|
func @dup () -> (ptr) local {
^entry:
  ret ptr null
}
ifunc @dup global = @dup
|};
  rejects "ifunc undef resolver" "resolver is not a function defined in this module"
    {|
ifunc @bad global = @missing
|};
  rejects "ifunc decl-only resolver" "resolver is only declared"
    {|
declare @res () -> (ptr) global
ifunc @bad global = @res
|};
  rejects "ifunc bad return" "resolver must return a single ptr"
    {|
func @res () -> (i64) local {
^entry:
  ret i64 0
}
ifunc @bad global = @res
|};
  rejects "ifunc bad params" "resolver parameters must be"
    {|
func @res (i32) -> (ptr) local {
^entry(%x: i32):
  ret ptr null
}
ifunc @bad global = @res
|};
  (* asm_goto with outputs: parse/print, verifier, native + LLVM *)
  let ag_out_src =
    {|
triple "x86_64-unknown-linux-gnu"
func @ag_out (i64) -> (i64) global {
^e(%x: i64):
  %a, %b = asm_goto volatile "addq $10, %0\0a\09movq $32, %1\0a\09testq %2, %2\0a\09jnz %l3" out("=r" i64, "=r" i64) in("0" i64 %x) ^fall [^taken]
^fall:
  %s1 = add i64 %a, %b
  ret %s1
^taken:
  %s2 = add i64 %a, %b
  %s3 = add i64 %s2, i64 100
  ret %s3
}
|}
  in
  (match parse "asm_goto_outputs" ag_out_src with
  | None -> ()
  | Some m -> (
      let printed = Fir_print.module_to_string m in
      (match parse "asm_goto_outputs_roundtrip" printed with None -> () | Some _ -> ());
      let ll = Fir_llvm.render m in
      let has_sub s sub =
        let n = String.length s and k = String.length sub in
        let rec loop i = i + k <= n && (String.sub s i k = sub || loop (i + 1)) in
        loop 0
      in
      check "asm_goto_outputs llvm callbr" (has_sub ll "callbr { i64, i64 } asm sideeffect");
      match Mir_driver.compile_object ~optimization:0 m with
      | Error e ->
          incr failures;
          Printf.printf "FAIL asm_goto_outputs: compile_object: %s\n%!" e
      | Ok _ -> ()));
  rejects "asm_goto tied mismatch" "is tied to output 0"
    {|
func @bad (i32) -> (i64) global {
^e(%x: i32):
  %a = asm_goto "" out("=r" i64) in("0" i32 %x) ^fall [^lab]
^fall:
  ret %a
^lab:
  ret %a
}
|};
  if Sys.getenv_opt "FIR_SHOW" <> None then (match Fir_parse.parse_module everything with m -> print_string (Fir_print.module_to_string m));
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1) else print_endline "fir: ok"
