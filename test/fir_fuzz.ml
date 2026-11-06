(* Differential IR fuzzer.

   Each seed generates a random Fir module as text. The module is parsed,
   verified, and converted to the old Ir. That Ir is then compiled twice:
   - by the native backend, straight from Fir (Mir_emit.compile: Mir_isel,
     Mir_ra, Mir_check; -mir is accepted for old command lines);
   - by LLVM: Ir.render, then clang -O2.
   Both programs run and must print the same bytes and exit with the same
   status.

   The generated programs are free of undefined behaviour by construction:
   - shift counts are masked below the width;
   - divisors are forced odd and positive;
   - float-to-int conversions saturate;
   - memory comes from one zeroed stack buffer, indexed with a mask;
   - loops have constant trip counts;
   - calls only go to earlier functions, so there is no recursion.
   So any difference is a bug in Fir_to_ir, in the native backend, or
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

type ty = I of int | F32 | F64

let ty_text = function I w -> "i" ^ string_of_int w | F32 -> "f32" | F64 -> "f64"
let size = function I w -> max 1 (w / 8) | F32 -> 4 | F64 -> 8

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
let all_tys = int_tys @ [ F32; F64 ]

(* An interesting constant of type [t], as Fir text with its type. *)
let const g t =
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

(* Fir text for an operand of type [t]: usually a value in scope *)
let pick g t =
  let have = List.filter (fun (_, t') -> t' = t) g.env in
  if have <> [] && chance g 0.85 then fst (choose g have) else const g t

let def g t name = g.env <- (name, t) :: g.env

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

and gen_int_op g w =
  let t = Printf.sprintf "i%d" w in
  let a = int_of g w and b = pick g (I w) in
  let r = fresh g in
  (match int g 11 with
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
  | 6 -> line g "%s = %s %s %s" r (choose g [ "popcnt"; "ctz"; "clz" ]) t a
  | 7 ->
      let c = gen_cond g in
      line g "%s = select %s %s, %s, %s" r t c a b
  | _ ->
      (* from a float, saturating *)
      let ft = choose g [ F32; F64 ] in
      line g "%s = %s %s %s" r (choose g [ "fptosi_sat"; "fptoui_sat" ]) t (pick g ft));
  def g (I w) r

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
     let t = choose g [ F32; F64 ] in
     line g "%s = fcmp %s i1 %s, %s" c
       (* the old Ir has only these six *)
       (choose g [ "oeq"; "une"; "olt"; "ole"; "ogt"; "oge" ])
       (pick g t) (pick g t)
   end);
  c

let gen_float_op g t =
  let tt = ty_text t in
  let r = fresh g in
  (match int g 6 with
  | 0 when int g 4 = 0 -> line g "%s = fma %s %s, %s, %s" r tt (pick g t) (pick g t) (pick g t)
  | 0 | 1 ->
      line g "%s = %s %s %s, %s" r (choose g [ "fadd"; "fsub"; "fmul"; "fdiv"; "frem"; "copysign"; "fmin"; "fmax" ]) tt (pick g t) (pick g t)
  | 2 -> line g "%s = %s %s %s" r (choose g [ "fneg"; "fabs"; "sqrt"; "floor"; "ceil"; "roundeven" ]) tt (pick g t)
  | 3 ->
      let w = (choose g [ 8; 16; 32; 64; 128 ]) in
      line g "%s = %s %s %s" r (choose g [ "sitofp"; "uitofp" ]) tt (int_of g w)
  | 4 ->
      let other = if t = F32 then F64 else F32 in
      line g "%s = %s %s %s" r (if t = F32 then "fptrunc" else "fpext") tt (pick g other)
  | _ ->
      let c = gen_cond g in
      line g "%s = select %s %s, %s, %s" r tt c (pick g t) (pick g t));
  def g t r

let gen_op g t = match t with I w -> gen_int_op g w | F32 | F64 -> gen_float_op g t

(* A pointer into the buffer, aligned for [t] *)
let slot g t =
  let s = size t in
  let ix = fresh g and p = fresh g in
  line g "%s = and i64 %s, i64 %d" ix (int_of g 64) ((64 / s) - 1);
  line g "%s = ptradd ptr %s, %s, %d, 0" p g.mem ix s;
  p

(* memmove of 1..32 bytes between two places in the buffer (overlap allowed) *)
let gen_move g =
  let pos () =
    let ix = fresh g and p = fresh g in
    line g "%s = and i64 %s, i64 31" ix (int_of g 64);
    line g "%s = ptradd ptr %s, %s, 1, 0" p g.mem ix;
    p
  in
  let d = pos () in
  let src = pos () in
  line g "memmove %s, %s, i64 %d" d src (1 + int g 32)

let gen_mem g =
  if chance g 0.15 then gen_move g else
  let t = choose g all_tys in
  let p = slot g t in
  if chance g 0.5 then line g "store %s, %s, align %d" (pick g t) p (size t)
  else begin
    let r = fresh g in
    line g "%s = load %s %s, align %d" r (ty_text t) p (size t);
    def g t r
  end

let sig_text (s : fn_sig) =
  Printf.sprintf "(%s) -> (%s)"
    (String.concat ", " (List.map (fun (t, abi) -> abi ^ ty_text t) s.params))
    (ty_text s.ret)

let gen_call g =
  if g.callees <> [] then begin
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
    | _ -> gen_op g (choose g all_tys)
  done

(* if/else diamond merging one value of a random type *)
and gen_if g depth =
  let t = choose g all_tys in
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
  let t = choose g all_tys in
  let w = choose g [ 8; 16; 32; 64; 128 ] in
  let x = fresh g in
  line g "%s = and i%d %s, i%d 7" x w (int_of g w) w;
  let n = 1 + int g 4 in
  let arms = List.init n (fun _ -> label g) in
  let default = label g and join = label g in
  let keys = List.sort_uniq compare (List.init n (fun _ -> int g 8)) in
  let cases = List.mapi (fun k key -> ignore w; Printf.sprintf "%d: %s" key (List.nth arms (k mod n))) keys in
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
  let t = choose g all_tys in
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
  let acc2 = fst (List.hd g.env) in
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
  line g "%%mem = alloca ptr i64 64, align 8";
  line g "memset %%mem, i8 0, i64 64";
  gen_stmts g 2 (3 + int g 10);
  line g "ret %s" (pick g s.ret);
  Buffer.add_string buf "}\n\n"

let gen_module seed =
  let rs = Random.State.make [| seed |] in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "triple \"x86_64-unknown-linux-gnu\"\n\n";
  let nf = 1 + Random.State.int rs 4 in
  let sigs =
    List.init nf (fun k ->
        let np = Random.State.int rs 11 in
        let params =
          List.init np (fun _ ->
              let t = List.nth all_tys (Random.State.int rs (List.length all_tys)) in
              let abi =
                match t with
                | I w when w < 32 && Random.State.bool rs ->
                    if Random.State.bool rs then "sext " else "zext "
                | _ -> ""
              in
              (t, abi))
        in
        let ret = List.nth all_tys (Random.State.int rs (List.length all_tys)) in
        { fname = "f" ^ string_of_int k; params; ret })
  in
  List.iteri (fun k s -> gen_func rs buf (List.filteri (fun j _ -> j < k) sigs) s) sigs;
  (* main: call every function with arguments derived from argc and
     constants, and write each result as 8 bytes *)
  let g = { rs; buf; next = 0; env = []; mem = "%out"; callees = [] } in
  Buffer.add_string buf "func @main (i32, ptr) -> (i32) global {\n^entry(%argc: i32, %argv: ptr):\n";
  g.env <- [ ("%argc", I 32) ];
  line g "%%out = alloca ptr i64 16, align 16";
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
      let bits =
        match s.ret with
        | I 64 | I 128 -> r
        | I w ->
            let z = fresh g in
            line g "%s = zext i64 %s" z r;
            ignore w;
            z
        | F32 | F64 ->
            (* NaN payloads may differ between constant folding and the
               hardware: print one canonical NaN *)
            let tt = ty_text s.ret in
            let u = fresh g and c = fresh g and b = fresh g in
            line g "%s = fcmp une i1 %s, %s" u r r;
            line g "%s = select %s %s, %s, %s" c tt u
              (if s.ret = F64 then "f64 0x7ff8000000000000" else "f32 0x7fc00000")
              r;
            if s.ret = F64 then line g "%s = bitcast i64 %s" b c
            else begin
              let b32 = fresh g in
              line g "%s = bitcast i32 %s" b32 c;
              line g "%s = zext i64 %s" b b32
            end;
            b
      in
      line g "store %s, %%out, align 8" bits;
      let pi = fresh g and w = fresh g in
      line g "%s = ptrtoint i64 %%out" pi;
      line g "%s = syscall i64 i64 1, i64 1, %s, i64 %d" w pi (if s.ret = I 128 then 16 else 8))
    sigs;
  line g "ret i32 0";
  Buffer.add_string buf "}\n";
  Buffer.contents buf

(* ---------- harness ---------- *)

let like : Ir.module_ =
  {
    target_triple = "x86_64-unknown-linux-gnu";
    data_layout = Target_layout.x86_64_linux.llvm_data_layout;
    structs = [];
    globals = [];
    funcs = [];
    no_inline_functions = [];
    inline_functions = [];
  }

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

let check_seed tmp seed =
  let text = if !file <> "" then read !file else gen_module seed in
  let save () =
    let path = Filename.concat !keep (Printf.sprintf "fir_fuzz_%d.fir" seed) in
    write path text;
    path
  in
  match Fir_parse.parse_module text with
  | exception Fir_parse.Error (l, msg) -> Broken (Printf.sprintf "parse line %d: %s (%s)" l msg (save ()))
  | m -> (
      match Fir_verify.verify m with
      | _ :: _ as errs -> Broken ("verifier: " ^ String.concat "; " errs ^ " (" ^ save () ^ ")")
      | [] -> (
          match Fir_to_ir.convert ~like m with
          | exception Fir_to_ir.Unsupported msg -> Broken ("fir -> ir: " ^ msg ^ " (" ^ save () ^ ")")
          | ir -> (
              let nexe = Filename.concat tmp "n" and lexe = Filename.concat tmp "l" in
              let ll = Filename.concat tmp "m.ll" in
              write ll (Ir.render ir);
              (* -lm: without SSE4.1, LLVM lowers floor/ceil/... to libm calls *)
              let lstatus =
                Sys.command
                  (Printf.sprintf "%s -O2 -w -x ir %s -lm -o %s 2>%s" !cc (Filename.quote ll) (Filename.quote lexe)
                     (Filename.quote (Filename.concat tmp "cc.err")))
              in
              if lstatus <> 0 then
                Broken ("clang: " ^ read (Filename.concat tmp "cc.err") ^ " (" ^ save () ^ ")")
              else
                (* the native backend gets its own copy, legalized *)
                let native () =
                  let m = Fir_parse.parse_module text in
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
                    if ls = ns && lo = no then Same
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
