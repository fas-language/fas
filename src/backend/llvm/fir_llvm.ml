(* Fir -> LLVM IR text: the LLVM path's emitter (roadmap v0.6, "one IR").

   It replaces Ir.render_bounded: the same module header, attribute groups
   (#0 "no-builtin-<f>" for every defined function that is called, #1 the
   target CPU and features), line-tables-only debug info, and a byte budget
   that debug metadata does not count against. Nothing here optimizes: each
   Fir instruction becomes the LLVM instruction or intrinsic with the same
   meaning (SPEC.md), and LLVM does the rest.

   The translations that are not one to one:
   - Values are %v<id> (their Fir id) and blocks b<n>; temporaries are %t<n>.
   - Block parameters become phis. LLVM wants one phi entry per CFG edge, and
     equal values on edges from the same block, so when a terminator sends
     different arguments to one block, the later edge goes through a
     trampoline block b<n>.e<k> holding only a branch. A block with
     parameters and no predecessor (unreachable) defines them as
     `freeze poison`.
   - [ptradd p, i, s, d] is a GEP over [s x i8] (scale 1: i8) indexed by i,
     then an i8 GEP by d; a constant index folds into one i8 GEP.
   - [smulh]/[umulh] widen to twice the width, multiply and keep the high
     half. [fmin]/[fmax] are llvm.minimumnum/maximumnum (IEEE minimumNumber);
     their reductions are chains of those, since llvm.vector.reduce.fmin
     has minnum semantics. [reduce fadd_ordered] is llvm.vector.reduce.fadd
     starting from -0.0 (no reassoc: in order). [expand] goes through an
     entry-block slot and llvm.masked.expandload.
   - Overflow arithmetic and cmpxchg give an LLVM struct; their two Fir
     results are its fields. A function or call with several results
     returns a literal struct.
   - [syscall] is inline asm (Linux x86-64 registers, as the old renderer).
   - Alias tags, noalias scopes, branch weights and dbg_value are hints and
     are dropped.

   Refused (Unsupported, never miscompiled): aggregates by value (C ABI
   lowering for LLVM is roadmap v0.7), va_arg of anything but integers up to
   64 bits, ptr, f32 and f64, 4-byte data relocations, and a blockaddr of an
   entry block. *)

open Fir

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

exception Exhausted

type target_group = { target_cpu : string; target_tune : string option; target_features : string option }

(* ---------- text helpers ---------- *)

let safe_char = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' | '-' -> true | _ -> false

(* LLVM string escapes: printable ASCII except the double quote and the
   backslash, everything else as a backslash and two hex digits *)
let escaped s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      let n = Char.code c in
      if n >= 32 && n < 127 && c <> '"' && c <> '\\' then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "\\%02X" n))
    s;
  Buffer.contents b

(* @name, quoted when it holds other characters or starts with a digit (an
   all-digit name would be a numbered global) *)
let global_name n =
  if n <> "" && String.for_all safe_char n && not (n.[0] >= '0' && n.[0] <= '9') then "@" ^ n
  else "@\"" ^ escaped n ^ "\""

let rec lty = function
  | Int n -> "i" ^ string_of_int n
  | F16 -> "half"
  | F32 -> "float"
  | F64 -> "double"
  | F80 -> "x86_fp80"
  | Ptr 0 -> "ptr"
  | Ptr k -> Printf.sprintf "ptr addrspace(%d)" k
  | Vec (n, t) -> Printf.sprintf "<%d x %s>" n (lty t)

(* overloaded-intrinsic suffix: i32, f64, p0, v4i32, ... *)
let rec sfx = function
  | Int n -> "i" ^ string_of_int n
  | F16 -> "f16"
  | F32 -> "f32"
  | F64 -> "f64"
  | F80 -> "f80"
  | Ptr k -> "p" ^ string_of_int k
  | Vec (n, t) -> Printf.sprintf "v%d%s" n (sfx t)

(* An integer constant: decimal (sign-extended) up to 64 bits, LLVM's u0x
   hex form above *)
let int_text w (l : int64 array) =
  let l0 = if Array.length l = 0 then 0L else l.(0) in
  if w = 1 then if Int64.logand l0 1L = 0L then "false" else "true"
  else if w <= 64 then
    let s = 64 - w in
    Int64.to_string (Int64.shift_right (Int64.shift_left l0 s) s)
  else begin
    let b = Buffer.create 40 in
    Buffer.add_string b "u0x";
    for k = Array.length l - 1 downto 0 do
      Buffer.add_string b (Printf.sprintf "%016LX" l.(k))
    done;
    Buffer.contents b
  end

(* Float constants, exact: f32 and f64 as LLVM's hex double (an f32 NaN as
   the bitcast of its bits, which a double cannot carry for every payload),
   f16 as 0xH, f80 as 0xK *)
let float_text t (l : int64 array) =
  let l0 = if Array.length l = 0 then 0L else l.(0) in
  match t with
  | F32 ->
      let bits = Int64.logand l0 0xffffffffL in
      if Int64.logand bits 0x7f800000L = 0x7f800000L && Int64.logand bits 0x7fffffL <> 0L then
        Printf.sprintf "bitcast (i32 %Ld to float)" bits
      else Printf.sprintf "0x%016LX" (Int64.bits_of_float (Int32.float_of_bits (Int64.to_int32 bits)))
  | F64 -> Printf.sprintf "0x%016LX" l0
  | F16 -> Printf.sprintf "0xH%04LX" (Int64.logand l0 0xffffL)
  | F80 ->
      let hi = if Array.length l > 1 then l.(1) else 0L in
      Printf.sprintf "0xK%04LX%016LX" (Int64.logand hi 0xffffL) l0
  | t -> invalid_arg ("Fir_llvm.float_text: " ^ ty_to_string t)

let ordering_text = function
  | Plain -> ""
  | Relaxed -> "monotonic"
  | Acquire -> "acquire"
  | Release -> "release"
  | Acq_rel -> "acq_rel"
  | Seq_cst -> "seq_cst"

let icmp_text = function
  | Eq -> "eq" | Ne -> "ne" | Slt -> "slt" | Sle -> "sle" | Sgt -> "sgt" | Sge -> "sge"
  | Ult -> "ult" | Ule -> "ule" | Ugt -> "ugt" | Uge -> "uge"

let fcmp_text = function
  | Ffalse -> "false" | Foeq -> "oeq" | Fogt -> "ogt" | Foge -> "oge" | Folt -> "olt"
  | Fole -> "ole" | Fone -> "one" | Ford -> "ord" | Fueq -> "ueq" | Fugt -> "ugt"
  | Fuge -> "uge" | Fult -> "ult" | Fule -> "ule" | Fune -> "une" | Funo -> "uno"
  | Ftrue -> "true"

let conv_text = function
  | Trunc -> "trunc" | Zext -> "zext" | Sext -> "sext" | Fptrunc -> "fptrunc"
  | Fpext -> "fpext" | Fptosi -> "fptosi" | Fptoui -> "fptoui" | Sitofp -> "sitofp"
  | Uitofp -> "uitofp" | Ptrtoint -> "ptrtoint" | Inttoptr -> "inttoptr"
  | Bitcast -> "bitcast"
  | Fptosi_sat | Fptoui_sat -> assert false

let rmw_text = function
  | Xchg -> "xchg" | Radd -> "add" | Rsub -> "sub" | Rand -> "and" | Ror -> "or"
  | Rxor -> "xor" | Rmin -> "min" | Rmax -> "max" | Rumin -> "umin" | Rumax -> "umax"
  | Rfadd -> "fadd" | Rfsub -> "fsub"

let abi_ty what = function
  | Scalar t -> (t, "")
  | Ext (t, Zx) -> (t, " zeroext")
  | Ext (t, Sx) -> (t, " signext")
  | Agg _ -> unsupported "%s: an aggregate by value (C ABI aggregates for LLVM come with roadmap v0.7)" what

(* The LLVM return type of a result list *)
let results_ty what abis =
  match abis with
  | [] -> ("void", "")
  | [ a ] ->
      let t, e = abi_ty what a in
      (lty t, e)
  | l -> ("{ " ^ String.concat ", " (List.map (fun a -> lty (fst (abi_ty what a))) l) ^ " }", "")

(* ---------- output ---------- *)

(* Text in [buf]; [counted] is what the budget sees (debug text is free) *)
type out = { buf : Buffer.t; mutable counted : int; budget : int }

let add o s =
  o.counted <- o.counted + String.length s;
  if o.counted > o.budget then raise Exhausted;
  Buffer.add_string o.buf s

let add_debug o s = Buffer.add_string o.buf s

(* ---------- debug metadata ---------- *)

type dbg = {
  enabled : bool;
  md : Buffer.t;  (** metadata lines, written after everything else *)
  mutable next_md : int;  (** next free !N; !0 is the compile unit *)
  files : (string, int) Hashtbl.t;
  mutable subroutine_type : int;
  locations : (int * int * int, int) Hashtbl.t;  (** line, column, scope *)
  lexical_files : (int * int, int) Hashtbl.t;  (** subprogram, file *)
}

let md_id d =
  let id = d.next_md in
  d.next_md <- id + 1;
  id

let absolute_directory path =
  match Filename.dirname path with
  | "." -> Sys.getcwd ()
  | dir when Filename.is_relative dir -> Filename.concat (Sys.getcwd ()) dir
  | dir -> dir

let md_file d path =
  match Hashtbl.find_opt d.files path with
  | Some id -> id
  | None ->
      let id = md_id d in
      Hashtbl.add d.files path id;
      Buffer.add_string d.md
        (Printf.sprintf "!%d = !DIFile(filename: \"%s\", directory: \"%s\")\n" id
           (escaped (Filename.basename path)) (escaped (absolute_directory path)));
      id

let md_location d line col scope =
  match Hashtbl.find_opt d.locations (line, col, scope) with
  | Some id -> id
  | None ->
      let id = md_id d in
      Hashtbl.add d.locations (line, col, scope) id;
      Buffer.add_string d.md
        (Printf.sprintf "!%d = !DILocation(line: %d, column: %d, scope: !%d)\n" id line col scope);
      id

(* A location in another file than its function's (a C header) needs a
   lexical-block-file scope naming that file *)
let md_scope d sp sp_file file =
  if file = sp_file then sp
  else
    let fid = md_file d file in
    match Hashtbl.find_opt d.lexical_files (sp, fid) with
    | Some id -> id
    | None ->
        let id = md_id d in
        Hashtbl.add d.lexical_files (sp, fid) id;
        Buffer.add_string d.md
          (Printf.sprintf "!%d = !DILexicalBlockFile(scope: !%d, file: !%d, discriminator: 0)\n" id sp fid);
        id

(* ---------- functions ---------- *)

type fcx = {
  m : modul;
  f : func;
  o : out;
  sym : string -> string;  (** symbol text, after the redirect *)
  intrinsics : (string, string) Hashtbl.t;  (** name -> declaration *)
  intrinsic_order : string Queue.t;
  mutable tmp : int;
  d : dbg;
  sp : (int * string) option;  (** this function's subprogram and file *)
  mutable cur : string * int * int;  (** the location of the code being emitted *)
}

let temp cx =
  let n = cx.tmp in
  cx.tmp <- n + 1;
  "%t" ^ string_of_int n

(* One instruction line, with its !dbg *)
let line cx text =
  add cx.o "  ";
  add cx.o text;
  (match cx.sp with
  | Some (sp, sp_file) ->
      let file, l, c = cx.cur in
      let scope = md_scope cx.d sp sp_file file in
      add_debug cx.o (Printf.sprintf ", !dbg !%d" (md_location cx.d l c scope))
  | None -> ());
  add cx.o "\n"

let set_loc cx loc =
  if loc <> 0 && cx.sp <> None then cx.cur <- Vec.get cx.m.locs loc

let rec const_text cx (c : const) =
  match c with
  | Cint (t, l) -> int_text (int_width t) l
  | Cfloat (t, l) -> float_text t l
  | Cnull _ -> "null"
  | Cpoison _ -> "poison"
  | Czero (Int 1) -> "false"
  | Czero (Int _) -> "0"
  | Czero (Ptr _) -> "null"
  | Czero (F16 | F32 | F64 | F80 as t) -> float_text t [| 0L; 0L |]
  | Czero (Vec _) -> "zeroinitializer"
  | Cvec (t, lanes) ->
      let e = lty (elem t) in
      "<" ^ String.concat ", " (Array.to_list (Array.map (fun x -> e ^ " " ^ v cx x) lanes)) ^ ">"
  | Csym (n, 0L) -> cx.sym n
  | Csym (n, a) -> Printf.sprintf "getelementptr (i8, ptr %s, i64 %Ld)" (cx.sym n) a
  | Cblockaddr (_, 0) -> unsupported "blockaddr of an entry block"
  | Cblockaddr (fn, b) -> Printf.sprintf "blockaddress(%s, %%b%d)" (cx.sym fn) b

(* The text of a value operand *)
and v cx x =
  let f = cx.f in
  let x = resolve f x in
  match def_of f x with
  | Const c -> const_text cx c
  | Param (0, k) -> "%a" ^ string_of_int k
  | Result _ | Param _ -> "%v" ^ string_of_int x
  | Alias _ | Unset -> unsupported "@%s: undefined value %d" f.name x

let tyv cx x = ty_of cx.f (resolve cx.f x)

(* "ty value" *)
let tv cx x = lty (tyv cx x) ^ " " ^ v cx x

let intrinsic cx name ret args =
  if not (Hashtbl.mem cx.intrinsics name) then begin
    Hashtbl.add cx.intrinsics name (Printf.sprintf "declare %s @%s(%s)" ret name (String.concat ", " args));
    Queue.add name cx.intrinsic_order
  end

(* "call RET @name(args)", declaring the intrinsic; args are values *)
let call_intrinsic cx name ret (args : (ty * string) list) =
  intrinsic cx name ret (List.map (fun (t, _) -> lty t) args);
  Printf.sprintf "call %s @%s(%s)" ret name
    (String.concat ", " (List.map (fun (t, s) -> lty t ^ " " ^ s) args))

let arg cx x = (tyv cx x, v cx x)

let is_zero_const cx x =
  match const_of cx.f x with
  | Some (Czero _) -> true
  | Some (Cint (_, l)) -> Array.for_all (( = ) 0L) l
  | _ -> false

let res (ir : inst_rec) k = "%v" ^ string_of_int ir.results.(k)
let rty cx (ir : inst_rec) k = ty_of cx.f ir.results.(k)

let mem_suffix (m : mem) = Printf.sprintf ", align %d" m.align

let flag_text fl pairs = String.concat "" (List.filter_map (fun (bit, s) -> if has fl bit then Some (" " ^ s) else None) pairs)

let fast_flags fl = flag_text fl [ (f_fast, "fast"); (f_contract, "contract") ]

(* min/max of two floats by IEEE minimumNumber / maximumNumber *)
let fminmax cx is_min t a b =
  call_intrinsic cx ((if is_min then "llvm.minimumnum." else "llvm.maximumnum.") ^ sfx t) (lty t) [ (t, a); (t, b) ]

let emit_inst cx (ir : inst_rec) =
  let f = cx.f in
  set_loc cx ir.loc;
  let r k = res ir k in
  let def text = line cx (r 0 ^ " = " ^ text) in
  match ir.op with
  | Nop | Dbg_value _ -> ()
  | Ibin (k, fl, a, b) -> (
      let t = tyv cx a in
      let plain name flags = def (Printf.sprintf "%s%s %s, %s" name flags (tv cx a) (v cx b)) in
      let intr name = def (call_intrinsic cx (name ^ "." ^ sfx t) (lty t) [ arg cx a; arg cx b ]) in
      let wrap = flag_text fl [ (f_nuw, "nuw"); (f_nsw, "nsw") ] in
      let exact = flag_text fl [ (f_exact, "exact") ] in
      match k with
      | Add -> plain "add" wrap
      | Sub -> plain "sub" wrap
      | Mul -> plain "mul" wrap
      | Shl -> plain "shl" wrap
      | Udiv -> plain "udiv" exact
      | Sdiv -> plain "sdiv" exact
      | Lshr -> plain "lshr" exact
      | Ashr -> plain "ashr" exact
      | Urem -> plain "urem" ""
      | Srem -> plain "srem" ""
      | And -> plain "and" ""
      | Or -> plain "or" (flag_text fl [ (f_disjoint, "disjoint") ])
      | Xor -> plain "xor" ""
      | Smin -> intr "llvm.smin"
      | Smax -> intr "llvm.smax"
      | Umin -> intr "llvm.umin"
      | Umax -> intr "llvm.umax"
      | Sadd_sat -> intr "llvm.sadd.sat"
      | Uadd_sat -> intr "llvm.uadd.sat"
      | Ssub_sat -> intr "llvm.ssub.sat"
      | Usub_sat -> intr "llvm.usub.sat"
      | Smulh | Umulh ->
          let w = int_width t in
          let wide = with_elem t (Int (2 * w)) in
          let ext = if k = Smulh then "sext" else "zext" in
          let xa = temp cx and xb = temp cx and p = temp cx and h = temp cx in
          line cx (Printf.sprintf "%s = %s %s to %s" xa ext (tv cx a) (lty wide));
          line cx (Printf.sprintf "%s = %s %s to %s" xb ext (tv cx b) (lty wide));
          line cx (Printf.sprintf "%s = mul %s %s, %s" p (lty wide) xa xb);
          let amount =
            match t with Vec (n, _) -> Printf.sprintf "<%s>" (String.concat ", " (List.init n (fun _ -> Printf.sprintf "i%d %d" (2 * w) w))) | _ -> string_of_int w
          in
          line cx (Printf.sprintf "%s = lshr %s %s, %s" h (lty wide) p amount);
          def (Printf.sprintf "trunc %s %s to %s" (lty wide) h (lty t)))
  | Iun (k, fl, a) -> (
      let t = tyv cx a in
      let s = sfx t and lt = lty t in
      let bit b = (Int 1, if b then "true" else "false") in
      match k with
      | Ctz -> def (call_intrinsic cx ("llvm.cttz." ^ s) lt [ arg cx a; bit (has fl f_zero_poison) ])
      | Clz -> def (call_intrinsic cx ("llvm.ctlz." ^ s) lt [ arg cx a; bit (has fl f_zero_poison) ])
      | Popcnt -> def (call_intrinsic cx ("llvm.ctpop." ^ s) lt [ arg cx a ])
      | Bswap -> def (call_intrinsic cx ("llvm.bswap." ^ s) lt [ arg cx a ])
      | Bitrev -> def (call_intrinsic cx ("llvm.bitreverse." ^ s) lt [ arg cx a ])
      | Abs -> def (call_intrinsic cx ("llvm.abs." ^ s) lt [ arg cx a; bit (has fl f_poison_min) ]))
  | Fbin (k, fl, a, b) -> (
      let t = tyv cx a in
      let plain name = def (Printf.sprintf "%s%s %s, %s" name (fast_flags fl) (tv cx a) (v cx b)) in
      match k with
      | Fadd -> plain "fadd"
      | Fsub -> plain "fsub"
      | Fmul -> plain "fmul"
      | Fdiv -> plain "fdiv"
      | Frem -> plain "frem"
      | Fmin -> def (fminmax cx true t (v cx a) (v cx b))
      | Fmax -> def (fminmax cx false t (v cx a) (v cx b))
      | Copysign -> def (call_intrinsic cx ("llvm.copysign." ^ sfx t) (lty t) [ arg cx a; arg cx b ]))
  | Fun (k, fl, a) -> (
      let t = tyv cx a in
      let intr name = def (call_intrinsic cx (name ^ "." ^ sfx t) (lty t) [ arg cx a ]) in
      match k with
      | Fneg -> def (Printf.sprintf "fneg%s %s" (fast_flags fl) (tv cx a))
      | Fabs -> intr "llvm.fabs"
      | Sqrt -> intr "llvm.sqrt"
      | Floor -> intr "llvm.floor"
      | Ceil -> intr "llvm.ceil"
      | Ftrunc -> intr "llvm.trunc"
      | Roundeven -> intr "llvm.roundeven")
  | Fma (_, a, b, c) ->
      let t = tyv cx a in
      def (call_intrinsic cx ("llvm.fma." ^ sfx t) (lty t) [ arg cx a; arg cx b; arg cx c ])
  | Fsh (left, a, b, c) ->
      let t = tyv cx a in
      def (call_intrinsic cx ((if left then "llvm.fshl." else "llvm.fshr.") ^ sfx t) (lty t) [ arg cx a; arg cx b; arg cx c ])
  | Ovf (k, a, b) ->
      let t = tyv cx a in
      let name =
        match k with
        | Sadd_ovf -> "llvm.sadd.with.overflow" | Uadd_ovf -> "llvm.uadd.with.overflow"
        | Ssub_ovf -> "llvm.ssub.with.overflow" | Usub_ovf -> "llvm.usub.with.overflow"
        | Smul_ovf -> "llvm.smul.with.overflow" | Umul_ovf -> "llvm.umul.with.overflow"
      in
      let st = Printf.sprintf "{ %s, %s }" (lty t) (lty (rty cx ir 1)) in
      let s = temp cx in
      line cx (s ^ " = " ^ call_intrinsic cx (name ^ "." ^ sfx t) st [ arg cx a; arg cx b ]);
      line cx (Printf.sprintf "%s = extractvalue %s %s, 0" (r 0) st s);
      line cx (Printf.sprintf "%s = extractvalue %s %s, 1" (r 1) st s)
  | Icmp (k, a, b) -> def (Printf.sprintf "icmp %s %s, %s" (icmp_text k) (tv cx a) (v cx b))
  | Fcmp (k, fl, a, b) -> def (Printf.sprintf "fcmp%s %s %s, %s" (fast_flags fl) (fcmp_text k) (tv cx a) (v cx b))
  | Select (c, a, b) -> def (Printf.sprintf "select %s, %s, %s" (tv cx c) (tv cx a) (tv cx b))
  | Freeze a -> def ("freeze " ^ tv cx a)
  | Conv ((Fptosi_sat | Fptoui_sat) as k, a) ->
      let st = tyv cx a and dt = rty cx ir 0 in
      let name = (if k = Fptosi_sat then "llvm.fptosi.sat." else "llvm.fptoui.sat.") ^ sfx dt ^ "." ^ sfx st in
      def (call_intrinsic cx name (lty dt) [ arg cx a ])
  | Conv (k, a) -> def (Printf.sprintf "%s %s to %s" (conv_text k) (tv cx a) (lty (rty cx ir 0)))
  | Alloca (n, al) -> (
      match int_value f n with
      | Some size when Int64.compare size 0L >= 0 -> def (Printf.sprintf "alloca [%Ld x i8], align %d" size al)
      | _ -> def (Printf.sprintf "alloca i8, %s, align %d" (tv cx n) al))
  | Load (m, p) ->
      let t = rty cx ir 0 in
      if m.order = Plain then
        def (Printf.sprintf "load%s %s, %s%s" (if m.volatile then " volatile" else "") (lty t) (tv cx p) (mem_suffix m))
      else
        def
          (Printf.sprintf "load atomic%s %s, %s %s%s" (if m.volatile then " volatile" else "") (lty t) (tv cx p)
             (ordering_text m.order) (mem_suffix m))
  | Store (m, x, p) ->
      if m.order = Plain then
        line cx (Printf.sprintf "store%s %s, %s%s" (if m.volatile then " volatile" else "") (tv cx x) (tv cx p) (mem_suffix m))
      else
        line cx
          (Printf.sprintf "store atomic%s %s, %s %s%s" (if m.volatile then " volatile" else "") (tv cx x) (tv cx p)
             (ordering_text m.order) (mem_suffix m))
  | Ptradd (fl, p, idx, scale, disp) -> (
      let ib = if has fl f_inbounds then " inbounds" else "" in
      let gep elt base off = Printf.sprintf "getelementptr%s %s, %s, %s" ib elt base off in
      let pt = lty (tyv cx p) in
      let constant_index =
        if is_zero_const cx idx then Some 0L else match int_value f idx with Some k -> Some k | None -> None
      in
      match constant_index with
      | Some k ->
          let off = Int64.add (Int64.mul k (Int64.of_int scale)) (Int64.of_int disp) in
          def (gep "i8" (tv cx p) (Printf.sprintf "i64 %Ld" off))
      | None ->
          (* p + idx * scale, then + disp *)
          let scaled =
            if scale = 0 then None
            else if scale = 1 then Some (gep "i8" (tv cx p) (tv cx idx))
            else if scale > 0 then Some (gep (Printf.sprintf "[%d x i8]" scale) (tv cx p) (tv cx idx))
            else begin
              let it = tyv cx idx in
              let wide = with_elem it (Int 64) in
              let x =
                if int_width it = 64 then v cx idx
                else begin
                  let t = temp cx in
                  line cx (Printf.sprintf "%s = %s %s to %s" t (if int_width it < 64 then "sext" else "trunc") (tv cx idx) (lty wide));
                  t
                end
              in
              let t = temp cx in
              let sc =
                match wide with Vec (n, _) -> Printf.sprintf "<%s>" (String.concat ", " (List.init n (fun _ -> Printf.sprintf "i64 %d" scale))) | _ -> string_of_int scale
              in
              line cx (Printf.sprintf "%s = mul %s %s, %s" t (lty wide) x sc);
              Some (gep "i8" (tv cx p) (lty wide ^ " " ^ t))
            end
          in
          match scaled with
          | None -> def (gep "i8" (tv cx p) (Printf.sprintf "i64 %d" disp))
          | Some text when disp = 0 -> def text
          | Some text ->
              let t = temp cx in
              line cx (t ^ " = " ^ text);
              def (gep "i8" (pt ^ " " ^ t) (Printf.sprintf "i64 %d" disp)))
  | Memcpy (vol, d, s, n) | Memmove (vol, d, s, n) ->
      let name = match ir.op with Memcpy _ -> "llvm.memcpy." | _ -> "llvm.memmove." in
      let name = name ^ sfx (tyv cx d) ^ "." ^ sfx (tyv cx s) ^ "." ^ sfx (tyv cx n) in
      line cx (call_intrinsic cx name "void" [ arg cx d; arg cx s; arg cx n; (Int 1, if vol then "true" else "false") ])
  | Memset (vol, d, b, n) ->
      let name = "llvm.memset." ^ sfx (tyv cx d) ^ "." ^ sfx (tyv cx n) in
      line cx (call_intrinsic cx name "void" [ arg cx d; arg cx b; arg cx n; (Int 1, if vol then "true" else "false") ])
  | Rmw (k, m, p, x) ->
      def
        (Printf.sprintf "atomicrmw%s %s %s, %s %s%s" (if m.volatile then " volatile" else "") (rmw_text k) (tv cx p) (tv cx x)
           (ordering_text m.order) (mem_suffix m))
  | Cmpxchg { mem = m; fail; weak; p; expected; desired } ->
      let t = tyv cx expected in
      let st = Printf.sprintf "{ %s, i1 }" (lty t) in
      let s = temp cx in
      line cx
        (Printf.sprintf "%s = cmpxchg%s%s %s, %s, %s %s %s%s" s (if weak then " weak" else "") (if m.volatile then " volatile" else "")
           (tv cx p) (tv cx expected) (tv cx desired) (ordering_text m.order) (ordering_text fail) (mem_suffix m));
      line cx (Printf.sprintf "%s = extractvalue %s %s, 0" (r 0) st s);
      line cx (Printf.sprintf "%s = extractvalue %s %s, 1" (r 1) st s)
  | Fence o -> line cx ("fence " ^ ordering_text o)
  | Extract (x, i) -> def (Printf.sprintf "extractelement %s, %s" (tv cx x) (tv cx i))
  | Insert (x, e, i) -> def (Printf.sprintf "insertelement %s, %s, %s" (tv cx x) (tv cx e) (tv cx i))
  | Shuffle (a, b, mask) ->
      let mask_text =
        Printf.sprintf "<%d x i32> <%s>" (Array.length mask)
          (String.concat ", " (Array.to_list (Array.map (fun k -> if k < 0 then "i32 poison" else Printf.sprintf "i32 %d" k) mask)))
      in
      def (Printf.sprintf "shufflevector %s, %s, %s" (tv cx a) (tv cx b) mask_text)
  | Splat x ->
      let vt = rty cx ir 0 in
      let t = temp cx in
      (* no poison operands: the front end's tests require none in its
         output (the other lanes are never read) *)
      line cx (Printf.sprintf "%s = insertelement %s zeroinitializer, %s, i64 0" t (lty vt) (tv cx x));
      def (Printf.sprintf "shufflevector %s %s, %s zeroinitializer, <%d x i32> zeroinitializer" (lty vt) t (lty vt) (lanes vt))
  | Reduce (k, x) -> (
      let vt = tyv cx x in
      let et = elem vt in
      let intr name = def (call_intrinsic cx ("llvm.vector.reduce." ^ name ^ "." ^ sfx vt) (lty et) [ arg cx x ]) in
      match k with
      | Red_add -> intr "add"
      | Red_mul -> intr "mul"
      | Red_and -> intr "and"
      | Red_or -> intr "or"
      | Red_xor -> intr "xor"
      | Red_smin -> intr "smin"
      | Red_smax -> intr "smax"
      | Red_umin -> intr "umin"
      | Red_umax -> intr "umax"
      | Red_fadd_ordered ->
          let neg_zero = float_text et [| (if et = F32 then 0x80000000L else if et = F16 then 0x8000L else if et = F80 then 0L else Int64.min_int); 0x8000L |] in
          def
            (call_intrinsic cx ("llvm.vector.reduce.fadd." ^ sfx vt) (lty et) [ (et, neg_zero); arg cx x ])
      | Red_fmin | Red_fmax ->
          let n = lanes vt in
          let lane i =
            let t = temp cx in
            line cx (Printf.sprintf "%s = extractelement %s, i64 %d" t (tv cx x) i);
            t
          in
          let acc = ref (lane 0) in
          for i = 1 to n - 1 do
            let l = lane i in
            if i = n - 1 then acc := "" ^ fminmax cx (k = Red_fmin) et !acc l
            else begin
              let t = temp cx in
              line cx (t ^ " = " ^ fminmax cx (k = Red_fmin) et !acc l);
              acc := t
            end
          done;
          if n = 1 then def (Printf.sprintf "freeze %s %s" (lty et) !acc) else def !acc)
  | Masked_load (al, p, mask, pass) ->
      let vt = rty cx ir 0 in
      def
        (call_intrinsic cx
           ("llvm.masked.load." ^ sfx vt ^ "." ^ sfx (tyv cx p))
           (lty vt)
           [ arg cx p; (Int 32, string_of_int al); arg cx mask; arg cx pass ])
  | Masked_store (al, x, p, mask) ->
      line cx
        (call_intrinsic cx
           ("llvm.masked.store." ^ sfx (tyv cx x) ^ "." ^ sfx (tyv cx p))
           "void"
           [ arg cx x; arg cx p; (Int 32, string_of_int al); arg cx mask ])
  | Compress (x, mask) ->
      let vt = tyv cx x in
      def
        (call_intrinsic cx ("llvm.experimental.vector.compress." ^ sfx vt) (lty vt)
           [ arg cx x; arg cx mask; (vt, "zeroinitializer") ])
  | Expand (x, mask) ->
      (* through the entry-block slot %s<inst> (see [expand_slots]) *)
      let vt = tyv cx x in
      let slot = "%x" ^ string_of_int ir.results.(0) in
      line cx (Printf.sprintf "store %s, ptr %s, align 1" (tv cx x) slot);
      def
        (call_intrinsic cx ("llvm.masked.expandload." ^ sfx vt) (lty vt)
           [ (ptr, slot); arg cx mask; (vt, "zeroinitializer") ])
  | Call { sig_; callee; args; attrs; va_aggs } ->
      if va_aggs <> [] then unsupported "a struct by value through `...` (C ABI aggregates for LLVM come with roadmap v0.7)";
      let what = match callee with Direct n -> "call of @" ^ n | Indirect _ -> "indirect call" in
      let ret, ret_ext = results_ty what sig_.result_abis in
      let nfixed = List.length sig_.param_abis in
      let arg_text =
        Array.to_list
          (Array.mapi
             (fun k x ->
               if k < nfixed then
                 let t, e = abi_ty what (List.nth sig_.param_abis k) in
                 lty t ^ e ^ " " ^ v cx x
               else tv cx x)
             args)
      in
      (* a variadic callee's type is spelled out; so is an indirect one's *)
      let fn_ty =
        if sig_.variadic then
          Printf.sprintf "%s (%s) " ret
            (String.concat ", " (List.map (fun a -> let t, e = abi_ty what a in lty t ^ e) sig_.param_abis @ [ "..." ]))
        else ret ^ " "
      in
      let callee_text = match callee with Direct n -> cx.sym n | Indirect p -> v cx p in
      let fattrs =
        flag_text attrs [ (a_noreturn, "noreturn"); (a_returns_twice, "returns_twice"); (a_nounwind, "nounwind") ]
      in
      let text =
        Printf.sprintf "%scall%s %s%s(%s)%s"
          (if has attrs a_tail then "tail " else "")
          ret_ext fn_ty callee_text (String.concat ", " arg_text) fattrs
      in
      (match ir.results with
      | [||] -> line cx text
      | [| _ |] -> def text
      | rs ->
          let s = temp cx in
          line cx (s ^ " = " ^ text);
          Array.iteri (fun k _ -> line cx (Printf.sprintf "%s = extractvalue %s %s, %d" (r k) ret s k)) rs)
  | Syscall args ->
      (* Linux x86-64: number in rax, arguments in rdi rsi rdx r10 r8 r9; the
         kernel clobbers rcx and r11 *)
      let registers = [ "{rax}"; "{rdi}"; "{rsi}"; "{rdx}"; "{r10}"; "{r8}"; "{r9}" ] in
      let used = List.filteri (fun n _ -> n < Array.length args) registers in
      def
        (Printf.sprintf "call i64 asm sideeffect \"syscall\", \"={rax},%s,~{rcx},~{r11},~{memory}\"(%s)"
           (String.concat "," used)
           (String.concat ", " (Array.to_list (Array.map (fun x -> "i64 " ^ v cx x) args))))
  | Va_start p -> line cx (call_intrinsic cx ("llvm.va_start." ^ sfx (tyv cx p)) "void" [ arg cx p ])
  | Va_arg p -> (
      match rty cx ir 0 with
      | (Int w) as t when w <= 64 -> def (Printf.sprintf "va_arg %s, %s" (tv cx p) (lty t))
      | (Ptr _ | F32 | F64) as t -> def (Printf.sprintf "va_arg %s, %s" (tv cx p) (lty t))
      | t -> unsupported "va_arg of %s on the LLVM path" (ty_to_string t))
  | Va_arg_agg _ -> unsupported "va_arg of a struct (C ABI aggregates for LLVM come with roadmap v0.7)"
  | Stack_save -> def (call_intrinsic cx "llvm.stacksave.p0" "ptr" [])
  | Stack_restore p -> line cx (call_intrinsic cx ("llvm.stackrestore." ^ sfx (tyv cx p)) "void" [ arg cx p ])
  | Lifetime (s, n, p) ->
      line cx
        (call_intrinsic cx
           ((if s then "llvm.lifetime.start." else "llvm.lifetime.end.") ^ sfx (tyv cx p))
           "void" [ (Int 64, string_of_int n); arg cx p ])
  | Trap -> line cx (call_intrinsic cx "llvm.trap" "void" [])

(* ---------- control flow ---------- *)

let edges = function
  | Jump t -> [ t ]
  | Br (_, t, e, _) -> [ t; e ]
  | Switch (_, d, cases) -> d :: Array.to_list (Array.map snd cases)
  | Indirect_br (_, bs) -> Array.to_list (Array.map (fun b -> target b) bs)
  | Ret _ | Unreachable -> []

let label b = "b" ^ string_of_int b

(* For each block: the label of each terminator edge (its own or a
   trampoline's) and the trampolines to emit after it; for each block with
   parameters: its incoming (arguments, predecessor label) entries *)
let plan_edges f =
  let nb = num_blocks f in
  let incoming = Array.make nb [] in
  let edge_labels = Array.make nb [||] in
  let trampolines = Array.make nb [] in
  for p = 0 to nb - 1 do
    let seen = Hashtbl.create 4 in
    let es = edges (block f p).term in
    edge_labels.(p) <-
      Array.of_list
        (List.mapi
           (fun k (t : target) ->
             let args = Array.map (resolve f) t.args in
             if (block f t.dest).params = [||] then label t.dest
             else
               match Hashtbl.find_opt seen t.dest with
               | Some a when a <> args ->
                   let tl = Printf.sprintf "b%d.e%d" p k in
                   trampolines.(p) <- (tl, t.dest) :: trampolines.(p);
                   incoming.(t.dest) <- (args, tl) :: incoming.(t.dest);
                   tl
               | _ ->
                   Hashtbl.replace seen t.dest args;
                   incoming.(t.dest) <- (args, label p) :: incoming.(t.dest);
                   label t.dest)
           es);
    trampolines.(p) <- List.rev trampolines.(p)
  done;
  (Array.map List.rev incoming, edge_labels, trampolines)

let emit_term cx b (labels : string array) =
  let br = block cx.f b in
  set_loc cx br.term_loc;
  let lab k = "label %" ^ labels.(k) in
  match br.term with
  | Jump _ -> line cx ("br " ^ lab 0)
  | Br (c, _, _, _) -> line cx (Printf.sprintf "br %s, %s, %s" (tv cx c) (lab 0) (lab 1))
  | Switch (x, _, cases) ->
      let t = tyv cx x in
      let case k (keys, _) = Printf.sprintf "%s %s, %s" (lty t) (int_text (int_width t) keys) (lab (k + 1)) in
      line cx
        (Printf.sprintf "switch %s, %s [%s]" (tv cx x) (lab 0)
           (String.concat " " (Array.to_list (Array.mapi case cases))))
  | Indirect_br (x, bs) ->
      line cx
        (Printf.sprintf "indirectbr %s, [%s]" (tv cx x)
           (String.concat ", " (Array.to_list (Array.mapi (fun k _ -> lab k) bs))))
  | Ret [||] -> line cx "ret void"
  | Ret [| x |] -> line cx ("ret " ^ tv cx x)
  | Ret xs ->
      let st = "{ " ^ String.concat ", " (Array.to_list (Array.map (fun x -> lty (tyv cx x)) xs)) ^ " }" in
      let acc = ref "poison" in
      Array.iteri
        (fun k x ->
          let t = temp cx in
          line cx (Printf.sprintf "%s = insertvalue %s %s, %s, %d" t st !acc (tv cx x) k);
          acc := t)
        xs;
      line cx (Printf.sprintf "ret %s %s" st !acc)
  | Unreachable -> line cx "unreachable"

(* ---------- module ---------- *)

let linkage_text = function Local -> "internal " | Global -> "" | Weak -> "weak "

let visibility_text linkage vis =
  if linkage = Local then ""
  else match vis with Default -> "" | Hidden -> "hidden " | Protected -> "protected "

(* The function's subprogram line: its declaration, else its first location *)
let decl_location m f =
  if f.decl_loc <> 0 then Some (Vec.get m.locs f.decl_loc)
  else
    let found = ref None in
    Vec.iter
      (fun (b : block_rec) ->
        Vec.iter (fun i -> if !found = None && (inst f i).loc <> 0 then found := Some (Vec.get m.locs (inst f i).loc)) b.insts;
        if !found = None && b.term_loc <> 0 then found := Some (Vec.get m.locs b.term_loc))
      f.blocks;
    !found

let render_bounded ?(redirect = Fun.id) ?(sanitize_address = false) ?(debug_info = false) ?target ?data_layout ~budget
    (m : modul) =
  if budget < 0 then Error "rendered LLVM text budget must not be negative"
  else
    let o = { buf = Buffer.create 65536; counted = 0; budget } in
    let sym n = global_name (redirect n) in
    let d =
      { enabled = debug_info; md = Buffer.create 1024; next_md = 1; files = Hashtbl.create 4; subroutine_type = 0;
        locations = Hashtbl.create 64; lexical_files = Hashtbl.create 4 }
    in
    let intrinsics = Hashtbl.create 16 and intrinsic_order = Queue.create () in
    let called = Hashtbl.create 64 in
    Vec.iter
      (fun f -> Vec.iter (fun (ir : inst_rec) -> match ir.op with Call { callee = Direct n; _ } -> Hashtbl.replace called n () | _ -> ()) f.insts)
      m.funcs;
    let no_builtin =
      let seen = Hashtbl.create 16 in
      Vec.fold
        (fun acc f ->
          if f.defined && Hashtbl.mem called f.name && not (Hashtbl.mem seen f.name) then begin
            Hashtbl.add seen f.name ();
            f.name :: acc
          end
          else acc)
        [] m.funcs
      |> List.rev
    in
    let data_layout =
      match data_layout with
      | Some dl -> Some dl
      | None ->
          let current = Target_layout.current () in
          if current.triple = m.triple then Some current.llvm_data_layout
          else
            List.find_map
              (fun (t : Target_layout.t) -> if t.triple = m.triple then Some t.llvm_data_layout else None)
              Target_layout.[ x86_64_linux; x86_64_windows; wasm32_wasi ]
    in
    (* a data relocation as wide as the target's pointers is a [ptr] *)
    let pointer_size =
      let current = Target_layout.current () in
      if current.triple = m.triple then current.pointer_size
      else
        Option.value ~default:8
          (List.find_map
             (fun (t : Target_layout.t) -> if t.triple = m.triple then Some t.pointer_size else None)
             Target_layout.[ x86_64_linux; x86_64_windows; wasm32_wasi ])
    in
    try
      add o "; ModuleID = 'fas'\nsource_filename = \"fas\"\n";
      Option.iter (fun dl -> add o (Printf.sprintf "target datalayout = \"%s\"\n" dl)) data_layout;
      add o (Printf.sprintf "target triple = \"%s\"\n\n" m.triple);
      (* globals *)
      Vec.iter
        (fun g ->
          let tls = if g.thread_local then "thread_local " else "" in
          let kind = if g.readonly then "constant" else "global" in
          let section = match g.section with Some s -> Printf.sprintf ", section \"%s\"" (escaped s) | None -> "" in
          let align = if g.galign > 0 then Printf.sprintf ", align %d" g.galign else "" in
          if not g.gdefined then
            add o
              (Printf.sprintf "%s = %s%s%s%s [%d x i8]%s%s\n" (sym g.gname)
                 (if g.glinkage = Weak then "extern_weak " else "external ")
                 (visibility_text g.glinkage g.gvisibility) tls kind g.size section align)
          else begin
            let linkage =
              match g.glinkage with Local -> if g.readonly then "private " else "internal " | l -> linkage_text l
            in
            let ty, init =
              match g.init with
              | None -> (Printf.sprintf "[%d x i8]" g.size, "zeroinitializer")
              | Some pieces ->
                  let len = List.fold_left (fun acc p -> acc + match p with Bytes s -> String.length s | Zeros k -> k | Reloc { width; _ } -> width | Blockref _ -> 8) 0 pieces in
                  let pieces = if len < g.size then pieces @ [ Zeros (g.size - len) ] else pieces in
                  let piece = function
                    | Bytes s -> (Printf.sprintf "[%d x i8]" (String.length s), "c\"" ^ escaped s ^ "\"")
                    | Zeros k -> (Printf.sprintf "[%d x i8]" k, "zeroinitializer")
                    | Reloc { sym = s; addend; width } when width = pointer_size ->
                        ("ptr", if addend = 0L then sym s else Printf.sprintf "getelementptr (i8, ptr %s, i64 %Ld)" (sym s) addend)
                    | Reloc { width; _ } -> unsupported "@%s: a %d-byte data relocation on the LLVM path" g.gname width
                    | Blockref { block = 0; _ } -> unsupported "@%s: blockaddr of an entry block" g.gname
                    | Blockref { fn; block } -> ("ptr", Printf.sprintf "blockaddress(%s, %%b%d)" (sym fn) block)
                  in
                  let all_zero = List.for_all (function Zeros _ -> true | Bytes s -> String.for_all (( = ) '\000') s | _ -> false) pieces in
                  if all_zero then (Printf.sprintf "[%d x i8]" g.size, "zeroinitializer")
                  else
                    match List.map piece pieces with
                    | [ (t, i) ] -> (t, i)
                    | ps ->
                        ( "<{ " ^ String.concat ", " (List.map fst ps) ^ " }>",
                          "<{ " ^ String.concat ", " (List.map (fun (t, i) -> t ^ " " ^ i) ps) ^ " }>" )
            in
            add o
              (Printf.sprintf "%s = %s%s%s%s %s %s%s%s\n" (sym g.gname) linkage (visibility_text g.glinkage g.gvisibility) tls kind
                 ty init section align)
          end)
        m.globals;
      Vec.iter
        (fun a ->
          let target = if a.addend = 0L then "ptr " ^ sym a.target else Printf.sprintf "ptr getelementptr (i8, ptr %s, i64 %Ld)" (sym a.target) a.addend in
          add o
            (Printf.sprintf "%s = %s%salias i8, %s\n" (sym a.aname) (linkage_text a.alinkage) (visibility_text a.alinkage a.avisibility) target))
        m.aliases;
      (* a defined global in a named section is part of the program even when
         nothing here names it (a registry walked between __start_s and
         __stop_s), so LLVM must not drop it *)
      let kept = Vec.fold (fun acc g -> if g.gdefined && g.section <> None then g.gname :: acc else acc) [] m.globals in
      if kept <> [] then
        add o
          (Printf.sprintf "@llvm.compiler.used = appending global [%d x ptr] [%s], section \"llvm.metadata\"\n"
             (List.length kept)
             (String.concat ", " (List.rev_map (fun n -> "ptr " ^ sym n) kept)));
      if Vec.length m.globals > 0 || Vec.length m.aliases > 0 then add o "\n";
      let attr_refs ~definition =
        (if no_builtin <> [] then " #0" else "") ^ if definition && target <> None then " #1" else ""
      in
      let fattr_text f ~definition =
        (if definition && has f.fattrs fa_noinline then " noinline"
         else if definition && has f.fattrs fa_inline then " alwaysinline"
         else "")
        ^ (if has f.fattrs fa_cold then " cold" else "")
        ^ if has f.fattrs fa_noreturn then " noreturn" else ""
      in
      let declared = Hashtbl.create 64 in
      Vec.iter
        (fun f ->
          Hashtbl.replace declared f.name ();
          let what = "@" ^ f.name in
          let ret, ret_ext = results_ty what f.sig_.result_abis in
          let ret_ext = if ret_ext = "" then "" else String.sub ret_ext 1 (String.length ret_ext - 1) ^ " " in
          if not f.defined then begin
            begin
              let params = List.map (fun a -> let t, e = abi_ty what a in lty t ^ e) f.sig_.param_abis in
              let params = if f.sig_.variadic then params @ [ "..." ] else params in
              add o
                (Printf.sprintf "declare %s%s%s %s(%s)%s%s\n" (if f.linkage = Weak then "extern_weak " else "")
                   ret_ext ret (sym f.name) (String.concat ", " params) (fattr_text f ~definition:false)
                   (attr_refs ~definition:false))
            end
          end
          else begin
            let sp =
              if not d.enabled then None
              else
                match decl_location m f with
                | None -> None
                | Some (file, line, _) ->
                    let fid = md_file d file in
                    if d.subroutine_type = 0 then begin
                      d.subroutine_type <- md_id d;
                      Buffer.add_string d.md (Printf.sprintf "!%d = !DISubroutineType(types: !{})\n" d.subroutine_type)
                    end;
                    let id = md_id d in
                    let display = Option.value f.display ~default:f.name in
                    let linkage_name = redirect f.name in
                    Buffer.add_string d.md
                      (Printf.sprintf
                         "!%d = distinct !DISubprogram(name: \"%s\"%s, scope: !%d, file: !%d, line: %d, type: !%d, scopeLine: %d, spFlags: DISPFlagDefinition, unit: !0)\n"
                         id (escaped display)
                         (if linkage_name <> display then Printf.sprintf ", linkageName: \"%s\"" (escaped linkage_name) else "")
                         fid fid line d.subroutine_type line);
                    Some (id, file, line)
            in
            let cx =
              { m; f; o; sym; intrinsics; intrinsic_order; tmp = 0; d; sp = Option.map (fun (id, file, _) -> (id, file)) sp;
                cur = (match sp with Some (_, file, line) -> (file, line, 0) | None -> ("", 0, 0)) }
            in
            let params =
              List.mapi
                (fun k a ->
                  let t, e = abi_ty what a in
                  Printf.sprintf "%s%s %%a%d" (lty t) e k)
                f.sig_.param_abis
            in
            let params = if f.sig_.variadic then params @ [ "..." ] else params in
            add o
              (Printf.sprintf "define %s%s%s%s %s(%s)%s%s%s" (linkage_text f.linkage) (visibility_text f.linkage f.visibility)
                 ret_ext ret (sym f.name) (String.concat ", " params) (fattr_text f ~definition:true)
                 (if sanitize_address then " sanitize_address" else "")
                 (attr_refs ~definition:true));
            Option.iter (fun (id, _, _) -> add_debug o (Printf.sprintf " !dbg !%d" id)) sp;
            add o " {\n";
            let incoming, edge_labels, trampolines = plan_edges f in
            for b = 0 to num_blocks f - 1 do
              let br = block f b in
              if b > 0 then add o "\n";
              add o (label b ^ ":\n");
              if b = entry then
                (* slots of [expand] *)
                Vec.iter
                  (fun (ir : inst_rec) ->
                    match ir.op with
                    | Expand (x, _) ->
                        add o (Printf.sprintf "  %%x%d = alloca [%d x i8], align 16\n" ir.results.(0) (size_of (tyv cx x)))
                    | _ -> ())
                  f.insts
              else
                Array.iteri
                  (fun k p ->
                    let t = lty (ty_of f p) in
                    match incoming.(b) with
                    | [] -> add o (Printf.sprintf "  %%v%d = freeze %s poison\n" p t)
                    | ins ->
                        add o
                          (Printf.sprintf "  %%v%d = phi %s %s\n" p t
                             (String.concat ", " (List.map (fun (args, l) -> Printf.sprintf "[ %s, %%%s ]" (v cx args.(k)) l) ins))))
                  br.params;
              Vec.iter (fun i -> emit_inst cx (inst f i)) br.insts;
              emit_term cx b edge_labels.(b);
              List.iter
                (fun (tl, dest) ->
                  add o (Printf.sprintf "\n%s:\n" tl);
                  line cx ("br label %" ^ label dest))
                trampolines.(b)
            done;
            add o "}\n"
          end)
        m.funcs;
      (* intrinsics this emitter used, unless the module declares them itself *)
      Queue.iter
        (fun name -> if not (Hashtbl.mem declared name) then add o (Hashtbl.find intrinsics name ^ "\n"))
        intrinsic_order;
      if no_builtin <> [] then
        add o
          ("attributes #0 = {"
          ^ String.concat "" (List.map (fun n -> " \"no-builtin-" ^ escaped n ^ "\"") no_builtin)
          ^ " }\n");
      Option.iter
        (fun g ->
          add o
            (Printf.sprintf "attributes #1 = { \"target-cpu\"=\"%s\"%s%s }\n" (escaped g.target_cpu)
               (match g.target_tune with Some t -> Printf.sprintf " \"tune-cpu\"=\"%s\"" (escaped t) | None -> "")
               (match g.target_features with Some t -> Printf.sprintf " \"target-features\"=\"%s\"" (escaped t) | None -> "")))
        target;
      if Buffer.length d.md > 0 then begin
        let flags = md_id d in
        let version = md_id d in
        add_debug o "!llvm.dbg.cu = !{!0}\n";
        add_debug o (Printf.sprintf "!llvm.module.flags = !{!%d, !%d}\n" flags version);
        add_debug o
          "!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: \"fas 0.6.3\", isOptimized: false, runtimeVersion: 0, emissionKind: LineTablesOnly)\n";
        add_debug o (Buffer.contents d.md);
        add_debug o (Printf.sprintf "!%d = !{i32 7, !\"Dwarf Version\", i32 5}\n" flags);
        add_debug o (Printf.sprintf "!%d = !{i32 2, !\"Debug Info Version\", i32 3}\n" version)
      end;
      Ok (Buffer.contents o.buf)
    with
    | Exhausted -> Error (Printf.sprintf "rendered LLVM text exceeds the configured limit of %d bytes" budget)
    | Unsupported msg -> Error ("LLVM backend: " ^ msg)

let render ?debug_info m =
  match render_bounded ?debug_info ~budget:max_int m with Ok s -> s | Error e -> failwith e
