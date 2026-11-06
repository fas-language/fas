(* Fir text printer. The format is defined by this file and Fir_parse, which
   mirror each other: print (parse (print m)) = print m.

   Values are printed by name when the function names them (and the name is
   not purely numeric), otherwise by a number assigned in program order, so
   the output never depends on internal ids. Constants are printed inline
   with their type: "i64 7", "f64 0x3ff0000000000000", "ptr null",
   "i32 poison", "<4 x i32> zero", "<2 x i64> [i64 1, i64 2]", "ptr @sym+8". *)

open Fir

let ibin_name = function
  | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Udiv -> "udiv" | Sdiv -> "sdiv"
  | Urem -> "urem" | Srem -> "srem" | Shl -> "shl" | Lshr -> "lshr" | Ashr -> "ashr"
  | And -> "and" | Or -> "or" | Xor -> "xor" | Smin -> "smin" | Smax -> "smax"
  | Umin -> "umin" | Umax -> "umax" | Smulh -> "smulh" | Umulh -> "umulh"
  | Sadd_sat -> "sadd_sat" | Uadd_sat -> "uadd_sat" | Ssub_sat -> "ssub_sat"
  | Usub_sat -> "usub_sat"

let iun_name = function
  | Ctz -> "ctz" | Clz -> "clz" | Popcnt -> "popcnt" | Bswap -> "bswap"
  | Bitrev -> "bitrev" | Abs -> "abs"

let fbin_name = function
  | Fadd -> "fadd" | Fsub -> "fsub" | Fmul -> "fmul" | Fdiv -> "fdiv" | Frem -> "frem"
  | Fmin -> "fmin" | Fmax -> "fmax" | Copysign -> "copysign"

let fun_name = function
  | Fneg -> "fneg" | Fabs -> "fabs" | Sqrt -> "sqrt" | Floor -> "floor" | Ceil -> "ceil"
  | Ftrunc -> "ftrunc" | Roundeven -> "roundeven"

let ovf_name = function
  | Sadd_ovf -> "sadd_ovf" | Uadd_ovf -> "uadd_ovf" | Ssub_ovf -> "ssub_ovf"
  | Usub_ovf -> "usub_ovf" | Smul_ovf -> "smul_ovf" | Umul_ovf -> "umul_ovf"

let icmp_name = function
  | Eq -> "eq" | Ne -> "ne" | Slt -> "slt" | Sle -> "sle" | Sgt -> "sgt" | Sge -> "sge"
  | Ult -> "ult" | Ule -> "ule" | Ugt -> "ugt" | Uge -> "uge"

let fcmp_name = function
  | Ffalse -> "false" | Foeq -> "oeq" | Fogt -> "ogt" | Foge -> "oge" | Folt -> "olt"
  | Fole -> "ole" | Fone -> "one" | Ford -> "ord" | Fueq -> "ueq" | Fugt -> "ugt"
  | Fuge -> "uge" | Fult -> "ult" | Fule -> "ule" | Fune -> "une" | Funo -> "uno"
  | Ftrue -> "true"

let conv_name = function
  | Trunc -> "trunc" | Zext -> "zext" | Sext -> "sext" | Fptrunc -> "fptrunc"
  | Fpext -> "fpext" | Fptosi -> "fptosi" | Fptoui -> "fptoui"
  | Fptosi_sat -> "fptosi_sat" | Fptoui_sat -> "fptoui_sat" | Sitofp -> "sitofp"
  | Uitofp -> "uitofp" | Ptrtoint -> "ptrtoint" | Inttoptr -> "inttoptr"
  | Bitcast -> "bitcast"

let ordering_name = function
  | Plain -> "plain" | Relaxed -> "relaxed" | Acquire -> "acquire" | Release -> "release"
  | Acq_rel -> "acq_rel" | Seq_cst -> "seq_cst"

let rmw_name = function
  | Xchg -> "xchg" | Radd -> "add" | Rsub -> "sub" | Rand -> "and" | Ror -> "or"
  | Rxor -> "xor" | Rmin -> "min" | Rmax -> "max" | Rumin -> "umin" | Rumax -> "umax"
  | Rfadd -> "fadd" | Rfsub -> "fsub"

let reduce_name = function
  | Red_add -> "add" | Red_mul -> "mul" | Red_and -> "and" | Red_or -> "or"
  | Red_xor -> "xor" | Red_smin -> "smin" | Red_smax -> "smax" | Red_umin -> "umin"
  | Red_umax -> "umax" | Red_fmin -> "fmin" | Red_fmax -> "fmax"
  | Red_fadd_ordered -> "fadd_ordered"

(* Instruction flags, in the order they are printed *)
let flag_names =
  [ (f_nsw, "nsw"); (f_nuw, "nuw"); (f_exact, "exact"); (f_disjoint, "disjoint");
    (f_inbounds, "inbounds"); (f_zero_poison, "zero_poison"); (f_contract, "contract");
    (f_fast, "fast"); (f_poison_min, "poison_min") ]

let call_attr_names =
  [ (a_noreturn, "noreturn"); (a_returns_twice, "returns_twice"); (a_nounwind, "nounwind");
    (a_tail, "tail") ]

let func_attr_names =
  [ (fa_inline, "inline"); (fa_noinline, "noinline"); (fa_cold, "cold");
    (fa_noreturn, "noreturn") ]

let linkage_name = function Local -> "local" | Global -> "global" | Weak -> "weak"

let visibility_name = function
  | Default -> "default" | Hidden -> "hidden" | Protected -> "protected"

(* Characters allowed in bare names; anything else is quoted *)
let bare_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' -> true
  | _ -> false

let is_numeric s = s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | ' ' .. '~' -> Buffer.add_char b c
      | c -> Buffer.add_string b (Printf.sprintf "\\%02x" (Char.code c)))
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let symbol s = if s <> "" && String.for_all bare_char s then "@" ^ s else "@" ^ quote s

(* Limbs as one hexadecimal number, most significant first *)
let hex_of_limbs (l : int64 array) =
  let n = Array.length l in
  let top = ref (n - 1) in
  while !top > 0 && l.(!top) = 0L do decr top done;
  let b = Buffer.create 20 in
  Buffer.add_string b (Printf.sprintf "0x%Lx" l.(!top));
  for i = !top - 1 downto 0 do Buffer.add_string b (Printf.sprintf "%016Lx" l.(i)) done;
  Buffer.contents b

(* Integers up to 64 bits print in signed decimal, wider ones in hex *)
let int_text ty l =
  match ty with
  | Int w when w <= 64 ->
      let x = l.(0) in
      let s = 64 - w in
      let x = if w = 64 then x else Int64.shift_right (Int64.shift_left x s) s in
      if w = 1 then Int64.to_string (Int64.logand x 1L) else Int64.to_string x
  | _ -> hex_of_limbs l

(* ---------- per-function naming ---------- *)

type names = { vnames : (value, string) Hashtbl.t; bnames : string array }

(* Assigns printable names to every value and block, in program order *)
let assign_names f =
  let used = Hashtbl.create 64 in
  let vnames = Hashtbl.create 64 in
  let counter = ref 0 in
  let fresh_named base =
    let rec go k =
      let s = if k = 0 then base else base ^ "." ^ string_of_int k in
      if Hashtbl.mem used s then go (k + 1) else s
    in
    let s = go 0 in
    Hashtbl.replace used s ();
    s
  in
  let name v =
    if not (Hashtbl.mem vnames v) then begin
      let s =
        match Hashtbl.find_opt f.names v with
        | Some n when n <> "" && not (is_numeric n) -> fresh_named n
        | _ ->
            let s = string_of_int !counter in
            incr counter;
            s
      in
      Hashtbl.replace vnames v s
    end
  in
  Vec.iter
    (fun (b : block_rec) ->
      Array.iter name b.params;
      Vec.iter (fun i -> Array.iter name (inst f i).results) b.insts)
    f.blocks;
  let busedb = Hashtbl.create 16 in
  let bnames =
    Array.init (num_blocks f) (fun k ->
        let n = (block f k).bname in
        let rec go j =
          let s =
            if n = "" || is_numeric n then "bb" ^ string_of_int (k + j)
            else if j = 0 then n
            else n ^ "." ^ string_of_int j
          in
          if Hashtbl.mem busedb s then go (j + 1) else s
        in
        let s = go 0 in
        Hashtbl.replace busedb s ();
        s)
  in
  { vnames; bnames }

let label s = if s <> "" && String.for_all bare_char s then "^" ^ s else "^" ^ quote s

(* ---------- printing ---------- *)

let rec const_text f c =
  match c with
  | Cint (ty, l) -> ty_to_string ty ^ " " ^ int_text ty l
  | Cfloat (ty, l) -> ty_to_string ty ^ " " ^ hex_of_limbs l
  | Cnull ty -> ty_to_string ty ^ " null"
  | Cpoison ty -> ty_to_string ty ^ " poison"
  | Czero ty -> ty_to_string ty ^ " zero"
  | Cvec (ty, lanes) ->
      ty_to_string ty ^ " ["
      ^ String.concat ", "
          (Array.to_list
             (Array.map
                (fun v ->
                  match const_of f v with Some c -> const_text f c | None -> "?")
                lanes))
      ^ "]"
  | Csym (s, 0L) -> "ptr " ^ symbol s
  | Csym (s, a) ->
      "ptr " ^ symbol s ^ (if Int64.compare a 0L < 0 then Int64.to_string a else "+" ^ Int64.to_string a)
  | Cblockaddr (fn, b) -> Printf.sprintf "ptr blockaddr(%s, %d)" (symbol fn) b

let value_text f nm v =
  let v = resolve f v in
  match def_of f v with
  | Const c -> const_text f c
  | _ -> (
      match Hashtbl.find_opt nm.vnames v with
      | Some s -> if String.for_all bare_char s then "%" ^ s else "%" ^ quote s
      | None -> "%?" ^ string_of_int v)

let flags_text fl =
  String.concat ""
    (List.filter_map (fun (bit, n) -> if has fl bit then Some (n ^ " ") else None) flag_names)

let mem_text m =
  let parts = ref [] in
  let add s = parts := s :: !parts in
  add (Printf.sprintf "align %d" m.align);
  if m.volatile then add "volatile";
  if m.order <> Plain then add (ordering_name m.order);
  if m.alias <> 0 then add (Printf.sprintf "!alias %d" m.alias);
  if m.noalias <> [] then
    add ("!noalias [" ^ String.concat ", " (List.map string_of_int m.noalias) ^ "]");
  String.concat " " (List.rev !parts)

let abi_text = function
  | Scalar t -> ty_to_string t
  | Ext (t, Zx) -> "zext " ^ ty_to_string t
  | Ext (t, Sx) -> "sext " ^ ty_to_string t
  | Agg { size; align; fields } ->
      Printf.sprintf "agg(%d, %d, [%s])" size align
        (String.concat ", "
           (List.map (fun (o, t) -> Printf.sprintf "%d: %s" o (ty_to_string t)) fields))

let sig_text s =
  let ps = List.map abi_text s.param_abis @ if s.variadic then [ "..." ] else [] in
  "(" ^ String.concat ", " ps ^ ") -> (" ^ String.concat ", " (List.map abi_text s.result_abis) ^ ")"

let attrs_text table bits =
  String.concat ""
    (List.filter_map (fun (bit, n) -> if bits land bit <> 0 then Some (n ^ " ") else None) table)

let results_ty_text f (ir : inst_rec) =
  match ir.results with
  | [| r |] -> ty_to_string (ty_of f r)
  | rs -> "(" ^ String.concat ", " (Array.to_list (Array.map (fun r -> ty_to_string (ty_of f r)) rs)) ^ ")"

let op_text f nm (ir : inst_rec) =
  let v = value_text f nm in
  let rt () = results_ty_text f ir in
  let list vs = String.concat ", " (Array.to_list (Array.map v vs)) in
  match ir.op with
  | Nop -> "nop"
  | Ibin (k, fl, a, b) -> Printf.sprintf "%s %s%s %s, %s" (ibin_name k) (flags_text fl) (rt ()) (v a) (v b)
  | Iun (k, fl, a) -> Printf.sprintf "%s %s%s %s" (iun_name k) (flags_text fl) (rt ()) (v a)
  | Fbin (k, fl, a, b) -> Printf.sprintf "%s %s%s %s, %s" (fbin_name k) (flags_text fl) (rt ()) (v a) (v b)
  | Fun (k, fl, a) -> Printf.sprintf "%s %s%s %s" (fun_name k) (flags_text fl) (rt ()) (v a)
  | Fma (fl, a, b, c) -> Printf.sprintf "fma %s%s %s, %s, %s" (flags_text fl) (rt ()) (v a) (v b) (v c)
  | Fsh (l, a, b, c) ->
      Printf.sprintf "%s %s %s, %s, %s" (if l then "fshl" else "fshr") (rt ()) (v a) (v b) (v c)
  | Ovf (k, a, b) -> Printf.sprintf "%s %s %s, %s" (ovf_name k) (rt ()) (v a) (v b)
  | Icmp (k, a, b) -> Printf.sprintf "icmp %s %s %s, %s" (icmp_name k) (rt ()) (v a) (v b)
  | Fcmp (k, fl, a, b) ->
      Printf.sprintf "fcmp %s %s%s %s, %s" (fcmp_name k) (flags_text fl) (rt ()) (v a) (v b)
  | Select (c, a, b) -> Printf.sprintf "select %s %s, %s, %s" (rt ()) (v c) (v a) (v b)
  | Freeze a -> Printf.sprintf "freeze %s %s" (rt ()) (v a)
  | Conv (k, a) -> Printf.sprintf "%s %s %s" (conv_name k) (rt ()) (v a)
  | Alloca (n, al) -> Printf.sprintf "alloca %s %s, align %d" (rt ()) (v n) al
  | Load (m, p) -> Printf.sprintf "load %s %s, %s" (rt ()) (v p) (mem_text m)
  | Store (m, x, p) -> Printf.sprintf "store %s, %s, %s" (v x) (v p) (mem_text m)
  | Ptradd (fl, p, i, s, d) ->
      Printf.sprintf "ptradd %s%s %s, %s, %d, %d" (flags_text fl) (rt ()) (v p) (v i) s d
  | Memcpy (vol, a, b, c) ->
      Printf.sprintf "memcpy %s%s, %s, %s" (if vol then "volatile " else "") (v a) (v b) (v c)
  | Memmove (vol, a, b, c) ->
      Printf.sprintf "memmove %s%s, %s, %s" (if vol then "volatile " else "") (v a) (v b) (v c)
  | Memset (vol, a, b, c) ->
      Printf.sprintf "memset %s%s, %s, %s" (if vol then "volatile " else "") (v a) (v b) (v c)
  | Rmw (k, m, p, x) -> Printf.sprintf "atomic_rmw %s %s %s, %s, %s" (rmw_name k) (rt ()) (v p) (v x) (mem_text m)
  | Cmpxchg c ->
      Printf.sprintf "cmpxchg %s%s %s, %s, %s, %s, fail %s" (if c.weak then "weak " else "") (rt ())
        (v c.p) (v c.expected) (v c.desired) (mem_text c.mem) (ordering_name c.fail)
  | Fence o -> "fence " ^ ordering_name o
  | Extract (a, i) -> Printf.sprintf "extract %s %s, %s" (rt ()) (v a) (v i)
  | Insert (a, x, i) -> Printf.sprintf "insert %s %s, %s, %s" (rt ()) (v a) (v x) (v i)
  | Shuffle (a, b, m) ->
      Printf.sprintf "shuffle %s %s, %s, [%s]" (rt ()) (v a) (v b)
        (String.concat ", " (Array.to_list (Array.map string_of_int m)))
  | Splat a -> Printf.sprintf "splat %s %s" (rt ()) (v a)
  | Reduce (k, a) -> Printf.sprintf "reduce %s %s %s" (reduce_name k) (rt ()) (v a)
  | Masked_load (al, p, m, pt) -> Printf.sprintf "masked_load %s %s, %s, %s, align %d" (rt ()) (v p) (v m) (v pt) al
  | Masked_store (al, x, p, m) -> Printf.sprintf "masked_store %s, %s, %s, align %d" (v x) (v p) (v m) al
  | Compress (a, m) -> Printf.sprintf "compress %s %s, %s" (rt ()) (v a) (v m)
  | Expand (a, m) -> Printf.sprintf "expand %s %s, %s" (rt ()) (v a) (v m)
  | Call { sig_; callee; args; attrs; va_aggs } ->
      let arg k x =
        match List.assoc_opt k va_aggs with Some a -> abi_text a ^ " " ^ v x | None -> v x
      in
      Printf.sprintf "call %s%s %s(%s)" (attrs_text call_attr_names attrs) (sig_text sig_)
        (match callee with Direct s -> symbol s | Indirect c -> v c)
        (String.concat ", " (List.mapi arg (Array.to_list args)))
  | Syscall args -> Printf.sprintf "syscall %s %s" (rt ()) (list args)
  | Va_start p -> "va_start " ^ v p
  | Va_arg p -> Printf.sprintf "va_arg %s %s" (rt ()) (v p)
  | Va_arg_agg (abi, p) -> Printf.sprintf "va_arg %s %s" (abi_text abi) (v p)
  | Stack_save -> "stacksave"
  | Stack_restore p -> "stackrestore " ^ v p
  | Trap -> "trap"
  | Dbg_value (var, a) -> Printf.sprintf "dbg_value %d, %s" var (v a)

let target_text f nm (t : target) =
  if t.args = [||] then label nm.bnames.(t.dest)
  else
    label nm.bnames.(t.dest) ^ "("
    ^ String.concat ", " (Array.to_list (Array.map (value_text f nm) t.args))
    ^ ")"

let term_text f nm = function
  | Jump t -> "jump " ^ target_text f nm t
  | Br (c, t, e, w) ->
      Printf.sprintf "br %s, %s, %s%s" (value_text f nm c) (target_text f nm t) (target_text f nm e)
        (if w >= 0 then Printf.sprintf ", weight %d" w else "")
  | Switch (x, d, cases) ->
      let ty = ty_of f (resolve f x) in
      Printf.sprintf "switch %s, %s, [%s]" (value_text f nm x) (target_text f nm d)
        (String.concat ", "
           (Array.to_list (Array.map (fun (k, t) -> int_text ty k ^ ": " ^ target_text f nm t) cases)))
  | Indirect_br (x, bs) ->
      Printf.sprintf "indirect_br %s, [%s]" (value_text f nm x)
        (String.concat ", " (Array.to_list (Array.map (fun b -> label nm.bnames.(b)) bs)))
  | Ret [||] -> "ret"
  | Ret vs -> "ret " ^ String.concat ", " (Array.to_list (Array.map (value_text f nm) vs))
  | Unreachable -> "unreachable"

let func_header f =
  Printf.sprintf "%s %s %s %s%s%s"
    (if f.defined then "func" else "declare")
    (symbol f.name) (sig_text f.sig_) (linkage_name f.linkage)
    (if f.visibility = Default then "" else " " ^ visibility_name f.visibility)
    ((if f.fattrs = 0 then "" else " " ^ String.trim (attrs_text func_attr_names f.fattrs))
    ^ match f.display with Some d -> " display " ^ quote d | None -> "")

let print_func buf f =
  let add = Buffer.add_string buf in
  add (func_header f);
  if not f.defined then add "\n"
  else begin
    add " {\n";
    let nm = assign_names f in
    Vec.iteri
      (fun k (b : block_rec) ->
        add (label nm.bnames.(k));
        if b.params <> [||] then begin
          add "(";
          Array.iteri
            (fun j p ->
              if j > 0 then add ", ";
              add (value_text f nm p);
              add ": ";
              add (ty_to_string (ty_of f p)))
            b.params;
          add ")"
        end;
        add ":\n";
        Vec.iter
          (fun i ->
            let ir = inst f i in
            if ir.op <> Nop then begin
              add "  ";
              if ir.results <> [||] then begin
                add (String.concat ", " (Array.to_list (Array.map (value_text f nm) ir.results)));
                add " = "
              end;
              add (op_text f nm ir);
              if ir.loc <> 0 then add (Printf.sprintf " !loc %d" ir.loc);
              add "\n"
            end)
          b.insts;
        add "  ";
        add (term_text f nm b.term);
        if b.term_loc <> 0 then add (Printf.sprintf " !loc %d" b.term_loc);
        add "\n")
      f.blocks;
    add "}\n"
  end

let piece_text = function
  | Bytes s -> "bytes " ^ quote s
  | Zeros n -> "zeros " ^ string_of_int n
  | Reloc { sym; addend; width } ->
      Printf.sprintf "reloc %s%s w%d" (symbol sym)
        (if addend = 0L then ""
         else if Int64.compare addend 0L < 0 then Int64.to_string addend
         else "+" ^ Int64.to_string addend)
        width

let print_global buf g =
  Buffer.add_string buf
    (Printf.sprintf "global %s size %d align %d %s%s%s%s%s = %s\n" (symbol g.gname) g.size g.galign
       (linkage_name g.glinkage)
       (if g.gvisibility = Default then "" else " " ^ visibility_name g.gvisibility)
       (match g.section with Some s -> " section " ^ quote s | None -> "")
       (if g.readonly then " readonly" else "")
       (if g.thread_local then " thread_local" else "")
       (if not g.gdefined then "extern"
        else
          match g.init with
          | None -> "zero"
          | Some ps -> "{" ^ String.concat ", " (List.map piece_text ps) ^ "}"))

let module_to_string m =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf (Printf.sprintf "triple %s\n" (quote m.triple));
  if not m.strict_aliasing then Buffer.add_string buf "strict_aliasing false\n";
  Vec.iteri
    (fun i (file, line, col) ->
      if i > 0 then Buffer.add_string buf (Printf.sprintf "loc %d %s %d %d\n" i (quote file) line col))
    m.locs;
  Vec.iteri
    (fun i (name, parent) ->
      if i > 0 then Buffer.add_string buf (Printf.sprintf "alias_tag %d %s %d\n" i (quote name) parent))
    m.alias_tags;
  Vec.iter (print_global buf) m.globals;
  Vec.iter
    (fun f ->
      Buffer.add_char buf '\n';
      print_func buf f)
    m.funcs;
  Buffer.contents buf

let func_to_string f =
  let buf = Buffer.create 1024 in
  print_func buf f;
  Buffer.contents buf
