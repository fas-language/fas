(* Fir text parser, the mirror of Fir_print. Errors raise [Error] with a
   line number.

   Values may be used before their definition (a block can appear before a
   block that dominates it). A use of an unknown name creates a placeholder
   value; when the definition arrives the placeholder becomes an alias of it,
   and [resolve_all] removes the aliases once the function is complete. *)

open Fir

exception Error of int * string

(* ---------- lexer ---------- *)

type tok =
  | Ident of string
  | Num of string  (** decimal (maybe negative) or 0x hex, as written *)
  | Str of string
  | Tlocal of string  (** %name *)
  | Sym of string  (** @name *)
  | Label of string  (** ^name *)
  | Bang of string  (** !name *)
  | P of string  (** punctuation: ( ) [ ] { } < > , : = * + -> ... *)
  | Eof

type lexer = { s : string; mutable pos : int; mutable line : int; mutable peeked : tok option }

let fail lx msg = raise (Error (lx.line, msg))

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' -> true
  | _ -> false

let read_quoted lx =
  (* the opening quote is at lx.pos *)
  lx.pos <- lx.pos + 1;
  let b = Buffer.create 16 in
  let n = String.length lx.s in
  let rec go () =
    if lx.pos >= n then fail lx "unterminated string";
    match lx.s.[lx.pos] with
    | '"' -> lx.pos <- lx.pos + 1
    | '\\' ->
        if lx.pos + 1 >= n then fail lx "bad escape";
        (match lx.s.[lx.pos + 1] with
        | '"' -> Buffer.add_char b '"'; lx.pos <- lx.pos + 2
        | '\\' -> Buffer.add_char b '\\'; lx.pos <- lx.pos + 2
        | _ ->
            if lx.pos + 2 >= n then fail lx "bad escape";
            let h = String.sub lx.s (lx.pos + 1) 2 in
            (match int_of_string_opt ("0x" ^ h) with
            | Some c -> Buffer.add_char b (Char.chr c)
            | None -> fail lx "bad escape");
            lx.pos <- lx.pos + 3);
        go ()
    | '\n' -> fail lx "newline in string"
    | c ->
        Buffer.add_char b c;
        lx.pos <- lx.pos + 1;
        go ()
  in
  go ();
  Buffer.contents b

let read_name lx =
  let n = String.length lx.s in
  if lx.pos < n && lx.s.[lx.pos] = '"' then read_quoted lx
  else begin
    let st = lx.pos in
    while lx.pos < n && is_ident_char lx.s.[lx.pos] do lx.pos <- lx.pos + 1 done;
    if lx.pos = st then fail lx "expected a name";
    String.sub lx.s st (lx.pos - st)
  end

let rec lex lx =
  let n = String.length lx.s in
  if lx.pos >= n then Eof
  else
    match lx.s.[lx.pos] with
    | ' ' | '\t' | '\r' ->
        lx.pos <- lx.pos + 1;
        lex lx
    | '\n' ->
        lx.pos <- lx.pos + 1;
        lx.line <- lx.line + 1;
        lex lx
    | ';' ->
        (* comment to end of line *)
        while lx.pos < n && lx.s.[lx.pos] <> '\n' do lx.pos <- lx.pos + 1 done;
        lex lx
    | '"' -> Str (read_quoted lx)
    | '%' -> lx.pos <- lx.pos + 1; Tlocal (read_name lx)
    | '@' -> lx.pos <- lx.pos + 1; Sym (read_name lx)
    | '^' -> lx.pos <- lx.pos + 1; Label (read_name lx)
    | '!' -> lx.pos <- lx.pos + 1; Bang (read_name lx)
    | '-' when lx.pos + 1 < n && lx.s.[lx.pos + 1] = '>' -> lx.pos <- lx.pos + 2; P "->"
    | '.' when lx.pos + 2 < n && lx.s.[lx.pos + 1] = '.' && lx.s.[lx.pos + 2] = '.' ->
        lx.pos <- lx.pos + 3;
        P "..."
    | '-' | '0' .. '9' ->
        let st = lx.pos in
        lx.pos <- lx.pos + 1;
        while lx.pos < n && (match lx.s.[lx.pos] with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | 'x' -> true | _ -> false) do
          lx.pos <- lx.pos + 1
        done;
        Num (String.sub lx.s st (lx.pos - st))
    | '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>' | ',' | ':' | '=' | '*' | '+' as c ->
        lx.pos <- lx.pos + 1;
        P (String.make 1 c)
    | c when is_ident_char c -> Ident (read_name lx)
    | c -> fail lx (Printf.sprintf "unexpected character %C" c)

let peek lx =
  match lx.peeked with
  | Some t -> t
  | None ->
      let t = lex lx in
      lx.peeked <- Some t;
      t

let next lx =
  let t = peek lx in
  lx.peeked <- None;
  t

let tok_text = function
  | Ident s -> s | Num s -> s | Str s -> "\"" ^ s ^ "\"" | Tlocal s -> "%" ^ s
  | Sym s -> "@" ^ s | Label s -> "^" ^ s | Bang s -> "!" ^ s | P s -> s | Eof -> "end of input"

let expect lx p =
  match next lx with
  | P q when q = p -> ()
  | t -> fail lx (Printf.sprintf "expected '%s', got '%s'" p (tok_text t))

let expect_ident lx w =
  match next lx with
  | Ident q when q = w -> ()
  | t -> fail lx (Printf.sprintf "expected '%s', got '%s'" w (tok_text t))

let accept lx p = match peek lx with P q when q = p -> ignore (next lx); true | _ -> false
let accept_ident lx w = match peek lx with Ident q when q = w -> ignore (next lx); true | _ -> false

let int_tok lx =
  match next lx with
  | Num s -> (match int_of_string_opt s with Some n -> n | None -> fail lx ("bad integer " ^ s))
  | t -> fail lx ("expected an integer, got " ^ tok_text t)

let int64_tok lx =
  match next lx with
  | Num s -> (match Int64.of_string_opt s with Some n -> n | None -> fail lx ("bad integer " ^ s))
  | t -> fail lx ("expected an integer, got " ^ tok_text t)

let str_tok lx = match next lx with Str s -> s | t -> fail lx ("expected a string, got " ^ tok_text t)

(* Parses an integer literal of any size into limbs: "-5", "42", "0x1234..." *)
let limbs_of_text lx width s =
  let n = limbs_for width in
  if String.length s > 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') then begin
    let hex = String.sub s 2 (String.length s - 2) in
    let a = Array.make (max n ((String.length hex + 15) / 16)) 0L in
    let len = String.length hex in
    for k = 0 to Array.length a - 1 do
      let hi = len - (16 * k) in
      if hi > 0 then begin
        let lo = max 0 (hi - 16) in
        a.(k) <- Int64.of_string ("0x" ^ String.sub hex lo (hi - lo))
      end
    done;
    normalize_limbs width a
  end
  else
    match Int64.of_string_opt s with
    | Some x -> limbs_of_int64 width x
    | None -> (
        (* unsigned decimal up to 2^64 - 1 *)
        match Int64.of_string_opt ("0u" ^ s) with
        | Some x -> normalize_limbs width [| x |]
        | None -> fail lx ("bad integer " ^ s))

(* ---------- names ---------- *)

let parse_ordering lx = function
  | "relaxed" -> Relaxed | "acquire" -> Acquire | "release" -> Release
  | "acq_rel" -> Acq_rel | "seq_cst" -> Seq_cst | "plain" -> Plain
  | s -> fail lx ("unknown ordering " ^ s)

let is_ordering = function
  | "relaxed" | "acquire" | "release" | "acq_rel" | "seq_cst" | "plain" -> true
  | _ -> false

let table_find names s = List.find_map (fun (bit, n) -> if n = s then Some bit else None) names

let ibin_of = function
  | "add" -> Some Add | "sub" -> Some Sub | "mul" -> Some Mul | "udiv" -> Some Udiv
  | "sdiv" -> Some Sdiv | "urem" -> Some Urem | "srem" -> Some Srem | "shl" -> Some Shl
  | "lshr" -> Some Lshr | "ashr" -> Some Ashr | "and" -> Some And | "or" -> Some Or
  | "xor" -> Some Xor | "smin" -> Some Smin | "smax" -> Some Smax | "umin" -> Some Umin
  | "umax" -> Some Umax | "smulh" -> Some Smulh | "umulh" -> Some Umulh
  | "sadd_sat" -> Some Sadd_sat | "uadd_sat" -> Some Uadd_sat | "ssub_sat" -> Some Ssub_sat
  | "usub_sat" -> Some Usub_sat | _ -> None

let iun_of = function
  | "ctz" -> Some Ctz | "clz" -> Some Clz | "popcnt" -> Some Popcnt | "bswap" -> Some Bswap
  | "bitrev" -> Some Bitrev | "abs" -> Some Abs | _ -> None

let fbin_of = function
  | "fadd" -> Some Fadd | "fsub" -> Some Fsub | "fmul" -> Some Fmul | "fdiv" -> Some Fdiv
  | "frem" -> Some Frem | "fmin" -> Some Fmin | "fmax" -> Some Fmax
  | "copysign" -> Some Copysign | _ -> None

let fun_of = function
  | "fneg" -> Some Fneg | "fabs" -> Some Fabs | "sqrt" -> Some Sqrt | "floor" -> Some Floor
  | "ceil" -> Some Ceil | "ftrunc" -> Some Ftrunc | "roundeven" -> Some Roundeven | _ -> None

let ovf_of = function
  | "sadd_ovf" -> Some Sadd_ovf | "uadd_ovf" -> Some Uadd_ovf | "ssub_ovf" -> Some Ssub_ovf
  | "usub_ovf" -> Some Usub_ovf | "smul_ovf" -> Some Smul_ovf | "umul_ovf" -> Some Umul_ovf
  | _ -> None

let conv_of = function
  | "trunc" -> Some Trunc | "zext" -> Some Zext | "sext" -> Some Sext
  | "fptrunc" -> Some Fptrunc | "fpext" -> Some Fpext | "fptosi" -> Some Fptosi
  | "fptoui" -> Some Fptoui | "fptosi_sat" -> Some Fptosi_sat | "fptoui_sat" -> Some Fptoui_sat
  | "sitofp" -> Some Sitofp | "uitofp" -> Some Uitofp | "ptrtoint" -> Some Ptrtoint
  | "inttoptr" -> Some Inttoptr | "bitcast" -> Some Bitcast | _ -> None

let icmp_of lx = function
  | "eq" -> Eq | "ne" -> Ne | "slt" -> Slt | "sle" -> Sle | "sgt" -> Sgt | "sge" -> Sge
  | "ult" -> Ult | "ule" -> Ule | "ugt" -> Ugt | "uge" -> Uge
  | s -> fail lx ("unknown icmp predicate " ^ s)

let fcmp_of lx = function
  | "false" -> Ffalse | "oeq" -> Foeq | "ogt" -> Fogt | "oge" -> Foge | "olt" -> Folt
  | "ole" -> Fole | "one" -> Fone | "ord" -> Ford | "ueq" -> Fueq | "ugt" -> Fugt
  | "uge" -> Fuge | "ult" -> Fult | "ule" -> Fule | "une" -> Fune | "uno" -> Funo
  | "true" -> Ftrue | s -> fail lx ("unknown fcmp predicate " ^ s)

let rmw_of lx = function
  | "xchg" -> Xchg | "add" -> Radd | "sub" -> Rsub | "and" -> Rand | "or" -> Ror
  | "xor" -> Rxor | "min" -> Rmin | "max" -> Rmax | "umin" -> Rumin | "umax" -> Rumax
  | "fadd" -> Rfadd | "fsub" -> Rfsub | s -> fail lx ("unknown atomic_rmw op " ^ s)

let reduce_of lx = function
  | "add" -> Red_add | "mul" -> Red_mul | "and" -> Red_and | "or" -> Red_or
  | "xor" -> Red_xor | "smin" -> Red_smin | "smax" -> Red_smax | "umin" -> Red_umin
  | "umax" -> Red_umax | "fmin" -> Red_fmin | "fmax" -> Red_fmax
  | "fadd_ordered" -> Red_fadd_ordered | s -> fail lx ("unknown reduction " ^ s)

let linkage_of = function
  | "local" -> Some Local | "global" -> Some Global | "weak" -> Some Weak | _ -> None

let visibility_of = function
  | "default" -> Some Default | "hidden" -> Some Hidden | "protected" -> Some Protected
  | _ -> None

(* ---------- types ---------- *)

let rec parse_ty lx =
  match next lx with
  | Ident "f16" -> F16
  | Ident "f32" -> F32
  | Ident "f64" -> F64
  | Ident "f80" -> F80
  | Ident "ptr" ->
      if accept lx "(" then begin
        let k = int_tok lx in
        expect lx ")";
        Ptr k
      end
      else Ptr 0
  | Ident s when String.length s > 1 && s.[0] = 'i' -> (
      match int_of_string_opt (String.sub s 1 (String.length s - 1)) with
      | Some n when n >= 1 && n <= max_int_width -> Int n
      | _ -> fail lx ("bad type " ^ s))
  | P "<" ->
      let n = int_tok lx in
      expect_ident lx "x";
      let t = parse_ty lx in
      expect lx ">";
      Vec (n, t)
  | t -> fail lx ("expected a type, got " ^ tok_text t)

let starts_ty = function
  | Ident ("f16" | "f32" | "f64" | "f80" | "ptr") -> true
  | Ident s -> String.length s > 1 && s.[0] = 'i' && Option.is_some (int_of_string_opt (String.sub s 1 (String.length s - 1)))
  | P "<" -> true
  | _ -> false

let parse_abi lx =
  match peek lx with
  | Ident "zext" -> ignore (next lx); Ext (parse_ty lx, Zx)
  | Ident "sext" -> ignore (next lx); Ext (parse_ty lx, Sx)
  | Ident "agg" ->
      ignore (next lx);
      expect lx "(";
      let size = int_tok lx in
      expect lx ",";
      let align = int_tok lx in
      expect lx ",";
      expect lx "[";
      let fields = ref [] in
      if not (accept lx "]") then begin
        let rec go () =
          let o = int_tok lx in
          expect lx ":";
          let t = parse_ty lx in
          fields := (o, t) :: !fields;
          if accept lx "," then go () else expect lx "]"
        in
        go ()
      end;
      expect lx ")";
      Agg { size; align; fields = List.rev !fields }
  | _ -> Scalar (parse_ty lx)

let parse_sig lx =
  expect lx "(";
  let params = ref [] and variadic = ref false in
  if not (accept lx ")") then begin
    let rec go () =
      if accept lx "..." then variadic := true else params := parse_abi lx :: !params;
      if accept lx "," then go () else expect lx ")"
    in
    go ()
  end;
  expect lx "->";
  expect lx "(";
  let results = ref [] in
  if not (accept lx ")") then begin
    let rec go () =
      results := parse_abi lx :: !results;
      if accept lx "," then go () else expect lx ")"
    in
    go ()
  end;
  { param_abis = List.rev !params; result_abis = List.rev !results; variadic = !variadic }

(* ---------- function bodies ---------- *)

type fstate = {
  f : func;
  lx : lexer;
  vals : (string, value) Hashtbl.t;  (** names seen so far *)
  defined : (string, unit) Hashtbl.t;  (** names with a definition *)
  blocks : (string, block) Hashtbl.t;
  bdefined : (string, unit) Hashtbl.t;
  mutable pending_keys : string array option;
      (** case keys of the switch just parsed, as text: they are converted
          once the switch value's type is known *)
}

let block_ref st name =
  match Hashtbl.find_opt st.blocks name with
  | Some b -> b
  | None ->
      let b = add_block ~name st.f in
      Hashtbl.replace st.blocks name b;
      b

(* A use of %name: the value, or a placeholder until it is defined *)
let use_name st name =
  match Hashtbl.find_opt st.vals name with
  | Some v -> v
  | None ->
      let v = new_value st.f Unset i1 in
      Hashtbl.replace st.vals name v;
      v

(* Binds %name to the newly defined value [v] *)
let define_name st name v =
  if Hashtbl.mem st.defined name then fail st.lx ("%" ^ name ^ " defined twice");
  Hashtbl.replace st.defined name ();
  (match Hashtbl.find_opt st.vals name with
  | Some placeholder -> Vec.set st.f.defs placeholder (Alias v)
  | None -> ());
  Hashtbl.replace st.vals name v;
  if not (Fir_print.is_numeric name) then Hashtbl.replace st.f.names v name

let rec parse_const_after_ty st ty =
  let lx = st.lx in
  let f = st.f in
  match peek lx with
  | Ident "null" -> ignore (next lx); null f ty
  | Ident "poison" -> ignore (next lx); poison f ty
  | Ident "zero" -> ignore (next lx); zero f ty
  | Sym s ->
      ignore (next lx);
      let addend =
        match peek lx with
        | P "+" -> ignore (next lx); int64_tok lx
        | Num n when String.length n > 0 && n.[0] = '-' -> int64_tok lx
        | _ -> 0L
      in
      const f (Csym (s, addend)) ty
  | Ident "blockaddr" ->
      ignore (next lx);
      expect lx "(";
      let fn = match next lx with Sym s -> s | t -> fail lx ("expected a symbol, got " ^ tok_text t) in
      expect lx ",";
      let b = int_tok lx in
      expect lx ")";
      const f (Cblockaddr (fn, b)) ty
  | P "[" ->
      ignore (next lx);
      let lanes = ref [] in
      if not (accept lx "]") then begin
        let rec go () =
          let lty = parse_ty lx in
          lanes := parse_const_after_ty st lty :: !lanes;
          if accept lx "," then go () else expect lx "]"
        in
        go ()
      end;
      vconst f ty (Array.of_list (List.rev !lanes))
  | Num s -> (
      ignore (next lx);
      match ty with
      | Int w -> const f (Cint (ty, limbs_of_text lx w s)) ty
      | F16 | F32 | F64 | F80 ->
          let bits = match ty with F80 -> 80 | t -> 8 * size_of t in
          fconst_bits f ty (limbs_of_text lx bits s)
      | _ -> fail lx "integer literal for a non-numeric type")
  | t -> fail lx ("expected a constant, got " ^ tok_text t)

let parse_value st =
  match peek st.lx with
  | Tlocal name -> ignore (next st.lx); use_name st name
  | _ ->
      let ty = parse_ty st.lx in
      parse_const_after_ty st ty

let parse_values_until st close =
  let vs = ref [] in
  if not (accept st.lx close) then begin
    let rec go () =
      vs := parse_value st :: !vs;
      if accept st.lx "," then go () else expect st.lx close
    in
    go ()
  end;
  Array.of_list (List.rev !vs)

let parse_flags lx =
  let fl = ref 0 in
  let rec go () =
    match peek lx with
    | Ident s -> (
        match table_find Fir_print.flag_names s with
        | Some bit -> ignore (next lx); fl := !fl lor bit; go ()
        | None -> ())
    | _ -> ()
  in
  go ();
  !fl

let parse_mem lx =
  expect_ident lx "align";
  let align = int_tok lx in
  let volatile = ref false and order = ref Plain and alias = ref 0 and noalias = ref [] in
  let rec go () =
    match peek lx with
    | Ident "volatile" -> ignore (next lx); volatile := true; go ()
    | Ident s when is_ordering s -> ignore (next lx); order := parse_ordering lx s; go ()
    | Bang "alias" -> ignore (next lx); alias := int_tok lx; go ()
    | Bang "noalias" ->
        ignore (next lx);
        expect lx "[";
        let l = ref [] in
        if not (accept lx "]") then begin
          let rec g () =
            l := int_tok lx :: !l;
            if accept lx "," then g () else expect lx "]"
          in
          g ()
        end;
        noalias := List.rev !l;
        go ()
    | _ -> ()
  in
  go ();
  { align; volatile = !volatile; order = !order; alias = !alias; noalias = !noalias }

(* Result types: "T" or "(T, U, ...)" *)
let parse_result_tys lx =
  if accept lx "(" then begin
    let ts = ref [] in
    if not (accept lx ")") then begin
      let rec go () =
        ts := parse_ty lx :: !ts;
        if accept lx "," then go () else expect lx ")"
      in
      go ()
    end;
    Array.of_list (List.rev !ts)
  end
  else [| parse_ty lx |]

let parse_target st =
  let lx = st.lx in
  let name = match next lx with Label s -> s | t -> fail lx ("expected a block label, got " ^ tok_text t) in
  let dest = block_ref st name in
  let args = if accept lx "(" then parse_values_until st ")" else [||] in
  { dest; args }

(* Parses the op after "name" (already consumed); returns the op and the
   result types *)
let parse_op st opname =
  let lx = st.lx in
  let v () = parse_value st in
  let comma () = expect lx "," in
  let two () = let a = v () in comma (); let b = v () in (a, b) in
  let three () = let a = v () in comma (); let b = v () in comma (); let c = v () in (a, b, c) in
  match opname with
  | s when Option.is_some (ibin_of s) ->
      let fl = parse_flags lx in
      let t = parse_ty lx in
      let a, b = two () in
      (Ibin (Option.get (ibin_of s), fl, a, b), [| t |])
  | s when Option.is_some (iun_of s) ->
      let fl = parse_flags lx in
      let t = parse_ty lx in
      (Iun (Option.get (iun_of s), fl, v ()), [| t |])
  | s when Option.is_some (fbin_of s) ->
      let fl = parse_flags lx in
      let t = parse_ty lx in
      let a, b = two () in
      (Fbin (Option.get (fbin_of s), fl, a, b), [| t |])
  | s when Option.is_some (fun_of s) ->
      let fl = parse_flags lx in
      let t = parse_ty lx in
      (Fun (Option.get (fun_of s), fl, v ()), [| t |])
  | s when Option.is_some (ovf_of s) ->
      let ts = parse_result_tys lx in
      let a, b = two () in
      (Ovf (Option.get (ovf_of s), a, b), ts)
  | s when Option.is_some (conv_of s) ->
      let t = parse_ty lx in
      (Conv (Option.get (conv_of s), v ()), [| t |])
  | "fma" ->
      let fl = parse_flags lx in
      let t = parse_ty lx in
      let a, b, c = three () in
      (Fma (fl, a, b, c), [| t |])
  | "fshl" | "fshr" ->
      let t = parse_ty lx in
      let a, b, c = three () in
      (Fsh (opname = "fshl", a, b, c), [| t |])
  | "icmp" ->
      let p = match next lx with Ident s -> icmp_of lx s | t -> fail lx ("bad predicate " ^ tok_text t) in
      let t = parse_ty lx in
      let a, b = two () in
      (Icmp (p, a, b), [| t |])
  | "fcmp" ->
      let p = match next lx with Ident s -> fcmp_of lx s | t -> fail lx ("bad predicate " ^ tok_text t) in
      let fl = parse_flags lx in
      let t = parse_ty lx in
      let a, b = two () in
      (Fcmp (p, fl, a, b), [| t |])
  | "select" ->
      let t = parse_ty lx in
      let c, a, b = three () in
      (Select (c, a, b), [| t |])
  | "freeze" ->
      let t = parse_ty lx in
      (Freeze (v ()), [| t |])
  | "alloca" ->
      let t = parse_ty lx in
      let n = v () in
      comma ();
      expect_ident lx "align";
      (Alloca (n, int_tok lx), [| t |])
  | "load" ->
      let t = parse_ty lx in
      let p = v () in
      comma ();
      (Load (parse_mem lx, p), [| t |])
  | "store" ->
      let x, p = two () in
      comma ();
      (Store (parse_mem lx, x, p), [||])
  | "ptradd" ->
      let fl = parse_flags lx in
      let t = parse_ty lx in
      let p, i = two () in
      comma ();
      let s = int_tok lx in
      comma ();
      let d = int_tok lx in
      (Ptradd (fl, p, i, s, d), [| t |])
  | "memcpy" | "memmove" | "memset" ->
      let vol = accept_ident lx "volatile" in
      let a, b, c = three () in
      ( (match opname with
        | "memcpy" -> Memcpy (vol, a, b, c)
        | "memmove" -> Memmove (vol, a, b, c)
        | _ -> Memset (vol, a, b, c)),
        [||] )
  | "atomic_rmw" ->
      let k = match next lx with Ident s -> rmw_of lx s | t -> fail lx ("bad rmw op " ^ tok_text t) in
      let t = parse_ty lx in
      let p, x = two () in
      comma ();
      (Rmw (k, parse_mem lx, p, x), [| t |])
  | "cmpxchg" ->
      let weak = accept_ident lx "weak" in
      let ts = parse_result_tys lx in
      let p, expected, desired = three () in
      comma ();
      let mem = parse_mem lx in
      comma ();
      expect_ident lx "fail";
      let fail_o = match next lx with Ident s -> parse_ordering lx s | t -> fail lx ("bad ordering " ^ tok_text t) in
      (Cmpxchg { mem; fail = fail_o; weak; p; expected; desired }, ts)
  | "fence" -> (
      match next lx with
      | Ident s -> (Fence (parse_ordering lx s), [||])
      | t -> fail lx ("bad ordering " ^ tok_text t))
  | "extract" ->
      let t = parse_ty lx in
      let a, i = two () in
      (Extract (a, i), [| t |])
  | "insert" ->
      let t = parse_ty lx in
      let a, x, i = three () in
      (Insert (a, x, i), [| t |])
  | "shuffle" ->
      let t = parse_ty lx in
      let a, b = two () in
      comma ();
      expect lx "[";
      let m = ref [] in
      if not (accept lx "]") then begin
        let rec go () =
          m := int_tok lx :: !m;
          if accept lx "," then go () else expect lx "]"
        in
        go ()
      end;
      (Shuffle (a, b, Array.of_list (List.rev !m)), [| t |])
  | "splat" ->
      let t = parse_ty lx in
      (Splat (v ()), [| t |])
  | "reduce" ->
      let k = match next lx with Ident s -> reduce_of lx s | t -> fail lx ("bad reduction " ^ tok_text t) in
      let t = parse_ty lx in
      (Reduce (k, v ()), [| t |])
  | "masked_load" ->
      let t = parse_ty lx in
      let p, m, pt = three () in
      comma ();
      expect_ident lx "align";
      (Masked_load (int_tok lx, p, m, pt), [| t |])
  | "masked_store" ->
      let x, p, m = three () in
      comma ();
      expect_ident lx "align";
      (Masked_store (int_tok lx, x, p, m), [||])
  | "compress" | "expand" ->
      let t = parse_ty lx in
      let a, m = two () in
      ((if opname = "compress" then Compress (a, m) else Expand (a, m)), [| t |])
  | "call" ->
      let attrs = ref 0 in
      let rec go () =
        match peek lx with
        | Ident s -> (
            match table_find Fir_print.call_attr_names s with
            | Some bit -> ignore (next lx); attrs := !attrs lor bit; go ()
            | None -> ())
        | _ -> ()
      in
      go ();
      let sig_ = parse_sig lx in
      let callee =
        match next lx with
        | Sym s -> Direct s
        | Tlocal n -> Indirect (use_name st n)
        | t -> fail lx ("expected a callee, got " ^ tok_text t)
      in
      expect lx "(";
      (* an argument may carry an agg ABI: a struct by value through `...` *)
      let args = ref [] and va_aggs = ref [] and k = ref 0 in
      if not (accept lx ")") then begin
        let rec go () =
          (match peek lx with
          | Ident "agg" -> va_aggs := (!k, parse_abi lx) :: !va_aggs
          | _ -> ());
          args := parse_value st :: !args;
          incr k;
          if accept lx "," then go () else expect lx ")"
        in
        go ()
      end;
      let args = Array.of_list (List.rev !args) and va_aggs = List.rev !va_aggs in
      let tys =
        Array.of_list
          (List.map (function Scalar t | Ext (t, _) -> t | Agg _ -> ptr) sig_.result_abis)
      in
      (Call { sig_; callee; args; attrs = !attrs; va_aggs }, tys)
  | "syscall" ->
      let t = parse_ty lx in
      let args = ref [] in
      let rec go () =
        args := parse_value st :: !args;
        if accept lx "," then go ()
      in
      go ();
      (Syscall (Array.of_list (List.rev !args)), [| t |])
  | "va_start" -> (Va_start (parse_value st), [||])
  | "va_arg" -> (
      match peek lx with
      | Ident "agg" ->
          let abi = parse_abi lx in
          (Va_arg_agg (abi, parse_value st), [| ptr |])
      | _ ->
          let t = parse_ty lx in
          (Va_arg (parse_value st), [| t |]))
  | "stacksave" -> (Stack_save, [| ptr |])
  | "stackrestore" -> (Stack_restore (parse_value st), [||])
  | "trap" -> (Trap, [||])
  | "nop" -> (Nop, [||])
  | "dbg_value" ->
      let var = int_tok lx in
      comma ();
      (Dbg_value (var, v ()), [||])
  | s -> fail lx ("unknown instruction " ^ s)

let parse_term st word =
  let lx = st.lx in
  match word with
  | "jump" -> Jump (parse_target st)
  | "br" ->
      let c = parse_value st in
      expect lx ",";
      let t = parse_target st in
      expect lx ",";
      let e = parse_target st in
      let w =
        if accept lx "," then begin
          expect_ident lx "weight";
          int_tok lx
        end
        else -1
      in
      Br (c, t, e, w)
  | "switch" ->
      let x = parse_value st in
      expect lx ",";
      let d = parse_target st in
      expect lx ",";
      expect lx "[";
      let cases = ref [] in
      if not (accept lx "]") then begin
        let rec go () =
          let k = match next lx with Num s -> s | t -> fail lx ("expected a case key, got " ^ tok_text t) in
          expect lx ":";
          let t = parse_target st in
          cases := (k, t) :: !cases;
          if accept lx "," then go () else expect lx "]"
        in
        go ()
      end;
      let cases = Array.of_list (List.rev !cases) in
      st.pending_keys <- Some (Array.map fst cases);
      Switch (x, d, Array.map (fun (_, t) -> ([||], t)) cases)
  | "indirect_br" ->
      let x = parse_value st in
      expect lx ",";
      expect lx "[";
      let bs = ref [] in
      if not (accept lx "]") then begin
        let rec go () =
          (match next lx with
          | Label s -> bs := block_ref st s :: !bs
          | t -> fail lx ("expected a label, got " ^ tok_text t));
          if accept lx "," then go () else expect lx "]"
        in
        go ()
      end;
      Indirect_br (x, Array.of_list (List.rev !bs))
  | "ret" ->
      (* "ret" alone returns nothing; otherwise a value list follows on the
         same logical statement, which ends at the next label or "}" *)
      let vs = ref [] in
      let rec go () =
        vs := parse_value st :: !vs;
        if accept lx "," then go ()
      in
      (match peek lx with Label _ | P "}" | Bang _ -> () | _ -> go ());
      Ret (Array.of_list (List.rev !vs))
  | "unreachable" -> Unreachable
  | _ -> fail lx ("unknown terminator " ^ word)

let is_term = function
  | "jump" | "br" | "switch" | "indirect_br" | "ret" | "unreachable" -> true
  | _ -> false

(* Parses "{ blocks }" into [f] *)
let parse_body lx f =
  let st =
    { f; lx; vals = Hashtbl.create 64; defined = Hashtbl.create 64; blocks = Hashtbl.create 16;
      bdefined = Hashtbl.create 16; pending_keys = None }
  in
  let switch_fixups = ref [] in
  expect lx "{";
  let order = ref [] in
  let rec blocks () =
    match next lx with
    | P "}" -> ()
    | Label name ->
        if Hashtbl.mem st.bdefined name then fail lx ("block ^" ^ name ^ " defined twice");
        Hashtbl.replace st.bdefined name ();
        let b = block_ref st name in
        order := b :: !order;
        if accept lx "(" then begin
          if not (accept lx ")") then begin
            let rec go () =
              let pname = match next lx with Tlocal s -> s | t -> fail lx ("expected a parameter, got " ^ tok_text t) in
              expect lx ":";
              let ty = parse_ty lx in
              let p = add_param f b ty in
              define_name st pname p;
              if accept lx "," then go () else expect lx ")"
            in
            go ()
          end
        end;
        expect lx ":";
        let rec insts () =
          match next lx with
          | Ident w when is_term w ->
              let t = parse_term st w in
              (match st.pending_keys with
              | Some raw ->
                  switch_fixups := (b, raw) :: !switch_fixups;
                  st.pending_keys <- None
              | None -> ());
              let loc = if (match peek lx with Bang "loc" -> true | _ -> false) then (ignore (next lx); int_tok lx) else 0 in
              set_term ~loc f b t
          | Tlocal _ as first_tok ->
              (* results: %a, %b = op ... *)
              let names = ref [ (match first_tok with Tlocal s -> s | _ -> assert false) ] in
              while accept lx "," do
                match next lx with
                | Tlocal s -> names := s :: !names
                | t -> fail lx ("expected a result name, got " ^ tok_text t)
              done;
              expect lx "=";
              let opname = match next lx with Ident s -> s | t -> fail lx ("expected an instruction, got " ^ tok_text t) in
              let op, tys = parse_op st opname in
              let names = Array.of_list (List.rev !names) in
              if Array.length names <> Array.length tys then
                fail lx (Printf.sprintf "%s defines %d values, %d named" opname (Array.length tys) (Array.length names));
              let loc = if (match peek lx with Bang "loc" -> true | _ -> false) then (ignore (next lx); int_tok lx) else 0 in
              let rs = emit ~loc f b op tys in
              Array.iteri (fun k n -> define_name st n rs.(k)) names;
              insts ()
          | Ident opname ->
              let op, tys = parse_op st opname in
              if tys <> [||] then
                (match op with
                | Call _ | Syscall _ | Rmw _ | Cmpxchg _ -> ()
                | _ -> fail lx (opname ^ " defines a value that is not named"));
              let loc = if (match peek lx with Bang "loc" -> true | _ -> false) then (ignore (next lx); int_tok lx) else 0 in
              ignore (emit ~loc f b op tys);
              insts ()
          | t -> fail lx ("expected an instruction, got " ^ tok_text t)
        in
        insts ();
        blocks ()
    | t -> fail lx ("expected a block label or '}', got " ^ tok_text t)
  in
  blocks ();
  (* every referenced block and value must be defined *)
  Hashtbl.iter (fun name _ -> if not (Hashtbl.mem st.bdefined name) then fail lx ("undefined block ^" ^ name)) st.blocks;
  Hashtbl.iter (fun name _ -> if not (Hashtbl.mem st.defined name) then fail lx ("undefined value %" ^ name)) st.vals;
  (* blocks were created on first reference; renumber them in text order so
     the entry block is block 0 and printing is stable *)
  let order = Array.of_list (List.rev !order) in
  let n = Array.length order in
  let new_of_old = Array.make n 0 in
  Array.iteri (fun nw old -> new_of_old.(old) <- nw) order;
  let old_recs = Array.init n (fun k -> block f k) in
  Array.iteri (fun nw old -> Vec.set f.blocks nw old_recs.(old)) order;
  let rn b = new_of_old.(b) in
  let rt (t : target) = { t with dest = rn t.dest } in
  Vec.iter
    (fun (br : block_rec) ->
      br.term <-
        (match br.term with
        | Jump t -> Jump (rt t)
        | Br (c, t, e, w) -> Br (c, rt t, rt e, w)
        | Switch (x, d, cs) -> Switch (x, rt d, Array.map (fun (k, t) -> (k, rt t)) cs)
        | Indirect_br (x, bs) -> Indirect_br (x, Array.map rn bs)
        | t -> t))
    f.blocks;
  Vec.iteri
    (fun nw (br : block_rec) ->
      Array.iteri (fun k p -> Vec.set f.defs p (Param (nw, k))) br.params)
    f.blocks;
  resolve_all f;
  (* switch keys, now that value types are final *)
  List.iter
    (fun (b, raw) ->
      let br = block f (rn b) in
      match br.term with
      | Switch (x, d, cs) ->
          let w = int_width (ty_of f x) in
          if w = 0 then fail lx "switch on a non-integer";
          br.term <- Switch (x, d, Array.mapi (fun k (_, t) -> (limbs_of_text lx w raw.(k), t)) cs)
      | _ -> ())
    !switch_fixups

(* ---------- module ---------- *)

let parse_linkage_vis_attrs lx =
  let linkage = ref Global and vis = ref Default and attrs = ref 0 and display = ref None in
  let seen_linkage = ref false in
  (* the token after the next one, without consuming either *)
  let peek2 () =
    let pos = lx.pos and line = lx.line and peeked = lx.peeked in
    ignore (next lx);
    let t = peek lx in
    lx.pos <- pos;
    lx.line <- line;
    lx.peeked <- peeked;
    t
  in
  let rec go () =
    match peek lx with
    (* at most one linkage, and `global @x` is the next top-level global, not
       this declaration's linkage (a bodiless `declare` has no end marker) *)
    | Ident s
      when Option.is_some (linkage_of s) && (not !seen_linkage)
           && (match peek2 () with Sym _ -> false | _ -> true) ->
        ignore (next lx);
        seen_linkage := true;
        linkage := Option.get (linkage_of s);
        go ()
    | Ident s when Option.is_some (visibility_of s) -> ignore (next lx); vis := Option.get (visibility_of s); go ()
    | Ident "display" -> ignore (next lx); display := Some (str_tok lx); go ()
    | Ident s -> (
        match table_find Fir_print.func_attr_names s with
        | Some bit -> ignore (next lx); attrs := !attrs lor bit; go ()
        | None -> ())
    | _ -> ()
  in
  go ();
  (!linkage, !vis, !attrs, !display)

let parse_piece lx =
  match next lx with
  | Ident "bytes" -> Bytes (str_tok lx)
  | Ident "zeros" -> Zeros (int_tok lx)
  | Ident "reloc" ->
      let sym = match next lx with Sym s -> s | t -> fail lx ("expected a symbol, got " ^ tok_text t) in
      let addend =
        match peek lx with
        | P "+" -> ignore (next lx); int64_tok lx
        | Num n when n.[0] = '-' -> int64_tok lx
        | _ -> 0L
      in
      let width =
        match next lx with
        | Ident "w4" -> 4
        | Ident "w8" -> 8
        | t -> fail lx ("expected w4 or w8, got " ^ tok_text t)
      in
      Reloc { sym; addend; width }
  | t -> fail lx ("expected bytes, zeros or reloc, got " ^ tok_text t)

let parse_module s =
  let lx = { s; pos = 0; line = 1; peeked = None } in
  let m = create_module () in
  let rec go () =
    match next lx with
    | Eof -> ()
    | Ident "triple" -> m.triple <- str_tok lx; go ()
    | Ident "strict_aliasing" ->
        (match next lx with
        | Ident "false" -> m.strict_aliasing <- false
        | Ident "true" -> m.strict_aliasing <- true
        | t -> fail lx ("expected true or false, got " ^ tok_text t));
        go ()
    | Ident "loc" ->
        let i = int_tok lx in
        let file = str_tok lx in
        let line = int_tok lx in
        let col = int_tok lx in
        if i <> Vec.length m.locs then fail lx "location ids must be consecutive from 1";
        ignore (add_loc m file line col);
        go ()
    | Ident "alias_tag" ->
        let i = int_tok lx in
        let name = str_tok lx in
        let parent = int_tok lx in
        if i <> Vec.length m.alias_tags then fail lx "alias tag ids must be consecutive from 1";
        ignore (Vec.push m.alias_tags (name, parent));
        go ()
    | Ident "global" ->
        let gname = match next lx with Sym s -> s | t -> fail lx ("expected a symbol, got " ^ tok_text t) in
        expect_ident lx "size";
        let size = int_tok lx in
        expect_ident lx "align";
        let galign = int_tok lx in
        let g =
          { gname; glinkage = Global; gvisibility = Default; section = None; galign; readonly = false;
            thread_local = false; size; init = None; gdefined = true }
        in
        let rec attrs () =
          match next lx with
          | Ident s when Option.is_some (linkage_of s) -> g.glinkage <- Option.get (linkage_of s); attrs ()
          | Ident s when Option.is_some (visibility_of s) -> g.gvisibility <- Option.get (visibility_of s); attrs ()
          | Ident "section" -> g.section <- Some (str_tok lx); attrs ()
          | Ident "readonly" -> g.readonly <- true; attrs ()
          | Ident "thread_local" -> g.thread_local <- true; attrs ()
          | P "=" -> ()
          | t -> fail lx ("unexpected " ^ tok_text t ^ " in global")
        in
        attrs ();
        (match next lx with
        | Ident "zero" -> ()
        | Ident "extern" -> g.gdefined <- false
        | P "{" ->
            let ps = ref [] in
            if not (accept lx "}") then begin
              let rec pieces () =
                ps := parse_piece lx :: !ps;
                if accept lx "," then pieces () else expect lx "}"
              in
              pieces ()
            end;
            g.init <- Some (List.rev !ps)
        | t -> fail lx ("expected an initializer, got " ^ tok_text t));
        add_global m g;
        go ()
    | Ident (("func" | "declare") as kw) ->
        let name = match next lx with Sym s -> s | t -> fail lx ("expected a symbol, got " ^ tok_text t) in
        let sig_ = parse_sig lx in
        let linkage, vis, attrs, display = parse_linkage_vis_attrs lx in
        let f = new_func ~linkage ~defined:(kw = "func") name sig_ in
        f.visibility <- vis;
        f.fattrs <- attrs;
        f.display <- display;
        if kw = "func" then parse_body lx f;
        add_func m f;
        go ()
    | t -> fail lx ("unexpected " ^ tok_text t ^ " at top level")
  in
  go ();
  m
