(* Fir verifier: structure, types, flags, and SSA dominance. Returns the
   list of problems found (empty = valid). Messages name the function, block
   and instruction in printed form so they can be matched against
   Fir_print output. *)

open Fir

let abi_ty = function Scalar t | Ext (t, _) -> t | Agg _ -> ptr

(* an agg ABI describes a C struct: a size, a power-of-two alignment, and
   scalar fields that lie inside it *)
let abi_problem = function
  | Scalar _ | Ext _ -> None
  | Agg { size; align; fields } ->
      if size < 0 then Some (Printf.sprintf "agg of %d bytes" size)
      else if align <= 0 || align land (align - 1) <> 0 then Some (Printf.sprintf "agg alignment %d is not a power of two" align)
      else
        List.find_map
          (fun (o, t) ->
            if o < 0 || o > size - size_of t then
              Some (Printf.sprintf "agg field %s at offset %d is outside its %d bytes" (ty_to_string t) o size)
            else None)
          fields

let verify_func (f : func) =
  let errors = ref [] in
  let nerr = ref 0 in
  let err fmt =
    Printf.ksprintf
      (fun s ->
        incr nerr;
        if !nerr <= 50 then errors := Printf.sprintf "@%s: %s" f.name s :: !errors)
      fmt
  in
  if not f.defined then []
  else if num_blocks f = 0 then [ Printf.sprintf "@%s: no blocks" f.name ]
  else begin
    let nv = num_values f in
    let nb = num_blocks f in
    (* where each value is defined: block and position (-1 = parameter) *)
    let def_block = Array.make nv (-1) and def_pos = Array.make nv (-1) in
    let placed = Array.make (Vec.length f.insts) false in
    Vec.iteri
      (fun b (br : block_rec) ->
        Array.iteri
          (fun k p ->
            if p < 0 || p >= nv then err "^%d: bad parameter id" b
            else begin
              (match def_of f p with
              | Param (b', k') when b' = b && k' = k -> ()
              | _ -> err "^%d: parameter %d has a wrong definition record" b k);
              def_block.(p) <- b;
              def_pos.(p) <- -1
            end)
          br.params;
        Vec.iteri
          (fun pos i ->
            if placed.(i) then err "^%d: instruction %d placed twice" b i;
            placed.(i) <- true;
            Array.iteri
              (fun k r ->
                (match def_of f r with
                | Result (i', k') when i' = i && k' = k -> ()
                | _ -> err "^%d: result %d of instruction %d has a wrong definition record" b k i);
                def_block.(r) <- b;
                def_pos.(r) <- pos)
              (inst f i).results)
          br.insts)
      f.blocks;
    (* entry parameters follow the signature *)
    let entry_params = (block f entry).params in
    let sig_params = List.map abi_ty f.sig_.param_abis in
    if Array.length entry_params <> List.length sig_params then
      err "entry block has %d parameters, signature %d" (Array.length entry_params) (List.length sig_params)
    else
      List.iteri
        (fun k t ->
          if ty_of f entry_params.(k) <> t then err "entry parameter %d is %s, signature says %s" k
              (ty_to_string (ty_of f entry_params.(k))) (ty_to_string t))
        sig_params;
    let c = Fir_dom.cfg f in
    let d = Fir_dom.dominators c in
    if c.pred_start.(1) - c.pred_start.(0) > 0 then err "the entry block has predecessors";
    (* a use of value [v] in block [b] at position [pos] (max_int = terminator) *)
    let check_use where b pos v =
      if v < 0 || v >= nv then err "%s: value id %d out of range" where v
      else
        let v = resolve f v in
        match def_of f v with
        | Const _ -> ()
        | Unset -> err "%s: use of an undefined value" where
        | Alias _ -> ()
        | Param _ | Result _ ->
            let db = def_block.(v) in
            if db < 0 then err "%s: use of a value whose definition is not in any block" where
            else if Fir_dom.reachable d b then
              if db = b then (if def_pos.(v) >= pos then err "%s: use before definition" where)
              else if not (Fir_dom.reachable d db && Fir_dom.dominates d db b) then
                err "%s: definition in ^%d does not dominate the use" where db
    in
    let ty v = ty_of f (resolve f v) in
    let tys v = ty_to_string (ty v) in
    let is_int_like t = match elem t with Int _ -> true | _ -> false in
    let is_float_like t = is_float (elem t) in
    let mask_of t = with_elem t i1 in
    Vec.iteri
      (fun b (br : block_rec) ->
        Vec.iteri
          (fun pos i ->
            let ir = inst f i in
            let where = Printf.sprintf "^%d inst %d" b i in
            iter_op_uses (check_use where b pos) ir.op;
            let rs = ir.results in
            let nres = Array.length rs in
            let want n = if nres <> n then err "%s: defines %d values, expected %d" where nres n in
            let r k = ty_of f rs.(k) in
            let same what a t = if ty a <> t then err "%s: %s is %s, expected %s" where what (tys a) (ty_to_string t) in
            let only_flags fl allowed = if fl land lnot allowed <> 0 then err "%s: flags not allowed here" where in
            (match ir.op with
            | Nop -> want 0
            | Ibin (k, fl, a, x) ->
                want 1;
                if nres = 1 then begin
                  if not (is_int_like (r 0)) then err "%s: integer op on %s" where (ty_to_string (r 0));
                  same "lhs" a (r 0);
                  same "rhs" x (r 0)
                end;
                only_flags fl
                  (match k with
                  | Add | Sub | Mul | Shl -> f_nsw lor f_nuw
                  | Udiv | Sdiv | Lshr | Ashr -> f_exact
                  | Or -> f_disjoint
                  | _ -> 0)
            | Iun (k, fl, a) ->
                want 1;
                if nres = 1 then begin
                  if not (is_int_like (r 0)) then err "%s: integer op on %s" where (ty_to_string (r 0));
                  same "operand" a (r 0);
                  if k = Bswap && int_width (r 0) mod 16 <> 0 then err "%s: bswap needs a multiple of 16 bits" where
                end;
                only_flags fl (match k with Ctz | Clz -> f_zero_poison | Abs -> f_poison_min | _ -> 0)
            | Fbin (_, fl, a, x) ->
                want 1;
                if nres = 1 then begin
                  if not (is_float_like (r 0)) then err "%s: float op on %s" where (ty_to_string (r 0));
                  same "lhs" a (r 0);
                  same "rhs" x (r 0)
                end;
                only_flags fl (f_contract lor f_fast)
            | Fun (_, fl, a) ->
                want 1;
                if nres = 1 then begin
                  if not (is_float_like (r 0)) then err "%s: float op on %s" where (ty_to_string (r 0));
                  same "operand" a (r 0)
                end;
                only_flags fl (f_contract lor f_fast)
            | Fma (fl, a, x, y) ->
                want 1;
                if nres = 1 then begin
                  if not (is_float_like (r 0)) then err "%s: fma on %s" where (ty_to_string (r 0));
                  same "a" a (r 0); same "b" x (r 0); same "c" y (r 0)
                end;
                only_flags fl (f_contract lor f_fast)
            | Fsh (_, a, x, y) ->
                want 1;
                if nres = 1 then begin
                  if not (is_int_like (r 0)) then err "%s: funnel shift on %s" where (ty_to_string (r 0));
                  same "a" a (r 0); same "b" x (r 0); same "amount" y (r 0)
                end
            | Ovf (_, a, x) ->
                want 2;
                if nres = 2 then begin
                  if not (is_int_like (r 0)) then err "%s: overflow op on %s" where (ty_to_string (r 0));
                  same "lhs" a (r 0);
                  same "rhs" x (r 0);
                  if r 1 <> mask_of (r 0) then err "%s: overflow flag must be %s" where (ty_to_string (mask_of (r 0)))
                end
            | Icmp (_, a, x) ->
                want 1;
                let t = ty a in
                if not (is_int_like t || is_ptr (elem t)) then err "%s: icmp on %s" where (ty_to_string t);
                same "rhs" x t;
                if nres = 1 && r 0 <> mask_of t then err "%s: icmp result must be %s" where (ty_to_string (mask_of t))
            | Fcmp (_, fl, a, x) ->
                want 1;
                let t = ty a in
                if not (is_float_like t) then err "%s: fcmp on %s" where (ty_to_string t);
                same "rhs" x t;
                if nres = 1 && r 0 <> mask_of t then err "%s: fcmp result must be %s" where (ty_to_string (mask_of t));
                only_flags fl f_fast
            | Select (cnd, a, x) ->
                want 1;
                if nres = 1 then begin
                  let ct = ty cnd in
                  if ct <> i1 && ct <> mask_of (r 0) then err "%s: select condition is %s" where (ty_to_string ct);
                  same "true arm" a (r 0);
                  same "false arm" x (r 0)
                end
            | Freeze a ->
                want 1;
                if nres = 1 then same "operand" a (r 0)
            | Conv (k, a) ->
                want 1;
                if nres = 1 then begin
                  let s = ty a and t = r 0 in
                  (* bitcast may change shape (<8 x i1> <-> i8); it only
                     keeps the total bit count *)
                  if k <> Bitcast && (lanes s <> lanes t || (match (s, t) with Vec _, Vec _ | (Int _ | F16 | F32 | F64 | F80 | Ptr _), (Int _ | F16 | F32 | F64 | F80 | Ptr _) -> false | _ -> true))
                  then err "%s: conversion between different shapes" where;
                  let se = elem s and te = elem t in
                  let fw = function F16 -> 16 | F32 -> 32 | F64 -> 64 | F80 -> 80 | _ -> 0 in
                  let ok =
                    match k with
                    | Trunc -> is_int se && is_int te && int_width se > int_width te
                    | Zext | Sext -> is_int se && is_int te && int_width se < int_width te
                    | Fptrunc -> is_float se && is_float te && fw se > fw te
                    | Fpext -> is_float se && is_float te && fw se < fw te
                    | Fptosi | Fptoui | Fptosi_sat | Fptoui_sat -> is_float se && is_int te
                    | Sitofp | Uitofp -> is_int se && is_float te
                    | Ptrtoint -> is_ptr se && is_int te
                    | Inttoptr -> is_int se && is_ptr te
                    | Bitcast ->
                        let bits e = match e with Int w -> w | _ -> fw e in
                        (not (is_ptr se)) && (not (is_ptr te)) && bits se * lanes s = bits te * lanes t
                  in
                  if not ok then err "%s: invalid %s from %s to %s" where (Fir_print.conv_name k) (ty_to_string s) (ty_to_string t)
                end
            | Alloca (n, al) ->
                want 1;
                if not (is_int (ty n)) then err "%s: alloca size is %s" where (tys n);
                if nres = 1 && not (is_ptr (r 0)) then err "%s: alloca must produce a pointer" where;
                if al <= 0 || al land (al - 1) <> 0 then err "%s: alignment %d is not a power of two" where al
            | Load (m, p) ->
                want 1;
                if not (is_ptr (ty p)) then err "%s: load from %s" where (tys p);
                if m.order = Release || m.order = Acq_rel then err "%s: load cannot have release ordering" where
            | Store (m, _, p) ->
                want 0;
                if not (is_ptr (ty p)) then err "%s: store to %s" where (tys p);
                if m.order = Acquire || m.order = Acq_rel then err "%s: store cannot have acquire ordering" where
            | Ptradd (fl, p, idx, _, _) ->
                want 1;
                if not (is_ptr (ty p)) then err "%s: ptradd base is %s" where (tys p);
                if not (is_int (ty idx)) then err "%s: ptradd index is %s" where (tys idx);
                if nres = 1 && r 0 <> ty p then err "%s: ptradd result type differs from base" where;
                only_flags fl f_inbounds
            | Memcpy (_, a, x, n) | Memmove (_, a, x, n) ->
                want 0;
                if not (is_ptr (ty a) && is_ptr (ty x)) then err "%s: memcpy/memmove on non-pointers" where;
                if not (is_int (ty n)) then err "%s: size is %s" where (tys n)
            | Memset (_, a, x, n) ->
                want 0;
                if not (is_ptr (ty a)) then err "%s: memset destination is %s" where (tys a);
                same "byte" x i8;
                if not (is_int (ty n)) then err "%s: size is %s" where (tys n)
            | Rmw (k, m, p, x) ->
                want 1;
                if not (is_ptr (ty p)) then err "%s: atomic_rmw on %s" where (tys p);
                if m.order = Plain then err "%s: atomic_rmw needs an ordering" where;
                if nres = 1 then begin
                  same "operand" x (r 0);
                  let fl = (k = Rfadd || k = Rfsub) in
                  if fl && not (is_float (r 0)) then err "%s: float atomic on %s" where (ty_to_string (r 0));
                  if (not fl) && k <> Xchg && not (is_int (r 0)) then err "%s: integer atomic on %s" where (ty_to_string (r 0))
                end
            | Cmpxchg { mem; fail; p; expected; desired; _ } ->
                want 2;
                if not (is_ptr (ty p)) then err "%s: cmpxchg on %s" where (tys p);
                if mem.order = Plain || fail = Plain then err "%s: cmpxchg needs orderings" where;
                if fail = Release || fail = Acq_rel then err "%s: cmpxchg failure ordering cannot release" where;
                if nres = 2 then begin
                  same "expected" expected (r 0);
                  same "desired" desired (r 0);
                  if r 1 <> i1 then err "%s: cmpxchg flag must be i1" where
                end
            | Fence o ->
                want 0;
                if o = Plain || o = Relaxed then err "%s: fence needs acquire or stronger" where
            | Extract (a, idx) ->
                want 1;
                (match ty a with
                | Vec (_, e) -> if nres = 1 && r 0 <> e then err "%s: extract result type" where
                | t -> err "%s: extract from %s" where (ty_to_string t));
                if not (is_int (ty idx)) then err "%s: lane index is %s" where (tys idx)
            | Insert (a, x, idx) ->
                want 1;
                (match ty a with
                | Vec (_, e) ->
                    same "element" x e;
                    if nres = 1 && r 0 <> ty a then err "%s: insert result type" where
                | t -> err "%s: insert into %s" where (ty_to_string t));
                if not (is_int (ty idx)) then err "%s: lane index is %s" where (tys idx)
            | Shuffle (a, x, mask) ->
                want 1;
                same "second vector" x (ty a);
                (match ty a with
                | Vec (n, e) ->
                    if nres = 1 && r 0 <> Vec (Array.length mask, e) then err "%s: shuffle result type" where;
                    Array.iter (fun k -> if k < -1 || k >= 2 * n then err "%s: shuffle lane %d out of range" where k) mask
                | t -> err "%s: shuffle of %s" where (ty_to_string t))
            | Splat x ->
                want 1;
                if nres = 1 then (match r 0 with Vec (_, e) -> same "element" x e | t -> err "%s: splat to %s" where (ty_to_string t))
            | Reduce (k, a) ->
                want 1;
                (match ty a with
                | Vec (_, e) ->
                    if nres = 1 && r 0 <> e then err "%s: reduce result type" where;
                    let flt = match k with Red_fmin | Red_fmax | Red_fadd_ordered -> true | _ -> false in
                    if flt <> is_float e then err "%s: reduction kind does not match element type" where
                | t -> err "%s: reduce of %s" where (ty_to_string t))
            | Masked_load (_, p, m, pt) ->
                want 1;
                if not (is_ptr (ty p)) then err "%s: masked_load from %s" where (tys p);
                if nres = 1 then begin
                  same "mask" m (mask_of (r 0));
                  same "passthru" pt (r 0)
                end
            | Masked_store (_, x, p, m) ->
                want 0;
                if not (is_ptr (ty p)) then err "%s: masked_store to %s" where (tys p);
                same "mask" m (mask_of (ty x))
            | Compress (a, m) | Expand (a, m) ->
                want 1;
                if nres = 1 then begin
                  same "vector" a (r 0);
                  same "mask" m (mask_of (r 0))
                end
            | Call { sig_; callee; args; attrs = _; va_aggs } ->
                List.iter
                  (fun a -> Option.iter (err "%s: %s" where) (abi_problem a))
                  (sig_.param_abis @ sig_.result_abis @ List.map snd va_aggs);
                let np0 = List.length sig_.param_abis in
                List.iter
                  (fun (k, a) ->
                    (match a with Agg _ -> () | _ -> err "%s: argument %d: only an agg ABI may be written on an argument" where k);
                    if k < np0 || k >= Array.length args then err "%s: argument %d: an agg ABI is only for variadic arguments" where k
                    else if not (is_ptr (ty args.(k))) then err "%s: agg argument %d is %s, not ptr" where k (tys args.(k)))
                  va_aggs;
                (match callee with Indirect v -> if not (is_ptr (ty v)) then err "%s: indirect callee is %s" where (tys v) | Direct _ -> ());
                let ps = List.map abi_ty sig_.param_abis in
                let np = List.length ps in
                if Array.length args < np || ((not sig_.variadic) && Array.length args > np) then
                  err "%s: call passes %d arguments, signature has %d" where (Array.length args) np
                else List.iteri (fun k t -> same (Printf.sprintf "argument %d" k) args.(k) t) ps;
                let rts = List.map abi_ty sig_.result_abis in
                if List.length rts <> nres then err "%s: call defines %d values, signature returns %d" where nres (List.length rts)
                else List.iteri (fun k t -> if r k <> t then err "%s: call result %d type" where k) rts
            | Syscall args ->
                want 1;
                if nres = 1 && r 0 <> i64 then err "%s: syscall result must be i64" where;
                if Array.length args < 1 || Array.length args > 7 then err "%s: syscall takes 1 to 7 values" where;
                Array.iteri (fun k a -> same (Printf.sprintf "syscall argument %d" k) a i64) args
            | Asm { tmpl; aflags; outs; ins; aargs; clobbers } -> (
                let pairs l = Array.map (fun (o : asm_operand) -> (o.aname, o.acon)) l in
                match
                  Fir_asm.parse ~labels:0 ~intel:(aflags land asm_intel <> 0) ~tmpl ~outs:(pairs outs) ~ins:(pairs ins)
                    ~clobbers
                with
                | exception Fir_asm.Bad m -> err "%s: %s" where m
                | st ->
                    if aflags land lnot (asm_volatile lor asm_intel) <> 0 then err "%s: unknown asm flags" where;
                    (* results: the outputs that are not memory operands, in order *)
                    let rtys = ref [] in
                    Array.iteri
                      (fun k (o : asm_operand) ->
                        if Fir_asm.output_is_result st.outs.(k) then rtys := o.aty :: !rtys;
                        if st.outs.(k).flag <> None && not (is_int o.aty) then
                          err "%s: asm flag output %d is %s, not an integer" where k (ty_to_string o.aty))
                      outs;
                    let rtys = List.rev !rtys in
                    if List.length rtys <> nres then
                      err "%s: asm defines %d values, its outputs give %d" where nres (List.length rtys)
                    else List.iteri (fun k t -> if r k <> t then err "%s: asm result %d type" where k) rtys;
                    (* values: outputs that take one, then every input *)
                    let want_args = ref [] in
                    Array.iteri
                      (fun k (o : asm_operand) ->
                        let c = st.outs.(k) in
                        if Fir_asm.output_takes_arg c then
                          want_args := (if Fir_asm.indirect c then ptr else o.aty) :: !want_args)
                      outs;
                    Array.iteri
                      (fun k (o : asm_operand) ->
                        let c = st.ins.(k) in
                        want_args := (if Fir_asm.indirect c then ptr else o.aty) :: !want_args;
                        match c.tied with
                        | Some t when outs.(t).aty <> o.aty ->
                            err "%s: asm input %d is %s but is tied to output %d of %s" where k
                              (ty_to_string o.aty) t (ty_to_string outs.(t).aty)
                        | _ -> ())
                      ins;
                    let want_args = List.rev !want_args in
                    if List.length want_args <> Array.length aargs then
                      err "%s: asm takes %d values, its operands need %d" where (Array.length aargs)
                        (List.length want_args)
                    else List.iteri (fun k t -> same (Printf.sprintf "asm value %d" k) aargs.(k) t) want_args)
            | Va_start p ->
                want 0;
                if not (is_ptr (ty p)) then err "%s: va_start of %s" where (tys p);
                if not f.sig_.variadic then err "%s: va_start in a function that is not variadic" where
            | Va_arg p ->
                want 1;
                if not (is_ptr (ty p)) then err "%s: va_arg of %s" where (tys p);
                if nres = 1 && not (is_int (r 0) || is_ptr (r 0) || r 0 = F16 || r 0 = F32 || r 0 = F64 || r 0 = F80 || (match r 0 with Vec _ -> true | _ -> false)) then
                  err "%s: va_arg reads an integer, a ptr, f16, f32, f64, f80 or a vector" where
            | Stack_save ->
                want 1;
                if nres = 1 && not (is_ptr (r 0)) then err "%s: stacksave defines a ptr" where
            | Stack_restore p ->
                want 0;
                if not (is_ptr (ty p)) then err "%s: stackrestore of %s" where (tys p)
            | Lifetime (_, n, p) ->
                want 0;
                if not (is_ptr (ty p)) then err "%s: lifetime marker on %s" where (tys p);
                if n < 0 then err "%s: lifetime marker of %d bytes" where n
            | Va_arg_agg (a, p) ->
                want 1;
                if not (is_ptr (ty p)) then err "%s: va_arg of %s" where (tys p);
                (match a with Agg _ -> () | _ -> err "%s: va_arg with an ABI needs agg" where);
                Option.iter (err "%s: %s" where) (abi_problem a);
                if nres = 1 && not (is_ptr (r 0)) then err "%s: va_arg agg defines a ptr" where
            | Trap -> want 0
            | Dbg_value (var, _) -> (
                want 0;
                (* VAR is the 1-based index of a [var] without a home *)
                match List.nth_opt f.vars (var - 1) with
                | _ when var < 1 -> err "%s: dbg_value of variable %d" where var
                | None -> err "%s: dbg_value of variable %d, the function has %d" where var (List.length f.vars)
                | Some { home = Some _; vname; _ } ->
                    err "%s: dbg_value of variable %d (%s), which lives in memory (at)" where var vname
                | Some _ -> ()));
            ignore i)
          br.insts;
        let where = Printf.sprintf "^%d terminator" b in
        iter_term_uses (check_use where b max_int) br.term;
        let rec check_target (t : target) =
          if t.dest < 0 || t.dest >= nb then err "%s: target block %d out of range" where t.dest
          else begin
            if t.dest = entry then err "%s: branch to the entry block" where;
            let ps = (block f t.dest).params in
            if Array.length ps <> Array.length t.args then
              err "%s: ^%d takes %d arguments, %d given" where t.dest (Array.length ps) (Array.length t.args)
            else Array.iteri (fun k a -> same (Printf.sprintf "argument %d to ^%d" k t.dest) a (ty_of f ps.(k))) t.args
          end
        and same what a t = if ty a <> t then err "%s: %s is %s, expected %s" where what (tys a) (ty_to_string t) in
        match br.term with
        | Jump t -> check_target t
        | Br (cnd, t, e, w) ->
            same "condition" cnd i1;
            check_target t;
            check_target e;
            if w > 1000 then err "%s: weight above 1000" where
        | Switch (x, dflt, cases) ->
            check_target dflt;
            let w = int_width (ty x) in
            if not (is_int (ty x)) then err "%s: switch on %s" where (tys x);
            let seen = Hashtbl.create (Array.length cases) in
            Array.iter
              (fun (k, t) ->
                check_target t;
                if Array.length k <> limbs_for w then err "%s: case key has the wrong size" where
                else if normalize_limbs w k <> k then err "%s: case key not normalized" where;
                if Hashtbl.mem seen k then err "%s: duplicate case key" where;
                Hashtbl.replace seen k ())
              cases
        | Indirect_br (x, bs) ->
            if not (is_ptr (ty x)) then err "%s: indirect_br on %s" where (tys x);
            Array.iter
              (fun t ->
                if t < 0 || t >= nb || t = entry then err "%s: bad indirect target" where
                else if Array.length (block f t).params > 0 then
                  err "%s: indirect target ^%d takes parameters" where t)
              bs
        | Asm_goto { tmpl; aflags; ins; aargs; clobbers; fall; labels } -> (
            (* the fall-through and every label: plain blocks without parameters *)
            Array.iter
              (fun t ->
                if t < 0 || t >= nb || t = entry then err "%s: bad asm_goto target" where
                else if Array.length (block f t).params > 0 then
                  err "%s: asm_goto target ^%d takes parameters" where t)
              (Array.append [| fall |] labels);
            let pairs l = Array.map (fun (o : asm_operand) -> (o.aname, o.acon)) l in
            match
              Fir_asm.parse ~labels:(Array.length labels) ~intel:(aflags land asm_intel <> 0) ~tmpl ~outs:[||]
                ~ins:(pairs ins) ~clobbers
            with
            | exception Fir_asm.Bad m -> err "%s: %s" where m
            | st ->
                if aflags land lnot (asm_volatile lor asm_intel) <> 0 then err "%s: unknown asm flags" where;
                if Array.length aargs <> Array.length ins then
                  err "%s: asm_goto takes %d values, its operands need %d" where (Array.length aargs)
                    (Array.length ins)
                else
                  Array.iteri
                    (fun k (o : asm_operand) ->
                      if st.ins.(k).tied <> None then err "%s: asm_goto input %d is tied, with no outputs" where k
                      else same (Printf.sprintf "asm value %d" k) aargs.(k)
                          (if Fir_asm.indirect st.ins.(k) then ptr else o.aty))
                    ins)
        | Ret vs ->
            let rts = List.map abi_ty f.sig_.result_abis in
            if Array.length vs <> List.length rts then err "%s: returns %d values, signature %d" where (Array.length vs) (List.length rts)
            else List.iteri (fun k t -> same (Printf.sprintf "return value %d" k) vs.(k) t) rts
        | Unreachable -> ())
      f.blocks;
    if !nerr > 50 then errors := Printf.sprintf "@%s: ... %d more" f.name (!nerr - 50) :: !errors;
    List.rev !errors
  end

let verify (m : modul) =
  let names = Hashtbl.create 64 in
  let errs = ref [] in
  let check_name n =
    if Hashtbl.mem names n then errs := Printf.sprintf "symbol @%s defined twice" n :: !errs;
    Hashtbl.replace names n ()
  in
  Vec.iter (fun g -> check_name g.gname) m.globals;
  Vec.iter (fun f -> check_name f.name) m.funcs;
  Vec.iter (fun a -> check_name a.aname) m.aliases;
  (* an alias names a function or global defined here, never another alias
     (so there are no cycles); an addend stays inside a global *)
  Vec.iter
    (fun a ->
      let bad why = errs := Printf.sprintf "alias @%s = @%s: %s" a.aname a.target why :: !errs in
      let fn = Vec.fold (fun acc f -> if f.name = a.target then Some f else acc) None m.funcs in
      let gl = Vec.fold (fun acc g -> if g.gname = a.target then Some g else acc) None m.globals in
      match (fn, gl) with
      | Some f, _ ->
          if not f.defined then bad "the function is only declared";
          if a.addend <> 0L then bad "an addend into a function"
      | None, Some g ->
          if not g.gdefined then bad "the global is only declared";
          if Int64.compare a.addend 0L < 0 || Int64.compare a.addend (Int64.of_int g.size) > 0 then
            bad "the addend is outside the global"
      | None, None -> bad "not a function or global defined in this module")
    m.aliases;
  Vec.iter
    (fun g ->
      if g.galign <= 0 || g.galign land (g.galign - 1) <> 0 then
        errs := Printf.sprintf "@%s: alignment %d is not a power of two" g.gname g.galign :: !errs;
      if g.size < 0 then errs := Printf.sprintf "@%s: size %d" g.gname g.size :: !errs;
      (match g.init with
      | Some ps when List.exists (function Zeros k -> k < 0 | _ -> false) ps ->
          errs := Printf.sprintf "@%s: a negative zeros piece" g.gname :: !errs
      | _ -> ());
      match g.init with
      | Some ps ->
          let n =
            List.fold_left
              (fun acc p -> acc + match p with Bytes s -> String.length s | Zeros k -> k | Reloc { width; _ } -> width | Blockref _ -> 8)
              0 ps
          in
          if n <> g.size then errs := Printf.sprintf "@%s: initializer is %d bytes, size %d" g.gname n g.size :: !errs
      | None -> ())
    m.globals;
  (* blockaddr(@g, b) names a block of a function defined in this module that
     an indirect_br could enter: not the entry, no parameters *)
  let funcs = Hashtbl.create 64 in
  Vec.iter (fun f -> Hashtbl.replace funcs f.name f) m.funcs;
  let check_blockaddr owner g b =
    let bad why = errs := Printf.sprintf "@%s: blockaddr(@%s, %d): %s" owner g b why :: !errs in
    match Hashtbl.find_opt funcs g with
    | Some gf when gf.defined ->
        if b < 0 || b >= Vec.length gf.blocks then bad "no such block"
        else if b = entry then bad "the entry block"
        else if Array.length (block gf b).params > 0 then bad "the block takes parameters"
    | _ -> bad "not a function defined in this module"
  in
  Vec.iter
    (fun f -> Hashtbl.iter (fun c _ -> match c with Cblockaddr (g, b) -> check_blockaddr f.name g b | _ -> ()) f.consts)
    m.funcs;
  Vec.iter
    (fun g ->
      match g.init with
      | Some ps -> List.iter (function Blockref { fn; block } -> check_blockaddr g.gname fn block | _ -> ()) ps
      | None -> ())
    m.globals;
  (* location ids index this module's table (0 = none): an id from another
     module's table would name the wrong line, or none *)
  let nlocs = Vec.length m.locs in
  Vec.iter
    (fun f ->
      if f.defined then begin
        let bad = ref 0 in
        if f.decl_loc < 0 || f.decl_loc >= nlocs then bad := f.decl_loc;
        Vec.iter (fun (ir : inst_rec) -> if ir.loc < 0 || ir.loc >= nlocs then bad := ir.loc) f.insts;
        Vec.iter (fun (b : block_rec) -> if b.term_loc < 0 || b.term_loc >= nlocs then bad := b.term_loc) f.blocks;
        if !bad <> 0 then
          errs := Printf.sprintf "@%s: location id %d out of range (%d locations)" f.name !bad (nlocs - 1) :: !errs
      end)
    m.funcs;
  (* debug information (roadmap v0.8): every id in range; a scope belongs to
     one function (the function whose locations or variables name it or a
     scope inside it) *)
  let ntypes = Vec.length m.dtypes and nscopes = Vec.length m.scopes in
  let type_ok t = t >= 0 && t < ntypes in
  Vec.iteri
    (fun i t ->
      let bad what = errs := Printf.sprintf "dtype %d: %s" i what :: !errs in
      let ref_ok what t = if not (type_ok t) then bad (Printf.sprintf "%s type %d out of range" what t) in
      match t with
      | Dvoid -> if i <> 0 then bad "void is only type 0"
      | Dbase { size; name; _ } ->
          if size <= 0 then bad "base type size must be positive";
          if name = "" then bad "base type needs a name"
      | Dpointer t -> ref_ok "pointee" t
      | Dstruct { size; members; _ } ->
          (match size with Some n when n < 0 -> bad "negative size" | _ -> ());
          List.iter
            (fun (_, t, o) ->
              ref_ok "member" t;
              if t = 0 then bad "a member cannot be void";
              if o < 0 then bad "negative member offset")
            members
      | Darray (t, n) | Dvector (t, n) ->
          ref_ok "element" t;
          if t = 0 then bad "an element cannot be void";
          if n < 0 then bad "negative count"
      | Dtypedef (_, t) -> ref_ok "aliased" t
      | Denum { base; _ } -> ref_ok "underlying" base)
    m.dtypes;
  (* DWARF strings are NUL-terminated (DW_FORM_string, the line table's
     file names), so a name with a NUL byte in it cannot be written *)
  let nul what s = if String.contains s '\000' then errs := Printf.sprintf "%s %S has a NUL byte" what s :: !errs in
  Vec.iteri (fun i (file, _, _) -> if i > 0 then nul (Printf.sprintf "loc %d: file name" i) file) m.locs;
  Vec.iteri
    (fun i t ->
      let nul what = nul (Printf.sprintf "dtype %d: %s" i what) in
      match t with
      | Dbase { name; _ } | Dtypedef (name, _) -> nul "name" name
      | Dstruct { name; members; _ } ->
          nul "name" name;
          List.iter (fun (m, _, _) -> nul "member" m) members
      | Denum { name; enumerators; _ } ->
          nul "name" name;
          List.iter (fun (e, _) -> nul "enumerator" e) enumerators
      | Dvoid | Dpointer _ | Darray _ | Dvector _ -> ())
    m.dtypes;
  Vec.iter
    (fun f ->
      Option.iter (nul (Printf.sprintf "@%s: display name" f.name)) f.display;
      List.iter (fun d -> nul (Printf.sprintf "@%s: var" f.name) d.vname) f.vars)
    m.funcs;
  Hashtbl.iter
    (fun l sc ->
      if l <= 0 || l >= nlocs then errs := Printf.sprintf "loc %d: no such location" l :: !errs;
      if sc <= 0 || sc >= nscopes then errs := Printf.sprintf "loc %d: scope %d out of range" l sc :: !errs)
    m.loc_scopes;
  let owner = Array.make (max 1 nscopes) "" in
  let claim fname sc =
    let rec up sc =
      if sc > 0 && sc < nscopes then begin
        if owner.(sc) = "" then owner.(sc) <- fname
        else if owner.(sc) <> fname then
          errs := Printf.sprintf "scope %d is used by both @%s and @%s" sc owner.(sc) fname :: !errs;
        up (Vec.get m.scopes sc)
      end
    in
    up sc
  in
  Vec.iter
    (fun f ->
      if f.defined then begin
        let seen = Hashtbl.create 8 in
        let note l = if l > 0 && l < nlocs && not (Hashtbl.mem seen l) then begin
            Hashtbl.replace seen l ();
            claim f.name (loc_scope m l)
          end
        in
        Vec.iter (fun (ir : inst_rec) -> note ir.loc) f.insts;
        Vec.iter (fun (b : block_rec) -> note b.term_loc) f.blocks;
        List.iter
          (fun d ->
            let bad what = errs := Printf.sprintf "@%s: var %S: %s" f.name d.vname what :: !errs in
            if not (type_ok d.vty) then bad (Printf.sprintf "type %d out of range" d.vty);
            if d.vscope < 0 || d.vscope >= nscopes then bad (Printf.sprintf "scope %d out of range" d.vscope)
            else claim f.name d.vscope;
            if d.vloc < 0 || d.vloc >= nlocs then bad (Printf.sprintf "location %d out of range" d.vloc);
            if d.varg < 0 || d.varg > List.length f.sig_.param_abis then
              bad (Printf.sprintf "arg %d out of range" d.varg);
            match d.home with
            | None -> ()
            | Some v ->
                let v = resolve f v in
                if v < 0 || v >= num_values f then bad "home is not a value"
                else (
                  match (def_of f v, ty_of f v) with
                  | Unset, _ -> bad "home is not defined"
                  | _, Ptr _ -> ()
                  | _, t -> bad ("home must be a pointer, not " ^ ty_to_string t)))
          f.vars
      end
      else if f.vars <> [] then errs := Printf.sprintf "@%s: a declaration has no variables" f.name :: !errs)
    m.funcs;
  Vec.iter
    (fun f ->
      List.iter
        (fun a -> Option.iter (fun p -> errs := Printf.sprintf "@%s: signature: %s" f.name p :: !errs) (abi_problem a))
        (f.sig_.param_abis @ f.sig_.result_abis);
      (* a vector constant has one lane per lane of its type, each of the
         element type *)
      Hashtbl.iter
        (fun c v ->
          match c with
          | Cvec (t, lanes) ->
              let bad why = errs := Printf.sprintf "@%s: vector constant %s: %s" f.name (ty_to_string t) why :: !errs in
              (match t with
              | Vec (n, e) ->
                  if Array.length lanes <> n then bad (Printf.sprintf "%d lanes given" (Array.length lanes))
                  else if Array.exists (fun l -> ty_of f l <> e) lanes then bad "a lane of another type"
              | _ -> bad "not a vector type");
              if ty_of f v <> t then bad "defines a value of another type"
          | _ -> ())
        f.consts)
    m.funcs;
  let fs = Vec.fold (fun acc f -> List.rev_append (verify_func f) acc) [] m.funcs in
  List.rev !errs @ List.rev fs
