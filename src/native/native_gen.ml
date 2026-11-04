(* Native code generation from [Ir]: scalar integers.

   Pipeline ([compile]): Mem2reg (promotion, addr+n as gep, DCE, jump
   threading, pruning), Ir_fold, store forwarding and block merging,
   reassociation, then per function: address analysis ([analyse_addrs] folds
   gep chains and constant index arithmetic into [base + idx*scale + disp]
   forms), linear-scan allocation (Regalloc), RPO block layout with loop
   rotation, and emission.

   Every SSA value has a home: a register from Regalloc or an 8-byte stack
   slot (aggregates get a slot of their size). Scratch registers are rax, rcx
   and rdx. Integer values are kept canonical: zero-extended from their width
   to 64 bits, with `i1` as 0 or 1; signed operations sign-extend operands.
   Instruction selection fuses compare-and-branch, folds address forms and
   immediates into operands, and turns load/op/store of one place into a
   read-modify-write instruction.

   Phis: each phi has a second "incoming" home. Edges write the incoming homes
   and the phi's block copies them into the phi homes on entry, so a phi that
   reads another phi of the same block always sees the old value.

   Calls between Fas functions use the SysV integer registers for the first
   six arguments and the stack for the rest, with the result in rax. Floats,
   vectors and `i128` are not supported yet and raise [Unsupported]. *)

open Ir
module A = X64

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

type ctx = { m : module_; a : A.t; func_labels : (string, A.label) Hashtbl.t }

let size_of ctx ty =
  match static_type_bytes ctx.m.structs [] ty with
  | Ok n -> n
  | Error () -> unsupported "type with no static size"

let bits = function
  | I1 -> 1
  | I8 -> 8
  | I16 -> 16
  | I32 -> 32
  | I64 | Pointer _ -> 64
  | I128 -> unsupported "i128 values"
  | F32 | F64 -> unsupported "floating-point values"
  | Vector _ -> unsupported "vector values"
  | Struct _ | Array _ -> unsupported "aggregate value in a scalar position"
  | Void -> unsupported "void value"

let is_aggregate = function Struct _ | Array _ -> true | _ -> false

let norm_const ty v =
  let w = bits ty in
  if w >= 64 then v else Int64.logand v (Int64.pred (Int64.shift_left 1L w))

let field_offset ctx name index =
  match List.find_opt (fun (d : struct_def) -> d.name = name) ctx.m.structs with
  | None -> unsupported "unknown struct %s" name
  | Some d ->
      let rec go n acc = function
        | [] -> unsupported "struct field index out of range"
        | f :: rest -> if n = index then acc else go (n + 1) (acc + size_of ctx f) rest
      in
      go 0 0 d.fields

(* Address forms. A pointer computed by `getelementptr`, `alloca` or a
   global is described as an anchor plus a constant offset plus at most one
   scaled 64-bit index, which is exactly an x86 memory operand. Pointers that
   are only ever used as the address of scalar loads and stores are folded
   into those instructions and never computed; constant ones (a global or a
   stack slot plus an offset) are rebuilt with one `lea` wherever a register
   needs them. *)

type anchor = Reg of value | Sym of string | Frame of int  (** alloca id *)
type form = { anchor : anchor; off : int; idx : (value * int) option }

type addrs = {
  forms : (int, form) Hashtbl.t;
  skip : (int, unit) Hashtbl.t;
  dead : (int, unit) Hashtbl.t;
      (** index arithmetic that folding into addresses left unused *)
}

let plain v = { anchor = Reg v; off = 0; idx = None }
let is_const_form f = f.idx = None && match f.anchor with Reg _ -> false | _ -> true

let form_operands f =
  (match f.anchor with Reg v -> [ v ] | _ -> [])
  @ match f.idx with Some (v, _) -> [ v ] | None -> []

let scalar_access = function
  | (Load (_, ty, p, _) | Load_volatile (_, ty, p, _)) when not (is_aggregate ty) ->
      Some (None, p)
  | (Store (ty, v, p, _) | Store_volatile (ty, v, p, _)) when not (is_aggregate ty) ->
      Some (Some v, p)
  | _ -> None

let analyse_addrs ctx (f : func) =
  let defs = Hashtbl.create 64 in
  let arith = Hashtbl.create 64 in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          match ins with
          | Bin (id, (Mul | Shl | Add), I64, _, Const _) -> Hashtbl.replace arith id ins
          | Gep (id, _, _, _)
          | Global_ptr (id, _, _)
          | String_ptr (id, _, _)
          | Alloca (id, _, _) ->
              Hashtbl.replace defs id ins
          | _ -> ())
        b.instrs)
    f.blocks;
  let memo = Hashtbl.create 64 in
  let rec form_of_id id =
    match Hashtbl.find_opt memo id with
    | Some r -> r
    | None ->
        let r =
          match Hashtbl.find_opt defs id with
          | Some (Global_ptr (_, name, _)) ->
              Some { anchor = Sym name; off = 0; idx = None }
          | Some (String_ptr (_, i, _)) ->
              Some { anchor = Sym (Printf.sprintf ".str.%d" i); off = 0; idx = None }
          | Some (Alloca (id, _, _)) -> Some { anchor = Frame id; off = 0; idx = None }
          | Some (Gep (_, ty, p, indices)) -> gep_form ty p indices
          | _ -> None
        in
        Hashtbl.replace memo id r;
        r
  and value_form v =
    match v with
    | Local (id, _) -> ( match form_of_id id with Some f -> f | None -> plain v)
    | Global (name, _) -> { anchor = Sym name; off = 0; idx = None }
    | v -> plain v
  and gep_form ty p indices =
    let exception No in
    let base = value_form p in
    let f = ref base in
    let add n =
      let o = Int64.add (Int64.of_int !f.off) n in
      if Int64.compare (Int64.abs o) 0x4000_0000L > 0 then raise No;
      f := { !f with off = Int64.to_int o }
    in
    let step size = function
      | Zero -> ()
      | Index (Const (ity, n)) ->
          let w = bits ity in
          let n =
            if w >= 64 then n
            else Int64.shift_right (Int64.shift_left n (64 - w)) (64 - w)
          in
          if Int64.compare (Int64.abs n) 0x4000_0000L > 0 then raise No;
          add (Int64.mul n (Int64.of_int size))
      | Index v ->
          (* A base that already uses the index slot, untouched so far, is
             used from its register instead: [q + v] *)
          (match p with
          | Local _ when !f.idx <> None && !f == base -> f := plain p
          | _ -> ());
          if value_ty v <> I64 || (not (List.mem size [ 1; 2; 4; 8 ])) || !f.idx <> None
          then raise No;
          (* (x + c) * k with k * size a valid scale: x scaled, c * size
             in the displacement *)
          let rec scaled v size =
            match v with
            | Local (id, _) -> (
                let small n = Int64.compare (Int64.abs n) 0x1000_0000L < 0 in
                match Hashtbl.find_opt arith id with
                | Some (Bin (_, Mul, _, x, Const (_, k)))
                  when small k && List.mem (Int64.to_int k * size) [ 1; 2; 4; 8 ] ->
                    scaled x (Int64.to_int k * size)
                | Some (Bin (_, Shl, _, x, Const (_, k)))
                  when Int64.compare k 0L >= 0
                       && Int64.compare k 3L <= 0
                       && List.mem (size lsl Int64.to_int k) [ 1; 2; 4; 8 ] ->
                    scaled x (size lsl Int64.to_int k)
                | Some (Bin (_, Add, _, x, Const (_, c))) when small c ->
                    add (Int64.mul c (Int64.of_int size));
                    scaled x size
                | _ -> (v, size))
            | _ -> (v, size)
          in
          let idx = scaled v size in
          f := { !f with idx = Some idx }
    in
    let rec walk ty = function
      | [] -> ()
      | idx :: rest -> (
          match ty with
          | Struct name ->
              let n =
                match idx with
                | Zero -> 0
                | Index (Const (_, n)) -> Int64.to_int n
                | Index _ -> raise No
              in
              add (Int64.of_int (field_offset ctx name n));
              let d = List.find (fun (d : struct_def) -> d.name = name) ctx.m.structs in
              walk (List.nth d.fields n) rest
          | Array (_, elem) | Vector (_, elem) ->
              step (size_of ctx elem) idx;
              walk elem rest
          | _ -> raise No)
    in
    try
      (match indices with
      | [] -> ()
      | first :: rest ->
          step (size_of ctx ty) first;
          walk ty rest);
      Some !f
    with No | Unsupported _ | Not_found | Failure _ | Invalid_argument _ -> None
  in
  let forms = Hashtbl.create 64 in
  Hashtbl.iter
    (fun id _ ->
      match form_of_id id with Some f -> Hashtbl.replace forms id f | None -> ())
    defs;
  (* Which form-carrying values are used other than as a scalar address? *)
  let escapes = Hashtbl.create 32 in
  let escape = function Local (id, _) -> Hashtbl.replace escapes id () | _ -> () in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          match (ins, scalar_access ins) with
          | Gep (id, _, _, _), _ when Hashtbl.mem forms id ->
              List.iter escape (form_operands (Hashtbl.find forms id))
          | _, Some (v, _) -> Option.iter escape v
          | _ -> Regalloc.iter_operands escape ins)
        b.instrs;
      Regalloc.iter_term_operands escape b.terminator)
    f.blocks;
  let skip = Hashtbl.create 32 in
  Hashtbl.iter
    (fun id f ->
      if is_const_form f || not (Hashtbl.mem escapes id) then Hashtbl.replace skip id ())
    forms;
  (* Index arithmetic whose only readers now read through the folded form *)
  let dead = Hashtbl.create 16 in
  let addrs0 = { forms; skip; dead } in
  let changed = ref true in
  while !changed do
    changed := false;
    let used = Hashtbl.create 64 in
    let use = function Local (id, _) -> Hashtbl.replace used id () | _ -> () in
    List.iter
      (fun (b : block) ->
        List.iter
          (fun ins ->
            match instruction_result ins with
            | Some (id, _) when Hashtbl.mem dead id -> ()
            | _ -> (
                match (ins, scalar_access ins) with
                | Gep (id, _, _, _), _ when Hashtbl.mem forms id ->
                    if not (Hashtbl.mem skip id) then
                      List.iter use (form_operands (Hashtbl.find forms id))
                | _, Some (v, p) -> (
                    Option.iter use v;
                    match p with
                    | Local (id, _) when Hashtbl.mem skip id ->
                        List.iter use (form_operands (Hashtbl.find forms id))
                    | p -> use p)
                | _ -> Regalloc.iter_operands use ins))
          b.instrs;
        Regalloc.iter_term_operands use b.terminator)
      f.blocks;
    Hashtbl.iter
      (fun id _ ->
        if (not (Hashtbl.mem used id)) && not (Hashtbl.mem dead id) then begin
          Hashtbl.replace dead id ();
          changed := true
        end)
      arith
  done;
  addrs0

(* What an instruction reads once folded addresses are expanded *)
let operands_view addrs ins =
  let ptr p =
    match p with
    | Local (id, _) when Hashtbl.mem addrs.skip id ->
        form_operands (Hashtbl.find addrs.forms id)
    | p -> [ p ]
  in
  match (ins, scalar_access ins) with
  | Bin (id, _, _, _, _), _ when Hashtbl.mem addrs.dead id -> []
  | Gep (id, _, _, _), _ when Hashtbl.mem addrs.forms id ->
      if Hashtbl.mem addrs.skip id then []
      else form_operands (Hashtbl.find addrs.forms id)
  | _, Some (v, p) -> Option.to_list v @ ptr p
  | _ -> Regalloc.default_operands ins

(* Store-to-load forwarding and dead-store elimination inside each block.
   Two accesses are the same location when their address forms have the same
   anchor, index and offset; they are disjoint when the anchors are distinct
   globals or stack slots, or when the anchor and index match and the byte
   ranges do not overlap. Anything else may alias. A load of a location whose
   value is known (stored or loaded earlier in the block, nothing that may
   alias written since) takes that value; a store that a later store to the
   same bytes overwrites, with no read of them in between, is dropped. Calls,
   system calls, volatile accesses and traps forget everything. This is what
   merges `for const` read-modify-write chains on one word into one store. *)
let forward_memory ctx (f : func) =
  let addrs = analyse_addrs ctx f in
  let form_of = function
    | Local (id, _) as v -> (
        match Hashtbl.find_opt addrs.forms id with Some fm -> fm | None -> plain v)
    | Global (name, _) -> { anchor = Sym name; off = 0; idx = None }
    | v -> plain v
  in
  let distinct a b =
    match (a, b) with
    | Sym x, Sym y -> x <> y
    | Frame x, Frame y -> x <> y
    | Sym _, Frame _ | Frame _, Sym _ -> true
    | _ -> false
  in
  let disjoint (f1, s1) (f2, s2) =
    distinct f1.anchor f2.anchor
    || f1.anchor = f2.anchor && f1.idx = f2.idx
       && (f1.off + s1 <= f2.off || f2.off + s2 <= f1.off)
  in
  let covers (f1, s1) (f2, s2) =
    (* [f1, s1] holds all of [f2, s2] *)
    f1.anchor = f2.anchor && f1.idx = f2.idx && f1.off <= f2.off
    && f2.off + s2 <= f1.off + s1
  in
  let env = Hashtbl.create 64 in
  let rec resolve = function
    | Local (i, _) as v -> (
        match Hashtbl.find_opt env i with Some w -> resolve w | None -> v)
    | v -> v
  in
  let size ty = try Some (size_of ctx ty) with Unsupported _ -> None in
  let blocks =
    List.map
      (fun (b : block) ->
        (* (location, type, value) known to be in memory *)
        let known = ref [] in
        (* stores not read since: (location, position in the block) *)
        let pending = ref [] in
        let dead = Hashtbl.create 8 in
        let forget () =
          known := [];
          pending := []
        in
        let read loc = pending := List.filter (fun (l, _) -> disjoint l loc) !pending in
        let instrs = List.map (Ir_fold.map_instr resolve) b.instrs in
        let instrs =
          List.mapi
            (fun pos ins ->
              match ins with
              | Load (id, ty, p, _) -> (
                  match size ty with
                  | Some n when not (is_aggregate ty) -> (
                      let loc = (form_of p, n) in
                      match
                        List.find_opt (fun (l, t, _) -> l = loc && t = ty) !known
                      with
                      | Some (_, _, v) ->
                          Hashtbl.replace env id v;
                          None
                      | None ->
                          read loc;
                          known := (loc, ty, Local (id, ty)) :: !known;
                          Some ins)
                  | Some n ->
                      read (form_of p, n);
                      Some ins
                  | None ->
                      pending := [];
                      Some ins)
              | Store (ty, v, p, _) -> (
                  match size ty with
                  | Some n ->
                      let loc = (form_of p, n) in
                      pending :=
                        List.filter
                          (fun (l, at) ->
                            if covers loc l then (
                              Hashtbl.replace dead at ();
                              false)
                            else true)
                          !pending;
                      known := List.filter (fun (l, _, _) -> disjoint l loc) !known;
                      if not (is_aggregate ty) then known := (loc, ty, v) :: !known;
                      pending := (loc, pos) :: !pending;
                      Some ins
                  | None ->
                      forget ();
                      Some ins)
              | Load_volatile _ | Store_volatile _ | Call _ | Call_indirect _
              | Syscall _ | Trap ->
                  forget ();
                  Some ins
              | _ -> Some ins)
            instrs
        in
        let instrs =
          List.filteri (fun pos ins -> ins <> None && not (Hashtbl.mem dead pos)) instrs
          |> List.filter_map Fun.id
        in
        { b with instrs })
      f.blocks
  in
  {
    f with
    blocks =
      List.map
        (fun (b : block) ->
          {
            b with
            instrs = List.map (Ir_fold.map_instr resolve) b.instrs;
            terminator = Ir_fold.map_term resolve b.terminator;
          })
        blocks;
  }

(* Per-function frame *)

type frame = {
  addrs : addrs;
  slots : (int, int) Hashtbl.t;  (** SSA id -> rbp offset *)
  allocas : (int, int) Hashtbl.t;  (** alloca id -> rbp offset of its memory *)
  params : (string, int) Hashtbl.t;
  mutable size : int;
  homes : (Regalloc.key, A.reg) Hashtbl.t;  (** values that live in a register *)
  saved : (A.reg * int) list;  (** callee-saved registers used, and their save slots *)
}

let reserve fr bytes align =
  fr.size <- (fr.size + bytes + align - 1) / align * align;
  -fr.size

let build_frame ctx (f : func) =
  let addrs = analyse_addrs ctx f in
  let fr =
    {
      addrs;
      slots = Hashtbl.create 64;
      allocas = Hashtbl.create 8;
      params = Hashtbl.create 8;
      size = 0;
      homes =
        Regalloc.allocate
          ~skip:(fun id -> Hashtbl.mem addrs.skip id || Hashtbl.mem addrs.dead id)
          ~operands:(operands_view addrs) f;
      saved = [];
    }
  in
  List.iter
    (fun (p : param) -> Hashtbl.replace fr.params p.name (reserve fr 8 8))
    f.params;
  List.iter
    (fun (b : block) ->
      List.iter
        (fun ins ->
          (match ins with
          | Alloca (id, ty, align) ->
              Hashtbl.replace fr.allocas id
                (reserve fr (max 1 (size_of ctx ty)) (max 1 align))
          | _ -> ());
          match instruction_result ins with
          | Some (id, ty) when is_aggregate ty ->
              Hashtbl.replace fr.slots id (reserve fr (max 1 (size_of ctx ty)) 16)
          | Some (id, _) -> Hashtbl.replace fr.slots id (reserve fr 8 8)
          | None -> ())
        b.instrs)
    f.blocks;
  let used =
    Hashtbl.fold (fun _ r acc -> if List.mem r acc then acc else r :: acc) fr.homes []
  in
  let saved =
    List.filter_map
      (fun r -> if List.mem r used then Some (r, reserve fr 8 8) else None)
      Regalloc.callee_saved
  in
  let fr = { fr with saved } in
  fr.size <- (fr.size + 15) / 16 * 16;
  fr

let slot fr id =
  match Hashtbl.find_opt fr.slots id with
  | Some o -> A.Base (A.Rbp, o)
  | None -> unsupported "use of %%v%d before any definition" id

(* Values *)

(* Load a scalar value into [r], canonical (zero-extended). *)
let load ctx fr r v =
  let a = ctx.a in
  match v with
  | Const (ty, n) -> A.mov_imm a r (norm_const ty n)
  | Null _ | Zero _ | Undef _ -> A.mov_imm a r 0L
  | Local (id, ty) when is_aggregate ty -> A.lea a r (slot fr id)
  | Local (id, _) when Hashtbl.mem fr.addrs.skip id -> (
      let f = Hashtbl.find fr.addrs.forms id in
      match f with
      | { anchor = Sym s; idx = None; off } -> A.lea a r (A.Rip (s, off))
      | { anchor = Frame al; idx = None; off } ->
          A.lea a r (A.Base (A.Rbp, Hashtbl.find fr.allocas al + off))
      | _ -> invalid_arg "folded address used as a value")
  | Local (id, _) -> (
      match Hashtbl.find_opt fr.homes (Regalloc.V id) with
      | Some home -> A.mov_rr a r home
      | None -> A.load_zx a 8 r (slot fr id))
  | Param (name, _) -> (
      match Hashtbl.find_opt fr.homes (Regalloc.P name) with
      | Some home -> A.mov_rr a r home
      | None -> (
          match Hashtbl.find_opt fr.params name with
          | Some o -> A.load_zx a 8 r (A.Base (A.Rbp, o))
          | None -> unsupported "unknown parameter %s" name))
  | Global (name, _) -> A.lea a r (A.Rip (name, 0))
  | Const_vector _ -> unsupported "vector constants"

let load_signed ctx fr r v =
  load ctx fr r v;
  A.sign_extend ctx.a (bits (value_ty v)) r

(* Store [r] as the result [id] of type [ty], normalizing to canonical form. *)
let rec store_result ctx fr id ty r =
  A.zero_extend ctx.a (bits ty) r;
  put ctx fr id r

(* Write a 64-bit register that is already canonical to [id]'s home. *)
and put ctx fr id r =
  match Hashtbl.find_opt fr.homes (Regalloc.V id) with
  | Some home -> A.mov_rr ctx.a home r
  | None -> A.store ctx.a 8 (slot fr id) r

let home fr = function
  | Local (id, _) -> Hashtbl.find_opt fr.homes (Regalloc.V id)
  | Param (name, _) -> Hashtbl.find_opt fr.homes (Regalloc.P name)
  | _ -> None

(* [v] as a sign-extended 32-bit immediate equal to its canonical value *)
let imm32 = function
  | Const (ty, n) ->
      let c = norm_const ty n in
      if A.fits_i32_64 c then Some (Int64.to_int c) else None
  | Null _ | Zero _ | Undef _ -> Some 0
  | _ -> None

(* A register that holds [v]: its home, or [scratch] after a load. *)
let in_reg ctx fr v scratch =
  match home fr v with
  | Some r -> r
  | None ->
      load ctx fr scratch v;
      scratch

(* The memory operand for a form, loading its registers into the scratch
   registers [sb] and [si] when they have no home. *)
let mem_of_form ctx fr f ~sb ~si =
  let index () = Option.map (fun (v, sc) -> (in_reg ctx fr v si, sc)) f.idx in
  match f.anchor with
  | Sym s -> (
      match index () with
      | None -> A.Rip (s, f.off)
      | Some (index, scale) ->
          A.Sib { base = None; index; scale; disp = f.off; sym = Some s })
  | Frame al -> (
      let off = Hashtbl.find fr.allocas al + f.off in
      match index () with
      | None -> A.Base (A.Rbp, off)
      | Some (index, scale) ->
          A.Sib { base = Some A.Rbp; index; scale; disp = off; sym = None })
  | Reg v -> (
      let base = in_reg ctx fr v sb in
      match index () with
      | None -> A.Base (base, f.off)
      | Some (index, scale) ->
          A.Sib { base = Some base; index; scale; disp = f.off; sym = None })

(* The memory operand addressed by pointer [p] *)
let mem_at ctx fr p ~sb ~si =
  match p with
  | Local (id, _) when Hashtbl.mem fr.addrs.skip id ->
      mem_of_form ctx fr (Hashtbl.find fr.addrs.forms id) ~sb ~si
  | p -> A.Base (in_reg ctx fr p sb, 0)

(* Where to compute [id]: its home register unless that is in [avoid]. *)
let dest_reg fr id avoid =
  match Hashtbl.find_opt fr.homes (Regalloc.V id) with
  | Some r when not (List.mem r avoid) -> r
  | _ -> A.Rax

let avoid_of fr v = match home fr v with Some r -> [ r ] | None -> []

(* Set the flags for [cmp c x y]. *)
let compare_flags ctx fr c x y =
  let a = ctx.a in
  let w = bits (value_ty x) in
  if (match c with Slt | Sle | Sgt | Sge -> true | _ -> false) && w < 64 then begin
    load_signed ctx fr A.Rax x;
    load_signed ctx fr A.Rcx y;
    A.alu_rr a A.Cmp A.Rax A.Rcx
  end
  else
    let rx = in_reg ctx fr x A.Rax in
    match imm32 y with
    | Some k -> A.alu_ri a A.Cmp rx k
    | None -> A.alu_rr a A.Cmp rx (in_reg ctx fr y A.Rcx)

let mem_size _ctx ty =
  match ty with
  | I1 | I8 -> 1
  | I16 -> 2
  | I32 -> 4
  | I64 | Pointer _ -> 8
  | ty ->
      ignore (bits ty);
      unsupported "memory access of this type"

(* Copy [n] bytes from the address in rsi to the address in rdi. *)
let copy_bytes ctx n =
  A.mov_imm ctx.a A.Rcx (Int64.of_int n);
  A.rep_movsb ctx.a

(* Instructions *)

let signed_cmp = function Slt | Sle | Sgt | Sge -> true | _ -> false

let cond_of = function
  | Eq -> A.E
  | Ne -> A.Ne
  | Slt -> A.L
  | Sle -> A.Le
  | Sgt -> A.G
  | Sge -> A.Ge
  | Ult -> A.B
  | Ule -> A.Be
  | Ugt -> A.A
  | Uge -> A.Ae
  | _ -> unsupported "floating-point comparison"

let negate = function
  | A.E -> A.Ne
  | A.Ne -> A.E
  | A.L -> A.Ge
  | A.Ge -> A.L
  | A.Le -> A.G
  | A.G -> A.Le
  | A.B -> A.Ae
  | A.Ae -> A.B
  | A.Be -> A.A
  | A.A -> A.Be
  | _ -> invalid_arg "negate"

let arg_regs = [| A.Rdi; A.Rsi; A.Rdx; A.Rcx; A.R8; A.R9 |]

let gen_call ctx fr result ret_ty target args =
  let a = ctx.a in
  let n = List.length args in
  let stack_args = max 0 (n - 6) in
  let pad = if stack_args mod 2 = 1 then 8 else 0 in
  if pad > 0 then A.alu_ri a A.Sub A.Rsp pad;
  (* Stack arguments, last first *)
  List.iteri
    (fun i (_, _, v) ->
      if n - 1 - i >= 6 then begin
        load ctx fr A.Rax v;
        A.push a A.Rax
      end)
    (List.rev args);
  List.iteri (fun i (_, _, v) -> if i < 6 then load ctx fr arg_regs.(i) v) args;
  (match target with
  | `Direct name -> (
      match Hashtbl.find_opt ctx.func_labels name with
      | Some l -> A.call_label a l
      | None ->
          unsupported
            "call to `%s`, which has no Fas definition (C functions are not available)"
            name)
  | `Indirect v ->
      load ctx fr A.R11 v;
      A.call_r a A.R11);
  let cleanup = (8 * stack_args) + pad in
  if cleanup > 0 then A.alu_ri a A.Add A.Rsp cleanup;
  match result with
  | Some id when ret_ty <> Void -> store_result ctx fr id ret_ty A.Rax
  | _ -> ()

let gen_intrinsic ctx fr result ret_ty name args =
  let a = ctx.a in
  let arg i =
    let _, _, v = List.nth args i in
    v
  in
  let prefix p = String.starts_with ~prefix:p name in
  let result_bits () = bits ret_ty in
  let store r =
    match result with Some id -> store_result ctx fr id ret_ty r | None -> ()
  in
  if prefix "llvm.lifetime." then ()
  else if prefix "llvm.ctpop." then begin
    (* Bit-by-bit population count; M2 favours simplicity. *)
    load ctx fr A.Rcx (arg 0);
    A.mov_imm a A.Rax 0L;
    let top = A.new_label a and done_ = A.new_label a in
    A.place a top;
    A.test_rr a A.Rcx A.Rcx;
    A.jcc a A.E done_;
    A.mov_rr a A.Rdx A.Rcx;
    A.alu_ri a A.And A.Rdx 1;
    A.alu_rr a A.Add A.Rax A.Rdx;
    (* shr rcx, 1 *)
    A.ext_r a [ 0xd1 ] 5 A.Rcx;
    A.jmp a top;
    A.place a done_;
    store A.Rax
  end
  else if prefix "llvm.cttz." || prefix "llvm.ctlz." then begin
    let w = result_bits () in
    load ctx fr A.Rax (arg 0);
    A.mov_imm a A.Rdx (Int64.of_int w);
    let zero = A.new_label a in
    A.test_rr a A.Rax A.Rax;
    A.jcc a A.E zero;
    if prefix "llvm.cttz." then A.rr a [ 0x0f; 0xbc ] ~reg:A.Rdx ~rm:A.Rax (* bsf *)
    else begin
      A.rr a [ 0x0f; 0xbd ] ~reg:A.Rdx ~rm:A.Rax (* bsr: index of the top bit *);
      A.mov_imm a A.Rcx (Int64.of_int (w - 1));
      A.alu_rr a A.Sub A.Rcx A.Rdx;
      A.mov_rr a A.Rdx A.Rcx
    end;
    A.place a zero;
    store A.Rdx
  end
  else if prefix "llvm.fshl." || prefix "llvm.fshr." then begin
    (* Only rotations (both inputs equal) come from Fas: rotl/rotr. *)
    let w = result_bits () in
    load ctx fr A.Rax (arg 0);
    load ctx fr A.Rcx (arg 2);
    A.alu_ri a A.And A.Rcx (w - 1);
    let left = prefix "llvm.fshl." in
    (* rot = (x << k) | (x >> (w - k)) computed in 64 bits on a canonical x *)
    A.mov_rr a A.Rdx A.Rax;
    A.shift_cl a (if left then A.Shl else A.Shr) A.Rax;
    A.mov_rr a A.Rsi A.Rcx;
    A.mov_imm a A.Rcx (Int64.of_int w);
    A.alu_rr a A.Sub A.Rcx A.Rsi;
    (* shifting a 64-bit value by 64 is a no-op on x86; mask k = 0 below *)
    A.shift_cl a (if left then A.Shr else A.Shl) A.Rdx;
    A.test_rr a A.Rsi A.Rsi;
    let skip = A.new_label a in
    A.jcc a A.E skip;
    A.alu_rr a A.Or A.Rax A.Rdx;
    A.place a skip;
    store A.Rax
  end
  else if
    prefix "llvm.uadd.sat." || prefix "llvm.usub.sat." || prefix "llvm.sadd.sat."
    || prefix "llvm.ssub.sat."
  then begin
    let w = result_bits () in
    if w >= 64 then unsupported "64-bit saturating arithmetic (M2)";
    let signed = prefix "llvm.s" in
    let add = prefix "llvm.uadd" || prefix "llvm.sadd" in
    let ld r v = if signed then load_signed ctx fr r v else load ctx fr r v in
    ld A.Rax (arg 0);
    ld A.Rcx (arg 1);
    A.alu_rr a (if add then A.Add else A.Sub) A.Rax A.Rcx;
    let lo, hi =
      if signed then
        ( Int64.neg (Int64.shift_left 1L (w - 1)),
          Int64.pred (Int64.shift_left 1L (w - 1)) )
      else (0L, Int64.pred (Int64.shift_left 1L w))
    in
    A.mov_imm a A.Rcx lo;
    A.alu_rr a A.Cmp A.Rax A.Rcx;
    A.cmov a A.L A.Rax A.Rcx;
    A.mov_imm a A.Rcx hi;
    A.alu_rr a A.Cmp A.Rax A.Rcx;
    A.cmov a A.G A.Rax A.Rcx;
    store A.Rax
  end
  else unsupported "intrinsic %s" name

let gen_instr ctx fr ins =
  let a = ctx.a in
  match ins with
  | Location _ -> ()
  | Bin (id, _, _, _, _) when Hashtbl.mem fr.addrs.dead id -> ()
  | Bin (id, op, ty, x, y) -> (
      match op with
      | Add | Sub | And | Or | Xor | Mul ->
          let commutes = op <> Sub in
          (* When the result's home already holds [y], compute [y op x]. *)
          let x, y =
            match (Hashtbl.find_opt fr.homes (Regalloc.V id), home fr y) with
            | Some r, Some ry when commutes && r = ry && home fr x <> Some r -> (y, x)
            | _ -> (x, y)
          in
          let x, y =
            if commutes && imm32 x <> None && imm32 y = None then (y, x) else (x, y)
          in
          let d = dest_reg fr id (avoid_of fr y) in
          let alu =
            match op with
            | Add -> A.Add
            | Sub -> A.Sub
            | And -> A.And
            | Or -> A.Or
            | Xor -> A.Xor
            | _ -> A.Cmp
          in
          (match (op, imm32 y) with
          | Mul, Some k -> A.imul_rri a d (in_reg ctx fr x d) k
          | Mul, None ->
              load ctx fr d x;
              A.imul_rr a d (in_reg ctx fr y A.Rcx)
          | _, Some k ->
              load ctx fr d x;
              A.alu_ri a alu d k
          | _, None ->
              load ctx fr d x;
              A.alu_rr a alu d (in_reg ctx fr y A.Rcx));
          (* Bitwise operations on canonical (zero-extended) inputs stay
             canonical. *)
          if match op with And | Or | Xor -> true | _ -> false then put ctx fr id d
          else store_result ctx fr id ty d
      | Udiv | Urem ->
          load ctx fr A.Rax x;
          load ctx fr A.Rcx y;
          A.mov_imm a A.Rdx 0L;
          A.div a A.Rcx;
          put ctx fr id (if op = Udiv then A.Rax else A.Rdx)
      | Sdiv | Srem ->
          load_signed ctx fr A.Rax x;
          load_signed ctx fr A.Rcx y;
          A.cqo a;
          A.idiv a A.Rcx;
          store_result ctx fr id ty (if op = Sdiv then A.Rax else A.Rdx)
      | Shl | Lshr | Ashr ->
          let d = dest_reg fr id (avoid_of fr y) in
          if op = Ashr then load_signed ctx fr d x else load ctx fr d x;
          let sh = match op with Shl -> A.Shl | Lshr -> A.Shr | _ -> A.Sar in
          (match y with
          | Const (_, k) -> A.shift_imm a sh d (Int64.to_int k)
          | _ ->
              load ctx fr A.Rcx y;
              A.shift_cl a sh d);
          if op = Lshr then put ctx fr id d else store_result ctx fr id ty d
      | Fadd | Fsub | Fmul | Fdiv -> unsupported "floating-point arithmetic")
  | Fneg _ -> unsupported "floating-point negation"
  | Cmp (id, c, _, x, y) ->
      compare_flags ctx fr c x y;
      A.setcc a (cond_of c) A.Rax;
      store_result ctx fr id I1 A.Rax
  | Alloca (id, _, _) when Hashtbl.mem fr.addrs.skip id -> ()
  | Alloca (id, _, _) ->
      A.lea a A.Rax (A.Base (A.Rbp, Hashtbl.find fr.allocas id));
      put ctx fr id A.Rax
  | Load (id, ty, p, _) | Load_volatile (id, ty, p, _) ->
      if is_aggregate ty then begin
        load ctx fr A.Rsi p;
        A.lea a A.Rdi (slot fr id);
        copy_bytes ctx (size_of ctx ty)
      end
      else begin
        let mem = mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx in
        let d = dest_reg fr id [] in
        A.load_zx a (mem_size ctx ty) d mem;
        put ctx fr id d
      end
  | Store (ty, v, p, _) | Store_volatile (ty, v, p, _) ->
      if is_aggregate ty then begin
        load ctx fr A.Rdi p;
        match v with
        | Zero _ | Undef _ | Null _ ->
            A.mov_imm a A.Rax 0L;
            A.mov_imm a A.Rcx (Int64.of_int (size_of ctx ty));
            A.rep_stosb a
        | Local _ ->
            load ctx fr A.Rsi v;
            copy_bytes ctx (size_of ctx ty)
        | _ -> unsupported "aggregate store of a constant"
      end
      else begin
        let src = in_reg ctx fr v A.Rcx in
        A.store a (mem_size ctx ty) (mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx) src
      end
  | (Gep (id, _, _, _) | Global_ptr (id, _, _) | String_ptr (id, _, _))
    when Hashtbl.mem fr.addrs.skip id ->
      ()
  | Gep (id, _, _, _) when Hashtbl.mem fr.addrs.forms id ->
      let mem =
        mem_of_form ctx fr (Hashtbl.find fr.addrs.forms id) ~sb:A.Rax ~si:A.Rdx
      in
      let d = dest_reg fr id [] in
      A.lea a d mem;
      put ctx fr id d
  | Gep (id, ty, p, indices) ->
      load ctx fr A.Rax p;
      let step size v =
        match v with
        | Const (ity, n) ->
            let w = bits ity in
            let n =
              if w >= 64 then n
              else Int64.shift_right (Int64.shift_left n (64 - w)) (64 - w)
            in
            let off = Int64.mul n (Int64.of_int size) in
            if A.fits_i32_64 off then (
              if off <> 0L then A.alu_ri a A.Add A.Rax (Int64.to_int off))
            else begin
              A.mov_imm a A.Rcx off;
              A.alu_rr a A.Add A.Rax A.Rcx
            end
        | _ ->
            load_signed ctx fr A.Rcx v;
            if size <> 1 then begin
              A.mov_imm a A.Rdx (Int64.of_int size);
              A.imul_rr a A.Rcx A.Rdx
            end;
            A.alu_rr a A.Add A.Rax A.Rcx
      in
      let rec walk ty = function
        | [] -> ()
        | idx :: rest -> (
            match ty with
            | Struct name ->
                let n =
                  match idx with
                  | Zero -> 0
                  | Index (Const (_, n)) -> Int64.to_int n
                  | Index _ -> unsupported "struct field index is not constant"
                in
                let off = field_offset ctx name n in
                if off <> 0 then A.alu_ri a A.Add A.Rax off;
                let d =
                  List.find (fun (d : struct_def) -> d.name = name) ctx.m.structs
                in
                walk (List.nth d.fields n) rest
            | Array (_, elem) | Vector (_, elem) ->
                (match idx with Zero -> () | Index v -> step (size_of ctx elem) v);
                walk elem rest
            | _ -> unsupported "gep into a scalar")
      in
      (match indices with
      | [] -> ()
      | first :: rest ->
          (match first with Zero -> () | Index v -> step (size_of ctx ty) v);
          walk ty rest);
      put ctx fr id A.Rax
  | Cast (id, kind, from, v, target) -> (
      match kind with
      | "zext" | "bitcast" | "ptrtoint" | "inttoptr" | "trunc" ->
          if kind = "bitcast" && (is_aggregate target || is_aggregate from) then
            unsupported "aggregate bitcast";
          load ctx fr A.Rax v;
          store_result ctx fr id target A.Rax
      | "sext" ->
          load ctx fr A.Rax v;
          A.sign_extend a (bits from) A.Rax;
          store_result ctx fr id target A.Rax
      | k -> unsupported "cast %s" k)
  | Call (result, _, ret, name, args) ->
      if String.starts_with ~prefix:"llvm." name then
        gen_intrinsic ctx fr result ret name args
      else gen_call ctx fr result ret (`Direct name) args
  | Call_indirect (result, _, ret, callee, args) ->
      gen_call ctx fr result ret (`Indirect callee) args
  | Phi _ -> () (* copied in from the incoming slot at block entry *)
  | Select (id, c, x, y) ->
      let ty = value_ty x in
      if is_aggregate ty then unsupported "aggregate select";
      load ctx fr A.Rax y;
      load ctx fr A.Rcx x;
      load ctx fr A.Rdx c;
      A.test_rr a A.Rdx A.Rdx;
      A.cmov a A.Ne A.Rax A.Rcx;
      store_result ctx fr id ty A.Rax
  | Extract _ | Insert _ | Shuffle_zero _ | Shufflevector _ ->
      unsupported "vector operations"
  | String_ptr (id, index, _) ->
      A.lea a A.Rax (A.Rip (Printf.sprintf ".str.%d" index, 0));
      put ctx fr id A.Rax
  | Global_ptr (id, name, _) ->
      A.lea a A.Rax (A.Rip (name, 0));
      put ctx fr id A.Rax
  | Trap -> A.ud2 a
  | Syscall (id, args) ->
      let regs = [| A.Rax; A.Rdi; A.Rsi; A.Rdx; A.R10; A.R8; A.R9 |] in
      (* Load the number last: rax is also a scratch register for nothing
         here, but keep the order obvious. *)
      List.iteri (fun i v -> if i > 0 then load ctx fr regs.(i) v) args;
      load ctx fr A.Rax (List.hd args);
      A.syscall a;
      put ctx fr id A.Rax

(* Edge copies: the phis of [target] take their values for the edge from
   [from_id], as one parallel move straight into each phi's home. rax breaks
   cycles and rcx carries memory-to-memory moves. *)
type loc = R of A.reg | M of int | C of value

let edge_moves ctx fr blocks from_id target =
  ignore ctx;
  let b : block = Hashtbl.find blocks target in
  let loc_of v =
    match (home fr v, v) with
    | Some r, _ -> R r
    | None, Local (id, _) when Hashtbl.mem fr.addrs.skip id -> C v
    | None, Local (id, ty) when not (is_aggregate ty) -> (
        match slot fr id with A.Base (_, o) -> M o | _ -> C v)
    | None, Param (name, _) -> M (Hashtbl.find fr.params name)
    | _ -> C v
  in
  let moves =
    List.filter_map
      (function
        | Phi (id, ty, incoming) -> (
            if is_aggregate ty then unsupported "aggregate phi";
            match List.find_opt (fun (_, p) -> p = from_id) incoming with
            | Some (v, _) ->
                let dst = loc_of (Local (id, ty)) and src = loc_of v in
                if dst = src then None else Some (dst, src)
            | None ->
                unsupported "phi in b%d has no value for edge from b%d" target from_id)
        | _ -> None)
      b.instrs
  in
  moves

let edge_copies ctx fr blocks from_id target =
  let a = ctx.a in
  let moves = edge_moves ctx fr blocks from_id target in
  let emit dst src =
    match (dst, src) with
    | R r, R q -> A.mov_rr a r q
    | R r, M o -> A.load_zx a 8 r (A.Base (A.Rbp, o))
    | R r, C v -> load ctx fr r v
    | M o, R q -> A.store a 8 (A.Base (A.Rbp, o)) q
    | M o, M o2 ->
        A.load_zx a 8 A.Rcx (A.Base (A.Rbp, o2));
        A.store a 8 (A.Base (A.Rbp, o)) A.Rcx
    | M o, C v ->
        load ctx fr A.Rcx v;
        A.store a 8 (A.Base (A.Rbp, o)) A.Rcx
    | C _, _ -> invalid_arg "edge_copies"
  in
  let pending = ref moves in
  while !pending <> [] do
    let is_source l = List.exists (fun (_, s) -> s = l) !pending in
    match List.find_opt (fun (d, _) -> not (is_source d)) !pending with
    | Some ((d, s) as m) ->
        emit d s;
        pending := List.filter (fun x -> x != m) !pending
    | None ->
        let d, _ = List.hd !pending in
        emit (R A.Rax) d;
        pending := List.map (fun (d', s) -> (d', if s = d then R A.Rax else s)) !pending
  done

let gen_function ctx (f : func) =
  let a = ctx.a in
  let fr = build_frame ctx f in
  let blocks = Hashtbl.create 16 in
  List.iter (fun (b : block) -> Hashtbl.replace blocks b.id b) f.blocks;
  let labels = Hashtbl.create 16 in
  List.iter (fun (b : block) -> Hashtbl.replace labels b.id (A.new_label a)) f.blocks;
  let block_label id = Hashtbl.find labels id in
  A.align a 16;
  A.place a (Hashtbl.find ctx.func_labels f.name);
  let start = A.pos a in
  A.push a A.Rbp;
  A.mov_rr a A.Rbp A.Rsp;
  if fr.size > 0 then A.alu_ri a A.Sub A.Rsp fr.size;
  List.iteri
    (fun i (p : param) ->
      let dst = A.Base (A.Rbp, Hashtbl.find fr.params p.name) in
      if i < 6 then A.store a 8 dst arg_regs.(i)
      else begin
        A.load_zx a 8 A.Rax (A.Base (A.Rbp, 16 + (8 * (i - 6))));
        A.store a 8 dst A.Rax
      end)
    f.params;
  List.iter (fun (r, o) -> A.store a 8 (A.Base (A.Rbp, o)) r) fr.saved;
  List.iter
    (fun (p : param) ->
      match Hashtbl.find_opt fr.homes (Regalloc.P p.name) with
      | Some home -> A.load_zx a 8 home (A.Base (A.Rbp, Hashtbl.find fr.params p.name))
      | None -> ())
    f.params;
  let epilogue () =
    List.iter (fun (r, o) -> A.load_zx a 8 r (A.Base (A.Rbp, o))) fr.saved;
    A.leave a;
    A.ret a
  in
  (* A compare used only by its block's branch becomes cmp + jcc. *)
  let uses = Hashtbl.create 64 in
  let count v =
    match v with
    | Local (id, _) ->
        Hashtbl.replace uses id (1 + Option.value ~default:0 (Hashtbl.find_opt uses id))
    | _ -> ()
  in
  List.iter
    (fun (b : block) ->
      List.iter (Regalloc.iter_operands count) b.instrs;
      Regalloc.iter_term_operands count b.terminator)
    f.blocks;
  let fused (b : block) =
    match (List.rev b.instrs, b.terminator) with
    | (Cmp (id, c, ty, x, y) as cmp) :: _, CondBr (Local (cid, _), _, _)
      when cid = id
           && Hashtbl.find_opt uses id = Some 1
           && (match c with
             | Eq | Ne | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge -> true
             | _ -> false)
           && match ty with F32 | F64 | Vector _ -> false | _ -> true ->
        Some (cmp, c, x, y)
    | _ -> None
  in
  let order = Array.of_list (List.map (fun (b : block) -> b.id) f.blocks) in
  let next_block = ref (-1) in
  let has_phis id =
    List.exists (function Phi _ -> true | _ -> false) (Hashtbl.find blocks id).instrs
  in
  (* load; op; store back to the same place, with the loaded and computed
     values used nowhere else, becomes one `op [mem], y`. *)
  let same_place p q =
    p = q
    ||
    match (p, q) with
    | Local (i, _), Local (j, _) -> (
        match
          (Hashtbl.find_opt fr.addrs.forms i, Hashtbl.find_opt fr.addrs.forms j)
        with
        | Some f, Some g ->
            Hashtbl.mem fr.addrs.skip i && Hashtbl.mem fr.addrs.skip j && f = g
        | _ -> false)
    | _ -> false
  in
  let rmw ins rest =
    let rec skip = function
      | Location _ :: r -> skip r
      | (Gep (id, _, _, _) | Global_ptr (id, _, _) | String_ptr (id, _, _)) :: r
        when Hashtbl.mem fr.addrs.skip id ->
          skip r
      | r -> r
    in
    let once id = Hashtbl.find_opt uses id = Some 1 in
    match ins with
    | Load (l, ty, p, _)
      when once l && match ty with I8 | I16 | I32 | I64 -> true | _ -> false -> (
        match skip rest with
        | Bin (r, op, ty', x, y) :: rest
          when ty' = ty && once r
               && match op with Add | Sub | And | Or | Xor -> true | _ -> false -> (
            let operand =
              match (x, y) with
              | Local (i, _), y when i = l -> Some y
              | x, Local (i, _) when i = l && op <> Sub -> Some x
              | _ -> None
            in
            match (operand, skip rest) with
            | Some y, Store (ty'', Local (s, _), p', _) :: rest
              when ty'' = ty && s = r && same_place p p'
                   && match y with Local (i, _) -> i <> l | _ -> true ->
                let alu =
                  match op with
                  | Add -> A.Add
                  | Sub -> A.Sub
                  | And -> A.And
                  | Or -> A.Or
                  | _ -> A.Xor
                in
                let size = mem_size ctx ty in
                let imm =
                  match y with
                  | Const (t, n) when size < 8 -> Some (Int64.to_int (norm_const t n))
                  | _ -> imm32 y
                in
                (match imm with
                | Some k -> A.alu_mi a alu size (mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx) k
                | None ->
                    let src = in_reg ctx fr y A.Rcx in
                    A.alu_mr a alu size (mem_at ctx fr p ~sb:A.Rax ~si:A.Rdx) src);
                Some rest
            | _ -> None)
        | _ -> None)
    | _ -> None
  in
  let position = Hashtbl.create 16 in
  Array.iteri (fun i id -> Hashtbl.replace position id i) order;
  let fusions = Hashtbl.create 16 in
  List.iter
    (fun (b : block) ->
      match fused b with Some f -> Hashtbl.replace fusions b.id f | None -> ())
    f.blocks;
  (* Instructions that emit no code of their own *)
  let silent ins =
    match ins with
    | Phi _ | Location _ -> true
    | Bin (id, _, _, _, _) -> Hashtbl.mem fr.addrs.dead id
    | Gep (id, _, _, _)
    | Global_ptr (id, _, _)
    | String_ptr (id, _, _)
    | Alloca (id, _, _) ->
        Hashtbl.mem fr.addrs.skip id
    | _ -> false
  in
  let plain from_id target = edge_moves ctx fr blocks from_id target = [] in
  (* A block with no code and no copies on to its jump target can be jumped
     over: jumps to it go straight on. *)
  let rec forward seen id =
    let b : block = Hashtbl.find blocks id in
    match b.terminator with
    | Br next
      when (not (List.mem id seen)) && List.for_all silent b.instrs && plain id next ->
        forward (id :: seen) next
    | _ -> id
  in
  let forward id = forward [] id in
  let rotating = ref false in
  (* A loop header that only compares and branches is copied to the end of
     each back edge, so the loop runs one branch per iteration. *)
  let rec jump_to from_id target =
    edge_copies ctx fr blocks from_id target;
    let t = forward target in
    let header = Hashtbl.find blocks t in
    match (Hashtbl.find_opt fusions t, header.terminator) with
    | Some ((cmp, c, x, y) : instr * cmp * value * value), CondBr (_, yes, no)
      when (not !rotating) && t <> !next_block
           && Hashtbl.find position t <= Hashtbl.find position from_id
           && List.for_all (fun ins -> ins == cmp || silent ins) header.instrs ->
        rotating := true;
        compare_flags ctx fr c x y;
        branch t (cond_of c) yes no;
        rotating := false
    | _ -> if target <> !next_block && t <> !next_block then A.jmp a (block_label t)
  (* Branch on condition [c] (already in the flags) to [yes] or [no]. *)
  and branch from_id c yes no =
    let py = plain from_id yes and pn = plain from_id no in
    if py && pn then begin
      let y = forward yes and n = forward no in
      if y = !next_block || yes = !next_block then A.jcc a (negate c) (block_label n)
      else begin
        A.jcc a c (block_label y);
        if n <> !next_block && no <> !next_block then A.jmp a (block_label n)
      end
    end
    else if py then begin
      A.jcc a c (block_label (forward yes));
      jump_to from_id no
    end
    else if pn then begin
      A.jcc a (negate c) (block_label (forward no));
      jump_to from_id yes
    end
    else begin
      let other = A.new_label a in
      A.jcc a (negate c) other;
      edge_copies ctx fr blocks from_id yes;
      A.jmp a (block_label yes);
      A.place a other;
      jump_to from_id no
    end
  in
  List.iteri
    (fun i (b : block) ->
      next_block := if i + 1 < Array.length order then order.(i + 1) else -1;
      A.place a (block_label b.id);
      let fusion = fused b in
      let rec emit = function
        | [] -> ()
        | ins :: rest -> (
            match rmw ins rest with
            | Some rest -> emit rest
            | None ->
                (match fusion with
                | Some (cmp, _, _, _) when ins == cmp -> ()
                | _ -> gen_instr ctx fr ins);
                emit rest)
      in
      emit b.instrs;
      match b.terminator with
      | Ret None -> epilogue ()
      | Ret (Some (_, v)) ->
          load ctx fr A.Rax v;
          epilogue ()
      | Br target -> jump_to b.id target
      | CondBr (_, yes, no) when fusion <> None ->
          let _, c, x, y = Option.get fusion in
          compare_flags ctx fr c x y;
          branch b.id (cond_of c) yes no
      | CondBr (c, yes, no) ->
          load ctx fr A.Rax c;
          A.test_rr a A.Rax A.Rax;
          branch b.id A.Ne yes no
      | Switch (ty, v, cases, default) ->
          load ctx fr A.Rax v;
          List.iter
            (fun (k, target) ->
              A.mov_imm a A.Rcx (norm_const ty k);
              A.alu_rr a A.Cmp A.Rax A.Rcx;
              if has_phis target then begin
                (* not the last code of the block: never elide this jump *)
                let next = A.new_label a in
                A.jcc a A.Ne next;
                edge_copies ctx fr blocks b.id target;
                A.jmp a (block_label target);
                A.place a next
              end
              else A.jcc a A.E (block_label target))
            cases;
          jump_to b.id default
      | Unreachable -> A.ud2 a)
    f.blocks;
  A.pos a - start

(* Data *)

type data_out = {
  buf : Buffer.t;
  mutable relocs : A.reloc list;
  mutable syms : Elf64.symbol list;
}

let place_data (d : data_out) section ~name ~align ~size write =
  while Buffer.length d.buf mod max 1 align <> 0 do
    Buffer.add_char d.buf '\000'
  done;
  let offset = Buffer.length d.buf in
  write offset;
  d.syms <- { Elf64.name; section; offset; size; func = false } :: d.syms

let int_le buf size v =
  for i = 0 to size - 1 do
    Buffer.add_char buf
      (Char.unsafe_chr (Int64.to_int (Int64.shift_right_logical v (8 * i)) land 0xff))
  done

let compile (m : module_) =
  try
    let m = Mem2reg.cleanup (Ir_fold.module_ (Mem2reg.module_ m)) in
    let m =
      let ctx = { m; a = A.create (); func_labels = Hashtbl.create 1 } in
      {
        m with
        funcs = List.map (fun f -> forward_memory ctx (Mem2reg.merge f)) m.funcs;
      }
    in
    let m =
      Mem2reg.cleanup
        (Ir_fold.module_ (Mem2reg.split_addresses (Mem2reg.reassociate m)))
    in
    if Sys.getenv_opt "FAS_NATIVE_IR" = Some "1" then prerr_string (Ir.render_debug m);
    let a = A.create () in
    let ctx = { m; a; func_labels = Hashtbl.create 64 } in
    let defined = List.filter (fun (f : func) -> f.blocks <> []) m.funcs in
    List.iter
      (fun (f : func) -> Hashtbl.replace ctx.func_labels f.name (A.new_label a))
      defined;
    if not (Hashtbl.mem ctx.func_labels "main") then unsupported "no `main` function";
    (* _start: argc/argv from the initial stack, call main, exit_group(result) *)
    A.alu_rr a A.Xor A.Rbp A.Rbp;
    A.load_zx a 8 A.Rdi (A.Base (A.Rsp, 0));
    A.lea a A.Rsi (A.Base (A.Rsp, 8));
    A.alu_ri a A.And A.Rsp (-16);
    A.call_label a (Hashtbl.find ctx.func_labels "main");
    A.rr a ~w:false [ 0x89 ] ~reg:A.Rax ~rm:A.Rdi;
    A.mov_imm a A.Rax 231L;
    A.syscall a;
    A.ud2 a;
    let start_size = A.pos a in
    let sizes = List.map (fun (f : func) -> (f, gen_function ctx f)) defined in
    let text, text_relocs = A.finish a in
    let func_syms =
      {
        Elf64.name = "_start";
        section = Elf64.Text;
        offset = 0;
        size = start_size;
        func = true;
      }
      :: List.map
           (fun ((f : func), size) ->
             let offset =
               Option.get (A.label_offset a (Hashtbl.find ctx.func_labels f.name))
             in
             { Elf64.name = f.name; section = Elf64.Text; offset; size; func = true })
           sizes
    in
    let rodata = { buf = Buffer.create 256; relocs = []; syms = [] } in
    let data = { buf = Buffer.create 256; relocs = []; syms = [] } in
    let bss = ref 0 and bss_syms = ref [] in
    List.iter
      (function
        | String_global { name; bytes } ->
            place_data rodata Elf64.Rodata ~name ~align:1 ~size:(String.length bytes)
              (fun _ -> Buffer.add_string rodata.buf bytes)
        | Array_global { name; elem_ty; elems; align } ->
            let es = size_of ctx elem_ty in
            place_data rodata Elf64.Rodata ~name ~align
              ~size:(es * List.length elems)
              (fun _ -> List.iter (int_le rodata.buf es) elems)
        | Storage_global { name; size; bytes; pointers; readonly; align; linkage; _ } ->
            (match linkage with
            | Ast.Import_c | Ast.Import_const_c ->
                unsupported "imported C global `%s`" name
            | _ -> ());
            let zero =
              match bytes with
              | None -> true
              | Some b -> String.for_all (( = ) '\000') b
            in
            if zero && pointers = [] && not readonly then begin
              bss := (!bss + align - 1) / max 1 align * max 1 align;
              bss_syms :=
                { Elf64.name; section = Elf64.Bss; offset = !bss; size; func = false }
                :: !bss_syms;
              bss := !bss + size
            end
            else
              let d, section =
                if readonly then (rodata, Elf64.Rodata) else (data, Elf64.Data)
              in
              place_data d section ~name ~align ~size (fun offset ->
                  (match bytes with
                  | Some b -> Buffer.add_string d.buf b
                  | None -> Buffer.add_string d.buf (String.make size '\000'));
                  List.iter
                    (fun (o, symbol, addend) ->
                      d.relocs <-
                        A.Abs64 { offset = offset + o; symbol; addend } :: d.relocs)
                    pointers))
      m.globals;
    let image =
      Elf64.link
        {
          text;
          text_relocs;
          rodata = Buffer.to_bytes rodata.buf;
          rodata_relocs = rodata.relocs;
          data = Buffer.to_bytes data.buf;
          data_relocs = data.relocs;
          bss_size = !bss;
          symbols = func_syms @ rodata.syms @ data.syms @ !bss_syms;
          entry = "_start";
        }
    in
    Ok image
  with
  | Unsupported message -> Error ("native backend: " ^ message)
  | Invalid_argument message -> Error ("native backend internal error: " ^ message)
