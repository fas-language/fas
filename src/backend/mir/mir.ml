(* Mir: the machine IR of the native x86-64 backend (roadmap v0.4).

     Fir --Mir_isel--> Mir over virtual registers
         --Mir_ra----> physical registers, spill/reload items
         --Mir_check-> independent proof that every use reads its value
         --Mir_emit--> frame layout, x86 bytes, ELF object

   An instruction is one real x86 instruction (or a tiny fixed pseudo that
   the emitter expands, such as [Frem87]), written over registers that are
   either virtual ([V n], its class in [func.vcls]) or physical ([G] for a
   GPR, [X] for an xmm register). Fixed-register needs (call arguments,
   rax/rdx around div, rcx for a shift count, rdi/rsi/rcx for rep movsb) are
   met by instruction selection with explicit copies into physical
   registers; the allocator's only duty there is to never put a virtual
   register into a physical one that holds such a value at that point.

   Conventions every selection rule keeps:
   - An integer iN (N <= 64) or a pointer lives in a GPR, zero-extended to
     64 bits ("canonical"). Rules that need a signed view sign-extend a copy.
   - f32/f64 live in the low lane of an xmm register (upper lanes are
     garbage). In memory an f32 slot holds 8 bytes, of which the low 4 count.
   - Flags: an instruction that reads flags follows the instruction that set
     them, with at most register copies, immediates loaded by [Movi] (always
     a real mov, never xor), loads and stores in between. The allocator only
     inserts plain movs, so flags survive its reloads and spills.
   - Virtual registers need not be in SSA form: two-address instructions
     ([Alu (Add, d, s)]) read and write the same virtual register, and block
     parameters are written once per incoming edge. *)

type rc = Gpr | Xmm

type reg =
  | V of int  (** virtual register *)
  | G of X64.reg  (** physical general-purpose register *)
  | X of int  (** physical xmm0..xmm15 *)

type base =
  | Bnone
  | Breg of reg
  | Bslot of int  (** a frame slot (alloca, scratch or spill), rbp-relative *)
  | Bout  (** the outgoing argument area at rsp *)
  | Bin  (** the incoming stack arguments, at rbp + 16 *)
  | Bsym of string  (** rip-relative to a symbol; never with an index *)

type addr = { base : base; index : reg option; scale : int; disp : int }

let at base = { base; index = None; scale = 1; disp = 0 }
let at_reg r = at (Breg r)

(* What a call jumps to *)
type target =
  | Tlabel of string  (** a function defined in this object, called directly *)
  | Tsym of string  (** through the symbol (PLT in position-independent code) *)
  | Treg of reg
  | Ttlsgd of string
      (** general-dynamic TLS: __tls_get_addr(&sym's GOT pair), as the
          canonical [lea sym@tlsgd(%rip), %rdi; call __tls_get_addr@PLT]
          sequence the linker recognizes; the variable's address comes back
          in rax. An ordinary call otherwise (rdi is clobbered anyway) *)

(* x87 memory operand forms *)
type x87w = M32 | M64 | M80 | I64  (** f32, f64, f80; I64: fild / fisttp m64 *)

(* One x87 instruction (f80 arithmetic). The x87 register stack is not
   allocated: each selection rule pushes its operands, computes and pops
   everything again, so the stack is empty between rules, except that a
   call leaves an f80 result in st0 for the [Fstp] right after it and [Fld]
   puts one there just before a return. *)
type x87 =
  | Fld of x87w * addr  (** push *)
  | Fstp of x87w * addr  (** pop into memory *)
  | Farith of X64.fop  (** st1 = st1 op st0, pop (Fadd Fsub Fmul Fdiv) *)
  | Fremloop  (** st0 = fmod(st0, st1) by an fprem loop; clobbers rax and the flags *)
  | Fsqrt87
  | Frndint  (** in the control word's rounding mode *)
  | Fucomip  (** flags as ucomis for st0 vs st1, pop *)
  | Fpop
  | Fnstcw of addr
  | Fldcw of addr

(* An operand of an inline asm statement, already in its place: every
   register is physical (instruction selection chose them, see
   Mir_isel's asm rule) *)
type asm_opnd =
  | Areg of reg * int  (** a register and the operand's width in bits (an xmm: 32 or 64) *)
  | Amem of addr * int  (** a memory operand and its size in bytes *)
  | Aimm of int64  (** an immediate *)
  | Asym of string * int64  (** a symbol plus an addend, as an immediate *)
  | Anone  (** a flag output ("=@cc..."): the template cannot name it *)

type asm = {
  pieces : Fir_asm.piece list;  (** the template, in the dialect below *)
  intel : bool;
  opnds : asm_opnd array;  (** GCC's numbering: outputs, then inputs *)
  ain : reg list;  (** registers it reads: inputs, read-write outputs, memory addresses *)
  aout : reg list;  (** registers it writes: outputs *)
  aclob : reg list;  (** registers it destroys (the clobber list) *)
}

type ins =
  | Copy of reg * reg  (** dst, src; same class (64-bit mov or movaps) *)
  | Movi of reg * int64  (** a real mov (flags untouched) *)
  | Load of int * bool * reg * addr  (** bytes, sign-extend?, dst, address *)
  | Store of int * addr * reg  (** bytes, address, src *)
  | Fload of bool * reg * addr  (** double?, dst xmm *)
  | Fstore of bool * addr * reg
  | Xload of reg * addr  (** all 16 bytes of an xmm register (movdqu) *)
  | Xstore of addr * reg
  | Align_sp of int  (** rsp rounded down to a multiple of N (> 16), for a call whose stack arguments need it *)
  | Lea of reg * addr
  | Got of reg * string  (** dst = the symbol's GOT entry *)
  | Tp of reg  (** dst = the thread pointer (fs:0) *)
  | Tlsoff of reg * string
      (** dst = the thread-local symbol's offset from the thread pointer
          (initial-exec in position-independent code, else local-exec) *)
  | Alu of X64.alu * reg * reg  (** dst op= src, 64-bit; Cmp writes only flags *)
  | Alui of X64.alu * reg * int  (** dst op= imm32 *)
  | Test of reg * reg
  | Imul of reg * reg  (** dst *= src, 64-bit *)
  | Shift of X64.shift * reg  (** dst by cl (reads rcx) *)
  | Shifti of X64.shift * reg * int
  | Shd of bool * reg * reg  (** shld (true) / shrd dst, src, cl *)
  | Neg of reg
  | Not of reg
  | Bswap of reg
  | Ext of int * bool * reg * reg  (** from bits (8/16/32), signed?, dst, src *)
  | Mul1 of bool * reg  (** rdx:rax = rax * src (signed?) *)
  | Div of bool * reg  (** rax, rdx = rdx:rax / src, % src (signed?) *)
  | Cqo  (** rdx = sign of rax *)
  | Setcc of X64.cond * reg  (** dst = 0/1 *)
  | Cmov of X64.cond * reg * reg  (** if cond then dst = src *)
  | Bsf of reg * reg  (** dst undefined when src is 0 *)
  | Bsr of reg * reg
  | Popcnt of reg * reg
  | Fop of X64.fop * bool * reg * reg  (** dst op= src (scalar, double?) *)
  | Fsqrt of bool * reg * reg  (** dst = sqrt src *)
  | Ucomi of bool * reg * reg
  | Movgx of bool * reg * reg  (** xmm dst = the low 64 (true) / 32 bits of a GPR *)
  | Movxg of bool * reg * reg  (** GPR dst = the low 64 / 32 bits of an xmm (zero-extended) *)
  | Cvtsi of bool * reg * reg  (** xmm dst = float of a signed 64-bit GPR *)
  | Cvttsi of bool * reg * reg  (** GPR dst = truncating signed 64-bit conversion *)
  | Cvtff of bool * reg * reg  (** from double?, dst, src *)
  | Round of bool * reg * reg * int  (** roundsd/ss mode (0 even, 1 floor, 2 ceil, 3 trunc) *)
  | Frem87 of bool * int
      (** x87 fprem loop on frame slot [s]: [s] = fmod([s], [s + 8]); clobbers rax *)
  | X87 of x87
  | Call of { target : target; args : reg list; rets : reg list }
      (** [args] are the physical registers the call reads (al included for
          variadic calls), [rets] those it defines; every other caller-saved
          register is clobbered *)
  | Syscall of reg list  (** reads rax and the argument registers, defines rax *)
  | Rep_movsb  (** rdi, rsi, rcx *)
  | Rep_stosb  (** rdi, al, rcx *)
  | Std
  | Cld
  | Xchg of int * addr * reg  (** bytes; reg <-> memory (implicitly locked) *)
  | Xadd of int * addr * reg  (** lock xadd *)
  | Cmpxchg of int * addr * reg  (** lock cmpxchg [addr], reg (compares with rax) *)
  | Mfence
  | Ud2
  | Loc of int
      (** no code: the following instructions come from Fir location N
          (the line table's rows, see Mir_dwarf) *)
  | Salloc of reg * reg
      (** dynamic alloca: rsp -= size (a multiple of 16); dst = the new
          block, just above the outgoing argument area *)
  | Spget of reg  (** dst = rsp (stacksave) *)
  | Spset of reg  (** rsp = src (stackrestore) *)
  | Asm of asm
      (** GNU inline asm, every register physical; Mir_emit renders the
          template and assembles it with an external assembler *)

type term =
  | Jmp of int
  | Jcc of X64.cond * int * int  (** on the flags set at the block's end: taken, else *)
  | Ret of reg list  (** the physical registers holding the results *)
  | Jmpr of reg * int list  (** indirect jump; the possible target blocks *)
  | Asmgoto of int * int list
      (** after an asm goto: jump to the first block; the asm itself may jump
          to the others, by their block symbols *)
  | Halt  (** ud2: control never gets here *)

type block = { mutable code : ins list; mutable term : term }
(** [code] is in program order *)

type func = {
  name : string;
  vcls : rc Dynarray.t;  (** class of each virtual register *)
  blocks : block Dynarray.t;  (** block 0 is the entry *)
  slots : (int * int) Dynarray.t;  (** frame slots: size, alignment *)
  mutable out_size : int;  (** bytes of outgoing stack arguments needed *)
}

let create name =
  { name; vcls = Dynarray.create (); blocks = Dynarray.create (); slots = Dynarray.create (); out_size = 0 }

let new_vreg f cls =
  Dynarray.add_last f.vcls cls;
  V (Dynarray.length f.vcls - 1)

let new_block f =
  Dynarray.add_last f.blocks { code = []; term = Halt };
  Dynarray.length f.blocks - 1

let new_slot f ~size ~align =
  Dynarray.add_last f.slots (size, align);
  Dynarray.length f.slots - 1

(* ---------- operands ---------- *)

let gpr_args = [| X64.Rdi; Rsi; Rdx; Rcx; R8; R9 |]
let caller_saved = X64.[ Rax; Rcx; Rdx; Rsi; Rdi; R8; R9; R10; R11 ]
let callee_saved = X64.[ Rbx; R12; R13; R14; R15 ]

let addr_regs a =
  (match a.base with Breg r -> [ r ] | _ -> []) @ match a.index with Some r -> [ r ] | None -> []

(* The registers an instruction reads and writes, implicit ones included *)
let uses_defs = function
  | Copy (d, s) -> ([ s ], [ d ])
  | Movi (d, _) | Got (d, _) | Tp d | Tlsoff (d, _) | Setcc (_, d) | Spget d -> ([], [ d ])
  | Salloc (d, n) -> ([ n ], [ d ])
  | Spset s -> ([ s ], [])
  | Load (_, _, d, a) | Fload (_, d, a) | Xload (d, a) | Lea (d, a) -> (addr_regs a, [ d ])
  | Store (_, a, s) | Fstore (_, a, s) | Xstore (a, s) -> (s :: addr_regs a, [])
  | Alu (X64.Cmp, x, y) | Test (x, y) | Ucomi (_, x, y) -> ([ x; y ], [])
  | Alu (_, d, s) | Imul (d, s) | Cmov (_, d, s) | Fop (_, _, d, s) -> ([ d; s ], [ d ])
  | Alui (X64.Cmp, x, _) -> ([ x ], [])
  | Alui (_, d, _) | Shifti (_, d, _) | Neg d | Not d | Bswap d -> ([ d ], [ d ])
  | Shift (_, d) -> ([ d; G Rcx ], [ d ])
  | Shd (_, d, s) -> ([ d; s; G Rcx ], [ d ])
  | Ext (_, _, d, s)
  | Bsf (d, s)
  | Bsr (d, s)
  | Popcnt (d, s)
  | Fsqrt (_, d, s)
  | Movgx (_, d, s)
  | Movxg (_, d, s)
  | Cvtsi (_, d, s)
  | Cvttsi (_, d, s)
  | Cvtff (_, d, s)
  | Round (_, d, s, _) ->
      ([ s ], [ d ])
  | Mul1 (_, s) -> ([ G Rax; s ], [ G Rax; G Rdx ])
  | Div (_, s) -> ([ G Rax; G Rdx; s ], [ G Rax; G Rdx ])
  | Cqo -> ([ G Rax ], [ G Rdx ])
  | Frem87 _ -> ([], [])
  | X87 (Fld (_, a) | Fstp (_, a) | Fnstcw a | Fldcw a) -> (addr_regs a, [])
  | X87 _ -> ([], [])
  | Call { target; args; rets } -> ((match target with Treg r -> [ r ] | _ -> []) @ args, rets)
  | Syscall args -> (args, [ G Rax ])
  | Rep_movsb -> ([ G Rdi; G Rsi; G Rcx ], [ G Rdi; G Rsi; G Rcx ])
  | Rep_stosb -> ([ G Rdi; G Rax; G Rcx ], [ G Rdi; G Rcx ])
  | Std | Cld | Mfence | Ud2 | Loc _ | Align_sp _ -> ([], [])
  | Xchg (_, a, r) | Xadd (_, a, r) -> (r :: addr_regs a, [ r ])
  | Cmpxchg (_, a, r) -> (G Rax :: r :: addr_regs a, [ G Rax ])
  | Asm a -> (a.ain, a.aout)

(* Physical registers an instruction destroys without defining a value *)
let clobbers = function
  | Call { rets; _ } ->
      List.filter
        (fun r -> not (List.mem r rets))
        (List.map (fun r -> G r) caller_saved @ List.init 16 (fun i -> X i))
  | Syscall _ -> [ G Rcx; G R11 ]
  | Frem87 _ | X87 Fremloop -> [ G Rax ]
  | Asm a -> a.aclob
  | _ -> []

let term_uses = function Ret rs -> rs | Jmpr (r, _) -> [ r ] | Jmp _ | Jcc _ | Asmgoto _ | Halt -> []

let successors = function
  | Jmp b -> [ b ]
  | Jcc (_, t, e) -> [ t; e ]
  | Jmpr (_, bs) -> bs
  | Asmgoto (b, bs) -> b :: bs
  | Ret _ | Halt -> []

(* Rewrites every register of an instruction *)
let map_regs g ins =
  let ma a =
    { a with base = (match a.base with Breg r -> Breg (g r) | b -> b); index = Option.map g a.index }
  in
  match ins with
  | Copy (d, s) -> Copy (g d, g s)
  | Movi (d, k) -> Movi (g d, k)
  | Load (n, sx, d, a) -> Load (n, sx, g d, ma a)
  | Store (n, a, s) -> Store (n, ma a, g s)
  | Fload (dbl, d, a) -> Fload (dbl, g d, ma a)
  | Fstore (dbl, a, s) -> Fstore (dbl, ma a, g s)
  | Xload (d, a) -> Xload (g d, ma a)
  | Xstore (a, s) -> Xstore (ma a, g s)
  | Lea (d, a) -> Lea (g d, ma a)
  | Got (d, s) -> Got (g d, s)
  | Tp d -> Tp (g d)
  | Tlsoff (d, s) -> Tlsoff (g d, s)
  | Alu (op, d, s) -> Alu (op, g d, g s)
  | Alui (op, d, k) -> Alui (op, g d, k)
  | Test (x, y) -> Test (g x, g y)
  | Imul (d, s) -> Imul (g d, g s)
  | Shift (op, d) -> Shift (op, g d)
  | Shifti (op, d, k) -> Shifti (op, g d, k)
  | Shd (l, d, s) -> Shd (l, g d, g s)
  | Neg d -> Neg (g d)
  | Not d -> Not (g d)
  | Bswap d -> Bswap (g d)
  | Ext (b, s, d, x) -> Ext (b, s, g d, g x)
  | Mul1 (s, x) -> Mul1 (s, g x)
  | Div (s, x) -> Div (s, g x)
  | Cqo -> Cqo
  | Setcc (c, d) -> Setcc (c, g d)
  | Cmov (c, d, s) -> Cmov (c, g d, g s)
  | Bsf (d, s) -> Bsf (g d, g s)
  | Bsr (d, s) -> Bsr (g d, g s)
  | Popcnt (d, s) -> Popcnt (g d, g s)
  | Fop (op, dbl, d, s) -> Fop (op, dbl, g d, g s)
  | Fsqrt (dbl, d, s) -> Fsqrt (dbl, g d, g s)
  | Ucomi (dbl, x, y) -> Ucomi (dbl, g x, g y)
  | Movgx (dbl, d, s) -> Movgx (dbl, g d, g s)
  | Movxg (dbl, d, s) -> Movxg (dbl, g d, g s)
  | Cvtsi (dbl, d, s) -> Cvtsi (dbl, g d, g s)
  | Cvttsi (dbl, d, s) -> Cvttsi (dbl, g d, g s)
  | Cvtff (dbl, d, s) -> Cvtff (dbl, g d, g s)
  | Round (dbl, d, s, m) -> Round (dbl, g d, g s, m)
  | Frem87 _ as i -> i
  | X87 (Fld (w, a)) -> X87 (Fld (w, ma a))
  | X87 (Fstp (w, a)) -> X87 (Fstp (w, ma a))
  | X87 (Fnstcw a) -> X87 (Fnstcw (ma a))
  | X87 (Fldcw a) -> X87 (Fldcw (ma a))
  | X87 _ as i -> i
  | Call c ->
      Call
        {
          target = (match c.target with Treg r -> Treg (g r) | t -> t);
          args = List.map g c.args;
          rets = List.map g c.rets;
        }
  | Syscall args -> Syscall (List.map g args)
  | (Rep_movsb | Rep_stosb | Std | Cld | Mfence | Ud2 | Loc _ | Align_sp _) as i -> i
  | Salloc (d, n) -> Salloc (g d, g n)
  | Spget d -> Spget (g d)
  | Spset s -> Spset (g s)
  | Xchg (n, a, r) -> Xchg (n, ma a, g r)
  | Xadd (n, a, r) -> Xadd (n, ma a, g r)
  | Cmpxchg (n, a, r) -> Cmpxchg (n, ma a, g r)
  | Asm a ->
      Asm
        {
          a with
          opnds =
            Array.map
              (function
                | Areg (r, w) -> Areg (g r, w)
                | Amem (m, n) -> Amem (ma m, n)
                | o -> o)
              a.opnds;
          ain = List.map g a.ain;
          aout = List.map g a.aout;
          aclob = List.map g a.aclob;
        }

let map_term g = function
  | Ret rs -> Ret (List.map g rs)
  | Jmpr (r, bs) -> Jmpr (g r, bs)
  | t -> t

(* Every register operand in a fixed order, uses and defs alike: two
   instructions with the same shape list their registers position by
   position, which is how the checker pairs a virtual register with the
   physical one that replaced it *)
let all_regs ins =
  let acc = ref [] in
  ignore (map_regs (fun r -> acc := r :: !acc; r) ins);
  List.rev !acc

let class_of_phys = function G _ -> Gpr | X _ -> Xmm | V _ -> invalid_arg "Mir.class_of_phys"

(* ---------- printing (FAS_MIR_DUMP=1) ---------- *)

let reg_s = function
  | V n -> Printf.sprintf "%%%d" n
  | G r -> X64.reg_name r
  | X i -> Printf.sprintf "xmm%d" i

let addr_s a =
  let parts =
    (match a.base with
    | Bnone -> []
    | Breg r -> [ reg_s r ]
    | Bslot s -> [ Printf.sprintf "slot%d" s ]
    | Bout -> [ "out" ]
    | Bin -> [ "in" ]
    | Bsym s -> [ "@" ^ s ])
    @ (match a.index with Some r -> [ Printf.sprintf "%s*%d" (reg_s r) a.scale ] | None -> [])
    @ if a.disp <> 0 then [ string_of_int a.disp ] else []
  in
  "[" ^ String.concat " + " parts ^ "]"

let cond_s c =
  X64.(
    match c with
    | O -> "o" | No -> "no" | B -> "b" | Ae -> "ae" | E -> "e" | Ne -> "ne" | Be -> "be" | A -> "a"
    | S -> "s" | Ns -> "ns" | P -> "p" | Np -> "np" | L -> "l" | Ge -> "ge" | Le -> "le" | G -> "g")

let alu_s = X64.(function Add -> "add" | Or -> "or" | And -> "and" | Sub -> "sub" | Xor -> "xor" | Cmp -> "cmp")
let shift_s = X64.(function Shl -> "shl" | Shr -> "shr" | Sar -> "sar")

let fop_s =
  X64.(function Fadd -> "fadd" | Fsub -> "fsub" | Fmul -> "fmul" | Fdiv -> "fdiv" | Fsqrt -> "fsqrt"
                | Fmin -> "fmin" | Fmax -> "fmax")

let ins_s i =
  let r = reg_s and p = Printf.sprintf in
  let d b = if b then "d" else "s" in
  match i with
  | Copy (a, b) -> p "%s = %s" (r a) (r b)
  | Movi (a, k) -> p "%s = %Ld" (r a) k
  | Load (n, sx, a, m) -> p "%s = load%d%s %s" (r a) n (if sx then "s" else "") (addr_s m)
  | Store (n, m, a) -> p "store%d %s, %s" n (addr_s m) (r a)
  | Fload (dbl, a, m) -> p "%s = fload%s %s" (r a) (d dbl) (addr_s m)
  | Fstore (dbl, m, a) -> p "fstore%s %s, %s" (d dbl) (addr_s m) (r a)
  | Xload (a, m) -> p "%s = xload %s" (r a) (addr_s m)
  | Xstore (m, a) -> p "xstore %s, %s" (addr_s m) (r a)
  | Lea (a, m) -> p "%s = lea %s" (r a) (addr_s m)
  | Got (a, s) -> p "%s = got @%s" (r a) s
  | Tp a -> p "%s = tp" (r a)
  | Tlsoff (a, s) -> p "%s = tlsoff @%s" (r a) s
  | Alu (op, a, b) -> p "%s %s, %s" (alu_s op) (r a) (r b)
  | Alui (op, a, k) -> p "%s %s, %d" (alu_s op) (r a) k
  | Test (a, b) -> p "test %s, %s" (r a) (r b)
  | Imul (a, b) -> p "imul %s, %s" (r a) (r b)
  | Shift (op, a) -> p "%s %s, cl" (shift_s op) (r a)
  | Shifti (op, a, k) -> p "%s %s, %d" (shift_s op) (r a) k
  | Shd (l, a, b) -> p "%s %s, %s, cl" (if l then "shld" else "shrd") (r a) (r b)
  | Neg a -> p "neg %s" (r a)
  | Not a -> p "not %s" (r a)
  | Bswap a -> p "bswap %s" (r a)
  | Ext (bits, sx, a, b) -> p "%s = %sext%d %s" (r a) (if sx then "s" else "z") bits (r b)
  | Mul1 (s, a) -> p "%smul rax, %s" (if s then "i" else "") (r a)
  | Div (s, a) -> p "%sdiv %s" (if s then "i" else "") (r a)
  | Cqo -> "cqo"
  | Setcc (c, a) -> p "%s = set%s" (r a) (cond_s c)
  | Cmov (c, a, b) -> p "cmov%s %s, %s" (cond_s c) (r a) (r b)
  | Bsf (a, b) -> p "%s = bsf %s" (r a) (r b)
  | Bsr (a, b) -> p "%s = bsr %s" (r a) (r b)
  | Popcnt (a, b) -> p "%s = popcnt %s" (r a) (r b)
  | Fop (op, dbl, a, b) -> p "%s%s %s, %s" (fop_s op) (d dbl) (r a) (r b)
  | Fsqrt (dbl, a, b) -> p "%s = sqrt%s %s" (r a) (d dbl) (r b)
  | Ucomi (dbl, a, b) -> p "ucomi%s %s, %s" (d dbl) (r a) (r b)
  | Movgx (dbl, a, b) -> p "%s = movgx%s %s" (r a) (d dbl) (r b)
  | Movxg (dbl, a, b) -> p "%s = movxg%s %s" (r a) (d dbl) (r b)
  | Cvtsi (dbl, a, b) -> p "%s = cvtsi2%s %s" (r a) (d dbl) (r b)
  | Cvttsi (dbl, a, b) -> p "%s = cvtt%s2si %s" (r a) (d dbl) (r b)
  | Cvtff (fd, a, b) -> p "%s = cvt%s2%s %s" (r a) (d fd) (d (not fd)) (r b)
  | Round (dbl, a, b, m) -> p "%s = round%s %s, %d" (r a) (d dbl) (r b) m
  | Frem87 (dbl, s) -> p "frem87%s slot%d" (d dbl) s
  | X87 x -> (
      let w = function M32 -> "m32" | M64 -> "m64" | M80 -> "m80" | I64 -> "i64" in
      match x with
      | Fld (k, m) -> p "fld %s %s" (w k) (addr_s m)
      | Fstp (k, m) -> p "fstp %s %s" (w k) (addr_s m)
      | Farith op -> p "%sp" (fop_s op)
      | Fremloop -> "fprem loop"
      | Fsqrt87 -> "fsqrt"
      | Frndint -> "frndint"
      | Fucomip -> "fucomip"
      | Fpop -> "fpop"
      | Fnstcw m -> p "fnstcw %s" (addr_s m)
      | Fldcw m -> p "fldcw %s" (addr_s m))
  | Call { target; args; rets } ->
      p "call %s (%s) -> (%s)"
        (match target with Tlabel s -> s | Tsym s -> "@" ^ s | Treg x -> r x | Ttlsgd s -> "tlsgd @" ^ s)
        (String.concat ", " (List.map r args))
        (String.concat ", " (List.map r rets))
  | Syscall args -> p "syscall (%s)" (String.concat ", " (List.map r args))
  | Rep_movsb -> "rep movsb"
  | Rep_stosb -> "rep stosb"
  | Std -> "std"
  | Cld -> "cld"
  | Xchg (n, m, a) -> p "xchg%d %s, %s" n (addr_s m) (r a)
  | Xadd (n, m, a) -> p "lock xadd%d %s, %s" n (addr_s m) (r a)
  | Cmpxchg (n, m, a) -> p "lock cmpxchg%d %s, %s" n (addr_s m) (r a)
  | Mfence -> "mfence"
  | Align_sp n -> Printf.sprintf "align_sp %d" n
  | Ud2 -> "ud2"
  | Loc n -> Printf.sprintf ".loc %d" n
  | Salloc (a, n) -> p "%s = salloc %s" (r a) (r n)
  | Spget a -> p "%s = rsp" (r a)
  | Spset a -> p "rsp = %s" (r a)
  | Asm a ->
      let o = function
        | Areg (x, w) -> p "%s:%d" (r x) w
        | Amem (m, n) -> p "%s:%d" (addr_s m) n
        | Aimm k -> Int64.to_string k
        | Asym (s, k) -> p "@%s%+Ld" s k
        | Anone -> "flags"
      in
      p "asm%s [%s] reads (%s) writes (%s) clobbers (%s)"
        (if a.intel then " intel" else "")
        (String.concat ", " (Array.to_list (Array.map o a.opnds)))
        (String.concat ", " (List.map r a.ain))
        (String.concat ", " (List.map r a.aout))
        (String.concat ", " (List.map r a.aclob))

let term_s = function
  | Jmp b -> Printf.sprintf "jmp b%d" b
  | Jcc (c, t, e) -> Printf.sprintf "j%s b%d else b%d" (cond_s c) t e
  | Ret rs -> "ret " ^ String.concat ", " (List.map reg_s rs)
  | Jmpr (r, _) -> "jmp " ^ reg_s r
  | Asmgoto (b, bs) -> Printf.sprintf "jmp b%d (asm goto %s)" b (String.concat ", " (List.map (Printf.sprintf "b%d") bs))
  | Halt -> "halt"

let to_string f =
  let b = Buffer.create 1024 in
  Printf.bprintf b "func %s:\n" f.name;
  Dynarray.iteri
    (fun i (bl : block) ->
      Printf.bprintf b "b%d:\n" i;
      List.iter (fun ins -> Printf.bprintf b "  %s\n" (ins_s ins)) bl.code;
      Printf.bprintf b "  %s\n" (term_s bl.term))
    f.blocks;
  Buffer.contents b
