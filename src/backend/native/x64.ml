(* x86-64 machine-code encoder for the native backend.

   An assembler buffer with local labels (rel32 jumps and calls, resolved by
   [finish]) and symbol references (RIP-relative displacements and absolute
   64-bit addresses, resolved by the linker in [Elf64] once every section has
   an address). Every jump is rel32 for now; branch relaxation comes later. *)

type reg =
  | Rax
  | Rcx
  | Rdx
  | Rbx
  | Rsp
  | Rbp
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

let reg_num = function
  | Rax -> 0
  | Rcx -> 1
  | Rdx -> 2
  | Rbx -> 3
  | Rsp -> 4
  | Rbp -> 5
  | Rsi -> 6
  | Rdi -> 7
  | R8 -> 8
  | R9 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15

let reg_name = function
  | Rax -> "rax"
  | Rcx -> "rcx"
  | Rdx -> "rdx"
  | Rbx -> "rbx"
  | Rsp -> "rsp"
  | Rbp -> "rbp"
  | Rsi -> "rsi"
  | Rdi -> "rdi"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

(* Condition codes, numbered as in the Jcc/SETcc/CMOVcc opcodes. *)
type cond = O | No | B | Ae | E | Ne | Be | A | S | Ns | P | Np | L | Ge | Le | G

let cond_num = function
  | O -> 0
  | No -> 1
  | B -> 2
  | Ae -> 3
  | E -> 4
  | Ne -> 5
  | Be -> 6
  | A -> 7
  | S -> 8
  | Ns -> 9
  | P -> 10
  | Np -> 11
  | L -> 12
  | Ge -> 13
  | Le -> 14
  | G -> 15

(* A memory operand: [base + disp], [rip + symbol + disp], or
   [base + index * scale + disp] where the base may be absent and the
   displacement may name a symbol (absolute, sign-extended 32 bits: the
   executable is not position independent and lives below 2 GiB). *)
type mem =
  | Base of reg * int
  | Rip of string * int
  | Sib of {
      base : reg option;
      index : reg;
      scale : int;
      disp : int;
      sym : string option;
    }

type label = int

(* A reference the linker patches once symbol addresses are known. *)
type reloc =
  | Rel32 of { offset : int; symbol : string; addend : int }
      (** 32-bit PC-relative: value = S + addend - (address of the next byte). *)
  | Call32 of { offset : int; symbol : string; addend : int }
      (** Direct call, allowing an external linker to select a PLT target. *)
  | Abs64 of { offset : int; symbol : string; addend : int }
  | Abs32s of { offset : int; symbol : string; addend : int }
      (** 32-bit absolute, sign-extended by the CPU. *)
  | Gotpcrel of { offset : int; symbol : string; addend : int }
      (** PC-relative address of the symbol's GOT entry, in a
          `mov r64, [rip + sym@GOTPCREL]` (see [load_got]); same "next byte"
          convention as [Rel32]. Position-independent objects reach symbols
          another module may define or interpose through it. *)

type t = {
  code : Buffer.t;
  mutable labels : int option array;
  mutable next_label : int;
  mutable fixups : (int * label) list;  (** rel32 field offset, target label *)
  mutable relocs : reloc list;
}

let create () =
  {
    code = Buffer.create 4096;
    labels = Array.make 64 None;
    next_label = 0;
    fixups = [];
    relocs = [];
  }

let pos a = Buffer.length a.code
let byte a b = Buffer.add_char a.code (Char.unsafe_chr (b land 0xff))

let int32 a v =
  byte a v;
  byte a (v lsr 8);
  byte a (v lsr 16);
  byte a (v lsr 24)

let int64 a (v : int64) =
  for i = 0 to 7 do
    byte a (Int64.to_int (Int64.shift_right_logical v (8 * i)))
  done

let fits_i8 v = v >= -128 && v <= 127
let fits_i32 v = v >= -0x8000_0000 && v <= 0x7fff_ffff

let fits_i32_64 v =
  Int64.compare v (-0x8000_0000L) >= 0 && Int64.compare v 0x7fff_ffffL <= 0

let fits_u32_64 v = Int64.compare v 0L >= 0 && Int64.compare v 0xffff_ffffL <= 0

(* Labels *)

let new_label a =
  let l = a.next_label in
  a.next_label <- l + 1;
  if l >= Array.length a.labels then begin
    let bigger = Array.make (2 * Array.length a.labels) None in
    Array.blit a.labels 0 bigger 0 (Array.length a.labels);
    a.labels <- bigger
  end;
  l

let place a l =
  match a.labels.(l) with
  | Some _ -> invalid_arg "x64: label placed twice"
  | None -> a.labels.(l) <- Some (pos a)

let rel32_label a l =
  a.fixups <- (pos a, l) :: a.fixups;
  int32 a 0

let patch32 buf offset v =
  Bytes.set buf offset (Char.unsafe_chr (v land 0xff));
  Bytes.set buf (offset + 1) (Char.unsafe_chr ((v lsr 8) land 0xff));
  Bytes.set buf (offset + 2) (Char.unsafe_chr ((v lsr 16) land 0xff));
  Bytes.set buf (offset + 3) (Char.unsafe_chr ((v lsr 24) land 0xff))

(* Resolve label fixups; symbol relocations stay for the linker. *)
let finish a =
  let bytes = Buffer.to_bytes a.code in
  List.iter
    (fun (offset, l) ->
      match a.labels.(l) with
      | None -> invalid_arg "x64: jump to a label that was never placed"
      | Some target -> patch32 bytes offset (target - (offset + 4)))
    a.fixups;
  (bytes, List.rev a.relocs)

let label_offset a l = a.labels.(l)

(* Prefixes and ModRM *)

(* REX with W, and the high bits of reg (R), index (X) and base/rm (B).
   [force] emits it even when empty, for byte registers spl..dil. *)
let rex a ?(force = false) ?(x = 0) ~w ~r ~b () =
  let v =
    (if w then 8 else 0)
    lor (if r >= 8 then 4 else 0)
    lor (if x >= 8 then 2 else 0)
    lor if b >= 8 then 1 else 0
  in
  if v <> 0 || force then byte a (0x40 lor v)

let modrm a md reg rm = byte a ((md lsl 6) lor ((reg land 7) lsl 3) lor (rm land 7))

(* ModRM (+SIB, +displacement) for [reg-field, memory operand]. *)
let mem_operand a reg_field = function
  | Base (base, disp) ->
      let b = reg_num base in
      let needs_sib = b land 7 = 4 in
      let md = if disp = 0 && b land 7 <> 5 then 0 else if fits_i8 disp then 1 else 2 in
      modrm a md reg_field (if needs_sib then 4 else b);
      if needs_sib then byte a 0x24;
      if md = 1 then byte a disp else if md = 2 then int32 a disp
  | Rip (symbol, addend) ->
      modrm a 0 reg_field 5;
      (* The displacement is relative to the end of the instruction. Every
         RIP-relative form here ends with this field, so the next byte is
         offset + 4. *)
      a.relocs <- Rel32 { offset = pos a; symbol; addend } :: a.relocs;
      int32 a 0
  | Sib { base; index; scale; disp; sym } -> (
      let i = reg_num index in
      if i = 4 then invalid_arg "x64: rsp as an index";
      let ss =
        match scale with
        | 1 -> 0
        | 2 -> 1
        | 4 -> 2
        | 8 -> 3
        | _ -> invalid_arg "x64: scale"
      in
      let disp32 () =
        (match sym with
        | Some symbol ->
            a.relocs <- Abs32s { offset = pos a; symbol; addend = disp } :: a.relocs
        | None -> ());
        int32 a (if sym = None then disp else 0)
      in
      match base with
      | None ->
          modrm a 0 reg_field 4;
          byte a ((ss lsl 6) lor ((i land 7) lsl 3) lor 5);
          disp32 ()
      | Some b ->
          let b = reg_num b in
          let md =
            if sym <> None then 2
            else if disp = 0 && b land 7 <> 5 then 0
            else if fits_i8 disp then 1
            else 2
          in
          modrm a md reg_field 4;
          byte a ((ss lsl 6) lor ((i land 7) lsl 3) lor (b land 7));
          if md = 1 then byte a disp else if md = 2 then disp32 ())

let mem_b_bits = function
  | Base (base, _) | Sib { base = Some base; _ } -> reg_num base
  | Rip _ | Sib { base = None; _ } -> 0

let mem_x_bits = function Sib { index; _ } -> reg_num index | _ -> 0

(* op reg, r/m (reg-reg) *)
let rr a ?(w = true) ?(force = false) opcode ~reg ~rm =
  rex a ~force ~w ~r:(reg_num reg) ~b:(reg_num rm) ();
  List.iter (byte a) opcode;
  modrm a 3 (reg_num reg) (reg_num rm)

(* op reg, [mem] *)
let rm a ?(w = true) ?(force = false) opcode ~reg mem =
  rex a ~force ~w ~r:(reg_num reg) ~x:(mem_x_bits mem) ~b:(mem_b_bits mem) ();
  List.iter (byte a) opcode;
  mem_operand a (reg_num reg) mem

(* op /ext, r/m register *)
let ext_r a ?(w = true) opcode ext r =
  rex a ~w ~r:0 ~b:(reg_num r) ();
  List.iter (byte a) opcode;
  modrm a 3 ext (reg_num r)

(* Moves *)

let mov_rr a dst src = if dst <> src then rr a [ 0x89 ] ~reg:src ~rm:dst

(* Load a 64-bit constant with the shortest encoding. *)
let mov_imm a dst (v : int64) =
  if v = 0L then rr a ~w:false [ 0x31 ] ~reg:dst ~rm:dst (* xor r32, r32 *)
  else if fits_u32_64 v then begin
    rex a ~w:false ~r:0 ~b:(reg_num dst) ();
    byte a (0xb8 + (reg_num dst land 7));
    int32 a (Int64.to_int v)
  end
  else if fits_i32_64 v then begin
    ext_r a [ 0xc7 ] 0 dst;
    int32 a (Int64.to_int v)
  end
  else begin
    rex a ~w:true ~r:0 ~b:(reg_num dst) ();
    byte a (0xb8 + (reg_num dst land 7));
    int64 a v
  end

(* Load [size] bytes, zero-extended to 64 bits. *)
let load_zx a size dst mem =
  match size with
  | 1 -> rm a ~w:false [ 0x0f; 0xb6 ] ~reg:dst mem
  | 2 -> rm a ~w:false [ 0x0f; 0xb7 ] ~reg:dst mem
  | 4 -> rm a ~w:false [ 0x8b ] ~reg:dst mem
  | 8 -> rm a [ 0x8b ] ~reg:dst mem
  | _ -> invalid_arg "x64.load_zx"

(* Load [size] bytes, sign-extended to 64 bits. *)
let load_sx a size dst mem =
  match size with
  | 1 -> rm a [ 0x0f; 0xbe ] ~reg:dst mem
  | 2 -> rm a [ 0x0f; 0xbf ] ~reg:dst mem
  | 4 -> rm a [ 0x63 ] ~reg:dst mem
  | 8 -> rm a [ 0x8b ] ~reg:dst mem
  | _ -> invalid_arg "x64.load_sx"

(* Store the low [size] bytes of a register. *)
let store a size mem src =
  match size with
  | 1 -> rm a ~w:false ~force:(reg_num src >= 4) [ 0x88 ] ~reg:src mem
  | 2 ->
      byte a 0x66;
      rm a ~w:false [ 0x89 ] ~reg:src mem
  | 4 -> rm a ~w:false [ 0x89 ] ~reg:src mem
  | 8 -> rm a [ 0x89 ] ~reg:src mem
  | _ -> invalid_arg "x64.store"

(* Atomics. [sized_rm] encodes an r/m-with-register op of [size] bytes,
   optionally with a lock prefix. xchg with memory is locked implicitly;
   cmpxchg compares with the low [size] bytes of rax. For 1- and 2-byte forms
   the upper bits of the register operand are left as they were. *)
let sized_rm a ~lock size op8 op ~reg mem =
  if lock then byte a 0xf0;
  match size with
  | 1 -> rm a ~w:false ~force:(reg_num reg >= 4) op8 ~reg mem
  | 2 ->
      byte a 0x66;
      rm a ~w:false op ~reg mem
  | 4 -> rm a ~w:false op ~reg mem
  | 8 -> rm a op ~reg mem
  | _ -> invalid_arg "x64.sized_rm"

let xchg_mr a size mem src = sized_rm a ~lock:false size [ 0x86 ] [ 0x87 ] ~reg:src mem
let lock_xadd a size mem src = sized_rm a ~lock:true size [ 0x0f; 0xc0 ] [ 0x0f; 0xc1 ] ~reg:src mem

let lock_cmpxchg a size mem src =
  sized_rm a ~lock:true size [ 0x0f; 0xb0 ] [ 0x0f; 0xb1 ] ~reg:src mem

let mfence a =
  byte a 0x0f;
  byte a 0xae;
  byte a 0xf0

let lea a dst mem = rm a [ 0x8d ] ~reg:dst mem

(* mov dst, [rip + symbol@GOTPCREL]: the address of [symbol] read from its
   GOT entry. Always REX.W 8B /r with a RIP-relative ModRM, the exact form
   R_X86_64_REX_GOTPCRELX promises, so a linker may relax it to a lea when
   the symbol turns out to be local. *)
let load_got a dst symbol =
  let d = reg_num dst in
  rex a ~w:true ~r:d ~b:0 ();
  byte a 0x8b;
  modrm a 0 d 5;
  a.relocs <- Gotpcrel { offset = pos a; symbol; addend = 0 } :: a.relocs;
  int32 a 0

(* Zero-extend the low [bits] of a register in place. *)
let zero_extend a bits r =
  match bits with
  | 1 ->
      ext_r a ~w:false [ 0x83 ] 4 r;
      byte a 1 (* and r32, 1 *)
  | 8 -> rr a ~w:false ~force:(reg_num r >= 4) [ 0x0f; 0xb6 ] ~reg:r ~rm:r
  | 16 -> rr a ~w:false [ 0x0f; 0xb7 ] ~reg:r ~rm:r
  | 32 -> rr a ~w:false [ 0x89 ] ~reg:r ~rm:r
  | 64 -> ()
  | _ -> invalid_arg "x64.zero_extend"

(* [dst] = the low [bits] of [src], zero-extended *)
let zero_extend_rr a bits dst src =
  match bits with
  | 8 -> rr a ~w:false ~force:(reg_num src >= 4) [ 0x0f; 0xb6 ] ~reg:dst ~rm:src
  | 16 -> rr a ~w:false [ 0x0f; 0xb7 ] ~reg:dst ~rm:src
  | 32 -> rr a ~w:false [ 0x89 ] ~reg:src ~rm:dst
  | 64 -> if dst <> src then rr a [ 0x89 ] ~reg:src ~rm:dst
  | _ ->
      if dst <> src then rr a [ 0x89 ] ~reg:src ~rm:dst;
      zero_extend a bits dst

(* Sign-extend the low [bits] of a register in place. *)
let sign_extend a bits r =
  match bits with
  | 1 ->
      (* 0/1 -> 0/-1 *)
      ext_r a [ 0xf7 ] 3 r (* neg *)
  | 8 -> rr a ~force:(reg_num r >= 4) [ 0x0f; 0xbe ] ~reg:r ~rm:r
  | 16 -> rr a [ 0x0f; 0xbf ] ~reg:r ~rm:r
  | 32 -> rr a [ 0x63 ] ~reg:r ~rm:r
  | 64 -> ()
  | _ -> invalid_arg "x64.sign_extend"

(* Arithmetic *)

type alu = Add | Or | And | Sub | Xor | Cmp

let alu_opcode = function
  | Add -> 0x01
  | Or -> 0x09
  | And -> 0x21
  | Sub -> 0x29
  | Xor -> 0x31
  | Cmp -> 0x39

let alu_ext = function Add -> 0 | Or -> 1 | And -> 4 | Sub -> 5 | Xor -> 6 | Cmp -> 7

(* op dst, src (64-bit) *)
let alu_rr a op dst src = rr a [ alu_opcode op ] ~reg:src ~rm:dst

(* op dst, [mem] (64-bit) *)
let alu_rm a op dst mem = rm a [ alu_opcode op + 2 ] ~reg:dst mem

let alu_ri a op dst imm =
  if fits_i8 imm then begin
    ext_r a [ 0x83 ] (alu_ext op) dst;
    byte a imm
  end
  else if fits_i32 imm then begin
    ext_r a [ 0x81 ] (alu_ext op) dst;
    int32 a imm
  end
  else invalid_arg "x64.alu_ri: immediate does not fit in 32 bits"

(* op [mem], imm on the low [size] bytes. [imm] is the operand's value; only
   its low [size] bytes matter. *)
let alu_mi a op size mem imm =
  let sext v bits = (v lsl (63 - bits)) asr (63 - bits) in
  let v = if size = 8 then imm else sext imm (size * 8) in
  let short = size = 1 || fits_i8 v in
  if size = 8 && not (fits_i32 v) then invalid_arg "x64.alu_mi: immediate";
  let tail = if short then 1 else if size = 2 then 2 else 4 in
  (* RIP-relative displacements count from the end of the instruction *)
  let mem = match mem with Rip (s, ad) -> Rip (s, ad - tail) | m -> m in
  if size = 2 then byte a 0x66;
  rex a ~w:(size = 8) ~r:0 ~x:(mem_x_bits mem) ~b:(mem_b_bits mem) ();
  byte a (if size = 1 then 0x80 else if short then 0x83 else 0x81);
  mem_operand a (alu_ext op) mem;
  if short then byte a v
  else if size = 2 then begin
    byte a v;
    byte a (v asr 8)
  end
  else int32 a v

(* mov [mem], imm on the low [size] bytes; a 64-bit store sign-extends a
   32-bit immediate. *)
let mov_mi a size mem imm =
  let sext v bits = (v lsl (63 - bits)) asr (63 - bits) in
  let v = if size = 8 then imm else sext imm (size * 8) in
  if size = 8 && not (fits_i32 v) then invalid_arg "x64.mov_mi: immediate";
  let tail = match size with 1 -> 1 | 2 -> 2 | _ -> 4 in
  let mem = match mem with Rip (s, ad) -> Rip (s, ad - tail) | m -> m in
  if size = 2 then byte a 0x66;
  rex a ~w:(size = 8) ~r:0 ~x:(mem_x_bits mem) ~b:(mem_b_bits mem) ();
  byte a (if size = 1 then 0xc6 else 0xc7);
  mem_operand a 0 mem;
  match size with
  | 1 -> byte a v
  | 2 ->
      byte a v;
      byte a (v asr 8)
  | _ -> int32 a v

(* op [mem], src on the low [size] bytes *)
let alu_mr a op size mem src =
  match size with
  | 1 -> rm a ~w:false ~force:(reg_num src >= 4) [ alu_opcode op - 1 ] ~reg:src mem
  | 2 ->
      byte a 0x66;
      rm a ~w:false [ alu_opcode op ] ~reg:src mem
  | 4 -> rm a ~w:false [ alu_opcode op ] ~reg:src mem
  | 8 -> rm a [ alu_opcode op ] ~reg:src mem
  | _ -> invalid_arg "x64.alu_mr"

let test_rr a x y = rr a [ 0x85 ] ~reg:y ~rm:x

(* test r, imm for 0 <= imm < 2^31: the 8-bit form for a byte mask, else the
   32-bit one (the upper half of the register is not tested) *)
let test_ri a r imm =
  if imm >= 0 && imm < 256 then begin
    rex a ~force:(reg_num r >= 4) ~w:false ~r:0 ~b:(reg_num r) ();
    byte a 0xf6;
    modrm a 3 0 (reg_num r);
    byte a imm
  end
  else if imm >= 0 && fits_i32 imm then begin
    ext_r a ~w:false [ 0xf7 ] 0 r;
    int32 a imm
  end
  else invalid_arg "x64.test_ri: immediate"

let imul_rr a dst src = rr a [ 0x0f; 0xaf ] ~reg:dst ~rm:src
let imul_rm a dst mem = rm a [ 0x0f; 0xaf ] ~reg:dst mem

(* imul dst, src, imm (64-bit) *)
let imul_rri a dst src imm =
  if fits_i8 imm then begin
    rr a [ 0x6b ] ~reg:dst ~rm:src;
    byte a imm
  end
  else if fits_i32 imm then begin
    rr a [ 0x69 ] ~reg:dst ~rm:src;
    int32 a imm
  end
  else invalid_arg "x64.imul_rri: immediate does not fit in 32 bits"

type shift = Shl | Shr | Sar

(* shift dst, cl *)
let shift_cl a op dst =
  ext_r a [ 0xd3 ] (match op with Shl -> 4 | Shr -> 5 | Sar -> 7) dst

(* shift dst, imm8 *)
let shift_imm a op dst k =
  ext_r a [ 0xc1 ] (match op with Shl -> 4 | Shr -> 5 | Sar -> 7) dst;
  byte a (k land 63)

let neg a r = ext_r a [ 0xf7 ] 3 r
let not_ a ?(w = true) r = ext_r a ~w [ 0xf7 ] 2 r
let div a r = ext_r a [ 0xf7 ] 6 r
let idiv a r = ext_r a [ 0xf7 ] 7 r
let mul1 a r = ext_r a [ 0xf7 ] 4 r
let imul1 a r = ext_r a [ 0xf7 ] 5 r

let cqo a =
  byte a 0x48;
  byte a 0x99

(* SSE scalar floating point. XMM registers are numbered 0..15; the
   backend keeps float values as bit patterns in general registers and uses
   xmm0..xmm2 only as scratch around arithmetic, compares and conversions. *)

type xmm = int

(* [prefix] [REX] 0F [op] ModRM(reg, rm), register-register form *)
let sse_rr a ?(w = false) prefix op ~reg ~rm =
  if prefix <> 0 then byte a prefix;
  rex a ~w ~r:reg ~b:rm ();
  byte a 0x0f;
  byte a op;
  modrm a 3 reg rm

(* [prefix] [REX] 0F [op] ModRM(reg, mem) *)
let sse_rm a ?(w = false) prefix op ~reg mem =
  if prefix <> 0 then byte a prefix;
  rex a ~w ~r:reg ~x:(mem_x_bits mem) ~b:(mem_b_bits mem) ();
  byte a 0x0f;
  byte a op;
  mem_operand a reg mem

(* movd/movq xmm, r32/r64 *)
let mov_xr a ~double (x : xmm) r = sse_rr a ~w:double 0x66 0x6e ~reg:x ~rm:(reg_num r)

(* movd/movq r32/r64, xmm (movd zero-extends into the 64-bit register) *)
let mov_rx a ~double r (x : xmm) = sse_rr a ~w:double 0x66 0x7e ~reg:x ~rm:(reg_num r)

(* movss/movsd xmm, [mem] *)
let mov_xm a ~double (x : xmm) mem =
  sse_rm a (if double then 0xf2 else 0xf3) 0x10 ~reg:x mem

let fprefix double = if double then 0xf2 else 0xf3

type fop = Fadd | Fsub | Fmul | Fdiv | Fsqrt | Fmin | Fmax

let fop_code = function
  | Fadd -> 0x58
  | Fsub -> 0x5c
  | Fmul -> 0x59
  | Fdiv -> 0x5e
  | Fsqrt -> 0x51
  | Fmin -> 0x5d
  | Fmax -> 0x5f

(* addsd/subsd/... dst, src *)
let fop_xx a ~double op (dst : xmm) (src : xmm) =
  sse_rr a (fprefix double) (fop_code op) ~reg:dst ~rm:src

(* addsd/... dst, [mem] *)
let fop_xm a ~double op (dst : xmm) mem =
  sse_rm a (fprefix double) (fop_code op) ~reg:dst mem

(* ucomisd/ucomiss x, y: flags as an unsigned compare of x with y; PF set
   when unordered (ZF and CF are then set too). *)
let ucomi a ~double (x : xmm) (y : xmm) =
  sse_rr a (if double then 0x66 else 0) 0x2e ~reg:x ~rm:y

(* cvtsi2sd/ss xmm, r64 *)
let cvt_si2f a ~double (x : xmm) r =
  sse_rr a ~w:true (fprefix double) 0x2a ~reg:x ~rm:(reg_num r)

(* cvttsd2si/cvttss2si r64, xmm (truncating) *)
let cvt_f2si a ~double r (x : xmm) =
  sse_rr a ~w:true (fprefix double) 0x2c ~reg:(reg_num r) ~rm:x

(* cvtss2sd (from single) or cvtsd2ss (from double) *)
let cvt_ff a ~from_double (dst : xmm) (src : xmm) =
  sse_rr a (fprefix from_double) 0x5a ~reg:dst ~rm:src

(* andpd/andps, orpd/orps, xorpd/xorps xmm, xmm *)
let logic_xx a ~double op (dst : xmm) (src : xmm) =
  let code =
    match op with `And -> 0x54 | `Andn -> 0x55 | `Or -> 0x56 | `Xor -> 0x57
  in
  sse_rr a (if double then 0x66 else 0) code ~reg:dst ~rm:src

(* movaps dst, src *)
let mov_xx a (dst : xmm) (src : xmm) =
  if dst <> src then sse_rr a 0 0x28 ~reg:dst ~rm:src

(* roundsd/roundss dst, src, mode (SSE4.1): 0 nearest-even, 1 floor, 2 ceil,
   3 truncate; bit 3 suppresses the inexact exception. *)
let round_xx a ~double (dst : xmm) (src : xmm) mode =
  byte a 0x66;
  rex a ~w:false ~r:dst ~b:src ();
  byte a 0x0f;
  byte a 0x3a;
  byte a (if double then 0x0b else 0x0a);
  modrm a 3 dst src;
  byte a (mode lor 8)

(* SSE2 packed integers on 128-bit values, for vectors that stay vectors.
   Lane widths are 8, 16, 32 or 64 bits. *)

type pop =
  | Padd
  | Psub
  | Pand
  | Pandn
  | Por
  | Pxor
  | Pmuludq
  | Pmullw
  | Pcmpeq
  | Pcmpgt

let pop_code op w =
  match (op, w) with
  | Padd, 8 -> 0xfc
  | Padd, 16 -> 0xfd
  | Padd, 32 -> 0xfe
  | Padd, _ -> 0xd4
  | Psub, 8 -> 0xf8
  | Psub, 16 -> 0xf9
  | Psub, 32 -> 0xfa
  | Psub, _ -> 0xfb
  | Pand, _ -> 0xdb
  | Pandn, _ -> 0xdf
  | Por, _ -> 0xeb
  | Pxor, _ -> 0xef
  | Pmuludq, _ -> 0xf4
  | Pmullw, _ -> 0xd5
  | Pcmpeq, 8 -> 0x74
  | Pcmpeq, 16 -> 0x75
  | Pcmpeq, _ -> 0x76
  | Pcmpgt, 8 -> 0x64
  | Pcmpgt, 16 -> 0x65
  | Pcmpgt, _ -> 0x66

let pop_xx a op w (dst : xmm) (src : xmm) =
  sse_rr a 0x66 (pop_code op w) ~reg:dst ~rm:src

let pop_xm a op w (dst : xmm) mem = sse_rm a 0x66 (pop_code op w) ~reg:dst mem

type pshift = Psll | Psrl | Psra

(* psllw/d/q, psrlw/d/q, psraw/d by an immediate *)
let pshift_imm a op w (x : xmm) k =
  let opc = match w with 16 -> 0x71 | 32 -> 0x72 | _ -> 0x73 in
  let ext = match op with Psll -> 6 | Psrl -> 2 | Psra -> 4 in
  sse_rr a 0x66 opc ~reg:ext ~rm:x;
  byte a k

(* the same by the count in the low 64 bits of [count] *)
let pshift_xx a op w (x : xmm) (count : xmm) =
  let code =
    match (op, w) with
    | Psll, 16 -> 0xf1
    | Psll, 32 -> 0xf2
    | Psll, _ -> 0xf3
    | Psrl, 16 -> 0xd1
    | Psrl, 32 -> 0xd2
    | Psrl, _ -> 0xd3
    | Psra, 16 -> 0xe1
    | Psra, _ -> 0xe2
  in
  sse_rr a 0x66 code ~reg:x ~rm:count

(* movdqu xmm, [mem] and movdqu [mem], xmm (no alignment needed) *)
let movdqu_load a (x : xmm) mem = sse_rm a 0xf3 0x6f ~reg:x mem
let movdqu_store a mem (x : xmm) = sse_rm a 0xf3 0x7f ~reg:x mem

(* pshufd dst, src, imm *)
let pshufd a (dst : xmm) (src : xmm) k =
  sse_rr a 0x66 0x70 ~reg:dst ~rm:src;
  byte a k

(* pshuflw dst, src, imm (low four words) *)
let pshuflw a (dst : xmm) (src : xmm) k =
  sse_rr a 0xf2 0x70 ~reg:dst ~rm:src;
  byte a k

(* punpcklqdq / punpcklbw dst, src *)
let punpcklqdq a (dst : xmm) (src : xmm) = sse_rr a 0x66 0x6c ~reg:dst ~rm:src
let punpcklbw a (dst : xmm) (src : xmm) = sse_rr a 0x66 0x60 ~reg:dst ~rm:src

(* pextrw r32, xmm, imm (zero-extends) *)
let pextrw a r (x : xmm) k =
  sse_rr a 0x66 0xc5 ~reg:(reg_num r) ~rm:x;
  byte a k

(* movdqa dst, src *)
let movdqa a (dst : xmm) (src : xmm) =
  if dst <> src then sse_rr a 0x66 0x6f ~reg:dst ~rm:src

(* bt/bts/btr/btc r64, imm8 *)
let bit_ri a op r k =
  ext_r a [ 0x0f; 0xba ]
    (match op with `Bt -> 4 | `Bts -> 5 | `Btr -> 6 | `Btc -> 7)
    r;
  byte a k

(* setcc r8, then zero-extend to 64 bits *)
let setcc a c r =
  rex a ~force:(reg_num r >= 4) ~w:false ~r:0 ~b:(reg_num r) ();
  byte a 0x0f;
  byte a (0x90 + cond_num c);
  modrm a 3 0 (reg_num r);
  zero_extend a 8 r

let cmov a c dst src = rr a [ 0x0f; 0x40 + cond_num c ] ~reg:dst ~rm:src
let cmov_rm a c dst mem = rm a [ 0x0f; 0x40 + cond_num c ] ~reg:dst mem

(* Control flow *)

let jmp a l =
  byte a 0xe9;
  rel32_label a l

let jcc a c l =
  byte a 0x0f;
  byte a (0x80 + cond_num c);
  rel32_label a l

let call_label a l =
  byte a 0xe8;
  rel32_label a l

(* call to a symbol resolved by the linker *)
let call_symbol a symbol =
  byte a 0xe8;
  a.relocs <- Call32 { offset = pos a; symbol; addend = 0 } :: a.relocs;
  int32 a 0

let call_r a r = ext_r a ~w:false [ 0xff ] 2 r
let ret a = byte a 0xc3

let push a r =
  rex a ~w:false ~r:0 ~b:(reg_num r) ();
  byte a (0x50 + (reg_num r land 7))

let pop a r =
  rex a ~w:false ~r:0 ~b:(reg_num r) ();
  byte a (0x58 + (reg_num r land 7))

let leave a = byte a 0xc9

let syscall a =
  byte a 0x0f;
  byte a 0x05

let ud2 a =
  byte a 0x0f;
  byte a 0x0b

let rep_movsb a =
  byte a 0xf3;
  byte a 0xa4

let rep_stosb a =
  byte a 0xf3;
  byte a 0xaa

(* Direction flag: set (std) makes string instructions run downwards; the
   ABI requires it clear (cld) at calls and returns *)
let std a = byte a 0xfd
let cld a = byte a 0xfc

(* x87, for frem only: fprem is the one exact remainder instruction (SSE has
   none). fld/fstp m32 (d9 /0, /3) or m64 (dd /0, /3); fprem (d9 f8); fnstsw
   ax (df e0); fstp st(0) pops (dd d8). *)
let fld a ~double mem = rm a ~w:false [ (if double then 0xdd else 0xd9) ] ~reg:Rax mem
let fstp a ~double mem = rm a ~w:false [ (if double then 0xdd else 0xd9) ] ~reg:Rbx mem
let fprem a = byte a 0xd9; byte a 0xf8
let fnstsw_ax a = byte a 0xdf; byte a 0xe0
let fpop a = byte a 0xdd; byte a 0xd8

(* Absolute 64-bit address of a symbol, as data inside code (unused for now)
   or via [mov r64, imm64] with a relocation. *)
let mov_symbol_abs a dst symbol addend =
  rex a ~w:true ~r:0 ~b:(reg_num dst) ();
  byte a (0xb8 + (reg_num dst land 7));
  a.relocs <- Abs64 { offset = pos a; symbol; addend } :: a.relocs;
  int64 a 0L

(* Pad with multi-byte NOPs to a multiple of [n]. *)
let align a n =
  let nops =
    [|
      [];
      [ 0x90 ];
      [ 0x66; 0x90 ];
      [ 0x0f; 0x1f; 0x00 ];
      [ 0x0f; 0x1f; 0x40; 0x00 ];
      [ 0x0f; 0x1f; 0x44; 0x00; 0x00 ];
      [ 0x66; 0x0f; 0x1f; 0x44; 0x00; 0x00 ];
      [ 0x0f; 0x1f; 0x80; 0x00; 0x00; 0x00; 0x00 ];
      [ 0x0f; 0x1f; 0x84; 0x00; 0x00; 0x00; 0x00; 0x00 ];
    |]
  in
  let rec go missing =
    if missing > 0 then begin
      let k = min missing 8 in
      List.iter (byte a) nops.(k);
      go (missing - k)
    end
  in
  go ((n - (pos a mod n)) mod n)
