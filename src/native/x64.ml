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
  | Abs64 of { offset : int; symbol : string; addend : int }
  | Abs32s of { offset : int; symbol : string; addend : int }
      (** 32-bit absolute, sign-extended by the CPU. *)

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

let lea a dst mem = rm a [ 0x8d ] ~reg:dst mem

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
let imul_rr a dst src = rr a [ 0x0f; 0xaf ] ~reg:dst ~rm:src

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
let not_ a r = ext_r a [ 0xf7 ] 2 r
let div a r = ext_r a [ 0xf7 ] 6 r
let idiv a r = ext_r a [ 0xf7 ] 7 r

let cqo a =
  byte a 0x48;
  byte a 0x99

(* setcc r8, then zero-extend to 64 bits *)
let setcc a c r =
  rex a ~force:(reg_num r >= 4) ~w:false ~r:0 ~b:(reg_num r) ();
  byte a 0x0f;
  byte a (0x90 + cond_num c);
  modrm a 3 0 (reg_num r);
  zero_extend a 8 r

let cmov a c dst src = rr a [ 0x0f; 0x40 + cond_num c ] ~reg:dst ~rm:src

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
  a.relocs <- Rel32 { offset = pos a; symbol; addend = 0 } :: a.relocs;
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
