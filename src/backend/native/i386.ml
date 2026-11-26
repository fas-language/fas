(* Integer i386 lowering for ELF32 objects. SSA values live in frame slots.
   Code16 uses the same registers, stack frame and calling convention, with
   operand/address overrides in a 16-bit code segment. *)
open Fir
module Obj = Fas_linker.Elf_object

exception Unsupported of string

let refuse fmt = Printf.ksprintf (fun msg -> raise (Unsupported msg)) fmt

let eax = 0 and ecx = 1 and edx = 2 and ebx = 3
let esp = 4 and ebp = 5 and esi = 6 and edi = 7

let align n a = (n + a - 1) land -a
let width = function
  | Int n when n <= 32 || n = 64 -> n
  | Ptr 0 -> 32
  | t -> refuse "type %s" (ty_to_string t)

let storage n =
  if n <= 8 then 1 else if n <= 16 then 2 else if n <= 32 then 4 else 8

type section = {
  id : int;
  name : string;
  flags : int;
  bss : bool;
  bytes : Buffer.t;
  mutable zero_size : int;
  mutable alignment : int;
}

type emitter = {
  real : bool;
  stack_align : int;
  regparm : int;
  endbr : bool;
  pic : bool;
  local : string -> bool;
  thunk_calls : bool;
  reloc : section -> int -> string -> int -> int64 -> unit;
  anchor : section -> int -> string;
  extra : string -> int -> bool -> int -> section;
  section : section;
  jumps : (int * int option ref) list ref;
}

let raw b n = Buffer.add_char b (Char.chr (n land 255))
let pos e = Buffer.length e.section.bytes
let byte e n = raw e.section.bytes n
let dword e n = Buffer.add_int32_le e.section.bytes (Int64.to_int32 n)
let imm e n = dword e (Int64.of_int n)
let op32 e = if e.real then byte e 0x66
let addr32 e = if e.real then byte e 0x67
let modrm e mode reg rm = byte e ((mode lsl 6) lor (reg lsl 3) lor rm)
let rr e op dst src = op32 e; byte e op; modrm e 3 src dst
let mov e dst src = if dst <> src then rr e 0x89 dst src
let inject e reg value = op32 e; byte e (0xb8 + reg); dword e value
let lit e reg n = inject e reg (Int64.of_int n)
let push e r = op32 e; byte e (0x50 + r)
let pop e r = op32 e; byte e (0x58 + r)
let alu e ext dst n =
  op32 e;
  if n >= -128 && n <= 127 then begin
    byte e 0x83; modrm e 3 ext dst; byte e n
  end else begin
    byte e 0x81; modrm e 3 ext dst; imm e n
  end

let shift e ext dst count =
  if count <> 0 then begin
    op32 e; byte e 0xc1; modrm e 3 ext dst; byte e count
  end

let shift_cl e ext dst = op32 e; byte e 0xd3; modrm e 3 ext dst
let muldiv e ext src = op32 e; byte e 0xf7; modrm e 3 ext src
let neg e dst = muldiv e 3 dst
let imul e dst src =
  op32 e; byte e 0x0f; byte e 0xaf; modrm e 3 dst src

let mem e reg base disp =
  if disp = 0 && base <> ebp then begin
    modrm e 0 reg base;
    if base = esp then byte e 0x24
  end else if disp >= -128 && disp <= 127 then begin
    modrm e 1 reg base;
    if base = esp then byte e 0x24;
    byte e disp
  end else begin
    modrm e 2 reg base;
    if base = esp then byte e 0x24;
    imm e disp
  end

let read e size dst base disp =
  if size <> 1 && size <> 2 && size <> 4 then invalid_arg "i386 read";
  addr32 e; op32 e;
  if size = 4 then byte e 0x8b else begin
    byte e 0x0f; byte e (if size = 1 then 0xb6 else 0xb7)
  end;
  mem e dst base disp

let write e size base disp src =
  if size <> 1 && size <> 2 && size <> 4 then invalid_arg "i386 write";
  if size = 1 && src > ebx then invalid_arg "i386 byte register";
  addr32 e;
  if (size = 4 && e.real) || (size = 2 && not e.real) then byte e 0x66;
  byte e (if size = 1 then 0x88 else 0x89);
  mem e src base disp

let lea e dst base disp =
  addr32 e; op32 e; byte e 0x8d; mem e dst base disp

let field32 e sym kind addend =
  e.reloc e.section (pos e) sym kind addend;
  imm e 0

let relative e label =
  e.jumps := (pos e, label) :: !(e.jumps);
  imm e 0

let jmp e label = op32 e; byte e 0xe9; relative e label
let jcc e cc label = op32 e; byte e 0x0f; byte e (0x80 + cc); relative e label
let call_near e label = op32 e; byte e 0xe8; relative e label
let here e label = label := Some (pos e)
let cmp e x y = rr e 0x39 x y
let test e x = rr e 0x85 x x

let condition e cc =
  byte e 0x0f; byte e (0x90 + cc); byte e 0xc0;
  op32 e; byte e 0x0f; byte e 0xb6; byte e 0xc0

let cmp_condition = function
  | Eq -> 4 | Ne -> 5 | Ult -> 2 | Ule -> 6 | Ugt -> 7 | Uge -> 3
  | Slt -> 12 | Sle -> 14 | Sgt -> 15 | Sge -> 13

let signed_cmp = function Slt | Sle | Sgt | Sge -> true | _ -> false
let sign e r n = if n < 32 then (shift e 4 r (32 - n); shift e 7 r (32 - n))
let mask e r n = if n < 32 then alu e 4 r ((1 lsl n) - 1)

let extend e r = function
  | Ext (t, Sx) -> sign e r (width t)
  | Ext (t, Zx) -> mask e r (width t)
  | _ -> ()

let register_parameters e (sig_ : signature) =
  if sig_.ms_abi || sig_.ms_xmm_result then refuse "Microsoft ABI";
  if sig_.variadic then 0 else e.regparm

type argument = Register of int | Stack of int

let arg_layout e (sig_ : signature) widths =
  let register = [| eax; edx; ecx |] in
  let remaining = ref (register_parameters e sig_) and stack = ref 0 in
  let locs = Array.map (fun n ->
    let slots = if n <= 32 then 1 else 2 in
    if slots <= !remaining then begin
      let starting = register_parameters e sig_ - !remaining in
      remaining := !remaining - slots;
      Register starting
    end else begin
      remaining := 0;
      let place = Stack !stack in
      stack := !stack + (4 * slots);
      place
    end) widths in
  register, locs, !stack

let abi_width = function
  | Scalar t | Ext (t, _) -> width t
  | Agg _ | Cplx _ -> refuse "aggregate or complex ABI"

let result_abi (sig_ : signature) =
  match sig_.result_abis with
  | [] -> ()
  | [abi] -> ignore (abi_width abi)
  | _ -> refuse "multiple function results"

type frame = {
  e : emitter;
  f : func;
  slots : int array;
  addresses : int option array;
  copies : int;
  local_space : int;
  labels : int option ref array;
  va_offset : int;
  got_at : int;
  reg_thunk : int option ref;
  uses_thunk : bool ref;
}

let bits g v = width (Vec.get g.f.tys v)
let offset g v = g.slots.(resolve g.f v)
let address g v = g.addresses.(resolve g.f v)
let hi n = Int64.shift_right_logical n 32

let get g ?(high = false) reg v =
  let e = g.e in
  if high && bits g v <= 32 then lit e reg 0 else
  match const_of g.f v with
  | Some (Cint (_, words)) -> inject e reg (if high then hi words.(0) else words.(0))
  | Some (Cnull _ | Czero _ | Cpoison _) -> lit e reg 0
  | Some (Csym (name, addend)) ->
      if e.pic then begin
        read e 4 reg ebp g.got_at;
        if e.local name then begin
          byte e 0x8d; modrm e 2 reg reg; field32 e name 9 addend
        end else begin
          byte e 0x8b; modrm e 2 reg reg; field32 e name 3 0L;
          if addend <> 0L then alu e 0 reg (Int64.to_int addend)
        end
      end else begin
        op32 e; byte e (0xb8 + reg);
        field32 e name 1 addend
      end
  | Some _ -> refuse "constant of type %s" (ty_to_string (Vec.get g.f.tys v))
  | None ->
      (match address g v with
       | Some off -> lea e reg ebp off
       | None ->
           let slot = offset g v in
           if slot = 0 then refuse "value %d has no frame slot" v;
           read e 4 reg ebp (slot + if high then 4 else 0))

let put g v =
  let n = bits g v in
  mask g.e eax n;
  write g.e 4 ebp (offset g v) eax;
  if n > 32 then write g.e 4 ebp (offset g v + 4) edx

let pair g v =
  get g eax v;
  if bits g v > 32 then get g ~high:true edx v

let copy_value g slot v =
  pair g v;
  write g.e 4 ebp slot eax;
  if bits g v > 32 then write g.e 4 ebp (slot + 4) edx

let jump g t =
  let params = (Vec.get g.f.blocks t.dest).params in
  (* Stage parallel edge copies before updating any destination slot. *)
  Array.iteri (fun i v -> copy_value g (g.copies + (i * 8)) v) t.args;
  Array.iteri (fun i v ->
    let src = g.copies + (i * 8) in
    read g.e 4 eax ebp src;
    if bits g v > 32 then read g.e 4 edx ebp (src + 4);
    put g v) params;
  jmp g.e g.labels.(t.dest)

let call g sig_ callee args =
  result_abi sig_;
  List.iter (fun abi -> ignore (abi_width abi)) sig_.param_abis;
  let e = g.e in
  let widths = Array.map (bits g) args in
  let regs, locs, stack = arg_layout e sig_ widths in
  let reserve = align stack e.stack_align in
  let param_abis = Array.of_list sig_.param_abis in
  let place r i v =
    get g r v;
    if i < Array.length param_abis then extend e r param_abis.(i)
  in
  if reserve <> 0 then alu e 5 esp reserve;
  Array.iteri (fun i arg ->
    match locs.(i) with
    | Register _ -> ()
    | Stack off ->
        place eax i arg; write e 4 esp off eax;
        if widths.(i) > 32 then begin
          get g ~high:true eax arg; write e 4 esp (off + 4) eax
        end) args;
  Array.iteri (fun i arg ->
    match locs.(i) with
    | Stack _ -> ()
    | Register at ->
        place regs.(at) i arg;
        if widths.(i) > 32 then get g ~high:true regs.(at + 1) arg) args;
  (match callee with
   | Direct name ->
       let through_plt = e.pic && not (e.local name) in
       if through_plt then read e 4 ebx ebp g.got_at;
       op32 e; byte e 0xe8; field32 e name (if through_plt then 4 else 2) (-4L)
   | Indirect v ->
       get g edi v;
       if e.thunk_calls then begin
         g.uses_thunk := true;
         call_near e g.reg_thunk
       end else begin op32 e; byte e 0xff; modrm e 3 2 edi end);
  if reserve <> 0 then alu e 0 esp reserve

let wide_compare e k =
  let high_diff = ref None and done_ = ref None in
  cmp e edx edi;
  jcc e 5 high_diff;
  cmp e eax ecx;
  let unsigned = match k with Slt -> Ult | Sle -> Ule | Sgt -> Ugt | Sge -> Uge | _ -> k in
  condition e (cmp_condition unsigned);
  jmp e done_;
  here e high_diff;
  condition e (cmp_condition k);
  here e done_

let wide_shift e kind =
  let less = ref None and done_ = ref None in
  alu e 7 ecx 32;
  jcc e 2 less;
  (* Shifts by >= 64 are poison. For valid counts, in the high case
     the hardware's modulo-32 count is the desired count - 32. *)
  (match kind with
   | Shl -> mov e edx eax; shift_cl e 4 edx; lit e eax 0
   | Lshr -> mov e eax edx; shift_cl e 5 eax; lit e edx 0
   | Ashr -> mov e eax edx; shift_cl e 7 eax; shift e 7 edx 31
   | _ -> assert false);
  jmp e done_;
  here e less;
  (match kind with
   | Shl -> op32 e; byte e 0x0f; byte e 0xa5; modrm e 3 eax edx; shift_cl e 4 eax
   | Lshr | Ashr ->
       op32 e; byte e 0x0f; byte e 0xad; modrm e 3 edx eax;
       shift_cl e (if kind = Lshr then 5 else 7) edx
   | _ -> assert false);
  here e done_

let negate_pair e low high =
  neg e low; alu e 2 high 0; neg e high

let wide_divide g kind flags =
  let e = g.e in
  let signed = kind = Sdiv || kind = Srem in
  let remainder = kind = Urem || kind = Srem in
  let quotient_sign = g.local_space and remainder_sign = g.local_space + 4 in
  if signed then begin
    let dividend_pos = ref None and divisor_pos = ref None in
    mov e esi edx; rr e 0x31 esi edi;
    write e 4 ebp quotient_sign esi;
    write e 4 ebp remainder_sign edx;
    if has flags f_trap then begin
      let safe = ref None in
      alu e 7 edx 0x8000_0000; jcc e 5 safe;
      test e eax; jcc e 5 safe;
      alu e 7 edi (-1); jcc e 5 safe;
      alu e 7 ecx (-1); jcc e 5 safe;
      byte e 0x0f; byte e 0x0b;
      here e safe
    end;
    test e edx; jcc e 9 dividend_pos;
    negate_pair e eax edx;
    here e dividend_pos;
    test e edi; jcc e 9 divisor_pos;
    negate_pair e ecx edi;
    here e divisor_pos
  end;
  if has flags f_trap then begin
    let nonzero = ref None in
    mov e esi ecx; rr e 0x09 esi edi; jcc e 5 nonzero;
    byte e 0x0f; byte e 0x0b;
    here e nonzero
  end;
  (* Restoring division. The numerator shifts into EBX:ESI, with the
     quotient bits feeding back into the vacated low bits of EDX:EAX. *)
  let counter = g.local_space + 8 in
  lit e esi 64; write e 4 ebp counter esi;
  lit e esi 0; lit e ebx 0;
  let loop = ref None and subtract = ref None and next = ref None in
  here e loop;
  rr e 0x01 eax eax;
  rr e 0x11 edx edx;
  rr e 0x11 esi esi;
  rr e 0x11 ebx ebx;
  jcc e 2 subtract;
  cmp e ebx edi; jcc e 2 next; jcc e 7 subtract;
  cmp e esi ecx; jcc e 2 next;
  here e subtract;
  rr e 0x29 esi ecx;
  rr e 0x19 ebx edi;
  alu e 1 eax 1;
  here e next;
  addr32 e; op32 e; byte e 0xff; mem e 1 ebp counter;
  jcc e 5 loop;
  if remainder then begin mov e eax esi; mov e edx ebx end;
  if signed then begin
    let positive = ref None in
    read e 4 esi ebp (if remainder then remainder_sign else quotient_sign);
    test e esi; jcc e 9 positive;
    negate_pair e eax edx;
    here e positive
  end

let divide g kind flags n =
  let e = g.e in
  let signed = kind = Sdiv || kind = Srem in
  if signed then begin
    sign e eax n; sign e ecx n;
    if n < 32 && has flags f_trap then begin
      let safe = ref None in
      alu e 7 eax (-(1 lsl (n - 1))); jcc e 5 safe;
      alu e 7 ecx (-1); jcc e 5 safe;
      byte e 0x0f; byte e 0x0b;
      here e safe
    end;
    op32 e; byte e 0x99
  end else lit e edx 0;
  muldiv e (if signed then 7 else 6) ecx;
  if kind = Urem || kind = Srem then mov e eax edx

let integer_binary g kind flags x y =
  let e = g.e and n = bits g x in
  pair g x; get g ecx y;
  if n > 32 then get g ~high:true edi y;
  match kind with
  | Add -> rr e 0x01 eax ecx; if n > 32 then rr e 0x11 edx edi
  | Sub -> rr e 0x29 eax ecx; if n > 32 then rr e 0x19 edx edi
  | Mul when n <= 32 -> imul e eax ecx
  | Mul ->
      imul e edx ecx; imul e edi eax; rr e 0x01 edi edx;
      muldiv e 4 ecx; rr e 0x01 edx edi
  | And -> rr e 0x21 eax ecx; if n > 32 then rr e 0x21 edx edi
  | Or -> rr e 0x09 eax ecx; if n > 32 then rr e 0x09 edx edi
  | Xor -> rr e 0x31 eax ecx; if n > 32 then rr e 0x31 edx edi
  | (Shl | Lshr | Ashr) when n > 32 -> wide_shift e kind
  | Shl -> shift_cl e 4 eax
  | Lshr -> shift_cl e 5 eax
  | Ashr -> sign e eax n; shift_cl e 7 eax
  | Udiv | Urem | Sdiv | Srem ->
      if n > 32 then wide_divide g kind flags else divide g kind flags n
  | _ -> refuse "integer operation"

let scan_bits e kind bits =
  let nonzero = ref None and done_ = ref None in
  test e eax; jcc e 5 nonzero;
  lit e eax bits;
  jmp e done_;
  here e nonzero;
  op32 e; byte e 0x0f; byte e (if kind = Ctz then 0xbc else 0xbd);
  modrm e 3 eax eax;
  if kind = Clz then begin
    lit e ecx (bits - 1); rr e 0x29 ecx eax; mov e eax ecx
  end;
  here e done_

let population e reg count =
  let loop = ref None and done_ = ref None in
  here e loop;
  test e reg; jcc e 4 done_;
  lea e ecx reg (-1); rr e 0x21 reg ecx;
  alu e 0 count 1;
  jmp e loop;
  here e done_

let reverse_bytes e reg bits =
  let m = if bits = 16 then 255 else 0x00ff00ff in
  mov e ecx reg;
  alu e 4 reg m; shift e 4 reg 8;
  shift e 5 ecx 8; alu e 4 ecx m; rr e 0x09 reg ecx;
  if bits = 32 then shift e 0 reg 16

let integer_unary g kind v =
  let e = g.e and n = bits g v in
  pair g v;
  match kind with
  | Abs ->
      let positive = ref None in
      if n > 32 then begin
        test e edx; jcc e 9 positive;
        negate_pair e eax edx
      end else begin
        sign e eax n; test e eax; jcc e 9 positive;
        neg e eax
      end;
      here e positive
  | (Clz | Ctz) when n > 32 ->
      let second = ref None and done_ = ref None in
      if kind = Clz then begin mov e edi eax; mov e eax edx end
      else mov e edi edx;
      test e eax; jcc e 4 second;
      scan_bits e kind 32; jmp e done_;
      here e second;
      mov e eax edi; scan_bits e kind 32; alu e 0 eax 32;
      here e done_; lit e edx 0
  | Clz | Ctz -> scan_bits e kind n
  | Popcnt ->
      lit e edi 0; population e eax edi;
      if n > 32 then population e edx edi;
      mov e eax edi
  | Bswap when n = 64 ->
      reverse_bytes e eax 32; reverse_bytes e edx 32;
      mov e ecx eax; mov e eax edx; mov e edx ecx
  | Bswap when n = 16 || n = 32 -> reverse_bytes e eax n
  | Bswap when n = 8 -> ()
  | _ -> refuse "integer operation %s" (Fir_print.iun_name kind)

let copy_bytes e kind =
  (* EDI = dst, ESI = src, ECX = size. The DF stays clear at ABI boundaries.
     String stores always use ES. In real mode C pointers are relative to DS. *)
  if e.real then begin byte e 0x06; byte e 0x1e; byte e 0x07 end;
  (match kind with
  | `Set -> addr32 e; byte e 0xf3; byte e 0xaa
  | `Copy -> addr32 e; byte e 0xf3; byte e 0xa4
  | `Move ->
      let forward = ref None and done_ = ref None in
      test e ecx; jcc e 4 done_;
      cmp e edi esi; jcc e 6 forward;
      rr e 0x01 edi ecx; alu e 5 edi 1;
      rr e 0x01 esi ecx; alu e 5 esi 1;
      byte e 0xfd; addr32 e; byte e 0xf3; byte e 0xa4; byte e 0xfc;
      jmp e done_;
      here e forward;
      addr32 e; byte e 0xf3; byte e 0xa4;
      here e done_);
  if e.real then byte e 0x07

let inline_asm g results ~tmpl ~aflags ~outs ~ins ~aargs ~clobbers =
  let e = g.e in
  let module A = I386_asm in
  let p = A.plan g.f ~tmpl ~aflags ~outs ~ins ~aargs ~clobbers in
  let snippet = A.assemble ~real:e.real p.source in
  List.iter (fun (reg, value) -> get g reg value) p.loads;
  let parts = Hashtbl.create 4 in
  let splice s (part : A.part) =
    let at = Buffer.length s.bytes in
    let off = align at part.alignment in
    s.alignment <- max s.alignment part.alignment;
    let pad = if s.flags land 4 <> 0 then '\x90' else '\000' in
    if off <> at then Buffer.add_string s.bytes (String.make (off - at) pad);
    Buffer.add_string s.bytes part.code;
    Hashtbl.add parts part.index (lazy (e.anchor s off));
    (s, off, part.relocations)
  in
  let text = splice e.section snippet.text in
  let extra = List.map (fun (part : A.part) ->
    splice (e.extra part.name part.flags false part.alignment) part) snippet.other in
  List.iter (fun (part, off, rs) ->
    List.iter (fun (r : A.relocation) ->
      let symbol =
        match r.target with `Section i -> Lazy.force (Hashtbl.find parts i) | `Symbol s -> s in
      e.reloc part (off + r.at) symbol r.kind r.addend) rs) (text :: extra);
  let assigned = List.combine (Array.to_list results) p.outputs in
  List.iter (function
    | (v, A.Reg (reg, _)) -> write e 4 ebp (offset g v) reg
    | (v, A.Flag cc) ->
        let cond = X64.cond_num (Mir_isel.cond_of_cc cc) in
        addr32 e; byte e 0x0f; byte e (0x90 + cond); mem e 0 ebp (offset g v)
    | _ -> invalid_arg "i386 inline asm output") assigned;
  List.iter (fun (v, out) ->
    if bits g v < 32 then begin
      let bytes = match out with A.Flag _ -> 1 | _ -> 4 in
      read e bytes eax ebp (offset g v);
      put g v
    end) assigned

let instruction g (ir : inst_rec) =
  let e = g.e in
  let output () =
    if Array.length ir.results <> 1 then refuse "expected a scalar result";
    ir.results.(0)
  in
  let output_bits () = bits g (output ()) in
  let finish () = put g (output ()) in
  let memory ptr =
    match address g ptr with
    | Some off -> (ebp, off)
    | None -> get g esi ptr; (esi, 0)
  in
  match ir.op with
  | Nop | Dbg_value _ | Lifetime _ -> ()
  | Mark _ -> refuse "constant-time markers"
  | Freeze v -> pair g v; finish ()
  | Ibin (kind, flags, x, y) -> integer_binary g kind flags x y; finish ()
  | Iun (kind, _, v) -> integer_unary g kind v; finish ()
  | Icmp (kind, x, y) ->
      pair g x; get g ecx y;
      let n = bits g x in
      if n > 32 then (get g ~high:true edi y; wide_compare e kind)
      else begin
        if signed_cmp kind then begin sign e eax n; sign e ecx n end;
        cmp e eax ecx; condition e (cmp_condition kind)
      end;
      finish ()
  | Conv ((Trunc | Zext | Sext | Ptrtoint | Inttoptr | Bitcast) as conv, v) ->
      let n = bits g v and dst = output_bits () in
      pair g v;
      if conv = Sext && n < 32 then sign e eax n;
      if dst > 32 && n <= 32 then begin
        if conv = Sext then (mov e edx eax; shift e 7 edx 31) else lit e edx 0
      end;
      finish ()
  | Select (flag, a, b) ->
      let no = ref None and done_ = ref None in
      get g eax flag; test e eax; jcc e 4 no;
      pair g a; jmp e done_;
      here e no; pair g b; here e done_;
      finish ()
  | (Ptradd _ | Alloca _) when address g (output ()) <> None -> ()
  | Ptradd (_, ptr, index, scale, disp) ->
      get g eax ptr; get g ecx index;
      if scale <> 1 then begin lit e edx scale; imul e ecx edx end;
      rr e 0x01 eax ecx;
      if disp <> 0 then alu e 0 eax disp;
      finish ()
  | Alloca (count, alignment) ->
      get g eax count;
      alu e 0 eax (e.stack_align - 1);
      alu e 4 eax (-e.stack_align);
      rr e 0x29 esp eax;
      if alignment > e.stack_align then alu e 4 esp (-alignment);
      mov e eax esp; finish ()
  | Load (m, ptr) ->
      if m.order <> Plain then refuse "atomic load";
      let base, at = memory ptr in
      let size = storage (output_bits ()) in
      read e (min 4 size) eax base at;
      if size = 8 then read e 4 edx base (at + 4);
      finish ()
  | Store (m, v, ptr) ->
      if m.order <> Plain then refuse "atomic store";
      let base, at = memory ptr in
      pair g v;
      let size = storage (bits g v) in
      write e (min 4 size) base at eax;
      if size = 8 then write e 4 base (at + 4) edx
  | Memcpy (_, dst, src, count) | Memmove (_, dst, src, count) ->
      get g edi dst; get g esi src; get g ecx count;
      copy_bytes e (match ir.op with Memcpy _ -> `Copy | _ -> `Move)
  | Memset (_, dst, value, count) ->
      get g edi dst; get g eax value; get g ecx count;
      copy_bytes e `Set
  | Call { sig_; callee; args; va_aggs; _ } ->
      if va_aggs <> [] then refuse "variadic aggregate arguments";
      call g sig_ callee args;
      if Array.length ir.results <> 0 then finish ()
  | Stack_save -> mov e eax esp; finish ()
  | Stack_restore v -> get g eax v; mov e esp eax
  | Va_start p ->
      get g esi p; lea e eax ebp g.va_offset; write e 4 esi 0 eax
  | Va_arg p ->
      get g esi p; read e 4 edi esi 0;
      let size = storage (output_bits ()) in
      read e (min 4 size) eax edi 0;
      if size = 8 then read e 4 edx edi 4;
      alu e 0 edi (max 4 size);
      write e 4 esi 0 edi; finish ()
  | Asm { tmpl; aflags; outs; ins; aargs; clobbers } ->
      inline_asm g ir.results ~tmpl ~aflags ~outs ~ins ~aargs ~clobbers
  | Trap -> byte e 0x0f; byte e 0x0b
  | _ -> refuse "instruction %s" (Fir_print.op_text g.f (Fir_print.assign_names g.f) ir)

let terminator g = function
  | Jump t -> jump g t
  | Br (cond, yes, no, _) ->
      let false_ = ref None in
      get g eax cond; test g.e eax; jcc g.e 4 false_;
      jump g yes;
      here g.e false_;
      jump g no
  | Switch (v, otherwise, cases, _) ->
      Array.iter (fun (key, dest) ->
        let next = ref None in
        pair g v;
        inject g.e ecx key.(0); cmp g.e eax ecx; jcc g.e 5 next;
        if bits g v > 32 then begin
          inject g.e ecx (hi key.(0)); cmp g.e edx ecx; jcc g.e 5 next
        end;
        jump g dest; here g.e next) cases;
      jump g otherwise
  | Ret vs ->
      (match Array.to_list vs, g.f.sig_.result_abis with
       | [], [] -> ()
       | [v], [abi] -> pair g v; extend g.e eax abi
       | _ -> refuse "multiple return values");
      read g.e 4 ebx ebp (-4);
      read g.e 4 esi ebp (-8);
      read g.e 4 edi ebp (-12);
      op32 g.e; byte g.e 0xc9; op32 g.e; byte g.e 0xc3
  | Unreachable -> byte g.e 0x0f; byte g.e 0x0b
  | Indirect_br _ | Asm_goto _ -> refuse "indirect branch or asm goto"

let body e (f : func) =
  result_abi f.sig_;
  if f.unwind_cleanup <> None || f.patchable <> None || has f.fattrs fa_secret then
    refuse "unwind cleanup, patchable entry or secret function";
  let entry = Vec.get f.blocks 0 in
  let regs, arguments, stack =
    arg_layout e f.sig_ (Array.of_list (List.map abi_width f.sig_.param_abis)) in
  let needed = ref 12 in
  let addresses = Array.make (num_values f) None in
  let count v =
    match const_of f v with
    | Some (Cint (_, [| n |])) -> Some n
    | _ -> None in
  (* Place small fixed allocas directly in the frame. C loads and stores can
     then address these slots without pinning a second copy of each pointer. *)
  Vec.iter (fun (block : block_rec) ->
    Vec.iter (fun i ->
      let ir = inst f i in
      match ir.op with
      | Alloca (size, alignment) when alignment <= e.stack_align ->
          (match count size with
           | Some n when n >= 0L && n <= 0x0100_0000L ->
               needed := align (!needed + max 1 (Int64.to_int n) + 8) alignment - 8;
               addresses.(ir.results.(0)) <- Some (- !needed)
           | _ -> ())
      | _ -> ()) block.insts) f.blocks;
  let rec frame_address v =
    let v = resolve f v in
    match addresses.(v), def_of f v with
    | Some off, _ -> Some off
    | None, Result (i, _) ->
        (match (inst f i).op with
         | Ptradd (_, base, idx, scale, disp) ->
             (match frame_address base, count idx with
              | Some base, Some idx ->
                  let off = Int64.(add (mul idx (of_int scale)) (of_int (base + disp))) in
                  let off = Int64.to_int (Int64.of_int32 (Int64.to_int32 off)) in
                  addresses.(v) <- Some off; Some off
              | _ -> None)
         | _ -> None)
    | _ -> None
  in
  let slots = Array.init (num_values f) (fun v ->
    match def_of f v, frame_address v with
    | (Param _ | Result _), None ->
        let n = width (Vec.get f.tys v) in
        needed := !needed + (if n > 32 then 8 else 4);
        - !needed
    | _ -> 0) in
  let most_params = Vec.fold (fun n b -> max n (Array.length b.params)) 0 f.blocks in
  needed := !needed + (most_params * 8);
  let copies = - !needed in
  needed := !needed + (if e.pic then 16 else 12);
  let local_space = - !needed in
  let frame_size = align (!needed + 8) e.stack_align - 8 in
  let labels = Array.init (num_blocks f) (fun _ -> ref None) in
  let get_pc = ref None in
  let g =
    { e; f; slots; addresses; copies; local_space; labels; va_offset = 8 + stack;
      got_at = local_space + 12; reg_thunk = ref None; uses_thunk = ref false } in
  if e.endbr && not (has f.fattrs fa_nocf_check) then begin
    byte e 0xf3; byte e 0x0f; byte e 0x1e; byte e 0xfb
  end;
  push e ebp; mov e ebp esp; alu e 5 esp frame_size;
  write e 4 ebp (-4) ebx; write e 4 ebp (-8) esi; write e 4 ebp (-12) edi;
  Array.iteri (fun i v ->
    match arguments.(i) with
    | Stack _ -> ()
    | Register at ->
        (* Save registers before processing any stack arguments, since
           even narrowing an early scalar can overwrite another input. *)
        write e 4 ebp (offset g v) regs.(at);
        if bits g v > 32 then write e 4 ebp (offset g v + 4) regs.(at + 1))
    entry.params;
  if e.pic then begin
    call_near e get_pc;
    let returned = pos e in
    byte e 0x81; modrm e 3 0 eax;
    field32 e "_GLOBAL_OFFSET_TABLE_" 10 (Int64.of_int (pos e - returned));
    write e 4 ebp g.got_at eax
  end;
  Array.iteri (fun i v ->
    match arguments.(i) with
    | Register _ -> ()
    | Stack off ->
        read e 4 eax ebp (off + 8);
        if bits g v > 32 then read e 4 edx ebp (off + 12);
        put g v) entry.params;
  Vec.iteri (fun b (block : block_rec) ->
    here e labels.(b);
    Vec.iter (fun i -> instruction g (inst f i)) block.insts;
    terminator g block.term) f.blocks;
  if e.pic then begin
    here e get_pc; read e 4 eax esp 0; byte e 0xc3
  end;
  if !(g.uses_thunk) then begin
    let replace_return = ref None and spin = ref None in
    here e g.reg_thunk;
    call_near e replace_return;
    here e spin;
    byte e 0xf3; byte e 0x90;
    byte e 0x0f; byte e 0xae; byte e 0xe8;
    jmp e spin;
    here e replace_return;
    write e 4 esp 0 edi;
    op32 e; byte e 0xc3
  end

let binding = function Local -> 0 | Global -> 1 | Weak -> 2
let visibility = function Default -> 0 | Hidden -> 2 | Protected -> 3
let integer n = Int64.of_int n

let compile_object ~stack_align ~regparm ~endbr ~pic ~thunk_calls ~iter_funcs (m : modul) =
  let sections = Dynarray.create () and symbols = Dynarray.create () in
  let section_names = Hashtbl.create 16 and symbol_names = Hashtbl.create 16 in
  let declarations = Hashtbl.create 16 in
  let relocations = ref [] and jumps = ref [] in
  let local name =
    match Hashtbl.find_opt declarations name with
    | Some (0, _) | Some (_, (2 | 3)) -> true
    | _ -> false in
  let section name flags bss alignment =
    match Hashtbl.find_opt section_names name with
    | Some s ->
        if s.flags <> flags || s.bss <> bss then refuse "conflicting flags for section %s" name;
        s.alignment <- max alignment s.alignment; s
    | None ->
        let s = { id = Dynarray.length sections; name; flags; bss;
                  bytes = Buffer.create 128; zero_size = 0; alignment } in
        Dynarray.add_last sections s;
        Hashtbl.add section_names name s; s
  in
  let sym name =
    match Hashtbl.find_opt symbol_names name with
    | Some i -> i
    | None ->
        let bind, vis = Option.value (Hashtbl.find_opt declarations name) ~default:(1, 0) in
        let undef : Obj.symbol =
          { name; binding = bind; visibility = vis; kind = 0; size = 0L; definition = Undefined } in
        let i = Dynarray.length symbols in
        Dynarray.add_last symbols undef;
        Hashtbl.add symbol_names name i; i
  in
  let record name linkage vis kind definition size =
    Dynarray.set symbols (sym name)
      { Obj.name; binding = binding linkage; visibility = visibility vis;
        kind; definition; size = integer size }
  in
  let reloc (target : section) offset name kind addend =
    let r : Obj.relocation =
      { section = target.id; offset; symbol = sym name; kind; addend } in
    relocations := r :: !relocations
  in
  let location (s : section) offset = Obj.Section (s.id, integer offset) in
  let id = ref 0 in
  let rec anchor s off =
    let name = Printf.sprintf ".Lfas.i386.asm.%d" !id in
    incr id;
    if Hashtbl.mem declarations name || Hashtbl.mem symbol_names name then anchor s off
    else begin
      record name Local Default 0 (location s off) 0;
      name
    end in
  let pad s size at =
    let count = at - size in
    if count <> 0 then Buffer.add_string s.bytes (String.make count '\000')
  in
  let emit_function (f : func) =
    if f.defined then begin
      let s = section (Option.value f.code_section ~default:".text") 6 false 4 in
      let e = { real = m.triple = i386_code16; stack_align; regparm; endbr; pic;
                local; thunk_calls; reloc; anchor; extra = section; section = s; jumps = ref [] } in
      let start = align (pos e) 4 in
      pad s (pos e) start;
      (try body e f with
      | Unsupported reason | I386_asm.Unsupported reason -> refuse "@%s: %s" f.name reason);
      List.iter (fun (off, label) -> jumps := (s.id, off, label) :: !jumps) !(e.jumps);
      record f.name f.linkage f.visibility 2 (location s start) (pos e - start)
    end
  in
  let emit_global (g : global) =
    let name = g.gname in
    let define = record name g.glinkage g.gvisibility in
    if g.thread_local && g.gdefined then refuse "thread-local global @%s" name;
    if not g.gdefined then ()
    else if g.gcommon then define 1 (Common (integer g.galign)) g.size
    else begin
      let default =
        if g.readonly then begin
          let has_address = function Reloc { pcrel = false; _ } -> true | _ -> false in
          if pic && (match g.init with Some init -> List.exists has_address init | None -> false) then ".data.rel.ro" else ".rodata"
        end else if g.init = None then ".bss" else ".data" in
      let section_name = Option.value g.section ~default in
      let bss = g.init = None &&
        (section_name = ".bss" || String.starts_with ~prefix:".bss." section_name) in
      let flags = if g.readonly && section_name <> ".data.rel.ro" then 2 else 3 in
      let s = section section_name flags bss g.galign in
      let size = if bss then s.zero_size else Buffer.length s.bytes in
      let at = align size g.galign in
      if bss then s.zero_size <- at + g.size
      else begin
        pad s size at;
        let zero n = if n <> 0 then Buffer.add_string s.bytes (String.make n '\000') in
        match g.init with
        | None -> zero g.size
        | Some init -> List.iter (function
            | Fir.Bytes bytes -> Buffer.add_string s.bytes bytes
            | Zeros n -> zero n
            | Reloc { sym; addend; width = 4; pcrel } ->
                reloc s (Buffer.length s.bytes) sym (if pcrel then 2 else 1) addend;
                zero 4
            | _ -> refuse "initializer for @%s" name) init
      end;
      define 1 (location s at) g.size
    end
  in
  let emit_alias (a : alias) =
    let target = Dynarray.get symbols (sym a.target) in
    let def =
      match target.definition with
      | Section (s, off) -> Obj.Section (s, Int64.add off a.addend)
      | Absolute bits -> Obj.Absolute (Int64.add bits a.addend)
      | _ -> refuse "@%s: alias target @%s has no definition" a.aname a.target in
    record a.aname a.alinkage a.avisibility target.kind def
      (if a.addend = 0L then Int64.to_int target.size else 0)
  in
  try
    if Vec.length m.ifuncs <> 0 then refuse "GNU indirect functions";
    if m.module_asm <> [] then refuse "module assembly";
    let declaration name linkage vis =
      Hashtbl.replace declarations name (binding linkage, visibility vis) in
    Vec.iter (fun (f : func) -> declaration f.name f.linkage f.visibility) m.funcs;
    Vec.iter (fun g -> declaration g.gname g.glinkage g.gvisibility) m.globals;
    Vec.iter (fun a -> declaration a.aname a.alinkage a.avisibility) m.aliases;
    iter_funcs emit_function;
    Vec.iter emit_global m.globals;
    Vec.iter emit_alias m.aliases;
    ignore (section ".note.GNU-stack" 0 false 1);
    let data = Array.init (Dynarray.length sections) (fun i -> Buffer.to_bytes (Dynarray.get sections i).bytes) in
    List.iter (fun (i, off, target) ->
      match !target with
      | Some loc -> Bytes.set_int32_le data.(i) off (Int32.of_int (loc - off - 4))
      | None -> invalid_arg "i386: unresolved control-flow label") !jumps;
    let secs = Array.mapi (fun i bytes ->
      let s = Dynarray.get sections i in
      { Obj.name = s.name; flags = s.flags; alignment = s.alignment;
        contents = if s.bss then Bss s.zero_size else Data bytes }) data in
    Ok (Obj.write ~encoding:Obj.I386
      { sections = secs; symbols = Dynarray.to_array symbols;
        relocations = Array.of_list (List.rev !relocations) })
  with
  | Unsupported reason -> Error ("native i386 backend: " ^ reason)
  | Fas_linker.Error.Error reason -> Error ("native i386 backend: " ^ reason)
