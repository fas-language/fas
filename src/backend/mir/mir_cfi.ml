(* Mir_cfi: .eh_frame call frame information for the machine-IR backend.

   Every function has the same frame shape (Mir_emit.func):

     push rbp              CFA = rsp+16, rbp saved at CFA-16
     mov rbp, rsp          CFA = rbp+16 from here on
     push r_0 ... r_k-1    r_i saved at CFA-24-8i
     sub rsp, N            (and dynamic allocas) leave the CFA alone
     ...
     lea rsp, [rbp-8k]     \
     pop r_k-1 ... r_0      | each return; the CFA is still rbp+16
     pop rbp               /  CFA = rsp+8
     ret                      back to the body's rules after it

   A return in the middle of a function is bracketed by remember_state /
   restore_state, so the code after it gets the body's rules again. Between
   the pops and the ret, the rules still say "r saved at CFA-n". That memory
   is below rsp now, but within the 128-byte red zone, which signal delivery
   leaves alone, so it still holds the saved values.

   _start gets an FDE whose return address is undefined: unwinders stop
   there.

   One CIE ("zR", pointers pc-relative sdata4) and one FDE per function.
   pc_begin is a 32-bit PC-relative relocation against a local symbol at
   the start of .text (not the function's own symbol: a weak function may
   be overridden, and its FDE must still describe this copy). *)

module A = X64

(* DWARF register numbers of x86-64 (System V psABI, figure 3.36) *)
let dwarf_reg = function
  | A.Rax -> 0 | Rdx -> 1 | Rcx -> 2 | Rbx -> 3 | Rsi -> 4 | Rdi -> 5 | Rbp -> 6 | Rsp -> 7
  | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11 | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

let ra_reg = 16
let text_symbol = ".Lfas.text"

(* what happened at a text offset inside a function *)
type event =
  | Pushed_rbp            (* after push rbp *)
  | Framed                (* after mov rbp, rsp *)
  | Saved of A.reg * int  (* after the i-th callee-saved push (0-based) *)
  | Epilogue              (* before lea rsp, [rbp-..] of a return *)
  | Popped_rbp            (* after pop rbp *)
  | Returned              (* after ret, when code follows *)

type fde = { start : int; size : int; events : (int * event) list (* offsets from start, in order *); entry : bool }

let uleb b n =
  let n = ref n in
  let continue = ref true in
  while !continue do
    let byte = !n land 0x7f in
    n := !n lsr 7;
    if !n = 0 then begin Buffer.add_char b (Char.chr byte); continue := false end
    else Buffer.add_char b (Char.chr (byte lor 0x80))
  done

let sleb b n =
  let n = ref n in
  let continue = ref true in
  while !continue do
    let byte = !n land 0x7f in
    n := !n asr 7;
    if (!n = 0 && byte land 0x40 = 0) || (!n = -1 && byte land 0x40 <> 0) then begin
      Buffer.add_char b (Char.chr byte);
      continue := false
    end
    else Buffer.add_char b (Char.chr (byte lor 0x80))
  done

let u8 b n = Buffer.add_char b (Char.chr (n land 0xff))

let u32 b n =
  for i = 0 to 3 do
    u8 b ((n lsr (8 * i)) land 0xff)
  done

let set_u32 b at n =
  for i = 0 to 3 do
    Bytes.set b (at + i) (Char.chr ((n lsr (8 * i)) land 0xff))
  done

(* DW_CFA_* opcodes *)
let advance b delta =
  if delta < 0 then invalid_arg "Mir_cfi: events out of order"
  else if delta = 0 then ()
  else if delta < 0x40 then u8 b (0x40 lor delta)
  else if delta < 0x100 then (u8 b 0x02; u8 b delta)
  else if delta < 0x10000 then (u8 b 0x03; u8 b delta; u8 b (delta lsr 8))
  else (u8 b 0x04; u32 b delta)

let def_cfa b reg off = u8 b 0x0c; uleb b reg; uleb b off
let def_cfa_offset b off = u8 b 0x0e; uleb b off
let def_cfa_register b reg = u8 b 0x0d; uleb b reg
let offset b reg factored = u8 b (0x80 lor reg); uleb b factored (* reg saved at CFA + factored * data_align *)
let remember_state b = u8 b 0x0a
let restore_state b = u8 b 0x0b
let undefined b reg = u8 b 0x07; uleb b reg

(* pad a record to a multiple of 8 with DW_CFA_nop *)
let pad8 b from =
  while (Buffer.length b - from) mod 8 <> 0 do
    u8 b 0
  done

(* the .eh_frame bytes and their relocations *)
let build (fdes : fde list) =
  let b = Buffer.create 1024 and relocs = ref [] in
  (* CIE *)
  let cie = Buffer.length b in
  u32 b 0 (* length, patched *);
  u32 b 0 (* CIE id *);
  u8 b 1 (* version *);
  Buffer.add_string b "zR\000";
  uleb b 1 (* code alignment *);
  sleb b (-8) (* data alignment *);
  uleb b ra_reg;
  uleb b 1 (* augmentation data length *);
  u8 b 0x1b (* FDE pointers: DW_EH_PE_pcrel | DW_EH_PE_sdata4 *);
  def_cfa b 7 8 (* at a call: CFA = rsp+8 *);
  offset b ra_reg 1 (* return address at CFA-8 *);
  pad8 b cie;
  let patches = ref [ (cie, Buffer.length b - cie - 4) ] in
  List.iter
    (fun f ->
      let at = Buffer.length b in
      u32 b 0 (* length, patched *);
      u32 b (Buffer.length b - cie) (* CIE pointer: back from this field to the CIE *);
      relocs := A.Rel32 { offset = Buffer.length b; symbol = text_symbol; addend = f.start + 4 } :: !relocs;
      u32 b 0 (* pc_begin *);
      u32 b f.size (* pc_range *);
      uleb b 0 (* augmentation data length *);
      if f.entry then undefined b ra_reg;
      let loc = ref 0 in
      List.iter
        (fun (o, ev) ->
          advance b (o - !loc);
          loc := o;
          match ev with
          | Pushed_rbp -> def_cfa_offset b 16; offset b 6 2
          | Framed -> def_cfa_register b 6
          | Saved (r, i) -> offset b (dwarf_reg r) (3 + i)
          | Epilogue -> remember_state b
          | Popped_rbp -> def_cfa b 7 8
          | Returned -> restore_state b)
        f.events;
      pad8 b at;
      patches := (at, Buffer.length b - at - 4) :: !patches)
    fdes;
  u32 b 0 (* terminator *);
  let bytes = Buffer.to_bytes b in
  List.iter (fun (at, len) -> set_u32 bytes at len) !patches;
  (bytes, List.rev !relocs)
