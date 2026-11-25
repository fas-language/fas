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

   One FDE per function references a CIE for ordinary ("zR") or cleanup
   ("zPLR") frames, with PC-relative sdata4 pointers.
   pc_begin is a 32-bit PC-relative relocation against a local symbol at
   the start of the code section. A weak function may be overridden, and
   its FDE must still describe this copy. *)

module A = X64

(* DWARF register numbers of x86-64 (System V psABI, figure 3.36) *)
let dwarf_reg = function
  | A.Rax -> 0 | Rdx -> 1 | Rcx -> 2 | Rbx -> 3 | Rsi -> 4 | Rdi -> 5 | Rbp -> 6 | Rsp -> 7
  | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11 | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

let ra_reg = 16
let text_symbol = ".Lfas.text"
let lsda_symbol = ".Lfas.lsda"
let personality_slot = ".Lfas.personality"
let personality = "__gcc_personality_v0"

(* what happened at a text offset inside a function *)
type event =
  | Pushed_rbp            (* after push rbp *)
  | Framed                (* after mov rbp, rsp *)
  | Saved of A.reg * int  (* after the i-th callee-saved push (0-based) *)
  | Saved_xmm of int * int  (* xmm number and its downward offset from rbp *)
  | Restored_xmm of int
  | Epilogue              (* before lea rsp, [rbp-..] of a return *)
  | Popped_rbp            (* after pop rbp *)
  | Returned              (* after ret, when code follows *)
  | Inline of string * int (* assembler CFI program, advanced to this offset *)

type fde = {
  code_base : string;
  start : int;
  size : int;
  events : (int * event) list;  (* offsets from start, in order *)
  entry : bool;
  cleanup : (int * int) option;  (* landing pad and resume offsets from start *)
}

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

(* The .eh_frame bytes and their relocations, plus the LSDA if any function
   has a C cleanup. The pad consults frame slots to decide which lexical
   cleanups are active. The call to _Unwind_Resume must have no landing pad
   of its own, or forced unwinding would reenter this pad indefinitely. *)
let build (fdes : fde list) =
  let b = Buffer.create 1024 and relocs = ref [] in
  let lsda = Buffer.create 32 and patches = ref [] in
  let pcrel_to symbol displacement =
    relocs := A.Rel32 { offset = Buffer.length b; symbol; addend = displacement + 4 } :: !relocs;
    u32 b 0
  in
  let cie ~cleanup =
    let at = Buffer.length b in
    u32 b 0 (* length, patched *);
    u32 b 0 (* CIE id *);
    u8 b 1 (* version *);
    Buffer.add_string b (if cleanup then "zPLR\000" else "zR\000");
    uleb b 1 (* code alignment *);
    sleb b (-8) (* data alignment *);
    uleb b ra_reg;
    uleb b (if cleanup then 7 else 1) (* augmentation data length *);
    if cleanup then begin
      u8 b 0x9b (* personality: indirect, pc-relative sdata4 *);
      pcrel_to personality_slot 0;
      u8 b 0x1b (* LSDA pointers: pc-relative sdata4 *)
    end;
    u8 b 0x1b (* FDE pointers: pc-relative sdata4 *);
    def_cfa b 7 8;
    offset b ra_reg 1;
    pad8 b at;
    patches := (at, Buffer.length b - at - 4) :: !patches;
    at
  in
  let plain_cie = cie ~cleanup:false in
  let cleanup_cie =
    if List.exists (fun f -> Option.is_some f.cleanup) fdes then cie ~cleanup:true else plain_cie
  in
  List.iter
    (fun f ->
      let at = Buffer.length b in
      u32 b 0 (* length, patched *);
      let cie = if Option.is_some f.cleanup then cleanup_cie else plain_cie in
      u32 b (Buffer.length b - cie) (* CIE pointer: back from this field to the CIE *);
      pcrel_to f.code_base f.start (* pc_begin *);
      u32 b f.size (* pc_range *);
      (match f.cleanup with
      | None -> uleb b 0
      | Some (landing, resume) ->
          uleb b 4;
          pcrel_to lsda_symbol (Buffer.length lsda);
          u8 lsda 0xff (* omitted LPStart: use the start of the function *);
          u8 lsda 0xff (* omitted type table: no catches *);
          u8 lsda 0x01 (* call-site fields are ULEB128 *);
          let sites = Buffer.create 16 in
          let add_site start length pad =
            uleb sites start; uleb sites length; uleb sites pad; uleb sites 0
          in
          add_site 0 resume landing;
          add_site resume (f.size - resume) 0;
          uleb lsda (Buffer.length sites);
          Buffer.add_buffer lsda sites);
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
          | Saved_xmm (x, off) -> offset b (17 + x) ((off + 16) / 8)
          | Restored_xmm x -> u8 b (0xc0 lor (17 + x))
          | Epilogue -> remember_state b
          | Popped_rbp -> def_cfa b 7 8
          | Returned -> restore_state b
          | Inline (program, finish) -> Buffer.add_string b program; loc := finish)
        f.events;
      pad8 b at;
      patches := (at, Buffer.length b - at - 4) :: !patches)
    fdes;
  (* A partial link can concatenate these input sections. A terminator
     between two inputs would hide the later FDEs from the final linker. *)
  let bytes = Buffer.to_bytes b in
  List.iter (fun (at, len) -> set_u32 bytes at len) !patches;
  ((bytes, List.rev !relocs), Buffer.to_bytes lsda)
