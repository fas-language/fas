(* GNU inline assembly for the i386 backend. The C constraints and template
   grammar are shared with the x86-64 backend. The assembler emits an ELF32
   snippet in the module's code mode, which is spliced into the function. *)
open Fir
module A = Fir_asm
module B = Fas_linker.Binary

exception Unsupported of string
let refuse fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

type location =
  | Reg of int * int
  | Mem of int * int
  | Imm of int64
  | Sym of string * int64
  | Flag of string

let regs = [| "rax"; "rcx"; "rdx"; "rbx"; "rsp"; "rbp"; "rsi"; "rdi" |]
let gprs = [ 0; 1; 2; 3; 6; 7 ]
let short_names = [| "ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di" |]
let reg_name r bits =
  match bits with
  | 8 when r < 4 -> String.make 1 short_names.(r).[0] ^ "l"
  | 16 -> short_names.(r)
  | 32 -> "e" ^ short_names.(r)
  | _ -> refuse "%d-bit i386 asm register operand" bits

let register_size = function
  | Int n when n <= 8 -> 8
  | Int n when n <= 16 -> 16
  | Int n when n <= 32 -> 32
  | Ptr 0 -> 32
  | ty -> refuse "asm register operand of type %s" (ty_to_string ty)

let sym_off name addend =
  if addend = 0L then name else Printf.sprintf "%s%+Ld" name addend

let render ~intel ~locations pieces =
  let b = Buffer.create 80 in
  let pre = if intel then "" else "%" in
  let bare = function Some ('c' | 'p' | 'P' | 'a') -> true | _ -> false in
  let reg r = pre ^ reg_name r 32 in
  let mem r extra =
    if intel then Printf.sprintf "[%s%s]" (reg r) (if extra = 0 then "" else Printf.sprintf "+%d" extra)
    else Printf.sprintf "%s(%s)" (if extra = 0 then "" else string_of_int extra) (reg r)
  in
  let suffix = function
    | 8 -> "b" | 16 -> "w" | 32 -> "l" | 64 -> "q"
    | bits -> refuse "asm operand size %d for %%z" bits
  in
  let operand modifier = function
    | Flag _ -> refuse "asm template references a flag output"
    | Reg (r, _) when modifier = Some 'a' -> mem r 0
    | Reg (r, bits) ->
        let bits = match modifier with Some 'k' -> 32 | Some 'w' -> 16 | Some ('b' | 'h') -> 8 | _ -> bits in
        (match modifier with
         | Some 'z' -> suffix bits
         | Some 'V' -> reg_name r bits
         | Some 'h' when r < 4 -> pre ^ String.make 1 short_names.(r).[0] ^ "h"
         | None | Some ('k' | 'w' | 'b') -> pre ^ reg_name r bits
         | Some c -> refuse "asm register modifier %%%c" c)
    | Mem (r, bits) ->
        let size = if intel then
            (match bits with 8 -> "BYTE PTR " | 16 -> "WORD PTR " | 32 -> "DWORD PTR " | 64 -> "QWORD PTR " | _ -> "")
          else "" in
        (match modifier with
         | Some 'z' -> suffix bits
         | Some 'H' -> mem r 8
         | Some 'a' -> mem r 0
         | None -> size ^ mem r 0
         | Some c -> refuse "asm memory modifier %%%c" c)
    | Imm n ->
        let n = if modifier = Some 'n' then Int64.neg n else n in
        (if intel || bare modifier || modifier = Some 'n' then "" else "$") ^ Int64.to_string n
    | Sym (s, n) ->
        (if bare modifier then "" else if intel then "OFFSET " else "$") ^ sym_off s n
  in
  List.iter (function
    | A.Text s -> Buffer.add_string b s
    | Unique -> Buffer.add_char b '0' (* An assembler invocation has one template. *)
    | Operand (modifier, i) -> Buffer.add_string b (operand modifier locations.(i))) pieces;
  Buffer.contents b

type plan = {
  source : string;
  loads : (int * value) list;
  outputs : location list;
}

let plan f ~tmpl ~aflags ~outs ~ins ~aargs ~clobbers =
  let intel = has aflags asm_intel in
  let spelling (a : asm_operand) = (a.aname, a.acon) in
  let statement =
    try A.parse ~labels:0 ~intel ~tmpl ~outs:(Array.map spelling outs)
        ~ins:(Array.map spelling ins) ~clobbers
    with A.Bad msg -> refuse "%s" msg
  in
  let nouts = Array.length outs in
  let operands = Array.append outs ins and cons = Array.append statement.outs statement.ins in
  let next = ref 0 in
  let args = Array.mapi (fun i con ->
    if i >= nouts || A.output_takes_arg con then begin
      let v = aargs.(!next) in incr next; Some v
    end else None) cons in
  let locations = Array.make (Array.length cons) None in
  let blocked = List.filter (fun r -> List.mem (A.Creg regs.(r)) statement.clob) gprs in
  let owners = Array.make (Array.length regs) [] in
  let allowed i c =
    if A.indirect c then gprs
    else if c.A.regs = [] then []
    else
      let bits = register_size operands.(i).aty in
      List.filter (fun r -> (bits <> 8 || r < 4) && List.mem regs.(r) c.A.regs) gprs
  in
  let location i = Option.get locations.(i) in
  let reserved_output i owner =
    i >= nouts && owner < nouts && cons.(owner).dir = A.Out && not cons.(owner).early in
  let assign_register i choices =
    let r =
      let usable r = not (List.mem r blocked) in
      match List.find_opt (fun r -> usable r && owners.(r) = []) choices with
      | Some r -> r
      | None ->
          (match List.find_opt (fun r ->
            usable r && (match owners.(r) with [o] -> reserved_output i o | _ -> false)) choices with
           | Some r -> r
           | None -> refuse "no i386 register for asm operand %d (%s)" i operands.(i).acon)
    in
    owners.(r) <- i :: owners.(r);
    let t = operands.(i).aty in
    let loc =
      if A.indirect cons.(i) then
        Mem (r, (match t with Ptr _ -> 32 | _ -> 8 * size_of t))
      else Reg (r, register_size t)
    in
    locations.(i) <- Some loc
  in
  Array.iteri (fun i c ->
    match c.A.flag, args.(i) with
    | Some flag, _ -> locations.(i) <- Some (Flag flag)
    | None, Some v when not (A.indirect c) && c.tied = None ->
        (match const_of f v with
         | Some (Cint (_, a)) when Array.length a = 1 &&
             (match c.imm with Some fits -> fits a.(0) | None -> false) ->
               locations.(i) <- Some (Imm a.(0))
         | Some (Csym (s, n)) when c.sym -> locations.(i) <- Some (Sym (s, n))
         | _ -> ())
    | _ -> ()) cons;
  let assign fixed i c =
    if locations.(i) = None && c.A.tied = None then begin
      let choices = allowed i c in
      if (List.length choices = 1) = fixed then assign_register i choices
    end
  in
  (* Pin explicit registers first, then choose the flexible operands. *)
  Array.iteri (assign true) cons;
  Array.iteri (assign false) cons;
  Array.iteri (fun i c -> Option.iter (fun o -> locations.(i) <- Some (location o)) c.A.tied) cons;
  let loads = ref [] in
  Array.iteri (fun i v ->
    match location i, v with
    | (Reg (r, _) | Mem (r, _)), Some value ->
        (match List.assoc_opt r !loads with
         | None -> loads := (r, value) :: !loads
         | Some v when v = value -> ()
         | Some _ -> refuse "asm inputs require different values in %s" regs.(r))
    | _ -> ()) args;
  let outputs = Array.to_list (Array.sub (Array.map Option.get locations) 0 nouts)
    |> List.filter (function Mem _ -> false | _ -> true) in
  let source = render ~intel ~locations:(Array.map Option.get locations) statement.pieces in
  { source = (if intel then ".intel_syntax noprefix\n" else "") ^ source; loads = List.rev !loads; outputs }

type relocation = {
  at : int;
  kind : int;
  target : [ `Section of int | `Symbol of string ];
  addend : int64;
}

type part = { index : int; name : string; flags : int; code : string; alignment : int;
              relocations : relocation list }
type snippet = { text : part; other : part list }

(* GNU as produces a small object. A snippet can add alternative instructions
   or other allocated PROGBITS sections alongside its main instruction bytes. *)
let read data =
  let length = String.length data and context = "i386 inline asm object" in
  let check off size = B.span ~context ~length off size in
  let u32 off = Int64.to_int (B.u32 data off) in
  let u16 = B.u16 data in
  let u8 = B.u8 data in
  check 0 52;
  if not (String.starts_with ~prefix:"\127ELF\001\001\001" data) ||
      u16 16 <> 1 || u16 18 <> 3 || u16 46 <> 40
  then refuse "the inline assembler did not produce an i386 relocatable object";
  let shoff = u32 32 and shnum = u16 48 and shstr = u16 50 in
  check shoff (shnum * 40);
  let section index =
    if index < 0 || index >= shnum then refuse "unknown i386 inline asm section %d" index;
    shoff + (index * 40)
  in
  let table sh = (u32 (sh + 16), u32 (sh + 20)) in
  let str (off, size) index =
    check off size;
    B.string_at ~context data ~offset:off ~size index in
  let names = table (section shstr) in
  let text_index = ref 0 and symbols = ref 0 in
  let parts = Hashtbl.create 4 and patches = Hashtbl.create 4 in
  for i = 1 to shnum - 1 do
    let sh = section i in
    let name = str names (u32 sh) in
    let typ = u32 (sh + 4) and flags = u32 (sh + 8) in
    let size = u32 (sh + 20) in
    match typ with
    | 1 when name = ".text" || (flags land 2 <> 0 && (size <> 0 || name <> ".data")) ->
        if flags land (lnot 7) <> 0 then refuse "inline asm section %s flags" name;
        Hashtbl.replace parts i name;
        if name = ".text" then text_index := i
    | 2 -> if !symbols <> 0 then refuse "multiple inline asm symbol tables" else symbols := i
    | 9 ->
        let info = u32 (sh + 28) in
        if Hashtbl.mem patches info then refuse "multiple relocation tables for an inline asm section";
        Hashtbl.add patches info sh
    | _ when flags land 2 <> 0 && size <> 0 -> refuse "inline asm section %s type %d" name typ
    | _ -> ()
  done;
  if !text_index = 0 then refuse "inline asm has no .text section";
  let relocs index at size =
    match Hashtbl.find_opt patches index with
    | None -> []
    | Some sh ->
        if !symbols = 0 || u32 (sh + 24) <> !symbols then refuse "inline asm relocations lack their symbol table";
        let sym = section !symbols in
        let off, len = table sh and symoff, symsize = table sym in
        check off len;
        check symoff symsize;
        if len mod 8 <> 0 || u32 (sym + 36) <> 16 then refuse "invalid i386 inline asm relocation table";
        let strings = table (section (u32 (sym + 24))) in
        List.init (len / 8) (fun i ->
          let r = off + (i * 8) in
          let info = u32 (r + 4) in
          let typ = info land 255 and offset = u32 r in
          let width =
            match typ with 1 | 2 | 3 | 4 | 9 | 10 | 43 -> 4 | 20 | 21 -> 2 | 22 | 23 -> 1
            | k -> refuse "unsupported inline asm relocation %d" k in
          B.span ~context ~length:size offset width;
          let patch = at + offset in
          let addend =
            match width with
            | 1 -> Int64.of_int (String.get_int8 data patch)
            | 2 -> Int64.of_int (String.get_int16_le data patch)
            | _ -> Int64.of_int32 (String.get_int32_le data patch) in
          let idx = info lsr 8 in
          B.span ~context ~length:symsize (idx * 16) 16;
          let sym = symoff + (idx * 16) in
          let def = u16 (sym + 14) and binding = u8 (sym + 12) lsr 4 in
          let target, addend =
            if binding = 0 && Hashtbl.mem parts def then
              (`Section def, Int64.add addend (Int64.of_int (u32 (sym + 4))))
            else if def = 0 && binding <> 0 then (`Symbol (str strings (u32 sym)), addend)
            else refuse "inline asm relocation against symbol in section %d" def in
          { at = offset; kind = typ; target; addend })
  in
  let part index name =
    let sh = section index in
    let at, size = table sh in
    check at size;
    { index; name; flags = u32 (sh + 8); code = String.sub data at size;
      alignment = max 1 (u32 (sh + 32)); relocations = relocs index at size }
  in
  let text = part !text_index ".text" in
  let other = List.init (shnum - 1) ((+) 1)
    |> List.filter_map (fun i ->
         if i = !text_index then None else Option.map (part i) (Hashtbl.find_opt parts i)) in
  { text; other }

let assemble ~real source =
  let src = Filename.temp_file "fas-i386-asm-" ".s" and obj = Filename.temp_file "fas-i386-asm-" ".o" in
  Fun.protect
    ~finally:(fun () -> List.iter (fun p -> try Sys.remove p with Sys_error _ -> ()) [ src; obj ])
    (fun () ->
      Out_channel.with_open_text src (fun ch ->
        output_string ch (if real then ".text\n.code16gcc\n" else ".text\n.code32\n");
        output_string ch source; output_char ch '\n');
      let words s = List.filter (( <> ) "") (String.split_on_char ' ' s) in
      let assembler =
        match Sys.getenv_opt "FAS_AS" with
        | Some command when String.trim command <> "" -> words command
        | _ -> [ "as"; "--32" ] in
      let argv = assembler @ [ "-o"; obj; src ] |> Array.of_list in
      match Process.run argv with
      | Ok _ ->
          let data = In_channel.with_open_bin obj In_channel.input_all in
          (try read data with Fas_linker.Error.Error message -> refuse "%s" message)
      | Error f -> refuse "%s" (String.trim (f.stderr ^ f.stdout)))
