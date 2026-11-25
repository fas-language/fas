(* Mir_emit: frame layout, x86-64 encoding and the ELF object for the Mir
   path (roadmap v0.4). Entry points [compile_object] and [link_files] take
   any verified Fir module: [generate] scalarizes vectors, adds the runtime
   helpers and legalizes wide integers itself (each step is a no-op on a
   module that already went through it, as in fas-fir).

   Frame of a function (rbp-based, rsp 16-aligned at calls):

     rbp + 16 + k   incoming stack arguments ([Bin])
     rbp + 8        return address
     rbp            saved rbp
     rbp - 8n       the n callee-saved registers the allocator used
     below          frame slots (allocas, scratch, one per vreg), each
                    aligned to its alignment (at most 16)
     rsp            outgoing stack arguments ([Bout])

   Every function is checked by Mir_check before it is encoded (by
   Mir_icheck, when FAS_RA_IMPORT supplies its allocation). *)

open Mir
module A = X64
module F = Fir

exception Unsupported = Mir_isel.Unsupported

let unsupported = Mir_isel.unsupported
let dump = Sys.getenv_opt "FAS_MIR_DUMP" = Some "1"

let gr = function G r -> r | r -> invalid_arg ("Mir_emit: not a GPR: " ^ reg_s r)
let xr = function X i -> i | r -> invalid_arg ("Mir_emit: not an xmm register: " ^ reg_s r)

(* the outgoing-argument area at the bottom of the frame, kept a multiple
   of 16 so a dynamic allocation below it stays 16-aligned *)
let out_area (f : func) = Mir_isel.align_up f.out_size 16

type frame = { slot_off : int array; nsaved : int; size : int; ms_xmm_off : int option }

let layout (f : func) nsaved =
  let off = ref (8 * nsaved) in
  let ms_xmm_off =
    if f.ms_save_xmm then begin
      off := Mir_isel.align_up !off 16 + 160;
      Some !off
    end else None
  in
  let slot_off =
    Array.init (Dynarray.length f.slots) (fun i ->
        let size, align = Dynarray.get f.slots i in
        let align = max 1 align in
        if align > 16 then unsupported "frame slot aligned to %d" align;
        off := Mir_isel.align_up (!off + size) align;
        !off)
  in
  let total = Mir_isel.align_up (!off + out_area f) 16 in
  (* frame offsets are 32-bit displacements off rbp *)
  if total > 0x7000_0000 then unsupported "@%s: a stack frame of %d bytes (at most 1.75 GiB)" f.name total;
  { slot_off; nsaved; size = total - (8 * nsaved); ms_xmm_off }

let mem fr (m : addr) =
  let sib base disp idx =
    A.Sib { base = Some base; index = gr idx; scale = m.scale; disp; sym = None }
  in
  let based base disp = match m.index with Some i -> sib base disp i | None -> A.Base (base, disp) in
  match m.base with
  | Breg r -> based (gr r) m.disp
  | Bslot s -> based A.Rbp (m.disp - fr.slot_off.(s))
  | Bout -> based A.Rsp m.disp
  | Bin -> based A.Rbp (16 + m.disp)
  | Bsym s ->
      if m.index <> None then invalid_arg "Mir_emit: rip-relative with an index";
      A.Rip (s, m.disp)
  | Babs s ->
      if m.index <> None then invalid_arg "Mir_emit: absolute symbol with an index";
      A.Abs (s, m.disp)
  | Bnone -> (
      match m.index with
      | Some i -> A.Sib { base = None; index = gr i; scale = m.scale; disp = m.disp; sym = None }
      | None -> invalid_arg "Mir_emit: absolute address")

(* mov r32, imm32 even for 0: unlike xor it leaves the flags alone *)
let movi a r (k : int64) =
  if k = 0L then begin
    A.rex a ~w:false ~r:0 ~b:(A.reg_num r) ();
    A.byte a (0xb8 + (A.reg_num r land 7));
    A.int32 a 0
  end
  else A.mov_imm a r k

let fstore a dbl m x = A.sse_rm a (A.fprefix dbl) 0x11 ~reg:x m

(* ---------- inline asm ---------- *)

(* GNU inline asm (Mir.Asm) goes through an external assembler: every
   statement's template is rendered with its operands in place (registers
   are physical by now, frame slots have offsets), all statements of the
   module are assembled in one run, one section .text.fas_asm_N each, and
   [ins] splices statement N's bytes and relocations into the text.

   The assembler is $FAS_AS (a command line), else `as --64`, else
   `$FAS_CC -c -x assembler` (cc by default). Section switches can add
   records or code elsewhere in the object. A snippet may not define a
   global symbol or refer directly to a place in another snippet. Its
   alignment contributes to that of .text. An inline
   .cfi_* rule becomes part of the enclosing function's FDE. *)

(* a snippet: its bytes, its alignment and its relocations, at offsets in
   the bytes, against an outside symbol or a place in the snippet *)
type snippet = {
  code : string;
  salign : int;
  srelocs : (int * [ `Sym of string | `Here of int ] * int * int64) list;
  sframe : string option;
  mutable sstart : int;
  mutable sbase : string;
}

let gpr_name width r =
  let n = A.reg_name r in
  if A.reg_num r >= 8 then match width with 8 -> n ^ "b" | 16 -> n ^ "w" | 32 -> n ^ "d" | _ -> n
  else
    match width with
    | 32 -> "e" ^ String.sub n 1 2
    | 16 -> String.sub n 1 2
    | 8 -> (
        match r with
        | A.Rax -> "al"
        | Rcx -> "cl"
        | Rdx -> "dl"
        | Rbx -> "bl"
        | Rsp -> "spl"
        | Rbp -> "bpl"
        | Rsi -> "sil"
        | Rdi -> "dil"
        | _ -> n)
    | _ -> n

let high_byte = function
  | A.Rax -> "ah"
  | Rcx -> "ch"
  | Rdx -> "dh"
  | Rbx -> "bh"
  | r -> unsupported "inline asm: %%h of %s, which has no high byte" (A.reg_name r)

let size_suffix bits =
  match bits with
  | 8 -> "b"
  | 16 -> "w"
  | 32 -> "l"
  | 64 -> "q"
  | n -> unsupported "inline asm: %%z of a %d-bit operand" n

let intel_size bytes =
  match bytes with
  | 1 -> "BYTE PTR "
  | 2 -> "WORD PTR "
  | 4 -> "DWORD PTR "
  | 8 -> "QWORD PTR "
  | 10 -> "TBYTE PTR "
  | 16 -> "XMMWORD PTR "
  | 32 -> "YMMWORD PTR "
  | _ -> ""

let sym_off s k = if k = 0L then s else if k > 0L then Printf.sprintf "%s+%Ld" s k else Printf.sprintf "%s%Ld" s k

(* the text of one statement, with %= as [uid] *)
let render_asm fr uid (x : asm) =
  let intel = x.intel in
  let pre = if intel then "" else "%" in
  let memtext (m : A.mem) extra ~size =
    if intel then begin
      let regs, disp, sym =
        match m with
        | A.Base (r, d) -> ([ A.reg_name r ], d, None)
        | Rip (s, d) -> ([ "rip" ], d, Some s)
        | Abs (s, d) -> ([], d, Some s)
        | Sib { base; index; scale; disp; sym } ->
            (Option.to_list (Option.map A.reg_name base) @ [ Printf.sprintf "%s*%d" (A.reg_name index) scale ], disp, sym)
      in
      let disp = disp + extra in
      let terms = regs @ Option.to_list sym in
      let d = if disp = 0 then "" else if disp > 0 then Printf.sprintf "+%d" disp else string_of_int disp in
      (match size with Some n -> intel_size n | None -> "") ^ "[" ^ String.concat "+" terms ^ d ^ "]"
    end
    else
      match m with
      | A.Base (r, d) -> Printf.sprintf "%d(%%%s)" (d + extra) (A.reg_name r)
      | Rip (s, d) -> Printf.sprintf "%s(%%rip)" (sym_off s (Int64.of_int (d + extra)))
      | Abs (s, d) -> sym_off s (Int64.of_int (d + extra))
      | Sib { base; index; scale; disp; sym } ->
          let d =
            match sym with
            | Some s -> sym_off s (Int64.of_int (disp + extra))
            | None -> string_of_int (disp + extra)
          in
          Printf.sprintf "%s(%s,%%%s,%d)" d
            (match base with Some b -> "%" ^ A.reg_name b | None -> "")
            (A.reg_name index) scale
  in
  let opnd md k =
    (match md with
    | Some c when not (String.contains "bhwkqcnpPzVxtgHal" c) -> unsupported "inline asm: the operand modifier %%%c" c
    | _ -> ());
    match (x.opnds.(k), md) with
    | Anone, _ -> unsupported "inline asm: the template names operand %d, a flag output" k
    | Areg (G r, _), Some 'a' -> if intel then "[" ^ A.reg_name r ^ "]" else "(%" ^ A.reg_name r ^ ")"
    | Areg (G r, w), _ ->
        let w = match md with Some 'b' -> 8 | Some 'w' -> 16 | Some 'k' -> 32 | Some 'q' -> 64 | _ -> w in
        if md = Some 'z' then size_suffix w
        else
          let name = if md = Some 'h' then high_byte r else gpr_name w r in
          if md = Some 'V' then name else pre ^ name
    | Areg (X i, _), _ ->
        let kind = match md with Some 't' -> "ymm" | Some 'g' -> "zmm" | _ -> "xmm" in
        if md = Some 'z' then unsupported "inline asm: %%z of an xmm register";
        (if md = Some 'V' then "" else pre) ^ kind ^ string_of_int i
    | Areg (V _, _), _ -> invalid_arg "Mir_emit: virtual register in inline asm"
    | Ax87 i, _ -> (if intel then "" else "%") ^ if i = 0 then "st" else Printf.sprintf "st(%d)" i
    | Amem (ad, size), _ ->
        if md = Some 'z' then size_suffix (8 * size)
        else
          memtext (mem fr ad)
            (if md = Some 'H' then 8 else 0)
            ~size:(if md = Some 'a' || md = Some 'H' then None else Some size)
    | Aimm k, Some 'n' -> Int64.to_string (Int64.neg k)
    | Aimm k, Some ('c' | 'p' | 'P' | 'a') -> Int64.to_string k
    | Aimm k, _ -> (if intel then "" else "$") ^ Int64.to_string k
    | Asym (s, k), Some ('c' | 'p' | 'P' | 'a' | 'l') -> sym_off s k
    | Asym (s, k), _ -> (if intel then "OFFSET " else "$") ^ sym_off s k
  in
  let b = Buffer.create 64 in
  List.iter
    (function
      | Fir_asm.Text s -> Buffer.add_string b s
      | Operand (md, k) -> Buffer.add_string b (opnd md k)
      | Unique -> Buffer.add_string b (string_of_int uid))
    x.pieces;
  Buffer.contents b

(* runs the assembler on [source]: the object's bytes *)
let run_assembler source =
  let src = Filename.temp_file "fas-asm-" ".s" and obj = Filename.temp_file "fas-asm-" ".o" in
  Fun.protect
    ~finally:(fun () -> List.iter (fun p -> try Sys.remove p with Sys_error _ -> ()) [ src; obj ])
    (fun () ->
      Out_channel.with_open_bin src (fun oc -> output_string oc source);
      let words s = List.filter (( <> ) "") (String.split_on_char ' ' s) in
      let cc () =
        let cc = match Sys.getenv_opt "FAS_CC" with Some c when String.trim c <> "" -> words c | _ -> [ "cc" ] in
        cc @ [ "-c"; "-x"; "assembler"; src; "-o"; obj ]
      in
      let attempts =
        match Sys.getenv_opt "FAS_AS" with
        | Some s when String.trim s <> "" -> [ words s @ [ "-o"; obj; src ] ]
        | _ -> [ [ "as"; "--64"; "-o"; obj; src ]; cc () ]
      in
      let rec go = function
        | [] -> unsupported "inline asm: no assembler found (set FAS_AS)"
        | argv :: rest -> (
            match Process.run (Array.of_list argv) with
            | Ok _ -> In_channel.with_open_bin obj In_channel.input_all
            | Error { status = Unix.WEXITED 127; _ } when rest <> [] -> go rest
            | Error f ->
                unsupported "inline asm: %s failed:\n%s" (String.concat " " argv) (String.trim (f.stderr ^ f.stdout)))
      in
      go attempts)

(* Assemble each sequence of inline CFI rules in a dummy FDE with an
   empty initial state. The final DW_CFA_nop makes the assembler advance
   to the end of the snippet. Splicing the program into the real FDE
   preserves these relative PC advances and uses the host frame's state. *)
let has_inline_cfi text =
  let rec find i =
    match String.index_from_opt text i '.' with
    | None -> false
    | Some j -> (j + 5 <= String.length text && String.sub text j 5 = ".cfi_") || find (j + 1)
  in
  find 0

let assemble_snippets (texts : (bool * string) list) =
  let n = List.length texts in
  let src = Buffer.create 1024 in
  let cfi = Array.make n false in
  List.iteri
    (fun i (intel, text) ->
      Printf.bprintf src ".section .text.fas_asm_%d,\"ax\",@progbits\n" i;
      if intel then Buffer.add_string src ".intel_syntax noprefix\n";
      cfi.(i) <- has_inline_cfi text;
      if cfi.(i) then Buffer.add_string src ".cfi_startproc simple\n";
      Buffer.add_string src text;
      Buffer.add_string src "\n";
      if cfi.(i) then Buffer.add_string src ".cfi_escape 0x00\n.cfi_endproc\n";
      if intel then Buffer.add_string src ".att_syntax prefix\n")
    texts;
  let data = run_assembler (Buffer.contents src) in
  let module R = Fas_linker.Elf_reader in
  let o = R.parse ~path:"<inline asm>" data in
  let sec = Array.make n (-1) in
  let snippet_of = Hashtbl.create n in
  let frame_section = ref None in
  let extra = Hashtbl.create 8 in
  let referenced = Hashtbl.create 8 in
  (* Alternative instruction records can point to a zero-byte replacement. *)
  Array.iter
    (fun (r : R.relocation) ->
      if r.kind <> 0 then Hashtbl.replace referenced o.symbols.(r.symbol).section ())
    o.relocations;
  if Array.length o.groups > 0 then unsupported "inline asm: section groups (COMDAT)";
  Array.iter
    (fun (s : R.section) ->
      let p = ".text.fas_asm_" in
      if String.starts_with ~prefix:p s.name then begin
        let i = int_of_string (String.sub s.name (String.length p) (String.length s.name - String.length p)) in
        sec.(i) <- s.index;
        Hashtbl.replace snippet_of s.index i
      end
      else if s.name = ".eh_frame" && Array.exists Fun.id cfi then frame_section := Some s
      else if (s.size > 0 || Hashtbl.mem referenced s.index) && (s.kind = 1 || s.kind = 8) then begin
        if s.flags land (lnot (1 lor 2 lor 4 lor 0x400)) <> 0 then
          unsupported "inline asm: section %s has unsupported flags %#x" s.name s.flags;
        Hashtbl.add extra s.index (Printf.sprintf ".Lfas.inline.section.%d" s.index)
      end else if s.size > 0 && s.flags land 2 <> 0 then
        unsupported "inline asm: section %s has unsupported type %#x" s.name s.kind)
    o.sections;
  Array.iter
    (fun (y : R.symbol) ->
      if y.binding <> 0 && (Hashtbl.mem snippet_of y.section || Hashtbl.mem extra y.section) then
        unsupported "inline asm defines the global symbol %s" y.name)
    o.symbols;
  let frames = Array.make n None in
  Option.iter
    (fun (s : R.section) ->
      let u32 off = Int32.to_int (String.get_int32_le o.data (s.offset + off)) in
      let byte off = o.data.[s.offset + off] in
      let bad () = unsupported "inline asm: unsupported frame entry or CIE rules" in
      Array.iter
        (fun (r : R.relocation) ->
          if r.target = s.index then begin
            let y = o.symbols.(r.symbol) in
            let i = match Hashtbl.find_opt snippet_of y.section with
              | Some i when cfi.(i) -> i
              | _ -> bad ()
            in
            let pc = r.offset in
            if r.kind <> 2 || Int64.add y.value r.addend <> 0L ||
               pc < 8 || pc > s.size - 9 || frames.(i) <> None then bad ();
            let fde = pc - 8 in
            let last = fde + 4 + u32 fde in
            let cie = pc - 4 - u32 (pc - 4) in
            let first = pc + 9 in
            let initial = "\001zR\000\001\120\016\001\027" in
            if cie < 0 || cie > fde - 8 - String.length initial ||
               last < first || last > s.size || byte (pc + 8) <> '\000' ||
               u32 (pc + 4) <> o.sections.(y.section).size then bad ();
            let rules = cie + 8 in
            let rules_end = cie + 4 + u32 cie in
            if u32 (cie + 4) <> 0 || rules_end < rules + String.length initial ||
               rules_end > fde ||
               String.sub o.data (s.offset + rules) (String.length initial) <> initial then bad ();
            for pos = rules + String.length initial to rules_end - 1 do
              if byte pos <> '\000' then bad ()
            done;
            frames.(i) <- Some (String.sub o.data (s.offset + first) (last - first))
          end)
        o.relocations)
    !frame_section;
  let relocs = Array.make n [] in
  let extra_relocs = Hashtbl.create 8 in
  let target (y : R.symbol) addend =
    if y.section = 0 then (y.name, addend)
    else match Hashtbl.find_opt extra y.section with
      | Some name -> (name, Int64.add addend y.value)
      | None ->
          unsupported "inline asm refers to %s, outside its own statement"
            (if y.name = "" then "a section" else y.name)
  in
  Array.iter
    (fun (r : R.relocation) ->
      if r.kind <> 0 then
        match Hashtbl.find_opt snippet_of r.target with
        | Some i ->
            let y = o.symbols.(r.symbol) in
            let sym, addend =
              if y.section = sec.(i) then (`Here (Int64.to_int y.value), r.addend)
              else let name, addend = target y r.addend in (`Sym name, addend)
            in
            relocs.(i) <- (r.offset, sym, r.kind, addend) :: relocs.(i)
        | None when Hashtbl.mem extra r.target ->
            let prior = Option.value (Hashtbl.find_opt extra_relocs r.target) ~default:[] in
            Hashtbl.replace extra_relocs r.target (r :: prior)
        | None -> ())
    o.relocations;
  let snippets = Array.init n (fun i ->
      if cfi.(i) && frames.(i) = None then unsupported "inline asm: missing frame entry";
      if sec.(i) < 0 then { code = ""; salign = 1; srelocs = []; sframe = frames.(i); sstart = 0; sbase = Mir_cfi.text_symbol }
      else
        let s = o.sections.(sec.(i)) in
        { code = String.sub o.data s.offset s.size; salign = max 1 s.alignment;
          srelocs = List.rev relocs.(i); sframe = frames.(i); sstart = 0; sbase = Mir_cfi.text_symbol })
  in
  (* Section relocations to GNU assembler local labels use the snippet's
     section and an addend. Once text is laid out, replace that base with
     .text and include the snippet's aligned offset. *)
  let materialize ~first ~section_of_base =
    let sections = ref [] and symbols = ref [] and index = ref first in
    Array.iter
      (fun (s : R.section) ->
        match Hashtbl.find_opt extra s.index with
        | None -> ()
        | Some name ->
            let relocs =
              List.rev_map
                (fun (r : R.relocation) ->
                  let y = o.symbols.(r.symbol) in
                  let symbol, addend =
                    match Hashtbl.find_opt snippet_of y.section with
                    | Some i ->
                        let offset = Int64.add (Int64.of_int snippets.(i).sstart) y.value in
                        (snippets.(i).sbase, Int64.add offset r.addend)
                    | None -> target y r.addend
                  in
                  A.Raw { offset = r.offset; symbol; kind = r.kind; addend = Int64.to_int addend })
                (Option.value (Hashtbl.find_opt extra_relocs s.index) ~default:[])
            in
            let contents =
              if s.kind = 8 then Fas_linker.Elf_object.Bss s.size
              else Fas_linker.Elf_object.Data (Bytes.of_string (String.sub o.data s.offset s.size))
            in
            sections :=
              { Native_object.name = s.name; alignment = max 1 s.alignment; flags = s.flags; contents; relocs }
              :: !sections;
            symbols := { Elf64.name; section = Elf64.Extra !index; offset = 0; size = 0; func = false }
                       :: !symbols;
            incr index)
      o.sections;
    (* Ordinary named labels in asm can also be referenced by C declarations
       in the same translation unit. Emit their final offsets in the containing
       code section, even when GNU as kept their binding local. *)
    let labels = Array.to_list o.symbols |> List.filter_map (fun (y : R.symbol) ->
      match Hashtbl.find_opt snippet_of y.section with
      | Some i when y.name <> "" && y.binding = 0 && (y.kind = 0 || y.kind = 2) ->
          let sn = snippets.(i) in
          Some { Elf64.name = y.name; section = section_of_base sn.sbase;
                 offset = sn.sstart + Int64.to_int y.value; size = Int64.to_int y.size;
                 func = y.kind = 2 }
      | _ -> None)
    in
    (List.rev !sections, List.rev !symbols, labels)
  in
  (snippets, materialize)

(* Top-level asm ([module_asm]): the text is assembled in one run, and every
   allocated section of the result becomes a section of its own in our
   object (after the [first] extra ones), with its symbols and relocations.
   A reference to a section (GNU as turns references to local labels into
   section plus offset) goes through a local symbol at the section's start.
   Returns the sections, the symbols they define, and ELF attributes from
   the asm, including directives on undefined symbols. *)
type masm = {
  msections : Native_object.extra list;
  msyms : Elf64.symbol list;
  mbinding : (string, int) Hashtbl.t;
  mvisibility : (string, int) Hashtbl.t;
}

let module_asm ~first (texts : string list) =
  let mbinding = Hashtbl.create 8 and mvisibility = Hashtbl.create 8 in
  if texts = [] then { msections = []; msyms = []; mbinding; mvisibility }
  else begin
    let data = run_assembler (String.concat "\n" texts ^ "\n") in
    let module R = Fas_linker.Elf_reader in
    let o = R.parse ~path:"<module asm>" data in
    if Array.length o.groups > 0 then unsupported "module asm: section groups (COMDAT)";
    (* the sections kept: allocated ones with contents (PROGBITS, NOBITS,
       NOTE, INIT/FINI/PREINIT_ARRAY, x86-64 UNWIND) *)
    let kept = Hashtbl.create 8 and order = ref [] in
    Array.iter
      (fun (s : R.section) ->
        let kinds = [ 1; 7; 8; 14; 15; 16; 0x70000001 ] in
        if s.flags land 2 <> 0 && List.mem s.kind kinds then begin
          Hashtbl.replace kept s.index (first + List.length !order);
          order := s :: !order
        end)
      o.sections;
    let order = List.rev !order in
    let section_symbol idx = Printf.sprintf ".Lfas.masm.%d" idx in
    let relocs = Hashtbl.create 8 in
    Array.iter
      (fun (r : R.relocation) ->
        if Hashtbl.mem kept r.target && r.kind <> 0 then begin
          let y = o.symbols.(r.symbol) in
          let name =
            if y.section = 0 then y.name
            else
              match Hashtbl.find_opt kept y.section with
              | Some _ when y.kind = 3 -> section_symbol y.section
              | Some _ -> y.name
              | None ->
                  unsupported "module asm refers to %s, in a section it cannot keep"
                    (if y.name = "" then "a section" else y.name)
          in
          let l = Option.value (Hashtbl.find_opt relocs r.target) ~default:[] in
          Hashtbl.replace relocs r.target
            (X64.Raw { offset = r.offset; symbol = name; kind = r.kind; addend = Int64.to_int r.addend } :: l)
        end)
      o.relocations;
    let msections =
      List.map
        (fun (s : R.section) ->
          {
            Native_object.name = s.name;
            (* write, alloc, exec and TLS; merge and strings need an entry
               size, which these sections do not carry *)
            flags = s.flags land (1 lor 2 lor 4 lor 0x400);
            alignment = max 1 s.alignment;
            contents =
              (if s.kind = 8 then Fas_linker.Elf_object.Bss s.size
               else Fas_linker.Elf_object.Data (Bytes.of_string (String.sub o.data s.offset s.size)));
            relocs = List.rev (Option.value (Hashtbl.find_opt relocs s.index) ~default:[]);
          })
        order
    in
    let msyms = ref [] in
    List.iter
      (fun (s : R.section) ->
        let i = Hashtbl.find kept s.index in
        msyms := { Elf64.name = section_symbol s.index; section = Elf64.Extra i; offset = 0; size = 0; func = false }
                 :: !msyms;
        Hashtbl.replace mbinding (section_symbol s.index) 0)
      order;
    Array.iter
      (fun (y : R.symbol) ->
        match Hashtbl.find_opt kept y.section with
        | Some i when y.kind <> 3 && y.kind <> 4 && y.name <> "" ->
            msyms :=
              { Elf64.name = y.name; section = Elf64.Extra i; offset = Int64.to_int y.value; size = Int64.to_int y.size;
                func = y.kind = 2 }
              :: !msyms;
            Hashtbl.replace mbinding y.name y.binding;
            Hashtbl.replace mvisibility y.name y.visibility
        | None when y.section = 0 && y.name <> "" ->
            (* An ordinary asm reference uses GLOBAL and DEFAULT. Preserve
               any explicit Fir attributes in that case. *)
            if y.binding = 2 then Hashtbl.replace mbinding y.name y.binding;
            if y.visibility <> 0 then Hashtbl.replace mvisibility y.name y.visibility
        | None when y.section >= 0xff00 && y.name <> "" ->
            unsupported "module asm: the absolute or common symbol %s" y.name
        | _ -> ())
      o.symbols;
    { msections; msyms = List.rev !msyms; mbinding; mvisibility }
  end

(* the snippets of every inline asm statement of [compiled], in the order
   [func] meets them *)
let asm_snippets iter =
  let texts = ref [] and uid = ref 0 in
  iter
    (fun (_, mf, (ra : Mir_ra.result)) ->
      let fr = layout mf (List.length ra.saved) in
      Array.iter
        (List.iter (function
          | Mir_ra.Orig (_, Asm x) ->
              incr uid;
              texts := (x.intel, render_asm fr !uid x) :: !texts
          | _ -> ()))
        ra.items);
  let snippets, materialize =
    match List.rev !texts with
    | [] -> ([||], fun ~first:_ ~section_of_base:_ -> ([], [], []))
    | l -> assemble_snippets l
  in
  let align = Array.fold_left (fun n s -> max n s.salign) 16 snippets in
  let next = ref 0 in
  let take () =
    let s = snippets.(!next) in
    incr next;
    s
  in
  (take, align, materialize)

(* Stack probes (the -fstack-clash-protection rule): rsp never drops more
   than a page below the lowest address already touched, so growing into
   the guard page always faults there instead of jumping over it into
   whatever is mapped below. *)
let page = 4096

(* or qword [rsp], 0: touches the page rsp points into, changes nothing *)
let probe a = A.alu_mi a A.Or 8 (A.Base (A.Rsp, 0)) 0

(* the prologue's rsp drop, after the pushes (which touched [rsp]). A frame
   under half a page is left unprobed: whatever the function touches next
   (a call's return address, a slot) is still within a page of the last
   push. Larger frames probe every page and the final rsp. r11 is free at
   entry (r10 carries the static chain). *)
let probed_frame a size =
  if size < page / 2 then (if size > 0 then A.alu_ri a A.Sub A.Rsp size)
  else begin
    let pages = size / page and rem = size mod page in
    if pages <= 4 then
      for _ = 1 to pages do
        A.alu_ri a A.Sub A.Rsp page;
        probe a
      done
    else begin
      (* lea r11, [rsp - pages*4096]; L: sub rsp,4096; or [rsp],0; cmp rsp,r11; jne L *)
      A.lea a A.R11 (A.Base (A.Rsp, - (pages * page)));
      let l = A.new_label a in
      A.place a l;
      A.alu_ri a A.Sub A.Rsp page;
      probe a;
      A.alu_rr a A.Cmp A.Rsp A.R11;
      A.jcc a A.Ne l
    end;
    if rem > 0 then begin
      A.alu_ri a A.Sub A.Rsp rem;
      probe a
    end
  end

let ins a fr ~pic ~func_label ~out ~snippet ~asm_frame ~code_base (i : ins) =
  let m = mem fr in
  match i with
  | Salloc (d, n) ->
      (* rsp drops by n (a multiple of 16); the block starts above the
         outgoing-argument area, which moves down with rsp *)
      (* probed: touch [rsp], compute the target, then step down a page
         at a time touching each one, and finally the target itself.
           or [rsp],0; d = rsp - n
         L: sub rsp,4096; cmp rsp,d; jb Last; or [rsp],0; jmp L
         Last: mov rsp,d; or [rsp],0
         n is only read, unless it is d itself (then neg; add rsp) *)
      probe a;
      if d = n then begin
        A.neg a (gr d);
        A.alu_rr a A.Add (gr d) A.Rsp
      end
      else begin
        A.mov_rr a (gr d) A.Rsp;
        A.alu_rr a A.Sub (gr d) (gr n)
      end;
      let step = A.new_label a and last = A.new_label a in
      A.place a step;
      A.alu_ri a A.Sub A.Rsp page;
      A.alu_rr a A.Cmp A.Rsp (gr d);
      A.jcc a A.B last;
      probe a;
      A.jmp a step;
      A.place a last;
      A.mov_rr a A.Rsp (gr d);
      probe a;
      A.lea a (gr d) (A.Base (A.Rsp, out))
  | Spget d -> A.mov_rr a (gr d) A.Rsp
  | Spset r -> A.mov_rr a A.Rsp (gr r)
  | Copy ((G _ as d), (G _ as s)) -> A.mov_rr a (gr d) (gr s)
  | Copy ((X _ as d), (X _ as s)) -> A.mov_xx a (xr d) (xr s)
  | Copy _ -> invalid_arg "Mir_emit: copy between classes"
  | Movi (d, k) -> movi a (gr d) k
  | Load (n, sx, d, ad) -> if sx then A.load_sx a n (gr d) (m ad) else A.load_zx a n (gr d) (m ad)
  | Store (n, ad, s) -> A.store a n (m ad) (gr s)
  | Fload (dbl, d, ad) -> A.mov_xm a ~double:dbl (xr d) (m ad)
  | Fstore (dbl, ad, s) -> fstore a dbl (m ad) (xr s)
  | Xload (d, ad) -> A.movdqu_load a (xr d) (m ad)
  | Xstore (ad, s) -> A.movdqu_store a (m ad) (xr s)
  | Lea (d, ad) -> A.lea a (gr d) (m ad)
  | Got (d, s) -> A.load_got a (gr d) s
  | Tp d -> A.load_tp a (gr d)
  | Tlsoff (d, s) -> A.tls_offset a ~pic (gr d) s
  | Alu (op, d, s) -> A.alu_rr a op (gr d) (gr s)
  | Alui (op, d, k) -> A.alu_ri a op (gr d) k
  | Test (x, y) -> A.test_rr a (gr x) (gr y)
  | Imul (d, s) -> A.imul_rr a (gr d) (gr s)
  | Shift (op, d) -> A.shift_cl a op (gr d)
  | Shifti (op, d, k) -> A.shift_imm a op (gr d) k
  | Shd (left, d, s) -> A.rr a [ 0x0f; (if left then 0xa5 else 0xad) ] ~reg:(gr s) ~rm:(gr d)
  | Neg d -> A.neg a (gr d)
  | Not d -> A.not_ a (gr d)
  | Bswap d ->
      let r = gr d in
      A.rex a ~w:true ~r:0 ~b:(A.reg_num r) ();
      A.byte a 0x0f;
      A.byte a (0xc8 + (A.reg_num r land 7))
  | Ext (bits, false, d, s) -> A.zero_extend_rr a bits (gr d) (gr s)
  | Ext (bits, true, d, s) -> (
      let d = gr d and s = gr s in
      match bits with
      | 8 -> A.rr a [ 0x0f; 0xbe ] ~reg:d ~rm:s
      | 16 -> A.rr a [ 0x0f; 0xbf ] ~reg:d ~rm:s
      | 32 -> A.rr a [ 0x63 ] ~reg:d ~rm:s
      | _ -> invalid_arg "Mir_emit: sign extension width")
  | Mul1 (signed, s) -> if signed then A.imul1 a (gr s) else A.mul1 a (gr s)
  | Div (signed, s) -> if signed then A.idiv a (gr s) else A.div a (gr s)
  | Cqo -> A.cqo a
  | Setcc (c, d) -> A.setcc a c (gr d)
  | Cmov (c, d, s) -> A.cmov a c (gr d) (gr s)
  | Bsf (d, s) -> A.rr a [ 0x0f; 0xbc ] ~reg:(gr d) ~rm:(gr s)
  | Bsr (d, s) -> A.rr a [ 0x0f; 0xbd ] ~reg:(gr d) ~rm:(gr s)
  | Fop (op, dbl, d, s) -> A.fop_xx a ~double:dbl op (xr d) (xr s)
  | Fsqrt (dbl, d, s) -> A.fop_xx a ~double:dbl A.Fsqrt (xr d) (xr s)
  | Ucomi (dbl, x, y) -> A.ucomi a ~double:dbl (xr x) (xr y)
  | Comi (dbl, x, y) -> A.ucomi ~signaling:true a ~double:dbl (xr x) (xr y)
  | Movgx (dbl, d, s) -> A.mov_xr a ~double:dbl (xr d) (gr s)
  | Movxg (dbl, d, s) -> A.mov_rx a ~double:dbl (gr d) (xr s)
  | Cvtsi (dbl, d, s) -> A.cvt_si2f a ~double:dbl (xr d) (gr s)
  | Cvttsi (dbl, d, s) -> A.cvt_f2si a ~double:dbl (gr d) (xr s)
  | Cvtff (from_double, d, s) -> A.cvt_ff a ~from_double (xr d) (xr s)
  | Frem87 (dbl, s) ->
      let x = m (at (Bslot s)) and y = m { (at (Bslot s)) with disp = 8 } in
      A.fld a ~double:dbl y;
      A.fld a ~double:dbl x;
      let loop = A.new_label a in
      A.place a loop;
      A.fprem a;
      A.fnstsw_ax a;
      A.test_ri a A.Rax 0x400;
      A.jcc a A.Ne loop;
      A.fstp a ~double:dbl x;
      A.fpop a
  | X87 x -> (
      match x with
      | Fld (M32, ad) -> A.fld a ~double:false (m ad)
      | Fld (M64, ad) -> A.fld a ~double:true (m ad)
      | Fld (M80, ad) -> A.fld80 a (m ad)
      | Fld (I64, ad) -> A.fild64 a (m ad)
      | Fstp (M32, ad) -> A.fstp a ~double:false (m ad)
      | Fstp (M64, ad) -> A.fstp a ~double:true (m ad)
      | Fstp (M80, ad) -> A.fstp80 a (m ad)
      | Fstp (I64, ad) -> A.fistp64 a (m ad)
      | Farith op -> A.farithp a op
      | Fremloop ->
          let loop = A.new_label a in
          A.place a loop;
          A.fprem a;
          A.fnstsw_ax a;
          A.test_ri a A.Rax 0x400;
          A.jcc a A.Ne loop
      | Fsqrt87 -> A.fsqrt87 a
      | Fucomip -> A.fucomip a
      | Fcomip -> A.fucomip ~signaling:true a
      | Fpop -> A.fpop a
      | Fnstcw ad -> A.fnstcw a (m ad)
      | Fldcw ad -> A.fldcw a (m ad))
  | Call { target; _ } -> (
      match target with
      | Tlabel name ->
          (match func_label name with Some l -> A.call_label a l | None -> A.call_symbol a name)
      | Tsym name -> A.call_symbol a name
      | Treg r -> A.call_r a (gr r)
      | Ttlsgd name -> A.tls_get_addr a name)
  | Syscall _ -> A.syscall a
  | Rep_movsb -> A.rep_movsb a
  | Rep_stosb -> A.rep_stosb a
  | Std -> A.std a
  | Cld -> A.cld a
  | Xchg (n, ad, r) -> A.xchg_mr a n (m ad) (gr r)
  | Xadd (n, ad, r) -> A.lock_xadd a n (m ad) (gr r)
  | Cmpxchg (n, ad, r) -> A.lock_cmpxchg a n (m ad) (gr r)
  | Cmpxchg16b ad -> A.lock_cmpxchg16b a (m ad)
  | Mfence -> A.mfence a
  | Align_sp n ->
      (* the drop is under n bytes, and n is at most a page (a by-value
         agg aligned above 4096 is refused), so touching both ends never
         skips a page *)
      probe a;
      A.alu_ri a A.And A.Rsp (- n);
      probe a
  | Ud2 -> A.ud2 a
  | Loc _ -> ()
  | Asm _ ->
      let sn : snippet = snippet () in
      while A.pos a mod sn.salign <> 0 do A.byte a 0x90 done;
      let start = A.pos a in
      sn.sstart <- start;
      sn.sbase <- code_base;
      Option.iter (asm_frame start (String.length sn.code)) sn.sframe;
      A.raw a sn.code
        (List.map
           (fun (o, t, kind, addend) ->
             match t with
             | `Sym s -> (o, s, kind, Int64.to_int addend)
             | `Here v -> (o, code_base, kind, start + v + Int64.to_int addend))
           sn.srelocs)

(* Zeroization at return (Fir SPEC §8), before the epilogue proper: zero
   every byte from the red zone below rsp (128 bytes) up to the saved
   registers, which covers the frame (slots, spills, outgoing arguments),
   then every caller-saved register except the results. The loop's length
   is the frame's size, never data. r10 and r11 are free here (never
   results); the flags end up set from the loop's compare.
     lea r11, [rsp - 128]; lea r10, [rbp - 8*nsaved]
   L: mov qword [r11], 0; add r11, 8; cmp r11, r10; jb L *)
let zeroize a ~red_zone fr results =
  if red_zone then A.lea a A.R11 (A.Base (A.Rsp, -128))
  else A.mov_rr a A.R11 A.Rsp;
  A.lea a A.R10 (A.Base (A.Rbp, -8 * fr.nsaved));
  let l = A.new_label a and done_ = A.new_label a in
  (* A fixed frame can be empty. With the red zone disabled, leave the
     saved registers and everything below the current stack pointer alone. *)
  if not red_zone then begin
    A.alu_rr a A.Cmp A.R11 A.R10;
    A.jcc a A.Ae done_
  end;
  A.place a l;
  A.mov_mi a 8 (A.Base (A.R11, 0)) 0;
  A.alu_ri a A.Add A.R11 8;
  A.alu_rr a A.Cmp A.R11 A.R10;
  A.jcc a A.B l;
  if not red_zone then A.place a done_;
  List.iter
    (fun r -> if not (List.mem (G r) results) then A.alu_rr a A.Xor r r)
    Mir.caller_saved;
  for x = 0 to 15 do
    if not (List.mem (X x) results) then A.logic_xx a ~double:false `Xor x x
  done

(* one function: returns its text size, its frame events for the
   .eh_frame (Mir_cfi), at offsets from the function's start, and its line
   rows for the .debug_line (Mir_dwarf): the text offset of each [Loc]
   marker with its location id, and where its source variables live
   (Mir_dwarf.vloc, by Fir variable index) *)
let func a ~red_zone ~pic ~func_label ~snippet ~block_pos ~code_base (f : func) (ra : Mir_ra.result) =
  if f.ms_abi && f.zeroize then unsupported "secret function with the Microsoft x64 calling convention";
  let fr = layout f (List.length ra.saved) in
  let start = A.pos a in
  let events = ref [] and rows = ref [] in
  let event e = events := (A.pos a - start, e) :: !events in
  A.place a (Option.get (func_label f.name));
  A.push a A.Rbp;
  event Mir_cfi.Pushed_rbp;
  A.mov_rr a A.Rbp A.Rsp;
  event Mir_cfi.Framed;
  List.iteri
    (fun i r ->
      A.push a r;
      event (Mir_cfi.Saved (r, i)))
    ra.saved;
  probed_frame a fr.size;
  let saved_xmms visit =
    Option.iter (fun base ->
      for x = 6 to 15 do
        let off = base - 16 * (x - 6) in
        visit x off (A.Base (A.Rbp, -off))
      done) fr.ms_xmm_off
  in
  saved_xmms (fun x off mem ->
    A.movdqu_store a mem x;
    event (Mir_cfi.Saved_xmm (x, off)));
  let nb = Array.length ra.items in
  let labels = Array.init nb (fun _ -> A.new_label a) in
  let slot_mem v = mem fr (at (Bslot ra.vslot.(v))) in
  (* an xmm register to or from a frame slot: all 16 bytes when the slot
     has them (a vector), else the low 8 *)
  let wide_slot s = fst (Dynarray.get f.slots s) >= 16 in
  let xld x m s = if wide_slot s then A.movdqu_load a x m else A.mov_xm a ~double:true x m in
  let xst m s x = if wide_slot s then A.movdqu_store a m x else fstore a true m x in
  (* the text offset where the code of each Mir_ra position ends (an
     instruction with its spills, or a block's terminator) *)
  let pend = Dynarray.create () in
  let ended () = Dynarray.add_last pend (A.pos a) in
  let asm_frame pc len code =
    events := (pc - start, Mir_cfi.Inline (code, pc + len - start)) :: !events in
  let emit = ins a fr ~pic ~func_label ~out:(out_area f) ~snippet ~asm_frame ~code_base in
  for b = 0 to nb - 1 do
    A.place a labels.(b);
    block_pos b (A.pos a);
    List.iter
      (function
        | Mir_ra.Orig (_, Loc n) -> rows := (A.pos a, n) :: !rows; ended ()
        | Mir_ra.Orig (_, i) -> emit i; ended ()
        | Reload (v, (G r)) -> A.load_zx a 8 r (slot_mem v)
        | Reload (v, (X x)) -> xld x (slot_mem v) ra.vslot.(v)
        | Spill (v, (G r)) -> A.store a 8 (slot_mem v) r; Dynarray.set pend (Dynarray.length pend - 1) (A.pos a)
        | Spill (v, (X x)) -> xst (slot_mem v) ra.vslot.(v) x; Dynarray.set pend (Dynarray.length pend - 1) (A.pos a)
        | Move (d, r) -> emit (Copy (d, r))
        | Ld (s, G r) -> A.load_zx a 8 r (mem fr (at (Bslot s)))
        | Ld (s, X x) -> xld x (mem fr (at (Bslot s))) s
        | St (s, G r) -> A.store a 8 (mem fr (at (Bslot s))) r
        | St (s, X x) -> xst (mem fr (at (Bslot s))) s x
        | Remat (_, i) -> emit i
        | Elided _ -> ()
        | Reload (_, V _) | Spill (_, V _) | Ld (_, V _) | St (_, V _) ->
            invalid_arg "Mir_emit: virtual register after allocation")
      ra.items.(b);
    let next = b + 1 in
    (match ra.terms.(b) with
    | Jmp t | Asmgoto (t, _) -> if t <> next then A.jmp a labels.(t)
    | Jcc (c, t, e) ->
        A.jcc a c labels.(t);
        if e <> next then A.jmp a labels.(e)
    | Ret results ->
        event Mir_cfi.Epilogue;
        if f.zeroize then zeroize a ~red_zone fr results;
        saved_xmms (fun x _ mem -> A.movdqu_load a x mem; event (Mir_cfi.Restored_xmm x));
        A.lea a A.Rsp (A.Base (A.Rbp, -8 * fr.nsaved));
        List.iter (A.pop a) (List.rev ra.saved);
        A.pop a A.Rbp;
        event Mir_cfi.Popped_rbp;
        A.ret a;
        event Mir_cfi.Returned
    | Halt -> A.ud2 a
    | Jmpr (r, _) -> A.ext_r a ~w:false [ 0xff ] 4 (gr r));
    ended ()
  done;
  let size = A.pos a - start in
  (* a restore_state at the very end describes no code *)
  (* two markers at one offset (a location with no code of its own): the
     later one describes the code *)
  let rows =
    List.fold_left
      (fun acc (o, n) -> match acc with (o', _) :: rest when o' = o -> (o, n) :: rest | _ -> (o, n) :: acc)
      [] (List.rev !rows)
  in
  (* variables: a frame slot is the variable's for the whole function, and
     so is a spill slot (never shared, written after every definition); a
     register holds its virtual register over the interval, from the end
     of its first position to the end of its last *)
  let dreg = function G r -> Mir_cfi.dwarf_reg r | X x -> 17 + x | V _ -> invalid_arg "Mir_emit: vreg" in
  let in_reg v op =
    match ra.assign.(v) with
    | Some r when ra.hi.(v) >= 0 ->
        let s = Dynarray.get pend ra.lo.(v) and e = Dynarray.get pend ra.hi.(v) in
        if s < e then Some (Mir_dwarf.Ranges [ (s, e, op (dreg r)) ]) else None
    | _ -> None
  in
  let vlocs =
    List.filter_map
      (fun (i, h) ->
        let l =
          match h with
          | Hslot s -> Some (Mir_dwarf.Expr (Mir_dwarf.op_fbreg (-fr.slot_off.(s))))
          | Hcell v when ra.vslot.(v) >= 0 -> Some (Mir_dwarf.Expr (Mir_dwarf.op_fbreg (-fr.slot_off.(ra.vslot.(v)))))
          | Hcell v -> in_reg v Mir_dwarf.op_reg
          | Hptr v when ra.vslot.(v) >= 0 ->
              Some (Mir_dwarf.Expr (Mir_dwarf.op_fbreg (-fr.slot_off.(ra.vslot.(v))) ^ Mir_dwarf.op_deref))
          | Hptr v -> in_reg v (fun r -> Mir_dwarf.op_breg r 0)
        in
        Option.map (fun l -> (i, l)) l)
      f.homes
  in
  ( size,
    List.rev (List.filter (fun (o, e) -> not (e = Mir_cfi.Returned && o = size)) !events),
    List.rev rows,
    vlocs )

(* ---------- startup ---------- *)

(* Thread-local storage for an executable without libc (part of _start).
   x86-64 TLS ABI, variant II: the thread pointer (fs base) points at a
   TCB whose first word is the thread pointer itself, and the TLS block,
   align_up(p_memsz, p_align) bytes, ends right at the thread pointer
   (the linker resolves TPOFF32/GOTTPOFF to offsets below it). The block
   starts with the p_filesz bytes of the .tdata image at p_vaddr; the rest
   (.tbss) stays zero.

   The PT_TLS program header is found through __ehdr_start (the ELF
   header, which the linker defines). With no PT_TLS nothing is done. The
   block and a 64-byte zeroed TCB come from one mmap; a failing mmap or
   arch_prctl stops the program with ud2. Uses only caller-saved registers
   and rbx/r13-r15 (_start has no caller); r12 holds the initial rsp. *)
let tls_setup a =
  let open A in
  let loop = new_label a and found = new_label a and fail = new_label a and done_ = new_label a in
  let m r d = Base (r, d) in
  lea a Rax (Rip ("__ehdr_start", 0));
  (* load bias: 0 for ET_EXEC; the header's own address for ET_DYN (PIE) *)
  mov_imm a R11 0L;
  load_zx a 2 R10 (m Rax 16);
  alu_ri a Cmp R10 3;
  let not_dyn = new_label a in
  jcc a Ne not_dyn;
  mov_rr a R11 Rax;
  place a not_dyn;
  load_zx a 8 Rcx (m Rax 32) (* e_phoff *);
  alu_rr a Add Rcx Rax;
  load_zx a 2 Rdx (m Rax 56) (* e_phnum *);
  load_zx a 2 R8 (m Rax 54) (* e_phentsize *);
  place a loop;
  test_rr a Rdx Rdx;
  jcc a E done_;
  load_zx a 4 R9 (m Rcx 0) (* p_type *);
  alu_ri a Cmp R9 7 (* PT_TLS *);
  jcc a E found;
  alu_rr a Add Rcx R8;
  alu_ri a Sub Rdx 1;
  jmp a loop;
  place a found;
  load_zx a 8 R13 (m Rcx 16) (* p_vaddr *);
  alu_rr a Add R13 R11;
  load_zx a 8 R14 (m Rcx 32) (* p_filesz *);
  load_zx a 8 R15 (m Rcx 40) (* p_memsz *);
  load_zx a 8 Rbx (m Rcx 48) (* p_align, 0 meaning 1 *);
  let aligned = new_label a in
  test_rr a Rbx Rbx;
  jcc a Ne aligned;
  mov_imm a Rbx 1L;
  place a aligned;
  (* r15 = the block size, align_up(p_memsz, p_align) *)
  lea a R15 (Sib { base = Some R15; index = Rbx; scale = 1; disp = -1; sym = None });
  mov_rr a Rax Rbx;
  neg a Rax;
  alu_rr a And R15 Rax;
  (* rbx = the thread pointer's alignment, max(p_align, 64) *)
  let big = new_label a in
  alu_ri a Cmp Rbx 64;
  jcc a Ae big;
  mov_imm a Rbx 64L;
  place a big;
  (* mmap(0, block + alignment + 64, PROT_READ|PROT_WRITE,
     MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) *)
  lea a Rsi (Sib { base = Some R15; index = Rbx; scale = 1; disp = 64; sym = None });
  mov_imm a Rdi 0L;
  mov_imm a Rdx 3L;
  mov_imm a R10 0x22L;
  mov_imm a R8 (-1L);
  mov_imm a R9 0L;
  mov_imm a Rax 9L;
  syscall a;
  alu_ri a Cmp Rax (-4095);
  jcc a Ae fail;
  (* thread pointer = align_up(mapping + block, alignment), in r15 after
     the copy; the block is [tp - block, tp) *)
  lea a Rax (Sib { base = Some Rax; index = R15; scale = 1; disp = -1; sym = None });
  alu_rr a Add Rax Rbx;
  mov_rr a Rcx Rbx;
  neg a Rcx;
  alu_rr a And Rax Rcx;
  mov_rr a Rdi Rax;
  alu_rr a Sub Rdi R15;
  mov_rr a R15 Rax;
  mov_rr a Rsi R13;
  mov_rr a Rcx R14;
  rep_movsb a;
  store a 8 (m R15 0) R15;
  (* arch_prctl(ARCH_SET_FS, tp) *)
  mov_imm a Rdi 0x1002L;
  mov_rr a Rsi R15;
  mov_imm a Rax 158L;
  syscall a;
  test_rr a Rax Rax;
  jcc a E done_;
  place a fail;
  ud2 a;
  place a done_

(* Static PIE (part of _start, before anything reads a pointer from
   data): no dynamic loader runs, so _start applies the image's
   R_X86_64_RELATIVE relocations itself, *(bias + offset) = bias + addend,
   walking DT_RELA/DT_RELASZ in _DYNAMIC. The image is linked at 0, so the
   bias is the runtime address of __ehdr_start. Any other relocation kind,
   or DT_REL/DT_RELR tables, stops the program with ud2. Runs before
   tls_setup, so a .tdata image holding pointers is relocated before it is
   copied. *)
let relocate_self a =
  let open A in
  let m r d = Base (r, d) in
  let fail = new_label a and dloop = new_label a and dend = new_label a and next = new_label a in
  let rloop = new_label a and rdone = new_label a in
  lea a Rax (Rip ("__ehdr_start", 0));
  lea a Rcx (Rip ("_DYNAMIC", 0));
  mov_imm a R8 0L;
  mov_imm a R9 0L;
  place a dloop;
  load_zx a 8 Rdx (m Rcx 0) (* d_tag *);
  test_rr a Rdx Rdx;
  jcc a E dend;
  let not_rela = new_label a and not_relasz = new_label a in
  alu_ri a Cmp Rdx 7 (* DT_RELA *);
  jcc a Ne not_rela;
  load_zx a 8 R8 (m Rcx 8);
  alu_rr a Add R8 Rax;
  jmp a next;
  place a not_rela;
  alu_ri a Cmp Rdx 8 (* DT_RELASZ *);
  jcc a Ne not_relasz;
  load_zx a 8 R9 (m Rcx 8);
  jmp a next;
  place a not_relasz;
  alu_ri a Cmp Rdx 17 (* DT_REL *);
  jcc a E fail;
  alu_ri a Cmp Rdx 36 (* DT_RELR *);
  jcc a E fail;
  place a next;
  alu_ri a Add Rcx 16;
  jmp a dloop;
  place a dend;
  place a rloop;
  test_rr a R9 R9;
  jcc a E rdone;
  load_zx a 4 R10 (m R8 8) (* ELF64_R_TYPE(r_info) *);
  alu_ri a Cmp R10 8 (* R_X86_64_RELATIVE *);
  jcc a Ne fail;
  load_zx a 8 R10 (m R8 0);
  alu_rr a Add R10 Rax;
  load_zx a 8 R11 (m R8 16);
  alu_rr a Add R11 Rax;
  store a 8 (m R10 0) R11;
  alu_ri a Add R8 24;
  alu_ri a Sub R9 24;
  jmp a rloop;
  place a fail;
  ud2 a;
  place a rdone

(* ---------- data ---------- *)

type data_out = {
  buf : Buffer.t;
  mutable alignment : int;
  mutable relocs : A.reloc list;
  mutable syms : Elf64.symbol list;
}

let place_data (d : data_out) section ~name ~align ~size write =
  d.alignment <- max d.alignment align;
  while Buffer.length d.buf mod max 1 align <> 0 do
    Buffer.add_char d.buf '\000'
  done;
  let offset = Buffer.length d.buf in
  write offset;
  d.syms <- { Elf64.name; section; offset; size; func = false } :: d.syms

(* a global's bytes (padded to its size) and its pointer relocations *)
let global_image block_offsets (g : F.global) =
  let b = Buffer.create (max 1 g.size) and relocs = ref [] in
  (match g.init with
  | None -> ()
  | Some pieces ->
      List.iter
        (function
          | F.Bytes s -> Buffer.add_string b s
          | Zeros n -> Buffer.add_string b (String.make n '\000')
          | Reloc { sym; addend; width = 8; pcrel = false } ->
              relocs := (Buffer.length b, sym, Int64.to_int addend, false) :: !relocs;
              Buffer.add_string b (String.make 8 '\000')
          | Reloc { sym; addend; width = 4; pcrel = true } ->
              (* R_X86_64_PC32: resolved at link time, no load-time relocation *)
              relocs := (Buffer.length b, sym, Int64.to_int addend, true) :: !relocs;
              Buffer.add_string b (String.make 4 '\000')
          | Reloc { width; _ } -> unsupported "@%s: a %d-byte relocation" g.gname width
          | Blockref { fn; block } ->
              relocs := (Buffer.length b, Mir_isel.block_symbol fn block, 0, false) :: !relocs;
              Buffer.add_string b (String.make 8 '\000')
          | Blockdiff { fn; lhs; rhs; width } ->
              let offset block =
                match Hashtbl.find_opt block_offsets (fn, block) with
                | Some offset -> offset
                | None -> unsupported "@%s: blockdiff block %d of `%s` was not emitted" g.gname block fn
              in
              let delta = offset lhs - offset rhs in
              for i = 0 to width - 1 do
                Buffer.add_char b (Char.chr ((delta asr (8 * i)) land 255))
              done)
        pieces);
  if Buffer.length b > g.size then unsupported "@%s: initializer larger than the global" g.gname;
  Buffer.add_string b (String.make (g.size - Buffer.length b) '\000');
  (Buffer.contents b, List.rev !relocs)

(* ---------- module ---------- *)

let prepare (m : F.modul) =
  (* trapping divisions the 64-bit div/idiv does not trap on by itself
     become a check and a `trap`, before scalarization and limbs drop
     the flag *)
  Fir_trapdiv.run ~hw:true m;
  (* vectors to lanes first: lane-wise fma becomes scalar fma, which
     the runtime helpers then serve *)
  Fir_scalarize.run m;
  Mir_runtime.add m;
  (* integers wider than 64 bits (also lanes of scalarized vectors) to
     i64 limbs: Mir_isel computes in i1..i64 only *)
  Fir_wide.run m;
  (match Fir_verify.verify m with
  | [] -> ()
  | errs -> invalid_arg ("Fir after scalarization and Fir_wide:\n" ^ String.concat "\n" errs));
  Fir_ct.check m

(* [startup]: add _start (an executable without libc). [pic]: position-
   independent code (-c objects, hosted links, PIE executables). [tls_gd]:
   general-dynamic TLS (see [compile_object]).
   [self_relocate]: _start applies the image's own RELATIVE relocations (a
   static PIE has no dynamic loader to do it). [debug]: add DWARF line
   tables (Mir_dwarf); the allocated sections are the same either way. *)
let generate ?(debug = false) ?(tls_gd = false) ?(red_zone = true) ?iter_funcs ?(has_asm = true)
    ~startup ~pic ?(kernel_model = false) ?(no_sse = false) ?(general_regs_only = false) ?(self_relocate = false) (m : F.modul) =
  try
    if m.triple <> Target_layout.x86_64_linux.triple then
      unsupported "only Linux x86-64 object emission is supported (target %s)" m.triple;
    let iter = match iter_funcs with
      | Some iter -> iter
      | None -> prepare m; fun visit -> F.Vec.iter visit m.funcs
    in
    let syms = Elf_attrs.of_fir m in
    let env = Mir_isel.env m in
    let a = A.create () in
    let labels = Hashtbl.create 64 in
    let named_code = Hashtbl.create 8 and code_names = ref [] in
    let function_sections = Hashtbl.create 16 in
    F.Vec.iter (fun (f : F.func) ->
        if f.defined then
          match f.code_section with
          | Some s when s <> ".text" -> Hashtbl.replace function_sections f.name s
          | _ -> ())
      m.funcs;
    let code_output name =
      match Hashtbl.find_opt named_code name with
      | Some out -> out
      | None ->
          let idx = Hashtbl.length named_code in
          let out = (A.create (), Hashtbl.create 16, Printf.sprintf ".Lfas.text.%d" idx, Elf64.Extra idx) in
          Hashtbl.add named_code name out;
          code_names := name :: !code_names;
          out
    in
    (* Labels may resolve to assembly offsets only within one code section. *)
    let func_label a labels section name =
      if Hashtbl.find_opt function_sections name <> section then None
      else
        match Hashtbl.find_opt labels name with
        | Some l -> Some l
        | None ->
          let l = A.new_label a in
          Hashtbl.add labels name l;
          Some l
    in
    let func_syms = ref [] in
    let fdes = ref [] in
    let lines = ref [] in
    (* text offsets of every block, for blockaddr symbols *)
    let block_offsets = Hashtbl.create 256 in
    (* Inline asm is rendered with final frame offsets and assembled as a
       group. The streamed driver replays the allocations from scratch
       instead of keeping every machine function alive until assembly. *)
    let select visit =
      iter
        (fun (fn : F.func) ->
          if fn.defined then begin
            let mf = Mir_isel.select ~pic ~tls_gd ~kernel_model ~no_sse ~general_regs_only env fn in
            if dump then prerr_string (Mir.to_string mf);
            Mir_export.add_func mf;
            let ra =
              match Mir_import.find mf with
              | [] ->
                  let ra = Mir_ra.run mf in
                  if dump then prerr_string (Mir_ra.to_string mf ra);
                  Mir_check.run mf ra;
                  Mir_ct.run mf ra;
                  ra
              | cands ->
                  (* FAS_RA_IMPORT: an allocation from the lab, checked by
                     Mir_icheck instead of Mir_check *)
                  let ra, slots = Mir_import.run mf cands in
                  if dump then prerr_string (Mir_ra.to_string mf ra);
                  Mir_icheck.run mf ra ~slots;
                  Mir_ct.run mf ra;
                  ra
            in
            visit ((fn : F.func), mf, ra)
          end);
      Mir_export.flush m
    in
    let emit snippet ((fn : F.func), mf, ra) =
      let section = Hashtbl.find_opt function_sections fn.name in
      let a, labels, code_base, elf_section =
        match section with
        | Some name -> code_output name
        | None -> (a, labels, Mir_cfi.text_symbol, Elf64.Text)
      in
      A.align a 16;
      let offset = A.pos a in
      let size, events, rows, vlocs =
        func a ~red_zone ~pic ~func_label:(func_label a labels section) ~snippet ~code_base
          ~block_pos:(fun b o -> Hashtbl.replace block_offsets (fn.name, b) o) mf ra
      in
      let block_offset b = Hashtbl.find block_offsets (fn.name, b) - offset in
      let cleanup = Option.map (fun (pad, resume) -> (block_offset pad, block_offset resume)) fn.unwind_cleanup in
      fdes := { Mir_cfi.code_base; start = offset; size; events; entry = false; cleanup } :: !fdes;
      if debug then begin
        (* rows without a file name say nothing *)
        let rows = List.filter (fun (_, n) -> let file, _, _ = F.Vec.get m.locs n in file <> "") rows in
        let vars = Array.of_list fn.vars in
        lines :=
          { Mir_dwarf.name = fn.name; display = fn.display; external_ = fn.linkage <> F.Local; code_base; start = offset;
            decl_loc = fn.decl_loc; size; rows; vars = fn.vars;
            vlocs = List.map (fun (i, location) ->
                let var = vars.(i) in
                if not var.vindirect then (i, location)
                else
                  let deref e = e ^ Mir_dwarf.op_deref in
                  (i, match location with
                      | Mir_dwarf.Expr e -> Mir_dwarf.Expr (deref e)
                      | Mir_dwarf.Ranges rs ->
                          Mir_dwarf.Ranges (List.map (fun (s, e, x) -> (s, e, deref x)) rs)))
              vlocs }
          :: !lines
      end;
      func_syms := { Elf64.name = fn.name; section = elf_section; offset; size; func = true } :: !func_syms
    in
    let text_align, inline_sections =
      if not has_asm then begin
        select (emit (fun () -> assert false));
        (16, fun ~first:_ ~section_of_base:_ -> ([], [], []))
      end else begin
        let emit_all iter =
          let snippet, align, materialize = asm_snippets iter in
          iter (emit snippet);
          (align, materialize)
        in
        match iter_funcs with
        | None ->
            let compiled = ref [] in
            select (fun f -> compiled := f :: !compiled);
            let compiled = List.rev !compiled in
            emit_all (fun visit -> List.iter visit compiled)
        | Some _ ->
            let compiled = Fir_replay.create () in
            Fun.protect ~finally:(fun () -> Fir_replay.close compiled) (fun () ->
              select (fun (fn, mf, ra) ->
                let header = F.func_header fn in
                header.vars <- fn.vars;
                Fir_replay.append compiled (header, mf, ra));
              emit_all (Fir_replay.iter compiled))
      end
    in
    if startup then begin
      A.align a 16;
      let offset = A.pos a in
      (* _start: argc at [rsp], argv after it; set up thread-local storage,
         then exit(main(argc, argv)) *)
      A.alu_rr a A.Xor A.Rbp A.Rbp;
      A.mov_rr a A.R12 A.Rsp;
      if self_relocate then relocate_self a;
      tls_setup a;
      A.load_zx a 8 A.Rdi (A.Base (A.R12, 0));
      A.lea a A.Rsi (A.Base (A.R12, 8));
      A.alu_ri a A.And A.Rsp (-16);
      (match if Hashtbl.mem env.label_funcs "main" then func_label a labels None "main" else None with
      | Some l -> A.call_label a l
      | None -> A.call_symbol a "main");
      A.rr a ~w:false [ 0x89 ] ~reg:A.Rax ~rm:A.Rdi;
      A.mov_imm a A.Rax 231L;
      A.syscall a;
      A.ud2 a;
      func_syms := { Elf64.name = "_start"; section = Elf64.Text; offset; size = A.pos a - offset; func = true }
                   :: !func_syms;
      fdes := { Mir_cfi.code_base = Mir_cfi.text_symbol; start = offset; size = A.pos a - offset;
                events = []; entry = true; cleanup = None } :: !fdes
    end;
    let text, text_relocs = A.finish a in
    let code_sections, code_syms =
      List.split (List.rev_map
        (fun name ->
          let code, _, symbol, section = Hashtbl.find named_code name in
          let bytes, relocs = A.finish code in
          ({ Native_object.name; flags = 6; alignment = text_align; contents = Fas_linker.Elf_object.Data bytes; relocs },
           { Elf64.name = symbol; section; offset = 0; size = 0; func = false }))
        !code_names)
    in
    let code_slots = List.length code_sections in
    let rodata = { buf = Buffer.create 256; alignment = 16; relocs = []; syms = [] } in
    let data = { buf = Buffer.create 256; alignment = 1; relocs = []; syms = [] } in
    let bss = ref 0 and bss_alignment = ref 1 and bss_syms = ref [] in
    (* named sections and .tdata/.tbss, in order of first use: their data,
       whether any global in them is writable, whether all are zero *)
    let extras = Hashtbl.create 8 and extra_order = ref [] in
    let extra name ~tls =
      match Hashtbl.find_opt extras name with
      | Some x -> x
      | None ->
          let x = ({ buf = Buffer.create 64; alignment = 1; relocs = []; syms = [] }, ref false, ref true, tls) in
          Hashtbl.add extras name x;
          extra_order := name :: !extra_order;
          x
    in
    let extra_index name =
      let rec find i = function
        | [] -> assert false
        | n :: rest -> if n = name then i else find (i + 1) rest
      in
      find code_slots (List.rev !extra_order)
    in
    let commons = ref [] in
    F.Vec.iter
      (fun (g : F.global) ->
        if g.gcommon then
          commons := (g.gname, g.size, max 1 g.galign) :: !commons
        else if g.gdefined then begin
          (* symbol offsets and rip-relative references are 32-bit *)
          if g.size > 0x7fff_ffff then
            unsupported "@%s: a global of %d bytes (at most 2 GiB - 1, the small code model)" g.gname g.size;
          (* the padding before an aligned global is written out; 2 MiB is
             a huge page *)
          if g.galign > 0x20_0000 then unsupported "@%s: aligned to %d (at most 2 MiB)" g.gname g.galign;
          (* an all-zero global is not materialized: a large one goes to
             .bss without its bytes ever being built *)
          let zero =
            match g.init with
            | None -> true
            | Some ps -> List.for_all (function F.Bytes s -> String.for_all (( = ) '\000') s | Zeros _ -> true | _ -> false) ps
          in
          let image = lazy (global_image block_offsets g) in
          let pointers = if zero then [] else snd (Lazy.force image) in
          let align = max 1 g.galign in
          List.iter
            (fun (_, sym, _, _) ->
              if Hashtbl.mem env.tls_syms sym then
                unsupported "@%s: the address of thread-local @%s in an initializer" g.gname sym)
            pointers;
          (* a pointer in a position-independent object needs a load-time
             relocation, which read-only pages cannot take *)
          let readonly = g.readonly && not (pic && List.exists (fun (_, _, _, pcrel) -> not pcrel) pointers) in
          let place d section =
            place_data d section ~name:g.gname ~align ~size:g.size (fun offset ->
                Buffer.add_string d.buf (fst (Lazy.force image));
                List.iter
                  (fun (o, symbol, addend, pcrel) ->
                    d.relocs <-
                      (if pcrel then A.Raw { offset = offset + o; symbol; kind = 2; addend }
                       else A.Abs64 { offset = offset + o; symbol; addend })
                      :: d.relocs)
                  pointers)
          in
          let named name ~tls =
            let d, writable, all_zero, _ = extra name ~tls in
            if not readonly then writable := true;
            if not (zero && pointers = []) then all_zero := false;
            (* the index is only known once every section is seen: patched below *)
            place d (Elf64.Extra (-1))
          in
          match (g.section, g.thread_local) with
          | Some s, tls -> named s ~tls
          | None, true -> named (if zero && pointers = [] then ".tbss" else ".tdata") ~tls:true
          | None, false ->
              if zero && pointers = [] && not g.readonly then begin
                bss_alignment := max !bss_alignment align;
                bss := Mir_isel.align_up !bss align;
                bss_syms := { Elf64.name = g.gname; section = Elf64.Bss; offset = !bss; size = g.size; func = false }
                            :: !bss_syms;
                bss := !bss + g.size
              end
              else place (if readonly then rodata else data) (if readonly then Elf64.Rodata else Elf64.Data)
        end)
      m.globals;
    let extra_names = List.rev !extra_order in
    (* DWARF, after every allocated section; the symbols at the start of
       .debug_abbrev and .debug_line are the targets of the section-offset
       relocations in .debug_info *)
    let debug_sections =
      if not debug then []
      else
        Mir_dwarf.build ~cwd:(Sys.getcwd ()) ~producer:"fas 0.6.3" ~text_symbol:Mir_cfi.text_symbol
          ~text_size:(Bytes.length text) ~loc:(F.Vec.get m.locs)
          ~dtypes:(Array.init (F.Vec.length m.dtypes) (F.Vec.get m.dtypes))
          ~scopes:(Array.init (F.Vec.length m.scopes) (F.Vec.get m.scopes))
          ~loc_scope:(F.loc_scope m) ~globals:(List.rev m.dglobals) (List.rev !lines)
    in
    let debug_syms =
      List.concat
        (List.mapi
           (fun i (d : Mir_dwarf.section) ->
             let name = d.ssym in
             Option.to_list
               (Option.map
                  (fun name ->
                    { Elf64.name; section = Elf64.Extra (code_slots + List.length extra_names + i); offset = 0; size = 0;
                      func = false })
                  name))
           debug_sections)
    in
    let extra_syms =
      List.concat_map
        (fun name ->
          let d, _, _, _ = Hashtbl.find extras name in
          let i = extra_index name in
          List.map (fun (s : Elf64.symbol) -> { s with section = Elf64.Extra i }) d.syms)
        extra_names
    in
    let extra_sections =
      code_sections @ List.map
        (fun name ->
          let d, writable, all_zero, tls = Hashtbl.find extras name in
          (* SHF_ALLOC, SHF_WRITE unless every global in it is read-only,
             SHF_TLS for thread-local data. Zero-filled .tbss and .bss*
             sections take no file space (SHT_NOBITS). *)
          let nobits =
            !all_zero
            && (name = ".tbss" || name = ".bss" || String.starts_with ~prefix:".bss." name
               || (tls && String.starts_with ~prefix:".tbss." name))
          in
          let is_lifecycle =
            name = ".init_array" || String.starts_with ~prefix:".init_array." name
            || name = ".fini_array" || String.starts_with ~prefix:".fini_array." name
            || name = ".preinit_array" || String.starts_with ~prefix:".preinit_array." name
          in
          {
            Native_object.name;
            flags = 2 lor (if !writable || is_lifecycle then 1 else 0) lor if tls then 0x400 else 0;
            alignment = d.alignment;
            contents =
              (if nobits then Fas_linker.Elf_object.Bss (Buffer.length d.buf)
               else Fas_linker.Elf_object.Data (Buffer.to_bytes d.buf));
            relocs = d.relocs;
          })
        extra_names
      @ List.map
          (fun (d : Mir_dwarf.section) ->
            { Native_object.name = d.sname; flags = 0; alignment = 1; contents = Fas_linker.Elf_object.Data d.bytes;
              relocs = d.relocs })
          debug_sections
    in
    let section_of_base name =
      if name = Mir_cfi.text_symbol then Elf64.Text
      else (List.find (fun (s : Elf64.symbol) -> s.name = name) code_syms).section
    in
    let inline_extras, inline_syms, inline_labels =
      inline_sections ~first:(List.length extra_sections) ~section_of_base
    in
    List.iter (fun (s : Elf64.symbol) -> Hashtbl.replace syms.binding s.name 0) inline_labels;
    let extra_sections = extra_sections @ inline_extras in
    let masm = module_asm ~first:(List.length extra_sections) m.module_asm in
    let extra_sections = extra_sections @ masm.msections in
    let eh_frame, lsda = Mir_cfi.build (List.rev !fdes) in
    let extra_sections, unwind_syms =
      if Bytes.length lsda = 0 then (extra_sections, [])
      else
        let index = List.length extra_sections in
        let sections =
          [ { Native_object.name = ".gcc_except_table"; flags = 2; alignment = 4;
              contents = Fas_linker.Elf_object.Data lsda; relocs = [] };
            { Native_object.name = ".data.rel.ro.fas.unwind"; flags = 3; alignment = 8;
              contents = Fas_linker.Elf_object.Data (Bytes.make 8 '\000');
              relocs = [ A.Abs64 { offset = 0; symbol = Mir_cfi.personality; addend = 0 } ] } ]
        in
        let symbol name i size =
          { Elf64.name; section = Elf64.Extra i; offset = 0; size; func = false }
        in
        ( extra_sections @ sections,
          [ symbol Mir_cfi.lsda_symbol index (Bytes.length lsda);
            symbol Mir_cfi.personality_slot (index + 1) 8 ] )
    in
    (* local symbols for the blocks whose address is taken, in code or data *)
    let blk_syms = Hashtbl.create 8 in
    let note_block fn b =
      if not (Hashtbl.mem blk_syms (fn, b)) then
        match Hashtbl.find_opt block_offsets (fn, b) with
        | Some offset ->
            let section =
              match Hashtbl.find_opt function_sections fn with
              | Some name -> let _, _, _, section = Hashtbl.find named_code name in section
              | None -> Elf64.Text
            in
            Hashtbl.replace blk_syms (fn, b)
              { Elf64.name = Mir_isel.block_symbol fn b; section; offset; size = 0; func = false }
        | None -> unsupported "blockaddr of block %d of `%s`, which is not defined here" b fn
    in
    Hashtbl.iter (fun (fn, b) () -> note_block fn b) env.blk_refs;
    F.Vec.iter
      (fun (g : F.global) ->
        if g.gdefined then
          Option.iter (List.iter (function F.Blockref { fn; block } -> note_block fn block | _ -> ())) g.init)
      m.globals;
    let blk_syms = List.sort compare (Hashtbl.fold (fun _ s l -> s :: l) blk_syms []) in
    (* FDEs point at their code through this local symbol (Mir_cfi) *)
    let text_sym = { Elf64.name = Mir_cfi.text_symbol; section = Elf64.Text; offset = 0; size = 0; func = false } in
    let defined_syms =
      text_sym :: List.rev !func_syms @ rodata.syms @ data.syms @ !bss_syms @ extra_syms @ debug_syms @ blk_syms
      @ code_syms @ inline_syms @ inline_labels @ masm.msyms @ unwind_syms
    in
    let alias_syms =
      List.map
        (fun (al : Elf_attrs.alias) ->
          match List.find_opt (fun (s : Elf64.symbol) -> s.name = al.target) defined_syms with
          | Some s -> { s with Elf64.name = al.name; offset = s.offset + al.addend; size = max 0 (s.size - al.addend) }
          | None -> unsupported "alias `%s`: its target `%s` was not emitted" al.name al.target)
        syms.aliases
    in
    let ifunc_names = Hashtbl.create 4 in
    let ifunc_syms =
      List.map
        (fun (iv : Elf_attrs.ifunc) ->
          Hashtbl.replace ifunc_names iv.iname ();
          match List.find_opt (fun (s : Elf64.symbol) -> s.name = iv.resolver) defined_syms with
          | Some s -> { s with Elf64.name = iv.iname }
          | None -> unsupported "ifunc `%s`: its resolver `%s` was not emitted" iv.iname iv.resolver)
        syms.ifuncs
    in
    Ok
      (Native_object.emit
         ~eh_frame
         ~extra:extra_sections
         ~commons:(List.rev !commons)
         ~tls:(Hashtbl.mem env.tls_syms)
         ~ifunc:(Hashtbl.mem ifunc_names)
         ~global:(fun n -> n = "_start")
         ~binding:(fun n ->
           match Hashtbl.find_opt masm.mbinding n with Some b -> Some b | None -> Hashtbl.find_opt syms.binding n)
         ~visibility:(fun n ->
           match Hashtbl.find_opt masm.mvisibility n with
           | Some v -> v
           | None -> Option.value (Hashtbl.find_opt syms.visibility n) ~default:0)
         ~alignments:[| text_align; rodata.alignment; data.alignment; !bss_alignment |]
         {
           text;
           text_relocs;
           rodata = Buffer.to_bytes rodata.buf;
           rodata_relocs = rodata.relocs;
           data = Buffer.to_bytes data.buf;
           data_relocs = data.relocs;
           bss_size = !bss;
           symbols = defined_syms @ alias_syms @ ifunc_syms;
           entry = "_start";
         })
  with
  | Unsupported msg -> Error ("native backend: " ^ msg)
  | Fir_scalarize.Unsupported msg -> Error ("native backend: " ^ msg)
  | Fir_wide.Unsupported msg -> Error ("native backend: " ^ msg)
  | Mir_ra.Failure msg -> Error ("native backend internal error (allocator): " ^ msg)
  | Mir_check.Failure msg -> Error ("native backend internal error (allocation check): " ^ msg)
  | Mir_ct.Failure msg -> Error ("native backend internal error (constant-time violation in machine code:\n" ^ msg ^ ")")
  | Fir_ct.Failure msg -> Error ("native backend: " ^ msg)
  | Mir_import.Failure msg -> Error ("FAS_RA_IMPORT: " ^ msg)
  | Mir_icheck.Failure msg -> Error ("FAS_RA_IMPORT: the imported allocation is wrong: " ^ msg)
  | Fas_linker.Error.Error msg -> Error ("native backend: " ^ msg)
  | Invalid_argument msg -> Error ("native backend internal error: " ^ msg)

(* [tls_gd]: reach thread-local variables through __tls_get_addr
   (general-dynamic TLS), for objects that go into a shared library, which
   dlopen may load after the static TLS block is laid out; otherwise
   initial-exec. Static links reject general-dynamic relocations. *)
let compile_object ?debug ?(pic = true) ?tls_gd ?red_zone
    ?(kernel_model = false) ?no_sse ?general_regs_only ?iter_funcs ?has_asm m =
  (* The kernel may run early code via an identity mapping. Normal C
     symbol addresses must still be the link-time high-half addresses. *)
  if kernel_model && tls_gd = Some true then
    Error "native backend: -mcmodel=kernel is incompatible with -fPIC"
  else
    generate ?debug ?tls_gd ?red_zone ?iter_funcs ?has_asm ~startup:false
      ~pic:(pic && not kernel_model) ~kernel_model ?no_sse ?general_regs_only m

(* a static executable, or a static PIE when [options] ask for one *)
let link_files ?debug ?iter_funcs ?has_asm ?(inputs = []) ?options ~output m =
  let mode = match options with Some (o : Fas_linker.Options.t) -> o.mode | None -> Static in
  let pic = mode <> Static in
  let tls_gd = mode = Shared in
  let startup = mode <> Shared in
  match generate ?debug ?iter_funcs ?has_asm ~startup ~pic ~tls_gd ~self_relocate:(mode = Static_pie) m with
  | Error _ as e -> e
  | Ok obj -> (
      try
        Fas_linker.Static_link.link_input_files ?options ~output
          (Fas_linker.Input.Object { path = "<native module>"; data = Bytes.to_string obj } :: inputs);
        Ok ()
      with Fas_linker.Error.Error msg -> Error ("native backend: linker: " ^ msg))

(* an executable image in memory *)
let compile ?(inputs = []) m =
  match generate ~startup:true ~pic:false m with
  | Error _ as e -> e
  | Ok obj -> (
      try
        Ok
          (Fas_linker.Static_link.link_inputs
             (Fas_linker.Input.Object { path = "<native module>"; data = Bytes.to_string obj } :: inputs))
      with Fas_linker.Error.Error msg -> Error ("native backend: linker: " ^ msg))
