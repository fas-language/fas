(* Mir_emit: frame layout, x86-64 encoding and the ELF object for the Mir
   path (roadmap v0.4). Entry points [compile_object] and [link_files] take
   a Fir module after Fir_wide.

   Frame of a function (rbp-based, rsp 16-aligned at calls):

     rbp + 16 + k   incoming stack arguments ([Bin])
     rbp + 8        return address
     rbp            saved rbp
     rbp - 8n       the n callee-saved registers the allocator used
     below          frame slots (allocas, scratch, one per vreg), each
                    aligned to its alignment (at most 16)
     rsp            outgoing stack arguments ([Bout])

   Every function is checked by Mir_check before it is encoded. *)

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

type frame = { slot_off : int array; nsaved : int; size : int  (** bytes below the saved registers *) }

let layout (f : func) nsaved =
  let off = ref (8 * nsaved) in
  let slot_off =
    Array.init (Dynarray.length f.slots) (fun i ->
        let size, align = Dynarray.get f.slots i in
        let align = max 1 align in
        if align > 16 then unsupported "frame slot aligned to %d" align;
        off := Mir_isel.align_up (!off + size) align;
        !off)
  in
  let total = Mir_isel.align_up (!off + out_area f) 16 in
  { slot_off; nsaved; size = total - (8 * nsaved) }

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

let ins a fr ~func_label ~out (i : ins) =
  let m = mem fr in
  match i with
  | Salloc (d, n) ->
      (* rsp drops by n (a multiple of 16); the block starts above the
         outgoing-argument area, which moves down with rsp *)
      A.alu_rr a A.Sub A.Rsp (gr n);
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
  | Lea (d, ad) -> A.lea a (gr d) (m ad)
  | Got (d, s) -> A.load_got a (gr d) s
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
  | Popcnt (d, s) ->
      A.byte a 0xf3;
      A.rr a [ 0x0f; 0xb8 ] ~reg:(gr d) ~rm:(gr s)
  | Fop (op, dbl, d, s) -> A.fop_xx a ~double:dbl op (xr d) (xr s)
  | Fsqrt (dbl, d, s) -> A.fop_xx a ~double:dbl A.Fsqrt (xr d) (xr s)
  | Ucomi (dbl, x, y) -> A.ucomi a ~double:dbl (xr x) (xr y)
  | Movgx (dbl, d, s) -> A.mov_xr a ~double:dbl (xr d) (gr s)
  | Movxg (dbl, d, s) -> A.mov_rx a ~double:dbl (gr d) (xr s)
  | Cvtsi (dbl, d, s) -> A.cvt_si2f a ~double:dbl (xr d) (gr s)
  | Cvttsi (dbl, d, s) -> A.cvt_f2si a ~double:dbl (gr d) (xr s)
  | Cvtff (from_double, d, s) -> A.cvt_ff a ~from_double (xr d) (xr s)
  | Round (dbl, d, s, mode) -> A.round_xx a ~double:dbl (xr d) (xr s) mode
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
  | Call { target; _ } -> (
      match target with
      | Tlabel name -> A.call_label a (func_label name)
      | Tsym name -> A.call_symbol a name
      | Treg r -> A.call_r a (gr r))
  | Syscall _ -> A.syscall a
  | Rep_movsb -> A.rep_movsb a
  | Rep_stosb -> A.rep_stosb a
  | Std -> A.std a
  | Cld -> A.cld a
  | Xchg (n, ad, r) -> A.xchg_mr a n (m ad) (gr r)
  | Xadd (n, ad, r) -> A.lock_xadd a n (m ad) (gr r)
  | Cmpxchg (n, ad, r) -> A.lock_cmpxchg a n (m ad) (gr r)
  | Mfence -> A.mfence a
  | Ud2 -> A.ud2 a

(* one function: returns its text size *)
let func a ~func_label ~block_pos (f : func) (ra : Mir_ra.result) =
  let fr = layout f (List.length ra.saved) in
  let start = A.pos a in
  A.place a (func_label f.name);
  A.push a A.Rbp;
  A.mov_rr a A.Rbp A.Rsp;
  List.iter (A.push a) ra.saved;
  if fr.size > 0 then A.alu_ri a A.Sub A.Rsp fr.size;
  let nb = Array.length ra.items in
  let labels = Array.init nb (fun _ -> A.new_label a) in
  let slot_mem v = mem fr (at (Bslot ra.vslot.(v))) in
  for b = 0 to nb - 1 do
    A.place a labels.(b);
    block_pos b (A.pos a);
    List.iter
      (function
        | Mir_ra.Orig (_, i) -> ins a fr ~func_label ~out:(out_area f) i
        | Reload (v, (G r)) -> A.load_zx a 8 r (slot_mem v)
        | Reload (v, (X x)) -> A.mov_xm a ~double:true x (slot_mem v)
        | Spill (v, (G r)) -> A.store a 8 (slot_mem v) r
        | Spill (v, (X x)) -> fstore a true (slot_mem v) x
        | Reload (_, V _) | Spill (_, V _) -> invalid_arg "Mir_emit: virtual register after allocation")
      ra.items.(b);
    let next = b + 1 in
    match ra.terms.(b) with
    | Jmp t -> if t <> next then A.jmp a labels.(t)
    | Jcc (c, t, e) ->
        A.jcc a c labels.(t);
        if e <> next then A.jmp a labels.(e)
    | Ret _ ->
        A.lea a A.Rsp (A.Base (A.Rbp, -8 * fr.nsaved));
        List.iter (A.pop a) (List.rev ra.saved);
        A.pop a A.Rbp;
        A.ret a
    | Halt -> A.ud2 a
    | Jmpr (r, _) -> A.ext_r a ~w:false [ 0xff ] 4 (gr r)
  done;
  A.pos a - start

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
let global_image (g : F.global) =
  let b = Buffer.create (max 1 g.size) and relocs = ref [] in
  (match g.init with
  | None -> ()
  | Some pieces ->
      List.iter
        (function
          | F.Bytes s -> Buffer.add_string b s
          | Zeros n -> Buffer.add_string b (String.make n '\000')
          | Reloc { sym; addend; width = 8 } ->
              relocs := (Buffer.length b, sym, Int64.to_int addend) :: !relocs;
              Buffer.add_string b (String.make 8 '\000')
          | Reloc { width; _ } -> unsupported "@%s: a %d-byte relocation" g.gname width
          | Blockref { fn; block } ->
              relocs := (Buffer.length b, Mir_isel.block_symbol fn block, 0) :: !relocs;
              Buffer.add_string b (String.make 8 '\000'))
        pieces);
  if Buffer.length b > g.size then unsupported "@%s: initializer larger than the global" g.gname;
  Buffer.add_string b (String.make (g.size - Buffer.length b) '\000');
  (Buffer.contents b, List.rev !relocs)

(* ---------- module ---------- *)

let generate ~startup (m : F.modul) =
  try
    let pic = not startup in
    (* vectors to lanes first: lane-wise fma becomes scalar fma, which
       the runtime helpers then serve *)
    Fir_scalarize.run m;
    Mir_runtime.add m;
    (match Fir_verify.verify m with
    | [] -> ()
    | errs -> invalid_arg ("Fir after scalarization:\n" ^ String.concat "\n" errs));
    let syms = Fir_to_ir.symbols m in
    let env = Mir_isel.env m in
    let a = A.create () in
    let labels = Hashtbl.create 64 in
    let func_label name =
      match Hashtbl.find_opt labels name with
      | Some l -> l
      | None ->
          let l = A.new_label a in
          Hashtbl.add labels name l;
          l
    in
    let func_syms = ref [] in
    (* text offsets of every block, for blockaddr symbols *)
    let block_offsets = Hashtbl.create 256 in
    F.Vec.iter
      (fun (fn : F.func) ->
        if fn.defined then begin
          let mf = Mir_isel.select ~pic env fn in
          if dump then prerr_string (Mir.to_string mf);
          let ra = Mir_ra.run mf in
          if dump then prerr_string (Mir_ra.to_string mf ra);
          Mir_check.run mf ra;
          A.align a 16;
          let offset = A.pos a in
          let size =
            func a ~func_label ~block_pos:(fun b o -> Hashtbl.replace block_offsets (fn.name, b) o) mf ra
          in
          func_syms := { Elf64.name = fn.name; section = Elf64.Text; offset; size; func = true } :: !func_syms
        end)
      m.funcs;
    if startup then begin
      A.align a 16;
      let offset = A.pos a in
      (* _start: argc at [rsp], argv after it; exit(main(argc, argv)) *)
      A.alu_rr a A.Xor A.Rbp A.Rbp;
      A.load_zx a 8 A.Rdi (A.Base (A.Rsp, 0));
      A.lea a A.Rsi (A.Base (A.Rsp, 8));
      A.alu_ri a A.And A.Rsp (-16);
      if Hashtbl.mem env.label_funcs "main" then A.call_label a (func_label "main")
      else A.call_symbol a "main";
      A.rr a ~w:false [ 0x89 ] ~reg:A.Rax ~rm:A.Rdi;
      A.mov_imm a A.Rax 231L;
      A.syscall a;
      A.ud2 a;
      func_syms := { Elf64.name = "_start"; section = Elf64.Text; offset; size = A.pos a - offset; func = true }
                   :: !func_syms
    end;
    let text, text_relocs = A.finish a in
    let rodata = { buf = Buffer.create 256; alignment = 16; relocs = []; syms = [] } in
    let data = { buf = Buffer.create 256; alignment = 1; relocs = []; syms = [] } in
    let bss = ref 0 and bss_alignment = ref 1 and bss_syms = ref [] in
    F.Vec.iter
      (fun (g : F.global) ->
        if g.gdefined then begin
          let bytes, pointers = global_image g in
          let align = max 1 g.galign in
          let zero = String.for_all (( = ) '\000') bytes in
          if zero && pointers = [] && not g.readonly then begin
            bss_alignment := max !bss_alignment align;
            bss := Mir_isel.align_up !bss align;
            bss_syms := { Elf64.name = g.gname; section = Elf64.Bss; offset = !bss; size = g.size; func = false }
                        :: !bss_syms;
            bss := !bss + g.size
          end
          else
            (* a pointer in a position-independent object needs a load-time
               relocation, which read-only pages cannot take *)
            let d, section =
              if g.readonly && not (pic && pointers <> []) then (rodata, Elf64.Rodata) else (data, Elf64.Data)
            in
            place_data d section ~name:g.gname ~align ~size:g.size (fun offset ->
                Buffer.add_string d.buf bytes;
                List.iter
                  (fun (o, symbol, addend) -> d.relocs <- A.Abs64 { offset = offset + o; symbol; addend } :: d.relocs)
                  pointers)
        end)
      m.globals;
    (* local symbols for the blocks whose address is taken, in code or data *)
    let blk_syms = Hashtbl.create 8 in
    let note_block fn b =
      if not (Hashtbl.mem blk_syms (fn, b)) then
        match Hashtbl.find_opt block_offsets (fn, b) with
        | Some offset ->
            Hashtbl.replace blk_syms (fn, b)
              { Elf64.name = Mir_isel.block_symbol fn b; section = Elf64.Text; offset; size = 0; func = false }
        | None -> unsupported "blockaddr of block %d of `%s`, which is not defined here" b fn
    in
    Hashtbl.iter (fun (fn, b) () -> note_block fn b) env.blk_refs;
    F.Vec.iter
      (fun (g : F.global) ->
        if g.gdefined then
          Option.iter (List.iter (function F.Blockref { fn; block } -> note_block fn block | _ -> ())) g.init)
      m.globals;
    let blk_syms = List.sort compare (Hashtbl.fold (fun _ s l -> s :: l) blk_syms []) in
    let defined_syms = List.rev !func_syms @ rodata.syms @ data.syms @ !bss_syms @ blk_syms in
    let alias_syms =
      List.map
        (fun (al : Elf_attrs.alias) ->
          match List.find_opt (fun (s : Elf64.symbol) -> s.name = al.target) defined_syms with
          | Some s -> { s with Elf64.name = al.name; offset = s.offset + al.addend; size = max 0 (s.size - al.addend) }
          | None -> unsupported "alias `%s`: its target `%s` was not emitted" al.name al.target)
        syms.aliases
    in
    Ok
      (Native_object.emit
         ~global:(fun n -> n = "_start")
         ~binding:(Hashtbl.find_opt syms.binding)
         ~visibility:(fun n -> Option.value (Hashtbl.find_opt syms.visibility n) ~default:0)
         ~alignments:[| 16; rodata.alignment; data.alignment; !bss_alignment |]
         {
           text;
           text_relocs;
           rodata = Buffer.to_bytes rodata.buf;
           rodata_relocs = rodata.relocs;
           data = Buffer.to_bytes data.buf;
           data_relocs = data.relocs;
           bss_size = !bss;
           symbols = defined_syms @ alias_syms;
           entry = "_start";
         })
  with
  | Unsupported msg -> Error ("mir backend: " ^ msg)
  | Fir_to_ir.Unsupported msg -> Error ("mir backend: " ^ msg)
  | Fir_scalarize.Unsupported msg -> Error ("mir backend: " ^ msg)
  | Fir_wide.Unsupported msg -> Error ("mir backend: " ^ msg)
  | Mir_ra.Failure msg -> Error ("mir backend internal error (allocator): " ^ msg)
  | Mir_check.Failure msg -> Error ("mir backend internal error (allocation check): " ^ msg)
  | Fas_linker.Error.Error msg -> Error ("mir backend: " ^ msg)
  | Invalid_argument msg -> Error ("mir backend internal error: " ^ msg)

let compile_object m = generate ~startup:false m

let link_files ?(inputs = []) ?options ~output m =
  match generate ~startup:true m with
  | Error _ as e -> e
  | Ok obj -> (
      try
        Fas_linker.Static_link.link_input_files ?options ~output
          (Fas_linker.Input.Object { path = "<mir module>"; data = Bytes.to_string obj } :: inputs);
        Ok ()
      with Fas_linker.Error.Error msg -> Error ("mir backend: linker: " ^ msg))

(* an executable image in memory *)
let compile ?(inputs = []) m =
  match generate ~startup:true m with
  | Error _ as e -> e
  | Ok obj -> (
      try
        Ok
          (Fas_linker.Static_link.link_inputs
             (Fas_linker.Input.Object { path = "<mir module>"; data = Bytes.to_string obj } :: inputs))
      with Fas_linker.Error.Error msg -> Error ("mir backend: linker: " ^ msg))
