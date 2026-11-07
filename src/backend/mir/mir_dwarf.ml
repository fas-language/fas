(* DWARF 5 line tables for the native backend (-g), as the LLVM path's
   "LineTablesOnly" debug info: debuggers and profilers map code back to
   source lines and name the functions, nothing more (variable locations are
   roadmap v0.8).

   Three non-allocated sections:

   .debug_abbrev  two abbreviations: the compile unit and a subprogram.
   .debug_info    one compile unit covering all of .text, with one
                  DW_TAG_subprogram per function (name, linkage name when the
                  source name differs, declaration line, address range,
                  frame base rbp).
   .debug_line    one line program; each function is its own sequence, from
                  DW_LNE_set_address at its first byte to DW_LNE_end_sequence
                  after its last.

   Where the rows come from: Mir_isel puts a [Loc n] pseudo-instruction
   before the code of each Fir instruction whose location differs from the
   previous one; Mir_emit records the code offset of each marker. The first
   row of a function is at its first byte (the prologue) with the line of
   its first marker; that marker's own row carries prologue_end, so a
   breakpoint on the function stops after the frame is built.

   Addresses are R_X86_64_64 relocations against [text_symbol] (the start of
   .text) plus the offset, the two section offsets in the compile unit
   header and DW_AT_stmt_list are R_X86_64_32 relocations against local
   symbols at the start of .debug_abbrev and .debug_line, so the linker can
   concatenate the sections of several objects. Strings are inline
   (DW_FORM_string): no .debug_str. *)

module A = X64

type func_lines = {
  name : string;  (** the symbol *)
  display : string option;  (** the source-level name, when it differs *)
  external_ : bool;
  start : int;  (** offset in .text *)
  size : int;
  rows : (int * int) list;  (** (offset in .text, location id), in code order *)
}

type section = { sname : string; bytes : Bytes.t; relocs : A.reloc list }

let abbrev_symbol = ".Lfas.debug_abbrev"
let line_symbol = ".Lfas.debug_line"

(* little-endian writers and LEB128 *)
let u8 b x = Buffer.add_uint8 b (x land 0xff)
let u16 b x = Buffer.add_uint16_le b x
let u32 b x = Buffer.add_int32_le b (Int32.of_int x)
let u64 b x = Buffer.add_int64_le b (Int64.of_int x)

let rec uleb b x =
  let byte = x land 0x7f and rest = x lsr 7 in
  if rest = 0 then u8 b byte
  else begin
    u8 b (byte lor 0x80);
    uleb b rest
  end

let rec sleb b x =
  let byte = x land 0x7f and rest = x asr 7 in
  if (rest = 0 && byte land 0x40 = 0) || (rest = -1 && byte land 0x40 <> 0) then u8 b byte
  else begin
    u8 b (byte lor 0x80);
    sleb b rest
  end

let cstr b s =
  if String.contains s '\000' then invalid_arg "Mir_dwarf: NUL in a debug string";
  Buffer.add_string b s;
  u8 b 0

(* the directory the LLVM path also records for a source path *)
let absolute_directory ~cwd path =
  match Filename.dirname path with
  | "." -> cwd
  | dir when Filename.is_relative dir -> Filename.concat cwd dir
  | dir -> dir

(* constants (DWARF 5, section 7) *)
let dw_tag_compile_unit = 0x11
let dw_tag_subprogram = 0x2e
let dw_at_name = 0x03
let dw_at_stmt_list = 0x10
let dw_at_low_pc = 0x11
let dw_at_high_pc = 0x12
let dw_at_language = 0x13
let dw_at_comp_dir = 0x1b
let dw_at_producer = 0x25
let dw_at_decl_file = 0x3a
let dw_at_decl_line = 0x3b
let dw_at_external = 0x3f
let dw_at_frame_base = 0x40
let dw_at_linkage_name = 0x6e
let dw_form_addr = 0x01
let dw_form_data2 = 0x05
let dw_form_data8 = 0x07
let dw_form_string = 0x08
let dw_form_flag = 0x0c
let dw_form_udata = 0x0f
let dw_form_sec_offset = 0x17
let dw_form_exprloc = 0x18
let dw_lang_c11 = 0x1d
let dw_lnct_path = 1
let dw_lnct_directory_index = 2

(* [loc id] gives (file, line, column) of a Fir location id (never 0).
   Returns no sections when no function has a row: nothing to describe. *)
let build ~cwd ~producer ~text_symbol ~text_size ~(loc : int -> string * int * int)
    (funcs : func_lines list) =
  let funcs = List.filter (fun f -> f.rows <> [] && f.size > 0) funcs in
  if funcs = [] then []
  else begin
    (* the primary source file is the one of the lowest location id used
       (front ends number the main file's locations first; a header's
       function may come first in the code); as in the LLVM path the
       compile unit is named by its basename, and its directory (absolute,
       from [cwd] when relative) is the compilation directory, DWARF 5
       directory 0. It is file 0; the others follow in order of first use. *)
    let primary_path =
      let lowest = List.fold_left (fun acc f -> List.fold_left (fun acc (_, l) -> min acc l) acc f.rows) max_int funcs in
      let file, _, _ = loc lowest in
      file
    in
    let comp_dir = absolute_directory ~cwd primary_path in
    let dirs = Hashtbl.create 4 and dir_order = ref [ comp_dir ] in
    Hashtbl.replace dirs comp_dir 0;
    let dir_index d =
      match Hashtbl.find_opt dirs d with
      | Some i -> i
      | None ->
          let i = Hashtbl.length dirs in
          Hashtbl.replace dirs d i;
          dir_order := d :: !dir_order;
          i
    in
    let files = Hashtbl.create 4 and file_order = ref [] in
    let file_index path =
      match Hashtbl.find_opt files path with
      | Some i -> i
      | None ->
          let i = Hashtbl.length files in
          Hashtbl.replace files path i;
          file_order := (Filename.basename path, dir_index (absolute_directory ~cwd path)) :: !file_order;
          i
    in
    ignore (file_index primary_path);
    List.iter (fun f -> List.iter (fun (_, l) -> let file, _, _ = loc l in ignore (file_index file)) f.rows) funcs;
    (* .debug_abbrev *)
    let ab = Buffer.create 64 in
    let abbrev code tag children attrs =
      uleb ab code;
      uleb ab tag;
      u8 ab (if children then 1 else 0);
      List.iter (fun (at, form) -> uleb ab at; uleb ab form) attrs;
      u8 ab 0;
      u8 ab 0
    in
    abbrev 1 dw_tag_compile_unit true
      [ (dw_at_producer, dw_form_string); (dw_at_language, dw_form_data2); (dw_at_name, dw_form_string);
        (dw_at_comp_dir, dw_form_string); (dw_at_stmt_list, dw_form_sec_offset);
        (dw_at_low_pc, dw_form_addr); (dw_at_high_pc, dw_form_data8) ];
    let sub_attrs =
      [ (dw_at_low_pc, dw_form_addr); (dw_at_high_pc, dw_form_data8); (dw_at_frame_base, dw_form_exprloc);
        (dw_at_name, dw_form_string) ]
    and decl = [ (dw_at_decl_file, dw_form_udata); (dw_at_decl_line, dw_form_udata); (dw_at_external, dw_form_flag) ] in
    abbrev 2 dw_tag_subprogram false (sub_attrs @ decl);
    abbrev 3 dw_tag_subprogram false (sub_attrs @ [ (dw_at_linkage_name, dw_form_string) ] @ decl);
    u8 ab 0;
    (* .debug_info *)
    let info = Buffer.create 256 and info_relocs = ref [] in
    let addr b relocs offset =
      relocs := A.Abs64 { offset = Buffer.length b; symbol = text_symbol; addend = offset } :: !relocs;
      u64 b 0
    in
    u32 info 0 (* unit_length, patched *);
    u16 info 5;
    u8 info 1 (* DW_UT_compile *);
    u8 info 8;
    info_relocs := A.Abs32 { offset = Buffer.length info; symbol = abbrev_symbol; addend = 0 } :: !info_relocs;
    u32 info 0;
    uleb info 1;
    cstr info producer;
    u16 info dw_lang_c11;
    cstr info (Filename.basename primary_path);
    cstr info comp_dir;
    info_relocs := A.Abs32 { offset = Buffer.length info; symbol = line_symbol; addend = 0 } :: !info_relocs;
    u32 info 0;
    addr info info_relocs 0;
    u64 info text_size;
    List.iter
      (fun f ->
        let file, line, _ = loc (snd (List.hd f.rows)) in
        let display = Option.value f.display ~default:f.name in
        uleb info (if display = f.name then 2 else 3);
        addr info info_relocs f.start;
        u64 info f.size;
        uleb info 1;
        u8 info 0x56 (* DW_OP_reg6: rbp *);
        cstr info display;
        if display <> f.name then cstr info f.name;
        uleb info (Hashtbl.find files file);
        uleb info line;
        u8 info (if f.external_ then 1 else 0))
      funcs;
    u8 info 0 (* end of the compile unit's children *);
    let info = Buffer.to_bytes info in
    Bytes.set_int32_le info 0 (Int32.of_int (Bytes.length info - 4));
    (* .debug_line *)
    let ln = Buffer.create 256 and line_relocs = ref [] in
    u32 ln 0 (* unit_length, patched *);
    u16 ln 5;
    u8 ln 8 (* address_size *);
    u8 ln 0 (* segment_selector_size *);
    let header_length_at = Buffer.length ln in
    u32 ln 0 (* header_length, patched *);
    let header_start = Buffer.length ln in
    u8 ln 1 (* minimum_instruction_length *);
    u8 ln 1 (* maximum_operations_per_instruction *);
    u8 ln 1 (* default_is_stmt *);
    u8 ln (-5) (* line_base *);
    u8 ln 14 (* line_range *);
    u8 ln 13 (* opcode_base *);
    List.iter (u8 ln) [ 0; 1; 1; 1; 1; 0; 0; 0; 1; 0; 0; 1 ];
    u8 ln 1 (* directory entry format: path as a string *);
    uleb ln dw_lnct_path;
    uleb ln dw_form_string;
    uleb ln (List.length !dir_order);
    List.iter (cstr ln) (List.rev !dir_order);
    u8 ln 2 (* file entry format: path, directory index *);
    uleb ln dw_lnct_path;
    uleb ln dw_form_string;
    uleb ln dw_lnct_directory_index;
    uleb ln dw_form_udata;
    uleb ln (List.length !file_order);
    List.iter (fun (name, d) -> cstr ln name; uleb ln d) (List.rev !file_order);
    let header_length = Buffer.length ln - header_start in
    List.iter
      (fun f ->
        (* registers at the start of a sequence: file 1, line 1, column 0;
           DWARF 5 numbers files from 0, so set the file every time *)
        u8 ln 0;
        uleb ln 9;
        u8 ln 2 (* DW_LNE_set_address *);
        line_relocs := A.Abs64 { offset = Buffer.length ln; symbol = text_symbol; addend = f.start } :: !line_relocs;
        u64 ln 0;
        let cur_file = ref (-1) and cur_line = ref 1 and cur_col = ref 0 and cur_pc = ref f.start in
        let row ?(prologue_end = false) pc l =
          let file, line, col = loc l in
          let fi = Hashtbl.find files file in
          if fi <> !cur_file then begin
            u8 ln 4 (* DW_LNS_set_file *);
            uleb ln fi;
            cur_file := fi
          end;
          if line <> !cur_line then begin
            u8 ln 3 (* DW_LNS_advance_line *);
            sleb ln (line - !cur_line);
            cur_line := line
          end;
          if col <> !cur_col then begin
            u8 ln 5 (* DW_LNS_set_column *);
            uleb ln (max 0 col);
            cur_col := col
          end;
          if pc <> !cur_pc then begin
            u8 ln 2 (* DW_LNS_advance_pc *);
            uleb ln (pc - !cur_pc);
            cur_pc := pc
          end;
          if prologue_end then u8 ln 10 (* DW_LNS_set_prologue_end *);
          u8 ln 1 (* DW_LNS_copy *)
        in
        let first = snd (List.hd f.rows) in
        if fst (List.hd f.rows) > f.start then row f.start first;
        List.iteri (fun i (pc, l) -> row ~prologue_end:(i = 0) pc l) f.rows;
        u8 ln 2;
        uleb ln (f.start + f.size - !cur_pc);
        u8 ln 0;
        uleb ln 1;
        u8 ln 1 (* DW_LNE_end_sequence *))
      funcs;
    let line = Buffer.to_bytes ln in
    Bytes.set_int32_le line 0 (Int32.of_int (Bytes.length line - 4));
    Bytes.set_int32_le line header_length_at (Int32.of_int header_length);
    [ { sname = ".debug_abbrev"; bytes = Buffer.to_bytes ab; relocs = [] };
      { sname = ".debug_info"; bytes = info; relocs = List.rev !info_relocs };
      { sname = ".debug_line"; bytes = line; relocs = List.rev !line_relocs } ]
  end
