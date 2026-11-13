(* DWARF 5 debug info for the native backend (-g): line tables, functions,
   source variables with their types and lexical blocks (roadmap v0.8; see
   README "Debug info" for where variable locations come from).

   Non-allocated sections:

   .debug_abbrev  the abbreviations the DIEs below use, made on demand.
   .debug_info    one compile unit covering all of .text: the module's
                  types (Fir dtype), then one DW_TAG_subprogram per
                  function (name, linkage name when the source name
                  differs, declaration line, address range, frame base
                  rbp) holding its parameters, variables and lexical
                  blocks.
   .debug_loclists  location lists of variables in registers (only when
                  some variable has one).
   .debug_rnglists  ranges of lexical blocks in several pieces (likewise).
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
  vars : Fir.dvar list;  (** the function's source variables *)
  vlocs : (int * vloc) list;  (** index in [vars] -> where it lives; absent: optimized out *)
}

(* where a variable lives: one DWARF expression for the whole function, or
   expressions over text ranges [start, end) (a location list) *)
and vloc = Expr of string | Ranges of (int * int * string) list

(* [ssym]: a local symbol at the section's start, for section offsets *)
type section = { sname : string; ssym : string option; bytes : Bytes.t; relocs : A.reloc list }

let abbrev_symbol = ".Lfas.debug_abbrev"
let line_symbol = ".Lfas.debug_line"
let loclists_symbol = ".Lfas.debug_loclists"
let rnglists_symbol = ".Lfas.debug_rnglists"

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

(* DWARF expressions; registers are DWARF numbers (Mir_cfi.dwarf_reg,
   xmm n = 17 + n) *)
let expr f =
  let b = Buffer.create 8 in
  f b;
  Buffer.contents b

let op_fbreg off = expr (fun b -> u8 b 0x91; sleb b off)
let op_reg r = expr (fun b -> if r < 32 then u8 b (0x50 + r) else (u8 b 0x90; uleb b r))
let op_breg r off = expr (fun b -> if r < 32 then u8 b (0x70 + r) else (u8 b 0x92; uleb b r); sleb b off)
let op_deref = "\x06"

(* the directory the LLVM path also records for a source path *)
let absolute_directory ~cwd path =
  match Filename.dirname path with
  | "." -> cwd
  | dir when Filename.is_relative dir -> Filename.concat cwd dir
  | dir -> dir

(* constants (DWARF 5, section 7) *)
let dw_tag_array_type = 0x01
let dw_tag_enumeration_type = 0x04
let dw_tag_formal_parameter = 0x05
let dw_tag_lexical_block = 0x0b
let dw_tag_member = 0x0d
let dw_tag_pointer_type = 0x0f
let dw_tag_structure_type = 0x13
let dw_tag_typedef = 0x16
let dw_tag_union_type = 0x17
let dw_tag_subrange_type = 0x21
let dw_tag_base_type = 0x24
let dw_tag_enumerator = 0x28
let dw_tag_variable = 0x34
let dw_at_location = 0x02
let dw_at_byte_size = 0x0b
let dw_at_const_value = 0x1c
let dw_at_count = 0x37
let dw_at_data_member_location = 0x38
let dw_at_declaration = 0x3c
let dw_at_encoding = 0x3e
let dw_at_type = 0x49
let dw_at_ranges = 0x55
let dw_at_gnu_vector = 0x2107
let dw_form_data1 = 0x0b
let dw_form_sdata = 0x0d
let dw_form_ref4 = 0x13
let dw_form_flag_present = 0x19
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
    ~(dtypes : Fir.dtype array) ~(scopes : int array) ~(loc_scope : int -> int) (funcs : func_lines list) =
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
    (* .debug_abbrev: abbreviations are made as DIEs need them *)
    let abbrevs = Hashtbl.create 16 and abbrev_order = ref [] in
    let abbrev tag children attrs =
      let key = (tag, children, attrs) in
      match Hashtbl.find_opt abbrevs key with
      | Some c -> c
      | None ->
          let c = Hashtbl.length abbrevs + 1 in
          Hashtbl.add abbrevs key c;
          abbrev_order := (c, key) :: !abbrev_order;
          c
    in
    (* .debug_info *)
    let info = Buffer.create 256 and info_relocs = ref [] in
    let addr b relocs offset =
      relocs := A.Abs64 { offset = Buffer.length b; symbol = text_symbol; addend = offset } :: !relocs;
      u64 b 0
    in
    (* a DIE: its abbreviation, then each attribute's value *)
    let die tag children (attrs : (int * int * (unit -> unit)) list) =
      uleb info (abbrev tag children (List.map (fun (a, f, _) -> (a, f)) attrs));
      List.iter (fun (_, _, w) -> w ()) attrs
    in
    let str a s = (a, dw_form_string, fun () -> cstr info s) in
    let udata a n = (a, dw_form_udata, fun () -> uleb info n) in
    let flag_present a = (a, dw_form_flag_present, fun () -> ()) in
    (* type references are offsets in the unit, patched once every type is out *)
    let type_off = Array.make (Array.length dtypes) 0 and type_refs = ref [] in
    let tref t =
      ( dw_at_type, dw_form_ref4,
        fun () ->
          type_refs := (Buffer.length info, t) :: !type_refs;
          u32 info 0 )
    in
    let typed t = if t = 0 then [] else [ tref t ] in
    (* lists: .debug_loclists and .debug_rnglists, entries after a header *)
    let list_section () =
      let b = Buffer.create 64 in
      u32 b 0 (* unit_length, patched *);
      u16 b 5;
      u8 b 8;
      u8 b 0;
      u32 b 0 (* offset_entry_count *);
      (b, ref [])
    in
    let loclists, loc_relocs = list_section () and rnglists, rng_relocs = list_section () in
    let used_loclists = ref false and used_rnglists = ref false in
    let sec_offset a sym (b : Buffer.t) used =
      ( a, dw_form_sec_offset,
        fun () ->
          used := true;
          info_relocs := A.Abs32 { offset = Buffer.length info; symbol = sym; addend = Buffer.length b } :: !info_relocs;
          u32 info 0 )
    in
    u32 info 0 (* unit_length, patched *);
    u16 info 5;
    u8 info 1 (* DW_UT_compile *);
    u8 info 8;
    info_relocs := A.Abs32 { offset = Buffer.length info; symbol = abbrev_symbol; addend = 0 } :: !info_relocs;
    u32 info 0;
    die dw_tag_compile_unit true
      [ str dw_at_producer producer;
        (dw_at_language, dw_form_data2, fun () -> u16 info dw_lang_c11);
        str dw_at_name (Filename.basename primary_path);
        str dw_at_comp_dir comp_dir;
        ( dw_at_stmt_list, dw_form_sec_offset,
          fun () ->
            info_relocs := A.Abs32 { offset = Buffer.length info; symbol = line_symbol; addend = 0 } :: !info_relocs;
            u32 info 0 );
        (dw_at_low_pc, dw_form_addr, fun () -> addr info info_relocs 0);
        (dw_at_high_pc, dw_form_data8, fun () -> u64 info text_size) ];
    (* the types (Fir dtype i is DIE type_off.(i)); a byte size where DWARF
       wants one, from the layout (typedef and enum chains are bounded) *)
    let rec size_of depth t =
      if depth > 64 || t <= 0 || t >= Array.length dtypes then 0
      else
        match dtypes.(t) with
        | Fir.Dbase { size; _ } -> size
        | Dpointer _ -> 8
        | Dstruct { size; _ } -> Option.value size ~default:0
        | Darray (e, n) | Dvector (e, n) -> n * size_of (depth + 1) e
        | Dtypedef (_, t) | Denum { base = t; _ } -> size_of (depth + 1) t
        | Dvoid -> 0
    in
    Array.iteri
      (fun i t ->
        if i > 0 then begin
          type_off.(i) <- Buffer.length info;
          match t with
          | Fir.Dvoid -> ()
          | Dbase { name; enc; size } ->
              let enc =
                match enc with
                | Fir.Dboolean -> 2 | Dfloat -> 4 | Dsigned -> 5 | Dsigned_char -> 6 | Dunsigned -> 7
                | Dunsigned_char -> 8
              in
              die dw_tag_base_type false
                [ str dw_at_name name; (dw_at_encoding, dw_form_data1, fun () -> u8 info enc); udata dw_at_byte_size size ]
          | Dpointer t -> die dw_tag_pointer_type false (udata dw_at_byte_size 8 :: typed t)
          | Dstruct { name; union; size = None; _ } ->
              die (if union then dw_tag_union_type else dw_tag_structure_type) false
                [ str dw_at_name name; flag_present dw_at_declaration ]
          | Dstruct { name; union; size = Some n; members } ->
              die (if union then dw_tag_union_type else dw_tag_structure_type) (members <> [])
                [ str dw_at_name name; udata dw_at_byte_size n ];
              if members <> [] then begin
                List.iter
                  (fun (m, t, o) ->
                    die dw_tag_member false ([ str dw_at_name m ] @ typed t @ [ udata dw_at_data_member_location o ]))
                  members;
                u8 info 0
              end
          | Darray (e, n) | Dvector (e, n) ->
              let vector = match t with Dvector _ -> [ flag_present dw_at_gnu_vector ] | _ -> [] in
              die dw_tag_array_type true (typed e @ vector);
              die dw_tag_subrange_type false [ udata dw_at_count n ];
              u8 info 0
          | Dtypedef (name, t) -> die dw_tag_typedef false (str dw_at_name name :: typed t)
          | Denum { name; base; enumerators } ->
              die dw_tag_enumeration_type (enumerators <> [])
                ([ str dw_at_name name ] @ typed base @ [ udata dw_at_byte_size (size_of 0 base) ]);
              if enumerators <> [] then begin
                List.iter
                  (fun (e, v) ->
                    die dw_tag_enumerator false
                      [ str dw_at_name e; (dw_at_const_value, dw_form_sdata, fun () -> sleb info (Int64.to_int v)) ])
                  enumerators;
                u8 info 0
              end
        end)
      dtypes;
    (* one variable *)
    let variable f i (d : Fir.dvar) =
      let decl =
        if d.vloc = 0 then []
        else
          let file, line, _ = loc d.vloc in
          if file = "" then [] else [ udata dw_at_decl_file (file_index file); udata dw_at_decl_line line ]
      in
      let location =
        match List.assoc_opt i f.vlocs with
        | None -> []
        | Some (Expr e) -> [ (dw_at_location, dw_form_exprloc, fun () -> uleb info (String.length e); Buffer.add_string info e) ]
        | Some (Ranges rs) ->
            [ sec_offset dw_at_location loclists_symbol loclists used_loclists;
              (* written when the attribute is, so the offset above is the entries' *)
              (0, 0, fun () ->
                List.iter
                  (fun (s, e, x) ->
                    u8 loclists 0x08 (* DW_LLE_start_length *);
                    loc_relocs := A.Abs64 { offset = Buffer.length loclists; symbol = text_symbol; addend = s } :: !loc_relocs;
                    u64 loclists 0;
                    uleb loclists (e - s);
                    uleb loclists (String.length x);
                    Buffer.add_string loclists x)
                  rs;
                u8 loclists 0 (* DW_LLE_end_of_list *)) ]
      in
      let attrs = [ str dw_at_name d.vname ] @ typed d.vty @ decl @ location in
      let tag = if d.varg > 0 then dw_tag_formal_parameter else dw_tag_variable in
      (* the pseudo attribute (0, 0) only writes the list *)
      uleb info (abbrev tag false (List.filter_map (fun (a, f, _) -> if a = 0 then None else Some (a, f)) attrs));
      List.iter (fun (_, _, w) -> w ()) attrs
    in
    List.iter
      (fun f ->
        let file, line, _ = loc (snd (List.hd f.rows)) in
        let display = Option.value f.display ~default:f.name in
        (* lexical blocks: each row's code, up to the next row, belongs to
           its location's scope and every scope around it *)
        let ranges = Hashtbl.create 8 in
        let rec segs = function
          | [] -> []
          | [ (o, l) ] -> [ (o, f.start + f.size, l) ]
          | (o, l) :: ((o', _) :: _ as rest) -> (o, o', l) :: segs rest
        in
        List.iter
          (fun (s, e, l) ->
            let rec up sc =
              if sc > 0 && sc < Array.length scopes then begin
                Hashtbl.replace ranges sc ((s, e) :: Option.value (Hashtbl.find_opt ranges sc) ~default:[]);
                up scopes.(sc)
              end
            in
            if e > s then up (loc_scope l))
          (segs f.rows);
        (* a scope without code is dropped: its variables go to the nearest
           enclosing scope with code (0: the function) *)
        let rec live sc = if sc <= 0 || sc >= Array.length scopes then 0 else if Hashtbl.mem ranges sc then sc else live scopes.(sc) in
        let vars = List.mapi (fun i d -> (i, d)) f.vars in
        let in_scope sc = List.filter (fun (_, (d : Fir.dvar)) -> live d.vscope = sc) vars in
        let params, locals =
          List.partition (fun (_, (d : Fir.dvar)) -> d.varg > 0) (in_scope 0)
        in
        let params = List.stable_sort (fun (_, (a : Fir.dvar)) (_, (b : Fir.dvar)) -> compare a.varg b.varg) params in
        (* a scope with code has code in every scope around it *)
        let children sc = List.sort compare (Hashtbl.fold (fun c _ acc -> if scopes.(c) = sc then c :: acc else acc) ranges []) in
        let has_children = f.vars <> [] || Hashtbl.length ranges > 0 in
        die dw_tag_subprogram has_children
          ([ (dw_at_low_pc, dw_form_addr, fun () -> addr info info_relocs f.start);
             (dw_at_high_pc, dw_form_data8, fun () -> u64 info f.size);
             (dw_at_frame_base, dw_form_exprloc, fun () -> uleb info 1; u8 info 0x56 (* DW_OP_reg6: rbp *));
             str dw_at_name display ]
          @ (if display <> f.name then [ str dw_at_linkage_name f.name ] else [])
          @ [ udata dw_at_decl_file (Hashtbl.find files file); udata dw_at_decl_line line;
              (dw_at_external, dw_form_flag, fun () -> u8 info (if f.external_ then 1 else 0)) ]);
        if has_children then begin
          List.iter (fun (i, d) -> variable f i d) (params @ locals);
          let rec block sc =
            let rs =
              List.sort_uniq compare (Hashtbl.find ranges sc)
              |> List.fold_left (fun acc (s, e) -> match acc with (s', e') :: rest when s <= e' -> (s', max e e') :: rest | _ -> (s, e) :: acc) []
              |> List.rev
            in
            let where =
              match rs with
              | [ (s, e) ] ->
                  [ (dw_at_low_pc, dw_form_addr, fun () -> addr info info_relocs s);
                    (dw_at_high_pc, dw_form_data8, fun () -> u64 info (e - s)) ]
              | _ ->
                  [ sec_offset dw_at_ranges rnglists_symbol rnglists used_rnglists;
                    (0, 0, fun () ->
                      List.iter
                        (fun (s, e) ->
                          u8 rnglists 0x07 (* DW_RLE_start_length *);
                          rng_relocs := A.Abs64 { offset = Buffer.length rnglists; symbol = text_symbol; addend = s } :: !rng_relocs;
                          u64 rnglists 0;
                          uleb rnglists (e - s))
                        rs;
                      u8 rnglists 0 (* DW_RLE_end_of_list *)) ]
            in
            uleb info (abbrev dw_tag_lexical_block true (List.filter_map (fun (a, f, _) -> if a = 0 then None else Some (a, f)) where));
            List.iter (fun (_, _, w) -> w ()) where;
            List.iter (fun (i, d) -> variable f i d) (in_scope sc);
            List.iter block (children sc);
            u8 info 0
          in
          List.iter block (children 0);
          u8 info 0
        end)
      funcs;
    u8 info 0 (* end of the compile unit's children *);
    let info = Buffer.to_bytes info in
    Bytes.set_int32_le info 0 (Int32.of_int (Bytes.length info - 4));
    List.iter (fun (o, t) -> Bytes.set_int32_le info o (Int32.of_int type_off.(t))) !type_refs;
    let ab = Buffer.create 64 in
    List.iter
      (fun (c, (tag, children, attrs)) ->
        uleb ab c;
        uleb ab tag;
        u8 ab (if children then 1 else 0);
        List.iter (fun (at, form) -> uleb ab at; uleb ab form) attrs;
        u8 ab 0;
        u8 ab 0)
      (List.rev !abbrev_order);
    u8 ab 0;
    let list_bytes b = let by = Buffer.to_bytes b in Bytes.set_int32_le by 0 (Int32.of_int (Bytes.length by - 4)); by in
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
    [ { sname = ".debug_abbrev"; ssym = Some abbrev_symbol; bytes = Buffer.to_bytes ab; relocs = [] };
      { sname = ".debug_info"; ssym = None; bytes = info; relocs = List.rev !info_relocs };
      { sname = ".debug_line"; ssym = Some line_symbol; bytes = line; relocs = List.rev !line_relocs } ]
    @ (if !used_loclists then
         [ { sname = ".debug_loclists"; ssym = Some loclists_symbol; bytes = list_bytes loclists; relocs = List.rev !loc_relocs } ]
       else [])
    @
    if !used_rnglists then
      [ { sname = ".debug_rnglists"; ssym = Some rnglists_symbol; bytes = list_bytes rnglists; relocs = List.rev !rng_relocs } ]
    else []
  end
