module Hashtbl = C_import_json.Hashtbl

type int_kind = U8 | U16 | U32 | I8 | I16 | I32 | I64 | U64 | Usize | Isize
type float_kind = F32 | F64

type ty =
  | Bool
  | Int of int_kind
  | Float of float_kind
  | Addr
  | Handle of string
  | Array of int * ty
  | Vec of int * ty
  | Struct of string
  | Opaque of string
  | Void

type field = {
  name : string;
  ty : ty;
  offset : int;
  unsupported_reason : string option;
}

type struct_def = {
  name : string;
  fields : field list;
  size : int;
  align : int;
  is_union : bool;
  byte_storage : bool;
}

type const_def = { name : string; ty : ty; bits : int64 }
type const_arr_def = { name : string; ty : ty; elems : int64 list }
type zero_initializer = { zero_ty : ty }

let zero_initializer zero_ty = { zero_ty }

let zero_integer_array_values { zero_ty } =
  match zero_ty with
  | Array (length, (Bool | Int _ | Float _)) -> Some (List.init length (fun _ -> 0L))
  | _ -> None

type func_sig = { params : (string * ty) list; ret : ty; variadic : bool }
type local = { name : string; ty : ty; id : int }
type linkage = Internal | External_c
type simd_load_kind = Masked | Gather | Gather_bytes
type simd_store_kind = Masked_store | Scatter | Scatter_bytes

type float_builtin =
  | Float_sqrt
  | Float_fma
  | Float_floor
  | Float_ceil
  | Float_round
  | Float_abs

type builtin =
  | Rotl
  | Rotr
  | Popcount
  | Ctz
  | Clz
  | Add_sat
  | Sub_sat
  | Mul_hi
  | Any
  | All
  | Select
  | Shuffle
  | Permute
  | Reduce_sum
  | Reduce_min
  | Reduce_max
  | Reduce_and
  | Reduce_or
  | Reduce_xor
  | Compress
  | Expand
  | Addr_bits
  | Addr_from_bits
  | Cvt of ty
  | Float_math of float_builtin
  | Handle_addr
  | Handle_from_addr of string
  | Volatile_load of ty
  | Simd_load of ty * simd_load_kind

type call_target = User of string | Builtin of builtin

type expr =
  | EInt of int64 * ty * Span.t
  | EFloat of int64 * ty * Span.t
  | EBool of bool * Span.t
  | EVector of int64 list * ty * Span.t
  | Vector_lit of expr list * ty * Span.t
  | Null of ty * Span.t
  | EString of int * bool * Span.t
  | Local of local * Span.t
  | Global of string * ty * Span.t
  | Unary of Ast.unop * expr * ty * Span.t
  | Binary of Ast.binop * expr * expr * ty * Span.t
  | Call of call_target * expr list * ty * Span.t
  | Indirect_call of expr * expr list * ty * Span.t
  | Cast of Ast.cast_kind * expr * ty * Span.t
  | Index of expr * expr * ty * Span.t
  | Field of expr * string * ty * int * Span.t
  | Raw_select of expr * expr * ty * Span.t
  | Address of expr * ty * Span.t
  | Function_address of string * Span.t
  | Sizeof of ty * int * Span.t
  | Alignof of ty * int * Span.t
  | Offsetof of ty * string * int * Span.t
  | Splat of expr * ty * Span.t
  | Ternary of expr * expr * expr * ty * Span.t
  | Const_array of string * ty * Span.t

type construction =
  | Init_value of expr
  | Init_zero of zero_initializer * bool * Span.t
  | Init_omitted of zero_initializer * Span.t
  | Init_aggregate of ty * bool * construction list * Span.t

type assign_target =
  | ALocal of local
  | AGlobal of string * ty
  | ARaw of expr * expr * ty
  | AIndex of expr * expr
  | AField of expr * string * int

type stmt =
  | Let of local * expr option * Span.t
  | Let_construct of local * construction * Span.t
  | View of local * expr * Span.t
  | Copy of expr * expr * ty * bool * Span.t
  | Volatile_store of ty * expr * expr * Span.t
  | Simd_store of ty * simd_store_kind * expr list * Span.t
  | Assign of assign_target * expr * Span.t
  | Compound_assign of assign_target * Ast.binop * expr * ty * Span.t
  | Return of expr option * Span.t
  | If of expr * stmt list * stmt list option * Span.t
  | While of string option * expr * stmt list * Span.t
  | For of string option * stmt option * expr option * stmt option * stmt list * Span.t
  | Switch of expr * (expr list * stmt list) list * stmt list option * Span.t
  | Break of string option * Span.t
  | Continue of string option * Span.t
  | Defer of stmt list * Span.t
  | Expr of expr * Span.t
  | Block of stmt list * Span.t

type func_body = Declaration | Statements of stmt list

type func = {
  name : string;
  params : local list;
  ret : ty;
  body : func_body;
  linkage : linkage;
  variadic : bool;
  span : Span.t;
  display : string option;
}

type global_initializer =
  | Global_zero of zero_initializer
  | Global_int of int64
  | Global_float of int64
  | Global_bool of bool
  | Global_null
  | Global_address of string * int
  | Global_vector of int64 list
  | Global_array of global_initializer list
  | Global_struct of global_initializer list

type global = {
  name : string;
  ty : ty;
  init_value : global_initializer option;
  readonly : bool;
  linkage : Ast.global_linkage;
}

type program = {
  structs : struct_def list;
  consts : const_def list;
  const_arrays : const_arr_def list;
  globals : global list;
  funcs : func list;
  strings : string list;
}

let rec ty_equal a b =
  match (a, b) with
  | Bool, Bool | Void, Void -> true
  | Int a, Int b -> a = b
  | Float a, Float b -> a = b
  | Addr, Addr -> true
  | Handle a, Handle b -> a = b
  | Array (na, a), Array (nb, b) | Vec (na, a), Vec (nb, b) -> na = nb && ty_equal a b
  | Struct a, Struct b | Opaque a, Opaque b -> a = b
  | _ -> false

let rec ty_name = function
  | Bool -> "bool"
  | Void -> "void"
  | Int U8 -> "u8"
  | Int U16 -> "u16"
  | Int U32 -> "u32"
  | Int U64 -> "u64"
  | Int I8 -> "i8"
  | Int I16 -> "i16"
  | Int I32 -> "i32"
  | Int I64 -> "i64"
  | Int Usize -> "usize"
  | Int Isize -> "isize"
  | Float F32 -> "f32"
  | Float F64 -> "f64"
  | Addr -> "addr"
  | Handle name -> "handle[" ^ name ^ "]"
  | Array (n, t) -> Printf.sprintf "arr[%d, %s]" n (ty_name t)
  | Vec (n, t) -> Printf.sprintf "vec[%d, %s]" n (ty_name t)
  | Struct n | Opaque n -> n

let expr_ty = function
  | EInt (_, t, _)
  | EFloat (_, t, _)
  | EVector (_, t, _)
  | Vector_lit (_, t, _)
  | Unary (_, _, t, _)
  | Binary (_, _, _, t, _)
  | Call (_, _, t, _)
  | Indirect_call (_, _, t, _)
  | Cast (_, _, t, _)
  | Index (_, _, t, _)
  | Field (_, _, t, _, _)
  | Raw_select (_, _, t, _)
  | Address (_, t, _)
  | Splat (_, t, _)
  | Ternary (_, _, _, t, _)
  | Const_array (_, t, _) ->
      t
  | Local (local, _) -> local.ty
  | Global (_, ty, _) -> ty
  | Function_address _ -> Addr
  | EBool _ -> Bool
  | Null (t, _) -> t
  | EString _ -> Addr
  | Sizeof _ | Alignof _ | Offsetof _ -> Int Usize

let expr_span = function
  | EInt (_, _, s)
  | EFloat (_, _, s)
  | EVector (_, _, s)
  | Vector_lit (_, _, s)
  | EBool (_, s)
  | Null (_, s)
  | EString (_, _, s)
  | Local (_, s)
  | Global (_, _, s)
  | Unary (_, _, _, s)
  | Binary (_, _, _, _, s)
  | Call (_, _, _, s)
  | Indirect_call (_, _, _, s)
  | Cast (_, _, _, s)
  | Index (_, _, _, s)
  | Field (_, _, _, _, s)
  | Raw_select (_, _, _, s)
  | Address (_, _, s)
  | Function_address (_, s)
  | Sizeof (_, _, s)
  | Alignof (_, _, s)
  | Offsetof (_, _, _, s)
  | Splat (_, _, s)
  | Ternary (_, _, _, _, s)
  | Const_array (_, _, s) ->
      s

let stmt_span = function
  | Let (_, _, s)
  | Let_construct (_, _, s)
  | View (_, _, s)
  | Copy (_, _, _, _, s)
  | Volatile_store (_, _, _, s)
  | Simd_store (_, _, _, s)
  | Assign (_, _, s)
  | Compound_assign (_, _, _, _, s)
  | Return (_, s)
  | If (_, _, _, s)
  | While (_, _, _, s)
  | For (_, _, _, _, _, s)
  | Switch (_, _, _, s)
  | Break (_, s)
  | Continue (_, s)
  | Defer (_, s)
  | Expr (_, s)
  | Block (_, s) ->
      s

type flow_summary = {
  falls_through : bool;
  returns : bool;
  breaks : bool;
  labeled_breaks : string list;
  continues : bool;
  labeled_continues : string list;
}

let flowing =
  {
    falls_through = true;
    returns = false;
    breaks = false;
    labeled_breaks = [];
    continues = false;
    labeled_continues = [];
  }

let union left right = List.sort_uniq String.compare (left @ right)
let remove name names = List.filter (fun current -> current <> name) names

let choose_flow left right =
  {
    falls_through = left.falls_through || right.falls_through;
    returns = left.returns || right.returns;
    breaks = left.breaks || right.breaks;
    labeled_breaks = union left.labeled_breaks right.labeled_breaks;
    continues = left.continues || right.continues;
    labeled_continues = union left.labeled_continues right.labeled_continues;
  }

let sequence_flow left right =
  {
    falls_through = left.falls_through && right.falls_through;
    returns = left.returns || (left.falls_through && right.returns);
    breaks = left.breaks || (left.falls_through && right.breaks);
    labeled_breaks =
      union left.labeled_breaks
        (if left.falls_through then right.labeled_breaks else []);
    continues = left.continues || (left.falls_through && right.continues);
    labeled_continues =
      union left.labeled_continues
        (if left.falls_through then right.labeled_continues else []);
  }

let cleanup_flow cleanup exits =
  {
    falls_through = cleanup.falls_through && exits.falls_through;
    returns = cleanup.falls_through && exits.returns;
    breaks = cleanup.falls_through && exits.breaks;
    labeled_breaks = (if cleanup.falls_through then exits.labeled_breaks else []);
    continues = cleanup.falls_through && exits.continues;
    labeled_continues = (if cleanup.falls_through then exits.labeled_continues else []);
  }

let condition_is_true = function EBool (true, _) -> true | _ -> false

let rec stmt_flow = function
  | Return _ -> { flowing with falls_through = false; returns = true }
  | Break (None, _) -> { flowing with falls_through = false; breaks = true }
  | Break (Some label, _) ->
      { flowing with falls_through = false; labeled_breaks = [ label ] }
  | Continue (None, _) -> { flowing with falls_through = false; continues = true }
  | Continue (Some label, _) ->
      { flowing with falls_through = false; labeled_continues = [ label ] }
  | Block (body, _) -> block_flow body
  | If (_, then_body, else_body, _) ->
      choose_flow (block_flow then_body)
        (match else_body with None -> flowing | Some body -> block_flow body)
  | Switch (_, arms, default, _) ->
      let branches = List.map (fun (_, body) -> block_flow body) arms in
      let branches =
        match default with
        | None -> flowing :: branches
        | Some body -> block_flow body :: branches
      in
      List.fold_left choose_flow { flowing with falls_through = false } branches
  | While (label, condition, body, _) ->
      loop_flow label (condition_is_true condition) (block_flow body)
  | For (label, init, condition, step, body, _) ->
      let prefix =
        match init with None -> flowing | Some statement -> stmt_flow statement
      in
      let iteration =
        sequence_flow (block_flow body)
          (match step with None -> flowing | Some statement -> stmt_flow statement)
      in
      let unconditional =
        match condition with
        | None -> true
        | Some expression -> condition_is_true expression
      in
      sequence_flow prefix (loop_flow label unconditional iteration)
  | Defer (body, _) -> cleanup_flow (block_flow body) flowing
  | Let _ | Let_construct _ | View _ | Copy _ | Volatile_store _ | Simd_store _
  | Assign _ | Compound_assign _ | Expr _ ->
      flowing

and block_flow = function
  | [] -> flowing
  | Defer (body, _) :: rest -> cleanup_flow (block_flow body) (block_flow rest)
  | statement :: rest -> sequence_flow (stmt_flow statement) (block_flow rest)

and loop_flow label unconditional body =
  let breaks =
    body.breaks
    || Option.fold ~none:false
         ~some:(fun name -> List.mem name body.labeled_breaks)
         label
  in
  let own name labels =
    Option.fold ~none:labels ~some:(fun label -> remove label labels) name
  in
  {
    falls_through = (not unconditional) || breaks;
    returns = body.returns;
    breaks = false;
    labeled_breaks = own label body.labeled_breaks;
    continues = false;
    labeled_continues = own label body.labeled_continues;
  }

let loop_body_has_break label body =
  let flow = block_flow body in
  flow.breaks
  || Option.fold ~none:false ~some:(fun name -> List.mem name flow.labeled_breaks) label

let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let add_size left right =
  if left < 0 || right < 0 || left > max_int - right then
    Error "aggregate size overflows"
  else Ok (left + right)

let int_bytes ?(target = Target_layout.current ()) = function
  | U8 | I8 -> 1
  | U16 | I16 -> 2
  | U32 | I32 -> 4
  | U64 | I64 -> 8
  | Usize | Isize -> target.Target_layout.pointer_size

let int_layout target k = Target_layout.integer target (int_bytes ~target k * 8)

let scalar_bits target = function
  | Bool -> Ok 1
  | Int k -> Ok (int_bytes ~target k * 8)
  | Float F32 -> Ok 32
  | Float F64 -> Ok 64
  | Addr | Handle _ -> Ok (target.Target_layout.pointer_size * 8)
  | _ -> Error "vector element type must be `bool` or an integer type"

let layout ?(target = Target_layout.current ()) structs ty =
  let rec go visiting = function
    | Bool -> Target_layout.integer target 1
    | Int k -> int_layout target k
    | Float F32 -> Target_layout.float target 32
    | Float F64 -> Target_layout.float target 64
    | Addr | Handle _ -> Target_layout.pointer target
    | Array (n, t) ->
        let* s, a = go visiting t in
        let* size = Target_layout.multiply_size n s in
        Ok (size, a)
    | Vec (n, t) ->
        let* bits = scalar_bits target t in
        Target_layout.vector target n bits
    | Void -> Error "void has no object layout"
    | Opaque n -> Error ("opaque type `" ^ n ^ "` has no layout")
    | Struct n -> (
        if List.mem n visiting then
          Error (Printf.sprintf "recursive by-value struct `%s`" n)
        else
          match List.find_opt (fun (s : struct_def) -> s.name = n) structs with
          | None -> Error (Printf.sprintf "unknown struct `%s`" n)
          | Some (s : struct_def) -> Ok (s.size, s.align))
  in
  go [] ty

type struct_layout_cache = {
  target : Target_layout.t;
  decls : (string * (string * ty) list * int option) list;
  decl_index : ((string * ty) list * int option) Hashtbl.t;
  unions : string list;
  union_index : unit Hashtbl.t;
  field_offsets : (string * (string * int) list) list;
  field_offsets_index : int Hashtbl.t Hashtbl.t;
  field_reasons : (string * (string * string) list) list;
  field_reasons_index : string Hashtbl.t Hashtbl.t;
  byte_storage : string list;
  byte_storage_index : unit Hashtbl.t;
  struct_sizes : (string * int) list;
  struct_sizes_index : int Hashtbl.t;
  definitions : struct_def Hashtbl.t;
}

let first_index entries = Hashtbl.of_seq (List.to_seq (List.rev entries))

let nested_index entries =
  first_index (List.map (fun (key, value) -> (key, first_index value)) entries)

let struct_layout_cache ?(target = Target_layout.current ()) ?(unions = [])
    ?(field_offsets = []) ?(field_reasons = []) ?(byte_storage = [])
    ?(struct_sizes = []) decls =
  let declarations =
    List.map (fun (name, fields, align) -> (name, (fields, align))) decls
  in
  let union_index = first_index (List.map (fun name -> (name, ())) unions) in
  let byte_storage_index =
    first_index (List.map (fun name -> (name, ())) byte_storage)
  in
  {
    target;
    decls;
    decl_index = first_index declarations;
    unions;
    union_index;
    field_offsets;
    field_offsets_index = nested_index field_offsets;
    field_reasons;
    field_reasons_index = nested_index field_reasons;
    byte_storage;
    byte_storage_index;
    struct_sizes;
    struct_sizes_index = first_index struct_sizes;
    definitions = Hashtbl.create (List.length decls);
  }

let field_offset cache structure field =
  Option.bind (Hashtbl.find_opt cache.field_offsets_index structure) (fun fields ->
      Hashtbl.find_opt fields field)

let field_reason cache structure field =
  Option.bind (Hashtbl.find_opt cache.field_reasons_index structure) (fun fields ->
      Hashtbl.find_opt fields field)

let compute_struct_cached ?overflow_field cache name =
  let target = cache.target in
  let mark_overflow struct_name field_name = function
    | Error "aggregate size overflows" as failure ->
        Option.iter
          (fun overflow_field ->
            if Option.is_none !overflow_field then
              overflow_field := Some (struct_name, field_name))
          overflow_field;
        failure
    | result -> result
  in
  let rec calc visiting n =
    match Hashtbl.find_opt cache.definitions n with
    | Some definition -> Ok (definition.fields, definition.size, definition.align)
    | None -> (
        if List.mem n visiting then
          Error (Printf.sprintf "recursive by-value struct `%s`" n)
        else
          match Hashtbl.find_opt cache.decl_index n with
          | None -> Error (Printf.sprintf "unknown struct `%s`" n)
          | Some (fields, explicit) ->
              let rec each off maxa out = function
                | [] ->
                    let byte_storage = Hashtbl.mem cache.byte_storage_index n in
                    let align =
                      if byte_storage then Option.value ~default:1 explicit
                      else max maxa (Option.value ~default:1 explicit)
                    in
                    let last_field =
                      match out with (field : field) :: _ -> field.name | [] -> n
                    in
                    let* size =
                      (match Hashtbl.find_opt cache.struct_sizes_index n with
                        | Some size -> Ok size
                        | None -> Target_layout.round_up_size off align)
                      |> mark_overflow n last_field
                    in
                    let definition =
                      {
                        name = n;
                        fields = List.rev out;
                        size;
                        align;
                        is_union = false;
                        byte_storage = Hashtbl.mem cache.byte_storage_index n;
                      }
                    in
                    Hashtbl.replace cache.definitions n definition;
                    Ok (definition.fields, definition.size, definition.align)
                | (fname, fty) :: rest ->
                    let* size, align =
                      match fty with
                      | Struct sn ->
                          let* _, sz, al = calc (n :: visiting) sn in
                          Ok (sz, al)
                      | _ -> field_layout (n :: visiting) fty |> mark_overflow n fname
                    in
                    let* natural_next =
                      Target_layout.round_up_size off align |> mark_overflow n fname
                    in
                    let next =
                      Option.value ~default:natural_next (field_offset cache n fname)
                    in
                    let* field_end = add_size next size |> mark_overflow n fname in
                    each (max off field_end) (max maxa align)
                      ({
                         name = fname;
                         ty = fty;
                         offset = next;
                         unsupported_reason = field_reason cache n fname;
                       }
                      :: out)
                      rest
              in
              let union_fields max_size max_align out =
                let rec go max_size max_align out = function
                  | [] ->
                      let byte_storage = Hashtbl.mem cache.byte_storage_index n in
                      let align =
                        if byte_storage then Option.value ~default:1 explicit
                        else max max_align (Option.value ~default:1 explicit)
                      in
                      let last_field =
                        match out with (field : field) :: _ -> field.name | [] -> n
                      in
                      let* size =
                        (match Hashtbl.find_opt cache.struct_sizes_index n with
                          | Some size -> Ok size
                          | None -> Target_layout.round_up_size max_size align)
                        |> mark_overflow n last_field
                      in
                      let definition =
                        {
                          name = n;
                          fields = List.rev out;
                          size;
                          align;
                          is_union = true;
                          byte_storage = Hashtbl.mem cache.byte_storage_index n;
                        }
                      in
                      Hashtbl.replace cache.definitions n definition;
                      Ok (definition.fields, definition.size, definition.align)
                  | (fname, fty) :: rest ->
                      let* size, align =
                        match fty with
                        | Struct sn ->
                            let* _, size, align = calc (n :: visiting) sn in
                            Ok (size, align)
                        | _ -> field_layout (n :: visiting) fty |> mark_overflow n fname
                      in
                      let offset =
                        Option.value ~default:0 (field_offset cache n fname)
                      in
                      let* end_offset = add_size offset size |> mark_overflow n fname in
                      go (max max_size end_offset) (max max_align align)
                        ({
                           name = fname;
                           ty = fty;
                           offset;
                           unsupported_reason = field_reason cache n fname;
                         }
                        :: out)
                        rest
                in
                go max_size max_align out fields
              in
              if Hashtbl.mem cache.union_index n then union_fields 0 1 []
              else each 0 1 [] fields)
  and field_layout visiting = function
    | Bool -> Target_layout.integer target 1
    | Int k -> int_layout target k
    | Float F32 -> Target_layout.float target 32
    | Float F64 -> Target_layout.float target 64
    | Addr | Handle _ -> Target_layout.pointer target
    | Void -> Error "void has no object layout"
    | Opaque n -> Error (Printf.sprintf "opaque type `%s` has no layout" n)
    | Struct n ->
        let* _, s, a = calc visiting n in
        Ok (s, a)
    | Array (n, t) ->
        let* s, a = field_layout visiting t in
        let* size = Target_layout.multiply_size n s in
        Ok (size, a)
    | Vec (n, t) ->
        let* bits = scalar_bits target t in
        Target_layout.vector target n bits
  in
  let* fields, size, align = calc [] name in
  Ok
    {
      name;
      fields;
      size;
      align;
      is_union = Hashtbl.mem cache.union_index name;
      byte_storage = Hashtbl.mem cache.byte_storage_index name;
    }

let compute_struct ?target ?unions ?field_offsets ?field_reasons ?byte_storage decls
    name =
  compute_struct_cached
    (struct_layout_cache ?target ?unions ?field_offsets ?field_reasons ?byte_storage
       decls)
    name

let render p =
  let one_struct (s : struct_def) =
    Printf.sprintf "struct %s size=%d align=%d" s.name s.size s.align
  in
  let one_fn (f : func) =
    Printf.sprintf "fn %s(%s) %s" f.name
      (String.concat ", "
         (List.map
            (fun (local : local) -> local.name ^ ":" ^ ty_name local.ty)
            f.params))
      (ty_name f.ret)
  in
  String.concat "\n" (List.map one_struct p.structs @ List.map one_fn p.funcs)
