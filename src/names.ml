type parser_operation =
  | Zext
  | Sext
  | Trunc
  | Bitcast
  | Sizeof
  | Alignof
  | Offsetof
  | Splat

type value_operation =
  | Len
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
  | Syscall
  | Addr_from_bits
  | Handle_addr
  | Handle_from_addr

type float_math = Sqrt | Fma | Floor | Ceil | Round | Abs
type type_constructor = Array | Vector | Address | Handle
type literal = True | False | Null

let scalar_type_names =
  [
    "bool";
    "void";
    "u8";
    "u16";
    "u32";
    "u64";
    "i8";
    "i16";
    "i32";
    "i64";
    "usize";
    "isize";
  ]

let primitive_type_names = scalar_type_names @ [ "addr"; "handle"; "arr"; "vec" ]
let literal_names = [ "true"; "false"; "null" ]
let reserved_float_names = [ "sqrt"; "fma"; "floor"; "ceil"; "round"; "abs"; "cvt" ]
let reserved_float_type_names = [ "f32"; "f64" ]

let parser_type_name name =
  List.mem name scalar_type_names
  || List.mem name reserved_float_type_names
  || List.mem name [ "ptr"; "addr"; "handle"; "arr"; "vec" ]

let c_type_words =
  [ "char"; "int"; "short"; "long"; "unsigned"; "signed"; "float"; "double" ]

let c_fixed_width_types =
  [
    ("size_t", "usize");
    ("ssize_t", "isize");
    ("ptrdiff_t", "isize");
    ("intptr_t", "isize");
    ("uintptr_t", "usize");
    ("int8_t", "i8");
    ("int16_t", "i16");
    ("int32_t", "i32");
    ("int64_t", "i64");
    ("uint8_t", "u8");
    ("uint16_t", "u16");
    ("uint32_t", "u32");
    ("uint64_t", "u64");
  ]

let c_type_word name =
  List.mem name c_type_words || List.mem_assoc name c_fixed_width_types

let c_fixed_width_equivalent name = List.assoc_opt name c_fixed_width_types

let c_word_equivalents =
  [
    ("char", "i8");
    ("int", "i32");
    ("short", "i16");
    ("long", "i64");
    ("unsigned", "u32");
    ("signed", "i32");
    ("float", "f32");
    ("double", "f64");
  ]

let c_type_equivalent name =
  match c_fixed_width_equivalent name with
  | Some equivalent -> Some equivalent
  | None -> List.assoc_opt name c_word_equivalents

let type_constructor = function
  | "addr" -> Some Address
  | "handle" -> Some Handle
  | "arr" -> Some Array
  | "vec" -> Some Vector
  | _ -> None

let literal = function
  | "true" -> Some True
  | "false" -> Some False
  | "null" -> Some Null
  | _ -> None

type operation_kind =
  | Parser of parser_operation
  | Value of value_operation
  | Float_math of float_math
  | Reserved

let operations =
  [
    ("zext", Parser Zext);
    ("sext", Parser Sext);
    ("trunc", Parser Trunc);
    ("bitcast", Parser Bitcast);
    ("sizeof", Parser Sizeof);
    ("alignof", Parser Alignof);
    ("offsetof", Parser Offsetof);
    ("len", Value Len);
    ("splat", Parser Splat);
    ("rotl", Value Rotl);
    ("rotr", Value Rotr);
    ("popcount", Value Popcount);
    ("clz", Value Clz);
    ("ctz", Value Ctz);
    ("select", Value Select);
    ("any", Value Any);
    ("all", Value All);
    ("add_sat", Value Add_sat);
    ("sub_sat", Value Sub_sat);
    ("mul_hi", Value Mul_hi);
    ("reduce_sum", Value Reduce_sum);
    ("reduce_min", Value Reduce_min);
    ("reduce_max", Value Reduce_max);
    ("reduce_and", Value Reduce_and);
    ("reduce_or", Value Reduce_or);
    ("reduce_xor", Value Reduce_xor);
    ("shuffle", Value Shuffle);
    ("permute", Value Permute);
    ("compress", Value Compress);
    ("expand", Value Expand);
    ("masked_load", Reserved);
    ("masked_store", Reserved);
    ("gather", Reserved);
    ("gather_bytes", Reserved);
    ("scatter", Reserved);
    ("scatter_bytes", Reserved);
    ("copy", Reserved);
    ("call_addr", Reserved);
    ("volatile_load", Reserved);
    ("volatile_store", Reserved);
    ("addr_bits", Value Addr_bits);
    ("syscall", Value Syscall);
    ("addr_from_bits", Value Addr_from_bits);
    ("handle_addr", Value Handle_addr);
    ("handle_from_addr", Value Handle_from_addr);
    ("sqrt", Float_math Sqrt);
    ("fma", Float_math Fma);
    ("floor", Float_math Floor);
    ("ceil", Float_math Ceil);
    ("round", Float_math Round);
    ("abs", Float_math Abs);
  ]

let operation_names = List.map fst operations

let builtin_name name =
  List.mem name operation_names || List.mem name reserved_float_names

type reserved_kind = Builtin | Type_name | Literal | Other

let classify_reserved name =
  if builtin_name name then Some Builtin
  else if List.mem name primitive_type_names || List.mem name reserved_float_type_names
  then Some Type_name
  else if List.mem name literal_names then Some Literal
  else if List.mem name [ "view"; "use"; "var" ] then Some Other
  else None

let reserved_binding_name name = Option.is_some (classify_reserved name)

let reserved_binding_message name =
  let kind =
    match classify_reserved name with
    | Some Builtin -> "a builtin"
    | Some Type_name -> "a type name"
    | Some Literal -> "a literal"
    | Some Other | None -> "reserved"
  in
  Printf.sprintf "`%s` is %s and cannot be used as a name" name kind

let reserved_c_name_reason name =
  let kind =
    match classify_reserved name with
    | Some Builtin -> "a Fas builtin"
    | Some Type_name -> "a Fas type name"
    | Some Literal -> "a Fas literal"
    | Some Other | None -> "reserved in Fas"
  in
  Printf.sprintf "name is %s; call it through a C container function with another name"
    kind

let type_misuse_message name =
  match classify_reserved name with
  | Some Builtin -> Some (Printf.sprintf "`%s` is a builtin, not a type" name)
  | Some Literal -> Some (Printf.sprintf "`%s` is a literal, not a type" name)
  | Some (Type_name | Other) | None -> None

let value_misuse_message name =
  if builtin_name name then
    Some (Printf.sprintf "`%s` is a builtin and must be called" name)
  else None

let missing_type_arguments_message name =
  let needs role form = Some (Printf.sprintf "`%s` needs %s: `%s`" name role form) in
  match name with
  | "zext" | "sext" | "trunc" | "bitcast" -> needs "a target type" (name ^ "[T](x)")
  | "cvt" -> needs "a target type" "cvt[T](x)"
  | "sizeof" -> needs "a type argument" "sizeof[T]"
  | "alignof" -> needs "a type argument" "alignof[T]"
  | "offsetof" -> needs "a type argument" "offsetof[T, field]"
  | "handle_from_addr" -> needs "a type argument" "handle_from_addr[T](p)"
  | "volatile_load" -> needs "a type argument" "volatile_load[T](p)"
  | "volatile_store" -> needs "a type argument" "volatile_store[T](p, x)"
  | "masked_load" -> needs "a type argument" "masked_load[T](p, m, fallback)"
  | "masked_store" -> needs "a type argument" "masked_store[T](p, m, values)"
  | "gather" -> needs "a type argument" "gather[T](p, idx, m, fallback)"
  | "scatter" -> needs "a type argument" "scatter[T](p, idx, m, values)"
  | "gather_bytes" -> needs "a type argument" "gather_bytes[T](p, off, m, fallback)"
  | "scatter_bytes" -> needs "a type argument" "scatter_bytes[T](p, off, m, values)"
  | "call_addr" -> needs "a result type" "call_addr[R](p, args...)"
  | _ -> None

let takes_no_type_arguments_message name =
  if builtin_name name && Option.is_none (missing_type_arguments_message name) then
    Some (Printf.sprintf "`%s` takes no type arguments" name)
  else None

let parser_operation name =
  match List.assoc_opt name operations with
  | Some (Parser operation) -> Some operation
  | Some (Value _ | Float_math _ | Reserved) -> None
  | None -> None

let value_operation name =
  match List.assoc_opt name operations with
  | Some (Value operation) -> Some operation
  | Some (Parser _ | Float_math _ | Reserved) -> None
  | None -> None

let float_math name =
  match List.assoc_opt name operations with
  | Some (Float_math operation) -> Some operation
  | Some (Parser _ | Value _ | Reserved) | None -> None
