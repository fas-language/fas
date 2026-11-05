val int_bits : Hir.int_kind -> int
val integer_value_bit_width : Hir.ty -> int option
val is_int : Hir.ty -> bool
val is_unsigned : Hir.ty -> bool
val is_scalar : Hir.ty -> bool
val is_float : Hir.ty -> bool
val is_numeric : Hir.ty -> bool
val cast_legal : Ast.cast_kind -> Hir.ty -> Hir.ty -> bool
val parse_integer : string -> (int64, string) result
val parse_float_literal : Hir.ty -> string -> (int64, string) result
val integer_exceeds_max_int : string -> bool
val unsigned_int64_to_string : int64 -> string
val literal_limit : negative:bool -> Hir.ty -> int64
val mask_value : Hir.ty -> int64 -> int64
val fits_literal : Hir.ty -> int64 -> bool
val fits_negative_literal : Hir.ty -> int64 -> bool
val sign_extend_bits : Hir.ty -> int64 -> int64
val sign_extend_value : Hir.ty -> int64 -> int64
val popcount64 : int64 -> int
val trailing64 : int64 -> int
val leading64 : int64 -> int
val constant_bitcast : Hir.ty -> int64 list -> Hir.ty -> (int64 list, unit) result
