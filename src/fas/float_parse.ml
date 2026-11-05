external f32_bits : string -> int64 = "fas_parse_f32_bits"
external f64_bits : string -> int64 = "fas_parse_f64_bits"
external literal_kind : string -> int = "fas_float_literal_kind"
external f32_binary_bits : int -> int64 -> int64 -> int64 = "fas_f32_binary_bits"
external float_unary_bits : int -> bool -> int64 -> int64 = "fas_float_unary_bits"

external float_fma_bits : bool -> int64 -> int64 -> int64 -> int64
  = "fas_float_fma_bits"

external int_to_float_bits : int64 -> int -> bool -> bool -> int64
  = "fas_int_to_float_bits"

external float_to_int_bits : int64 -> int -> int -> bool -> int64
  = "fas_float_to_int_bits"
