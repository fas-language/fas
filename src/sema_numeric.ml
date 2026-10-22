let int_bits = function
  | Hir.U8 | I8 -> 8
  | U16 | I16 -> 16
  | U32 | I32 -> 32
  | U64 | I64 -> 64
  | Usize | Isize -> (Target_layout.current ()).pointer_size * 8

let integer_value_bit_width = function
  | Hir.Bool -> Some 1
  | Hir.Int kind -> Some (int_bits kind)
  | Hir.Vec (length, Hir.Bool) -> Some length
  | Hir.Vec (length, Hir.Int kind) ->
      let element_width = int_bits kind in
      if length > max_int / element_width then None else Some (length * element_width)
  | _ -> None

let is_int = function Hir.Int _ -> true | _ -> false

let is_unsigned = function
  | Hir.Int (Hir.U8 | U16 | U32 | U64 | Usize) -> true
  | _ -> false

let is_scalar = function Hir.Bool | Hir.Int _ | Hir.Float _ -> true | _ -> false
let is_float = function Hir.Float _ -> true | _ -> false
let is_numeric = function Hir.Int _ | Hir.Vec (_, Hir.Int _) -> true | _ -> false

let cast_legal kind from target =
  let value_bits = function
    | (Hir.Bool | Hir.Int _) as ty -> integer_value_bit_width ty
    | _ -> None
  in
  let strictly_wider () =
    match (value_bits from, value_bits target) with
    | Some source_width, Some destination_width -> destination_width > source_width
    | _ -> false
  in
  let strictly_narrower () =
    match (value_bits from, value_bits target) with
    | Some source_width, Some destination_width -> destination_width < source_width
    | _ -> false
  in
  let lane_conversion relation source target =
    match (source, target) with
    | Hir.Vec (source_lanes, source_element), Hir.Vec (target_lanes, target_element)
      when source_lanes = target_lanes -> (
        match
          ( integer_value_bit_width source_element,
            integer_value_bit_width target_element )
        with
        | Some source_width, Some target_width -> relation source_width target_width
        | _ -> false)
    | _ -> false
  in
  let bit_width = function
    | Hir.Float Hir.F32 -> Some 32
    | Hir.Float Hir.F64 -> Some 64
    | Hir.Vec (lanes, Hir.Float Hir.F32) -> Some (lanes * 32)
    | Hir.Vec (lanes, Hir.Float Hir.F64) -> Some (lanes * 64)
    | ty -> integer_value_bit_width ty
  in
  match kind with
  | Ast.Float_widen -> from = Hir.Float Hir.F32 && target = Hir.Float Hir.F64
  | Ast.Zext | Ast.Sext ->
      ((from = Hir.Bool || is_int from) && is_int target && strictly_wider ())
      || lane_conversion ( < ) from target
  | Ast.Trunc ->
      (is_int from && (is_int target || target = Hir.Bool) && strictly_narrower ())
      || lane_conversion ( > ) from target
  | Ast.Bitcast -> (
      match (from, target) with
      | Hir.Float Hir.F32, Hir.Int kind | Hir.Int kind, Hir.Float Hir.F32 ->
          int_bits kind = 32
      | Hir.Float Hir.F64, Hir.Int kind | Hir.Int kind, Hir.Float Hir.F64 ->
          int_bits kind = 64
      | _ -> (
          match (bit_width from, bit_width target) with
          | Some source_width, Some destination_width ->
              source_width = destination_width
          | _ -> false))

let parse_integer raw =
  let clean = String.concat "" (String.split_on_char '_' raw) in
  let radix, digits =
    if
      String.length clean > 2
      && (String.sub clean 0 2 = "0x" || String.sub clean 0 2 = "0X")
    then (16, String.sub clean 2 (String.length clean - 2))
    else if
      String.length clean > 2
      && (String.sub clean 0 2 = "0b" || String.sub clean 0 2 = "0B")
    then (2, String.sub clean 2 (String.length clean - 2))
    else if
      String.length clean > 2
      && (String.sub clean 0 2 = "0o" || String.sub clean 0 2 = "0O")
    then (8, String.sub clean 2 (String.length clean - 2))
    else (10, clean)
  in
  let value = ref 0L in
  let digit c =
    match c with
    | '0' .. '9' -> Char.code c - 48
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _ -> -1
  in
  if digits = "" then Error "integer literal has no digits"
  else
    let valid =
      String.for_all
        (fun c ->
          let d = digit c in
          d >= 0 && d < radix)
        digits
    in
    let limit =
      match radix with
      | 10 -> "18446744073709551615"
      | 16 -> "FFFFFFFFFFFFFFFF"
      | 8 -> "1777777777777777777777"
      | 2 -> String.make 64 '1'
      | _ -> ""
    in
    let first_nonzero =
      let rec find index =
        if index = String.length digits || digits.[index] <> '0' then index
        else find (index + 1)
      in
      find 0
    in
    let significant =
      if first_nonzero = String.length digits then "0"
      else String.sub digits first_nonzero (String.length digits - first_nonzero)
    in
    let normalized =
      if radix = 16 then String.uppercase_ascii significant else significant
    in
    let overflow =
      String.length normalized > String.length limit
      || String.length normalized = String.length limit
         && String.compare normalized limit > 0
    in
    if not valid then Error "invalid integer literal"
    else if overflow then Error "integer literal overflows 64 bits"
    else (
      String.iter
        (fun c ->
          value :=
            Int64.add (Int64.mul !value (Int64.of_int radix)) (Int64.of_int (digit c)))
        digits;
      Ok !value)

let parse_float_literal ty raw =
  let clean = String.concat "" (String.split_on_char '_' raw) in
  let bits =
    match ty with
    | Hir.Float Hir.F32 -> Some (Float_parse.f32_bits clean, 0x7f800000L, 0x007fffffL)
    | Hir.Float Hir.F64 ->
        Some (Float_parse.f64_bits clean, 0x7ff0000000000000L, 0x000fffffffffffffL)
    | _ -> None
  in
  match bits with
  | None -> Error "expected a floating point type"
  | Some (bits, exponent_mask, fraction_mask) ->
      let infinity_bits = exponent_mask in
      let is_infinity =
        Int64.logand bits exponent_mask = infinity_bits
        && Int64.logand bits fraction_mask = 0L
      in
      if is_infinity then
        Error
          (Printf.sprintf "floating point literal rounds to infinity for `%s`"
             (Hir.ty_name ty))
      else if bits = 0L && Float_parse.literal_kind clean land 4 <> 0 then
        Error
          (Printf.sprintf "nonzero floating point literal rounds to zero for `%s`"
             (Hir.ty_name ty))
      else Ok bits

let integer_exceeds_max_int raw =
  match parse_integer raw with
  | Ok value -> Int64.unsigned_compare value (Int64.of_int max_int) > 0
  | Error message -> message = "integer literal overflows 64 bits"

let unsigned_int64_to_string value =
  let rec digits value =
    if Int64.unsigned_compare value 10L < 0 then
      String.make 1 (Char.chr (48 + Int64.to_int value))
    else
      let quotient = Int64.unsigned_div value 10L in
      let remainder = Int64.unsigned_rem value 10L in
      digits quotient ^ String.make 1 (Char.chr (48 + Int64.to_int remainder))
  in
  digits value

let mask_value ty value =
  match ty with
  | Hir.Bool -> if value = 0L then 0L else 1L
  | Hir.Int k ->
      let bits = int_bits k in
      if bits = 64 then value
      else Int64.logand value (Int64.sub (Int64.shift_left 1L bits) 1L)
  | _ -> value

let literal_limit ~negative = function
  | Hir.Int k ->
      let bits = int_bits k in
      let signed =
        match k with Hir.I8 | I16 | I32 | I64 | Isize -> true | _ -> false
      in
      if signed then
        if negative then Int64.shift_left 1L (bits - 1)
        else if bits = 64 then Int64.max_int
        else Int64.sub (Int64.shift_left 1L (bits - 1)) 1L
      else if bits = 64 then Int64.minus_one
      else Int64.sub (Int64.shift_left 1L bits) 1L
  | _ -> 0L

let fits_literal ty value =
  match ty with
  | Hir.Int _ -> Int64.unsigned_compare value (literal_limit ~negative:false ty) <= 0
  | _ -> false

let fits_negative_literal ty value =
  match ty with
  | Hir.Int _ when is_unsigned ty -> value = 0L
  | Hir.Int _ -> Int64.unsigned_compare value (literal_limit ~negative:true ty) <= 0
  | _ -> false

let sign_extend_bits ty value =
  match ty with
  | Hir.Int k ->
      let bits = int_bits k in
      if bits = 64 then value
      else
        let mask = Int64.sub (Int64.shift_left 1L bits) 1L in
        let value = Int64.logand value mask in
        let sign_bit = Int64.shift_left 1L (bits - 1) in
        if Int64.logand value sign_bit <> 0L then Int64.logor value (Int64.lognot mask)
        else value
  | _ -> value

let sign_extend_value ty value =
  if is_int ty && not (is_unsigned ty) then sign_extend_bits ty value else value

let popcount64 x =
  let rec go n v = if v = 0L then n else go (n + 1) (Int64.logand v (Int64.sub v 1L)) in
  go 0 x

let trailing64 x =
  if x = 0L then 64
  else
    let rec go n v =
      if Int64.logand v 1L <> 0L then n else go (n + 1) (Int64.shift_right_logical v 1)
    in
    go 0 x

let leading64 x =
  if x = 0L then 64
  else
    let rec go n bit =
      if Int64.logand x bit <> 0L then n
      else go (n + 1) (Int64.shift_right_logical bit 1)
    in
    go 0 Int64.min_int

let constant_bitcast source_ty source_values destination_ty =
  let shape ty values =
    match ty with
    | Hir.Bool | Hir.Int _ ->
        Option.map (fun width -> (width, 1, values)) (integer_value_bit_width ty)
    | Hir.Float Hir.F32 -> Some (32, 1, values)
    | Hir.Float Hir.F64 -> Some (64, 1, values)
    | Hir.Vec (lanes, ((Hir.Bool | Hir.Int _ | Hir.Float _) as element)) ->
        Option.map
          (fun width -> (width, lanes, values))
          (match element with
          | Hir.Float Hir.F32 -> Some 32
          | Hir.Float Hir.F64 -> Some 64
          | _ -> integer_value_bit_width element)
    | _ -> None
  in
  match (shape source_ty source_values, shape destination_ty []) with
  | ( Some (source_width, source_lanes, source_values),
      Some (destination_width, destination_lanes, _) )
    when source_width * source_lanes = destination_width * destination_lanes
         && List.length source_values = source_lanes ->
      let source_values = Array.of_list source_values in
      let bit position =
        let lane = position / source_width in
        let offset = position mod source_width in
        Int64.logand (Int64.shift_right_logical source_values.(lane) offset) 1L
      in
      Ok
        (List.init destination_lanes (fun lane ->
             let value = ref 0L in
             for offset = 0 to destination_width - 1 do
               if bit ((lane * destination_width) + offset) <> 0L then
                 value := Int64.logor !value (Int64.shift_left 1L offset)
             done;
             !value))
  | _ -> Error ()
