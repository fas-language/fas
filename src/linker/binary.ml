(* Binary integers are bit patterns, never host-layout records. ELF relocation
   expressions use 64-bit modular arithmetic; file/layout arithmetic is checked. *)
let span ~context ~length offset size =
  if offset < 0 || size < 0 || offset > length || size > length - offset then
    Error.fail "%s: byte range (%d, %d) exceeds size %d" context offset size length

let u8 data offset = Char.code data.[offset]
let u16 = String.get_uint16_le

let u32 data offset =
  Int64.logand (Int64.of_int32 (String.get_int32_le data offset)) 0xffff_ffffL

let u64 = String.get_int64_le

let index ~context ~limit value =
  if value < 0L || value > Int64.of_int limit then
    Error.fail "%s: value 0x%Lx exceeds supported limit %d" context value limit;
  Int64.to_int value

let add ~context ~limit a b =
  if a < 0 || b < 0 || a > limit || b > limit - a then
    Error.fail "%s: size exceeds supported limit %d" context limit;
  a + b

let align ~context ~limit value alignment =
  if alignment <= 0 || alignment land (alignment - 1) <> 0 then
    Error.fail "%s: invalid alignment %d" context alignment;
  let padding = -value land (alignment - 1) in
  add ~context ~limit value padding

let set bytes offset width value =
  match width with
  | 1 -> Bytes.set_uint8 bytes offset (Int64.to_int (Int64.logand value 255L))
  | 2 -> Bytes.set_uint16_le bytes offset (Int64.to_int (Int64.logand value 65535L))
  | 4 -> Bytes.set_int32_le bytes offset (Int64.to_int32 value)
  | 8 -> Bytes.set_int64_le bytes offset value
  | _ -> invalid_arg "Binary.set: unsupported integer width"

let set_int bytes offset width value = set bytes offset width (Int64.of_int value)

let string_at ~context data ~offset ~size name_offset =
  if name_offset < 0 || name_offset >= size then
    Error.fail "%s: string offset %d exceeds string table size %d" context name_offset
      size;
  let start = offset + name_offset in
  let limit = min (offset + size) (start + 65536) in
  let ending = ref start in
  while !ending < limit && data.[!ending] <> '\000' do
    incr ending
  done;
  if !ending = limit then
    Error.fail "%s: unterminated string or name exceeds 65535 bytes" context;
  String.sub data start (!ending - start)
