type c_abi_extension = C_no_extension | C_sign_extension | C_zero_extension
type os = Linux | Windows | Wasi

type t = {
  os : os;
  triple : string;
  llvm_data_layout : string;
  pointer_size : int;
  pointer_align : int;
  integer_alignments : (int * int) list;
  vector_alignments : (int * int) list;
  max_type_alignment : int;
  c_abi_bool_extension : c_abi_extension;
  c_abi_integer_extensions : (int * c_abi_extension * c_abi_extension) list;
  c_long_bits : int;
}

let x86_64_linux =
  {
    os = Linux;
    triple = "x86_64-unknown-linux-gnu";
    llvm_data_layout =
      "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128";
    pointer_size = 8;
    pointer_align = 8;
    integer_alignments = [ (1, 1); (8, 1); (16, 2); (32, 4); (64, 8) ];
    vector_alignments = [];
    max_type_alignment = 1 lsl 31;
    c_abi_bool_extension = C_zero_extension;
    c_abi_integer_extensions =
      [
        (8, C_sign_extension, C_zero_extension); (16, C_sign_extension, C_zero_extension);
      ];
    c_long_bits = 64;
  }

let x86_64_windows =
  {
    x86_64_linux with
    os = Windows;
    triple = "x86_64-w64-windows-gnu";
    llvm_data_layout =
      "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128";
    c_abi_integer_extensions = [];
    c_long_bits = 32;
  }

let wasm32_wasi =
  {
    x86_64_linux with
    os = Wasi;
    triple = "wasm32-unknown-wasip1";
    llvm_data_layout =
      "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20";
    pointer_size = 4;
    pointer_align = 4;
    c_long_bits = 32;
  }

let of_os = function
  | Linux -> x86_64_linux
  | Windows -> x86_64_windows
  | Wasi -> wasm32_wasi

let host_os = if Sys.win32 || Sys.cygwin then Windows else Linux
let selected = ref (of_os host_os)
let select os = selected := of_os os
let current () = !selected

let ( let* ) result f =
  match result with Ok value -> f value | Error message -> Error message

let multiply_size count size =
  if count < 0 then Error "negative aggregate length"
  else if size > 0 && count > max_int / size then Error "aggregate size overflows"
  else Ok (count * size)

let next_power_of_two value =
  let rec grow power =
    if power >= value then Ok power
    else if power > max_int / 2 then Error "aggregate size overflows"
    else grow (power * 2)
  in
  grow 1

let round_up_size size align =
  if align <= 0 then Error "invalid target alignment"
  else
    let remainder = size mod align in
    if remainder = 0 then Ok size
    else
      let padding = align - remainder in
      if size > max_int - padding then Error "aggregate size overflows"
      else Ok (size + padding)

let validate_type_alignment target align =
  if align <= 0 || align land (align - 1) <> 0 then
    Error "alignment must be a positive power of two"
  else if align > target.max_type_alignment then
    Error
      (Printf.sprintf "alignment exceeds target maximum of %d" target.max_type_alignment)
  else Ok ()

let pointer_integer_bits target =
  match target.pointer_size with
  | 4 -> Ok 32
  | 8 -> Ok 64
  | size -> Error (Printf.sprintf "unsupported pointer size: %d" size)

let c_integer_extension target ~signed bits =
  match
    List.find_opt
      (fun (candidate, _, _) -> candidate = bits)
      target.c_abi_integer_extensions
  with
  | Some (_, signed_extension, unsigned_extension) ->
      if signed then signed_extension else unsigned_extension
  | None -> C_no_extension

let pointer target =
  let* size = round_up_size target.pointer_size target.pointer_align in
  Ok (size, target.pointer_align)

let integer target bits =
  if bits <= 0 || bits > max_int - 7 then Error "integer has invalid bit width"
  else
    let size = (bits + 7) / 8 in
    match List.assoc_opt bits target.integer_alignments with
    | Some align ->
        let* size = round_up_size size align in
        Ok (size, align)
    | None -> Error (Printf.sprintf "target has no i%d layout" bits)

let float _target bits =
  match bits with
  | 32 -> Ok (4, 4)
  | 64 -> Ok (8, 8)
  | _ -> Error (Printf.sprintf "target has no f%d layout" bits)

let vector target lanes element_bits =
  if lanes <= 0 then Error "vector lane count must be positive"
  else if element_bits <= 0 then Error "vector element has no bit width"
  else if lanes > max_int / element_bits then Error "aggregate size overflows"
  else
    let bits = lanes * element_bits in
    if bits > max_int - 7 then Error "aggregate size overflows"
    else
      let bytes = (bits + 7) / 8 in
      let* align =
        match List.assoc_opt bits target.vector_alignments with
        | Some align -> Ok align
        | None -> next_power_of_two bytes
      in
      let* size = round_up_size bytes align in
      Ok (size, align)
