module Int64_set = Set.Make (Int64)

type ty =
  | I1
  | I8
  | I16
  | I32
  | I64
  | I128
  | F32
  | F64
  | Pointer of ty
  | Vector of int * ty
  | Struct of string
  | Array of int * ty
  | Void

type value =
  | Const of ty * int64
  | Const_vector of ty * int64 list
  | Null of ty
  | Undef of ty
  | Zero of ty
  | Local of int * ty
  | Param of string * ty
  | Global of string * ty

type binop =
  | Add
  | Sub
  | Mul
  | Sdiv
  | Srem
  | Udiv
  | Urem
  | And
  | Or
  | Xor
  | Shl
  | Lshr
  | Ashr
  | Fadd
  | Fsub
  | Fmul
  | Fdiv

type cmp =
  | Eq
  | Ne
  | Slt
  | Sle
  | Sgt
  | Sge
  | Ult
  | Ule
  | Ugt
  | Uge
  | Foeq
  | Fune
  | Folt
  | Fole
  | Fogt
  | Foge

type gep_index = Zero | Index of value
type extension = No_extension | Sign_extension | Zero_extension

type instr =
  | Bin of int * binop * ty * value * value
  | Fneg of int * ty * value
  | Cmp of int * cmp * ty * value * value
  | Alloca of int * ty * int
  | Load of int * ty * value * int
  | Load_volatile of int * ty * value * int
  | Store of ty * value * value * int
  | Store_volatile of ty * value * value * int
  | Gep of int * ty * value * gep_index list
  | Cast of int * string * ty * value * ty
  | Call of int option * extension * ty * string * (ty * extension * value) list
  | Call_indirect of int option * extension * ty * value * (ty * extension * value) list
  | Phi of int * ty * (value * int) list
  | Select of int * value * value * value
  | Extract of int * ty * value * value
  | Insert of int * ty * value * value * value
  | Shuffle_zero of int * ty * value
  | Shufflevector of int * ty * value * value * value
  | String_ptr of int * int * int
  | Global_ptr of int * string * ty
  | Trap
  | Location of int * int

type terminator =
  | Ret of (ty * value) option
  | Br of int
  | CondBr of value * int * int
  | Switch of ty * value * (int64 * int) list * int
  | Unreachable

type param = { name : string; ty : ty; extension : extension }
type linkage = Internal | External

type func = {
  name : string;
  params : param list;
  ret : ty;
  ret_extension : extension;
  blocks : block list;
  linkage : linkage;
  variadic : bool;
  source : (string * int) option;
  display : string option;
}

and block = { id : int; label : string; instrs : instr list; terminator : terminator }

type struct_def = { name : string; fields : ty list; tail_padding : int }

type global =
  | String_global of { name : string; bytes : string }
  | Array_global of { name : string; elem_ty : ty; elems : int64 list; align : int }
  | Storage_global of {
      name : string;
      storage_ty : ty;
      size : int;
      bytes : string option;
      pointers : (int * string * int) list;
      readonly : bool;
      align : int;
      linkage : Ast.global_linkage;
    }

type module_ = {
  target_triple : string;
  data_layout : string;
  structs : struct_def list;
  globals : global list;
  funcs : func list;
  no_inline_functions : string list;
}

let real_instrs block =
  List.filter (function Location _ -> false | _ -> true) block.instrs

let safe_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' -> true
  | _ -> false

let quote_identifier n =
  if String.for_all safe_identifier_char n then n else "\"" ^ String.escaped n ^ "\""

let struct_name n = "%" ^ quote_identifier ("struct." ^ n)

let rec ty_name = function
  | I1 -> "i1"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | I128 -> "i128"
  | F32 -> "float"
  | F64 -> "double"
  | Pointer _ -> "ptr"
  | Vector (n, t) -> Printf.sprintf "<%d x %s>" n (ty_name t)
  | Struct n -> struct_name n
  | Array (n, t) -> Printf.sprintf "[%d x %s]" n (ty_name t)
  | Void -> "void"

let value_ty = function
  | Const (t, _)
  | Const_vector (t, _)
  | Null t
  | Undef t
  | Zero t
  | Local (_, t)
  | Param (_, t)
  | Global (_, t) ->
      t

let ( let* ) result next =
  match result with Ok value -> next value | Error _ as e -> e

let rec type_equal left right =
  match (left, right) with
  | Pointer _, Pointer _ -> true
  | Vector (left_lanes, left_elem), Vector (right_lanes, right_elem) ->
      left_lanes = right_lanes && type_equal left_elem right_elem
  | Array (left_length, left_elem), Array (right_length, right_elem) ->
      left_length = right_length && type_equal left_elem right_elem
  | _ -> left = right

let integer_width = function
  | I1 -> Some 1
  | I8 -> Some 8
  | I16 -> Some 16
  | I32 -> Some 32
  | I64 -> Some 64
  | I128 -> Some 128
  | F32 | F64 | Pointer _ | Vector _ | Struct _ | Array _ | Void -> None

let integer_shape = function
  | Vector (lanes, elem) ->
      Option.map (fun width -> (Some lanes, width)) (integer_width elem)
  | ty -> Option.map (fun width -> (None, width)) (integer_width ty)

let is_integer ty = Option.is_some (integer_width ty)
let is_integer_like ty = Option.is_some (integer_shape ty)
let is_pointer = function Pointer _ -> true | _ -> false

let valid_extension ty = function
  | No_extension -> true
  | Sign_extension | Zero_extension -> is_integer ty

let is_equality_type = function
  | Vector (_, (Pointer _ | F32 | F64)) | Pointer _ | F32 | F64 -> true
  | ty -> is_integer_like ty

let rec valid_value_type = function
  | Void -> false
  | Vector (lanes, elem) ->
      lanes > 0 && (is_integer elem || is_pointer elem || elem = F32 || elem = F64)
  | Array (length, elem) -> length >= 0 && valid_value_type elem
  | I1 | I8 | I16 | I32 | I64 | I128 | F32 | F64 | Pointer _ | Struct _ -> true

let integer_constant_fits ty value =
  match integer_width ty with
  | None -> false
  | Some 1 -> value = 0L || value = 1L
  | Some 64 | Some 128 -> true
  | Some width ->
      let minimum = Int64.neg (Int64.shift_left 1L (width - 1)) in
      let maximum = Int64.pred (Int64.shift_left 1L width) in
      value >= minimum && value <= maximum

let validate_value_form = function
  | Const ((F32 | F64), _) -> Ok ()
  | Const (ty, value) ->
      if integer_constant_fits ty value then Ok ()
      else Error "integer constant does not fit its type"
  | Const_vector (Vector (lanes, elem), values) ->
      if lanes <= 0 then Error "vector constant has a non-positive lane count"
      else if List.length values <> lanes then
        Error "vector constant lane count does not match its type"
      else if elem = F32 || elem = F64 then Ok ()
      else if List.for_all (integer_constant_fits elem) values then Ok ()
      else Error "vector constant element does not fit its type"
  | Const_vector _ -> Error "vector constant requires an integer vector type"
  | Null (Pointer _) -> Ok ()
  | Null _ -> Error "null constant requires a pointer type"
  | Undef ty | Zero ty | Local (_, ty) | Param (_, ty) | Global (_, ty) ->
      if valid_value_type ty then Ok () else Error "value has an invalid type"

let instruction_result = function
  | Bin (id, _, ty, _, _)
  | Fneg (id, ty, _)
  | Load (id, ty, _, _)
  | Load_volatile (id, ty, _, _)
  | Cast (id, _, _, _, ty)
  | Phi (id, ty, _)
  | Insert (id, ty, _, _, _)
  | Shuffle_zero (id, ty, _) ->
      Some (id, ty)
  | Shufflevector (id, ty, _, _, _) -> Some (id, ty)
  | Alloca (id, ty, _) | Gep (id, ty, _, _) -> Some (id, Pointer ty)
  | Extract (id, Vector (_, elem), _, _) -> Some (id, elem)
  | Extract (id, ty, _, _) -> Some (id, ty)
  | Cmp (id, _, ty, _, _) ->
      Some (id, match ty with Vector (lanes, _) -> Vector (lanes, I1) | _ -> I1)
  | Select (id, _, yes, _) -> Some (id, value_ty yes)
  | Call (Some id, _, ty, _, _) -> Some (id, ty)
  | Call_indirect (Some id, _, ty, _, _) -> Some (id, ty)
  | String_ptr (id, _, _) -> Some (id, Pointer I8)
  | Global_ptr (id, _, ty) -> Some (id, Pointer ty)
  | Store _ | Store_volatile _
  | Call (None, _, _, _, _)
  | Call_indirect (None, _, _, _, _)
  | Trap | Location _ ->
      None

let terminator_successors = function
  | Ret _ | Unreachable -> []
  | Br successor -> [ successor ]
  | CondBr (_, yes, no) -> [ yes; no ]
  | Switch (_, _, cases, default) -> default :: List.map snd cases

let validate_function struct_names globals functions (func : func) =
  let fail format =
    Printf.ksprintf
      (fun message -> Error ("function `" ^ func.name ^ "` " ^ message))
      format
  in
  let rec references_defined_type = function
    | Struct name -> Hashtbl.mem struct_names name
    | Array (_, elem) | Vector (_, elem) -> references_defined_type elem
    | Pointer _ | I1 | I8 | I16 | I32 | I64 | I128 | F32 | F64 | Void -> true
  in
  let valid_module_value_type ty = valid_value_type ty && references_defined_type ty in
  let block_ids = Hashtbl.create (List.length func.blocks) in
  let blocks_by_id = Hashtbl.create (List.length func.blocks) in
  let predecessors = Hashtbl.create (List.length func.blocks) in
  let definitions = Hashtbl.create 64 in
  let definition_locations = Hashtbl.create 64 in
  let parameters = Hashtbl.create (List.length func.params) in
  let rec collect_blocks = function
    | [] -> Ok ()
    | (block : block) :: rest ->
        if block.id < 0 then fail "has negative block id %d" block.id
        else if Hashtbl.mem block_ids block.id then
          fail "has duplicate block id %d" block.id
        else (
          Hashtbl.add block_ids block.id ();
          Hashtbl.add blocks_by_id block.id block;
          Hashtbl.add predecessors block.id [];
          collect_blocks rest)
  in
  let rec collect_parameters = function
    | [] -> Ok ()
    | (parameter : param) :: rest ->
        if Hashtbl.mem parameters parameter.name then
          fail "has duplicate parameter name `%s`" parameter.name
        else if not (valid_module_value_type parameter.ty) then
          fail "parameter `%s` has an invalid type" parameter.name
        else if not (valid_extension parameter.ty parameter.extension) then
          fail "parameter `%s` has an invalid extension" parameter.name
        else (
          Hashtbl.add parameters parameter.name parameter.ty;
          collect_parameters rest)
  in
  let add_predecessor predecessor successor =
    match Hashtbl.find_opt predecessors successor with
    | None -> fail "block %d has unknown successor %d" predecessor successor
    | Some current ->
        if not (List.mem predecessor current) then
          Hashtbl.replace predecessors successor (predecessor :: current);
        Ok ()
  in
  let rec collect_successors predecessor = function
    | [] -> Ok ()
    | successor :: rest ->
        let* () = add_predecessor predecessor successor in
        collect_successors predecessor rest
  in
  let rec collect_edges = function
    | [] -> Ok ()
    | (block : block) :: rest ->
        let* () =
          collect_successors block.id (terminator_successors block.terminator)
        in
        collect_edges rest
  in
  let validate_entry_predecessors () =
    match func.blocks with
    | [] -> Ok ()
    | (entry : block) :: _ ->
        if Hashtbl.find predecessors entry.id = [] then Ok ()
        else fail "entry block %d has predecessors" entry.id
  in
  let add_definition block_id instruction_index instruction =
    match instruction_result instruction with
    | None -> Ok ()
    | Some (id, ty) ->
        if id < 0 then fail "block %d defines negative value id %d" block_id id
        else if Hashtbl.mem definitions id then fail "has duplicate value id %d" id
        else if not (valid_module_value_type ty) then
          fail "block %d value %d has invalid result type `%s`" block_id id (ty_name ty)
        else (
          Hashtbl.add definitions id ty;
          Hashtbl.add definition_locations id (block_id, instruction_index);
          Ok ())
  in
  let rec collect_instructions block_id instruction_index = function
    | [] -> Ok ()
    | instruction :: rest ->
        let* () = add_definition block_id instruction_index instruction in
        collect_instructions block_id (instruction_index + 1) rest
  in
  let rec collect_definitions = function
    | [] -> Ok ()
    | (block : block) :: rest ->
        let* () = collect_instructions block.id 0 block.instrs in
        collect_definitions rest
  in
  let validate_reference block_id value =
    let* () =
      if not (valid_module_value_type (value_ty value)) then
        fail "block %d has a value with an invalid type" block_id
      else
        match validate_value_form value with
        | Ok () -> Ok ()
        | Error message -> fail "block %d has invalid value: %s" block_id message
    in
    match value with
    | Local (id, claimed) -> (
        match Hashtbl.find_opt definitions id with
        | None -> fail "block %d uses undefined value %d" block_id id
        | Some actual when type_equal actual claimed -> Ok ()
        | Some _ -> fail "block %d value %d claims the wrong type" block_id id)
    | Param (name, claimed) -> (
        match Hashtbl.find_opt parameters name with
        | None -> fail "block %d uses unknown parameter `%s`" block_id name
        | Some actual when type_equal actual claimed -> Ok ()
        | Some _ -> fail "block %d parameter `%s` claims the wrong type" block_id name)
    | Global (name, claimed) -> (
        match Hashtbl.find_opt globals name with
        | Some actual when type_equal claimed (Pointer actual) -> Ok ()
        | Some _ -> fail "block %d global `%s` claims the wrong type" block_id name
        | None when Hashtbl.mem functions name && is_pointer claimed -> Ok ()
        | None -> fail "block %d uses unknown global `%s`" block_id name)
    | Const _ | Const_vector _ | Null _ | Undef _ | Zero _ -> Ok ()
  in
  let operand block_id expected value =
    let* () = validate_reference block_id value in
    if type_equal expected (value_ty value) then Ok ()
    else fail "block %d has an operand with the wrong type" block_id
  in
  let pointer_operand block_id value =
    let* () = validate_reference block_id value in
    if is_pointer (value_ty value) then Ok ()
    else fail "block %d requires a pointer operand" block_id
  in
  let integer_operand block_id value =
    let* () = validate_reference block_id value in
    if is_integer (value_ty value) then Ok ()
    else fail "block %d requires an integer operand" block_id
  in
  let valid_alignment alignment = alignment > 0 && alignment land (alignment - 1) = 0 in
  let validate_alignment block_id alignment =
    if valid_alignment alignment then Ok ()
    else fail "block %d has invalid alignment %d" block_id alignment
  in
  let validate_cast block_id opcode source value destination =
    let* () = operand block_id source value in
    let integer_cast relation =
      match (integer_shape source, integer_shape destination) with
      | Some (source_lanes, source_width), Some (destination_lanes, destination_width)
        when source_lanes = destination_lanes && relation source_width destination_width
        ->
          Ok ()
      | _ -> fail "block %d has invalid `%s` types" block_id opcode
    in
    let float_type = function F32 | F64 -> true | _ -> false in
    let lane_shape = function
      | Vector (lanes, elem) -> Some (lanes, elem)
      | ty -> Some (1, ty)
    in
    let lane_conversion source_ok destination_ok relation =
      match (lane_shape source, lane_shape destination) with
      | Some (source_lanes, source_elem), Some (destination_lanes, destination_elem)
        when source_lanes = destination_lanes
             && source_ok source_elem
             && destination_ok destination_elem ->
          relation source_elem destination_elem
      | _ -> false
    in
    match opcode with
    | "sitofp" | "uitofp" ->
        if
          lane_conversion is_integer float_type (fun _ _ -> true)
          && match source with Vector _ -> true | _ -> is_integer source
        then Ok ()
        else fail "block %d has invalid `%s` types" block_id opcode
    | "fpext" ->
        if lane_conversion float_type float_type (fun s d -> s = F32 && d = F64) then
          Ok ()
        else fail "block %d has invalid `fpext` types" block_id
    | "fptrunc" ->
        if lane_conversion float_type float_type (fun s d -> s = F64 && d = F32) then
          Ok ()
        else fail "block %d has invalid `fptrunc` types" block_id
    | "zext" | "sext" -> integer_cast ( < )
    | "trunc" -> integer_cast ( > )
    | "ptrtoint" ->
        if is_pointer source && is_integer destination then Ok ()
        else fail "block %d has invalid `ptrtoint` types" block_id
    | "inttoptr" ->
        if is_integer source && is_pointer destination then Ok ()
        else fail "block %d has invalid `inttoptr` types" block_id
    | "bitcast" -> (
        let bitcast_shape = function
          | Vector (lanes, elem) ->
              let width =
                match integer_width elem with
                | Some _ as width -> width
                | None -> (
                    match elem with F32 -> Some 32 | F64 -> Some 64 | _ -> None)
              in
              Option.map (fun width -> (Some lanes, width)) width
          | F32 -> Some (None, 32)
          | F64 -> Some (None, 64)
          | ty -> integer_shape ty
        in
        match (bitcast_shape source, bitcast_shape destination) with
        | Some (source_lanes, source_width), Some (destination_lanes, destination_width)
          ->
            let source_count = Option.value ~default:1 source_lanes in
            let destination_count = Option.value ~default:1 destination_lanes in
            let rec gcd left right =
              if right = 0 then left else gcd right (left mod right)
            in
            let divisor = gcd source_width destination_width in
            let source_factor = destination_width / divisor in
            let destination_factor = source_width / divisor in
            if
              source_count mod source_factor = 0
              && destination_count mod destination_factor = 0
              && source_count / source_factor = destination_count / destination_factor
            then Ok ()
            else fail "block %d has unequal-width `bitcast` types" block_id
        | _ ->
            let scalar_int_float = function
              | I8 -> Some 8
              | I16 -> Some 16
              | I32 -> Some 32
              | I64 -> Some 64
              | I128 -> Some 128
              | _ -> None
            in
            let float_int_pair =
              match (source, destination) with
              | F32, int_ty | int_ty, F32 -> scalar_int_float int_ty = Some 32
              | F64, int_ty | int_ty, F64 -> scalar_int_float int_ty = Some 64
              | _ -> false
            in
            let equal_lane_width_float =
              lane_conversion
                (fun ty -> is_integer ty || float_type ty)
                (fun ty -> is_integer ty || float_type ty)
                (fun source_elem destination_elem ->
                  let width = function
                    | F32 -> Some 32
                    | F64 -> Some 64
                    | ty -> integer_width ty
                  in
                  width source_elem = width destination_elem)
            in
            if
              float_int_pair || equal_lane_width_float
              || (is_pointer source && is_pointer destination)
            then Ok ()
            else fail "block %d has invalid `bitcast` types" block_id)
    | _ -> fail "block %d has unknown cast opcode `%s`" block_id opcode
  in
  let rec validate_gep_indices block_id = function
    | [] -> Ok ()
    | Zero :: rest -> validate_gep_indices block_id rest
    | Index value :: rest ->
        let* () = integer_operand block_id value in
        validate_gep_indices block_id rest
  in
  let validate_phi block_id ty incoming =
    let rec validate_values seen = function
      | [] -> Ok seen
      | (value, predecessor) :: rest ->
          if List.mem predecessor seen then
            fail "block %d phi has duplicate incoming block %d" block_id predecessor
          else
            let* () = operand block_id ty value in
            validate_values (predecessor :: seen) rest
    in
    let* incoming_ids = validate_values [] incoming in
    let expected = Option.value ~default:[] (Hashtbl.find_opt predecessors block_id) in
    if incoming_ids = [] then fail "block %d has a phi without inputs" block_id
    else if List.sort compare incoming_ids = List.sort compare expected then Ok ()
    else fail "block %d phi inputs do not match its predecessors" block_id
  in
  let validate_instruction block_id = function
    | Bin (_, operation, ty, left, right) ->
        let is_float_operation =
          match operation with Fadd | Fsub | Fmul | Fdiv -> true | _ -> false
        in
        let is_float_type = function
          | F32 | F64 | Vector (_, (F32 | F64)) -> true
          | _ -> false
        in
        if
          (is_float_operation && not (is_float_type ty))
          || ((not is_float_operation) && not (is_integer_like ty))
        then fail "block %d has a non-integer binary type" block_id
        else
          let* () = operand block_id ty left in
          operand block_id ty right
    | Fneg (_, ty, value) ->
        if match ty with F32 | F64 | Vector (_, (F32 | F64)) -> false | _ -> true then
          fail "block %d has a non-float fneg type" block_id
        else operand block_id ty value
    | Cmp (_, comparison, ty, left, right) ->
        let valid_type =
          match comparison with
          | Eq | Ne -> is_equality_type ty
          | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge -> is_integer_like ty
          | Foeq | Fune | Folt | Fole | Fogt | Foge -> (
              match ty with F32 | F64 | Vector (_, (F32 | F64)) -> true | _ -> false)
        in
        if not valid_type then fail "block %d has an invalid comparison type" block_id
        else
          let* () = operand block_id ty left in
          operand block_id ty right
    | Alloca (_, ty, alignment) ->
        if not (valid_module_value_type ty) then
          fail "block %d allocates an invalid type" block_id
        else validate_alignment block_id alignment
    | Load (_, ty, pointer, alignment) | Load_volatile (_, ty, pointer, alignment) ->
        if not (valid_module_value_type ty) then
          fail "block %d loads an invalid type" block_id
        else
          let* () = pointer_operand block_id pointer in
          validate_alignment block_id alignment
    | Store (ty, value, pointer, alignment)
    | Store_volatile (ty, value, pointer, alignment) ->
        if not (valid_module_value_type ty) then
          fail "block %d stores an invalid type" block_id
        else
          let* () = operand block_id ty value in
          let* () = pointer_operand block_id pointer in
          validate_alignment block_id alignment
    | Gep (_, ty, pointer, indices) ->
        if not (valid_module_value_type ty) then
          fail "block %d indexes an invalid type" block_id
        else
          let* () = pointer_operand block_id pointer in
          validate_gep_indices block_id indices
    | Cast (_, opcode, source, value, destination) ->
        validate_cast block_id opcode source value destination
    | Call (result, _, ty, _, arguments) ->
        let* () =
          match (result, ty) with
          | None, Void
          | ( Some _,
              ( I1 | I8 | I16 | I32 | I64 | I128 | F32 | F64 | Pointer _ | Vector _
              | Struct _ | Array _ ) ) ->
              Ok ()
          | None, _ -> fail "block %d discards a non-void call result" block_id
          | Some _, Void -> fail "block %d assigns a void call result" block_id
        in
        let rec validate_arguments = function
          | [] -> Ok ()
          | (argument_ty, _, value) :: rest ->
              let* () = operand block_id argument_ty value in
              validate_arguments rest
        in
        validate_arguments arguments
    | Call_indirect (result, ret_extension, ty, callee, arguments) ->
        let* () =
          match (result, ty) with
          | None, Void
          | ( Some _,
              ( I1 | I8 | I16 | I32 | I64 | I128 | F32 | F64 | Pointer _ | Vector _
              | Struct _ | Array _ ) ) ->
              Ok ()
          | None, _ -> fail "block %d discards a non-void call result" block_id
          | Some _, Void -> fail "block %d assigns a void call result" block_id
        in
        let* () = pointer_operand block_id callee in
        if not (valid_extension ty ret_extension) then
          fail "block %d has an invalid indirect call result extension" block_id
        else
          let rec validate_arguments = function
            | [] -> Ok ()
            | (argument_ty, extension, value) :: rest ->
                if not (valid_extension argument_ty extension) then
                  fail "block %d has an invalid indirect call argument extension"
                    block_id
                else
                  let* () = operand block_id argument_ty value in
                  validate_arguments rest
          in
          validate_arguments arguments
    | Phi (_, ty, incoming) -> validate_phi block_id ty incoming
    | Select (_, condition, yes, no) ->
        let selected = value_ty yes in
        let* () = validate_reference block_id condition in
        let valid_condition =
          match (value_ty condition, selected) with
          | I1, _ -> true
          | Vector (condition_lanes, I1), Vector (value_lanes, _) ->
              condition_lanes = value_lanes
          | _ -> false
        in
        if not valid_condition then
          fail "block %d has an invalid select condition" block_id
        else
          let* () = validate_reference block_id yes in
          operand block_id selected no
    | Extract (_, vector_ty, vector, index) -> (
        match vector_ty with
        | Vector _ ->
            let* () = operand block_id vector_ty vector in
            integer_operand block_id index
        | _ -> fail "block %d extracts from a non-vector type" block_id)
    | Insert (_, vector_ty, vector, index, value) -> (
        match vector_ty with
        | Vector (_, element_ty) ->
            let* () = operand block_id vector_ty vector in
            let* () = integer_operand block_id index in
            operand block_id element_ty value
        | _ -> fail "block %d inserts into a non-vector type" block_id)
    | Shuffle_zero (_, vector_ty, vector) -> (
        match vector_ty with
        | Vector _ -> operand block_id vector_ty vector
        | _ -> fail "block %d shuffles a non-vector type" block_id)
    | Shufflevector (_, vector_ty, a, b, mask) -> (
        match (vector_ty, value_ty a, value_ty b) with
        | Vector (m, _), (Vector _ as ta), (Vector _ as tb) ->
            let* () = operand block_id ta a in
            let* () = operand block_id tb b in
            operand block_id (Vector (m, I32)) mask
        | _ -> fail "block %d shuffles a non-vector type" block_id)
    | String_ptr (_, index, length) -> (
        let name = ".str." ^ string_of_int index in
        match Hashtbl.find_opt globals name with
        | Some (Array (actual_length, I8)) when actual_length = length -> Ok ()
        | Some (Array (_, I8)) ->
            fail "block %d string pointer `%s` has the wrong length" block_id name
        | Some _ ->
            fail "block %d string pointer `%s` has the wrong global type" block_id name
        | None -> fail "block %d references unknown string global `%s`" block_id name)
    | Global_ptr (_, name, ty) -> (
        match Hashtbl.find_opt globals name with
        | None -> fail "block %d references unknown global `%s`" block_id name
        | Some actual when type_equal actual ty -> Ok ()
        | Some _ -> fail "block %d global pointer `%s` has the wrong type" block_id name
        )
    | Trap -> Ok ()
    | Location (line, column) ->
        if line < 0 || column < 0 then
          fail "block %d has a negative source location" block_id
        else Ok ()
  in
  let validate_phi_order block_id instructions =
    let rec validate seen_non_phi = function
      | [] -> Ok ()
      | Phi _ :: _ when seen_non_phi ->
          fail "block %d has a phi after a non-phi instruction" block_id
      | Phi _ :: rest -> validate false rest
      | Location _ :: rest -> validate seen_non_phi rest
      | _ :: rest -> validate true rest
    in
    validate false instructions
  in
  let rec validate_instructions block_id = function
    | [] -> Ok ()
    | instruction :: rest ->
        let* () = validate_instruction block_id instruction in
        validate_instructions block_id rest
  in
  let validate_terminator block_id = function
    | Ret None ->
        if func.ret = Void then Ok ()
        else fail "block %d returns void from a non-void function" block_id
    | Ret (Some (ty, value)) ->
        if func.ret = Void then
          fail "block %d returns a value from a void function" block_id
        else if not (type_equal func.ret ty) then
          fail "block %d returns the wrong type" block_id
        else operand block_id ty value
    | Br _ | Unreachable -> Ok ()
    | CondBr (condition, _, _) -> operand block_id I1 condition
    | Switch (ty, value, cases, _) ->
        if not (is_integer ty) then
          fail "block %d switches on a non-integer type" block_id
        else
          let* () = operand block_id ty value in
          let rec unique seen = function
            | [] -> Ok ()
            | (case, _) :: rest ->
                if Int64_set.mem case seen then
                  fail "block %d has duplicate switch case %Ld" block_id case
                else if integer_constant_fits ty case then
                  unique (Int64_set.add case seen) rest
                else fail "block %d has a switch case outside its type" block_id
          in
          unique Int64_set.empty cases
  in
  let rec validate_blocks = function
    | [] -> Ok ()
    | (block : block) :: rest ->
        let* () = validate_phi_order block.id block.instrs in
        let* () = validate_instructions block.id block.instrs in
        let* () = validate_terminator block.id block.terminator in
        validate_blocks rest
  in
  let reachable = Hashtbl.create (List.length func.blocks) in
  let compute_dominators () =
    match func.blocks with
    | [] -> Hashtbl.create 0
    | (entry : block) :: _ ->
        let block_count = List.length func.blocks in
        let vertex = Array.make (block_count + 1) 0 in
        let parent = Array.make (block_count + 1) 0 in
        let dfs_index = Hashtbl.create block_count in
        vertex.(1) <- entry.id;
        Hashtbl.add dfs_index entry.id 1;
        Hashtbl.add reachable entry.id ();
        let entry_block = Hashtbl.find blocks_by_id entry.id in
        let stack = ref [ (entry.id, terminator_successors entry_block.terminator) ] in
        let count = ref 1 in
        while !stack <> [] do
          match !stack with
          | (block_id, successor :: rest) :: remaining ->
              stack := (block_id, rest) :: remaining;
              if not (Hashtbl.mem dfs_index successor) then (
                incr count;
                vertex.(!count) <- successor;
                parent.(!count) <- Hashtbl.find dfs_index block_id;
                Hashtbl.add dfs_index successor !count;
                Hashtbl.add reachable successor ();
                let block = Hashtbl.find blocks_by_id successor in
                stack := (successor, terminator_successors block.terminator) :: !stack)
          | (_, []) :: remaining -> stack := remaining
          | [] -> ()
        done;
        let semi = Array.init (block_count + 1) (fun index -> index) in
        let idom = Array.make (block_count + 1) 0 in
        let ancestor = Array.make (block_count + 1) 0 in
        let label = Array.init (block_count + 1) (fun index -> index) in
        let buckets = Array.make (block_count + 1) [] in
        let eval value =
          if ancestor.(value) = 0 then label.(value)
          else
            let path = ref [] in
            let current = ref value in
            while ancestor.(ancestor.(!current)) <> 0 do
              path := !current :: !path;
              current := ancestor.(!current)
            done;
            List.iter
              (fun node ->
                let previous = ancestor.(node) in
                if semi.(label.(previous)) < semi.(label.(node)) then
                  label.(node) <- label.(previous);
                ancestor.(node) <- ancestor.(previous))
              !path;
            let previous = ancestor.(value) in
            if previous <> 0 && semi.(label.(previous)) < semi.(label.(value)) then
              label.(previous)
            else label.(value)
        in
        for index = !count downto 2 do
          let block_id = vertex.(index) in
          let preds =
            Option.value ~default:[] (Hashtbl.find_opt predecessors block_id)
          in
          List.iter
            (fun predecessor ->
              match Hashtbl.find_opt dfs_index predecessor with
              | None -> ()
              | Some predecessor_index ->
                  let candidate = eval predecessor_index in
                  semi.(index) <- min semi.(index) semi.(candidate))
            preds;
          buckets.(semi.(index)) <- index :: buckets.(semi.(index));
          ancestor.(index) <- parent.(index);
          List.iter
            (fun node ->
              let candidate = eval node in
              idom.(node) <-
                (if semi.(candidate) < semi.(node) then candidate else parent.(index)))
            buckets.(parent.(index));
          buckets.(parent.(index)) <- []
        done;
        for index = 2 to !count do
          if idom.(index) <> semi.(index) then idom.(index) <- idom.(idom.(index))
        done;
        idom.(1) <- 1;
        let children = Array.make (block_count + 1) [] in
        for index = 2 to !count do
          children.(idom.(index)) <- index :: children.(idom.(index))
        done;
        let before = Array.make (block_count + 1) 0 in
        let after = Array.make (block_count + 1) 0 in
        let clock = ref 1 in
        before.(1) <- 1;
        let tree_stack = ref [ (1, children.(1)) ] in
        while !tree_stack <> [] do
          match !tree_stack with
          | (node, child :: rest) :: remaining ->
              tree_stack := (node, rest) :: remaining;
              incr clock;
              before.(child) <- !clock;
              tree_stack := (child, children.(child)) :: !tree_stack
          | (node, []) :: remaining ->
              after.(node) <- !clock;
              tree_stack := remaining
          | [] -> ()
        done;
        let intervals = Hashtbl.create !count in
        for index = 1 to !count do
          Hashtbl.add intervals vertex.(index) (before.(index), after.(index))
        done;
        intervals
  in
  let validate_dominance () =
    let dominator_intervals = compute_dominators () in
    let validate_use report_block use_block use_index = function
      | Local (id, _) -> (
          match Hashtbl.find_opt definition_locations id with
          | None -> Ok ()
          | Some (definition_block, definition_index) ->
              let dominates =
                if not (Hashtbl.mem reachable use_block) then true
                else if definition_block = use_block then definition_index < use_index
                else
                  match
                    ( Hashtbl.find_opt dominator_intervals definition_block,
                      Hashtbl.find_opt dominator_intervals use_block )
                  with
                  | ( Some (definition_before, definition_after),
                      Some (use_before, use_after) ) ->
                      definition_before <= use_before && use_after <= definition_after
                  | _ -> false
              in
              if dominates then Ok ()
              else fail "block %d value %d does not dominate its use" report_block id)
      | Const _ | Const_vector _ | Null _ | Undef _ | Zero _ | Param _ | Global _ ->
          Ok ()
    in
    let rec validate_uses report_block use_block use_index = function
      | [] -> Ok ()
      | value :: rest ->
          let* () = validate_use report_block use_block use_index value in
          validate_uses report_block use_block use_index rest
    in
    let instruction_uses = function
      | Bin (_, _, _, left, right) | Cmp (_, _, _, left, right) -> [ left; right ]
      | Fneg (_, _, value) -> [ value ]
      | Load (_, _, pointer, _) -> [ pointer ]
      | Load_volatile (_, _, pointer, _) -> [ pointer ]
      | Store (_, value, pointer, _) -> [ value; pointer ]
      | Store_volatile (_, value, pointer, _) -> [ value; pointer ]
      | Gep (_, _, pointer, indices) ->
          pointer
          :: List.filter_map
               (function Zero -> None | Index value -> Some value)
               indices
      | Cast (_, _, _, value, _) -> [ value ]
      | Call (_, _, _, _, arguments) -> List.map (fun (_, _, value) -> value) arguments
      | Call_indirect (_, _, _, callee, arguments) ->
          callee :: List.map (fun (_, _, value) -> value) arguments
      | Select (_, condition, yes, no) -> [ condition; yes; no ]
      | Extract (_, _, vector, index) -> [ vector; index ]
      | Insert (_, _, vector, index, value) -> [ vector; index; value ]
      | Shuffle_zero (_, _, vector) -> [ vector ]
      | Shufflevector (_, _, a, b, mask) -> [ a; b; mask ]
      | Alloca _ | Phi _ | String_ptr _ | Global_ptr _ | Trap | Location _ -> []
    in
    let rec validate_phi_uses block_id = function
      | [] -> Ok ()
      | (value, predecessor) :: rest ->
          let predecessor_block = Hashtbl.find blocks_by_id predecessor in
          let* () =
            validate_use block_id predecessor
              (List.length predecessor_block.instrs)
              value
          in
          validate_phi_uses block_id rest
    in
    let rec validate_instructions block_id instruction_index = function
      | [] -> Ok ()
      | Phi (_, _, incoming) :: rest ->
          let* () = validate_phi_uses block_id incoming in
          validate_instructions block_id (instruction_index + 1) rest
      | instruction :: rest ->
          let* () =
            validate_uses block_id block_id instruction_index
              (instruction_uses instruction)
          in
          validate_instructions block_id (instruction_index + 1) rest
    in
    let terminator_uses = function
      | Ret None | Br _ | Unreachable -> []
      | Ret (Some (_, value)) -> [ value ]
      | CondBr (condition, _, _) -> [ condition ]
      | Switch (_, value, _, _) -> [ value ]
    in
    let rec validate_block_uses = function
      | [] -> Ok ()
      | (block : block) :: rest ->
          let* () = validate_instructions block.id 0 block.instrs in
          let* () =
            validate_uses block.id block.id (List.length block.instrs)
              (terminator_uses block.terminator)
          in
          validate_block_uses rest
    in
    validate_block_uses func.blocks
  in
  let* () =
    if func.ret = Void || valid_module_value_type func.ret then Ok ()
    else fail "has an invalid return type"
  in
  let* () =
    if valid_extension func.ret func.ret_extension then Ok ()
    else fail "has an invalid return extension"
  in
  let* () =
    match func.source with
    | Some (_, line) when line < 0 -> fail "has a negative source line"
    | Some _ | None -> Ok ()
  in
  let* () = collect_blocks func.blocks in
  let* () = collect_parameters func.params in
  let* () = collect_edges func.blocks in
  let* () = validate_entry_predecessors () in
  let* () = collect_definitions func.blocks in
  let* () = validate_blocks func.blocks in
  validate_dominance ()

let validate module_ =
  let struct_names = Hashtbl.create (List.length module_.structs) in
  let rec collect_structs = function
    | [] -> Ok ()
    | (struct_def : struct_def) :: rest ->
        if Hashtbl.mem struct_names struct_def.name then
          Error ("module has duplicate struct name `" ^ struct_def.name ^ "`")
        else (
          Hashtbl.add struct_names struct_def.name struct_def;
          collect_structs rest)
  in
  let rec references_defined_type = function
    | Struct name -> Hashtbl.mem struct_names name
    | Array (_, elem) | Vector (_, elem) -> references_defined_type elem
    | Pointer _ | I1 | I8 | I16 | I32 | I64 | I128 | F32 | F64 | Void -> true
  in
  let validate_struct (struct_def : struct_def) =
    let rec validate_fields = function
      | [] -> Ok ()
      | field :: rest ->
          if not (valid_value_type field) then
            Error ("struct `" ^ struct_def.name ^ "` has an invalid field type")
          else if not (references_defined_type field) then
            Error ("struct `" ^ struct_def.name ^ "` references an unknown struct")
          else validate_fields rest
    in
    if struct_def.tail_padding < 0 then
      Error ("struct `" ^ struct_def.name ^ "` has negative tail padding")
    else validate_fields struct_def.fields
  in
  let rec validate_structs = function
    | [] -> Ok ()
    | struct_def :: rest ->
        let* () = validate_struct struct_def in
        validate_structs rest
  in
  let struct_states = Hashtbl.create (List.length module_.structs) in
  let rec validate_struct_cycles name =
    match Hashtbl.find_opt struct_states name with
    | Some 2 -> Ok ()
    | Some 1 -> Error ("struct `" ^ name ^ "` has a recursive value layout")
    | _ ->
        Hashtbl.replace struct_states name 1;
        let struct_def = Hashtbl.find struct_names name in
        let rec validate_type = function
          | Struct referenced -> validate_struct_cycles referenced
          | Array (_, elem) | Vector (_, elem) -> validate_type elem
          | Pointer _ | I1 | I8 | I16 | I32 | I64 | I128 | F32 | F64 | Void -> Ok ()
        in
        let rec validate_fields = function
          | [] -> Ok ()
          | field :: rest ->
              let* () = validate_type field in
              validate_fields rest
        in
        let* () = validate_fields struct_def.fields in
        Hashtbl.replace struct_states name 2;
        Ok ()
  in
  let rec validate_all_struct_cycles = function
    | [] -> Ok ()
    | (struct_def : struct_def) :: rest ->
        let* () = validate_struct_cycles struct_def.name in
        validate_all_struct_cycles rest
  in
  let globals = Hashtbl.create (List.length module_.globals) in
  let valid_alignment alignment = alignment > 0 && alignment land (alignment - 1) = 0 in
  let global_name = function
    | String_global { name; _ } | Array_global { name; _ } | Storage_global { name; _ }
      ->
        name
  in
  let global_type = function
    | String_global { bytes; _ } -> Array (String.length bytes, I8)
    | Array_global { elem_ty; elems; _ } -> Array (List.length elems, elem_ty)
    | Storage_global { storage_ty; _ } -> storage_ty
  in
  let validate_global = function
    | String_global _ -> Ok ()
    | Array_global { name; elem_ty; elems; align } ->
        let constant_fits =
          match elem_ty with
          | F32 | F64 -> Fun.const true
          | _ -> integer_constant_fits elem_ty
        in
        if not (is_integer elem_ty || elem_ty = F32 || elem_ty = F64) then
          Error ("global `" ^ name ^ "` has an unsupported element type")
        else if not (List.for_all constant_fits elems) then
          Error ("global `" ^ name ^ "` has an element outside its type")
        else if not (valid_alignment align) then
          Error (Printf.sprintf "global `%s` has invalid alignment %d" name align)
        else Ok ()
    | Storage_global
        { name; storage_ty; size; bytes; pointers; readonly; align; linkage } ->
        let valid_storage =
          valid_value_type storage_ty
          && references_defined_type storage_ty
          && size >= 0
          && Option.fold ~none:true ~some:(fun data -> String.length data = size) bytes
          && valid_alignment align
          && List.for_all
               (fun (offset, symbol, _) ->
                 offset >= 0
                 && offset <= size - (Target_layout.current ()).pointer_size
                 && (List.exists
                       (fun global -> global_name global = symbol)
                       module_.globals
                    || List.exists
                         (fun (func : func) -> func.name = symbol)
                         module_.funcs))
               pointers
          && (let rec ordered = function
                | (offset, _, _) :: ((next, _, _) :: _ as rest) ->
                    next >= offset + (Target_layout.current ()).pointer_size
                    && ordered rest
                | _ -> true
              in
              ordered pointers)
          && ((not readonly) || linkage = Ast.Internal_global)
          &&
          match (linkage, bytes) with
          | (Ast.Import_c | Ast.Import_const_c), None
          | Ast.Export_c, Some _
          | Ast.Internal_global, _ ->
              true
          | _ -> false
        in
        if not valid_storage then
          Error (Printf.sprintf "global `%s` has an invalid storage declaration" name)
        else Ok ()
  in
  let rec collect_globals = function
    | [] -> Ok ()
    | global :: rest ->
        let name = global_name global in
        if Hashtbl.mem globals name then
          Error ("module has duplicate global name `" ^ name ^ "`")
        else
          let* () = validate_global global in
          Hashtbl.add globals name (global_type global);
          collect_globals rest
  in
  let functions = Hashtbl.create (List.length module_.funcs) in
  let rec collect_functions = function
    | [] -> Ok ()
    | (func : func) :: rest ->
        if Hashtbl.mem globals func.name then
          Error ("module symbol `" ^ func.name ^ "` is both a global and a function")
        else if Hashtbl.mem functions func.name then
          Error ("module has duplicate function name `" ^ func.name ^ "`")
        else (
          Hashtbl.add functions func.name func;
          collect_functions rest)
  in
  let validate_call (caller : func) block_id result_extension result_ty target_name
      arguments =
    let fail format =
      Printf.ksprintf
        (fun message ->
          Error
            (Printf.sprintf "function `%s` block %d call to `%s` %s" caller.name
               block_id target_name message))
        format
    in
    match Hashtbl.find_opt functions target_name with
    | None -> fail "has no matching function"
    | Some target ->
        if not (type_equal result_ty target.ret) then fail "has the wrong return type"
        else if result_extension <> target.ret_extension then
          fail "has the wrong return extension"
        else if not (valid_extension result_ty result_extension) then
          fail "has an invalid return extension"
        else
          let fixed_count = List.length target.params in
          let argument_count = List.length arguments in
          if argument_count < fixed_count then
            fail "has %d arguments but requires at least %d" argument_count fixed_count
          else if (not target.variadic) && argument_count <> fixed_count then
            fail "has %d arguments but requires exactly %d" argument_count fixed_count
          else
            let rec validate_arguments index params args =
              match (params, args) with
              | [], rest ->
                  let rec validate_variadic index = function
                    | [] -> Ok ()
                    | (ty, extension, _) :: tail ->
                        if valid_extension ty extension then
                          validate_variadic (index + 1) tail
                        else fail "argument %d has an invalid extension" index
                  in
                  validate_variadic index rest
              | (parameter : param) :: params, (ty, extension, _) :: args ->
                  if not (type_equal ty parameter.ty) then
                    fail "argument %d has the wrong type" index
                  else if extension <> parameter.extension then
                    fail "argument %d has the wrong extension" index
                  else if not (valid_extension ty extension) then
                    fail "argument %d has an invalid extension" index
                  else validate_arguments (index + 1) params args
              | _ :: _, [] -> fail "is missing fixed argument %d" index
            in
            validate_arguments 0 target.params arguments
  in
  let validate_function_calls (func : func) =
    let rec validate_instructions block_id = function
      | [] -> Ok ()
      | Call (_, extension, ty, name, arguments) :: rest ->
          let* () = validate_call func block_id extension ty name arguments in
          validate_instructions block_id rest
      | _ :: rest -> validate_instructions block_id rest
    in
    let rec validate_blocks = function
      | [] -> Ok ()
      | (block : block) :: rest ->
          let* () = validate_instructions block.id block.instrs in
          validate_blocks rest
    in
    validate_blocks func.blocks
  in
  let rec validate_functions = function
    | [] -> Ok ()
    | func :: rest ->
        let* () = validate_function struct_names globals functions func in
        let* () = validate_function_calls func in
        validate_functions rest
  in
  let* () = collect_structs module_.structs in
  let* () = validate_structs module_.structs in
  let* () = validate_all_struct_cycles module_.structs in
  let* () = collect_globals module_.globals in
  let* () = collect_functions module_.funcs in
  let* () =
    List.fold_left
      (fun result name ->
        Result.bind result (fun () ->
            match Hashtbl.find_opt functions name with
            | Some func when func.blocks <> [] -> Ok ()
            | Some _ -> Error ("no-inline function `" ^ name ^ "` has no definition")
            | None -> Error ("no-inline function `" ^ name ^ "` does not exist")))
      (Ok ()) module_.no_inline_functions
  in
  validate_functions module_.funcs

let extension_name = function
  | No_extension -> ""
  | Sign_extension -> "signext "
  | Zero_extension -> "zeroext "

let extension_attr = function
  | No_extension -> ""
  | Sign_extension -> " signext"
  | Zero_extension -> " zeroext"

exception Render_exhausted

type sink = { text : string -> unit; room : int -> bool; redirect : string -> string }

let emit_escaped_identifier sink n =
  let len = String.length n in
  let rec run i =
    if i < len then (
      let stop = min len (i + 256) in
      sink.text (String.escaped (String.sub n i (stop - i)));
      run stop)
  in
  run 0

let emit_symbol sink n =
  let n = sink.redirect n in
  if not (sink.room (String.length n + 1)) then raise Render_exhausted;
  if String.for_all safe_identifier_char n then (
    sink.text "@";
    sink.text n)
  else (
    sink.text "@\"";
    emit_escaped_identifier sink n;
    sink.text "\"")

let emit_struct_name sink n =
  if not (sink.room (String.length n + 8)) then raise Render_exhausted;
  if String.for_all safe_identifier_char n then (
    sink.text "%struct.";
    sink.text n)
  else (
    sink.text "%\"struct.";
    emit_escaped_identifier sink n;
    sink.text "\"")

let emit_ty sink t =
  let rec go = function
    | I1 -> sink.text "i1"
    | I8 -> sink.text "i8"
    | I16 -> sink.text "i16"
    | I32 -> sink.text "i32"
    | I64 -> sink.text "i64"
    | I128 -> sink.text "i128"
    | F32 -> sink.text "float"
    | F64 -> sink.text "double"
    | Pointer _ -> sink.text "ptr"
    | Vector (n, t) ->
        sink.text "<";
        sink.text (string_of_int n);
        sink.text " x ";
        go t;
        sink.text ">"
    | Struct n -> emit_struct_name sink n
    | Array (n, t) ->
        sink.text "[";
        sink.text (string_of_int n);
        sink.text " x ";
        go t;
        sink.text "]"
    | Void -> sink.text "void"
  in
  go t

let float_constant_text ty bits =
  if
    ty = F32
    && Int64.logand bits 0x7fc00000L = 0x7f800000L
    && Int64.logand bits 0x3fffffL <> 0L
  then Printf.sprintf "bitcast (i32 %Ld to float)" (Int64.logand bits 0xffffffffL)
  else
    let bits =
      match ty with
      | F32 -> Int64.bits_of_float (Int32.float_of_bits (Int64.to_int32 bits))
      | F64 -> bits
      | _ -> invalid_arg "internal error: float constant has a non-float type"
    in
    Printf.sprintf "0x%016LX" bits

let emit_value sink = function
  | Const (I1, 0L) -> sink.text "false"
  | Const (I1, _) -> sink.text "true"
  | Const (((F32 | F64) as ty), bits) -> sink.text (float_constant_text ty bits)
  | Const (_, v) -> sink.text (Int64.to_string v)
  | Const_vector (Vector (_, element_ty), values) ->
      sink.text "<";
      List.iteri
        (fun i value ->
          if i > 0 then sink.text ", ";
          emit_ty sink element_ty;
          sink.text " ";
          sink.text
            (if element_ty = F32 || element_ty = F64 then
               float_constant_text element_ty value
             else Int64.to_string value))
        values;
      sink.text ">"
  | Const_vector (_, _) -> invalid_arg "vector constant requires a vector type"
  | Null _ -> sink.text "null"
  | Undef _ -> sink.text "poison"
  | Zero _ -> sink.text "zeroinitializer"
  | Local (i, _) ->
      sink.text "%v";
      sink.text (string_of_int i)
  | Param (n, _) ->
      sink.text "%";
      sink.text n
  | Global (n, _) -> emit_symbol sink n

let bin_name = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Sdiv -> "sdiv"
  | Srem -> "srem"
  | Udiv -> "udiv"
  | Urem -> "urem"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Shl -> "shl"
  | Lshr -> "lshr"
  | Ashr -> "ashr"
  | Fadd -> "fadd"
  | Fsub -> "fsub"
  | Fmul -> "fmul"
  | Fdiv -> "fdiv"

let cmp_name = function
  | Eq -> "eq"
  | Ne -> "ne"
  | Slt -> "slt"
  | Sle -> "sle"
  | Sgt -> "sgt"
  | Sge -> "sge"
  | Ult -> "ult"
  | Ule -> "ule"
  | Ugt -> "ugt"
  | Uge -> "uge"
  | Foeq -> "oeq"
  | Fune -> "une"
  | Folt -> "olt"
  | Fole -> "ole"
  | Fogt -> "ogt"
  | Foge -> "oge"

let emit_instr variadic_signatures sink = function
  | Bin (i, op, t, a, b) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = ";
      sink.text (bin_name op);
      sink.text " ";
      emit_ty sink t;
      sink.text " ";
      emit_value sink a;
      sink.text ", ";
      emit_value sink b
  | Fneg (i, t, value) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = fneg ";
      emit_ty sink t;
      sink.text " ";
      emit_value sink value
  | Cmp (i, c, t, a, b) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text
        (if c = Foeq || c = Fune || c = Folt || c = Fole || c = Fogt || c = Foge then
           " = fcmp "
         else " = icmp ");
      sink.text (cmp_name c);
      sink.text " ";
      emit_ty sink t;
      sink.text " ";
      emit_value sink a;
      sink.text ", ";
      emit_value sink b
  | Alloca (i, t, a) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = alloca ";
      emit_ty sink t;
      sink.text ", align ";
      sink.text (string_of_int a)
  | Load (i, t, p, a) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = load ";
      emit_ty sink t;
      sink.text ", ptr ";
      emit_value sink p;
      sink.text ", align ";
      sink.text (string_of_int a)
  | Load_volatile (i, t, p, a) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = load volatile ";
      emit_ty sink t;
      sink.text ", ptr ";
      emit_value sink p;
      sink.text ", align ";
      sink.text (string_of_int a)
  | Store (t, v, p, a) ->
      sink.text "  store ";
      emit_ty sink t;
      sink.text " ";
      emit_value sink v;
      sink.text ", ptr ";
      emit_value sink p;
      sink.text ", align ";
      sink.text (string_of_int a)
  | Store_volatile (t, v, p, a) ->
      sink.text "  store volatile ";
      emit_ty sink t;
      sink.text " ";
      emit_value sink v;
      sink.text ", ptr ";
      emit_value sink p;
      sink.text ", align ";
      sink.text (string_of_int a)
  | Gep (i, t, p, idxs) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = getelementptr ";
      emit_ty sink t;
      sink.text ", ptr ";
      emit_value sink p;
      sink.text ", ";
      List.iteri
        (fun i idx ->
          if i > 0 then sink.text ", ";
          match idx with
          | Zero -> sink.text "i64 0"
          | Index v ->
              emit_ty sink (value_ty v);
              sink.text " ";
              emit_value sink v)
        idxs
  | Cast (i, k, st, v, dt) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = ";
      sink.text k;
      sink.text " ";
      emit_ty sink st;
      sink.text " ";
      emit_value sink v;
      sink.text " to ";
      emit_ty sink dt
  | Call (result, extension, t, n, args) ->
      (match result with
      | Some i ->
          sink.text "  %v";
          sink.text (string_of_int i);
          sink.text " = call "
      | None -> sink.text "  call ");
      sink.text (extension_name extension);
      emit_ty sink t;
      (match Hashtbl.find_opt variadic_signatures n with
      | None -> ()
      | Some params ->
          sink.text " (";
          List.iteri
            (fun i (parameter : param) ->
              if i > 0 then sink.text ", ";
              emit_ty sink parameter.ty)
            params;
          if params <> [] then sink.text ", ";
          sink.text "...)");
      sink.text " ";
      emit_symbol sink n;
      sink.text "(";
      List.iteri
        (fun i (t, extension, v) ->
          if i > 0 then sink.text ", ";
          emit_ty sink t;
          sink.text " ";
          sink.text (extension_name extension);
          emit_value sink v)
        args;
      sink.text ")"
  | Call_indirect (result, extension, t, callee, args) ->
      (match result with
      | Some i ->
          sink.text "  %v";
          sink.text (string_of_int i);
          sink.text " = call "
      | None -> sink.text "  call ");
      sink.text (extension_name extension);
      emit_ty sink t;
      sink.text " ";
      emit_value sink callee;
      sink.text "(";
      List.iteri
        (fun i (argument_ty, argument_extension, value) ->
          if i > 0 then sink.text ", ";
          emit_ty sink argument_ty;
          sink.text " ";
          sink.text (extension_name argument_extension);
          emit_value sink value)
        args;
      sink.text ")"
  | Phi (i, t, xs) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = phi ";
      emit_ty sink t;
      sink.text " ";
      List.iteri
        (fun i (v, b) ->
          if i > 0 then sink.text ", ";
          sink.text "[ ";
          emit_value sink v;
          sink.text ", %b";
          sink.text (string_of_int b);
          sink.text " ]")
        xs
  | Select (i, c, a, b) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = select ";
      emit_ty sink (value_ty c);
      sink.text " ";
      emit_value sink c;
      sink.text ", ";
      emit_ty sink (value_ty a);
      sink.text " ";
      emit_value sink a;
      sink.text ", ";
      emit_ty sink (value_ty b);
      sink.text " ";
      emit_value sink b
  | Extract (i, vt, v, l) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = extractelement ";
      emit_ty sink vt;
      sink.text " ";
      emit_value sink v;
      sink.text ", ";
      emit_ty sink (value_ty l);
      sink.text " ";
      emit_value sink l
  | Insert (i, vt, v, l, x) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = insertelement ";
      emit_ty sink vt;
      sink.text " ";
      emit_value sink v;
      sink.text ", ";
      emit_ty sink (value_ty x);
      sink.text " ";
      emit_value sink x;
      sink.text ", ";
      emit_ty sink (value_ty l);
      sink.text " ";
      emit_value sink l
  | Shuffle_zero (i, vt, v) ->
      let n = match vt with Vector (n, _) -> n | _ -> 0 in
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = shufflevector ";
      emit_ty sink vt;
      sink.text " ";
      emit_value sink v;
      sink.text ", ";
      emit_ty sink vt;
      sink.text " zeroinitializer, <";
      sink.text (string_of_int n);
      sink.text " x i32> zeroinitializer"
  | Shufflevector (i, _vt, a, b, mask) ->
      let ta = value_ty a in
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = shufflevector ";
      emit_ty sink ta;
      sink.text " ";
      emit_value sink a;
      sink.text ", ";
      emit_ty sink ta;
      sink.text " ";
      emit_value sink b;
      sink.text ", ";
      emit_ty sink (value_ty mask);
      sink.text " ";
      emit_value sink mask
  | String_ptr (i, index, n) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = getelementptr [";
      sink.text (string_of_int n);
      sink.text " x i8], ptr @.str.";
      sink.text (string_of_int index);
      sink.text ", i64 0, i64 0"
  | Global_ptr (i, n, t) ->
      sink.text "  %v";
      sink.text (string_of_int i);
      sink.text " = getelementptr ";
      emit_ty sink t;
      sink.text ", ptr @";
      sink.text n;
      sink.text ", i64 0"
  | Trap -> sink.text "  call void @llvm.trap()"
  | Location _ -> invalid_arg "internal error: location markers are not instructions"

let instr_line variadic_signatures instr =
  let buffer = Buffer.create 64 in
  emit_instr variadic_signatures
    { text = Buffer.add_string buffer; room = (fun _ -> true); redirect = Fun.id }
    instr;
  Buffer.contents buffer

let emit_term sink = function
  | Ret None -> sink.text "  ret void"
  | Ret (Some (t, v)) ->
      sink.text "  ret ";
      emit_ty sink t;
      sink.text " ";
      emit_value sink v
  | Br b ->
      sink.text "  br label %b";
      sink.text (string_of_int b)
  | CondBr (c, a, b) ->
      sink.text "  br i1 ";
      emit_value sink c;
      sink.text ", label %b";
      sink.text (string_of_int a);
      sink.text ", label %b";
      sink.text (string_of_int b)
  | Switch (t, v, cases, d) ->
      sink.text "  switch ";
      emit_ty sink t;
      sink.text " ";
      emit_value sink v;
      sink.text ", label %b";
      sink.text (string_of_int d);
      sink.text " [";
      sink.text " ";
      List.iteri
        (fun i (k, b) ->
          if i > 0 then sink.text " ";
          emit_ty sink t;
          sink.text " ";
          sink.text (Int64.to_string k);
          sink.text ", label %b";
          sink.text (string_of_int b))
        cases;
      sink.text " ]"
  | Unreachable -> sink.text "  unreachable"

let term_line terminator =
  let buffer = Buffer.create 64 in
  emit_term
    { text = Buffer.add_string buffer; room = (fun _ -> true); redirect = Fun.id }
    terminator;
  Buffer.contents buffer

let absolute_directory path =
  match Filename.dirname path with
  | "." -> Sys.getcwd ()
  | dir when Filename.is_relative dir -> Filename.concat (Sys.getcwd ()) dir
  | dir -> dir

type target_group = {
  target_cpu : string;
  target_tune : string option;
  target_features : string option;
}

let render_bounded ?(redirect = Fun.id) ?(sanitize_address = false)
    ?(debug_info = false) ?target ~budget m =
  if budget < 0 then Error "rendered LLVM text budget must not be negative"
  else
    let buffer = Buffer.create 4096 in
    let debug_bytes = ref 0 and in_debug = ref false in
    let counted () = Buffer.length buffer - !debug_bytes in
    let add s =
      if !in_debug then (
        debug_bytes := !debug_bytes + String.length s;
        Buffer.add_string buffer s)
      else if counted () > budget - String.length s then raise Render_exhausted
      else Buffer.add_string buffer s
    in
    let debug f =
      in_debug := true;
      f ();
      in_debug := false
    in
    let sink = { text = add; room = (fun n -> counted () <= budget - n); redirect } in
    let variadic_signatures = Hashtbl.create (List.length m.funcs) in
    List.iter
      (fun (f : func) ->
        if f.variadic then Hashtbl.replace variadic_signatures f.name f.params)
      m.funcs;
    let newline () = add "\n" in
    let add_escaped_bytes s =
      let len = String.length s in
      let plain j =
        let n = Char.code s.[j] in
        n >= 32 && n < 127 && s.[j] <> '"' && s.[j] <> '\\'
      in
      let rec run i =
        if i < len then
          if plain i then (
            let rec scan j =
              if j < len && j - i < 256 && plain j then scan (j + 1) else j
            in
            let stop = scan i in
            add (String.sub s i (stop - i));
            run stop)
          else (
            add (Printf.sprintf "\\%02X" (Char.code s.[i]));
            run (i + 1))
      in
      run 0
    in
    let add_params named variadic params =
      let emitted = ref false in
      List.iter
        (fun (p : param) ->
          if !emitted then add ", " else emitted := true;
          emit_ty sink p.ty;
          add (extension_attr p.extension);
          if named then (
            add " %";
            add p.name))
        params;
      if variadic then if !emitted then add ", ..." else add "..."
    in
    let called_names = Hashtbl.create (List.length m.funcs) in
    List.iter
      (fun f ->
        List.iter
          (fun b ->
            List.iter
              (function
                | Call (_, _, _, name, _) -> Hashtbl.replace called_names name ()
                | _ -> ())
              b.instrs)
          f.blocks)
      m.funcs;
    let no_builtin_names =
      let seen = Hashtbl.create (List.length m.funcs) in
      List.filter_map
        (fun (f : func) ->
          let is_called = Hashtbl.mem called_names f.name in
          if f.blocks = [] || (not is_called) || Hashtbl.mem seen f.name then None
          else (
            Hashtbl.add seen f.name ();
            Some f.name))
        m.funcs
    in
    let debug_source (f : func) =
      if debug_info && f.blocks <> [] then f.source else None
    in
    let file_ids = Hashtbl.create 4 in
    let file_order = ref [] in
    List.iter
      (fun f ->
        match debug_source f with
        | Some (path, _) when not (Hashtbl.mem file_ids path) ->
            file_order := path :: !file_order;
            Hashtbl.add file_ids path (Hashtbl.length file_ids + 1)
        | Some _ | None -> ())
      m.funcs;
    let flags_id = Hashtbl.length file_ids + 1 in
    let scope_base = flags_id + 2 in
    let scopes =
      let next = ref scope_base in
      List.map
        (fun f ->
          match debug_source f with
          | Some _ ->
              let id = !next in
              incr next;
              Some id
          | None -> None)
        m.funcs
    in
    let type_id = scope_base + List.length (List.filter Option.is_some scopes) in
    let next_location = ref (type_id + 1) in
    let locations = Hashtbl.create 16 in
    let location_order = ref [] in
    let location_id line column scope =
      match Hashtbl.find_opt locations (line, column, scope) with
      | Some id -> id
      | None ->
          let id = !next_location in
          incr next_location;
          Hashtbl.add locations (line, column, scope) id;
          location_order := (id, line, column, scope) :: !location_order;
          id
    in
    try
      add "; ModuleID = 'fas'";
      newline ();
      add "source_filename = \"fas\"";
      newline ();
      add "target datalayout = \"";
      add m.data_layout;
      add "\"";
      newline ();
      add "target triple = \"";
      add m.target_triple;
      add "\"";
      newline ();
      newline ();
      List.iter
        (fun s ->
          emit_struct_name sink s.name;
          add " = type { ";
          List.iteri
            (fun i ty ->
              if i > 0 then add ", ";
              emit_ty sink ty)
            s.fields;
          if s.tail_padding <> 0 then (
            if s.fields <> [] then add ", ";
            add "[";
            add (string_of_int s.tail_padding);
            add " x i8]");
          add " }";
          newline ())
        m.structs;
      List.iter
        (function
          | String_global { name; bytes } ->
              add "@";
              add name;
              add " = private unnamed_addr constant [";
              add (string_of_int (String.length bytes));
              add " x i8] c\"";
              add_escaped_bytes bytes;
              add "\"";
              newline ()
          | Array_global { name; elem_ty; elems; align } ->
              add "@";
              add name;
              add " = private unnamed_addr constant [";
              add (string_of_int (List.length elems));
              add " x ";
              emit_ty sink elem_ty;
              if List.for_all (( = ) 0L) elems then add "] zeroinitializer, align "
              else (
                add "] [";
                List.iteri
                  (fun i v ->
                    if i > 0 then add ", ";
                    emit_ty sink elem_ty;
                    add " ";
                    emit_value sink (Const (elem_ty, v)))
                  elems;
                add "], align ");
              add (string_of_int align);
              newline ()
          | Storage_global { name; size; bytes; pointers; readonly; align; linkage; _ }
            ->
              let linkage_text =
                match linkage with
                | Ast.Internal_global ->
                    if readonly then "private constant " else "internal global "
                | Ast.Export_c -> "global "
                | Ast.Import_c -> "external global "
                | Ast.Import_const_c -> "external constant "
              in
              (if pointers = [] then (
                 add (Printf.sprintf "@%s = %s[%d x i8]" name linkage_text size);
                 match bytes with
                 | None when linkage = Ast.Import_c || linkage = Ast.Import_const_c ->
                     ()
                 | None -> add " zeroinitializer"
                 | Some data when String.for_all (fun byte -> byte = '\000') data ->
                     add " zeroinitializer"
                 | Some data ->
                     add " c\"";
                     add_escaped_bytes data;
                     add "\"")
               else
                 let data = Option.get bytes in
                 let cursor = ref 0 in
                 let pieces = ref [] in
                 let bytes_until stop =
                   if stop > !cursor then
                     pieces :=
                       `Bytes (String.sub data !cursor (stop - !cursor)) :: !pieces;
                   cursor := stop
                 in
                 List.iter
                   (fun (offset, symbol, displacement) ->
                     bytes_until offset;
                     pieces := `Pointer (symbol, displacement) :: !pieces;
                     cursor := offset + (Target_layout.current ()).pointer_size)
                   pointers;
                 bytes_until size;
                 let pieces = List.rev !pieces in
                 let piece_ty = function
                   | `Bytes data -> Printf.sprintf "[%d x i8]" (String.length data)
                   | `Pointer _ -> "ptr"
                 in
                 add
                   (Printf.sprintf "@%s = %s<{ %s }> <{ " name linkage_text
                      (String.concat ", " (List.map piece_ty pieces)));
                 List.iteri
                   (fun index piece ->
                     if index > 0 then add ", ";
                     add (piece_ty piece);
                     match piece with
                     | `Bytes data ->
                         add " c\"";
                         add_escaped_bytes data;
                         add "\""
                     | `Pointer (symbol, 0) ->
                         add " ";
                         emit_symbol sink symbol
                     | `Pointer (symbol, offset) ->
                         add " getelementptr (i8, ptr ";
                         emit_symbol sink symbol;
                         add (Printf.sprintf ", i64 %d)" offset))
                   pieces;
                 add " }>");
              add (Printf.sprintf ", align %d\n" align))
        m.globals;
      List.iter2
        (fun (f : func) scope ->
          (if f.blocks = [] then (
             add "declare ";
             add (extension_name f.ret_extension);
             emit_ty sink f.ret;
             add " ";
             emit_symbol sink f.name;
             add "(";
             add_params (f.blocks <> []) f.variadic f.params;
             add ")";
             if no_builtin_names <> [] then add " #0")
           else
             let link = if f.linkage = Internal then "internal " else "" in
             let no_inline =
               if List.mem f.name m.no_inline_functions then " noinline" else ""
             in
             let sanitize_address =
               if sanitize_address then " sanitize_address" else ""
             in
             add "define ";
             add link;
             add (extension_name f.ret_extension);
             emit_ty sink f.ret;
             add " ";
             emit_symbol sink f.name;
             add "(";
             add_params (f.blocks <> []) f.variadic f.params;
             add ")";
             add no_inline;
             add sanitize_address;
             if no_builtin_names <> [] then add " #0";
             if target <> None then add " #1";
             Option.iter
               (fun sp -> debug (fun () -> add (Printf.sprintf " !dbg !%d" sp)))
               scope;
             add " {";
             newline ();
             let current =
               ref (match f.source with Some (_, line) -> (line, 0) | None -> (0, 0))
             in
             let suffix () =
               match scope with
               | None -> ()
               | Some sp ->
                   let line, column = !current in
                   debug (fun () ->
                       add ", !dbg !";
                       add (string_of_int (location_id line column sp)))
             in
             List.iteri
               (fun i b ->
                 if i > 0 then newline ();
                 add "b";
                 add (string_of_int b.id);
                 add ":";
                 List.iter
                   (function
                     | Location (line, column) -> current := (line, column)
                     | instr ->
                         newline ();
                         emit_instr variadic_signatures sink instr;
                         suffix ())
                   b.instrs;
                 newline ();
                 emit_term sink b.terminator;
                 suffix ())
               f.blocks;
             newline ();
             add "}");
          newline ())
        m.funcs scopes;
      if no_builtin_names <> [] then (
        add "attributes #0 = {";
        List.iter
          (fun name ->
            add " \"no-builtin-";
            add_escaped_bytes name;
            add "\"")
          no_builtin_names;
        add " }";
        newline ());
      Option.iter
        (fun group ->
          add "attributes #1 = { \"target-cpu\"=\"";
          add_escaped_bytes group.target_cpu;
          add "\"";
          Option.iter
            (fun tune ->
              add " \"tune-cpu\"=\"";
              add_escaped_bytes tune;
              add "\"")
            group.target_tune;
          Option.iter
            (fun features ->
              add " \"target-features\"=\"";
              add_escaped_bytes features;
              add "\"")
            group.target_features;
          add " }";
          newline ())
        target;
      if !file_order <> [] then
        debug (fun () ->
            add "!llvm.dbg.cu = !{!0}";
            newline ();
            add
              (Printf.sprintf "!llvm.module.flags = !{!%d, !%d}" flags_id (flags_id + 1));
            newline ();
            add
              "!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: \
               \"fas 0.6.3\", isOptimized: false, runtimeVersion: 0, emissionKind: \
               LineTablesOnly)";
            newline ();
            List.iter
              (fun path ->
                add
                  (Printf.sprintf "!%d = !DIFile(filename: \""
                     (Hashtbl.find file_ids path));
                add_escaped_bytes (Filename.basename path);
                add "\", directory: \"";
                add_escaped_bytes (absolute_directory path);
                add "\")";
                newline ())
              (List.rev !file_order);
            add (Printf.sprintf "!%d = !{i32 7, !\"Dwarf Version\", i32 5}" flags_id);
            newline ();
            add
              (Printf.sprintf "!%d = !{i32 2, !\"Debug Info Version\", i32 3}"
                 (flags_id + 1));
            newline ();
            List.iter2
              (fun (f : func) scope ->
                match (scope, f.source) with
                | Some sp, Some (path, line) ->
                    let file = Hashtbl.find file_ids path in
                    let display = Option.value f.display ~default:f.name in
                    add (Printf.sprintf "!%d = distinct !DISubprogram(name: \"" sp);
                    add_escaped_bytes display;
                    add "\"";
                    let linkage_name = redirect f.name in
                    if linkage_name <> display then (
                      add ", linkageName: \"";
                      add_escaped_bytes linkage_name;
                      add "\"");
                    add
                      (Printf.sprintf
                         ", scope: !%d, file: !%d, line: %d, type: !%d, scopeLine: %d, \
                          spFlags: DISPFlagDefinition, unit: !0)"
                         file file line type_id line);
                    newline ()
                | Some _, None | None, _ -> ())
              m.funcs scopes;
            add (Printf.sprintf "!%d = !DISubroutineType(types: !{})" type_id);
            newline ();
            List.iter
              (fun (id, line, column, scope) ->
                add
                  (Printf.sprintf "!%d = !DILocation(line: %d, column: %d, scope: !%d)"
                     id line column scope);
                newline ())
              (List.rev !location_order));
      Ok (Buffer.contents buffer)
    with Render_exhausted ->
      Error
        (Printf.sprintf "rendered LLVM text exceeds the configured limit of %d bytes"
           budget)

let render ?(debug_info = false) m =
  match render_bounded ~debug_info ~budget:max_int m with
  | Ok text -> text
  | Error _ -> assert false

let render_debug_bounded ~limits m =
  let budget = limits.Limits.max_rendered_ir_bytes in
  if budget < 0 then
    Error
      (Printf.sprintf "budget max_rendered_ir_bytes must not be negative (profile %s)"
         (Limits.budget_profile_name limits))
  else
    let buffer = Buffer.create 4096 in
    let variadic_signatures = Hashtbl.create 0 in
    let add s =
      if Buffer.length buffer > budget - String.length s then raise Render_exhausted
      else Buffer.add_string buffer s
    in
    let line s =
      add s;
      add "\n"
    in
    let global = function
      | String_global { name; bytes } ->
          line (Printf.sprintf "    String_global { name = %S; bytes = %S }" name bytes)
      | Array_global { name; elem_ty; elems; align } ->
          line
            (Printf.sprintf
               "    Array_global { name = %S; elem_ty = %s; elems = [%s]; align = %d }"
               name (ty_name elem_ty)
               (String.concat "; " (List.map Int64.to_string elems))
               align)
      | Storage_global { name; storage_ty; _ } ->
          line (Printf.sprintf "    Storage_global %S %s" name (ty_name storage_ty))
    in
    let block (b : block) =
      line (Printf.sprintf "      Block { id = %d; label = %S; instrs = [" b.id b.label);
      List.iter
        (fun instruction ->
          line ("        " ^ String.trim (instr_line variadic_signatures instruction)))
        (real_instrs b);
      line "      ];";
      line ("      terminator = " ^ String.trim (term_line b.terminator));
      line "      }"
    in
    let func (f : func) =
      let params =
        f.params
        |> List.map (fun (p : param) -> Printf.sprintf "%s:%s" p.name (ty_name p.ty))
        |> String.concat ", "
      in
      line
        (Printf.sprintf
           "    Function { name = %S; params = [%s]; ret = %s; variadic = %b; blocks = \
            ["
           f.name params (ty_name f.ret) f.variadic);
      List.iter block f.blocks;
      line "    ] }"
    in
    try
      line "Module {";
      line (Printf.sprintf "  target_triple = %S;" m.target_triple);
      line (Printf.sprintf "  data_layout = %S;" m.data_layout);
      line
        (Printf.sprintf "  no_inline_functions = [%s];"
           (String.concat "; " (List.map (Printf.sprintf "%S") m.no_inline_functions)));
      line "  globals = [";
      List.iter global m.globals;
      line "  ];";
      line "  functions = [";
      List.iter func m.funcs;
      line "  ];";
      line "}";
      Ok (Buffer.contents buffer)
    with Render_exhausted ->
      Error
        (Printf.sprintf
           "rendered debug IR bytes exceed budget max_rendered_ir_bytes of %d (profile \
            %s)"
           budget
           (Limits.budget_profile_name limits))

let render_debug m =
  match
    render_debug_bounded
      ~limits:{ Limits.default with max_rendered_ir_bytes = max_int }
      m
  with
  | Ok text -> text
  | Error _ -> assert false

let count_lowered_nodes m =
  List.fold_left
    (fun total (f : func) ->
      total + 1
      + List.fold_left
          (fun block_total (b : block) ->
            block_total + 1 + List.length (real_instrs b) + 1)
          0 f.blocks)
    0 m.funcs

let check_lowered_nodes ~limits m =
  let budget = limits.Limits.max_ir_nodes in
  if budget < 0 then
    Error
      ( None,
        Printf.sprintf "budget max_ir_nodes must not be negative (profile %s)"
          (Limits.budget_profile_name limits) )
  else
    let rec go total = function
      | [] -> Ok ()
      | (f : func) :: rest ->
          let func_nodes =
            1
            + List.fold_left
                (fun block_total (b : block) ->
                  block_total + 1 + List.length (real_instrs b) + 1)
                0 f.blocks
          in
          if func_nodes > budget - total then
            Error
              ( Some f.name,
                Printf.sprintf
                  "cumulative lowered IR nodes exceed budget max_ir_nodes of %d \
                   (profile %s) at function `%s`"
                  budget
                  (Limits.budget_profile_name limits)
                  f.name )
          else go (total + func_nodes) rest
    in
    go 0 m.funcs

let rec static_type_bytes structs visiting ty =
  match ty with
  | I1 | I8 -> Ok 1
  | I16 -> Ok 2
  | I32 -> Ok 4
  | I64 -> Ok 8
  | I128 -> Ok 16
  | F32 -> Ok 4
  | F64 -> Ok 8
  | Void -> Ok 0
  | Pointer _ -> Ok (Target_layout.current ()).pointer_size
  | Struct name ->
      if List.mem name visiting then Error ()
      else
        List.find_map
          (fun (def : struct_def) -> if def.name = name then Some def else None)
          structs
        |> Option.fold ~none:(Error ()) ~some:(fun (def : struct_def) ->
            let visiting = name :: visiting in
            let rec go_fields total = function
              | [] ->
                  if def.tail_padding > max_int - total then Error ()
                  else Ok (total + def.tail_padding)
              | field :: rest -> (
                  match static_type_bytes structs visiting field with
                  | Error () -> Error ()
                  | Ok bytes ->
                      if bytes > max_int - total then Error ()
                      else go_fields (total + bytes) rest)
            in
            go_fields 0 def.fields)
  | Array (length, elem) -> (
      if length < 0 then Error ()
      else if length = 0 then Ok 0
      else
        match static_type_bytes structs visiting elem with
        | Error () -> Error ()
        | Ok bytes ->
            if bytes <> 0 && length > max_int / bytes then Error ()
            else Ok (length * bytes))
  | Vector (length, elem) -> (
      if length < 0 then Error ()
      else if length = 0 then Ok 0
      else
        let elem_bits =
          match elem with
          | I1 -> Ok 1
          | I8 -> Ok 8
          | I16 -> Ok 16
          | I32 -> Ok 32
          | I64 -> Ok 64
          | I128 -> Ok 128
          | F32 -> Ok 32
          | F64 -> Ok 64
          | Pointer _ -> Ok ((Target_layout.current ()).pointer_size * 8)
          | _ -> Error ()
        in
        match elem_bits with
        | Error () -> Error ()
        | Ok bits -> (
            match Target_layout.vector (Target_layout.current ()) length bits with
            | Error _ -> Error ()
            | Ok (size, _) -> Ok size))

let check_static_data_bytes ~limits m =
  let budget = limits.Limits.max_static_data_bytes in
  if budget < 0 then
    Error
      ( None,
        Printf.sprintf "budget max_static_data_bytes must not be negative (profile %s)"
          (Limits.budget_profile_name limits) )
  else
    let global_bytes = function
      | String_global { bytes; _ } -> Ok (String.length bytes)
      | Array_global { elem_ty; elems; _ } -> (
          match static_type_bytes m.structs [] elem_ty with
          | Error () -> Error ()
          | Ok width ->
              let length = List.length elems in
              if width <> 0 && length > max_int / width then Error ()
              else Ok (length * width))
      | Storage_global { bytes; _ } ->
          Ok (Option.fold ~none:0 ~some:String.length bytes)
    in
    let name_of = function
      | String_global { name; _ }
      | Array_global { name; _ }
      | Storage_global { name; _ } ->
          name
    in
    let rec go total = function
      | [] -> Ok ()
      | global :: rest -> (
          match global_bytes global with
          | Error () ->
              Error
                ( Some (name_of global),
                  Printf.sprintf
                    "declared static data bytes exceed budget max_static_data_bytes of \
                     %d (profile %s) at global `%s`"
                    budget
                    (Limits.budget_profile_name limits)
                    (name_of global) )
          | Ok bytes ->
              if bytes > budget - total then
                Error
                  ( Some (name_of global),
                    Printf.sprintf
                      "declared static data bytes exceed budget max_static_data_bytes \
                       of %d (profile %s) at global `%s`"
                      budget
                      (Limits.budget_profile_name limits)
                      (name_of global) )
              else go (total + bytes) rest)
    in
    go 0 m.globals

let check_stack_scratch_bytes ~limits m =
  let budget = limits.Limits.max_stack_scratch_bytes in
  if budget < 0 then
    Error
      ( None,
        Printf.sprintf
          "budget max_stack_scratch_bytes must not be negative (profile %s)"
          (Limits.budget_profile_name limits) )
  else
    let func_scratch (f : func) =
      let rec instrs_total total = function
        | [] -> Ok total
        | Alloca (_, slot_ty, _) :: rest -> (
            match static_type_bytes m.structs [] slot_ty with
            | Error () -> Error ()
            | Ok bytes ->
                if bytes > max_int - total then Error ()
                else instrs_total (total + bytes) rest)
        | _ :: rest -> instrs_total total rest
      in
      let rec blocks_total total = function
        | [] -> Ok total
        | b :: rest -> (
            match instrs_total total b.instrs with
            | Error () -> Error ()
            | Ok total -> blocks_total total rest)
      in
      blocks_total 0 f.blocks
    in
    let message name =
      Printf.sprintf
        "cumulative alloca scratch bytes exceed budget max_stack_scratch_bytes of %d \
         (profile %s) at function `%s`"
        budget
        (Limits.budget_profile_name limits)
        name
    in
    let rec go total = function
      | [] -> Ok ()
      | f :: rest -> (
          match func_scratch f with
          | Error () -> Error (Some f.name, message f.name)
          | Ok bytes ->
              if bytes > budget - total then Error (Some f.name, message f.name)
              else go (total + bytes) rest)
    in
    go 0 m.funcs
