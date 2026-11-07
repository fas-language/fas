(* The lowering's operand vocabulary (roadmap v0.6: one IR).

   lower.ml emits Fir directly, through Lower_emit, but it names types and
   operands with these small types first:
   - [ty] keeps the front end's shapes: named structs and arrays, so that
     Lower_emit can size allocas and copy aggregates. Fir itself only has
     scalars, vectors and pointers.
   - [value] is symbolic: a constant, a result id reserved with
     [Lower.fresh], a parameter or a global. Lower_emit turns it into a Fir
     value when an instruction uses it.

   There are no instructions here. Each one is a Lower_emit function that
   appends Fir straight to the current block. These types were the old Ir's
   (deleted in v0.6), so lower.ml kept its shape through the port. *)

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
  | Const of ty * int64  (** floats: the IEEE bits *)
  | Const_vector of ty * int64 list
  | Null of ty
  | Undef of ty  (** becomes Fir poison *)
  | Zero of ty
  | Local of int * ty  (** a result id from [Lower.fresh] *)
  | Param of string * ty
  | Global of string * ty  (** the address of a function or global *)

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

(* a typed address step, as in LLVM's getelementptr: [Zero] is index 0 *)
type gep_index = Zero | Index of value

(* how a C ABI widens a narrow integer argument or result *)
type extension = No_extension | Sign_extension | Zero_extension

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
