(comment) @comment

(integer) @number
(float) @number
(string) @string
(cstring) @string
(character) @string
(system_header) @string
(boolean) @boolean
(null) @constant.builtin

(raw_c_open) @keyword
(raw_c_body) @embedded
(pkg_declaration "pkg" @keyword)
(struct_declaration "value" @keyword)
(const_for_statement "in" @keyword)

[
  "fn"
  "return"
  "if"
  "else"
  "while"
  "break"
  "continue"
  "const"
  "var"
  "struct"
  "opaque"
  "extern"
  "defer"
  "use"
  "for"
  "switch"
  "case"
  "default"
  "view"
  "error"
  "fails"
  "fail"
  "try"
  "catch"
  "errdefer"
] @keyword

[
  "handle"
  "arr"
  "vec"
] @type.builtin

(address_type) @type.builtin
(primitive_type) @type.builtin
(error_type) @type.builtin

[
  "+"
  "-"
  "*"
  "/"
  "%"
  "&"
  "|"
  "^"
  "~"
  "!"
  "<<"
  ">>"
  "&&"
  "||"
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
  "="
  "+="
  "-="
  "*="
  "/="
  "%="
  "&="
  "|="
  "^="
  "<<="
  ">>="
] @operator

["(" ")" "[" "]" "{" "}"] @punctuation.bracket
["," "." ";" ":"] @punctuation.delimiter
(variadic_parameter) @punctuation.special

(identifier) @variable
(type_identifier) @type
(field_expression field: (identifier) @property)
(field_declaration name: (identifier) @property)
(array_literal name: (identifier) @property)
(struct_declaration name: (identifier) @type)
(opaque_declaration name: (identifier) @type)
(named_type name: (type_identifier) @type)
(generic_param name: (identifier) @type)
(const_declaration name: (identifier) @constant)
(global_declaration name: (identifier) @variable)
(parameter_list name: (identifier) @variable.parameter)
(attribute name: (identifier) @attribute)
(labeled_statement label: (identifier) @label)
(break_statement label: (identifier) @label)
(continue_statement label: (identifier) @label)

(function_declaration name: (identifier) @function)
(call_expression function: (identifier) @function)
(call_expression function: (index_expression value: (identifier) @function))
(call_expression function: (field_expression field: (identifier) @function))

((identifier) @function.builtin
  (#any-of? @function.builtin
    "zext" "sext" "trunc" "bitcast" "sizeof" "alignof" "offsetof" "len"
    "splat" "rotl" "rotr" "popcount" "clz" "ctz" "select" "any" "all"
    "add_sat" "sub_sat" "mul_hi" "reduce_sum" "reduce_min" "reduce_max"
    "reduce_and" "reduce_or" "reduce_xor" "shuffle" "permute" "compress"
    "expand" "masked_load" "masked_store" "gather" "gather_bytes" "scatter"
    "scatter_bytes" "copy" "call_addr" "errno_err" "err_name" "volatile_load" "volatile_store"
    "addr_bits" "addr_from_bits" "handle_addr" "handle_from_addr" "syscall" "sqrt"
    "atomic_load" "atomic_store" "atomic_xchg" "atomic_cas" "atomic_add"
    "atomic_sub" "atomic_and" "atomic_or" "atomic_xor" "atomic_fence"
    "fma" "floor" "ceil" "round" "abs" "cvt"))
