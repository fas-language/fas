(function_declaration
  "fn" @context
  name: (identifier) @name) @item

(struct_declaration
  "struct" @context
  name: (identifier) @name) @item

(type_alias_declaration
  "type" @context
  name: (identifier) @name) @item

(const_declaration
  "const" @context
  name: (identifier) @name) @item

(global_declaration
  "var" @context
  name: (identifier) @name) @item
