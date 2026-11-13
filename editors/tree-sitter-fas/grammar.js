const PREC = {
  or: 1,
  and: 2,
  equality: 3,
  relational: 4,
  bitor: 5,
  bitxor: 6,
  bitand: 7,
  shift: 8,
  additive: 9,
  multiplicative: 10,
  unary: 11,
  postfix: 12,
};

const NL = repeat('\n');

const primitive_types = [
  'bool', 'void', 'f32', 'f64', 'u8', 'u16', 'u32', 'u64',
  'i8', 'i16', 'i32', 'i64', 'usize', 'isize',
];

const keywords = [
  'fn', 'return', 'if', 'else', 'while', 'break', 'continue', 'const', 'var',
  'struct', 'opaque', 'extern', 'defer', 'use', 'for', 'switch', 'case',
  'default', 'view', 'addr', 'handle', 'arr', 'vec', 'true', 'false', 'null',
  'error', 'fails', 'fail', 'try', 'catch', 'errdefer',
  ...primitive_types,
];

const binary_operators = [
  ['||', PREC.or],
  ['&&', PREC.and],
  ['==', PREC.equality],
  ['!=', PREC.equality],
  ['<', PREC.relational],
  ['<=', PREC.relational],
  ['>', PREC.relational],
  ['>=', PREC.relational],
  ['|', PREC.bitor],
  ['^', PREC.bitxor],
  ['&', PREC.bitand],
  ['<<', PREC.shift],
  ['>>', PREC.shift],
  ['+', PREC.additive],
  ['-', PREC.additive],
  ['*', PREC.multiplicative],
  ['/', PREC.multiplicative],
  ['%', PREC.multiplicative],
];

const assignment_operators = [
  '=', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=',
];

const string_char = /[^"\\]/;
const char_char = /[\x20-\x26\x28-\x5B\x5D-\x7E]/;
const escape = choice(/\\x[0-9A-Fa-f]{2}/, /\\[nrt\\'"0]/);

function delimited(item, open, close) {
  return seq(
    open,
    NL,
    optional(
      seq(
        item,
        repeat(seq(NL, ',', NL, item)),
        optional(seq(NL, ',')),
        NL,
      ),
    ),
    close,
  );
}

function statements($) {
  return seq(
    repeat(
      choice(
        $._sep,
        $._compound_statement,
        seq($._simple_statement, $._sep),
      ),
    ),
    optional($._simple_statement),
  );
}

function commaSeparated(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

module.exports = grammar({
  name: 'fas',

  extras: ($) => [/[ \t\r\f\v]/, $.comment],

  word: ($) => $.identifier,

  externals: ($) => [$.raw_c_open, $.raw_c_body],

  conflicts: ($) => [
    [$._primary, $.named_type],
    [$._sep, $.array_literal],
    [$.block, $.array_literal],
    [$._simple_statement, $._expression],
    [$._sep, $.if_expression],
    [$.if_statement],
  ],

  rules: {
    source_file: ($) => repeat(choice($._sep, $._item)),

    _sep: (_) => choice(';', '\n'),

    _item: ($) =>
      choice(
        $.use_declaration,
        $.pkg_declaration,
        $.const_declaration,
        $.global_declaration,
        $.struct_declaration,
        $.opaque_declaration,
        $.extern_declaration,
        $.function_declaration,
        $.error_declaration,
      ),

    use_declaration: ($) =>
      seq(
        'use',
        // `use linux "f.fas"`: only when building for that OS
        optional(field('os', $.identifier)),
        field('path', $.string),
        optional(
          choice(
            $.system_header,
            $.string,
            seq($.raw_c_open, '\n', $.raw_c_body),
          ),
        ),
      ),

    pkg_declaration: ($) => seq('use', 'pkg', field('modules', $.string)),

    system_header: (_) => token(seq('<', /[A-Za-z0-9_.\/+-]+/, '>')),

    const_declaration: ($) =>
      seq(
        'const',
        field('name', $.identifier),
        field('type', $._type),
        '=',
        NL,
        field('value', $._expression),
      ),

    global_declaration: ($) =>
      seq(
        'var',
        field('name', $.identifier),
        field('type', $._type),
        optional(seq('=', NL, field('value', $._expression))),
      ),

    // `value struct`: `value` is a keyword only here
    struct_declaration: ($) =>
      seq(
        optional('value'),
        'struct',
        field('name', $.identifier),
        optional($.generic_params),
        optional($.attribute),
        $.struct_body,
      ),

    attribute: ($) =>
      seq('@', field('name', $.identifier), '(', $._expression, ')'),

    struct_body: ($) =>
      seq(
        '{',
        repeat(choice('\n', ',')),
        repeat(seq($.field_declaration, repeat(choice('\n', ',')))),
        '}',
      ),

    field_declaration: ($) =>
      seq(field('name', $.identifier), field('type', $._type)),

    opaque_declaration: ($) => seq('opaque', field('name', $.identifier)),

    // error Empty, BadDigit, NotFound = 2
    error_declaration: ($) => seq('error', commaSeparated($.error_name)),

    error_name: ($) =>
      seq(
        field('name', $.identifier),
        optional(seq('=', field('code', $.integer))),
      ),

    extern_declaration: ($) =>
      seq(
        'extern',
        field('abi', $.string),
        '{',
        repeat(
          choice(
            $._sep,
            $.global_declaration,
            $.function_declaration,
          ),
        ),
        '}',
      ),

    function_declaration: ($) =>
      seq(
        'fn',
        field('name', $.identifier),
        optional($.generic_params),
        field('parameters', $.parameter_list),
        choice(
          seq(field('result', $._type), optional(field('fails', 'fails'))),
          field('fails', 'fails'),
        ),
        optional(field('body', $.block)),
      ),

    generic_params: ($) => delimited($.generic_param, '[', ']'),

    generic_param: ($) =>
      seq(
        field('name', $.identifier),
        optional(seq('const', field('type', $._type))),
      ),

    parameter_list: ($) => delimited($._parameter, '(', ')'),

    _parameter: ($) =>
      choice(
        seq(field('name', $.identifier), field('type', $._type)),
        $.variadic_parameter,
      ),

    variadic_parameter: (_) => '...',

    block: ($) => prec.dynamic(1, seq('{', statements($), '}')),

    _simple_statement: ($) =>
      choice(
        $.declaration,
        $.assignment,
        $.view_statement,
        $.return_statement,
        $.break_statement,
        $.continue_statement,
        $.fail_statement,
        $._expr,
      ),

    _compound_statement: ($) =>
      choice(
        $.if_statement,
        $.while_statement,
        $.for_statement,
        $.const_for_statement,
        $.switch_statement,
        $.defer_statement,
        $.errdefer_statement,
        $.block,
        $.labeled_statement,
      ),

    declaration: ($) =>
      seq(
        field('name', $.identifier),
        field('type', $._type),
        optional(seq('=', NL, field('value', $._expression))),
      ),

    assignment: ($) =>
      seq(
        field('target', $._postfix),
        field('operator', choice(...assignment_operators)),
        NL,
        field('value', $._expression),
      ),

    view_statement: ($) =>
      seq(
        'view',
        field('name', $.identifier),
        '=',
        NL,
        field('place', $._expression),
      ),

    return_statement: ($) => seq('return', optional($._expression)),

    break_statement: ($) => seq('break', optional(field('label', $.identifier))),

    continue_statement: ($) =>
      seq('continue', optional(field('label', $.identifier))),

    defer_statement: ($) => seq('defer', $.block),

    errdefer_statement: ($) =>
      seq('errdefer', optional(field('error', $.identifier)), $.block),

    fail_statement: ($) => seq('fail', field('error', $._expression)),

    labeled_statement: ($) =>
      seq(
        field('label', $.identifier),
        ':',
        choice($.while_statement, $.for_statement),
      ),

    if_statement: ($) =>
      seq(
        'if',
        field('condition', $._expr),
        $.block,
        optional(seq(NL, 'else', choice($.if_statement, $.block))),
      ),

    while_statement: ($) =>
      seq('while', field('condition', $._expr), $.block),

    for_statement: ($) =>
      seq(
        'for',
        optional(field('init', $._for_clause)),
        ';',
        optional(field('condition', $._expression)),
        ';',
        optional(field('step', $._for_clause)),
        $.block,
      ),

    const_for_statement: ($) =>
      seq(
        'for',
        'const',
        field('name', $.identifier),
        field('type', $._type),
        'in',
        field('low', $._expr),
        '..',
        field('high', $._expr),
        $.block,
      ),

    _for_clause: ($) =>
      choice($.declaration, $.assignment, $.view_statement, $._expr),

    switch_statement: ($) =>
      seq(
        'switch',
        field('value', $._expr),
        '{',
        repeat($._sep),
        repeat(choice($.case_clause, $.default_clause)),
        '}',
      ),

    case_clause: ($) =>
      seq('case', commaSeparated($._expression), ':', statements($)),

    default_clause: ($) => seq('default', ':', statements($)),

    if_expression: ($) =>
      seq(
        'if',
        field('condition', $._expr),
        '{',
        NL,
        field('consequence', $._expression),
        NL,
        '}',
        NL,
        'else',
        choice(
          field('alternative', $.if_expression),
          seq('{', NL, field('alternative', $._expression), NL, '}'),
        ),
      ),

    _expression: ($) => choice($.if_expression, $._expr),

    _expr: ($) => choice($.binary_expression, $._unary),

    binary_expression: ($) =>
      choice(
        ...binary_operators.map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(
              field('left', $._expr),
              field('operator', operator),
              NL,
              field('right', $._expr),
            ),
          ),
        ),
        prec.left(
          PREC.additive,
          seq(
            field('left', $._expr),
            field('operator', '-'),
            token.immediate('>'),
            field('field', $._field_name),
          ),
        ),
      ),

    _unary: ($) =>
      choice(
        $.unary_expression,
        $.try_expression,
        $.c_dereference,
        $.c_pointer_cast,
        $._postfix,
      ),

    unary_expression: ($) =>
      prec(
        PREC.unary,
        seq(
          field('operator', choice('-', '!', '~', '&')),
          field('operand', $._unary),
        ),
      ),

    // binds tighter than every operator: `try f() + 1` adds to the value
    try_expression: ($) =>
      prec(PREC.unary, seq('try', field('call', $._postfix))),

    c_dereference: ($) =>
      prec(
        PREC.unary,
        seq(
          '*',
          '(',
          field('type', choice($.pointer_type, $.address_type)),
          ')',
          field('operand', $._unary),
        ),
      ),

    c_pointer_cast: ($) =>
      prec(
        PREC.unary,
        seq(
          '(',
          field('type', $.pointer_type),
          ')',
          field('operand', $._unary),
        ),
      ),

    pointer_type: ($) =>
      seq(field('pointee', choice($.primitive_type, $.address_type)), '*'),

    _postfix: ($) =>
      choice(
        $.call_expression,
        $.catch_expression,
        $.index_expression,
        $.field_expression,
        $.dereference_field,
        $._primary,
      ),

    call_expression: ($) =>
      prec.left(
        PREC.postfix,
        seq(field('function', $._postfix), field('arguments', $.argument_list)),
      ),

    // `f() catch 0`: the default value takes the whole expression to its
    // right; `f() catch { }` and `f() catch e { }` run a block
    catch_expression: ($) =>
      prec.right(
        -1,
        seq(
          field('call', $._postfix),
          'catch',
          choice(
            seq(optional(field('error', $.identifier)), field('handler', $.block)),
            field('value', $._expression),
          ),
        ),
      ),

    index_expression: ($) =>
      prec.left(
        PREC.postfix,
        seq(field('value', $._postfix), field('arguments', $.bracket_arguments)),
      ),

    field_expression: ($) =>
      prec.left(
        PREC.postfix,
        seq(field('value', $._postfix), '.', field('field', $._field_name)),
      ),

    dereference_field: ($) =>
      prec.left(PREC.postfix, seq(field('value', $._postfix), '.', '*')),

    _primary: ($) =>
      choice(
        $.identifier,
        $.integer,
        $.float,
        $.character,
        $.string,
        $.cstring,
        $.boolean,
        $.null,
        $.array_literal,
        $.parenthesized_expression,
      ),

    parenthesized_expression: ($) => seq('(', NL, $._expression, NL, ')'),

    array_literal: ($) => delimited($._array_element, '{', '}'),

    _array_element: ($) =>
      choice(
        seq('.', field('name', $.identifier), '=', NL, $._expression),
        seq('[', $._expression, ']', '=', NL, $._expression),
        $._expression,
      ),

    argument_list: ($) => delimited($._expression, '(', ')'),

    bracket_arguments: ($) => delimited($._bracket_argument, '[', ']'),

    _bracket_argument: ($) =>
      choice($.range_argument, $.fails_argument, $._type, $._expression),

    // call_addr[T fails] and call_addr[fails]
    fails_argument: ($) => seq(optional(field('type', $._type)), 'fails'),

    range_argument: (_) => seq('.', '.'),

    _type: ($) =>
      choice(
        $.primitive_type,
        $.address_type,
        $.error_type,
        $.handle_type,
        $.array_type,
        $.vector_type,
        $.named_type,
      ),

    primitive_type: (_) => choice(...primitive_types),

    address_type: (_) => 'addr',

    error_type: (_) => 'error',

    handle_type: ($) => seq('handle', '[', $._type, ']'),

    array_type: ($) =>
      seq('arr', '[', NL, $._expression, NL, ',', NL, $._type, NL, ']'),

    vector_type: ($) =>
      seq('vec', '[', NL, $._expression, NL, ',', NL, $._type, NL, ']'),

    named_type: ($) =>
      seq(
        field('name', alias($.identifier, $.type_identifier)),
        optional($.bracket_arguments),
      ),

    _field_name: ($) =>
      choice(
        $.identifier,
        alias(choice(...keywords), $.identifier),
      ),

    identifier: (_) => /[A-Za-z_][A-Za-z0-9_]*/,

    boolean: (_) => choice('true', 'false'),

    null: (_) => 'null',

    integer: (_) =>
      token(choice(/0x[0-9A-Fa-f_]+/, /0b[01_]+/, /0o[0-7_]+/, /[0-9][0-9_]*/)),

    float: (_) =>
      token(
        choice(
          /[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9_]+)?/,
          /[0-9][0-9_]*[eE][+-]?[0-9_]+/,
        ),
      ),

    character: (_) => token(seq("'", choice(char_char, escape), "'")),

    string: (_) => token(seq('"', repeat(choice(string_char, escape)), '"')),

    cstring: (_) => token(seq('c"', repeat(choice(string_char, escape)), '"')),

    comment: (_) =>
      token(
        choice(
          seq('//', /[^\n]*/),
          seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'),
        ),
      ),
  },
});
