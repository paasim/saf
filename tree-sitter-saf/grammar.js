module.exports = grammar({
  name: 'saf',

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => seq(choice($.statement, alias($._exp, $.exp)), ';'),
    statement: $ => seq($.identifier, '=', $._exp),

    _exp: $ => choice(
      $.call,
      $.cond,
      $.binexp,
      $.paren,
      $.unexp,
      $._value
    ),

    call: $ => prec.right(41, seq($._exp, '(', optional($.args), ')')),
    args: $ => seq($._exp, repeat(seq(',', $._exp))),

    cond: $ => prec.left(2, seq($._exp, '?', $._exp, ':', $._exp)),

    binexp: $ => choice($._binexp5, $._binexp15, $._binexp25, $._binexp35),
    _binexp5: $ => prec.left(5, seq($._exp, alias($.binop5, $.binop), $._exp)),
    _binexp15: $ => prec.left(15, seq($._exp, alias($.binop15, $.binop), $._exp)),
    _binexp25: $ => prec.left(25, seq($._exp, alias($.binop25, $.binop), $._exp)),
    _binexp35: $ => prec.left(35, seq($._exp, alias($.binop35, $.binop), $._exp)),
    binop5: $ => choice('|', '&'),
    binop15: $ => choice('<', '>', '!=', '=='),
    binop25: $ => choice('+', '-'),
    binop35: $ => choice('*', '/'),

    paren: $ => seq('(', $._exp, ')'),

    unexp: $ => seq($.unop, $._exp),
    unop: $ => choice($._unop20, $._unop40),
    _unop20: $ => prec(10, '-'),
    _unop40: $ => prec(40, choice('!', '<')),

    _value: $ => choice($.bool, $.int, $.string, $.identifier, $.array, $.func),

    bool: $ => choice('true', 'false'),
    int: $ => /\d+/,

    string: $ => seq('"', $._string, '"'),
    _string: $ => /[^"]+/,

    identifier: $ => /[a-zA-Z][a-z0-9_]*/,

    array: $ => seq('[', optional($._vals), ']'),
    _vals: $ => seq($._value, repeat(seq(',', $._value))),

    func: $ => prec.left(1, seq(
      'fn',
      '(', optional($.params), ')',
      $.def,
    )),

    params: $ => seq($.identifier, repeat(seq(',', $.identifier))),

    def: $ => choice($._exp, seq('{', $._def_braces, '}')),
    _def_braces: $ => seq(repeat($._statement), $._exp),

  }
});
