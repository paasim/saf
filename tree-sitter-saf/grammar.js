const ORD = {
  FUNC: 1,
  COND: 2,
  LOGICAL: 5,
  MINUS: 6,
  COMP: 7,
  ADD: 8,
  MULT: 9,
  NEG: 10,
  CALL: 20,
}

module.exports = grammar({
  name: 'saf',

  extras: $ => [$.comment, /\s/, /\\\r?\n/],

  rules: {
    source_file: $ => repeat($._statement),

    comment: $ => seq('#', /.*\n/),

    _statement: $ => seq(choice(
      $.statement,
      alias($._exp, $.exp)
    ), ';'),
    statement: $ => seq(
      field('name', $.identifier),
      '=',
      field('value', $._exp)
    ),

    _exp: $ => choice(
      $.call,
      $.cond,
      $.binexp,
      $.paren,
      $.unexp,
      $._value
    ),

    call: $ => prec.right(ORD.CALL, seq(
      field('expression', $._exp),
      '(',
      field('arguments', optional($.args)),
      ')'
    )),
    args: $ => seq($._exp, repeat(seq(',', $._exp))),

    cond: $ => prec.left(ORD.COND, seq($._exp, '?', $._exp, ':', $._exp)),

    binexp: $ => choice(
      prec.left(ORD.LOGICAL, seq($._exp, alias(choice('|', '&'), $.binop), $._exp)),
      prec.left(ORD.COMP, seq($._exp, alias(choice('<', '>', '!=', '=='), $.binop), $._exp)),
      prec.left(ORD.ADD, seq($._exp, alias(choice('+', '-'), $.binop), $._exp)),
      prec.left(ORD.MULT, seq($._exp, alias(choice('*', '/'), $.binop), $._exp))
    ),

    paren: $ => seq('(', $._exp, ')'),

    unexp: $ => seq($.unop, $._exp),
    unop: $ => choice(
      prec(ORD.MINUS, '-'),
      prec(ORD.NEG, choice('!', '<'))
    ),

    _value: $ => choice($.bool, $.int, $.string, $.identifier, $.array, $.func),

    bool: $ => choice('true', 'false'),
    int: $ => /\d+/,
    string: $ => seq('"', /[^"]+/, '"'),
    identifier: $ => /[a-zA-Z][a-z0-9_]*/,

    array: $ => seq(
      '[',
      optional(seq($._value, repeat(seq(',', $._value)))),
      ']'
    ),

    func: $ => prec.left(ORD.FUNC, seq(
      'fn',
      '(',
      field('parameters', optional($.params)),
      ')',
      field('definition', $.def),
    )),

    params: $ => seq($.identifier, repeat(seq(',', $.identifier))),

    def: $ => choice(
      $._exp,
      seq('{', seq(repeat($._statement), $._exp), '}')
    ),
  }
});
