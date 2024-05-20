use super::op::{token_is_binop, token_is_unop};
use super::parse::parse_sep;
use super::val::token_starts_value;
use super::{fmt_vec, BinOp, UnOp, Value};
use crate::err::{Error, Res};
use crate::text::{expect_token, next_token, Token, Tokens};
use std::{fmt, mem};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Value(Value),
    Ident(String),
    Unary(UnOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}

impl Expr {
    pub fn parse(tokens: &mut Tokens) -> Res<Self> {
        let mut e = match next_token(tokens, "an expression")? {
            Token::Ident(ident) => Ok(Self::Ident(ident)),
            t if token_is_unop(&t) => Self::parse_unary(UnOp::try_from(t)?, tokens),
            t if token_starts_value(&t) => Value::parse_with(t, tokens).map(Self::Value),
            t => Error::parsing(format!("saw '{}', expected an expression", t)),
        }?;
        while e.add_call(tokens)? {}
        e.add_cond(tokens)?;
        if let Some(t) = tokens.next_if(token_is_binop) {
            e = Self::mk_binary(e, BinOp::try_from(t)?, Self::parse(tokens)?);
        };
        Ok(e)
    }

    fn parse_unary(op: UnOp, tokens: &mut Tokens) -> Res<Self> {
        let e = Self::parse(tokens)?;
        if let UnOp::Paren = op {
            expect_token(tokens, &Token::Rparen)?;
        }
        e.mk_unary(op)
    }

    fn mk_unary(self, op: UnOp) -> Res<Self> {
        if let UnOp::Paren = op {
            return Ok(Self::Unary(op, Box::new(self)));
        }
        match self {
            Expr::Cond(c, t, f) => Ok(Expr::Cond(Box::new(c.mk_unary(op)?), t, f)),
            Expr::Binary(l, o, r) => Ok(Expr::Binary(Box::new(l.mk_unary(op)?), o, r)),
            e => Ok(Self::Unary(op, Box::new(e))),
        }
    }

    fn add_call(&mut self, tokens: &mut Tokens) -> Res<bool> {
        match tokens.peek() {
            Some(Token::Lparen) => {
                expect_token(tokens, &Token::Lparen)?;
                *self = Self::Call(
                    Box::new(mem::replace(self, Self::Ident(String::new()))),
                    parse_sep(Expr::parse, &Token::Comma, &Token::Rparen, tokens)?,
                );
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn add_cond(&mut self, tokens: &mut Tokens) -> Res<()> {
        while tokens.next_if(|t| t == &Token::Cond).is_some() {
            let true_ = Self::parse(tokens)?;
            expect_token(tokens, &Token::Colon)?;
            let false_ = Self::parse(tokens)?;
            *self = Self::Cond(
                Box::new(mem::replace(self, Self::Ident(String::new()))),
                Box::new(true_),
                Box::new(false_),
            );
        }
        Ok(())
    }

    fn mk_binary(lhs: Self, op: BinOp, rhs: Self) -> Self {
        match rhs {
            Self::Cond(cond, true_, false_) => {
                let cond = Box::new(Self::mk_binary(lhs, op, *cond));
                Self::Cond(cond, true_, false_)
            }
            Self::Binary(mid, op2, rhs) if op >= op2 => {
                let lhs = Box::new(Self::mk_binary(lhs, op, *mid));
                Self::Binary(lhs, op2, rhs)
            }
            e => Self::Binary(Box::new(lhs), op, Box::new(e)),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Value(v) => write!(f, "{}", v),
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Unary(UnOp::Paren, e) => write!(f, "{}{})", UnOp::Paren, e),
            Expr::Unary(o, e) => write!(f, "{}{}", o, e),
            Expr::Binary(l, o, r) => write!(f, "{} {} {}", l, o, r),
            Expr::Cond(c, t, fa) => write!(f, "{} ? {} : {}", c, t, fa),
            Expr::Call(ident, args) => {
                let args = fmt_vec(args, ", ");
                write!(f, "{}({})", ident, args)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, Val};
    use crate::text::scan;

    fn int(i: isize) -> Box<Expr> {
        Box::new(Expr::Value(Val::Int(i)))
    }

    #[test]
    fn simple_expr_works() {
        let s = "5 + 3 * 2";
        let ts: Vec<Token> = scan(s).unwrap();
        let expr = Expr::parse(&mut ts.into_iter().peekable()).unwrap();
        let exp = Expr::Binary(
            int(5),
            BinOp::Plus,
            Box::new(Expr::Binary(int(3), BinOp::Mult, int(2))),
        );
        assert_eq!(expr, exp)
    }

    #[test]
    fn simple_expr_works2() {
        let s = "5 * 3 + 2";
        let ts: Vec<Token> = scan(s).unwrap();
        let expr = Expr::parse(&mut ts.into_iter().peekable()).unwrap();
        let exp = Expr::Binary(
            Box::new(Expr::Binary(int(5), BinOp::Mult, int(3))),
            BinOp::Plus,
            int(2),
        );
        assert_eq!(expr, exp)
    }

    #[test]
    fn simple_expr_works3() {
        let s = "-5 / 2 > 3";
        let ts: Vec<Token> = scan(s).unwrap();
        let expr = Expr::parse(&mut ts.into_iter().peekable()).unwrap();
        let exp = Expr::Binary(
            Box::new(Expr::Binary(
                Box::new(Expr::Unary(UnOp::Minus, int(5))),
                BinOp::Div,
                int(2),
            )),
            BinOp::Gt,
            int(3),
        );
        assert_eq!(expr, exp)
    }

    #[test]
    fn simple_expr_works4() {
        let s = "3 - (4 + 2)";
        let ts: Vec<Token> = scan(s).unwrap();
        let expr = Expr::parse(&mut ts.into_iter().peekable()).unwrap();
        let exp = Expr::Binary(
            int(3),
            BinOp::Minus,
            Box::new(Expr::Unary(
                UnOp::Paren,
                Box::new(Expr::Binary(int(4), BinOp::Plus, int(2))),
            )),
        );
        assert_eq!(expr, exp)
    }

    #[test]
    fn multiple_plus_works() {
        let s = "5 + 3 - 2 + 7 - 9";
        let ts: Vec<Token> = scan(s).unwrap();
        let expr = Expr::parse(&mut ts.into_iter().peekable()).unwrap();
        let exp = Expr::Binary(
            Box::new(Expr::Binary(
                Box::new(Expr::Binary(
                    Box::new(Expr::Binary(int(5), BinOp::Plus, int(3))),
                    BinOp::Minus,
                    int(2),
                )),
                BinOp::Plus,
                int(7),
            )),
            BinOp::Minus,
            int(9),
        );
        assert_eq!(expr, exp)
    }

    #[test]
    fn cond_works() {
        let s = "5 / 2 > 3 ? 2 * 5 : 1 + 7";
        let ts: Vec<Token> = scan(s).unwrap();
        let expr = Expr::parse(&mut ts.into_iter().peekable()).unwrap();
        let exp = Expr::Cond(
            Box::new(Expr::Binary(
                Box::new(Expr::Binary(int(5), BinOp::Div, int(2))),
                BinOp::Gt,
                int(3),
            )),
            Box::new(Expr::Binary(int(2), BinOp::Mult, int(5))),
            Box::new(Expr::Binary(int(1), BinOp::Plus, int(7))),
        );
        assert_eq!(expr, exp)
    }
}
