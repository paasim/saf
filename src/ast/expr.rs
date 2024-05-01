use super::parse::parse_sep;
use super::{fmt_vec, BinOp, UnOp, Val, Value};
use crate::err::{Error, Res};
use crate::text::{expect_token, next_token, Token, Tokens};
use std::{fmt, mem};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Call(Box<Expr>, Vec<Expr>),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Ident(String),
    Value(Value),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Call(ident, args) => {
                let args = fmt_vec(args, ", ");
                write!(f, "{}({})", ident, args)
            }
            Expr::Cond(c, t, fa) => write!(f, "{} ? {} : {}", c, t, fa),
            Expr::Binary(l, o, r) => write!(f, "{} {} {}", l, o, r),
            Expr::Unary(UnOp::Paren, e) => write!(f, "{}", e),
            Expr::Unary(o, e) => write!(f, "{}{}", o, e),
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Value(v) => write!(f, "{}", v),
        }
    }
}

impl Expr {
    pub fn parse(tokens: &mut Tokens) -> Res<Self> {
        let mut e = match next_token(tokens, "an expression")? {
            Token::Lparen => {
                let e = Self::parse_unary(UnOp::Paren, tokens)?;
                expect_token(tokens, &Token::Rparen)?;
                Ok(e)
            }
            Token::Minus => Self::parse_unary(UnOp::Minus, tokens),
            Token::Negation => Self::parse_unary(UnOp::Negation, tokens),
            Token::Len => Self::parse_unary(UnOp::Len, tokens),
            Token::TypeOf => Self::parse_unary(UnOp::TypeOf, tokens),
            Token::Ident(ident) => Ok(Self::Ident(ident)),
            Token::Function => Val::parse_function(tokens).map(Self::Value),
            Token::Gt => Self::parse_unary(UnOp::Pop, tokens),
            Token::Lt => Self::parse_unary(UnOp::Init, tokens),
            Token::String(s) => Ok(Self::Value(Val::String(s))),
            Token::True => Ok(Self::Value(Val::Bool(true))),
            Token::False => Ok(Self::Value(Val::Bool(false))),
            Token::Int(i) => Ok(Self::Value(Val::Int(i))),
            Token::Lbracket => Val::parse_array(tokens).map(Self::Value),
            t => Error::parsing_custom(format!("saw '{}', expected an expression", t)),
        }?;
        while e.add_call(tokens)? {}
        e.add_cond(tokens)?;
        if let Some(t) = tokens.next_if(BinOp::is_binop) {
            let op = BinOp::try_from(t)?;
            let rhs = Self::parse(tokens)?;
            e = Self::mk_binary(e, op, rhs);
        };
        Ok(e)
    }

    fn parse_unary(op: UnOp, tokens: &mut Tokens) -> Res<Self> {
        let e = Self::parse(tokens)?;
        e.mk_unary(op)
    }

    fn mk_unary(self, op: UnOp) -> Res<Self> {
        match self {
            Expr::Cond(c, t, f) => Ok(Expr::Cond(Box::new(c.mk_unary(op)?), t, f)),
            Expr::Binary(l, o, r) => Ok(Expr::Binary(Box::new(l.mk_unary(op)?), o, r)),
            e => Ok(Self::Unary(op, Box::new(e))),
        }
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
