use super::parse::parse_sep;
use super::{fmt_vec, Expr, Stmt};
use crate::err::{Error, Res};
use crate::text::{expect_token, next_token, Token, Tokens};
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Val<S, T> {
    Bool(bool),
    Int(isize),
    String(String),
    Array(Vec<Val<S, T>>),
    Function(Vec<S>, Vec<(S, Val<S, T>)>, Vec<T>),
}

pub type Value = Val<String, Stmt>;

impl Value {
    pub fn parse(tokens: &mut Tokens) -> Res<Self> {
        match next_token(tokens, "value")? {
            Token::String(s) => Ok(Self::String(s)),
            Token::True => Ok(Self::Bool(true)),
            Token::False => Ok(Self::Bool(false)),
            Token::Int(i) => Ok(Val::Int(i)),
            Token::Lbracket => Self::parse_array(tokens),
            Token::Function => Self::parse_function(tokens),
            t => Error::parsing_custom(format!("saw '{}', expected a value", t))?,
        }
    }

    pub fn parse_array(tokens: &mut Tokens) -> Res<Self> {
        let vals = parse_sep(Self::parse, &Token::Comma, &Token::Rbracket, tokens)?;
        Ok(Self::Array(vals))
    }

    pub fn parse_function(tokens: &mut Tokens) -> Res<Self> {
        expect_token(tokens, &Token::Lparen)?;
        let args = parse_sep(parse_ident, &Token::Comma, &Token::Rparen, tokens)?;
        match tokens.peek() {
            Some(Token::Lbrace) => {}
            _ => {
                return Expr::parse(tokens)
                    .map(|e| Self::Function(args, Vec::new(), vec![Stmt::Expr(e)]))
            }
        }
        expect_token(tokens, &Token::Lbrace)?;
        let def = parse_sep(
            Stmt::parse_without_semicolon,
            &Token::Semicolon,
            &Token::Rbrace,
            tokens,
        )?;
        match def.last() {
            Some(Stmt::Expr(_)) => Ok(Self::Function(args, Vec::new(), def)),
            Some(s) => Error::parsing_custom(format!("saw '{}', expected an expression", s)),
            None => Error::parsing_custom("expected an expression"),
        }
    }
}

fn parse_ident(tokens: &mut Tokens) -> Res<String> {
    match tokens.next() {
        Some(Token::Ident(s)) => Ok(s),
        Some(t) => Error::parsing_custom(format!("saw '{}', expected an identifier", t)),
        None => Error::parsing_custom("expected an identifier"),
    }
}

impl<S: fmt::Display, T: fmt::Display> fmt::Display for Val<S, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Int(i) => write!(f, "{}", i),
            Self::Function(pars, env, stmts) => {
                let defs: String = env
                    .iter()
                    .map(|(k, v)| format!("{} = {};", k, v))
                    .collect::<Vec<_>>()
                    .join("; ");
                let stmts = fmt_vec(stmts, " ");
                write!(f, "fn({}) {{ {} {} }}", fmt_vec(pars, ", "), defs, stmts)
            }

            Val::Array(v) => {
                let elems: Vec<_> = v.iter().map(ToString::to_string).collect();
                write!(f, "[{}]", elems.join(", "))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::BinOp, text::scan};

    #[test]
    fn function_works() {
        let s = "fn(a, b) { x = a * 2; x + b }";
        let ts: Vec<Token> = scan(s).unwrap();
        let e = Val::parse(&mut ts.into_iter().peekable()).unwrap();
        let two = Box::new(Expr::Value(Val::Int(2)));
        let a = String::from("a");
        let x = String::from("x");
        let b = String::from("b");
        let args = vec![a.clone(), b.clone()];
        let a2 = Expr::Binary(Box::new(Expr::Ident(a)), BinOp::Mult, two);
        let xa2 = Stmt::Let(x.clone(), a2);
        let xb = Expr::Binary(
            Box::new(Expr::Ident(x)),
            BinOp::Plus,
            Box::new(Expr::Ident(b)),
        );
        let exp = Val::Function(args, Vec::new(), vec![xa2, Stmt::Expr(xb)]);
        assert_eq!(e, exp)
    }

    #[test]
    fn simple_function_works() {
        let s = "fn(a, b) a + b";
        let ts: Vec<Token> = scan(s).unwrap();
        let e = Val::parse(&mut ts.into_iter().peekable()).unwrap();
        let a = String::from("a");
        let b = String::from("b");
        let args = vec![a.clone(), b.clone()];
        let ab = Expr::Binary(
            Box::new(Expr::Ident(a)),
            BinOp::Plus,
            Box::new(Expr::Ident(b)),
        );
        let exp = Val::Function(args, Vec::new(), vec![Stmt::Expr(ab)]);
        assert_eq!(e, exp)
    }
}
