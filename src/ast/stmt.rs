use super::Expr;
use crate::err::Res;
use crate::text::{expect_token, Token, Tokens};
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Let(String, Expr),
    Expr(Expr),
}

pub fn parse_assignment_rhs(tokens: &mut Tokens) -> Res<Option<Expr>> {
    if tokens.next_if(|t| t == &Token::Assign).is_none() {
        return Ok(None);
    }
    Expr::parse(tokens).map(Some)
}

impl Stmt {
    pub fn parse_opt(tokens: &mut Tokens) -> Option<Res<Self>> {
        tokens.peek()?;
        Some(Self::parse(tokens))
    }

    fn parse(tokens: &mut Tokens) -> Res<Self> {
        let res = Self::parse_without_semicolon(tokens)?;
        expect_token(tokens, &Token::Semicolon)?;
        Ok(res)
    }

    pub fn parse_without_semicolon(tokens: &mut Tokens) -> Res<Self> {
        match Expr::parse(tokens)? {
            Expr::Ident(ident) => Self::parse_let(ident, tokens),
            e => Ok(Self::Expr(e)),
        }
    }

    fn parse_let(ident: String, tokens: &mut Tokens) -> Res<Self> {
        match parse_assignment_rhs(tokens)? {
            Some(rhs) => Ok(Self::Let(ident, rhs)),
            None => Ok(Self::Expr(Expr::Ident(ident))),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let(s, e) => write!(f, "{} = {};", s, e),
            Self::Expr(e) => write!(f, "{};", e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, Val};
    use crate::text::scan;

    #[test]
    fn parse_let_works() {
        let s = "x = 5 * 2;";
        let ts: Vec<Token> = scan(s).unwrap();
        let stmt = Stmt::parse(&mut ts.into_iter().peekable()).unwrap();
        let five = Box::new(Expr::Value(Val::Int(5)));
        let two = Box::new(Expr::Value(Val::Int(2)));
        let e = Expr::Binary(five, BinOp::Mult, two);
        let exp = Stmt::Let("x".to_string(), e);
        assert_eq!(stmt, exp);
    }
}
