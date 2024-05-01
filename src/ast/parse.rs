use super::Stmt;
use crate::err::Res;
use crate::text::{expect_token, Token, Tokens};

pub struct Parser(Tokens);

impl From<Vec<Token>> for Parser {
    fn from(v: Vec<Token>) -> Self {
        Self(v.into_iter().peekable())
    }
}

impl Iterator for Parser {
    type Item = Res<Stmt>;

    fn next(&mut self) -> Option<Self::Item> {
        Stmt::parse_opt(&mut self.0)
    }
}

pub fn parse(v: Vec<Token>) -> Res<Vec<Stmt>> {
    Parser::from(v).collect()
}

pub fn parse_sep<T>(
    parse: impl Fn(&mut Tokens) -> Res<T>,
    sep: &Token,
    end: &Token,
    tokens: &mut Tokens,
) -> Res<Vec<T>> {
    if tokens.next_if(|t| t == end).is_some() {
        return Ok(vec![]);
    }
    let mut args = vec![parse(tokens)?];
    while tokens.next_if(|t| t == sep).is_some() {
        args.push(parse(tokens)?)
    }
    expect_token(tokens, end)?;
    Ok(args)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{Expr, Val},
        text::scan,
    };

    #[test]
    fn parser_works() {
        let s = r#"x = 5;
            y = "x";
            y = x;"#;
        let tokens = scan(s).unwrap();
        let stmts = parse(tokens).unwrap();
        let exp0 = Stmt::Let("x".to_string(), Expr::Value(Val::Int(5)));
        let exp1 = Stmt::Let("y".to_string(), Expr::Value(Val::String("x".to_string())));
        let exp2 = Stmt::Let("y".to_string(), Expr::Ident("x".to_string()));
        assert_eq!(stmts, vec![exp0, exp1, exp2]);
    }
}
