use super::Val;
use crate::err::{Error, Res};
use crate::text::Token;
use std::{cmp, fmt};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum UnOp {
    Paren,
    Minus,
    Negation,
    Init,
    Pop,
    Len,
    TypeOf,
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Paren => write!(f, ""), // () handled by expr
            Self::Minus => write!(f, "-"),
            Self::Negation => write!(f, "!"),
            Self::Init => write!(f, "<"),
            Self::Pop => write!(f, ">"),
            Self::Len => write!(f, "#"),
            Self::TypeOf => write!(f, "@"),
        }
    }
}

impl TryFrom<Token> for UnOp {
    type Error = Error;

    fn try_from(t: Token) -> Res<Self> {
        match t {
            Token::And => Ok(Self::Minus),
            Token::Or => Ok(Self::Negation),
            Token::Len => Ok(Self::Len),
            Token::TypeOf => Ok(Self::TypeOf),
            t => Error::parsing(
                Some(t),
                vec![Token::Minus, Token::Negation, Token::Len, Token::TypeOf],
            ),
        }
    }
}

impl UnOp {
    pub fn eval<S: Clone + fmt::Display, T: Clone + fmt::Display>(
        &self,
        v: Val<S, T>,
    ) -> Res<Val<S, T>> {
        match (self, v) {
            (UnOp::Paren, v) => Ok(v),
            (UnOp::Minus, Val::Int(i)) => Ok(Val::Int(-i)),
            (UnOp::Negation, Val::Bool(b)) => Ok(Val::Bool(!b)),
            (UnOp::Pop, Val::Array(v)) => match v.last() {
                Some(v) => Ok(v.clone()),
                None => Error::value_custom("trying to pop from an empty array"),
            },
            (UnOp::Init, Val::Array(mut v)) => {
                v.pop();
                Ok(Val::Array(v))
            }
            (UnOp::Len, Val::String(s)) => Ok(Val::Int(s.len().try_into()?)),
            (UnOp::Len, Val::Array(v)) => Ok(Val::Int(v.len().try_into()?)),
            (UnOp::TypeOf, Val::String(_)) => Ok(Val::String(String::from("string"))),
            (UnOp::TypeOf, Val::Bool(_)) => Ok(Val::String(String::from("bool"))),
            (UnOp::TypeOf, Val::Int(_)) => Ok(Val::String(String::from("int"))),
            (UnOp::TypeOf, Val::Array(_)) => Ok(Val::String(String::from("array"))),
            (UnOp::TypeOf, Val::Function(_, _, _)) => Ok(Val::String(String::from("function"))),
            (op, v) => Error::unop(op, &v),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BinOp {
    Or,
    And,
    Lt,
    Gt,
    NotEq,
    Eq,
    Minus,
    Plus,
    Div,
    Mult,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Token::from(self))
    }
}

// order of evaluation
impl PartialOrd for BinOp {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (t1, t2) if t1 == t2 => Some(cmp::Ordering::Equal),
            (Self::Mult, Self::Div) => Some(cmp::Ordering::Equal),
            (Self::Div, Self::Mult) => Some(cmp::Ordering::Equal),
            (Self::Mult, _) => Some(cmp::Ordering::Greater),
            (Self::Div, _) => Some(cmp::Ordering::Greater),
            (_, Self::Mult) => Some(cmp::Ordering::Less),
            (_, Self::Div) => Some(cmp::Ordering::Less),
            (Self::Plus, Self::Minus) => Some(cmp::Ordering::Equal),
            (Self::Minus, Self::Plus) => Some(cmp::Ordering::Equal),
            (Self::Plus, _) => Some(cmp::Ordering::Greater),
            (Self::Minus, _) => Some(cmp::Ordering::Greater),
            (_, Self::Plus) => Some(cmp::Ordering::Less),
            (_, Self::Minus) => Some(cmp::Ordering::Less),
            (Self::And, Self::Or) => Some(cmp::Ordering::Greater),
            (Self::And, _) => Some(cmp::Ordering::Less),
            (Self::Or, _) => Some(cmp::Ordering::Less),
            (_, Self::And) => Some(cmp::Ordering::Greater),
            (_, Self::Or) => Some(cmp::Ordering::Greater),
            _ => Some(cmp::Ordering::Equal),
        }
    }
}

impl BinOp {
    pub fn is_binop(t: &Token) -> bool {
        matches!(
            t,
            Token::And
                | Token::Or
                | Token::Plus
                | Token::Minus
                | Token::Mult
                | Token::Div
                | Token::Eq
                | Token::NotEq
                | Token::Gt
                | Token::Lt
        )
    }

    pub fn eval<S: fmt::Display + cmp::PartialEq, T: fmt::Display + cmp::PartialEq>(
        &self,
        lhs: Val<S, T>,
        rhs: Val<S, T>,
    ) -> Res<Val<S, T>> {
        match (self, lhs, rhs) {
            (BinOp::Or, Val::Bool(l), Val::Bool(r)) => Ok(Val::Bool(l || r)),
            (BinOp::Lt, Val::Array(mut l), v) => {
                l.push(v);
                Ok(Val::Array(l))
            }
            (BinOp::And, Val::Bool(l), Val::Bool(r)) => Ok(Val::Bool(l && r)),
            (BinOp::Lt, Val::Int(l), Val::Int(r)) => Ok(Val::Bool(l < r)),
            (BinOp::Gt, Val::Int(l), Val::Int(r)) => Ok(Val::Bool(l > r)),
            (BinOp::Minus, Val::Int(l), Val::Int(r)) => Ok(Val::Int(l - r)),
            (BinOp::Plus, Val::Int(l), Val::Int(r)) => Ok(Val::Int(l + r)),
            (BinOp::Plus, Val::String(l), Val::String(r)) => Ok(Val::String(l + &r)),
            (BinOp::Div, Val::Int(l), Val::Int(r)) => Ok(Val::Int(l / r)),
            (BinOp::Mult, Val::Int(l), Val::Int(r)) => Ok(Val::Int(l * r)),
            (BinOp::Eq, Val::Bool(l), Val::Bool(r)) => Ok(Val::Bool(l == r)),
            (BinOp::Eq, Val::Int(l), Val::Int(r)) => Ok(Val::Bool(l == r)),
            (BinOp::Eq, Val::String(l), Val::String(r)) => Ok(Val::Bool(l == r)),
            (BinOp::Eq, Val::Array(l), Val::Array(r)) => Ok(Val::Bool(l == r)),
            (BinOp::NotEq, Val::Bool(l), Val::Bool(r)) => Ok(Val::Bool(l != r)),
            (BinOp::NotEq, Val::Int(l), Val::Int(r)) => Ok(Val::Bool(l != r)),
            (BinOp::NotEq, Val::String(l), Val::String(r)) => Ok(Val::Bool(l != r)),
            (BinOp::NotEq, Val::Array(l), Val::Array(r)) => Ok(Val::Bool(l != r)),
            (op, l, r) => Error::binop(&l, &r, op),
        }
    }
}

impl TryFrom<Token> for BinOp {
    type Error = Error;

    fn try_from(t: Token) -> Res<Self> {
        match t {
            Token::And => Ok(Self::And),
            Token::Or => Ok(Self::Or),
            Token::Plus => Ok(Self::Plus),
            Token::Minus => Ok(Self::Minus),
            Token::Mult => Ok(Self::Mult),
            Token::Div => Ok(Self::Div),
            Token::Eq => Ok(Self::Eq),
            Token::NotEq => Ok(Self::NotEq),
            Token::Gt => Ok(Self::Gt),
            Token::Lt => Ok(Self::Lt),
            t => Error::parsing(
                Some(t),
                vec![
                    Token::And,
                    Token::Or,
                    Token::Plus,
                    Token::Minus,
                    Token::Mult,
                    Token::Div,
                    Token::Eq,
                    Token::NotEq,
                    Token::Gt,
                    Token::Lt,
                ],
            ),
        }
    }
}

impl From<&BinOp> for Token {
    fn from(op: &BinOp) -> Self {
        match op {
            BinOp::And => Self::And,
            BinOp::Or => Self::Or,
            BinOp::Plus => Self::Plus,
            BinOp::Minus => Self::Minus,
            BinOp::Mult => Self::Mult,
            BinOp::Div => Self::Div,
            BinOp::Eq => Self::Eq,
            BinOp::NotEq => Self::NotEq,
            BinOp::Gt => Self::Gt,
            BinOp::Lt => Self::Lt,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_single_char_works() {
        // these are equal
        assert!(BinOp::Div >= BinOp::Mult);
        assert!(BinOp::Div <= BinOp::Mult);
        assert!(BinOp::Eq >= BinOp::Lt);
        assert!(BinOp::Lt >= BinOp::Eq);

        // mult is bigger than everything
        assert!(BinOp::Mult > BinOp::Or);
        assert!(BinOp::Mult > BinOp::And);

        // plus bigger than eq
        assert!(BinOp::Plus > BinOp::Lt);

        // equalities are bigger than and
        assert!(BinOp::NotEq > BinOp::And);
    }
}
