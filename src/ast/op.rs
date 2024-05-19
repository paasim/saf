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
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Paren => write!(f, ""), // () handled by expr
            Self::Minus => write!(f, "-"),
            Self::Negation => write!(f, "!"),
            Self::Init => write!(f, "<"),
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
            (UnOp::Minus, Val::Array(mut v)) => match v.pop() {
                Some(v) => Ok(v),
                None => Error::eval("trying to pop from an empty array"),
            },
            (UnOp::Negation, Val::Bool(b)) => Ok(Val::Bool(!b)),
            (UnOp::Negation, Val::Array(v)) => Ok(Val::Bool(v.is_empty())),
            (UnOp::Init, Val::Array(mut v)) => {
                v.pop();
                Ok(Val::Array(v))
            }
            (op, v) => Error::eval(format!("{} cannot be evaluated with {}", v, op)),
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
            (BinOp::And, Val::Bool(l), Val::Bool(r)) => Ok(Val::Bool(l && r)),
            (BinOp::Lt, Val::Int(l), Val::Int(r)) => Ok(Val::Bool(l < r)),
            (BinOp::Gt, Val::Int(l), Val::Int(r)) => Ok(Val::Bool(l > r)),
            (BinOp::Minus, Val::Int(l), Val::Int(r)) => Ok(Val::Int(l - r)),
            (BinOp::Plus, Val::Int(l), Val::Int(r)) => Ok(Val::Int(l + r)),
            (BinOp::Plus, Val::String(l), Val::String(r)) => Ok(Val::String(l + &r)),
            (BinOp::Plus, Val::Array(mut l), v) => {
                l.push(v);
                Ok(Val::Array(l))
            }
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
            (op, l, r) => Error::eval(format!("{} and {} cannot be evaluated with {}", l, r, op)),
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
            t => Error::parsing(format!("saw '{}', expected a binary op", t)),
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
