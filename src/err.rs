use crate::ast::{BinOp, UnOp};
use std::{error, fmt, num};

#[derive(Debug)]
pub enum Error {
    Lexing(String),
    Parsing(String),
    Value(String),
}

pub type Res<T> = Result<T, Error>;

fn mismatch<U: fmt::Display>(saw: Option<U>, exp: Vec<U>) -> String {
    let exp: Option<String> = if exp.is_empty() {
        None
    } else {
        let strs: Vec<_> = exp.into_iter().map(|u| u.to_string()).collect();
        Some(strs.join(", "))
    };
    match (saw, exp) {
        (Some(saw), Some(exp)) => format!("unexpected '{}', expected '{}'", saw, exp),
        (Some(saw), _) => format!("unexpected '{}'", saw),
        (_, Some(exp)) => format!("expected '{}'", exp),
        _ => "unexpected input".to_string(),
    }
}

impl Error {
    pub fn lexing<T, U: fmt::Display>(saw: Option<U>, exp: Vec<U>) -> Res<T> {
        Err(Self::Lexing(mismatch(saw, exp)))
    }

    pub fn parsing<T, U: fmt::Display>(saw: Option<U>, exp: Vec<U>) -> Res<T> {
        Err(Self::Parsing(mismatch(saw, exp)))
    }

    pub fn unop<T>(op: &UnOp, v: impl fmt::Display) -> Res<T> {
        Err(Self::Value(format!(
            "{} cannot be evaluated with {}",
            v, op
        )))
    }

    pub fn binop<T>(lhs: impl fmt::Display, rhs: impl fmt::Display, op: &BinOp) -> Res<T> {
        Err(Self::Value(format!(
            "{} and {} cannot be evaluated with {}",
            lhs, rhs, op
        )))
    }

    pub fn undefined<T>(var: &str) -> Res<T> {
        Err(Self::Value(format!("{} is undefined", var)))
    }

    pub fn lexing_custom<T>(msg: impl fmt::Display) -> Res<T> {
        Err(Self::Lexing(msg.to_string()))
    }

    pub fn parsing_custom<T>(msg: impl fmt::Display) -> Res<T> {
        Err(Self::Parsing(msg.to_string()))
    }

    pub fn value_custom<T>(msg: impl fmt::Display) -> Res<T> {
        Err(Self::Value(msg.to_string()))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Lexing(e) => write!(f, "{}", e),
            Error::Parsing(e) => write!(f, "{}", e),
            Error::Value(e) => write!(f, "{}", e),
        }
    }
}

impl error::Error for Error {}

impl From<num::ParseIntError> for Error {
    fn from(e: num::ParseIntError) -> Self {
        Self::Lexing(e.to_string())
    }
}

impl From<num::TryFromIntError> for Error {
    fn from(e: num::TryFromIntError) -> Self {
        Self::Lexing(e.to_string())
    }
}
