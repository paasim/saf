use crate::ast::{BinOp, UnOp};
use std::{error, fmt, io, num, string};

#[derive(Debug)]
pub enum Error {
    Vm(String),
    Lexing(String),
    Parsing(String),
    Value(String),
    Compile(String),
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

    pub fn run_custom<T>(msg: impl fmt::Display) -> Res<T> {
        Err(Self::Vm(msg.to_string()))
    }

    pub fn compile_custom<T>(msg: impl fmt::Display) -> Res<T> {
        Err(Self::Compile(msg.to_string()))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Lexing(e) => write!(f, "lexing: {}", e),
            Error::Parsing(e) => write!(f, "parsing: {}", e),
            Error::Value(e) => write!(f, "evaluation: {}", e),
            Error::Vm(e) => write!(f, "vm: {}", e),
            Error::Compile(e) => write!(f, "compilation: {}", e),
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

impl From<string::FromUtf8Error> for Error {
    fn from(e: string::FromUtf8Error) -> Self {
        Self::Parsing(e.to_string())
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::Parsing(e.to_string())
    }
}
