use std::{error, fmt, io, num, string};

#[derive(Debug)]
pub enum Error {
    Lexing(String),
    Parsing(String),
    Eval(String),
    Compile(String),
}

pub type Res<T> = Result<T, Error>;

impl Error {
    pub fn eval<T>(msg: impl fmt::Display) -> Res<T> {
        Err(Self::Eval(msg.to_string()))
    }

    pub fn lexing<T>(msg: impl fmt::Display) -> Res<T> {
        Err(Self::Lexing(msg.to_string()))
    }

    pub fn parsing<T>(msg: impl fmt::Display) -> Res<T> {
        Err(Self::Parsing(msg.to_string()))
    }

    pub fn compile<T>(msg: impl fmt::Display) -> Res<T> {
        Err(Self::Compile(msg.to_string()))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Lexing(e) => write!(f, "lexing: {}", e),
            Error::Parsing(e) => write!(f, "parsing: {}", e),
            Error::Eval(e) => write!(f, "eval: {}", e),
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
