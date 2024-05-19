use super::{Chars, Tokens};
use crate::err::{Error, Res};
use std::{fmt, str, vec};

#[derive(Debug, PartialEq)]
pub enum Token {
    // Identifiers + literals
    Ident(String),
    Int(isize),
    String(String),

    // Operators
    Assign,
    And,
    Or,
    Plus,
    Minus,
    Negation,
    Mult,
    Div,
    Eq,
    NotEq,
    Gt,
    Lt,
    Cond,
    Colon,
    Len,
    TypeOf,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbracket,
    Rbracket,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    True,
    False,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(v) => write!(f, "{}", v),
            Self::Int(i) => write!(f, "{}", i),
            Self::String(s) => write!(f, "{}", s),
            Self::Assign => write!(f, "="),
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Negation => write!(f, "!"),
            Self::Mult => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Eq => write!(f, "=="),
            Self::NotEq => write!(f, "!="),
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::Cond => write!(f, "?"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::Lparen => write!(f, "("),
            Self::Rparen => write!(f, ")"),
            Self::Lbracket => write!(f, "["),
            Self::Rbracket => write!(f, "]"),
            Self::Lbrace => write!(f, "{{"),
            Self::Rbrace => write!(f, "}}"),
            Self::Function => write!(f, "fn"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Len => write!(f, "#"),
            Self::TypeOf => write!(f, "@"),
        }
    }
}

pub fn expect_token(tokens: &mut impl Iterator<Item = Token>, t: &Token) -> Res<Token> {
    match tokens.next() {
        Some(t_) if t == &t_ => Ok(t_),

        Some(t_) => Error::parsing(format!("expected '{}', saw '{}'", t, t_)),
        None => Error::lexing(format!("expected '{}'", t)),
    }
}

pub fn next_token(tokens: &mut Tokens, exp: &str) -> Res<Token> {
    match tokens.next() {
        Some(t) => Ok(t),
        None => Error::parsing(format!("expected '{}'", exp)),
    }
}

impl Token {
    pub fn scan_opt(chars: &mut Chars) -> Option<Res<Self>> {
        chars.next().map(|c| Self::scan_next(c, chars))
    }

    pub fn scan_next(c: char, chars: &mut Chars) -> Res<Self> {
        match c {
            '=' => Ok(Self::scan_equal(chars)),
            '!' => Ok(Self::scan_bang(chars)),
            '&' => Ok(Self::And),
            '|' => Ok(Self::Or),
            '+' => Ok(Self::Plus),
            '-' => Ok(Self::Minus),
            '*' => Ok(Self::Mult),
            '/' => Ok(Self::Div),
            '?' => Ok(Self::Cond),
            ':' => Ok(Self::Colon),
            ',' => Ok(Self::Comma),
            ';' => Ok(Self::Semicolon),
            '(' => Ok(Self::Lparen),
            ')' => Ok(Self::Rparen),
            '[' => Ok(Self::Lbracket),
            ']' => Ok(Self::Rbracket),
            '{' => Ok(Self::Lbrace),
            '}' => Ok(Self::Rbrace),
            '<' => Ok(Self::Lt),
            '>' => Ok(Self::Gt),
            '@' => Ok(Self::TypeOf),
            '#' => Ok(Self::Len),
            '"' => Self::scan_string(chars),
            '_' => Ok(Self::scan_ident('_', chars)),
            '0'..='9' => Self::scan_digit(c, chars),
            'A'..='Z' => Ok(Self::scan_ident(c, chars)),
            'a'..='z' => Ok(Self::scan_ident(c, chars)),
            _ => Error::lexing(format!("unexpected '{}'", c)),
        }
    }

    fn scan_equal(chars: &mut Chars) -> Self {
        chars.next_if_eq(&'=').map_or(Self::Assign, |_| Self::Eq)
    }

    fn scan_bang(chars: &mut Chars) -> Self {
        chars
            .next_if_eq(&'=')
            .map_or(Self::Negation, |_| Self::NotEq)
    }

    fn scan_digit(d0: char, chars: &mut Chars) -> Res<Self> {
        let digits = scan_seq(Some(d0), chars, |c| c.is_ascii_digit());
        Ok(Self::Int(digits.parse()?))
    }

    fn scan_ident(c0: char, chars: &mut Chars) -> Self {
        let s = scan_seq(Some(c0), chars, |c| c.is_ascii_alphanumeric() || *c == '_');
        match s.as_str() {
            "fn" => Self::Function,
            "true" => Self::True,
            "false" => Self::False,
            _ => Self::Ident(s),
        }
    }

    fn scan_string(chars: &mut Chars) -> Res<Self> {
        let s = scan_seq(None, chars, |c| c.is_ascii_alphanumeric() || *c != '"');
        match chars.next() {
            Some('"') => Ok(Self::String(s)),
            Some(c) => Error::lexing(format!("expected '\"', saw '{}'", c)),
            None => Error::lexing("expected '\"'"),
        }
    }
}

fn scan_seq(c: Option<char>, chars: &mut Chars, f: impl Fn(&char) -> bool) -> String {
    let mut s = c.map_or(vec![], |c| vec![c]);
    while let Some(c) = chars.next_if(&f) {
        s.push(c);
    }
    String::from_iter(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_single_char_works() {
        let v = vec![
            ("=", Token::Assign),
            (",", Token::Comma),
            ("}", Token::Rbrace),
        ];
        for (c, t_exp) in v {
            let t = Token::scan_opt(&mut c.chars().peekable()).unwrap().unwrap();
            assert_eq!(t, t_exp);
        }
        assert!(Token::scan_opt(&mut "".chars().peekable()).is_none());
        assert!(Token::scan_opt(&mut "^".chars().peekable())
            .unwrap()
            .is_err());
    }

    #[test]
    fn scan_digit_works() {
        let v = vec!["0", "7", "53", "0909"];
        for n in v {
            let t = Token::scan_opt(&mut n.chars().peekable()).unwrap().unwrap();
            assert_eq!(t, Token::Int(n.parse().unwrap()));
        }
    }

    #[test]
    fn scan_string_works() {
        let v = vec![("asddd\""), ("file;\n\n name\"")];
        for s in v {
            let t = Token::scan_string(&mut s.chars().peekable()).unwrap();
            assert_eq!(t, Token::String(String::from(&s[0..s.len() - 1])));
        }
    }
}
