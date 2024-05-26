use super::Token;
use crate::err::Res;
use std::{iter, str};

pub fn scan(s: &str) -> Res<Vec<Token>> {
    Scanner::from(s).collect()
}

pub struct Scanner<'a>(iter::Peekable<str::Chars<'a>>);

impl<'a> Scanner<'a> {
    fn skip_until_next_line(&mut self) {
        while self.0.next_if(|c| *c != '\n').is_some() {}
        self.0.next_if_eq(&'\n');
    }
}

impl<'a> From<&'a str> for Scanner<'a> {
    fn from(s: &'a str) -> Self {
        Self(s.chars().peekable())
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Res<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.0.next_if(|c| c.is_whitespace() || *c == '#') {
            if c == '#' {
                self.skip_until_next_line()
            }
        }
        Token::scan_opt(&mut self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scanner_works() {
        let s = r#"
            five = 5;
            ten = 10;
            add = fn(x, y) x + y;
            result = add(five, ten);"#;

        let tokens: Vec<Token> = scan(s).unwrap();
        let exp = vec![
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::Rparen,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::Lparen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::Rparen,
            Token::Semicolon,
        ];
        assert_eq!(tokens, exp);
    }

    #[test]
    fn skip_comment_works() {
        let s = r#"
            five = 5;
            # this is a comment
            # this is another comment
            ten = 10;
            add = fn(x, y) x + y; # as is this: x = 2 * 5;
            result = add(five, ten);"#;

        let tokens: Vec<Token> = scan(s).unwrap();
        let exp = vec![
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::Rparen,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::Lparen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::Rparen,
            Token::Semicolon,
        ];
        assert_eq!(tokens, exp);
    }
}
