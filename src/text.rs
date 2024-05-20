pub use scan::scan;
use std::{iter, str, vec};
pub use token::{expect_ident, expect_token, next_token, Token};

type Chars<'a> = iter::Peekable<str::Chars<'a>>;
pub type Tokens = iter::Peekable<vec::IntoIter<Token>>;

mod scan;
mod token;
