pub use expr::Expr;
pub use op::{BinOp, UnOp};
pub use parse::parse;
pub use stmt::Stmt;
pub use val::{Val, Value};

mod expr;
mod op;
mod parse;
mod stmt;
mod val;

fn fmt_vec<T: std::fmt::Display>(v: &[T], sep: &str) -> String {
    let stmts: Vec<_> = v.iter().map(ToString::to_string).collect();
    stmts.join(sep)
}
