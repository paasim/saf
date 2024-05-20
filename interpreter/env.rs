use saf::ast::{Expr, Stmt, Val, Value};
use saf::err::{Error, Res};
use std::collections::HashMap;

#[derive(Default)]
pub struct Env<'a> {
    vars: HashMap<String, Value>,
    parent: Option<&'a Env<'a>>,
}

impl<'a> Env<'a> {
    fn insert_var(&mut self, k: String, v: Value) -> Option<Value> {
        self.vars.insert(k, v)
    }

    fn get_opt(&self, k: &str) -> Option<&Value> {
        match self.vars.get(k) {
            Some(v) => Some(v),
            None => self.parent.and_then(|p| p.get_opt(k)),
        }
    }

    fn get_var(&self, k: &str) -> Res<&Value> {
        match self.get_opt(k) {
            Some(v) => Ok(v),
            None => Error::eval(format!("{} is undefined", k)),
        }
    }

    pub fn eval_stmts(&mut self, stmts: Vec<Stmt>) -> Res<Option<Value>> {
        let last = stmts.into_iter().map(|stmt| self.eval_stmt(stmt)).last();
        last.transpose().map(|v| v.flatten())
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Res<Option<Value>> {
        match stmt {
            Stmt::Let(ident, e) => Ok(self.insert_var(ident, self.eval_expr(e)?)),
            Stmt::Expr(e) => self.eval_expr(e).map(Some),
        }
    }

    fn eval_expr(&self, expr: Expr) -> Res<Value> {
        match expr {
            Expr::Value(v) => Ok(v),
            Expr::Ident(ident) => self.get_var(&ident).cloned(),
            Expr::Unary(op, e) => self.eval_expr(*e).and_then(|v| op.eval(v)),
            Expr::Binary(lhs, op, rhs) => op.eval(self.eval_expr(*lhs)?, self.eval_expr(*rhs)?),
            Expr::Cond(cond, true_, false_) => match self.eval_expr(*cond)? {
                Val::Bool(true) => self.eval_expr(*true_),
                Val::Bool(false) => self.eval_expr(*false_),
                e => Error::eval(format!("condition '{}' is not boolean", e)),
            },
            Expr::Call(e, args) => self.eval_call(*e, args),
        }
    }

    fn eval_call(&self, expr: Expr, args: Vec<Expr>) -> Res<Value> {
        let (mut params, mut defs, stmts) = match self.eval_expr(expr)? {
            Val::Function(params, defs, stmts) => (params, defs, stmts),
            v => Error::eval(format!("{} is not callable", v))?,
        };
        if args.len() > params.len() {
            Error::eval("too many arguments")?
        }
        let params_rest = params.split_off(args.len());
        let new_defs = args
            .into_iter()
            .zip(params)
            .map(|(arg, par)| Ok((par, self.eval_expr(arg)?)))
            .collect::<Res<Vec<_>>>()?;
        defs.extend(new_defs);
        match params_rest.is_empty() {
            true => self.eval_within_subenv(defs.into_iter().collect(), stmts),
            false => Ok(Val::Function(params_rest, defs, stmts)),
        }
    }

    fn eval_within_subenv(&'a self, vars: HashMap<String, Value>, stmts: Vec<Stmt>) -> Res<Value> {
        let mut env = Self {
            vars,
            parent: Some(self),
        };
        match env.eval_stmts(stmts)? {
            Some(v) => Ok(v),
            _ => Error::eval("last statement is not an expression")?,
        }
    }
}

#[cfg(test)]
mod tests {
    use saf::ast::parse;
    use saf::text::scan;

    use super::*;

    fn to_ast(prog: &str) -> Res<Vec<Stmt>> {
        scan(prog).and_then(parse)
    }

    #[test]
    fn binary_expr() {
        let prog = "5 / 2;";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Int(2)));
    }

    #[test]
    fn cond() {
        let prog = "5 > 2 ? 2 + 3 : 5 * 5;";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Int(2 + 3)));

        let prog = "5 < 2 ? 2 + 3 : 5 * 5;";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Int(5 * 5)));
    }

    #[test]
    fn var() {
        let prog = "x = 7; y = 11; (x - y) * y;";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Int((7 - 11) * 11)));
    }

    #[test]
    fn const_function() {
        let prog = "f = fn() 7; f() + f();";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Int(14)));
    }

    #[test]
    fn unary_function() {
        let prog = "f = fn(x) !x & true; f(true);";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Bool(false)));
    }

    #[test]
    fn ternary_function() {
        let prog = r#"f = fn(x, y, z) x + y + z; f("a", "b", "c");"#;
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::String(String::from("abc"))));
    }

    #[test]
    fn partial_application() {
        let prog = "f = fn(x, y) x + y; f1 = f(1); f1(7);";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Int(1 + 7)));

        let prog = "f = fn(x, y, z) x + y + z; f(1)(2)(3) == f(1,2,3); ";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Bool(true)));
    }

    #[test]
    fn recursion() {
        let prog = "fib = fn(n) n < 3 ? 1 : fib(n-1) + fib(n-2); fib(6);";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Int(8)));
    }

    #[test]
    fn simple_array() {
        let prog = "b = 5; a = [1,2] + b; a;";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        let v = vec![1, 2, 5].into_iter().map(Value::Int).collect();
        assert_eq!(val, Some(Value::Array(v)));

        let prog = "a = [1,2,7,4,5]; -a;";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Int(5)));

        let prog = "a = [1,2,7,4,5]; <a;";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        let v = vec![1, 2, 7, 4].into_iter().map(Value::Int).collect();
        assert_eq!(val, Some(Value::Array(v)));
    }

    #[test]
    fn subset_call_nest_silly() {
        let prog = "c = [1] + fn() [99]; -(-c)();";
        let mut env = Env::default();
        let val = to_ast(prog)
            .and_then(|stmts| env.eval_stmts(stmts))
            .unwrap();
        assert_eq!(val, Some(Value::Int(99)));
    }
}
