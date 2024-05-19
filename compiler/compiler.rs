use super::instruction::Instruction;
use crate::bytecode::{add_symbols, to_bytecode};
use saf::ast::{Expr, Stmt, Val, Value};
use saf::err::{Error, Res};
use std::collections::HashMap;
use std::fmt;

pub type CValue = Val<u16, Instruction>;

#[derive(Debug, Default)]
pub struct Compiler {
    instructions: Vec<Instruction>,
    constants: HashMap<CValue, u16>,
    symbols: HashMap<String, u16>,
}

impl Compiler {
    pub fn add_instr(&mut self, instr: Instruction) -> usize {
        self.instructions.push(instr);
        1
    }

    pub fn clear_instr(&mut self) {
        self.instructions.clear()
    }

    pub fn add_const(&mut self, val: CValue) -> u16 {
        if let Ok(v) = self.get_const(&val) {
            return v;
        }
        let len = self.constants.len() as u16;
        self.constants.insert(val, len);
        len
    }

    fn get_const(&self, c: &CValue) -> Res<u16> {
        match self.constants.get(c) {
            Some(ind) => Ok(*ind),
            None => Error::compile(format!("constant {} not defined", c)),
        }
    }

    fn add_sym(&mut self, sym: String) -> u16 {
        if let Ok(i) = self.resolve_sym(&sym) {
            return i;
        }
        let len = self.symbols.len() as u16;
        self.symbols.insert(sym, len);
        len
    }

    fn resolve_sym(&self, sym: &str) -> Res<u16> {
        match self.symbols.get(sym).copied() {
            Some(s) => Ok(s),
            None => Error::compile(format!("symbol {} not defined", sym)),
        }
    }

    pub fn add_val(&mut self, v: Value) -> Res<usize> {
        match v {
            Val::Bool(b) => Ok(self.add_instr(Instruction::Bool(b))),
            Val::String(_) | Val::Int(_) | Val::Array(_) | Val::Function(_, _, _) => {
                let v = self.compile_val(v)?;
                let const_pos = self.add_const(v).to_be_bytes();
                Ok(self.add_instr(Instruction::Constant(const_pos)))
            }
        }
    }

    fn compile_val(&mut self, val: Value) -> Res<CValue> {
        match val {
            Val::Bool(b) => Ok(Val::Bool(b)),
            Val::Int(i) => Ok(Val::Int(i)),
            Val::String(s) => Ok(Val::String(s)),
            Val::Array(a) => a
                .into_iter()
                .map(|t| self.compile_val(t))
                .collect::<Res<_>>()
                .map(Val::Array),
            Val::Function(pars, defs, stmts) => self.add_func(pars, defs, stmts),
        }
    }

    fn add_func(
        &mut self,
        pars: Vec<String>,
        defs: Vec<(String, Val<String, Stmt>)>,
        stmts: Vec<Stmt>,
    ) -> Res<CValue> {
        let pars: Vec<_> = pars.into_iter().map(|s| self.add_sym(s)).collect();
        let n_instr = self.instructions.len();
        let defs = defs
            .into_iter()
            .map(|(s, v)| Ok((self.resolve_sym(&s)?, self.compile_val(v)?)))
            .collect::<Res<_>>()?;
        stmts
            .into_iter()
            .try_for_each(|def| self.add_stmt(def).map(|_| ()))?;
        let mut stmts = self.instructions.split_off(n_instr);
        stmts.push(Instruction::ExitCall([pars.len() as u8]));
        stmts.reverse();
        Ok(CValue::Function(pars, defs, stmts))
    }

    fn add_expr(&mut self, e: Expr) -> Res<usize> {
        match e {
            Expr::Call(e, args) => {
                let args_len = args.len();
                Ok(args
                    .into_iter()
                    .map(|a| self.add_expr(a))
                    .sum::<Res<usize>>()?
                    + self.add_expr(*e)?
                    + self.add_instr(Instruction::Call([args_len as u8])))
            }
            Expr::Cond(cond, true_, false_) => self.add_cond(*cond, *true_, *false_),
            Expr::Binary(lhs, op, rhs) => Ok(self.add_expr(*rhs)?
                + self.add_expr(*lhs)?
                + self.add_instr(Instruction::BinOp(op))),
            Expr::Unary(op, e) => Ok(self.add_expr(*e)? + self.add_instr(Instruction::UnOp(op))),
            Expr::Ident(s) => {
                let sym = self.add_sym(s);
                Ok(self.add_instr(Instruction::GetVar(sym.to_be_bytes())))
            }
            Expr::Value(v) => self.add_val(v),
        }
    }

    fn add_cond(&mut self, cond: Expr, true_: Expr, false_: Expr) -> Res<usize> {
        let jmp_if = Instruction::JumpIfNot(0u16.to_be_bytes());
        let w = self.add_expr(cond)? + self.add_instr(jmp_if);
        let jmp_if_pos = self.instructions.len() - 1;

        let jmp = Instruction::Jump(0u16.to_be_bytes());
        let true_w = self.add_expr(true_)? + self.add_instr(jmp);
        let jmp_pos = self.instructions.len() - 1;
        self.fix_jmp_pos(jmp_if_pos, true_w as u16)?;
        let false_w = self.add_expr(false_)?;
        self.fix_jmp_pos(jmp_pos, false_w as u16)?;
        Ok(w + true_w + false_w)
    }

    fn fix_jmp_pos(&mut self, pos: usize, val: u16) -> Res<()> {
        match self.instructions.get_mut(pos) {
            Some(Instruction::JumpIfNot(v)) => *v = val.to_be_bytes(),
            Some(Instruction::Jump(v)) => *v = val.to_be_bytes(),
            _ => Error::compile("expected jump if")?,
        };
        Ok(())
    }
    pub fn add_stmts(&mut self, stmts: Vec<Stmt>) -> Res<usize> {
        stmts.into_iter().map(|s| self.add_stmt(s)).sum()
    }

    pub fn add_stmt(&mut self, s: Stmt) -> Res<usize> {
        match s {
            Stmt::Let(s, e) => {
                let we = self.add_expr(e)?;
                let sym = self.add_sym(s).to_be_bytes();
                Ok(we + self.add_instr(Instruction::SetVar(sym)))
            }
            Stmt::Expr(e) => self.add_expr(e),
        }
    }

    pub fn bytecode(&self) -> Vec<u8> {
        to_bytecode(&self.constants, &self.instructions)
    }

    pub fn bytecode_with_symbols(&self) -> Vec<u8> {
        add_symbols(self.bytecode(), &self.symbols)
    }
}

impl fmt::Display for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: Vec<_> = self.instructions.iter().map(ToString::to_string).collect();
        write!(f, "{}", s.join("\n"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::Bytecode;
    use saf::ast::{parse, BinOp, UnOp};
    use saf::text::scan;

    #[test]
    fn instrs() {
        let mut comp = Compiler::default();
        comp.add_instr(Instruction::Constant([0, 1]));
        comp.add_instr(Instruction::Constant([0, 2]));
        comp.add_instr(Instruction::BinOp(BinOp::Plus));
        let s = comp.to_string();
        let exp = "Constant 1\nConstant 2\nPlus";
        assert_eq!(s, exp);
    }

    #[test]
    fn consts() {
        let mut comp = Compiler::default();
        assert_eq!(comp.add_instr(Instruction::Constant([0, 1])), 1);
        assert_eq!(comp.add_instr(Instruction::Constant([0, 2])), 1);
        assert_eq!(
            comp.add_instr(Instruction::Constant(65535u16.to_be_bytes())),
            1
        );
        let s = comp.to_string();
        let exp = "Constant 1\nConstant 2\nConstant 65535";
        assert_eq!(s, exp);
        assert_eq!(comp.constants.len(), 0);
        assert_eq!(comp.symbols.len(), 0);
    }

    #[test]
    fn sym() {
        let mut comp = Compiler::default();
        let s0 = "sym0";
        let s1 = "sym1";
        let a0 = comp.add_sym(s0.to_string());
        let a1 = comp.add_sym(s1.to_string());
        assert_eq!(comp.add_sym(s0.to_string()), a0);
        assert_eq!(comp.instructions.len(), 0);
        assert_eq!(comp.constants.len(), 0);
        assert_eq!(comp.symbols.len(), 2);
        assert_eq!(comp.resolve_sym("sym0").unwrap(), a0);
        assert_eq!(comp.resolve_sym("sym1").unwrap(), a1);
    }

    #[test]
    fn bool() {
        let mut comp = Compiler::default();
        let b = true;
        assert_eq!(comp.add_val(Value::Bool(b)).unwrap(), 1);
        assert_eq!(comp.instructions, vec![Instruction::Bool(b)]);
        assert_eq!(comp.constants.len(), 0);
        assert_eq!(comp.symbols.len(), 0);
    }

    #[test]
    fn vals() {
        let mut comp = Compiler::default();
        let s = "str0";
        let i = 5;
        let v0 = Value::String("str0".to_string());
        let v1 = Value::Int(i);
        assert_eq!(comp.add_val(v0.clone()).unwrap(), 1);
        assert_eq!(comp.add_val(v1.clone()).unwrap(), 1);
        let instr_exp = vec![Instruction::Constant([0, 0]), Instruction::Constant([0, 1])];
        assert_eq!(comp.instructions, instr_exp);
        let constants_exp =
            HashMap::from([(CValue::String(s.to_string()), 0), (CValue::Int(i), 1)]);
        assert_eq!(comp.constants, constants_exp);
        assert_eq!(comp.symbols.len(), 0);
    }

    #[test]
    fn array() {}

    #[test]
    fn ident() {
        let mut comp = Compiler::default();
        let x = "x";
        assert_eq!(comp.add_sym(x.to_string()), 0);
        let e = Expr::Ident(x.to_string());
        assert_eq!(comp.add_expr(e).unwrap(), 1);
        assert_eq!(comp.instructions.len(), 1);
        assert_eq!(comp.constants.len(), 0);
        assert_eq!(comp.symbols.len(), 1);
        let ax = comp.resolve_sym("x").unwrap();
        let i_exp = Instruction::GetVar(ax.to_be_bytes());
        assert_eq!(comp.instructions.pop().unwrap(), i_exp);
    }

    #[test]
    fn unary() {
        let mut comp = Compiler::default();
        let b = false;
        let e = Box::new(Expr::Value(Value::Bool(b)));
        let len = 2;
        assert_eq!(comp.add_expr(Expr::Unary(UnOp::Negation, e)).unwrap(), len);
        assert_eq!(comp.instructions.len(), len);
        let instr_exp = vec![Instruction::Bool(b), Instruction::UnOp(UnOp::Negation)];
        assert_eq!(comp.instructions, instr_exp);
        assert_eq!(comp.constants.len(), 0);
        assert_eq!(comp.symbols.len(), 0);
    }

    #[test]
    fn binary() {
        let mut comp = Compiler::default();
        let i = 2;
        let l = Expr::Value(Value::Int(i));
        let x = "x";
        let ax = comp.add_sym(x.to_string());
        let r = Expr::Unary(UnOp::Minus, Box::new(Expr::Ident(x.to_string())));
        let e = Expr::Binary(Box::new(l), BinOp::Eq, Box::new(r));
        let len = 4;
        assert_eq!(comp.add_expr(e).unwrap(), len);
        let instr_exp = vec![
            Instruction::GetVar(ax.to_be_bytes()),
            Instruction::UnOp(UnOp::Minus),
            Instruction::Constant([0, 0]),
            Instruction::BinOp(BinOp::Eq),
        ];
        assert_eq!(comp.instructions, instr_exp);
        assert_eq!(comp.constants, HashMap::from([(CValue::Int(i), 0)]));
        assert_eq!(comp.symbols.len(), 1);
    }

    #[test]
    fn cond() {
        let mut comp = Compiler::default();
        let b = true;
        let cond = Box::new(Expr::Value(Value::Bool(b)));
        let l = Expr::Value(Value::Bool(b));
        let x = "x";
        let ax = comp.add_sym(x.to_string());
        let r = Expr::Unary(UnOp::Negation, Box::new(Expr::Ident(x.to_string())));
        let true_ = Box::new(Expr::Binary(Box::new(l), BinOp::And, Box::new(r)));
        let y = "y";
        let yx = comp.add_sym(y.to_string());
        let y_ = Box::new(Expr::Ident(y.to_string()));
        let false_ = Box::new(Expr::Unary(UnOp::Paren, y_));
        let w = comp.add_expr(Expr::Cond(cond, true_, false_)).unwrap();
        // jumpifnot + cond + true_ + jump + false_
        assert_eq!(w, 9);
        let instr_exp = vec![
            Instruction::Bool(b),
            Instruction::JumpIfNot([0, 5]),
            Instruction::GetVar(ax.to_be_bytes()),
            Instruction::UnOp(UnOp::Negation),
            Instruction::Bool(b),
            Instruction::BinOp(BinOp::And),
            Instruction::Jump([0, 2]),
            Instruction::GetVar(yx.to_be_bytes()),
            Instruction::UnOp(UnOp::Paren),
        ];
        assert_eq!(comp.instructions, instr_exp);
    }

    #[test]
    fn let_stmt() {
        let mut comp = Compiler::default();
        let x = "x";
        let b = true;
        let b_ = Box::new(Expr::Value(Value::Bool(b)));
        let stmt = Stmt::Let(x.to_string(), Expr::Unary(UnOp::Negation, b_));
        assert_eq!(comp.add_stmt(stmt).unwrap(), 3);
        let ax = comp.resolve_sym(x).unwrap();
        let instr_exp = vec![
            Instruction::Bool(b),
            Instruction::UnOp(UnOp::Negation),
            Instruction::SetVar(ax.to_be_bytes()),
        ];
        assert_eq!(comp.instructions, instr_exp);
    }

    #[test]
    fn function() {
        let mut comp = Compiler::default();
        let pars = vec!["x".to_string(), "_".to_string()];
        let vy = 5;
        let defs = vec![("y".to_string(), Value::Int(vy))];

        let x = Expr::Ident("x".to_string());
        let y = Expr::Ident("y".to_string());
        let stmt = Stmt::Expr(Expr::Binary(Box::new(x), BinOp::Plus, Box::new(y)));
        let f = Value::Function(pars.clone(), defs, vec![stmt]);
        let ay = comp.add_sym("y".to_string());
        assert_eq!(comp.add_val(f).unwrap(), 1);
        assert_eq!(comp.to_string(), "Constant 0");

        let ax = comp.resolve_sym("x").unwrap();
        let a_ = comp.resolve_sym("_").unwrap();
        assert_eq!(comp.resolve_sym("y").unwrap(), ay);
        let defs = vec![(ay, CValue::Int(vy))];
        let instrs = vec![
            Instruction::ExitCall([pars.len() as u8]),
            Instruction::BinOp(BinOp::Plus),
            Instruction::GetVar(ax.to_be_bytes()),
            Instruction::GetVar(ay.to_be_bytes()),
        ];
        let f_exp = CValue::Function(vec![ax, a_], defs, instrs);
        assert_eq!(comp.constants, HashMap::from([(f_exp, 0)]));
    }

    #[test]
    fn call() {
        let mut comp = Compiler::default();

        let f = "f";
        let fname = Expr::Ident(f.to_string());
        let (x, y, z) = (9, 13, 77);
        let x_ = Expr::Value(Value::Int(x));
        let y_ = Expr::Value(Value::Int(y));
        let bin = Expr::Binary(Box::new(x_), BinOp::Minus, Box::new(y_));
        let z_ = Expr::Value(Value::Int(z));
        let args = vec![z_, bin];
        let n_args = args.len() as u8;
        let call = Expr::Call(Box::new(fname), args);

        let af = comp.add_sym(f.to_string());
        // bin + z + def f + call + exit
        assert_eq!(comp.add_expr(call).unwrap(), 1 + 1 + 1 + 3);
        let az = [0, 0];
        let ay = [0, 1];
        let ax = [0, 2];
        let constants_exp = vec![CValue::Int(z), CValue::Int(y), CValue::Int(x)]
            .into_iter()
            .enumerate()
            .map(|(i, v)| (v, i as u16))
            .collect();

        assert_eq!(comp.constants, constants_exp);
        let instr_exp = vec![
            Instruction::Constant(az),
            Instruction::Constant(ay),
            Instruction::Constant(ax),
            Instruction::BinOp(BinOp::Minus),
            Instruction::GetVar(af.to_be_bytes()),
            Instruction::Call([n_args]),
        ];
        assert_eq!(comp.instructions, instr_exp);
    }

    #[test]
    fn to_bytecode_works() {
        let prog = r#"
            z = 5;
            plus = fn(x, y) x + y;
            plus(z, 2);

            c = [1] < fn() [99];
            -(-c)();

            fib = fn(n) n < 3 ? 1 : fib(n-1) + fib(n-2);
            fib(10);
        "#;
        let mut comp = Compiler::default();
        scan(prog)
            .and_then(parse)
            .and_then(|stmts| comp.add_stmts(stmts))
            .unwrap();
        let mut btc = Bytecode::from(comp.bytecode_with_symbols());
        let (constants, mut instructions) = btc.deserialize().unwrap();

        // constants can be recovered
        let constants = constants
            .into_iter()
            .enumerate()
            .map(|(i, v)| (v, i as u16))
            .collect();
        assert_eq!(comp.constants, constants);

        // instructions can be recovered
        instructions.reverse();
        assert_eq!(comp.instructions, instructions);

        // symbols can be recovered
        let symbols = btc.deserialize_symbols().unwrap().unwrap();
        let symbols = symbols
            .into_iter()
            .enumerate()
            .map(|(i, s)| (s, i as u16))
            .collect();
        assert_eq!(comp.symbols, symbols);
    }
}
