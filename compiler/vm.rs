use super::compiler::CValue;
use super::instruction::Instruction;
use crate::bytecode::Bytecode;
use saf::ast::Val;
use saf::err::{Error, Res};
use std::mem;

#[derive(Debug, Default)]
pub struct Vm {
    constants: Vec<CValue>,
    stack: Vec<CValue>,
    variables: Vec<(u16, CValue)>,
    instructions: Vec<Instruction>,
    symbols: Option<Vec<String>>,
}

impl Vm {
    fn get_const(&self, ind: u16) -> Res<&CValue> {
        match self.constants.get(ind as usize) {
            Some(v) => Ok(v),
            None => Error::eval(format!("constant {} not found", ind)),
        }
    }

    fn stack_pop(&mut self) -> Res<CValue> {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => Error::eval("trying to get value from an empty stack"),
        }
    }

    fn stack_push(&mut self, v: CValue) {
        self.stack.push(v)
    }

    fn get_var(&mut self, ind: u16) -> Res<&CValue> {
        match self.variables.iter().rev().find(|(i, _)| *i == ind) {
            Some((_, v)) => Ok(v),
            None => Error::eval(format!("{} is undefined", ind)),
        }
    }

    fn set_var(&mut self, ind: u16, v: CValue) {
        self.variables.push((ind, v));
    }

    fn clean_variables(&mut self, n: usize) -> Res<()> {
        self.variables.truncate(self.variables.len() - n);
        Ok(())
    }

    fn next_instr(&mut self) -> Res<Instruction> {
        match self.instructions.pop() {
            Some(i) => Ok(i),
            None => Error::eval("unexpected end of instructions"),
        }
    }

    fn skip_instr(&mut self, n: usize) -> Res<()> {
        (0..n).try_for_each(|_| self.next_instr().map(|_| ()))
    }

    fn run_instr(&mut self, i: Instruction) -> Res<()> {
        match i {
            Instruction::Bool(b) => self.stack_push(CValue::Bool(b)),
            Instruction::Constant(c) => self
                .get_const(u16::from_be_bytes(c))
                .cloned()
                .map(|c| self.stack_push(c))?,
            Instruction::GetVar(c) => self
                .get_var(u16::from_be_bytes(c))
                .cloned()
                .map(|v| self.stack_push(v))?,
            Instruction::SetVar(c) => self
                .stack_pop()
                .map(|v| self.set_var(u16::from_be_bytes(c), v))?,
            Instruction::Jump(n) => self.skip_instr(u16::from_be_bytes(n) as usize)?,
            Instruction::JumpIfNot(n) => match self.stack_pop()? {
                CValue::Bool(false) => self.skip_instr(u16::from_be_bytes(n) as usize)?,
                CValue::Bool(true) => {}
                v => Error::eval(format!("{} is not boolean", v))?,
            },
            Instruction::UnOp(op) => {
                let v = self.stack_pop()?;
                self.stack_push(op.eval(v)?)
            }
            Instruction::BinOp(op) => {
                let (l, r) = (self.stack_pop()?, self.stack_pop()?);
                self.stack_push(op.eval(l, r)?)
            }
            Instruction::Call([n]) => self.run_call(n as usize)?,
            Instruction::ExitCall([n]) => self.clean_variables(n as usize)?,
        };
        Ok(())
    }

    fn run_call(&mut self, n: usize) -> Res<()> {
        let (mut params, mut defs, mut stmts) = match self.stack_pop()? {
            Val::Function(p, d, s) => (p, d, s),
            v => Error::eval(format!("{} is not callable", v))?,
        };
        let args = self.stack.split_off(self.stack.len() - n);
        if args.len() > params.len() {
            Error::eval("too many arguments")?
        }
        let params_rest = params.split_off(args.len());
        defs.extend(params.into_iter().zip(args));
        if !params_rest.is_empty() {
            self.stack.push(Val::Function(params_rest, defs, stmts));
            return Ok(());
        }
        self.variables.append(&mut defs);
        self.instructions.append(&mut stmts);
        Ok(())
    }

    pub fn run_all(&mut self) -> Res<()> {
        while let Ok(i) = self.next_instr() {
            self.run_instr(i)?;
        }
        Ok(())
    }

    pub fn take_stack(&mut self) -> Vec<CValue> {
        mem::take(&mut self.stack)
    }

    pub fn read_bytecode(&mut self, btc: Vec<u8>) -> Res<()> {
        let mut btc = Bytecode::from(btc);
        (self.constants, self.instructions) = btc.deserialize()?;
        self.symbols = btc.deserialize_symbols()?;
        Ok(())
    }

    pub fn with_symbols(&self, val: &CValue) -> String {
        let syms = match self.symbols.as_ref() {
            Some(syms) => syms,
            None => return val.to_string(),
        };
        match val {
            Val::Array(arr) => {
                let vs: Vec<_> = arr.iter().map(|v| self.with_symbols(v)).collect();
                format!("[{}]", vs.join(", "))
            }
            Val::Function(pars, defs, _) => self.func_with_symbols(syms, pars, defs),
            _ => val.to_string(),
        }
    }

    fn func_with_symbols(
        &self,
        syms: &[String],
        pars: &[u16],
        defs: &[(u16, Val<u16, Instruction>)],
    ) -> String {
        let pars: Vec<_> = pars.iter().map(|i| syms[*i as usize].as_str()).collect();
        let defs: Vec<_> = defs
            .iter()
            .map(|(i, v)| format!("{} = {}", syms[*i as usize], self.with_symbols(v).as_str()))
            .collect();
        format!("fn({}) {{ {} ... }}", pars.join(", "), defs.join("; "))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::Compiler;
    use saf::ast::parse;
    use saf::text::scan;

    fn to_bytecode(prog: &str) -> Res<Vec<u8>> {
        let mut comp = Compiler::default();
        scan(prog).and_then(parse).and_then(|stmts| {
            comp.clear_instr();
            comp.add_stmts(stmts)
        })?;
        Ok(comp.bytecode_with_symbols())
    }

    fn to_vm(prog: &str) -> Res<Vm> {
        let mut vm = Vm::default();
        vm.read_bytecode(to_bytecode(prog)?)?;
        Ok(vm)
    }

    #[test]
    fn binary_expr() {
        let prog = "5 / 2;";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Int(2)]);
    }

    #[test]
    fn cond() {
        let prog = "5 > 2 ? 2 + 3 : 5 * 5;";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Int(2 + 3)]);

        let prog = "5 < 2 ? 2 + 3 : 5 * 5;";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Int(5 * 5)]);
    }

    #[test]
    fn var() {
        let prog = "x = 7; y = 11; (x - y) * y;";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Int((7 - 11) * 11)]);
    }

    #[test]
    fn const_function() {
        let prog = "f = fn() 7; f() + f();";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Int(14)]);
    }

    #[test]
    fn unary_function() {
        let prog = "f = fn(x) !x & true; f(true);";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Bool(false)]);
    }

    #[test]
    fn ternary_function() {
        let prog = r#"f = fn(x, y, z) x + y + z; f("a", "b", "c");"#;
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::String(String::from("abc"))]);
    }

    #[test]
    fn partial_application() {
        let prog = "f = fn(x, y) x + y; f1 = f(1); f1(7);";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Int(8)]);

        let prog = "f = fn(x, y, z) x + y + z; f(1)(2)(3) == f(1,2,3); ";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Bool(true)]);
    }

    #[test]
    fn recursion() {
        let prog = "fib = fn(n) n < 3 ? 1 : fib(n-1) + fib(n-2); fib(6);";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Int(8)]);
    }

    #[test]
    fn simple_array() {
        let prog = "b = 5; a = [1,2] + b; a;";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        let v = vec![1, 2, 5].into_iter().map(CValue::Int).collect();
        assert_eq!(vm.take_stack(), vec![CValue::Array(v)]);

        let prog = "a = [1,2,7,4,5]; -a;";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Int(5)]);

        let prog = "a = [1,2,7,4,5]; <a;";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        let v = vec![1, 2, 7, 4].into_iter().map(CValue::Int).collect();
        assert_eq!(vm.take_stack(), vec![CValue::Array(v)]);
    }

    #[test]
    fn subset_call_nest_silly() {
        let prog = "c = [1] + fn() [99]; -(-c)();";
        let mut vm = to_vm(prog).unwrap();
        vm.run_all().unwrap();
        assert_eq!(vm.take_stack(), vec![CValue::Int(99)]);
    }
}
