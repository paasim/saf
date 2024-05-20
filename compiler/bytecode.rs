use crate::compiler::CValue;
use crate::instruction::Instruction;
use saf::err::{Error, Res};
use std::collections::HashMap;
use std::{fmt, vec};

const BYTECODE_VERSION: u8 = 0;

#[derive(Debug)]
enum State {
    New,
    VersionRead,
    ConstantsRead,
    InstructionsRead,
    SymbolsRead,
}

impl State {
    fn incr(&mut self) {
        match self {
            State::New => *self = State::VersionRead,
            State::VersionRead => *self = State::ConstantsRead,
            State::ConstantsRead => *self = State::InstructionsRead,
            State::InstructionsRead => *self = State::SymbolsRead,
            State::SymbolsRead => *self = State::SymbolsRead,
        }
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            State::New => write!(f, "initialized"),
            State::VersionRead => write!(f, "version read"),
            State::ConstantsRead => write!(f, "constants read"),
            State::InstructionsRead => write!(f, "instructions read"),
            State::SymbolsRead => write!(f, "symbols read"),
        }
    }
}

pub struct Bytecode {
    bytes: vec::IntoIter<u8>,
    state: State,
}

impl Bytecode {
    fn next(&mut self) -> Res<u8> {
        match self.bytes.next() {
            Some(b) => Ok(b),
            None => Error::parsing("unexpected end of bytes")?,
        }
    }

    fn next_8(&mut self) -> Res<[u8; 8]> {
        Ok([
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
        ])
    }

    fn deser_vec<T>(&mut self, mut f: impl FnMut(&mut Self) -> Res<T>) -> Res<Vec<T>> {
        let len = self.next_8().map(usize::from_be_bytes)?;
        (0..len).map(|_| f(self)).collect()
    }

    pub fn deserialize(&mut self) -> Res<(Vec<CValue>, Vec<Instruction>)> {
        match self.get_version()? {
            BYTECODE_VERSION => {}
            v => Error::parsing(format!("unexpected version {}", v))?,
        }
        Ok((self.deser_constants()?, self.deser_instructions()?))
    }

    pub fn get_version(&mut self) -> Res<u8> {
        match &self.state {
            State::New => {}
            s => Error::parsing(format!("cant read version after {}", s))?,
        }
        let res = self.next()?;
        self.state.incr();
        Ok(res)
    }

    fn deser_constants(&mut self) -> Res<Vec<CValue>> {
        match &self.state {
            State::VersionRead => {}
            s => Error::parsing(format!("cant get constants after {}", s))?,
        }
        let n = usize::from_be_bytes(self.next_8()?);
        let v = (0..n).map(|_| self.deser_cvalue()).collect::<Res<_>>()?;
        self.state.incr();
        Ok(v)
    }

    fn deser_instructions(&mut self) -> Res<Vec<Instruction>> {
        match &self.state {
            State::ConstantsRead => {}
            s => Error::parsing(format!("cant get instructions after {}", s))?,
        }
        let n = usize::from_be_bytes(self.next_8()?);
        let v = (0..n)
            .map(|_| Instruction::ser(&mut self.bytes))
            .collect::<Res<_>>()?;
        self.state.incr();
        Ok(v)
    }

    pub fn deserialize_symbols(&mut self) -> Res<Option<Vec<String>>> {
        let n = match self.next_8() {
            Ok(n) => usize::from_be_bytes(n),
            Err(_) => return Ok(None),
        };
        let v = (0..n).map(|_| self.deser_str()).collect::<Res<_>>()?;
        Ok(Some(v))
    }

    fn deser_cvalue(&mut self) -> Res<CValue> {
        match self.next()? {
            0 => Ok(CValue::Bool(false)),
            1 => Ok(CValue::Bool(true)),
            2 => self.deser_int(),
            3 => self.deser_str().map(CValue::String),
            4 => self.deser_arr(),
            5 => self.deser_func(),
            b => Error::parsing(format!("invalid type {}, must be in {{1..5}}", b))?,
        }
    }

    fn deser_int(&mut self) -> Res<CValue> {
        self.next_8().map(|b| CValue::Int(isize::from_be_bytes(b)))
    }

    fn deser_str(&mut self) -> Res<String> {
        self.deser_vec(|it| it.next())
            .and_then(|b| Ok(String::from_utf8(b)?))
    }

    fn deser_arr(&mut self) -> Res<CValue> {
        self.deser_vec(|it| it.deser_cvalue()).map(CValue::Array)
    }

    fn deser_func(&mut self) -> Res<CValue> {
        let pars = self.deser_vec(|it| Ok(u16::from_be_bytes([it.next()?, it.next()?])))?;
        let defs = self.deser_vec(|it| {
            let id = u16::from_be_bytes([it.next()?, it.next()?]);
            let val = it.deser_cvalue()?;
            Ok((id, val))
        })?;
        let stmts = self.deser_vec(|it| Instruction::ser(&mut it.bytes))?;
        Ok(CValue::Function(pars, defs, stmts))
    }
}

pub fn ser_to_bytecode(constants: &HashMap<CValue, u16>, instructions: &[Instruction]) -> Vec<u8> {
    let mut buf = vec![BYTECODE_VERSION];
    ser_constants(&mut buf, constants);
    ser_instructions(&mut buf, instructions);
    buf
}

fn ser_constants(buf: &mut Vec<u8>, constants: &HashMap<CValue, u16>) {
    let mut consts: Vec<_> = constants.iter().collect();
    consts.sort_by(|(_, i0), (_, i1)| i0.cmp(i1));
    extend_with_vec(buf, consts.as_slice(), |buf, (v, _)| append_cvalue(buf, v))
}

fn ser_instructions(buf: &mut Vec<u8>, instructions: &[Instruction]) {
    buf.extend_from_slice(&instructions.len().to_be_bytes());
    instructions.iter().rev().for_each(|instr| {
        buf.push(instr.op_code());
        buf.extend_from_slice(instr.operands())
    })
}

pub fn add_symbols(mut bytes: Vec<u8>, symbols: &HashMap<String, u16>) -> Vec<u8> {
    let mut syms: Vec<_> = symbols.iter().collect();
    syms.sort_by(|(_, i0), (_, i1)| i0.cmp(i1));
    extend_with_vec(&mut bytes, syms.as_slice(), |buf, (s, _)| ser_str(buf, s));
    bytes
}

fn ser_str(buf: &mut Vec<u8>, s: &str) {
    let bs = s.as_bytes();
    buf.extend_from_slice(&bs.len().to_be_bytes());
    buf.extend_from_slice(bs)
}

fn append_cvalue(buf: &mut Vec<u8>, v: &CValue) {
    match v {
        CValue::Bool(false) => buf.push(0),
        CValue::Bool(true) => buf.push(1),
        CValue::Int(i) => {
            buf.push(2);
            buf.extend_from_slice(&i.to_be_bytes())
        }
        CValue::String(s) => {
            buf.push(3);
            ser_str(buf, s);
        }
        CValue::Array(arr) => {
            buf.push(4);
            extend_with_vec(buf, arr, append_cvalue)
        }
        CValue::Function(pars, defs, stmts) => {
            buf.push(5);
            extend_with_vec(buf, pars, |buf, par| {
                buf.extend_from_slice(&par.to_be_bytes())
            });
            extend_with_vec(buf, defs, |buf, (id, val)| {
                buf.extend_from_slice(&id.to_be_bytes());
                append_cvalue(buf, val)
            });
            extend_with_vec(buf, stmts, |buf, instr| {
                buf.push(instr.op_code());
                buf.extend_from_slice(instr.operands());
            });
        }
    }
}

fn extend_with_vec<T>(buf: &mut Vec<u8>, v: &[T], f: fn(&mut Vec<u8>, &T)) {
    buf.extend(v.len().to_be_bytes());
    v.iter().for_each(|t| f(buf, t));
}

impl From<Vec<u8>> for Bytecode {
    fn from(v: Vec<u8>) -> Self {
        Self {
            bytes: v.into_iter(),
            state: State::New,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use saf::ast::UnOp;

    #[test]
    fn empty_to_bytecode() {
        let btc = ser_to_bytecode(&HashMap::new(), &Vec::new());
        assert_eq!(btc, [0; 1 + 8 + 8]);
        let btc = add_symbols(btc, &HashMap::new());
        assert_eq!(btc, [0; 1 + 8 + 8 + 8]);
    }

    #[test]
    fn simple_bytecode() {
        let v = CValue::Array(vec![CValue::Int(1), CValue::String("test".to_string())]);
        let d = vec![(7, CValue::Bool(true))];
        let f = CValue::Function(vec![0], d, vec![Instruction::Bool(true)]);
        let consts_arr = [(f, 0), (v, 1)];
        let consts_exp: Vec<_> = consts_arr.iter().cloned().map(|(v, _)| v).collect();

        let mut instrs_exp = vec![Instruction::Call([1]), Instruction::UnOp(UnOp::Minus)];

        let symbols_arr = [(String::from("s1"), 0)];
        let symbols_exp: Vec<_> = symbols_arr.iter().cloned().map(|(v, _)| v).collect();

        let bytes = ser_to_bytecode(&HashMap::from(consts_arr), &instrs_exp);
        // reversed so that first instr is on top of stack for the vm
        instrs_exp.reverse();
        let mut btc = Bytecode::from(bytes.clone());
        let (consts, instrs) = btc.deserialize().unwrap();
        let syms = btc.deserialize_symbols().unwrap();
        assert_eq!(consts_exp, consts);
        assert_eq!(instrs_exp, instrs);
        assert!(syms.is_none());

        let bytes_with_symbols = add_symbols(bytes, &HashMap::from(symbols_arr));
        let mut btc = Bytecode::from(bytes_with_symbols);
        let (consts, instrs) = btc.deserialize().unwrap();
        let symbols = btc.deserialize_symbols().unwrap().unwrap();
        assert_eq!(consts_exp, consts);
        assert_eq!(instrs_exp, instrs);

        assert_eq!(symbols_exp, symbols);
    }
}
