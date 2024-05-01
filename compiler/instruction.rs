use saf::ast::{BinOp, UnOp};
use saf::err::{Error, Res};
use std::{fmt, vec};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Instruction {
    Constant([u8; 2]),
    GetVar([u8; 2]),
    SetVar([u8; 2]),
    Call([u8; 1]),
    ExitCall([u8; 1]),
    Jump([u8; 2]),
    JumpIfNot([u8; 2]),
    Bool(bool),
    UnOp(UnOp), // paren is a no-op
    BinOp(BinOp),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Constant(operands) => {
                write!(f, "Constant {}", u16::from_be_bytes(*operands))
            }
            Self::GetVar(operands) => {
                write!(f, "GetVar {}", u16::from_be_bytes(*operands))
            }
            Self::SetVar(operands) => {
                write!(f, "SetVar {}", u16::from_be_bytes(*operands))
            }
            Self::Call(n) => write!(f, "Call {}", u8::from_be_bytes(*n)),
            Self::ExitCall(n) => write!(f, "ExitCall {}", u8::from_be_bytes(*n)),
            Self::Jump(n) => write!(f, "Jump {}", u16::from_be_bytes(*n)),
            Self::JumpIfNot(n) => write!(f, "JumpIfNot {}", u16::from_be_bytes(*n)),
            Self::Bool(b) => write!(f, "Bool {}", b),
            Self::UnOp(UnOp::Paren) => write!(f, "Paren"),
            Self::UnOp(UnOp::Minus) => write!(f, "Minus"),
            Self::UnOp(UnOp::Negation) => write!(f, "Negation"),
            Self::UnOp(UnOp::Init) => write!(f, "Init"),
            Self::UnOp(UnOp::Pop) => write!(f, "Pop"),
            Self::UnOp(UnOp::Len) => write!(f, "Len"),
            Self::UnOp(UnOp::TypeOf) => write!(f, "TypeOf"),
            Self::BinOp(BinOp::Or) => write!(f, "Or"),
            Self::BinOp(BinOp::And) => write!(f, "And"),
            Self::BinOp(BinOp::Lt) => write!(f, "Lt"),
            Self::BinOp(BinOp::Gt) => write!(f, "Gt"),
            Self::BinOp(BinOp::NotEq) => write!(f, "NotEq"),
            Self::BinOp(BinOp::Eq) => write!(f, "Eq"),
            Self::BinOp(BinOp::Minus) => write!(f, "Minus"),
            Self::BinOp(BinOp::Plus) => write!(f, "Plus"),
            Self::BinOp(BinOp::Div) => write!(f, "Div"),
            Self::BinOp(BinOp::Mult) => write!(f, "Mult"),
        }
    }
}

/*
pub struct Bytecode(vec::IntoIter<u8>);

impl From<Vec<u8>> for Bytecode {
    fn from(v: Vec<u8>) -> Self {
        Self(v.into_iter())
    }
}

impl Iterator for Bytecode {
    type Item = Res<Instruction>;

    fn next(&mut self) -> Option<Self::Item> {
        Instruction::ser_opt(&mut self.0)
    }
}
*/

fn get_u8(it: &mut vec::IntoIter<u8>, op: &str) -> Res<u8> {
    match it.next() {
        Some(it) => Ok(it),
        None => Error::run_custom(format!("expected operand for {}", op)),
    }
}

fn get_u16(it: &mut vec::IntoIter<u8>, op: &str) -> Res<[u8; 2]> {
    Ok([get_u8(it, op)?, get_u8(it, op)?])
}

impl Instruction {
    pub fn ser_opt(it: &mut vec::IntoIter<u8>) -> Option<Res<Self>> {
        let b = match it.next() {
            Some(b) => b,
            None => return None,
        };
        let instr = match b {
            0 => Ok(Self::Bool(false)),
            1 => Ok(Self::Bool(true)),
            2 => get_u16(it, "Constant").map(Self::Constant),
            3 => get_u16(it, "GetVar").map(Self::GetVar),
            4 => get_u16(it, "SetVar").map(Self::SetVar),
            5 => get_u16(it, "Jump").map(Self::Jump),
            6 => get_u16(it, "JumpIfNot").map(Self::JumpIfNot),
            7 => get_u8(it, "Call").map(|i| Self::Call([i])),
            8 => get_u8(it, "ExitCall").map(|i| Self::ExitCall([i])),
            9 => Ok(Self::UnOp(UnOp::Paren)),
            10 => Ok(Self::UnOp(UnOp::Minus)),
            11 => Ok(Self::UnOp(UnOp::Negation)),
            12 => Ok(Self::UnOp(UnOp::Init)),
            13 => Ok(Self::UnOp(UnOp::Pop)),
            14 => Ok(Self::UnOp(UnOp::Len)),
            15 => Ok(Self::UnOp(UnOp::TypeOf)),
            16 => Ok(Self::BinOp(BinOp::Or)),
            17 => Ok(Self::BinOp(BinOp::And)),
            18 => Ok(Self::BinOp(BinOp::Lt)),
            19 => Ok(Self::BinOp(BinOp::Gt)),
            20 => Ok(Self::BinOp(BinOp::NotEq)),
            21 => Ok(Self::BinOp(BinOp::Eq)),
            22 => Ok(Self::BinOp(BinOp::Minus)),
            23 => Ok(Self::BinOp(BinOp::Plus)),
            24 => Ok(Self::BinOp(BinOp::Div)),
            25 => Ok(Self::BinOp(BinOp::Mult)),
            b => Error::run_custom(format!("unknown opcode {}", b)),
        };
        Some(instr)
    }

    pub fn ser(it: &mut vec::IntoIter<u8>) -> Res<Self> {
        match Self::ser_opt(it) {
            Some(v) => v,
            None => Error::run_custom("unexpected end of input"),
        }
    }

    pub fn op_code(&self) -> u8 {
        match self {
            Self::Bool(false) => 0,
            Self::Bool(true) => 1,
            Self::Constant(_) => 2,
            Self::GetVar(_) => 3,
            Self::SetVar(_) => 4,
            Self::Jump(_) => 5,
            Self::JumpIfNot(_) => 6,
            Self::Call(_) => 7,
            Self::ExitCall(_) => 8,
            Self::UnOp(UnOp::Paren) => 9,
            Self::UnOp(UnOp::Minus) => 10,
            Self::UnOp(UnOp::Negation) => 11,
            Self::UnOp(UnOp::Init) => 12,
            Self::UnOp(UnOp::Pop) => 13,
            Self::UnOp(UnOp::Len) => 14,
            Self::UnOp(UnOp::TypeOf) => 15,
            Self::BinOp(BinOp::Or) => 16,
            Self::BinOp(BinOp::And) => 17,
            Self::BinOp(BinOp::Lt) => 18,
            Self::BinOp(BinOp::Gt) => 19,
            Self::BinOp(BinOp::NotEq) => 20,
            Self::BinOp(BinOp::Eq) => 21,
            Self::BinOp(BinOp::Minus) => 22,
            Self::BinOp(BinOp::Plus) => 23,
            Self::BinOp(BinOp::Div) => 24,
            Self::BinOp(BinOp::Mult) => 25,
        }
    }

    pub fn operands(&self) -> &[u8] {
        match self {
            Instruction::Constant(o)
            | Instruction::SetVar(o)
            | Instruction::GetVar(o)
            | Instruction::Jump(o)
            | Instruction::JumpIfNot(o) => o.as_ref(),
            Instruction::Call(n) | Instruction::ExitCall(n) => n.as_ref(),
            _ => &[],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add() {
        let n = 25;
        assert_eq!(Instruction::BinOp(BinOp::Mult).op_code(), n);
        for i in 0..=n {
            // 2 is enough operands to parse
            let data = &mut vec![i; 1 + 2].into_iter();
            let instr = Instruction::ser_opt(data).unwrap().unwrap();
            assert_eq!(instr.op_code(), i);
        }
    }
}
