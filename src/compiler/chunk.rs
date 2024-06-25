use std::{fmt::Display, ops::Index};

use crate::compiler::code::Op;

#[derive(Debug, Clone, PartialEq)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Instructions(Vec::new())
    }

    pub fn vec(vec: Vec<u8>) -> Self {
        Self(vec)
    }
}

impl Index<usize> for Instructions {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        let mut i = 0;
        while i < self.0.len() {
            let byte = self.0[i];
            let op: Op = Op::from_u8(byte);

            let (operands, read) = op.read_operands(&self.0[i + 1..]);
            s = format!("{s}{i:#04} {}", op.fmt_instruction(&operands));
            i += 1 + read;

            if i < self.0.len() {
                s.push('\n');
            }
        }
        write!(f, "{s}")
    }
}
