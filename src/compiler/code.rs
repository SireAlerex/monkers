use crate::utils;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Op {
    Constant,
    Add,
    Pop,
    Sub,
    Mul,
    Div,
    True,
    False,
    Equal,
    NotEqual,
    GreaterThan,
    Minus,
    Bang,
    Jump,
    JumpNotTruthy,
    Null,
    GetGlobal,
    SetGlobal,
    Array,
    Hash,
    Index,
    Call,
    ReturnValue,
    Return,
    GetLocal,
    SetLocal,
    GetBuiltin,
}

impl Op {
    pub const fn lookup(&self) -> &[u8] {
        match self {
            Self::Constant
            | Self::Jump
            | Self::JumpNotTruthy
            | Self::GetGlobal
            | Self::SetGlobal
            | Self::Array
            | Self::Hash => &[2],
            Self::GetLocal | Self::SetLocal | Self::Call | Self::GetBuiltin => &[1],
            Self::Add
            | Self::Pop
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::True
            | Self::False
            | Self::Equal
            | Self::NotEqual
            | Self::GreaterThan
            | Self::Minus
            | Self::Bang
            | Self::Null
            | Self::Index
            | Self::ReturnValue
            | Self::Return => &[],
        }
    }

    pub const fn from_u8(value: u8) -> Self {
        match value {
            0 => Self::Constant,
            1 => Self::Add,
            2 => Self::Pop,
            3 => Self::Sub,
            4 => Self::Mul,
            5 => Self::Div,
            6 => Self::True,
            7 => Self::False,
            8 => Self::Equal,
            9 => Self::NotEqual,
            10 => Self::GreaterThan,
            11 => Self::Minus,
            12 => Self::Bang,
            13 => Self::Jump,
            14 => Self::JumpNotTruthy,
            15 => Self::Null,
            16 => Self::GetGlobal,
            17 => Self::SetGlobal,
            18 => Self::Array,
            19 => Self::Hash,
            20 => Self::Index,
            21 => Self::Call,
            22 => Self::ReturnValue,
            23 => Self::Return,
            24 => Self::GetLocal,
            25 => Self::SetLocal,
            26 => Self::GetBuiltin,
            _ => panic!(),
        }
    }

    pub fn make(self, operands: &[u64]) -> Vec<u8> {
        let def = self.lookup();
        let mut instruction_len = 1;
        for w in def {
            instruction_len += w;
        }

        let mut instruction = vec![0; instruction_len.into()];
        instruction[0] = self as u8;

        let mut offset = 1;
        for (i, operand) in operands.iter().enumerate() {
            let width = def[i];
            match width {
                1 => instruction[offset] = *operand as u8,
                2 => utils::write_u16(&mut instruction[offset..=offset + 1], *operand),
                _ => {}
            }
            offset += width as usize;
        }

        instruction
    }

    pub fn read_operands(self, ins: &[u8]) -> (Vec<u64>, usize) {
        let mut operands = vec![0; self.lookup().len()];
        let mut offset = 0;

        for (i, width) in self.lookup().iter().enumerate() {
            match width {
                1 => operands[i] = ins[offset] as u64,
                2 => operands[i] = utils::read_u16(&ins[offset..=offset + 1]) as u64,
                _ => panic!("can't read operand from width={width}"),
            }
            offset += *width as usize;
        }

        (operands, offset)
    }

    pub fn fmt_instruction(self, operands: &[u64]) -> String {
        match operands.len() {
            0 => format!("Op{self:?}"),
            1 => format!("Op{self:?} {}", operands[0]),
            _ => format!("ERROR: unhandled operandCount for {self:?}"),
        }
    }
}
