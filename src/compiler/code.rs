use crate::utils;

#[derive(Debug, Clone, Copy)]
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
}

impl Op {
    pub const fn lookup(&self) -> &[u8] {
        match self {
            Self::Constant => &[2],
            Self::Add
            | Self::Pop
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::True
            | Self::False
            | Self::Equal
            | Self::NotEqual
            | Self::GreaterThan => &[],
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
