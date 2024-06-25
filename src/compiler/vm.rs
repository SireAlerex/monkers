use crate::{compiler::code::Op, interpreter::evaluator::object::Object, utils};

use super::{chunk::Instructions, compiler::ByteCode};

macro_rules! null {
    ($self: ident) => {
        $self.push(Object::Null)
    };
}

const STACK_SIZE: usize = 2048;
const UNINT_OBJECT: Object = Object::Uninit;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Box<[Object; STACK_SIZE]>,
    sp: usize,
}

impl VM {
    pub fn new(byte_code: ByteCode) -> Self {
        Self {
            constants: byte_code.constants,
            instructions: byte_code.instructions,
            stack: Box::new([UNINT_OBJECT; STACK_SIZE]),
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> Object {
        self.stack[self.sp - 1].clone()
    }

    #[allow(unreachable_patterns)]
    pub fn run(&mut self) -> Result<(), String> {
        println!("vm run ins:{:?}", self.instructions);
        let mut ip = 0;
        while ip < self.instructions.0.len() {
            println!("loop ip={ip}");
            let op: Op = Op::from_u8(self.instructions[ip as usize]);

            match op {
                Op::Constant => {
                    let idx = utils::read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;
                    self.push(self.constants[idx as usize].clone())?;
                }
                Op::Add => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(left + right)?;
                }
                _ => null!(self)?,
            }

            ip += 1;
        }

        Ok(())
    }

    fn pop(&mut self) -> Object {
        let obj = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        obj
    }

    fn push(&mut self, obj: Object) -> Result<(), String> {
        if self.sp >= STACK_SIZE {
            Err("stack overflow".to_owned())
        } else {
            self.stack[self.sp] = obj;
            self.sp += 1;

            Ok(())
        }
    }
}
