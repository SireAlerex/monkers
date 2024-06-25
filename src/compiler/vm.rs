use crate::{compiler::code::Op, interpreter::evaluator::object::Object, utils};

use super::{chunk::Instructions, ByteCode};

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
        self.stack[self.sp].clone()
    }

    #[allow(unreachable_patterns)]
    pub fn run(&mut self) -> Result<(), String> {
        println!("vm run ins:{:?}", self.instructions.to_string());
        let mut ip = 0;
        while ip < self.instructions.0.len() {
            // println!("loop ip={ip}");
            let op: Op = Op::from_u8(self.instructions[ip]);

            match op {
                Op::Constant => {
                    let idx = utils::read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;
                    self.push(self.constants[idx as usize].clone())?;
                }
                Op::Add | Op::Sub | Op::Mul | Op::Div => {
                    let right = self.pop();
                    let left = self.pop();
                    match match op {
                        Op::Add => left + right,
                        Op::Sub => left - right,
                        Op::Mul => left * right,
                        Op::Div => left / right,
                        _ => unreachable!(),
                    } {
                        Object::Error(err) => return Err(err),
                        obj => self.push(obj)?,
                    }
                }
                Op::Pop => {
                    _ = self.pop();
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

    // used in tests
    #[allow(dead_code)]
    pub fn show_stack(&self) -> String {
        format!(
            "{:?}",
            self.stack
                .iter()
                .filter(|obj| **obj != Object::Uninit)
                .collect::<Vec<&Object>>()
        )
    }
}
