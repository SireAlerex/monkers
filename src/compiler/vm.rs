use crate::{compiler::code::Op, interpreter::evaluator::object::Object, utils};

use super::{chunk::Instructions, ByteCode};

macro_rules! null {
    ($self: ident) => {
        $self.push(Object::Null)
    };
}

const STACK_SIZE: usize = 2048;
const UNINT_OBJECT: Object = Object::Uninit;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

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
                Op::True => self.push(TRUE)?,
                Op::False => self.push(FALSE)?,
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

#[cfg(test)]
mod test {
    use std::error::Error;

    use crate::{compiler::{vm::VM, Compiler}, interpreter::{evaluator::object::Object, parser::Parser}};

    fn vm_test<T>(tests: &[(&str, T)]) -> Result<(), Box<dyn Error>>
    where
    T: Into<Object> + Sized + Clone
     {
        for test in tests {
            let mut compiler = Compiler::new();
            compiler.compile(Parser::parse(test.0))?;
            println!("testing vm for:\n{}", compiler.byte_code().instructions);
            let mut vm = VM::new(compiler.byte_code());
            vm.run()?;

            let stack_elem = vm.stack_top();
            println!("vm stack: {}", vm.show_stack());
            assert_eq!(stack_elem, test.1.clone().into());
        }

        Ok(())
    }

    #[test]
    fn boolean_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("true", true),
            ("false", false),
        ])
    }

    #[test]
    fn integer_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("1", 1),
            ("2", 2),
            ("1 + 2", 3),
            ("1 - 2", -1),
            ("1 * 2", 2),
            ("4 / 2", 2),
            ("50 / 2 * 2 + 10 - 5", 55),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("5 * (2 + 10)", 60),
        ])
    }
}
