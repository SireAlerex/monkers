use std::ops::{Add, Div, Mul, Sub};

use crate::{compiler::code::Op, interpreter::evaluator::object::Object, utils};

use super::{chunk::Instructions, ByteCode};

macro_rules! null {
    ($self: ident) => {
        $self.push(Object::Null)
    };
}

macro_rules! err {
    ($($arg:tt)*) => {
        Err(format!("VM error: {}", format!($($arg)*)))
    };
}

macro_rules! check_binary {
    ($self: ident, $func: ident) => {{
        let right = $self.pop();
        let left = $self.pop();
        match left.$func(right) {
            Object::Error(err) => return Err(err),
            obj => $self.push(obj)?,
        }
    }};
}

const STACK_SIZE: usize = 2048;
pub const UNINT_OBJECT: Object = Object::Uninit;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;
pub const GLOBAL_SIZE: usize = 65536;

// to avoid allocation, stack of &Object, pointing to alloc handler
pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Box<[Object; STACK_SIZE]>,
    sp: usize,
    // heap allocated
    globals: Box<[Object]>,
}

impl VM {
    #[allow(dead_code)]
    pub fn new(byte_code: ByteCode) -> Self {
        Self {
            constants: byte_code.constants,
            instructions: byte_code.instructions,
            stack: Box::new([UNINT_OBJECT; STACK_SIZE]),
            sp: 0,
            globals: vec![UNINT_OBJECT; GLOBAL_SIZE].into_boxed_slice(),
        }
    }

    pub fn new_with_globals_store(byte_code: ByteCode, globals: Box<[Object]>) -> Self {
        Self {
            constants: byte_code.constants,
            instructions: byte_code.instructions,
            stack: Box::new([UNINT_OBJECT; STACK_SIZE]),
            sp: 0,
            globals,
        }
    }

    pub fn stack_top(&self) -> Object {
        self.stack[self.sp].clone()
    }

    #[allow(unreachable_patterns)]
    pub fn run(&mut self) -> Result<(), String> {
        // println!("vm run ins:{:?}", self.instructions.to_string());
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
                Op::Add => check_binary!(self, add),
                Op::Sub => check_binary!(self, sub),
                Op::Mul => check_binary!(self, mul),
                Op::Div => check_binary!(self, div),
                Op::Pop => {
                    _ = self.pop();
                }
                Op::True => self.push(TRUE)?,
                Op::False => self.push(FALSE)?,
                Op::Equal => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(Object::Boolean(left == right))?;
                }
                Op::NotEqual => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(Object::Boolean(left != right))?;
                }
                Op::GreaterThan => check_binary!(self, greater),
                Op::Bang => match self.pop() {
                    FALSE | NULL => self.push(TRUE)?,
                    _ => self.push(FALSE)?,
                },
                Op::Minus => match self.pop() {
                    Object::Integer(a) => self.push(Object::Integer(-a))?,
                    obj => return err!("unsupported type for negation: {}", obj.get_type()),
                },
                Op::Jump => {
                    let pos = utils::read_u16(&self.instructions.0[ip + 1..]);
                    ip = pos as usize - 1;
                }
                Op::JumpNotTruthy => {
                    let pos = utils::read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;

                    let cond = self.pop();
                    if !cond.is_truthy() {
                        ip = pos as usize - 1;
                    }
                }
                Op::Null => null!(self)?,
                Op::SetGlobal => {
                    let global_idx = utils::read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;

                    self.globals[global_idx as usize] = self.pop();
                }
                Op::GetGlobal => {
                    let global_idx = utils::read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;

                    self.push(self.globals[global_idx as usize].clone())?;
                }
                op => err!("'{op:?}' is unimplemented for vm")?,
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

    pub fn globals(self) -> Box<[Object]> {
        self.globals
    }
}

#[cfg(test)]
mod test {
    use std::error::Error;

    use crate::{
        compiler::{vm::VM, Compiler},
        interpreter::{evaluator::object::Object, parser::Parser},
    };

    use super::NULL;

    fn vm_test<T>(tests: &[(&str, T)]) -> Result<(), Box<dyn Error>>
    where
        T: Into<Object> + Sized + Clone,
    {
        for test in tests {
            let mut compiler = Compiler::new();
            compiler.compile(Parser::parse(test.0))?;

            let mut vm = VM::new(compiler.byte_code());
            vm.run()?;
            let stack_elem = vm.stack_top();

            if stack_elem != test.1.clone().into() {
                println!(
                    "Testing vm for input: '{}'\nBytecode:\n{}",
                    test.0,
                    compiler.byte_code().instructions
                );
                println!("vm stack: {}", vm.show_stack());
            }

            assert_eq!(stack_elem, test.1.clone().into());
        }

        Ok(())
    }

    #[test]
    fn string_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("\"monkey\"", "monkey"),
            ("\"mon\" + \"key\"", "monkey"),
            ("\"mon\" + \"key\" + \"banana\"", "monkeybanana"),
        ])
    }

    #[test]
    fn global_symbol_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("let one = 1; one", 1),
            ("let one = 1; let two = 2; one + two", 3),
            ("let one = 1; let two = one + one; one + two", 3),
        ])
    }

    #[test]
    fn conditional_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("if (true) { 10 }", 10),
            ("if (true) { 10 } else { 20 }", 10),
            ("if (false) { 10 } else { 20 } ", 20),
            ("if (1) { 10 }", 10),
            ("if (1 < 2) { 10 }", 10),
            ("if (1 < 2) { 10 } else { 20 }", 10),
            ("if (1 > 2) { 10 } else { 20 }", 20),
            ("if ((if (false) { 10 })) { 10 } else { 20 }", 20),
        ])?;

        vm_test(&[("if (false) { 10 }", NULL), ("if (1 > 2) { 10 }", NULL)])
    }

    #[test]
    fn boolean_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
            ("!(if (false) { 5; })", true),
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
            ("-5", -5),
            ("-10", -10),
            ("-50 + 100 + -50", 0),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ])
    }
}
