use crate::interpreter::{
    ast::{Expr, Literal, Operator, Program, Stmt},
    evaluator::object::Object,
};

use chunk::Instructions;
use code::Op;

pub mod chunk;
pub mod code;
// pub mod compiler;
pub mod vm;

macro_rules! err {
    ($($arg:tt)*) => {
        Err(format!("Compilation error: {}", format!($($arg)*)))
    };
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile(&mut self, program: Program) -> Result<(), String> {
        for stmt in program.0 {
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }

    fn compile_stmt(&mut self, stmt: Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Expr(expr) => {
                let res = self.compile_expr(expr);
                let _ = self.emit(Op::Pop, &[]);
                res
            }
            _ => err!("unimplemented stmt: {stmt}"),
        }
    }

    fn compile_expr(&mut self, expr: Expr) -> Result<(), String> {
        match expr {
            Expr::Prefix(operator, right) => {
                self.compile_expr(*right)?;
                let _ = match operator {
                    Operator::Bang => self.emit(Op::Bang, &[]),
                    Operator::Minus => self.emit(Op::Minus, &[]),
                    _ => return err!("unimplemented infix operator: {operator}"),
                };

                Ok(())
            }
            Expr::Infix(left, operator, right) => {
                if let Operator::Less = operator {
                    self.compile_expr(*right)?;
                    self.compile_expr(*left)?;
                    let _ = self.emit(Op::GreaterThan, &[]);
                    return Ok(());
                }

                self.compile_expr(*left)?;
                self.compile_expr(*right)?;
                let _ = match operator {
                    Operator::Plus => self.emit(Op::Add, &[]),
                    Operator::Minus => self.emit(Op::Sub, &[]),
                    Operator::Asterisk => self.emit(Op::Mul, &[]),
                    Operator::Slash => self.emit(Op::Div, &[]),
                    Operator::Equal => self.emit(Op::Equal, &[]),
                    Operator::NotEqual => self.emit(Op::NotEqual, &[]),
                    Operator::Greater => self.emit(Op::GreaterThan, &[]),
                    _ => return err!("unimplemented infix operator: {operator}"),
                };
                Ok(())
            }
            Expr::Literal(lit) => self.compile_literal(&lit),
            _ => err!("unimplemented expr: {expr}"),
        }
    }

    fn compile_literal(&mut self, lit: &Literal) -> Result<(), String> {
        match lit {
            Literal::Int(x) => {
                let integer = Object::Integer(*x);
                let index = self.add_constant(integer);
                let _ = self.emit(Op::Constant, &[index as u64]);
                Ok(())
            }
            Literal::Boolean(bool) => {
                let _ = if *bool {
                    self.emit(Op::True, &[])
                } else {
                    self.emit(Op::False, &[])
                };

                Ok(())
            }
            _ => err!("unimplemented literal: {lit}"),
        }
    }

    fn emit(&mut self, op: Op, operands: &[u64]) -> usize {
        self.add_instruction(&mut op.make(operands))
    }

    fn add_instruction(&mut self, ins: &mut Vec<u8>) -> usize {
        let pos_new_ins = self.instructions.0.len();
        self.instructions.0.append(ins);
        pos_new_ins
    }

    fn add_constant(&mut self, value: Object) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn byte_code(&self) -> ByteCode {
        // TODO keep clone ?
        ByteCode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

pub struct ByteCode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[cfg(test)]
mod test {
    use std::error::Error;

    use crate::{
        compiler::Compiler,
        interpreter::{evaluator::object::Object, parser::Parser},
    };

    use super::{chunk::Instructions, code::Op};

    fn flatten(vec: &mut Vec<Instructions>) -> Instructions {
        let mut res = Vec::new();
        for instr in vec {
            res.append(&mut instr.0);
        }

        Instructions::vec(res)
    }

    fn compile_test(
        tests: &[(&str, Vec<Object>, Vec<Instructions>)],
    ) -> Result<(), Box<dyn Error>> {
        for test in tests {
            let mut compiler = Compiler::new();
            compiler.compile(Parser::parse(test.0))?;
            let byte_code = compiler.byte_code();

            if byte_code.instructions.to_string() != flatten(&mut test.2.clone()).to_string() {
                println!(
                    "Testing input: '{}'\nGot bytecode:\n{}\n\nWanted bytecode:\n{}",
                    test.0,
                    byte_code.instructions,
                    flatten(&mut test.2.clone())
                );
            }

            assert_eq!(byte_code.constants, test.1);
            assert_eq!(
                byte_code.instructions.to_string(),
                flatten(&mut test.2.clone()).to_string()
            );
        }

        Ok(())
    }

    #[test]
    fn boolean_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "true",
                vec![],
                vec![
                    Instructions::vec(Op::True.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "false",
                vec![],
                vec![
                    Instructions::vec(Op::False.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "1 > 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::GreaterThan.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "1 < 2",
                vec![Object::Integer(2), Object::Integer(1)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::GreaterThan.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "1 == 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Equal.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "1 != 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::NotEqual.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "true == false",
                vec![],
                vec![
                    Instructions::vec(Op::True.make(&[])),
                    Instructions::vec(Op::False.make(&[])),
                    Instructions::vec(Op::Equal.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "true != false",
                vec![],
                vec![
                    Instructions::vec(Op::True.make(&[])),
                    Instructions::vec(Op::False.make(&[])),
                    Instructions::vec(Op::NotEqual.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "!true",
                vec![],
                vec![
                    Instructions::vec(Op::True.make(&[])),
                    Instructions::vec(Op::Bang.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
        ])
    }

    #[test]
    fn integer_test() -> Result<(), Box<dyn Error>> {
        // TODO add function to take ints and return objects (and other type)
        compile_test(&[
            (
                "1 + 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Add.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "1; 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Pop.make(&[])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "1 - 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Sub.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "1 * 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Mul.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "2 / 1",
                vec![Object::Integer(2), Object::Integer(1)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Div.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "-1",
                vec![Object::Integer(1)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Minus.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
        ])
    }

    #[test]
    fn read_operands_test() {
        let tests = &[(Op::Constant, &[65535], 2)];

        for test in tests {
            let ins = test.0.make(test.1);
            let (operands_read, n) = test.0.read_operands(&ins[1..]);

            assert_eq!(n, test.2);
            for (i, want) in operands_read.iter().enumerate() {
                assert_eq!(*want, test.1[i]);
            }
        }
    }

    #[test]
    fn instruction_string() {
        let tests = &[(
            vec![
                Instructions::vec(Op::Add.make(&[])),
                Instructions::vec(Op::Constant.make(&[2])),
                Instructions::vec(Op::Constant.make(&[65535])),
            ],
            "0000 OpAdd\n0001 OpConstant 2\n0004 OpConstant 65535",
        )];

        for test in tests {
            assert_eq!(flatten(&mut test.0.clone()).to_string(), test.1);
        }
    }

    #[test]
    fn make_test() {
        let tests = &[
            (
                Op::Constant,
                vec![255],
                vec![Op::Constant as u8, 0x00, 0xFF],
            ),
            (
                Op::Constant,
                vec![65534],
                vec![Op::Constant as u8, 0xFF, 0xFE],
            ),
            (Op::Add, vec![], vec![Op::Add as u8]),
        ];

        for test in tests {
            let instruction = test.0.make(&test.1);
            assert_eq!(instruction, test.2);
        }
    }
}
