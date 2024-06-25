use crate::interpreter::{
    ast::{Expr, Literal, Operator, Program, Stmt},
    evaluator::object::Object,
};

use super::{chunk::Instructions, code::Op};

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
            Stmt::Expr(expr) => self.compile_expr(expr),
            _ => err!("unimplemented stmt: {stmt}")
        }
    }

    fn compile_expr(&mut self, expr: Expr) -> Result<(), String> {
        match expr {
            Expr::Infix(left, operator, right) => {
                self.compile_expr(*left)?;
                self.compile_expr(*right)?;

                let _ = match operator {
                    Operator::Plus => self.emit(Op::Add, &[]),
                    _ => return err!("unimplemented infix operator: {operator}")
                };
                Ok(())
            }
            Expr::Literal(lit) => self.compile_literal(lit),
            _ => err!("unimplemented expr: {expr}")
        }
    }

    fn compile_literal(&mut self, lit: Literal) -> Result<(), String> {
        match lit {
            Literal::Int(x) => {
                let integer = Object::Integer(x);
                let index = self.add_constant(integer);
                let _ = self.emit(Op::Constant, &[index as u64]);
                Ok(())
            }
            _ => err!("unimplemented literal: {lit}")
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
