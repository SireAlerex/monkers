use crate::interpreter::{
    ast::{Expr, Literal, Operator, Program, Stmt},
    evaluator::object::Object,
};

use super::{chunk::Instructions, code::Op};

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

    // TODO: return error
    pub fn compile(&mut self, program: Program) {
        for stmt in program.0 {
            self.compile_stmt(stmt);
        }
    }

    fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Return(_) => todo!(),
            Stmt::Let(_, _) => todo!(),
            Stmt::Expr(expr) => self.compile_expr(expr),
        }
    }

    fn compile_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Infix(left, operator, right) => {
                self.compile_expr(*left);
                self.compile_expr(*right);

                let _ = match operator {
                    Operator::Plus => self.emit(Op::Add, &[]),
                    _ => panic!("unimplemented infix operator"),
                };
            }
            Expr::Literal(lit) => self.compile_literal(lit),
            _ => todo!(),
        }
    }

    fn compile_literal(&mut self, lit: Literal) {
        match lit {
            Literal::Int(x) => {
                let integer = Object::Integer(x);
                let index = self.add_constant(integer);
                let _ = self.emit(Op::Constant, &[index as u64]);
            }
            _ => todo!(),
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
