use std::{
    borrow::Borrow,
    cell::{RefCell, RefMut},
    rc::Rc,
};

use crate::interpreter::{
    ast::{Expr, Literal, Operator, Program, Stmt},
    evaluator::object::Object,
};

use chunk::Instructions;
use code::Op;
use symbol_table::SymbolTable;

pub mod chunk;
pub mod code;
pub mod symbol_table;
pub mod vm;

macro_rules! err {
    ($($arg:tt)*) => {
        Err(format!("Compilation error: {}", format!($($arg)*)))
    };
}

#[derive(Debug, Clone)]
pub struct Compiler {
    constants: Vec<Object>,
    symbol_table: Rc<RefCell<SymbolTable>>,
    scopes: Vec<Scope>,
    scope_index: usize,
}

#[derive(Debug, Clone)]
pub struct Scope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Scope {
    pub const fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            last_instruction: None,
            previous_instruction: None,
        }
    }
}

impl Compiler {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            symbol_table: Rc::new(RefCell::new(SymbolTable::new_with_builtins())),
            scopes: vec![Scope::new()],
            scope_index: 0,
        }
    }

    pub fn new_with_state(symbol_table: &Rc<RefCell<SymbolTable>>, constants: Vec<Object>) -> Self {
        Self {
            constants,
            symbol_table: Rc::clone(symbol_table),
            scopes: vec![Scope::new()],
            scope_index: 0,
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
                self.compile_expr(expr)?;
                let _ = self.emit(Op::Pop, &[]);
            }
            Stmt::Let(name, expr) => {
                self.compile_expr(expr)?;
                let symbol = self.symbol_table.borrow_mut().define(name);
                let _ = match symbol.scope {
                    symbol_table::Scope::Global => {
                        self.emit(Op::SetGlobal, &[symbol.index() as u64])
                    }
                    symbol_table::Scope::Local => self.emit(Op::SetLocal, &[symbol.index() as u64]),
                    symbol_table::Scope::Builtin => {
                        todo!()
                    }
                };
            }
            Stmt::Return(expr) => {
                self.compile_expr(expr)?;
                let _ = self.emit(Op::ReturnValue, &[]);
            }
        }
        Ok(())
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
            }
            Expr::Infix(left, operator, right) => {
                if operator == Operator::Less {
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
            }
            Expr::Literal(lit) => self.compile_literal(&lit),
            Expr::If {
                cond,
                consequence,
                alternative,
            } => {
                self.compile_expr(*cond)?;

                // emit JumpNotThruthy with a temp value
                let jump_not_truthy = self.emit(Op::JumpNotTruthy, &[9999]);

                self.compile(consequence)?;
                if self.last_instruction_is(Op::Pop) {
                    self.remove_last_pop();
                }

                // emit Jump with a temp value
                let jump_pos = self.emit(Op::Jump, &[9999]);

                let after_consequence_pos = self.current_instruction().0.len();
                self.change_operand(jump_not_truthy, after_consequence_pos as u64);

                if let Some(block) = alternative {
                    self.compile(block)?;
                    if self.last_instruction_is(Op::Pop) {
                        self.remove_last_pop();
                    }
                } else {
                    self.emit(Op::Null, &[]);
                }

                let after_alternative_pos = self.current_instruction().0.len();
                self.change_operand(jump_pos, after_alternative_pos as u64);
            }
            Expr::Ident(name) => {
                let sym = self.symbol_table.borrow_mut().resolve(&name);
                if let Some(symbol) = sym {
                    let _ = match symbol.scope {
                        symbol_table::Scope::Global => {
                            self.emit(Op::GetGlobal, &[symbol.index() as u64])
                        }
                        symbol_table::Scope::Local => {
                            self.emit(Op::GetLocal, &[symbol.index() as u64])
                        }
                        symbol_table::Scope::Builtin => {
                            self.emit(Op::GetBuiltin, &[symbol.index() as u64])
                        }
                    };
                } else {
                    return err!("undefined variable: {name}");
                }
            }
            Expr::Array(vec) => {
                let size = vec.len();
                for expr in vec {
                    self.compile_expr(expr)?;
                }
                let _ = self.emit(Op::Array, &[size as u64]);
            }
            Expr::HashLiteral(hash) => {
                let size = hash.len() * 2;
                for (key, value) in hash {
                    self.compile_expr(key)?;
                    self.compile_expr(value)?;
                }
                let _ = self.emit(Op::Hash, &[size as u64]);
            }
            Expr::Index(left, index) => {
                self.compile_expr(*left)?;
                self.compile_expr(*index)?;
                let _ = self.emit(Op::Index, &[]);
            }
            Expr::FunctionLiteral { parameters, body } => {
                let param_count = parameters.len();
                self.enter_scope();
                {
                    // droping ref mut on self after scope
                    let mut table: RefMut<'_, SymbolTable> = self.symbol_table.borrow_mut();
                    for param in parameters {
                        table.define(param);
                    }
                }
                self.compile(body)?;

                if self.last_instruction_is(Op::Pop) {
                    self.replace_last_pop_with_return();
                }
                if !self.last_instruction_is(Op::ReturnValue) {
                    let _ = self.emit(Op::Return, &[]);
                }

                let table: &RefCell<SymbolTable> = self.symbol_table.borrow();
                let locals_count = table.borrow().num_definitions;
                let ins = self.leave_scope();
                let compiled_function = Object::CompiledFunction(ins, locals_count, param_count);
                let index = self.add_constant(compiled_function);
                let _ = self.emit(Op::Constant, &[index as u64]);
            }
            Expr::Call {
                function,
                arguments,
            } => {
                let arg_count = arguments.len();
                self.compile_expr(*function)?;
                for arg in arguments {
                    self.compile_expr(arg)?;
                }

                let _ = self.emit(Op::Call, &[arg_count as u64]);
            }
        }

        Ok(())
    }

    fn compile_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Int(x) => {
                let index = self.add_constant(Object::Integer(*x));
                let _ = self.emit(Op::Constant, &[index as u64]);
            }
            Literal::Boolean(bool) => {
                let _ = if *bool {
                    self.emit(Op::True, &[])
                } else {
                    self.emit(Op::False, &[])
                };
            }
            Literal::String(s) => {
                let index = self.add_constant(Object::String(s.to_owned()));
                let _ = self.emit(Op::Constant, &[index as u64]);
            }
        }
    }

    fn current_instruction(&mut self) -> &mut Instructions {
        &mut self.scopes.get_mut(self.scope_index).unwrap().instructions
    }

    fn emit(&mut self, op: Op, operands: &[u64]) -> usize {
        let pos = self.add_instruction(&mut op.make(operands));

        self.set_last_instruction(op, pos);

        pos
    }

    fn add_instruction(&mut self, ins: &mut Vec<u8>) -> usize {
        let instructions = self.current_instruction();
        let pos_new_ins = instructions.0.len();
        instructions.0.append(ins);
        pos_new_ins
    }

    fn add_constant(&mut self, value: Object) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn change_operand(&mut self, pos: usize, operand: u64) {
        let op = Op::from_u8(self.current_instruction().0[pos]);
        let new = op.make(&[operand]);

        self.current_instruction().replace(pos, &new);
    }

    fn replace_last_pop_with_return(&mut self) {
        let last_pop = self.scopes[self.scope_index].last_instruction.unwrap().pos;
        self.current_instruction()
            .replace(last_pop, &Op::ReturnValue.make(&[]));
        let last_scope = self.scopes.get_mut(self.scope_index).unwrap();
        last_scope.last_instruction = Some(EmittedInstruction {
            op: Op::ReturnValue,
            pos: last_scope.last_instruction.unwrap().pos,
        });
    }

    fn set_last_instruction(&mut self, op: Op, pos: usize) {
        let previous = self.scopes[self.scope_index].last_instruction;
        let last = EmittedInstruction { op, pos };

        self.scopes[self.scope_index].previous_instruction = previous;
        self.scopes[self.scope_index].last_instruction = Some(last);
    }

    fn last_instruction_is(&mut self, op: Op) -> bool {
        if self.current_instruction().0.is_empty() {
            return false;
        }

        self.scopes[self.scope_index].last_instruction.unwrap().op == op
    }

    fn remove_last_pop(&mut self) {
        let pos = self.scopes[self.scope_index].last_instruction.unwrap().pos;
        self.current_instruction().remove_last_instruction(pos);
        self.scopes[self.scope_index].last_instruction =
            self.scopes[self.scope_index].previous_instruction;
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
        self.scope_index += 1;
        let table = SymbolTable::new_enclosed(Rc::clone(&self.symbol_table));
        self.symbol_table = Rc::new(RefCell::new(table));
    }

    fn leave_scope(&mut self) -> Instructions {
        let scope = self.scopes.remove(self.scope_index);
        self.scope_index -= 1;
        let binding = self.symbol_table.clone();
        let table: &RefCell<SymbolTable> = binding.borrow();
        self.symbol_table = Rc::clone(table.borrow().outer.as_ref().unwrap());

        scope.instructions
    }

    pub fn byte_code(&self) -> ByteCode {
        ByteCode {
            instructions: self.scopes[self.scope_index].instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    pub fn consume(self) -> (Rc<RefCell<SymbolTable>>, Vec<Object>) {
        (self.symbol_table, self.constants)
    }
}

pub struct ByteCode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Debug, Clone, Copy)]
struct EmittedInstruction {
    op: Op,
    pos: usize,
}

#[cfg(test)]
mod test {
    use std::{borrow::Borrow, cell::RefCell, error::Error, rc::Rc};

    use crate::{
        compiler::{symbol_table::SymbolTable, Compiler},
        interpreter::{evaluator::object::Object, parser::Parser},
    };

    use super::{chunk::Instructions, code::Op};

    fn flatten(vec: Vec<Instructions>) -> Instructions {
        let mut res = Vec::new();
        for mut instr in vec {
            res.append(&mut instr.0);
        }

        Instructions::vec(res)
    }

    fn compile_test(
        tests: &[(&str, Vec<Object>, Vec<Instructions>)],
    ) -> Result<(), Box<dyn Error>> {
        for test in tests {
            let mut compiler = Compiler::new();
            let program = Parser::parse(test.0)?;
            compiler.compile(program.clone())?;
            let byte_code = compiler.byte_code();

            if (byte_code.instructions.to_string() != flatten(test.2.clone()).to_string())
                || (byte_code.constants != test.1)
            {
                println!(
                    "Testing input: '{}'\nGot bytecode:\n{}\n({:?})\n\nWanted bytecode:\n{}\n({:?})",
                    test.0,
                    byte_code.instructions,
                    byte_code.instructions,
                    flatten(test.2.clone()),
                    flatten(test.2.clone())
                );
                println!(
                    "Got constants:\n{:?}\nWanted constants:\n{:?}",
                    byte_code.constants, test.1
                );
                println!("program:\n{program:?}");
            }

            assert_eq!(byte_code.constants, test.1);
            assert_eq!(
                byte_code.instructions.0.len(),
                flatten(test.2.clone()).0.len()
            );
            assert_eq!(
                byte_code.instructions.to_string(),
                flatten(test.2.clone()).to_string()
            );
        }

        Ok(())
    }

    fn ins(op: Op, operands: &[u64]) -> Instructions {
        Instructions::vec(op.make(operands))
    }

    fn fct(vec: Vec<Instructions>, locals: usize, params: usize) -> Object {
        Object::CompiledFunction(flatten(vec), locals, params)
    }

    #[test]
    fn builtins_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "len([]); push([], 1);",
                vec![Object::Integer(1)],
                vec![
                    ins(Op::GetBuiltin, &[0]),
                    ins(Op::Array, &[0]),
                    ins(Op::Call, &[1]),
                    ins(Op::Pop, &[]),
                    ins(Op::GetBuiltin, &[4]),
                    ins(Op::Array, &[0]),
                    ins(Op::Constant, &[0]),
                    ins(Op::Call, &[2]),
                    ins(Op::Pop, &[]),
                ],
            ),
            (
                "fn() { len([]) }",
                vec![fct(
                    vec![
                        ins(Op::GetBuiltin, &[0]),
                        ins(Op::Array, &[0]),
                        ins(Op::Call, &[1]),
                        ins(Op::ReturnValue, &[]),
                    ],
                    0,
                    0,
                )],
                vec![ins(Op::Constant, &[0]), ins(Op::Pop, &[])],
            ),
        ])
    }

    #[test]
    fn let_scope_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "let num = 55; fn() { num }",
                vec![
                    Object::Integer(55),
                    fct(
                        vec![ins(Op::GetGlobal, &[0]), ins(Op::ReturnValue, &[])],
                        0,
                        0,
                    ),
                ],
                vec![
                    ins(Op::Constant, &[0]),
                    ins(Op::SetGlobal, &[0]),
                    ins(Op::Constant, &[1]),
                    ins(Op::Pop, &[]),
                ],
            ),
            (
                "fn() { let num = 55; num }",
                vec![
                    Object::Integer(55),
                    fct(
                        vec![
                            ins(Op::Constant, &[0]),
                            ins(Op::SetLocal, &[0]),
                            ins(Op::GetLocal, &[0]),
                            ins(Op::ReturnValue, &[]),
                        ],
                        1,
                        0,
                    ),
                ],
                vec![ins(Op::Constant, &[1]), ins(Op::Pop, &[])],
            ),
            (
                "fn() { let a = 55; let b = 77; a + b }",
                vec![
                    Object::Integer(55),
                    Object::Integer(77),
                    fct(
                        vec![
                            ins(Op::Constant, &[0]),
                            ins(Op::SetLocal, &[0]),
                            ins(Op::Constant, &[1]),
                            ins(Op::SetLocal, &[1]),
                            ins(Op::GetLocal, &[0]),
                            ins(Op::GetLocal, &[1]),
                            ins(Op::Add, &[]),
                            ins(Op::ReturnValue, &[]),
                        ],
                        2,
                        0,
                    ),
                ],
                vec![ins(Op::Constant, &[2]), ins(Op::Pop, &[])],
            ),
        ])
    }

    #[test]
    fn call_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "fn() { 24 }()",
                vec![
                    Object::Integer(24),
                    fct(
                        vec![ins(Op::Constant, &[0]), ins(Op::ReturnValue, &[])],
                        0,
                        0,
                    ),
                ],
                vec![
                    ins(Op::Constant, &[1]),
                    ins(Op::Call, &[0]),
                    ins(Op::Pop, &[]),
                ],
            ),
            (
                "let noArg = fn() { 24 }; noArg();",
                vec![
                    Object::Integer(24),
                    fct(
                        vec![ins(Op::Constant, &[0]), ins(Op::ReturnValue, &[])],
                        0,
                        0,
                    ),
                ],
                vec![
                    ins(Op::Constant, &[1]),
                    ins(Op::SetGlobal, &[0]),
                    ins(Op::GetGlobal, &[0]),
                    ins(Op::Call, &[0]),
                    ins(Op::Pop, &[]),
                ],
            ),
            (
                "let oneArg = fn(a) { a }; oneArg(24);",
                vec![
                    fct(
                        vec![ins(Op::GetLocal, &[0]), ins(Op::ReturnValue, &[])],
                        1,
                        1,
                    ),
                    Object::Integer(24),
                ],
                vec![
                    ins(Op::Constant, &[0]),
                    ins(Op::SetGlobal, &[0]),
                    ins(Op::GetGlobal, &[0]),
                    ins(Op::Constant, &[1]),
                    ins(Op::Call, &[1]),
                    ins(Op::Pop, &[]),
                ],
            ),
            (
                "let manyArg = fn(a, b, c) { a; b; c }; manyArg(24, 25, 26);",
                vec![
                    fct(
                        vec![
                            ins(Op::GetLocal, &[0]),
                            ins(Op::Pop, &[]),
                            ins(Op::GetLocal, &[1]),
                            ins(Op::Pop, &[]),
                            ins(Op::GetLocal, &[2]),
                            ins(Op::ReturnValue, &[]),
                        ],
                        3,
                        3,
                    ),
                    Object::Integer(24),
                    Object::Integer(25),
                    Object::Integer(26),
                ],
                vec![
                    ins(Op::Constant, &[0]),
                    ins(Op::SetGlobal, &[0]),
                    ins(Op::GetGlobal, &[0]),
                    ins(Op::Constant, &[1]),
                    ins(Op::Constant, &[2]),
                    ins(Op::Constant, &[3]),
                    ins(Op::Call, &[3]),
                    ins(Op::Pop, &[]),
                ],
            ),
        ])
    }

    #[test]
    fn function_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "fn() { return 5 + 10 }",
                vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    fct(
                        vec![
                            ins(Op::Constant, &[0]),
                            ins(Op::Constant, &[1]),
                            ins(Op::Add, &[]),
                            ins(Op::ReturnValue, &[]),
                        ],
                        0,
                        0,
                    ),
                ],
                vec![ins(Op::Constant, &[2]), ins(Op::Pop, &[])],
            ),
            (
                "fn() { 5 + 10 }",
                vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    fct(
                        vec![
                            ins(Op::Constant, &[0]),
                            ins(Op::Constant, &[1]),
                            ins(Op::Add, &[]),
                            ins(Op::ReturnValue, &[]),
                        ],
                        0,
                        0,
                    ),
                ],
                vec![ins(Op::Constant, &[2]), ins(Op::Pop, &[])],
            ),
            (
                "fn() { 1; 2 }",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    fct(
                        vec![
                            ins(Op::Constant, &[0]),
                            ins(Op::Pop, &[]),
                            ins(Op::Constant, &[1]),
                            ins(Op::ReturnValue, &[]),
                        ],
                        0,
                        0,
                    ),
                ],
                vec![ins(Op::Constant, &[2]), ins(Op::Pop, &[])],
            ),
            (
                "fn() { }",
                vec![fct(vec![ins(Op::Return, &[])], 0, 0)],
                vec![ins(Op::Constant, &[0]), ins(Op::Pop, &[])],
            ),
        ])
    }

    #[test]
    fn scope_test() {
        let mut compiler = Compiler::new();
        assert_eq!(compiler.scope_index, 0);
        let global_table = Rc::clone(&compiler.symbol_table);

        compiler.emit(Op::Mul, &[]);

        // Entering scope
        compiler.enter_scope();
        assert_eq!(compiler.scope_index, 1);

        compiler.emit(Op::Sub, &[]);
        assert_eq!(
            compiler.scopes[compiler.scope_index].instructions.0.len(),
            1
        );
        assert_eq!(
            compiler.scopes[compiler.scope_index]
                .last_instruction
                .unwrap()
                .op,
            Op::Sub
        );

        let sym = Rc::clone(&compiler.symbol_table);
        let table: &RefCell<SymbolTable> = sym.borrow();
        let outer = &table.borrow().outer;
        assert!(outer.is_some());
        if let Some(table) = outer {
            assert_eq!(table, &global_table);
        }

        compiler.leave_scope();
        // Leaving scope

        assert_eq!(compiler.scope_index, 0);
        assert_eq!(compiler.symbol_table, global_table.clone());
        let sym = Rc::clone(&compiler.symbol_table);
        let table: &RefCell<SymbolTable> = sym.borrow();
        let outer = &table.borrow().outer;
        assert!(outer.is_none());

        compiler.emit(Op::Add, &[]);
        assert_eq!(
            compiler.scopes[compiler.scope_index].instructions.0.len(),
            2
        );
        assert_eq!(
            compiler.scopes[compiler.scope_index]
                .last_instruction
                .unwrap()
                .op,
            Op::Add
        );
        assert_eq!(
            compiler.scopes[compiler.scope_index]
                .previous_instruction
                .unwrap()
                .op,
            Op::Mul
        );
    }

    #[test]
    fn index_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "[1, 2, 3][1 + 1]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(1),
                    Object::Integer(1),
                ],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Constant.make(&[2])),
                    Instructions::vec(Op::Array.make(&[3])),
                    Instructions::vec(Op::Constant.make(&[3])),
                    Instructions::vec(Op::Constant.make(&[4])),
                    Instructions::vec(Op::Add.make(&[])),
                    Instructions::vec(Op::Index.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "{1: 2}[2 - 1]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(2),
                    Object::Integer(1),
                ],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Hash.make(&[2])),
                    Instructions::vec(Op::Constant.make(&[2])),
                    Instructions::vec(Op::Constant.make(&[3])),
                    Instructions::vec(Op::Sub.make(&[])),
                    Instructions::vec(Op::Index.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
        ])
    }

    #[test]
    fn hash_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "{}",
                vec![],
                vec![
                    Instructions::vec(Op::Hash.make(&[0])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "{1: 2, 3: 4, 5: 6}",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Constant.make(&[2])),
                    Instructions::vec(Op::Constant.make(&[3])),
                    Instructions::vec(Op::Constant.make(&[4])),
                    Instructions::vec(Op::Constant.make(&[5])),
                    Instructions::vec(Op::Hash.make(&[6])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "{1: 2 + 3, 4: 5 * 6}",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Constant.make(&[2])),
                    Instructions::vec(Op::Add.make(&[])),
                    Instructions::vec(Op::Constant.make(&[3])),
                    Instructions::vec(Op::Constant.make(&[4])),
                    Instructions::vec(Op::Constant.make(&[5])),
                    Instructions::vec(Op::Mul.make(&[])),
                    Instructions::vec(Op::Hash.make(&[4])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
        ])
    }

    #[test]
    fn array_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "[]",
                vec![],
                vec![
                    Instructions::vec(Op::Array.make(&[0])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "[1, 2, 3]",
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Constant.make(&[2])),
                    Instructions::vec(Op::Array.make(&[3])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Add.make(&[])),
                    Instructions::vec(Op::Constant.make(&[2])),
                    Instructions::vec(Op::Constant.make(&[3])),
                    Instructions::vec(Op::Sub.make(&[])),
                    Instructions::vec(Op::Constant.make(&[4])),
                    Instructions::vec(Op::Constant.make(&[5])),
                    Instructions::vec(Op::Mul.make(&[])),
                    Instructions::vec(Op::Array.make(&[3])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
        ])
    }

    #[test]
    fn string_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "\"monkey\"",
                vec![Object::String("monkey".to_owned())],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "\"mon\" + \"key\"",
                vec![
                    Object::String("mon".to_owned()),
                    Object::String("key".to_owned()),
                ],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::Add.make(&[])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
        ])
    }

    #[test]
    fn global_let_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "let one = 1; let two= 2;",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::SetGlobal.make(&[0])),
                    Instructions::vec(Op::Constant.make(&[1])),
                    Instructions::vec(Op::SetGlobal.make(&[1])),
                ],
            ),
            (
                "let one = 1; one;",
                vec![Object::Integer(1)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::SetGlobal.make(&[0])),
                    Instructions::vec(Op::GetGlobal.make(&[0])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
            (
                "let one = 1; let two = one; two;",
                vec![Object::Integer(1)],
                vec![
                    Instructions::vec(Op::Constant.make(&[0])),
                    Instructions::vec(Op::SetGlobal.make(&[0])),
                    Instructions::vec(Op::GetGlobal.make(&[0])),
                    Instructions::vec(Op::SetGlobal.make(&[1])),
                    Instructions::vec(Op::GetGlobal.make(&[1])),
                    Instructions::vec(Op::Pop.make(&[])),
                ],
            ),
        ])
    }

    #[test]
    fn conditional_test() -> Result<(), Box<dyn Error>> {
        compile_test(&[
            (
                "if (true) { 10 }; 3333;",
                vec![Object::Integer(10), Object::Integer(3333)],
                vec![
                    Instructions::vec(Op::True.make(&[])),            // 0000
                    Instructions::vec(Op::JumpNotTruthy.make(&[10])), // 0001
                    Instructions::vec(Op::Constant.make(&[0])),       // 0004
                    Instructions::vec(Op::Jump.make(&[11])),          // 0007
                    Instructions::vec(Op::Null.make(&[])),            // 0010
                    Instructions::vec(Op::Pop.make(&[])),             // 0011
                    Instructions::vec(Op::Constant.make(&[1])),       // 0012
                    Instructions::vec(Op::Pop.make(&[])),             // 0015
                ],
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                vec![
                    Instructions::vec(Op::True.make(&[])),            // 0000
                    Instructions::vec(Op::JumpNotTruthy.make(&[10])), // 0001
                    Instructions::vec(Op::Constant.make(&[0])),       // 0004
                    Instructions::vec(Op::Jump.make(&[13])),          // 0007
                    Instructions::vec(Op::Constant.make(&[1])),       // 0010
                    Instructions::vec(Op::Pop.make(&[])),             // 0013
                    Instructions::vec(Op::Constant.make(&[2])),       // 0014
                    Instructions::vec(Op::Pop.make(&[])),             // 0017
                ],
            ),
        ])
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
        let tests = &[(Op::Constant, &[65535], 2), (Op::GetLocal, &[255], 1)];

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
                Instructions::vec(Op::GetLocal.make(&[1])),
            ],
            "0000 OpAdd\n0001 OpConstant 2\n0004 OpConstant 65535\n0007 OpGetLocal 1",
        )];

        for test in tests {
            assert_eq!(flatten(test.0.clone()).to_string(), test.1);
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
            (Op::GetLocal, vec![255], vec![Op::GetLocal as u8, 0xFF]),
        ];

        for test in tests {
            let instruction = test.0.make(&test.1);
            assert_eq!(instruction, test.2);
        }
    }
}
