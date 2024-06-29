use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Sub},
};

use crate::{
    compiler::code::Op,
    interpreter::{
        ast::Literal,
        evaluator::{
            builtin::BuiltinFunctions,
            object::{Closure, CompiledFunction, Object, NULL, UNINT_OBJECT},
        },
    },
    utils,
};

use super::{chunk::Instructions, ByteCode};

macro_rules! null {
    ($self: ident) => {
        $self.push(NULL)
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

macro_rules! check_binary_ref {
    ($self: ident, $func: ident) => {{
        let right = $self.pop();
        let left = $self.pop();
        match left.$func(&right) {
            Object::Error(err) => return Err(err),
            obj => $self.push(obj)?,
        }
    }};
}

const STACK_SIZE: usize = 2048;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
pub const GLOBAL_SIZE: usize = 65536;
const MAX_FRAMES: usize = 1024;

#[derive(Debug, Clone)]
pub struct Frame {
    closure: Closure,
    ip: usize,
    base_pointer: usize,
}

impl Frame {
    pub const fn new(closure: Closure, base_pointer: usize) -> Self {
        Self {
            closure,
            ip: 0,
            base_pointer,
        }
    }

    #[inline]
    pub const fn instructions(&self) -> &Instructions {
        &self.closure.func.instructions
    }
}

impl Default for Frame {
    fn default() -> Self {
        Self {
            closure: Closure {
                func: Box::new(CompiledFunction {
                    instructions: Instructions::new(),
                    locals_count: 0,
                    parameters_count: 0,
                }),
                free_variables: vec![],
            },
            ip: Default::default(),
            base_pointer: Default::default(),
        }
    }
}

// to avoid allocation, stack of &Object, pointing to alloc handler ?
#[derive(Debug, Clone)]
pub struct VM {
    constants: Vec<Object>,
    stack: Box<[Object; STACK_SIZE]>,
    sp: usize,
    // heap allocated
    globals: Box<[Object]>,
    frames: Vec<Frame>,
    frame_index: usize,
}

impl VM {
    #[allow(dead_code)]
    pub fn new(byte_code: ByteCode) -> Self {
        let mut frames = vec![Frame::default(); MAX_FRAMES];
        let main_frame = Frame::new(Closure::from_instructions(byte_code.instructions), 0);
        frames[0] = main_frame;

        Self {
            constants: byte_code.constants,
            stack: Box::new([UNINT_OBJECT; STACK_SIZE]),
            sp: 0,
            globals: vec![UNINT_OBJECT; GLOBAL_SIZE].into_boxed_slice(),
            frames,
            frame_index: 1,
        }
    }

    pub fn new_with_globals_store(byte_code: ByteCode, globals: Box<[Object]>) -> Self {
        let mut frames = vec![Frame::default(); MAX_FRAMES];
        let main_frame = Frame::new(Closure::from_instructions(byte_code.instructions), 0);
        frames[0] = main_frame;

        Self {
            constants: byte_code.constants,
            stack: Box::new([UNINT_OBJECT; STACK_SIZE]),
            sp: 0,
            globals,
            frames,
            frame_index: 1,
        }
    }

    pub fn stack_top(&self) -> Object {
        self.stack[self.sp].clone()
    }

    // TODO check for Object::Error
    #[allow(unreachable_patterns)]
    pub fn run(&mut self) -> Result<(), String> {
        while self.current_frame().ip < self.current_frame().instructions().0.len() {
            // let pretty_stack = self.show_stack();
            let ip = self.current_frame().ip;
            let ins = self.current_frame().instructions();
            let op: Op = Op::from_u8(ins[ip]);
            // println!("stack: {pretty_stack}\nvm run ip={} op:{:?} operands:{:?}", ip, op, op.read_operands(&ins.0[ip+1..]));

            match op {
                Op::Constant => {
                    let idx = self.next_two_bytes();
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
                    let start = ip + 1;
                    let pos = utils::read_u16(&ins.0[start..]);
                    self.current_frame().ip = pos as usize - 1;
                }
                Op::JumpNotTruthy => {
                    let pos = self.next_two_bytes();

                    let cond = self.pop();
                    if !cond.is_truthy() {
                        self.current_frame().ip = pos as usize - 1;
                    }
                }
                Op::Null => null!(self)?,
                Op::SetGlobal => {
                    let global_idx = self.next_two_bytes();

                    self.globals[global_idx as usize] = self.pop();
                }
                Op::GetGlobal => {
                    let global_idx = self.next_two_bytes();

                    self.push(self.globals[global_idx as usize].clone())?;
                }
                Op::Array => {
                    let elem_count = self.next_two_bytes();

                    let array = self.build_array(self.sp - elem_count as usize, self.sp);
                    self.sp -= elem_count as usize;
                    self.push(array)?;
                }
                Op::Hash => {
                    let elem_count = self.next_two_bytes();

                    let hash = self.build_hash(self.sp - elem_count as usize, self.sp)?;
                    self.push(hash)?;
                }
                Op::Index => {
                    check_binary_ref!(self, get_unchecked_key);
                }
                Op::Call => {
                    let args_count = self.next_byte();

                    match self.stack[self.sp - 1 - args_count as usize].clone() {
                        Object::Closure(closure) => {
                            if args_count as usize != closure.func.parameters_count {
                                return err!(
                                    "wrong number of arguments: want={}, got={args_count}",
                                    closure.func.parameters_count
                                );
                            }

                            let base_pointer = self.sp - args_count as usize;
                            self.sp = base_pointer + closure.func.locals_count;
                            let frame = Frame::new(closure, base_pointer);
                            self.push_frame(frame);
                            continue;
                        }
                        Object::Builtin(builtin) => {
                            let args = &self.stack[self.sp - args_count as usize..self.sp];
                            let result = builtin(args);
                            self.push(result)?;
                        }
                        obj => panic!("calling non-function type: {}", obj.get_type()),
                    }
                }
                Op::ReturnValue => {
                    let value = self.pop();

                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push(value)?;
                }
                Op::Return => {
                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push(NULL)?;
                }
                Op::GetLocal => {
                    let local_index = self.next_byte();

                    let frame = self.current_frame();
                    let idx = frame.base_pointer + local_index as usize;
                    let obj = self.stack[idx].clone();
                    self.push(obj)?;
                }
                Op::SetLocal => {
                    let local_index = self.next_byte();

                    let obj = self.pop();
                    let frame = self.current_frame();
                    self.stack[frame.base_pointer + local_index as usize] = obj;
                }
                Op::GetBuiltin => {
                    let index = self.next_byte();
                    self.push(BuiltinFunctions::get_from_index(index as usize))?;
                }
                Op::Closure => {
                    let const_index = self.next_two_bytes();
                    let free_count = self.next_byte() as usize;

                    let constant = self.constants[const_index as usize].clone();
                    if let Object::CompiledFunction(func) = constant {
                        let mut free_variables: Vec<Object> = Vec::with_capacity(free_count);
                        for i in 0..free_count {
                            let mut obj = UNINT_OBJECT;
                            core::mem::swap(&mut obj, &mut self.stack[self.sp - free_count + i]);
                            free_variables.insert(i, obj);
                        }
                        self.sp -= free_count;

                        self.push(Object::Closure(Closure {
                            func: Box::new(func),
                            free_variables,
                        }))?;
                    } else {
                        return err!("not a function: {constant:?}");
                    }
                }
                Op::GetFree => {
                    let free_index = self.next_byte();
                    let current_closure =
                        self.current_frame().closure.free_variables[free_index as usize].clone();
                    self.push(current_closure)?;
                }
                Op::CurrentClosure => {
                    let closure = self.current_frame().closure.clone();
                    self.push(Object::Closure(closure))?;
                }
                op => err!("'{op:?}' is unimplemented for vm")?,
            }

            self.current_frame().ip += 1;
        }
        Ok(())
    }

    fn build_hash(&mut self, start: usize, end: usize) -> Result<Object, String> {
        let mut hash: HashMap<Literal, Object> = HashMap::new();

        for i in (start..end).step_by(2) {
            let mut key = UNINT_OBJECT;
            core::mem::swap(&mut key, &mut self.stack[i]);
            let key = Literal::try_from(key)?;

            let mut value = UNINT_OBJECT;
            core::mem::swap(&mut value, &mut self.stack[i + 1]);

            hash.insert(key, value);
        }

        Ok(Object::Hash(Box::new(hash)))
    }

    fn build_array(&mut self, start: usize, end: usize) -> Object {
        let mut elements: Vec<Object> = vec![UNINT_OBJECT; end - start];

        for i in start..end {
            core::mem::swap(&mut elements[i - start], &mut self.stack[i]);
        }

        Object::Array(elements)
    }

    #[inline]
    fn next_byte(&mut self) -> u8 {
        self.current_frame().ip += 1;
        let ip = self.current_frame().ip;
        self.current_frame().instructions()[ip]
    }

    #[inline]
    fn next_two_bytes(&mut self) -> u16 {
        let start = self.current_frame().ip + 1;
        let global_idx = utils::read_u16(&self.current_frame().instructions().0[start..]);
        self.current_frame().ip += 2;
        global_idx
    }

    #[inline]
    fn pop(&mut self) -> Object {
        let obj = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        obj
    }

    #[inline]
    fn push(&mut self, obj: Object) -> Result<(), String> {
        if self.sp >= STACK_SIZE {
            Err("stack overflow".to_owned())
        } else {
            self.stack[self.sp] = obj;
            self.sp += 1;

            Ok(())
        }
    }

    #[inline]
    fn current_frame(&mut self) -> &mut Frame {
        self.frames.get_mut(self.frame_index - 1).unwrap()
    }

    #[inline]
    fn push_frame(&mut self, frame: Frame) {
        self.frames[self.frame_index] = frame;
        self.frame_index += 1;
    }

    #[inline]
    fn pop_frame(&mut self) -> &mut Frame {
        self.frame_index -= 1;
        &mut self.frames[self.frame_index]
    }

    // debug function
    #[allow(dead_code)]
    pub fn show_stack(&self) -> String {
        format!(
            "{:?}",
            self.stack
                .iter()
                .enumerate()
                .filter(|(idx, obj)| **obj != Object::Uninit && *idx < self.sp)
                .map(|(_, obj)| obj)
                .collect::<Vec<&Object>>()
        )
    }

    pub fn globals(self) -> Box<[Object]> {
        self.globals
    }
}

#[cfg(test)]
mod test {
    use std::{collections::HashMap, error::Error};

    use crate::{
        compiler::{vm::VM, Compiler},
        interpreter::{ast::Literal, evaluator::object::Object, parser::Parser},
    };

    use super::NULL;

    fn vm_test<T>(tests: &[(&str, T)]) -> Result<(), Box<dyn Error>>
    where
        T: Into<Object> + Sized + Clone,
    {
        for test in tests {
            let mut compiler = Compiler::new();
            compiler.compile(Parser::parse(test.0)?)?;
            println!(
                "Testing input: '{}'\nBytecode:\n{}\n({:?})",
                test.0,
                compiler.byte_code().instructions,
                compiler.byte_code().instructions
            );
            println!(
                "Constants:\n[{}]",
                compiler
                    .byte_code()
                    .constants
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            );
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

    fn hash<L: Into<Literal>, T: Into<Object>>(pairs: Vec<(L, T)>) -> Object {
        let mut hash: HashMap<Literal, Object> = HashMap::new();
        for (k, v) in pairs {
            hash.insert(k.into(), v.into());
        }
        Object::Hash(Box::new(hash))
    }

    #[test]
    fn fibonacci_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[(
            "let fibonacci = fn(x) {
                if (x == 0) {
                return 0;
                } else {
                if (x == 1) {
                return 1;
                } else {
                fibonacci(x - 1) + fibonacci(x - 2);
                }
                }
                };
                fibonacci(10);",
            55,
        )])
    }

    #[test]
    fn recursive_function_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            (
                "let countDown = fn(x) {
                if (x == 0) {
                return 0;
                } else {
                countDown(x - 1);
                }
                };
                countDown(1);",
                0,
            ),
            (
                "let countDown = fn(x) {
                if (x == 0) {
                return 0;
                } else {
                countDown(x - 1);
                }
                };
                let wrapper = fn() {
                countDown(1);
                };
                wrapper();",
                0,
            ),
            (
                "let wrapper = fn() {
                let countDown = fn(x) {
                if (x == 0) {
                return 0;
                } else {
                countDown(x - 1);
                }
                };
                countDown(1);
                };
                wrapper();",
                0,
            ),
        ])
    }

    #[test]
    fn closure_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            (
                "let newClosure = fn(a) { fn() { a; } }; let closure = newClosure(99); closure();",
                99,
            ),
            (
                "let newAdder = fn(a, b) {
                fn(c) { a + b + c }; };
              let adder = newAdder(1, 2);
              adder(8);",
                11,
            ),
            (
                "let newAdder = fn(a, b) {
                let c = a + b;
                fn(d) { c + d }; };
              let adder = newAdder(1, 2);
              adder(8);",
                11,
            ),
            (
                "let newAdderOuter = fn(a, b) {
                let c = a + b;
                fn(d) {
                let e = d + c;
                fn(f) { e + f; };
                };
                };
                let newAdderInner = newAdderOuter(1, 2)
                let adder = newAdderInner(3);
                adder(8);",
                14,
            ),
            (
                "let a = 1;
            let newAdderOuter = fn(b) {
            fn(c) {
            fn(d) { a + b + c + d };
            };
            };
            let newAdderInner = newAdderOuter(2)
            let adder = newAdderInner(3);
            adder(8);",
                14,
            ),
            (
                "let newClosure = fn(a, b) {
                let one = fn() { a; };
                let two = fn() { b; };
                fn() { one() + two(); };
                };
                let closure = newClosure(9, 90);
                closure();",
                99,
            ),
        ])
    }

    #[test]
    fn builtins_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("len(\"\")", 0),
            ("len(\"four\")", 4),
            ("len(\"hello world\")", 11),
            ("len([1, 2, 3])", 3),
            ("len([])", 0),
            ("first([1, 2, 3])", 1),
            ("last([1, 2, 3])", 3),
        ])?;
        vm_test(&[
            (
                "len(1)",
                Object::Error("argument to 'len' not supported, got INTEGER".to_string()),
            ),
            (
                "len(\"one\", \"two\")",
                Object::Error("wrong number of arguments. got=2, want=1".to_string()),
            ),
            (
                "first(1)",
                Object::Error("argument to 'first' must be ARRAY, got INTEGER".to_string()),
            ),
            (
                "last(1)",
                Object::Error("argument to 'last' must be ARRAY, got INTEGER".to_string()),
            ),
            (
                "push(1, 1)",
                Object::Error("argument to 'push' must be ARRAY, got INTEGER".to_string()),
            ),
        ])?;
        vm_test(&[
            ("puts(\"hello\", \"world!\")", NULL),
            ("first([])", NULL),
            ("last([])", NULL),
            ("rest([])", NULL),
        ])?;
        vm_test(&[("rest([1, 2, 3])", vec![2, 3]), ("push([], 1)", vec![1])])
    }

    #[test]
    fn call_with_wrong_args() -> Result<(), Box<dyn Error>> {
        let tests = &[
            (
                "fn() { 1; }(1);",
                "VM error: wrong number of arguments: want=0, got=1",
            ),
            (
                "fn(a) { a; }();",
                "VM error: wrong number of arguments: want=1, got=0",
            ),
            (
                "fn(a, b) { a + b; }(1);",
                "VM error: wrong number of arguments: want=2, got=1",
            ),
        ];

        for test in tests {
            let program = Parser::parse(test.0)?;
            let mut compiler = Compiler::new();
            compiler.compile(program)?;

            let mut vm = VM::new(compiler.byte_code());
            let res = vm.run();
            assert!(res.is_err());
            assert_eq!(res.err().unwrap(), test.1);
        }
        Ok(())
    }

    #[test]
    fn function_with_args_and_bindings() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("let identity = fn(a) { a; }; identity(4);", 4),
            ("let sum = fn(a, b) { a + b; }; sum(1, 2);", 3),
            ("let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2);", 3),
            ("let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2) + sum(3, 4);", 10),
            ("let sum = fn(a, b) { let c = a + b; c; }; let outer = fn() { sum(1, 2) + sum(3, 4); }; outer();", 10),
            ("let globalNum = 10; let sum = fn(a, b) { let c = a + b; c + globalNum; }; let outer = fn() { sum(1, 2) + sum(3, 4) + globalNum; }; outer() + globalNum;", 50),
        ])
    }

    #[test]
    fn function_with_bindings() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("let one = fn() { let one = 1; one }; one();", 1),
            ("let oneAndTwo = fn() { let one = 1; let two = 2; one + two }; oneAndTwo();", 3),
            ("let oneAndTwo = fn() { let one = 1; let two = 2; one + two }; let threeAndFour = fn() { let three = 3; let four = 4; three + four }; oneAndTwo() + threeAndFour();", 10),
            ("let firstFoobar = fn() { let foobar = 50; foobar; }; let secondFoobar = fn() { let foobar = 100; foobar; }; firstFoobar() + secondFoobar();", 150),
            ("let globalSeed = 50; let minusOne = fn() { let num = 1; globalSeed - num; } let minusTwo = fn() { let num = 2; globalSeed - num; } minusOne() + minusTwo();", 97),
        ])
    }

    #[test]
    fn first_class_function_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("let returnsOne = fn() { 1; }; let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();", 1),
            ("let returnsOneReturner = fn() { let returnsOne = fn() { 1; }; returnsOne; }; returnsOneReturner()();", 1),
        ])
    }

    #[test]
    fn function_without_return_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("let noReturn = fn() { }; noReturn();", Object::Null),
            ("let noReturn = fn() { }; let noReturnTwo = fn() { noReturn(); }; noReturn(); noReturnTwo();", Object::Null),
        ])
    }

    #[test]
    fn early_exit_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("let earlyExit = fn() { return 99; 100; }; earlyExit();", 99),
            (
                "let earlyExit = fn() { return 99; return 100; }; earlyExit();",
                99,
            ),
        ])
    }

    #[test]
    fn call_no_arg_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("let fivePlusTen = fn() { 5 + 10; }; fivePlusTen();", 15),
            ("let one = fn() { 5 }; let two = fn() { one() }; let three = fn() { two() }; three();", 5),
            ("let one = fn() { 1; }; let two = fn() { 2; }; one() + two()", 3),
            ("let a = fn() { 1 }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c();", 3),
        ])
    }

    #[test]
    fn index_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("[1, 2, 3][1]", 2),
            ("[1, 2, 3][0 + 2]", 3),
            ("[[1, 1, 1]][0][0]", 1),
            ("{1: 1, 2: 2}[1]", 1),
            ("{1: 1, 2: 2}[2]", 2),
        ])?;
        vm_test(&[
            ("[][0]", Object::Null),
            ("[1, 2, 3][99]", Object::Null),
            ("[1][-1]", Object::Null),
            ("{1: 1}[0]", Object::Null),
            ("{}[0]", Object::Null),
        ])
    }

    #[test]
    fn hash_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("{}", hash::<i64, i64>(vec![])),
            ("{1: 2, 2: 3}", hash(vec![(1, 2), (2, 3)])),
            ("{1 + 1: 2 * 2, 3 + 3: 4 * 4}", hash(vec![(2, 4), (6, 16)])),
        ])
    }

    #[test]
    fn array_test() -> Result<(), Box<dyn Error>> {
        vm_test(&[
            ("[]", vec![]),
            ("[1, 2, 3]", vec![1, 2, 3]),
            ("[1 + 2, 3 * 4, 5 + 6]", vec![3, 12, 11]),
        ])
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
