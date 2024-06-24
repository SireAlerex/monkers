use std::{cell::RefCell, collections::HashMap, rc::Rc};

use builtin::BuiltinFunctions;
use env::Environment;
use object::{error, null, Object};

use super::ast::{Block, Expr, Literal, Operator, Program, Stmt};

pub mod builtin;
pub mod env;
pub mod object;

macro_rules! check {
    ($obj: ident) => {
        if $obj.is_error() {
            return $obj;
        }
    };
}

#[derive(Debug)]
pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn eval_program(&mut self, program: Program) -> Object {
        let res = self.eval_block(program);
        Self::unwrap_return(res)
    }

    fn eval_block(&mut self, block: Block) -> Object {
        let mut result = Object::Null;
        for stmt in block.0 {
            result = self.eval_stmt(stmt);
            if matches!(result, Object::Returned(_)) || matches!(result, Object::Error(_)) {
                return result;
            }
        }
        result
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Object {
        match stmt {
            Stmt::Expr(expr) => self.eval_expr(expr),
            Stmt::Return(expr) => {
                let res = self.eval_expr(expr);
                check!(res);
                Object::Returned(Box::new(res))
            }
            Stmt::Let(name, expr) => {
                let val = self.eval_expr(expr);
                check!(val);

                self.env.borrow_mut().set(name, val.clone())
            }
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> Object {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Int(x) => Object::Integer(x),
                Literal::Boolean(b) => Object::Boolean(b),
                Literal::String(s) => Object::String(s),
            },
            Expr::Prefix(op, expr) => {
                let right = self.eval_expr(*expr);
                check!(right);
                self.eval_prefix(op, right)
            }
            Expr::Infix(left, op, right) => {
                let left = self.eval_expr(*left);
                check!(left);
                let right = self.eval_expr(*right);
                check!(right);

                self.eval_infix(left, op, right)
            }
            Expr::If {
                cond,
                consequence,
                alternative,
            } => {
                let cond = self.eval_expr(*cond);
                check!(cond);

                if cond.is_truthy() {
                    self.eval_block(consequence)
                } else if let Some(block) = alternative {
                    self.eval_block(block)
                } else {
                    // need to return null if condition is false and no alternative
                    null!()
                }
            }
            Expr::Ident(name) => {
                if let Some(obj) = self.env.borrow().get(&name) {
                    obj.clone()
                } else if let Some(builtin) = BuiltinFunctions::get(&name) {
                    builtin
                } else {
                    error!("identifier not found: {name}")
                }
            }
            Expr::FunctionLiteral { parameters, body } => {
                Object::function(parameters, body, Rc::clone(&self.env))
            }
            Expr::Call {
                function,
                arguments,
            } => {
                let function = self.eval_expr(*function);
                check!(function);

                let args = self.eval_expr_list(&arguments);
                if args.len() == 1 {
                    let first = args.first().unwrap().clone();
                    check!(first);
                }
                self.apply_function(function, args)
            }
            Expr::Array(array) => {
                let elements = self.eval_expr_list(&array);
                if elements.len() == 1 {
                    let first = elements.first().unwrap().clone();
                    check!(first);
                }
                Object::Array(elements)
            }
            Expr::Index(left, index) => {
                let left = self.eval_expr(*left);
                check!(left);

                let index = self.eval_expr(*index);
                check!(index);

                left.get(&index)
            }
            Expr::HashLiteral(hash) => self.eval_hash(hash),
        }
    }

    fn apply_function(&mut self, function: Object, args: Vec<Object>) -> Object {
        match function {
            Object::Function(function) => {
                let old_env = Rc::clone(&self.env);

                let extented_env = Self::extend_fn_env(function.parameters, function.env, args);
                self.env = Rc::new(RefCell::new(extented_env));
                let evaluated = self.eval_block(function.body);
                self.env = old_env;

                Self::unwrap_return(evaluated)
            }
            Object::Builtin(builtin) => builtin(args),
            _ => error!("not a function : {}", function.get_type()),
        }
    }

    fn extend_fn_env(
        params: Vec<String>,
        env: Rc<RefCell<Environment>>,
        args: Vec<Object>,
    ) -> Environment {
        let mut env = Environment::enclosed_env(env);

        for (idx, param) in params.iter().enumerate() {
            env.set(
                param.to_string(),
                args.get(idx).expect("args get error").clone(),
            );
        }

        env
    }

    fn unwrap_return(object: Object) -> Object {
        if let Object::Returned(obj) = object {
            *obj
        } else {
            object
        }
    }

    fn eval_expr_list(&mut self, expressions: &[Expr]) -> Vec<Object> {
        let mut result = Vec::new();

        for expr in expressions {
            let evaluated = self.eval_expr(expr.clone());
            if evaluated.is_error() {
                return vec![evaluated];
            }
            result.push(evaluated);
        }

        result
    }

    fn eval_infix(&mut self, left: Object, op: Operator, right: Object) -> Object {
        match op {
            Operator::Plus => left + right,
            Operator::Minus => left - right,
            Operator::Asterisk => left * right,
            Operator::Slash => left / right,
            Operator::Equal => Object::Boolean(left == right),
            Operator::NotEqual => Object::Boolean(left != right),
            Operator::Less => left.less(right),
            Operator::Greater => left.greater(right),
            _ => error!("unimplemented infix operator"),
        }
    }

    fn eval_prefix(&mut self, op: Operator, right: Object) -> Object {
        match op {
            Operator::Bang => match right {
                Object::Boolean(b) => Object::Boolean(!b),
                Object::Null => Object::Boolean(true),
                _ => Object::Boolean(false),
            },
            Operator::Minus => {
                if let Object::Integer(x) = right {
                    Object::Integer(-x)
                } else {
                    error!("unknown operator: {op}{}", right.get_type())
                }
            }
            _ => error!("unknown operator: {op}{right:?}"),
        }
    }

    fn eval_hash(&mut self, pairs: Vec<(Expr, Expr)>) -> Object {
        let mut hash = HashMap::new();
        for (key, value) in pairs {
            let evaluated_key = self.eval_expr(key);
            if let Some(key) = evaluated_key.literal() {
                let value = self.eval_expr(value);
                check!(value);

                hash.insert(key, value);
            } else {
                return error!(
                    "only INTEGER, STRING and BOOLEAN are hashable, received: {}",
                    evaluated_key.get_type()
                );
            }
        }

        Object::Hash(hash)
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use crate::interpreter::{
        ast::{Block, Expr, Literal, Operator, Stmt},
        evaluator::Evaluator,
        lexer::Lexer,
        parser::Parser,
        token::Source,
    };

    use super::{
        env::Environment,
        object::{Function, Object},
    };

    fn check_tests(tests: &[(&str, Object)]) {
        for test in tests {
            println!("testing for: {}", test.0);
            let mut parser = Parser::new(Lexer::new(test.0, Source::Repl));
            let program = parser.parse_program();

            let mut evaluator = Evaluator::new();
            let object = evaluator.eval_program(program);
            println!("object: {object:?}");
            assert_eq!(object, test.1);
            assert_eq!(test.1, object);
        }
    }

    #[test]
    fn hash_test() {
        let mut hash = HashMap::new();
        hash.insert(Literal::String("one".to_owned()), Object::Integer(1));
        hash.insert(Literal::String("two".to_owned()), Object::Integer(2));
        hash.insert(Literal::String("three".to_owned()), Object::Integer(3));
        hash.insert(Literal::Int(4), Object::Integer(4));
        hash.insert(Literal::Boolean(true), Object::Integer(5));
        hash.insert(Literal::Boolean(false), Object::Integer(6));

        check_tests(&[
            (
                "let two = \"two\";
            {
                \"one\": 10 - 9,
                two: 1 + 1,
                \"thr\" + \"ee\": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }",
                Object::Hash(hash),
            ),
            ("{\"foo\": 5}[\"foo\"]", Object::Integer(5)),
            ("{\"foo\": 5}[\"bar\"]", Object::Null),
            ("let key = \"foo\"; {\"foo\": 5}[key]", Object::Integer(5)),
            ("{}[\"foo\"]", Object::Null),
            ("{5: 5}[5]", Object::Integer(5)),
            ("{true: 5}[true]", Object::Integer(5)),
            ("{false: 5}[false]", Object::Integer(5)),
        ])
    }

    #[test]
    fn array_test() {
        check_tests(&[
            (
                "[1, 2 * 2, 3 + 3]",
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(4),
                    Object::Integer(6),
                ]),
            ),
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ]);
    }

    #[test]
    fn builtin_test() {
        let tests = [
            ("len(\"\")", Object::Integer(0)),
            ("len(\"four\")", Object::Integer(4)),
            ("len(\"hello world\")", Object::Integer(11)),
            (
                "len(1)",
                Object::Error("argument to 'len' not supported, got INTEGER".to_owned()),
            ),
            (
                "len(\"one\", \"two\")",
                Object::Error("wrong number of arguments. got=2, want=1".to_owned()),
            ),
            (
                "   let map = fn(arr, f) {
                        let iter = fn(arr, accumulated) {
                            if (len(arr) == 0) {
                                accumulated
                            } else {
                                iter(rest(arr), push(accumulated, f(first(arr))));
                            }
                        };
                        iter(arr, []);
                    };
                    let a = [1, 2, 3, 4];
                    let double = fn(x) { x * 2};
                    map(a, double);",
                Object::Array(vec![
                    Object::Integer(2),
                    Object::Integer(4),
                    Object::Integer(6),
                    Object::Integer(8),
                ]),
            ),
            (
                "   let reduce = fn(arr, initial, f) {
                        let iter = fn(arr, result) {
                            if (len(arr) == 0) {
                                result
                            } else {
                                iter(rest(arr), f(result, first(arr)));
                            }
                        };
                    iter(arr, initial);
                    };
                    let sum = fn(arr) {
                        reduce(arr, 0, fn(initial, el) { initial + el });
                    };
                    sum([1, 2, 3, 4, 5])",
                Object::Integer(15),
            ),
        ];

        check_tests(&tests);
    }

    #[test]
    fn string_test() {
        let tests = [
            (
                "\"Hello World!\"",
                Object::String("Hello World!".to_owned()),
            ),
            (
                "\"Hello\" + \" \" + \"World!\"",
                Object::String("Hello World!".to_owned()),
            ),
        ];

        check_tests(&tests);
    }

    #[test]
    fn closure_test() {
        let tests = [(
            "let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);",
            Object::Integer(4),
        )];

        check_tests(&tests);
    }

    #[test]
    fn function_app_test() {
        let tests = [
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("fn(x) { x; }(5)", Object::Integer(5)),
        ];

        check_tests(&tests);
    }

    #[test]
    fn function_object_test() {
        let tests = [(
            "fn(x) { x + 2; };",
            Object::Function(Function {
                parameters: vec!["x".to_string()],
                body: Block(vec![Stmt::Expr(Expr::Infix(
                    Box::new(Expr::Ident("x".to_owned())),
                    Operator::Plus,
                    Box::new(Expr::Literal(Literal::Int(2))),
                ))]),
                env: Rc::new(RefCell::new(Environment::new())),
            }),
        )];

        check_tests(&tests);
    }

    #[test]
    fn let_test() {
        let tests = [
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];

        check_tests(&tests);
    }

    #[test]
    fn error_test() {
        let tests = [
            (
                "5 + true;",
                Object::Error("type mismatch: INTEGER + BOOLEAN".to_string()),
            ),
            (
                "5 + true; 5;",
                Object::Error("type mismatch: INTEGER + BOOLEAN".to_string()),
            ),
            (
                "-true",
                Object::Error("unknown operator: -BOOLEAN".to_string()),
            ),
            (
                "true + false;",
                Object::Error("unknown operator: BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "5; true + false; 5",
                Object::Error("unknown operator: BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }
                ",
                Object::Error("unknown operator: BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "foobar",
                Object::Error("identifier not found: foobar".to_string()),
            ),
            (
                "\"Hello\" - \"World\";",
                Object::Error("unknown operator: STRING - STRING".to_string()),
            ),
            (
                "{\"name\": \"Monkey\"}[fn(x) { x }];",
                Object::Error("unusable as hash key: FUNCTION".to_string()),
            ),
        ];

        check_tests(&tests);
    }

    #[test]
    fn return_test() {
        let tests = [
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                "if (10 > 1) {
                if (10 > 1) {
                return 10;
                }
                return 1;
                }",
                Object::Integer(10),
            ),
        ];

        check_tests(&tests);
    }

    #[test]
    fn if_else_test() {
        let tests = [
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        check_tests(&tests);
    }

    #[test]
    fn bang_op_test() {
        let tests = [
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!5", Object::Boolean(false)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
            ("!!5", Object::Boolean(true)),
        ];

        check_tests(&tests);
    }

    #[test]
    fn boolean_expr_test() {
        let tests = [
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("1 < 2", Object::Boolean(true)),
            ("1 > 2", Object::Boolean(false)),
            ("1 < 1", Object::Boolean(false)),
            ("1 > 1", Object::Boolean(false)),
            ("1 == 1", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("1 == 2", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
            ("true == true", Object::Boolean(true)),
            ("false == false", Object::Boolean(true)),
            ("true == false", Object::Boolean(false)),
            ("true != false", Object::Boolean(true)),
            ("false != true", Object::Boolean(true)),
            ("(1 < 2) == true", Object::Boolean(true)),
            ("(1 < 2) == false", Object::Boolean(false)),
            ("(1 > 2) == true", Object::Boolean(false)),
            ("(1 > 2) == false", Object::Boolean(true)),
        ];

        check_tests(&tests);
    }

    #[test]
    fn int_expr_test() {
        let tests = [
            ("5", Object::Integer(5)),
            ("11", Object::Integer(11)),
            ("-5", Object::Integer(-5)),
            ("-11", Object::Integer(-11)),
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("20 + 2 * -10", Object::Integer(0)),
            ("50 / 2 * 2 + 10", Object::Integer(60)),
            ("2 * (5 + 10)", Object::Integer(30)),
            ("3 * 3 * 3 + 10", Object::Integer(37)),
            ("3 * (3 * 3) + 10", Object::Integer(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ];

        check_tests(&tests);
    }
}
