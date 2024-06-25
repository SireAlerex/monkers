pub mod chunk;
pub mod code;
pub mod compiler;
pub mod vm;

#[cfg(test)]
mod test {
    use std::error::Error;

    use crate::{
        compiler::{compiler::Compiler, vm::VM},
        interpreter::{
            ast::Program, evaluator::object::Object, lexer::Lexer, parser::Parser, token::Source,
        },
    };

    use super::{chunk::Instructions, code::Op};

    fn parse(input: &str) -> Program {
        Parser::new(Lexer::new(input, Source::Repl)).parse_program()
    }

    fn flatten(vec: &mut Vec<Instructions>) -> Instructions {
        let mut res = Vec::new();
        for instr in vec {
            res.append(&mut instr.0);
        }

        Instructions::vec(res)
    }

    fn vm_test(tests: &[(&str, Object)]) -> Result<(), Box<dyn Error>> {
        for test in tests {
            let mut compiler = Compiler::new();
            compiler.compile(parse(test.0));
            let mut vm = VM::new(compiler.byte_code());
            vm.run()?;

            let stack_elem = vm.stack_top();
            assert_eq!(stack_elem, test.1);
        }

        Ok(())
    }

    #[test]
    fn integer_test() -> Result<(), Box<dyn Error>> {
        let tests = &[
            ("1", Object::Integer(1)),
            ("2", Object::Integer(2)),
            ("1 + 2", Object::Integer(3)),
        ];
        vm_test(tests)
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
    fn compile_test() {
        // TODO add function to take ints and return objects (and other type)
        let tests = &[(
            "1 + 2",
            vec![Object::Integer(1), Object::Integer(2)],
            vec![
                Instructions::vec(Op::Constant.make(&[0])),
                Instructions::vec(Op::Constant.make(&[1])),
                Instructions::vec(Op::Add.make(&[])),
            ],
        )];

        for test in tests {
            let mut compiler = Compiler::new();
            compiler.compile(parse(test.0));
            let byte_code = compiler.byte_code();

            assert_eq!(byte_code.constants, test.1);
            println!("Got bytecode:\n{}", byte_code.instructions);
            println!("\nCorrect bytecode:\n{}", flatten(&mut test.2.clone()));
            assert_eq!(
                byte_code.instructions.to_string(),
                flatten(&mut test.2.clone()).to_string()
            );
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
