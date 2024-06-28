use std::{
    cell::RefCell,
    error::Error,
    fs::File,
    io::{BufRead, BufReader, Read, Write},
    path::Path,
    rc::Rc,
};

use crate::{
    compiler::{
        symbol_table::SymbolTable,
        vm::{GLOBAL_SIZE, VM},
        Compiler,
    },
    interpreter::{
        evaluator::{
            object::{Object, UNINT_OBJECT},
            Evaluator,
        },
        lexer::Lexer,
        parser::Parser,
        token::Source,
    },
};

#[allow(unused_variables, unused_mut)]
pub fn start<R: BufRead, W: Write>(input: R, mut output: W) -> Result<(), Box<dyn Error>> {
    let mut evaluator = Evaluator::new();
    let mut symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
    let mut constants: Vec<Object> = Vec::new();
    let mut globals = (vec![UNINT_OBJECT; GLOBAL_SIZE]).into_boxed_slice();

    write_flush(&mut output, b">> ");
    for line in input.lines().map_while(Result::ok) {
        // eval_str(&line, &mut evaluator, &mut output, Source::Repl);

        let mut parser = Parser::new(Lexer::new(&line, Source::Repl));
        let program = parser.parse_program();

        if parser.is_err() {
            print_parser_errors(&mut output, parser.errors());
            return Err("can't run code because of parsing errors".to_owned().into());
        }

        let mut compiler = Compiler::new_with_state(&symbol_table, constants);
        compiler.compile(program)?;
        let mut vm = VM::new_with_globals_store(compiler.byte_code(), globals);
        vm.run()?;
        if !matches!(vm.stack_top(), Object::Null) {
            write_flush(&mut output, format!("{}\n", vm.stack_top()).as_bytes());
        }

        globals = vm.globals();
        (symbol_table, constants) = compiler.consume();

        write_flush(&mut output, b">> ");
    }
    Ok(())
}

pub fn read<W: Write>(file_path: String, mut output: W) -> Result<(), Box<dyn Error>> {
    let path = Path::new(&file_path);
    let source = Source::File(
        path.file_stem()
            .ok_or("no file name")?
            .to_string_lossy()
            .to_string(),
    );

    let mut content = Vec::new();
    BufReader::new(File::open(file_path)?).read_to_end(&mut content)?;

    let input = String::from_utf8(content)?;

    let mut parser = Parser::new(Lexer::new(&input, source));
    let program = parser.parse_program();

    if parser.is_err() {
        print_parser_errors(&mut output, parser.errors());
        return Err("can't run code because of parsing errors".to_owned().into());
    }

    let mut compiler = Compiler::new();
    compiler.compile(program)?;
    let mut vm = VM::new(compiler.byte_code());
    vm.run()?;
    if !matches!(vm.stack_top(), Object::Null) {
        write_flush(&mut output, format!("{}\n", vm.stack_top()).as_bytes());
    }

    Ok(())
}

// fn run<W: Write>(
//     input: &str,
//     output: &mut W,
//     source: Source,
//     symbol_table: SymbolTable,
//     constants: Vec<Object>,
//     globals: Box<[Object; GLOBAL_SIZE]>,
// ) -> Result<(), Box<dyn Error>> {
//     let mut parser = Parser::new(Lexer::new(input, source));
//     let program = parser.parse_program();

//     if parser.is_err() {
//         print_parser_errors(output, parser.errors());
//         Err("can't run code because of parsing errors".to_owned().into())
//     } else {
//         vm(program, output, symbol_table, constants, globals)
//     }
// }

fn eval_str<W: Write>(input: &str, evaluator: &mut Evaluator, output: &mut W, source: Source) {
    let mut parser = Parser::new(Lexer::new(input, source));
    let program = parser.parse_program();

    if parser.is_err() {
        print_parser_errors(output, parser.errors());
    } else {
        let eval = evaluator.eval_program(program);
        if !matches!(eval, Object::Null) {
            write_flush(output, format!("{eval}\n").as_bytes());
        }
    }
}

// fn vm<W: Write>(
//     program: Program,
//     output: &mut W,
//     symbol_table: SymbolTable,
//     constants: Vec<Object>,
//     globals: Box<[Object; GLOBAL_SIZE]>,
// ) -> Result<(), Box<dyn Error>> {
//     let mut compiler = Compiler::new_with_state(symbol_table, constants);
//     compiler.compile(program)?;
//     let mut vm = VM::new_with_globals_store(compiler.byte_code(), globals);
//     vm.run()?;
//     if !matches!(vm.stack_top(), Object::Null) {
//         write_flush(output, format!("{}\n", vm.stack_top()).as_bytes());
//     }
//     Ok(())
// }

fn print_parser_errors<W: Write>(output: &mut W, errors: &[String]) {
    write_flush(output, b"Parser errors:\n");
    for error in errors {
        write_flush(output, format!("\t{error}\n").as_bytes());
    }
}

fn write_flush<W: Write>(output: &mut W, buf: &[u8]) {
    output.write_all(buf).expect("Error writing to output");
    output.flush().expect("Error flushing output");
}
