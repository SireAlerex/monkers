use std::{
    cell::RefCell,
    error::Error,
    fs::File,
    io::{BufRead, BufReader, Read, Write},
    path::Path,
    rc::Rc, time::Instant,
};

use crate::{
    compiler::{
        symbol_table::SymbolTable,
        vm::{GLOBAL_SIZE, VM},
        Compiler,
    }, interpreter::{
        ast::Program, evaluator::{
            object::{Object, UNINT_OBJECT},
            Evaluator,
        }, lexer::Lexer, parser::Parser, token::Source
    }, utils
};

enum ReturnValue {
    Object(Object),
    ObjectAndData(Object, Compiler, VM)
}

pub fn start<R: BufRead, W: Write>(input: R, mut output: W, tree_walk: bool, debug: bool) -> Result<Object, Box<dyn Error>> {
    let mut symbol_table = Rc::new(RefCell::new(SymbolTable::new_with_builtins()));
    let mut constants: Vec<Object> = Vec::new();
    let mut globals = (vec![UNINT_OBJECT; GLOBAL_SIZE]).into_boxed_slice();
    let mut last_obj = UNINT_OBJECT;

    if tree_walk {
        write_flush(&mut output, b"Tree-walk interpreter:\n");
    } else {
        write_flush(&mut output, b"Compiling for the VM:\n");
    }

    write_flush(&mut output, b">> ");
    for line in input.lines().map_while(Result::ok) {
        if tree_walk {
            last_obj = return_value_into_object(eval_str(&line, &mut Evaluator::new(), &mut output, Source::Repl, debug))?;
        } else {
            (last_obj, globals, (symbol_table, constants)) =  match vm(&line, &mut output, symbol_table, constants, globals, Source::Repl, debug)? {
                ReturnValue::Object(_) => panic!("vm should return data"),
                ReturnValue::ObjectAndData(obj, compiler, vm) => (obj, vm.globals(), compiler.consume()),
            };
        }       

        write_flush(&mut output, b">> ");
    }
    Ok(last_obj)
}

pub fn read<W: Write>(file_path: String, mut output: W, tree_walk: bool, debug: bool) -> Result<Object, Box<dyn Error>> {
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

    let return_value = if tree_walk {
        eval_str(&input, &mut Evaluator::new(), &mut output, source, debug)
    } else {
        vm(&input, &mut output, Rc::new(RefCell::new(SymbolTable::new_with_builtins())), Vec::new(), (vec![UNINT_OBJECT; GLOBAL_SIZE]).into_boxed_slice(), source, debug)
    };
    return_value_into_object(return_value)
}

fn eval_str<W: Write>(input: &str, evaluator: &mut Evaluator, output: &mut W, source: Source, debug: bool) -> Result<ReturnValue, Box<dyn Error>> {
    let program = parse(input, source, output, debug)?;

    let eval = if debug {
        let start = Instant::now();
        let eval = evaluator.eval_program(program);
        let end = start.elapsed();
        println!("Evaluating execution time: {}", utils::fmt_duration(end));
        eval
    } else {
        evaluator.eval_program(program)
    };

    if !matches!(eval, Object::Null) {
        write_flush(output, format!("{eval}\n").as_bytes());
    }
    Ok(ReturnValue::Object(eval))
}

fn vm<W: Write>(
    input: &str,
    output: &mut W,
    symbol_table: Rc<RefCell<SymbolTable>>,
    constants: Vec<Object>,
    globals: Box<[Object]>,
    source: Source,
    debug: bool
) -> Result<ReturnValue, Box<dyn Error>> {
    let program = parse(input, source, output, debug)?;

    let mut compiler = Compiler::new_with_state(&symbol_table, constants);
    if debug {
        let start = Instant::now();
        compiler.compile(program)?;
        let end = start.elapsed();
        println!("Compiling execution time: {}", utils::fmt_duration(end));
    } else {
        compiler.compile(program)?;
    }

    let mut vm = VM::new_with_globals_store(compiler.byte_code(), globals);
    if debug {
        let start = Instant::now();
        vm.run()?;
        let end = start.elapsed();
        println!("VM execution time: {}", utils::fmt_duration(end));
    } else {
        vm.run()?;
    }

    if !matches!(vm.stack_top(), Object::Null) {
        write_flush(output, format!("{}\n", vm.stack_top()).as_bytes());
    }

    Ok(ReturnValue::ObjectAndData(vm.stack_top(), compiler, vm))
}

fn parse<W: Write>(input: &str, source: Source, output: &mut W, debug: bool) -> Result<Program, String> {
    let mut parser = Parser::new(Lexer::new(&input, source));

    let program = if debug {
        let start = Instant::now();
        let program = parser.parse_program();
        let end = start.elapsed();
        println!("Parsing execution time: {}", utils::fmt_duration(end));
        program
    } else {
        parser.parse_program()
    };

    if parser.is_err() {
        print_parser_errors(output, parser.errors());
        Err("can't run code because of parsing errors".to_string())
    } else {
        Ok(program)
    }
}

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

fn return_value_into_object(value: Result<ReturnValue, Box<dyn Error>>) -> Result<Object, Box<dyn Error>> {
    match value {
        Ok(rv) => match rv {
            ReturnValue::Object(obj) => Ok(obj),
            ReturnValue::ObjectAndData(obj, _, _) => Ok(obj),
        }
        Err(e) => Err(e),
    }
}
