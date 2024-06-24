use std::{
    error::Error,
    fs::File,
    io::{BufRead, BufReader, Read, Write},
    path::Path,
};

use crate::interpreter::{
    evaluator::{object::Object, Evaluator},
    lexer::Lexer,
    parser::Parser,
    token::Source,
};

pub fn start<R: BufRead, W: Write>(input: R, mut output: W) {
    let mut evaluator = Evaluator::new();

    write_flush(&mut output, b">> ");
    for line in input.lines().map_while(Result::ok) {
        eval_str(&line, &mut evaluator, &mut output, Source::Repl);
        write_flush(&mut output, b">> ");
    }
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
    let mut evaluator = Evaluator::new();
    eval_str(&input, &mut evaluator, &mut output, source);

    Ok(())
}

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
