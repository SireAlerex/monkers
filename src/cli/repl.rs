use std::io::{BufRead, Write};

use crate::interpreter::{lexer::Lexer, parser::Parser, token::Source};

pub fn start<R: BufRead, W: Write>(input: R, mut output: W) {
    write_flush(&mut output, b">> ");
    for line in input.lines().map_while(Result::ok) {
        let mut parser = Parser::new(Lexer::new(&line, Source::Repl));
        let program = parser.parse_program();

        if parser.errors().is_empty() {
            print_parser_errors(&mut output, parser.errors());
            write_flush(&mut output, b">> ");
            continue;
        }

        write_flush(&mut output, format!("{program}\n").as_bytes());

        write_flush(&mut output, b">> ");
    }
}

fn print_parser_errors<W: Write>(output: &mut W, errors: &[String]) {
    for error in errors {
        write_flush(output, format!("Parser errors:\n\t{error}\n").as_bytes());
    }
}

fn write_flush<W: Write>(output: &mut W, buf: &[u8]) {
    output.write_all(buf).expect("Error writing to output");
    output.flush().expect("Error flushing output");
}
