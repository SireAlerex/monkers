use std::io::{BufRead, Write};

use crate::interpreter::{
    lexer::Lexer,
    token::{Source, TokenKind},
};

pub fn start<R: BufRead, W: Write>(input: R, mut output: W) {
    write_flush(&mut output, b">> ");
    for res in input.lines() {
        if let Ok(line) = res {
            let mut lexer = Lexer::new(&line, Source::REPL);
            loop {
                let token = lexer.next_token();
                if token.kind == TokenKind::EOF {
                    break;
                }
                println!("{token:?}");
            }
            write_flush(&mut output, b">> ");
        }
    }
}

fn write_flush<W: Write>(output: &mut W, buf: &[u8]) {
    output.write(buf).expect("Error writing to output");
    output.flush().expect("Error flushing output");
}
