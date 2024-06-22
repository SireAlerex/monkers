use std::{char, iter::Peekable, str::CharIndices};

use crate::interpreter::token::{Keyword, Source, Token, TokenKind};

macro_rules! token {
    ($data: ident, $kind: expr) => {
        Token::new($kind, $data.0, $data.1, $data.2)
    };
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    ch: Option<(usize, char)>,
    source: Source,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, source: Source) -> Self {
        let mut chars = input.char_indices().peekable();
        let ch = chars.next();
        Lexer {
            input,
            chars,
            ch,
            source,
            line: 1,
            column: 1,
        }
    }

    fn read_char(&mut self) {
        self.column += 1;
        self.ch = self.chars.next();
    }

    pub fn next_token(&mut self) -> Token {
        let tok: Token;
        self.skip_white_space();
        let x = (self.line, self.column, self.source.clone());

        match self.ch {
            Some((_, '=')) => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    tok = token!(x, TokenKind::EQ)
                } else {
                    tok = token!(x, TokenKind::Assign)
                }
            }
            Some((_, '!')) => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    tok = token!(x, TokenKind::NotEQ)
                } else {
                    tok = token!(x, TokenKind::Bang)
                }
            }
            Some((_, ';')) => tok = token!(x, TokenKind::Semicolon),
            Some((_, '/')) => tok = token!(x, TokenKind::Slash),
            Some((_, '*')) => tok = token!(x, TokenKind::Asterisk),
            Some((_, '<')) => tok = token!(x, TokenKind::LT),
            Some((_, '>')) => tok = token!(x, TokenKind::GT),
            Some((_, '(')) => tok = token!(x, TokenKind::LParen),
            Some((_, ')')) => tok = token!(x, TokenKind::RParen),
            Some((_, ',')) => tok = token!(x, TokenKind::Comma),
            Some((_, '+')) => tok = token!(x, TokenKind::Plus),
            Some((_, '-')) => tok = token!(x, TokenKind::Minus),
            Some((_, '{')) => tok = token!(x, TokenKind::LBrace),
            Some((_, '}')) => tok = token!(x, TokenKind::RBrace),
            Some((_, s)) => {
                if s.is_ascii_digit() {
                    if let Some(int) = self.read_number() {
                        return token!(x, TokenKind::Int(int));
                    }
                    println!("illegal token from read_number");
                    tok = token!(x, TokenKind::Illegal);
                } else if Self::is_letter(s) {
                    if let Some(ss) = self.read_ident() {
                        tok = match ss {
                            "fn" => token!(x, TokenKind::Key(Keyword::Function)),
                            "let" => token!(x, TokenKind::Key(Keyword::Let)),
                            "true" => token!(x, TokenKind::Key(Keyword::True)),
                            "false" => token!(x, TokenKind::Key(Keyword::False)),
                            "if" => token!(x, TokenKind::Key(Keyword::If)),
                            "else" => token!(x, TokenKind::Key(Keyword::Else)),
                            "return" => token!(x, TokenKind::Key(Keyword::Return)),
                            _ => token!(x, TokenKind::Identifier(String::from(ss))),
                        };
                        return tok;
                    }
                    tok = token!(x, TokenKind::Illegal);
                    println!("illegal token from read_ident");
                } else {
                    tok = token!(x, TokenKind::Illegal);
                    println!("illegal token from char");
                }
            }
            None => tok = token!(x, TokenKind::EOF),
        }

        self.read_char();
        tok
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, ch)| *ch)
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn read<F>(&mut self, mut check: F) -> Option<&str>
    where
        F: Fn(char) -> bool,
    {
        let start = self.ch?.0;
        let mut end = start;

        while self.read_check(&mut check) {
            self.read_char();
            end += 1;
        }
        let s = self.input.get(start..end)?;
        Some(s)
    }

    fn read_check<F>(&self, check: &mut F) -> bool
    where
        F: Fn(char) -> bool,
    {
        if let Some((_, c)) = self.ch {
            check(c)
        } else {
            false
        }
    }

    fn read_ident(&mut self) -> Option<&str> {
        self.read(Self::is_letter)
    }

    fn read_number(&mut self) -> Option<i64> {
        let s = self.read(|ch| ch.is_ascii_digit())?;
        s.parse::<i64>().ok()
    }

    fn skip_white_space(&mut self) {
        let mut ch;
        loop {
            ch = if self.ch.is_some() {
                self.ch.unwrap().1
            } else {
                return;
            };
            if ch == '\n' {
                self.read_char();
                self.line += 1;
                self.column = 0;
            } else if ch == ' ' || ch == '\t' || ch == '\r' {
                self.read_char();
            } else {
                break;
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::token::{Keyword, Source, TokenKind};

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;
        
        let add = fn(x, y) {
          x + y;
        };
        
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        
        10 == 10;
        10 != 9;";

        let expected_tokens = [
            TokenKind::Key(Keyword::Let),
            TokenKind::Identifier(String::from("five")),
            TokenKind::Assign,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::Key(Keyword::Let),
            TokenKind::Identifier(String::from("ten")),
            TokenKind::Assign,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Key(Keyword::Let),
            TokenKind::Identifier(String::from("add")),
            TokenKind::Assign,
            TokenKind::Key(Keyword::Function),
            TokenKind::LParen,
            TokenKind::Identifier(String::from("x")),
            TokenKind::Comma,
            TokenKind::Identifier(String::from("y")),
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Identifier(String::from("x")),
            TokenKind::Plus,
            TokenKind::Identifier(String::from("y")),
            TokenKind::Semicolon,
            TokenKind::RBrace,
            TokenKind::Semicolon,
            TokenKind::Key(Keyword::Let),
            TokenKind::Identifier(String::from("result")),
            TokenKind::Assign,
            TokenKind::Identifier(String::from("add")),
            TokenKind::LParen,
            TokenKind::Identifier(String::from("five")),
            TokenKind::Comma,
            TokenKind::Identifier(String::from("ten")),
            TokenKind::RParen,
            TokenKind::Semicolon,
            TokenKind::Bang,
            TokenKind::Minus,
            TokenKind::Slash,
            TokenKind::Asterisk,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::Int(5),
            TokenKind::LT,
            TokenKind::Int(10),
            TokenKind::GT,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::Key(Keyword::If),
            TokenKind::LParen,
            TokenKind::Int(5),
            TokenKind::LT,
            TokenKind::Int(10),
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Key(Keyword::Return),
            TokenKind::Key(Keyword::True),
            TokenKind::Semicolon,
            TokenKind::RBrace,
            TokenKind::Key(Keyword::Else),
            TokenKind::LBrace,
            TokenKind::Key(Keyword::Return),
            TokenKind::Key(Keyword::False),
            TokenKind::Semicolon,
            TokenKind::RBrace,
            TokenKind::Int(10),
            TokenKind::EQ,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Int(10),
            TokenKind::NotEQ,
            TokenKind::Int(9),
            TokenKind::Semicolon,
            TokenKind::EOF,
        ];

        let mut l = Lexer::new(input, Source::Repl);

        for i in 0..expected_tokens.len() {
            let tok = l.next_token();

            assert_eq!(&tok.kind, expected_tokens.get(i).unwrap());
        }
    }
}
