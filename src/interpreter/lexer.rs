use std::{char, iter::Peekable, rc::Rc, str::CharIndices};

use crate::interpreter::token::{Keyword, Kind, Source, Token};

macro_rules! token {
    ($data: ident, $kind: expr) => {
        Token::new($kind, $data.0, $data.1, $data.2)
    };
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    ch: Option<(usize, char)>,
    source: Rc<Source>,
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
            source: Rc::new(source),
            line: 1,
            column: 1,
        }
    }

    fn read_char(&mut self) {
        self.column += 1;
        self.ch = self.chars.next();
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_white_space();
        let data = (self.line, self.column, self.source.clone());

        let tok = match self.ch {
            Some((_, '=')) => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    token!(data, Kind::EQ)
                } else {
                    token!(data, Kind::Assign)
                }
            }
            Some((_, '!')) => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    token!(data, Kind::NotEQ)
                } else {
                    token!(data, Kind::Bang)
                }
            }
            Some((_, ';')) => token!(data, Kind::Semicolon),
            Some((_, '/')) => token!(data, Kind::Slash),
            Some((_, '*')) => token!(data, Kind::Asterisk),
            Some((_, '<')) => token!(data, Kind::LT),
            Some((_, '>')) => token!(data, Kind::GT),
            Some((_, '(')) => token!(data, Kind::LParen),
            Some((_, ')')) => token!(data, Kind::RParen),
            Some((_, ',')) => token!(data, Kind::Comma),
            Some((_, '+')) => token!(data, Kind::Plus),
            Some((_, '-')) => token!(data, Kind::Minus),
            Some((_, '{')) => token!(data, Kind::LBrace),
            Some((_, '}')) => token!(data, Kind::RBrace),
            Some((_, '[')) => token!(data, Kind::LBracket),
            Some((_, ']')) => token!(data, Kind::RBracket),
            Some((_, ':')) => token!(data, Kind::Colon),
            Some((_, '"')) => {
                if let Some(s) = self.read_string() {
                    token!(data, Kind::String(s))
                } else {
                    token!(data, Kind::Illegal)
                }
            }
            Some((_, s)) => {
                if s.is_ascii_digit() {
                    if let Some(int) = self.read_number() {
                        return token!(data, Kind::Int(int));
                    }
                    println!("illegal token from read_number");
                    token!(data, Kind::Illegal)
                } else if Self::is_letter(s) {
                    if let Some(ss) = self.read_ident() {
                        let t = match ss {
                            "fn" => token!(data, Kind::Key(Keyword::Function)),
                            "let" => token!(data, Kind::Key(Keyword::Let)),
                            "true" => token!(data, Kind::Key(Keyword::True)),
                            "false" => token!(data, Kind::Key(Keyword::False)),
                            "if" => token!(data, Kind::Key(Keyword::If)),
                            "else" => token!(data, Kind::Key(Keyword::Else)),
                            "return" => token!(data, Kind::Key(Keyword::Return)),
                            _ => token!(data, Kind::Identifier(String::from(ss))),
                        };
                        return t;
                    }
                    println!("illegal token from read_ident");
                    token!(data, Kind::Illegal)
                } else {
                    println!("illegal token from char");
                    token!(data, Kind::Illegal)
                }
            }
            None => token!(data, Kind::EOF),
        };

        self.read_char();
        tok
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, ch)| *ch)
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn read<F>(&mut self, check: F) -> Option<&str>
    where
        F: Fn(char) -> bool,
    {
        let start = self.ch?.0;
        let mut end = start;

        while self.read_check(&check) {
            self.read_char();
            end += 1;
        }
        let s = self.input.get(start..end)?;
        Some(s)
    }

    fn read_check<F>(&self, check: &F) -> bool
    where
        F: Fn(char) -> bool,
    {
        // dbg!(self.ch);
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
        let s = self.read(|c| c.is_ascii_digit())?;
        s.parse::<i64>().ok()
    }

    fn read_string(&mut self) -> Option<String> {
        self.read_char();

        self.read(|c| c != '"')
            .map(|s| s.to_owned().replace("\\t", "\t").replace("\\n", "\n"))
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
    use crate::interpreter::token::{Keyword, Kind, Source};

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
        10 != 9;
        \"foobar\"
        \"foo\\t bar\"
        \"foo\\nbar\"
        [1, 2];
        {\"foo\": \"bar\"}";

        let expected_tokens = [
            Kind::Key(Keyword::Let),
            Kind::Identifier(String::from("five")),
            Kind::Assign,
            Kind::Int(5),
            Kind::Semicolon,
            Kind::Key(Keyword::Let),
            Kind::Identifier(String::from("ten")),
            Kind::Assign,
            Kind::Int(10),
            Kind::Semicolon,
            Kind::Key(Keyword::Let),
            Kind::Identifier(String::from("add")),
            Kind::Assign,
            Kind::Key(Keyword::Function),
            Kind::LParen,
            Kind::Identifier(String::from("x")),
            Kind::Comma,
            Kind::Identifier(String::from("y")),
            Kind::RParen,
            Kind::LBrace,
            Kind::Identifier(String::from("x")),
            Kind::Plus,
            Kind::Identifier(String::from("y")),
            Kind::Semicolon,
            Kind::RBrace,
            Kind::Semicolon,
            Kind::Key(Keyword::Let),
            Kind::Identifier(String::from("result")),
            Kind::Assign,
            Kind::Identifier(String::from("add")),
            Kind::LParen,
            Kind::Identifier(String::from("five")),
            Kind::Comma,
            Kind::Identifier(String::from("ten")),
            Kind::RParen,
            Kind::Semicolon,
            Kind::Bang,
            Kind::Minus,
            Kind::Slash,
            Kind::Asterisk,
            Kind::Int(5),
            Kind::Semicolon,
            Kind::Int(5),
            Kind::LT,
            Kind::Int(10),
            Kind::GT,
            Kind::Int(5),
            Kind::Semicolon,
            Kind::Key(Keyword::If),
            Kind::LParen,
            Kind::Int(5),
            Kind::LT,
            Kind::Int(10),
            Kind::RParen,
            Kind::LBrace,
            Kind::Key(Keyword::Return),
            Kind::Key(Keyword::True),
            Kind::Semicolon,
            Kind::RBrace,
            Kind::Key(Keyword::Else),
            Kind::LBrace,
            Kind::Key(Keyword::Return),
            Kind::Key(Keyword::False),
            Kind::Semicolon,
            Kind::RBrace,
            Kind::Int(10),
            Kind::EQ,
            Kind::Int(10),
            Kind::Semicolon,
            Kind::Int(10),
            Kind::NotEQ,
            Kind::Int(9),
            Kind::Semicolon,
            Kind::String(String::from("foobar")),
            Kind::String(String::from("foo\t bar")),
            Kind::String(String::from("foo\nbar")),
            Kind::LBracket,
            Kind::Int(1),
            Kind::Comma,
            Kind::Int(2),
            Kind::RBracket,
            Kind::Semicolon,
            Kind::LBrace,
            Kind::String(String::from("foo")),
            Kind::Colon,
            Kind::String(String::from("bar")),
            Kind::RBrace,
            Kind::EOF,
        ];

        let mut l = Lexer::new(input, Source::Repl);

        for i in 0..expected_tokens.len() {
            let tok = l.next_token();
            assert_eq!(&tok.kind, expected_tokens.get(i).unwrap());
        }
    }
}
