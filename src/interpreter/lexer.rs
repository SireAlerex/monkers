use std::{char, iter::Peekable, rc::Rc, str::CharIndices};

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
                    Token::new(TokenKind::EQ, data.0, data.1, Rc::clone(&data.2))
                } else {
                    token!(data, TokenKind::Assign)
                }
            }
            Some((_, '!')) => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    token!(data, TokenKind::NotEQ)
                } else {
                    token!(data, TokenKind::Bang)
                }
            }
            Some((_, ';')) => token!(data, TokenKind::Semicolon),
            Some((_, '/')) => token!(data, TokenKind::Slash),
            Some((_, '*')) => token!(data, TokenKind::Asterisk),
            Some((_, '<')) => token!(data, TokenKind::LT),
            Some((_, '>')) => token!(data, TokenKind::GT),
            Some((_, '(')) => token!(data, TokenKind::LParen),
            Some((_, ')')) => token!(data, TokenKind::RParen),
            Some((_, ',')) => token!(data, TokenKind::Comma),
            Some((_, '+')) => token!(data, TokenKind::Plus),
            Some((_, '-')) => token!(data, TokenKind::Minus),
            Some((_, '{')) => token!(data, TokenKind::LBrace),
            Some((_, '}')) => token!(data, TokenKind::RBrace),
            Some((_, '[')) => token!(data, TokenKind::LBracket),
            Some((_, ']')) => token!(data, TokenKind::RBracket),
            Some((_, '"')) => {
                if let Some(s) = self.read_string() {
                    token!(data, TokenKind::String(s))
                } else {
                    token!(data, TokenKind::Illegal)
                }
            }
            Some((_, s)) => {
                if s.is_ascii_digit() {
                    if let Some(int) = self.read_number() {
                        return token!(data, TokenKind::Int(int));
                    }
                    println!("illegal token from read_number");
                    token!(data, TokenKind::Illegal)
                } else if Self::is_letter(s) {
                    if let Some(ss) = self.read_ident() {
                        let t = match ss {
                            "fn" => token!(data, TokenKind::Key(Keyword::Function)),
                            "let" => token!(data, TokenKind::Key(Keyword::Let)),
                            "true" => token!(data, TokenKind::Key(Keyword::True)),
                            "false" => token!(data, TokenKind::Key(Keyword::False)),
                            "if" => token!(data, TokenKind::Key(Keyword::If)),
                            "else" => token!(data, TokenKind::Key(Keyword::Else)),
                            "return" => token!(data, TokenKind::Key(Keyword::Return)),
                            _ => token!(data, TokenKind::Identifier(String::from(ss))),
                        };
                        return t;
                    }
                    println!("illegal token from read_ident");
                    token!(data, TokenKind::Illegal)
                } else {
                    println!("illegal token from char");
                    token!(data, TokenKind::Illegal)
                }
            }
            None => token!(data, TokenKind::EOF),
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
        10 != 9;
        \"foobar\"
        \"foo\\t bar\"
        \"foo\\nbar\"
        [1, 2];
        ";

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
            TokenKind::String(String::from("foobar")),
            TokenKind::String(String::from("foo\t bar")),
            TokenKind::String(String::from("foo\nbar")),
            TokenKind::LBracket,
            TokenKind::Int(1),
            TokenKind::Comma,
            TokenKind::Int(2),
            TokenKind::RBracket,
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
