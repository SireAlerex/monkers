use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Token {
    pub kind: TokenKind,
    line: usize,
    column: usize,
    source: Rc<Source>,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize, column: usize, source: Rc<Source>) -> Self {
        Token {
            kind,
            line,
            column,
            source,
        }
    }

    pub fn illegal() -> Self {
        Token {
            kind: TokenKind::Illegal,
            line: 0,
            column: 0,
            source: Rc::new(Source::Repl),
        }
    }

    pub fn ident(&self) -> Option<String> {
        match &self.kind {
            TokenKind::Identifier(s) => Some(s.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Source {
    // File(String),
    Repl,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum TokenKind {
    Illegal,
    EOF,
    Identifier(String),
    Int(i64),
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    EQ,
    NotEQ,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Key(Keyword),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum Keyword {
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
