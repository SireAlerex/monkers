use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
pub struct Token {
    pub kind: Kind,
    pub line: usize,
    pub column: usize,
    pub source: Rc<Source>,
}

impl Token {
    pub fn new(kind: Kind, line: usize, column: usize, source: Rc<Source>) -> Self {
        Self {
            kind,
            line,
            column,
            source,
        }
    }

    pub fn illegal() -> Self {
        Self {
            kind: Kind::Illegal,
            line: 0,
            column: 0,
            source: Rc::new(Source::Repl),
        }
    }

    pub fn ident(&self) -> Option<String> {
        match &self.kind {
            Kind::Identifier(s) => Some(s.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
pub enum Source {
    File(String),
    Repl,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Kind {
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
    LBracket,
    RBracket,
    Colon,
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
