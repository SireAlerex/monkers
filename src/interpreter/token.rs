#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Token {
    pub kind: TokenKind,
    line: usize,
    column: usize,
    source: Source,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize, column: usize, source: Source) -> Self {
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
            source: Source::REPL,
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
    File(String),
    REPL,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TokenKind {
    Illegal,
    EOF,
    Identifier(String),
    Int(u64),
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
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Keyword {
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
