use super::{
    ast::{Expr, Ident, Program, Stmt},
    lexer::{self, Lexer},
    token::{Keyword, Token, TokenKind},
};

macro_rules! cur_token_is {
    ($self: ident, $match: pat) => {
        matches!($self.cur_token.kind, $match)
    };
}

macro_rules! peek_token_is {
    ($self: ident, $match: pat) => {
        matches!($self.peek_token.kind, $match)
    };
}

macro_rules! expect_peek {
    ($self: ident, $match: pat) => {
        if peek_token_is!($self, $match) {
            $self.next_token();
            true
        } else {
            false
        }
    };
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: Token::illegal(),
            peek_token: Token::illegal(),
            errors: Vec::new(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        // TODO: remove clone (change Token ? use iter over tokens ?)
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();

        while self.cur_token.kind != TokenKind::EOF {
            if let Some(stmt) = self.parser_stmt() {
                program.push(stmt);
            }
            self.next_token();
        }

        program
    }

    fn parser_stmt(&mut self) -> Option<Stmt> {
        match self.cur_token.kind {
            TokenKind::Key(Keyword::Let) => self.parse_let_stmt(),
            TokenKind::Key(Keyword::Return) => self.parse_return_stmt(),
            _ => None,
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        if !expect_peek!(self, TokenKind::Identifier(_)) {
            return None;
        }

        let name: Ident = Token::ident(&self.cur_token)?;

        if !self.expect_peek(TokenKind::Assign) {
            return None;
        }

        // TODO: skipping expr for now
        while !self.cur_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Stmt::LetStmt(name, Expr::Temp))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.next_token();

        // TODO: skipping expr for now
        while !self.cur_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Stmt::ReturnStmt(Expr::Temp))
    }

    fn cur_token_is(&self, kind: &TokenKind) -> bool {
        self.cur_token.kind == *kind
    }

    fn peek_token_is(&self, kind: &TokenKind) -> bool {
        self.peek_token.kind == *kind
    }

    fn expect_peek(&mut self, kind: TokenKind) -> bool {
        if self.peek_token_is(&kind) {
            self.next_token();
            true
        } else {
            self.peek_error(kind);
            false
        }
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, kind: TokenKind) {
        self.errors.push(format!(
            "expected next token to be {kind:?}, got {:?} instead",
            self.peek_token.kind
        ))
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::{
        ast::{Expr, Stmt},
        lexer::Lexer,
        parser::Parser,
        token::Source,
    };

    fn check_parser_errors(parser: Parser) -> bool {
        let errors = parser.errors();

        if errors.is_empty() {
            return true;
        }

        println!("parser had {} errors", errors.len());
        for msg in errors {
            println!("parser error: {msg}");
        }

        false
    }

    #[test]
    fn let_stmt_test() {
        let input = "let x = 5; \
        let y = 10; \
        let foobar = 838383;";

        let lexer = Lexer::new(input, Source::REPL);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert!(check_parser_errors(parser));
        assert_eq!(program.len(), 3);

        let tests = vec![
            Stmt::LetStmt("x".to_owned(), Expr::Temp),
            Stmt::LetStmt("y".to_owned(), Expr::Temp),
            Stmt::LetStmt("foobar".to_owned(), Expr::Temp),
        ];
        for (res, expected) in program.iter().zip(tests) {
            assert_eq!(*res, expected);
        }
    }

    #[test]
    fn return_stmt_test() {
        let input = "return 5; \
        return 10; \
        return 993322;";

        let lexer = Lexer::new(input, Source::REPL);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert!(check_parser_errors(parser));
        assert_eq!(program.len(), 3);

        let tests = vec![
            Stmt::ReturnStmt(Expr::Temp),
            Stmt::ReturnStmt(Expr::Temp),
            Stmt::ReturnStmt(Expr::Temp),
        ];
        for (res, expected) in program.iter().zip(tests) {
            assert_eq!(*res, expected);
        }
    }
}
