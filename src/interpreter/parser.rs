use std::collections::HashMap;

use super::{
    ast::{Expr, Ident, InfixParseFn, Operator, PrefixParseFn, Program, Stmt},
    lexer::Lexer,
    token::{Keyword, Token, TokenKind},
};

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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_fns: HashMap<TokenKind, PrefixParseFn>,
    infix_fns: HashMap<TokenKind, InfixParseFn>,
}

fn precedence(kind: &TokenKind) -> Precedence {
    match kind {
        TokenKind::EQ => Precedence::Equals,
        TokenKind::NotEQ => Precedence::Equals,
        TokenKind::LT => Precedence::LessGreater,
        TokenKind::GT => Precedence::LessGreater,
        TokenKind::Plus => Precedence::Sum,
        TokenKind::Minus => Precedence::Sum,
        TokenKind::Slash => Precedence::Product,
        TokenKind::Asterisk => Precedence::Product,
        _ => Precedence::Lowest,
    }
}

fn parse_ident(parser: &mut Parser) -> Expr {
    if let TokenKind::Identifier(s) = &parser.cur_token.kind {
        Expr::Ident(s.clone())
    } else {
        panic!("should never happen")
    }
}

fn parse_int_literal(parser: &mut Parser) -> Expr {
    if let TokenKind::Int(x) = &parser.cur_token.kind {
        Expr::IntLiteral(*x)
    } else {
        panic!("should never happen")
    }
}

fn parse_prefix(parser: &mut Parser) -> Expr {
    let op = match parser.cur_token.kind {
        TokenKind::Minus => Operator::Minus,
        TokenKind::Bang => Operator::Bang,
        _ => panic!("prefix op should never happen"),
    };

    parser.next_token();

    let right = parser
        .parse_expression(Precedence::Prefix)
        .expect("parse prefix right expr error");

    Expr::Prefix(op, Box::new(right))
}

fn parse_infix(parser: &mut Parser, left: Expr) -> Expr {
    let op = match parser.cur_token.kind {
        TokenKind::Plus => Operator::Plus,
        TokenKind::Minus => Operator::Minus,
        TokenKind::Asterisk => Operator::Asterisk,
        TokenKind::Slash => Operator::Slash,
        TokenKind::GT => Operator::Greater,
        TokenKind::LT => Operator::Less,
        TokenKind::EQ => Operator::Equal,
        TokenKind::NotEQ => Operator::NotEqual,
        _ => panic!("infix op should never happen"),
    };

    let precedence = parser.cur_precedence();
    parser.next_token();
    let right = parser
        .parse_expression(precedence)
        .expect("parse infix right expr error");

    Expr::Infix(Box::new(left), op, Box::new(right))
}

impl<'a, 'b> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: Token::illegal(),
            peek_token: Token::illegal(),
            errors: Vec::new(),
            prefix_fns: HashMap::new(),
            infix_fns: HashMap::new(),
        };

        // Prefix functions
        parser
            .prefix_fns
            .insert(TokenKind::Identifier("_".to_owned()), parse_ident);
        parser
            .prefix_fns
            .insert(TokenKind::Int(0), parse_int_literal);
        parser.prefix_fns.insert(TokenKind::Bang, parse_prefix);
        parser.prefix_fns.insert(TokenKind::Minus, parse_prefix);

        // Infix functions
        parser.infix_fns.insert(TokenKind::Plus, parse_infix);
        parser.infix_fns.insert(TokenKind::Minus, parse_infix);
        parser.infix_fns.insert(TokenKind::Asterisk, parse_infix);
        parser.infix_fns.insert(TokenKind::Slash, parse_infix);
        parser.infix_fns.insert(TokenKind::GT, parse_infix);
        parser.infix_fns.insert(TokenKind::LT, parse_infix);
        parser.infix_fns.insert(TokenKind::EQ, parse_infix);
        parser.infix_fns.insert(TokenKind::NotEQ, parse_infix);

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        // TODO: remove clone (change Token ? use iter over tokens ?)
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program(Vec::new());

        while self.cur_token.kind != TokenKind::EOF {
            if let Some(stmt) = self.parser_stmt() {
                program.0.push(stmt);
            }
            self.next_token();
        }

        program
    }

    fn parser_stmt(&mut self) -> Option<Stmt> {
        match self.cur_token.kind {
            TokenKind::Key(Keyword::Let) => self.parse_let_stmt(),
            TokenKind::Key(Keyword::Return) => self.parse_return_stmt(),
            _ => self.parse_expression_stmt(),
        }
    }

    fn parse_expression_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Stmt::Expr(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expr> {
        if let Some(prefix) = Self::get_fn(&self.prefix_fns, &self.cur_token.kind) {
            let mut left = prefix(self);

            while !self.peek_token_is(&TokenKind::Semicolon) && precedence < self.peek_precedence()
            {
                // TODO: get rid of clone
                let map = self.infix_fns.clone();
                if let Some(infix) = Self::get_fn(&map, &self.peek_token.kind) {
                    self.next_token();
                    left = infix(self, left);
                } else {
                    return Some(left);
                }
            }

            Some(left)
        } else {
            self.no_prefix_error();
            None
        }
    }

    fn no_prefix_error(&mut self) {
        self.errors.push(format!(
            "no prefix parse function for {:?} found",
            self.cur_token.kind
        ));
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

        Some(Stmt::Let(name, Expr::Temp))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.next_token();

        // TODO: skipping expr for now
        while !self.cur_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Stmt::Return(Expr::Temp))
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

    fn peek_error(&mut self, kind: TokenKind) {
        self.errors.push(format!(
            "expected next token to be {kind:?}, got {:?} instead",
            self.peek_token.kind
        ))
    }

    // Standardized enums for the ones that can hold data to find them in hash
    fn get_fn<T>(map: &'b HashMap<TokenKind, T>, kind: &TokenKind) -> Option<&'b T> {
        match kind {
            TokenKind::Identifier(_) => map.get(&TokenKind::Identifier("_".to_owned())),
            TokenKind::Int(_) => map.get(&TokenKind::Int(0)),
            _ => map.get(kind),
        }
    }

    fn peek_precedence(&self) -> Precedence {
        precedence(&self.peek_token.kind)
    }

    fn cur_precedence(&self) -> Precedence {
        precedence(&self.cur_token.kind)
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::{
        ast::{Expr, Operator, Stmt},
        lexer::Lexer,
        parser::Parser,
        token::Source,
    };

    fn check_parser_errors(parser: Parser) -> bool {
        let errors = parser.errors;

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
    fn op_precedence_test() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for test in tests {
            let mut parser = Parser::new(Lexer::new(test.0, Source::Repl));
            let program = parser.parse_program();
            assert!(check_parser_errors(parser));
            assert_eq!(program.to_string().replace('\n', ""), test.1);
        }
    }

    #[test]
    fn infix_expr_test() {
        let inputs = [
            "5 + 5", "5 - 5", "5 * 5", "5 / 5", "5 > 5", "5 < 5", "5 == 5", "5 != 5",
        ];

        let tests = vec![
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::IntLiteral(5)),
                Operator::Plus,
                Box::new(Expr::IntLiteral(5)),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::IntLiteral(5)),
                Operator::Minus,
                Box::new(Expr::IntLiteral(5)),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::IntLiteral(5)),
                Operator::Asterisk,
                Box::new(Expr::IntLiteral(5)),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::IntLiteral(5)),
                Operator::Slash,
                Box::new(Expr::IntLiteral(5)),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::IntLiteral(5)),
                Operator::Greater,
                Box::new(Expr::IntLiteral(5)),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::IntLiteral(5)),
                Operator::Less,
                Box::new(Expr::IntLiteral(5)),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::IntLiteral(5)),
                Operator::Equal,
                Box::new(Expr::IntLiteral(5)),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::IntLiteral(5)),
                Operator::NotEqual,
                Box::new(Expr::IntLiteral(5)),
            )),
        ];

        for (input, expected) in inputs.iter().zip(tests) {
            let mut parser = Parser::new(Lexer::new(input, Source::Repl));
            let program = parser.parse_program();

            assert!(check_parser_errors(parser));
            // assert_eq!(program.0.len(), 1);
            assert_eq!(*program.0.first().unwrap(), expected);
        }
    }

    #[test]
    fn prefix_expr_test() {
        let inputs = ["!5", "-15"];

        let tests = vec![
            Stmt::Expr(Expr::Prefix(Operator::Bang, Box::new(Expr::IntLiteral(5)))),
            Stmt::Expr(Expr::Prefix(
                Operator::Minus,
                Box::new(Expr::IntLiteral(15)),
            )),
        ];

        for (input, expected) in inputs.iter().zip(tests) {
            let mut parser = Parser::new(Lexer::new(input, Source::Repl));
            let program = parser.parse_program();

            assert!(check_parser_errors(parser));
            assert_eq!(program.0.len(), 1);
            assert_eq!(*program.0.first().unwrap(), expected);
        }
    }

    #[test]
    fn ident_expr_test() {
        let input = "foobar; \
        var;";

        let lexer = Lexer::new(input, Source::Repl);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert!(check_parser_errors(parser));
        assert_eq!(program.0.len(), 2);

        let tests = vec![
            Stmt::Expr(Expr::Ident("foobar".to_owned())),
            Stmt::Expr(Expr::Ident("var".to_owned())),
        ];

        for (res, expected) in program.0.iter().zip(tests) {
            assert_eq!(*res, expected);
        }
    }

    #[test]
    fn int_literal_expr_test() {
        let input = "5; \
        65987314;";

        let lexer = Lexer::new(input, Source::Repl);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert!(check_parser_errors(parser));
        assert_eq!(program.0.len(), 2);

        let tests = vec![
            Stmt::Expr(Expr::IntLiteral(5)),
            Stmt::Expr(Expr::IntLiteral(65987314)),
        ];

        for (res, expected) in program.0.iter().zip(tests) {
            assert_eq!(*res, expected);
        }
    }

    #[test]
    fn let_stmt_test() {
        let input = "let x = 5; \
        let y = 10; \
        let foobar = 838383;";

        let lexer = Lexer::new(input, Source::Repl);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert!(check_parser_errors(parser));
        assert_eq!(program.0.len(), 3);

        let tests = vec![
            Stmt::Let("x".to_owned(), Expr::Temp),
            Stmt::Let("y".to_owned(), Expr::Temp),
            Stmt::Let("foobar".to_owned(), Expr::Temp),
        ];
        for (res, expected) in program.0.iter().zip(tests) {
            assert_eq!(*res, expected);
        }
    }

    #[test]
    fn return_stmt_test() {
        let input = "return 5; \
        return 10; \
        return 993322;";

        let lexer = Lexer::new(input, Source::Repl);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert!(check_parser_errors(parser));
        assert_eq!(program.0.len(), 3);

        let tests = vec![
            Stmt::Return(Expr::Temp),
            Stmt::Return(Expr::Temp),
            Stmt::Return(Expr::Temp),
        ];
        for (res, expected) in program.0.iter().zip(tests) {
            assert_eq!(*res, expected);
        }
    }
}
