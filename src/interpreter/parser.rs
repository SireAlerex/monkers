use std::collections::HashMap;

use super::{
    ast::{Block, Expr, Ident, InfixParseFn, Operator, PrefixParseFn, Program, Stmt},
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
        TokenKind::LParen => Precedence::Call,
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

fn parse_boolean(parser: &mut Parser) -> Expr {
    Expr::BooleanLiteral(parser.cur_token_is(&TokenKind::Key(Keyword::True)))
}

fn parse_paren(parser: &mut Parser) -> Expr {
    parser.next_token();

    let expr = parser
        .parse_expression(Precedence::Lowest)
        .expect("parse gouped expr error");

    if !parser.expect_peek(TokenKind::RParen) {
        panic!("paren no RParen");
    }

    expr
}

fn parse_if(parser: &mut Parser) -> Expr {

    parser.next_token();
    let cond = Box::new(
        parser
            .parse_expression(Precedence::Lowest)
            .expect("parse if bad cond"),
    );

    if !parser.expect_peek(TokenKind::LBrace) {
        panic!("if no LBrace");
    }

    let consequence = parser.parse_block();

    let alternative = if parser.peek_token_is(&TokenKind::Key(Keyword::Else)) {
        parser.next_token();

        if !parser.expect_peek(TokenKind::LBrace) {
            panic!("else no LBrace");
        }

        Some(parser.parse_block())
    } else {
        None
    };

    Expr::If {
        cond,
        consequence,
        alternative,
    }
}

fn parse_fn(parser: &mut Parser) -> Expr {
    if !parser.expect_peek(TokenKind::LParen) {
        panic!("fn no LParen");
    }

    let parameters = parser.parse_parameters();

    if !parser.expect_peek(TokenKind::LBrace) {
        panic!("fn no LBrace");
    }

    let body = parser.parse_block();

    Expr::FunctionLiteral { parameters, body }
}

fn parse_call(parser: &mut Parser, function: Expr) -> Expr {
    let arguments = parser.parse_arguments();

    Expr::Call {
        function: Box::new(function),
        arguments,
    }
}

// TODO: function to return None and add an error
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
        parser
            .prefix_fns
            .insert(TokenKind::Key(Keyword::True), parse_boolean);
        parser
            .prefix_fns
            .insert(TokenKind::Key(Keyword::False), parse_boolean);
        parser
            .prefix_fns
            .insert(TokenKind::Key(Keyword::If), parse_if);
        parser
            .prefix_fns
            .insert(TokenKind::Key(Keyword::Function), parse_fn);
        parser.prefix_fns.insert(TokenKind::Bang, parse_prefix);
        parser.prefix_fns.insert(TokenKind::Minus, parse_prefix);
        parser.prefix_fns.insert(TokenKind::LParen, parse_paren);

        // Infix functions
        parser.infix_fns.insert(TokenKind::Plus, parse_infix);
        parser.infix_fns.insert(TokenKind::Minus, parse_infix);
        parser.infix_fns.insert(TokenKind::Asterisk, parse_infix);
        parser.infix_fns.insert(TokenKind::Slash, parse_infix);
        parser.infix_fns.insert(TokenKind::GT, parse_infix);
        parser.infix_fns.insert(TokenKind::LT, parse_infix);
        parser.infix_fns.insert(TokenKind::EQ, parse_infix);
        parser.infix_fns.insert(TokenKind::NotEQ, parse_infix);
        parser.infix_fns.insert(TokenKind::LParen, parse_call);

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
        let mut program = Block(Vec::new());

        while self.cur_token.kind != TokenKind::EOF {
            if let Some(stmt) = self.parse_stmt() {
                program.0.push(stmt);
            }
            self.next_token();
        }

        program
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
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

    fn parse_block(&mut self) -> Block {
        self.next_token();

        let mut stmts = Vec::new();
        while !self.cur_token_is(&TokenKind::RBrace) && !self.cur_token_is(&TokenKind::EOF) {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                eprintln!("parse stmt none from parse block");
            }
            self.next_token();
        }

        Block(stmts)
    }

    fn no_prefix_error(&mut self) {
        self.errors.push(format!(
            "no prefix parse function for {:?} found",
            self.cur_token.kind
        ));
    }

    // TODO: remove Option for fns like this when possible (errors instead) ?? (maybe need option)
    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        if !expect_peek!(self, TokenKind::Identifier(_)) {
            return None;
        }

        let name: Ident = Token::ident(&self.cur_token)?;

        if !self.expect_peek(TokenKind::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Stmt::Let(name, value))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.next_token();

        let ret = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Stmt::Return(ret))
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

    fn parse_parameters(&mut self) -> Vec<Ident> {
        let mut parameters = Vec::new();

        if self.peek_token_is(&TokenKind::RParen) {
            self.next_token();
            return parameters;
        }

        self.next_token();
        if let TokenKind::Identifier(ident) = &self.cur_token.kind {
            parameters.push(ident.clone());
        }

        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            self.next_token();

            if let TokenKind::Identifier(ident) = &self.cur_token.kind {
                parameters.push(ident.clone());
            }
        }

        if !self.expect_peek(TokenKind::RParen) {
            //TODO: deal with error
        }

        parameters
    }

    fn parse_arguments(&mut self) -> Vec<Expr> {
        let mut arguments = Vec::new();

        if self.peek_token_is(&TokenKind::RParen) {
            self.next_token();
            return arguments;
        }

        self.next_token();
        arguments.push(
            self.parse_expression(Precedence::Lowest)
                .expect("parse args expr none"),
        );

        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            self.next_token();

            arguments.push(
                self.parse_expression(Precedence::Lowest)
                    .expect("parse args expr none"),
            );
        }

        if !self.expect_peek(TokenKind::RParen) {
            //TODO: deal with error
        }

        arguments
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::{
        ast::{Block, Expr, Operator, Stmt},
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
    fn call_test() {
        let inputs = ["add(1, 2 * 3, 4 + 5);"];

        let tests = vec![Stmt::Expr(Expr::Call {
            function: Box::new(Expr::Ident("add".to_owned())),
            arguments: vec![
                Expr::IntLiteral(1),
                Expr::Infix(
                    Box::new(Expr::IntLiteral(2)),
                    Operator::Asterisk,
                    Box::new(Expr::IntLiteral(3)),
                ),
                Expr::Infix(
                    Box::new(Expr::IntLiteral(4)),
                    Operator::Plus,
                    Box::new(Expr::IntLiteral(5)),
                ),
            ],
        })];

        for (input, expected) in inputs.iter().zip(tests) {
            let mut parser = Parser::new(Lexer::new(input, Source::Repl));
            let program = parser.parse_program();

            assert!(check_parser_errors(parser));
            assert_eq!(program.0.len(), 1);
            assert_eq!(*program.0.first().unwrap(), expected);
        }
    }

    #[test]
    fn functions_test() {
        let inputs = [
            "fn(x, y) { x + y; }",
            "fn() {};",
            "fn(x) {};",
            "fn(x, y, z) {};",
        ];

        let tests = vec![
            Stmt::Expr(Expr::FunctionLiteral {
                parameters: vec!["x".to_owned(), "y".to_owned()],
                body: Block(vec![Stmt::Expr(Expr::Infix(
                    Box::new(Expr::Ident("x".to_owned())),
                    Operator::Plus,
                    Box::new(Expr::Ident("y".to_owned())),
                ))]),
            }),
            Stmt::Expr(Expr::FunctionLiteral {
                parameters: vec![],
                body: Block(vec![]),
            }),
            Stmt::Expr(Expr::FunctionLiteral {
                parameters: vec!["x".to_owned()],
                body: Block(vec![]),
            }),
            Stmt::Expr(Expr::FunctionLiteral {
                parameters: vec!["x".to_owned(), "y".to_owned(), "z".to_owned()],
                body: Block(vec![]),
            }),
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
    fn if_test() {
        let inputs = ["if (x < y) { x }", "if x < y { x } else { y }"];

        let tests = vec![
            Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Box::new(Expr::Ident("x".to_owned())),
                    Operator::Less,
                    Box::new(Expr::Ident("y".to_owned())),
                )),
                consequence: Block(vec![Stmt::Expr(Expr::Ident("x".to_owned()))]),
                alternative: None,
            }),
            Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Box::new(Expr::Ident("x".to_owned())),
                    Operator::Less,
                    Box::new(Expr::Ident("y".to_owned())),
                )),
                consequence: Block(vec![Stmt::Expr(Expr::Ident("x".to_owned()))]),
                alternative: Some(Block(vec![Stmt::Expr(Expr::Ident("y".to_owned()))])),
            }),
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
    fn boolean_test() {
        let inputs = ["true", "false"];

        let tests = vec![
            Stmt::Expr(Expr::BooleanLiteral(true)),
            Stmt::Expr(Expr::BooleanLiteral(false)),
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
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
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
            "5 + 5",
            "5 - 5",
            "5 * 5",
            "5 / 5",
            "5 > 5",
            "5 < 5",
            "5 == 5",
            "5 != 5",
            "true == true",
            "true != false",
            "false == false",
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
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::BooleanLiteral(true)),
                Operator::Equal,
                Box::new(Expr::BooleanLiteral(true)),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::BooleanLiteral(true)),
                Operator::NotEqual,
                Box::new(Expr::BooleanLiteral(false)),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::BooleanLiteral(false)),
                Operator::Equal,
                Box::new(Expr::BooleanLiteral(false)),
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
        let inputs = ["!5", "-15", "!true", "!false"];

        let tests = vec![
            Stmt::Expr(Expr::Prefix(Operator::Bang, Box::new(Expr::IntLiteral(5)))),
            Stmt::Expr(Expr::Prefix(
                Operator::Minus,
                Box::new(Expr::IntLiteral(15)),
            )),
            Stmt::Expr(Expr::Prefix(
                Operator::Bang,
                Box::new(Expr::BooleanLiteral(true)),
            )),
            Stmt::Expr(Expr::Prefix(
                Operator::Bang,
                Box::new(Expr::BooleanLiteral(false)),
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
        let y = true; \
        let foobar = 838383;";

        let lexer = Lexer::new(input, Source::Repl);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert!(check_parser_errors(parser));
        assert_eq!(program.0.len(), 3);

        let tests = vec![
            Stmt::Let("x".to_owned(), Expr::IntLiteral(5)),
            Stmt::Let("y".to_owned(), Expr::BooleanLiteral(true)),
            Stmt::Let("foobar".to_owned(), Expr::IntLiteral(838383)),
        ];
        for (res, expected) in program.0.iter().zip(tests) {
            assert_eq!(*res, expected);
        }
    }

    #[test]
    fn return_stmt_test() {
        let input = "return 5; \
        return false; \
        return 993322;";

        let lexer = Lexer::new(input, Source::Repl);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert!(check_parser_errors(parser));
        assert_eq!(program.0.len(), 3);

        let tests = vec![
            Stmt::Return(Expr::IntLiteral(5)),
            Stmt::Return(Expr::BooleanLiteral(false)),
            Stmt::Return(Expr::IntLiteral(993322)),
        ];
        for (res, expected) in program.0.iter().zip(tests) {
            assert_eq!(*res, expected);
        }
    }
}
