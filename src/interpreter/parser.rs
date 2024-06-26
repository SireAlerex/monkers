use std::collections::HashMap;

use super::{
    ast::{Block, Expr, Ident, InfixParseFn, Literal, Operator, PrefixParseFn, Program, Stmt},
    lexer::Lexer,
    token::{Keyword, Kind, Source, Token},
};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_fns: HashMap<Kind, PrefixParseFn>,
    infix_fns: HashMap<Kind, InfixParseFn>,
}

const fn precedence(kind: &Kind) -> Precedence {
    match kind {
        Kind::EQ | Kind::NotEQ => Precedence::Equals,
        Kind::LT | Kind::GT => Precedence::LessGreater,
        Kind::Plus | Kind::Minus => Precedence::Sum,
        Kind::Asterisk | Kind::Slash => Precedence::Product,
        Kind::LParen => Precedence::Call,
        Kind::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

fn parse_ident(parser: &mut Parser) -> Option<Expr> {
    if let Kind::Identifier(s) = &parser.cur_token.kind {
        Some(Expr::Ident(s.clone()))
    } else {
        parser.error_from_current("error parsing ident: not an ident, should never happen")?
    }
}

fn parse_int_literal(parser: &mut Parser) -> Option<Expr> {
    if let Kind::Int(x) = &parser.cur_token.kind {
        Some(Expr::Literal(Literal::Int(*x)))
    } else {
        parser.error_from_current("error parsing integer: not an integer, should never happen")?
    }
}

fn parse_prefix(parser: &mut Parser) -> Option<Expr> {
    let op = match parser.cur_token.kind {
        Kind::Minus => Operator::Minus,
        Kind::Bang => Operator::Bang,
        _ => parser
            .error_from_current(&format!("bad prefix operator: {:?}", parser.cur_token.kind))?,
    };

    parser.next_token()?;

    if let Some(right) = parser.parse_expression(Precedence::Prefix) {
        Some(Expr::Prefix(op, Box::new(right)))
    } else {
        parser.error_from_current("prefix expr: can't parse right expr")?
    }
}

fn parse_infix(parser: &mut Parser, left: Expr) -> Option<Expr> {
    let op = match parser.cur_token.kind {
        Kind::Plus => Operator::Plus,
        Kind::Minus => Operator::Minus,
        Kind::Asterisk => Operator::Asterisk,
        Kind::Slash => Operator::Slash,
        Kind::GT => Operator::Greater,
        Kind::LT => Operator::Less,
        Kind::EQ => Operator::Equal,
        Kind::NotEQ => Operator::NotEqual,
        _ => parser
            .error_from_current(&format!("bad prefix operator: {:?}", parser.cur_token.kind))?,
    };

    let precedence = parser.cur_precedence();
    parser.next_token()?;

    if let Some(right) = parser.parse_expression(precedence) {
        Some(Expr::Infix(Box::new(left), op, Box::new(right)))
    } else {
        parser.error_from_current("infix expr: can't parse right expr")?
    }
}

// #[allow(unnecessary_wraps)]
fn parse_boolean(parser: &mut Parser) -> Option<Expr> {
    Some(Expr::Literal(Literal::Boolean(
        parser.cur_token_is(&Kind::Key(Keyword::True)),
    )))
}

fn parse_paren(parser: &mut Parser) -> Option<Expr> {
    parser.next_token()?;

    if let Some(expr) = parser.parse_expression(Precedence::Lowest) {
        parser.check_peek(&Kind::RParen)?;

        Some(expr)
    } else {
        parser.error_from_current("grouped expr: can't parse expr")?
    }
}

fn parse_if(parser: &mut Parser) -> Option<Expr> {
    parser.next_token()?;
    let cond = if let Some(cond) = parser.parse_expression(Precedence::Lowest) {
        Box::new(cond)
    } else {
        parser.error_from_current("if expr: can't parse condition")?
    };

    parser.check_peek(&Kind::LBrace)?;

    let consequence = parser.parse_block();

    let alternative = if parser.peek_token_is(&Kind::Key(Keyword::Else)) {
        parser.next_token()?;
        parser.check_peek(&Kind::LBrace)?;

        Some(parser.parse_block())
    } else {
        None
    };

    Some(Expr::If {
        cond,
        consequence,
        alternative,
    })
}

fn parse_fn(parser: &mut Parser) -> Option<Expr> {
    // parsing parameters
    parser.check_peek(&Kind::LParen)?;
    let parameters = parser.parse_parameters()?;
    parser.check_peek(&Kind::LBrace)?;

    let body = parser.parse_block();

    Some(Expr::FunctionLiteral { parameters, body })
}

fn parse_call(parser: &mut Parser, function: Expr) -> Option<Expr> {
    let arguments = parser.parse_arguments()?;

    Some(Expr::Call {
        function: Box::new(function),
        arguments,
    })
}

fn parse_string(parser: &mut Parser) -> Option<Expr> {
    if let Kind::String(ref s) = parser.cur_token.kind {
        Some(Expr::Literal(Literal::String(s.to_owned())))
    } else {
        parser.error_from_current("error parsing string: not an string, should never happen")?
    }
}

fn parse_array(parser: &mut Parser) -> Option<Expr> {
    let elements = parser.parse_expr_list(&Kind::RBracket);

    Some(Expr::Array(elements?))
}

fn parse_index(parser: &mut Parser, left: Expr) -> Option<Expr> {
    parser.next_token()?;
    if let Some(index) = parser.parse_expression(Precedence::Lowest) {
        parser.check_peek(&Kind::RBracket)?;

        Some(Expr::Index(Box::new(left), Box::new(index)))
    } else {
        parser.error_from_current("index expr: can't parse index")?
    }
}

fn parse_hash(parser: &mut Parser) -> Option<Expr> {
    let mut pairs = Vec::new();

    while !parser.peek_token_is(&Kind::RBrace) {
        parser.next_token()?;
        let key = match parser.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => parser.error_from_current("hashmap error: can't parse key")?,
        };

        parser.check_peek(&Kind::Colon)?;

        parser.next_token()?;
        let value = match parser.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => parser.error_from_current("hashmap error: can't parse value")?,
        };

        pairs.push((key, value));

        if !parser.peek_token_is(&Kind::RBrace) {
            parser.check_peek(&Kind::Comma)?;
        }
    }

    parser.check_peek(&Kind::RBrace)?;

    Some(Expr::HashLiteral(pairs))
}

// TODO: function to return None and add an error
impl<'a, 'b> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let mut prefix_fns: HashMap<Kind, PrefixParseFn> = HashMap::new();
        let mut infix_fns: HashMap<Kind, InfixParseFn> = HashMap::new();

        // Prefix functions
        prefix_fns.insert(Kind::Identifier("_".to_owned()), parse_ident);
        prefix_fns.insert(Kind::String("_".to_owned()), parse_string);
        prefix_fns.insert(Kind::Int(0), parse_int_literal);
        prefix_fns.insert(Kind::Key(Keyword::True), parse_boolean);
        prefix_fns.insert(Kind::Key(Keyword::False), parse_boolean);
        prefix_fns.insert(Kind::Key(Keyword::If), parse_if);
        prefix_fns.insert(Kind::Key(Keyword::Function), parse_fn);
        prefix_fns.insert(Kind::Bang, parse_prefix);
        prefix_fns.insert(Kind::Minus, parse_prefix);
        prefix_fns.insert(Kind::LParen, parse_paren);
        prefix_fns.insert(Kind::LBracket, parse_array);
        prefix_fns.insert(Kind::LBrace, parse_hash);

        // Infix functions
        infix_fns.insert(Kind::Plus, parse_infix);
        infix_fns.insert(Kind::Minus, parse_infix);
        infix_fns.insert(Kind::Asterisk, parse_infix);
        infix_fns.insert(Kind::Slash, parse_infix);
        infix_fns.insert(Kind::GT, parse_infix);
        infix_fns.insert(Kind::LT, parse_infix);
        infix_fns.insert(Kind::EQ, parse_infix);
        infix_fns.insert(Kind::NotEQ, parse_infix);
        infix_fns.insert(Kind::LParen, parse_call);
        infix_fns.insert(Kind::LBracket, parse_index);

        let cur_token = lexer
            .next_token()
            .inspect_err(|err| panic!("error getting first token: {err}"))
            .unwrap();
        let peek_token = lexer
            .next_token()
            .inspect_err(|err| panic!("error getting second token: {err}"))
            .unwrap();

        Parser {
            lexer,
            cur_token,
            peek_token,
            errors: Vec::new(),
            prefix_fns,
            infix_fns,
        }
    }

    #[must_use]
    fn next_token(&mut self) -> Option<()> {
        // TODO: remove clone (change Token ? use iter over tokens ?)
        self.cur_token = self.peek_token.clone();
        match self.lexer.next_token() {
            Ok(tok) => self.peek_token = tok,
            Err(err) => self.error_from_current::<()>(&err)?,
        }
        Some(())
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Block(Vec::new());

        while self.cur_token.kind != Kind::EOF && !self.is_err() {
            if let Some(stmt) = self.parse_stmt() {
                program.0.push(stmt);
            }
            if self.next_token().is_none() {
                break;
            }
        }

        program
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.cur_token.kind {
            Kind::Key(Keyword::Let) => self.parse_let_stmt(),
            Kind::Key(Keyword::Return) => self.parse_return_stmt(),
            _ => self.parse_expression_stmt(),
        }
    }

    fn parse_expression_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Kind::Semicolon) {
            self.next_token()?;
        }

        Some(Stmt::Expr(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expr> {
        if let Some(prefix) = Self::get_fn(&self.prefix_fns, &self.cur_token.kind) {
            let mut left = prefix(self)?;

            while !self.peek_token_is(&Kind::Semicolon) && precedence < self.peek_precedence() {
                // TODO: get rid of clone
                let map = self.infix_fns.clone();
                if let Some(infix) = Self::get_fn(&map, &self.peek_token.kind) {
                    self.next_token()?;
                    left = infix(self, left)?;
                } else {
                    return Some(left);
                }
            }

            Some(left)
        } else {
            self.error_from_current(&format!(
                "no prefix parse function for {:?} found",
                self.cur_token.kind
            ))
        }
    }

    fn parse_block(&mut self) -> Block {
        let mut stmts = Vec::new();
        if self.next_token().is_none() {
            return Block(stmts);
        }

        while !self.cur_token_is(&Kind::RBrace) && !self.cur_token_is(&Kind::EOF) {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                _ = self.error_from_current::<()>("block error: can't parse statement");
            }
            if self.next_token().is_none() {
                break;
            }
        }

        Block(stmts)
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        if let Kind::Identifier(_) = &self.peek_token.kind {
            self.next_token()?;
        } else {
            return self.error_from_token::<Stmt>(
                &format!(
                    "expected next token to be Identifier, got {:?} instead",
                    self.peek_token.kind
                ),
                &self.peek_token.clone(),
            );
        }

        let name: Ident = Token::ident(&self.cur_token)?;

        self.check_peek(&Kind::Assign)?; // <--
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Kind::Semicolon) {
            self.next_token()?;
        }

        Some(Stmt::Let(name, value))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.next_token()?;

        let ret = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(&Kind::Semicolon) {
            self.next_token()?;
        }

        Some(Stmt::Return(ret))
    }

    fn parse_arguments(&mut self) -> Option<Vec<Expr>> {
        self.parse_expr_list(&Kind::RParen)
    }

    fn parse_expr_list(&mut self, end: &Kind) -> Option<Vec<Expr>> {
        let mut list = Vec::new();

        if self.peek_token_is(end) {
            self.next_token()?;
            return Some(list);
        }

        self.next_token()?;
        if let Some(expr) = self.parse_expression(Precedence::Lowest) {
            list.push(expr);
        }

        while self.peek_token_is(&Kind::Comma) {
            self.next_token()?;
            self.next_token()?;

            if let Some(expr) = self.parse_expression(Precedence::Lowest) {
                list.push(expr);
            }
        }

        self.check_peek(end)?;

        Some(list)
    }

    fn parse_parameters(&mut self) -> Option<Vec<Ident>> {
        let mut parameters = Vec::new();

        if self.peek_token_is(&Kind::RParen) {
            self.next_token()?;
            return Some(parameters);
        }

        self.next_token()?;
        if let Kind::Identifier(ident) = &self.cur_token.kind {
            parameters.push(ident.clone());
        }

        while self.peek_token_is(&Kind::Comma) {
            self.next_token()?;
            self.next_token()?;

            if let Kind::Identifier(ident) = &self.cur_token.kind {
                parameters.push(ident.clone());
            }
        }

        self.check_peek(&Kind::RParen)?;

        Some(parameters)
    }

    // Precedence and prefix/infix function handling

    // Standardized enums for the ones that can hold data to find them in hash
    fn get_fn<T>(map: &'b HashMap<Kind, T>, kind: &Kind) -> Option<&'b T> {
        match kind {
            Kind::Identifier(_) => map.get(&Kind::Identifier("_".to_owned())),
            Kind::Int(_) => map.get(&Kind::Int(0)),
            Kind::String(_) => map.get(&Kind::String("_".to_owned())),
            _ => map.get(kind),
        }
    }

    const fn peek_precedence(&self) -> Precedence {
        precedence(&self.peek_token.kind)
    }

    const fn cur_precedence(&self) -> Precedence {
        precedence(&self.cur_token.kind)
    }

    // Checking tokens and error handling

    fn cur_token_is(&self, kind: &Kind) -> bool {
        self.cur_token.kind == *kind
    }

    fn peek_token_is(&self, kind: &Kind) -> bool {
        self.peek_token.kind == *kind
    }

    fn check_peek(&mut self, kind: &Kind) -> Option<()> {
        if self.peek_token_is(kind) {
            self.next_token()?;
            Some(())
        } else {
            self.peek_error(kind)
        }
    }

    fn peek_error(&mut self, kind: &Kind) -> Option<()> {
        self.expect_token_error(&self.peek_token.clone(), kind)
    }

    fn expect_token_error(&mut self, token: &Token, expected: &Kind) -> Option<()> {
        self.error_from_token::<()>(
            &format!(
                "expected next token to be {expected:?}, got {:?} instead",
                token.kind
            ),
            token,
        )
    }

    fn error_from_current<T>(&mut self, msg: &str) -> Option<T> {
        self.error_from_token(msg, &self.cur_token.clone())
    }

    fn error_from_token<T>(&mut self, msg: &str, token: &Token) -> Option<T> {
        let source = match token.source.as_ref() {
            Source::File(file) => format!("{file}: "),
            Source::Repl => String::new(),
        };
        self.error(format!(
            "{source}line={} column={} -> {msg}",
            token.line, token.column
        ))
    }

    fn error<T>(&mut self, msg: String) -> Option<T> {
        self.errors.push(msg);
        None
    }

    pub const fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn is_err(&self) -> bool {
        !self.errors.is_empty()
    }

    // used in tests
    #[allow(dead_code)]
    pub fn parse(input: &str) -> Program {
        Parser::new(Lexer::new(input, Source::Repl)).parse_program()
    }
}

#[allow(clippy::unreadable_literal)]
#[cfg(test)]
mod test {
    use crate::interpreter::{
        ast::{Block, Expr, Literal, Operator, Stmt},
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

    fn check_tests(tests: &[(&str, Stmt)]) {
        for (input, expected) in tests {
            let mut parser = Parser::new(Lexer::new(input, Source::Repl));
            let program = parser.parse_program();

            assert!(check_parser_errors(parser));
            println!("program: {:?}", program.0);
            assert_eq!(program.0.len(), 1);
            assert_eq!(program.0.first().unwrap(), expected);
        }
    }

    #[test]
    fn hash_test() {
        check_tests(&[
            (
                "{\"one\": 1, \"two\": 2, \"three\": 3}",
                Stmt::Expr(Expr::HashLiteral(vec![
                    (
                        Expr::Literal(Literal::String("one".to_owned())),
                        Expr::Literal(Literal::Int(1)),
                    ),
                    (
                        Expr::Literal(Literal::String("two".to_owned())),
                        Expr::Literal(Literal::Int(2)),
                    ),
                    (
                        Expr::Literal(Literal::String("three".to_owned())),
                        Expr::Literal(Literal::Int(3)),
                    ),
                ])),
            ),
            ("{}", Stmt::Expr(Expr::HashLiteral(vec![]))),
            (
                "{\"one\": 0+1, \"two\": 10-8, \"three\": 15/5}",
                Stmt::Expr(Expr::HashLiteral(vec![
                    (
                        Expr::Literal(Literal::String("one".to_owned())),
                        Expr::Infix(
                            Box::new(Expr::Literal(Literal::Int(0))),
                            Operator::Plus,
                            Box::new(Expr::Literal(Literal::Int(1))),
                        ),
                    ),
                    (
                        Expr::Literal(Literal::String("two".to_owned())),
                        Expr::Infix(
                            Box::new(Expr::Literal(Literal::Int(10))),
                            Operator::Minus,
                            Box::new(Expr::Literal(Literal::Int(8))),
                        ),
                    ),
                    (
                        Expr::Literal(Literal::String("three".to_owned())),
                        Expr::Infix(
                            Box::new(Expr::Literal(Literal::Int(15))),
                            Operator::Slash,
                            Box::new(Expr::Literal(Literal::Int(5))),
                        ),
                    ),
                ])),
            ),
        ]);
    }

    #[test]
    fn array_test() {
        check_tests(&[
            (
                "[1, 2 * 2, 3 + 3]",
                Stmt::Expr(Expr::Array(vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(2))),
                        Operator::Asterisk,
                        Box::new(Expr::Literal(Literal::Int(2))),
                    ),
                    Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Operator::Plus,
                        Box::new(Expr::Literal(Literal::Int(3))),
                    ),
                ])),
            ),
            (
                "myArray[2+1]",
                Stmt::Expr(Expr::Index(
                    Box::new(Expr::Ident("myArray".to_string())),
                    Box::new(Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(2))),
                        Operator::Plus,
                        Box::new(Expr::Literal(Literal::Int(1))),
                    )),
                )),
            ),
        ]);
    }

    #[test]
    fn string_literal_test() {
        let tests = [(
            "\"hello world\";",
            Stmt::Expr(Expr::Literal(Literal::String("hello world".to_string()))),
        )];

        check_tests(&tests);
    }

    #[test]
    fn call_test() {
        let tests = [(
            "add(1, 2 * 3, 4 + 5);",
            Stmt::Expr(Expr::Call {
                function: Box::new(Expr::Ident("add".to_owned())),
                arguments: vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(2))),
                        Operator::Asterisk,
                        Box::new(Expr::Literal(Literal::Int(3))),
                    ),
                    Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(4))),
                        Operator::Plus,
                        Box::new(Expr::Literal(Literal::Int(5))),
                    ),
                ],
            }),
        )];

        check_tests(&tests);
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
            Stmt::Expr(Expr::Literal(Literal::Boolean(true))),
            Stmt::Expr(Expr::Literal(Literal::Boolean(false))),
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
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
            ("{1: 2, true: 5}", "{1: 2, true: 5}"),
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
                Box::new(Expr::Literal(Literal::Int(5))),
                Operator::Plus,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Operator::Minus,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Operator::Asterisk,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Operator::Slash,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Operator::Greater,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Operator::Less,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Operator::Equal,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Int(5))),
                Operator::NotEqual,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Boolean(true))),
                Operator::Equal,
                Box::new(Expr::Literal(Literal::Boolean(true))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Boolean(true))),
                Operator::NotEqual,
                Box::new(Expr::Literal(Literal::Boolean(false))),
            )),
            Stmt::Expr(Expr::Infix(
                Box::new(Expr::Literal(Literal::Boolean(false))),
                Operator::Equal,
                Box::new(Expr::Literal(Literal::Boolean(false))),
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
            Stmt::Expr(Expr::Prefix(
                Operator::Bang,
                Box::new(Expr::Literal(Literal::Int(5))),
            )),
            Stmt::Expr(Expr::Prefix(
                Operator::Minus,
                Box::new(Expr::Literal(Literal::Int(15))),
            )),
            Stmt::Expr(Expr::Prefix(
                Operator::Bang,
                Box::new(Expr::Literal(Literal::Boolean(true))),
            )),
            Stmt::Expr(Expr::Prefix(
                Operator::Bang,
                Box::new(Expr::Literal(Literal::Boolean(false))),
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
            Stmt::Expr(Expr::Literal(Literal::Int(5))),
            Stmt::Expr(Expr::Literal(Literal::Int(65987314))),
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
            Stmt::Let("x".to_owned(), Expr::Literal(Literal::Int(5))),
            Stmt::Let("y".to_owned(), Expr::Literal(Literal::Boolean(true))),
            Stmt::Let("foobar".to_owned(), Expr::Literal(Literal::Int(838383))),
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
            Stmt::Return(Expr::Literal(Literal::Int(5))),
            Stmt::Return(Expr::Literal(Literal::Boolean(false))),
            Stmt::Return(Expr::Literal(Literal::Int(993322))),
        ];
        for (res, expected) in program.0.iter().zip(tests) {
            assert_eq!(*res, expected);
        }
    }

    #[test]
    fn string_error() {
        let lexer = Lexer::new("let a = \"a", Source::Repl);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert!(parser.is_err());
        assert!(parser
            .errors()
            .iter()
            .any(|s| s.ends_with("missing '\"' to end string")));
        assert_eq!(program, Block(Vec::new()));
    }
}
