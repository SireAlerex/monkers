use std::fmt::Display;

use super::parser::Parser;

pub struct Program(pub Vec<Stmt>);

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Return(Expr),
    Let(Ident, Expr),
    Expr(Expr),
}

pub type Ident = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    //TODO: remove
    Temp,
    Ident(Ident),
    IntLiteral(i64),
    Prefix(Operator, Box<Expr>),
    Infix(Box<Expr>, Operator, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Greater,
    Less,
    Equal,
    NotEqual,
    Bang,
}

pub type PrefixParseFn = fn(&mut Parser) -> Expr;
pub type InfixParseFn = fn(&mut Parser, Expr) -> Expr;

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for stmt in &self.0 {
            s.push_str(&(stmt.to_string() + "\n"))
        }
        f.write_str(&s)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Let(ident, expr) => format!("let {ident} = {expr};"),
            Self::Return(expr) => format!("return {expr};"),
            Self::Expr(expr) => format!("{expr}"),
        };
        f.write_fmt(format_args!("{s}"))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Temp => "temp".to_string(),
            Self::Ident(ident) => ident.clone(),
            Self::IntLiteral(int) => int.to_string(),
            Self::Prefix(op, expr) => format!("({op}{expr})"),
            Self::Infix(left, op, right) => format!("({left} {op} {right})"),
        };
        f.write_str(&s)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Operator::Bang => "!",
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Asterisk => "*",
            Operator::Slash => "/",
            Operator::Greater => ">",
            Operator::Less => "<",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
        };
        f.write_str(s)
    }
}

#[cfg(test)]
mod test {
    use super::{Expr, Program, Stmt};

    #[test]
    fn program_print() {
        let program = Program(vec![
            Stmt::Let("myVar".to_owned(), Expr::Ident("otherVar".to_owned())),
            Stmt::Return(Expr::Ident("result".to_owned())),
        ]);

        let expected_str = "\
        let myVar = otherVar;\n\
        return result;\n\
        ";

        assert_eq!(program.to_string(), expected_str);
    }
}
