use std::fmt::Display;

use super::parser::Parser;

pub type Program = Block;

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Return(Expr),
    Let(Ident, Expr),
    Expr(Expr),
}

pub type Ident = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(Ident),
    IntLiteral(i64),
    BooleanLiteral(bool),
    Prefix(Operator, Box<Expr>),
    Infix(Box<Expr>, Operator, Box<Expr>),
    If {
        cond: Box<Expr>,
        consequence: Block,
        alternative: Option<Block>,
    },
    FunctionLiteral {
        parameters: Vec<Ident>,
        body: Block,
    },
    Call {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
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
            s.push_str(&format!("{stmt}\n"))
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
            Self::Ident(ident) => ident.clone(),
            Self::IntLiteral(int) => int.to_string(),
            Self::BooleanLiteral(bool) => bool.to_string(),
            Self::Prefix(op, expr) => format!("({op}{expr})"),
            Self::Infix(left, op, right) => format!("({left} {op} {right})"),
            Self::If {
                cond,
                consequence,
                alternative,
            } => {
                let s = format!("if {cond} {{\n\t{consequence}\n}}");
                if let Some(block) = alternative {
                    format!("{s} else {{\n\t{block}\n}}")
                } else {
                    s
                }
            }
            Self::FunctionLiteral { parameters, body } => {
                format!("fn ({}) {{ {} }}", parameters.join(","), body)
            }
            Self::Call {
                function,
                arguments,
            } => {
                format!(
                    "{function}({})",
                    (*arguments)
                        .iter()
                        .map(|expr| expr.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
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
    use crate::interpreter::ast::Block;

    use super::{Expr, Stmt};

    #[test]
    fn program_print() {
        let program = Block(vec![
            Stmt::Let("myVar".to_owned(), Expr::Ident("otherVar".to_owned())),
            Stmt::Return(Expr::Ident("result".to_owned())),
        ]);

        let expected_str = "\
        let myVar = otherVar;\
        return result;\
        ";

        assert_eq!(program.to_string().replace('\n', ""), expected_str);
    }
}
