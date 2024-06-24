use std::{
    fmt::{self, Display, Formatter},
    string::ToString,
};

use crate::utils;

use super::parser::Parser;

pub type Program = Block;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Stmt {
    Return(Expr),
    Let(Ident, Expr),
    Expr(Expr),
}

pub type Ident = String;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    HashLiteral(Vec<(Expr, Expr)>),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Literal {
    Int(i64),
    Boolean(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
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

pub type PrefixParseFn = fn(&mut Parser) -> Option<Expr>;
pub type InfixParseFn = fn(&mut Parser, Expr) -> Option<Expr>;

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for stmt in &self.0 {
            s = format!("{s}{stmt}\n");
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
            Self::Literal(lit) => lit.to_string(),
            Self::HashLiteral(hash) => format!(
                "{{{}}}",
                utils::join(hash.iter(), |(key, value)| format!("{key}: {value}"))
            ),
            Self::Array(vec) => format!("[{}]", utils::join(vec.iter(), ToString::to_string)),
            Self::Index(left, index) => format!("({left}[{index}])"),
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
                format!("fn ({}) {{ {} }}", parameters.join(", "), body)
            }
            Self::Call {
                function,
                arguments,
            } => {
                format!(
                    "{function}({})",
                    utils::join(arguments.iter(), ToString::to_string)
                )
            }
        };
        f.write_str(&s)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{x}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "{s}"),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Bang => "!",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Asterisk => "*",
            Self::Slash => "/",
            Self::Greater => ">",
            Self::Less => "<",
            Self::Equal => "==",
            Self::NotEqual => "!=",
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
