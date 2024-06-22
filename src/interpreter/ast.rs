use std::fmt::Display;

use super::parser::Parser;

pub struct Program(pub Vec<Stmt>);

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ReturnStmt(Expr),
    LetStmt(Ident, Expr),
    ExprStmt(Expr),
}

pub type Ident = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    //TODO: remove
    Temp,
    IdentExpr(Ident),
    IntLiteral(i64),
}

pub type PrefixParseFn = fn(&mut Parser) -> Expr;
pub type InfixParseFn = fn(Expr) -> Expr;

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
            Self::LetStmt(ident, expr) => format!("let {ident} = {expr};"),
            Self::ReturnStmt(expr) => format!("return {expr};"),
            Self::ExprStmt(expr) => format!("{expr}"),
        };
        f.write_fmt(format_args!("{s}"))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Temp => "temp".to_string(),
            Self::IdentExpr(ident) => ident.clone(),
            Self::IntLiteral(int) => int.to_string(),
        };
        f.write_str(&s)
    }
}

#[cfg(test)]
mod test {
    use super::{Expr, Program, Stmt};

    #[test]
    fn program_print() {
        let program = Program(vec![
            Stmt::LetStmt("myVar".to_owned(), Expr::IdentExpr("otherVar".to_owned())),
            Stmt::ReturnStmt(Expr::IdentExpr("result".to_owned())),
        ]);

        let expected_str = "\
        let myVar = otherVar;\n\
        return result;\n\
        ";

        assert_eq!(program.to_string(), expected_str);
    }
}
