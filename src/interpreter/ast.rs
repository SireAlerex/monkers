pub type Program = Vec<Stmt>;

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
    IdentExpr,
}
