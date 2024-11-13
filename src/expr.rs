use crate::token::TokenKind;

pub enum ExprType {
    Literal(TokenKind),
    Binary(TokenKind),
    Unary(TokenKind),
    Variable(TokenKind),
    Logical(TokenKind),
    Grouping,
    Call(TokenKind),
    Assign(TokenKind),
    Get(TokenKind),
    Set(TokenKind),
    This(TokenKind),
    Super(TokenKind),
}

pub struct Expr {
    pub kind: ExprType,
    pub left: Option<Box<Expr>>,
    pub right: Option<Box<Expr>>,
    pub list: Option<Vec<Expr>>,
}
