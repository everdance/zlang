use crate::token::{Token, TokenKind};

pub enum ExprType {
    Literal(TokenKind),
    Binary(TokenKind),
    Unary(TokenKind),
    Variable(TokenKind),
    Logical(TokenKind),
    Grouping,
    Call,
    Assign,
    Get(TokenKind),
    Set,
    This(TokenKind),
    Super(TokenKind),
}

pub struct Expr {
    pub kind: ExprType,
    pub left: Option<Box<Expr>>,
    pub right: Option<Box<Expr>>,
    pub list: Option<Vec<Expr>>,
}

#[macro_export]
macro_rules! new_expr {
    ($kind: expr) => {
        Expr {
            kind: $kind,
            left: None,
            right: None,
            list: None,
        }
    };
    ($kind: expr, $left: expr) => {
        Expr {
            kind: $kind,
            left: $left,
            right: None,
            list: None,
        }
    };
    ($kind: expr, $left: expr, $right: expr) => {
        Expr {
            kind: $kind,
            left: $left,
            right: $right,
            list: None,
        }
    };
    ($kind: expr, $left: expr, $right: expr, $list: expr) => {
        Expr {
            kind: $kind,
            left: $left,
            right: $right,
            list: $list,
        }
    };
}

pub enum Stmt {
    Expr(Expr),
    Var(Token, Expr),
    Return(Token, Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Block(Vec<Stmt>),
    While(Expr, Box<Stmt>),
    Fun(Token, Vec<Token>, Vec<Stmt>),
    Class(Token, Vec<Stmt>),
}
