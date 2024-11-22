use crate::token::{Kind, Token};

#[derive(Debug, PartialEq)]
pub enum ExprType {
    Literal,
    Binary,
    Unary,
    Variable,
    Logical,
    Grouping,
    Call,
    Assign,
    Get,
    Set,
    This,
    Super,
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprType,
    pub token: Token,
    pub left: Option<Box<Expr>>,
    pub right: Option<Box<Expr>>,
    pub list: Option<Vec<Expr>>,
}

pub fn single(t: Token) -> Expr {
    let kind = match t.kind {
        Kind::Var => ExprType::Variable,
        Kind::This => ExprType::This,
        _ => ExprType::Literal,
    };

    Expr {
        kind,
        token: t,
        left: None,
        right: None,
        list: None,
    }
}

pub fn unary(t: Token, opr: Expr) -> Expr {
    let kind = match t.kind {
        Kind::Minus | Kind::Bang => ExprType::Unary,
        Kind::LeftParen => ExprType::Grouping,
        Kind::Identifier(_) => ExprType::Get,
        Kind::Super => ExprType::Super,
        _ => panic!("unexpected token type: {:?}", t),
    };

    Expr {
        kind,
        token: t,
        left: Some(Box::new(opr)),
        right: None,
        list: None,
    }
}

pub fn binary(t: Token, left: Expr, right: Expr) -> Expr {
    let kind = match t.kind {
        Kind::Slash | Kind::Star | Kind::Minus | Kind::Plus => ExprType::Binary,
        Kind::Dot => ExprType::Get,
        Kind::Equal => match right.kind {
            ExprType::Variable | ExprType::Literal => ExprType::Assign,
            _ => ExprType::Set,
        },
        Kind::And | Kind::Or => ExprType::Logical,
        _ => panic!("unexpected token type: {:?}", t),
    };

    Expr {
        kind,
        token: t,
        left: Some(Box::new(left)),
        right: Some(Box::new(right)),
        list: None,
    }
}

pub fn list(t: Token, left: Expr, list: Vec<Expr>) -> Expr {
    Expr {
        kind: ExprType::Call,
        token: t,
        left: Some(Box::new(left)),
        right: None,
        list: Some(list),
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Var(Token, Option<Expr>),
    //Return(Token, Expr),
    If(Expr, Box<Stmt>),
    For(Vec<Stmt>, Box<Stmt>),
    Block(Vec<Stmt>),
    While(Expr, Box<Stmt>),
    Fun(Token, Vec<Token>, Vec<Stmt>),
    Class(Token, Vec<Stmt>),
}
