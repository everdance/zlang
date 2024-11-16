use crate::token::{Kind, Token};

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

pub struct Expr {
    pub kind: ExprType,
    pub token: Token,
    pub left: Option<Box<Expr>>,
    pub right: Option<Box<Expr>>,
    pub list: Option<Vec<Expr>>,
}

pub fn literal(t: Token) -> Expr {
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

pub fn single(t: Token, opr: Expr) -> Expr {
    let kind = match t.kind {
        Kind::Minus | Kind::Bang => ExprType::Unary,
        Kind::LeftParen => ExprType::Grouping,
        Kind::Equal => ExprType::Assign,
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

pub fn double(t: Token, left: Expr, right: Expr) -> Expr {
    let kind = match t.kind {
        Kind::Slash | Kind::Star | Kind::Minus | Kind::Plus => ExprType::Binary,
        Kind::Dot => ExprType::Get,
        Kind::Equal => ExprType::Set,
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

pub enum Stmt {
    Expr(Expr),
    Var(Token, Expr),
    Return(Token, Expr),
    If(Expr, Box<Stmt>),
    Block(Vec<Stmt>),
    While(Expr, Box<Stmt>),
    Fun(Token, Vec<Token>, Vec<Stmt>),
    Class(Token, Vec<Stmt>),
}
