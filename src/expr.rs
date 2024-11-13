use crate::token::{Token, TokenKind};

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

pub struct IfStmt {
    pub cond: Expr,
    pub thens: Vec<Stmt>,
    pub elses: Option<Vec<Stmt>>,
}

pub struct BlkStmt {
    pub list: Vec<Stmt>,
}

pub struct WhileStmt {
    pub cond: Expr,
    pub list: Vec<Stmt>,
}

pub struct FunStmt {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

pub struct ClassStmt {
    pub name: Token,
    pub methods: Vec<FunStmt>,
}

pub enum Stmt {
    Expr(Expr),
    Var(Token, Expr),
    Return(Token, Expr),
    If(IfStmt),
    Block(BlkStmt),
    While(WhileStmt),
    Fun(FunStmt),
    Class(ClassStmt),
}
