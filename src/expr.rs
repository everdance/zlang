enum ExprType {
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

struct Expr {
    pub kind: ExprType,
    pub left: Option<Expr>,
    pub right: Option<Expr>,
    pub params: Vector<Expr>,
}
