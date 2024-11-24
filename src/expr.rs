use core::fmt;

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

impl Expr {
    fn type_str(&self) -> String {
        match self.kind {
            ExprType::Literal => {
                let mut val = self.token.val();
                if val == "" {
                    val = format!("{:?}", self.token.kind);
                }
                return val;
            }
            ExprType::Unary | ExprType::Binary | ExprType::Logical => {
                format!("{}", self.token.kind)
            }
            ExprType::Variable => self.token.val(),
            _ => format!("{:?}", self.kind),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (&self.left, &self.right, &self.list) {
            (None, None, None) => write!(f, "{}", self.type_str()),
            (Some(left), None, None) => write!(f, "{}({})", self.type_str(), left),
            (Some(left), Some(right), None) => {
                write!(f, "{}({}, {})", self.type_str(), left, right)
            }
            (Some(left), None, Some(list)) => {
                let list_str = list
                    .iter()
                    .map(|x| x.to_string() + ",")
                    .collect::<String>()
                    .trim_end_matches(",")
                    .to_string();
                write!(f, "{}({}, [{}])", self.type_str(), left, list_str)
            }
            _ => write!(f, "{}", self.type_str()),
        }
    }
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
        Kind::Slash
        | Kind::Star
        | Kind::DoubleEqual
        | Kind::BangEqual
        | Kind::Greater
        | Kind::GreaterEqual
        | Kind::Less
        | Kind::LessEqual
        | Kind::Minus
        | Kind::Plus => ExprType::Binary,
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

pub fn to_string(list: &Vec<Stmt>) -> String {
    list.iter()
        .map(|x| x.to_string() + ",")
        .collect::<String>()
        .trim_end_matches(",")
        .to_string()
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Expr(expr) => write!(f, "{}", expr),
            Stmt::Var(t, None) => write!(f, "Var({})", t.val()),
            Stmt::Var(t, Some(expr)) => write!(f, "Var({},{})", t.val(), expr),
            Stmt::If(expr, stmts) => write!(f, "If({},{})", expr, stmts),
            Stmt::For(list, stmts) => write!(f, "For([{}],{})", to_string(list), stmts),
            Stmt::Block(list) => write!(f, "Block({})", to_string(list)),
            Stmt::While(expr, stmts) => write!(f, "While({},{})", expr, stmts),
            Stmt::Fun(t, tokens, stmts) => {
                let params = tokens
                    .iter()
                    .map(|x| x.val() + ",")
                    .collect::<String>()
                    .trim_end_matches(",")
                    .to_string();

                write!(f, "Func({},<{}>,[{}])", t.val(), params, to_string(stmts))
            }
            Stmt::Class(t, stmts) => write!(f, "Class({},[{}])", t.val(), to_string(stmts)),
        }
    }
}
