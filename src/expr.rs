use crate::token::{Kind, Token};
use core::fmt;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum ExprType {
    Literal,
    Identifier,
    Binary,
    Unary,
    Logical,
    Grouping,
    Call,
    Assign,
    Get,
    This,
    Super,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprType,
    pub token: Token,
    pub left: Option<Box<Expr>>,
    pub right: Option<Box<Expr>>,
    pub list: Option<Vec<Expr>>,
}

impl Expr {
    pub fn is_logic(&self) -> bool {
        match self.kind {
            ExprType::Logical => true,
            ExprType::Literal => self.token.kind == Kind::True || self.token.kind == Kind::False,
            _ => false,
        }
    }

    fn type_str(&self) -> String {
        match self.kind {
            ExprType::Literal | ExprType::Identifier => {
                let mut val = self.token.val();
                if val == "" {
                    val = format!("{:?}", self.token.kind);
                }
                return val;
            }
            ExprType::Unary | ExprType::Binary | ExprType::Logical => {
                format!("{}", self.token.kind)
            }
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
        Kind::This => ExprType::This,
        Kind::Super => ExprType::Super,
        Kind::Identifier(_) => ExprType::Identifier,
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
        Kind::Equal => ExprType::Assign,
        Kind::DoubleEqual
        | Kind::BangEqual
        | Kind::Greater
        | Kind::GreaterEqual
        | Kind::Less
        | Kind::LessEqual
        | Kind::And
        | Kind::Or => ExprType::Logical,
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

#[derive(Debug, Clone)]
pub struct For {
    pub var: Option<Box<Stmt>>,
    pub cond: Option<Box<Stmt>>,
    pub incr: Option<Box<Stmt>>,
    pub body: Vec<Stmt>,
}

impl fmt::Display for For {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let param = [&self.var, &self.cond, &self.incr]
            .iter()
            .map(|e| match e {
                Some(exp) => format!("{exp},"),
                _ => "".to_string(),
            })
            .collect::<String>()
            .trim_end_matches(",")
            .to_string();

        write!(f, "For(<{param}>,[{}])", to_string(&self.body))
    }
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

impl fmt::Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|x| x.val() + ",")
            .collect::<String>()
            .trim_end_matches(",")
            .to_string();

        write!(
            f,
            "Fun({},<{}>,[{}])",
            self.name.val(),
            params,
            to_string(&self.body)
        )
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Token,
    pub methods: HashMap<String, Fun>,
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut methods_str = "".to_string();
        for (_, m) in self.methods.iter() {
            methods_str = format!("{},{{{m}}}", methods_str.as_str());
        }
        methods_str = methods_str.trim_start_matches(",").to_string();
        write!(f, "Class({},{})", self.name.val(), methods_str.as_str())
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Var(Token, Option<Expr>),
    Return(Expr),
    Print(Expr),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    Block(Vec<Stmt>),
    While(Expr, Vec<Stmt>),
    For(For),
    Fun(Fun),
    Class(Class),
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
            Stmt::Print(expr) => write!(f, "Print({})", expr),
            Stmt::Var(t, None) => write!(f, "Var({})", t.val()),
            Stmt::Return(expr) => write!(f, "Return({})", expr),
            Stmt::Var(t, Some(expr)) => write!(f, "Var({},{})", t.val(), expr),
            Stmt::If(expr, stmts, None) => write!(f, "If({},[{}])", expr, to_string(&stmts)),
            Stmt::If(expr, stmts, Some(elst)) => {
                write!(
                    f,
                    "If({},[{}],[{}])",
                    expr,
                    to_string(&stmts),
                    to_string(&elst)
                )
            }
            Stmt::Block(list) => write!(f, "Block({})", to_string(list)),
            Stmt::While(expr, stmts) => write!(f, "While({},[{}])", expr, to_string(&stmts)),
            Stmt::For(exp) => write!(f, "{exp}"),
            Stmt::Fun(func) => write!(f, "{func}"),
            Stmt::Class(cls) => write!(f, "{cls}"),
        }
    }
}
