use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: Kind,
    pub line: usize,
    pub pos: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Token {
    pub fn val(&self) -> String {
        match self.kind.clone() {
            Kind::Identifier(x) | Kind::StrLiteral(x) => x,
            Kind::NumLiteral(y) => format!("{y}"),
            _ => "".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Kind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    DoubleEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(String),
    StrLiteral(String),
    NumLiteral(f64),

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Print,

    Comment,
}

impl Kind {
    pub fn type_eq(&self, other: &Kind) -> bool {
        match (self, other) {
            (Kind::Identifier(_), Kind::Identifier(_)) => true,
            (Kind::StrLiteral(_), Kind::StrLiteral(_)) => true,
            (Kind::NumLiteral(_), Kind::NumLiteral(_)) => true,
            _ => self == other,
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
