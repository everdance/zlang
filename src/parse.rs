use crate::expr::Expr;
use crate::token::Token;

pub struct Parser;

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Expr, String> {
        Err("to implement".to_string())
    }
}
