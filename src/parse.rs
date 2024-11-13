use crate::expr::Stmt;
use crate::token::Token;

pub struct Parser;

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, String> {
        Err("to implement".to_string())
    }
}
