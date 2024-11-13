use crate::expr::Stmt;
use crate::token::Token;

pub struct Parser;

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, String> {
        Err("to implement".to_string())
    }
}

struct ParseState<'a> {
    tokens: &'a Vec<Token>,
    cursor: usize,
    stmts: Vec<Stmt>,
}

impl ParseState<'_> {
    fn get_stmt(&mut self) -> Result<Stmt, String> {
        Err("to implement".to_string())
    }
}
