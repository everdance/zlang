use crate::expr::{ClassStmt, FunStmt, Stmt};
use crate::token::{Token, TokenKind};

pub struct Parser;

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, String> {
        let mut state = ParseState {
            tokens: &tokens,
            cursor: 0,
            stmts: vec![],
        };

        while state.cursor < tokens.len() {
            match state.parse() {
                Ok(stmt) => state.stmts.push(stmt),
                Err(msg) => return Err(msg),
            }
        }

        Ok(state.stmts)
    }
}

struct ParseState<'a> {
    tokens: &'a Vec<Token>,
    cursor: usize,
    stmts: Vec<Stmt>,
}

/// statement ordering
/// group 1: high level language syntax
///          function, class, variable definition
/// group 2: common control flow
///          if, for, while, block, return
/// group 3: fundamental expressions (priority ascending ordered)
///          assign, set object field
///          or
///          and
///          equality
///          comparison
///          binary(+-)
///          binary(*/)
///          unary(!,-)
///          call
///          super, this, identifer, grouping
///          number, string, bool, nil

impl ParseState<'_> {
    fn parse(&mut self) -> Result<Stmt, String> {
        if let Some(token) = self.peek() {
            return match token.kind {
                TokenKind::Class => self.class(),
                TokenKind::Fun => self.func(),
                TokenKind::Var => self.var(),
                _ => self.stmt(),
            };
        }

        Err("empty tokens to parse".to_string())
    }

    fn class(&mut self) -> Result<Stmt, String> {
        Err("to implement".to_string())
    }

    fn func(&mut self) -> Result<Stmt, String> {
        Err("to implement".to_string())
    }

    fn var(&mut self) -> Result<Stmt, String> {
        Err("to implement".to_string())
    }

    fn stmt(&mut self) -> Result<Stmt, String> {
        Err("to implement".to_string())
    }

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if let Some(token) = self.peek() {
            if token.kind == kind {
                self.next();
                return true;
            }
        }

        false
    }

    fn next(&mut self) -> Option<&Token> {
        if self.cursor < self.tokens.len() {
            return self.tokens.get(self.cursor);
        }
        None
    }

    fn peek(&self) -> Option<&Token> {
        if (self.cursor + 1) < self.tokens.len() {
            return self.tokens.get(self.cursor + 1);
        }
        None
    }

    fn prev(&self) -> Option<&Token> {
        if self.cursor > 0 {
            return self.tokens.get(self.cursor - 1);
        }
        None
    }
}
