use crate::expr::{self, Expr, ExprType, Stmt};
use crate::scan::Scanner;
use crate::token::{Kind, Kind::*, Token};

pub struct Parser;

impl Parser {
    pub fn parse(s: &str) -> Result<Vec<Stmt>, String> {
        let (tokens, issues) = Scanner::scan(s);

        if !issues.is_empty() {
            let msg = issues
                .iter()
                .map(|i| i.to_string() + ",")
                .collect::<String>()
                .trim_end_matches(",")
                .to_string();
            return Err(msg);
        }

        let mut state = ParseState {
            tokens: &tokens,
            cursor: 0,
            stmts: vec![],
        };

        while state.cursor < tokens.len() {
            if state.matches_any(vec![Kind::Comment, Kind::Semicolon]) {
                continue;
            }

            match state.parse() {
                Ok(stmt) => state.stmts.push(stmt),
                Err(msg) => return Err(msg),
            }
        }

        Ok(state.stmts)
    }
}

struct ParseState<'a> {
    tokens: &'a [Token],
    cursor: usize,
    stmts: Vec<Stmt>,
}

impl ParseState<'_> {
    // group 1: high level language syntax
    //          function, class, variable definition
    fn parse(&mut self) -> Result<Stmt, String> {
        if self.matches(Kind::Class) {
            self.class()
        } else if self.matches(Kind::Fun) {
            self.func()
        } else if self.matches(Kind::Var) {
            self.var()
        } else {
            self.stmt()
        }
    }

    fn class(&mut self) -> Result<Stmt, String> {
        let token: Token;

        if self.matches(Identifier("".to_string())) {
            token = self.prev().unwrap().clone();
        } else {
            return Err("expect class identifier".to_string());
        }

        if !self.matches(LeftBrace) {
            return Err("expect left brace for class definition".to_string());
        }

        let mut stmts = vec![];
        loop {
            if self.matches(RightBrace) {
                break;
            }

            let res;

            if self.matches(Kind::Var) {
                res = self.var();
            } else if self.matches(Kind::Fun) {
                res = self.func();
            } else {
                let token = self.cur().unwrap();
                return Err(format!("unexpected token {} in class definition", token).to_string());
            }

            if let Ok(stmt) = res {
                stmts.push(stmt);
            } else {
                return res;
            }
        }

        Ok(Stmt::Class(token, stmts))
    }

    fn func(&mut self) -> Result<Stmt, String> {
        let token: Token;

        if self.matches(Identifier("".to_string())) {
            token = self.prev().unwrap().clone();
        } else {
            return Err("expect fun identifier".to_string());
        }

        if !self.matches(LeftParen) {
            return Err("expect left paren for argument list".to_string());
        }

        let mut params = vec![];
        loop {
            if self.matches(RightParen) {
                break;
            }

            if self.matches(Comma) {
                continue;
            }

            if let Some(param) = self.next() {
                params.push(param.clone());
            } else {
                return Err("unexpected end of paramater definition".to_string());
            }
        }

        if !self.matches(LeftBrace) {
            return Err("expect left brace for function body".to_string());
        }

        let mut stmts = vec![];

        loop {
            if self.matches(RightBrace) {
                break;
            }
            match self.stmt() {
                Ok(st) => stmts.push(st),
                Err(msg) => return Err(msg),
            }
        }
        Ok(Stmt::Fun(token, params, stmts))
    }

    fn var(&mut self) -> Result<Stmt, String> {
        match self.expr() {
            Ok(epx) => match epx.kind {
                ExprType::Variable => Ok(Stmt::Var(epx.token, None)),
                ExprType::Assign => Ok(Stmt::Var(
                    epx.left.unwrap().token,
                    Some(*epx.right.unwrap()),
                )),
                x => Err(format!("unexpected expression after var: {:?}", x).to_string()),
            },
            Err(msg) => Err(msg),
        }
    }

    // group 2: common control flow
    //          if, for, while, block, return
    fn stmt(&mut self) -> Result<Stmt, String> {
        if self.matches(Kind::If) {
            match self.ifwhilestmt() {
                Ok((epr, stmt)) => Ok(Stmt::If(epr, Box::new(stmt))),
                Err(msg) => Err(msg),
            }
        } else if self.matches(Kind::For) {
            return self.forstmt();
        } else if self.matches(Kind::While) {
            match self.ifwhilestmt() {
                Ok((epr, stmt)) => Ok(Stmt::While(epr, Box::new(stmt))),
                Err(msg) => Err(msg),
            }
        } else if self.matches(Kind::LeftBrace) {
            return self.blockstmt();
        } else {
            return self.exprstmt();
        }
    }

    fn forstmt(&mut self) -> Result<Stmt, String> {
        if !self.matches(LeftParen) {
            return Err("expect left paren for if condition".to_string());
        }

        let mut exprs = vec![];
        loop {
            if self.matches(RightParen) {
                break;
            }

            if self.matches(Semicolon) {
                continue;
            }

            if self.matches(Var) {
                if exprs.len() != 0 {
                    return Err("unexpected var definition in for".to_string());
                }

                match self.var() {
                    Ok(def) => exprs.push(def),
                    Err(msg) => return Err(msg),
                }
            } else {
                match self.exprstmt() {
                    Ok(epx) => exprs.push(epx),
                    Err(msg) => return Err(msg),
                }
            }
        }

        if exprs.len() != 1 && exprs.len() != 3 {
            return Err("unexpected for expression conditions".to_string());
        }

        if let Ok(stmt) = self.exprstmt() {
            Ok(Stmt::For(exprs, Box::new(stmt)))
        } else {
            Err("expect statement".to_string())
        }
    }

    fn ifwhilestmt(&mut self) -> Result<(Expr, Stmt), String> {
        if !self.matches(LeftParen) {
            return Err("expect left paren".to_string());
        }

        match self.exprstmt() {
            Ok(Stmt::Expr(cond)) => {
                if !self.matches(RightParen) {
                    return Err("expect right paren".to_string());
                }

                // block or single expression
                if let Ok(stmt) = self.exprstmt() {
                    Ok((cond, stmt))
                } else {
                    Err("expect statement".to_string())
                }
            }
            Err(msg) => Err(msg),
            _ => Err("unreachable branch".to_string()),
        }
    }

    fn blockstmt(&mut self) -> Result<Stmt, String> {
        let mut stmts = vec![];

        loop {
            if self.matches(RightBrace) {
                break;
            }

            match self.stmt() {
                Ok(st) => stmts.push(st),
                Err(msg) => return Err(msg),
            }
        }

        Ok(Stmt::Block(stmts))
    }

    fn exprstmt(&mut self) -> Result<Stmt, String> {
        match self.expr() {
            Ok(expr) => Ok(Stmt::Expr(expr)),
            Err(msg) => Err(msg),
        }
    }

    //  group 3: fundamental expressions (priority ascending ordered)
    //           assign, set
    //           or
    //           and
    //           equality
    //           comparison
    //           binary(+-)
    //           binary(*/)
    //           unary(!,-)
    //           call
    //           super, this, identifer, grouping
    //           number, string, bool, nil, etc
    fn expr(&mut self) -> Result<Expr, String> {
        match self.or() {
            Ok(left) => {
                if self.matches(Kind::Equal) {
                    let eq = self.prev().unwrap().clone();
                    match self.expr() {
                        Ok(val) => match val.kind {
                            ExprType::Variable | ExprType::Literal => {
                                Ok(expr::binary(eq, left, val))
                            } // assign
                            ExprType::Get => Ok(expr::binary(eq, left, val)), // set
                            k => return Err(format!("invalid type: {:?}", k).to_string()),
                        },
                        Err(e) => return Err(e),
                    }
                } else {
                    return Ok(left);
                }
            }
            Err(e) => Err(e),
        }
    }

    fn or(&mut self) -> Result<Expr, String> {
        let res = self.and();
        if let Err(msg) = res {
            return Err(msg);
        }

        let mut epx = res.unwrap();
        while self.matches(Or) {
            let or = self.prev().unwrap().clone();
            match self.and() {
                Ok(right) => epx = expr::binary(or, epx, right),
                Err(msg) => return Err(msg),
            }
        }

        Ok(epx)
    }

    fn and(&mut self) -> Result<Expr, String> {
        let res = self.equal();
        if let Err(msg) = res {
            return Err(msg);
        }

        let mut epx = res.unwrap();
        while self.matches(And) {
            let and = self.prev().unwrap().clone();
            match self.equal() {
                Ok(right) => epx = expr::binary(and, epx, right),
                Err(msg) => return Err(msg),
            }
        }

        Ok(epx)
    }

    // ==
    fn equal(&mut self) -> Result<Expr, String> {
        let res = self.compare();
        if let Err(msg) = res {
            return Err(msg);
        }

        let mut epx = res.unwrap();
        while self.matches_any(vec![BangEqual, DoubleEqual]) {
            let eqx = self.prev().unwrap().clone();
            match self.compare() {
                Ok(right) => epx = expr::binary(eqx, epx, right),
                Err(msg) => return Err(msg),
            }
        }

        Ok(epx)
    }

    // >,<,>=,<=
    fn compare(&mut self) -> Result<Expr, String> {
        let res = self.term();
        if let Err(msg) = res {
            return Err(msg);
        }

        let mut epx = res.unwrap();
        while self.matches_any(vec![Greater, GreaterEqual, Less, LessEqual]) {
            let t = self.prev().unwrap().clone();
            match self.term() {
                Ok(right) => epx = expr::binary(t, epx, right),
                Err(msg) => return Err(msg),
            }
        }

        Ok(epx)
    }

    // +,-
    fn term(&mut self) -> Result<Expr, String> {
        let res = self.factor();
        if let Err(msg) = res {
            return Err(msg);
        }

        let mut epx = res.unwrap();
        while self.matches_any(vec![Minus, Plus]) {
            let t = self.prev().unwrap().clone();
            match self.factor() {
                Ok(right) => epx = expr::binary(t, epx, right),
                Err(msg) => return Err(msg),
            }
        }

        Ok(epx)
    }

    // *,/
    fn factor(&mut self) -> Result<Expr, String> {
        let res = self.unary();
        if let Err(msg) = res {
            return Err(msg);
        }

        let mut epx = res.unwrap();
        while self.matches_any(vec![Slash, Star]) {
            let t = self.prev().unwrap().clone();
            match self.unary() {
                Ok(opr) => epx = expr::binary(t, epx, opr),
                Err(msg) => return Err(msg),
            }
        }

        Ok(epx)
    }

    // !,-
    fn unary(&mut self) -> Result<Expr, String> {
        if self.matches_any(vec![Bang, Minus]) {
            let t = self.prev().unwrap().clone();

            return match self.unary() {
                Ok(opr) => Ok(expr::unary(t, opr)),
                Err(msg) => Err(msg),
            };
        }

        self.call()
    }

    // call
    fn call(&mut self) -> Result<Expr, String> {
        let res = self.primary();
        if let Err(msg) = res {
            return Err(msg);
        }

        let mut epx = res.unwrap();
        loop {
            let suffix = self.peek();
            if suffix == None {
                break;
            }

            match suffix.unwrap().kind {
                LeftParen => match self.make_call(epx, suffix.unwrap().clone()) {
                    Ok(call) => epx = call,
                    Err(msg) => return Err(msg),
                },
                Dot => {
                    if self.matches(Identifier("".to_string())) {
                        let token = self.prev().unwrap().clone();
                        epx = expr::unary(token, epx);
                    } else {
                        return Err("Expect identifier after dot".to_string());
                    }
                }
                _ => break,
            }
        }

        Ok(epx)
    }

    fn make_call(&mut self, callee: Expr, t: Token) -> Result<Expr, String> {
        let mut args = vec![];

        if self.matches(RightParen) {
            return Ok(expr::list(t, callee, vec![]));
        }

        loop {
            if args.len() >= 255 {
                return Err("can't have more than 255 parameters".to_string());
            }
            match self.expr() {
                Ok(param) => args.push(param),
                Err(msg) => return Err(msg),
            }

            if self.matches(Comma) {
                continue;
            }
            break;
        }

        Ok(expr::list(t, callee, args))
    }

    // literals
    fn primary(&mut self) -> Result<Expr, String> {
        let res = self.cur();
        if res == None {
            return Err(format!("expect primary, cursor:{}", self.cursor).to_string());
        }

        let token = res.unwrap().clone();
        self.next();

        match token.kind {
            Identifier(_) | StrLiteral(_) | NumLiteral(_) | True | False | Nil => {
                Ok(expr::single(token))
            }

            LeftParen => match self.expr() {
                Ok(sub) => {
                    if self.matches(RightParen) {
                        Ok(expr::unary(token, sub))
                    } else {
                        Err("missing right paren after expression".to_string())
                    }
                }
                Err(msg) => Err(msg),
            },

            This => Ok(expr::single(token)),

            Super => {
                if self.matches(Dot) {
                    match self.expr() {
                        Ok(method) => Ok(expr::unary(token, method)),
                        Err(msg) => Err(msg),
                    }
                } else {
                    Err("missing dot after super".to_string())
                }
            }

            x => Err(format!("unexpected primary token:{}", x).to_string()),
        }
    }

    fn matches(&mut self, kind: Kind) -> bool {
        if let Some(token) = self.cur() {
            if token.kind.type_eq(&kind) {
                self.next();
                return true;
            }
        }

        false
    }

    fn matches_any(&mut self, kinds: Vec<Kind>) -> bool {
        for k in kinds {
            if self.matches(k) {
                return true;
            }
        }
        false
    }

    fn next(&mut self) -> Option<&Token> {
        self.cursor += 1;
        if self.cursor < self.tokens.len() {
            return self.tokens.get(self.cursor);
        }
        None
    }

    fn cur(&self) -> Option<&Token> {
        return self.tokens.get(self.cursor);
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

#[cfg(test)]
mod tests {
    use super::*;
    use expr::to_string;

    #[test]
    fn var_def() {
        let s = "var x = 1;// var definition";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(to_string(&stmts), "Var(Identifier(\"x\"),Literal(1))")
            }
            Err(msg) => assert!(false, "parse var err:{}", msg),
        }
    }

    #[test]
    fn fun_def() {
        let s = "fun multiply(x,y) { x*y }";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(to_string(&stmts), "Var(Identifier(\"x\"),Literal(1))")
            }
            Err(msg) => assert!(false, "parse fun err:{}", msg),
        }
    }
}
