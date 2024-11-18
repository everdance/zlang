use crate::expr::{self, Expr, ExprType, Stmt};
use crate::token::{Kind, Kind::*, Token};

pub struct Parser;

impl Parser {
    pub fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, String> {
        let mut state = ParseState {
            tokens,
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
        let res = self.next();
        if res == None {
            return Err("unexpected end for class".to_string());
        }

        let token = res.unwrap().clone();
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
                return Err("Unexpected statement start token in class definition".to_string());
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
        let res = self.next();
        if res == None {
            return Err("unexpected end for function".to_string());
        }

        let token = res.unwrap().clone();
        match token.kind {
            Identifier(_) => {
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
                        return Err("unexpected function params end".to_string());
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
            _ => Err("expect identifer after fun".to_string()),
        }
    }

    fn var(&mut self) -> Result<Stmt, String> {
        if let Ok(epx) = self.expr() {
            match epx.kind {
                ExprType::Variable => Ok(Stmt::Var(epx.token, None)),
                ExprType::Assign => Ok(Stmt::Var(epx.token, Some(*epx.left.unwrap()))),
                _ => Err("unexpected expression after var".to_string()),
            }
        } else {
            Err("expect variable definition after var".to_string())
        }
    }

    // group 2: common control flow
    //          if, for, while, block, return
    fn stmt(&mut self) -> Result<Stmt, String> {
        if self.matches(Kind::If) {
            return self.ifstmt();
        } else if self.matches(Kind::For) {
            return self.forstmt();
        } else if self.matches(Kind::While) {
            return self.whilestmt();
        } else if self.matches(Kind::LeftBrace) {
            return self.blockstmt();
        } else {
            return self.exprstmt();
        }
    }

    fn ifstmt(&mut self) -> Result<Stmt, String> {
        if !self.matches(LeftParen) {
            return Err("expect left paren for if condition".to_string());
        }

        match self.exprstmt() {
            Ok(Stmt::Expr(cond)) => {
                if !self.matches(RightParen) {
                    return Err("expect right paren for if condition".to_string());
                }

                // block or single expression
                if let Ok(stmt) = self.exprstmt() {
                    Ok(Stmt::If(cond, Box::new(stmt)))
                } else {
                    Err("expect statement for if expression".to_string())
                }
            }
            Err(msg) => Err(msg),
            _ => Err("unreachable branch".to_string()),
        }
    }

    fn forstmt(&mut self) -> Result<Stmt, String> {
        Err("to implement".to_string())
    }

    fn whilestmt(&mut self) -> Result<Stmt, String> {
        Err("to implement".to_string())
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
            Ok(epx) => {
                if self.matches(Kind::Equal) {
                    let t = self.prev().unwrap().clone();
                    match self.expr() {
                        Ok(val) => match epx.kind {
                            ExprType::Variable => Ok(expr::single(t, val)), // assign
                            ExprType::Get => Ok(expr::double(t, epx, val)), // set
                            _ => return Err("invalid assignment".to_string()),
                        },
                        Err(e) => return Err(e),
                    }
                } else {
                    return Ok(epx);
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
            let t = self.prev().unwrap().clone();
            match self.and() {
                Ok(right) => epx = expr::double(t, epx, right),
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
            let t = self.prev().unwrap().clone();
            match self.equal() {
                Ok(right) => epx = expr::double(t, epx, right),
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
            let t = self.prev().unwrap().clone();
            match self.compare() {
                Ok(right) => epx = expr::double(t, epx, right),
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
                Ok(right) => epx = expr::double(t, epx, right),
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
                Ok(right) => epx = expr::double(t, epx, right),
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
                Ok(opr) => epx = expr::double(t, epx, opr),
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
                Ok(opr) => Ok(expr::single(t, opr)),
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
                        epx = expr::single(token, epx);
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
        let res = self.next();
        if res == None {
            return Err("unexpected end".to_string());
        }

        let token = res.unwrap().clone();

        match token.kind {
            Identifier(_) | StrLiteral(_) | NumLiteral(_) | True | False | Nil => {
                Ok(expr::literal(token))
            }

            LeftParen => match self.expr() {
                Ok(sub) => {
                    if self.matches(RightParen) {
                        Ok(expr::single(token, sub))
                    } else {
                        Err("missing right paren after expression".to_string())
                    }
                }
                Err(msg) => Err(msg),
            },

            This => Ok(expr::literal(token)),

            Super => {
                if self.matches(Dot) {
                    match self.expr() {
                        Ok(method) => Ok(expr::single(token, method)),
                        Err(msg) => Err(msg),
                    }
                } else {
                    Err("missing dot after super".to_string())
                }
            }
            _ => Err("".to_string()),
        }
    }

    fn matches(&mut self, kind: Kind) -> bool {
        if let Some(token) = self.peek() {
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
        if self.cursor < self.tokens.len() - 1 {
            self.cursor += 1;
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
