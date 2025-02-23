use std::collections::HashMap;

use crate::expr::{self, Expr, ExprType, Stmt};
use crate::scan::Scanner;
use crate::token::{
    Kind::{self, *},
    Token,
};

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
            cursor: 0,
            fun_level: 0,
            tokens: &tokens,
        };

        state.parse()
    }
}

struct ParseState<'a> {
    tokens: &'a [Token],
    cursor: usize,
    fun_level: usize,
}

impl<'a> ParseState<'a> {
    fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = vec![];

        while self.cursor < self.tokens.len() {
            if self.matches_any(vec![Kind::Comment, Kind::Semicolon]) {
                continue;
            }

            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(msg) => return Err(msg),
            }
        }

        Ok(stmts)
    }
    // group 1: high level language syntax
    //          function, class, variable definition
    fn parse_stmt(&mut self) -> Result<Stmt, String> {
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
        let name: Token;
        let mut parent: Option<String> = None;

        if self.matches_identifier() {
            name = self.prev().unwrap().clone();
        } else {
            return Err(format!(
                "expect identifier, got {:?}",
                self.cur().unwrap().clone()
            ));
        }

        if self.matches(Extends) {
            if self.matches_identifier() {
                parent = Some(self.prev().unwrap().val());
            } else {
                return Err(format!(
                    "expect class parent identifier, got {:?}",
                    self.cur().unwrap().clone()
                ));
            }
        }

        if !self.matches(LeftBrace) {
            return Err(format!(
                "expect left brace, got {:?}",
                self.cur().unwrap().clone()
            ));
        }

        let mut methods = HashMap::new();
        loop {
            if self.matches(RightBrace) {
                break;
            }

            if self.matches(Kind::Fun) {
                match self.func() {
                    Ok(Stmt::Fun(func)) => methods.insert(func.name.val(), func),
                    Ok(_) => unreachable!(),
                    Err(msg) => {
                        return Err(msg);
                    }
                };
            } else {
                return Err(format!(
                    "unexpected {:?} in class definition",
                    self.cur().unwrap()
                ));
            }
        }

        Ok(Stmt::Class(expr::Class {
            name,
            parent,
            methods,
        }))
    }

    fn func(&mut self) -> Result<Stmt, String> {
        let name: Token;

        if self.matches_identifier() {
            name = self.prev().unwrap().clone();
        } else {
            return Err(format!(
                "expect function identifier, got {:?}",
                self.cur().unwrap().clone()
            ));
        }

        if !self.matches(LeftParen) {
            return Err(format!(
                "expect left paren, got {:?}",
                self.cur().unwrap().clone()
            ));
        }

        let mut params = vec![];
        loop {
            if self.matches(RightParen) {
                break;
            }

            if self.matches(Comma) {
                continue;
            }

            if self.matches_identifier() {
                params.push(self.prev().unwrap().clone());
            } else {
                return Err(format!(
                    "unexpected {:?} for function parameter",
                    self.cur().unwrap().clone()
                ));
            }
        }

        if !self.matches(LeftBrace) {
            return Err(format!(
                "expect left brace, got {:?}",
                self.cur().unwrap().clone()
            ));
        }

        self.fun_level += 1;

        let mut body = vec![];
        loop {
            if self.matches(RightBrace) {
                break;
            }

            if self.matches(Semicolon) {
                continue;
            }

            match self.stmt() {
                Ok(st) => body.push(st),
                Err(msg) => return Err(msg),
            }
        }

        self.fun_level -= 1;

        Ok(Stmt::Fun(expr::Fun { name, params, body }))
    }

    fn var(&mut self) -> Result<Stmt, String> {
        match self.expr() {
            Ok(epx) => match epx.kind {
                ExprType::Identifier => Ok(Stmt::Var(epx.token, None)),
                ExprType::Assign => Ok(Stmt::Var(
                    epx.left.unwrap().token,
                    Some(*epx.right.unwrap()),
                )),
                _ => Err(format!("unexpected expression after var: {}", epx)),
            },
            Err(msg) => Err(msg),
        }
    }

    // group 2: common control flow
    //          if, for, while, block, return
    fn stmt(&mut self) -> Result<Stmt, String> {
        if self.matches(Kind::If) {
            return self.ifelsestmt();
        } else if self.matches(Kind::For) {
            return self.forstmt();
        } else if self.matches(Kind::While) {
            return self.whilestmt();
        } else if self.matches(Kind::LeftBrace) {
            return self.blockstmt();
        } else if self.matches(Kind::Return) {
            return self.retstmt();
        } else if self.matches(Kind::Var) {
            return self.var();
        } else if self.matches(Kind::Print) {
            return self.printstmt();
        } else {
            return self.exprstmt();
        }
    }

    fn forstmt(&mut self) -> Result<Stmt, String> {
        if !self.matches(LeftParen) {
            return Err(format!(
                "expect left paren, got {:?}",
                self.cur().unwrap().clone()
            ));
        }

        let mut var = None;
        let mut cond = None;
        let mut incr = None;

        if self.matches(Var) {
            match self.var() {
                Ok(def) => var = Some(Box::new(def)),
                Err(msg) => return Err(msg),
            }
        }

        if !self.matches(Semicolon) {
            return Err(format!(
                "expect semicolon after initializer, got {:?}",
                self.cur().unwrap().clone()
            ));
        }

        if !self.matches(Semicolon) {
            match self.exprstmt() {
                Ok(epx) => {
                    match &epx {
                        Stmt::Expr(condition) => {
                            if !condition.is_logic() {
                                return Err(format!("expect logic expression, got {}", condition));
                            }
                            cond = Some(Box::new(condition.clone()));
                        }
                        _ => unreachable!(),
                    };
                }
                Err(msg) => return Err(msg),
            };
            if !self.matches(Semicolon) {
                return Err(format!(
                    "expect semicolon after condition, got {:?}",
                    self.cur().unwrap().clone()
                ));
            }
        }

        if !self.matches(RightParen) {
            match self.exprstmt() {
                Ok(epx) => incr = Some(Box::new(epx)),
                Err(msg) => return Err(msg),
            };
            if !self.matches(RightParen) {
                return Err(format!(
                    "expect right paren, got {:?}",
                    self.cur().unwrap().clone()
                ));
            }
        }

        self.block_or_expr().map(|body| {
            Stmt::For(expr::For {
                var,
                cond,
                incr,
                body,
            })
        })
    }

    fn ifelsestmt(&mut self) -> Result<Stmt, String> {
        if !self.matches(LeftParen) {
            return Err(format!(
                "expect left paren, got {:?}",
                self.cur().unwrap().clone()
            ));
        }

        let cond: Expr;
        let ifstmt: Vec<Stmt>;
        match self.exprstmt() {
            Ok(Stmt::Expr(c)) => {
                cond = c;
                if !cond.is_logic() {
                    return Err(format!("expect logic expression, got {}", cond));
                }

                if !self.matches(RightParen) {
                    return Err(format!(
                        "expect right paren, got {:?}",
                        self.cur().unwrap().clone()
                    ));
                }

                ifstmt = self.block_or_expr()?;
            }
            Err(msg) => return Err(msg),
            _ => return Err("unreachable".to_string()),
        };

        if !self.matches(Else) {
            return Ok(Stmt::If(cond, ifstmt, None));
        }

        if self.matches(If) {
            self.ifelsestmt()
                .map(|elsestmt| Stmt::If(cond, ifstmt, Some(vec![elsestmt])))
        } else {
            self.block_or_expr()
                .map(|elsestmt| Stmt::If(cond, ifstmt, Some(elsestmt)))
        }
    }

    fn whilestmt(&mut self) -> Result<Stmt, String> {
        if !self.matches(LeftParen) {
            return Err(format!(
                "expect left paren, got {:?}",
                self.cur().unwrap().clone()
            ));
        }

        match self.exprstmt()? {
            Stmt::Expr(cond) => {
                if !cond.is_logic() {
                    return Err(format!("expect logic expression, got {}", cond));
                }

                if !self.matches(RightParen) {
                    return Err(format!(
                        "expect right paren, got {:?}",
                        self.cur().unwrap().clone()
                    ));
                }

                self.block_or_expr().map(|stmt| Stmt::While(cond, stmt))
            }
            _ => Err("unreachable branch".to_string()),
        }
    }

    fn block_or_expr(&mut self) -> Result<Vec<Stmt>, String> {
        if self.matches(Kind::LeftBrace) {
            self.blockstmt().map(|stmt| match stmt {
                Stmt::Block(v) => v,
                _ => unreachable!(),
            })
        } else {
            self.exprstmt().map(|stmt| vec![stmt])
        }
    }

    fn blockstmt(&mut self) -> Result<Stmt, String> {
        let mut stmts = vec![];

        loop {
            if self.matches(RightBrace) {
                break;
            }

            if self.matches(Semicolon) {
                continue;
            }

            stmts.push(self.stmt()?);
        }

        Ok(Stmt::Block(stmts))
    }

    fn retstmt(&mut self) -> Result<Stmt, String> {
        if self.fun_level == 0 {
            return Err("Return statement only allowed in function body".to_string());
        }

        let token = self.prev().unwrap().clone();
        self.expr()
            .map(|expr| Stmt::Return(expr))
            .map_err(|e| format!("{} at {:?}", e, token))
    }

    fn printstmt(&mut self) -> Result<Stmt, String> {
        let token = self.prev().unwrap().clone();
        self.expr()
            .map(|expr| Stmt::Print(expr))
            .map_err(|e| format!("{} at {:?}", e, token))
    }

    fn exprstmt(&mut self) -> Result<Stmt, String> {
        self.expr().map(|expr| Stmt::Expr(expr))
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
        let left = self.or()?;
        if self.matches(Kind::Equal) {
            let eq = self.prev().unwrap().clone();
            let val = self.expr()?;
            match left.kind {
                ExprType::Identifier | ExprType::Get => Ok(expr::binary(eq, left, val)),
                _ => return Err(format!("invalid left hand value {}", left)),
            }
        } else {
            Ok(left)
        }
    }

    fn or(&mut self) -> Result<Expr, String> {
        let mut epx = self.and()?;

        while self.matches(Or) {
            if !epx.is_logic() {
                return Err(format!(
                    "expect logic expression on left operand, got {}",
                    epx
                ));
            }

            let token = self.prev().unwrap().clone();
            let right = self.and()?;
            if !right.is_logic() {
                return Err(format!(
                    "expect logic expression on right operand, got {}",
                    right
                ));
            }
            epx = expr::binary(token, epx, right);
        }

        Ok(epx)
    }

    fn and(&mut self) -> Result<Expr, String> {
        let mut epx = self.equal()?;
        while self.matches(And) {
            if !epx.is_logic() {
                return Err(format!(
                    "expect logic expression on left operand, got {}",
                    epx
                ));
            }

            let token = self.prev().unwrap().clone();
            let right = self.equal()?;
            if !right.is_logic() {
                return Err(format!(
                    "expect logic expression on right operand, got {}",
                    right
                ));
            }
            epx = expr::binary(token, epx, right);
        }

        Ok(epx)
    }

    // ==
    fn equal(&mut self) -> Result<Expr, String> {
        let mut epx = self.compare()?;
        while self.matches_any(vec![BangEqual, DoubleEqual]) {
            let token = self.prev().unwrap().clone();
            epx = expr::binary(token, epx, self.compare()?);
        }

        Ok(epx)
    }

    // >,<,>=,<=
    fn compare(&mut self) -> Result<Expr, String> {
        let mut epx = self.term()?;
        while self.matches_any(vec![Greater, GreaterEqual, Less, LessEqual]) {
            let token = self.prev().unwrap().clone();
            epx = expr::binary(token, epx, self.term()?);
        }

        Ok(epx)
    }

    // +,-
    fn term(&mut self) -> Result<Expr, String> {
        let mut epx = self.factor()?;
        while self.matches_any(vec![Minus, Plus]) {
            let token = self.prev().unwrap().clone();
            epx = expr::binary(token, epx, self.factor()?);
        }

        Ok(epx)
    }

    // *,/
    fn factor(&mut self) -> Result<Expr, String> {
        let mut epx = self.unary()?;
        while self.matches_any(vec![Slash, Star]) {
            let token = self.prev().unwrap().clone();
            epx = expr::binary(token, epx, self.unary()?);
        }

        Ok(epx)
    }

    // !,-
    fn unary(&mut self) -> Result<Expr, String> {
        if self.matches_any(vec![Bang, Minus]) {
            let token = self.prev().unwrap().clone();
            return self.unary().map(|opr| expr::unary(token, opr));
        }

        self.call()
    }

    // call
    fn call(&mut self) -> Result<Expr, String> {
        let mut epx = self.primary()?;
        loop {
            if self.matches(LeftParen) {
                epx = self.make_call(epx, self.prev().unwrap().clone())?;
            } else if self.matches(Dot) {
                let dot = self.prev().unwrap().clone();
                if self.matches_identifier() {
                    let right = expr::single(self.prev().unwrap().clone());
                    epx = expr::binary(dot, epx, right);
                } else {
                    return Err(format!(
                        "expect identifier, got {:?}",
                        self.cur().unwrap().clone()
                    ));
                }
            } else {
                break;
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
            args.push(self.expr()?);

            if self.matches(Comma) {
                continue;
            }

            if self.matches(RightParen) {
                break;
            }
        }

        Ok(expr::list(t, callee, args))
    }

    // literals
    fn primary(&mut self) -> Result<Expr, String> {
        let res = self.cur();
        if res == None {
            return Err(format!(
                "expect token after {:?}",
                self.prev().unwrap().clone()
            ));
        }

        let token = res.unwrap().clone();
        self.next();

        match token.kind {
            Identifier(_) | StrLiteral(_) | NumLiteral(_) | This | Super | Print | True | False
            | Nil => Ok(expr::single(token)),

            LeftParen => match self.expr() {
                Ok(sub) => {
                    if self.matches(RightParen) {
                        Ok(expr::unary(token, sub))
                    } else {
                        return Err(format!(
                            "expect right paren after {:?}",
                            self.prev().unwrap()
                        ));
                    }
                }
                Err(msg) => Err(msg),
            },

            _ => Err(format!("unexpected primary:{:?}", token)),
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

    fn matches_identifier(&mut self) -> bool {
        return self.matches(Identifier("".to_string()));
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
    fn expr() {
        let s = "(x = (y == 3/2 or 2 != 1))";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(
                    to_string(&stmts),
                    "Grouping(Assign(x, Grouping(Or(DoubleEqual(y, Slash(3, 2)), BangEqual(2, 1)))))"
                )
            }
            Err(msg) => assert!(false, "parse expr err:{}", msg),
        }
    }

    #[test]
    fn expr_super() {
        let s = "y == 3/2 or 2 != 1 and z > x or a != \"decode\"";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(
                    to_string(&stmts),
                    "Or(Or(DoubleEqual(y, Slash(3, 2)), And(BangEqual(2, 1), Greater(z, x))), BangEqual(a, decode))"
                )
            }
            Err(msg) => assert!(false, "parse expr err:{}", msg),
        }
    }

    #[test]
    fn lhv_err() {
        let s = "2 = 1";
        match Parser::parse(s) {
            Ok(_) => {
                assert!(false, "expect left hand value error")
            }
            Err(msg) => assert_eq!(msg, "invalid left hand value 2"),
        }
    }

    #[test]
    fn expr_err() {
        let s = "x = (y == 3/2 or 2 != )";
        match Parser::parse(s) {
            Ok(_) => {
                assert!(false, "expect expression error")
            }
            Err(msg) => assert_eq!(
                msg,
                "unexpected primary:Token { kind: RightParen, line: 0, pos: 22 }"
            ),
        }
    }

    #[test]
    fn logic_err() {
        let s = "y == 0 or (x = 1)";
        match Parser::parse(s) {
            Ok(_) => {
                assert!(false, "expect logic error")
            }
            Err(msg) => assert_eq!(
                msg,
                "expect logic expression on right operand, got Grouping(Assign(x, 1))"
            ),
        }
    }

    #[test]
    fn var_def() {
        let s = "var x; print x";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(to_string(&stmts), "Var(x),Print(x)")
            }
            Err(msg) => assert!(false, "parse var err:{}", msg),
        }
    }

    #[test]
    fn var_def_with_assign() {
        let s = "var x = 1; x = x+1";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(to_string(&stmts), "Var(x,1),Assign(x, Plus(x, 1))")
            }
            Err(msg) => assert!(false, "parse var err:{}", msg),
        }
    }

    #[test]
    fn var_err() {
        let s = "var 1;";
        match Parser::parse(s) {
            Ok(_) => {
                assert!(false, "should error on var def")
            }
            Err(msg) => assert_eq!(msg, "unexpected expression after var: 1"),
        }
    }

    #[test]
    fn return_err() {
        let s = "if (x == 1) {return true;}";
        match Parser::parse(s) {
            Ok(_) => {
                assert!(false, "should error on unexpected return")
            }
            Err(msg) => assert_eq!(msg, "Return statement only allowed in function body"),
        }
    }

    #[test]
    fn if_def() {
        let s = "if (x == 1) y = 2;";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(to_string(&stmts), "If(DoubleEqual(x, 1),[Assign(y, 2)])")
            }
            Err(msg) => assert!(false, "parse if err:{}", msg),
        }
    }

    #[test]
    fn if_else_def() {
        let s = "if (x != y) { y = 2 } else if (x > 3) { y = 3} else {y =5};";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(
                    to_string(&stmts),
                    "If(BangEqual(x, y),[Assign(y, 2)],[If(Greater(x, 3),[Assign(y, 3)],[Assign(y, 5)])])")
            }
            Err(msg) => assert!(false, "parse if else err:{}", msg),
        }
    }

    #[test]
    fn if_err() {
        let s = "if (x = 1) y = 2;";
        match Parser::parse(s) {
            Ok(_) => {
                assert!(false, "should err on logic condition")
            }
            Err(msg) => assert_eq!(msg, "expect logic expression, got Assign(x, 1)"),
        }
    }

    #[test]
    fn while_def() {
        let s = "while (true) { a = 1; x = y * 2}";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(
                    to_string(&stmts),
                    "While(True,[Assign(a, 1),Assign(x, Star(y, 2))])"
                )
            }
            Err(msg) => assert!(false, "parse while err:{}", msg),
        }
    }

    #[test]
    fn block_def() {
        let s = "{ var a = 1; if (x == 3) x = y * 2; }";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(
                    to_string(&stmts),
                    "Block(Var(a,1),If(DoubleEqual(x, 3),[Assign(x, Star(y, 2))]))"
                )
            }
            Err(msg) => assert!(false, "parse while err:{}", msg),
        }
    }

    #[test]
    fn for_def() {
        let s = "for (var x = 0; x <= 10; x = x + 1) y = y - 2;";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(to_string(&stmts),
                           "For(<Var(x,0),Assign(x, Plus(x, 1)):LessEqual(x, 10)>,[Assign(y, Minus(y, 2))])")
            }
            Err(msg) => assert!(false, "parse while err:{}", msg),
        }
    }

    #[test]
    fn for_empty_def() {
        let s = "for (;;) {}";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(to_string(&stmts), "For(<:>,[])")
            }
            Err(msg) => assert!(false, "parse for err:{}", msg),
        }
    }

    #[test]
    fn fun_def() {
        let s = "fun multiply(x,y) { return x*y }; multiply(3*5)";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(
                    to_string(&stmts),
                    "Fun(multiply,<x,y>,[Return(Star(x, y))]),Call(multiply, [Star(3, 5)])"
                )
            }
            Err(msg) => assert!(false, "parse fun err:{}", msg),
        }
    }

    #[test]
    fn class_def() {
        let s = "class Test extends Object { fun set(x) { this.x = super.x; return this } }";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(
                    to_string(&stmts),
                    "Class(Test<:Object,{Fun(set,<x>,[Assign(Get(This, x), Get(Super, x)),Return(This)])})"
                )
            }
            Err(msg) => assert!(false, "parse class err:{}", msg),
        }
    }
}
