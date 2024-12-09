use std::collections::HashMap;

use crate::env::{self, Environments, Value};
use crate::expr;
use crate::expr::{
    Expr,
    ExprType::{self, *},
    Stmt,
};
use crate::token::Kind;

pub struct Eval;

impl Eval {
    pub fn exec(stmts: &[Stmt]) -> () {
        let mut eval = Evaluator { envs: env::new() };
        for stmt in stmts.iter() {
            eval.stmt(stmt);
        }
    }
}

struct Evaluator {
    envs: Environments,
}

impl Evaluator {
    fn pop(&mut self) {
        self.envs.pop();
    }

    fn new_env(&mut self, ev: Option<HashMap<String, Value>>) {
        return self.envs.push(ev);
    }

    fn set_val(&mut self, id: String, val: Value) {
        self.envs.set(id, val);
    }

    fn get_val(&mut self, id: &str) -> Option<&Value> {
        self.envs.get(id)
    }

    fn stmt(&mut self, stmt: &expr::Stmt) -> Value {
        match stmt {
            Stmt::Expr(exp) => self.expr(exp),
            Stmt::Var(name, exp) => {
                let val = match exp {
                    Some(epr) => self.expr(epr),
                    _ => Value::Nil,
                };
                self.set_val(name.val(), val);
                Value::Nil
            }
            Stmt::Return(epr) => Value::Nil, //TODO
            Stmt::If(cond, ifstmt, elsest) => match self.expr(cond) {
                Value::Bool(true) => {
                    for st in ifstmt.iter() {
                        self.stmt(st);
                    }
                    Value::Nil
                }
                Value::Bool(false) => {
                    if let Some(stmts) = elsest {
                        for st in stmts.iter() {
                            self.stmt(st);
                        }
                    }
                    Value::Nil
                }
                _ => unreachable!(),
            },
            Stmt::While(cond, stmts) => {
                loop {
                    match self.expr(cond) {
                        Value::Bool(true) => (),
                        _ => break,
                    }
                    for stmt in stmts.iter() {
                        self.stmt(stmt);
                    }
                }
                Value::Nil
            }
            Stmt::For(stmt) => {
                if let Some(st) = &stmt.var {
                    self.stmt(st);
                }
                loop {
                    if let Some(cond) = &stmt.cond {
                        match self.stmt(cond) {
                            Value::Bool(true) => (),
                            _ => break,
                        }
                    }
                    for stmt in stmt.body.iter() {
                        self.stmt(stmt);
                    }
                }
                Value::Nil
            }
            Stmt::Block(stmts) => {
                for stmt in stmts.iter() {
                    self.stmt(stmt);
                }
                Value::Nil
            }
            Stmt::Fun(fun) => Value::Nil,
            Stmt::Class(cls) => Value::Nil,
        }
    }

    fn expr(&mut self, exp: &Expr) -> Value {
        match exp.kind {
            Literal => match exp.token.kind.clone() {
                Kind::NumLiteral(x) => Value::Num(x),
                Kind::StrLiteral(x) => Value::Str(x),
                Kind::True => Value::Bool(true),
                Kind::False => Value::Bool(false),
                Kind::Nil => Value::Nil,
                _ => unreachable!(),
            },
            Identifier => {
                if let Some(val) = self.get_val(&exp.token.val()) {
                    val.clone()
                } else {
                    panic!("undefined variable")
                }
            }
            Unary => match self.expr(exp.left.as_deref().unwrap()) {
                Value::Bool(x) => Value::Bool(!x),
                Value::Num(x) => Value::Num(-x),
                _ => panic!("unexpected value"),
            },
            Binary => {
                let left = self.expr(exp.left.as_deref().unwrap());
                let right = self.expr(exp.right.as_deref().unwrap());
                match (left, right) {
                    (Value::Num(x), Value::Num(y)) => match exp.token.kind {
                        Kind::Star => Value::Num(x * y),
                        Kind::Slash => Value::Num(x / y),
                        Kind::Plus => Value::Num(x + y),
                        Kind::Minus => Value::Num(x - y),
                        Kind::Greater => Value::Bool(x > y),
                        Kind::GreaterEqual => Value::Bool(x >= y),
                        Kind::Less => Value::Bool(x < y),
                        Kind::LessEqual => Value::Bool(x <= y),
                        _ => panic!("unexpected binary token"),
                    },
                    (_, _) => panic!("unexpected value for binary"),
                }
            }
            Logical => {
                let left = self.expr(exp.left.as_deref().unwrap());
                let right = self.expr(exp.right.as_deref().unwrap());
                match (left, right) {
                    (Value::Bool(x), Value::Bool(y)) => {
                        if exp.token.kind == Kind::Or {
                            Value::Bool(x || y)
                        } else if exp.token.kind == Kind::And {
                            Value::Bool(x && y)
                        } else {
                            panic!("unexpected logic token")
                        }
                    }
                    (_, _) => panic!("unexpected value for logic"),
                }
            }
            Grouping => self.expr(exp.left.as_deref().unwrap()),
            Call => self.call(exp.left.as_deref().unwrap(), exp.list.as_ref().unwrap()),
            Assign => self.assign(exp),
            Get => self.get(exp),
            _ => Value::Nil,
        }
    }

    fn call(&mut self, callee: &Expr, params: &[Expr]) -> Value {
        // find func definition
        let method = match callee.kind {
            ExprType::Identifier => self.get_val(&callee.token.val()).unwrap().clone(),
            ExprType::Get => self.get(callee),

            _ => unreachable!(),
        };

        if let Value::Fun(fun) = method {
            let mut closure = HashMap::new();
            for (index, param) in fun.params.iter().enumerate() {
                closure.insert(param.val(), self.expr(params.get(index).unwrap()));
            }
            self.new_env(Some(closure));

            for stmt in fun.body.iter() {
                self.stmt(stmt);
            }
        } else {
            panic!("unexpected value for function:{:?}", method)
        }
        Value::Nil
    }

    fn get(&mut self, exp: &Expr) -> Value {
        Value::Nil
    }

    fn assign(&mut self, exp: &Expr) -> Value {
        Value::Nil
    }
}
