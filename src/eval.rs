use std::collections::HashMap;

use crate::env::{self, Environment, Environments, Value};
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
        let mut eval = evaluator { envs: env::new() };
        let mut global = eval.new_env(1);
        for stmt in stmts.iter() {
            eval.eval_stmt(stmt, &mut global);
        }
    }
}

struct evaluator {
    envs: Environments,
}

impl evaluator {
    fn new_env(&mut self, current: usize) -> Environment {
        return self.envs.new_stack(current);
    }

    fn eval_stmt(&mut self, stmt: &expr::Stmt, ev: &mut Environment) -> Value {
        match stmt {
            Stmt::Expr(exp) => self.eval_expr(exp, ev),
            Stmt::Var(name, exp) => {
                let val = match exp {
                    Some(epr) => self.eval_expr(epr, ev),
                    _ => Value::Nil,
                };
                ev.set(name.val(), val);
                Value::Nil
            }
            Stmt::Return(epr) => Value::Nil, //TODO
            Stmt::If(cond, ifstmt, elsest) => match self.eval_expr(cond, ev) {
                Value::Bool(true) => {
                    for st in ifstmt.iter() {
                        self.eval_stmt(st, ev);
                    }
                    Value::Nil
                }
                Value::Bool(false) => {
                    if let Some(stmts) = elsest {
                        for st in stmts.iter() {
                            self.eval_stmt(st, ev);
                        }
                    }
                    Value::Nil
                }
                _ => unreachable!(),
            },
            Stmt::While(cond, stmts) => {
                loop {
                    match self.eval_expr(cond, ev) {
                        Value::Bool(true) => (),
                        _ => break,
                    }
                    for stmt in stmts.iter() {
                        self.eval_stmt(stmt, ev);
                    }
                }
                Value::Nil
            }
            Stmt::For(stmt) => {
                if let Some(st) = &stmt.var {
                    self.eval_stmt(st, ev);
                }
                loop {
                    if let Some(cond) = &stmt.cond {
                        match self.eval_stmt(cond, ev) {
                            Value::Bool(true) => (),
                            _ => break,
                        }
                    }
                    for stmt in stmt.body.iter() {
                        self.eval_stmt(stmt, ev);
                    }
                }
                Value::Nil
            }
            Stmt::Block(stmts) => {
                for stmt in stmts.iter() {
                    self.eval_stmt(stmt, ev);
                }
                Value::Nil
            }
            Stmt::Fun(fun) => Value::Nil,
            Stmt::Class(cls) => Value::Nil,
        }
    }

    fn eval_expr(&mut self, exp: &Expr, ev: &mut Environment) -> Value {
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
                if let Some(val) = ev.get(&exp.token.val()) {
                    val.clone()
                } else {
                    panic!("undefined variable")
                }
            }
            Unary => match self.eval_expr(exp.left.as_deref().unwrap(), ev) {
                Value::Bool(x) => Value::Bool(!x),
                Value::Num(x) => Value::Num(-x),
                _ => panic!("unexpected value"),
            },
            Binary => {
                let left = self.eval_expr(exp.left.as_deref().unwrap(), ev);
                let right = self.eval_expr(exp.right.as_deref().unwrap(), ev);
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
                let left = self.eval_expr(exp.left.as_deref().unwrap(), ev);
                let right = self.eval_expr(exp.right.as_deref().unwrap(), ev);
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
            Grouping => self.eval_expr(exp.left.as_deref().unwrap(), ev),
            Call => self.eval_call(exp.left.as_deref().unwrap(), exp.list.as_ref().unwrap(), ev),
            Assign => self.eval_assign(exp, ev),
            Get => self.eval_get(exp, ev),
            _ => Value::Nil,
        }
    }

    fn eval_call(&mut self, callee: &Expr, params: &[Expr], ev: &mut Environment) -> Value {
        // find func definition
        let method = match callee.kind {
            ExprType::Identifier => ev.get(&callee.token.val()).unwrap().clone(),
            ExprType::Get => self.eval_get(callee, ev),
            _ => unreachable!(),
        };

        if let Value::Fun(fun) = method {
            let mut closure = self.new_env(ev.depth());

            for (index, param) in fun.params.iter().enumerate() {
                closure.set(param.val(), self.eval_expr(params.get(index).unwrap(), ev));
            }

            for stmt in fun.body.iter() {
                self.eval_stmt(stmt, &mut closure);
            }
        } else {
            panic!("unexpected value for function:{:?}", method)
        }
        Value::Nil
    }

    fn eval_get(&mut self, exp: &Expr, ev: &mut Environment) -> Value {
        Value::Nil
    }

    fn eval_assign(&mut self, exp: &Expr, ev: &mut Environment) -> Value {
        Value::Nil
    }
}
