use crate::env::{self, Environment, Value};
use crate::expr;
use crate::expr::{
    Expr,
    ExprType::{self, *},
    Stmt,
};
use crate::token::Kind;

pub struct Eval;

impl Eval {
    pub fn exec(stmts: &[Stmt]) -> () {}
}

fn eval_expr<'a>(exp: &Expr, ev: &'a mut Environment<'a>) -> Value<'a> {
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
        Unary => match eval_expr(exp.left.as_deref().unwrap(), ev) {
            Value::Bool(x) => Value::Bool(!x),
            Value::Num(x) => Value::Num(-x),
            _ => panic!("unexpected value"),
        },
        Binary => {
            let left = eval_expr(exp.left.as_deref().unwrap(), ev);
            let right = eval_expr(exp.right.as_deref().unwrap(), ev);
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
            let left = eval_expr(exp.left.as_deref().unwrap(), ev);
            let right = eval_expr(exp.right.as_deref().unwrap(), ev);
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
        Grouping => eval_expr(exp.left.as_deref().unwrap(), ev),
        Call => eval_call(exp.left.as_deref().unwrap(), exp.list.as_ref().unwrap(), ev),
        Assign => eval_assign(exp, ev),
        Get => eval_get(exp, ev),
        _ => Value::Nil,
    }
}

fn eval_call<'a>(callee: &Expr, params: &[Expr], ev: &'a mut Environment<'a>) -> Value<'a> {
    // find func definition
    let method = match callee.kind {
        ExprType::Identifier => ev.get(&callee.token.val()).unwrap(),
        ExprType::Get => &eval_get(callee, ev),
        _ => unreachable!(),
    };

    let mut closure = env::new(Some(ev));

    if let Value::Fun(fun) = method {
        for (index, param) in fun.params.iter().enumerate() {
            closure.set(param.val(), eval_expr(params.get(index).unwrap(), ev));
        }
        for stmt in fun.body.iter() {
            eval_stmt(stmt, &closure);
        }
    } else {
        panic!("unexpected value for function:{:?}", method)
    }
    Value::Nil
}

fn eval_get<'a>(exp: &Expr, ev: &'a mut Environment<'a>) -> Value<'a> {
    Value::Nil
}

fn eval_assign<'a>(exp: &Expr, ev: &'a mut Environment<'a>) -> Value<'a> {
    Value::Nil
}

fn eval_stmt<'a>(stmt: &expr::Stmt, ev: &'a mut Environment<'a>) -> Value<'a> {
    match stmt {
        Stmt::Expr(exp) => eval_expr(exp, ev),
        Stmt::Var(name, exp) => {
            let val = match exp {
                Some(epr) => eval_expr(epr, ev),
                _ => Value::Nil,
            };
            ev.set(name.val(), val);
            Value::Nil
        }
        Stmt::Return(epr) => Value::Nil, //TODO
        Stmt::If(cond, ifstmt, elsest) => match eval_expr(cond, ev) {
            Value::Bool(true) => {
                for st in ifstmt.iter() {
                    eval_stmt(st, ev);
                }
                Value::Nil
            }
            Value::Bool(false) => {
                if let Some(stmts) = elsest {
                    for st in stmts.iter() {
                        eval_stmt(st, ev);
                    }
                }
                Value::Nil
            }
            _ => unreachable!(),
        },
        Stmt::While(cond, stmts) => {
            loop {
                match eval_expr(cond, ev) {
                    Value::Bool(true) => (),
                    _ => break,
                }
                for stmt in stmts.iter() {
                    eval_stmt(stmt, ev);
                }
            }
            Value::Nil
        }
        Stmt::For(stmt) => {
            if let Some(st) = &stmt.var {
                eval_stmt(st, ev);
            }
            loop {
                if let Some(cond) = &stmt.cond {
                    match eval_stmt(cond, ev) {
                        Value::Bool(true) => (),
                        _ => break,
                    }
                }
                for stmt in stmt.body.iter() {
                    eval_stmt(stmt, ev);
                }
            }
            Value::Nil
        }
        Stmt::Block(stmts) => {
            for stmt in stmts.iter() {
                eval_stmt(stmt, ev);
            }
            Value::Nil
        }
        Stmt::Fun(fun) => Value::Nil,
        Stmt::Class(cls) => Value::Nil,
    }
}
