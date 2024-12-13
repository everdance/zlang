use crate::env::{self, Environments, Value};
use crate::expr;
use crate::expr::{
    Expr,
    ExprType::{self, *},
    Stmt,
};
use crate::token::Kind;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Eval;

impl Eval {
    pub fn exec(stmts: &[Stmt]) -> Value {
        let mut eval = Evaluator {
            envs: env::new(),
            func_stack: vec![],
        };
        let mut ret = Value::Nil;
        for stmt in stmts.iter() {
            ret = eval.stmt(stmt);
        }

        ret
    }
}

struct Evaluator {
    envs: Environments,
    func_stack: Vec<bool>,
}

impl Evaluator {
    fn enter_func(&mut self, closure: HashMap<String, Value>) {
        self.envs.push(Some(closure));
        self.func_stack.push(false);
    }

    fn exit_func(&mut self) {
        self.envs.pop();
        self.func_stack.pop();
    }

    fn stmt(&mut self, stmt: &expr::Stmt) -> Value {
        match stmt {
            Stmt::Expr(exp) => self.expr(exp),
            Stmt::Var(name, exp) => {
                let val = match exp {
                    Some(epr) => self.expr(epr),
                    _ => Value::Nil,
                };
                self.envs.set(name.val(), val);
                Value::Nil
            }
            Stmt::Return(epx) => {
                // TODO: should move this to parser
                if self.func_stack.len() == 0 {
                    panic!("unexpected return stmt without function");
                }
                *self.func_stack.last_mut().unwrap() = true;
                self.expr(epx)
            }
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
            Stmt::Fun(fun) => {
                let copy = fun.clone();
                self.envs.set(copy.name.val(), Value::Fun(copy));
                Value::Nil
            }
            Stmt::Class(cls) => {
                let copy = cls.clone();
                self.envs.set(copy.name.val(), Value::Class(copy.methods));
                Value::Nil
            }
            Stmt::Print(exp) => {
                let val = self.expr(exp);
                println!("{val}");
                val
            }
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
                if let Some(val) = self.envs.get(&exp.token.val()) {
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
                    (Value::Bool(x), Value::Bool(y)) => match exp.token.kind {
                        Kind::Or => Value::Bool(x || y),
                        Kind::And => Value::Bool(x && y),
                        Kind::BangEqual => Value::Bool(x != y),
                        Kind::DoubleEqual => Value::Bool(x == y),
                        _ => panic!("unexpected operator for bool:{}", exp.token.kind),
                    },
                    (Value::Num(x), Value::Num(y)) => match exp.token.kind {
                        Kind::DoubleEqual => Value::Bool(x == y),
                        Kind::BangEqual => Value::Bool(x != y),
                        Kind::Greater => Value::Bool(x > y),
                        Kind::GreaterEqual => Value::Bool(x >= y),
                        Kind::Less => Value::Bool(x < y),
                        Kind::LessEqual => Value::Bool(x <= y),
                        _ => panic!("unexpected operator for number:{}", exp.token.kind),
                    },
                    (x, y) => panic!("unmatched operand for {}:{x}, {y}", exp.token.kind),
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
        let method = match callee.kind {
            ExprType::Identifier => self.envs.get(&callee.token.val()).unwrap().clone(),
            ExprType::Get => self.get(callee),

            _ => unreachable!(),
        };

        if let Value::Fun(fun) = method {
            let mut closure = HashMap::new();
            for (index, param) in fun.params.iter().enumerate() {
                closure.insert(param.val(), self.expr(params.get(index).unwrap()));
            }
            self.enter_func(closure);

            let mut result = Value::Nil;
            for stmt in fun.body.iter() {
                if *self.func_stack.last().unwrap() {
                    break;
                }
                result = self.stmt(stmt);
            }
            self.exit_func();

            result
        } else if let Value::Class(_) = method {
            Value::Object(callee.token.val(), Rc::new(RefCell::new(HashMap::new())))
        } else {
            panic!("unexpected value for function:{:?}", method)
        }
    }

    fn get(&mut self, exp: &Expr) -> Value {
        let left = self.expr(exp.left.as_deref().unwrap());
        let right = exp.right.as_deref().unwrap();
        assert!(right.kind == ExprType::Identifier);
        match left {
            Value::Object(_, map) => {
                let key = right.token.val();
                map.borrow().get(&key).unwrap().clone()
            }
            _ => panic!("unexpected value for get:{:?}", left),
        }
    }

    fn assign(&mut self, exp: &Expr) -> Value {
        let left = exp.left.as_deref().unwrap();
        let val = self.expr(exp.right.as_deref().unwrap()).clone();
        match left.kind {
            Identifier => self.envs.set(left.token.val(), val),
            Get => {
                let obj = self.get(left.left.as_deref().unwrap());
                match obj {
                    Value::Object(_, map) => {
                        let key = left.right.as_deref().unwrap().token.val();
                        map.borrow_mut().insert(key, val);
                    }
                    _ => panic!("unexpected value type for set:{:?}", obj),
                }
            }
            _ => panic!("unexpected left operand for set:{:?}", left),
        };
        Value::Nil
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::Parser;

    #[test]
    fn expr() {
        let s = "print 1 == 1 and true != false;";
        match Parser::parse(s) {
            Ok(stmts) => {
                assert_eq!(format!("{}", Eval::exec(&stmts)), "true")
            }
            Err(msg) => assert!(false, "parse expr err:{}", msg),
        }
    }
}
