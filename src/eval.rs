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

use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Str(String),
    Num(f64),
    Bool(bool),
    Fun(expr::Fun),
    Class(HashMap<String, expr::Fun>),
    Object(String, Rc<RefCell<HashMap<String, Value>>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Value::Nil => "nil".to_string(),
            Value::Str(s) => s.clone(),
            Value::Num(num) => num.to_string(),
            Value::Bool(bl) => bl.to_string(),
            Value::Fun(fun) => fun.to_string(),
            Value::Class(cls) => {
                let mut methods = "".to_string();
                for (_, m) in cls.iter() {
                    methods = format!("{},{{{m}}}", methods.as_str());
                }
                methods.trim_start_matches(",").to_string()
            }
            Value::Object(cls_name, properties) => {
                let mut obj = format!("object:<class:{cls_name}>{{");
                for (k, v) in properties.borrow().iter() {
                    obj = format!("{obj} {k} => {v}")
                }
                format!("{obj} }}")
            }
        };
        write!(f, "{s}")
    }
}

struct Environments {
    stack: Vec<HashMap<String, Value>>,
}

fn new_env() -> Environments {
    let global = HashMap::new();
    Environments {
        stack: vec![global],
    }
}

impl Environments {
    fn push(&mut self, hm: Option<HashMap<String, Value>>) {
        self.stack.push(hm.unwrap_or_default());
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn get(&self, id: &str) -> Option<&Value> {
        for ev in self.stack.iter().rev() {
            if let Some(val) = ev.get(id) {
                return Some(val);
            }
        }
        None
    }

    pub fn set(&mut self, id: String, val: Value) {
        self.stack.last_mut().unwrap().insert(id, val);
    }
}

pub trait Exec {
    fn exec(&mut self, stmts: &[Stmt]) -> Value;
}

pub fn evaluate(stmts: &[Stmt]) -> Value {
    let mut eval = Evaluator {
        envs: new_env(),
        fstack: vec![],
        printval: None,
    };
    stmts.iter().for_each(|st| eval.stmt(st));
    eval.print_val().unwrap_or(Value::Nil)
}

pub fn evaluator() -> Box<dyn Exec> {
    Box::new(Evaluator {
        envs: new_env(),
        fstack: vec![],
        printval: None,
    })
}

impl Exec for Evaluator {
    fn exec(&mut self, stmts: &[Stmt]) -> Value {
        stmts.iter().for_each(|st| self.stmt(st));
        self.print_val().unwrap_or(Value::Nil)
    }
}

struct FunStack {
    returned: bool,
    val: Value,
}

struct Evaluator {
    envs: Environments,
    fstack: Vec<FunStack>,
    printval: Option<Value>,
}

impl Evaluator {
    fn print_val(&self) -> Option<Value> {
        self.printval.clone()
    }

    fn enterfun(&mut self, closure: HashMap<String, Value>) {
        self.envs.push(Some(closure));
        self.fstack.push(FunStack {
            returned: false,
            val: Value::Nil,
        });
    }

    fn exitfun(&mut self) -> Option<FunStack> {
        self.envs.pop();
        self.fstack.pop()
    }

    fn stmt(&mut self, stmt: &expr::Stmt) {
        match stmt {
            Stmt::Expr(exp) => {
                self.expr(exp);
            }
            Stmt::Var(name, exp) => {
                let val = exp.as_ref().map_or(Value::Nil, |epr| self.expr(epr));
                self.envs.set(name.val(), val);
            }
            Stmt::Return(epx) => {
                // TODO: should move this to parser
                if self.fstack.len() == 0 {
                    panic!("unexpected return stmt without function");
                }
                *self.fstack.last_mut().unwrap() = FunStack {
                    returned: true,
                    val: self.expr(epx),
                };
            }
            Stmt::If(cond, ifstmt, elsest) => match self.expr(cond) {
                Value::Bool(true) => ifstmt.iter().for_each(|st| self.stmt(st)),
                Value::Bool(false) => {
                    elsest
                        .as_ref()
                        .map(|stmts| stmts.iter().for_each(|st| self.stmt(st)));
                }
                _ => unreachable!(),
            },
            Stmt::While(cond, stmts) => loop {
                match self.expr(cond) {
                    Value::Bool(true) => (),
                    _ => break,
                }
                stmts.iter().for_each(|st| self.stmt(st));
            },
            Stmt::For(stmt) => {
                stmt.var.as_ref().map(|st| self.stmt(st));

                loop {
                    if let Some(cond) = &stmt.cond {
                        match self.expr(cond) {
                            Value::Bool(true) => (),
                            _ => break,
                        };
                    }
                    stmt.body.iter().for_each(|st| self.stmt(st));
                    stmt.incr.as_ref().map(|st| self.stmt(st));
                }
            }
            Stmt::Block(stmts) => stmts.iter().for_each(|st| self.stmt(st)),
            Stmt::Fun(fun) => {
                let copy = fun.clone();
                self.envs.set(copy.name.val(), Value::Fun(copy));
            }
            Stmt::Class(cls) => {
                let copy = cls.clone();
                self.envs.set(copy.name.val(), Value::Class(copy.methods));
            }
            Stmt::Print(exp) => {
                let val = self.expr(exp);
                self.printval = Some(val.clone());
                println!("{val}");
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
            This => self.envs.get("this").unwrap().clone(),
            Grouping => self.expr(exp.left.as_deref().unwrap()),
            Call => self.call(exp.left.as_deref().unwrap(), exp.list.as_ref().unwrap()),
            Assign => self.assign(exp),
            Get => self.get(exp),
            _ => Value::Nil,
        }
    }

    fn call(&mut self, callee: &Expr, params: &[Expr]) -> Value {
        let mut object = Value::Nil;
        let method = match callee.kind {
            ExprType::Identifier => self.envs.get(&callee.token.val()).unwrap().clone(),
            ExprType::Get => {
                let method_name = callee.right.as_deref().unwrap().token.val();
                object = self.expr(callee.left.as_deref().unwrap());
                match &object {
                    Value::Object(cls, _) => match self.envs.get(&cls) {
                        Some(Value::Class(cls_obj)) => {
                            Value::Fun(cls_obj.get(&method_name).unwrap().clone())
                        }
                        val => panic!("unexpected class reference:{:?}", val),
                    },
                    obj => panic!("cannot call method on non-object entity:{:?}", obj),
                }
            }

            _ => unreachable!(),
        };

        if let Value::Fun(fun) = method {
            let mut closure = HashMap::new();
            for (index, param) in fun.params.iter().enumerate() {
                closure.insert(param.val(), self.expr(params.get(index).unwrap()));
            }
            if let Value::Object(_, _) = &object {
                closure.insert("this".to_string(), object);
            }
            self.enterfun(closure);

            for stmt in fun.body.iter() {
                if self.fstack.last().unwrap().returned {
                    break;
                }
                self.stmt(stmt);
            }
            self.exitfun().unwrap().val
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
            // TODO: check if var is predefined
            Identifier => self.envs.set(left.token.val(), val),
            Get => {
                let obj = self.expr(left.left.as_deref().unwrap());
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
        let stmts = Parser::parse(s).unwrap();
        assert_eq!(evaluate(&stmts).to_string(), "true");
    }

    #[test]
    fn var() {
        let s = "var x = 3 /2 + 1 * 9.0; print x";
        let stmts = Parser::parse(s).unwrap();
        assert_eq!(evaluate(&stmts).to_string(), "10.5");
    }

    #[test]
    fn if_else() {
        let s = "var x = 0; if (6 >= 5) {x = 5} else x = 3; print x";
        let stmts = Parser::parse(s).unwrap();
        assert_eq!(evaluate(&stmts).to_string(), "5");
    }

    #[test]
    fn for_expr() {
        let s = "var x = 0; for(var i = 0; i < 10; i = i+1) x = x+1; print x";
        let stmts = Parser::parse(s).unwrap();
        assert_eq!(evaluate(&stmts).to_string(), "10");
    }

    #[test]
    fn while_expr() {
        let s = "var x = 1; while(x < 100) x = x*2; print x";
        let stmts = Parser::parse(s).unwrap();
        assert_eq!(evaluate(&stmts).to_string(), "128");
    }

    #[test]
    fn func_expr() {
        let s = "var x = 3; fun multiply(x, y) {return x*y;}; print multiply(x,5)";
        let stmts = Parser::parse(s).unwrap();
        assert_eq!(evaluate(&stmts).to_string(), "15");
    }

    #[test]
    fn class_expr() {
        let s = "class Math { fun multiply(x) {return this.val*x;}}; var x = Math(); x.val = 2; print x.multiply(4);";
        let stmts = Parser::parse(s).unwrap();
        assert_eq!(evaluate(&stmts).to_string(), "8");
    }
}
