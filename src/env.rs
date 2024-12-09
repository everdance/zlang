use crate::expr;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Str(String),
    Num(f64),
    Bool(bool),
    Fun(Rc<expr::Fun>),
    Class(Rc<HashMap<String, expr::Fun>>),
    Object(String, Rc<HashMap<String, Value>>),
}

pub struct Environments {
    stack: Vec<HashMap<String, Value>>,
}

pub fn new() -> Environments {
    Environments { stack: vec![] }
}

impl Environments {
    pub fn push(&mut self, hm: Option<HashMap<String, Value>>) {
        self.stack.push(hm.unwrap_or_default());
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn get(&self, id: &str) -> Option<&Value> {
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
