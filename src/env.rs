use std::collections::HashMap;

use crate::expr::Stmt;

pub struct Fun {
    params: Vec<String>,
    body: Vec<Stmt>,
}

pub enum Value {
    Nil,
    Str(String),
    Num(f64),
    Bool(bool),
    Fun(Box<Fun>),
}

pub struct Environment {
    parent: Option<Box<Environment>>,
    map: HashMap<String, Value>,
}

impl Environment {
    pub fn get(&self, id: &str) -> Option<&Value> {
        match self.map.get(id) {
            Some(val) => Some(val),
            None => {
                if let Some(penv) = &self.parent {
                    penv.get(id)
                } else {
                    None
                }
            }
        }
    }

    pub fn set(&mut self, id: String, val: Value) {
        self.map.insert(id, val);
    }
}
