use std::{collections::HashMap, rc::Rc};

use crate::expr::{self, Stmt};

#[derive(Debug)]
pub struct Object {
    class: String,
    props: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Str(String),
    Num(f64),
    Bool(bool),
    Fun(Rc<expr::Fun>),
    Class(Rc<HashMap<String, expr::Fun>>),
    Object(Rc<Object>),
}

pub fn new<'a>(parent: Option<&'a Environment<'a>>) -> Environment<'a> {
    Environment {
        parent,
        map: HashMap::new(),
    }
}

pub struct Environment<'a> {
    parent: Option<&'a Environment<'a>>,
    map: HashMap<String, Value>,
}

impl Environment<'_> {
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
