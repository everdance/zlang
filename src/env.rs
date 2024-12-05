use std::{collections::HashMap, rc::Rc};

use crate::expr::{self, Stmt};

pub struct Object {
    class: String,
    props: HashMap<String, Value>,
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Str(String),
    Num(f64),
    Bool(bool),
    Fun(Rc<expr::Fun>),
    Class(Rc<expr::Class>),
    Object(Rc<Object>),
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
