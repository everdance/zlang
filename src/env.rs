use crate::expr;
use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Object<'a> {
    class: String,
    props: HashMap<String, Value<'a>>,
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Nil,
    Str(String),
    Num(f64),
    Bool(bool),
    Fun(&'a expr::Fun),
    Class(HashMap<String, &'a expr::Fun>),
    Object(Rc<Object<'a>>),
}

pub fn new(parent: Option<Rc<Environment>>) -> Environment {
    Environment {
        parent,
        map: HashMap::new(),
    }
}

pub struct Environment<'a> {
    parent: Option<Rc<Environment<'a>>>,
    map: HashMap<String, Value<'a>>,
}

impl<'a> Environment<'a> {
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

    pub fn set(&mut self, id: String, val: Value<'a>) {
        self.map.insert(id, val);
    }
}
