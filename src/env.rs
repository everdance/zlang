use crate::expr;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

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

pub struct Environments {
    stack: Vec<HashMap<String, Value>>,
}

pub fn new() -> Environments {
    let global = HashMap::new();
    Environments {
        stack: vec![global],
    }
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
