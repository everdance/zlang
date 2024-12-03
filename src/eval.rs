use crate::env::{Environment, Value};
use crate::expr::{Expr, ExprType::*, Stmt};
use crate::token::Kind;

pub struct Eval;

impl Eval {
    pub fn exec(stmts: &[Stmt]) -> () {}
}

fn eval_expr(exp: Expr, env: &Environment) -> Value {
    match exp.kind {
        Literal => match exp.token.kind {
            Kind::NumLiteral(x) => Value::Num(x),
            Kind::StrLiteral(x) => Value::Str(x),
            Kind::True => Value::Bool(true),
            Kind::False => Value::Bool(false),
            Kind::Nil => Value::Nil,
            _ => unreachable!(),
        },
        Identifier => {
            if let Some(val) = env.get(&exp.token.val()) {
                *val
            } else {
                panic!("undefined variable")
            }
        }
        Unary => match eval_expr(*exp.left.unwrap(), env) {
            Value::Bool(x) => Value::Bool(!x),
            Value::Num(x) => Value::Num(-x),
            _ => panic!("unexpected value"),
        },
        Binary => {
            let left = eval_expr(*exp.left.unwrap(), env);
            let right = eval_expr(*exp.right.unwrap(), env);
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
            let left = eval_expr(*exp.left.unwrap(), env);
            let right = eval_expr(*exp.right.unwrap(), env);
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
        Grouping => eval_expr(*exp.left.unwrap(), env),
        // Call => Value::Nil,
        // Assign => Value::Nil,
        // Get => Value::Nil,
        _ => Value::Nil,
    }
}
