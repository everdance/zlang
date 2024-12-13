mod env;
mod eval;
mod expr;
mod parse;
mod scan;
mod token;
use eval::Eval;
use parse::Parser;

fn main() {
    let s = "var x = 1; print x";

    match Parser::parse(s) {
        Ok(stmts) => Eval::exec(&stmts),
        Err(e) => println!("{}", e),
    };
}
