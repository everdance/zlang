mod expr;
mod parse;
mod scan;
mod token;

use parse::Parser;

fn main() {
    let s = "var x = 1;// this is a test\n class a {}";
    println!("Parsing:\n\"{}\"\n=>", s);
    match Parser::parse(s) {
        Ok(stmts) => {
            for stmt in stmts.iter() {
                println!("{}", stmt)
            }
        }
        Err(e) => println!("{}", e),
    };
}
