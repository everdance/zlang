mod expr;
mod parse;
mod scan;
mod token;

use parse::Parser;
use scan::Scanner;

fn main() {
    let s = "var x = 1;// this is a test\n class a {}";
    let (tokens, issues) = Scanner::scan(s);
    assert_eq!(issues.len(), 0);
    match Parser::parse(&tokens) {
        Ok(stmts) => {
            for stmt in stmts.iter() {
                println!("{}", stmt)
            }
        }
        Err(e) => println!("{}", e),
    };
}
