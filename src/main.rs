mod expr;
mod parse;
mod scan;
mod token;

use parse::Parser;
use scan::Scanner;

fn main() {
    let s = "var x = 1;// this is a test\n class a {}";
    let (tokens, issues) = Scanner::scan(s);
    _ = Parser::parse(&tokens);

    assert_eq!(issues.len(), 0);
}
