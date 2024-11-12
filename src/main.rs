mod expr;
mod scan;
mod token;

use scan::Scanner;

fn main() {
    let s = "var x = 1;// this is a test\n class a {}";
    let (tokens, issues) = Scanner::scan(s);
    for t in tokens {
        println!("{t}");
    }
    assert_eq!(issues.len(), 0);
}
