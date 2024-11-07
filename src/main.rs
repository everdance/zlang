mod parser;
mod token;

fn main() {
    let s = "var x = 1;// this is a test";
    let tokens = parser::parse(s);
    for t in tokens {
        println!("{t:?}");
    }
}
