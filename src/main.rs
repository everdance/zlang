mod parser;
mod token;

fn main() {
    let s = "var x = 1";
    parser::parse(&s);
    println!("Hello, world!");
}
