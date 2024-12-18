mod eval;
mod expr;
mod parse;
mod scan;
mod token;

use eval::Eval;
use parse::Parser;
use std::io;
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        let file = &args[1];
        let script = fs::read_to_string(file).expect("input file error");
        match Parser::parse(&script) {
            Ok(stmts) => {
                Eval::exec(&stmts);
            }
            Err(e) => println!("{}", e),
        };
    } else {
        repl();
    }
}

fn repl() {
    println!("> welcome to zlang! use `quit` to exit.");
    loop {
        println!("> ");
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                if buffer.trim() == "quit" {
                    break;
                }
                match Parser::parse(&buffer).map(|stmts| Eval::exec(&stmts)) {
                    Ok(v) => println!("> {v}"),
                    Err(e) => println!("> {e}"),
                }
            }
            Err(e) => println!("> {e}"),
        };
    }
}
