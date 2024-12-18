mod eval;
mod expr;
mod parse;
mod scan;
mod token;

use eval::{evaluate, evaluator};
use parse::Parser;
use std::io::{self, stdout, Write};
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        let file = &args[1];
        let script = fs::read_to_string(file).expect("input file error");
        match Parser::parse(&script) {
            Ok(stmts) => {
                evaluate(&stmts);
            }
            Err(e) => println!("{}", e),
        };
    } else {
        repl();
    }
}

fn repl() {
    println!("> welcome to zlang! use `quit` to exit.");
    let mut eval = evaluator();
    loop {
        print!("> ");
        _ = stdout().flush();
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                if buffer.trim() == "quit" {
                    break;
                }
                _ = Parser::parse(&buffer)
                    .map(|stmts| eval.exec(&stmts))
                    .map_err(|e| println!("{e}"));
            }
            Err(e) => println!("{e}"),
        };
    }
}
