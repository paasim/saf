use env::Env;
use saf::ast::parse;
use saf::text::scan;
use std::{fs, io};

mod env;

fn main() {
    let mut args = std::env::args();
    let pname = args.next().expect("program name");
    match args.next().as_deref() {
        Some("--scan") => scanner(),
        Some("--parse") => parser(),
        Some("-f") => eval_file(args, &pname),
        None => repl(),
        _ => print_usage(&pname),
    }
}

fn print_usage(pname: &str) {
    println!("usage: {} [--scan | --parse | -f filename]", pname)
}

fn scanner() {
    let mut buffer = String::new();
    let stdin = io::stdin();
    while stdin.read_line(&mut buffer).is_ok() {
        match scan(&buffer) {
            Ok(tokens) => tokens.iter().for_each(|t| println!("{}", t)),
            Err(e) => println!("Error: {}", e),
        };
        buffer.clear()
    }
}

fn parser() {
    let mut buffer = String::new();
    let stdin = io::stdin();
    while stdin.read_line(&mut buffer).is_ok() {
        match scan(&buffer).and_then(parse) {
            Ok(stmts) => stmts.iter().for_each(|s| println!("{}", s)),
            Err(e) => println!("Error: {}", e),
        };
        buffer.clear()
    }
}

fn eval_file(mut args: std::env::Args, pname: &str) {
    let f = match args.next().map(fs::read_to_string) {
        Some(f) => f.expect("file read unsuccesful"),
        None => return print_usage(pname),
    };
    let mut env = Env::default();
    for line in f.lines() {
        match scan(line)
            .and_then(parse)
            .and_then(|stmts| env.eval_stmts(stmts))
        {
            Ok(Some(v)) => println!("{}", v),
            Ok(None) => {}
            Err(e) => println!("Error: {}", e),
        };
    }
}

fn repl() {
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut env = Env::default();
    while stdin.read_line(&mut buffer).is_ok() {
        match scan(&buffer)
            .and_then(parse)
            .and_then(|stmts| env.eval_stmts(stmts))
        {
            Ok(Some(v)) => println!("{}", v),
            Ok(None) => {}
            Err(e) => println!("Error: {}", e),
        };
        buffer.clear()
    }
}
