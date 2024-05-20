use env::Env;
use saf::ast::parse;
use saf::text::scan;
use std::{fs, io, iter};

mod env;

fn main() {
    let mut args = std::env::args().peekable();
    let pname = args.next().expect("program name");
    let echo = args.next_if(|arg| arg == "--echo").is_some();
    match args.next().as_deref() {
        Some("--scan") => scanner(echo),
        Some("--parse") => parser(echo),
        Some("-f") => eval_file(echo, args, &pname),
        None => repl(echo),
        _ => print_usage(&pname),
    }
}

fn print_usage(pname: &str) {
    println!("usage: {} [--echo] [--scan | --parse | -f filename]", pname)
}

fn scanner(echo: bool) {
    let mut buffer = String::new();
    let stdin = io::stdin();
    while stdin.read_line(&mut buffer).is_ok() {
        if echo {
            println!("> {}", buffer);
        }
        match scan(&buffer) {
            Ok(tokens) => tokens.iter().for_each(|t| println!("{}", t)),
            Err(e) => println!("Error: {}", e),
        };
        buffer.clear()
    }
}

fn parser(echo: bool) {
    let mut buffer = String::new();
    let stdin = io::stdin();
    while stdin.read_line(&mut buffer).is_ok() {
        if echo {
            println!("> {}", buffer);
        }
        match scan(&buffer).and_then(parse) {
            Ok(stmts) => stmts.iter().for_each(|s| println!("{}", s)),
            Err(e) => println!("Error: {}", e),
        };
        buffer.clear()
    }
}

fn eval_file(echo: bool, mut args: iter::Peekable<std::env::Args>, pname: &str) {
    let f = match args.next().map(fs::read_to_string) {
        Some(f) => f.expect("file read unsuccesful"),
        None => return print_usage(pname),
    };
    let mut env = Env::default();
    for line in f.lines() {
        if echo {
            println!("> {}", line);
        }
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

fn repl(echo: bool) {
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut env = Env::default();
    while stdin.read_line(&mut buffer).is_ok() {
        if echo {
            println!("> {}", buffer);
        }
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
