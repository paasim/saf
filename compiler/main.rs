use compiler::Compiler;
use saf::ast::parse;
use saf::err::Res;
use saf::text::scan;
use std::io::Write;
use std::{fs, io};
use vm::Vm;

mod bytecode;
mod compiler;
mod instruction;
mod vm;

fn main() {
    let mut args = std::env::args();
    let pname = args.next().expect("program name");
    let res = match args.next().as_deref() {
        Some("-c") => compile(args, &pname),
        Some("-f") => run_file(args, &pname),
        None => jit(),
        _ => {
            print_usage(&pname);
            Ok(())
        }
    };
    if let Err(e) = res {
        eprintln!("{}", e)
    }
}

fn print_usage(pname: &str) {
    println!("usage: {} [-c file.saf | -f compiled_file.safc]", pname)
}

fn compile(mut args: std::env::Args, pname: &str) -> Res<()> {
    let mut fname = match args.next() {
        Some(fname) => fname,
        _ => {
            print_usage(pname);
            return Ok(());
        }
    };
    let prog = fs::read_to_string(&fname)?;
    let bytes = to_bytecode(&prog, &mut Compiler::default())?;
    fname.push('c');
    let mut file = fs::File::create(&fname)?;
    Ok(file.write_all(&bytes)?)
}

fn run_file(mut args: std::env::Args, pname: &str) -> Res<()> {
    let fname = match args.next() {
        Some(fname) => fname,
        _ => {
            print_usage(pname);
            return Ok(());
        }
    };
    run(fs::read(fname)?, &mut Vm::default())
}

fn to_bytecode(input: &str, comp: &mut Compiler) -> Res<Vec<u8>> {
    let stmts = scan(input).and_then(parse)?;
    comp.clear_instr();
    comp.add_stmts(stmts)?;
    Ok(comp.bytecode_with_symbols())
}

fn run(v: Vec<u8>, vm: &mut Vm) -> Res<()> {
    vm.read_bytecode(v)?;
    vm.run_all()?;
    vm.take_stack()
        .iter()
        .for_each(|x| println!("{}", vm.with_symbols(x)));
    Ok(())
}

fn jit() -> Res<()> {
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut comp = Compiler::default();
    let mut vm = Vm::default();
    while stdin.read_line(&mut buffer).is_ok() {
        to_bytecode(&buffer, &mut comp).and_then(|v| run(v, &mut vm))?;
        buffer.clear()
    }
    Ok(())
}
