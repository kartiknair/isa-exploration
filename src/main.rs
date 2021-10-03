mod analyzer;
mod ast;
mod codegen;
mod inst;
mod lexer;
mod parser;
mod shared;
mod vm;

use std::{env, fs, process::exit};

const MEMORY_SIZE: usize = 128_000;

fn fail(message: &str) {
    eprintln!("{}", message);
    exit(1);
}

fn format_err(filename: &str, err: &shared::Error) -> String {
    format!(
        "{}:{}:{}: {}",
        filename, err.pos.line, err.pos.column, err.message
    )
}

fn run_file(filename: &str) {
    let contents = match fs::read_to_string(filename) {
        Ok(contents) => contents,
        Err(file_error) => {
            fail(&format!(
                "Could not read file: '{}'\n{}",
                filename,
                file_error.to_string()
            ));
            unreachable!();
        }
    };

    let tokens = match lexer::lex(&contents) {
        Ok(tokens) => tokens,
        Err(err) => {
            fail(&format_err(filename, &err));
            unreachable!();
        }
    };

    let mut expressions = match parser::parse(&tokens) {
        Ok(expr) => expr,
        Err(err) => {
            fail(&format_err(filename, &err));
            unreachable!();
        }
    };

    expressions = match analyzer::analyze(&expressions) {
        Ok(analyzed_exprs) => analyzed_exprs,
        Err(err) => {
            fail(&format_err(filename, &err));
            unreachable!();
        }
    };

    let blocks = codegen::gen(&expressions);
    for block in &blocks {
        println!("{}", block.as_asm());
    }
    let mut machine = vm::VM::<MEMORY_SIZE>::new();
    machine.interpret(&blocks);
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!(
            r#"Expected only one argument.

Usage:
{} [filename]
"#,
            args[0]
        );
        exit(1);
    }

    run_file(&args[1]);
}
