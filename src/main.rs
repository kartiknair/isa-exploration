use std::{env, process::exit};

use ariadne::{Label, Report, ReportKind, Source};
use common::Error;

mod analyzer;
mod ast;
mod codegen;
mod common;
mod lexer;
mod parser;
mod risc;
mod token;

fn report_error_and_exit(file: &ast::File, err: Error) -> ! {
    let filename = file
        .path
        .as_path()
        .as_os_str()
        .to_str()
        .expect("could not convert file path to string");

    Report::build(ReportKind::Error, filename, err.span.start)
        .with_label(Label::new((filename, err.span)).with_message(err.message))
        .finish()
        .print((filename, Source::from(&file.source)))
        .unwrap();

    exit(1);
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!(
            r#"
Usage:

{} [filename]

Arguments:

filename = Path to the file that you would like to compile
"#,
            args[0]
        );
        exit(1);
    }

    let mut file = match ast::File::new(&args[1]) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("{}", err.to_string());
            exit(1);
        }
    };
    dbg!("read file");

    let tokens = match lexer::lex(&file.source) {
        Ok(tokens) => tokens,
        Err(err) => report_error_and_exit(&file, err),
    };
    dbg!("lexed tokens");

    file.stmts = match parser::parse(&tokens) {
        Ok(stmts) => stmts,
        Err(err) => report_error_and_exit(&file, err),
    };
    dbg!("parsed into ast");

    if let Err(err) = analyzer::analyze_mut(&mut file) {
        report_error_and_exit(&file, err)
    };
    dbg!("analyzed");

    let blocks = codegen::gen(&file);

    dbg!("generated asm");
    const MEMORY_SIZE: usize = 128_000; // 128 KiB
    let mut machine = risc::vm::VM::<MEMORY_SIZE>::new(&blocks);
    if cfg!(debug_assertions) {
        for block in &blocks {
            println!("{}", block.as_asm())
        }
    }

    dbg!("program output");
    machine.interpret();
}
