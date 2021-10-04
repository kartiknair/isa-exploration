use std::{borrow::Borrow, env, ops::Range, process::exit};

use ariadne::{Label, Report, ReportKind, Source, Span};
use common::Error;

mod analyzer;
mod ast;
mod codegen;
mod common;
mod inst;
mod lexer;
mod parser;
mod token;
mod vm;

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

    let tokens = match lexer::lex(&file.source) {
        Ok(tokens) => tokens,
        Err(err) => report_error_and_exit(&file, err),
    };

    file.stmts = match parser::parse(&tokens) {
        Ok(stmts) => stmts,
        Err(err) => report_error_and_exit(&file, err),
    };

    if let Err(err) = analyzer::analyze_mut(&mut file) {
        report_error_and_exit(&file, err)
    };

    let blocks = codegen::gen(&file);

    const MEMORY_SIZE: usize = 128_000; // 128 KiB
    let mut machine = vm::VM::<MEMORY_SIZE>::new();
    if cfg!(debug_assertions) {
        for block in &blocks {
            println!("{}", block.as_asm())
        }
    }
    machine.interpret(&blocks);
}
