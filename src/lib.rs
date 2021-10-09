use std::{io::BufWriter, path::PathBuf, str::FromStr};

use ariadne::{Label, Report, ReportKind, Source};
use common::Error;
use wasm_bindgen::prelude::*;

mod analyzer;
mod ast;
mod cisc;
mod codegen;
mod common;
mod lexer;
mod parser;
mod risc;
mod token;
mod utils;

fn create_pseudo_file(source: &str) -> Result<ast::File, Error> {
    let mut file = ast::File {
        path: PathBuf::from_str("").unwrap(),
        source: source.to_string(),
        stmts: Vec::new(),
    };

    let tokens = lexer::lex(source)?;
    file.stmts = parser::parse(&tokens)?;
    analyzer::analyze_mut(&mut file)?;

    Ok(file)
}

#[wasm_bindgen]
pub fn compile_risc(source: &str) -> Option<String> {
    let file = match create_pseudo_file(source) {
        Ok(file) => file,
        Err(_) => return None,
    };

    let risc_blocks = codegen::risc::gen(&file);
    let mut risc_asm = String::new();
    for block in &risc_blocks {
        if !block.label.starts_with("print_") {
            risc_asm += &block.as_asm()
        }
    }

    Some(risc_asm)
}

#[wasm_bindgen]
pub fn run_risc(source: &str) -> String {
    let file = match create_pseudo_file(source) {
        Ok(file) => file,
        Err(err) => return build_error_report(source, err),
    };

    let risc_blocks = codegen::risc::gen(&file);

    const MEMORY_SIZE: usize = 128_000; // 128 KiB

    let mut stdout = Vec::<u8>::new();
    let mut machine = risc::vm::VM::<_, MEMORY_SIZE>::new(&risc_blocks, &mut stdout);
    machine.interpret();

    String::from_utf8(stdout).unwrap()
}

#[wasm_bindgen]
pub fn compile_cisc(source: &str) -> Option<String> {
    let file = match create_pseudo_file(source) {
        Ok(file) => file,
        Err(_) => return None,
    };

    let cisc_blocks = codegen::cisc::gen(&file);
    let mut cisc_asm = String::new();
    for block in &cisc_blocks {
        if !block.label.starts_with("print_") {
            cisc_asm += &block.as_asm()
        }
    }

    Some(cisc_asm)
}

#[wasm_bindgen]
pub fn run_cisc(source: &str) -> String {
    let file = match create_pseudo_file(source) {
        Ok(file) => file,
        Err(err) => return build_error_report(source, err),
    };

    let cisc_blocks = codegen::cisc::gen(&file);

    const MEMORY_SIZE: usize = 128_000; // 128 KiB

    let mut stdout = Vec::<u8>::new();
    let mut machine = cisc::vm::VM::<_, MEMORY_SIZE>::new(&cisc_blocks, &mut stdout);
    machine.interpret();

    String::from_utf8(stdout).unwrap()
}

fn build_error_report(source: &str, err: Error) -> String {
    let mut buffer = BufWriter::new(Vec::new());

    Report::build(ReportKind::Error, (), err.span.start)
        .with_label(Label::new(err.span).with_message(err.message))
        .finish()
        .write(Source::from(source), &mut buffer)
        .unwrap();

    let bytes = buffer.into_inner().unwrap();
    let plain_bytes = strip_ansi_escapes::strip(&bytes).unwrap();

    String::from_utf8(plain_bytes).unwrap()
}
