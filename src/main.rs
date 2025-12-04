use miette::{Diagnostic, IntoDiagnostic, Result};
use thiserror::Error;
use std::io;


#[derive(Error, Diagnostic, Debug)]
pub enum OxError {
    #[error(transparent)]
    #[diagnostic(code(ox_lang::io_error))]
    IoError(#[from] io::Error),
    #[error(transparent)]
    #[diagnostic(code(ox_lang::eval_error))]
    EvalError(#[from] evaluator::EvalError),
    #[error(transparent)]
    #[diagnostic(code(ox_lang::lexer_error))]
    LexerError(#[from] lexer::LexerError),
}

mod ast;
mod evaluator;
mod lexer;
mod parser;
mod token;

use crate::evaluator::{eval, Environment};
// use crate::lexer::Lexer;
use crate::parser::Parser;
use std::cell::RefCell;
use std::fs;
use std::io::{Read};
use std::rc::Rc;

fn main() -> Result<()> {
    println!("Glam Lang Interpreter initialised.");

    let file_path = "main.lm";
    let mut file = fs::File::open(file_path).into_diagnostic()?;
    let mut input = String::new();
    file.read_to_string(&mut input).into_diagnostic()?;

    let p = Parser::new();
    let program = p.parse(&input)?;

    let mut env = Rc::new(RefCell::new(Environment::new(Rc::new(RefCell::new(
        io::stdout(),
    )))));
    let evaluated = eval(program, &mut env)?;

    println!("Evaluated result: {}", evaluated);
    Ok(())
}
