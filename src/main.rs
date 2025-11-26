mod token;
mod ast;
mod lexer;
mod parser;
mod evaluator;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::evaluator::{eval, Environment};
use std::fs;
use std::io::{self, Read};
use std::rc::Rc;
use std::cell::RefCell;

fn main() -> io::Result<()> {
    println!("Ox Lang Interpreter initialised.");

    let file_path = "main.ox";
    let mut file = fs::File::open(file_path)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;

    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    if !p.errors.is_empty() {
        eprintln!("Parser errors:");
        for err in p.errors {
            eprintln!("\t{}", err);
        }
        return Ok(());
    }

    let mut env = Rc::new(RefCell::new(Environment::new(Rc::new(RefCell::new(io::stdout())))));
    let evaluated = eval(program, &mut env);

    println!("Evaluated result: {:#?}", evaluated);
    Ok(())
}