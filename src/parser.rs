use lalrpop_util::lalrpop_mod;
use miette::{Diagnostic, NamedSource};
use thiserror::Error;
use crate::ast::Program;
use crate::lexer::{Lexer, LexerError};
use lalrpop_util::ParseError;

lalrpop_mod!(pub glam);

#[derive(Error, Debug, Diagnostic)]
#[error("{message}")]
#[diagnostic(
    code(parser_error),
    help("The parser encountered an error. Please check the syntax of your code."),
)]
pub struct ParserError {
    message: String,
    #[source_code]
    src: NamedSource<String>,
    #[label("here")]
    span: (usize, usize),
}

pub struct Parser {
    parser: glam::ProgramParser,
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            parser: glam::ProgramParser::new(),
        }
    }

    pub fn parse(&self, source: &str) -> Result<Program, miette::Report> {
        let lexer = Lexer::new(source.to_string());
        let tokens = lexer.map(|token_result| {
            token_result.map(|token| {
                let start = token.start;
                let end = start + token.len;
                (start, token, end)
            })
        });

        self.parser.parse(tokens).map_err(|e| {
            let (message, span) = match e {
                ParseError::InvalidToken { location } => {
                    ("Invalid token".to_string(), (location, 1))
                }
                ParseError::UnrecognizedEof { location, expected } => {
                    (format!("Unrecognized EOF, expected: {:?}", expected), (location, 1))
                }
                ParseError::UnrecognizedToken { token, expected } => {
                    (
                        format!("Unrecognized token: {:?}, expected: {:?}", token.1, expected),
                        (token.0, token.2 - token.0),
                    )
                }
                ParseError::ExtraToken { token } => {
                    (format!("Extra token: {:?}", token.1), (token.0, token.2 - token.0))
                }
                ParseError::User { error } => {
                    let (message, span) = match error {
                        LexerError::IllegalCharacter { found: _, span } => {
                            ("Illegal character".to_string(), (span.offset(), span.len()))
                        }
                        LexerError::UnterminatedString { span } => {
                            ("Unterminated string".to_string(), (span.offset(), span.len()))
                        }
                    };
                    (message, span)
                }
            };

            let err = ParserError {
                message,
                src: NamedSource::new("main.lm", source.to_string()),
                span,
            };
            miette::Report::new(err)
        })
    }
}