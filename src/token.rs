#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Token {
    // Keywords
    Fn,     // fn
    Let,    // let
    Mut,    // mut
    If,     // if
    Else,   // else
    Return, // return
    True,   // true
    False,  // false
    Print,  // print (for a simple print function)

    // Operators
    Assign,   // =
    Plus,     // +
    Minus,    // -
    Asterisk, // *
    Slash,    // /
    Bang,     // !
    Eq,       // ==
    NotEq,    // !=
    Lt,       // <
    Gt,       // >
    LtEq,     // <=
    GtEq,     // >=

    // Delimiters
    Comma,     // ,
    Semicolon, // ;
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }

    // Literals
    Identifier(String),
    Int(String),
    String(String),

    // Special
    Eof,
    Illegal,
}
