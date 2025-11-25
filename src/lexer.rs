use crate::token::Token;

pub struct Lexer {
    input: String,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: char,             // current char under examination
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0', // null character
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comments(); // Skip comments after skipping whitespace

        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char(); // consume the second '='
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => {
                if self.peek_char() == '/' {
                    // This case should be handled by skip_comments, but just in case
                    self.skip_comments();
                    return self.next_token(); // Get the next actual token
                }
                Token::Slash
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char(); // consume the '='
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char(); // consume the '='
                    Token::LtEq
                } else {
                    Token::Lt
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char(); // consume the '='
                    Token::GtEq
                } else {
                    Token::Gt
                }
            }
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '"' => self.read_string(),
            '\0' => Token::Eof,
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    return match literal.as_str() {
                        "fn" => Token::Fn,
                        "let" => Token::Let,
                        "mut" => Token::Mut,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "return" => Token::Return,
                        "true" => Token::True,
                        "false" => Token::False,
                        "print" => Token::Print,
                        _ => Token::Identifier(literal),
                    };
                } else if is_digit(self.ch) {
                    return Token::Int(self.read_number());
                } else {
                    Token::Illegal
                }
            }
        };

        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) || is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_string(&mut self) -> Token {
        let position = self.position + 1; // skip the opening quote
        self.read_char(); // move past the opening quote
        while self.ch != '"' && self.ch != '\0' {
            self.read_char();
        }

        if self.ch == '\0' {
            return Token::Illegal; // Unterminated string
        }

        let literal = self.input[position..self.position].to_string();
        Token::String(literal)
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn skip_comments(&mut self) {
        while self.ch == '/' && self.peek_char() == '/' {
            while self.ch != '\n' && self.ch != '\0' {
                self.read_char();
            }
            self.read_char(); // consume the '\n'
            self.skip_whitespace(); // skip any whitespace after the newline
        }
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}