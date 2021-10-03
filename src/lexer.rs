use crate::shared::{Error, Pos};
use std::str;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum TokenType {
    // Literals
    Int,
    Float,
    Ident,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    ColonEqual,

    // Other Symbols
    Semicolon,
    LeftBrace,
    RightBrace,

    // Keywords
    Print,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub lexeme: String,
    pub pos: Pos,
}

impl TokenType {
    pub fn is_binary_op(&self) -> bool {
        (*self as u8) >= (Self::Plus as u8) && (*self as u8) <= (Self::ColonEqual as u8)
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub source: &'a str,

    // Internal state
    tokens: Vec<Token>,
    cursor: usize,
    line: usize,
    line_begin: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::<Token>::new(),
            cursor: 0,
            line: 1,
            line_begin: 0,
        }
    }

    fn add_token(&mut self, typ: TokenType, lexeme: &str) {
        self.tokens.push(Token {
            typ,
            lexeme: lexeme.to_string(),
            pos: self.get_pos(lexeme.len()),
        });
    }

    fn get_pos(&self, length: usize) -> Pos {
        Pos {
            line: self.line,
            column: self.cursor - self.line_begin - length,
            offset: self.cursor - length,
            length,
        }
    }

    pub fn lex(&mut self) -> Result<(), Error> {
        let characters: Vec<char> = self.source.chars().collect();

        while self.cursor < characters.len() {
            match characters[self.cursor] {
                '0'..='9' => {
                    let number_start = self.cursor;
                    let mut is_float = false;

                    while self.cursor < characters.len()
                        && matches!(characters[self.cursor], '0'..='9' | '.')
                    {
                        if characters[self.cursor] == '.' {
                            if is_float {
                                return Err(Error {
                                    pos: self.get_pos(1),
                                    message: "found two '.'s in number".into(),
                                });
                            }
                            is_float = true;
                        }
                        self.cursor += 1;
                    }

                    self.add_token(
                        if is_float {
                            TokenType::Float
                        } else {
                            TokenType::Int
                        },
                        &self.source[number_start..self.cursor].to_string(),
                    );
                }
                '\n' => {
                    self.line += 1;
                    self.cursor += 1;
                    self.line_begin = self.cursor;
                }
                ':' => {
                    self.cursor += 1;
                    if self.cursor >= characters.len() || characters[self.cursor] != '=' {
                        return Err(Error {
                            pos: self.get_pos(1),
                            message: "expected '=' after ':'".into(),
                        });
                    }
                    self.cursor += 1;
                    self.add_token(
                        TokenType::ColonEqual,
                        &self.source[self.cursor - 1..self.cursor],
                    )
                }
                '+' | '-' | '*' | '/' | ';' | '=' | '{' | '}' => {
                    self.cursor += 1;
                    self.add_token(
                        match characters[self.cursor - 1] {
                            '+' => TokenType::Plus,
                            '-' => TokenType::Minus,
                            '*' => TokenType::Star,
                            '/' => TokenType::Slash,
                            ';' => TokenType::Semicolon,
                            '=' => TokenType::Equal,
                            '{' => TokenType::LeftBrace,
                            '}' => TokenType::RightBrace,
                            _ => unreachable!(),
                        },
                        &self.source[self.cursor - 1..self.cursor],
                    );
                }
                _ if characters[self.cursor].is_alphabetic() => {
                    let ident_start = self.cursor;

                    while self.cursor < characters.len()
                        && characters[self.cursor].is_alphanumeric()
                    {
                        self.cursor += 1;
                    }

                    let lexeme = &self.source[ident_start..self.cursor];
                    self.add_token(
                        match lexeme {
                            "print" => TokenType::Print,
                            _ => TokenType::Ident,
                        },
                        lexeme,
                    );
                }
                _ if characters[self.cursor].is_whitespace() => {
                    // just ignore it
                    self.cursor += 1;
                }
                _ => {
                    return Err(Error {
                        pos: self.get_pos(1),
                        message: "unexpected character".into(),
                    });
                }
            }
        }

        Ok(())
    }
}

pub fn lex(code: &str) -> Result<Vec<Token>, Error> {
    let mut lexer = Lexer::new(code);
    lexer.lex()?;
    Ok(lexer.tokens)
}
