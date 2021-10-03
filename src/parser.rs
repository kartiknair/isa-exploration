use crate::ast;
use crate::lexer;
use crate::shared::Error;

#[derive(Debug)]
pub struct Parser<'a> {
    pub tokens: &'a [lexer::Token],

    cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [lexer::Token]) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn peek(&self) -> &lexer::Token {
        &self.tokens[self.cursor]
    }

    fn expect(&mut self, typ: lexer::TokenType, message: &str) -> Result<&lexer::Token, Error> {
        if self.cursor >= self.tokens.len() {
            Err(Error {
                pos: self.tokens.last().unwrap().pos,
                message: message.into(),
            })
        } else if self.peek().typ != typ {
            Err(Error {
                pos: self.peek().pos,
                message: message.into(),
            })
        } else {
            self.cursor += 1;
            Ok(&self.tokens[self.cursor - 1])
        }
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, Error> {
        let mut expr = match self.peek().typ {
            lexer::TokenType::LeftBrace => {
                let lbrace_token = self.peek().clone();
                let mut exprs = Vec::new();
                self.cursor += 1;

                while self.cursor < self.tokens.len()
                    && self.peek().typ != lexer::TokenType::RightBrace
                {
                    exprs.push(self.parse_expression()?);
                    self.expect(
                        lexer::TokenType::Semicolon,
                        "expected ';' after expression in block",
                    )?;
                }

                self.expect(lexer::TokenType::RightBrace, "expected '}' to close block")?;

                ast::Expression {
                    variant: ast::ExpressionEnum::Block {
                        lbrace_token,
                        exprs,
                    },
                    typ: None,
                }
            }
            lexer::TokenType::Ident => {
                let var_expr = ast::Expression {
                    variant: ast::ExpressionEnum::Variable(self.peek().clone()),
                    typ: None,
                };
                self.cursor += 1;
                var_expr
            }
            lexer::TokenType::Int | lexer::TokenType::Float => {
                let literal_value = ast::Expression {
                    variant: ast::ExpressionEnum::Literal(self.peek().clone()),
                    typ: None,
                };
                self.cursor += 1;
                literal_value
            }
            lexer::TokenType::Print => {
                self.cursor += 1;
                let expr = self.parse_expression()?;
                ast::Expression {
                    variant: ast::ExpressionEnum::Print(Box::new(expr)),
                    typ: None,
                }
            }
            _ => {
                return Err(Error {
                    pos: self.peek().pos,
                    message: "unexpected token".into(),
                })
            }
        };

        while self.cursor < self.tokens.len() && self.peek().typ.is_binary_op() {
            let op = self.peek().clone();
            self.cursor += 1;

            let right = self.parse_expression()?;
            expr = ast::Expression {
                variant: ast::ExpressionEnum::Binary {
                    left: Box::new(expr),
                    right: Box::new(right),
                    op,
                },
                typ: None,
            }
        }

        Ok(expr)
    }

    pub fn parse(&mut self) -> Result<Vec<ast::Expression>, Error> {
        let mut expressions = Vec::new();
        while self.cursor < self.tokens.len() {
            let expr = self.parse_expression()?;
            self.expect(
                lexer::TokenType::Semicolon,
                "expected semicolon after top-level expression",
            )?;
            expressions.push(expr);
        }
        Ok(expressions)
    }
}

pub fn parse(tokens: &[lexer::Token]) -> Result<Vec<ast::Expression>, Error> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
