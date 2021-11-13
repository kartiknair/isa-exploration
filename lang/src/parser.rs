use crate::{
    ast,
    common::Error,
    token::{Token, TokenKind},
};

#[derive(Debug, Clone)]
struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Assoc {
    Ltr,
    Rtl,
}

#[derive(Debug, Clone, Copy)]
struct OpInfo {
    prec: u8,
    assoc: Assoc,
}

impl TokenKind {
    fn op_info(&self) -> OpInfo {
        match self {
            Self::Dot => OpInfo {
                prec: 7,
                assoc: Assoc::Ltr,
            },

            Self::Percent => OpInfo {
                prec: 6,
                assoc: Assoc::Ltr,
            },
            Self::Star => OpInfo {
                prec: 6,
                assoc: Assoc::Ltr,
            },
            Self::Slash => OpInfo {
                prec: 6,
                assoc: Assoc::Ltr,
            },

            Self::Plus => OpInfo {
                prec: 5,
                assoc: Assoc::Ltr,
            },
            Self::Minus => OpInfo {
                prec: 5,
                assoc: Assoc::Ltr,
            },

            Self::Lesser => OpInfo {
                prec: 4,
                assoc: Assoc::Ltr,
            },
            Self::LesserEqual => OpInfo {
                prec: 4,
                assoc: Assoc::Ltr,
            },
            Self::Greater => OpInfo {
                prec: 4,
                assoc: Assoc::Ltr,
            },
            Self::GreaterEqual => OpInfo {
                prec: 4,
                assoc: Assoc::Ltr,
            },

            Self::EqualEqual => OpInfo {
                prec: 3,
                assoc: Assoc::Ltr,
            },
            Self::BangEqual => OpInfo {
                prec: 3,
                assoc: Assoc::Ltr,
            },

            Self::AndAnd => OpInfo {
                prec: 2,
                assoc: Assoc::Ltr,
            },
            Self::OrOr => OpInfo {
                prec: 2,
                assoc: Assoc::Ltr,
            },

            Self::Equal => OpInfo {
                prec: 1,
                assoc: Assoc::Ltr,
            },

            _ => {
                panic!("`op_info()` has not been implemented for token: {:?}", self)
            }
        }
    }
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    fn peek(&self) -> Result<&Token, Error> {
        if let Some(token) = self.tokens.get(self.current) {
            Ok(token)
        } else {
            Err(Error {
                message: "unexpected end of file".into(),
                span: self.tokens.last().unwrap().span.clone(),
            })
        }
    }

    fn error_at_current(&self, message: &str) -> Error {
        Error {
            message: message.into(),
            span: {
                if let Some(token) = self.tokens.get(self.current) {
                    token.span.clone()
                } else {
                    self.tokens.last().unwrap().span.clone()
                }
            },
        }
    }

    fn expect(&mut self, kind: TokenKind, message: &str) -> Result<&Token, Error> {
        if let Some(token) = self.tokens.get(self.current) {
            if token.kind == kind {
                self.current += 1;
                Ok(token)
            } else {
                Err(Error {
                    message: message.into(),
                    span: token.span.clone(),
                })
            }
        } else {
            Err(Error {
                message: message.into(),
                span: self.tokens.last().unwrap().span.clone(),
            })
        }
    }

    fn parse_block(&mut self) -> Result<ast::BlockStmt, Error> {
        self.expect(TokenKind::LeftBrace, "expect block")?;

        let mut stmts = Vec::new();
        while self.peek()?.kind != TokenKind::RightBrace {
            stmts.push(self.parse_stmt()?);
        }

        self.expect(TokenKind::RightBrace, "unclosed block")?;
        if self.peek()?.kind == TokenKind::Semicolon {
            self.current += 1; // optional trailing ';'
        }

        Ok(ast::BlockStmt { stmts })
    }

    fn parse_type(&mut self) -> Result<ast::Type, Error> {
        let mut typ = None;

        match self.peek()?.kind {
            TokenKind::Ident => {
                let ident = self.peek()?.clone();
                self.current += 1;

                if (self.current < self.tokens.len()) && self.peek()?.kind == TokenKind::Dot {
                    self.current += 1;
                    let segment = self
                        .expect(TokenKind::Ident, "expect type name after module name")?
                        .clone();

                    typ = Some(ast::Type {
                        span: ident.span,
                        kind: ast::NamedType { name: segment }.into(),
                    });
                } else {
                    typ = Some(ast::Type {
                        span: ident.span.clone(),
                        kind: ast::NamedType { name: ident }.into(),
                    });
                }
            }
            TokenKind::LeftParen => {
                // Type grouping, does not get a seperate AST node but makes is
                // easier to clarify what you mean. For example to make `*int | str`
                // unambiguous you could explicitly write it as `(*int) | str` or
                // `*(int | str)`
                let grouping_start = self.peek()?.span.start;
                self.current += 1;

                let mut nested = self.parse_type()?;
                self.expect(TokenKind::RightParen, "unclosed type grouping")?;

                nested.span = grouping_start..nested.span.end;
                typ = Some(nested);
            }
            _ => {}
        }

        if let Some(typ) = typ {
            Ok(typ)
        } else {
            Err(self.error_at_current("expect type"))
        }
    }

    fn parse_primary(&mut self) -> Result<ast::Expr, Error> {
        let mut expr = None;

        let token = self.peek()?.clone();

        match &token.kind {
            TokenKind::Int | TokenKind::Float | TokenKind::True | TokenKind::False => {
                self.current += 1;
                expr = Some(ast::Expr {
                    span: token.span.clone(),
                    kind: ast::Lit { token }.into(),
                    typ: None,
                })
            }
            TokenKind::LeftParen => {
                return Err(self.error_at_current("grouping expressions are not yet implemented"))
            }
            TokenKind::Ident => {
                self.current += 1;
                if self.peek()?.kind == TokenKind::LeftBrace {
                    self.current += 1;

                    let mut inits = Vec::new();
                    if self.peek()?.kind != TokenKind::RightBrace {
                        loop {
                            let init_ident = self
                                .expect(TokenKind::Ident, "expect initializer name")?
                                .clone();
                            self.expect(
                                TokenKind::Colon,
                                "expect ':' after member name in struct literal",
                            )?;
                            let init_expr = self.parse_expr()?;
                            inits.push((init_ident, init_expr));

                            if self.peek()?.kind != TokenKind::Comma {
                                break;
                            } else {
                                self.current += 1;
                            }
                        }
                    }

                    let rbrace_token =
                        self.expect(TokenKind::RightBrace, "unclosed struct literal")?;

                    expr = Some(ast::Expr {
                        span: token.span.start..rbrace_token.span.end,
                        kind: ast::StructLit {
                            typ: ast::Type {
                                span: token.span.clone(),
                                kind: ast::NamedType {
                                    name: token.clone(),
                                }
                                .into(),
                            },
                            inits,
                        }
                        .into(),
                        typ: None,
                    })
                } else {
                    expr = Some(ast::Expr {
                        span: token.span.clone(),
                        kind: ast::VarExpr {
                            ident: token.clone(),
                        }
                        .into(),
                        typ: None,
                    })
                }
            }
            _ if token.kind.is_prefix_op() => {
                self.current += 1;
                let target = self.parse_primary()?;
                expr = Some(ast::Expr {
                    span: token.span.clone(),
                    kind: ast::UnaryExpr {
                        op: token.clone(),
                        expr: Box::new(target),
                    }
                    .into(),
                    typ: None,
                });
            }
            _ => {}
        }

        let mut expr = if let Some(expr) = expr {
            expr
        } else {
            return Err(self.error_at_current("expected expression"));
        };

        while self.peek()?.kind == TokenKind::LeftParen {
            if let TokenKind::LeftParen = self.peek()?.kind {
                self.current += 1;

                let mut args = Vec::new();
                if self.peek()?.kind != TokenKind::RightParen {
                    loop {
                        args.push(self.parse_expr()?);

                        if self.peek()?.kind != TokenKind::Comma {
                            break;
                        } else {
                            self.current += 1;
                        }
                    }
                }

                let rparen_token = self.expect(
                    TokenKind::RightParen,
                    "missing closing ')' in call expression",
                )?;

                expr = ast::Expr {
                    span: expr.span.start..rparen_token.span.end,
                    kind: ast::CallExpr {
                        callee: Box::new(expr),
                        args,
                    }
                    .into(),
                    typ: None,
                };
            }
        }

        Ok(expr)
    }

    fn parse_prec_expr(&mut self, mut lhs: ast::Expr, min_prec: u8) -> Result<ast::Expr, Error> {
        let mut lookahead = self.peek()?.clone();

        while lookahead.kind.is_binary_op() && lookahead.kind.op_info().prec >= min_prec {
            let op = lookahead;
            self.current += 1;
            let mut rhs = self.parse_primary()?;
            lookahead = self.peek()?.clone();

            while lookahead.kind.is_binary_op()
                && ((lookahead.kind.op_info().assoc == Assoc::Ltr
                    && lookahead.kind.op_info().prec > op.kind.op_info().prec)
                    || (lookahead.kind.op_info().assoc == Assoc::Rtl
                        && lookahead.kind.op_info().prec == op.kind.op_info().prec))
            {
                rhs = self.parse_prec_expr(rhs, min_prec + 1)?;
                lookahead = self.peek()?.clone();
            }

            lhs = ast::Expr {
                span: lhs.span.start..rhs.span.end,
                kind: ast::BinaryExpr {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                }
                .into(),
                typ: None,
            };
        }

        Ok(lhs)
    }

    fn parse_expr(&mut self) -> Result<ast::Expr, Error> {
        let primary = self.parse_primary()?;
        self.parse_prec_expr(primary, 0)
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt, Error> {
        let token = self.peek()?.clone();

        match &token.kind {
            TokenKind::Fn => {
                self.current += 1;

                let ident = self
                    .expect(TokenKind::Ident, "expect function name")?
                    .clone();

                let mut parameters = Vec::new();
                self.expect(TokenKind::LeftParen, "expect '(' after function name")?;

                if self.peek()?.kind != TokenKind::RightParen {
                    loop {
                        let param_ident = self
                            .expect(TokenKind::Ident, "expect parameter name")?
                            .clone();
                        let param_type = self.parse_type()?;
                        parameters.push((param_ident, param_type));

                        if self.peek()?.kind != TokenKind::Comma {
                            break;
                        } else {
                            self.current += 1;
                        }
                    }
                }

                self.expect(TokenKind::RightParen, "missing closing ')'")?;

                let return_type = match self.peek()?.kind {
                    TokenKind::LeftBrace | TokenKind::Semicolon => None,
                    _ => Some(self.parse_type()?),
                };

                self.expect(TokenKind::LeftBrace, "expect function body")?;
                self.current -= 1;
                let block = self.parse_block()?;

                Ok(ast::Stmt {
                    kind: ast::StmtKind::Fn(ast::FnDecl {
                        ident,
                        parameters,
                        return_type,
                        block,
                    }),
                    pointer: token.span,
                })
            }
            TokenKind::Struct => {
                self.current += 1;

                let ident = self.expect(TokenKind::Ident, "expect struct name")?.clone();
                self.expect(TokenKind::LeftBrace, "expect struct body")?;

                if self.peek()?.kind == TokenKind::RightBrace {
                    return Err(self.error_at_current("empty struct is illegal"));
                }

                let mut members = Vec::new();
                while self.peek()?.kind != TokenKind::RightBrace {
                    let member_ident = self
                        .expect(TokenKind::Ident, "expect struct member name")?
                        .clone();
                    let member_type = self.parse_type()?;
                    self.expect(TokenKind::Semicolon, "expect ';' after struct member type")?;
                    members.push((member_ident, member_type));
                }

                self.expect(TokenKind::RightBrace, "unclosed struct body")?;

                if self.peek()?.kind == TokenKind::Semicolon {
                    self.current += 1; // optional trailing ';'
                }

                Ok(ast::Stmt {
                    kind: ast::StmtKind::Struct(ast::StructDecl { ident, members }),
                    pointer: token.span.clone(),
                })
            }
            TokenKind::Let => {
                self.current += 1;

                let ident = self
                    .expect(TokenKind::Ident, "expect variable name")?
                    .clone();

                let typ = if self.peek()?.kind != TokenKind::Equal {
                    Some(self.parse_type()?)
                } else {
                    None
                };
                let init = if self.peek()?.kind == TokenKind::Equal {
                    self.current += 1;
                    self.parse_expr()?
                } else {
                    return Err(self.peek()?.error_at("expect variable initializer"));
                };

                self.expect(
                    TokenKind::Semicolon,
                    "expect ';' after variable declaration",
                )?;

                Ok(ast::Stmt {
                    kind: ast::StmtKind::Let(ast::LetStmt { ident, typ, init }),
                    pointer: token.span.clone(),
                })
            }
            TokenKind::If => {
                self.current += 1;

                let condition = self.parse_expr()?;
                let if_block = self.parse_block()?;
                let mut elif_stmts = Vec::new();
                let mut else_block = None;

                while self.peek()?.kind == TokenKind::Else {
                    self.current += 1;
                    if self.peek()?.kind == TokenKind::If {
                        if else_block.is_some() {
                            return Err(self.error_at_current("else if after final else block"));
                        }

                        self.current += 1;
                        let elif_cond = self.parse_expr()?;
                        let elif_block = self.parse_block()?;
                        elif_stmts.push((elif_cond, elif_block));
                    } else {
                        else_block = Some(self.parse_block()?);
                    }
                }

                Ok(ast::Stmt {
                    kind: ast::StmtKind::If(ast::IfStmt {
                        condition,
                        if_block,
                        elif_stmts,
                        else_block,
                    }),
                    pointer: token.span.clone(),
                })
            }
            TokenKind::While => {
                self.current += 1;

                let condition = self.parse_expr()?;
                let block = self.parse_block()?;

                Ok(ast::Stmt {
                    kind: ast::StmtKind::While(ast::WhileStmt { condition, block }),
                    pointer: token.span.clone(),
                })
            }
            TokenKind::Return => {
                self.current += 1;

                let value = if self.peek()?.kind != TokenKind::Semicolon {
                    Some(self.parse_expr()?)
                } else {
                    None
                };

                self.expect(TokenKind::Semicolon, "expect ';' after return statement")?;

                Ok(ast::Stmt {
                    kind: ast::StmtKind::Return(ast::ReturnStmt { value }),
                    pointer: token.span.clone(),
                })
            }
            TokenKind::LeftBrace => {
                self.current += 1;
                Ok(ast::Stmt {
                    kind: ast::StmtKind::Block(self.parse_block()?),
                    pointer: token.span.clone(),
                })
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(
                    TokenKind::Semicolon,
                    "expect ';' after top-level expression",
                )?;

                Ok(ast::Stmt {
                    pointer: expr.span.clone(),
                    kind: ast::StmtKind::Expr(ast::ExprStmt { expr }),
                })
            }
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<ast::Stmt>, Error> {
    let mut decls = Vec::new();

    if !tokens.is_empty() {
        let mut parser = Parser::new(tokens);
        while parser.peek()?.kind != TokenKind::Eof {
            decls.push(parser.parse_stmt()?);
        }
    }

    Ok(decls)
}
