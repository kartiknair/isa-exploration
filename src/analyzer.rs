use std::collections::HashMap;

use crate::{ast, lexer, shared::Error};

#[derive(Debug)]
struct Analyzer<'a> {
    expressions: &'a [ast::Expression],
    symbol_table: HashMap<String, ast::Type>,
}

impl<'a> Analyzer<'a> {
    fn analyze(&mut self) -> Result<Vec<ast::Expression>, Error> {
        let mut typed_expressions = self.expressions.to_vec();

        for expr in typed_expressions.iter_mut() {
            self.analyze_expression(expr)?;
        }

        Ok(typed_expressions)
    }

    fn analyze_expression(&mut self, expr: &mut ast::Expression) -> Result<(), Error> {
        match &mut expr.variant {
            ast::ExpressionEnum::Block {
                exprs,
                lbrace_token: _,
            } => {
                for expr in exprs.iter_mut() {
                    self.analyze_expression(expr)?;
                }

                if let Some(last) = exprs.last() {
                    expr.typ = last.typ.clone();
                }
            }
            ast::ExpressionEnum::Literal(token) => {
                expr.typ = Some(match token.typ {
                    lexer::TokenType::Int => ast::Type::Primitive(ast::PrimitiveType::Int),
                    lexer::TokenType::Float => ast::Type::Primitive(ast::PrimitiveType::Float),
                    _ => panic!(
                        "type analysis not yet implemented for primitive type: '{:?}'",
                        token.typ
                    ),
                });
            }
            ast::ExpressionEnum::Variable(ident) => {
                let resolved_type = self.symbol_table.get(&ident.lexeme);

                if let Some(resolved_type) = resolved_type {
                    expr.typ = Some(resolved_type.clone());
                } else {
                    return Err(Error {
                        pos: ident.pos,
                        message: "undefined variable".into(),
                    });
                }
            }
            ast::ExpressionEnum::Print(to_print) => {
                self.analyze_expression(to_print)?;
            }
            ast::ExpressionEnum::Binary { left, right, op } => {
                self.analyze_expression(right)?;

                if let lexer::TokenType::ColonEqual = op.typ {
                    if let ast::ExpressionEnum::Variable(ident) = &left.variant {
                        if let Some(typ) = right.typ.as_ref() {
                            self.symbol_table.insert(ident.lexeme.clone(), typ.clone());
                        } else {
                            return Err(Error {
                                pos: right.get_pos(),
                                message: "cannot use void expression as variable initializer"
                                    .into(),
                            });
                        }
                    } else {
                        return Err(Error {
                            pos: left.get_pos(),
                            message: "left-hand of declaration must be a variable".into(),
                        });
                    }
                } else {
                    self.analyze_expression(left)?;
                    if !left
                        .typ
                        .as_ref()
                        .unwrap()
                        .equals(right.typ.as_ref().unwrap())
                    {
                        return Err(Error {
                            pos: right.get_pos(),
                            message: "left and right side of binary expression must have same type"
                                .into(),
                        });
                    }
                }

                expr.typ = right.typ.clone();
            }
        }

        Ok(())
    }
}

pub fn analyze(expressions: &[ast::Expression]) -> Result<Vec<ast::Expression>, Error> {
    let mut analyzer = Analyzer {
        expressions,
        symbol_table: HashMap::new(),
    };
    analyzer.analyze()
}
