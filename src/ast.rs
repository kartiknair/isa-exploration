use std::mem::discriminant;

use crate::{lexer, shared::Pos};

#[derive(Debug, Clone)]
pub enum ExpressionEnum {
    Literal(lexer::Token),
    Variable(lexer::Token),
    Print(Box<Expression>),
    Block {
        lbrace_token: lexer::Token,
        exprs: Vec<Expression>,
    },
    Binary {
        left: Box<Expression>,
        right: Box<Expression>,
        op: lexer::Token,
    },
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub variant: ExpressionEnum,
    pub typ: Option<Type>,
}

impl Expression {
    pub fn get_pos(&self) -> Pos {
        match &self.variant {
            ExpressionEnum::Literal(token) => token.pos,
            ExpressionEnum::Variable(ident) => ident.pos,
            ExpressionEnum::Print(expr) => expr.get_pos(),
            ExpressionEnum::Block {
                exprs: _,
                lbrace_token,
            } => lbrace_token.pos,
            ExpressionEnum::Binary {
                left: _,
                right: _,
                op,
            } => op.pos,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitiveType {
    Int,
    Float,
}

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
}

impl Type {
    pub fn equals(&self, other: &Self) -> bool {
        match self {
            Self::Primitive(primitive) => {
                let Self::Primitive(other_primitive) = other;
                discriminant(primitive) == discriminant(other_primitive)
            }
        }
    }
}
