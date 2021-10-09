use std::{
    fs, io,
    path::{Path, PathBuf},
};

use derive_more::{From, TryInto};

use crate::{common::Span, token};

#[derive(Debug, Clone, PartialEq)]
pub enum PrimType {
    Int(u8),
    UInt(u8),
    Float(u8),
    Bool,
}

impl PrimType {
    pub fn is_numeric(&self) -> bool {
        !matches!(self, Self::Bool)
    }
}

#[derive(Debug, Clone)]
pub struct FnType {
    pub name: String,
    pub parameters: Vec<Type>,
    pub returns: Option<Box<Type>>,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub name: String,
    pub members: Vec<(String, Type)>,
}

// #[derive(Debug, Clone)]
// pub struct PtrType {
//     pub eltype: Box<Type>,
// }

// Arbitrary named type. can resolve to any kind of type (currently
// only struct and primitive types, but once aliases are introduced)
#[derive(Debug, Clone)]
pub struct NamedType {
    pub name: token::Token,
}

#[derive(Debug, Clone, From, TryInto)]
#[try_into(ref, ref_mut)]
pub enum TypeKind {
    Prim(PrimType),
    Fn(FnType),
    Struct(StructType),
    Named(NamedType),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub ident: token::Token,
    pub parameters: Vec<(token::Token, Type)>,
    pub return_type: Option<Type>,
    pub block: BlockStmt,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub ident: token::Token,
    pub members: Vec<(token::Token, Type)>,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub ident: token::Token,
    pub typ: Option<Type>,
    pub init: Expr,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub if_block: BlockStmt,
    pub elif_stmts: Vec<(Expr, BlockStmt)>,
    pub else_block: Option<BlockStmt>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub block: BlockStmt,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, From, TryInto)]
pub enum StmtKind {
    Fn(FnDecl),
    Struct(StructDecl),

    Let(LetStmt),
    If(IfStmt),
    While(WhileStmt),
    Return(ReturnStmt),
    Expr(ExprStmt),
    Block(BlockStmt),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub pointer: Span,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: token::Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub op: token::Token,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct LetExpr {
    pub ident: token::Token,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructLit {
    pub typ: Type,
    pub inits: Vec<(token::Token, Expr)>,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub token: token::Token,
}

#[derive(Debug, Clone, From, TryInto)]
pub enum ExprKind {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Let(LetExpr),
    Call(CallExpr),
    StructLit(StructLit),
    Lit(Lit),
}

impl ExprKind {
    pub fn is_lvalue(&self) -> bool {
        match self {
            Self::Let(_) => true,
            Self::Binary(binary_expr) => binary_expr.op.kind == token::TokenKind::Dot,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub typ: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct File {
    pub path: PathBuf,
    pub source: String,
    pub stmts: Vec<Stmt>,
}

impl File {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, io::Error> {
        let path = path.as_ref().to_path_buf();
        let source = fs::read_to_string(&path)?;

        Ok(Self {
            path,
            source,
            stmts: Vec::new(),
        })
    }

    pub fn lexeme(&self, span: &Span) -> &str {
        &self.source[span.clone()]
    }
}
