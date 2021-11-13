use std::{borrow::Borrow, collections::HashMap, convert::TryInto, mem::discriminant};

use crate::{ast, common::Error, token};

#[derive(Debug)]
struct Analyzer<'a> {
    file: &'a mut ast::File,
    namespace: HashMap<String, ast::Type>,
    typespace: HashMap<String, ast::TypeKind>,

    within_function: Option<ast::FnDecl>,
}

impl<'a> Analyzer<'a> {
    fn new(file: &'a mut ast::File) -> Self {
        let mut typespace = HashMap::<String, ast::TypeKind>::new();

        typespace.insert("i8".into(), ast::PrimType::Int(8).into());
        typespace.insert("i16".into(), ast::PrimType::Int(16).into());
        typespace.insert("i32".into(), ast::PrimType::Int(32).into());
        typespace.insert("i64".into(), ast::PrimType::Int(64).into());

        typespace.insert("u8".into(), ast::PrimType::UInt(8).into());
        typespace.insert("u16".into(), ast::PrimType::UInt(16).into());
        typespace.insert("u32".into(), ast::PrimType::UInt(32).into());
        typespace.insert("u64".into(), ast::PrimType::UInt(64).into());

        typespace.insert("f32".into(), ast::PrimType::Float(32).into());
        typespace.insert("f64".into(), ast::PrimType::Float(64).into());

        typespace.insert("bool".into(), ast::PrimType::Bool.into());

        let mut namespace = HashMap::<String, ast::Type>::new();

        // For debugging purposes
        for (type_name, built_in_type) in &typespace {
            namespace.insert(
                format!("print_{}", &type_name),
                ast::Type {
                    span: 0..0,
                    kind: ast::FnType {
                        name: format!("print_{}", &type_name),
                        parameters: vec![ast::Type {
                            span: 0..0,
                            kind: built_in_type.clone(),
                        }],
                        returns: None,
                    }
                    .into(),
                },
            );
        }

        Self {
            file,
            namespace,
            typespace,

            within_function: None,
        }
    }

    fn type_eq(&self, left: &ast::Type, right: &ast::Type) -> bool {
        if discriminant(&left.kind) != discriminant(&right.kind) {
            return false;
        }

        match &left.kind {
            ast::TypeKind::Prim(left_prim_type) => {
                let right_prim_type: &ast::PrimType = right.kind.borrow().try_into().unwrap();
                left_prim_type == right_prim_type
            }
            ast::TypeKind::Struct(left_struct_type) => {
                let right_struct_type: &ast::StructType = right.kind.borrow().try_into().unwrap();
                left_struct_type.name == right_struct_type.name
            }
            _ => {
                panic!(
                    "Type equality has not been implemented for kind: {:?}",
                    left.kind
                );
            }
        }
    }

    fn analyze_type(&self, typ: &mut ast::Type) -> Result<(), Error> {
        match &mut typ.kind {
            ast::TypeKind::Named(named_type) => {
                let lexeme = self.file.lexeme(&named_type.name.span);
                if let Some(resolved_type) = self.typespace.get(lexeme) {
                    typ.kind = resolved_type.clone();
                } else {
                    return Err(Error {
                        message: "unknown named type".into(),
                        span: named_type.name.span.clone(),
                    });
                }
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn analyze_stmt(&mut self, stmt: &mut ast::Stmt) -> Result<(), Error> {
        match &mut stmt.kind {
            ast::StmtKind::Fn(fn_decl) => {
                if self.within_function.is_some() {
                    return Err(fn_decl
                        .ident
                        .error_at("nested functions have not yet been implemented"));
                }

                let function_name = self.file.lexeme(&fn_decl.ident.span);
                self.within_function = Some(fn_decl.clone());

                if let Some(return_type) = &mut fn_decl.return_type {
                    self.analyze_type(return_type)?;
                }

                for param in fn_decl.parameters.iter_mut() {
                    self.analyze_type(&mut param.1)?;

                    if self.file.lexeme(&param.0.span) == function_name {
                        return Err(param
                            .0
                            .error_at("parameter name cannot be same as function name"));
                    }

                    self.namespace
                        .insert(self.file.lexeme(&param.0.span).into(), param.1.clone());
                }

                self.namespace.insert(
                    function_name.into(),
                    ast::Type {
                        span: 0..0,
                        kind: ast::TypeKind::Fn(ast::FnType {
                            name: self.file.lexeme(&fn_decl.ident.span).to_string(),
                            parameters: fn_decl
                                .parameters
                                .iter()
                                .map(|(_, typ)| typ.clone())
                                .collect(),
                            returns: fn_decl
                                .return_type
                                .as_ref()
                                .map(|return_type| Box::new(return_type.clone())),
                        }),
                    },
                );

                self.within_function = Some(fn_decl.clone());

                for stmt in fn_decl.block.stmts.iter_mut() {
                    self.analyze_stmt(stmt)?;
                }

                self.within_function = None;
            }
            ast::StmtKind::Struct(struct_decl) => {
                for member in struct_decl.members.iter_mut() {
                    self.analyze_type(&mut member.1)?;
                }

                let struct_name = self.file.lexeme(&struct_decl.ident.span).to_string();
                let struct_type = ast::StructType {
                    name: struct_name.clone(),
                    members: struct_decl
                        .members
                        .iter()
                        .map(|(ident, typ)| (self.file.lexeme(&ident.span).into(), typ.clone()))
                        .collect(),
                };

                self.typespace
                    .insert(struct_name, ast::TypeKind::Struct(struct_type));
            }

            ast::StmtKind::Let(let_stmt) => {
                if let Some(typ) = &mut let_stmt.typ {
                    self.analyze_expr(&mut let_stmt.init)?;

                    // e.g. let x int = 32
                    if let Some(init_type) = &let_stmt.init.typ {
                        if !self.type_eq(init_type, typ) {
                            return Err(Error {
                                message: "variable initializer is not assignable to provided type"
                                    .into(),
                                span: let_stmt.init.span.clone(),
                            });
                        }
                    } else {
                        return Err(Error {
                            message: "cannot use void expression to declare variable".into(),
                            span: let_stmt.init.span.clone(),
                        });
                    }
                } else {
                    // e.g. let x = 34
                    self.analyze_expr(&mut let_stmt.init)?;
                    let_stmt.typ = let_stmt.init.typ.clone();
                }

                if let Some(let_type) = &let_stmt.typ {
                    self.namespace.insert(
                        self.file.lexeme(&let_stmt.ident.span).to_string(),
                        let_type.clone(),
                    );
                } else {
                    panic!("internal-error: could not get type for variable declaration")
                }
            }
            ast::StmtKind::If(if_stmt) => {
                self.analyze_expr(&mut if_stmt.condition)?;

                if let Some(cond_type) = &if_stmt.condition.typ {
                    if let ast::TypeKind::Prim(ast::PrimType::Bool) = cond_type.kind {
                        for stmt in if_stmt.if_block.stmts.iter_mut() {
                            self.analyze_stmt(stmt)?;
                        }

                        for (elif_cond, elif_block) in if_stmt.elif_stmts.iter_mut() {
                            self.analyze_expr(elif_cond)?;
                            if let Some(cond_type) = &elif_cond.typ {
                                if let ast::TypeKind::Prim(ast::PrimType::Bool) = cond_type.kind {
                                    for stmt in elif_block.stmts.iter_mut() {
                                        self.analyze_stmt(stmt)?;
                                    }
                                }
                            } else {
                                return Err(Error {
                                    span: if_stmt.condition.span.clone(),
                                    message: "void expression cannot be used as else if condition"
                                        .into(),
                                });
                            }
                        }

                        if let Some(else_block) = &mut if_stmt.else_block {
                            for stmt in else_block.stmts.iter_mut() {
                                self.analyze_stmt(stmt)?;
                            }
                        }
                    } else {
                        return Err(Error {
                            span: if_stmt.condition.span.clone(),
                            message: "if statement condition must be a boolean".into(),
                        });
                    }
                } else {
                    return Err(Error {
                        span: if_stmt.condition.span.clone(),
                        message: "void expression cannot be used as if statement condition".into(),
                    });
                }
            }
            ast::StmtKind::While(while_stmt) => {
                self.analyze_expr(&mut while_stmt.condition)?;
                if let Some(typ) = &while_stmt.condition.typ {
                    if let ast::TypeKind::Prim(ast::PrimType::Bool) = &typ.kind {
                        return Ok(());
                    }

                    return Err(Error {
                        message: "while condition must be of boolean type".into(),
                        span: while_stmt.condition.span.clone(),
                    });
                } else {
                    return Err(Error {
                        message: "cannot use void expression as while condition".into(),
                        span: while_stmt.condition.span.clone(),
                    });
                }
            }
            ast::StmtKind::Return(return_stmt) => {
                if let Some(value) = &mut return_stmt.value {
                    self.analyze_expr(value)?;
                }

                if let Some(current_fnc) = &self.within_function {
                    if let Some(value) = &mut return_stmt.value {
                        if let Some(return_type) = &current_fnc.return_type {
                            if let Some(value_type) = &value.typ {
                                if !self.type_eq(value_type, return_type) {
                                    return Err(Error {
                                        message: "return value is not assignable to return type"
                                            .into(),
                                        span: value.span.clone(),
                                    });
                                }
                            } else {
                                return Err(Error {
                                    message: "cannot return void expression".into(),
                                    span: value.span.clone(),
                                });
                            }
                        } else {
                            return Err(Error {
                                message: "returning value in void function".into(),
                                span: value.span.clone(),
                            });
                        }
                    } else if current_fnc.return_type.is_some() {
                        return Err(Error {
                            message: "void return in function with return type".into(),
                            span: stmt.pointer.clone(),
                        });
                    }
                } else {
                    return Err(Error {
                        message: "return statement must be inside function".into(),
                        span: stmt.pointer.clone(),
                    });
                }
            }
            ast::StmtKind::Expr(expr_stmt) => {
                self.analyze_expr(&mut expr_stmt.expr)?;
            }
            ast::StmtKind::Block(block) => {
                for stmt in block.stmts.iter_mut() {
                    self.analyze_stmt(stmt)?;
                }
            }
        }

        Ok(())
    }

    fn analyze_expr(&mut self, expr: &mut ast::Expr) -> Result<(), Error> {
        match &mut expr.kind {
            ast::ExprKind::Unary(unary_expr) => {
                self.analyze_expr(&mut unary_expr.expr)?;
                if let Some(expr_type) = &mut unary_expr.expr.typ {
                    match &unary_expr.op.kind {
                        token::TokenKind::Minus => {
                            if let ast::TypeKind::Prim(prim_type) = &expr_type.kind {
                                if prim_type.is_numeric() {
                                    expr.typ = Some(expr_type.clone());
                                    return Ok(());
                                }
                            }

                            return Err(Error {
                                message: "unary negate is only valid on numeric expressions".into(),
                                span: expr.span.clone(),
                            });
                        }
                        token::TokenKind::Bang => {
                            if let ast::TypeKind::Prim(ast::PrimType::Bool) = &expr_type.kind {
                                expr.typ = Some(ast::Type {
                                    kind: ast::TypeKind::Prim(ast::PrimType::Bool),
                                    span: 0..0,
                                });
                                return Ok(());
                            }

                            return Err(Error {
                                message: "unary not is only valid on boolean expressions".into(),
                                span: expr.span.clone(),
                            });
                        }
                        _ => {
                            panic!(
                                "Analysis has not been implemented for unary operator: {:?}",
                                unary_expr.op.kind
                            )
                        }
                    }
                } else {
                    return Err(Error {
                        message: "unary expression cannot be done on void expression".into(),
                        span: expr.span.clone(),
                    });
                }
            }
            ast::ExprKind::Binary(binary_expr) => {
                if let token::TokenKind::Dot = &binary_expr.op.kind {
                    self.analyze_expr(&mut binary_expr.left)?;
                    if let ast::ExprKind::Let(let_expr) = &binary_expr.right.kind {
                        let member_name = self.file.lexeme(&let_expr.ident.span);
                        if let Some(target_type) = &binary_expr.left.typ {
                            if let ast::TypeKind::Struct(struct_type) = &target_type.kind {
                                let found_member = struct_type
                                    .members
                                    .iter()
                                    .find(|(type_member_name, _)| type_member_name == member_name);

                                if let Some(found_member) = found_member {
                                    expr.typ = Some(found_member.1.clone());
                                } else {
                                    return Err(Error {
                                        message: format!(
                                            "field '{}' does not exist on struct type: '{}'",
                                            member_name, struct_type.name
                                        ),
                                        span: binary_expr.left.span.clone(),
                                    });
                                }
                            } else {
                                return Err(Error {
                                    message:
                                        "operator '.' must have struct type expression on left"
                                            .to_string(),
                                    span: binary_expr.left.span.clone(),
                                });
                            }
                        } else {
                            return Err(Error {
                                message: "operator '.' cannot have void expression on left"
                                    .to_string(),
                                span: binary_expr.left.span.clone(),
                            });
                        }
                    } else {
                        return Err(Error {
                            message: "operator '.' can only have an identifier on it's right"
                                .to_string(),
                            span: binary_expr.right.span.clone(),
                        });
                    }
                } else {
                    self.analyze_expr(&mut binary_expr.left)?;
                    self.analyze_expr(&mut binary_expr.right)?;

                    if let Some(left_expr_type) = &binary_expr.left.typ {
                        if let Some(right_expr_type) = &binary_expr.right.typ {
                            if !self.type_eq(right_expr_type, left_expr_type) {
                                return Err(Error {
                                message: "binary expressions must have the same type expression on both sides".into(),
                                span: expr.span.clone(),
                            });
                            }

                            if let ast::TypeKind::Prim(prim_type) = &left_expr_type.kind {
                                match &binary_expr.op.kind {
                                    token::TokenKind::Equal => {
                                        if !binary_expr.left.kind.is_lvalue() {
                                            return Err(Error {
                                                message: "left of assignment can only be variable or get expression".into(),
                                                span: binary_expr.left.span.clone(),
                                            });
                                        }
                                        expr.typ = Some(left_expr_type.clone());
                                    }

                                    token::TokenKind::Plus
                                    | token::TokenKind::Minus
                                    | token::TokenKind::Star
                                    | token::TokenKind::Slash
                                    | token::TokenKind::Percent => {
                                        if !prim_type.is_numeric() {
                                            return Err(Error {
                                            message:
                                                "binary expressions are only valid on primitive numeric operands"
                                                    .into(),
                                            span: expr.span.clone(),
                                        });
                                        }

                                        expr.typ = Some(left_expr_type.clone());
                                    }
                                    token::TokenKind::Lesser
                                    | token::TokenKind::Greater
                                    | token::TokenKind::LesserEqual
                                    | token::TokenKind::GreaterEqual
                                    | token::TokenKind::EqualEqual
                                    | token::TokenKind::BangEqual => {
                                        expr.typ = Some(ast::Type {
                                            span: 0..0,
                                            kind: ast::TypeKind::Prim(ast::PrimType::Bool),
                                        });
                                    }
                                    token::TokenKind::AndAnd | token::TokenKind::OrOr => {
                                        if !matches!(prim_type, ast::PrimType::Bool) {
                                            return Err(Error {
                                            message:
                                                "operator `&&` & `||` can only be used with boolean operands"
                                                    .into(),
                                            span: expr.span.clone(),
                                        });
                                        }
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        }
                    } else {
                        return Err(Error {
                            message: "cannot use void expression in a binary expression".into(),
                            span: expr.span.clone(),
                        });
                    }
                }
            }
            ast::ExprKind::Let(let_expr) => {
                let let_name = self.file.lexeme(&let_expr.ident.span);
                if let Some(let_type) = self.namespace.get(let_name) {
                    expr.typ = Some(let_type.clone());
                } else {
                    return Err(Error {
                        message: "undefined variable".into(),
                        span: expr.span.clone(),
                    });
                }
            }
            ast::ExprKind::Call(call_expr) => {
                self.analyze_expr(&mut call_expr.callee)?;
                for arg in call_expr.args.iter_mut() {
                    self.analyze_expr(arg)?;
                }

                if let Some(callee_type) = &call_expr.callee.typ {
                    if let ast::TypeKind::Fn(fn_type) = &callee_type.kind {
                        // Validate arguments
                        for (i, arg) in call_expr.args.iter().enumerate() {
                            if arg.typ.is_some() {
                                let param_type = &fn_type.parameters[i];
                                if let Some(arg_type) = &arg.typ {
                                    if !self.type_eq(arg_type, param_type) {
                                        return Err(Error {
                                            message: "invalid argument type".into(),
                                            span: arg.span.clone(),
                                        });
                                    }
                                } else {
                                    return Err(Error {
                                        message: "cannot use void expression as argument".into(),
                                        span: arg.span.clone(),
                                    });
                                }
                            } else {
                                return Err(Error {
                                    message: "cannot use void expression as function argument"
                                        .into(),
                                    span: call_expr.callee.span.clone(),
                                });
                            }
                        }

                        expr.typ = fn_type
                            .returns
                            .as_ref()
                            .map(|return_type| (**return_type).clone());
                    } else {
                        return Err(Error {
                            message: "callee must be of function type".into(),
                            span: call_expr.callee.span.clone(),
                        });
                    }
                } else {
                    return Err(Error {
                        message: "cannot call void expression".into(),
                        span: call_expr.callee.span.clone(),
                    });
                }
            }
            ast::ExprKind::StructLit(struct_lit) => {
                self.analyze_type(&mut struct_lit.typ)?;
                expr.typ = Some(struct_lit.typ.clone());
            }
            ast::ExprKind::Lit(lit) => {
                expr.typ = Some(match &lit.token.kind {
                    token::TokenKind::Int => ast::Type {
                        span: 0..0,
                        kind: ast::TypeKind::Prim(ast::PrimType::Int(32)),
                    },
                    token::TokenKind::Float => ast::Type {
                        span: 0..0,
                        kind: ast::TypeKind::Prim(ast::PrimType::Float(64)),
                    },
                    token::TokenKind::True => ast::Type {
                        span: 0..0,
                        kind: ast::TypeKind::Prim(ast::PrimType::Bool),
                    },
                    token::TokenKind::False => ast::Type {
                        span: 0..0,
                        kind: ast::TypeKind::Prim(ast::PrimType::Bool),
                    },
                    _ => {
                        panic!(
                            "Analysis has not yet been implemented for literal: {:?}",
                            lit.token.kind
                        )
                    }
                });
            }
        }
        Ok(())
    }

    fn analyze(&mut self) -> Result<(), Error> {
        let mut new_stmts = self.file.stmts.clone();
        for stmt in new_stmts.iter_mut() {
            self.analyze_stmt(stmt)?;
        }

        self.file.stmts = new_stmts;
        Ok(())
    }
}

#[allow(dead_code)]
pub fn analyze(file: &ast::File) -> Result<ast::File, Error> {
    let mut new_file = file.clone();
    let mut analyzer = Analyzer::new(&mut new_file);
    analyzer.analyze()?;
    Ok(new_file)
}

pub fn analyze_mut(file: &mut ast::File) -> Result<(), Error> {
    let mut analyzer = Analyzer::new(file);
    analyzer.analyze()
}
