use std::{borrow::Borrow, collections::HashMap};

use crate::{
    ast,
    inst::{self, Register},
    token,
};

#[derive(Debug)]
struct Generator<'a> {
    current_stack_offset: u64,
    available_tmp_registers: Vec<Register>,
    blocks: Vec<inst::Block>,
    file: &'a ast::File,
    namespace: HashMap<String, u64>,
}

impl<'a> Generator<'a> {
    fn get_tmp_reg(&mut self) -> Register {
        let tmp_reg = if let Some(tmp_reg) = self.available_tmp_registers.get(0) {
            *tmp_reg
        } else {
            // I'm unsure if it's actually possible to reach here
            panic!("internal-compiler-error: ran out of temporary registers during code generation")
        };

        self.available_tmp_registers.retain(|reg| *reg != tmp_reg);
        tmp_reg
    }

    fn make_reg_available(&mut self, to_free: &Register) {
        if !self.available_tmp_registers.contains(to_free) {
            self.available_tmp_registers.push(*to_free);
        }
    }

    fn gen_stmt(&mut self, stmt: &ast::Stmt) {
        match &stmt.kind {
            ast::StmtKind::Fun(fun_decl) => {
                let fun_init_block = inst::Block {
                    label: format!("{}_init", self.file.lexeme(&fun_decl.ident.span)),
                    insts: Vec::new(),
                };
                self.blocks.push(fun_init_block);

                for stmt in &fun_decl.block.stmts {
                    self.gen_stmt(stmt);
                }
            }
            ast::StmtKind::Struct(struct_decl) => todo!(),

            ast::StmtKind::Var(var_stmt) => {
                let mut current_block = if let Some(current_block) = self.blocks.last() {
                    current_block.clone()
                } else {
                    panic!("top-level variable declaration")
                };

                let initializer_reg = self.gen_expression(&var_stmt.init, &mut current_block);

                self.namespace.insert(
                    self.file.lexeme(&var_stmt.ident.span).to_string(),
                    self.current_stack_offset,
                );

                current_block.insts.push(inst::Inst::Rega(
                    Register::R16,
                    inst::Imm::Int(self.current_stack_offset as i64),
                ));
                current_block
                    .insts
                    .push(inst::Inst::Store(Register::R16, initializer_reg));

                let last_idx = self.blocks.len() - 1;
                self.blocks[last_idx] = current_block;

                self.make_reg_available(&initializer_reg);
                self.current_stack_offset += 8;
            }
            ast::StmtKind::If(if_stmt) => todo!(),
            ast::StmtKind::While(while_stmt) => todo!(),
            ast::StmtKind::Return(return_stmt) => todo!(),
            ast::StmtKind::Expr(expr_stmt) => todo!(),
            ast::StmtKind::Block(block_stmt) => todo!(),
        }
    }

    fn gen_expression(&mut self, expr: &ast::Expr, block: &mut inst::Block) -> Register {
        match &expr.kind {
            ast::ExprKind::Unary(unary_expr) => {
                let target_reg = self.gen_expression(&(*unary_expr.expr), block);
                match &unary_expr.op.kind {
                    token::TokenKind::Minus => {
                        todo!()
                    }
                    token::TokenKind::Bang => {
                        todo!()
                    }
                    _ => unreachable!(),
                }
            }
            ast::ExprKind::Binary(binary_expr) => {
                if binary_expr.op.kind == token::TokenKind::Equal {
                    let right_reg = self.gen_expression(&(*binary_expr.right), block);
                    if let ast::ExprKind::Var(var_expr) = &binary_expr.left.kind {
                        let resolved_stack_offset = self
                            .namespace
                            .get(self.file.lexeme(&var_expr.ident.span))
                            .unwrap();
                        block.insts.push(inst::Inst::Rega(
                            Register::R16,
                            inst::Imm::Int(*resolved_stack_offset as i64),
                        ));
                        block
                            .insts
                            .push(inst::Inst::Store(Register::R16, right_reg));
                        right_reg
                    } else {
                        panic!("can only assign to variables")
                    }
                } else {
                    let left_reg = self.gen_expression(&(*binary_expr.left), block);
                    let right_reg = self.gen_expression(&(*binary_expr.right), block);
                    let result_reg = self.get_tmp_reg();

                    if let ast::TypeKind::Prim(prim_type) =
                        &binary_expr.left.typ.borrow().as_ref().unwrap().kind
                    {
                        match &binary_expr.op.kind {
                            token::TokenKind::Plus => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::SAdd(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::UInt(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::UAdd(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::Float(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::FAdd(result_reg, left_reg, right_reg));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Minus => match &prim_type {
                                ast::PrimType::Int(_) | ast::PrimType::UInt(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::Sub(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::Float(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::FSub(result_reg, left_reg, right_reg));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Star => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::SMul(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::UInt(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::UMul(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::Float(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::FMul(result_reg, left_reg, right_reg));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Slash => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::SDiv(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::UInt(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::UDiv(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::Float(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::FDiv(result_reg, left_reg, right_reg));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Percent => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::SRem(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::UInt(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::URem(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::Float(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::FRem(result_reg, left_reg, right_reg));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Lesser => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::SLt(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::UInt(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::ULt(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::Float(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::FLt(result_reg, left_reg, right_reg));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Greater => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::SGt(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::UInt(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::UGt(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::Float(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::FGt(result_reg, left_reg, right_reg));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::LesserEqual => {
                                todo!()
                            }
                            token::TokenKind::GreaterEqual => {
                                todo!()
                            }
                            token::TokenKind::EqualEqual => match &prim_type {
                                ast::PrimType::Int(_) | ast::PrimType::UInt(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::Eq(result_reg, left_reg, right_reg));
                                }
                                ast::PrimType::Float(_) => {
                                    block
                                        .insts
                                        .push(inst::Inst::FEq(result_reg, left_reg, right_reg));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::BangEqual => {
                                todo!()
                            }
                            token::TokenKind::AndAnd => {
                                todo!()
                            }
                            token::TokenKind::OrOr => {
                                todo!()
                            }
                            _ => unreachable!(),
                        }

                        // Since these can now be overwritten
                        self.make_reg_available(&left_reg);
                        self.make_reg_available(&right_reg);
                        result_reg
                    } else {
                        panic!("binary operations are only valid on primitives")
                    }
                }
            }
            ast::ExprKind::Var(var_expr) => {
                let resolved_stack_offset = self
                    .namespace
                    .get(self.file.lexeme(&var_expr.ident.span))
                    .unwrap();
                block.insts.push(inst::Inst::Rega(
                    Register::R16,
                    inst::Imm::Int(*resolved_stack_offset as i64),
                ));

                let reg = self.get_tmp_reg();
                block.insts.push(inst::Inst::Load(reg, Register::R16));
                reg
            }
            ast::ExprKind::Call(call_expr) => todo!(),
            ast::ExprKind::StructLit(struct_lit) => todo!(),
            ast::ExprKind::Lit(lit) => {
                let reg = self.get_tmp_reg();

                match &lit.token.kind {
                    token::TokenKind::Int => {
                        let int_value = inst::Imm::Int(
                            self.file.lexeme(&lit.token.span).parse::<i64>().unwrap(),
                        );
                        block.insts.push(inst::Inst::Rega(reg, int_value));
                    }
                    token::TokenKind::Float => {
                        let float_value = inst::Imm::Float(
                            self.file.lexeme(&lit.token.span).parse::<f64>().unwrap(),
                        );
                        block.insts.push(inst::Inst::Rega(reg, float_value));
                    }
                    token::TokenKind::True => {
                        block.insts.push(inst::Inst::Rega(reg, inst::Imm::Int(1)));
                    }
                    token::TokenKind::False => {
                        block.insts.push(inst::Inst::Rega(reg, inst::Imm::Int(0)));
                    }
                    _ => unreachable!(),
                }

                reg
            }
        }
    }
}

pub fn gen(file: &ast::File) -> Vec<inst::Block> {
    let mut generator = Generator {
        current_stack_offset: 0,
        available_tmp_registers: vec![
            Register::R8,
            Register::R9,
            Register::R10,
            Register::R11,
            Register::R12,
            Register::R13,
            Register::R14,
            Register::R15,
        ],
        blocks: Vec::new(),
        file,
        namespace: HashMap::new(),
    };

    for stmt in &file.stmts {
        generator.gen_stmt(stmt);
    }

    generator.blocks
}
