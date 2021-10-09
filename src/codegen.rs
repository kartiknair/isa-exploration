use std::{borrow::Borrow, collections::HashMap, convert::TryInto};

use crate::{
    ast,
    risc::inst::{self, Register},
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
        if !self.available_tmp_registers.contains(to_free)
            && (*to_free >= Register::R8 && *to_free <= Register::R15)
        {
            self.available_tmp_registers.push(*to_free);
        }
    }

    fn gen_stmt(&mut self, stmt: &ast::Stmt) {
        match &stmt.kind {
            ast::StmtKind::Fun(fun_decl) => {
                let label = self.file.lexeme(&fun_decl.ident.span).to_string();
                let mut fun_init_block = inst::Block {
                    label,
                    insts: Vec::new(),
                };

                let pre_fun_stack_adr = self.current_stack_offset;
                if fun_decl.parameters.len() > 8 {
                    panic!("can only generate assembly for functions with less than 8 parameters")
                }

                for (i, (param_ident, _)) in fun_decl.parameters.iter().enumerate() {
                    let param_reg = Register::from_id((i + 1).try_into().unwrap());

                    let param_adr_reg = self.get_tmp_reg();
                    fun_init_block.insts.push(inst::Inst::Rega(
                        param_adr_reg,
                        inst::Imm::Int(self.current_stack_offset),
                    ));
                    fun_init_block.insts.push(inst::Inst::UAdd(
                        param_adr_reg,
                        Register::Rfp,
                        param_adr_reg,
                    ));
                    fun_init_block
                        .insts
                        .push(inst::Inst::Store(param_adr_reg, param_reg));
                    self.make_reg_available(&param_adr_reg);

                    self.namespace.insert(
                        self.file.lexeme(&param_ident.span).to_string(),
                        self.current_stack_offset,
                    );
                    self.current_stack_offset += 8;
                }

                self.blocks.push(fun_init_block);

                for stmt in &fun_decl.block.stmts {
                    self.gen_stmt(stmt);
                }

                if self.file.lexeme(&fun_decl.ident.span) == "main" {
                    let final_block = self.blocks.last_mut().unwrap();
                    final_block
                        .insts
                        .push(inst::Inst::Rega(Register::Rip, inst::Imm::Int(u64::MAX)));
                }

                self.current_stack_offset = pre_fun_stack_adr;
            }
            ast::StmtKind::Struct(_) => {
                // Nothing to do
            }

            ast::StmtKind::Var(var_stmt) => {
                let mut block = if let Some(block) = self.blocks.last() {
                    block.clone()
                } else {
                    unreachable!()
                };

                let initializer_reg = self.gen_expression(&var_stmt.init, &mut block);
                let last_idx = self.blocks.len() - 1;

                self.namespace.insert(
                    self.file.lexeme(&var_stmt.ident.span).to_string(),
                    self.current_stack_offset,
                );

                let adr_reg = self.get_tmp_reg();
                block.insts.push(inst::Inst::Rega(
                    adr_reg,
                    inst::Imm::Int(self.current_stack_offset),
                ));
                block
                    .insts
                    .push(inst::Inst::UAdd(adr_reg, Register::Rfp, adr_reg));
                block
                    .insts
                    .push(inst::Inst::Store(adr_reg, initializer_reg));

                self.make_reg_available(&adr_reg);
                self.make_reg_available(&initializer_reg);
                self.current_stack_offset += 8;
                self.blocks[last_idx] = block;
            }
            ast::StmtKind::If(_if_stmt) => todo!(),
            ast::StmtKind::While(_while_stmt) => todo!(),
            ast::StmtKind::Return(return_stmt) => {
                if let Some(value) = &return_stmt.value {
                    let return_adr_reg = self.get_tmp_reg();

                    let mut block = if let Some(block) = self.blocks.last() {
                        block.clone()
                    } else {
                        unreachable!()
                    };
                    let value_reg = self.gen_expression(value, &mut block);
                    let last_idx = self.blocks.len() - 1;
                    self.blocks[last_idx] = block;

                    let current_block = if let Some(current_block) = self.blocks.last_mut() {
                        current_block
                    } else {
                        panic!("top-level variable declaration")
                    };

                    current_block
                        .insts
                        .push(inst::Inst::Copy(return_adr_reg, Register::R0));
                    current_block
                        .insts
                        .push(inst::Inst::Copy(Register::R0, value_reg));
                    current_block
                        .insts
                        .push(inst::Inst::Copy(Register::Rip, return_adr_reg));

                    self.make_reg_available(&return_adr_reg);
                    self.make_reg_available(&value_reg);
                } else {
                    let current_block = if let Some(current_block) = self.blocks.last_mut() {
                        current_block
                    } else {
                        panic!("top-level variable declaration")
                    };

                    current_block
                        .insts
                        .push(inst::Inst::Copy(Register::Rip, Register::R0));
                }
            }
            ast::StmtKind::Expr(expr_stmt) => {
                let mut block = if let Some(block) = self.blocks.last() {
                    block.clone()
                } else {
                    unreachable!()
                };
                let expr_reg = self.gen_expression(&expr_stmt.expr, &mut block);
                let last_idx = self.blocks.len() - 1;
                self.blocks[last_idx] = block;

                self.make_reg_available(&expr_reg);
            }
            ast::StmtKind::Block(_block_stmt) => todo!(),
        }
    }

    fn gen_expression(&mut self, expr: &ast::Expr, block: &mut inst::Block) -> Register {
        let expr_reg = match &expr.kind {
            ast::ExprKind::Unary(unary_expr) => {
                let operand_reg = self.gen_expression(&(*unary_expr.expr), block);
                let result_reg = self.get_tmp_reg();

                match &unary_expr.op.kind {
                    token::TokenKind::Minus => match &unary_expr.expr.typ.as_ref().unwrap().kind {
                        ast::TypeKind::Prim(prim_type) => match &prim_type {
                            ast::PrimType::Int(_) | ast::PrimType::UInt(_) => {
                                block
                                    .insts
                                    .push(inst::Inst::Rega(result_reg, inst::Imm::Int(0)));
                                block.insts.push(inst::Inst::Sub(
                                    result_reg,
                                    result_reg,
                                    operand_reg,
                                ));
                            }
                            ast::PrimType::Float(_) => {
                                block
                                    .insts
                                    .push(inst::Inst::Rega(result_reg, inst::Imm::Float(0.0)));
                                block.insts.push(inst::Inst::FSub(
                                    result_reg,
                                    result_reg,
                                    operand_reg,
                                ));
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    },
                    token::TokenKind::Bang => {
                        block.insts.push(inst::Inst::Not(result_reg, operand_reg));
                    }
                    _ => unreachable!(),
                }

                self.make_reg_available(&operand_reg);
                result_reg
            }
            ast::ExprKind::Binary(binary_expr) => {
                if binary_expr.op.kind == token::TokenKind::Equal {
                    let right_reg = self.gen_expression(&(*binary_expr.right), block);
                    if let ast::ExprKind::Var(var_expr) = &binary_expr.left.kind {
                        let resolved_stack_offset = *self
                            .namespace
                            .get(self.file.lexeme(&var_expr.ident.span))
                            .unwrap();

                        let adr_reg = self.get_tmp_reg();
                        block.insts.push(inst::Inst::Rega(
                            adr_reg,
                            inst::Imm::Int(resolved_stack_offset),
                        ));
                        block
                            .insts
                            .push(inst::Inst::UAdd(adr_reg, Register::Rfp, adr_reg));
                        block.insts.push(inst::Inst::Store(adr_reg, right_reg));

                        self.make_reg_available(&adr_reg);
                        right_reg
                    } else {
                        panic!("can only assign to variables")
                    }
                } else if binary_expr.op.kind == token::TokenKind::Dot {
                    let struct_pointer = self.gen_expression(&(*binary_expr.left), block);
                    let member_name =
                        if let ast::ExprKind::Var(var_expr) = &(*binary_expr.right).kind {
                            self.file.lexeme(&var_expr.ident.span)
                        } else {
                            unreachable!()
                        };

                    let struct_type = if let Some(left_type) = &(*binary_expr.left).typ {
                        if let ast::TypeKind::Struct(struct_type) = &left_type.kind {
                            struct_type
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    };

                    let member_offset = struct_type
                        .members
                        .iter()
                        .position(|(type_member_name, _)| type_member_name == member_name);
                    if let Some(member_offset) = member_offset {
                        let member_adr_reg = self.get_tmp_reg();

                        block.insts.push(inst::Inst::Rega(
                            member_adr_reg,
                            inst::Imm::Int((member_offset * 8) as u64),
                        ));
                        block.insts.push(inst::Inst::UAdd(
                            member_adr_reg,
                            struct_pointer,
                            member_adr_reg,
                        ));

                        let value_reg = self.get_tmp_reg();
                        block
                            .insts
                            .push(inst::Inst::Load(value_reg, member_adr_reg));

                        self.make_reg_available(&member_adr_reg);
                        self.make_reg_available(&struct_pointer);

                        value_reg
                    } else {
                        unreachable!()
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
                let resolved_stack_offset = *self
                    .namespace
                    .get(self.file.lexeme(&var_expr.ident.span))
                    .unwrap();

                let adr_reg = self.get_tmp_reg();
                block.insts.push(inst::Inst::Rega(
                    adr_reg,
                    inst::Imm::Int(resolved_stack_offset),
                ));
                block
                    .insts
                    .push(inst::Inst::UAdd(adr_reg, Register::Rfp, adr_reg));

                let reg = self.get_tmp_reg();
                block.insts.push(inst::Inst::Load(reg, adr_reg));
                self.make_reg_available(&adr_reg);
                reg
            }
            ast::ExprKind::Call(call_expr) => {
                if let Some(callee_type) = &call_expr.callee.typ {
                    if let ast::TypeKind::Fun(fun_type) = &callee_type.kind {
                        for (i, arg) in call_expr.args.iter().enumerate() {
                            let param_reg = Register::from_id((i + 1).try_into().unwrap());
                            let expr_reg = self.gen_expression(arg, block);
                            block.insts.push(inst::Inst::Copy(param_reg, expr_reg));
                            self.make_reg_available(&expr_reg);
                        }

                        let offset_reg = self.get_tmp_reg();
                        block
                            .insts
                            .push(inst::Inst::Copy(Register::Rsp, Register::Rfp));
                        block.insts.push(inst::Inst::Rega(
                            offset_reg,
                            inst::Imm::Int(self.current_stack_offset),
                        ));
                        block.insts.push(inst::Inst::UAdd(
                            Register::Rfp,
                            offset_reg,
                            Register::Rfp,
                        ));
                        self.make_reg_available(&offset_reg);

                        block
                            .insts
                            .push(inst::Inst::Copy(Register::R0, Register::Rip));

                        let inst_offset_reg = self.get_tmp_reg();
                        block
                            .insts
                            .push(inst::Inst::Rega(inst_offset_reg, inst::Imm::Int(3)));
                        block.insts.push(inst::Inst::UAdd(
                            Register::R0,
                            Register::R0,
                            inst_offset_reg,
                        ));
                        block
                            .insts
                            .push(inst::Inst::Jump(inst::Label::new(&fun_type.name)));
                        self.make_reg_available(&inst_offset_reg);

                        block
                            .insts
                            .push(inst::Inst::Copy(Register::Rfp, Register::Rsp));

                        Register::R0
                    } else {
                        panic!("callee has non functiion type")
                    }
                } else {
                    panic!("callee is void type")
                }
            }
            ast::ExprKind::StructLit(struct_lit) => {
                if let ast::TypeKind::Struct(struct_type) = &struct_lit.typ.kind {
                    let mut initializers = HashMap::new();
                    for (member_ident, member_value) in &struct_lit.inits {
                        initializers.insert(self.file.lexeme(&member_ident.span), member_value);
                    }

                    let struct_stack_offset_reg = self.get_tmp_reg();
                    block.insts.push(inst::Inst::Rega(
                        struct_stack_offset_reg,
                        inst::Imm::Int(self.current_stack_offset),
                    ));
                    block.insts.push(inst::Inst::UAdd(
                        struct_stack_offset_reg,
                        Register::Rfp,
                        struct_stack_offset_reg,
                    ));

                    for (member_name, _) in &struct_type.members {
                        let member_value = initializers[member_name.as_str()];
                        let member_reg = self.gen_expression(member_value, block);

                        let member_adr_reg = self.get_tmp_reg();
                        block.insts.push(inst::Inst::Rega(
                            member_adr_reg,
                            inst::Imm::Int(self.current_stack_offset),
                        ));
                        block.insts.push(inst::Inst::UAdd(
                            member_adr_reg,
                            Register::Rfp,
                            member_adr_reg,
                        ));
                        block
                            .insts
                            .push(inst::Inst::Store(member_adr_reg, member_reg));

                        self.make_reg_available(&member_adr_reg);
                        self.make_reg_available(&member_reg);
                        self.current_stack_offset += 8;
                    }

                    struct_stack_offset_reg
                } else {
                    unreachable!()
                }
            }
            ast::ExprKind::Lit(lit) => {
                let reg = self.get_tmp_reg();

                match &lit.token.kind {
                    token::TokenKind::Int => {
                        let int_value = inst::Imm::Int(
                            self.file.lexeme(&lit.token.span).parse::<i64>().unwrap() as u64,
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
                        block.insts.push(inst::Inst::Rega(reg, inst::Imm::True));
                    }
                    token::TokenKind::False => {
                        block.insts.push(inst::Inst::Rega(reg, inst::Imm::False));
                    }
                    _ => unreachable!(),
                }

                reg
            }
        };

        expr_reg
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

    // For debugging purposes
    for (type_name, built_in_type) in &typespace {
        generator.blocks.push(inst::Block {
            label: format!("print_{}", type_name),
            insts: vec![
                match &built_in_type {
                    ast::TypeKind::Prim(prim_type) => match &prim_type {
                        ast::PrimType::Int(_) => inst::Inst::PrintInt(Register::R1),
                        ast::PrimType::UInt(_) => inst::Inst::PrintUInt(Register::R1),
                        ast::PrimType::Float(_) => inst::Inst::PrintFloat(Register::R1),
                        ast::PrimType::Bool => inst::Inst::PrintInt(Register::R1),
                    },
                    _ => unreachable!(),
                },
                inst::Inst::Copy(Register::Rip, Register::R0),
            ],
        });
    }

    for stmt in &file.stmts {
        generator.gen_stmt(stmt);
    }

    generator.blocks
}
