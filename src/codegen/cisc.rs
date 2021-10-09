use std::{borrow::Borrow, collections::HashMap, convert::TryInto};

use crate::{
    ast,
    cisc::inst::{self, Imm, Inst, Operand, Register},
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

    fn make_operand_reg_available(&mut self, operand: &Operand) {
        match operand {
            Operand::Adr(reg) => self.make_reg_available(reg),
            Operand::Data(reg) => self.make_reg_available(reg),
            Operand::Imm(_) => {}
        }
    }

    fn gen_stmt(&mut self, stmt: &ast::Stmt) {
        match &stmt.kind {
            ast::StmtKind::Fn(fn_decl) => {
                let label = self.file.lexeme(&fn_decl.ident.span).to_string();
                let mut fn_init_block = inst::Block {
                    label,
                    insts: Vec::new(),
                };

                let pre_fn_stack_offset = self.current_stack_offset;
                if fn_decl.parameters.len() > 8 {
                    panic!("can only generate assembly for functions with less than 8 parameters")
                }

                for (i, (param_ident, _)) in fn_decl.parameters.iter().enumerate() {
                    let param_reg = Register::from_id((i + 1).try_into().unwrap());

                    fn_init_block.insts.push(Inst::Move(
                        Operand::Adr(Register::Rsp),
                        Operand::Data(param_reg),
                    ));

                    self.namespace.insert(
                        self.file.lexeme(&param_ident.span).to_string(),
                        self.current_stack_offset,
                    );

                    fn_init_block.insts.push(Inst::UAdd(
                        Operand::Data(Register::Rsp),
                        Operand::Data(Register::Rsp),
                        Operand::Imm(Imm::Int(8)),
                    ));
                    self.current_stack_offset += 8;
                }

                self.blocks.push(fn_init_block);

                for stmt in &fn_decl.block.stmts {
                    self.gen_stmt(stmt);
                }

                if self.file.lexeme(&fn_decl.ident.span) == "main" {
                    let final_block = self.blocks.last_mut().unwrap();
                    final_block.insts.push(Inst::Move(
                        Operand::Data(Register::Rip),
                        Operand::Imm(Imm::Int(u64::MAX)),
                    ));
                }

                self.current_stack_offset = pre_fn_stack_offset;
            }
            ast::StmtKind::Struct(_) => {
                // Nothing to do
            }

            ast::StmtKind::Let(let_stmt) => {
                let mut block = if let Some(block) = self.blocks.last() {
                    block.clone()
                } else {
                    unreachable!()
                };

                let initializer = self.gen_expression(&let_stmt.init, &mut block);
                let last_idx = self.blocks.len() - 1;

                self.namespace.insert(
                    self.file.lexeme(&let_stmt.ident.span).to_string(),
                    self.current_stack_offset,
                );

                block
                    .insts
                    .push(Inst::Move(Operand::Adr(Register::Rsp), initializer));
                self.make_operand_reg_available(&initializer);

                self.current_stack_offset += 8;
                block.insts.push(Inst::UAdd(
                    Operand::Data(Register::Rsp),
                    Operand::Imm(Imm::Int(8)),
                    Operand::Data(Register::Rsp),
                ));

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
                    let value = self.gen_expression(value, &mut block);
                    let last_idx = self.blocks.len() - 1;
                    self.blocks[last_idx] = block;

                    let block = if let Some(block) = self.blocks.last_mut() {
                        block
                    } else {
                        panic!("top-level variable declaration")
                    };

                    block.insts.push(Inst::Move(
                        Operand::Data(return_adr_reg),
                        Operand::Data(Register::R0),
                    ));
                    block
                        .insts
                        .push(Inst::Move(Operand::Data(Register::R0), value));
                    block.insts.push(Inst::Move(
                        Operand::Data(Register::Rip),
                        Operand::Data(return_adr_reg),
                    ));

                    self.make_reg_available(&return_adr_reg);
                    self.make_operand_reg_available(&value)
                } else {
                    let current_block = if let Some(current_block) = self.blocks.last_mut() {
                        current_block
                    } else {
                        panic!("top-level variable declaration")
                    };

                    current_block.insts.push(Inst::Move(
                        Operand::Data(Register::Rip),
                        Operand::Data(Register::R0),
                    ));
                }
            }
            ast::StmtKind::Expr(expr_stmt) => {
                let mut block = if let Some(block) = self.blocks.last() {
                    block.clone()
                } else {
                    unreachable!()
                };
                let expr_value = self.gen_expression(&expr_stmt.expr, &mut block);
                let last_idx = self.blocks.len() - 1;
                self.blocks[last_idx] = block;
                self.make_operand_reg_available(&expr_value)
            }
            ast::StmtKind::Block(_block_stmt) => todo!(),
        }
    }

    fn gen_expression(&mut self, expr: &ast::Expr, block: &mut inst::Block) -> Operand {
        let expr_reg = match &expr.kind {
            ast::ExprKind::Unary(unary_expr) => {
                let operand = self.gen_expression(&(*unary_expr.expr), block);
                let result_reg = self.get_tmp_reg();

                match &unary_expr.op.kind {
                    token::TokenKind::Minus => match &unary_expr.expr.typ.as_ref().unwrap().kind {
                        ast::TypeKind::Prim(prim_type) => match &prim_type {
                            ast::PrimType::Int(_) | ast::PrimType::UInt(_) => {
                                block.insts.push(Inst::Move(
                                    Operand::Data(result_reg),
                                    Operand::Imm(Imm::Int(0)),
                                ));
                                block.insts.push(Inst::Sub(
                                    Operand::Data(result_reg),
                                    Operand::Data(result_reg),
                                    operand,
                                ));
                            }
                            ast::PrimType::Float(_) => {
                                block.insts.push(Inst::Move(
                                    Operand::Data(result_reg),
                                    Operand::Imm(Imm::Float(0.0)),
                                ));
                                block.insts.push(Inst::FSub(
                                    Operand::Data(result_reg),
                                    Operand::Data(result_reg),
                                    operand,
                                ));
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    },
                    token::TokenKind::Bang => {
                        block
                            .insts
                            .push(Inst::Not(Operand::Data(result_reg), operand));
                    }
                    _ => unreachable!(),
                }

                self.make_operand_reg_available(&operand);

                Operand::Data(result_reg)
            }
            ast::ExprKind::Binary(binary_expr) => {
                if binary_expr.op.kind == token::TokenKind::Equal {
                    let right_value = self.gen_expression(&(*binary_expr.right), block);
                    if let ast::ExprKind::Let(let_expr) = &binary_expr.left.kind {
                        let resolved_stack_offset = *self
                            .namespace
                            .get(self.file.lexeme(&let_expr.ident.span))
                            .unwrap();

                        let adr_reg = self.get_tmp_reg();
                        block.insts.push(Inst::Move(
                            Operand::Data(adr_reg),
                            Operand::Imm(Imm::Int(resolved_stack_offset)),
                        ));
                        block.insts.push(Inst::UAdd(
                            Operand::Data(adr_reg),
                            Operand::Data(Register::Rfp),
                            Operand::Data(adr_reg),
                        ));
                        block
                            .insts
                            .push(Inst::Move(Operand::Adr(adr_reg), right_value));

                        self.make_reg_available(&adr_reg);
                        right_value
                    } else {
                        panic!("can only assign to variables")
                    }
                } else if binary_expr.op.kind == token::TokenKind::Dot {
                    let struct_pointer = match self.gen_expression(&(*binary_expr.left), block) {
                        Operand::Adr(ptr_reg) => ptr_reg,
                        _ => {
                            panic!("struct value must be generated as pointer into the stack")
                        }
                    };

                    let member_name =
                        if let ast::ExprKind::Let(let_expr) = &(*binary_expr.right).kind {
                            self.file.lexeme(&let_expr.ident.span)
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

                        block.insts.push(Inst::Move(
                            Operand::Data(member_adr_reg),
                            Operand::Imm(Imm::Int((member_offset * 8) as u64)),
                        ));
                        block.insts.push(Inst::UAdd(
                            Operand::Data(member_adr_reg),
                            Operand::Data(struct_pointer),
                            Operand::Data(member_adr_reg),
                        ));

                        let value_reg = self.get_tmp_reg();
                        block.insts.push(Inst::Move(
                            Operand::Data(value_reg),
                            Operand::Data(member_adr_reg),
                        ));

                        self.make_reg_available(&member_adr_reg);
                        self.make_reg_available(&struct_pointer);

                        Operand::Data(value_reg)
                    } else {
                        unreachable!()
                    }
                } else {
                    let left_value = self.gen_expression(&(*binary_expr.left), block);
                    let right_value = self.gen_expression(&(*binary_expr.right), block);
                    let result_reg = self.get_tmp_reg();

                    if let ast::TypeKind::Prim(prim_type) =
                        &binary_expr.left.typ.borrow().as_ref().unwrap().kind
                    {
                        match &binary_expr.op.kind {
                            token::TokenKind::Plus => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block.insts.push(Inst::SAdd(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::UInt(_) => {
                                    block.insts.push(Inst::UAdd(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::Float(_) => {
                                    block.insts.push(Inst::FAdd(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Minus => match &prim_type {
                                ast::PrimType::Int(_) | ast::PrimType::UInt(_) => {
                                    block.insts.push(Inst::Sub(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::Float(_) => {
                                    block.insts.push(Inst::FSub(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Star => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block.insts.push(Inst::SMul(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::UInt(_) => {
                                    block.insts.push(Inst::UMul(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::Float(_) => {
                                    block.insts.push(Inst::FMul(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Slash => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block.insts.push(Inst::SDiv(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::UInt(_) => {
                                    block.insts.push(Inst::UDiv(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::Float(_) => {
                                    block.insts.push(Inst::FDiv(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Percent => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block.insts.push(Inst::SRem(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::UInt(_) => {
                                    block.insts.push(Inst::URem(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::Float(_) => {
                                    block.insts.push(Inst::FRem(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Lesser => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block.insts.push(Inst::SLt(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::UInt(_) => {
                                    block.insts.push(Inst::ULt(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::Float(_) => {
                                    block.insts.push(Inst::FLt(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                _ => unreachable!(),
                            },
                            token::TokenKind::Greater => match &prim_type {
                                ast::PrimType::Int(_) => {
                                    block.insts.push(Inst::SGt(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::UInt(_) => {
                                    block.insts.push(Inst::UGt(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::Float(_) => {
                                    block.insts.push(Inst::FGt(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
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
                                    block.insts.push(Inst::Eq(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
                                }
                                ast::PrimType::Float(_) => {
                                    block.insts.push(Inst::FEq(
                                        Operand::Data(result_reg),
                                        left_value,
                                        right_value,
                                    ));
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
                        self.make_operand_reg_available(&left_value);
                        self.make_operand_reg_available(&right_value);

                        Operand::Data(result_reg)
                    } else {
                        panic!("binary operations are only valid on primitives")
                    }
                }
            }
            ast::ExprKind::Let(let_expr) => {
                let resolved_stack_offset = *self
                    .namespace
                    .get(self.file.lexeme(&let_expr.ident.span))
                    .unwrap();

                let adr_reg = self.get_tmp_reg();
                block.insts.push(Inst::Move(
                    Operand::Data(adr_reg),
                    Operand::Imm(Imm::Int(resolved_stack_offset)),
                ));
                block.insts.push(Inst::UAdd(
                    Operand::Data(adr_reg),
                    Operand::Data(Register::Rfp),
                    Operand::Data(adr_reg),
                ));

                let reg = self.get_tmp_reg();
                block
                    .insts
                    .push(Inst::Move(Operand::Data(reg), Operand::Adr(adr_reg)));
                self.make_reg_available(&adr_reg);

                Operand::Data(reg)
            }
            ast::ExprKind::Call(call_expr) => {
                if let Some(callee_type) = &call_expr.callee.typ {
                    if let ast::TypeKind::Fn(fn_type) = &callee_type.kind {
                        for (i, arg) in call_expr.args.iter().enumerate() {
                            let param_reg = Register::from_id((i + 1).try_into().unwrap());
                            let expr_value = self.gen_expression(arg, block);
                            block
                                .insts
                                .push(Inst::Move(Operand::Data(param_reg), expr_value));
                            self.make_operand_reg_available(&expr_value);
                        }

                        block.insts.push(Inst::Move(
                            Operand::Data(Register::R15),
                            Operand::Data(Register::Rsp),
                        ));
                        block.insts.push(Inst::UAdd(
                            Operand::Data(Register::Rfp),
                            Operand::Data(Register::Rsp),
                            Operand::Data(Register::Rfp),
                        ));

                        block.insts.push(Inst::Move(
                            Operand::Data(Register::R0),
                            Operand::Data(Register::Rip),
                        ));

                        block.insts.push(Inst::UAdd(
                            Operand::Data(Register::R0),
                            Operand::Data(Register::R0),
                            Operand::Imm(Imm::Int(2)),
                        ));
                        block
                            .insts
                            .push(Inst::Jump(inst::Target::Label(inst::Label::new(
                                &fn_type.name,
                            ))));

                        block.insts.push(Inst::Move(
                            Operand::Data(Register::Rsp),
                            Operand::Data(Register::R15),
                        ));
                        block.insts.push(Inst::Sub(
                            Operand::Data(Register::Rfp),
                            Operand::Data(Register::Rfp),
                            Operand::Data(Register::Rsp),
                        ));

                        Operand::Data(Register::R0)
                    } else {
                        panic!("callee has non fnctiion type")
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
                    block.insts.push(Inst::Move(
                        Operand::Data(struct_stack_offset_reg),
                        Operand::Data(Register::Rsp),
                    ));

                    for (member_name, _) in &struct_type.members {
                        let member_value = initializers[member_name.as_str()];
                        let member_value = self.gen_expression(member_value, block);

                        block
                            .insts
                            .push(Inst::Move(Operand::Data(Register::Rsp), member_value));
                        block.insts.push(Inst::UAdd(
                            Operand::Data(Register::Rsp),
                            Operand::Imm(Imm::Int(8)),
                            Operand::Data(Register::Rsp),
                        ));

                        self.make_operand_reg_available(&member_value);
                        self.current_stack_offset += 8;
                    }

                    Operand::Adr(struct_stack_offset_reg)
                } else {
                    unreachable!()
                }
            }
            ast::ExprKind::Lit(lit) => Operand::Imm(match &lit.token.kind {
                token::TokenKind::Int => {
                    Imm::Int(self.file.lexeme(&lit.token.span).parse::<i64>().unwrap() as u64)
                }
                token::TokenKind::Float => {
                    Imm::Float(self.file.lexeme(&lit.token.span).parse::<f64>().unwrap())
                }
                token::TokenKind::True => Imm::True,
                token::TokenKind::False => Imm::False,
                _ => unreachable!(),
            }),
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
                        ast::PrimType::Int(_) => Inst::PrintInt(Operand::Data(Register::R1)),
                        ast::PrimType::UInt(_) => Inst::PrintUInt(Operand::Data(Register::R1)),
                        ast::PrimType::Float(_) => Inst::PrintFloat(Operand::Data(Register::R1)),
                        ast::PrimType::Bool => Inst::PrintInt(Operand::Data(Register::R1)),
                    },
                    _ => unreachable!(),
                },
                Inst::Move(Operand::Data(Register::Rip), Operand::Data(Register::R0)),
            ],
        });
    }

    for stmt in &file.stmts {
        generator.gen_stmt(stmt);
    }

    generator.blocks
}
