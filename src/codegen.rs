use std::collections::HashMap;

use crate::{
    ast,
    inst::{self, Register},
    lexer,
};

#[derive(Debug)]
struct Generator {
    current_stack_offset: u64,
    namespace: HashMap<String, u64>,
}

impl Generator {
    fn gen_expression(
        &mut self,
        expr: &ast::Expression,
        block: &mut inst::Block,
    ) -> inst::Register {
        match &expr.variant {
            ast::ExpressionEnum::Literal(token) => match token.typ {
                lexer::TokenType::Int => {
                    let int_value = inst::Imm::Int(token.lexeme.parse::<i64>().unwrap());
                    block.insts.push(inst::Inst::Rega(Register::R8, int_value));
                    Register::R8
                }
                lexer::TokenType::Float => {
                    let float_value = inst::Imm::Float(token.lexeme.parse::<f64>().unwrap());
                    block
                        .insts
                        .push(inst::Inst::Rega(Register::R8, float_value));
                    Register::R8
                }
                _ => unreachable!(),
            },
            ast::ExpressionEnum::Variable(ident) => {
                let resolved_stack_offset = self.namespace.get(&ident.lexeme).unwrap();
                block.insts.push(inst::Inst::Rega(
                    Register::R16,
                    inst::Imm::Int(*resolved_stack_offset as i64),
                ));
                block
                    .insts
                    .push(inst::Inst::Load(Register::R9, Register::R16));
                Register::R9
            }
            ast::ExpressionEnum::Print(expr) => {
                let expr_reg = self.gen_expression(expr, block);
                if let Some(expr_type) = &expr.typ {
                    match &expr_type {
                        ast::Type::Primitive(prim_type) => match &prim_type {
                            ast::PrimitiveType::Int => {
                                block.insts.push(inst::Inst::PrintInt(expr_reg));
                            }
                            ast::PrimitiveType::Float => {
                                block.insts.push(inst::Inst::PrintFloat(expr_reg));
                            }
                        },
                    }
                } else {
                    panic!("uncaught printing of void expression")
                }

                Register::R0 // This should never get used, analysis should catch any uses of void expressions
            }
            ast::ExpressionEnum::Block {
                exprs,
                lbrace_token: _,
            } => {
                let block_result_reg = Register::R10;
                for (i, expr) in exprs.iter().enumerate() {
                    let expr_reg = self.gen_expression(expr, block);
                    if i == exprs.len() - 1 {
                        block
                            .insts
                            .push(inst::Inst::Copy(block_result_reg, expr_reg));
                    }
                }
                block_result_reg
            }
            ast::ExpressionEnum::Binary { left, right, op } => match op.typ {
                lexer::TokenType::ColonEqual => {
                    if let ast::ExpressionEnum::Variable(ident) = &left.variant {
                        let initializer_reg = self.gen_expression(right, block);

                        self.namespace
                            .insert(ident.lexeme.clone(), self.current_stack_offset);

                        block.insts.push(inst::Inst::Rega(
                            Register::R16,
                            inst::Imm::Int(self.current_stack_offset as i64),
                        ));
                        block
                            .insts
                            .push(inst::Inst::Store(Register::R16, initializer_reg));

                        self.current_stack_offset += 8;
                        Register::R0 // Also never used, since declaration is void
                    } else {
                        panic!("uncaught non-variable lhs in variable binding")
                    }
                }
                lexer::TokenType::Equal => {
                    if let ast::ExpressionEnum::Variable(ident) = &left.variant {
                        let resolved_stack_offset = self.namespace.get(&ident.lexeme).unwrap();
                        block.insts.push(inst::Inst::Rega(
                            Register::R16,
                            inst::Imm::Int(*resolved_stack_offset as i64),
                        ));
                        block
                            .insts
                            .push(inst::Inst::Load(Register::R11, Register::R16));
                        Register::R11
                    } else {
                        panic!("uncaught non-variable lhs in variable assignment")
                    }
                }
                _ => {
                    let mut right_reg = self.gen_expression(&right, block);
                    block.insts.push(inst::Inst::Copy(Register::R12, right_reg));
                    right_reg = Register::R12;

                    let mut left_reg = self.gen_expression(&left, block);
                    block.insts.push(inst::Inst::Copy(Register::R13, left_reg));
                    left_reg = Register::R13;

                    let result_reg = Register::R14;

                    if let Some(left_type) = &left.typ {
                        match &left_type {
                            ast::Type::Primitive(primitive_type) => match primitive_type {
                                ast::PrimitiveType::Int => match op.typ {
                                    lexer::TokenType::Plus => block
                                        .insts
                                        .push(inst::Inst::SAdd(result_reg, left_reg, right_reg)),
                                    lexer::TokenType::Minus => block
                                        .insts
                                        .push(inst::Inst::Sub(result_reg, left_reg, right_reg)),
                                    lexer::TokenType::Star => block
                                        .insts
                                        .push(inst::Inst::SMul(result_reg, left_reg, right_reg)),
                                    lexer::TokenType::Slash => block
                                        .insts
                                        .push(inst::Inst::SDiv(result_reg, left_reg, right_reg)),
                                    _ => unreachable!(),
                                },
                                ast::PrimitiveType::Float => match op.typ {
                                    lexer::TokenType::Plus => block
                                        .insts
                                        .push(inst::Inst::FAdd(result_reg, left_reg, right_reg)),
                                    lexer::TokenType::Minus => block
                                        .insts
                                        .push(inst::Inst::FSub(result_reg, left_reg, right_reg)),
                                    lexer::TokenType::Star => block
                                        .insts
                                        .push(inst::Inst::FMul(result_reg, left_reg, right_reg)),
                                    lexer::TokenType::Slash => block
                                        .insts
                                        .push(inst::Inst::FDiv(result_reg, left_reg, right_reg)),
                                    _ => unreachable!(),
                                },
                            },
                        }

                        result_reg
                    } else {
                        panic!("void expression as binary expression's left-hand, not caught in analysis phase")
                    }
                }
            },
        }
    }
}

pub fn gen(exprs: &[ast::Expression]) -> Vec<inst::Block> {
    let mut generator = Generator {
        current_stack_offset: 0,
        namespace: HashMap::new(),
    };

    let mut block = inst::Block {
        label: "main".to_string(),
        insts: Vec::new(),
    };
    for expr in exprs {
        generator.gen_expression(expr, &mut block);
    }

    vec![block]
}
