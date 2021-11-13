use std::{collections::HashMap, convert::TryFrom, io::Write, thread, time::Duration};

use super::inst::*;
use crate::shared::{Imm, Register, Registers};

#[derive(Debug, Clone)]
pub struct VM<'a, W: Write, const MEMORY_SIZE: usize> {
    registers: Registers,
    memory: [u8; MEMORY_SIZE],
    writer: W,

    insts: Vec<Inst>,
    block_table: HashMap<&'a str, usize>,
}

impl<'a, W: Write, const MEMORY_SIZE: usize> VM<'a, W, MEMORY_SIZE> {
    pub fn new(blocks: &'a [Block], writer: W) -> Self {
        let mut block_table = HashMap::new();
        let mut insts = Vec::new();

        for block in blocks {
            block_table.insert(
                block.label.as_str(),
                if insts.is_empty() { 0 } else { insts.len() },
            );

            for inst in &block.insts {
                insts.push(inst.clone());
            }
        }

        Self {
            registers: Registers::new(),
            memory: [0; MEMORY_SIZE],
            writer,

            insts,
            block_table,
        }
    }

    fn resolve_operand(&self, operand: &Operand) -> u64 {
        match operand {
            Operand::Data(reg) => self.registers.get(reg),
            Operand::Adr(reg) => {
                let offset = self.registers.get(reg);
                // Find offset in memory and read 8 bytes forward
                let bytes: [u8; 8] = [
                    self.memory[usize::try_from(offset)
                        .expect("This VM can only function on 64 bit machines.")],
                    self.memory[usize::try_from(offset + 1)
                        .expect("This VM can only function on 64 bit machines.")],
                    self.memory[usize::try_from(offset + 2)
                        .expect("This VM can only function on 64 bit machines.")],
                    self.memory[usize::try_from(offset + 3)
                        .expect("This VM can only function on 64 bit machines.")],
                    self.memory[usize::try_from(offset + 4)
                        .expect("This VM can only function on 64 bit machines.")],
                    self.memory[usize::try_from(offset + 5)
                        .expect("This VM can only function on 64 bit machines.")],
                    self.memory[usize::try_from(offset + 6)
                        .expect("This VM can only function on 64 bit machines.")],
                    self.memory[usize::try_from(offset + 7)
                        .expect("This VM can only function on 64 bit machines.")],
                ];
                u64::from_ne_bytes(bytes)
            }
            Operand::Imm(imm) => imm.as_u64(),
        }
    }

    fn get_inst_offset(&self, target: &Target) -> u64 {
        match target {
            Target::Label(label) => {
                if let Some(inst_offset) = self.block_table.get(label.0.as_str()) {
                    (*inst_offset) as u64
                } else {
                    panic!("undefined label ID in `jump`")
                }
            }
            Target::Pointer(reg) => self.registers.get(reg),
        }
    }

    fn store(&mut self, dst: &Operand, src: &Operand) {
        match dst {
            Operand::Imm(_) => {
                panic!("move destination cannot be immediate value")
            }
            Operand::Data(reg) => {
                self.registers.set(reg, self.resolve_operand(src));
            }
            Operand::Adr(reg) => {
                let offset = self.registers.get(reg);

                match src {
                    Operand::Imm(_) | Operand::Data(_) => {
                        let value = self.resolve_operand(src);
                        let bytes = value.to_ne_bytes();
                        self.memory[offset as usize] = bytes[0];
                        self.memory[(offset + 1) as usize] = bytes[1];
                        self.memory[(offset + 2) as usize] = bytes[2];
                        self.memory[(offset + 3) as usize] = bytes[3];
                        self.memory[(offset + 4) as usize] = bytes[4];
                        self.memory[(offset + 5) as usize] = bytes[5];
                        self.memory[(offset + 6) as usize] = bytes[6];
                        self.memory[(offset + 7) as usize] = bytes[7];
                    }
                    Operand::Adr(src_adr_reg) => {
                        let src_offset = self.registers.get(src_adr_reg);
                        for i in 0..8 {
                            self.memory[(offset + i) as usize] =
                                self.memory[(src_offset + i) as usize];
                        }
                    }
                }
            }
        }
    }

    fn execute_syscall(&mut self, id: u64) {
        match id {
            0 => {
                let uint_value = self.registers.get(&Register::R1);
                writeln!(self.writer, "{}", uint_value).unwrap();
            }
            1 => {
                let int_value = self.registers.get(&Register::R1) as i64;
                writeln!(self.writer, "{}", int_value).unwrap();
            }
            2 => {
                let float_value = f32::from_bits(self.registers.get(&Register::R1) as u32);
                writeln!(self.writer, "{}", float_value).unwrap();
            }
            3 => {
                let double_value = f64::from_bits(self.registers.get(&Register::R1));
                writeln!(self.writer, "{}", double_value).unwrap();
            }
            4 => {
                let duration = self.registers.get(&Register::R1);
                thread::sleep(Duration::from_millis(duration));
            }
            _ => panic!("Unknown syscall ID: {}", id),
        }
    }

    pub fn interpret_inst(&mut self, inst: &Inst) {
        match inst {
            Inst::SysCall(operand) => {
                let raw_value = self.resolve_operand(operand);
                self.execute_syscall(raw_value);
            }

            Inst::Move(dst, src) => self.store(dst, src),

            Inst::Jump(target) => {
                let target_inst_offset = self.get_inst_offset(target);
                self.registers.set(&Register::Rip, target_inst_offset);
                return; // return early to avoid the ip increment
            }
            Inst::CJump(cond, target) => {
                if self.resolve_operand(cond) == 1 {
                    let target_inst_offset = self.get_inst_offset(target);
                    self.registers.set(&Register::Rip, target_inst_offset);
                    return;
                } else {
                    panic!("undefined target label ID in `cjump`")
                }
            }
            Inst::Branch(cond, true_target, false_target) => {
                let inst_offset = if self.resolve_operand(cond) == 1 {
                    self.get_inst_offset(true_target)
                } else {
                    self.get_inst_offset(false_target)
                };

                self.registers.set(&Register::Rip, inst_offset);
                return;
            }

            // Arithmetic operations
            Inst::SAdd(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs) as i64;
                let rhs_value = self.resolve_operand(rhs) as i64;
                let sum = lhs_value + rhs_value;
                self.store(dst, &Operand::Imm(Imm::Int(sum as u64)));
            }
            Inst::UAdd(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                let sum = lhs_value + rhs_value;
                self.store(dst, &Operand::Imm(Imm::Int(sum)));
            }

            Inst::Sub(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs) as i64;
                let rhs_value = self.resolve_operand(rhs) as i64;
                let difference = lhs_value - rhs_value;
                self.store(dst, &Operand::Imm(Imm::Int(difference as u64)));
            }

            Inst::SMul(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs) as i64;
                let rhs_value = self.resolve_operand(rhs) as i64;
                let product = lhs_value * rhs_value;
                self.store(dst, &Operand::Imm(Imm::Int(product as u64)));
            }
            Inst::UMul(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                let product = lhs_value * rhs_value;
                self.store(dst, &Operand::Imm(Imm::Int(product)));
            }

            Inst::SDiv(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs) as i64;
                let rhs_value = self.resolve_operand(rhs) as i64;
                let quotient = lhs_value / rhs_value;
                self.store(dst, &Operand::Imm(Imm::Int(quotient as u64)));
            }
            Inst::UDiv(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                let quotient = lhs_value / rhs_value;
                self.store(dst, &Operand::Imm(Imm::Int(quotient)));
            }

            Inst::SRem(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs) as i64;
                let rhs_value = self.resolve_operand(rhs) as i64;
                let remainder = lhs_value % rhs_value;
                self.store(dst, &Operand::Imm(Imm::Int(remainder as u64)));
            }
            Inst::URem(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                let remainder = lhs_value % rhs_value;
                self.store(dst, &Operand::Imm(Imm::Int(remainder)));
            }

            Inst::FAdd(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.resolve_operand(lhs));
                let rhs_value = f64::from_bits(self.resolve_operand(rhs));
                let sum = lhs_value + rhs_value;
                self.store(dst, &Operand::Imm(Imm::Float(sum)));
            }
            Inst::FSub(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.resolve_operand(lhs));
                let rhs_value = f64::from_bits(self.resolve_operand(rhs));
                let difference = lhs_value - rhs_value;
                self.store(dst, &Operand::Imm(Imm::Float(difference)));
            }
            Inst::FMul(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.resolve_operand(lhs));
                let rhs_value = f64::from_bits(self.resolve_operand(rhs));
                let product = lhs_value * rhs_value;
                self.store(dst, &Operand::Imm(Imm::Float(product)));
            }
            Inst::FDiv(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.resolve_operand(lhs));
                let rhs_value = f64::from_bits(self.resolve_operand(rhs));
                let quotient = lhs_value / rhs_value;
                self.store(dst, &Operand::Imm(Imm::Float(quotient)));
            }
            Inst::FRem(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.resolve_operand(lhs));
                let rhs_value = f64::from_bits(self.resolve_operand(rhs));
                let remainder = lhs_value % rhs_value;
                self.store(dst, &Operand::Imm(Imm::Float(remainder)));
            }

            // Bitwise operations
            Inst::Shl(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                self.store(dst, &Operand::Imm(Imm::Int(lhs_value << rhs_value)));
            }
            Inst::Shr(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                self.store(dst, &Operand::Imm(Imm::Int(lhs_value >> rhs_value)));
            }
            Inst::And(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                self.store(dst, &Operand::Imm(Imm::Int(lhs_value & rhs_value)));
            }
            Inst::Or(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                self.store(dst, &Operand::Imm(Imm::Int(lhs_value | rhs_value)));
            }
            Inst::Xor(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                self.store(dst, &Operand::Imm(Imm::Int(lhs_value ^ rhs_value)));
            }
            Inst::Not(dst, src) => {
                let value = self.resolve_operand(src);
                let result = !value;
                self.store(dst, &Operand::Imm(Imm::Int(result)));
            }

            // Comparative operations
            Inst::Eq(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                self.store(
                    dst,
                    &Operand::Imm(if lhs_value == rhs_value {
                        Imm::True
                    } else {
                        Imm::False
                    }),
                );
            }
            Inst::FEq(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);

                // We directly compare the bits instad of converting back to f64
                self.store(
                    dst,
                    &Operand::Imm(if lhs_value == rhs_value {
                        Imm::True
                    } else {
                        Imm::False
                    }),
                );
            }

            Inst::SLt(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs) as i64;
                let rhs_value = self.resolve_operand(rhs) as i64;
                self.store(
                    dst,
                    &Operand::Imm(if lhs_value < rhs_value {
                        Imm::True
                    } else {
                        Imm::False
                    }),
                );
            }
            Inst::ULt(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                self.store(
                    dst,
                    &Operand::Imm(if lhs_value < rhs_value {
                        Imm::True
                    } else {
                        Imm::False
                    }),
                );
            }
            Inst::FLt(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.resolve_operand(lhs));
                let rhs_value = f64::from_bits(self.resolve_operand(rhs));
                self.store(
                    dst,
                    &Operand::Imm(if lhs_value < rhs_value {
                        Imm::True
                    } else {
                        Imm::False
                    }),
                );
            }

            Inst::SGt(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs) as i64;
                let rhs_value = self.resolve_operand(rhs) as i64;
                self.store(
                    dst,
                    &Operand::Imm(if lhs_value > rhs_value {
                        Imm::True
                    } else {
                        Imm::False
                    }),
                );
            }
            Inst::UGt(dst, lhs, rhs) => {
                let lhs_value = self.resolve_operand(lhs);
                let rhs_value = self.resolve_operand(rhs);
                self.store(
                    dst,
                    &Operand::Imm(if lhs_value > rhs_value {
                        Imm::True
                    } else {
                        Imm::False
                    }),
                );
            }
            Inst::FGt(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.resolve_operand(lhs));
                let rhs_value = f64::from_bits(self.resolve_operand(rhs));
                self.store(
                    dst,
                    &Operand::Imm(if lhs_value > rhs_value {
                        Imm::True
                    } else {
                        Imm::False
                    }),
                );
            }
        }

        let rip = self.registers.get(&Register::Rip);
        if rip != u64::MAX {
            self.registers.set(&Register::Rip, rip + 1);
        }
    }

    pub fn interpret(&mut self) {
        if let Some(main_routine_begin) = self.block_table.get("main") {
            let main_routine_begin = *main_routine_begin;
            self.registers
                .set(&Register::Rip, main_routine_begin as u64);

            while self.registers.get(&Register::Rip) != u64::MAX {
                let inst = if let Some(inst) = self
                    .insts
                    .get((self.registers.get(&Register::Rip)) as usize)
                {
                    inst.clone()
                } else {
                    unreachable!()
                };

                self.interpret_inst(&inst);
            }
        }
    }
}
