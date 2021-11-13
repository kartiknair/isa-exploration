use std::{collections::HashMap, convert::TryFrom, io::Write, thread, time::Duration};

use super::inst::*;
use crate::shared::{Register, Registers};

#[derive(Debug, Clone)]
pub struct VM<'a, W: Write, const MEMORY_SIZE: usize> {
    registers: Registers,
    memory: [u8; MEMORY_SIZE],
    pub writer: W,

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
            Inst::SysCall(reg) => {
                let raw_value = self.registers.get(reg);
                self.execute_syscall(raw_value);
            }

            Inst::Rega(dst, value) => self.registers.set(dst, value.as_u64()),
            Inst::Copy(dst, src) => self.registers.set(dst, self.registers.get(src)),
            Inst::Load(dst, adr) => {
                let offset = self.registers.get(adr);
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
                let value = u64::from_ne_bytes(bytes);
                self.registers.set(dst, value)
            }
            Inst::Store(adr, val) => {
                let value = self.registers.get(val);
                let offset = self.registers.get(adr);

                let bytes = value.to_ne_bytes();
                self.memory[usize::try_from(offset)
                    .expect("This VM can only function on 64 bit machines.")] = bytes[0];
                self.memory[usize::try_from(offset + 1)
                    .expect("This VM can only function on 64 bit machines.")] = bytes[1];
                self.memory[usize::try_from(offset + 2)
                    .expect("This VM can only function on 64 bit machines.")] = bytes[2];
                self.memory[usize::try_from(offset + 3)
                    .expect("This VM can only function on 64 bit machines.")] = bytes[3];
                self.memory[usize::try_from(offset + 4)
                    .expect("This VM can only function on 64 bit machines.")] = bytes[4];
                self.memory[usize::try_from(offset + 5)
                    .expect("This VM can only function on 64 bit machines.")] = bytes[5];
                self.memory[usize::try_from(offset + 6)
                    .expect("This VM can only function on 64 bit machines.")] = bytes[6];
                self.memory[usize::try_from(offset + 7)
                    .expect("This VM can only function on 64 bit machines.")] = bytes[7];
            }

            Inst::Jump(target_label) => {
                if let Some(inst_offset) = self.block_table.get(target_label.0.as_str()) {
                    self.registers.set(&Register::Rip, (*inst_offset) as u64);
                    return; // return early to avoid the ip increment
                } else {
                    panic!("undefined label ID in `jump`")
                }
            }
            Inst::CJump(cond, target_label) => {
                if self.registers.get(cond) == 1 {
                    if let Some(target_block_offset) = self.block_table.get(target_label.0.as_str())
                    {
                        self.registers
                            .set(&Register::Rip, *target_block_offset as u64);
                        return;
                    } else {
                        panic!("undefined target label ID in `cjump`")
                    }
                }
            }
            Inst::Branch(cond, true_label, false_label) => {
                if self.registers.get(cond) == 1 {
                    if let Some(target_block_offset) = self.block_table.get(true_label.0.as_str()) {
                        self.registers
                            .set(&Register::Rip, *target_block_offset as u64);
                        return;
                    } else {
                        panic!("undefined target label ID in `branch`")
                    }
                } else if let Some(target_block_offset) =
                    self.block_table.get(false_label.0.as_str())
                {
                    self.registers
                        .set(&Register::Rip, *target_block_offset as u64);
                    return;
                } else {
                    panic!("undefined target label ID in `branch`")
                }
            }

            // Arithmetic operations
            Inst::SAdd(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs) as i64;
                let rhs_value = self.registers.get(rhs) as i64;
                let sum = lhs_value + rhs_value;
                self.registers.set(dst, sum as u64)
            }
            Inst::UAdd(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                let sum = lhs_value + rhs_value;
                self.registers.set(dst, sum)
            }
            Inst::Sub(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs) as i64;
                let rhs_value = self.registers.get(rhs) as i64;
                let diff = lhs_value - rhs_value;
                self.registers.set(dst, diff as u64)
            }
            Inst::SMul(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs) as i64;
                let rhs_value = self.registers.get(rhs) as i64;
                let product = lhs_value * rhs_value;
                self.registers.set(dst, product as u64)
            }
            Inst::UMul(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                let product = lhs_value * rhs_value;
                self.registers.set(dst, product)
            }
            Inst::SDiv(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs) as i64;
                let rhs_value = self.registers.get(rhs) as i64;
                let quotient = lhs_value / rhs_value;
                self.registers.set(dst, quotient as u64)
            }
            Inst::UDiv(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                let quotient = lhs_value / rhs_value;
                self.registers.set(dst, quotient)
            }
            Inst::SRem(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs) as i64;
                let rhs_value = self.registers.get(rhs) as i64;
                let remainder = lhs_value % rhs_value;
                self.registers.set(dst, remainder as u64)
            }
            Inst::URem(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                let remainder = lhs_value % rhs_value;
                self.registers.set(dst, remainder)
            }
            Inst::FAdd(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.registers.get(lhs));
                let rhs_value = f64::from_bits(self.registers.get(rhs));
                let sum = lhs_value + rhs_value;
                self.registers.set(dst, sum.to_bits())
            }
            Inst::FSub(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.registers.get(lhs));
                let rhs_value = f64::from_bits(self.registers.get(rhs));
                let difference = lhs_value - rhs_value;
                self.registers.set(dst, difference.to_bits())
            }
            Inst::FMul(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.registers.get(lhs));
                let rhs_value = f64::from_bits(self.registers.get(rhs));
                let product = lhs_value * rhs_value;
                self.registers.set(dst, product.to_bits())
            }
            Inst::FDiv(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.registers.get(lhs));
                let rhs_value = f64::from_bits(self.registers.get(rhs));
                let quotient = lhs_value / rhs_value;
                self.registers.set(dst, quotient.to_bits())
            }
            Inst::FRem(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.registers.get(lhs));
                let rhs_value = f64::from_bits(self.registers.get(rhs));
                let remainder = lhs_value % rhs_value;
                self.registers.set(dst, remainder.to_bits())
            }

            // Bitwise operations
            Inst::Shl(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                self.registers.set(dst, lhs_value << rhs_value)
            }
            Inst::Shr(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                self.registers.set(dst, lhs_value >> rhs_value)
            }
            Inst::And(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                self.registers.set(dst, lhs_value & rhs_value)
            }
            Inst::Or(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                self.registers.set(dst, lhs_value | rhs_value)
            }
            Inst::Xor(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                self.registers.set(dst, lhs_value ^ rhs_value)
            }
            Inst::Not(dst, src) => {
                let value = self.registers.get(src);
                let result = !value;
                self.registers.set(dst, result)
            }

            // Comparative operations
            Inst::Eq(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                self.registers.set(dst, (lhs_value == rhs_value) as u64)
            }
            Inst::FEq(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.registers.get(lhs));
                let rhs_value = f64::from_bits(self.registers.get(rhs));
                self.registers
                    .set(dst, ((lhs_value - rhs_value).abs() < f64::EPSILON) as u64)
            }

            Inst::SLt(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs) as i64;
                let rhs_value = self.registers.get(rhs) as i64;
                self.registers.set(dst, (lhs_value < rhs_value) as u64)
            }
            Inst::ULt(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                self.registers.set(dst, (lhs_value < rhs_value) as u64)
            }
            Inst::FLt(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.registers.get(lhs));
                let rhs_value = f64::from_bits(self.registers.get(rhs));
                self.registers.set(dst, (lhs_value < rhs_value) as u64)
            }

            Inst::SGt(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs) as i64;
                let rhs_value = self.registers.get(rhs) as i64;
                self.registers.set(dst, (lhs_value > rhs_value) as u64)
            }
            Inst::UGt(dst, lhs, rhs) => {
                let lhs_value = self.registers.get(lhs);
                let rhs_value = self.registers.get(rhs);
                self.registers.set(dst, (lhs_value > rhs_value) as u64)
            }
            Inst::FGt(dst, lhs, rhs) => {
                let lhs_value = f64::from_bits(self.registers.get(lhs));
                let rhs_value = f64::from_bits(self.registers.get(rhs));
                self.registers.set(dst, (lhs_value > rhs_value) as u64)
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
