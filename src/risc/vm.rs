use super::inst::*;
use std::{collections::HashMap, convert::TryFrom, io::Write};

#[derive(Debug, Clone, Copy)]
struct Registers {
    r0: u64,
    r1: u64,
    r2: u64,
    r3: u64,
    r4: u64,
    r5: u64,
    r6: u64,
    r7: u64,
    r8: u64,
    r9: u64,
    r10: u64,
    r11: u64,
    r12: u64,
    r13: u64,
    r14: u64,
    r15: u64,
    rsp: u64,
    rfp: u64,
    rip: u64,
}

impl Registers {
    fn new() -> Self {
        Self {
            r0: 0,
            r1: 0,
            r2: 0,
            r3: 0,
            r4: 0,
            r5: 0,
            r6: 0,
            r7: 0,
            r8: 0,
            r9: 0,
            r10: 0,
            r11: 0,
            r12: 0,
            r13: 0,
            r14: 0,
            r15: 0,
            rsp: 0,
            rfp: 0,
            rip: 0,
        }
    }

    fn get(&self, reg_id: &Register) -> u64 {
        match reg_id {
            Register::R0 => self.r0,
            Register::R1 => self.r1,
            Register::R2 => self.r2,
            Register::R3 => self.r3,
            Register::R4 => self.r4,
            Register::R5 => self.r5,
            Register::R6 => self.r6,
            Register::R7 => self.r7,
            Register::R8 => self.r8,
            Register::R9 => self.r9,
            Register::R10 => self.r10,
            Register::R11 => self.r11,
            Register::R12 => self.r12,
            Register::R13 => self.r13,
            Register::R14 => self.r14,
            Register::R15 => self.r15,
            Register::Rsp => self.rsp,
            Register::Rfp => self.rfp,
            Register::Rip => self.rip,
        }
    }

    fn set(&mut self, reg_id: &Register, new_value: u64) {
        match reg_id {
            Register::R0 => self.r0 = new_value,
            Register::R1 => self.r1 = new_value,
            Register::R2 => self.r2 = new_value,
            Register::R3 => self.r3 = new_value,
            Register::R4 => self.r4 = new_value,
            Register::R5 => self.r5 = new_value,
            Register::R6 => self.r6 = new_value,
            Register::R7 => self.r7 = new_value,
            Register::R8 => self.r8 = new_value,
            Register::R9 => self.r9 = new_value,
            Register::R10 => self.r10 = new_value,
            Register::R11 => self.r11 = new_value,
            Register::R12 => self.r12 = new_value,
            Register::R13 => self.r13 = new_value,
            Register::R14 => self.r14 = new_value,
            Register::R15 => self.r15 = new_value,
            Register::Rsp => self.rsp = new_value,
            Register::Rfp => self.rfp = new_value,
            Register::Rip => self.rip = new_value,
        }
    }
}

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

    pub fn interpret_inst(&mut self, inst: &Inst) {
        match inst {
            Inst::Dbg(reg) => {
                let raw_value = self.registers.get(reg);
                writeln!(
                    self.writer,
                    "r{} = int {}, uint {}, float {}",
                    reg.get_id(),
                    raw_value as i64,
                    raw_value,
                    f64::from_bits(raw_value),
                )
                .unwrap();
            }
            Inst::PrintInt(reg) => {
                writeln!(self.writer, "{}", self.registers.get(reg)).unwrap();
            }
            Inst::PrintUInt(reg) => {
                writeln!(self.writer, "{}", self.registers.get(reg) as i64).unwrap();
            }
            Inst::PrintFloat(reg) => {
                writeln!(self.writer, "{}", f64::from_bits(self.registers.get(reg))).unwrap();
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
