#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,

    Rsp,
    Rfp,
    Rip,
}

impl Register {
    pub fn from_id(id: u8) -> Self {
        match id {
            0 => Self::R0,
            1 => Self::R1,
            2 => Self::R2,
            3 => Self::R3,
            4 => Self::R4,
            5 => Self::R5,
            6 => Self::R6,
            7 => Self::R7,
            8 => Self::R8,
            9 => Self::R9,
            10 => Self::R10,
            11 => Self::R11,
            12 => Self::R12,
            13 => Self::R13,
            14 => Self::R14,
            15 => Self::R15,
            16 => Self::Rsp,
            17 => Self::Rfp,
            18 => Self::Rip,
            _ => panic!("expected register ID between 0 and 18"),
        }
    }

    pub fn get_id(&self) -> u8 {
        match self {
            Self::R0 => 0,
            Self::R1 => 1,
            Self::R2 => 2,
            Self::R3 => 3,
            Self::R4 => 4,
            Self::R5 => 5,
            Self::R6 => 6,
            Self::R7 => 7,
            Self::R8 => 8,
            Self::R9 => 9,
            Self::R10 => 10,
            Self::R11 => 11,
            Self::R12 => 12,
            Self::R13 => 13,
            Self::R14 => 14,
            Self::R15 => 15,
            Self::Rsp => 16,
            Self::Rfp => 17,
            Self::Rip => 18,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Label(pub String);

impl Label {
    pub fn new(name: &str) -> Self {
        Self(name.to_string())
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Imm {
    Int(u64),
    Float(f64),
    True,
    False,
}

impl Imm {
    pub fn as_u64(&self) -> u64 {
        match self {
            Self::Int(int_value) => *int_value as u64,
            Self::Float(float_value) => float_value.to_bits(),
            Self::True => u64::MAX,
            Self::False => 0,
        }
    }

    pub fn as_asm(&self) -> String {
        match self {
            Self::Int(i) => i.to_string(),
            Self::Float(f) => f.to_string(),
            Self::True => "true".to_string(),
            Self::False => "false".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Registers {
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
    pub fn new() -> Self {
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

    pub fn get(&self, reg_id: &Register) -> u64 {
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

    pub fn set(&mut self, reg_id: &Register, new_value: u64) {
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

impl Default for Registers {
    fn default() -> Self {
        Self::new()
    }
}
