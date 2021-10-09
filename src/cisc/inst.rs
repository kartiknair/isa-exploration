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

#[derive(Debug, Clone)]
pub struct Block {
    pub label: String,
    pub insts: Vec<Inst>,
}

impl Block {
    pub fn as_asm(&self) -> String {
        let mut result = self.label.clone() + ":\n";
        for inst in &self.insts {
            result += &format!("  {}\n", &inst.as_asm());
        }
        result
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Imm(Imm),
    Data(Register),
    Adr(Register),
}

impl Operand {
    pub fn as_asm(&self) -> String {
        match self {
            Self::Imm(imm) => imm.as_asm(),
            Self::Data(reg) => format!("%{}", reg.get_id()),
            Self::Adr(reg) => format!("[%{}]", reg.get_id()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Target {
    Label(Label),
    Pointer(Register),
}

impl Target {
    pub fn as_asm(&self) -> String {
        match self {
            Self::Label(label) => format!("@{}", label.0.clone()),
            Self::Pointer(reg) => format!("%{}", reg.get_id()),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Inst {
    // Useful for debugging purposes prints the registers contents
    Dbg(Operand),
    PrintInt(Operand),
    PrintUInt(Operand),
    PrintFloat(Operand),

    // Memory & registers
    Move(Operand, Operand),

    // Control flow
    Jump(Target),
    CJump(Operand, Target),
    Branch(Operand, Target, Target),

    // Bitwise operations
    Shl(Operand, Operand, Operand),
    Shr(Operand, Operand, Operand),
    And(Operand, Operand, Operand),
    Or(Operand, Operand, Operand),
    Xor(Operand, Operand, Operand),
    Not(Operand, Operand),

    // Arithmetic operations
    SAdd(Operand, Operand, Operand),
    UAdd(Operand, Operand, Operand),
    FAdd(Operand, Operand, Operand),

    Sub(Operand, Operand, Operand),
    FSub(Operand, Operand, Operand),

    SMul(Operand, Operand, Operand),
    UMul(Operand, Operand, Operand),
    FMul(Operand, Operand, Operand),

    SDiv(Operand, Operand, Operand),
    UDiv(Operand, Operand, Operand),
    FDiv(Operand, Operand, Operand),

    SRem(Operand, Operand, Operand),
    URem(Operand, Operand, Operand),
    FRem(Operand, Operand, Operand),

    // Comparative operators
    Eq(Operand, Operand, Operand),
    FEq(Operand, Operand, Operand),

    SLt(Operand, Operand, Operand),
    ULt(Operand, Operand, Operand),
    FLt(Operand, Operand, Operand),

    SGt(Operand, Operand, Operand),
    UGt(Operand, Operand, Operand),
    FGt(Operand, Operand, Operand),
}

impl Inst {
    pub fn as_asm(&self) -> String {
        match self {
            Self::Dbg(operand) => format!("dbg {}", operand.as_asm()),
            Self::PrintInt(operand) => format!("print_int {}", operand.as_asm()),
            Self::PrintUInt(operand) => format!("print_uint {}", operand.as_asm()),
            Self::PrintFloat(operand) => format!("print_float {}", operand.as_asm()),

            Self::Move(dst, src) => format!("move {} {}", dst.as_asm(), src.as_asm()),

            Self::Jump(target) => format!("jump {}", target.as_asm()),
            Self::CJump(cond, if_target) => {
                format!("cjump {} {}", cond.as_asm(), if_target.as_asm())
            }
            Self::Branch(cond, true_target, false_target) => format!(
                "branch {} {} {}",
                cond.as_asm(),
                true_target.as_asm(),
                false_target.as_asm(),
            ),
            Self::Shl(dst, lhs, rhs) => {
                format!("shl {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::Shr(dst, lhs, rhs) => {
                format!("shr {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::And(dst, lhs, rhs) => {
                format!("and {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::Or(dst, lhs, rhs) => {
                format!("or {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::Xor(dst, lhs, rhs) => {
                format!("xor {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::Not(dst, reg) => format!("not {} {}", dst.as_asm(), reg.as_asm()),

            Self::SAdd(dst, lhs, rhs) => {
                format!("sadd {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::UAdd(dst, lhs, rhs) => {
                format!("uadd {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::FAdd(dst, lhs, rhs) => {
                format!("fadd {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }

            Self::Sub(dst, lhs, rhs) => {
                format!("sub {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::FSub(dst, lhs, rhs) => {
                format!("fsub {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }

            Self::SMul(dst, lhs, rhs) => {
                format!("smul {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::UMul(dst, lhs, rhs) => {
                format!("umul {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::FMul(dst, lhs, rhs) => {
                format!("fmul {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }

            Self::SDiv(dst, lhs, rhs) => {
                format!("sdiv {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::UDiv(dst, lhs, rhs) => {
                format!("udiv {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::FDiv(dst, lhs, rhs) => {
                format!("fdiv {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }

            Self::SRem(dst, lhs, rhs) => {
                format!("srem {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::URem(dst, lhs, rhs) => {
                format!("urem {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::FRem(dst, lhs, rhs) => {
                format!("frem {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }

            Self::Eq(dst, lhs, rhs) => {
                format!("eq {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::FEq(dst, lhs, rhs) => {
                format!("feq {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::SLt(dst, lhs, rhs) => {
                format!("slt {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::ULt(dst, lhs, rhs) => {
                format!("ult {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::FLt(dst, lhs, rhs) => {
                format!("flt {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::SGt(dst, lhs, rhs) => {
                format!("sgt {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::UGt(dst, lhs, rhs) => {
                format!("ugt {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
            Self::FGt(dst, lhs, rhs) => {
                format!("fgt {} {} {}", dst.as_asm(), lhs.as_asm(), rhs.as_asm())
            }
        }
    }
}
