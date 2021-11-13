use crate::shared::{Imm, Label, Register};

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
    SysCall(Operand),

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
            Self::SysCall(operand) => format!("syscall {}", operand.as_asm()),

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
