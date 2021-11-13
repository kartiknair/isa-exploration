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

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Inst {
    SysCall(Register),

    // Memory & registers
    Rega(Register, Imm),
    Copy(Register, Register),
    Load(Register, Register),
    Store(Register, Register),

    // Control flow
    Jump(Label),
    CJump(Register, Label),
    Branch(Register, Label, Label),

    // Bitwise operations
    Shl(Register, Register, Register),
    Shr(Register, Register, Register),
    And(Register, Register, Register),
    Or(Register, Register, Register),
    Xor(Register, Register, Register),
    Not(Register, Register),

    // Arithmetic operations
    SAdd(Register, Register, Register),
    UAdd(Register, Register, Register),
    FAdd(Register, Register, Register),

    Sub(Register, Register, Register),
    FSub(Register, Register, Register),

    SMul(Register, Register, Register),
    UMul(Register, Register, Register),
    FMul(Register, Register, Register),

    SDiv(Register, Register, Register),
    UDiv(Register, Register, Register),
    FDiv(Register, Register, Register),

    SRem(Register, Register, Register),
    URem(Register, Register, Register),
    FRem(Register, Register, Register),

    // Comparative operators
    Eq(Register, Register, Register),
    FEq(Register, Register, Register),

    SLt(Register, Register, Register),
    ULt(Register, Register, Register),
    FLt(Register, Register, Register),

    SGt(Register, Register, Register),
    UGt(Register, Register, Register),
    FGt(Register, Register, Register),
}

impl Inst {
    pub fn as_asm(&self) -> String {
        match self {
            Self::SysCall(reg) => format!("syscall %{}", reg.get_id()),

            Self::Rega(dst, imm) => format!("rega %{} {}", dst.get_id(), imm.as_asm()),
            Self::Copy(dst, src) => format!("copy %{} %{}", dst.get_id(), src.get_id()),
            Self::Load(dst, adr) => format!("load %{} %{}", dst.get_id(), adr.get_id()),
            Self::Store(adr, src) => format!("store %{} %{}", adr.get_id(), src.get_id()),

            Self::Jump(label) => format!("jump @{}", &label.0),
            Self::CJump(cond_reg, true_label) => {
                format!("cjump %{} @{}", cond_reg.get_id(), &true_label.0)
            }
            Self::Branch(cond_reg, true_label, false_label) => format!(
                "branch %{} @{} @{}",
                cond_reg.get_id(),
                &true_label.0,
                &false_label.0
            ),

            Self::Shl(dst, lhs, rhs) => {
                format!("shl %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::Shr(dst, lhs, rhs) => {
                format!("shr %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::And(dst, lhs, rhs) => {
                format!("and %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::Or(dst, lhs, rhs) => {
                format!("or %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::Xor(dst, lhs, rhs) => {
                format!("xor %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::Not(dst, reg) => format!("not %{} %{}", dst.get_id(), reg.get_id()),

            Self::SAdd(dst, lhs, rhs) => {
                format!("sadd %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::UAdd(dst, lhs, rhs) => {
                format!("uadd %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::FAdd(dst, lhs, rhs) => {
                format!("fadd %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }

            Self::Sub(dst, lhs, rhs) => {
                format!("sub %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::FSub(dst, lhs, rhs) => {
                format!("fsub %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }

            Self::SMul(dst, lhs, rhs) => {
                format!("smul %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::UMul(dst, lhs, rhs) => {
                format!("umul %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::FMul(dst, lhs, rhs) => {
                format!("fmul %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }

            Self::SDiv(dst, lhs, rhs) => {
                format!("sdiv %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::UDiv(dst, lhs, rhs) => {
                format!("udiv %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::FDiv(dst, lhs, rhs) => {
                format!("fdiv %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }

            Self::SRem(dst, lhs, rhs) => {
                format!("srem %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::URem(dst, lhs, rhs) => {
                format!("urem %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::FRem(dst, lhs, rhs) => {
                format!("frem %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }

            Self::Eq(dst, lhs, rhs) => {
                format!("eq %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::FEq(dst, lhs, rhs) => {
                format!("feq %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::SLt(dst, lhs, rhs) => {
                format!("slt %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::ULt(dst, lhs, rhs) => {
                format!("ult %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::FLt(dst, lhs, rhs) => {
                format!("flt %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::SGt(dst, lhs, rhs) => {
                format!("sgt %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::UGt(dst, lhs, rhs) => {
                format!("ugt %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
            Self::FGt(dst, lhs, rhs) => {
                format!("fgt %{} %{} %{}", dst.get_id(), lhs.get_id(), rhs.get_id())
            }
        }
    }
}
