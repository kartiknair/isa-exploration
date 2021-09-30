#[derive(Debug, Clone, Copy)]
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
    R16,
}

#[derive(Debug, Clone)]
pub struct Label(pub String);

#[derive(Debug, Clone, Copy)]
pub enum Imm {
    Int(i64),
    Float(f64),
}

impl Imm {
    pub fn as_u64(&self) -> u64 {
        // SAFETY: We transmute here because we are basically trying to emulate the `bitcast` LLVM instruction.
        match self {
            Self::Int(int_value) => *int_value as u64,
            Self::Float(float_value) => float_value.to_bits(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub label: String,
    pub insts: Vec<Inst>,
}

#[derive(Debug, Clone)]
pub enum Inst {
    // Useful for debugging purposes prints the registers contents
    Dbg(Register),

    // Memory & registers
    Rega(Register, Imm),
    Copy(Register, Register),
    Load(Register, Register),
    Store(Register, Register),

    // Control flow
    Jump(Label),
    CJump(Register, Label),
    Branch(Register, Label, Label),

    // // Binary operations
    // Shl(Register, Register, Register),
    // LShr(Register, Register, Register),
    // AShr(Register, Register, Register),
    // And(Register, Register, Register),
    // Or(Register, Register, Register),
    // Xor(Register, Register, Register),
    // Not(Register, Register),

    // Arithmetic operations
    SAdd(Register, Register, Register),
    // UAdd(Register, Register, Register),
    // FAdd(Register, Register, Register),

    // Sub(Register, Register, Register),
    // FSub(Register, Register, Register),

    // SMul(Register, Register, Register),
    // UMul(Register, Register, Register),
    // FMul(Register, Register, Register),

    // SDiv(Register, Register, Register),
    // UDiv(Register, Register, Register),
    // FDiv(Register, Register, Register),

    // SRem(Register, Register, Register),
    // URem(Register, Register, Register),
    // FRem(Register, Register, Register),
}
