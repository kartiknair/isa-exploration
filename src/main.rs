// #![allow(warnings)]

mod inst;
mod vm;

const MEMORY_SIZE: usize = 128_000;

use inst::{Block, Imm, Inst, Register};

fn main() {
    let mut vm = vm::VM::<MEMORY_SIZE>::new();

    vm.interpret(&[
        /*
        addition:
            sadd %0 %1 %2

        main:
            rega %1 4
            rega %2 6
            jump addition
            dbg %0
        */
        // Block {
        //     label: "addition".to_string(),
        //     insts: vec![Inst::SAdd(Register::R0, Register::R1, Register::R2)],
        // },
        // Block {
        //     label: "main".to_string(),
        //     insts: vec![
        //         Inst::Rega(Register::R1, Imm::Int(4)),
        //         Inst::Rega(Register::R2, Imm::Int(6)),
        //         Inst::Jump(Label("addition".to_string())),
        //         Inst::Dbg(Register::R0),
        //     ],
        // },

        /*
        main:
            rega %8 56.234

            ; store the value in r8 into main memory at r16
            store %16 %8

            ; load the value at the stack pointer into r9
            load %9 %16

            dbg %8
            dbg %9

            rega %8 4
            ; increment the stack pointer in bytes (maybe bits?)
            add %16 %8
        */
        Block {
            label: "main".to_string(),
            insts: vec![
                Inst::Rega(Register::R8, Imm::Float(56.234)),
                Inst::Store(Register::R16, Register::R8),
                Inst::Load(Register::R9, Register::R16),
                Inst::Dbg(Register::R8),
                Inst::Dbg(Register::R9),
            ],
        },
    ]);
}
