## Register Convention

-   `%0` - Return
-   `%1`-`%8`- Arguments
-   `%8`-`%15` - Temporaries
-   `%16` - Stack Pointer

### Example of compiling a function call using these conventions

The program in a higher-level C-like language would look like:

```
fun add(x int, y int) {
    return x + y
}

fun main() {
    var n1 = 45
    var n2 = 56
    var n3 = add(n1, n2)
    dbg(n3)
}
```

After compilation it would generate the following assembly:

```
add:
    add %0 %1 %2

main:
    rega %1 45
    rega %2 56
    jump add
    copy %3 %0
    dbg %3
```

## Memory

Every register is 64 bits wide, all smaller data is zero padded. Main memory is accessed through `load`s and `store`s, both those instructions take a memory offset (address) and allow for interop between main memory and registers. For example:

```
main:
    rega %8 56.234

    ; store the value in r8 into main memory at r16
    store %16 %8

    ; load the value at the stack pointer into r9
    load %9 %16

    rega %8 4

    ; increment the stack pointer in bytes (maybe bits?)
    add %16 %8
```

## Program structure

Programs written in these ISAs are split into blocks, for debugging purposes the block labelled `main` will be executed. Every block has a list of instructions. The full list of instructions can be seen below, the syntax for this list is:

-   `%X` for register where `X` will be the register ID and in this list is given a helpful name
-   `$imm` for an immediate value meaning either an integer or floating point number
-   `@label`for a block label

The basic instructions for reading and making use of registers & main memory:

-   `rega %r $value` - Stores an immediate value into a register
-   `copy %dst %src` - Copies the value within `%src` into `%dst`
-   `jump @target` - Jumps to a block and then executes any following instructions
-   `cjump %cond @label` - Jumps if the value in `%cond` is `true`
-   `branch %cond @iftrue @iffalse`- Jumps to `@iftrue` or `@iffalse` based on the value of `%cond`
-   `load %dst %adr` - Loads the value within main memory at `%adr` into `%dst`
-   `store %adr %val` - Stores the value in `%src` into main memory at `%adr`

Binary operations:

-   `shl %dst %val %amount` - Shifts the bits left in `%val` by `%amount`
-   `lshr %dst %val %amount` - Shifts the bits right in `%val` by `%amount` with zero padding
-   `ashr %dst %val %amount` - Shifts the bits right in `%val` by `%amount` with sign extension
-   `and %dst %lhs %rhs` - Logically `and`s each bit in `%lhs` with `%rhs` and stores the result in `%dst`
-   `or %dst %lhs %rhs` - Logically `or`s each bit in `%lhs` with `%rhs` and stores the result in `%dst`
-   `xor %dst %lhs %rhs` - Logically `xor`s each bit in `%lhs` with `%rhs` and stores the result in `%dst`
-   `not %dst %src` - Logically inverts each bit in `%src` and stores the result in `%dst`

Arithmetic operations:

-   `sadd %dst %lhs %rhs` - Calculates the sum of signed integer values within `%lhs` and `%rhs` and stores it in `%dst`
-   `uadd %dst %lhs %rhs` - Calculates the sum of unsigned integer values within `%lhs` and `%rhs` and stores it in `%dst`
-   `fadd %dst %lhs %rhs` - Calculates the sum of floating point values within `%lhs` and `%rhs` and stores it in `%dst`

-   `sub %dst %lhs %rhs` - Calculates the (sign agnostic) difference integer values (`%lhs` - `%rhs`) and stores it in `%dst`
-   `fsub %dst %lhs %rhs` - Calculates the difference of floating point values (`%lhs` - `%rhs`) and stores it in `%dst`

-   `smul %dst %lhs %rhs` - Calculates the product of signed integer values within `%lhs` and `%rhs` and stores it in `%dst`
-   `umul %dst %lhs %rhs` - Calculates the product of unsigned integer values within `%lhs` and `%rhs` and stores it in `%dst`
-   `fmul %dst %lhs %rhs` - Calculates the product of floating point values within `%lhs` and `%rhs` and stores it in `%dst`

-   `sdiv %dst %lhs %rhs` - Calculates the quotient of signed integer values within `%lhs` and `%rhs` and stores it in `%dst`
-   `udiv %dst %lhs %rhs` - Calculates the quotient of unsigned integer values within `%lhs` and `%rhs` and stores it in `%dst`
-   `fdiv %dst %lhs %rhs` - Calculates the quotient of floating point values within `%lhs` and `%rhs` and stores it in `%dst`

-   `srem %dst %lhs %rhs` - Calculates the remainder of signed integer values within `%lhs` and `%rhs` and stores it in `%dst`
-   `urem %dst %lhs %rhs` - Calculates the remainder of unsigned integer values within `%lhs` and `%rhs` and stores it in `%dst`
-   `frem %dst %lhs %rhs` - Calculates the remainder of floating point values within `%lhs` and `%rhs` and stores it in `%dst`
