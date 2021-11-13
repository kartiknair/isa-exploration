# Syscall Table

| ID  | Name         | Purpose                                                     |
| --- | ------------ | ----------------------------------------------------------- |
| 0   | `print_u64`  | Prints the value in `r1` as a `u64`                         |
| 1   | `print_i64`  | Prints the value in `r1` as an `i64`                        |
| 2   | `print_f32`  | Prints the value in `r1` as an `f32`                        |
| 3   | `print_f64`  | Prints the value in `r1` as an `f64`                        |
| 4   | `print_bool` | Prints the value in `r1` as a `bool`                        |
| 5   | `sleep`      | Blocks the virtual machine for the duration in `r1` (in ms) |
