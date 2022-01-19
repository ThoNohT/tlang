use crate::console::ReturnOnError;
use crate::console::{build_flag::BuildFlag, compiler_flag::CompilerFlag};
use crate::project::project::*;
use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::Write;
use std::str;

/// Writes code to print an int64 to stdout. This code is generated from some c code.
fn write_print_int_64(wl: &mut dyn FnMut(u8, bool, &str)) {
    wl(0, false, "_PrintInt64:");
    wl(1, false, "sub rsp, 56");
    wl(1, false, "mov rcx, rdi");
    wl(1, false, "mov r10, rdi");
    wl(1, false, "mov r8d, 1");
    wl(1, false, "mov BYTE [rsp+32], 10");
    wl(1, false, "neg rcx");
    wl(1, false, "lea r9, [rsp+32]");
    wl(1, false, "cmovs rcx, rdi");
    wl(1, false, "mov rdi, -3689348814741910323");
    wl(0, false, ".L2:");
    wl(1, false, "mov rax, rcx");
    wl(1, false, "mov rsi, r9");
    wl(1, false, "mul rdi");
    wl(1, false, "sub rsi, r8");
    wl(1, false, "shr rdx, 3");
    wl(1, false, "lea rax, [rdx+rdx*4]");
    wl(1, false, "add rax, rax");
    wl(1, false, "sub rcx, rax");
    wl(1, false, "mov rax, r8");
    wl(1, false, "add r8, 1");
    wl(1, false, "add ecx, 48");
    wl(1, false, "mov BYTE [rsi], cl");
    wl(1, false, "mov rcx, rdx");
    wl(1, false, "test rdx, rdx");
    wl(1, false, "jne .L2");
    wl(1, false, "test r10, r10");
    wl(1, false, "jns .L3");
    wl(1, false, "mov edx, 32");
    wl(1, false, "sub rdx, r8");
    wl(1, false, "lea r8, [rax+2]");
    wl(1, false, "mov BYTE [rsp+rdx], 45");
    wl(0, false, ".L3:");
    wl(1, false, "mov eax, 33");
    wl(1, false, "mov rdx, r8");
    wl(1, false, "mov edi, 1");
    wl(1, false, "sub rax, r8");
    wl(1, false, "lea rsi, [rsp+rax]");
    wl(1, false, "mov rax, 1");
    wl(1, false, "syscall");
    wl(1, false, "add rsp, 56");
    wl(1, false, "ret");
}

/// Creates a string that is compatible with assembly.
fn asm_encode_string(str: &str) -> String {
    let (_, in_str, r) = str.chars().fold((true, false, String::new()), |(first, prev_in_str, acc), c| {
        // 32 to 126 is printable.
        let now_in_str = c as u32 >= 32 && c as u32 <= 126;
        let sep = !first && ((!now_in_str) || (now_in_str && !prev_in_str));
        let prefix = if sep { "," } else { "" };
        let start_quote = if !prev_in_str && now_in_str { "\"" } else { "" };
        let end_quote = if prev_in_str && !now_in_str { "\"" } else { "" };
        let c_ = if now_in_str { c.to_string() } else { format!(" {}", c as u32) };

        return (false, now_in_str, format!("{}{}{}{}{}", acc, end_quote, prefix, start_quote, c_));
    });
    return if in_str { format!("{}\"", r) } else { r };
}

/// Writes an operator in an expression, given a writing function.
fn write_op(wl: &mut dyn FnMut(u8, bool, &str), offset: u8, op: &Operator) {
    match op {
        Operator::Add(_) => {
            wl(offset, false, "add rax, rbx");
        }
        Operator::Sub(_) => {
            wl(offset, false, "sub rax, rbx");
        }
    }
}

/// Writes an expression, given a writing function.
/// The result of the expression will be on top of the stack.
fn write_expression(wl: &mut dyn FnMut(u8, bool, &str), offset: u8, expr: &Expression) {
    match expr {
        Expression::IntLiteral(_, int_val) => {
            wl(offset, true, "; Int literal.");
            wl(offset, false, format!("push {}", int_val).as_str());
        }
        Expression::Variable(_, variable) => {
            wl(offset, true, format!("; Variable {}.", variable.name).as_str());
            wl(offset, false, format!("call __var_{}", variable.index).as_str());
            // The result of calling the variable will be in rax, put it on the stack.
            wl(offset, false, "push rax");
        }
        Expression::Binary(_, op, l_expr, r_expr) => {
            wl(offset, true, "; Binary add.");
            write_expression(wl, offset + 1, l_expr);
            write_expression(wl, offset + 1, r_expr);
            wl(offset, true, "; Binary add, calculate result");
            wl(offset, false, "pop rbx");
            wl(offset, false, "pop rax");
            // Calculate result, and push.
            write_op(wl, offset, op);
            wl(offset, false, format!("push rax").as_str());
        }
    }
}

type Assignments = VecDeque<(Variable, Assignment)>;

/// Writes a function that performs an assignment if needed, and returns the value otherwise.
fn write_assignment_func(wl: &mut dyn FnMut(u8, bool, &str), assignments: &mut Assignments) {
    let assmt_opt = assignments.pop_front();
    if let Some((var, assmt)) = assmt_opt {
        let mut scoped_var = var.context.clone();
        scoped_var.push(var.name.clone());

        wl(0, true, format!("; Assignment for variable {}", scoped_var.join("::")).as_str());
        wl(0, false, format!("__var_{}:", var.index).as_str());

        // Check if the variable's init bit is 1.
        let byte_offset = var.index / 8;
        let base: usize = 2;
        let test_bit = base.pow(7 - (var.index % 8));
        wl(1, true, "; Check if the variable was already assigned.");
        wl(1, false, "mov rbx, mem_init");
        wl(1, false, format!("add rbx, {}", byte_offset).as_str());
        wl(1, false, "mov rax, [rbx]");
        wl(1, false, "mov rcx, rax");

        // Set the init bit to 1.
        wl(1, true, "; Set the init bit to 1.");
        wl(1, false, format!("or rax, {}", test_bit).as_str());
        wl(1, false, "mov [rbx], rax");

        // Jump if the init bit was 0.
        wl(1, false, format!("test rcx, {}", test_bit).as_str());
        wl(1, true, "; Jump to calculate value if it is 0.");
        wl(1, false, format!("jz __var_{}_calc", var.index).as_str());

        // If the value is known.
        wl(1, true, "; The value is known, retrieve it.");
        wl(1, false, "mov rbx, mem");
        wl(1, false, format!("add rbx, {}", var.offset).as_str());
        wl(1, false, "mov rax, [rbx]");

        // Return.
        wl(1, false, "ret");
        wl(0, true, "");

        // Calculate the expression value, it will be on top of the stack.
        wl(1, false, format!("__var_{}_calc:", var.index).as_str());
        write_assignment(wl, 2, assignments, Some(&var), &assmt);

        // Store the value.
        wl(1, false, format!("__var_{}_end:", var.index).as_str());
        wl(1, false, "pop rax");
        wl(1, true, "; Store the value.");
        wl(1, false, "mov rbx, mem");
        wl(1, false, format!("add rbx, {}", var.offset).as_str());
        wl(1, false, "mov [rbx], rax");

        // Return.
        wl(1, false, "ret");
        wl(0, true, "");
    }
}

/// Writes an assignment for a variable, given a writing function.
fn write_assignment(
    wl: &mut dyn FnMut(u8, bool, &str),
    offset: u8,
    assignments: &mut Assignments,
    variable: Option<&Variable>,
    assmt: &Assignment,
) {
    match assmt {
        Assignment::ExprAssignment(_, expr) => write_expression(wl, offset, expr),
        Assignment::BlockAssignment(_, stmts) => {
            wl(offset, true, "; Block assignment.");
            for stmt in stmts {
                write_statement(wl, offset + 1, assignments, variable, stmt);
            }
            wl(offset, true, "; End of block assignment.");
        }
    }
}

/// Writes a statement, given a writing function.
fn write_statement(
    wl: &mut dyn FnMut(u8, bool, &str),
    offset: u8,
    assignments: &mut Assignments,
    variable: Option<&Variable>,
    stmt: &Statement,
) {
    match stmt {
        Statement::PrintStr(_, StringLiteral::StringLiteral(_, idx, str)) => {
            wl(offset, true, format!("; PrintStr {}.", asm_encode_string(str)).as_str());
            wl(offset, false, format!("mov rsi, txt_{}", idx).as_str());
            wl(offset, false, format!("mov rdx, {}", str.len()).as_str());
            wl(offset, false, "mov rax, 1");
            wl(offset, false, "mov rdi, 1");
            wl(offset, false, "syscall");

            wl(0, true, "");
        }
        Statement::PrintExpr(_, expr) => {
            wl(offset, true, "; PrintExpr start.");
            write_expression(wl, offset + 1, expr);
            wl(offset, true, "; PrintExpr print call.");
            wl(offset, false, "pop rdi");
            wl(offset, false, "call _PrintInt64");

            wl(0, true, "");
        }
        Statement::Assignment(_, var, assmt) => {
            assignments.push_back((var.clone(), assmt.clone()));
        }
        Statement::Return(_, expr) => {
            write_expression(wl, offset, &expr);

            // After a return statement, jump to the end of the current variable assignment block,
            // or to the end of the application if we are not in a variable assignment.
            match variable {
                Some(var) => {
                    wl(offset, false, format!("jmp __var_{}_end", var.index).as_str());
                }
                None => {
                    wl(offset, false, "jmp _exit");
                }
            }

            wl(0, true, "");
            // TODO: Unreachable code analysis in checker?
        }
    }
}

/// Writes the project to x86_64 linux assembly for nasm or fasm.
pub fn write_x86_64_linux_asm(file_name: &str, program: Program, flags: &HashSet<BuildFlag>) {
    let pretty_print = BuildFlag::PrettyPrintAsm.active(flags);
    let mut file = File::create(file_name).map_err(|_| "Failed to create the file.".to_string()).handle_with_exit(None);

    let mut wl = |indent: u8, pp: bool, line: &str| {
        if pp && !pretty_print {
            return;
        }
        let indent_ = if !pretty_print && indent > 1 { 1 } else { indent };
        let indent_str = (0..indent_ * 4).map(|_| " ").collect::<String>();
        writeln!(&mut file, "{}{}", indent_str, line)
            .map_err(|_| "Failed to write a line to the file.".to_string())
            .handle_with_exit(None);
    };

    // Start of program.
    if BuildFlag::UseNasm.active(flags) {
        wl(0, false, "Section .text");
        wl(1, false, "global _start");
    } else {
        wl(0, false, "format ELF64 executable");
        wl(0, false, "segment readable executable");
        wl(0, false, "entry _start");
    }

    wl(0, false, "");
    wl(0, false, "_start:");
    wl(1, true, "; Entry point.");
    wl(0, true, "");

    let mut assignments = VecDeque::new();
    // Top level statements.
    for stmt in program.stmts.iter() {
        write_statement(&mut wl, 1, &mut assignments, None, stmt);
    }

    wl(1, true, "; Exit call. Return number on stack (returned by last statement).");
    wl(1, false, "_exit:");
    wl(1, false, "mov rax, 60");
    wl(1, false, "pop rdi");
    wl(1, false, "syscall");
    wl(0, false, "");

    wl(1, true, "; Subroutines.");
    wl(0, true, "");

    while !assignments.is_empty() {
        write_assignment_func(&mut wl, &mut assignments);
    }

    write_print_int_64(&mut wl);
    wl(0, true, "");

    // Start of data section.
    if BuildFlag::UseNasm.active(flags) {
        wl(0, false, "section .data");

        for (str, idx) in program.strings.iter() {
            wl(1, false, format!("txt_{} db {}", idx, asm_encode_string(str)).as_str());
        }
    } else {
        wl(0, false, "segment readable writable");

        for (str, idx) in program.strings.iter() {
            wl(1, false, format!("txt_{}: db {}", idx, asm_encode_string(str)).as_str());
        }
    }

    wl(0, true, "");

    // Start of memory section.
    if !program.variables_size > 0 {
        // The init field needs one bit per variable, so we can divide the count by 8, round up and
        // reserve that many bytes.
        let over = if program.variables_count % 8 > 0 { 1 } else { 0 };
        let count_bytes = (program.variables_count / 8) + over;

        if BuildFlag::UseNasm.active(flags) {
            wl(0, false, "segment .bss");
            wl(1, false, format!("mem_init: resb {}", count_bytes).as_str());
            wl(1, false, format!("mem: resb {}", program.variables_size).as_str());
        } else {
            wl(1, false, format!("mem_init: rb {}", count_bytes).as_str());
            wl(1, false, format!("mem: rb {}", program.variables_size).as_str());
        }
    }
}
