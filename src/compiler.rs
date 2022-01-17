use crate::console::ReturnOnError;
use crate::console::{build_flag::BuildFlag, compiler_flag::CompilerFlag};
use crate::project::project::*;
use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::Write;
use std::{iter, str};

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
            wl(offset, false, "mov rax, mem");
            wl(offset, false, format!("add rax, {}", variable.offset * 8).as_str());
            wl(offset, false, "mov rbx, [rax]");
            wl(offset, false, "push rbx");
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
fn write_assignment_func(wl: &mut dyn FnMut(u8, bool, &str), offset: u8, assignments: &mut Assignments) {
    let assmt_opt = assignments.pop_front();
    if let Some((var, assmt)) = assmt_opt {
        let mut scoped_var = var.context;
        scoped_var.push(var.name);

        wl(offset, true, format!("; Assignment for variable {}", scoped_var.join("::")).as_str());
        wl(offset, false, format!("__var_{}:", var.index).as_str());

        // Check if the variable's init bit is 1.
        wl(offset, false, "mov rax, mem_init");
        wl(offset, false, format!("add rax, {}", (var.index / 8) * 8).as_str());
        wl(offset, false, format!("shr rax, {}", 7 - (var.index % 8)).as_str());
        wl(offset, false, "mov rbx, [rax]");
        wl(offset, false, "add rbx, 0x1");
        wl(offset, false, "test rbx, rbx");
        wl(offset, false, format!("jnz, __var_{}_known", var.index).as_str());

        // Calculate the expression value, it will be on top of the stack.
        write_assignment(wl, offset + 1, assignments, &assmt);

        // Duplicate the return value on the stack.
        wl(offset, false, "pop rax");
        wl(offset, false, "push rax");
        wl(offset, false, "push rax");

        // Set the init bit.
        wl(offset, false, "mov rax, mem_init");
        wl(offset, false, format!("add rax, {}", (var.index / 8) * 8).as_str());
        wl(offset, false, "mov rbx, [rax]");
        wl(offset, false, format!("or rbx, {}", 2 ^ (7 - (var.index % 8))).as_str());
        wl(offset, false, "mov [rax], rbx");

        // Store the value.
        wl(offset, false, "mov rax, mem");
        wl(offset, false, format!("add rax, {}", var.offset).as_str());
        wl(offset, false, "pop rbx");
        wl(offset, false, "mov [rax], rbx");

        // Return.
        wl(offset, false, "ret");

        // If the value is known.
        wl(offset, false, "__var_{}_known:");
        wl(offset, false, "mov rax, mem");
        wl(offset, false, format!("add rax, {}", var.offset).as_str());
        wl(offset, false, "mov rbx, [rax]");

        // Store value and return.
        wl(offset, false, "pop rbx");
        wl(offset, false, "ret");
    }
}

/// Writes an assignment for a variable, given a writing function.
fn write_assignment(wl: &mut dyn FnMut(u8, bool, &str), offset: u8, assignments: &mut Assignments, assmt: &Assignment) {
    match assmt {
        Assignment::ExprAssignment(_, expr) => write_expression(wl, offset, expr),
        Assignment::BlockAssignment(_, stmts) => {
            wl(offset, true, "; Block assignment.");
            for stmt in stmts {
                write_statement(wl, offset + 1, assignments, stmt);
            }
            wl(offset, true, "; End of block assignment.");
        }
    }
}

/// Writes a statement, given a writing function.
fn write_statement(wl: &mut dyn FnMut(u8, bool, &str), offset: u8, assignments: &mut Assignments, stmt: &Statement) {
    match stmt {
        Statement::PrintStr(_, StringLiteral::StringLiteral(_, idx, str)) => {
            wl(offset, true, format!("; PrintStr {}.", asm_encode_string(str)).as_str());
            wl(offset, false, format!("mov rsi, txt_{}", idx).as_str());
            wl(offset, false, format!("mov rdx, {}", str.len()).as_str());
            wl(offset, false, "mov rax, 1");
            wl(offset, false, "mov rdi, 1");
            wl(offset, false, "syscall");
        }
        Statement::PrintExpr(_, expr) => {
            wl(offset, true, "; PrintExpr start.");
            write_expression(wl, offset + 1, expr);
            wl(offset, true, "; PrintExpr print call.");
            wl(offset, false, "pop rdi");
            wl(offset, false, "call _PrintInt64");
        }
        Statement::Assignment(_, var, assmt) => {
            assignments.push_back((var.clone(), assmt.clone()));

            // // TODO: Don't assign directly, but make a Variable expression a call to a subroutine
            // // that checks if it was assigned before, and calculates and assigns if not,
            // // and returns otherwise.
            // let Variable::Variable(_, var_offset, name) = var;
            // wl(offset, true, format!("; Assignment {} start.", name).as_str());
            // write_assignment(wl, offset + 1, var, assmt);
            // wl(offset, true, format!("; Assignment {} store.", name).as_str());
            // wl(offset, false, "pop rbx");
            // // Put address offset from mem in rax
            // wl(offset, false, "mov rax, mem");
            //
            // wl(offset, false, format!("add rax, {}", var_offset * 8).as_str());
            // // Store value of rbx in address at rax.
            // wl(offset, false, "mov [rax], rbx");
        }
        Statement::Return(_, expr) => {
            write_expression(wl, offset, &expr);
            // TODO: Return early from the assignment block.
            // TODO: Unreachable code analysis in checker?
        }
    }
    wl(0, true, "");
}

/// Writes the project to x86_64 linux assembly for fasm.
pub fn write_x86_64_linux_fasm(file_name: &str, program: Program, flags: &HashSet<BuildFlag>) {
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
    wl(0, false, "format ELF64 executable");
    wl(0, false, "segment readable executable");
    wl(0, false, "entry _start");
    wl(0, false, "");
    wl(0, false, "_start:");
    wl(1, true, "; Entry point.");
    wl(0, true, "");

    let mut assignments = VecDeque::new();
    // Top level statements.
    for stmt in program.stmts.iter() {
        write_statement(&mut wl, 1, &mut assignments, stmt);
    }

    wl(1, true, "; Exit call.");
    wl(1, false, "mov rax, 60");
    wl(1, false, "mov rdi, 0");
    wl(1, false, "syscall");
    wl(0, false, "");

    write_print_int_64(&mut wl);
    wl(0, true, "");

    wl(1, true, "; Subroutines.");
    wl(0, true, "");

    while !assignments.is_empty() {
        write_assignment_func(&mut wl, 1, &mut assignments);
    }

    // Start of data section.
    wl(0, false, "segment readable writable");

    for (str, idx) in program.strings.iter() {
        wl(1, false, format!("txt_{}: db {}", idx, asm_encode_string(str)).as_str());
    }
    wl(0, true, "");

    // Start of memory section.
    if !program.variables_size > 0 {
        // The init field needs one bit per variable, so we can divide the count by 8, round up and
        // reserve that many bytes.
        let over = if program.variables_count % 8 > 0 { 1 } else { 0 };
        let count_bytes = (program.variables_count / 8) + over;
        wl(1, false, format!("mem_init: rb {}", count_bytes).as_str());
        wl(1, false, format!("mem: rb {}", program.variables_size).as_str());
    }
}
