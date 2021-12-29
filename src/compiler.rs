use crate::console::ReturnOnError;
use crate::project::project::*;
use std::fs::File;
use std::io::Write;
use std::str;

/// Writes code to print an int64 to stdout. This code is generated from some c code.
fn write_print_int_64(wl: &mut dyn FnMut(&str)) {
    wl("_PrintInt64:");
    wl("    sub rsp, 56");
    wl("    mov rcx, rdi");
    wl("    mov r10, rdi");
    wl("    mov r8d, 1");
    wl("    mov BYTE [rsp+32], 10");
    wl("    neg rcx");
    wl("    lea r9, [rsp+32]");
    wl("    cmovs rcx, rdi");
    wl("    mov rdi, -3689348814741910323");
    wl(".L2:");
    wl("    mov rax, rcx");
    wl("    mov rsi, r9");
    wl("    mul rdi");
    wl("    sub rsi, r8");
    wl("    shr rdx, 3");
    wl("    lea rax, [rdx+rdx*4]");
    wl("    add rax, rax");
    wl("    sub rcx, rax");
    wl("    mov rax, r8");
    wl("    add r8, 1");
    wl("    add ecx, 48");
    wl("    mov BYTE [rsi], cl");
    wl("    mov rcx, rdx");
    wl("    test rdx, rdx");
    wl("    jne .L2");
    wl("    test r10, r10");
    wl("    jns .L3");
    wl("    mov edx, 32");
    wl("    sub rdx, r8");
    wl("    lea r8, [rax+2]");
    wl("    mov BYTE [rsp+rdx], 45");
    wl(".L3:");
    wl("    mov eax, 33");
    wl("    mov rdx, r8");
    wl("    mov edi, 1");
    wl("    sub rax, r8");
    wl("    lea rsi, [rsp+rax]");
    wl("    mov rax, 1");
    wl("    syscall");
    wl("    add rsp, 56");
    wl("    ret");
}

fn asm_encode_string(str: &str) -> String {
    let (_, in_str, r) = str.chars().fold(
        (true, false, String::new()),
        |(first, prev_in_str, acc), c| {
            // 32 to 126 is printable.
            let now_in_str = c as u32 >= 32 && c as u32 <= 126;
            let sep = !first && ((!now_in_str) || (now_in_str && !prev_in_str));
            let prefix = if sep { "," } else { "" };
            let start_quote = if !prev_in_str && now_in_str { "\"" } else { "" };
            let end_quote = if prev_in_str && !now_in_str { "\"" } else { "" };
            let c_ = if now_in_str {
                c.to_string()
            } else {
                format!(" {}", c as u32)
            };

            return (
                false,
                now_in_str,
                format!("{}{}{}{}{}", acc, end_quote, prefix, start_quote, c_),
            );
        },
    );
    return if in_str { format!("{}\"", r) } else { r };
}

// TODO: Make wl that accepts &str and String, using Into<String>?
// TODO: Customize Rust code formatter?
// TODO: Make wl accept indentation parameter?

fn write_statement(wl: &mut dyn FnMut(&str), stmt: &Statement) {
    match stmt {
        Statement::PrintStr(_, StringLiteral::StringLiteral(_, idx, str)) => {
            wl(format!("    ; PrintStr {}", asm_encode_string(str)).as_str());
            wl(format!("    mov rsi, txt_{}", idx).as_str());
            wl(format!("    mov rdx, {}", str.len()).as_str());
            wl("    call _PrintStr");
        }
        Statement::PrintVar(_, Variable::Variable(_, offset, name)) => {
            wl(format!("    ; PrintVar {}", name).as_str());
            wl("    mov rax, mem");
            wl(format!("    add rax, {}", offset * 8).as_str());
            wl("    mov rdi, [rax]");
            wl("    call _PrintInt64");
        }
        Statement::Call(_, SubroutineName::SubroutineName(_, name)) => {
            wl(format!("    call__{}", name).as_str());
        }
        Statement::Assignment(_, Variable::Variable(_, offset, name), int_val) => {
            wl(format!("    ; Assignment {}, {}", name, int_val).as_str());
            wl("    mov rax, mem");
            wl(format!("    mov rbx, {}", int_val).as_str());
            wl(format!("    add rax, {}", offset * 8).as_str());
            wl("    mov [rax], rbx");
        }
    }
    wl("");
}

/// Writes a subroutine, given a writing function.
/// These are handled in a separate function, since they need to be after all
/// regular  statements.
fn write_subroutine(wl: &mut dyn FnMut(&str), sub: &TopLevelStatement) {
    match sub {
        TopLevelStatement::Subroutine(_, SubroutineName::SubroutineName(_, name), stmts) => {
            wl(format!("__{}", name).as_str());
            for stmt in stmts.iter() {
                write_statement(wl, stmt);
            }
            wl("    ret");
            wl("");
        }
        _ => {}
    }
}

/// Writes the p roject to x86_64 linux assembly for Nasm.
pub fn write_x86_64_linux_nasm(file_name: &str, program: Program) {
    let mut file = File::create(file_name)
        .map_err(|_| "Failed to create the file.".to_string())
        .handle_with_exit(None);

    let mut wl = |line: &str| {
        writeln!(&mut file, "{}", line)
            .map_err(|_| "Failed to write a line to the file.".to_string())
            .handle_with_exit(None);
    };

    // Start of program.
    wl("Section .text");
    wl("    global _start");
    wl("");
    wl("_start:");
    wl("    ; Entry point.");
    wl("");

    //Program statements.
    for stmt in program.statements().iter() {
        write_statement(&mut wl, stmt);
        wl("");
    }

    wl("    ; Exit call.");
    wl("    mov rax, 60");
    wl("    mov rdi, 0");
    wl("    syscall");
    wl("");

    wl("_PrintStr:");
    wl("    ; PrintStr helper. Assumes rsi and rdx have been set before calling.");
    wl("    mov rax, 1");
    wl("    mov rdi, 1");

    wl("    syscall");
    wl("    ret");

    write_print_int_64(&mut wl);
    wl("");

    wl("    ; Subroutines.");
    wl("");

    for sr in program.subroutines().iter() {
        write_subroutine(&mut wl, sr);
    }

    // Start of data section.
    wl("section .data");

    for (str, idx) in program.strings.iter() {
        wl(format!("txt_{} db {}", idx, asm_encode_string(str)).as_str());
    }
    wl("");

    // Start of memory section.
    wl("segment .bss");
    if !program.variables.is_empty() {
        wl(format!("mem: resb {}", program.variables.len() * 8).as_str());
    }
}
