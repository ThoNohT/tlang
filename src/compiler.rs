use crate::console::ReturnOnError;
use crate::project::project::*;
use std::fs::File;
use std::io::Write;
use std::str;

/// Writes code to print an int64 to stdout. This code is generated from some c code.
fn write_print_int_64(wl: &mut dyn FnMut(u8, &str)) {
    wl(0, "_PrintInt64:");
    wl(1, "sub rsp, 56");
    wl(1, "mov rcx, rdi");
    wl(1, "mov r10, rdi");
    wl(1, "mov r8d, 1");
    wl(1, "mov BYTE [rsp+32], 10");
    wl(1, "neg rcx");
    wl(1, "lea r9, [rsp+32]");
    wl(1, "cmovs rcx, rdi");
    wl(1, "mov rdi, -3689348814741910323");
    wl(0, ".L2:");
    wl(1, "mov rax, rcx");
    wl(1, "mov rsi, r9");
    wl(1, "mul rdi");
    wl(1, "sub rsi, r8");
    wl(1, "shr rdx, 3");
    wl(1, "lea rax, [rdx+rdx*4]");
    wl(1, "add rax, rax");
    wl(1, "sub rcx, rax");
    wl(1, "mov rax, r8");
    wl(1, "add r8, 1");
    wl(1, "add ecx, 48");
    wl(1, "mov BYTE [rsi], cl");
    wl(1, "mov rcx, rdx");
    wl(1, "test rdx, rdx");
    wl(1, "jne .L2");
    wl(1, "test r10, r10");
    wl(1, "jns .L3");
    wl(1, "mov edx, 32");
    wl(1, "sub rdx, r8");
    wl(1, "lea r8, [rax+2]");
    wl(1, "mov BYTE [rsp+rdx], 45");
    wl(0, ".L3:");
    wl(1, "mov eax, 33");
    wl(1, "mov rdx, r8");
    wl(1, "mov edi, 1");
    wl(1, "sub rax, r8");
    wl(1, "lea rsi, [rsp+rax]");
    wl(1, "mov rax, 1");
    wl(1, "syscall");
    wl(1, "add rsp, 56");
    wl(1, "ret");
}

/// Creates a string that is compatible with assembly.
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

// TODO: Customize Rust code formatter?
/// Writes a statement, given a writing function.
fn write_statement(wl: &mut dyn FnMut(u8, &str), stmt: &Statement) {
    match stmt {
        Statement::PrintStr(_, StringLiteral::StringLiteral(_, idx, str)) => {
            wl(1, format!("; PrintStr {}", asm_encode_string(str)).as_str());
            wl(1, format!("mov rsi, txt_{}", idx).as_str());
            wl(1, format!("mov rdx, {}", str.len()).as_str());
            wl(1, "call _PrintStr");
        }
        Statement::PrintVar(_, Variable::Variable(_, offset, name)) => {
            wl(1, format!("; PrintVar {}", name).as_str());
            wl(1, "mov rax, mem");
            wl(1, format!("add rax, {}", offset * 8).as_str());
            wl(1, "mov rdi, [rax]");
            wl(1, "call _PrintInt64");
        }
        Statement::Call(_, SubroutineName::SubroutineName(_, name)) => {
            wl(1, format!("call__{}", name).as_str());
        }
        Statement::Assignment(_, Variable::Variable(_, offset, name), int_val) => {
            wl(1, format!("; Assignment {}, {}", name, int_val).as_str());
            wl(1, "mov rax, mem");
            wl(1, format!("mov rbx, {}", int_val).as_str());
            wl(1, format!("add rax, {}", offset * 8).as_str());
            wl(1, "mov [rax], rbx");
        }
    }
    wl(0, "");
}

/// Writes a subroutine, given a writing function.
/// These are handled in a separate function, since they need to be after all
/// regular  statements.
fn write_subroutine(wl: &mut dyn FnMut(u8, &str), sub: &TopLevelStatement) {
    match sub {
        TopLevelStatement::Subroutine(_, SubroutineName::SubroutineName(_, name), stmts) => {
            wl(0, format!("__{}", name).as_str());
            for stmt in stmts.iter() {
                write_statement(wl, stmt);
            }
            wl(1, "ret");
            wl(0, "");
        }
        _ => {}
    }
}

/// Writes the p roject to x86_64 linux assembly for fasm.
pub fn write_x86_64_linux_fasm(file_name: &str, program: Program) {
    let mut file = File::create(file_name)
        .map_err(|_| "Failed to create the file.".to_string())
        .handle_with_exit(None);

    let mut wl = |indent: u8, line: &str| {
        let indent_str = (0..indent * 4).map(|_| " ").collect::<String>();
        writeln!(&mut file, "{}{}", indent_str, line)
            .map_err(|_| "Failed to write a line to the file.".to_string())
            .handle_with_exit(None);
    };

    // Start of program.
    wl(0, "format ELF64 executable");
    wl(0, "segment readable executable");
    wl(0, "entry _start");
    wl(0, "");
    wl(0, "_start:");
    wl(1, "; Entry point.");
    wl(0, "");

    //Program statements.
    for stmt in program.statements().iter() {
        write_statement(&mut wl, stmt);
    }

    wl(1, "; Exit call.");
    wl(1, "mov rax, 60");
    wl(1, "mov rdi, 0");
    wl(1, "syscall");
    wl(0, "");

    wl(0, "_PrintStr:");
    wl(1, "; PrintStr helper. Assumes rsi and rdx have been set before calling.");
    wl(1, "mov rax, 1");
    wl(1, "mov rdi, 1");
    wl(1, "syscall");
    wl(1, "ret");

    write_print_int_64(&mut wl);
    wl(0, "");

    wl(1, "; Subroutines.");
    wl(0, "");

    for sr in program.subroutines().iter() {
        write_subroutine(&mut wl, sr);
    }

    // Start of data section.
    wl(0, "segment readable writable");

    for (str, idx) in program.strings.iter() {
        wl(1, format!("txt_{}: db {}", idx, asm_encode_string(str)).as_str());
    }
    wl(0, "");

    // Start of memory section.
    if !program.variables.is_empty() {
        wl(1, format!("mem: rb {}", program.variables.len() * 8).as_str());
    }
}
