use std::fs::File;
use std::io::Write;
use std::process::exit;
use std::process::Command;
use std::str;

pub fn write_x86_64_linux_nasm(file_name: String) {
    let mut file = File::create(file_name).unwrap(); // TODO: Handle the error gracefully.

    let mut prl = |line: &str| writeln!(&mut file, "{}", line).unwrap(); // TODO: Handle the error gracefully.

    prl("section .data");
    prl("    text db \"Hello, World!\", 10");
    prl("");
    prl("section .text");
    prl("    global _start");
    prl("_start:");
    prl("    mov rax, 1");
    prl("    mov rdi, 1");
    prl("    mov rsi, text");
    prl("    mov rdx, 14");
    prl("    syscall");
    prl("");
    prl("    mov rax, 60");
    prl("    mov rdi, 0");
    prl("    syscall");
}

/// Runs a command, and echoes the command to sdtout.
/// If the command fails, the output from stderr is returned and the program exits.
fn run_cmd_echoed(args: Vec<&str>) {
    println!("{}", args.join(" "));
    let output = Command::new(args[0])
        .args(args[1..].iter())
        .output()
        .expect("Command failed to execute");
    let code = output.status.code();

    if None == code {
        println!("Command was terminated by a signal.");
        println!("{}", str::from_utf8(&output.stderr).unwrap());
        exit(1);
    } else if let Some(c) = code {
        if c != 0 {
            println!("Command exited with status {}.", c);
            println!("{}", str::from_utf8(&output.stderr).unwrap());
            exit(1);
        }
    }
}

fn compile(project_name: &str) {
    println!("Generating {}.asm", project_name);
    write_x86_64_linux_nasm(format!("{}.asm", project_name));

    run_cmd_echoed(vec![
        "nasm",
        "-f",
        "elf64",
        "-o",
        format!("{}.o", project_name).as_str(),
        format!("{}.asm", project_name).as_str(),
    ]);

    run_cmd_echoed(vec![
        "ld",
        format!("{}.o", project_name).as_str(),
        "-o",
        format!("{}", project_name).as_str(),
    ]);
}

fn main() {
    compile("test");
}
