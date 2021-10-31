use std::env;
use std::fs::remove_file;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::exit;
use std::process::Command;
use std::str;

pub fn write_x86_64_linux_nasm(file_name: String) {
    let mut file = File::create(file_name).expect("Failed to create the file.");
    let mut prl =
        |line: &str| writeln!(&mut file, "{}", line).expect("Failed to write a line to the file.");

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
        .expect("Command failed to execute.");
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
    println!("Generating {}.asm.", project_name);
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

/// Cleans up the intermediary files created while compiling the program.
/// If include_exe is true, then also the executable is cleaned up.
fn cleanup(project_name: &str, include_exe: bool) {
    println!("Cleaning up files for {}", project_name);

    let asm_file = format!("{}.asm", project_name);
    if Path::new(asm_file.as_str()).exists() {
        println!("Removing {}", asm_file);
        remove_file(asm_file).expect("Failed to remove file.");
    }

    let o_file = format!("{}.o", project_name);
    if Path::new(o_file.as_str()).exists() {
        println!("Removing {}", o_file);
        remove_file(format!("{}", o_file).as_str()).expect("Failed to remove file.");
    }

    let exe_file = format!("{}", project_name);
    if include_exe && Path::new(exe_file.as_str()).exists() {
        println!("Removing {}", exe_file);
        remove_file(exe_file).expect("Failed to remove file.");
    }
}

/// Prints the usage string.
fn print_usage() {
    println!("Usage:");
}

/// Checks a condition, and if it fails, shows an error with the usage details
/// appended.
fn test_condition_with_usage_error(condition: bool, error: &str) {
    if condition {
        return;
    }

    println!("{}", error);
    print_usage();
    exit(1);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    test_condition_with_usage_error(args.len() >= 2, "Not enough arguments provided.");

    let cmd = &args[1];
    match cmd.as_str() {
        "build" => {
            test_condition_with_usage_error(args.len() >= 3, "Missing build target.");
            let target = &args[2];

            let remaining = args[3..].to_vec();
            compile(target);

            if remaining.contains(&"-r".to_string()) {
                Command::new(format!("./{}", target).as_str())
                    .spawn()
                    .expect("Failed to run compiled program.");
                exit(0);
            }
        }
        "clean" => {
            test_condition_with_usage_error(args.len() >= 3, "Missing clean target.");
            let target = &args[2];
            let remaining = args[3..].to_vec();
            let include_exe = remaining.contains(&"-e".to_string());
            cleanup(target, include_exe);
        }
        _ => {
            println!("Unknown command: '{}'.", cmd);
            print_usage();
            exit(1);
        }
    }
}
