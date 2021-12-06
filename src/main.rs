use std::env;
use std::fs::remove_file;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::process::exit;
use std::process::Command;
use std::str;
use crate::console::ReturnOnError;

mod console;

pub fn asm_encode_string(str: &str) -> String {
    let (_, in_str, r) = str.chars().fold(
        (true, false, "".to_string()),
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

pub fn write_x86_64_linux_nasm(file_name: &str, program: String) -> console::AppResult<()> {
    let mut file = File::create(file_name).map_err(|_| "Failed to create the file.".to_string())?;
    let mut prl = |line: &str| {
        writeln!(&mut file, "{}", line)
            .map_err(|_| "Failed to write a line to the file.".to_string())
    };

    prl("section .data")?;
    prl(format!("    text db {}", asm_encode_string(program.as_str())).as_str())?;
    prl("")?;
    prl("section .text")?;
    prl("    global _start")?;
    prl("_start:")?;
    prl("    mov rax, 1")?;
    prl("    mov rdi, 1")?;
    prl("    mov rsi, text")?;
    prl("    mov rdx, 14")?;
    prl("    syscall")?;
    prl("")?;
    prl("    mov rax, 60")?;
    prl("    mov rdi, 0")?;
    prl("    syscall")?;

    return Ok(());
}


fn parse_file(file_name: &str) -> console::AppResult<String> {
    let mut file = File::open(file_name).map_err(|_| "Failed to open input file.".to_string())?;
    let mut buf: Vec<u8> = Vec::new();
    file.read_to_end(&mut buf)
        .map_err(|_| "Failed reading input file.".to_string())?;
    return String::from_utf8(buf)
        .map_err(|_| "Unable to convert utf8 bytes into string.".to_string());
}

fn compile(project_name: &str) -> console::AppResult<()> {
    let program: String = parse_file(format!("{}.tl", project_name).as_str())?;

    println!("Generating {}.asm.", project_name);
    write_x86_64_linux_nasm(format!("{}.asm", project_name).as_str(), program)?;

    console::run_cmd_echoed(vec![
        "nasm",
        "-f",
        "elf64",
        "-o",
        format!("{}.o", project_name).as_str(),
        format!("{}.asm", project_name).as_str(),
    ])?;

    console::run_cmd_echoed(vec![
        "ld",
        format!("{}.o", project_name).as_str(),
        "-o",
        format!("{}", project_name).as_str(),
    ])?;

    return Ok(());
}

/// Cleans up the intermediary files created while compiling the program.
/// If include_exe is true, then also the executable is cleaned up.
fn cleanup(project_name: &str, include_exe: bool) -> console::AppResult<()> {
    println!("Cleaning up files for {}", project_name);

    let asm_file = format!("{}.asm", project_name);
    if Path::new(&asm_file).exists() {
        println!("Removing {}", asm_file);
        remove_file(asm_file).map_err(|_| "Failed to remove file.".to_string())?;
    }

    let o_file = format!("{}.o", project_name);
    if Path::new(&o_file).exists() {
        println!("Removing {}", o_file);
        remove_file(format!("{}", o_file).as_str())
            .map_err(|_| "Failed to remove file.".to_string())?;
    }

    let exe_file = format!("{}", project_name);
    if include_exe && Path::new(&exe_file).exists() {
        println!("Removing {}", exe_file);
        remove_file(exe_file).map_err(|_| "Failed to remove file.".to_string())?;
    }

    return Ok(());
}


fn main() {
    let args: Vec<String> = env::args().collect();
    console::test_condition_with_usage_error(&args[0], args.len() >= 2, "Not enough arguments provided.");

    let cmd = &args[1];
    match cmd.as_str() {
        "build" => {
            console::test_condition_with_usage_error(&args[0], args.len() >= 3, "Missing build target.");
            let target = &args[2];

            let remaining = args[3..].to_vec();
            compile(target).handle_with_exit(Some("Error while compiling."));

            if remaining.contains(&"-r".to_string()) {
                Command::new(format!("./{}", target).as_str())
                    .spawn()
                    .map_err(|e| format!("{}", e))
                    .handle_with_exit(Some("Failed to run compiled program."));
                exit(0);
            }
        }
        "clean" => {
            console::test_condition_with_usage_error(&args[0], args.len() >= 3, "Missing clean target.");
            let target = &args[2];
            let remaining = args[3..].to_vec();
            let include_exe = remaining.contains(&"-e".to_string());
            cleanup(target, include_exe).handle_with_exit(Some("Error while cleaning up."));
        }
        _ => {
            println!("Unknown command: '{}'.", cmd);
            console::print_usage(&args[0]);
            exit(1);
        }
    }
}
