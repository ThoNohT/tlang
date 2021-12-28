use crate::checker::CheckResult;
use crate::console::build_flag::BuildFlag;
use crate::console::clean_flag::CleanFlag;
use crate::console::compiler_flag::CompilerFlag;
use crate::console::ReturnOnError;
use crate::project::project::ProjectType;
use std::collections::HashSet;
use std::env;
use std::fs::remove_file;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::process::exit;
use std::process::Command;
use std::str;

mod checker;
mod console;
mod lexer;
mod parser;
mod project;

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

pub fn write_x86_64_linux_nasm(file_name: &str, program: String) {
    let mut file = File::create(file_name)
        .map_err(|_| "Failed to create the file.".to_string())
        .handle_with_exit(None);
    let mut prl = |line: &str| {
        writeln!(&mut file, "{}", line)
            .map_err(|_| "Failed to write a line to the file.".to_string())
            .handle_with_exit(None);
    };

    prl("section .data");
    prl(format!("    text db {}", asm_encode_string(program.as_str())).as_str());
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

fn read_file(file_name: &str) -> console::AppResult<String> {
    let mut file =
        File::open(file_name).map_err(|e| format!("Failed to open input file: {}", e))?;
    let mut buf: Vec<u8> = Vec::new();
    file.read_to_end(&mut buf)
        .map_err(|e| format!("Failed reading input file: {}", e))?;
    return String::from_utf8(buf)
        .map_err(|_| "Unable to convert utf8 bytes into string.".to_string());
}

fn compile(file_name: &str, flags: &HashSet<BuildFlag>) -> String {
    let input = read_file(format!("{}", file_name).as_str())
        .handle_with_exit(Some("Error reading source file."));

    let keywords = HashSet::from(["Executable", "let", "call", "print"]);

    let tokens = crate::lexer::lexer::lex_file(keywords, file_name, &input);

    if flags.contains(&BuildFlag::DumpLexerTokens) {
        for (i, t) in tokens.iter().enumerate() {
            println!("{}: {}", i, t.to_string());
        }
        exit(0);
    }

    let project = crate::parser::parse_project(tokens);

    if flags.contains(&BuildFlag::DumpUncheckedSyntaxTree) {
        println!("{:#?}", &project);
        exit(0);
    }

    let ProjectType::Executable(project_name) = project.project_type.clone();

    match crate::checker::check::check(project) {
        CheckResult::Failed(issues) => {
            println!("Issues found:\n");
            for issue in issues.iter() {
                eprintln!("{}", issue.to_string());
            }
            exit(1);
        }
        CheckResult::Checked(checked_project, warnings) => {
            if !warnings.is_empty() {
                println!("Issues found:\n");
                for warning in warnings.iter() {
                    eprintln!("{}", warning.to_string());
                }
            }

            if flags.contains(&BuildFlag::DumpCheckedSyntaxTree) {
                println!("{:#?}", checked_project);
                exit(0);
            }

            // Determine file names.
            let asm_file = format!("{}.asm", project_name);
            let o_file = format!("{}.o", project_name);
            let exe_file = format!("{}", project_name);

            // Write nasm.
            println!("Generating {}", asm_file);
            write_x86_64_linux_nasm(asm_file.as_str(), "TODO: Accept program.".to_string());

            // Compile nasm.
            console::run_cmd_echoed(Vec::from([
                "nasm",
                "-f",
                "elf64",
                "-o",
                o_file.as_str(),
                asm_file.as_str(),
            ]));

            // Link file.
            console::run_cmd_echoed(Vec::from(["ld", o_file.as_str(), "-o", exe_file.as_str()]));

            exe_file
        }
    }
}

/// Cleans up the intermediary files created while compiling the program.
/// If include_exe is true, then also the executable is cleaned up.
fn cleanup(project_name: &str, flags: HashSet<CleanFlag>) {
    println!("Cleaning up files for {}", project_name);

    let asm_file = format!("{}.asm", project_name);
    if Path::new(&asm_file).exists() {
        println!("Removing {}", asm_file);
        remove_file(asm_file)
            .map_err(|_| "Failed to remove file.".to_string())
            .handle_with_exit(Some("Error cleaning up asm file."));
    }

    let o_file = format!("{}.o", project_name);
    if Path::new(&o_file).exists() {
        println!("Removing {}", o_file);
        remove_file(format!("{}", o_file).as_str())
            .map_err(|_| "Failed to remove file.".to_string())
            .handle_with_exit(Some("Error cleaning up out file."));
    }

    let exe_file = format!("{}", project_name);
    if flags.contains(&CleanFlag::IncludeExe) && Path::new(&exe_file).exists() {
        println!("Removing {}", exe_file);
        remove_file(exe_file)
            .map_err(|_| "Failed to remove file.".to_string())
            .handle_with_exit(Some("Error cleaning up executable file."));
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    console::test_condition_with_usage_error(
        &args[0],
        args.len() >= 2,
        "Not enough arguments provided.",
    );

    let cmd = &args[1];
    match cmd.as_str() {
        "build" => {
            console::test_condition_with_usage_error(
                &args[0],
                args.len() >= 3,
                "Missing build target.",
            );
            let target = &args[2];

            let flags: HashSet<BuildFlag> = CompilerFlag::accumulate(args[3..].iter());
            let exe_file = compile(target, &flags);

            if flags.contains(&BuildFlag::Run) {
                Command::new(format!("./{}", exe_file).as_str())
                    .spawn()
                    .map_err(|e| format!("{}", e))
                    .handle_with_exit(Some("Failed to run compiled program."));
                exit(0);
            }
        }
        "clean" => {
            console::test_condition_with_usage_error(
                &args[0],
                args.len() >= 3,
                "Missing clean target.",
            );
            let target = &args[2];
            let flags: HashSet<CleanFlag> = CompilerFlag::accumulate(args[3..].iter());
            cleanup(target, flags);
        }
        _ => {
            console::test_condition_with_usage_error(
                &args[0],
                false,
                format!("Unknown command: '{}'.", cmd).as_str(),
            );
        }
    }
}
