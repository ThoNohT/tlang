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
use std::path::Path;
use std::process::exit;
use std::process::Command;
use std::str;

mod checker;
mod compiler;
mod console;
mod lexer;
mod parser;
mod prelude;
mod project;

fn read_file(file_name: &str) -> console::AppResult<String> {
    let mut file = File::open(file_name).map_err(|e| format!("Failed to open input file: {}", e))?;
    let mut buf: Vec<u8> = Vec::new();
    file.read_to_end(&mut buf).map_err(|e| format!("Failed reading input file: {}", e))?;
    return String::from_utf8(buf).map_err(|_| "Unable to convert utf8 bytes into string.".to_string());
}

/// Compiles the project in the specified file.
fn compile(file_name: &str, flags: &HashSet<BuildFlag>) -> String {
    let input = read_file(format!("{}", file_name).as_str()).handle_with_exit(Some("Error reading source file."));

    let keywords = HashSet::from(["Executable", "let", "call", "print", "return"]);

    let tokens = crate::lexer::lexer::lex_file(keywords, file_name, &input);

    if BuildFlag::DumpLexerTokens.active(flags) {
        for (i, t) in tokens.iter().enumerate() {
            println!("{}: {}", i, t.to_string());
        }
        exit(0);
    }

    let project = crate::parser::parse_project(tokens);

    if BuildFlag::DumpUncheckedSyntaxTree.active(flags) {
        println!("{:#?}", &project);
        exit(0);
    }

    let ProjectType::Executable(_, project_name) = project.project_type.clone();

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

            if BuildFlag::DumpCheckedSyntaxTree.active(flags) {
                println!("{:#?}", checked_project);
                exit(0);
            }

            // Determine file names.
            let asm_file = format!("{}.asm", project_name);
            let o_file = format!("{}.o", project_name);
            let exe_file = format!("{}", project_name);

            // Write asm.
            println!("Generating {}", asm_file);
            crate::compiler::write_x86_64_linux_asm(asm_file.as_str(), checked_project.program, flags);

            if BuildFlag::UseNasm.active(flags) {
                // Compile nasm.
                console::run_cmd_echoed(Vec::from(["nasm", "-f", "elf64", "-o", o_file.as_str(), asm_file.as_str()]));

                // Link file.
                console::run_cmd_echoed(Vec::from(["ld", o_file.as_str(), "-o", exe_file.as_str()]));
            } else {
                // Compile fasm.
                console::run_cmd_echoed(Vec::from(["fasm", "-m", "524288", asm_file.as_str(), exe_file.as_str()]));
            }

            exe_file
        }
    }
}

/// Cleans up the intermediary files created while compiling the program.
fn cleanup(file_name: &str, flags: &HashSet<CleanFlag>) {
    let input = read_file(format!("{}", file_name).as_str()).handle_with_exit(Some("Error reading source file."));
    let keywords = HashSet::from(["Executable", "let", "call", "print"]);
    let tokens = crate::lexer::lexer::lex_file(keywords, file_name, &input);
    let project = crate::parser::parse_project(tokens);

    let ProjectType::Executable(_, project_name) = project.project_type;

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
        remove_file(o_file)
            .map_err(|_| "Failed to remove file.".to_string())
            .handle_with_exit(Some("Error cleaning up o file."));
    }

    let exe_file = format!("{}", project_name);
    if CleanFlag::IncludeExe.active(flags) && Path::new(&exe_file).exists() {
        println!("Removing {}", exe_file);
        remove_file(exe_file)
            .map_err(|_| "Failed to remove file.".to_string())
            .handle_with_exit(Some("Error cleaning up executable file."));
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    console::test_condition_with_usage_error(&args[0], args.len() >= 2, "Not enough arguments provided.");

    let cmd = &args[1];
    match cmd.as_str() {
        "build" => {
            console::test_condition_with_usage_error(&args[0], args.len() >= 3, "Missing build target.");
            let target = &args[2];

            let flags: HashSet<BuildFlag> = CompilerFlag::accumulate(args[3..].iter());
            let exe_file = compile(target, &flags);

            if BuildFlag::Run.active(&flags) {
                Command::new(format!("./{}", exe_file).as_str())
                    .spawn()
                    .map_err(|e| format!("{}", e))
                    .handle_with_exit(Some("Failed to run compiled program."));
                exit(0);
            }
        }
        "clean" => {
            console::test_condition_with_usage_error(&args[0], args.len() >= 3, "Missing clean target.");
            let target = &args[2];
            let flags: HashSet<CleanFlag> = CompilerFlag::accumulate(args[3..].iter());
            cleanup(target, &flags);
        }
        _ => {
            console::test_condition_with_usage_error(&args[0], false, format!("Unknown command: '{}'.", cmd).as_str());
        }
    }
}
