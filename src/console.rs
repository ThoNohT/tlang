use colour::e_red;
use std::fmt;
use std::process::exit;
use std::process::Command;
use std::str;

/// The Result type in this application always contains a String as error.
pub type AppResult<T> = Result<T, String>;

pub trait ReturnOnError<T, E> {
    /// Checks the results, and if it is an error, the error message and additionaly an
    /// aditional message are shown and the application exits with exit code 1.
    fn handle_with_exit(self, additional_msg: Option<&str>) -> T;
}

/// Implementation of ReturnOnError for a Result with a displayable error.
impl<T, E> ReturnOnError<T, E> for Result<T, E>
where E: fmt::Display,
{
    fn handle_with_exit(self, additional_msg: Option<&str>) -> T {
        match self {
            Err(e) => {
                if let Some(msg) = additional_msg {
                    e_red!(msg);
                }
                e_red!("{}", e);
                exit(1);
            }
            Ok(res) => {
                return res;
            }
        }
    }
}

/// Prints the usage string.
pub fn print_usage(compiler_name: &str) {
    println!("Usage: {} <COMMAND> [OPTIONS]", compiler_name);
    println!("  COMMAND:");
    println!("    build <name>     Build the program with the specified name.");
    println!("      OPTIONS:");
    println!("        -r:            Run the program after compiling it.");
    println!("    clean <name>     Clean the output for the program with the specified name.");
    println!("      OPTIONS:");
    println!("        -e:            Also cleanup the compiled executable.");
}

/// Checks a condition, and if it fails, prints the usage string, displays the specified error
/// and then exits with exit code 1.
pub fn test_condition_with_usage_error(compiler_name: &str, condition: bool, error: &str) {
    if condition {
        return;
    }

    print_usage(compiler_name);
    e_red!(error);
    exit(1);
}

/// Checks a condition, and if it fails, displays the specified error and then exits with exit
/// code 1.
pub fn test_condition(condition: bool, error: &str) {
    if condition {
        return;
    }

    e_red!(error);
    exit(1);
}

/// Runs a command, and echoes the command to sdtout.
/// If the command fails, the output from stderr is returned and the program exits.
pub fn run_cmd_echoed(args: Vec<&str>) -> AppResult<()> {
    println!("{}", args.join(" "));
    let output = Command::new(args[0])
        .args(args[1..].iter())
        .output()
        .map_err(|e| format!("{}", e))?;
    let code = output.status.code();

    if None == code {
        e_red!("Command was terminated by a signal.");
        e_red!("{}", str::from_utf8(&output.stderr).handle_with_exit(None));
        exit(1);
    } else if let Some(c) = code {
        if c != 0 {
            e_red!("Command exited with status {}.", c);
            e_red!("{}", str::from_utf8(&output.stderr).handle_with_exit(None));
            exit(1);
        }
    }

    return Ok(());
}

