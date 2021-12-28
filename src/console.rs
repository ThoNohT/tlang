pub mod compiler_flag {
    use std::collections::HashMap;
    use std::collections::HashSet;
    use std::hash::Hash;

    /// A flag that can be used to define custom behavior for a command.
    pub trait CompilerFlag: Sized + Hash + Eq + Clone {
        /// Returns a map for all flags, indexed by the string that triggers the flag.
        fn all_flags() -> HashMap<String, Self>;

        /// Returns as tring explaining the specified flag.
        fn explain(flag: &Self) -> &str;

        /// Checks whether the trait is active in the provided set of traits.
        fn active(self: &Self, flags: &HashSet<Self>) -> bool {
            flags.contains(self)
        }

        /// Converts a string into a flag, if it is specified in all_flags.
        fn from_string(flag: &String) -> Option<Self> {
            let map = Self::all_flags();
            map.get(flag).cloned()
        }

        /// Converts an iterator of strings into a set containing all valid flags specified in this iterator.
        fn accumulate<'a, I>(args: I) -> HashSet<Self>
        where
            I: Iterator<Item = &'a String>,
        {
            HashSet::from_iter(args.filter_map(Self::from_string))
        }

        /// Prints all flags of the specified type, including their explanation.
        fn print_flags() {
            for (key, val) in Self::all_flags().iter() {
                println!("        {:<14} {}", key, Self::explain(val));
            }
        }
    }
}

pub mod build_flag {
    use crate::console::compiler_flag::CompilerFlag;
    use std::collections::HashMap;

    #[derive(PartialEq, Eq, Clone, Hash)]
    pub enum BuildFlag {
        Run,
        DumpLexerTokens,
        DumpUncheckedSyntaxTree,
        DumpCheckedSyntaxTree,
    }

    impl CompilerFlag for BuildFlag {
        fn all_flags() -> HashMap<String, BuildFlag> {
            HashMap::from([
                ("-r".to_string(), BuildFlag::Run),
                ("-dt".to_string(), BuildFlag::DumpLexerTokens),
                ("-dtu".to_string(), BuildFlag::DumpUncheckedSyntaxTree),
                ("-dtc".to_string(), BuildFlag::DumpCheckedSyntaxTree),
            ])
        }

        fn explain(flag: &BuildFlag) -> &str {
            match flag {
                BuildFlag::Run => "Run the program after compiling it.",
                BuildFlag::DumpLexerTokens => "Dump the tokens produced by the lexer and exit.",
                BuildFlag::DumpUncheckedSyntaxTree => {
                    "Dump the unchecked syntax tree produced by the parser and exit."
                }
                BuildFlag::DumpCheckedSyntaxTree => {
                    "Dump the checked syntax tree produced by the checker and exit."
                }
            }
        }
    }
}

pub mod clean_flag {
    use crate::console::compiler_flag::CompilerFlag;
    use std::collections::HashMap;

    #[derive(PartialEq, Eq, Clone, Hash)]
    pub enum CleanFlag {
        IncludeExe,
    }

    impl CompilerFlag for CleanFlag {
        fn all_flags() -> HashMap<String, CleanFlag> {
            HashMap::from([("-e".to_string(), CleanFlag::IncludeExe)])
        }

        fn explain(flag: &CleanFlag) -> &str {
            match flag {
                CleanFlag::IncludeExe => "Also cleanup the compiled executable.",
            }
        }
    }
}

/// The Result type in this application always contains a String as error.
pub type AppResult<T> = Result<T, String>;

pub trait ReturnOnError<T, E> {
    /// Checks the results, and if it is an error, the error message and additionaly an
    /// aditional message are shown and the application exits with exit code 1.
    fn handle_with_exit(self, additional_msg: Option<&str>) -> T;
}

use crate::console::compiler_flag::CompilerFlag;
use colour::e_red;
use std::fmt;
use std::process::exit;
use std::process::Command;
use std::str;

/// Implementation of ReturnOnError for a Result with a displayable error.
impl<T, E> ReturnOnError<T, E> for Result<T, E>
where
    E: fmt::Display,
{
    fn handle_with_exit(self, additional_msg: Option<&str>) -> T {
        match self {
            Err(e) => {
                if let Some(msg) = additional_msg {
                    e_red!("{}\n", msg);
                }
                e_red!("{}\n", e);
                exit(1);
            }
            Ok(res) => res,
        }
    }
}

/// Prints the usage string.
pub fn print_usage(compiler_name: &str) {
    println!("Usage: {} <COMMAND> [OPTIONS]", compiler_name);
    println!("  COMMAND:");
    println!("    build <name>     Build the program with the specified name.");
    println!("      OPTIONS:");
    build_flag::BuildFlag::print_flags();
    println!("    clean <name>     Clean the output for the program with the specified name.");
    println!("      OPTIONS:");
    clean_flag::CleanFlag::print_flags();
}

/// Checks a condition, and if it fails, prints the usage string, displays the specified error
/// and then exits with exit code 1.
pub fn test_condition_with_usage_error(compiler_name: &str, condition: bool, error: &str) {
    if !condition {
        print_usage(compiler_name);
        e_red!("{}\n", error);
        exit(1);
    }
}

/// Checks a condition, and if it fails, displays the specified error and then exits with exit
/// code 1.
pub fn test_condition(condition: bool, error: &str) {
    if !condition {
        e_red!("{}\n", error);
        exit(1);
    }
}

/// Runs a command, and echoes the command to sdtout.
/// If the command fails, the output from stderr is returned and the program exits.
pub fn run_cmd_echoed(args: Vec<&str>) {
    println!("{}", args.join(" "));
    let output = Command::new(args[0])
        .args(args[1..].iter())
        .output()
        .map_err(|e| format!("{}", e))
        .handle_with_exit(None);
    let code = output.status.code();

    if None == code {
        e_red!("Command was terminated by a signal.\n");
        e_red!(
            "{}\n",
            str::from_utf8(&output.stderr).handle_with_exit(None)
        );
        exit(1);
    } else if let Some(c) = code {
        if c != 0 {
            e_red!("Command exited with status {}.\n", c);
            e_red!(
                "{}\n",
                str::from_utf8(&output.stderr).handle_with_exit(None)
            );
            exit(1);
        }
    }
}
