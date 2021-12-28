use crate::project::project::Project;
use std::collections::HashMap;

pub enum CheckIssue {
    /// An error that prevetns compilation.
    CheckError(String),
    /// A warning that indicates a posible problem, but doesn't prevent compilation.
    CheckWarning(String),
}

impl CheckIssue {
    /// Converts an issue to a string to be shown to the user, including its severity.
    pub fn to_string(self: &Self) -> String {
        match self {
            Self::CheckError(err) => format!("Error: {}", err),
            Self::CheckWarning(warn) => format!("Warning: {}", warn),
        }
    }
}

pub enum CheckResult {
    Checked(Project, Vec<CheckIssue>),
    Failed(Vec<CheckIssue>),
}

type StringIndexes = HashMap<String, usize>;
type VariableOffsets = HashMap<String, usize>;

/// Returns the string index for the specified string updates the set of string literals. If
/// the string was defined before, this index is returned to prevent allocating a new string.
fn get_string_idx<'a>(strings: &'a mut StringIndexes, name: &String) -> usize {
    if let Some(idx) = strings.get(name) {
        idx.clone()
    } else {
        let idx = strings.len();
        strings.insert(name.clone(), idx);
        idx
    }
}

/// Returns the offset for the specified variable name and the updated set of variable offsets. If
/// the variable was defined before, this offset is returned.
// TODO: Probably prevent assigning to the same variable multiple times.
// TODO: Variables local to subroutines.
fn get_variable_offset<'a>(variables: &'a mut VariableOffsets, name: &String) -> usize {
    if let Some(idx) = variables.get(name) {
        idx.clone()
    } else {
        let idx = variables.len();
        variables.insert(name.clone(), idx);
        idx
    }
}

pub mod check {
    use crate::checker::{get_string_idx, get_variable_offset};
    use crate::checker::{CheckIssue, CheckResult, StringIndexes, VariableOffsets};
    use crate::project::project::*;
    use crate::project::unchecked_project::*;
    use std::collections::{HashMap, HashSet, VecDeque};

    fn unused_subs(program: &UncheckedProgram) -> HashSet<SubroutineName> {
        let subroutines = program.subroutines();
        let mut unused = subroutines
            .clone()
            .iter()
            .filter_map(UncheckedTopLevelStatement::name)
            .collect::<HashSet<SubroutineName>>();

        let mut stmts_to_check = VecDeque::from(program.statements());
        while let Some(stmt) = stmts_to_check.pop_front() {
            if let Some(UncheckedStatement::UCall(n)) = stmt.call() {
                unused.remove(&n);
                let new_stmts = subroutines
                    .clone()
                    .into_iter()
                    .find(|s| Some(n.clone()) == s.name().clone())
                    .filter(|s| s.name().map_or(false, |n| unused.contains(&n)))
                    .map(|tls| tls.subroutine_statements())
                    .unwrap_or(Vec::new());

                stmts_to_check.extend(new_stmts);
            }
        }

        unused
    }

    fn check_program(program: &UncheckedProgram) -> Program {
        fn check_stmt(
            strings: &mut StringIndexes,
            variables: &mut VariableOffsets,
            stmt: &UncheckedStatement,
        ) -> Statement {
            match stmt {
                UncheckedStatement::UPrintStr(UncheckedStringLiteral::UStringLiteral(str)) => {
                    let idx = get_string_idx(strings, str);
                    Statement::PrintStr(StringLiteral::StringLiteral(idx, str.clone()))
                }
                UncheckedStatement::UPrintVar(UncheckedVariable::UVariable(name)) => {
                    let offset = get_variable_offset(variables, name);
                    Statement::PrintVar(Variable::Variable(offset, name.clone()))
                }
                UncheckedStatement::UCall(name) => Statement::Call(name.clone()),
                UncheckedStatement::UAssignment(UncheckedVariable::UVariable(name), value) => {
                    let offset = get_variable_offset(variables, name);
                    Statement::Assignment(Variable::Variable(offset, name.clone()), value.clone())
                }
            }
        }

        fn check_top_lvl_stmt(
            strings: &mut StringIndexes,
            variables: &mut VariableOffsets,
            top_stmt: &UncheckedTopLevelStatement,
        ) -> TopLevelStatement {
            match top_stmt {
                UncheckedTopLevelStatement::USubroutine(name, stmts) => {
                    let mut checked_stmts = Vec::new();
                    for stmt in stmts.iter() {
                        checked_stmts.push(check_stmt(strings, variables, &stmt));
                    }
                    TopLevelStatement::Subroutine(name.clone(), checked_stmts)
                }
                UncheckedTopLevelStatement::UStmt(stmt) => {
                    TopLevelStatement::Stmt(check_stmt(strings, variables, &stmt))
                }
            }
        }

        let mut strings: StringIndexes = HashMap::new();
        let mut variables: VariableOffsets = HashMap::new();

        // Check and convert each statement one by one.
        let mut stmts = Vec::new();
        for stmt in program.stmts.iter() {
            stmts.push(check_top_lvl_stmt(&mut strings, &mut variables, &stmt));
        }

        Program {
            stmts,
            strings,
            variables,
        }
    }

    /// Checks a project for issues.
    pub fn check(project: UncheckedProject) -> CheckResult {
        let prog = project.program;

        let call_names = prog
            .calls()
            .iter()
            .filter_map(UncheckedStatement::name)
            .collect::<HashSet<SubroutineName>>();
        let sub_names = prog
            .subroutines()
            .iter()
            .filter_map(UncheckedTopLevelStatement::name)
            .collect::<HashSet<SubroutineName>>();

        let undefined_calls = call_names
            .difference(&sub_names)
            .collect::<HashSet<&SubroutineName>>();

        let mut call_errors = undefined_calls
            .iter()
            .map(|SubroutineName::SubroutineName(s)| {
                CheckIssue::CheckError(format!("Call to undefined subroutine '{}'.", s))
            })
            .collect::<Vec<CheckIssue>>();

        let mut sub_warnings = unused_subs(&prog)
            .iter()
            .map(|SubroutineName::SubroutineName(s)| {
                CheckIssue::CheckWarning(format!("Unused subroutine '{}'.", s))
            })
            .collect::<Vec<CheckIssue>>();

        if call_errors.is_empty() {
            CheckResult::Checked(
                Project {
                    program: check_program(&prog),
                    project_type: project.project_type,
                },
                sub_warnings,
            )
        } else {
            sub_warnings.append(&mut call_errors);
            CheckResult::Failed(sub_warnings)
        }
    }
}
