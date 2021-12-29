use std::collections::HashMap;

#[derive(Clone, Debug)]
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

#[derive(Clone)]
pub enum CheckResult<T>
where
    T: Clone,
{
    Checked(T, Vec<CheckIssue>),
    Failed(Vec<CheckIssue>),
}

impl<T: Clone> CheckResult<T> {
    /// Checks whether a check result is failed.
    pub fn is_failed(self: &Self) -> bool {
        match self {
            Self::Failed(_) => true,
            _ => false,
        }
    }

    /// Returns all issues in a check result.
    pub fn issues(self: &Self) -> Vec<CheckIssue> {
        match self {
            Self::Failed(issues) => issues.clone(),
            Self::Checked(_, issues) => issues.clone(),
        }
    }

    /// Returns the resulting value from a check result, if it is Checked, and None otherwise.
    pub fn value(self: &Self) -> Option<T> {
        match self {
            Self::Failed(_) => None,
            Self::Checked(v, _) => Some(v.clone()),
        }
    }

    /// creates a CheckResult that is Checked, and has no issues.
    pub fn perfect(value: T) -> Self {
        Self::Checked(value, Vec::new())
    }

    /// Applies a mapping function to the result of a CheckResult, if it is Checked.
    pub fn map<R: Clone>(self: &Self, f: impl Fn(T) -> R) -> CheckResult<R> {
        match self {
            Self::Failed(issues) => CheckResult::Failed(issues.clone()),
            Self::Checked(v, issues) => CheckResult::Checked(f(v.clone()), issues.clone()),
        }
    }
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
/// If `assign` is false, then a reference to a non-existent string will fail the compilation.
// TODO: Probably prevent assigning to the same variable multiple times.
// TODO: Variables local to subroutines.
fn get_variable_offset<'a>(
    variables: &'a mut VariableOffsets,
    assign: bool,
    name: &String,
) -> CheckResult<usize> {
    if let Some(idx) = variables.get(name) {
        CheckResult::Checked(idx.clone(), Vec::new())
    } else if assign {
        let idx = variables.len();
        variables.insert(name.clone(), idx);
        CheckResult::Checked(idx, Vec::new())
    } else {
        CheckResult::Failed(Vec::from([CheckIssue::CheckError(format!(
            "Variable {} not defined.",
            name
        ))]))
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

    fn check_program(program: &UncheckedProgram) -> CheckResult<Program> {
        fn check_stmt(
            strings: &mut StringIndexes,
            variables: &mut VariableOffsets,
            stmt: &UncheckedStatement,
        ) -> CheckResult<Statement> {
            match stmt {
                UncheckedStatement::UPrintStr(UncheckedStringLiteral::UStringLiteral(str)) => {
                    let idx = get_string_idx(strings, str);
                    CheckResult::perfect(Statement::PrintStr(StringLiteral::StringLiteral(
                        idx,
                        str.clone(),
                    )))
                }
                UncheckedStatement::UPrintVar(UncheckedVariable::UVariable(name)) => {
                    get_variable_offset(variables, false, name)
                        .map(|offset| Statement::PrintVar(Variable::Variable(offset, name.clone())))
                }
                UncheckedStatement::UCall(name) => {
                    CheckResult::perfect(Statement::Call(name.clone()))
                }

                UncheckedStatement::UAssignment(UncheckedVariable::UVariable(name), value) => {
                    get_variable_offset(variables, true, name).map(|offset| {
                        Statement::Assignment(
                            Variable::Variable(offset, name.clone()),
                            value.clone(),
                        )
                    })
                }
            }
        }

        fn check_top_lvl_stmt(
            strings: &mut StringIndexes,
            variables: &mut VariableOffsets,
            top_stmt: &UncheckedTopLevelStatement,
        ) -> CheckResult<TopLevelStatement> {
            match top_stmt {
                UncheckedTopLevelStatement::USubroutine(name, stmts) => {
                    let mut checked_stmts = Vec::new();
                    for stmt in stmts.iter() {
                        checked_stmts.push(check_stmt(strings, variables, &stmt));
                    }

                    let issues = checked_stmts.iter().flat_map(CheckResult::issues).collect();
                    let failed = checked_stmts.iter().map(CheckResult::is_failed).all(|x| x);

                    if !failed {
                        let new_stmts = checked_stmts
                            .iter()
                            .filter_map(CheckResult::value)
                            .collect();
                        CheckResult::Checked(
                            TopLevelStatement::Subroutine(name.clone(), new_stmts),
                            issues,
                        )
                    } else {
                        CheckResult::Failed(issues)
                    }
                }
                UncheckedTopLevelStatement::UStmt(stmt) => {
                    check_stmt(strings, variables, &stmt).map(|s| TopLevelStatement::Stmt(s))
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

        let issues = stmts.iter().flat_map(CheckResult::issues).collect();
        let failed = stmts.iter().map(CheckResult::is_failed).any(|x| x);
        if !failed {
            let new_stmts = stmts.iter().filter_map(CheckResult::value).collect();
            CheckResult::Checked(
                Program {
                    stmts: new_stmts,
                    strings,
                    variables,
                },
                issues,
            )
        } else {
            CheckResult::Failed(issues)
        }
    }

    /// Checks a project for issues.
    pub fn check(project: UncheckedProject) -> CheckResult<Project> {
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
            .collect();

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

        let checked_program = check_program(&prog);

        match (call_errors.is_empty(), &checked_program) {
            (true, CheckResult::Checked(prog, issues)) => {
                let mut issues = issues.clone();
                sub_warnings.append(&mut issues);
                CheckResult::Checked(
                    Project {
                        program: prog.clone(),
                        project_type: project.project_type,
                    },
                    sub_warnings,
                )
            }
            _ => {
                let mut issues = checked_program.issues();
                sub_warnings.append(&mut call_errors);
                sub_warnings.append(&mut issues);
                CheckResult::Failed(sub_warnings)
            }
        }
    }
}
