use crate::lexer::Range;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum CheckIssue {
    /// An error that prevetns compilation.
    CheckError(Range, String),
    /// A warning that indicates a possible problem, but doesn't prevent compilation.
    CheckWarning(Range, String),
}

impl CheckIssue {
    /// Converts an issue to a string to be shown to the user, including its severity.
    pub fn to_string(self: &Self) -> String {
        match self {
            Self::CheckError(r, err) => format!("Error: {}: {}", r.to_full_string(), err),
            Self::CheckWarning(r, warn) => format!("Warn: {}: {}", r.to_full_string(), warn),
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
    pub fn map<R: Clone>(self: &Self, f: impl FnOnce(T) -> R) -> CheckResult<R> {
        self.bind(|r| CheckResult::perfect(f(r)))
    }

    pub fn bind<R: Clone>(self: &Self, f: impl FnOnce(T) -> CheckResult<R>) -> CheckResult<R> {
        match self {
            Self::Failed(issues) => CheckResult::Failed(issues.clone()),
            Self::Checked(v, issues) => match f(v.clone()) {
                CheckResult::Failed(issues_) => CheckResult::Failed(
                    issues
                        .iter()
                        .cloned()
                        .chain(issues_.iter().cloned())
                        .collect::<Vec<CheckIssue>>(),
                ),
                CheckResult::Checked(r, issues_) => CheckResult::Checked(
                    r.clone(),
                    issues
                        .iter()
                        .cloned()
                        .chain(issues_.iter().cloned())
                        .collect::<Vec<CheckIssue>>(),
                ),
            },
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
/// If `assign` is false, then a reference to a non-existent variable will fail the compilation.
/// If `assign` is true, then a reference to an existing variable will fail the compilation.
fn get_variable_offset<'a>(
    range: &Range,
    variables: &'a mut VariableOffsets,
    assign: bool,
    name: &String,
) -> CheckResult<usize> {
    if let Some(idx) = variables.get(name) {
        if !assign {
            CheckResult::Checked(idx.clone(), Vec::new())
        } else {
            println!("{} defined", name);
            CheckResult::Failed(Vec::from([CheckIssue::CheckError(
                range.clone(),
                format!("Variable {} already defined.", name),
            )]))
        }
    } else if assign {
        let idx = variables.len();
        variables.insert(name.clone(), idx);
        CheckResult::Checked(idx, Vec::new())
    } else {
        CheckResult::Failed(Vec::from([CheckIssue::CheckError(
            range.clone(),
            format!("Variable {} not defined.", name),
        )]))
    }
}

pub mod check {
    use crate::checker::{get_string_idx, get_variable_offset};
    use crate::checker::{CheckIssue, CheckResult, StringIndexes, VariableOffsets};
    use crate::project::project::*;
    use crate::project::unchecked_project::*;
    use std::collections::{HashSet, VecDeque};

    fn unused_subs(program: &UncheckedProgram) -> HashSet<SubroutineName> {
        let subroutines = program.subroutines();
        let mut unused = subroutines
            .iter()
            .filter_map(|s| s.name().map(|n| n.value().clone()))
            .collect::<HashSet<String>>();

        let mut stmts_to_check = VecDeque::from(program.statements());
        while let Some(stmt) = stmts_to_check.pop_front() {
            if let Some(UncheckedStatement::UCall(_, n)) = stmt.call() {
                unused.remove(n.value());
                let new_stmts = subroutines
                    .clone()
                    .into_iter()
                    .find(|s| n.equals(s.name()))
                    .filter(|s| s.name().map_or(false, |n| unused.contains(n.value())))
                    .map(|tls| tls.subroutine_statements())
                    .unwrap_or(Vec::new());

                stmts_to_check.extend(new_stmts);
            }
        }

        subroutines
            .iter()
            .filter_map(UncheckedTopLevelStatement::name)
            .filter(|s| unused.contains(s.value()))
            .collect::<HashSet<SubroutineName>>()
    }

    fn check_expression(
        strings: &mut StringIndexes,
        variables: &mut VariableOffsets,
        expr: &UncheckedExpression,
    ) -> CheckResult<Expression> {
        match expr {
            UncheckedExpression::UIntLiteral(r, int_val) => {
                CheckResult::perfect(Expression::IntLiteral(r.clone(), int_val.clone()))
            }
            UncheckedExpression::UVariable(r, UncheckedVariable::UVariable(r2, name)) => {
                get_variable_offset(r2, variables, false, name).map(|offset| {
                    Expression::Variable(
                        r.clone(),
                        Variable::Variable(r2.clone(), offset, name.clone()),
                    )
                })
            }
            UncheckedExpression::UBinary(r, op, ex_l, ex_r) => {
                check_expression(strings, variables, ex_l).bind(|le| {
                    check_expression(strings, variables, ex_r).map(|re| {
                        Expression::Binary(r.clone(), op.clone(), Box::new(le), Box::new(re))
                    })
                })
            }
        }
    }

    fn check_program(program: &UncheckedProgram) -> CheckResult<Program> {
        fn check_stmt(
            strings: &mut StringIndexes,
            variables: &mut VariableOffsets,
            stmt: &UncheckedStatement,
        ) -> CheckResult<Statement> {
            match stmt {
                UncheckedStatement::UPrintStr(
                    r1,
                    UncheckedStringLiteral::UStringLiteral(r2, str),
                ) => {
                    let idx = get_string_idx(strings, &str);
                    CheckResult::perfect(Statement::PrintStr(
                        r1.clone(),
                        StringLiteral::StringLiteral(r2.clone(), idx, str.clone()),
                    ))
                }
                UncheckedStatement::UPrintVar(r1, UncheckedVariable::UVariable(r2, name)) => {
                    get_variable_offset(&r2, variables, false, &name).map(|offset| {
                        Statement::PrintVar(
                            r1.clone(),
                            Variable::Variable(r2.clone(), offset, name.clone()),
                        )
                    })
                }
                UncheckedStatement::UCall(r, name) => {
                    CheckResult::perfect(Statement::Call(r.clone(), name.clone()))
                }

                UncheckedStatement::UAssignment(
                    r1,
                    UncheckedVariable::UVariable(r2, name),
                    expr,
                ) => {
                    // Check expressio before variable so the variable is not yet known during
                    // expression evaluation. But do check the variable even if expression fails
                    // so it is known later.
                    let expr = check_expression(strings, variables, &expr);
                    let var_offset = get_variable_offset(&r2, variables, true, &name);

                    expr.bind(|e| {
                        var_offset.map(|o| {
                            Statement::Assignment(
                                r1.clone(),
                                Variable::Variable(r2.clone(), o, name.clone()),
                                e,
                            )
                        })
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
                UncheckedTopLevelStatement::USubroutine(r, name, stmts) => {
                    let mut checked_stmts = Vec::new();
                    let mut local_variables = VariableOffsets::new();
                    for stmt in stmts.iter() {
                        checked_stmts.push(check_stmt(strings, &mut local_variables, stmt));
                    }

                    let issues = checked_stmts.iter().flat_map(CheckResult::issues).collect();
                    let failed = checked_stmts.iter().map(CheckResult::is_failed).any(|x| x);

                    if !failed {
                        let new_stmts = checked_stmts
                            .iter()
                            .filter_map(CheckResult::value)
                            .collect();
                        CheckResult::Checked(
                            TopLevelStatement::Subroutine(r.clone(), name.clone(), new_stmts),
                            issues,
                        )
                    } else {
                        CheckResult::Failed(issues)
                    }
                }
                UncheckedTopLevelStatement::UStmt(r, stmt) => check_stmt(strings, variables, stmt)
                    .map(|s| TopLevelStatement::Stmt(r.clone(), s)),
            }
        }

        let mut strings = StringIndexes::new();
        let mut variables = VariableOffsets::new();

        // Check and convert each statement one by one.
        let mut stmts: Vec<CheckResult<TopLevelStatement>> = Vec::new();
        for stmt in program.stmts.iter() {
            stmts.push(check_top_lvl_stmt(&mut strings, &mut variables, stmt));
        }

        let issues = stmts.iter().flat_map(CheckResult::issues).collect();
        let failed = stmts.iter().map(CheckResult::is_failed).any(|x| x);
        if !failed {
            let new_stmts = stmts.iter().filter_map(CheckResult::value).collect();
            CheckResult::Checked(
                Program {
                    range: program.range.clone(),
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
            .filter_map(|s| s.name().map(|n| n.value().clone()))
            .collect::<HashSet<String>>();
        let sub_names = prog
            .subroutines()
            .iter()
            .filter_map(|s| s.name().map(|n| n.value().clone()))
            .collect();

        let undefined_calls = call_names
            .difference(&sub_names)
            .collect::<HashSet<&String>>();

        let mut call_errors = prog
            .calls()
            .iter()
            .filter_map(UncheckedStatement::name)
            .filter(|n| undefined_calls.contains(n.value()))
            .map(|SubroutineName::SubroutineName(r, s)| {
                CheckIssue::CheckError(r.clone(), format!("Call to undefined subroutine '{}'.", s))
            })
            .collect::<Vec<CheckIssue>>();

        let mut sub_warnings = unused_subs(&prog)
            .iter()
            .map(|SubroutineName::SubroutineName(r, s)| {
                CheckIssue::CheckWarning(r.clone(), format!("Unused subroutine '{}'.", s))
            })
            .collect::<Vec<CheckIssue>>();

        let checked_program = check_program(&prog);

        match (call_errors.is_empty(), &checked_program) {
            (true, CheckResult::Checked(prg, issues)) => {
                let mut issues = issues.clone();
                sub_warnings.append(&mut issues);
                CheckResult::Checked(
                    Project {
                        program: prg.clone(),
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
