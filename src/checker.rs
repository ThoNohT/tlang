use crate::lexer::Range;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum CheckIssue {
    /// An error that prevetns compilation.
    CheckError(Range, String),
    /// A warning that indicates a possible problem, but doesn't prevent compilation.
    #[allow(dead_code)]
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
                    issues.iter().cloned().chain(issues_.iter().cloned()).collect::<Vec<CheckIssue>>(),
                ),
                CheckResult::Checked(r, issues_) => CheckResult::Checked(
                    r.clone(),
                    issues.iter().cloned().chain(issues_.iter().cloned()).collect::<Vec<CheckIssue>>(),
                ),
            },
        }
    }
}

/// The string indexes known throughout the program.
type StringIndexes = HashMap<String, usize>;

/// The variable offsets an indexes known in the current scope, the current counter for the number of variables
/// assigned and the offset to use for the next variable.
type VariableOffsets = (HashMap<String, (usize, usize)>, usize, usize);

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
) -> CheckResult<(usize, usize)> {
    if let Some(idx_and_offset) = variables.0.get(name) {
        if !assign {
            CheckResult::Checked(idx_and_offset.clone(), Vec::new())
        } else {
            println!("{} defined", name);
            CheckResult::Failed(Vec::from([CheckIssue::CheckError(
                range.clone(),
                format!("Variable {} already defined.", name),
            )]))
        }
    } else if assign {
        let result = (variables.1, variables.2);
        variables.0.insert(name.clone(), result);
        variables.1 += 1;
        variables.2 += 8;
        CheckResult::perfect(result)
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
    use std::collections::{HashMap, VecDeque};

    fn check_expression(
        strings: &mut StringIndexes,
        variables: &mut VariableOffsets,
        ctx: &mut VecDeque<String>,
        expr: &UncheckedExpression,
    ) -> CheckResult<Expression> {
        match expr {
            UncheckedExpression::UIntLiteral(r, int_val) => {
                CheckResult::perfect(Expression::IntLiteral(r.clone(), int_val.clone()))
            }
            UncheckedExpression::UVariable(r, UncheckedVariable::UVariable(r2, name)) => {
                get_variable_offset(r2, variables, false, name).map(|(index, offset)| {
                    Expression::Variable(
                        r.clone(),
                        Variable {
                            range: r2.clone(),
                            index,
                            offset,
                            name: name.clone(),
                            context: ctx.iter().map(|v| v.clone()).collect(),
                        },
                    )
                })
            }
            UncheckedExpression::UBinary(r, op, ex_l, ex_r) => {
                check_expression(strings, variables, ctx, ex_l).bind(|le| {
                    check_expression(strings, variables, ctx, ex_r)
                        .map(|re| Expression::Binary(r.clone(), op.clone(), Box::new(le), Box::new(re)))
                })
            }
        }
    }

    fn check_assignment(
        strings: &mut StringIndexes,
        variables: &mut VariableOffsets,
        ctx: &mut VecDeque<String>,
        assmt: &UncheckedAssignment,
    ) -> CheckResult<Assignment> {
        match assmt {
            UncheckedAssignment::UExprAssignment(r, expr) => {
                check_expression(strings, variables, ctx, expr).map(|e| Assignment::ExprAssignment(r.clone(), e))
            }
            UncheckedAssignment::UBlockAssignment(r, stmts) => {
                let mut checked_stmts = Vec::new();

                // Allocate variables after the global variables.
                let mut local_variables: VariableOffsets = (HashMap::new(), variables.1, variables.2);
                for stmt in stmts.iter() {
                    checked_stmts.push(check_stmt(strings, &mut local_variables, ctx, stmt));
                }

                // Continue allocating after the variables of this block.
                variables.1 = local_variables.1;
                variables.2 = local_variables.2;

                let mut issues: Vec<CheckIssue> = checked_stmts.iter().flat_map(CheckResult::issues).collect();

                // TODO: When types are introduced, all returns need to be the same type.
                // The last expression has to be a return expression.
                let last_stmt = checked_stmts.last().map(|e| e.value()).flatten();
                match last_stmt {
                    None => {
                        issues.push(CheckIssue::CheckError(
                            r.clone(),
                            "A block assignment needs at least one statement.".to_string(),
                        ));
                    }
                    Some(Statement::Return(_, _)) => {}
                    Some(stmt) => {
                        issues.push(CheckIssue::CheckError(
                            stmt.range(),
                            "The last statement of a block assignment needs to be a return.".to_string(),
                        ));
                    }
                }

                let failed = checked_stmts.iter().map(CheckResult::is_failed).any(|x| x);

                if !failed {
                    let new_stmts = checked_stmts.iter().filter_map(CheckResult::value).collect();
                    CheckResult::Checked(Assignment::BlockAssignment(r.clone(), new_stmts), issues)
                } else {
                    CheckResult::Failed(issues)
                }
            }
        }
    }

    fn check_stmt(
        strings: &mut StringIndexes,
        variables: &mut VariableOffsets,
        ctx: &mut VecDeque<String>,
        stmt: &UncheckedStatement,
    ) -> CheckResult<Statement> {
        match stmt {
            UncheckedStatement::UPrintStr(r1, UncheckedStringLiteral::UStringLiteral(r2, str)) => {
                let idx = get_string_idx(strings, &str);
                CheckResult::perfect(Statement::PrintStr(
                    r1.clone(),
                    StringLiteral::StringLiteral(r2.clone(), idx, str.clone()),
                ))
            }
            UncheckedStatement::UPrintExpr(r1, expr) => {
                check_expression(strings, variables, ctx, expr).map(|e| Statement::PrintExpr(r1.clone(), e))
            }
            UncheckedStatement::UAssignment(r1, UncheckedVariable::UVariable(r2, name), expr) => {
                // Check assignment before variable so the variable is not yet known during
                // assignment evaluation. But do check the variable even if assignment fails
                // so it is known later.
                ctx.push_back(name.clone());
                let assmt = check_assignment(strings, variables, ctx, &expr);
                ctx.pop_back();
                let var_offset = get_variable_offset(&r2, variables, true, &name);

                assmt.bind(|a| {
                    var_offset.map(|(i, o)| {
                        Statement::Assignment(
                            r1.clone(),
                            Variable {
                                range: r2.clone(),
                                index: i,
                                offset: o,
                                name: name.clone(),
                                context: ctx.iter().map(|v| v.clone()).collect(),
                            },
                            a,
                        )
                    })
                })
            }
            UncheckedStatement::UReturn(r, expr) => {
                check_expression(strings, variables, ctx, expr).map(|e| Statement::Return(r.clone(), e))
            }
        }
    }

    fn check_program(program: &UncheckedProgram) -> CheckResult<Program> {
        let mut strings = StringIndexes::new();
        let mut variables = (HashMap::new(), 0, 0);

        // Check and convert each statement one by one.
        let mut stmts: Vec<CheckResult<Statement>> = Vec::new();
        for stmt in program.stmts.iter() {
            stmts.push(check_stmt(&mut strings, &mut variables, &mut VecDeque::new(), stmt));
        }

        let mut issues: Vec<CheckIssue> = stmts.iter().flat_map(CheckResult::issues).collect();

        let last_stmt = stmts.last().map(|e| e.value()).flatten();
        match last_stmt {
            None => {
                issues.push(CheckIssue::CheckError(
                    program.range.clone(),
                    "A program needs at least one statement.".to_string(),
                ));
            }
            Some(Statement::Return(_, _)) => {}
            Some(stmt) => {
                issues.push(CheckIssue::CheckError(
                    stmt.range(),
                    "The last statement of a program needs to be a return.".to_string(),
                ));
            }
        }

        let failed = stmts.iter().map(CheckResult::is_failed).any(|x| x);
        if !failed {
            let new_stmts = stmts.iter().filter_map(CheckResult::value).collect();
            CheckResult::Checked(
                Program {
                    range: program.range.clone(),
                    stmts: new_stmts,
                    strings,
                    variables_count: variables.1,
                    variables_size: variables.2,
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

        let checked_program = check_program(&prog);

        match &checked_program {
            CheckResult::Checked(prg, issues) => CheckResult::Checked(
                Project {
                    program: prg.clone(),
                    project_type: project.project_type,
                },
                issues.clone(),
            ),
            _ => CheckResult::Failed(checked_program.issues()),
        }
    }
}
