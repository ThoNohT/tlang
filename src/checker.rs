use crate::{
    console,
    lexer::{Range, WithRange},
    prelude::OptExt,
    project::project::Variable,
};
use std::collections::{HashMap, VecDeque};

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

    /// Indicates whether the issue is an error.
    pub fn is_error(self: &Self) -> bool {
        match self {
            Self::CheckError(_, _) => true,
            _ => false,
        }
    }
}

impl WithRange for CheckIssue {
    fn range(self: &Self) -> &Range {
        match self {
            Self::CheckError(r, _) => r,
            Self::CheckWarning(r, _) => r,
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

impl<T: Clone + WithRange> WithRange for CheckResult<T> {
    fn range(self: &Self) -> &Range {
        match self {
            CheckResult::Checked(e, _) => e.range(),
            CheckResult::Failed(issues) => issues
                .iter()
                .next()
                .assert_some(|| console::return_with_error("A failed check result needs at least one issue."))
                .range(),
        }
    }
}

/// The string indexes known throughout the program.
type StringIndexes = HashMap<String, usize>;

/// The variable offsets an indexes known in the current scope, the current counter for the number of variables
/// assigned and the offset to use for the next variable.
type VariableCache = (HashMap<String, Variable>, u32, usize);

/// Prints the currently known variables.
#[allow(unused)]
fn dbg_vars(ctx: &mut VecDeque<String>, vars: &mut VariableCache) {
    print!("{:?}: ", ctx);
    for (_, var) in vars.0.iter() {
        print!("{} ", var.name);
    }
    println!();
}

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

/// Defines a new variable, and returns the new variable. The next offset and index for variables is updated.
/// If the variable was defined before, a failed CheckResult will be returned and no variable will be defined.
fn define_variable<'a>(
    range: &Range,
    cache: &'a mut VariableCache,
    ctx: &mut VecDeque<String>,
    name: &String,
    takes_param: bool,
) -> CheckResult<Variable> {
    if let Some(var) = cache.0.get(name) {
        CheckResult::Failed(Vec::from([CheckIssue::CheckError(
            range.clone(),
            format!("Variable {} already defined. Original definition is at {}", name, var.range.to_full_string()),
        )]))
    } else {
        let result = Variable {
            index: cache.1,
            range: range.clone(),
            offset: cache.2,
            name: name.clone(),
            context: ctx.iter().map(|v| v.clone()).collect(),
            takes_param,
        };
        cache.0.insert(name.clone(), result.clone());
        cache.1 += 1;
        cache.2 += 8;
        CheckResult::perfect(result)
    }
}

/// Gets an existing variable. If the variable does not exist a failed CheckResult will be returned.
fn get_variable<'a>(range: &Range, cache: &'a mut VariableCache, name: &String) -> CheckResult<Variable> {
    if let Some(var) = cache.0.get(name) {
        CheckResult::perfect(var.clone())
    } else {
        CheckResult::Failed(Vec::from([CheckIssue::CheckError(
            range.clone(),
            format!("Variable {} is not defined.", name),
        )]))
    }
}

pub mod check {
    use crate::checker::{define_variable, get_string_idx, get_variable};
    use crate::checker::{CheckIssue, CheckResult, StringIndexes, VariableCache};
    use crate::lexer::WithRange;
    use crate::project::project::*;
    use crate::project::unchecked_project::*;
    use std::collections::{HashMap, VecDeque};

    fn check_expression(
        strings: &mut StringIndexes,
        variables: &mut VariableCache,
        ctx: &mut VecDeque<String>,
        expr: &UncheckedExpression,
    ) -> CheckResult<Expression> {
        match expr {
            UncheckedExpression::UIntLiteral(r, int_val) => {
                CheckResult::perfect(Expression::IntLiteral(r.clone(), int_val.clone()))
            }
            UncheckedExpression::UVariable(r, variable, expr_opt) => {
                let var = get_variable(&variable.range, variables, &variable.name);
                let expr_opt = expr_opt.clone().map(|e| check_expression(strings, variables, ctx, &e));

                let expr_opt = match expr_opt {
                    Some(CheckResult::Checked(e, i)) => CheckResult::Checked(Some(e), i),
                    Some(CheckResult::Failed(i)) => CheckResult::Failed(i),
                    None => CheckResult::perfect(None),
                };

                var.bind(|var| {
                    expr_opt.bind(|expr_opt| match (var.takes_param, expr_opt) {
                        (true, Some(expr)) => {
                            CheckResult::perfect(Expression::Variable(r.clone(), var, Some(Box::new(expr))))
                        }
                        (false, None) => CheckResult::perfect(Expression::Variable(r.clone(), var, None)),
                        (false, Some(_)) => CheckResult::Failed(Vec::from([CheckIssue::CheckError(
                            r.clone(),
                            format!("Variable {} does not take a parameter, but one was provided.", var.name),
                        )])),
                        (true, None) => CheckResult::Failed(Vec::from([CheckIssue::CheckError(
                            r.clone(),
                            format!("Variable {} takes a parameter, but none was provided.", var.name),
                        )])),
                    })
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
        variables: &mut VariableCache,
        ctx: &mut VecDeque<String>,
        assmt: &UncheckedAssignment,
    ) -> CheckResult<Assignment> {
        match assmt {
            UncheckedAssignment::UExprAssignment(r, expr) => {
                check_expression(strings, variables, ctx, expr).map(|e| Assignment::ExprAssignment(r.clone(), e))
            }
            UncheckedAssignment::UBlockAssignment(r, stmts) => {
                let mut checked_stmts = Vec::new();

                // Variables defined inside these statements should not leak out.
                let variables_backup = variables.clone();
                for stmt in stmts.iter() {
                    checked_stmts.push(check_stmt(strings, variables, ctx, stmt));
                }

                // Restore previous variable list.
                variables.0 = variables_backup.0;

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
                            stmt.range().clone(),
                            "The last statement of a block assignment needs to be a return.".to_string(),
                        ));
                    }
                }

                let mut issues: Vec<CheckIssue> = checked_stmts.iter().flat_map(CheckResult::issues).collect();

                // The first statement after a return is not reachable.

                if let Some(first_unreachable) =
                    checked_stmts.iter().skip_while(|x| x.value().map_or(true, |s| !s.is_return())).skip(1).next()
                {
                    issues.push(CheckIssue::CheckWarning(
                        first_unreachable.range().clone(),
                        "Statement is unreachable.".to_string(),
                    ));
                }

                if !issues.iter().map(CheckIssue::is_error).any(|x| x) {
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
        variables: &mut VariableCache,
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
            UncheckedStatement::UAssignment(r1, variable, param_opt, expr) => {
                // Check assignment before variable so the variable is not yet known during
                // assignment evaluation. But do check the variable even if assignment fails
                // so it is known later.
                ctx.push_back(variable.name.clone());

                // The parameter is only known inside the assignment.
                let variables_backup = variables.clone();

                let param_opt = param_opt.clone().map(|p| define_variable(&p.range, variables, ctx, &p.name, false));
                let assmt = check_assignment(strings, variables, ctx, &expr);

                // Restore the previous context and variables.
                ctx.pop_back();
                variables.0 = variables_backup.0;
                let var = define_variable(&variable.range, variables, ctx, &variable.name, param_opt.is_some());

                let param_opt =
                    // A parameter must now simply be an i64 parameter, so it cannot take a parameter.
                    match param_opt {
                        None => CheckResult::perfect(None),
                        Some(CheckResult::Checked(r, i)) => CheckResult::Checked(Some(r), i),
                        Some(CheckResult::Failed(i)) => CheckResult::Failed(i),
                    };

                assmt.bind(|a| var.bind(|v| param_opt.map(|po_opt| Statement::Assignment(r1.clone(), v, po_opt, a))))
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

        // The last statement must be a return statament.
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
                    stmt.range().clone(),
                    "The last statement of a program needs to be a return.".to_string(),
                ));
            }
        }

        // The first statement after a return is not reachable.
        if let Some(first_unreachable) =
            stmts.iter().skip_while(|x| x.value().map_or(true, |s| !s.is_return())).skip(1).next()
        {
            issues.push(CheckIssue::CheckWarning(
                first_unreachable.range().clone(),
                "Statement is unreachable.".to_string(),
            ));
        }

        if !stmts.iter().map(CheckResult::is_failed).any(|x| x) {
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
