pub mod project {
    use crate::{
        console::formatting::{bold, color, Formattable},
        lexer::{Range, WithRange},
    };
    use std::collections::HashMap;

    /// A string literal, including its index in the list of declared string literals.
    #[derive(Clone, Debug)]
    pub enum StringLiteral {
        StringLiteral(Range, usize, String),
    }

    impl Formattable for StringLiteral {
        fn format_bare(self: &Self) -> String {
            let StringLiteral::StringLiteral(r, idx, str) = self;
            format!("{} {} \"{}\"\nindex: {}", r.format(0), bold("StringLiteral"), color(35, str), color(35, idx))
        }
    }

    /// A variable.
    #[derive(Clone, Debug)]
    pub struct Variable {
        pub range: Range,
        pub index: u32,
        pub offset: usize,
        pub name: String,
        pub context: Vec<String>,
        pub takes_param: bool,
    }

    impl Formattable for Variable {
        fn format_bare(self: &Self) -> String {
            format!(
                "{} {} {}\nindex: {}, offset: {}, context: {}, takes param: {}",
                self.range.format(0),
                bold("Variable"),
                color(35, &self.name),
                color(35, self.index),
                color(35, self.offset),
                color(
                    35,
                    if !self.context.is_empty() {
                        format!("root::{}", self.context.join("::"))
                    } else {
                        format!("root")
                    }
                ),
                color(35, self.takes_param)
            )
        }
    }

    #[derive(Clone, Debug)]
    pub enum Operator {
        Add(Range),
        Sub(Range),
    }

    impl Formattable for Operator {
        fn format_bare(self: &Self) -> String {
            match self {
                Operator::Add(r) => format!("{} {}", r.format(0), bold("Add")),
                Operator::Sub(r) => format!("{} {}", r.format(0), bold("Sub")),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum Expression {
        IntLiteral(Range, i64),
        Variable(Range, Variable, Option<Box<Expression>>),
        Binary(Range, Operator, Box<Expression>, Box<Expression>),
    }

    impl Formattable for Expression {
        fn format_bare(self: &Self) -> String {
            match self {
                Expression::IntLiteral(r, i) => format!("{} {} {}", r.format(0), bold("IntLiteral"), color(35, i)),
                Expression::Variable(r, v, expr_opt) => match expr_opt {
                    Some(expr) => {
                        format!("{} {}\n{}\n{}", r.format(0), bold("Variable"), v.format(1), expr.format(1))
                    }
                    None => format!("{} {}\n{}", r.format(0), bold("Variable"), v.format(1)),
                },
                Expression::Binary(r, op, expr_l, expr_r) => {
                    format!(
                        "{} {}\n{}\n{}\n{}",
                        r.format(0),
                        bold("Binary"),
                        op.format(1),
                        expr_l.format(1),
                        expr_r.format(1)
                    )
                }
            }
        }
    }

    /// An assignment to a variable can be done either using a simple expression, or with a block
    /// of statements ending with a Return statement.
    #[derive(Clone, Debug)]
    pub enum Assignment {
        ExprAssignment(Range, Expression),
        BlockAssignment(Range, Vec<Statement>),
    }

    impl Formattable for Assignment {
        fn format_bare(self: &Self) -> String {
            match self {
                Assignment::ExprAssignment(r, expr) => {
                    format!("{} {}\n{}", r.format(0), bold("ExprAssignment"), expr.format(1))
                }
                Assignment::BlockAssignment(r, stmts) => {
                    format!(
                        "{} {}\n{}",
                        r.format(0),
                        bold("BlockAssignment"),
                        stmts.iter().map(|s| s.format(1)).collect::<Vec<String>>().join("\n")
                    )
                }
            }
        }
    }

    /// A statement that can either happen on top level or in a subroutine.
    #[derive(Clone, Debug)]
    pub enum Statement {
        /// Print a string to stdout.
        PrintStr(Range, StringLiteral),
        /// Print the i64 value in an expression.
        PrintExpr(Range, Expression),
        /// Assign an expression to a variable.
        /// TODO: currently, the end result of an assignment can always be stored in a variable,
        // this will change once variable assignments can take parameters and they effectively become
        // functions with or without parameters.
        Assignment(Range, Variable, Option<Variable>, Assignment),
        /// Return an expression from a function.
        Return(Range, Expression),
    }

    impl Statement {
        pub fn is_return(self: &Self) -> bool {
            match self {
                Self::Return(_, _) => true,
                _ => false,
            }
        }
    }

    impl WithRange for Statement {
        fn range(self: &Self) -> &Range {
            match self {
                Statement::PrintStr(r, _) => r,
                Statement::PrintExpr(r, _) => r,
                Statement::Assignment(r, _, _, _) => r,
                Statement::Return(r, _) => r,
            }
        }
    }

    impl Formattable for Statement {
        fn format_bare(self: &Self) -> String {
            match self {
                Statement::PrintStr(r, str) => format!("{} {}\n{}", r.format(0), bold("PrintStr"), str.format(1)),
                Statement::PrintExpr(r, expr) => format!("{} {}\n{}", r.format(0), bold("PrintExpr"), expr.format(1)),
                Statement::Assignment(r, v, arg_opt, assmt) => match arg_opt {
                    Some(var) => {
                        format!(
                            "{} {}\n{}\n{}\n{}",
                            r.format(0),
                            bold("Assignment"),
                            v.format(1),
                            var.format(1),
                            assmt.format(1)
                        )
                    }
                    None => format!("{} {}\n{}\n{}", r.format(0), bold("Assignment"), v.format(1), assmt.format(1)),
                },
                Statement::Return(r, expr) => format!("{} {}\n{}", r.format(0), bold("Return"), expr.format(1)),
            }
        }
    }

    /// The functional part of a project.
    #[derive(Clone, Debug)]
    pub struct Program {
        pub range: Range,
        pub stmts: Vec<Statement>,
        pub strings: HashMap<String, usize>,
        pub variables_size: usize,
        pub variables_count: u32,
    }

    impl Formattable for Program {
        fn format_bare(self: &Self) -> String {
            format!(
                "{} {}\n{}\n",
                self.range.format(0),
                bold("Program"),
                self.stmts.iter().map(|s| s.format(1)).collect::<Vec<String>>().join("\n")
            )
        }
    }

    /// The different types of projects that can be defined.
    #[derive(Clone, Debug)]
    pub enum ProjectType {
        /// An executable gets compiled into an executable file and cannot be referenced.
        /// The parameter is the name of the executable.
        Executable(Range, String),
    }

    impl Formattable for ProjectType {
        fn format_bare(self: &Self) -> String {
            let Self::Executable(r, name) = self;
            format!("{} {} {}", r.format(0), bold("Executable"), color(35, name))
        }
    }

    /// A complete project, parsed and checked from a file.
    #[derive(Clone, Debug)]
    pub struct Project {
        pub project_type: ProjectType,
        pub program: Program,
    }

    impl Formattable for Project {
        fn format_bare(self: &Self) -> String {
            format!("{}\n{}\n{}", bold("Project"), self.project_type.format(1), self.program.format(1))
        }
    }
}

pub mod unchecked_project {
    use crate::console::formatting::{bold, color, Formattable};
    use crate::lexer::Range;
    use crate::project::project::{Operator, ProjectType};

    #[derive(Clone, Debug)]
    pub enum UncheckedStringLiteral {
        UStringLiteral(Range, String),
    }

    impl Formattable for UncheckedStringLiteral {
        fn format_bare(self: &Self) -> String {
            let UncheckedStringLiteral::UStringLiteral(r, str) = self;
            format!("{} {} \"{}\"", r.format(0), bold("UStringLiteral"), color(35, str))
        }
    }

    #[derive(Clone, Debug)]
    pub struct UncheckedVariable {
        pub range: Range,
        pub name: String,
    }

    impl Formattable for UncheckedVariable {
        fn format_bare(self: &Self) -> String {
            format!("{} {} {}", self.range.format(0), bold("UncheckedVariable"), color(35, &self.name))
        }
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedExpression {
        UIntLiteral(Range, i64),
        UVariable(Range, UncheckedVariable, Option<Box<UncheckedExpression>>),
        UBinary(Range, Operator, Box<UncheckedExpression>, Box<UncheckedExpression>),
    }

    impl UncheckedExpression {
        pub fn get_range(self: &Self) -> Range {
            match self {
                Self::UIntLiteral(r, _) => r,
                Self::UVariable(r, _, _) => r,
                Self::UBinary(r, _, _, _) => r,
            }
            .clone()
        }
    }

    impl Formattable for UncheckedExpression {
        fn format_bare(self: &Self) -> String {
            match self {
                UncheckedExpression::UIntLiteral(r, i) => {
                    format!("{} {} {}", r.format(0), bold("UIntLiteral"), color(35, i))
                }
                UncheckedExpression::UVariable(r, v, expr_opt) => match expr_opt {
                    Some(expr) => format!("{} {}\n{}\n{}", r.format(0), bold("UVariable"), v.format(1), expr.format(1)),
                    None => format!("{} {}\n{}", r.format(0), bold("UVariable"), v.format(1)),
                },
                UncheckedExpression::UBinary(r, op, expr_l, expr_r) => {
                    format!(
                        "{} {}\n{}\n{}\n{}",
                        r.format(0),
                        bold("UBinary"),
                        op.format(1),
                        expr_l.format(1),
                        expr_r.format(1)
                    )
                }
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedAssignment {
        UExprAssignment(Range, UncheckedExpression),
        UBlockAssignment(Range, Vec<UncheckedStatement>),
    }

    impl Formattable for UncheckedAssignment {
        fn format_bare(self: &Self) -> String {
            match self {
                UncheckedAssignment::UExprAssignment(r, expr) => {
                    format!("{} {}\n{}", r.format(0), bold("UExprAssignment"), expr.format(1))
                }
                UncheckedAssignment::UBlockAssignment(r, stmts) => {
                    format!(
                        "{} {}\n{}",
                        r.format(0),
                        bold("UBlockAssignment"),
                        stmts.iter().map(|s| s.format(1)).collect::<Vec<String>>().join("\n")
                    )
                }
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedStatement {
        UPrintStr(Range, UncheckedStringLiteral),
        UPrintExpr(Range, UncheckedExpression),
        UAssignment(Range, UncheckedVariable, Option<UncheckedVariable>, UncheckedAssignment),
        UReturn(Range, UncheckedExpression),
    }

    impl Formattable for UncheckedStatement {
        fn format_bare(self: &Self) -> String {
            match self {
                UncheckedStatement::UPrintStr(r, str) => {
                    format!("{} {}\n{}", r.format(0), bold("UPrintStr"), str.format(1))
                }
                UncheckedStatement::UPrintExpr(r, expr) => {
                    format!("{} {}\n{}", r.format(0), bold("UPrintExpr"), expr.format(1))
                }
                UncheckedStatement::UAssignment(r, v, arg_opt, assmt) => match arg_opt {
                    Some(var) => {
                        format!(
                            "{} {}\n{}\n{}\n{}",
                            r.format(0),
                            bold("UAssignment"),
                            v.format(1),
                            var.format(1),
                            assmt.format(1)
                        )
                    }
                    None => format!("{} {}\n{}\n{}", r.format(0), bold("UAssignment"), v.format(1), assmt.format(1)),
                },
                UncheckedStatement::UReturn(r, expr) => {
                    format!("{} {}\n{}", r.format(0), bold("UReturn"), expr.format(1))
                }
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct UncheckedProgram {
        pub range: Range,
        pub stmts: Vec<UncheckedStatement>,
    }

    impl Formattable for UncheckedProgram {
        fn format_bare(self: &Self) -> String {
            format!(
                "{} {}\n{}\n",
                self.range.format(0),
                bold("UncheckedProgam"),
                self.stmts.iter().map(|s| s.format(1)).collect::<Vec<String>>().join("\n")
            )
        }
    }

    #[derive(Debug)]
    pub struct UncheckedProject {
        pub project_type: ProjectType,
        pub program: UncheckedProgram,
    }

    impl Formattable for UncheckedProject {
        fn format_bare(self: &Self) -> String {
            format!("{}\n{}\n{}", bold("UncheckedProject"), self.project_type.format(1), self.program.format(1))
        }
    }
}
