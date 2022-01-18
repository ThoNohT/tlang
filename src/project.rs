pub mod project {
    use crate::lexer::Range;
    use std::collections::HashMap;

    /// A string literal, including its index in the list of declared string literals.
    #[derive(Clone, Debug)]
    pub enum StringLiteral {
        StringLiteral(Range, usize, String),
    }

    /// A variable.
    #[derive(Clone, Debug)]
    pub struct Variable {
        pub range: Range,
        pub index: u32,
        pub offset: usize,
        pub name: String,
        pub context: Vec<String>,
    }

    #[derive(Clone, Debug)]
    pub enum Operator {
        Add(Range),
        Sub(Range),
    }

    #[derive(Clone, Debug)]
    pub enum Expression {
        IntLiteral(Range, i64),
        Variable(Range, Variable),
        Binary(Range, Operator, Box<Expression>, Box<Expression>),
    }

    /// An assignment to a variable can be done either using a simple expression, or with a block
    /// of statements ending with a Return statement.
    #[derive(Clone, Debug)]
    pub enum Assignment {
        ExprAssignment(Range, Expression),
        BlockAssignment(Range, Vec<Statement>),
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
        Assignment(Range, Variable, Assignment),
        /// Return an expression from a function.
        Return(Range, Expression),
    }

    impl Statement {
        pub fn range(self: &Self) -> Range {
            match self {
                Self::PrintStr(range, _) => range,
                Self::PrintExpr(range, _) => range,
                Self::Assignment(range, _, _) => range,
                Self::Return(range, _) => range,
            }
            .clone()
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

    /// The different types of projects that can be defined.
    #[derive(Clone, Debug)]
    pub enum ProjectType {
        /// An executable gets compiled into an executable file and cannot be referenced.
        /// The parameter is the name of the executable.
        Executable(Range, String),
    }

    /// A complete project, parsed and checked from a file.
    #[derive(Clone, Debug)]
    pub struct Project {
        pub project_type: ProjectType,
        pub program: Program,
    }
}

pub mod unchecked_project {
    use crate::lexer::Range;
    use crate::project::project::{Operator, ProjectType};

    #[derive(Clone, Debug)]
    pub enum UncheckedStringLiteral {
        UStringLiteral(Range, String),
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedVariable {
        UVariable(Range, String),
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedExpression {
        UIntLiteral(Range, i64),
        UVariable(Range, UncheckedVariable),
        UBinary(Range, Operator, Box<UncheckedExpression>, Box<UncheckedExpression>),
    }

    impl UncheckedExpression {
        pub fn get_range(self: &Self) -> Range {
            match self {
                Self::UIntLiteral(r, _) => r,
                Self::UVariable(r, _) => r,
                Self::UBinary(r, _, _, _) => r,
            }
            .clone()
        }
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedAssignment {
        UExprAssignment(Range, UncheckedExpression),
        UBlockAssignment(Range, Vec<UncheckedStatement>),
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedStatement {
        UPrintStr(Range, UncheckedStringLiteral),
        UPrintExpr(Range, UncheckedExpression),
        UAssignment(Range, UncheckedVariable, UncheckedAssignment),
        UReturn(Range, UncheckedExpression),
    }

    #[derive(Clone, Debug)]
    pub struct UncheckedProgram {
        pub range: Range,
        pub stmts: Vec<UncheckedStatement>,
    }

    #[derive(Debug)]
    pub struct UncheckedProject {
        pub project_type: ProjectType,
        pub program: UncheckedProgram,
    }
}
