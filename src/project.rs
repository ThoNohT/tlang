#![allow(dead_code)]
pub mod project {
    use crate::lexer::Range;
    use std::collections::HashMap;

    /// The name of a subroutine.
    #[derive(PartialEq, Eq, Hash, Clone, Debug)]
    pub enum SubroutineName {
        SubroutineName(Range, String),
    }

    impl SubroutineName {
        pub fn value(self: &Self) -> &String {
            let SubroutineName::SubroutineName(_, value) = self;
            value
        }

        /// Checks whether two subroutine names are equal.
        pub fn equals(self: &Self, other: Option<Self>) -> bool {
            other.map_or(false, |o| o.value() == self.value())
        }
    }

    /// A string literal, including its index in the list of declared string literals.
    #[derive(Clone, Debug)]
    pub enum StringLiteral {
        StringLiteral(Range, usize, String),
    }

    /// A variable, including its offset in memory.
    #[derive(Clone, Debug)]
    pub enum Variable {
        Variable(Range, usize, String),
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

    /// A statement that can either happen on top level or in a subroutine.
    #[derive(Clone, Debug)]
    pub enum Statement {
        /// Print a string to stdout.
        PrintStr(Range, StringLiteral),
        /// Print the i64 value in a variable.
        PrintVar(Range, Variable),
        /// Call a subroutine.
        Call(Range, SubroutineName),
        /// Assign an expression to a variable.
        Assignment(Range, Variable, Expression),
    }

    /// A statement that can only happen on the top level.
    #[derive(Clone, Debug)]
    pub enum TopLevelStatement {
        /// Define a new subroutine.
        Subroutine(Range, SubroutineName, Vec<Statement>),
        /// A regular statement.
        Stmt(Range, Statement),
    }

    impl TopLevelStatement {
        pub fn subroutine(self: &Self) -> Option<TopLevelStatement> {
            match self {
                Self::Subroutine(_, _, _) => Some(self.clone()),
                _ => None,
            }
        }

        pub fn statement(self: &Self) -> Option<Statement> {
            match self {
                Self::Stmt(_, stmt) => Some(stmt.clone()),
                _ => None,
            }
        }
    }

    /// The functional part of a project.
    #[derive(Clone, Debug)]
    pub struct Program {
        pub range: Range,
        pub stmts: Vec<TopLevelStatement>,
        pub strings: HashMap<String, usize>,
        pub variables: HashMap<String, usize>,
    }

    impl Program {
        pub fn subroutines(self: &Self) -> Vec<TopLevelStatement> {
            self.stmts.iter().filter_map(|s| s.subroutine()).collect::<Vec<TopLevelStatement>>()
        }

        pub fn statements(self: &Self) -> Vec<Statement> {
            self.stmts.iter().filter_map(|s| s.statement()).collect::<Vec<Statement>>()
        }
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
    use crate::project::project::{Operator, ProjectType, SubroutineName};

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

    #[derive(Clone, Debug)]
    pub enum UncheckedStatement {
        UPrintStr(Range, UncheckedStringLiteral),
        UPrintVar(Range, UncheckedVariable),
        UCall(Range, SubroutineName),
        UAssignment(Range, UncheckedVariable, UncheckedExpression),
    }

    impl UncheckedStatement {
        pub fn call(self: &Self) -> Option<UncheckedStatement> {
            match self {
                Self::UCall(_, _) => Some(self.clone()),
                _ => None,
            }
        }

        pub fn name(self: &Self) -> Option<SubroutineName> {
            match self {
                Self::UCall(_, name) => Some(name.clone()),
                _ => None,
            }
        }

        pub fn range(self: &Self) -> &Range {
            match self {
                Self::UPrintStr(range, _) => range,
                Self::UPrintVar(range, _) => range,
                Self::UCall(range, _) => range,
                Self::UAssignment(range, _, _) => range,
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedTopLevelStatement {
        USubroutine(Range, SubroutineName, Vec<UncheckedStatement>),
        UStmt(Range, UncheckedStatement),
    }

    impl UncheckedTopLevelStatement {
        pub fn subroutine(self: &Self) -> Option<UncheckedTopLevelStatement> {
            match self {
                Self::USubroutine(_, _, _) => Some(self.clone()),
                _ => None,
            }
        }

        pub fn statement(self: &Self) -> Option<UncheckedStatement> {
            match self {
                Self::UStmt(_, stmt) => Some(stmt.clone()),
                _ => None,
            }
        }

        pub fn subroutine_statements(self: &Self) -> Vec<UncheckedStatement> {
            match self {
                Self::USubroutine(_, _, stmts) => stmts.clone(),
                _ => Vec::new(),
            }
        }

        pub fn name(self: &Self) -> Option<SubroutineName> {
            match self {
                Self::USubroutine(_, name, _) => Some(name.clone()),
                _ => None,
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct UncheckedProgram {
        pub range: Range,
        pub stmts: Vec<UncheckedTopLevelStatement>,
    }

    impl UncheckedProgram {
        /// Returns all top-level statements that are subroutine calls in the program.
        pub fn subroutines(self: &Self) -> Vec<UncheckedTopLevelStatement> {
            self.stmts
                .iter()
                .filter_map(UncheckedTopLevelStatement::subroutine)
                .collect::<Vec<UncheckedTopLevelStatement>>()
        }

        /// Returns all top-level statements that are statements in the program.
        pub fn statements(self: &Self) -> Vec<UncheckedStatement> {
            self.stmts.iter().filter_map(UncheckedTopLevelStatement::statement).collect::<Vec<UncheckedStatement>>()
        }

        /// Returns all calls to subroutines that are made in the program.
        pub fn calls(self: &Self) -> Vec<UncheckedStatement> {
            let stmts = self.statements();
            let mut direct_calls =
                stmts.iter().filter_map(UncheckedStatement::call).collect::<Vec<UncheckedStatement>>();
            let mut subroutine_statements = self
                .subroutines()
                .iter()
                .flat_map(UncheckedTopLevelStatement::subroutine_statements)
                .collect::<Vec<UncheckedStatement>>()
                .iter()
                .filter_map(UncheckedStatement::call)
                .collect::<Vec<UncheckedStatement>>();
            direct_calls.append(&mut subroutine_statements);
            direct_calls
        }
    }

    #[derive(Debug)]
    pub struct UncheckedProject {
        pub project_type: ProjectType,
        pub program: UncheckedProgram,
    }
}
