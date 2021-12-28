#![allow(dead_code)]
pub mod project {
    use std::collections::HashMap;

    /// The name of a subroutine.
    #[derive(PartialEq, Eq, Hash, Clone, Debug)]
    pub enum SubroutineName {
        SubroutineName(String),
    }

    impl SubroutineName {
        pub fn value(self: &Self) -> &String {
            let SubroutineName::SubroutineName(value) = self;
            value
        }
    }

    /// A string literal, including its index in the list of declared string literals.
    #[derive(Clone, Debug)]
    pub enum StringLiteral {
        StringLiteral(usize, String),
    }

    /// A variable, including its offset in memory.
    #[derive(Clone, Debug)]
    pub enum Variable {
        Variable(usize, String),
    }

    /// A statement that can either happen on top level or in a subroutine.
    #[derive(Clone, Debug)]
    pub enum Statement {
        /// Print a string to stdout.
        PrintStr(StringLiteral),
        /// Print the i64 value in a variable.
        PrintVar(Variable),
        /// Call a subroutine.
        Call(SubroutineName),
        /// Assign an i64 to a variable.
        Assignment(Variable, i64),
    }

    /// A statement that can only happen on the top level.
    #[derive(Clone, Debug)]
    pub enum TopLevelStatement {
        /// Define a new subroutine.
        Subroutine(SubroutineName, Vec<Statement>),
        /// A regular statement.
        Stmt(Statement),
    }

    impl TopLevelStatement {
        pub fn subroutine(self: &Self) -> Option<TopLevelStatement> {
            match self {
                Self::Subroutine(_, _) => Some(self.clone()),
                _ => None,
            }
        }

        pub fn statement(self: &Self) -> Option<Statement> {
            match self {
                Self::Stmt(stmt) => Some(stmt.clone()),
                _ => None,
            }
        }
    }

    /// The functional part of a project.
    #[derive(Debug)]
    pub struct Program {
        pub stmts: Vec<TopLevelStatement>,
        pub strings: HashMap<String, usize>,
        pub variables: HashMap<String, usize>,
    }

    impl Program {
        pub fn subroutines(self: &Self) -> Vec<TopLevelStatement> {
            self.stmts
                .iter()
                .filter_map(|s| s.subroutine())
                .collect::<Vec<TopLevelStatement>>()
        }

        pub fn statements(self: &Self) -> Vec<Statement> {
            self.stmts
                .iter()
                .filter_map(|s| s.statement())
                .collect::<Vec<Statement>>()
        }
    }

    /// The different types of projects that can be defined.
    #[derive(Clone, Debug)]
    pub enum ProjectType {
        /// An executable gets compiled into an executable file and cannot be referenced.
        /// The parameter is the name of the executable.
        Executable(String),
    }

    /// A complete project, parsed and checked from a file.
    #[derive(Debug)]
    pub struct Project {
        pub project_type: ProjectType,
        pub program: Program,
    }
}

pub mod unchecked_project {
    use crate::project::project::{ProjectType, SubroutineName};

    #[derive(Clone, Debug)]
    pub enum UncheckedStringLiteral {
        UStringLiteral(String),
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedVariable {
        UVariable(String),
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedStatement {
        UPrintStr(UncheckedStringLiteral),
        UPrintVar(UncheckedVariable),
        UCall(SubroutineName),
        UAssignment(UncheckedVariable, i64),
    }

    impl UncheckedStatement {
        pub fn call(self: &Self) -> Option<UncheckedStatement> {
            match self {
                Self::UCall(_) => Some(self.clone()),
                _ => None,
            }
        }

        pub fn name(self: &Self) -> Option<SubroutineName> {
            match self {
                Self::UCall(name) => Some(name.clone()),
                _ => None,
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum UncheckedTopLevelStatement {
        USubroutine(SubroutineName, Vec<UncheckedStatement>),
        UStmt(UncheckedStatement),
    }

    impl UncheckedTopLevelStatement {
        pub fn subroutine(self: &Self) -> Option<UncheckedTopLevelStatement> {
            match self {
                Self::USubroutine(_, _) => Some(self.clone()),
                _ => None,
            }
        }

        pub fn statement(self: &Self) -> Option<UncheckedStatement> {
            match self {
                Self::UStmt(stmt) => Some(stmt.clone()),
                _ => None,
            }
        }

        pub fn subroutine_statements(self: &Self) -> Vec<UncheckedStatement> {
            match self {
                Self::USubroutine(_, stmts) => stmts.clone(),
                _ => Vec::new(),
            }
        }

        pub fn name(self: &Self) -> Option<SubroutineName> {
            match self {
                Self::USubroutine(name, _) => Some(name.clone()),
                _ => None,
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct UncheckedProgram {
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
            self.stmts
                .iter()
                .filter_map(UncheckedTopLevelStatement::statement)
                .collect::<Vec<UncheckedStatement>>()
        }

        /// Returns all calls to subroutines that are made in the program.
        pub fn calls(self: &Self) -> Vec<UncheckedStatement> {
            let stmts = self.statements();
            let mut direct_calls = stmts
                .iter()
                .filter_map(UncheckedStatement::call)
                .collect::<Vec<UncheckedStatement>>();
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
