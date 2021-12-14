#![allow(dead_code)]
mod project {
    use std::collections::HashMap;

    /// The name of a subroutine.
    #[derive(Clone)]
    pub enum SubroutineName<'a> {
        SubroutineName(&'a str),
    }

    impl<'a> SubroutineName<'a> {
        pub fn value(self: &Self) -> &'a str {
            let SubroutineName::SubroutineName(value) = self;
            value
        }
    }

    /// A string literal, including its index in the list of declared string literals.
    #[derive(Clone)]
    pub enum StringLiteral<'a> {
        StringLiteral(u64, &'a str),
    }

    /// A variable, including its offset in memory.
    #[derive(Clone)]
    pub enum Variable<'a> {
        Variable(usize, &'a str),
    }

    /// A statement that can either happen on top level or in a subroutine.
    #[derive(Clone)]
    pub enum Statement<'a> {
        /// Print a string to stdout.
        PrintStr(StringLiteral<'a>),
        /// Print the i64 value in a variable.
        PrintVar(Variable<'a>),
        /// Call a subroutine.
        Call(SubroutineName<'a>),
        /// Assign an i64 to a variable.
        Assignment(Variable<'a>, i64),
    }

    /// A statement that can only happen on the top level.
    #[derive(Clone)]
    pub enum TopLevelStatement<'a> {
        /// Define a new subroutine.
        Subroutine(SubroutineName<'a>, Vec<Statement<'a>>),
        /// A regular statement.
        Stmt(Statement<'a>),
    }

    impl<'a> TopLevelStatement<'a> {
        pub fn subroutine(self: &Self) -> Option<TopLevelStatement<'a>> {
            match self {
                Self::Subroutine(_, _) => Some(self.clone()),
                _ => None,
            }
        }

        pub fn statement(self: &Self) -> Option<Statement<'a>> {
            match self {
                Self::Stmt(stmt) => Some(stmt.clone()),
                _ => None,
            }
        }
    }

    /// The functional part of a project.
    pub struct Program<'a> {
        stmts: Vec<TopLevelStatement<'a>>,
        strings: HashMap<&'a str, u64>,
        variables: HashMap<&'a str, u64>,
    }

    impl<'a> Program<'a> {
        pub fn subroutines(self: &Self) -> Vec<TopLevelStatement<'a>> {
            self.stmts
                .iter()
                .filter_map(|s| s.subroutine())
                .collect::<Vec<TopLevelStatement<'a>>>()
        }

        pub fn statements(self: &Self) -> Vec<Statement<'a>> {
            self.stmts
                .iter()
                .filter_map(|s| s.statement())
                .collect::<Vec<Statement<'a>>>()
        }
    }

    /// The different types of projects that can be defined.
    pub enum ProjectType<'a> {
        /// An executable gets compiled into an executable file and cannot be referenced.
        /// The parameter is the name of the executable.
        Executable(&'a str),
    }

    /// A complete project, parsed and checked from a file.
    pub struct Project<'a> {
        project_type: ProjectType<'a>,
        program: Program<'a>,
    }
}

mod unchecked_project {
    use crate::project::project::{ProjectType, SubroutineName};

    #[derive(Clone)]
    pub enum UncheckedStringLiteral<'a> {
        UStringLiteral(&'a str),
    }

    #[derive(Clone)]
    pub enum UncheckedVariable<'a> {
        UVariable(&'a str),
    }

    #[derive(Clone)]
    pub enum UncheckedStatement<'a> {
        UPrintStr(UncheckedStringLiteral<'a>),
        UPrintVar(UncheckedVariable<'a>),
        UCall(SubroutineName<'a>),
        UAssignment(UncheckedVariable<'a>, i64),
    }

    impl<'a> UncheckedStatement<'a> {
        pub fn call(self: &Self) -> Option<UncheckedStatement<'a>> {
            match self {
                Self::UCall(_) => Some(self.clone()),
                _ => None,
            }
        }

        pub fn name(self: &Self) -> Option<SubroutineName<'a>> {
            match self {
                Self::UCall(name) => Some(name.clone()),
                _ => None,
            }
        }
    }

    #[derive(Clone)]
    pub enum UncheckedTopLevelStatement<'a> {
        USubroutine(SubroutineName<'a>, Vec<UncheckedStatement<'a>>),
        UStmt(UncheckedStatement<'a>),
    }

    impl<'a> UncheckedTopLevelStatement<'a> {
        pub fn subroutine(self: &Self) -> Option<UncheckedTopLevelStatement<'a>> {
            match self {
                Self::USubroutine(_, _) => Some(self.clone()),
                _ => None,
            }
        }

        pub fn statement(self: &Self) -> Option<UncheckedStatement<'a>> {
            match self {
                Self::UStmt(stmt) => Some(stmt.clone()),
                _ => None,
            }
        }

        pub fn subroutine_statements(self: &Self) -> Vec<UncheckedStatement<'a>> {
            match self {
                Self::USubroutine(_, stmts) => stmts.clone(),
                _ => Vec::new(),
            }
        }

        pub fn name(self: &Self) -> Option<SubroutineName<'a>> {
            match self {
                Self::USubroutine(name, _) => Some(name.clone()),
                _ => None,
            }
        }
    }

    #[derive(Clone)]
    pub enum UncheckedProgram<'a> {
        UProgram(Vec<UncheckedTopLevelStatement<'a>>),
    }

    impl<'a> UncheckedProgram<'a> {
        pub fn subroutines(self: &Self) -> Vec<UncheckedTopLevelStatement<'a>> {
            let UncheckedProgram::UProgram(stmts) = self;
            stmts
                .iter()
                .filter_map(UncheckedTopLevelStatement::subroutine)
                .collect::<Vec<UncheckedTopLevelStatement<'a>>>()
        }

        pub fn statements(self: &Self) -> Vec<UncheckedStatement<'a>> {
            let UncheckedProgram::UProgram(stmts) = self;
            stmts
                .iter()
                .filter_map(UncheckedTopLevelStatement::statement)
                .collect::<Vec<UncheckedStatement<'a>>>()
        }

        /// Returns all calls to subroutines that are made in the program.
        pub fn calls(self: &Self) -> Vec<UncheckedStatement<'a>> {
            let stmts = self.statements();
            let mut direct_calls = stmts
                .iter()
                .filter_map(UncheckedStatement::call)
                .collect::<Vec<UncheckedStatement<'a>>>();
            let mut subroutine_statements = self
                .subroutines()
                .iter()
                .flat_map(UncheckedTopLevelStatement::subroutine_statements)
                .collect::<Vec<UncheckedStatement<'a>>>()
                .iter()
                .filter_map(UncheckedStatement::call)
                .collect::<Vec<UncheckedStatement<'a>>>();
            direct_calls.append(&mut subroutine_statements);
            direct_calls
        }
    }

    pub struct UncheckedProject<'a> {
        project_type: ProjectType<'a>,
        program: UncheckedProgram<'a>,
    }
}
