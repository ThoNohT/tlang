module tlang.Project

////////// Model //////////

/// The name of a subroutine.
type SubroutineName = SubroutineName of string

/// A string literal, including its index in the list of declared string literals.
type StringLiteral = IndexedStringLiteral of int * string

/// A variable, including its offset in memory.
type Variable = Variable of int * string

type Statement =
    | PrintStr of StringLiteral
    | PrintVar of Variable
    | Call of SubroutineName
    | Assignment of Variable * int64

type TopLevelStatement =
    | Subroutine of SubroutineName * List<Statement>
    | Stmt of Statement

type Program = {
    Stmts: List<TopLevelStatement>
    Strings: Map<string, int>
    Variables: Map<string, int>
}

/// The different types of projects that can be defined.
type ProjectType =
    /// An executable gets compiled into an executable file and cannot be referenced.
    /// The parameter is the name of the executable.
    | Executable of string


type Project = { Type: ProjectType ; Program: Program }


////////// UncheckedModel //////////

type UncheckedStringLiteral = StringLiteral of string

/// A variable.
type UncheckedVariable = Variable of string

/// A statement that can happen either on top level or in a subroutine.
type UncheckedStatement =
    // Print a string to stdout.
    | PrintStr of UncheckedStringLiteral
    // Print the int64 value in a variable.
    | PrintVar of UncheckedVariable
    // Call a subroutine.
    | Call of SubroutineName
    // Assignment of an int64 to a variable.
    | Assignment of UncheckedVariable * int64

/// A statement that can only appear at the top level of a program.
type UncheckedTopLevelStatement =
    // Defines a new subroutine.
    | Subroutine of SubroutineName * List<UncheckedStatement>
    // A regular statement.
    | Stmt of UncheckedStatement

type UncheckedProgram = Program of List<UncheckedTopLevelStatement>

/// A complete project parsed from a file.
type UncheckedProject = { Type: ProjectType ; Program: UncheckedProgram }


////////// Operations Model //////////

module SubroutineName =
    let value (SubroutineName name) = name

module TopLevelStatement =
    let subroutine  =
        function
        | ((TopLevelStatement.Subroutine _) as s) -> Some s
        | _ -> None

    let statement =
        function
        | TopLevelStatement.Stmt stmt -> Some stmt
        | _ -> None

    /// Get the name of a subroutine referenced in a statement.
    let name =
        function
        | TopLevelStatement.Subroutine (name, _) -> Some name
        | _ -> None

module Program =
    let subroutines prog = List.choose TopLevelStatement.subroutine prog.Stmts
    let statements prog = List.choose TopLevelStatement.statement prog.Stmts


///////// Operations UncheckedModel /////////

module UncheckedStatement =
    let call =
        function
        | ((UncheckedStatement.Call _) as c) -> Some c
        | _ -> None

    /// Get the name of a subroutine referenced in a statement.
    let name =
        function
        | UncheckedStatement.Call name -> Some name
        | _ -> None

module UncheckedTopLevelStatement =
    let subroutine  =
        function
        | ((UncheckedTopLevelStatement.Subroutine _) as s) -> Some s
        | _ -> None

    let statement =
        function
        | UncheckedTopLevelStatement.Stmt stmt -> Some stmt
        | _ -> None

    /// Returns all statements in a subroutine, or an empty list if it is not a subroutine.
    let subroutineStatements =
        function
        | (UncheckedTopLevelStatement.Subroutine (_, stmts)) -> stmts
        | _ -> []

module UncheckedProgram =
    let subroutines (UncheckedProgram.Program stmts) = List.choose UncheckedTopLevelStatement.subroutine stmts
    let statements (UncheckedProgram.Program stmts) = List.choose UncheckedTopLevelStatement.statement stmts

    /// Returns all calls to subroutines that are made in the program.
    let calls program =
        let stmts = statements program
        let directCalls = List.choose UncheckedStatement.call stmts
        let subroutineStatements =
            subroutines program
            |> List.collect UncheckedTopLevelStatement.subroutineStatements
            |> List.choose UncheckedStatement.call

        directCalls @ subroutineStatements

