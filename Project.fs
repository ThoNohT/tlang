module tlang.Project

////////// Model //////////

/// The name of a subroutine.
type SubroutineName = SubroutineName of string

/// A string literal, including its index in the list of declared string literals.
type StringLiteral = StringLiteral of int * string

/// A variable, including its offset in memory.
type Variable = Variable of int * string

type Statement =
    // Print a string to stdout.
    | PrintStr of StringLiteral
    // Print the int64 value in a variable.
    | PrintVar of Variable
    // Call a subroutine.
    | Call of SubroutineName
    // Assignment of an int64 to a variable.
    | Assignment of Variable * int64

/// A statement that can happen either on top level or in a subroutine.
type TopLevelStatement =
    // Defines a new subroutine.
    | Subroutine of SubroutineName * List<Statement>
    // A regular statement.
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

/// A complete project parsed from a file.
type Project = { Type: ProjectType ; Program: Program }


////////// UncheckedModel //////////

type UncheckedStringLiteral = UStringLiteral of string

type UncheckedVariable = UVariable of string

type UncheckedStatement =
    | UPrintStr of UncheckedStringLiteral
    | UPrintVar of UncheckedVariable
    | UCall of SubroutineName
    | UAssignment of UncheckedVariable * int64

type UncheckedTopLevelStatement =
    | USubroutine of SubroutineName * List<UncheckedStatement>
    | UStmt of UncheckedStatement

type UncheckedProgram = UProgram of List<UncheckedTopLevelStatement>

type UncheckedProject = { Type: ProjectType ; Program: UncheckedProgram }


////////// Operations Model //////////

module SubroutineName =
    let value (SubroutineName name) = name

module TopLevelStatement =
    let subroutine  =
        function
        | ((Subroutine _) as s) -> Some s
        | _ -> None

    let statement =
        function
        |Stmt stmt -> Some stmt
        | _ -> None

module Program =
    let subroutines prog = List.choose TopLevelStatement.subroutine prog.Stmts
    let statements prog = List.choose TopLevelStatement.statement prog.Stmts


///////// Operations UncheckedModel /////////

module UncheckedStatement =
    let call =
        function
        | ((UCall _) as c) -> Some c
        | _ -> None

    /// Get the name of a subroutine referenced in a statement.
    let name =
        function
        | UCall name -> Some name
        | _ -> None

module UncheckedTopLevelStatement =
    let subroutine  =
        function
        | ((USubroutine _) as s) -> Some s
        | _ -> None

    let statement =
        function
        | UStmt stmt -> Some stmt
        | _ -> None

    /// Returns all statements in a subroutine, or an empty list if it is not a subroutine.
    let subroutineStatements =
        function
        | (USubroutine (_, stmts)) -> stmts
        | _ -> []

    /// Get the name of a subroutine referenced in a statement.
    let name =
        function
        | USubroutine (name, _) -> Some name
        | _ -> None

module UncheckedProgram =
    let subroutines (UProgram stmts) = List.choose UncheckedTopLevelStatement.subroutine stmts
    let statements (UProgram stmts) = List.choose UncheckedTopLevelStatement.statement stmts

    /// Returns all calls to subroutines that are made in the program.
    let calls program =
        let stmts = statements program
        let directCalls = List.choose UncheckedStatement.call stmts
        let subroutineStatements =
            subroutines program
            |> List.collect UncheckedTopLevelStatement.subroutineStatements
            |> List.choose UncheckedStatement.call

        directCalls @ subroutineStatements

