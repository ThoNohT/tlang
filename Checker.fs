module tlang.Checker

open tlang.Project

/// Issues that arise from checking a project.
type CheckIssue =
    /// An error that prevents compilation.
    | CheckError of string
    /// A warning that indicates a possible problem, but doesn't prevent compilation.
    | CheckWarning of string

module CheckIssue =
    /// Indicates whether an issue is an error that prevents compilation.
    let isError = function
    | CheckError _ -> true
    | _ -> false

    /// Converts an issue to a string to be shown to the user, including its severity.
    let toString = function
    | CheckError e -> sprintf "Error: %s"  e
    | CheckWarning w -> sprintf "Warning: %s" w


type StringIndexes = Map<string, int>

/// Returns the string index for the specified string and the updated set of string literals. If the string was defined
/// before, this index is returned to prevent allocating a new string.
let getStringIdx (strings: StringIndexes) str  =
    match Map.tryFind str strings with
    | Some idx -> (idx, strings)
    | None ->
        let idx = Map.count strings
        (idx, Map.add str idx strings)

type CheckResult =
    | Checked of CheckedProject * List<CheckIssue>
    | Failed of List<CheckIssue>

let private checkProgram (Program stmts) : CheckedProgram =
    let checkStmt strings : Statement -> CheckedStatement * Map<string, int> =
        function
        | Statement.PrintStr (StringLiteral str) ->
            let (idx, strings') = getStringIdx strings str
            CheckedStatement.PrintStr (IndexedStringLiteral (idx, str)), strings'
        | Statement.Call n -> Call n, strings

    let checkTopStmt strings =
        function
        | TopLevelStatement.Subroutine (n, stmts) ->
            let folder (stmts, strings) stmt =
                let (stmt', strings') = checkStmt strings stmt
                (stmt' :: stmts, strings')

            let (stmts', strings') = List.fold folder ([], strings) stmts
            (CheckedTopLevelStatement.Subroutine (n, List.rev stmts'), strings')
        | TopLevelStatement.Stmt stmt ->
            let stmt', strings' = checkStmt strings stmt
            (Stmt stmt', strings')

    let folder (stmts, strings) stmt =
        let (stmt', strings') = checkTopStmt strings stmt
        (stmt' :: stmts, strings')

    let (stmts', strings) = List.fold folder ([], Map.empty) stmts
    { CheckedProgram.Stmts = List.rev stmts' ; Strings = strings }


/// Checks a project for issues.
let check (project: Project) : CheckResult =
    let prog = project.Program

    let callNames = Program.calls prog |> List.choose Statement.name |> Set.ofList
    let subNames = Program.subroutines prog |> List.choose TopLevelStatement.name |> Set.ofList

    let undefinedCalls = Set.difference callNames subNames
    let unusedSubs = Set.difference subNames callNames

    // TODO: A subroutine can be called from itself, or another uncalled subroutine.
    let callErrors =
        undefinedCalls
        |> Set.toList
        |> List.map (CheckError << sprintf "Call to undefined subroutine '%s'." << SubroutineName.value)

    let subWarnings =
        unusedSubs
        |> Set.toList
        |> List.map (CheckWarning << sprintf "Unused subroutine '%s'." << SubroutineName.value)

    if List.isEmpty callErrors then
        let checkedProgram = checkProgram project.Program
        Checked ({ Program = checkedProgram ; CheckedProject.Type = project.Type } , subWarnings)
    else
        Failed <| subWarnings @ callErrors
