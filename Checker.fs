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
type VariableOffsets = Map<string, int>

/// Returns the string index for the specified string and the updated set of string literals. If the string was defined
/// before, this index is returned to prevent allocating a new string.
let getStringIdx (strings: StringIndexes) str =
    match Map.tryFind str strings with
    | Some idx -> (idx, strings)
    | None ->
        let idx = Map.count strings
        (idx, Map.add str idx strings)

/// Returns the offset for the specified variable name and the updated set of variable offsets. If the variable was
/// defined before, this offset is returned.
// TODO: Probably prevent assigning to the same variable multiple times.
let getVariableOffset (variables: VariableOffsets) name =
    match Map.tryFind name variables with
    | Some idx -> (idx, variables)
    | None ->
        let idx = Map.count variables
        (idx, Map.add name idx variables)

type CheckResult =
    | Checked of CheckedProject * List<CheckIssue>
    | Failed of List<CheckIssue>

let private checkProgram (Program stmts) : CheckedProgram =
    let checkStmt strings variables : Statement -> CheckedStatement * StringIndexes * VariableOffsets =
        function
        | Statement.PrintStr (StringLiteral str) ->
            let (idx, strings') = getStringIdx strings str
            CheckedStatement.PrintStr (IndexedStringLiteral (idx, str)), strings', variables
        | Statement.Call n -> Call n, strings, variables
        | Statement.Assignment ((Variable n), value) ->
            let (offset, variables') = getVariableOffset variables n
            CheckedStatement.Assignment (OffsetVariable (offset, n), value), strings, variables'

    let checkTopStmt strings variables =
        function
        | TopLevelStatement.Subroutine (n, stmts) ->
            let folder (stmts, strings, variables) stmt =
                let stmt', strings', variables' = checkStmt strings variables stmt
                (stmt' :: stmts, strings', variables')

            let stmts', strings', variables' = List.fold folder ([], strings, variables) stmts
            (CheckedTopLevelStatement.Subroutine (n, List.rev stmts'), strings', variables')
        | TopLevelStatement.Stmt stmt ->
            let stmt', strings', variables' = checkStmt strings variables stmt
            Stmt stmt', strings', variables'

    let folder (stmts, strings, variables) stmt =
        let stmt', strings', variables' = checkTopStmt strings variables stmt
        stmt' :: stmts, strings', variables'

    let (stmts', strings, variables) = List.fold folder ([], Map.empty, Map.empty) stmts
    { CheckedProgram.Stmts = List.rev stmts' ; Strings = strings ; Variables = variables }

/// Determine the names of all unused subroutines. Starts by marking all subroutines as unused, then collects the
/// subroutines that are directly called, and marks them as used. Every subroutine that is used yields more statements
/// that can be checked that indirectly use other subroutines. Stop when there are no more statements to check.
let unusedSubs program =
    let subroutines = Program.subroutines program
    let mutable unusedSoFar = subroutines |> List.choose TopLevelStatement.name |> Set.ofList

    let mutable stmtsToCheck = Program.statements program
    let mutable cnt = true
    while cnt do
        cnt <-
            match stmtsToCheck with
            | [] -> false
            | x :: xs ->
                match Statement.call x with
                | Some (Statement.Call n) when Set.contains n unusedSoFar ->
                    unusedSoFar <- Set.remove n unusedSoFar
                    let newStmts =
                        match List.find (fun s -> TopLevelStatement.name s = Some n) subroutines with
                        | (TopLevelStatement.Subroutine (_, subStmts)) -> subStmts
                        | _ -> []

                    stmtsToCheck <- xs @ newStmts
                    true

                | _ ->
                    stmtsToCheck <- xs
                    true

    unusedSoFar

/// Checks a project for issues.
let check (project: Project) : CheckResult =
    let prog = project.Program

    let callNames = Program.calls prog |> List.choose Statement.name |> Set.ofList
    let subNames = Program.subroutines prog |> List.choose TopLevelStatement.name |> Set.ofList
    let undefinedCalls = Set.difference callNames subNames

    let callErrors =
        undefinedCalls
        |> Set.toList
        |> List.map (CheckError << sprintf "Call to undefined subroutine '%s'." << SubroutineName.value)

    let subWarnings =
        unusedSubs prog
        |> Set.toList
        |> List.map (CheckWarning << sprintf "Unused subroutine '%s'." << SubroutineName.value)

    if List.isEmpty callErrors then
        let checkedProgram = checkProgram project.Program
        Checked ({ Program = checkedProgram ; CheckedProject.Type = project.Type } , subWarnings)
    else
        Failed <| subWarnings @ callErrors
