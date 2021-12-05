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
    | Checked of Project * List<CheckIssue>
    | Failed of List<CheckIssue>

let private checkProgram (UProgram stmts) : Program =
    let checkStmt strings variables : UncheckedStatement -> Statement * StringIndexes * VariableOffsets =
        function
        | UPrintStr (UStringLiteral str) ->
            let (idx, strings') = getStringIdx strings str
            Statement.PrintStr (StringLiteral.StringLiteral (idx, str)), strings', variables
        | UPrintVar (UVariable n) ->
            let (offset, variables') = getVariableOffset variables n
            Statement.PrintVar (Variable.Variable (offset, n)), strings, variables'
        | UCall n -> Statement.Call n, strings, variables
        | UAssignment (UVariable n, value) ->
            let (offset, variables') = getVariableOffset variables n
            Statement.Assignment (Variable.Variable (offset, n), value), strings, variables'

    let checkTopStmt strings variables =
        function
        | USubroutine (n, stmts) ->
            let folder (stmts, strings, variables) stmt =
                let stmt', strings', variables' = checkStmt strings variables stmt
                (stmt' :: stmts, strings', variables')

            let stmts', strings', variables' = List.fold folder ([], strings, variables) stmts
            (TopLevelStatement.Subroutine (n, List.rev stmts'), strings', variables')
        | UStmt stmt ->
            let stmt', strings', variables' = checkStmt strings variables stmt
            TopLevelStatement.Stmt stmt', strings', variables'

    let folder (stmts, strings, variables) stmt =
        let stmt', strings', variables' = checkTopStmt strings variables stmt
        stmt' :: stmts, strings', variables'

    let (stmts', strings, variables) = List.fold folder ([], Map.empty, Map.empty) stmts
    { Program.Stmts = List.rev stmts' ; Strings = strings ; Variables = variables }

/// Determine the names of all unused subroutines. Starts by marking all subroutines as unused, then collects the
/// subroutines that are directly called, and marks them as used. Every subroutine that is used yields more statements
/// that can be checked that indirectly use other subroutines. Stop when there are no more statements to check.
let unusedSubs program =
    let subroutines = UncheckedProgram.subroutines program
    let mutable unusedSoFar = subroutines |> List.choose UncheckedTopLevelStatement.name |> Set.ofList

    let mutable stmtsToCheck = UncheckedProgram.statements program
    let mutable cnt = true
    while cnt do
        cnt <-
            match stmtsToCheck with
            | [] -> false
            | x :: xs ->
                match UncheckedStatement.call x with
                | Some (UCall n) when Set.contains n unusedSoFar ->
                    unusedSoFar <- Set.remove n unusedSoFar
                    let newStmts =
                        match List.find (fun s -> UncheckedTopLevelStatement.name s = Some n) subroutines with
                        | (USubroutine (_, subStmts)) -> subStmts
                        | _ -> []

                    stmtsToCheck <- xs @ newStmts
                    true

                | _ ->
                    stmtsToCheck <- xs
                    true

    unusedSoFar

/// Checks a project for issues.
let check (project: UncheckedProject) : CheckResult =
    let prog = project.Program

    let callNames = UncheckedProgram.calls prog |> List.choose UncheckedStatement.name |> Set.ofList
    let subNames = UncheckedProgram.subroutines prog |> List.choose UncheckedTopLevelStatement.name |> Set.ofList
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
        Checked ({ Program = checkedProgram ; Project.Type = project.Type } , subWarnings)
    else
        Failed <| subWarnings @ callErrors
