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

/// Checks a project for issues.
let check (project: Project) =
    let prog = project.Program

    let callNames = Program.calls prog |> List.map Statement.name |> Set.ofList
    let subNames = Program.subroutines prog |> List.map Statement.name |> Set.ofList

    let undefinedCalls = Set.difference callNames subNames
    let unusedSubs = Set.difference subNames callNames

    let callErrors =
        undefinedCalls |> Set.toList |> List.map (CheckError << sprintf "Call to undefined subroutine '%s'.")

    let subWarnings = unusedSubs |> Set.toList |> List.map (CheckWarning << sprintf "Unused subroutine '%s'.")

    subWarnings @ callErrors
