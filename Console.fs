module tlang.Console

open System
open System.Diagnostics

/// A flag that can be provided with the build command.
type BuildFlag =
    | Run
    | DumpLexerTokens
    | DumpUncheckedSyntaxTree
    | DumpCheckedSyntaxTree

module BuildFlag =
    let allFlags =
        Map.ofList
            [ "-r", Run
              "-dlt", DumpLexerTokens
              "-dsu", DumpUncheckedSyntaxTree
              "-dsc", DumpCheckedSyntaxTree
            ]

    let fromString str = Map.tryFind str allFlags

    let explain = function
        | Run -> "Run the program after compiling it."
        | DumpLexerTokens -> "Dump the tokens produced by the lexer and exit."
        | DumpUncheckedSyntaxTree -> "Dump the unchecked syntax tree prooduced by the parser and exit."
        | DumpCheckedSyntaxTree -> "Dump the checked syntax tree prooduced by the checker and exit."

    let accumulate args = args |> List.choose fromString |> Set.ofList

/// A flag that cna be provided with the clean command.
type CleanFlag =
    | IncludeExe

module CleanFlag =
    let allFlags = Map.ofList [ "-e", IncludeExe ]

    let fromString str = Map.tryFind str allFlags

    let explain = function
        | IncludeExe -> "Also cleanup the compiled executable."

    let accumulate args = args |> List.choose fromString |> Set.ofList

let printFlags flags explain =
    flags
    |> Map.toList
    |> List.map (fun (s, f) -> sprintf "        %s %s" ((sprintf "%s:"s).PadRight 14) (explain f))
    |> String.concat "\n"
    |> printfn "%s"



/// Prints an error to stdout in red.
let printErr (str: string) =
    Console.ForegroundColor <- ConsoleColor.Red
    eprintfn "%s" str
    Console.ResetColor ()

///  Runs a command, echoing the command before it is executed.
/// If the command runs a nonzero status code, the stderr output is printed and the application exits.
let runCmdEchoed (args: List<string>) =
    printfn "%s" <| String.concat " " args
    let output = Process.Start (ProcessStartInfo (args.[0], String.concat " " args.[1..]))

    output.WaitForExit ()
    let code = output.ExitCode

    if code <> 0 then
        printErr <| sprintf "Command exited with status %i" code
        printErr <| output.StandardError.ReadToEnd ()
        Environment.Exit 1
    else
        ()

/// Prints the usage string and exits the application with exit code 1.
let printUsage compilerName =
    printfn "Usage: %s <COMMAND> [OPTIONS]" compilerName
    printfn "  COMMAND:"
    printfn "    build <name>     Build the project with the specified name."
    printfn "      OPTIONS:"
    printFlags BuildFlag.allFlags BuildFlag.explain
    printfn "    clean <name>     Clean the output for the project with the specified name."
    printfn "      OPTIONS:"
    printFlags CleanFlag.allFlags CleanFlag.explain
    printfn "    test             Run internal unit tests."
    Environment.Exit 1

/// Checks a condition, and if it fails, displays the specified error, prints the usage string
/// and then exits with exit code 1.
let testConditionWithUsageError compilerName condition error =
    if not condition then
        printErr error
        printUsage compilerName
    else
        ()

/// Checks a condition, and if it fails, displays the specified error and then exits with exit code 1.
let testCondition condition error =
    if not condition then
        printErr error
        Environment.Exit 1
    else
        ()

/// Performs an action, if the action throws an exception, the exception
/// message is shown and the application exits with exit code 1.
let handleError failureMsg action =
    try
        action ()
    with
    | e ->
        printErr failureMsg
        printErr e.Message
        Environment.Exit 1
        failwith "unreachable"



