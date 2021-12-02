module tlang.Console

open System
open System.Diagnostics

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
    printfn "    build <name>     Build the program with the specified name."
    printfn "      OPTIONS:"
    printfn "        -r:            Run the program after compiling it."
    printfn "    clean <name>     Clean the output for the program with the specified name."
    printfn "      OPTIONS:"
    printfn "        -e:            Also cleanup the compiled executable."
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
