module tlang.Program

open System
open System.Diagnostics
open Console
open Compiler

[<EntryPoint>]
let main argv =
    let compilerName = AppDomain.CurrentDomain.FriendlyName
    let args = List.ofArray argv
    testConditionWithUsageError compilerName (List.length args >= 1) "Not enough arguments provided."

    let cmd = args.[0]

    match cmd with
    | "build" ->
        testConditionWithUsageError compilerName (List.length args >= 2) "Missing build target."
        let target = args.[1]
        let remaining = args.[2..]

        handleError "Error while compiling." (fun _ -> compile target)

        if List.contains "-r" remaining then
            handleError "Failed to run compiled program." (fun _ -> ignore <| Process.Start ($"./{target}"))

    | "clean" ->
        testConditionWithUsageError compilerName (List.length args >= 2) "Missing clean target."
        let target = args.[1]
        let remaining = args.[2..]
        let includeExe = List.contains "-e" remaining

        handleError "Error while cleaning up." (fun _ -> cleanup target includeExe)

    | _ ->
        printErr <| sprintf "Unknown command: '%s'." cmd
        printUsage compilerName

    0
