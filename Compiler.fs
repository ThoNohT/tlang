module tlang.Compiler

open System
open System.IO
open tlang.Console
open tlang.Parser
open tlang.Project
open tlang.Checker

module Syscall =
    let idReg = "rax"
    let argRegs = Map.ofSeq [ 0, "rdi" ; 1, "rsi" ; 2, "rdx" ; 3, "r10" ; 4, "r8" ; 5, "r9" ]

/// Encodes a string so it can be included in assembly.
let asmEncodeString (str: string) =
    let step (first, prevInStr, acc) (c: char) =
        let nowInStr = int c >= 32 && int c <= 126
        let sep = not first && ((not nowInStr) || nowInStr && not prevInStr)

        let prefix = if sep then "," else ""
        let startQuote = if not prevInStr && nowInStr then "\"" else ""
        let endQuote = if prevInStr && not nowInStr then "\"" else ""
        let c_ = if nowInStr then sprintf "%c" c else sprintf " %i" (int c)

        (false, nowInStr, sprintf "%s%s%s%s%s" acc endQuote prefix startQuote c_)

    let _, inStr, r = List.fold step (true, false, "") (List.ofSeq str)
    if inStr then sprintf "%s\"" r else r


/// Writes a data declaration for a statement, given a writing function.
let writeDecl wl =
    function
    | Subroutine (name, value) -> wl <| sprintf "    txt_%s db %s" name (asmEncodeString (sprintf "%s%c" value '\n'))
    | _ -> ()


/// Writes a statement, given a writing function.
let writeStatement wl =
    function
    | Subroutine (name, value) ->
        wl <| sprintf "_%s:" name
        wl "    mov rax, 1"
        wl "    mov rdi, 1"
        wl <| sprintf "    mov rsi, txt_%s" name
        wl <| sprintf "    mov rdx, %i" (String.length value + 1)
        wl "    syscall"
        wl "    ret"
        wl ""
    | Call name ->
        wl <| sprintf "    call _%s" name

/// Writes the project to x86_64 linux assembly for Nasm.
let write_x86_64_LinuxNasm fileName (project: Project) =
    let writer = new StreamWriter (fileName, false)

    let wl (str: string) = writer.WriteLine str

    // Start of data section.
    wl "section .data"

    for decl in Program.subroutines project.Program do
        writeDecl wl decl

    wl ""

    // Start of program.
    wl "section .text"
    wl "    global _start"
    wl ""
    wl "_start:"
    wl ""
    wl "    ;  Entry point."

    // Program statements.
    for call in Program.calls project.Program do
        writeStatement wl call

    wl ""
    wl "    ; Exit call."
    wl "    mov rax, 60"
    wl "    mov rdi, 0"
    wl "    syscall"
    wl ""
    wl "    ; Subroutines."

    for sr in Program.usedSubroutines project.Program do
        printfn "Writing Subroutine %s" (Statement.name sr)
        writeStatement wl sr

    writer.Close ()

/// Parses an input file to a Project.
let parseProject inputFile =
    let parseResult = File.ReadAllText inputFile |> ParseState.prepareString |> Parser.runParser Project.parser

    match parseResult with
    | Failure (l, m, s) ->
        printErr <| ParseResult.showError l m s
        Environment.Exit 1
        failwith "unreachable"

    | Success (prog, _) -> prog

/// Compiles the project from the specified file.
/// Returns the name of the generated executable.
let compile inputFile =
    let project = parseProject inputFile
    let (Executable projectName) = project.Type

    /// Check for issues.
    let issues = check project
    if not <| List.isEmpty issues then printfn "Issues found:\n"
    for issue in issues do eprintfn "%s" (CheckIssue.toString issue)
    if List.exists CheckIssue.isError issues then Environment.Exit 1

    /// Determine file names.
    let asmFile = sprintf "%s.asm" projectName
    let oFile = sprintf "%s.o" projectName
    let exeFile = sprintf "%s" projectName

    /// Write nasm.
    printfn "Generating %s" asmFile
    write_x86_64_LinuxNasm (sprintf "%s.asm" projectName) project

    /// Compile nasm.
    runCmdEchoed [ "nasm" ; "-f" ; "elf64" ; "-o" ; oFile ; asmFile ]

    /// Link file.
    runCmdEchoed [ "ld" ; oFile ; "-o" ; exeFile ]

    exeFile

/// Cleans up files for the project from the specified file.
let cleanup inputFile includeExe =
    let project = parseProject inputFile
    let (Executable projectName) = project.Type

    printfn "Cleaning up files for %s" projectName

    let asmFile = sprintf "%s.asm" projectName

    if File.Exists asmFile then
        printfn "Removing %s" asmFile
        File.Delete asmFile

    let oFile = sprintf "%s.o" projectName

    if File.Exists oFile then
        printfn "Removing %s" oFile
        File.Delete oFile

    let exeFile = sprintf "%s" projectName

    if includeExe && File.Exists exeFile then
        printfn "Removing %s" exeFile
        File.Delete exeFile
