module tlang.Compiler

open System
open System.IO
open System.Collections.Generic
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

let writeDecl wl (kvp: KeyValuePair<string, int>) =
        wl <| sprintf "txt_%i db %s" kvp.Value (asmEncodeString kvp.Key)

let writeStatement wl =
    function
    | PrintStr (IndexedStringLiteral (strId, strVal)) ->
        wl <| sprintf "    ; PrintStr %s" (asmEncodeString strVal)
        wl <| sprintf "    mov rsi, txt_%i" strId
        wl <| sprintf "    mov rdx, %i" (String.length strVal)
        wl "    call _PrintStr"
    | PrintVar (OffsetVariable (offset, name)) ->
        wl <| sprintf "    ; PrintVar %s" name
        wl <| "    mov rax, mem"
        wl <| sprintf "    add rax, %d" (offset * 8)
        wl <| sprintf "    mov rdi, [rax]"
        wl "    call _printInt64"
    | Call (SubroutineName name) ->
        wl <| sprintf "    call __%s" name
    | Assignment ((OffsetVariable (offset, name)), intVal) ->
        wl <| sprintf "    ; Assignment %s, %i" name intVal
        wl "    mov rax, mem"
        wl <| sprintf "    mov rbx, %d" intVal
        wl <| sprintf "    add rax, %d" (offset * 8)
        wl "    mov [rax], rbx"

/// Writes code to print an int64 to stdout. This code is generated from some c code.
let writePrintInt64 wl =
    wl "_printInt64:"
    wl "    sub rsp, 56"
    wl "    mov rcx, rdi"
    wl "    mov r10, rdi"
    wl "    mov r8d, 1"
    wl "    mov BYTE [rsp+32], 10"
    wl "    neg rcx"
    wl "    lea r9, [rsp+32]"
    wl "    cmovs rcx, rdi"
    wl "    mov rdi, -3689348814741910323"
    wl ".L2:"
    wl "    mov rax, rcx"
    wl "    mov rsi, r9"
    wl "    mul rdi"
    wl "    sub rsi, r8"
    wl "    shr rdx, 3"
    wl "    lea rax, [rdx+rdx*4]"
    wl "    add rax, rax"
    wl "    sub rcx, rax"
    wl "    mov rax, r8"
    wl "    add r8, 1"
    wl "    add ecx, 48"
    wl "    mov BYTE [rsi], cl"
    wl "    mov rcx, rdx"
    wl "    test rdx, rdx"
    wl "    jne .L2"
    wl "    test r10, r10"
    wl "    jns .L3"
    wl "    mov edx, 32"
    wl "    sub rdx, r8"
    wl "    lea r8, [rax+2]"
    wl "    mov BYTE [rsp+rdx], 45"
    wl ".L3:"
    wl "    mov eax, 33"
    wl "    mov rdx, r8"
    wl "    mov edi, 1"
    wl "    sub rax, r8"
    wl "    lea rsi, [rsp+rax]"
    wl "    mov rax, 1"
    wl "    syscall"
    wl "    add rsp, 56"
    wl "    ret"

/// Writes a subroutine, given a writing function.
/// These are handled in a separate function, since they need to be after all
/// regular  statements.
let writeSubroutine wl =
    function
    | Subroutine (SubroutineName name, stmts) ->
        wl <| sprintf "__%s:" name
        for stmt in stmts do
            writeStatement wl stmt
            wl ""
        wl "    ret"
    | _ -> ()

/// Writes the project to x86_64 linux assembly for Nasm.
let write_x86_64_LinuxNasm fileName (project: CheckedProject) =
    let writer = new StreamWriter (fileName, false)

    let wl (str: string) = writer.WriteLine str

    // Start of program.
    wl "section .text"
    wl "    global _start"
    wl ""
    wl "_start:"
    wl "    ;  Entry point."
    wl ""

    // Program statements.
    for stmt in CheckedProgram.statements project.Program do
        writeStatement wl stmt
        wl ""

    wl "    ; Exit call."
    wl "    mov rax, 60"
    wl "    mov rdi, 0"
    wl "    syscall"
    wl ""
    wl "_PrintStr:"
    wl "    ; PrintStr helper. Assumes rsi and rdx have been set before calling."
    wl "    mov rax, 1"
    wl "    mov rdi, 1"
    wl "    syscall"
    wl "    ret"
    wl ""
    writePrintInt64 wl
    wl ""
    wl "    ; Subroutines."
    wl ""

    for sr in CheckedProgram.subroutines project.Program do
        writeSubroutine wl sr
        wl ""

    // Start of data section.
    wl "section .data"

    for str in project.Program.Strings do
        writeDecl wl str

    // Start of memory section
    wl ""
    wl "segment .bss"
    if not <| Map.isEmpty project.Program.Variables then
        wl <| sprintf "mem: resb %d" (Map.count project.Program.Variables * 8) // 64 bit int = 8 bytes.

    writer.Close ()

/// Parses an input file to a Project.
let parseProject inputFile =
    let parseResult = File.ReadAllText inputFile |> ParseState.prepareString |> projectParser.Run

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
    match check project with
    | Failed issues ->
        printfn "Issues found:\n"
        for issue in issues do eprintfn "%s" (CheckIssue.toString issue)
        Environment.Exit 1
        failwith "Unreachable"

    | Checked (project', warnings) ->
        if not <| List.isEmpty warnings then printfn "Issues found:\n"
        for warning in warnings do eprintfn "%s" (CheckIssue.toString warning)

        /// Determine file names.
        let asmFile = sprintf "%s.asm" projectName
        let oFile = sprintf "%s.o" projectName
        let exeFile = sprintf "%s" projectName

        /// Write nasm.
        printfn "Generating %s" asmFile
        write_x86_64_LinuxNasm (sprintf "%s.asm" projectName) project'

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
