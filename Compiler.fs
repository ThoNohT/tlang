module tlang.Compiler

open System
open System.IO
open tlang.Console
open tlang.Parser

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

/// Writes the program to x86_64 linux assembly for Nasm.
let write_x86_64_LinuxNasm fileName (program: Program) =
    let writer = new StreamWriter (fileName, false)

    let wl (str: string) = writer.WriteLine str

    wl "section .data"
    wl <| sprintf "    text db %s" (asmEncodeString program.Value)
    wl ""
    wl "section .text"
    wl "    global _start"
    wl "_start:"
    wl "    mov rax, 1"
    wl "    mov rdi, 1"
    wl "    mov rsi, text"
    wl <| sprintf "    mov rdx, %i" (String.length program.Value)
    wl "    syscall"
    wl ""
    wl "    mov rax, 60"
    wl "    mov rdi, 0"
    wl "    syscall"

    writer.Close ()

/// Parses an input file to a Program.
let parseProgram inputFile =
    let parseResult = File.ReadAllText inputFile |> parse pProgram

    match parseResult with
    | Error e ->
        printErr <| sprintf "Parse error at %s" (Position.toString e.Pos)
        printErr <| sprintf "Error message: %s" e.Message
        Environment.Exit 1
        failwith "unreachable"

    | Ok prog -> prog

/// Compiles the program from the specified file.
/// Returns the name of the generated executable.
let compile inputFile =
    let program = parseProgram inputFile
    let (Executable programName) = program.Type

    let asmFile = sprintf "%s.asm" programName
    let oFile = sprintf "%s.o" programName
    let exeFile = sprintf "%s" programName

    printfn "Generating %s" asmFile
    write_x86_64_LinuxNasm (sprintf "%s.asm" programName) program

    runCmdEchoed [ "nasm" ; "-f" ; "elf64" ; "-o" ; oFile ; asmFile ]

    runCmdEchoed [ "ld" ; oFile ; "-o" ; exeFile ]

    exeFile

/// Cleans up files for the program from the specified file.
let cleanup inputFile includeExe =
    let program = parseProgram inputFile
    let (Executable programName) = program.Type

    printfn "Cleaning up files for %s" programName

    let asmFile = sprintf "%s.asm" programName

    if File.Exists asmFile then
        printfn "Removing %s" asmFile
        File.Delete asmFile

    let oFile = sprintf "%s.o" programName

    if File.Exists oFile then
        printfn "Removing %s" oFile
        File.Delete oFile

    let exeFile = sprintf "%s" programName

    if includeExe && File.Exists exeFile then
        printfn "Removing %s" exeFile
        File.Delete exeFile
