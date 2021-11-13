module tlang.Project

open tlang.Parser

type Statement =
    | Subroutine of string * string
    | Call of string

module Statement =
    /// A parser for a statement.
    let parser =
        /// A parser for a subroutine. Must be the name of the subroutine, followed by a colon.
        /// Then on the lines below, indented by 2 characters, the text to display for this routine.
        let pSubroutine =
            parser {
                let! name = pIdentifier |> mapError (fun e -> sprintf "Error parsing subroutine: %s" e)
                let! _ = litC ':'
                let! _ = star space
                do! eol
                let! str = plus <| indented pFullLine
                return Subroutine (name, String.concat "\n" str)
            }
            |> mapError (fun e -> sprintf "Subroutine parser failed: %s" e)


        /// A parser for a call. Must be the name of the subroutine, as the only thing on the line (excluding trailing space).
        let pCall =
            parser {
                let! name = pIdentifier |> mapError (fun e -> sprintf "Error parsing call: %s" e)
                let! _ = star space
                do! eol
                return Call name
            }
            |> mapError (fun e -> sprintf "Call parser failed: %s" e)

        alt pSubroutine pCall

    let subroutine =
        function
        | ((Subroutine _) as s) -> Some s
        | _ -> None

    let call =
        function
        | ((Call _) as c) -> Some c
        | _ -> None

    let name =
        function
        | Subroutine (name, _) -> name
        | Call name -> name

    let value =
        function
        | (Subroutine (_, value)) -> value
        | _ -> ""


type Program = Program of List<Statement>

module Program =
    let subroutines (Program stmts) = List.choose Statement.subroutine stmts
    let calls (Program stmts) = List.choose Statement.call stmts
    let statements (Program stmts) = stmts

    /// Returns all subroutines that are actually called.
    let usedSubroutines program =
        let callNames = calls program |> List.map Statement.name |> Set.ofList
        subroutines program |> List.filter (fun stmt -> Set.contains (Statement.name stmt) callNames)

    let parser =
        let emptyLine = skipPrev (star (check ((<>) '\n') ws)) (litC '\n') |> map ignore
        let between = star emptyLine
        map Program <| separated between Statement.parser


/// The different types of projects that can be defined.
type ProjectType =
    /// An executable gets compiled into an executable file and cannot be referenced.
    /// The parameter is the name of the executable.
    | Executable of string


module ProjectType =
    let parser =
        parser {
            let! _ = lit "Executable: "
            let! first = alpha |> mapError (fun _ -> "Expected name of project to start with a letter.")
            let! rest = stringOf_ alphaNum
            return Executable (sprintf "%c%s" first rest)
        }


/// A complete project parsed from a file.
type Project = { Type: ProjectType ; Program: Program }

module Project =
    /// A parser for a project. A project is defined as:
    /// Line 1: The type,
    /// Line 2: A separator of at least one '-',
    /// The rest: The program
    let parser =
        parser {
            let! typ = line ProjectType.parser |> mapError (fun e -> sprintf "Unable to parse project type: %s" e)
            let! _ = plus (litC '-') |> line |> mapError (fun _ -> "Expected a separator of a line of '-' characters.")
            let! prog = Program.parser
            do! ~~ (star <| ignoreAll [ ~~ (star wsNoEol) ; ~~ eolNoEoi ])
            do! ignoreAll [ ~~ (star wsNoEol) ; ~~ eoi ]

            return { Type = typ ; Program = prog }
        }
