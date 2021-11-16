module tlang.Project

open tlang.Parser

/// A parser that parses a full line, and also consumes the newline.
let pFullLine = skipNext (takeWhile ((<>) '\n')) (litC '\n')

/// Takes a parser, but expects n spaces before it.
let indented n p =
    parser {
        let! _ = times n space
        return! p
    }
    |> Parser.setLabel (sprintf "%i indented %s" n p.Label)

let trailingWhitespace = ~~(skipPrev (star space) eol)

/// Parses an identifier.
let pIdentifier = stringOf2 alpha alphaNum "identifier"

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
                let! name = pIdentifier |> Parser.setLabel "subroutine name"
                let! _ = litC ':'
                do! trailingWhitespace
                let! str = plus (indented 2 pFullLine) |> Parser.setLabel "subroutine contents"
                return Subroutine (name, String.concat "\n" str)
            }

        /// A parser for a call. Must be the name of the subroutine, as the only thing on the line (excluding trailing space).
        let pCall =
            parser {
                let! name = pIdentifier |> Parser.setLabel "call name"
                do! trailingWhitespace
                return Call name
            }

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
        let emptyLine = skipPrev (star wsNoEol) eol
        let between = star emptyLine
        separated between Statement.parser |> map Program |> Parser.setLabel "program"


/// The different types of projects that can be defined.
type ProjectType =
    /// An executable gets compiled into an executable file and cannot be referenced.
    /// The parameter is the name of the executable.
    | Executable of string


module ProjectType =
    let parser =
        parser {
            let! _ = lit "Executable: "
            let! name = pIdentifier |> Parser.setLabel "project name"
            return Executable name
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
            let! typ = line ProjectType.parser |> Parser.setLabel "project type"
            let! _ = plus (litC '-') |> line |> Parser.setLabel "separator"
            let! prog = Program.parser
            do! ~~ (star <| skipNext (star wsNoEol) eol )
            do! eoi

            return { Type = typ ; Program = prog }
        }
