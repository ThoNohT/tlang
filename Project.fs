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

let trailingWhitespace = ~~(skipPrev (star space) eol) |> Parser.setLabel "Trailing whitespace"

/// Parses an identifier.
// TODO: How to not allow parsing a keyword?
let pIdentifier = stringOf2 alpha alphaNum "identifier"

/// Parses a specific keyword.
let pKeyword str =
    skipNext
        (lit str)
        (peek (neg "A keyword should end with something else than an alphanumeric character" alphaNum))
        |> Parser.setLabel (sprintf "Keyword %s" str)

type StringLiteral = StringLiteral of string

/// Parses a character inside a string liter. Either an unescaped regular character, or an escaped special character.
let stringEscapedChar =
    // TODO: Support for unicode
    let reservedChars = [ '"' ; '\\' ]
    let mappedChars =
        Map.ofList [('\\', '\\') ; ('/', '/') ; ('b', '\b') ; ('f', '\f') ; ('n', '\n') ; ('r', '\r') ; ('t', '\t') ]
    let unescapedChar = check (fun c -> not <| List.contains c reservedChars) pChar |> Parser.setLabel "unescaped character"
    let mappedChar = map (flip Map.find mappedChars) (anyOf (List.ofSeq <| Map.keys mappedChars))
    let unicodeChar = times 4 digit |> map (List.fold (fun s e -> s * 10 + e) 0) |> map char

    let escapedChar =
        parser {
            do! ~~ (litC '\\')
            do! commit true

            return! oneOf [ mappedChar ; unicodeChar ]
        } |> Parser.setLabel "escaped character"
    alt unescapedChar escapedChar

let pStringLiteral =
    parser {
     do! ~~ (litC '"')
     do! commit true
     let! value = stringOf stringEscapedChar
     do! ~~ (litC '"')
     return StringLiteral value
    }

/// The name of a subroutine.
type SubroutineName = SubroutineName of string

module SubroutineName =
    let value (SubroutineName name) = name

/// A statement that can happen either on top level or in a subroutine.
type Statement =
    // Print a string to stdout.
    PrintStr of StringLiteral

module Statement =
    let parser =
        parser {
            do! ~~ (pKeyword "print")
            do! commit true

            do! ~~ (plus ws)

            let! value = pStringLiteral
            return PrintStr value
        }

/// A statement that can only appear at the top level of a program.
type TopLevelStatement =
    // Defines a new subroutine.
    | Subroutine of SubroutineName * List<Statement>
    // Calls a subroutine.
    | Call of SubroutineName
    // A regular statement.
    | Stmt of Statement

module TopLevelStatement =
    /// A parser for a top-level statement.
    let parser =
        /// A parser for a subroutine. Must be the name of the subroutine, followed by a colon.
        /// Then on the lines below, indented by 2 characters, the text to display for this routine.
        let pSubroutine =
            parser {
                let! name = pIdentifier |> Parser.setLabel "subroutine name"
                let! _ = litC ':' |> Parser.setLabel "subroutine name"
                do! commit true
                do! trailingWhitespace
                let! stmts = plus (skipNext (indented 2 Statement.parser) trailingWhitespace)
                return Subroutine (SubroutineName name, stmts)
            }

        /// A parser for a call. Must be the name of the subroutine, as the only thing on the line (excluding trailing space).
        let pCall =
            parser {
                let! name = pIdentifier |> Parser.setLabel "call name"
                do! commit true
                do! trailingWhitespace
                return Call <| SubroutineName name
            } |> Parser.setLabel "pCall"

        let pStatement = skipNext Statement.parser trailingWhitespace |> map Stmt

        oneOfc [ pSubroutine ; pCall ; pStatement ] |> Parser.setLabel "statement"

    let subroutine =
        function
        | ((Subroutine _) as s) -> Some s
        | _ -> None

    let call =
        function
        | ((Call _) as c) -> Some c
        | _ -> None

    /// Get the name of a subroutine referenced in a statement.
    let name =
        function
        | Subroutine (name, _) -> Some name
        | Call name -> Some name
        | _ -> None


type Program = Program of List<TopLevelStatement>

module Program =
    let subroutines (Program stmts) = List.choose TopLevelStatement.subroutine stmts
    let calls (Program stmts) = List.choose TopLevelStatement.call stmts
    let statements (Program stmts) = stmts

    /// Returns all subroutines that are actually called.
    let usedSubroutines program =
        let callNames = calls program |> List.map TopLevelStatement.name |> Set.ofList
        subroutines program |> List.filter (fun stmt -> Set.contains (TopLevelStatement.name stmt) callNames)

    let parser =
        let emptyLine = skipPrev (star wsNoEol) eol |> Parser.setLabel "emptyLine"
        let between = star emptyLine |> Parser.setLabel "between"
        separated1 between TopLevelStatement.parser |> map Program |> Parser.setLabel "program"


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
            do! ~~ (star <| skipNext (star wsNoEol) eol ) |> Parser.setLabel "trailing white lines"
            do! eoi |> Parser.setLabel "End of project"

            return { Type = typ ; Program = prog }
        }
