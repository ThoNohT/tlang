module tlang.Project

////////// Model //////////

type StringLiteral = StringLiteral of string

/// The name of a subroutine.
type SubroutineName = SubroutineName of string

/// A variable.
type Variable = Variable of string

/// A statement that can happen either on top level or in a subroutine.
type Statement =
    // Print a string to stdout.
    | PrintStr of StringLiteral
    // Print the int64 value in a variable.
    | PrintVar of Variable
    // Call a subroutine.
    | Call of SubroutineName
    // Assignment of an int64 to a variable.
    | Assignment of Variable * int64

/// A statement that can only appear at the top level of a program.
type TopLevelStatement =
    // Defines a new subroutine.
    | Subroutine of SubroutineName * List<Statement>
    // A regular statement.
    | Stmt of Statement

type Program = Program of List<TopLevelStatement>

/// The different types of projects that can be defined.
type ProjectType =
    /// An executable gets compiled into an executable file and cannot be referenced.
    /// The parameter is the name of the executable.
    | Executable of string

/// A complete project parsed from a file.
type Project = { Type: ProjectType ; Program: Program }


////////// Checked Model //////////

type IndexedStringLiteral = IndexedStringLiteral of int * string

type OffsetVariable = OffsetVariable of int * string

type CheckedStatement =
    | PrintStr of IndexedStringLiteral
    | PrintVar of OffsetVariable
    | Call of SubroutineName
    | Assignment of OffsetVariable * int64

type CheckedTopLevelStatement =
    | Subroutine of SubroutineName * List<CheckedStatement>
    | Stmt of CheckedStatement

type CheckedProgram = {
    Stmts: List<CheckedTopLevelStatement>
    Strings: Map<string, int>
    Variables: Map<string, int>
}

type CheckedProject = { Type: ProjectType ; Program: CheckedProgram }


////////// Parser //////////

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

/// Parses zero or more whitespace followed by the end of a line.
let trailingWhitespace = ~~(skipPrev (star wsNoEol) eol) |> Parser.setLabel "Trailing whitespace"

/// Takes a parser and accepts zero or more whitespaces after it.
let followedByWs p = skipNext p (star wsNoEol)

/// The list of keywords, that cannot be used as identifiers.
let keywords = [ "executable" ; "print" ; "call" ; "let" ] |> Set.ofList

/// Parses an identifier.
let pIdentifier = stringOf2 alpha alphaNum "identifier"

/// Parses a specific keyword.
let pKeyword str =
    skipNext
        (lit str)
        (peek (neg "A keyword should be followed by something else than an alphanumeric character" alphaNum))
        |> Parser.setLabel (sprintf "Keyword %s" str)
        |> map ignore

/// Parses a character inside a string liter. Either an unescaped regular character, or an escaped special character.
let stringEscapedChar =
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


type Printable = PrintableStr of StringLiteral | PrintableVar of Variable

let statementParser =
    let pPrint =
        parser {
            do! followedByWs <| pKeyword "print"
            do! commit true

            let! printable = altc (map PrintableStr pStringLiteral) (map (Variable >> PrintableVar) pIdentifier)
            return match printable with
                   | PrintableStr value -> Statement.PrintStr value
                   | PrintableVar var -> Statement.PrintVar var
        }

    let pCall =
        parser {
            do! followedByWs <| pKeyword "call"
            do! commit true
            let! name = pIdentifier |> Parser.setLabel "call name"
            // TODO: Do we want recognition of keywords embedded this deeply in the parser or performed
            // in a later check?
            do! if Set.contains name keywords
                then fail <| sprintf "%s is a keyword and cannot be used as an identifier." name
                else succeed ()
            return Statement.Call <| SubroutineName name
        } |> Parser.setLabel "call"


    let pAssignment =
        parser {
            do! followedByWs <| pKeyword "let"
            do! commit true
            let! name = pIdentifier |> followedByWs |> Parser.setLabel "variable name"
            // TODO: Do we want recognition of keywords embedded this deeply in the parser or performed
            // in a later check?
            do! if Set.contains name keywords
                then fail <| sprintf "%s is a keyword and cannot be used as an identifier." name
                else succeed ()
            do! ~~(followedByWs <| litC '=')
            let! value = fullInt64
            return Statement.Assignment (Variable name, value)
        } |> Parser.setLabel "assignment"

    oneOfc [ pPrint ; pCall ; pAssignment ] |> Parser.setLabel "statement"

let topLevelStatementParser =
    let pSubroutine =
        parser {
            let! name = pIdentifier |> Parser.setLabel "subroutine name"
            let! _ = litC ':' |> Parser.setLabel "subroutine name"
            do! commit true
            // TODO: Do we want recognition of keywords embedded this deeply in the parser or performed
            // in a later check?
            do! if Set.contains name keywords
                then fail <| sprintf "%s is a keyword and cannot be used as an identifier." name
                else succeed ()
            do! trailingWhitespace
            let! stmts = plus (skipNext (indented 2 statementParser) trailingWhitespace)
            return TopLevelStatement.Subroutine (SubroutineName name, stmts)
        }


    let pStatement = skipNext statementParser trailingWhitespace |> map TopLevelStatement.Stmt

    altc pSubroutine pStatement |> Parser.setLabel "top level statement"

let programParser =
    let emptyLine = skipPrev (star wsNoEol) eol |> Parser.setLabel "emptyLine"
    let between = star emptyLine |> Parser.setLabel "between"
    separated1 between topLevelStatementParser |> map Program.Program |> Parser.setLabel "program"

let projectTypeParser =
    parser {
        let! _ = lit "Executable: "
        let! name = pIdentifier |> Parser.setLabel "project name"
        return Executable name
    }

/// A parser for a project. A project is defined as:
/// Line 1: The type,
/// Line 2: A separator of at least one '-',
/// The rest: The program
let projectParser =
    parser {
        let! typ = line projectTypeParser |> Parser.setLabel "project type"
        let! _ = plus (litC '-') |> line |> Parser.setLabel "separator"
        let! prog = programParser
        do! ~~ (star <| skipNext (star wsNoEol) eol ) |> Parser.setLabel "trailing white lines"
        do! eoi |> Parser.setLabel "End of project"

        return { Project.Type = typ ; Program = prog }
    }


////////// Operations //////////

module SubroutineName =
    let value (SubroutineName name) = name

module Statement =
    let call =
        function
        | ((Statement.Call _) as c) -> Some c
        | _ -> None

    /// Get the name of a subroutine referenced in a statement.
    let name =
        function
        | Statement.Call name -> Some name
        | _ -> None

module TopLevelStatement =
    let subroutine  =
        function
        | ((TopLevelStatement.Subroutine _) as s) -> Some s
        | _ -> None

    /// Returns all statements in a subroutine, or an empty list if it is not a subroutine.
    let subroutineStatements =
        function
        | (TopLevelStatement.Subroutine (_, stmts)) -> stmts
        | _ -> []

    let statement =
        function
        | TopLevelStatement.Stmt stmt -> Some stmt
        | _ -> None

    /// Get the name of a subroutine referenced in a statement.
    let name =
        function
        | TopLevelStatement.Subroutine (name, _) -> Some name
        | _ -> None

module CheckedTopLevelStatement =
    let subroutine  =
        function
        | ((CheckedTopLevelStatement.Subroutine _) as s) -> Some s
        | _ -> None

    let statement =
        function
        | CheckedTopLevelStatement.Stmt stmt -> Some stmt
        | _ -> None

module Program =
    let subroutines (Program stmts) = List.choose TopLevelStatement.subroutine stmts
    let statements (Program stmts) = List.choose TopLevelStatement.statement stmts

    /// Returns all calls to subroutines that are made in the program.
    let calls program =
        let stmts = statements program
        let directCalls = List.choose Statement.call stmts
        let subroutineStatements =
            subroutines program |> List.collect TopLevelStatement.subroutineStatements |> List.choose Statement.call

        directCalls @ subroutineStatements

module CheckedProgram =
    let subroutines prog = List.choose CheckedTopLevelStatement.subroutine prog.Stmts
    let statements prog = List.choose CheckedTopLevelStatement.statement prog.Stmts

