module tlang.Parser

open System
open tlang.Lexer
open tlang.Project

/// Parses a project from a list of tokens.
/// The parsing functions in this parser can fail in two ways:
/// Either they return an Option, in which case there is a chance that another parser may be used instead, or they
/// directly return the parsed value, and failure means that parsing failed. The parsing function will then print the
/// error and exit the program. Note that a parser that returns an Option can still fail if it has decided that what it
/// is parsing is indeed what it is supposed to be, but the input later down is not correct.
let parseProject (input: List<Token>) : UncheckedProject =
    let mutable index = 0
    let mutable curIndent = 0

    /// Returns the next token in the input and advances the index.
    let nextToken () =
        let c = input.[index]
        index <- index + 1
        c

    /// Peeks at the next token in the input.
    let peekToken () = input.[index]

    /// Performs a check on a token, and if it fails displays a message that parsing the entity with the provided label
    /// failed, and exits with exit code 1.
    let checkNext (f: Token -> bool) label =
        let t = nextToken ()
        let msg =
            sprintf "%s: Error parsing %s, unexpected %s."
                (Range.toString t.Range) label (TokenData.toString t.Data)
        Console.testCondition (f t) msg

    /// Consumes the next token, if the check on the token returns Some. If the check returns None, displays an error
    /// message that parsing the entity with the provide label failed is displayed, and exits with exit code 1.
    let consumeNext (f: Token -> Option<'a>) label =
        let t = nextToken ()
        match f t with
        | Some v -> v
        | None ->
            let msg =
                sprintf "%s: Error parsing %s, unexpected %s."
                    (Range.toString t.Range) label (TokenData.toString t.Data)
            Console.testCondition false msg
            failwith "unreachable"

    /// Consumes an end of line token.
    let consumeEndOfLine label = checkNext (fun t -> t.Data = EndOfLineToken) label

    /// Consumes one or more end of line tokens.
    let consumeEndOfLines label =
        consumeEndOfLine label
        while (peekToken ()).Data = EndOfLineToken do consumeEndOfLine label

    /// Parses a project type.
    let parseProjectType () =
        checkNext (fun t -> t.Data = KeywordToken "Executable") "project type"
        checkNext (fun t -> t.Data = SymbolToken ":") "project type"
        let name = consumeNext (fun t -> TokenData.getIdentifier t.Data) "project name"
        consumeEndOfLine "project type"
        Executable name

    /// Parses a separator between the project type definition and the program.
    let parseSeparator () =
        checkNext (fun t -> t.Data = SeparatorToken) "separator"
        consumeEndOfLine "separator"

    let parseTopLevelStatement () =
        failwith "TopLevelStatement parsing is not yet implemented."

    /// Parses a program.
    let parseProgram () =
        let mutable stmts = []
        while (peekToken ()).Data <> EndOfInputToken do
            stmts <- parseTopLevelStatement () :: stmts
            consumeEndOfLines "program"

        UncheckedProgram.Program <| List.rev stmts


    let pType = parseProjectType ()
    parseSeparator ()
    let program = parseProgram ()
    checkNext (fun t -> t.Data = EndOfInputToken) "project"
    { Type = pType ; Program = program }
