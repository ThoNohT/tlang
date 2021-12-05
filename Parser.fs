module tlang.Parser

open tlang.Lexer
open tlang.Project

/// Parses a project from a list of tokens.
/// The parsing functions in this parser can fail in two ways:
/// Either they return an Option, in which case there is a chance that another parser may be used instead, or they
/// directly return the parsed value, and failure means that parsing failed. Any parser that returns None is responsible
/// for ensuring that the state is reset to before it performed any actions. In case of failure, the parsing function
/// will print the error and exit the program. Note that a parser that returns an Option can still fail if it has
/// decided that what it is parsing is indeed what it is supposed to be, but the input later down is not correct.
let parseProject (input: List<Token>) : UncheckedProject =
    let mutable index = 0

    // Returns the next token in the input and advances the index.
    let nextToken () =
        let c = input.[index]
        index <- index + 1
        c

    // Resets the parser state to the specified value for the index.
    let resetState newIndex =
        index <- newIndex

    // Peeks at the next token in the input.
    let peekToken () = input.[index]

    // Consumes the next token, and returns the result of th check on the token.
    let tryCheckNext (f: Token ->  bool) =
        let t = nextToken ()
        f t

    // Consumes the next token, and performs a check on a token, and if it fails displays a message that parsing the
    // entity with the provided label failed, and exits with exit code 1.
    let checkNext (f: Token -> bool) label =
        let t = nextToken ()
        let msg =
            sprintf "%s: Error parsing %s, unexpected %s."
                (Range.toString t.Range) label (TokenData.toString true t.Data)
        Console.testCondition (f t) msg

    // Consumes the next token, returns the result of the check on the token.
    // If the check returns None, the token is still consumed.
    let tryConsumeNext (f: Token -> Option<'a>) =
        let t = nextToken ()
        f t

    // Consumes the next token, if the check on the token returns Some. If the check returns None, displays an error
    // message that parsing the entity with the provide label failed is displayed, and exits with exit code 1.
    let consumeNext (f: Token -> Option<'a>) label =
        let t = nextToken ()
        match f t with
        | Some v -> v
        | None ->
            let msg =
                sprintf "%s: Error parsing %s, unexpected %s."
                    (Range.toString t.Range) label (TokenData.toString true t.Data)
            Console.testCondition false msg
            failwith "unreachable"

    // Consumes an end of line token.
    let consumeEndOfLine label = checkNext (fun t -> t.Data = EndOfLineToken) label

    // Consumes one or more end of line tokens. All but the first end of line token may be prefixed by indentation
    // tokens of any level
    let consumeEndOfLines label =
        consumeEndOfLine label
        while (peekToken ()).Data = EndOfLineToken do consumeEndOfLine label

    // Parses a project type.
    let parseProjectType () =
        checkNext (fun t -> t.Data = KeywordToken "Executable") "project type"
        checkNext (fun t -> t.Data = SymbolToken ":") "project type"
        let name = consumeNext (fun t -> TokenData.tryGetIdentifier t.Data) "project name"
        consumeEndOfLine "project type"
        Executable name

    // Parses a separator between the project type definition and the program.
    let parseSeparator () =
        checkNext (fun t -> t.Data = SeparatorToken) "separator"
        consumeEndOfLine "separator"

    // Checks that the next token is indented to the specified indent level.
    // For an indent level of 0, it is checked that the next token is not an indent token, and no tokens are consumed.
    // If the next token is the desired indent token (greater than 0), it is consumed, otherwise nothing is consumed.
    let checkIndent = function
        | 0 ->
            let peek = peekToken ()
            match peek.Data with
            | IndentationToken _ -> false
            | _ -> true
        | indent ->
            let oldIndex = index
            if not <| tryCheckNext (fun t -> t.Data = IndentationToken indent) then
                resetState oldIndex
                false
            else true

    // Tries to parse a print statement, wil lreturn None if the first keyword is not mached and fail if anything later
    // fails.
    let tryParsePrint () =
        let oldIndex = index
        if not <| tryCheckNext (fun t -> t.Data = KeywordToken "print") then
            resetState oldIndex
            None
        else
            let t = nextToken ()
            let strLit =  TokenData.tryGetStringLiteral t.Data
            let varName = TokenData.tryGetIdentifier t.Data

            match strLit, varName with
            | Some sl , _ -> Some <| UncheckedStatement.PrintStr (UncheckedStringLiteral.StringLiteral sl)
            | _, Some vn -> Some <| UncheckedStatement.PrintVar (UncheckedVariable.Variable vn)
            | _ ->
                let msg =
                    sprintf "Error parsing a print statement, expected a %s or %s, but got %s."
                        (TokenData.toString false (StringLiteralToken ""))
                        (TokenData.toString false (IdentifierToken ""))
                        (TokenData.toString true (t.Data))
                Console.testCondition false msg
                failwith "unreachable"

    // Tries to parse a call, will return None if the first keyword is not matched and fail if anything later fails.
    let tryParseCall () =
        let oldIndex = index
        if not <| tryCheckNext (fun t -> t.Data = KeywordToken "call") then
            resetState oldIndex
            None
        else
            let subName = consumeNext (fun t -> TokenData.tryGetIdentifier t.Data) "call subroutine name"
            Some <| UncheckedStatement.Call (SubroutineName subName)

    // Tries to parse an assignment, will return None if the first keyword is not matched and fail if anything later
    // fails.
    let tryParseAssignment () =
        let oldIndex = index
        if not <| tryCheckNext (fun t -> t.Data = KeywordToken "let") then
            resetState oldIndex
            None
        else
            let name = consumeNext (fun t -> TokenData.tryGetIdentifier t.Data) "assignment variable"
            checkNext (fun t -> t.Data = SymbolToken "=") "assignment"
            let value = consumeNext (fun t -> TokenData.tryGetNumber t.Data) "assignment value"
            Some <| UncheckedStatement.Assignment (UncheckedVariable.Variable name, value)

    // Tries to parse a statement. This parser first checks whether the next token has the correct indentation, then
    // applies one of the parsers for the specific statements. Consumes no tokens if the indentation is incorrect, or
    // no statement was parsed.
    let tryParseStatement indent =
        let indentIsCorrect = checkIndent indent
        if not indentIsCorrect then None
        else
            tryParsePrint ()
            |> Option.orElseWith tryParseCall
            |> Option.orElseWith tryParseAssignment


    // Tries to parse a subroutine. This parser will commit to parsing a statement after having parsed the subroutine
    // name followed by a ":".
    let tryParseSubroutine () =
        let oldIndex = index
        let name = tryConsumeNext (fun t -> TokenData.tryGetIdentifier t.Data)
        let hasColon = tryCheckNext (fun t -> t.Data = SymbolToken ":")
        match name, hasColon with
        | Some n, true ->
            consumeEndOfLines "subroutine"
            let mutable stmts = []
            let mutable stmtOpt = tryParseStatement 1
            while Option.isSome stmtOpt do
                match stmtOpt with
                | Some stmt ->
                    stmts <- stmt :: stmts
                    stmtOpt <- tryParseStatement 1
                    consumeEndOfLines "subroutine"
                | _ -> ()
            Some <| Subroutine (SubroutineName n, List.rev stmts)

        | _ ->
            resetState oldIndex
            None

    // Parses a top-level statement, which is either a subroutine, or a regular statement.
    let tryParseTopLevelStatement () =
        Option.orElseWith
            (fun _ -> Option.map UncheckedTopLevelStatement.Stmt <| tryParseStatement 0)
            (tryParseSubroutine ())

    // Parses a program.
    let parseProgram () =
        let mutable stmts = []
        let mutable stmtOpt = tryParseTopLevelStatement ()
        while Option.isSome stmtOpt do
            match stmtOpt with
            | Some stmt ->
                stmts <- stmt :: stmts
                consumeEndOfLines "program"
                stmtOpt <- tryParseTopLevelStatement ()
            | None -> ()

        UncheckedProgram.Program <| List.rev stmts

    let pType = parseProjectType ()
    parseSeparator ()
    let program = parseProgram ()
    checkNext (fun t -> t.Data = EndOfInputToken) "project"
    { Type = pType ; Program = program }
