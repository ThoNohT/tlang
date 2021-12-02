module tlang.Lexer

/// A position for a token.
type Position = { Line: int ; Col: int }
type Range = { File: string ; StartLine: int ; StartCol: int ; EndLine: int ; EndCol: int }

module Range =
    let fromPositions file startPos endPos =
           { File = file
             StartLine = startPos.Line ; StartCol = startPos.Col
             EndLine = endPos.Line ; endCol = endPos.Col
           }

type TokenData =
    | IndentationToken of int
    | KeywordToken of string
    | IdentifierToken of string
    | SymbolToken of char
    | StringLiteralToken of string
    | NumberToken of int64
    | SeparatorToken
    | EndOfLineToken
    | EndOfInputToken

/// A token produced by the lexer.
type Token = {
    /// The range over which this token spans.
    Range: Range

    /// The data for the token
    Data: TokenData

    /// Indicates whether whitespace separates the token from the previous token.
    /// If a token follows an IndentationToken, then any whitespace that is part of the IndentationToken does not count
    /// as whitespace before the token.
    WhitespaceBefore: bool
    }

/// Lexes a full file into a list of tokens.
/// keywords: These words are identified as keywords, other words are considered identifiers.
/// filename: The name of the file to be lexed.
/// input: The contents of the file to be lexed.
let lexFile (keywords: Set<string>) (filename: string) (input: string): List<Token> =
    // Don't bother checking carriage returns. They don't affect the position, only the index may change,
    // but that is not exposed. The only way this could be an issue if there are carriage returns without line feeds,
    // and then I feel that strange results are justified.
    let sanitizedInput = input.Replace("\r", "")
    let mutable tokens = []
    let mutable spacesPerIndent = NotDetermined
    let mutable index = 0
    let mutable prevPos = { Line = 0 ; Col = 0 }
    let mutable pos = { Line = 0 ; Col = 0 }
    let mutable whitespaceBefore = false
    let mutable atEndOfInput = false

    // Custom version of Char.isWhitespace that excludes the newline character.
    let isWhitespace c = Char.isWhitespace c && c <> '\n'

    let curChar () = sanitizedInput.[index]

    // Advance the index, and the position of the lexer.
    let nextChar () =
        prevPos <- pos
        match curChar () with
        | '\n' -> pos <- { Line = pos.Line + 1 ; Col = 0 }
        | _ -> pos <- { pos | Col = pos.Col + 1 }
        index <- index + 1
        if index = sanitizedInput.Length then atEndOfInput <- true

    // Add a token to the list of tokens. This is done in reverse, so return the tokens in reverse.
    // Fills in the range from the provided old position, and the state's previous position (a token is added after the
    // first character that does not fit in it is encountered).
    // Fills in whitespaceBefore from the current state, and resets the state.
    let addToken oldPos tknData =
        let tkn = {
            Range = Range.fromPositions filename oldPos prevPos
            WhitespaceBefore = whitespaceBefore
            date = tnkData
        }
        whitespaceBefore <- false
        tokens <- tkn :: tokens

    // Can be used to check a predicate, and if it fails, exit with the specified error message,
    // including some location data.
    let checkLexerPredicate pred msg =
        let msg = sprintf "Lexer error at %s:%i:%i: %s" filename pos.Line pos.Col msg
        Console.testCondition pred msg

    // Can be used to execute an action that may result in an exception. If an exception is thrown, the exception
    // message, including some location data is printed and exit.
    let tryWithErrorReporting msg action =
        let msg = sprintf "Lexer error at %s:%i:%i: %s" filename pos.Line pos.Col msg
        Console.handleError msg action

    // Lexes an indentation token, consisting of spaces at the start of a line.
    // Only allows spaces as indentation. The lexer will fail when it encounters any other whitespace character,
    // or when an unexpected number of spaces is encountered.
    let lexIndent () =
        let oldPos = pos
        match spacesPerIndent with
        | None ->
            // The number of spaces per indent is not yet known, the total number of spaces encountered during the
            // first time leading whitespace occurs is taken as the number of spaces per indent.
            let oldIndex = index
            while not atEndOfInput && isWhitespace <| curChar () do
                nextChar ()
                checkLexerPredicate (c = ' ') "Leading whitespace may only consist of spaces."

            spacesPerIndent <- Some <| index - oldIndex
            addToken <| IndentationToken 1
        | Some spi ->
            // The number of spaces per indent is known, so take a multiple of this number of spaces, and return this
            // as the indent level.
            let oldIndex = index
            while not atEndOfInput && isWhitespace <| curChar () do
                nextChar ()
                checkLexerPredicate (c = ' ') "Leading whitespace may only consist of spaces."

            let nSpaces = index - oldIndex
            let extraSpace = nSpaces % spi
            let msg = sprintf
                        "%s %i, but got %i, which is %i too many or %i too few."
                        "Invalid number of leading spaces. Expected a multiple of"
                        spi nSpaces extraSpace (spi - extraSpace)
            checkLexerPredicate (extraSpace = 0) msg
            let identLevel = int <|
            addToken <| IndentationToken (nSpaces / sp)

    // Lexes a number, simply a token with a value as long as the characters are numeric.
    let lexNumber () =
        let oldPos = pos
        let oldIndex = index
        while not atEndOfInput && Char.isDigit <| curChar () do nextChar ()
        let nrStr = input.[oldIndex .. index - 1]
        let nr = tryWithErrorReporting "Failed to parse a number." (int64.Parse nrStr)
        addToken oldPos (NumberToken nr)

    // Lexes an identifier, consumes characters as long as they are alphanumeric.
    // If the resulting name is contained in the set of keywords, a keyword token is returned, otherwise an identifier
    // token.
    let lexIdentifier () =
        let oldPos = pos
        let oldIndex = index
        while not atEndOfInput && (Char.isDigit <| curChar () || Char.isAlpha curChar ()) do nextChar ()
        let name = input.[oldIndex .. index - 1]
        if Set.contains name keywords then addToken oldPos <| KeywordToken name
        else addToken oldPos <| IdentifierToken name

    // Lexes a string literal.
    let lexStringLiteral () =
        let mappedChars =
            Map.ofList [('\\', '\\') ; ('/', '/') ; ('b', '\b') ; ('f', '\f') ; ('n', '\n') ; ('r', '\r') ; ('t', '\t') ]

        let assertNotAtEnd () =
            checkLexerPredicate (not atEndOfInput) "Input ended before escaped character in string literal ended."

        let assertDigit c =
            checkLexerPredicate (Char.isDigit c) "Invalid escaped character."

        // An escaped character can either be one of the characters defined in mappedChars or a unicode value.
        let lexEscapedCharacter () =
            nextChar ()
            assertNotAtEnd ()
            let c = curChar ()
            match Map.tryGet c mappedChars with
            // A mapped character.
            | Some mc -> mc
            // If not, it has to be a unicode character.
            | None ->
                assertDigit c
                nextChar () ; assertNotAtEnd () ; let d2 = curChar () ; assertDigit d2
                nextChar () ; assertNotAtEnd () ; let d3 = curChar () ; assertDigit d3
                nextChar () ; assertNotAtEnd () ; let d4 = curChar () ; assertDigit d4
                char <| (int.Parse c) * 1000 + (int.Parse d2) * 100 + (int.Parse d3) * 10 + (int.Parse d4)

        let oldPos = pos

        // Go to the fist character that makes up the string.
        nextChar ()

        // Parse string characters until we reached the closing quote.
        let sb = StringBuilder ()
        while not atEndOfInput && curChar () <> '"' do
            let c = curChar ()
            if c = '\\' then
                sb.Append <| lexEscapedCharacter ()
            else
                sb.Append c

            // Move to the next character.
            nextChar ()

        // Consume the closing quote.
        checkLexerPredicate (not atEndOfInput) "Input ended before string literal ended."
        nextChar ()

        addToken oldPos <| StringLiteralToken (sb.ToString ())

    // Lexes any other symbol, continues as long as non word/digit/whitespace characters are encountered. Can also start
    // checking a comment or a separator.
    let lexSymbol () =
        let isSymbolChar c = not Char.isWhitespace c && not Char.isDigit c && not Char.isWord c
        let oldPos = pos
        let oldIndex = index
        nextChar ()

        let lexRegularSymbol () =
            while not atEndOfInput && isSymbolChar (curChar ()) do nextChar
            let symbolStr = sanitizedInput.[oldIndex .. index - 1]
            addToken oldPos SymbolToken symbolStr

        match curChar () with
        | '-' ->
            if not atEndOfInput && curChar () = '-' then
                while not atEndOfInput && curChar () = '-' do nextChar ()
                addToken oldPos SeparatorToken
            else lexRegularSymbol ()

        | '/' ->
            // Two /'s indicate a single-line comment. Just move on until a newline is consumed and throw everything away.
            if not atEndOfInput && curChar () = '/' then
                while not atEndOfInput && curChar () <> '\n' do nextChar ()
                // Move on to the next line.
                if not atEndOfInput then nextChar ()
            else lexRegularSymbol ()

        | c -> lexRegularSymbol ()

    while not atEndOfInput do
        match curChar () with
        | c when isWhitespace c && startOfLine -> lexIndent ()
        | c when isWhitespace c ->
            whitespaceBefore <- true
            while not atEndOfInput && isWhitespace <| curChar () do nextChar ()
        | c when Char.isDigit c -> lexNumber ()
        | c when Char.isAlpha c -> lexIdentifier ()
        | c when c = '"' -> lexStringLiteral ()
        | c when c '\n' ->
            addToken pos EndOfLineToken
            nextChar ()
        | c -> lexSymbol ()

    addToken EndOfInputToken
    Lidst.rev tokens

