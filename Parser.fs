module tlang.Parser

open System

// ----- Helper functions -----

/// Converts a list of characters to a string.
let private listToStr = Array.ofList >> (fun a -> new string (a))

/// A position in the input.
type Position = { Line: int ; Col: int }

module Position =
    let zero = { Line = 0 ; Col = 0 }

    let toString pos = sprintf "Line %i, col %i" (pos.Line + 1) (pos.Col + 1)

/// The current state before parsing. The position is equal to that of the last token that was consumed. If no tokens
/// were consumed yet, it is at 1, 0.
type ParseState = { CurPos: Position ; Input: List<string> ; Committed: bool }

module ParseState =
    /// Creates a ParseState from an input string, turning the string into lines, and initializing the position.
    let prepareString (str: String) : ParseState =
        if str = null || str = "" then
            { CurPos = Position.zero ; Input = [] ; Committed = false }
        else
            { CurPos = Position.zero
              Input = List.ofSeq <| str.Split ([| "\r\n" ; "\n" ; "\r" |], StringSplitOptions.None)
              Committed = false }

    /// Returns the contents of the line that is currently being parsed. If all input has been consumed,
    /// then the text 'End of input' is returned.
    let currentLine (state: ParseState) =
        if state.CurPos.Line < List.length state.Input then state.Input.[state.CurPos.Line] else "End of input"

    /// Returns the remaining input that is still to be consumed in the parse state.
    let remainingInput (state: ParseState) =
        let input = state.Input
        let pos = state.CurPos
        if pos.Line >= List.length input then ""
        else input.[pos.Line].Substring (pos.Col) :: input.[pos.Line + 1..] |> String.concat "\n"

    /// Advances the state to the next character, and returns this character, or None if there are no more
    /// characters to return.
    let nextToken (state: ParseState) : ParseState * Option<char> =
        if state.CurPos.Line >= List.length state.Input then
            // We are at the end of input, don't update position and return None.
            state, None
        else
            let curLine = currentLine state

            if state.CurPos.Col < curLine.Length then
                // Inside a line, return the character at this position and increase the column by one.
                { state with CurPos = { state.CurPos with Col = state.CurPos.Col + 1 } },
                Some curLine.[state.CurPos.Col]
            else
                // At the end of a line, return a newline and move state to the next line.
                { state with CurPos = { Line = state.CurPos.Line + 1 ; Col = 0 } }, Some '\n'

    /// Returns a string that marks the current location in the sate when parsing.
    let markLocation (state: ParseState) =
        let lineMarker = "^".PadLeft (state.CurPos.Col + 1)
        String.concat "\n" [ currentLine state ; lineMarker ]

// ------ Parser type and builder -----

type ParserLabel = string
type ErrorMessage = string

type ParseResult<'a> =
    | Success of 'a * ParseState
    | Failure of ParserLabel * ErrorMessage * ParseState

module ParseResult =
    /// Creates a failed parse result from the specified parser, with the specified state and message.
    let failure label (s: ParseState) msg : ParseResult<_> = Failure (label, msg, s)

    let success result state = Success (result, state)

    let result =
        function
        | Failure _ -> None
        | Success (r, _) -> Some r

    let newState =
        function
        | Failure (_, _, s) -> Some s
        | Success (_, s) -> Some s

    /// Returns the specified parse result if it is a success, or else the second parse result.
    let orElse (r2: unit -> ParseResult<_>) (r1: ParseResult<_>) =
        match r1 with
        | Success (r, s) -> Success (r, s)
        | _ -> r2 ()

    /// Displays an error message about what failed, where, given the contents of a failed parse result.
    let showError (label: ParserLabel) (message: ErrorMessage) (state: ParseState) =
        let failedMsg = sprintf "Parsing %s failed." label
        let positionMsg = sprintf "at %i:%i:" (state.CurPos.Line + 1) (state.CurPos.Col + 1)
        String.concat "\n" [ failedMsg ; positionMsg ; message ; ParseState.markLocation state ]

    /// Sets the committed state of a parse result to the specified value.
    let setCommitted committed = function
        | Failure (l, m, s) -> Failure (l, m, { s with Committed = committed })
        | Success (r, s) -> Success (r, { s with Committed = committed })

    /// Sets the committed state of a parse result to that from the provided state.
    let setCommittedFromState (state: ParseState) = setCommitted state.Committed

    /// Sets the committed state of a parse result to true if either of the two provided states is committed.
    let setCommittedFromStates (state1: ParseState) (state2: ParseState) =
        setCommitted (state1.Committed || state2.Committed)

/// The parser type.
type Parser<'a> = { Run: ParseState -> ParseResult<'a> ; Label: ParserLabel }

module Parser =
    /// Runs a parser, getting the result and the remaining input.
    let runParser (p: Parser<_>) = p.Run

    /// Runs a parser, returning only the result.
    let parse p state = runParser p (ParseState.prepareString state) |> ParseResult.result

    let setLabel newLabel p =
        { Label = newLabel
          Run =
            fun input ->
                /// Let the new parser function return the new label.
                match p.Run input with
                | Success (r, s) -> Success (r, s)
                | Failure (_, msg, pos) -> Failure (newLabel, msg, pos) }

/// A parser that dumps its results, but does not alter the provided parser otherwise.
/// Useful for testing parsers.
let mutable indent = 0
let dump (p: Parser<_>) =
    { Label = p.Label
      Run =
        fun state ->
            let curIndent = indent
            let prefix = "".PadLeft(indent * 2)
            printfn "%s=== Dump for %s ===" prefix p.Label
            printfn "%sCommitted before: %b" prefix state.Committed
            indent <- indent + 1
            let res =
                match p.Run state with
                | Success (r, s) ->
                    printfn "%sSuccess" prefix
                    printfn "%sResult:    %A" prefix r
                    printfn "%sPosition:  %s" prefix <| Position.toString s.CurPos
                    printfn "%sCommitted: %b" prefix s.Committed
                    printfn "%s" <| ParseState.markLocation s
                    Success (r, s)
                | Failure (l, m, s) ->
                    printfn "%sFailure" prefix
                    printfn "%sMessage:   %s" prefix m
                    printfn "%sPosition:  %s" prefix <| Position.toString s.CurPos
                    printfn "%sCommitted: %b" prefix s.Committed
                    printfn "%s" <| ParseState.markLocation s
                    Failure (l, m, s)
            printfn "%s=== End of dump for %s ===" prefix p.Label
            indent <- curIndent
            res
    }

/// Bind operation on parsers.
let bind (f: 'a -> Parser<'b>) (p: Parser<'a>) =
    { Label = sprintf "bind after %s" p.Label
      Run =
        fun state ->
            // Committing does not pass on through parsers called with bind, only through the
            // result produced by these parsers.
            match p.Run state with
            | Failure (l, m, s') -> Failure (l, m, s') |> ParseResult.setCommittedFromStates state s'
            | Success (r, s') ->
                match (f r).Run { s' with Committed = false } with
                | Failure (l', m', s'') -> Failure (l', m', s'') |> ParseResult.setCommittedFromStates s' s''
                | Success (r'', s'') -> Success (r'', s'') |> ParseResult.setCommittedFromStates s' s''
    }
    
/// Creates a parser that always returns the specified input and doesn't consume anything.
let succeed x = { Label = "succeed" ; Run = fun state -> ParseResult.success x state }

/// Creates a parser that always fails.
let fail () =
    let label = "Fail" in { Label = label ; Run = fun state -> ParseResult.failure label state "Fail parser failed." }

/// Simply returns the current committed state.
let isCommitted = 
    { Label = "IsCommited"
      Run = fun s -> ParseResult.success s.Committed s
    }

/// A parser that does nothing except mark the state as committed or uncommitted.
/// After the state is committed, an alt parser will no longer try another option.
let commit committed =
    { Label = sprintf "commit %b" committed
      Run = fun state -> Success ((), { state with Committed = committed })
    }

/// Changes a parser to have a committed result.
let commit' committed p =
    { Label = sprintf "committed %b %s" committed p.Label
      Run = fun state -> p.Run state |> ParseResult.setCommitted committed
    }

/// Changes a parser, such that if it fails, the error message is altered using the provided function.
let mapError (f: string -> string) (p: Parser<_>) =
    { Label = p.Label
      Run =
        fun state ->
            match p.Run state with
            | Success (r, s) -> Success (r, s)
            | Failure (l, msg, pos) -> Failure (l, f msg, pos) }

type ParserBuilder () =
    member _.Bind ((a: Parser<_>), f) = bind f a
    member _.Return x = succeed x
    member _.ReturnFrom (p: Parser<_>) = p
    member _.Delay f = f
    member _.Run f = bind f (succeed ())
    member _.Zero = succeed ()
    member _.Combine (p1, p2) = bind (fun () -> p2) p1

let parser = new ParserBuilder ()


// ----- Combinators -----

/// Performs a parser, and succeeds if it succeeds, but consumes no input.
let peek p =
    { Label = sprintf "peek (%s)" p.Label
      Run =
        fun state ->
            match p.Run state with
            | Success (r, _) -> Success (r, state)
            | Failure (l, m, s) -> Failure (l, m, s) }

/// Returns a parser that performs a check on an input token and fails if the check fails or there is no more input.
let satisfy f label =
    { Label = label
      Run =
        fun state ->
            let state', chOpt = ParseState.nextToken state

            match chOpt with
            | None -> ParseResult.failure label state "No more input"
            | Some ch ->
                if f ch then
                    ParseResult.success ch state'
                else
                    ParseResult.failure label state <| sprintf "Unexpected '%c'" ch }

/// Runs a check on the result of a parser, and fails if the check fails.
let check f p =
    let label = sprintf "check %s" p.Label

    { Label = label
      Run =
        fun state ->
            match p.Run state with
            | Failure (l, m, s) -> Failure (l, m, s)
            | Success (r, s') when f r -> Success (r, s')
            | Success (r, _) -> ParseResult.failure label state (sprintf "Unexpected %A" r) }

/// Performs 2 parsers in sequence, and combines them with the provided function.
let combine f p1 p2 =
    parser {
        let! res1 = p1
        let! res2 = p2
        return f res1 res2
    }
    |> Parser.setLabel (sprintf "(%s) combine (%s)" p1.Label p2.Label)

/// The sequence parser except the order of the parsers is reversed.
/// This can be used when piping the result of one parser into another parser, since the pipe operator places the result
let andThen f p1 p2 =
    parser {
        let! res1 = p1
        let! res2 = p2
        return f res2 res1
    }
    |> Parser.setLabel (sprintf "(%s) andThen (%s)" p2.Label p1.Label)

/// Performs two parsers in order, but ignores the result of the second.
let skipNext p1 p2 =
    parser {
        let! res = p1
        let! _ = p2
        return res
    }
    |> Parser.setLabel (sprintf "(%s) skipNext (%s)" p1.Label p2.Label)

/// Performs two parsers in order, but ignores the result of the first.
let skipPrev p1 p2 =
    parser {
        let! _ = p1
        return! p2
    }
    |> Parser.setLabel (sprintf "(%s) skipPrev (%s)" p1.Label p2.Label)

/// Applies the specified function over the result of the specified parser.
let map f p =
    parser {
        let! res = p
        return f res
    }
    |> Parser.setLabel p.Label

/// Ignores the result of a parser.
let (~~) (p: Parser<_>) = map ignore p

/// Alt, but copies the committed state from the called parsers.
let altc (p1: Parser<_>) (p2: Parser<_>) =
    { Label = sprintf "(%s) alt (%s)" p1.Label p2.Label
      Run = fun state ->
        match p1.Run state with
        | Success (r, s) -> Success (r, s )
        | Failure (l, m, s) when s.Committed -> Failure (l, m, s)
        | Failure (l, m, s) ->
            match p2.Run state with
            | Success (r, s) -> Success (r, s)
            | Failure (l', m', s') -> Failure (l', m', s')
    }

/// Combines two parsers by trying to apply the first one first, and if it fails, applying the second one to the same
/// input. Fails if both parsers fail.
let alt (p1: Parser<_>) (p2: Parser<_>) =
    let temp = altc p1 p2
    { temp with Run = fun state -> temp.Run state |> ParseResult.setCommittedFromState state }

/// Tries each of the specified parsers in order.
let rec oneOf (ps: List<Parser<_>>) = List.reduce alt ps

/// Tail recursive helper to run a parser zero or more times.
/// Doesn't accept a parse result that hasn't consumed input (running the same parser multiple times without consuming
/// input is a guaranteed infinite loop).
/// If a parser inside the loop has committed and fails, or consumes no input, this parser also fails.
let rec private parseZeroOrMore label acc p s =
    match p.Run s with
    | Success (r, s') when s.CurPos <> s'.CurPos -> parseZeroOrMore label (r :: acc) p { s' with Committed = s.Committed }
    | Success (r, s') when s'.Committed && s.CurPos = s'.CurPos ->
        let msg = sprintf "Parser %s committed but did not consume input." p.Label
        Failure (label, msg, s')
    | Failure (l, m, s') when s'.Committed -> Failure (l, m, s') |> ParseResult.setCommitted s.Committed
    | _ -> Success (List.rev acc, s) |> ParseResult.setCommitted s.Committed

/// Returns a parser which runs the specified parser zero or more times.
/// Also stops if the parser succeeds but consumes no more input, in this case, the parse result is not included.
let star p =
    let label = sprintf "star %s" p.Label
    { Label = label
      Run = fun s -> parseZeroOrMore label [] p s
    }

/// Returns a parser which runs the specified parser one or more times.
let plus p =
    parser {
        let! committedBefore = isCommitted
        let! first = p |> commit' committedBefore // the first parser could commit, revert this.
        let! rest = star p  // Star doesn't commit.
        return first :: rest
    }
    |> Parser.setLabel (sprintf "plus %s" p.Label)

/// Tail recursive helper to run a parser exactly a specified number of times.
let rec private matchExactly acc n p s =
    match n with
    | 0 -> ParseResult.success (List.rev acc) s
    | n ->
        match p.Run s with
        | Success (r, s') -> matchExactly (r :: acc) (n - 1) p s'
        | Failure (l, m, s) -> Failure (l, m, s)

/// Returns a parser which runs the specified parser exactly n times.
let times n p = { Label = sprintf "times %s" p.Label ; Run = fun state -> matchExactly [] n p state }

/// Returns a parser which runs the specified parser zero times or once.
let optional p = alt (map Some p) (succeed None) |> Parser.setLabel (sprintf "optional %s" p.Label)

/// Returns a parser that keeps the result of the middle parser.
let between l p r = skipNext (skipPrev l p) r

/// Returns a parser that applies a parser one or more times, but takes a separator parser in between every occurrence.
let separated1 sep p =
    let sepP = skipPrev sep p
    combine (fun x xs -> x :: xs) p (star sepP) |> Parser.setLabel (sprintf "%s separated by %s" p.Label sep.Label)

/// Separated1, but allows 0 matches of the parser.
let separated sep p = alt (separated1 sep p) (succeed [])

/// Tail recursive helper that applies multiple parsers in sequence and returns the result as a list.
/// Succeeds only if all parsers in the list succeed.
let rec private sequenceHelper acc ps s =
    match ps with
    | [] -> ParseResult.success (List.rev acc) s
    | p :: ps' ->
        match p.Run s with
        | Success (r, s') -> sequenceHelper (r :: acc) ps' s'
        | Failure (l, m, s) -> Failure (l, m, s)

let sequence ps = { Label = sprintf "sequence %A" (List.map (fun p -> p.Label) ps) ; Run = sequenceHelper [] ps }


// ----- Character parsers -----

/// A parser that parses a single character, or fails when the input is empty.
let pChar: Parser<char> = satisfy (fun _ -> true) "pChar"

/// A parser that parses a character only if it is numeric.
let num = satisfy Char.IsDigit "num"

/// A parser that parses a character only if it is a letter.
let alpha = satisfy Char.IsLetter "alpha"

/// A parser that parses a character only if it is numeric or a letter.
let alphaNum = alt num alpha |> Parser.setLabel "alphaNum"

/// Returns a parser that parses the specified character.
let litC c = satisfy ((=) c) "litC"

/// A parser that parses the end of a line.
let eol = litC '\n' |> Parser.setLabel "eol"

/// Returns a parser that parses any of the specified characters.
let anyOf chars = chars |> List.map litC |> oneOf |> Parser.setLabel (sprintf "anyOf %A" chars)

/// A parser that fails if there is more input to consume, and succeeds otherwise.
let eoi =
    { Label = "eoi"
      Run =
        fun state ->
            let _, chOpt = ParseState.nextToken state

            match chOpt with
            | None -> ParseResult.success () state
            | Some ch -> ParseResult.failure "eoi" state (sprintf "Expected end of input, but got '%c'" ch) }

/// A parser that consumes a space character.
let space = litC ' ' |> Parser.setLabel "space"

/// A parser that consumes any whitespace character.
let ws = satisfy Char.IsWhiteSpace "ws"

/// A parser that consumes any whitespace character except a newline.
let wsNoEol = satisfy (fun c -> Char.IsWhiteSpace c && c <> '\n' && c <> '\r') "wsNoEol"


// ----- String parsers -----

/// Returns a parser that parses a literal string.
let lit (str: string) = str |> List.ofSeq |> List.map litC |> sequence |> map listToStr |> Parser.setLabel str

/// Modifies a parser to fail if it is not followed by the end of a line.
let line p = skipNext p eol |> Parser.setLabel (sprintf "line %s" p.Label)

/// The same as separated1, except the separator is a string literal.
let separated1_ sep = separated1 (lit sep)

/// The same as separated, except the separator is a string literal.
let separated_ sep = separated (lit sep)

/// Returns a parser that parses a string composed from the result of the specified parser, as long as the parser
/// succeeds. The parser needs to succeed at least once for this parser to succeed.
let stringOf1 p =
    parser {
        let! result = plus p
        return listToStr result
    }
    |> Parser.setLabel (sprintf "string of %s" p.Label)

/// Returns a parser that parses a string composed from the result of the specified parser, as long as the parser
/// succeeds. If the parser never succeeds, an empty string is returned.
let stringOf p =
    parser {
        let! result = star p
        return listToStr result
    }
    |> Parser.setLabel (sprintf "string of %s" p.Label)

/// Returns a parser that parses a string composed from the result of the specified parser, as long as it matches
/// at least the specified number of times. More matches are ignored, fewer matches result in failure.
let stringOfLength length p =
    parser {
        let! result = times length p
        return listToStr result
    }
    |> Parser.setLabel (sprintf "%i length string of %s" length p.Label)

/// Returns a parser that consumes one character at a time, as long as the condition on the character is positive.
/// The condition needs to be positive at least once for this parser to succeed.
let takeWhile1 condition = stringOf1 (satisfy condition "") |> Parser.setLabel "takeWhile"

/// Returns a parser that consumes one character at a time, as long as the condition on the character is positive.
/// If the condition is never positive, an empty string is returned.
let takeWhile condition = stringOf (satisfy condition "") |> Parser.setLabel "takeWhile"

/// Returns a parser that takes the first parser for the first character of a string, and the next parser for the rest.
/// Returns in a string of length at least 1, with the prefix parser result, and as long as the second parser keeps
/// matching.
let stringOf2 prefix rest label =
    parser {
        let! p = prefix
        let! r = stringOf rest
        return sprintf "%c%s" p r
    }
    |> Parser.setLabel label

// ----- Int parsers -----

/// Returns a parser that succeeds when the specified string is an integer, and returns this integer.
let parseInt (str: string) =
    match Int32.TryParse str with
    | true, i -> succeed i
    | _ -> fail ()

/// A parser that parses a single digit as an integer.
let digit =
    parser {
        let! d = num
        return! parseInt (listToStr [ d ])
    }
    |> Parser.setLabel "digit"

/// A parser that parses a non-negative integer.
let nonNegativeInt =
    parser {
        let! ds = stringOf1 num
        return! parseInt ds
    }
    |> Parser.setLabel "nonNegativeInt"
