module tlang.Parser

open System

// ----- Helper functions -----

/// Converts a list of characters to a string.
let private listToStr = Array.ofList >> (fun a -> new string (a))

/// A position in the input.
type Position = { Line: int ; Col: int }

module Position =
    let zero = { Line = 1 ; Col = 1 }

    let toString pos = sprintf "Line %i, col %i" pos.Line pos.Col

/// One token, consisting of a character with a position.
type Token = { Char: char ; Pos: Position }

/// The current state before parsing. The position is equal to that of the last token that was consumed. If no tokens
/// were consumed yet, it is at 1, 1.
type ParseState = { CurPos: Position ; Input: List<Token> }

module ParseState =
    /// Updates the state with new remaining tokens, and updates the current position to that of the last token that
    /// was consumed. If no tokens were consumed, the position is not updated.
    let update state remaining consumed =
        let newState = { state with Input = remaining }

        match List.tryLast consumed with
        | Some last -> { newState with CurPos = last.Pos }
        | _ -> newState


module Token =
    /// Converts a token to a character.
    let toChar (ic: Token) = ic.Char

    /// Converts a list of tokens to a string.
    let toString (ics: List<Token>) = List.map toChar ics |> listToStr

    /// Returns the position of a token.
    let pos { Pos = pos } = pos

    /// Turns a string into a list of Token, containing information about the line and column the characters are
    /// located on.
    let prepareString (str: String) : ParseState =
        let prepareLine lineNr line =
            Seq.mapi
                (fun idx char -> { Char = char ; Pos = { Line = lineNr + 1 ; Col = idx + 1 } })
                (Seq.append line [ '\n' ])

        // Drop the last element of a list.
        let rec dropLast =
            function
            | [] -> []
            | [ _ ] -> []
            | x :: xs -> x :: dropLast xs

        if str = null then
            { CurPos = Position.zero ; Input = [] }
        else
            { CurPos = Position.zero
              Input =
                str.Split ([| '\n' |], StringSplitOptions.None)
                |> Seq.mapi prepareLine
                |> Seq.concat
                |> List.ofSeq
                |> dropLast }



// ------ Parser type and builder -----

type ParseError = { Pos: Position ; Message: string }

module ParseError =
    /// Create a parse error from the specified state with the specified message.
    let make state msg : Result<_, ParseError> = Error { Pos = state.CurPos ; Message = msg }

/// The parser type.
type Parser<'a> = Parser of (ParseState -> Result<'a * ParseState, ParseError>)

/// Runs a parser, getting the result and the remaining string.
let runParser (Parser f) input = f <| input

/// Runs a parser, returning only the result.
let parse p input = runParser p (Token.prepareString input) |> Result.map fst

/// Bind operation on parsers.
let bind (f: 'a -> Parser<'b>) (Parser p) =
    Parser (fun state ->
        match p state with
        | Error e -> Error e
        | Ok (res1, rest1) -> runParser (f res1) rest1)

/// Creates a parser that always returns the specified input and doesn't consume anything.
let succeed x = Parser (fun state -> Ok (x, state))

/// Creates a parser that always fails.
let fail () = Parser (fun state -> ParseError.make state "Fail parser failed.")

/// Creates a parser that consumes all input.
let all = Parser (fun state -> Ok (Token.toString state.Input, ParseState.update state [] state.Input))

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

/// Changes a parser to fail if it succeeded, and succeed with unit without consuming input if it failed.
let inv (Parser p) =
    Parser (fun state ->
        match p state with
        | Error _ -> Ok ((), state)
        | _ -> ParseError.make state "Inv parser failed.")

/// Performs a parser, and succeeds if it succeeds, but returns nothing and consumes no input.
let peek p = inv p |> inv

/// Changes a parser to fail if the provided check on it's succesful result fails.
let check f p =
    parser {
        let! res = p
        return! if f res then succeed res else fail ()
    }

/// Performs 2 parsers in sequence, and combines them with the provided function.
let sequence combine p1 p2 =
    parser {
        let! res1 = p1
        let! res2 = p2
        return combine res1 res2
    }

/// The sequence parser except the order of the parsers is reversed.
/// This can be used when piping the result of one parser into another parser, since the pipe operator places the result
let andThen combine p1 p2 =
    parser {
        let! res1 = p1
        let! res2 = p2
        return combine res2 res1
    }

/// Performs two parsers in order, but ignores the result of the second.
let skipNext p1 p2 =
    parser {
        let! res = p1
        let! _ = p2
        return res
    }

/// Performs two parsers in order, but ignores the result of the first.
let skipPrev p1 p2 =
    parser {
        let! _ = p1
        return! p2
    }

/// Applies the specified function over the result of the specified parser.
let map f p =
    parser {
        let! res = p
        return f res
    }

/// Combines two parsers by trying to apply the first one first, and if it fails, applying the second one to the same
/// input. Fails if both parsers fail.
let alt (Parser p1) (Parser p2) =
    Parser (fun state ->
        match p1 state with
        | Ok r -> Ok r
        | Error e1 ->
            match p2 state with
            | Ok r -> Ok r
            | Error e2 -> ParseError.make state (sprintf "Alternative parsers failed:\n%s\n%s" e1.Message e2.Message))

/// Tries each of the specified parsers in order.
let rec oneOf =
    function
    | [] -> failwith "No parsers provided."
    | x :: [] -> x
    | x :: xs -> alt x (oneOf xs)


// ----- Regex combinators -----

/// Returns a parser which runs the specified parser zero or more times.
let rec star p = alt (bind (fun r -> map (fun rs -> r :: rs) (star p)) p) (succeed [])

/// Returns a parser which runs the specified parser one or more times.
let rec plus p =
    parser {
        let! first = p
        let! rest = star p
        return first :: rest
    }

/// Returns a parser which runs the specified parser exactly n times.
let rec times n p = parser { if n <= 0 then return [] else return! sequence (fun x xs -> x :: xs) p (times (n - 1) p) }

/// Returns a parser which runs the specified parser zero or one times.
let optional p = alt (map Some p) (succeed None)


// ----- Character parsers -----

/// A parser that parses a single character, or fails when the input is empty.
let pChar: Parser<char> =
    Parser (fun state ->
        match Seq.toList state.Input with
        | x :: xs -> Ok (Token.toChar x, ParseState.update state xs [ x ])
        | _ -> ParseError.make state "Expected a character, but got end of input")

/// A parser that parses a character only if it is numeric.
let num = check Char.IsDigit pChar

/// A parser that parses a character only if it is a letter.
let alpha = check Char.IsLetter pChar

/// A parser that parses a character only if it is numeric or a letter.
let alphaNum = alt num alpha

/// Returns a parser that parses the specified character.
let litC c = check ((=) c) pChar


// ----- String parsers -----

/// Returns a parser that parses a literal string.
let lit (str: string) =
    Parser (fun state ->
        if (Token.toString state.Input).StartsWith str then
            let consumed = List.take str.Length state.Input
            Ok (str, ParseState.update state (List.skip str.Length state.Input) consumed)
        else
            ParseError.make state <| sprintf "Expected the string '%s'" str)

/// A parser that fails if there is more input to consume, and succeeds otherwise.
let eoi = inv pChar

/// A parser that parses the end of a line.
let eol = litC '\n'

/// Modifies a parser to fail if it is not followed by the end of a line.
let line p = skipNext p (alt (map ignore eol) eoi)

/// Returns a parser that applies a parser one or more times, but takes a separator parser in between every occurrence.
let separated separator p = sequence (fun x xs -> x :: xs) p (star (skipPrev separator p))

/// The same as separated, except the separator is a string literal.
let separated_ separator = separated (lit separator)

/// Returns a parser that parses a string composed from the result of the specified parser, as long as the parser
/// succeeds. The parser needs to succeed at least once for this parser to succeed.
let stringOf p =
    parser {
        let! result = plus p
        return listToStr result
    }

/// Returns a parser that parses a string composed from the result of the specified parser, as long as the parser
/// succeeds. If the parser never succeeds, an empty string is returned.
let stringOf_ p =
    parser {
        let! result = star p
        return listToStr result
    }

/// Returns a parser that parses a string composed from the result of the specified parser, as long as it matches
/// at least the specified number of times. More matches are ignored, fewer matches result in failure.
let stringOfLength length p =
    parser {
        let! result = times length p
        return listToStr result

    }

/// Returns a parser that consumes one character at a time, as long as the condition on the character is positive.
/// The condition needs to be positive at least once for this parser to succeed.
let takeWhile condition = stringOf (check condition pChar)

/// Returns a parser that consumes one character at a time, as long as the condition on the character is positive.
/// If the condition is never positive, an empty string is returned.
let takeWhile_ condition = stringOf_ (check condition pChar)


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

/// A parser that parses a non-negative integer.
let nonNegativeInt =
    parser {
        let! ds = stringOf num
        return! parseInt ds
    }

/// The different types of programs that can be defined.
type ProgramType =
    /// An executable gets compiled into an executable file and cannot be referenced.
    /// The parameter is the name of the executable.
    | Executable of string


/// A complete program parsed from a file.
type Program = { Type: ProgramType ; Value: String }

/// A parser for program types.
let pProgramType =
    parser {
        let! _ = lit "Executable: "
        let! first = alpha
        let! rest = stringOf_ alphaNum
        return Executable (sprintf "%c%s" first rest)
    }

/// A parser for a program. A program is defined as:
/// Line 1: The type,
/// Line 2: A separator of at least one '-',
/// The rest: The value to print out.
let pProgram =
    parser {
        let! typ = line pProgramType
        let! _ = line <| plus (litC '-')
        let! value = all

        return { Type = typ ; Value = value }
    }
