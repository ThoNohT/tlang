module tlang.Parser

open System

// ----- Helper functions -----

/// Converts a list of characters to a string.
let private listToStr = Array.ofList >> (fun a -> new string (a))


// ------ Parser type and builder -----

/// The parser type.
type Parser<'a> = Parser of (string -> Option<'a * string>)

/// Runs a parser, getting the result and the remaining string.
let runParser (Parser f) input = f input

/// Runs a parser, returning only the result.
let parse p input = runParser p input |> Option.map fst

/// Bind operation on parsers.
let bind (f: 'a -> Parser<'b>) (Parser p) =
    Parser (fun input ->
        match p input with
        | None -> None
        | Some (res1, rest1) -> runParser (f res1) rest1)

/// Creates a parser that always returns the specified input.
let succeed x = Parser (fun input -> Some (x, input))

/// Creates a parser that always fails.
let fail () = Parser (fun _ -> None)

/// Creates a parser that consumes all input.
let all = Parser (fun input -> Some (input, ""))

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
    Parser (fun input ->
        match p input with
        | None -> Some ((), input)
        | _ -> None)

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
        return combine p2 p1
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
let alt (Parser p1) (Parser p2) = Parser (fun input -> Option.orElseWith (fun _ -> p2 input) (p1 input))

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
    Parser (fun input ->
        match Seq.toList input with
        | x :: xs -> Some (x, listToStr xs)
        | _ -> None)

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
    Parser (fun input -> if input.StartsWith str then Some (str, input.Substring (str.Length)) else None)

/// A parser that fails if there is more input to consume, and succeeds otherwise.
let eoi = inv pChar

/// A parser that parses the end of a line.
let eol = litC '\n'

/// Modifies a parser to fail if it is not followed by the end of a line.
let line p = skipNext p eol

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

let stringOfLength length p =
    parser {
        let! result = times length p
        return listToStr result

    }

/// Returns a parser that parses a string composed from the result of the specified parser, as long as the parser
/// succeeds. If the parser never succeeds, an empty string is returned.
let stringOf_ p =
    parser {
        let! result = star p
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

/// A parser that parses a positive integer.
let positiveInt =
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
