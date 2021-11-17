module tlang.Test.Parser

open Fuchu

module P = tlang.Parser

let assertSuccessWith p input result remaining line col =
    match P.Parser.runParser p (P.ParseState.prepareString input) with
    | P.Failure (l, m, s) ->
        Test.Fail <| sprintf "Expected the parser to succeed, but got error %s: %s at %s." l m (P.Position.toString s.CurPos)
    | P.Success (res, state) ->
        Assert.Equal ("Wrong result.", result, res)
        Assert.Equal ("Wrong remaining.", remaining, P.ParseState.remainingInput state)
        Assert.Equal ("Wrong line.", line, state.CurPos.Line)
        Assert.Equal ("Wrong column.", col, state.CurPos.Col)

let assertFailureAt p input line col =
    match P.Parser.runParser p (P.ParseState.prepareString input) with
    | P.Failure (_, _, s) ->
        Assert.Equal ("Wrong line.", line, s.CurPos.Line)
        Assert.Equal ("Wrong column.", col, s.CurPos.Col)
    | _ -> Test.Fail "Expected the parser to fail."

let assertFailuresAt p inputs line col =
    for input in inputs do
        assertFailureAt p input line col

let parserTests =
    testList
        "Parser"
        [ testCase "succeed succeeds on empty input" <| fun _ -> assertSuccessWith (P.succeed ()) "" () "" 0 0
          testCase "succeed succeeds on nonempty input and doesn't consume"
          <| fun _ -> assertSuccessWith (P.succeed ()) "x" () "x" 0 0
          testCase "fail fails on empty input" <| fun _ -> assertFailureAt (P.fail ()) "" 0 0
          testCase "fail fails on nonempty input" <| fun _ -> assertFailureAt (P.fail ()) "x" 0 0 ]

let combinatorTests =
    testList
        "Combinators"
        [ testCase "peek succeeds but doesn't consume"
          <| fun _ -> assertSuccessWith (P.peek P.pChar) "abc" 'a' "abc" 0 0
          testCase "peek fails on failure" <| fun _ -> assertFailureAt (P.peek P.num) "abc" 0 0
          testCase "check fails on failure" <| fun _ -> assertFailureAt (P.check (fun _ -> true) P.num) "abc" 0 0
          testCase "check fails on negative check"
          <| fun _ -> assertFailureAt (P.check (fun c -> c = 'x') P.pChar) "abc" 0 0
          testCase "check succeeds on positive check"
          <| fun _ -> assertSuccessWith (P.check (fun _ -> true) P.pChar) "abc" 'a' "bc" 0 1
          testCase "andThen uses the provided combine function"
          <| fun _ -> assertSuccessWith (P.andThen (-) P.digit P.digit) "642" -2 "2" 0 2
          testCase "andThen fails when the first parser fails"
          <| fun _ -> assertFailureAt (P.andThen (-) P.digit P.digit) "a42" 0 0
          testCase "andThen fails when the second parser fails"
          <| fun _ -> assertFailureAt (P.andThen (-) P.digit P.digit) "6a2" 0 1
          testCase "skipNext skips the second parser"
          <| fun _ -> assertSuccessWith (P.skipNext P.digit P.digit) "642" 6 "2" 0 2
          testCase "andThen fails when the first parser fails"
          <| fun _ -> assertFailureAt (P.skipNext P.digit P.digit) "a42" 0 0
          testCase "skipNext fails when the second parser fails"
          <| fun _ -> assertFailureAt (P.skipNext P.digit P.digit) "6a2" 0 1
          testCase "skipPrev skips the second parser"
          <| fun _ -> assertSuccessWith (P.skipPrev P.digit P.digit) "642" 4 "2" 0 2
          testCase "andThen fails when the first parser fails"
          <| fun _ -> assertFailureAt (P.skipPrev P.digit P.digit) "a42" 0 0
          testCase "skipPrev fails when the second parser fails"
          <| fun _ -> assertFailureAt (P.skipPrev P.digit P.digit) "6a2" 0 1
          testCase "map applies the mapping"
          <| fun _ -> assertSuccessWith (P.map (fun x -> x + 1) P.digit) "1a" 2 "a" 0 1
          testCase "map fails on failure" <| fun _ -> assertFailureAt (P.map (fun x -> x + 1) P.digit) "ab" 0 0
          testCase "alt tries the first parser first"
          <| fun _ -> assertSuccessWith (P.alt (P.lit "abc") (P.lit "ab")) "abcd" "abc" "d" 0 3
          testCase "alt falls back to the second parser"
          <| fun _ -> assertSuccessWith (P.alt (P.lit "abc") (P.lit "abd")) "abde" "abd" "e" 0 3
          testCase "alt fails when all parsers fail" <| fun _ -> assertFailureAt (P.alt (P.lit "a") (P.lit "b")) "c" 0 0
          testCase "oneOf tries the first parser first"
          <| fun _ -> assertSuccessWith (P.oneOf [ P.lit "abc" ; P.lit "ab" ; P.lit "a" ]) "abcd" "abc" "d" 0 3
          testCase "oneOf falls back to the second parser"
          <| fun _ -> assertSuccessWith (P.oneOf [ P.lit "abc" ; P.lit "abd" ; P.lit "a" ]) "abde" "abd" "e" 0 3
          testCase "oneOf falls back to the third parser"
          <| fun _ -> assertSuccessWith (P.oneOf [ P.lit "abc" ; P.lit "abd" ; P.lit "ab" ]) "abff" "ab" "ff" 0 2
          testCase "oneOf fails when all parsers fail"
          <| fun _ -> assertFailureAt (P.oneOf [ P.lit "a" ; P.lit "b" ; P.lit "c" ]) "d" 0 0 ]

let regexCombinatorTests =
    testList
        "Regex combinators"
        [ testCase "star consumes no input when the first occurrence fails"
          <| fun _ -> assertSuccessWith (P.star P.digit) "a123" [] "a123" 0 0
          testCase "star consumes input until the first occurrence that fails"
          <| fun _ -> assertSuccessWith (P.star P.digit) "123a" [ 1 ; 2 ; 3 ] "a" 0 3
          testCase "plus fails when the first occurrence fails" <| fun _ -> assertFailureAt (P.plus P.digit) "a123" 0 0
          testCase "plus consumes input until the first occurrence that fails"
          <| fun _ -> assertSuccessWith (P.plus P.digit) "123a" [ 1 ; 2 ; 3 ] "a" 0 3
          testCase "times fails when the too few instances succeed"
          <| fun _ -> assertFailureAt (P.times 4 P.digit) "123ab" 0 3
          testCase "times doesn't consume more than the specied times"
          <| fun _ -> assertSuccessWith (P.times 2 P.digit) "1234" [ 1 ; 2 ] "34" 0 2
          testCase "optional consumes no input when the first occurrence fails"
          <| fun _ -> assertSuccessWith (P.optional P.digit) "a123" None "a123" 0 0
          testCase "optional consumes input only once"
          <| fun _ -> assertSuccessWith (P.optional P.digit) "123a" (Some 1) "23a" 0 1 ]

let characterParserTests =
    testList
        "Character parsers"
        [ testCase "pChar fails on empty input" <| fun _ -> assertFailureAt P.pChar "" 0 0
          testCase "pChar succeeds on nonempty input" <| fun _ -> assertSuccessWith P.pChar "abc" 'a' "bc" 0 1
          testCase "num succeeds on a number" <| fun _ -> assertSuccessWith P.num "123" '1' "23" 0 1
          testCase "num fails with non-numeric characters"
          <| fun _ -> assertFailuresAt P.num [ "a" ; "-" ; ";" ; " " ] 0 0
          testCase "num fails on empty input" <| fun _ -> assertFailureAt P.num "" 0 0
          testCase "alpha succeeds on a letter" <| fun _ -> assertSuccessWith P.alpha "abc" 'a' "bc" 0 1
          testCase "alpha fails with non-alpha characters"
          <| fun _ -> assertFailuresAt P.alpha [ "1" ; "-" ; ";" ; " " ] 0 0
          testCase "alpha fails on empty input" <| fun _ -> assertFailureAt P.alpha "" 0 0
          testCase "alphaNum succeeds on a letter" <| fun _ -> assertSuccessWith P.alphaNum "abc" 'a' "bc" 0 1
          testCase "alphaNum succeeds on a number" <| fun _ -> assertSuccessWith P.alphaNum "123" '1' "23" 0 1
          testCase "alphaNum fails with non-alphaNumeric characters"
          <| fun _ -> assertFailuresAt P.alphaNum [ "-" ; ";" ; " " ] 0 0
          testCase "alphaNum fails on empty input" <| fun _ -> assertFailureAt P.alphaNum "" 0 0
          testCase "litC succeeds on the exact character" <| fun _ -> assertSuccessWith (P.litC 'a') "abc" 'a' "bc" 0 1
          testCase "litC fails on any other character"
          <| fun _ -> assertFailuresAt (P.litC 'a') [ "bc" ; "123" ; "f" ; " " ; ";" ] 0 0
          testCase "litC fails on empty input" <| fun _ -> assertFailureAt (P.litC 'a') "" 0 0
          testCase "litC with end of line" <| fun _ -> assertSuccessWith (P.litC '\n') "\nabc" '\n' "abc" 1 0 ]

let stringParserTests =
    testList
        "String parsers"
        [ testCase "lit succeeds on the exact input" <| fun _ -> assertSuccessWith (P.lit "abc") "abcd" "abc" "d" 0 3
          testCase "lit fails on the empty input" <| fun _ -> assertFailureAt (P.lit "abc") "" 0 0
          testCase "eoi succeeds on empty input" <| fun _ -> assertSuccessWith P.eoi "" () "" 0 0
          testCase "eoi fails when there is more input" <| fun _ -> assertFailuresAt P.eoi [ "a" ; " " ; "\n" ] 0 0
          testCase "eol succeeds at the end of a line" <| fun _ -> assertSuccessWith P.eol "\nabc" '\n' "abc" 1 0
          testCase "eol fails on any other input" <| fun _ -> assertFailuresAt P.eol [  "a" ; "\t" ; " " ] 0 0
          testCase "eol fails when there is no more input" <| fun _ -> assertFailureAt P.eol "" 0 0
          testCase "line succeeds when the parser leaves a newline"
          <| fun _ -> assertSuccessWith (P.line P.digit) "5\n5" 5 "5" 1 0
          testCase "line succeeds when the parser leaves no input"
          <| fun _ -> assertSuccessWith (P.line P.digit) "5" 5 "" 1 0
          testCase "line fails when the parser doesnt leave end of line or input"
          <| fun _ -> assertFailuresAt (P.line P.digit) [ "5a" ; "5 " ; "5-" ] 0 1
          testCase "line fails if parser fails" <| fun _ -> assertFailuresAt (P.line P.digit) [ "a\n" ; "\n" ] 0 0
          testCase "separated succeeds when multiple items are separated"
          <| fun _ -> assertSuccessWith (P.separated1 P.digit P.alpha) "a1b2c3d456" [ 'a' ; 'b' ; 'c' ; 'd' ] "456" 0 7
          testCase "separated1 fails when the parser never matches"
          <| fun _ -> assertFailureAt (P.separated1 (P.litC ';') P.digit) "a;b;c" 0 0
          testCase "separated1 returns the first element when separators never match"
          <| fun _ -> assertSuccessWith (P.separated1 (P.litC ';') P.digit) "1.2.3" [ 1 ] ".2.3" 0 1
          testCase "separated1_ succeeds when multiple items are separated_"
          <| fun _ -> assertSuccessWith (P.separated1_ "1" P.alpha) "a1b1c1d156" [ 'a' ; 'b' ; 'c' ; 'd' ] "156" 0 7
          testCase "separated1_ fails when the parser never matches"
          <| fun _ -> assertFailureAt (P.separated1_ ";" P.digit) "a;b;c" 0 0
          testCase "separated1_ returns the first element when separators never match"
          <| fun _ -> assertSuccessWith (P.separated1_ ";" P.digit) "1.2.3" [ 1 ] ".2.3" 0 1
          testCase "stringof1 takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.stringOf1 P.num) "1234abc" "1234" "abc" 0 4
          testCase "stringof1 fails when the first character doesn't match"
          <| fun _ -> assertFailureAt (P.stringOf1 P.num) "a2134" 0 0
          testCase "stringOf takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.stringOf P.num) "1234abc" "1234" "abc" 0 4
          testCase "stringOf succeeds when the first character doesn't match"
          <| fun _ -> assertSuccessWith (P.stringOf P.num) "a2134" "" "a2134" 0 0
          testCase "stringOfLength takes characters as long as the parser matches and the length is not yet reached"
          <| fun _ -> assertSuccessWith (P.stringOfLength 3 P.num) "1234abc" "123" "4abc" 0 3
          testCase "stringOfLength fails when the first character doesn't match"
          <| fun _ -> assertFailureAt (P.stringOfLength 3 P.num) "a2134" 0 0
          testCase "stringOfLength fails when the not enough characters match"
          <| fun _ -> assertFailureAt (P.stringOfLength 3 P.num) "12a134" 0 2
          testCase "takeWhile1 takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.takeWhile1 ((=) 'a')) "aaaa123" "aaaa" "123" 0 4
          testCase "takeWhile1 fails when the first character doesn't match"
          <| fun _ -> assertFailureAt (P.takeWhile1 ((=) 'a')) "baaa" 0 0
          testCase "takeWhile takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.takeWhile ((=) 'a')) "aaaa123" "aaaa" "123" 0 4
          testCase "takeWhile succeeds when the first character doesn't match"
          <| fun _ -> assertSuccessWith (P.takeWhile ((=) 'a')) "baaa" "" "baaa" 0 0 ]


let intParserTests =
    testList
        "Int parsers"
        [ testCase "parseInt returns a succeeding parser for a positive int"
          <| fun _ -> assertSuccessWith (P.parseInt "123") "" 123 "" 0 0
          testCase "parseInt returns a succeeding parser for a negative int"
          <| fun _ -> assertSuccessWith (P.parseInt "-123") "" -123 "" 0 0
          testCase "digit succeeds on a digit" <| fun _ -> assertSuccessWith P.digit "1abc" 1 "abc" 0 1
          testCase "digit fails on other characters" <| fun _ -> assertFailuresAt P.digit [ "a" ; ";" ; " " ; "-" ] 0 0
          testCase "nonNegativeInt succeeds for a positive int"
          <| fun _ -> assertSuccessWith P.nonNegativeInt "123ab" 123 "ab" 0 3
          testCase "nonNegativeInt succeeds for zero" <| fun _ -> assertSuccessWith P.nonNegativeInt "0ab" 0 "ab" 0 1
          testCase "nonNegativeInt fails for a negative int" <| fun _ -> assertFailureAt P.nonNegativeInt "-123ab" 0 0 ]

let tests =
    testList
        "Parser tests"
        [ parserTests
          combinatorTests
          regexCombinatorTests
          characterParserTests
          stringParserTests
          intParserTests ]
