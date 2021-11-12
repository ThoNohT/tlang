module tlang.Test.Parser

open Fuchu

module P = tlang.Parser

let assertSuccessWith p input result remaining =
    match P.runParser p (P.Token.prepareString input) with
    | Error e ->
        Test.Fail
        <| sprintf "Expected the parser to succeed, but got error %s at %s." e.Message (P.Position.toString e.Pos)
    | Ok (res, state) ->
        Assert.Equal ("Wrong result.", result, res)
        Assert.Equal ("Wrong remaining.", remaining, P.Token.toString state.Input)

let assertFailure p input =
    match P.parse p input with
    | Error _ -> ()
    | _ -> Test.Fail "Expected the parser to fail."

let assertFailures p inputs =
    for input in inputs do
        assertFailure p input

let parserTests =
    testList
        "Parser"
        [ testCase "succeed succeeds on empty input" <| fun _ -> assertSuccessWith (P.succeed ()) "" () ""
          testCase "succeed succeeds on nonempty input and doesn't consume"
          <| fun _ -> assertSuccessWith (P.succeed ()) "x" () "x"
          testCase "fail fails on empty input" <| fun _ -> assertFailure (P.fail ()) ""
          testCase "fail fails on nonempty input" <| fun _ -> assertFailure (P.fail ()) "x"
          testCase "all succeeds on empty input and consumes everything" <| fun _ -> assertSuccessWith P.all "" "" ""
          testCase "all succeeds on nonempty input" <| fun _ -> assertSuccessWith P.all "abc" "abc" "" ]

let combinatorTests =
    testList
        "Combinators"
        [ testCase "inv fails on success" <| fun _ -> assertFailure (P.inv (P.succeed ())) ""
          testCase "inv succeeds on failure" <| fun _ -> assertSuccessWith (P.inv (P.fail ())) "abc" () "abc"
          testCase "peek succeeds but doesn't consume" <| fun _ -> assertSuccessWith (P.peek P.pChar) "abc" () "abc"
          testCase "peek fails on failure" <| fun _ -> assertFailure (P.peek P.num) "abc"
          testCase "check fails on failure" <| fun _ -> assertFailure (P.check (fun _ -> true) P.num) "abc"
          testCase "check fails on negative check" <| fun _ -> assertFailure (P.check (fun c -> c = 'x') P.pChar) "abc"
          testCase "check succeeds on positive check"
          <| fun _ -> assertSuccessWith (P.check (fun _ -> true) P.pChar) "abc" 'a' "bc"
          testCase "sequence uses the provided combine function"
          <| fun _ -> assertSuccessWith (P.sequence (-) P.digit P.digit) "642" 2 "2"
          testCase "sequence fails when the first parser fails"
          <| fun _ -> assertFailure (P.sequence (-) P.digit P.digit) "a42"
          testCase "sequence fails when the second parser fails"
          <| fun _ -> assertFailure (P.sequence (-) P.digit P.digit) "6a2"
          testCase "andThen uses the provided combine function"
          <| fun _ -> assertSuccessWith (P.andThen (-) P.digit P.digit) "642" -2 "2"
          testCase "andThen fails when the first parser fails"
          <| fun _ -> assertFailure (P.andThen (-) P.digit P.digit) "a42"
          testCase "andThen fails when the second parser fails"
          <| fun _ -> assertFailure (P.andThen (-) P.digit P.digit) "6a2"
          testCase "skipNext skips the second parser"
          <| fun _ -> assertSuccessWith (P.skipNext P.digit P.digit) "642" 6 "2"
          testCase "andThen fails when the first parser fails"
          <| fun _ -> assertFailure (P.skipNext P.digit P.digit) "a42"
          testCase "skipNext fails when the second parser fails"
          <| fun _ -> assertFailure (P.skipNext P.digit P.digit) "6a2"
          testCase "skipPrev skips the second parser"
          <| fun _ -> assertSuccessWith (P.skipPrev P.digit P.digit) "642" 4 "2"
          testCase "andThen fails when the first parser fails"
          <| fun _ -> assertFailure (P.skipPrev P.digit P.digit) "a42"
          testCase "skipPrev fails when the second parser fails"
          <| fun _ -> assertFailure (P.skipPrev P.digit P.digit) "6a2"
          testCase "map applies the mapping" <| fun _ -> assertSuccessWith (P.map (fun x -> x + 1) P.digit) "1a" 2 "a"
          testCase "map fails on failure" <| fun _ -> assertFailure (P.map (fun x -> x + 1) P.digit) "ab"
          testCase "alt tries the first parser first"
          <| fun _ -> assertSuccessWith (P.alt (P.lit "abc") (P.lit "ab")) "abcd" "abc" "d"
          testCase "alt falls back to the second parser"
          <| fun _ -> assertSuccessWith (P.alt (P.lit "abc") (P.lit "abd")) "abde" "abd" "e"
          testCase "alt fails when all parsers fail" <| fun _ -> assertFailure (P.alt (P.lit "a") (P.lit "b")) "c"
          testCase "oneOf tries the first parser first"
          <| fun _ -> assertSuccessWith (P.oneOf [ P.lit "abc" ; P.lit "ab" ; P.lit "a" ]) "abcd" "abc" "d"
          testCase "oneOf falls back to the second parser"
          <| fun _ -> assertSuccessWith (P.oneOf [ P.lit "abc" ; P.lit "abd" ; P.lit "a" ]) "abde" "abd" "e"
          testCase "oneOf falls back to the third parser"
          <| fun _ -> assertSuccessWith (P.oneOf [ P.lit "abc" ; P.lit "abd" ; P.lit "ab" ]) "abff" "ab" "ff"
          testCase "oneOf fails when all parsers fail"
          <| fun _ -> assertFailure (P.oneOf [ P.lit "a" ; P.lit "b" ; P.lit "c" ]) "d" ]

let regexCombinatorTests =
    testList
        "Regex combinators"
        [ testCase "star consumes no input when the first occurrence fails"
          <| fun _ -> assertSuccessWith (P.star P.digit) "a123" [] "a123"
          testCase "star consumes input until the first occurrence that fails"
          <| fun _ -> assertSuccessWith (P.star P.digit) "123a" [ 1 ; 2 ; 3 ] "a"
          testCase "plus fails when the first occurrence fails" <| fun _ -> assertFailure (P.plus P.digit) "a123"
          testCase "plus consumes input until the first occurrence that fails"
          <| fun _ -> assertSuccessWith (P.plus P.digit) "123a" [ 1 ; 2 ; 3 ] "a"
          testCase "times fails when the too few instances succeed" <| fun _ -> assertFailure (P.times 4 P.digit) "123"
          testCase "times doesn't consume more than the specied times"
          <| fun _ -> assertSuccessWith (P.times 2 P.digit) "1234" [ 1 ; 2 ] "34"
          testCase "optional consumes no input when the first occurrence fails"
          <| fun _ -> assertSuccessWith (P.optional P.digit) "a123" None "a123"
          testCase "optional consumes input only once"
          <| fun _ -> assertSuccessWith (P.optional P.digit) "123a" (Some 1) "23a" ]

let characterParserTests =
    testList
        "Character parsers"
        [ testCase "pChar fails on empty input" <| fun _ -> assertFailure P.pChar ""
          testCase "pChar succeeds on nonempty input" <| fun _ -> assertSuccessWith P.pChar "abc" 'a' "bc"
          testCase "num succeeds on a number" <| fun _ -> assertSuccessWith P.num "123" '1' "23"
          testCase "num fails with non-numeric characters" <| fun _ -> assertFailures P.num [ "a" ; "-" ; ";" ; " " ]
          testCase "num fails on empty input" <| fun _ -> assertFailure P.num ""
          testCase "alpha succeeds on a letter" <| fun _ -> assertSuccessWith P.alpha "abc" 'a' "bc"
          testCase "alpha fails with non-alpha characters" <| fun _ -> assertFailures P.alpha [ "1" ; "-" ; ";" ; " " ]
          testCase "alpha fails on empty input" <| fun _ -> assertFailure P.alpha ""
          testCase "alphaNum succeeds on a letter" <| fun _ -> assertSuccessWith P.alphaNum "abc" 'a' "bc"
          testCase "alphaNum succeeds on a number" <| fun _ -> assertSuccessWith P.alphaNum "123" '1' "23"
          testCase "alphaNum fails with non-alphaNumeric characters"
          <| fun _ -> assertFailures P.alphaNum [ "-" ; ";" ; " " ]
          testCase "alphaNum fails on empty input" <| fun _ -> assertFailure P.alphaNum ""
          testCase "litC succeeds on the exact character" <| fun _ -> assertSuccessWith (P.litC 'a') "abc" 'a' "bc"
          testCase "litC fails on any other character"
          <| fun _ -> assertFailures (P.litC 'a') [ "bc" ; "123" ; "f" ; " " ; ";" ]
          testCase "litC fails on empty input" <| fun _ -> assertFailure (P.litC 'a') ""
          testCase "litC with end of line" <| fun _ -> assertSuccessWith (P.litC '\n') "\nabc" '\n' "abc" ]

let stringParserTests =
    testList
        "String parsers"
        [ testCase "lit succeeds on the exact input" <| fun _ -> assertSuccessWith (P.lit "abc") "abcd" "abc" "d"
          testCase "lit fails on the empty input" <| fun _ -> assertFailure (P.lit "abc") ""
          testCase "eoi succeeds on empty input" <| fun _ -> assertSuccessWith P.eoi "" () ""
          testCase "eoi fails when there is more input" <| fun _ -> assertFailures P.eoi [ "a" ; " " ; "\n" ]
          testCase "eol succeeds at the end of a line" <| fun _ -> assertSuccessWith P.eol "\nabc" '\n' "abc"
          testCase "eol fails on any other input" <| fun _ -> assertFailures P.eol [ "" ; "a" ; "\t" ; " " ]
          testCase "line succeeds when the parser leaves a newline"
          <| fun _ -> assertSuccessWith (P.line P.digit) "5\n5" 5 "5"
          testCase "line succeeds when the parser leaves no input"
          <| fun _ -> assertSuccessWith (P.line P.digit) "5" 5 ""
          testCase "line fails when the parser doesnt leave end of line or input"
          <| fun _ -> assertFailures (P.line P.digit) [ "5a" ; "5 " ; "5-" ]
          testCase "line fails if parser fails" <| fun _ -> assertFailures (P.line P.digit) [ "a\n" ; "\n" ]
          testCase "separated succeeds when multiple items are separated"
          <| fun _ -> assertSuccessWith (P.separated P.digit P.alpha) "a1b2c3d456" [ 'a' ; 'b' ; 'c' ; 'd' ] "456"
          testCase "separated fails when the parser never matches"
          <| fun _ -> assertFailure (P.separated (P.litC ';') P.digit) "a;b;c"
          testCase "separated returns the first element when separators never match"
          <| fun _ -> assertSuccessWith (P.separated (P.litC ';') P.digit) "1.2.3" [ 1 ] ".2.3"
          testCase "separated_ succeeds when multiple items are separated_"
          <| fun _ -> assertSuccessWith (P.separated_ "1" P.alpha) "a1b1c1d156" [ 'a' ; 'b' ; 'c' ; 'd' ] "156"
          testCase "separated_ fails when the parser never matches"
          <| fun _ -> assertFailure (P.separated_ ";" P.digit) "a;b;c"
          testCase "separated_ returns the first element when separators never match"
          <| fun _ -> assertSuccessWith (P.separated_ ";" P.digit) "1.2.3" [ 1 ] ".2.3"
          testCase "stringof takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.stringOf P.num) "1234abc" "1234" "abc"
          testCase "stringof fails when the first character doesn't match"
          <| fun _ -> assertFailure (P.stringOf P.num) "a2134"
          testCase "stringOf_ takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.stringOf_ P.num) "1234abc" "1234" "abc"
          testCase "stringOf_ succeeds when the first character doesn't match"
          <| fun _ -> assertSuccessWith (P.stringOf_ P.num) "a2134" "" "a2134"
          testCase "stringOfLength takes characters as long as the parser matches and the length is not yet reached"
          <| fun _ -> assertSuccessWith (P.stringOfLength 3 P.num) "1234abc" "123" "4abc"
          testCase "stringOfLength fails when the first character doesn't match"
          <| fun _ -> assertFailure (P.stringOfLength 3 P.num) "a2134"
          testCase "stringOfLength fails when the not enough characters match"
          <| fun _ -> assertFailure (P.stringOfLength 3 P.num) "12a134"
          testCase "takeWhile takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.takeWhile ((=) 'a')) "aaaa123" "aaaa" "123"
          testCase "takeWhile fails when the first character doesn't match"
          <| fun _ -> assertFailure (P.takeWhile ((=) 'a')) "baaa"
          testCase "takeWhile_ takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.takeWhile_ ((=) 'a')) "aaaa123" "aaaa" "123"
          testCase "takeWhile_ succeeds when the first character doesn't match"
          <| fun _ -> assertSuccessWith (P.takeWhile_ ((=) 'a')) "baaa" "" "baaa" ]


let intParserTests =
    testList
        "Int parsers"
        [ testCase "parseInt returns a succeeding parser for a positive int"
          <| fun _ -> assertSuccessWith (P.parseInt "123") "" 123 ""
          testCase "parseInt returns a succeeding parser for a negative int"
          <| fun _ -> assertSuccessWith (P.parseInt "-123") "" -123 ""
          testCase "digit succeeds on a digit" <| fun _ -> assertSuccessWith P.digit "1abc" 1 "abc"
          testCase "digit fails on other characters" <| fun _ -> assertFailures P.digit [ "a" ; ";" ; " " ; "-" ]
          testCase "nonNegativeInt succeeds for a positive int"
          <| fun _ -> assertSuccessWith P.nonNegativeInt "123ab" 123 "ab"
          testCase "nonNegativeInt succeeds for zero" <| fun _ -> assertSuccessWith P.nonNegativeInt "0ab" 0 "ab"
          testCase "nonNegativeInt fails for a negative int" <| fun _ -> assertFailure P.nonNegativeInt "-123ab" ]

let tests =
    testList
        "Parser tests"
        [ parserTests
          combinatorTests
          regexCombinatorTests
          characterParserTests
          stringParserTests
          intParserTests ]
