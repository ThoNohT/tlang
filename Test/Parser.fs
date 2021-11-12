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

let assertFailureAt p input line col =
    match P.parse p input with
    | Error e ->
        Assert.Equal ("Wrong line.", line, e.Pos.Line)
        Assert.Equal ("Wrong column.", col, e.Pos.Col)
    | _ -> Test.Fail "Expected the parser to fail."

let assertFailuresAt p inputs line col =
    for input in inputs do
        assertFailureAt p input line col

let parserTests =
    testList
        "Parser"
        [ testCase "succeed succeeds on empty input" <| fun _ -> assertSuccessWith (P.succeed ()) "" () ""
          testCase "succeed succeeds on nonempty input and doesn't consume"
          <| fun _ -> assertSuccessWith (P.succeed ()) "x" () "x"
          testCase "fail fails on empty input" <| fun _ -> assertFailureAt (P.fail ()) "" 1 1
          testCase "fail fails on nonempty input" <| fun _ -> assertFailureAt (P.fail ()) "x" 1 1
          testCase "all succeeds on empty input and consumes everything" <| fun _ -> assertSuccessWith P.all "" "" ""
          testCase "all succeeds on nonempty input" <| fun _ -> assertSuccessWith P.all "abc" "abc" "" ]

let combinatorTests =
    testList
        "Combinators"
        [ testCase "inv fails on success" <| fun _ -> assertFailureAt (P.inv (P.succeed ())) "" 1 1
          testCase "inv succeeds on failure" <| fun _ -> assertSuccessWith (P.inv (P.fail ())) "abc" () "abc"
          testCase "peek succeeds but doesn't consume" <| fun _ -> assertSuccessWith (P.peek P.pChar) "abc" () "abc"
          testCase "peek fails on failure" <| fun _ -> assertFailureAt (P.peek P.num) "abc" 1 1
          testCase "check fails on failure" <| fun _ -> assertFailureAt (P.check (fun _ -> true) P.num) "abc" 1 1
          testCase "check fails on negative check"
          <| fun _ -> assertFailureAt (P.check (fun c -> c = 'x') P.pChar) "abc" 1 1
          testCase "check succeeds on positive check"
          <| fun _ -> assertSuccessWith (P.check (fun _ -> true) P.pChar) "abc" 'a' "bc"
          testCase "sequence uses the provided combine function"
          <| fun _ -> assertSuccessWith (P.sequence (-) P.digit P.digit) "642" 2 "2"
          testCase "sequence fails when the first parser fails"
          <| fun _ -> assertFailureAt (P.sequence (-) P.digit P.digit) "a42" 1 1
          testCase "sequence fails when the second parser fails"
          <| fun _ -> assertFailureAt (P.sequence (-) P.digit P.digit) "6a2" 1 2
          testCase "andThen uses the provided combine function"
          <| fun _ -> assertSuccessWith (P.andThen (-) P.digit P.digit) "642" -2 "2"
          testCase "andThen fails when the first parser fails"
          <| fun _ -> assertFailureAt (P.andThen (-) P.digit P.digit) "a42" 1 1
          testCase "andThen fails when the second parser fails"
          <| fun _ -> assertFailureAt (P.andThen (-) P.digit P.digit) "6a2" 1 2
          testCase "skipNext skips the second parser"
          <| fun _ -> assertSuccessWith (P.skipNext P.digit P.digit) "642" 6 "2"
          testCase "andThen fails when the first parser fails"
          <| fun _ -> assertFailureAt (P.skipNext P.digit P.digit) "a42" 1 1
          testCase "skipNext fails when the second parser fails"
          <| fun _ -> assertFailureAt (P.skipNext P.digit P.digit) "6a2" 1 2
          testCase "skipPrev skips the second parser"
          <| fun _ -> assertSuccessWith (P.skipPrev P.digit P.digit) "642" 4 "2"
          testCase "andThen fails when the first parser fails"
          <| fun _ -> assertFailureAt (P.skipPrev P.digit P.digit) "a42" 1 1
          testCase "skipPrev fails when the second parser fails"
          <| fun _ -> assertFailureAt (P.skipPrev P.digit P.digit) "6a2" 1 2
          testCase "map applies the mapping" <| fun _ -> assertSuccessWith (P.map (fun x -> x + 1) P.digit) "1a" 2 "a"
          testCase "map fails on failure" <| fun _ -> assertFailureAt (P.map (fun x -> x + 1) P.digit) "ab" 1 1
          testCase "alt tries the first parser first"
          <| fun _ -> assertSuccessWith (P.alt (P.lit "abc") (P.lit "ab")) "abcd" "abc" "d"
          testCase "alt falls back to the second parser"
          <| fun _ -> assertSuccessWith (P.alt (P.lit "abc") (P.lit "abd")) "abde" "abd" "e"
          testCase "alt fails when all parsers fail" <| fun _ -> assertFailureAt (P.alt (P.lit "a") (P.lit "b")) "c" 1 1
          testCase "oneOf tries the first parser first"
          <| fun _ -> assertSuccessWith (P.oneOf [ P.lit "abc" ; P.lit "ab" ; P.lit "a" ]) "abcd" "abc" "d"
          testCase "oneOf falls back to the second parser"
          <| fun _ -> assertSuccessWith (P.oneOf [ P.lit "abc" ; P.lit "abd" ; P.lit "a" ]) "abde" "abd" "e"
          testCase "oneOf falls back to the third parser"
          <| fun _ -> assertSuccessWith (P.oneOf [ P.lit "abc" ; P.lit "abd" ; P.lit "ab" ]) "abff" "ab" "ff"
          testCase "oneOf fails when all parsers fail"
          <| fun _ -> assertFailureAt (P.oneOf [ P.lit "a" ; P.lit "b" ; P.lit "c" ]) "d" 1 1 ]

let regexCombinatorTests =
    testList
        "Regex combinators"
        [ testCase "star consumes no input when the first occurrence fails"
          <| fun _ -> assertSuccessWith (P.star P.digit) "a123" [] "a123"
          testCase "star consumes input until the first occurrence that fails"
          <| fun _ -> assertSuccessWith (P.star P.digit) "123a" [ 1 ; 2 ; 3 ] "a"
          testCase "plus fails when the first occurrence fails" <| fun _ -> assertFailureAt (P.plus P.digit) "a123" 1 1
          testCase "plus consumes input until the first occurrence that fails"
          <| fun _ -> assertSuccessWith (P.plus P.digit) "123a" [ 1 ; 2 ; 3 ] "a"
          testCase "times fails when the too few instances succeed"
          <| fun _ -> assertFailureAt (P.times 4 P.digit) "123ab" 1 4
          testCase "times doesn't consume more than the specied times"
          <| fun _ -> assertSuccessWith (P.times 2 P.digit) "1234" [ 1 ; 2 ] "34"
          testCase "optional consumes no input when the first occurrence fails"
          <| fun _ -> assertSuccessWith (P.optional P.digit) "a123" None "a123"
          testCase "optional consumes input only once"
          <| fun _ -> assertSuccessWith (P.optional P.digit) "123a" (Some 1) "23a" ]

let characterParserTests =
    testList
        "Character parsers"
        [ testCase "pChar fails on empty input" <| fun _ -> assertFailureAt P.pChar "" 1 1
          testCase "pChar succeeds on nonempty input" <| fun _ -> assertSuccessWith P.pChar "abc" 'a' "bc"
          testCase "num succeeds on a number" <| fun _ -> assertSuccessWith P.num "123" '1' "23"
          testCase "num fails with non-numeric characters"
          <| fun _ -> assertFailuresAt P.num [ "a" ; "-" ; ";" ; " " ] 1 1
          testCase "num fails on empty input" <| fun _ -> assertFailureAt P.num "" 1 1
          testCase "alpha succeeds on a letter" <| fun _ -> assertSuccessWith P.alpha "abc" 'a' "bc"
          testCase "alpha fails with non-alpha characters"
          <| fun _ -> assertFailuresAt P.alpha [ "1" ; "-" ; ";" ; " " ] 1 1
          testCase "alpha fails on empty input" <| fun _ -> assertFailureAt P.alpha "" 1 1
          testCase "alphaNum succeeds on a letter" <| fun _ -> assertSuccessWith P.alphaNum "abc" 'a' "bc"
          testCase "alphaNum succeeds on a number" <| fun _ -> assertSuccessWith P.alphaNum "123" '1' "23"
          testCase "alphaNum fails with non-alphaNumeric characters"
          <| fun _ -> assertFailuresAt P.alphaNum [ "-" ; ";" ; " " ] 1 1
          testCase "alphaNum fails on empty input" <| fun _ -> assertFailureAt P.alphaNum "" 1 1
          testCase "litC succeeds on the exact character" <| fun _ -> assertSuccessWith (P.litC 'a') "abc" 'a' "bc"
          testCase "litC fails on any other character"
          <| fun _ -> assertFailuresAt (P.litC 'a') [ "bc" ; "123" ; "f" ; " " ; ";" ] 1 1
          testCase "litC fails on empty input" <| fun _ -> assertFailureAt (P.litC 'a') "" 1 1
          testCase "litC with end of line" <| fun _ -> assertSuccessWith (P.litC '\n') "\nabc" '\n' "abc" ]

let stringParserTests =
    testList
        "String parsers"
        [ testCase "lit succeeds on the exact input" <| fun _ -> assertSuccessWith (P.lit "abc") "abcd" "abc" "d"
          testCase "lit fails on the empty input" <| fun _ -> assertFailureAt (P.lit "abc") "" 1 1
          testCase "eoi succeeds on empty input" <| fun _ -> assertSuccessWith P.eoi "" () ""
          testCase "eoi fails when there is more input" <| fun _ -> assertFailuresAt P.eoi [ "a" ; " " ; "\n" ] 1 1
          testCase "eol succeeds at the end of a line" <| fun _ -> assertSuccessWith P.eol "\nabc" '\n' "abc"
          testCase "eol fails on any other input" <| fun _ -> assertFailuresAt P.eol [ "" ; "a" ; "\t" ; " " ] 1 1
          testCase "line succeeds when the parser leaves a newline"
          <| fun _ -> assertSuccessWith (P.line P.digit) "5\n5" 5 "5"
          testCase "line succeeds when the parser leaves no input"
          <| fun _ -> assertSuccessWith (P.line P.digit) "5" 5 ""
          testCase "line fails when the parser doesnt leave end of line or input"
          <| fun _ -> assertFailuresAt (P.line P.digit) [ "5a" ; "5 " ; "5-" ] 1 1
          testCase "line fails if parser fails" <| fun _ -> assertFailuresAt (P.line P.digit) [ "a\n" ; "\n" ] 1 1
          testCase "separated succeeds when multiple items are separated"
          <| fun _ -> assertSuccessWith (P.separated P.digit P.alpha) "a1b2c3d456" [ 'a' ; 'b' ; 'c' ; 'd' ] "456"
          testCase "separated fails when the parser never matches"
          <| fun _ -> assertFailureAt (P.separated (P.litC ';') P.digit) "a;b;c" 1 1
          testCase "separated returns the first element when separators never match"
          <| fun _ -> assertSuccessWith (P.separated (P.litC ';') P.digit) "1.2.3" [ 1 ] ".2.3"
          testCase "separated_ succeeds when multiple items are separated_"
          <| fun _ -> assertSuccessWith (P.separated_ "1" P.alpha) "a1b1c1d156" [ 'a' ; 'b' ; 'c' ; 'd' ] "156"
          testCase "separated_ fails when the parser never matches"
          <| fun _ -> assertFailureAt (P.separated_ ";" P.digit) "a;b;c" 1 1
          testCase "separated_ returns the first element when separators never match"
          <| fun _ -> assertSuccessWith (P.separated_ ";" P.digit) "1.2.3" [ 1 ] ".2.3"
          testCase "stringof takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.stringOf P.num) "1234abc" "1234" "abc"
          testCase "stringof fails when the first character doesn't match"
          <| fun _ -> assertFailureAt (P.stringOf P.num) "a2134" 1 1
          testCase "stringOf_ takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.stringOf_ P.num) "1234abc" "1234" "abc"
          testCase "stringOf_ succeeds when the first character doesn't match"
          <| fun _ -> assertSuccessWith (P.stringOf_ P.num) "a2134" "" "a2134"
          testCase "stringOfLength takes characters as long as the parser matches and the length is not yet reached"
          <| fun _ -> assertSuccessWith (P.stringOfLength 3 P.num) "1234abc" "123" "4abc"
          testCase "stringOfLength fails when the first character doesn't match"
          <| fun _ -> assertFailureAt (P.stringOfLength 3 P.num) "a2134" 1 1
          testCase "stringOfLength fails when the not enough characters match"
          <| fun _ -> assertFailureAt (P.stringOfLength 3 P.num) "12a134" 1 3
          testCase "takeWhile takes characters as long as the parser matches"
          <| fun _ -> assertSuccessWith (P.takeWhile ((=) 'a')) "aaaa123" "aaaa" "123"
          testCase "takeWhile fails when the first character doesn't match"
          <| fun _ -> assertFailureAt (P.takeWhile ((=) 'a')) "baaa" 1 1
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
          testCase "digit fails on other characters" <| fun _ -> assertFailuresAt P.digit [ "a" ; ";" ; " " ; "-" ] 1 1
          testCase "nonNegativeInt succeeds for a positive int"
          <| fun _ -> assertSuccessWith P.nonNegativeInt "123ab" 123 "ab"
          testCase "nonNegativeInt succeeds for zero" <| fun _ -> assertSuccessWith P.nonNegativeInt "0ab" 0 "ab"
          testCase "nonNegativeInt fails for a negative int" <| fun _ -> assertFailureAt P.nonNegativeInt "-123ab" 1 1 ]

let tests =
    testList
        "Parser tests"
        [ parserTests
          combinatorTests
          regexCombinatorTests
          characterParserTests
          stringParserTests
          intParserTests ]
