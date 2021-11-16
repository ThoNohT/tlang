module tlang.Test.Run

open Fuchu
open tlang.Test

let runTests () =
    testList "All" [ Parser.tests ]
    |> runParallel 

