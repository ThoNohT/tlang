module tlang.Test.Run

open Fuchu
open tlang.Test

let runTests () = runParallel <| testList "All" [ Parser.tests ]
