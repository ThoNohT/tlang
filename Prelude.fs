namespace tlang

[<AutoOpen>]
module Prelude =
    /// Flips the first two paramters of a function.
    let flip f a b = f b a
