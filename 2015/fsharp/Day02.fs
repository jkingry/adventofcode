namespace AdventOfCode.FSharp.Y2015

// Day 2: I Was Told There Would Be No Math
module Day02 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let mutable pos = 0
        let mutable total = 0

        while pos < input.Length do
            let p, a = parseIntToDelim input pos 'x'B
            let p, b = parseIntToDelim input p 'x'B
            let p, c = parseIntToAny input p
            pos <- p + 1
            let s = [| a; b; c |] |> Array.sort
            total <- total + (2 * a * b) + (2 * b * c) + (2 * c * a) + (s[0] * s[1])

        total |> string |> output 1
