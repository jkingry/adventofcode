namespace AdventOfCode.FSharp.Y2015

// Day 2: I Was Told There Would Be No Math
module Day02 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let mutable pos = 0
        let mutable total = 0
        let mutable ribbon = 0

        while pos < input.Length do
            let p, a = parseIntToDelim input pos 'x'B
            let p, b = parseIntToDelim input p 'x'B
            let p, c = parseIntToAny input p
            pos <- p + 1
            let m = [ a; b; c ] |> List.sort
            let m1 = m[0]
            let m2 = m[1]

            total <- total + (2 * a * b) + (2 * b * c) + (2 * c * a) + (m1 * m2)

            ribbon <- ribbon + (2 * m1) + (2 * m2) + (a * b * c)

        total |> string |> output 1
        ribbon |> string |> output 2
