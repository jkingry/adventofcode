namespace AdventOfCode.FSharp.Y2015

// Day 8: Matchsticks
module Day08 =
    open AdventOfCode.FSharp.Util

    let meaureString (x: string) = 2

    let run (input: byte array) (output: int -> string -> unit) =
        input
        |> text
        |> splitLine
        |> Array.map meaureString
        |> Array.sum
        |> string
        |> output 1

        2 |> string |> output 2
