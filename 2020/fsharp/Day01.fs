namespace AdventOfCode.FSharp.Y2020

module Day01 =
    open AdventOfCode.FSharp.Util

    let parse input = input |> splitLine |> Array.map int

    let part1 input =
        let x = input |> parse

        Array.allPairs x x
        |> Seq.pick (fun (a, b) -> if a <> b && (a + b) = 2020 then Some(a * b) else None)
        |> string

    let part2 input =
        let x = input |> parse

        Array.allPairs x x
        |> Seq.allPairs x
        |> Seq.pick (fun (a, (b, c)) ->
            if a <> b && a <> c && (a + b + c) = 2020 then
                Some(a * b * c)
            else
                None)
        |> string

    let run (input: byte array) output =
        let textInput = text input
        part1 textInput |> output 1
        part2 textInput |> output 2
