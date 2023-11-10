namespace AdventOfCode.FSharp.Y2019

// The Tyranny of the Rocket Equation
module Day01 =
    open System
    open AdventOfCode.FSharp.Util

    let part1 (input: string) =
        let f x = max ((x / 3) - 2) 0
        let q = input |> Seq.map int |> Seq.map f
        q |> Seq.sum |> string

    let part2 (input: string) =
        let f x = max ((x / 3) - 2) 0

        let g x =
            let y = f x
            if y > 0 then Some(y, y) else None

        let h x = (List.unfold g x) |> List.sum

        input |> splitLine |> Seq.map int |> Seq.map h |> Seq.sum |> string

    let run (input: byte array) output =
        let textInput = text input
        part1 textInput |> output 1
        part2 textInput |> output 2
