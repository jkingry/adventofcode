namespace AdventOfCode.FSharp.Y2019

// The Tyranny of the Rocket Equation
module Day01 =
    open System
    open AdventOfCode.FSharp.Util

    let part1 inputs =
        let f x = max ((x / 3) - 2) 0
        let q = inputs |> Seq.map f
        q |> Seq.sum

    let part2 inputs =
        let f x = max ((x / 3) - 2) 0

        let g x =
            let y = f x
            if y > 0 then Some(y, y) else None

        let h x = 
            List.unfold g x 
            |> List.sum

        inputs |> Seq.map h |> Seq.sum

    let run (input: byte array) output =
        let nums = parseInts input
        part1 nums |> string |> output 1
        part2 nums |> string |> output 2
