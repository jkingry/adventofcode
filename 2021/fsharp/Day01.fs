namespace AdventOfCode.FSharp.Y2021

// Day 1: Sonar Sweep
module Day01 =
    open AdventOfCode.FSharp.Util

    let increasing input =
        input
        |> Seq.pairwise
        |> Seq.filter (fun (a, b) -> (b > a))
        |> Seq.length

    let part1 input =
        input
        |> splitLine
        |> Seq.map int
        |> increasing
        |> string

    let part2 input =
        input
        |> splitLine
        |> Seq.map int
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> increasing
        |> string
        
    let run input (output: int -> string -> unit) =
        let numbers =
            input
            |> splitLine
            |> Array.map int

        let part1 =
            numbers
            |> increasing
            |> string
        output 1 part1

        let part2 =
            numbers
            |> Seq.windowed 3
            |> Seq.map Seq.sum
            |> increasing
            |> string
        output 2 part2
