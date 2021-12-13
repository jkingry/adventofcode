namespace AdventOfCode.FSharp.Y2021

open System

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
        |> Seq.map Int32.Parse
        |> increasing
        |> string

    let part2 input =
        input
        |> splitLine
        |> Seq.map Int32.Parse
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> increasing
        |> string
