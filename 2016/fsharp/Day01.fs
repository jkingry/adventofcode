namespace AdventOfCode.FSharp.Y2016

module Day01 =
    open System
    open AdventOfCode.FSharp.Util

    let parse input =
        input |> splitLine |> Array.map int

    let part1 input =
        let x = input |> parse

        Array.allPairs x x
        |> Seq.filter (fun (a, b) -> a <> b && (a + b) = 2020)
        |> Seq.map (fun (a, b) -> a * b)
        |> Seq.head
        |> bigint
        |> string

    let part2 input =
        let x = input |> parse

        Array.allPairs x x
        |> Array.allPairs x
        |> Seq.filter (fun (a, (b, c)) -> a <> b && a <> c && (a + b + c) = 2020)
        |> Seq.map (fun (a, (b, c)) -> a * b * c)
        |> Seq.head
        |> bigint
        |> string
