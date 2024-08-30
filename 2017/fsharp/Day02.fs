namespace AdventOfCode.FSharp.Y2017

// Day 2: Corruption Checksum https://adventofcode.com/2017/day/2
module Day02 =
    open AdventOfCode.FSharp.Util

    let part1CheckSumRow (input: byte array) =
        let values = input |> parseInts
        let minValue = values |> Array.reduce min
        let maxValue = values |> Array.reduce max
        maxValue - minValue

    let part2CheckSumRow (input: byte array) =
        let values = input |> parseInts

        Array.allPairs values values
        |> Array.choose (fun (a, b) -> if a > b && a % b = 0 then Some(a / b) else None)
        |> Array.head

    let run (input: byte array) (output: int -> string -> unit) =
        let rows = input |> bsplit '\n'B

        rows
        |> Array.fold (fun a row -> a + part1CheckSumRow row) 0
        |> string
        |> output 1

        rows
        |> Array.fold (fun a row -> a + part2CheckSumRow row) 0
        |> string
        |> output 2
