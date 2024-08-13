namespace AdventOfCode.FSharp.Y2017

// Day 1: Inverse Captcha https://adventofcode.com/2017/day/1
module Day01 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        input
        |> Array.append [| input[0] |]
        |> Array.pairwise
        |> Array.fold (fun total (a, b) -> total + if a = b then (a - '0'B) |> int else 0) 0
        |> string
        |> output 1

        let offset = input.Length / 2

        input
        |> Seq.indexed
        |> Seq.map (fun (index, v) ->
            let otherIndex = (index + offset) % (input.Length - 1)
            if v = input[otherIndex] then int (v - '0'B) else 0)
        |> Seq.sum
        |> string
        |> output 2


    let totalMatching (offset: int) (values: byte array) =
        let mutable total = 0

        for index = 0 to values.Length - 1 do
            let matchIndex = (index + offset) % (values.Length - 1)

            if values[index] = values[matchIndex] then
                total <- total + int (values[index] - '0'B)

        total

    let runFast (input: byte array) (output: int -> string -> unit) =
        input |> totalMatching 1 |> string |> output 1
        input |> totalMatching (input.Length / 2) |> string |> output 2
