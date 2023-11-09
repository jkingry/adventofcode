namespace AdventOfCode.FSharp.Y2021

// Day 6: Lanternfish
module Day06 =
    open AdventOfCode.FSharp.Util

    let simulate offset days gestationTime ages =
        let maxAge = Array.length ages

        for day = 0 to days - 1 do
            let birthDay = (day + offset) % maxAge
            let nextBirthDay = ((day + offset) + gestationTime) % maxAge

            ages.[nextBirthDay] <- ages.[nextBirthDay] + ages.[birthDay]

    let part1 (input: string) =
        let ages: int64[] = Array.zeroCreate 9

        ints input
        |> Array.groupBy id
        |> Array.iter (fun (k, v) -> ages.[k] <- v.LongLength)

        simulate 0 80 7 ages

        Array.sum ages |> string

    let part2 (input: string) =
        let ages: int64[] = Array.zeroCreate 9

        ints input
        |> Array.groupBy id
        |> Array.iter (fun (k, v) -> ages.[k] <- v.LongLength)

        simulate 0 256 7 ages

        Array.sum ages |> string

    let runOld (input: byte array) (output: int -> string -> unit) =
        let inputText = input |> text
        inputText |> part1 |> output 1
        inputText |> part2 |> output 2

    let run (input: byte array) (output: int -> string -> unit) =
        let ages: int64[] = Array.zeroCreate 9

        let mutable i = 0

        while i < (input.Length - 1) do
            let (ni, v) = parseIntToAny input i
            ages.[v] <- ages.[v] + 1L
            i <- ni

        simulate 0 80 7 ages

        Array.sum ages |> string |> output 1

        simulate 80 (256 - 80) 7 ages

        Array.sum ages |> string |> output 2
