namespace AdventOfCode.FSharp.Y2025

// Day 3: Lobby

module Day03 =
    open AdventOfCode.FSharp.Util
    open System

    let getJoltageSpan (line: ReadOnlySpan<byte>) =
        let mutable m1 = -1
        let mutable m2 = -1

        for i = 0 to line.Length - 1 do
            match line[i] - '0'B |> int with
            | n when n > m1 && i < line.Length - 1 ->
                m1 <- n
                m2 <- -1
            | n when n > m2 -> m2 <- n
            | _ -> ()

        m1 * 10 + m2

    let runSpan (input: byte array) (output: int -> string -> unit) =
        let inputSpan = ReadOnlySpan<byte> input
        let mutable part1 = 0

        for r in inputSpan.Split '\n'B do
            let line = trySlice r inputSpan

            if line.Length > 0 then
                part1 <- part1 + getJoltageSpan line

        part1 |> string |> output 1

    let parseBatteries = Array.map (fun c -> c - '0'B |> int)

    let getJoltage batteries =
        let m1, m2 =
            batteries
            |> Array.indexed
            |> Array.fold
                (fun (m1, m2) (i, v) ->
                    match v with
                    | n when n > m1 && i < batteries.Length - 1 -> n, -1
                    | n when n > m2 -> m1, n
                    | _ -> m1, m2)
                (-1, -1)

        m1 * 10 + m2

    let run input output =
        input
        |> bsplit '\n'B
        |> Array.map (parseBatteries >> getJoltage)
        |> Array.sum
        |> string
        |> output 1
