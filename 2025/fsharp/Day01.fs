namespace AdventOfCode.FSharp.Y2025

// Day 1: Secret Entrance

module Day01 =
    open AdventOfCode.FSharp.Util
    open System

    let inline rotateDial delta position =
        let rawPosition = position + delta
        let modPosition = rawPosition % 100
        let newPosition = if modPosition < 0 then 100 + modPosition else modPosition

        let zeroClicks =
            if rawPosition > 0 then
                rawPosition / 100
            else
                (rawPosition / 100 |> abs) + if position = 0 then 0 else 1

        newPosition, zeroClicks

    let inline parseLine (line: ReadOnlySpan<byte>) =
        let numberSpan = line.Slice 1

        parseIntSpan numberSpan
        * match line[0] with
          | 'R'B -> 1
          | 'L'B -> -1
          | _ -> failwith "invalid line"

    let runSpan (input: byte array) (output: int -> string -> unit) =
        let inputSpan = ReadOnlySpan<byte> input

        let mutable zeroCount = 0
        let mutable zeroClickCount = 0
        let mutable position = 50

        for r in inputSpan.Split '\n'B do
            let lineSpan = trySlice r inputSpan

            if lineSpan.Length > 0 then
                let delta = parseLine lineSpan

                let newPosition, clickCounts = rotateDial delta position

                position <- newPosition
                zeroClickCount <- zeroClickCount + clickCounts

                if position = 0 then
                    zeroCount <- zeroCount + 1

        zeroCount |> string |> output 1
        zeroClickCount |> string |> output 2

    let run (input: byte array) (output: int -> string -> unit) =
        let part1, part2 =
            input
            |> bsplit '\n'B
            |> Array.scan
                (fun (position, _) line ->
                    let _, delta = parseIntToAny line 1
                    let delta = delta * if line[0] = 'L'B then -1 else 1
                    position |> rotateDial delta)
                (50, 0)
            |> Array.fold
                (fun (totalZeros, totalZeroClicks) (position, zeroClicks) ->
                    let totalZeros = totalZeros + if position = 0 then 1 else 0
                    let totalZeroClicks = totalZeroClicks + zeroClicks
                    totalZeros, totalZeroClicks)
                (0, 0)

        part1 |> string |> output 1
        part2 |> string |> output 2

    let runDumb (input: byte array) (output: int -> string -> unit) =
        let deltas =
            input
            |> bsplit '\n'B
            |> Array.map (fun line ->
                let _, delta = parseIntToAny line 1
                delta * if line[0] = 'L'B then -1 else 1)

        let mutable position = 50
        let mutable zeroCount = 0
        let mutable zeroClicks = 0

        for delta in deltas do
            let mutable d = delta

            if delta > 0 then
                while d <> 0 do
                    position <- (position + 1) % 100
                    d <- d - 1

                    if position = 0 then
                        zeroClicks <- zeroClicks + 1
            else
                while d <> 0 do
                    position <- (position - 1) % 100

                    if position < 0 then
                        position <- 100 + position

                    d <- d + 1

                    if position = 0 then
                        zeroClicks <- zeroClicks + 1

            if position = 0 then
                zeroCount <- zeroCount + 1

        zeroCount |> string |> output 1
        zeroClicks |> string |> output 2
