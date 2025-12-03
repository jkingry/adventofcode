namespace AdventOfCode.FSharp.Y2025

// Day 3: Lobby

module Day03 =
    open AdventOfCode.FSharp.Util
    open System

    let getJoltageSpan digitCount (line: ReadOnlySpan<byte>) =
        let digits = Array.zeroCreate digitCount

        for i = 0 to line.Length - 1 do
            let n = line[i] - '0'B |> int64

            (*
                can only assign digits that there is enough room left to assign to

                digitCount = 2
                line.Length = 9
                              i = 7
                line = 012345678
                              01
                            j <- j should be 0
                            2 - (9 - 7) = 0
                               i
                line = 012345678
                              01
                            j <- j should be 1
                            2 - (9 - 8) = 1

            *)

            let mutable j = digitCount - (line.Length - i) |> max 0

            while j < digitCount do
                if n > digits[j] then
                    digits[j] <- n

                    for k = j + 1 to digitCount - 1 do
                        digits[k] <- 0

                    j <- digitCount

                j <- j + 1

        let mutable joltage = 0L
        let mutable magnitude = 1L

        for j = digitCount - 1 downto 0 do
            joltage <- joltage + digits[j] * magnitude
            magnitude <- magnitude * 10L

        joltage

    let runSpan (input: byte array) (output: int -> string -> unit) =
        let inputSpan = ReadOnlySpan<byte> input
        let mutable part1 = 0L
        let mutable part2 = 0L

        for r in inputSpan.Split '\n'B do
            let line = trySlice r inputSpan

            if line.Length > 0 then
                part1 <- part1 + getJoltageSpan 2 line
                part2 <- part2 + getJoltageSpan 12 line

        part1 |> string |> output 1
        part2 |> string |> output 2

    let parseBatteries = Array.map (fun c -> c - '0'B |> int)

    let rec checkDigit digitIndex n digits =
        if digitIndex = 0 then
            []
        else
            match digits with
            | [] -> [ n ]
            | x :: _ when n > x -> [ n ]
            | x :: xs -> x :: checkDigit (digitIndex - 1) n xs

    let getJoltage digitCount batteries =

        let offset = digitCount - (batteries |> Array.length)

        let updateDigits digits (index, n) =
            let prefixLength = offset + index |> max 0
            let remainderLength = digitCount - prefixLength

            let prefix, suffix = digits |> List.splitAt prefixLength

            let suffix' = checkDigit remainderLength n suffix

            List.append prefix suffix'

        batteries
        |> Array.indexed
        |> Array.fold updateDigits []
        |> List.fold (fun total n -> total * 10L + int64 n) 0L

    let runFunc input (output: int -> string -> unit) =
        let part1, part2 =
            input
            |> bsplit '\n'B
            |> Array.map (fun line ->
                let batteries = parseBatteries line
                getJoltage 2 batteries, getJoltage 12 batteries)
            |> Array.fold (fun (part1, part2) (a, b) -> part1 + a, part2 + b) (0L, 0L)

        part1 |> string |> output 1
        part2 |> string |> output 2
