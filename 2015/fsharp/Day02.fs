namespace AdventOfCode.FSharp.Y2015

// Day 2: I Was Told There Would Be No Math https://adventofcode.com/2015/day/2
module Day02 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let mutable pos = 0
        let mutable total = 0
        let mutable ribbon = 0

        while pos < input.Length do
            let p, a = parseIntToDelim input pos 'x'B
            let p, b = parseIntToDelim input p 'x'B
            let p, c = parseIntToAny input p
            pos <- p + 1
            let m = [ a; b; c ] |> List.sort
            let m1 = m[0]
            let m2 = m[1]

            total <- total + (2 * a * b) + (2 * b * c) + (2 * c * a) + (m1 * m2)

            ribbon <- ribbon + (2 * m1) + (2 * m2) + (a * b * c)

        total |> string |> output 1
        ribbon |> string |> output 2

    open System

    let runSpan (input: byte array) (output: int -> string -> unit) =
        let inputSpan = ReadOnlySpan<byte> input

        let parts = Array.zeroCreate 3
        let mutable part1 = 0
        let mutable part2 = 0

        for sliceRange in inputSpan.Split '\n'B do
            let slice = trySlice sliceRange inputSpan

            if slice.Length > 0 then
                let mutable partIndex = 0

                for partRange in slice.Split 'x'B do
                    let partSlice = trySlice partRange slice

                    if partSlice.Length > 0 then
                        parts[partIndex] <- parseIntSpan partSlice
                        partIndex <- partIndex + 1

                let a = parts[0]
                let b = parts[1]
                let c = parts[2]

                let s = parts |> Array.sort
                let m1 = s[0]
                let m2 = s[1]

                part1 <- part1 + 2 * a * b + 2 * b * c + 2 * c * a + m1 * m2
                part2 <- part2 + 2 * m1 + 2 * m2 + a * b * c

        part1 |> string |> output 1
        part2 |> string |> output 2
