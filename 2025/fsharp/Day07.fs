namespace AdventOfCode.FSharp.Y2025

// Day 7: Laboratories
module Day07 =
    open AdventOfCode.FSharp.Util
    open Checked

    let analyzeManifold (lines: byte[][]) =
        let mutable beams = Array.zeroCreate lines[0].Length
        let mutable next = beams |> Array.copy
        let mutable zero = beams |> Array.copy

        let mutable splitCount = 0

        for line in lines do
            for index = 0 to line.Length - 1 do
                match line[index] with
                | 'S'B -> next[index] <- 1L
                | '^'B when beams[index] > 0 ->
                    splitCount <- splitCount + 1
                    next[index - 1] <- next[index - 1] + beams[index]
                    next[index + 1] <- next[index + 1] + beams[index]
                    next[index] <- 0L
                | _ -> next[index] <- next[index] + beams[index]

            Array.blit next 0 beams 0 beams.Length
            Array.blit zero 0 next 0 next.Length

        splitCount, beams |> Array.sum

    let run (input: byte array) (output: int -> string -> unit) =
        let manifold = input |> bsplit '\n'B

        let part1, part2 = manifold |> analyzeManifold
        part1 |> string |> output 1
        part2 |> string |> output 2
