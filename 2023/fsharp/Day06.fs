namespace AdventOfCode.FSharp.Y2023

// Day 6: Wait For It
module Day06 =
    open AdventOfCode.FSharp.Util

    let parse (input: byte[]) =
        let numberLines =
            input
            |> text
            |> splitLine
            |> Array.map (fun s ->
                s.Split(' ')
                |> Array.tail
                |> Array.filter (not << System.String.IsNullOrWhiteSpace)
                |> Array.map int64)

        let timeLine = numberLines[0]
        let distLine = numberLines[1]

        Array.zip timeLine distLine

    let getBetterTimes (t, d) =
        // d = x * (t - x)
        // 0 = -1 * x^2 + t*x - d
        // x = (-t +/- sqrt(t^2 - 4 * -1 * - d)) / 2 * - 1
        // x = (-t - sqrt(t * t - 4 * d)) / -2
        // x = (-t + sqrt(t * t - 4 * d)) / -2
        let t = t |> float
        let d = d |> float

        let root = sqrt (t * t - 4.0 * d)

        let rootMin = (-t + root) / -2.0
        let rootMax = (-t - root) / -2.0

        let minSolve = floor rootMin |> int64
        let maxSolve = ceil rootMax |> int64
        (maxSolve - minSolve) - 1L

    let run (input: byte[]) (output: int -> string -> unit) =
        let part1 = input |> parse

        part1
        |> Array.fold (fun product td -> product * (getBetterTimes td)) 1L
        |> string
        |> output 1

        let ts, ds =
            part1
            |> Array.fold (fun (ts, ds) (t, d) -> (ts + (string t)), (ds + (string d))) ("", "")

        let part2 = (int64 ts), (int64 ds)

        getBetterTimes part2 |> string |> output 2
