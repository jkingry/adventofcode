namespace AdventOfCode.FSharp.Y2016

// Day 6: Signals and Noise
module Day06 =
    open AdventOfCode.FSharp.Util

    let largestValueKey map =
        map |> Map.toSeq |> Seq.maxBy snd |> fst

    let smallestValueKey map =
        map |> Map.toSeq |> Seq.minBy snd |> fst

    let run (input: byte array) output =
        let lines = input |> text |> splitLine

        let msgLen = lines[0].Length

        let frequencies =
            lines
            |> Array.fold
                (fun maps line -> maps |> Array.mapi (fun i map -> map |> Counter.incr line[i]))
                (Array.create msgLen Map.empty)

        let part1 = frequencies |> Array.map largestValueKey
        let part2 = frequencies |> Array.map smallestValueKey

        System.String part1 |> output 1
        System.String part2 |> output 2
