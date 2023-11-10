namespace AdventOfCode.FSharp.Y2018

module Day01 =
    open System
    open AdventOfCode.FSharp.Util

    let parse input =
        input |> splitLine |> Seq.map Int32.Parse |> Seq.toArray

    let part1 (input: string) = parse input |> Seq.sum |> string

    let part2 (input: string) =
        let x = parse input

        let mutable dupe: int option = None
        let mutable found = Set.empty
        let mutable sum = 0

        while dupe.IsNone do
            for y in x do
                sum <- sum + y

                if Set.contains sum found then
                    dupe <- Some sum

                found <- Set.add sum found

        dupe.Value |> string


    let part3 (input: string) =
        let x = parse input

        let rec f i sum found =
            let ni = i % x.Length
            let nsum = x[ni] + sum

            if Set.contains nsum found then
                nsum
            else
                f (ni + 1) nsum (Set.add nsum found)

        f 0 0 Set.empty |> string

    let run (input: byte array) output =
        let textInput = text input
        part1 textInput |> output 1
        part2 textInput |> output 2
