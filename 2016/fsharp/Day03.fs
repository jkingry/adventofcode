namespace AdventOfCode.FSharp.Y2016

// Day 3: Squares With Three Sides
module Day03 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) output =
        let numbers =
            seq {
                let mutable pos = 0

                while pos < input.Length do
                    let (pos', value) = parseIntToAny input pos
                    yield value
                    pos <- pos' + 1
            }
            |> Array.ofSeq

        let isValidTriangle (side: int[]) =
            if side[0] + side[1] > side[2] then 1 else 0

        let countValidTriangles (sides: int[][]) =
            sides |> Array.fold (fun count side -> count + (isValidTriangle side)) 0

        numbers
        |> Array.chunkBySize 3
        |> Array.map Array.sort
        |> countValidTriangles
        |> string
        |> output 1

        numbers
        |> Array.chunkBySize 9
        |> Array.collect (Array.chunkBySize 3 >> Array.transpose)
        |> Array.map Array.sort
        |> countValidTriangles
        |> string
        |> output 2
