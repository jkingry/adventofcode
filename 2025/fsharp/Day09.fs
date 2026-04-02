namespace AdventOfCode.FSharp.Y2025

// Day 9: Movie Theater
module Day09 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: byte array) (output: int -> string -> unit) =
        let redTiles =
            input
            |> bsplit '\n'B
            |> Array.map (
                parseInts
                >> function
                    | [| a; b |] -> a, b
                    | _ -> failwith "invalid line"
            )

        let corners =
            [ redTiles |> Array.minBy (fun (a, b) -> a + b)
              redTiles |> Array.maxBy (fun (a, b) -> a + b)
              redTiles |> Array.minBy fst
              redTiles |> Array.maxBy fst
              redTiles |> Array.minBy snd
              redTiles |> Array.maxBy snd ]

        List.allPairs corners corners
        |> List.filter (fun (a, b) -> a <> b)
        |> List.map (fun ((ax, ay), (bx, by)) ->
            let x = 1 + abs ax - bx |> int64
            let y = 1 + abs ay - by |> int64
            x * y)
        |> List.max
        |> string
        |> output 1

        "not implemented" |> output 2
