namespace AdventOfCode.FSharp.Y2025

// Day 9: Movie Theater
module Day09 =
    open AdventOfCode.FSharp.Util

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

        "not implemented" |> output 1
        "not implemented" |> output 2
