namespace AdventOfCode.FSharp.Y2015

// Day 1: Not Quite Lisp https://adventofcode.com/2015/day/1
module Day01 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let part1, part2 =
            input
            |> Array.indexed
            |> Array.fold
                (fun (floor, basementPosition) (i, b) ->
                    let floor' = floor + if b = '('B then 1 else -1

                    let basementPosition' =
                        match basementPosition, floor' with
                        | None, -1 -> i + 1 |> Some
                        | _ -> basementPosition

                    floor', basementPosition')
                (0, None)

        part1 |> string |> output 1
        part2.Value |> string |> output 2
