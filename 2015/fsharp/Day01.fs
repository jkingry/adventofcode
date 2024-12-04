namespace AdventOfCode.FSharp.Y2015

// Day 1: Not Quite Lisp https://adventofcode.com/2015/day/1
module Day01 =
    let run (input: byte array) (output: int -> string -> unit) =
        let struct (part1, part2) =
            input
            |> Array.indexed
            |> Array.fold
                (fun struct (floor, basementPosition) (index, directionChar) ->
                    let floor' =
                        match directionChar with
                        | '('B -> floor + 1
                        | ')'B -> floor - 1
                        | _ -> failwithf "Invalid char: %c" (char directionChar)

                    let basementPosition' =
                        if floor' = -1 && basementPosition < 0 then
                            index + 1
                        else
                            basementPosition

                    floor', basementPosition')
                struct (0, -1)

        part1 |> string |> output 1
        part2 |> string |> output 2
