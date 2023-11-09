open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2019

let inline E v = v |> string |> Some

let inline old (part1: string -> string) (part2: string -> string) =
    [ fun data output ->
          let text = AdventOfCode.FSharp.Util.text data
          part1 text |> output 1
          part2 text |> output 2 ]

let days =
    [ 01, old Day01.part1 Day01.part2 ]
    |> List.map (fun (d, r) -> { year = 2019; day = d; runs = r })

runCommandLine days
