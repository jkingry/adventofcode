open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2016

let inline E v = v |> string |> Some

let inline old (part1: string -> string) (part2: string -> string) =
    [ fun data output ->
          let text = AdventOfCode.FSharp.Util.text data
          part1 text |> output 1
          part2 text |> output 2 ]

let days =
    [ 01, [ Day01.run ] ]
    |> List.map (fun (d, r) -> { year = 2016; day = d; runs = r })

runCommandLine days
