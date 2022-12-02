open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2022

let inline E v = v |> string |> Some

let inline old (part1: string -> string) (part2: string -> string) =
  fun text output ->
    part1 text |> output 1
    part2 text |> output 2

let days =
    [ 
      01, Day01.run, E 74394, E 212836
      02, Day02.run, None, None
//      01, Day01.run, E 1676, E 1706
    ]
    |> List.map (fun (d, r, e1, e2) ->
            { day = d
              run = r
              expected = [ (1,e1); (2,e2) ] |> Map.ofList })

runCommandLine days
