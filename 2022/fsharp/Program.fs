open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2022

let inline E v = v |> string |> Some

let days =
    [ 
      01, Day01.run, E 74394, E 212836
      02, Day02.run, E 11386, E 13600
      03, Day03.run, E 8072, E 2567
      04, Day04.run, E 471, E 888
      05, Day05.run, E "PTWLTDSJV", E "WZMFVGGZP"
      06, Day06.run, None, None
    ]
    |> List.map (fun (d, r, e1, e2) ->
            { day = d
              run = r
              expected = [ (1,e1); (2,e2) ] |> Map.ofList })

runCommandLine days
