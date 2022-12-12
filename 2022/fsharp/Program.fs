open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2022

let days =
    [ 
      01, [ Day01.run; Day01.runFast ]
      02, [ Day02.run; Day02.runFast ]
      03, [ Day03.run ]
      04, [ Day04.run ]
      05, [ Day05.run ]
      06, [ Day06.run ]
      07, [ Day07.run ]
      08, [ Day08.run ]
      09, [ Day09.run; Day09.runFast ]
      10, [ Day10.run; Day10.runFast ]
      11, [ Day11.run; Day11.runFast ]
    ]
    |> List.map (fun (d, r ) ->
            { year = 2022
              day = d
              runs = r })

runCommandLine days
