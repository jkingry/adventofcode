open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2022

let days =
    [ 
      01, [ Day01.run ]
      02, [ Day02.run ]
      03, [ Day03.run ]
      04, [ Day04.run ]
      05, [ Day05.run ]
      06, [ Day06.run ]
      07, [ Day07.run ]
      08, [ Day08.run ]
      09, [ Day09.run; Day09Simple.run ]
      10, [ Day10.run ]
    ]
    |> List.map (fun (d, r ) ->
            { day = d
              runs = r })

runCommandLine days
