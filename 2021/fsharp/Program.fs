open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2021

let inline E v = v |> string |> Some

let inline old (part1: string -> string) (part2: string -> string) =
    [ fun data output ->
          let text = AdventOfCode.FSharp.Util.text data
          part1 text |> output 1
          part2 text |> output 2 ]

let days =
    [ 01, [ Day01.run ]
      02, [ Day02.run ]
      03, [ Day03.run ]
      04, [ Day04.run ]
      05, [ Day05.run ]
      06, [ Day06.runOld; Day06.run ]
      07, old Day07.part1 Day07.part2
      08, old Day08.part1 Day08.part2
      09, old Day09.part1 Day09.part2
      10, old Day10.part1 Day10.part2
      11, old Day11.part1 Day11.part2
      12, old Day12.part1 Day12.part2
      13, [ Day13.run ]
      14, [ Day14.run ]
      15, [ Day15.run ]
      16, [ Day16.run ]
      17, [ Day17.run ]
      18, [ Day18.run ]
      19, [ Day19.run ]
      20, [ Day20.run ]
      21, [ Day21.run ]
      22, [ Day22.run ]
      23, [ Day23.run ] ]
    |> List.map (fun (d, r) -> { year = 2021; day = d; runs = r })

runCommandLine days
