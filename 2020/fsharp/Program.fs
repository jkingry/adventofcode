open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2020

let inline E v = v |> string |> Some

let inline old (part1: string -> string) (part2: string -> string) =
    [ fun (data: byte[]) output ->
          let text = data |> AdventOfCode.FSharp.Util.text
          part1 text |> output 1
          part2 text |> output 2 ]

let days =
    [ 01, old Day01.part1 Day01.part2
      02, old Day02.part1 Day02.part2
      03, old Day03.part1 Day03.part2
      17, old Day17.part1 Day17.part2
      18, old Day18.part1 Day18.part2
      19, old Day19.part1 Day19.part2
      20, old Day20.part1 Day20.part2
      21, old Day21.part1 Day21.part2
      22, old Day22.part1 Day22.part2
      23, old Day23.part1 Day23.part2
      24, [ Day24.run ] ]
    |> List.map (fun (d, r) -> { year = 2020; day = d; runs = r })


runCommandLine days
