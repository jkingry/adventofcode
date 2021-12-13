open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2020

let days =
    [ 1, 1, Day01.part1, None
      1, 2, Day01.part2, None

      2, 1, Day02.part1, None
      2, 2, Day02.part2, None

      3, 1, Day03.part1, None
      3, 2, Day03.part2, None

      17, 1, Day17.part1, None
      17, 2, Day17.part2, None

      18, 1, Day18.part1, None
      18, 2, Day18.part2, None

      19, 1, Day19.part1, None
      19, 2, Day19.part2, None

      20, 1, Day20.part1, None
      20, 2, Day20.part2, None

      21, 1, Day21.part1, None
      21, 2, Day21.part2, None

      22, 1, Day22.part1, None
      22, 2, Day22.part2, None

      23, 1, Day23.part1, None
      23, 2, Day23.part2, None

      24, 1, Day24.part1, None
      24, 2, Day24.part2, None ]
    |> List.map
        (fun (d, p, r, e) ->
            { day = d
              part = p
              run = r
              expected = e })

runCommandLine days
