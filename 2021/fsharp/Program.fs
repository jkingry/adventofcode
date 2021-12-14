open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2021

open Checked

let inline E v = v |> string |> Some

let days =
    [ 1, 1, Day01.part1, E 1676
      1, 2, Day01.part2, E 1706

      2, 1, Day02.part1, E 2036120
      2, 2, Day02.part2, E 2015547716

      3, 1, Day03.part1, E 738234
      3, 2, Day03.part2, E 3969126

      4, 1, Day04.part1, E 89001
      4, 2, Day04.part2, E 7296

      5, 1, Day05.part1, E 4993
      5, 2, Day05.part2, E 21101

      6, 1, Day06.part1, E 363101
      6, 2, Day06.part2, Some "1644286074024"

      7, 1, Day07.part1, E 344605
      7, 2, Day07.part2, E 93699985

      8, 1, Day08.part1, E 245
      8, 2, Day08.part2, E 983026

      9, 1, Day09.part1, E 456
      9, 2, Day09.part2, E 1047744

      10, 1, Day10.part1, E 442131
      10, 2, Day10.part2, Some "3646451424"

      11, 1, Day11.part1, E 1747
      11, 2, Day11.part2, E 505

      12, 1, Day12.part1, E 4659
      12, 2, Day12.part2, E 148962

      13, 1, Day13.part1, E 737
      13, 2, Day13.part2, Some
          """
####.#..#...##.#..#..##..####.#..#.###.
...#.#..#....#.#..#.#..#.#....#..#.#..#
..#..#..#....#.#..#.#..#.###..####.#..#
.#...#..#....#.#..#.####.#....#..#.###.
#....#..#.#..#.#..#.#..#.#....#..#.#...
####..##...##...##..#..#.#....#..#.#...
""" 
      14, 1, Day14.part1, None
      14, 2, Day14.part2, None

    ]
    |> List.map
        (fun (d, p, r, e) ->
            { day = d
              part = p
              run = r
              expected = e })

runCommandLine days
