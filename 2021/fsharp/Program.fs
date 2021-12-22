open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2021

open Checked

let inline E v = v |> string |> Some

let inline old (part1: string -> string) (part2: string -> string) =
  fun text output ->
    part1 text |> output 1
    part2 text |> output 2

let days =
    [ 
      01, Day01.run, E 1676, E 1706
      02, Day02.run, E 2036120, E 2015547716
      03, old Day03.part1 Day03.part2, E 738234, E 3969126
      04, old Day04.part1 Day04.part2, E 89001, E 7296
      05, old Day05.part1 Day05.part2, E 4993, E 21101
      06, old Day06.part1 Day06.part2, E 363101, Some "1644286074024"
      07, old Day07.part1 Day07.part2, E 344605, E 93699985
      08, old Day08.part1 Day08.part2, E 245, E 983026
      09, old Day09.part1 Day09.part2, E 456, E 1047744
      10, old Day10.part1 Day10.part2, E 442131, Some "3646451424"
      11, old Day11.part1 Day11.part2, E 1747, E 505
      12, old Day12.part1 Day12.part2, E 4659, E 148962
      13, Day13.run, E 737, Some """
####.#..#...##.#..#..##..####.#..#.###.
...#.#..#....#.#..#.#..#.#....#..#.#..#
..#..#..#....#.#..#.#..#.###..####.#..#
.#...#..#....#.#..#.####.#....#..#.###.
#....#..#.#..#.#..#.#..#.#....#..#.#...
####..##...##...##..#..#.#....#..#.#...
"""
      14, Day14.run, E 2447, Some "3018019237563"
      15, Day15.run, E 613, E 2899
      16, Day16.run, E 901, Some "110434737925"
      17, Day17.run, E 4186, E 2709
      18, Day18.run, E 4243, E 4701
      19, Day19.run, E 315, E 13192
    ]
    |> List.map (fun (d, r, e1, e2) ->
            { day = d
              run = r
              expected = [ (1,e1); (2,e2) ] |> Map.ofList })

runCommandLine days
