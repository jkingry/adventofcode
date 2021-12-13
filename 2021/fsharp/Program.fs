open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2021

open Checked

let inline E v = v |> string |> Some 

let days = 
    [
        01,1,Day01.part1,E 1676
        01,2,Day01.part2,E 1706

        02,1,Day02.part1,E 2036120
        02,2,Day02.part2,E 2015547716

        03,1,Day03.part1,E 738234
        03,2,Day03.part2,E 3969126

        04,1,Day04.part1,E 89001
        04,2,Day04.part2,E 7296

        05,1,Day05.part1,E 4993
        05,2,Day05.part2,E 21101

        06,1,Day06.part1,E 363101
        06,2,Day06.part2,Some "1644286074024"

        07,1,Day07.part1,E 344605
        07,2,Day07.part2,E 93699985

        08,1,Day08.part1,E 245
        08,2,Day08.part2,E 983026

        09,1,Day09.part1,E 456
        09,2,Day09.part2,E 1047744

        10,1,Day10.part1,E 442131
        10,2,Day10.part2,Some "3646451424"

        11,1,Day11.part1,E 1747
        11,2,Day11.part2,E 505

        12,1,Day12.part1,E 4659
        12,2,Day12.part2,E 148962

        13,1,Day13.part1,E 737
        13,2,Day13.part2,Some """
####.#..#...##.#..#..##..####.#..#.###.
...#.#..#....#.#..#.#..#.#....#..#.#..#
..#..#..#....#.#..#.#..#.###..####.#..#
.#...#..#....#.#..#.####.#....#..#.###.
#....#..#.#..#.#..#.#..#.#....#..#.#...
####..##...##...##..#..#.#....#..#.#...
"""
    ]
    |> List.map (fun (d,p,r,e) -> {day=d;part=p;run=r;expected=e})

runCommandLine days
