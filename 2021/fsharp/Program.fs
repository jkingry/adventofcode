open AdventOfCode.FSharp.NorthPole
open AdventOfCode.FSharp.Y2021

open Checked

let days = 
    [
        01,1,Day01.part1,1676|>string|>Some
        01,2,Day01.part2,1706|>string|>Some

        02,1,Day02.part1,2036120|>string|>Some
        02,2,Day02.part2,2015547716|>string|>Some

        03,1,Day03.part1,738234|>string|>Some
        03,2,Day03.part2,3969126|>string|>Some
        
        04,1,Day04.part1,89001|>string|>Some
        04,2,Day04.part2,7296|>string|>Some

        05,1,Day05.part1,4993|>string|>Some
        05,2,Day05.part2,21101|>string|>Some
        
        06,1,Day06.part1,363101|>string|>Some
        06,2,Day06.part2,"1644286074024"|>Some

        07,1,Day07.part1,344605|>string|>Some
        07,2,Day07.part2,93699985|>string|>Some

        08,1,Day08.part1,245|>string|>Some
        08,2,Day08.part2,983026|>string|>Some

        09,1,Day09.part1,456|>string|>Some
        09,2,Day09.part2,1047744|>string|>Some

        10,1,Day10.part1,442131|>string|>Some
        10,2,Day10.part2,"3646451424"|>Some

        11,1,Day11.part1,1747|>string|>Some
        11,2,Day11.part2,505|>string|>Some

        12,1,Day12.part1,4659|>string|>Some
        12,2,Day12.part2,148962|>string|>Some

        13,1,Day13.part1,737|>string|>Some
        13,1,Day13.part2,Some """
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
