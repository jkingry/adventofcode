namespace AdventOfCode.FSharp.Y2015

// Day 3: Perfectly Spherical Houses in a Vacuum https://adventofcode.com/2015/day/3
module Day03 =
    let run (input: byte array) (output: int -> string -> unit) =
        let mutable posYear1 = 0, 0
        let mutable year1 = Set.empty |> Set.add (0, 0)

        let mutable year2 = year1
        let mutable posYear2 = Array.create 2 (0, 0)
        let mutable turn = 0

        for b in input do
            let move (x, y) =
                match b with
                | '>'B -> x + 1, y
                | '<'B -> x - 1, y
                | '^'B -> x, y + 1
                | 'v'B -> x, y - 1
                | _ -> failwithf "invalid: %c" (char b)

            posYear1 <- posYear1 |> move
            year1 <- year1 |> Set.add posYear1

            posYear2[turn] <- posYear2[turn] |> move
            year2 <- year2 |> Set.add posYear2[turn]
            turn <- (turn + 1) % 2

        year1.Count |> string |> output 1
        year2.Count |> string |> output 2
