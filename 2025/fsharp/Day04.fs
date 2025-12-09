namespace AdventOfCode.FSharp.Y2025

// Day 4: Printing Department
module Day04 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let diagram = input |> bsplit '\n'B |> array2D // |> SafeArray2D
        let rows = diagram |> Array2D.length1
        let cols = diagram |> Array2D.length2

        let mutable q =
            [ 0 .. rows - 1 ]
            |> List.collect (fun x -> [ for y in 0 .. cols - 1 -> x, y ])
            |> Set.ofList

        let mutable part1 = -1
        let mutable part2 = 0

        while q |> Set.isEmpty |> not do
            let mutable nq = Set.empty
            let mutable moved = []

            for x, y in q do
                if diagram[x, y] = '@'B then
                    let mutable adj = Set.empty

                    for bx = x - 1 |> max 0 to x + 1 |> min (rows - 1) do
                        for by = y - 1 |> max 0 to y + 1 |> min (cols - 1) do
                            if (bx <> x || by <> y) && diagram[bx, by] = '@'B then
                                adj <- adj |> Set.add (bx, by)

                    if adj |> Set.count < 4 then
                        moved <- (x, y) :: moved
                        nq <- nq |> Set.union adj

            if part1 < 0 then
                part1 <- moved.Length

            part2 <- part2 + moved.Length

            for x, y in moved do
                diagram[x, y] <- '.'B

            q <- nq


        part1 |> string |> output 1
        part2 |> string |> output 2
