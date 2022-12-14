namespace AdventOfCode.FSharp.Y2021

// Day 11: Dumbo Octopus
module Day11 =
    open AdventOfCode.FSharp.Util
    open Checked

    let step grid =
        grid |> Array2D.iteri (fun x y v -> grid.[x, y] <- v + 1)

        let mutable flashed = Set.empty

        let flash (x, y) =
            for nx = x - 1 to x + 1 do
                for ny = y - 1 to y + 1 do
                    if
                        (nx <> x || ny <> y)
                        && nx >= 0
                        && ny >= 0
                        && nx < Array2D.length1 grid
                        && ny < Array2D.length2 grid
                    then
                        grid.[nx, ny] <- grid.[nx, ny] + 1

        let findDumbos g =
            let mutable f = Set.empty

            g
            |> Array2D.iteri (fun x y v ->
                if v > 9 && (flashed |> Set.contains (x, y) |> not) then
                    f <- f |> Set.add (x, y))

            f

        let mutable dumbos = findDumbos grid

        while dumbos.Count > 0 do
            flashed <- flashed |> Set.union dumbos
            dumbos |> Seq.iter flash
            dumbos <- findDumbos grid

        flashed |> Seq.iter (fun (x, y) -> grid.[x, y] <- 0)

        flashed.Count

    let part1 (text: string) =
        let grid =
            text
            |> splitLine
            |> Array.map (fun line -> line |> Seq.map (string >> int))
            |> array2D

        [ 1..100 ] |> List.map (fun _ -> step grid) |> List.sum |> string

    let part2 (text: string) =
        let grid =
            text
            |> splitLine
            |> Array.map (fun line -> line |> Seq.map (string >> int))
            |> array2D

        let n = grid.Length

        Seq.initInfinite (fun i -> i, step grid)
        |> Seq.pick (fun (i, v) -> if v = n then Some(i + 1) else None)
        |> string
