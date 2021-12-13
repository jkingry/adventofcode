namespace AdventOfCode.FSharp.Y2021

// Day 5: Hydrothermal Venture
module Day05 =
    open AdventOfCode.FSharp.Util

    let parseLine text =
        match text with
        | Regex @"(\d+),(\d+) -> (\d+),(\d+)" [ x1; y1; x2; y2 ] -> (int x1, int y1), (int x2, int y2)
        | _ -> failwith "Invalid"

    let points ((x1, y1), (x2, y2)) (grid: int [,]) =
        let (dx, dy) = (sign (x2 - x1), sign (y2 - y1))
        let mutable (nx, ny) = (x1, y1)
        let mutable doublePoints = 0

        let inline mark x y =
            let v = grid.[x, y]

            if v = 1 then
                doublePoints <- doublePoints + 1

            grid.[x, y] <- v + 1

        mark nx ny

        while (nx, ny) <> (x2, y2) do
            nx <- nx + dx
            ny <- ny + dy
            mark nx ny

        doublePoints

    let countPoints lines =
        let (mx, my) =
            lines
            |> Array.fold (fun (mx, my) ((x1, y1), (x2, y2)) -> (max (max mx x1) x2, max (max my y1) y2)) (0, 0)

        let grid = Array2D.zeroCreate (mx + 1) (my + 1)

        lines
        |> Array.fold (fun a line -> a + (points line grid)) 0

    let part1 (input: string) =
        let lines =
            input |> splitLine |> Array.map parseLine

        lines
        |> Array.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
        |> countPoints
        |> string

    let part2 (input: string) =
        let lines =
            input |> splitLine |> Array.map parseLine

        lines |> countPoints |> string
