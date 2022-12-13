namespace AdventOfCode.FSharp.Y2021

// Day 15: Chiton
module Day15 =
    open AdventOfCode.FSharp.Util

    let run data output =
        let text = data |> text
 
        let grid =
            text
            |> splitLine
            |> Array.map (fun line -> line |> Seq.map (string >> int))
            |> array2D

        let width = grid |> Array2D.length1
        let height = grid |> Array2D.length2

        let risk x y =
            let baseRisk = grid[x % width, y % height]
            let tileFactor = (x / width) + (y / height)
            let risk = (baseRisk + tileFactor) - 1
            (risk % 9) + 1

        let part1risk _ _ x y =
            if x < 0 || y < 0 || x >= width || y >= height then None else
            risk x y |> Some

        let costs, q =
            Dijkstra2D.init width height System.Int32.MaxValue
            |> Dijkstra2D.add 0 0 0
            |> Dijkstra2D.run 4 0 (OrthoGrid.movesToBuffers part1risk) (fun x y -> x = (width - 1) && y = (height - 1))

        costs[width - 1, height - 1] |> string |> output 1

        let width' = width * 5
        let height' = height * 5

        let part2risk _ _ x y =
            if x < 0 || y < 0 || x >= width' || y >= height' then None else
            risk x y |> Some

        let costs, _ =
            Dijkstra2D.init width' height' System.Int32.MaxValue
            |> Dijkstra2D.add 0 0 0
            |> Dijkstra2D.run 4 0 (OrthoGrid.movesToBuffers part2risk) (fun x y -> x = width' - 1 && y = height' - 1)
        
        costs[width' - 1, height' - 1] |> string |> output 2
