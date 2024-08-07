namespace AdventOfCode.FSharp.Y2023

// Day 11: Cosmic Expansion http://adventofcode.com/2023/day/11
module Day11 =
    open AdventOfCode.FSharp.Util

    let shortestPathAllPairs (universe: int[,]) (galaxies: Set<int * int>) =
        let maxRows = Array2D.length1 universe
        let maxCols = Array2D.length2 universe

        let mutable total = 0L

        let mutable index = 1
        let infiniteCost = System.Int64.MaxValue

        let moveCost fx fy tx ty =
            if 0 <= tx && tx < maxRows && 0 <= ty && ty < maxCols then
                universe[tx, ty] |> int64 |> Some
            else
                None

        for (ax, ay) in (galaxies |> Seq.take (galaxies.Count - 1)) do
            let mutable costs, q =
                Dijkstra2D.init maxRows maxCols infiniteCost |> Dijkstra2D.add ax ay 0

            for (bx, by) in (galaxies |> Seq.skip index) do
                if costs[bx, by] = infiniteCost then
                    let costs', q' =
                        (costs, q)
                        |> Dijkstra2D.run 4 0L (OrthoGrid.movesToBuffers moveCost) (fun gx gy -> bx = gx && by = gy)

                    costs <- costs'
                    q <- q'

                let shortestPath = costs[bx, by]
                total <- total + shortestPath

            index <- index + 1

        total

    let _run (input: byte[]) (output: int -> string -> unit) =
        let mutable galaxies = Set.empty

        let mutable rows = Set.empty
        let mutable cols = Set.empty

        let universe =
            input
            |> bsplit '\n'B
            |> Array.mapi (fun ri row ->
                row
                |> Array.mapi (fun ci c ->
                    match c with
                    | '.'B -> 1
                    | '#'B ->
                        galaxies <- galaxies |> Set.add (ri, ci)
                        rows <- rows |> Set.add ri
                        cols <- cols |> Set.add ci
                        1
                    | _ -> failwithf "%c" (char c)))
            |> array2D

        let maxRows = Array2D.length1 universe
        let maxCols = Array2D.length2 universe

        let emptyRows = ([ 0 .. maxRows - 1 ] |> Set.ofList) - rows
        let emptyCols = ([ 0 .. maxCols - 1 ] |> Set.ofList) - cols

        for r in emptyRows do
            universe[r, *] <- Array.create maxCols 2

        for c in emptyCols do
            universe[*, c] <- Array.create maxRows 2

        shortestPathAllPairs universe galaxies |> string |> output 1

        for r in emptyRows do
            universe[r, *] <- Array.create maxCols 1000000

        for c in emptyCols do
            universe[*, c] <- Array.create maxRows 1000000

        shortestPathAllPairs universe galaxies |> string |> output 2

    let parseGalaxyCoordinates (input: byte[]) =
        let mutable galaxies = Set.empty

        let mutable row = 0
        let mutable col = 0

        for c in input do
            match c with
            | '\n'B ->
                row <- row + 1
                col <- 0
            | '.'B ->
                ()
                col <- col + 1
            | '#'B ->
                galaxies <- galaxies |> Set.add (row, col)
                col <- col + 1
            | _ -> failwithf "Unsupported character: %c" (char c)

        galaxies

    let expandGalaxies factor galaxies =
        let galX = galaxies |> Seq.map fst |> Set.ofSeq
        let galY = galaxies |> Seq.map snd |> Set.ofSeq

        galaxies
        |> Set.map (fun (gx, gy) ->
            let prevGalaxyColumns = galX |> Seq.filter (fun x -> x < gx) |> Seq.length
            let prevNonGalaxyColumns = gx - (prevGalaxyColumns + galX.MinimumElement)
            let prevGalaxyRows = galY |> Seq.filter (fun y -> y < gy) |> Seq.length
            let prevNonGalaxyRows = gy - (prevGalaxyRows + galY.MinimumElement)

            gx + (prevNonGalaxyColumns * (factor - 1)), gy + (prevNonGalaxyRows * (factor - 1)))

    let allPairsManhattenDistanceTotal (galaxies: Set<int * int>) =
        galaxies
        |> Seq.take (galaxies.Count - 1)
        |> Seq.indexed
        |> Seq.collect (fun (index, a) -> galaxies |> Seq.skip (index + 1) |> Seq.map (fun b -> a, b))
        |> Seq.map (fun ((ax, ay), (bx, by)) -> (abs (ax - bx)) + (abs (ay - by)) |> int64)
        |> Seq.sum

    let runManhatten (input: byte[]) (output: int -> string -> unit) =
        let galaxies = parseGalaxyCoordinates input

        galaxies
        |> expandGalaxies 2
        |> allPairsManhattenDistanceTotal
        |> string
        |> output 1

        galaxies
        |> expandGalaxies 1_000_000
        |> allPairsManhattenDistanceTotal
        |> string
        |> output 2
