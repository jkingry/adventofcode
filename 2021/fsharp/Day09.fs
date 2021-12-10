namespace AdventOfCode.FSharp.Y2021

// Day 9: Smoke Basin
module Day09 =    
    open AdventOfCode.FSharp.Util

    let neighbors (x,y) grid =
        [1,0;-1,0;0,1;0,-1]
        |> List.map (fun (dx, dy) -> dx + x,dy + y)
        |> List.filter (fun (nx, ny) -> nx >= 0 && ny >= 0 && nx < Array2D.length1 grid && ny < Array2D.length2 grid)

    let isLow x y v grid = 
        neighbors (x,y) grid 
        |> List.forall (fun (nx, ny) -> grid[nx,ny] > v)
 
    let part1 (text : string) =
        let grid = 
            text 
            |> splitLine
            |> Array.map (fun line -> line |> Seq.map (string >> int))
            |> array2D
        
        let mutable riskValue = 0

        grid 
        |> Array2D.iteri (fun x y v -> if isLow x y v grid then riskValue <- riskValue + v + 1)

        riskValue

    let part2 (text : string) =
        let grid = 
            text 
            |> splitLine
            |> Array.map (fun line -> line |> Seq.map (string >> int))
            |> array2D

        let mutable mapped = Set.empty   
        
        let findBasin sx sy =
            let mutable q = [ (sx,sy) ]
            let mutable basin = Set.empty
            let mutable loop = true

            while loop do
                match q with
                | [] -> loop <- false
                | (x,y)::nq ->               
                    q <- nq

                    if mapped |> Set.contains (x, y) |> not then
                        mapped <- mapped |> Set.add (x,y)

                        let v = grid[x,y]

                        if v <> 9 then
                            basin <- basin |> Set.add (x,y)

                            let next = 
                                neighbors (x,y) grid
                                |> List.filter (fun p -> not (Set.contains p mapped))

                            q <- q @ next
            basin

        let mutable lows = []
        grid 
        |> Array2D.iteri (fun x y v -> if isLow x y v grid then lows <- (x,y)::lows)

        let basins =
            lows |> List.fold (fun b (x,y) -> (findBasin x y)::b) []

        basins |> List.map Set.count |> List.sortDescending |> List.take 3 |> List.reduce (fun a b -> a * b) 