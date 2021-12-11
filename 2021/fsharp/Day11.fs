namespace AdventOfCode.FSharp.Y2021

// Day 11
module Day11 =    
    open AdventOfCode.FSharp.Util
    open Checked

    let rec flash x y (grid : int[,])  =
        for nx = x-1 to x+1 do
            for ny = y-1 to y+1 do
                if (nx <> x || ny <> y) &&  nx >= 0 && ny >= 0 && nx < Array2D.length1 grid && ny < Array2D.length2 grid then
                    let v = grid[nx,ny] + 1
                    grid[nx,ny] <- v

    let step grid = 
        let flashed = ref Set.empty
        grid  |> Array2D.iteri (fun x y v -> grid[x,y] <- v + 1)
        
        let mutable found = true

        while found do
            found <- false
            grid |> Array2D.iteri (fun x y v -> 
                if v > 9 && not (Set.contains (x, y) flashed.Value) then
                    found <- true
                    flashed.Value <- Set.add (x,y) flashed.Value
                    flash x y grid)

        !flashed |> Seq.iter (fun (x,y) -> grid[x,y] <- 0)
        flashed.Value.Count        

    let part1 (text : string) =   
        let grid = 
            text 
            |> splitLine
            |> Array.map (fun line -> line |> Seq.map (string >> int))
            |> array2D

        let mutable ff = 0
        for i = 1 to 100000 do
            let f = step grid
            if f = 100 then 
                printfn "FOUND IT %d" i
                ignore <| System.Console.ReadLine ()
            ff <- ff + f
        ff

    let part2 (text : string) =
        -1