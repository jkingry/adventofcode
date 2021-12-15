namespace AdventOfCode.FSharp.Y2021

// Day 15
module Day15 =
    open AdventOfCode.FSharp.Util

    let neighbors (x, y) grid =
        [ 1, 0; -1, 0; 0, 1; 0, -1 ]
        |> List.map (fun (dx, dy) -> dx + x, dy + y)
        |> List.filter
            (fun (nx, ny) ->
                nx >= 0
                && ny >= 0
                && nx < Array2D.length1 grid
                && ny < Array2D.length2 grid)

    let reconstruct_path cameFrom current =
        let mutable total_path = [current]
        let mutable c = current
        while cameFrom |> Map.containsKey c do
            c <- cameFrom.[c]
            total_path <- c::total_path
        total_path

    open Checked 

    type Node =
        {
            p: int*int
            o: bool
            index: int
        }

    let astar start goal (grid : int[,]) =
        let (gx,gy) = goal
        let inline h (a,b) = (gx - a) + (gy - b)  

        let mutable openSet = [start]
        let mutable cameFrom = Map.empty
        let (sx,sy) = start
        let gScore = Array2D.create (Array2D.length1 grid) (Array2D.length2 grid) System.Int32.MaxValue
        gScore[sx,sy] <- 0

        let fScore = Array2D.create (Array2D.length1 grid) (Array2D.length2 grid) System.Int32.MaxValue
        fScore[sx,sy] <- (h start)
        
        let mutable found = None
        let mutable mx = 0
        let mutable my = 0

        let mutable visited = 0L

        while (Option.isNone found) && (not (List.isEmpty openSet)) do
            let (cx,cy)::nextCurrent = openSet

            if cx = gx && cy = gy  then found <- Some (cx,cy) else

            if cx > mx then
                mx <- cx
                printfn "mx %d" mx
            if cy > my then
                my <- cy
                printfn "my %d" my


            // printfn "%A" current
            visited <- visited + 1L

            openSet <- nextCurrent
            for (nx, ny) in (neighbors (cx,cy) grid) do
                let tentative_gScore = gScore[cx,cy] + grid.[nx, ny]
                if tentative_gScore < gScore[nx,ny] then
                    cameFrom <- cameFrom |> Map.add (nx,ny) (cx,cy)
                    gScore[nx,ny] <- tentative_gScore
                    fScore[nx,ny] <- tentative_gScore + (h (nx,ny))
                    openSet <- (nx,ny)::openSet
            openSet <- 
                openSet
                |> List.distinct
                |> List.sortBy (fun (x,y) -> fScore[x,y])

        printfn "visited = %d" visited
        match found with
        | Some c -> reconstruct_path cameFrom c
        | None -> failwith "failure"    

    let run (text: string): string*string =
        let grid =
            text
            |> splitLine
            |> Array.map (fun line -> line |> Seq.map (string >> int))
            |> array2D

        let expandGrid grid n = 
            let dx = grid |> Array2D.length1
            let dy = grid |> Array2D.length2
            Array2D.init (dx*5) (dy*5) (fun x y -> 
                let v = grid[x%dx, y%dy] 
                let r = (x/dx) + (y/dy)
                let vv = (v + r) - 1
                (vv % 9) + 1)
        let grid = expandGrid grid 4


        let start = (0,0)
        let goal = ((Array2D.length1 grid) - 1, (Array2D.length2 grid) - 1)
        printfn "%A" goal

        let path = astar start goal grid |> List.tail
        let part1 = path |> List.map (fun (x,y) -> grid[x,y]) |> List.sum

        (part1 |> string , "-1")