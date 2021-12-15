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
    open FSharpx.Collections

    let astar start goal (grid : int[,]) =
        printfn "START"
        let (gx,gy) = goal
        let inline h (a,b) = (gx - a) + (gy - b)  
        let mutable cameFrom = Map.empty
        let (sx,sy) = start
        let gScore = Array2D.create (Array2D.length1 grid) (Array2D.length2 grid) System.Int32.MaxValue
        gScore[sx,sy] <- 0

        //let fScore = Array2D.create (Array2D.length1 grid) (Array2D.length2 grid) System.Int32.MaxValue
        let hs = (h start)
        //fScore[sx,sy] <- hs
        let mutable openSet = Heap.empty false |> Heap.insert (hs, sx, sy)
        
        let mutable found = None
        // let mutable mx = 0
        // let mutable my = 0

        let mutable visited = 0L

        while (Option.isNone found) && (not (Heap.isEmpty openSet)) do
            let ((ch, cx,cy),nextOpenSet) = openSet |> Heap.uncons

            if cx = gx && cy = gy  then found <- Some (cx,cy) else

            // if cx > mx then
            //     mx <- cx
            //     printfn "mx %d" mx
            // if cy > my then
            //     my <- cy
            //     printfn "my %d" my


            // printfn "%A" current
            visited <- visited + 1L

            openSet <- nextOpenSet
            for (nx, ny) in (neighbors (cx,cy) grid) do
                let tentative_gScore = gScore[cx,cy] + grid.[nx, ny]
                if tentative_gScore < gScore[nx,ny] then
                    cameFrom <- cameFrom |> Map.add (nx,ny) (cx,cy)
                    let nh = h (nx,ny)
                    gScore[nx,ny] <- tentative_gScore
                    //fScore[nx,ny] <- tentative_gScore + nh
                    openSet <- openSet |> Heap.insert (tentative_gScore + nh, nx, ny) 

        printfn "FINISH"

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
        // for (x,y) in path do        
        //     printfn "%A %A %A" (x,y) (x/100,y/100) (x%100,y%100)
        let part1 = path |> List.map (fun (x,y) -> grid[x,y]) |> List.sum

        (part1 |> string , "-1")