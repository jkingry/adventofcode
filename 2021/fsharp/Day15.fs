namespace AdventOfCode.FSharp.Y2021

// Day 15
module Day15 =
    open AdventOfCode.FSharp.Util

    let neighbors (x, y) (mx,my) =
        let mutable n = []
        if x > 0 then n <- (x-1,y)::n
        if y > 0 then n <- (x,y-1)::n
        if x < (mx - 1) then n <- (x+1,y)::n
        if y < (my - 1) then n <- (x,y+1)::n
        n

    let reconstruct_path (cameFrom: ((int*int) option)[,]) (cx,cy) =
        let mutable total_path = [(cx,cy)]
        let mutable x = cx
        let mutable y = cy
        while Option.isSome cameFrom.[x,y] do
            let (nx,ny) = (cameFrom.[x,y]).Value
            x <- nx
            y <- ny
            total_path <- (x,y)::total_path
        total_path

    open Checked 
    open FSharpx.Collections

    let astar (sx,sy) (gx,gy) (mx,my) (w: int->int->int) =
        printfn "START"
        let inline h x y = (gx - x) + (gy - y)
        let cameFrom = Array2D.create mx my None

        let gScore = Array2D.create mx my System.Int32.MaxValue
        gScore[sx,sy] <- 0

        let hs = (h sx sy)

        let mutable openSet = Heap.empty false |> Heap.insert (hs, sx, sy)
        
        let mutable found = None

        let mutable visited = 0L

        while (Option.isNone found) && (not (Heap.isEmpty openSet)) do
            let ((_, cx,cy),nextOpenSet) = openSet |> Heap.uncons

            if cx = gx && cy = gy  then found <- Some (cx,cy) else

            visited <- visited + 1L

            openSet <- nextOpenSet
            for (nx, ny) in (neighbors (cx,cy) (mx, my)) do
                let tentative_gScore = gScore[cx,cy] + (w nx ny)
                if tentative_gScore < gScore[nx,ny] then
                    cameFrom[nx,ny] <- Some (cx,cy)
                    let nh = h nx ny
                    gScore[nx,ny] <- tentative_gScore
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
        let mx = grid |> Array2D.length1
        let my = grid |> Array2D.length2

        let w x y =
            let v = grid[x%mx, y%my] 
            let r = (x/mx) + (y/my)
            let vv = (v + r) - 1
            (vv % 9) + 1


        let start = (0,0)
        let goal = ((mx * 5)-1,(my*5)-1)

        printfn "%A" goal

        let path = astar start goal (mx*5,my*5) w |> List.tail

        // for (x,y) in path do        
        //     printfn "%A %A %A" (x,y) (x/100,y/100) (x%100,y%100)
        let part1 = path |> List.map (fun (x,y) -> w x y) |> List.sum

        (part1 |> string , "-1")