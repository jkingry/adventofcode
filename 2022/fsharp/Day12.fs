namespace AdventOfCode.FSharp.Y2022

// Day 12: Hill Climbing Algorithm
module Day12 =
    open Checked
    open FSharpx.Collections
    open AdventOfCode.FSharp.Util 

    let dijkstra moves start goalFunc =
        let mutable gScore = Map [ (start, 0) ]

        let mutable q = Heap.empty false |> Heap.insert (0, start)

        let mutable found = None

        while Option.isNone found && not (Heap.isEmpty q) do
            let ((_, current), nq) = Heap.uncons q

            if goalFunc current gScore[current] then
                found <- Some gScore[current]
            else
                q <- nq

                for (move, moveCost) in moves current do
                    let tentative_gScore = gScore[current] + moveCost

                    if tentative_gScore < (gScore |> Map.tryFind move |> Option.defaultValue System.Int32.MaxValue) then
                        gScore <- gScore |> Map.add move tentative_gScore
                        q <- q |> Heap.insert (tentative_gScore, move)
        found

    let run (input: byte array) (output: int -> string -> unit) =
        let lines = input |> text |> splitLine 
        let a = lines |> Array.map (
            fun s -> s.ToCharArray()) |> array2D

        let mutable start = (0,0)
        let mutable goal = (0,0)

        let a = a |> Array2D.mapi (fun x y v -> 
            match v with
            | 'S' -> start <- (x, y); 0uy
            | 'E' -> goal <- (x,y); 25uy
            | _ -> (byte v) - (byte 'a'))
        
        let mx = Array2D.length1 a
        let my = Array2D.length2 a

        let canMoveUp (fx, fy) (tx, ty) =
            let fh = a[fx, fy]
            let th = a[tx, ty]
            (fh + 1uy) >= th

        let canMoveDown src dest = canMoveUp dest src

        let moves allowFunc (x,y) =
            seq {
                if x > 0 && allowFunc (x,y) (x - 1,y) then yield ((x - 1,y), 1)
                if y > 0 && allowFunc (x,y) (x,y - 1) then yield ((x,y - 1), 1)
                if x < (mx - 1) && allowFunc (x,y) (x + 1,y) then yield ((x + 1,y), 1)
                if y < (my - 1) && allowFunc (x,y) (x, y + 1) then yield ((x,y + 1), 1)
            }
  
        let mutable minCost = None
        let findStart (x,y) cost =
            if a[x,y] = 0uy && Option.isNone minCost then
                minCost <- Some cost
            start = (x,y)

        let startCost = dijkstra (moves canMoveDown) goal findStart

        startCost |> Option.get |> string |> output 1
        minCost |> Option.get |> string |> output 2    

    let moves2D mx my allowFunc cx cy (mxBuf: int array) (myBuf: int array) (mcBuf: int array) =
        let mutable c = 0
        if cx > 0 && allowFunc cx cy (cx - 1) cy then 
            mxBuf[c] <- cx - 1
            myBuf[c] <- cy
            mcBuf[c] <- 1
            c <- c + 1
        if cy > 0 && allowFunc cx cy cx (cy - 1) then 
            mxBuf[c] <- cx
            myBuf[c] <- cy - 1
            mcBuf[c] <- 1
            c <- c + 1
        if cx < (mx - 1) && allowFunc cx cy (cx + 1) cy then 
            mxBuf[c] <- cx + 1
            myBuf[c] <- cy
            mcBuf[c] <- 1
            c <- c + 1
        if cy < (my - 1) && allowFunc cx cy cx (cy + 1) then 
            mxBuf[c] <- cx
            myBuf[c] <- cy + 1
            mcBuf[c] <- 1                                                
            c <- c + 1
        c

    let dijkstra2D mx my moveFunc (sx, sy) goalFunc =
        let gScore = Array2D.create mx my System.Int32.MaxValue
        gScore[sx, sy] <- 0

        let mutable q = Heap.empty false |> Heap.insert (0, (sx, sy))

        let mutable found = None

        let mxBuf = Array.create 4 0
        let myBuf = Array.create 4 0
        let mcBuf = Array.create 4 0
        
        while Option.isNone found && not (Heap.isEmpty q) do
            let ((_, (cx, cy)), nq) = Heap.uncons q

            if goalFunc (cx, cy) (gScore[cx, cy]) then
                found <- Some gScore[cx, cy]
            else
                q <- nq

                let moveCount = moveFunc cx cy mxBuf myBuf mcBuf
                for i = 1 to moveCount do
                    let mx = mxBuf[i - 1]
                    let my = myBuf[i - 1]
                    let moveCost = mcBuf[i - 1]

                    let tentative_gScore = gScore[cx, cy] + moveCost

                    if tentative_gScore < gScore[mx, my] then
                        gScore[mx, my] <- tentative_gScore
                        q <- q |> Heap.insert (tentative_gScore, (mx, my))
        found
    
    let runFast (input: byte array) (output: int -> string -> unit) =
        let lines = input |> text |> splitLine 
        let a = lines |> Array.map (
            fun s -> s.ToCharArray()) |> array2D

        let mutable start = (0,0)
        let mutable goal = (0,0)

        let a = a |> Array2D.mapi (fun x y v -> 
            match v with
            | 'S' -> start <- (x, y); 0uy
            | 'E' -> goal <- (x,y); 25uy
            | _ -> (byte v) - (byte 'a'))
        
        let mx = Array2D.length1 a
        let my = Array2D.length2 a

        let canMoveUp fx fy tx ty =
            let fh = a[fx, fy]
            let th = a[tx, ty]
            (fh + 1uy) >= th

        let canMoveDown fx fy tx ty = canMoveUp tx ty fx fy
  
        let mutable minCost = None
        let findStart (x,y) cost =
            if a[x,y] = 0uy && Option.isNone minCost then
                minCost <- Some cost
            start = (x,y)

        let startCost = dijkstra2D mx my (moves2D mx my canMoveDown) goal findStart

        startCost |> Option.get |> string |> output 1
        minCost |> Option.get |> string |> output 2    