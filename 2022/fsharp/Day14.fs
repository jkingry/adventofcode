namespace AdventOfCode.FSharp.Y2022

// Day 14
module Day14 =
    open Checked
    open AdventOfCode.FSharp.Util

    type BitmapType = System.Collections.BitArray * int * int * int
    module Bitmap =
        let create (w: int) (h: int) (ox: int) (oy: int) : BitmapType=
            (new System.Collections.BitArray (w*h)), w, ox, oy

        let inline get (x: int) (y: int) (b: BitmapType) : bool =
            let (a, width, offsetX, offsetY) = b
            let p = (width * (y - offsetY)) + (x - offsetX)           
            a[p]
        
        let inline set (x: int) (y: int) (v: bool) (b: BitmapType) : unit =
            let (a, width, offsetX, offsetY) = b
            let p = (width * (y - offsetY)) + (x - offsetX)           
            a[p] <- v

        let height (b: BitmapType) =
            let (a, width, _, _) = b
            a.Count / width

        let extend (rows: int) (b: BitmapType) : BitmapType =
            let (a, width, offsetX, offsetY) = b

            let a' = new System.Collections.BitArray (a.Count + (width * rows))
            let a' = a'.Or(a)
            (a', width, offsetX, offsetY) 

        let safeSet (x: int) (y: int) (v: bool) (b: BitmapType) : BitmapType =
            let (a, width, offsetX, offsetY) = b
            let height = a.Count / width
            let tx = x - offsetX
            let ty = y - offsetY
            if 0 <= tx && tx < width && 0 <= ty && ty < height then
                set x y v b
                b
            else
                let (offsetX', width') =
                    if tx < 0 then
                        (x, width - tx)
                    elif tx > 0 then
                        (offsetX, width + (tx - width) + 1)
                    else
                        (offsetX, width)
                
                let (offsetY', height') =
                    if ty < 0 then
                        (y, height - ty)
                    elif tx > 0 then
                        (offsetY, height + (ty - height) + 1)
                    else
                        (offsetY, height)
                let a' = new System.Collections.BitArray(width' * height')

                printfn "Setting %A so extending from %A to %A" (x,y) (width, height, offsetX, offsetY) (width', height', offsetX', offsetY')

                for yy = offsetY' to (offsetY' + height' - 1) do
                    for xx = offsetX' to (offsetX' + width' - 1) do
                        if offsetX <= xx && xx < (width + offsetX) && offsetY <= yy && yy < (height + offsetY) then
                            let p = (width * (yy - offsetY)) + (xx - offsetX)
                            let p' = (width' * (yy - offsetY')) + (xx - offsetX')
                            printfn "Transfering %A: %i to %i" (xx,yy) p p'
                            a'[p'] <- a[p]
                let b' = (a', width', offsetX', offsetY')
                set x y v b'
                b'

    let run (input: byte[]) (output: int -> string -> unit) =
        let mutable walls = Bitmap.create 200 200 400 0
        let mutable i = 0
        let mutable px = -1
        let mutable py = -1

        while i < input.Length do
            let (i2, x) = parseIntToAny input i
            let (i3, y) = parseIntToAny input i2
            i <- i3

            // printfn "%i Point %A" i (x,y)

            if x = 0 then failwith "wtf"

            if px > 0 && py > 0 then
                for dx = (min x px) to (max x px) do
                    for dy = (min y py) to (max y py) do
                        // printfn " -  %A" (dx,dy)
                        walls <- walls |> Bitmap.safeSet dx dy true
            px <- x
            py <- y
            if input[i - 1] = '\n'B then                 
                px <- -1 
                py <- -1


        let height = (walls |> Bitmap.height) - 1

        let addSand (height) (startX, startY) (walls: BitmapType) =
            let mutable sx = startX
            let mutable sy = startY        
            let mutable stopped = false

            if walls |> Bitmap.get sx sy then false else    

            while sy < height && not stopped do
                if walls |> Bitmap.get sx (sy + 1) |> not then
                    sy <- sy + 1
                elif walls |> Bitmap.get (sx - 1) (sy + 1) |> not then
                    sx <- sx - 1
                    sy <- sy + 1
                elif walls |> Bitmap.get (sx + 1) (sy + 1) |> not then
                    sy <- sy + 1
                    sx <- sx + 1
                else 
                    stopped <- true
            
            if stopped then
                walls |> Bitmap.set sx sy true

            stopped
                
        let mutable totalSand = 0
        while addSand height (500, 0) walls do
            totalSand <- totalSand + 1

        totalSand |> string |> output 1
        1 |> string |> output 2

    let printWalls (walls: char[,]) =
        let screenW = System.Console.WindowWidth
        let screenH = System.Console.WindowHeight
        let screenH = screenH - 5

        let mapWidth = (Array2D.length1 walls)
        let mapHeight = (Array2D.length2 walls)

        let columns = ceil((float mapHeight) / (float screenH)) |> int 

        let maxColumns = screenW / ((mapWidth) + 1)
        if columns > maxColumns then
            failwithf 
                "Screen (%i,%i)=%i too small for map (%i,%i)=%i" 
                screenW 
                screenH 
                (screenW * screenH)
                mapWidth
                mapHeight
                (mapWidth * mapHeight)
        
        let rows = if columns = 1 then mapHeight else screenH

        for r = 1 to rows do
            for c = 1 to columns do
                for x = 1 to mapWidth do
                    let x = x - 1
                    let y = ((c - 1) * (screenH)) + (r - 1)
                    if x < mapWidth && y < mapHeight then                        
                        printf "%c" walls[x,y]
                    else
                        printf "@"               
                printf "X"
            printf "\n"

    let inline array2tuple (a: 'a[]) : 'a*'a =
        a[0],a[1] 

    let runVisualize (input: byte[]) (output: int -> string -> unit) =

        let wallsList = 
            input 
            |> text 
            |> splitLine
            |> Array.map (fun line -> 
                line.Split(" -> ") |> Array.map (ints >> array2tuple))

        let  (minX, maxX, maxY) = 
            wallsList
            |> Array.fold (Array.fold (fun (mi, mx, my) (x,y) -> 
                    ((min x mi),(max x mx),(max my y)))) (System.Int32.MaxValue, 0, 0)

        let minX = minX - 1
        let maxX = maxX + 1
        let maxY = maxY + 2

        let walls = Array2D.create ((maxX - minX) * 2) (maxY + 1) ' '
        for wall in wallsList do
            let mutable (sx, sy) = wall[0]
            sx <- sx - minX
            for (px, py) in Array.skip 1 wall do
                let px = px - minX
                if sx = px then                
                    for y = (min sy py) to (max sy py) do 
                        walls[sx, y] <- '#'
                else 
                    for x = (min sx px) to (max sx px) do
                        walls[x, sy] <- '#'
                sx <- px
                sy <- py
        for x = 1 to ((maxX - minX) * 2) do
            let x = x - 1
            walls[x, maxY] <- '#'

        let addSand (walls: char[,]) =
            let mutable sx = 500 - minX 
            let mutable sy = 0
            let mutable stopped = false
            
            while sy < maxY && not stopped do
                if walls[sx, sy + 1] = ' ' then
                    sy <- sy + 1
                elif walls[sx - 1, sy + 1] = ' ' then
                    sx <- sx - 1
                    sy <- sy + 1
                elif walls[sx + 1, sy + 1] = ' ' then
                    sy <- sy + 1
                    sx <- sx + 1
                else 
                    stopped <- true
            
            if stopped then
                walls[sx,sy] <- 'O'
            stopped
        
        System.Console.Clear ()
        System.Console.CursorVisible <- false
        let struct (x, y) = System.Console.GetCursorPosition ()
        
        printfn "Start:"
        printWalls walls

        
        let mutable totalSand = 0
        while addSand walls do
            System.Console.SetCursorPosition (x, y)
            printfn "Sand: %i" totalSand
            printWalls walls
            totalSand <- totalSand + 1

        System.Console.SetCursorPosition (x, y)
        printfn "Final:"
        printWalls walls
        System.Console.CursorVisible <- true
        printfn "%i" maxY

        totalSand |> string |> output 1
        1 |> string |> output 2
