namespace AdventOfCode.FSharp.Y2022

// Day 14
module Day14 =
    open Checked
    open AdventOfCode.FSharp.Util

    type BitmapType = System.Collections.BitArray * int * int * int

    module Bitmap =
        let create (w: int) (h: int) (ox: int) (oy: int) : BitmapType =
            (new System.Collections.BitArray(w * h)), w, ox, oy

        let inline get (x: int) (y: int) (b: BitmapType) : bool =
            let (a, width, offsetX, offsetY) = b
            let p = (width * (y - offsetY)) + (x - offsetX)
            a[p]

        let inline set (x: int) (y: int) (v: bool) (b: BitmapType) : unit =
            let (a, width, offsetX, offsetY) = b
            let p = (width * (y - offsetY)) + (x - offsetX)
            a[p] <- v

    let parse (input: byte[]) : (int * BitmapType) =
        let mutable i = 0

        let mutable wallLists = []
        let mutable wall = []
        let mutable maxY = 0

        while i < input.Length do
            let (i2, x) = parseIntToAny input i
            let (i3, y) = parseIntToAny input i2
            i <- i3
            wall <- (x, y) :: wall

            if y > maxY then
                maxY <- y

            if input[i - 1] = '\n'B then
                wallLists <- wall :: wallLists
                wall <- []

        wallLists <- wall :: wallLists

        let height = maxY + 3
        let width = height * 2
        let offsetX = 500 - (width / 2)
        let mutable walls = Bitmap.create width height offsetX 0

        for wall in wallLists do
            for ((px, py), (x, y)) in List.pairwise wall do
                for dx = (min x px) to (max x px) do
                    for dy = (min y py) to (max y py) do
                        walls |> Bitmap.set dx dy true

        maxY, walls

    let run (input: byte[]) (output: int -> string -> unit) =
        let maxDepth, walls = parse input

        let rec addSand depth path sand (walls: BitmapType) =
            match path with
            | (sx, sy) :: rest when sy < depth ->
                if walls |> Bitmap.get sx (sy + 1) |> not then
                    (sand, walls) ||> addSand depth ((sx, sy + 1) :: path)
                elif walls |> Bitmap.get (sx - 1) (sy + 1) |> not then
                    (sand, walls) ||> addSand depth ((sx - 1, sy + 1) :: path)
                elif walls |> Bitmap.get (sx + 1) (sy + 1) |> not then
                    (sand, walls) ||> addSand depth ((sx + 1, sy + 1) :: path)
                else
                    walls |> Bitmap.set sx sy true
                    ((sand + 1), walls) ||> addSand depth rest
            | _ -> sand

        let part1 = (0, walls) ||> addSand maxDepth [ (500, 0) ]

        part1 |> string |> output 1

        let floorHeight = maxDepth + 2
        let (_, width, offsetX, _) = walls

        for x = 0 to (width - 1) do
            walls |> Bitmap.set (x + offsetX) floorHeight true

        let maxDepth = maxDepth + 3

        let part2 = (part1, walls) ||> addSand maxDepth [ (500, 0) ]

        part2 |> string |> output 2

    let printWalls (walls: char[,]) =
        let screenW = System.Console.WindowWidth
        let screenH = System.Console.WindowHeight
        let screenH = screenH - 5

        let mapWidth = (Array2D.length1 walls)
        let mapHeight = (Array2D.length2 walls)

        let columns = ceil ((float mapHeight) / (float screenH)) |> int

        let maxColumns = screenW / ((mapWidth) + 1)

        let columns = max columns maxColumns

        let rows = mapHeight / columns

        for r = 1 to rows do
            for c = 1 to columns do
                for x = 1 to mapWidth do
                    let x = x - 1
                    let y = ((c - 1) * (screenH)) + (r - 1)

                    if x < mapWidth && y < mapHeight then
                        printf "%c" walls[x, y]
                    else
                        printf "@"

                printf "X"

            printf "\n"

    let inline array2tuple (a: 'a[]) : 'a * 'a = a[0], a[1]

    let runVisualize (input: byte[]) (output: int -> string -> unit) =

        let wallsList =
            input
            |> text
            |> splitLine
            |> Array.map (fun line -> line.Split(" -> ") |> Array.map (ints >> array2tuple))

        let maxY = wallsList |> Array.fold (Array.fold (fun my (_, y) -> max my y)) 0

        let height = maxY + 3
        let width = height * 2
        let offsetX = 500 - (width / 2)

        let walls = Array2D.create width height ' '

        for wall in wallsList do
            let mutable (sx, sy) = wall[0]
            sx <- sx - offsetX

            for (px, py) in Array.skip 1 wall do
                let px = px - offsetX

                if sx = px then
                    for y = (min sy py) to (max sy py) do
                        walls[sx, y] <- '#'
                else
                    for x = (min sx px) to (max sx px) do
                        walls[x, sy] <- '#'

                sx <- px
                sy <- py

        let addSand maxY (walls: char[,]) =
            let mutable sx = 500 - offsetX
            let mutable sy = 0
            let mutable stopped = false

            if walls[sx, sy] <> ' ' then
                false
            else

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
                    walls[sx, sy] <- 'O'

                stopped

        // printfn "Start:"
        // printWalls walls

        let mutable totalSand = 0

        while addSand maxY walls do
            // printfn "Sand: %i" totalSand
            // printWalls walls
            totalSand <- totalSand + 1

        // printfn "Final:"
        // printWalls walls

        totalSand |> string |> output 1

        let maxY = maxY + 2

        for x = 0 to (width - 1) do
            walls[x, maxY] <- '#'

        // printWalls walls

        while addSand maxY walls do
            // printfn "Sand: %i" totalSand
            // printWalls walls
            totalSand <- totalSand + 1

        // printWalls walls
        // 15047 is too low, 27694 too high
        totalSand |> string |> output 2
