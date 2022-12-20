namespace AdventOfCode.FSharp.Y2022

// Day 15
module Day15 =
    open Checked
    open AdventOfCode.FSharp.Util

    let maxBoundary = 4000000
    // let maxBoundary = 20

    let inline manhattanDistance ax ay bx by = (abs (ax - bx)) + (abs (ay - by))

    let parse input =
        input 
        |> text
        |> splitLine
        |> Array.map (function 
        | Regex @"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" [sx;sy;bx;by] ->
            (int sx), (int sy), (int bx), (int by)
        | line -> failwithf "Failed parsing: %s" line)
        |> Array.map (fun (sx, sy, bx, by) ->
            let d = manhattanDistance sx sy bx by
            sx, sy, d)

    let rec addMarked (s,f) list =
        match list with
        | [] -> [(s,f)]
        | (rs, rf)::xs ->
            if intersects s f rs rf then
                addMarked ((min rs s),(max rf f)) xs
            else
                (rs,rf)::(addMarked (s,f) xs)

    let findMarked minLength marked row readings =
        let mutable marked = marked
        let mutable ri = 0
        while List.length marked > minLength && ri < (Array.length readings) do
            let (sx,sy,d) = readings[ri]
            ri <- ri + 1

            let drow = d - (abs (sy - row))

            if drow > 0 then
                let fx = sx - drow
                let tx = sx + drow

                marked <- marked |> addMarked (fx, tx)        
        marked

    let run (input: byte[]) (output: int -> string -> unit) =
        let readings = parse input

        readings
            |> findMarked -1 [] (maxBoundary / 2)  
            |> List.map (fun (a,b) -> b - a)
            |> List.sum        
            |> string |> output 1
        
        let mutable found = None
        let mutable row = 0
        let boundaries = [(System.Int32.MinValue, 0); (maxBoundary, System.Int32.MaxValue)]
        while found.IsNone && row <= maxBoundary do
            let marked = readings |> findMarked 1 boundaries row 
            if marked.Length > 1 then
                let x = (marked |> List.map snd |> List.min) + 1
                found <- (x, row) |> Some

            row <- row + 1

        let (x,y) = found.Value
        let x = x |> int64
        let y = y |> int64

        (x * 4_000_000L) + y |> string |> output 2

    let getManhattanPerimeter x y d =
        seq {
            for dx=0 to d do
                let dy = d - dx

                yield (x + dx, y + dy)
                yield (x + dx, y - dy)
                yield (x - dx, y + dy)
                yield (x - dx, y - dy)
        }

    let findElf (minX,minY) (maxX, maxY) (readings: (int*int*int)[])  =
        let mutable elf = None
        let mutable i = 0 

        while elf.IsNone && i < readings.Length do
            let (sx, sy, d) = readings[i]
            let outsideBorder =
                getManhattanPerimeter sx sy (d + 1)
                |> Seq.filter (fun (x,y) -> minX <= x && x <= maxX && minY <= y && y <= maxY)
            
            elf <-
                outsideBorder
                |> Seq.tryFind (fun (x,y) -> readings |> Array.forall (fun (sx,sy,d) -> (manhattanDistance sx sy x y) > d))
            i <- i + 1
        elf

    let runFast (input: byte[]) (output: int -> string -> unit) =
        let readings = parse input

        readings
            |> findMarked -1 [] (maxBoundary / 2)  
            |> List.map (fun (a,b) -> b - a)
            |> List.sum        
            |> string |> output 1

        let found = findElf (0,0) (maxBoundary, maxBoundary) readings

        let (x,y) = found.Value
        let x = x |> int64
        let y = y |> int64
        (x * 4_000_000L) + y |> string |> output 2

