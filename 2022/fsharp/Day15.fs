namespace AdventOfCode.FSharp.Y2022

// Day 15
module Day15 =
    open Checked
    open AdventOfCode.FSharp.Util

    let parse input =
        input 
        |> text
        |> splitLine
        |> Array.map (function 
        | Regex @"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" [sx;sy;bx;by] ->
            (int sx), (int sy), (int bx), (int by)
        | line -> failwithf "Failed parsing: %s" line)

    let rec addMarked (s,f) list =
        match list with
        | [] -> [(s,f)]
        | (rs, rf)::xs ->
            if intersects s f rs rf then
                addMarked ((min rs s),(max rf f)) xs
            else
                (rs,rf)::(addMarked (s,f) xs)

    let findMarked marked row readings =
        let mutable marked = marked
        
        for (sx,sy,bx,by ) in readings do
            let d = (abs (sx - bx)) + (abs (sy - by))
            let drow = d - (abs (sy - row))

            if drow > 0 then
                let fx = sx - drow
                let tx = sx + drow

                marked <- marked |> addMarked (fx, tx)        
        marked

    let run (input: byte[]) (output: int -> string -> unit) =
        let readings = parse input

        let maxBoundary = 4000000
        // let maxBoundary = 20

        readings
            |> findMarked [] (maxBoundary / 2)  
            |> List.map (fun (a,b) -> b - a)
            |> List.sum        
            |> string |> output 1
        
        let mutable found = None
        let mutable row = 0
        let boundaries = [(System.Int32.MinValue, 0); (maxBoundary, System.Int32.MaxValue)]
        while found.IsNone && row <= maxBoundary do
            let marked = readings |> findMarked boundaries row 
            if marked.Length > 1 then
                let x = (marked |> List.map snd |> List.min) + 1
                found <- (x, row) |> Some

            row <- row + 1

        let (x,y) = found.Value
        let x = x |> int64
        let y = y |> int64

        (x * 4_000_000L) + y |> string |> output 2
