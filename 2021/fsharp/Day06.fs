namespace AdventOfCode.FSharp.Y2021

open System
open AdventOfCode.FSharp.Util

// Day 5: Hydrothermal Venture
module Day06 =    
    let parseLine text =
        match text with
        | Regex @"(\d+),(\d+) -> (\d+),(\d+)" [x1;y1;x2;y2] -> 
            ([Int32.Parse x1;Int32.Parse y1], [Int32.Parse x2;Int32.Parse y2])
        | _ -> failwith "Invalid"
        
    let points (p1, p2) =
        let delta = List.map2 (fun a b -> sign(b - a)) p1 p2
        let mutable n = p1
        seq {
            yield n
            while n <> p2 do 
                n <- List.map2 (fun a b -> a + b) n delta
                yield n
        }

    let countPoints lines =
        lines
        |> Seq.map points
        |> Seq.concat
        |> Seq.groupBy id
        |> Seq.map snd
        |> Seq.filter (fun v -> (Seq.length v) > 1)
        |> Seq.length

    let part1 (input : string) =
        let lines = input |> lineSplit |> Seq.map parseLine
        -1
 

    let part2 (input : string) =
        -1
