namespace AdventOfCode.FSharp.Y2021

open System
open AdventOfCode.FSharp.Util

// Day 5: 
module Day05 =    
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

    let incr v =
        match v with 
        | Some s -> Some (s + 1)
        | _ -> Some 1

    let part1 (input : string seq) =
        let lines = input |> Seq.map parseLine
 
        let marked =
            lines
            |> Seq.filter (fun (p1, p2) -> p1[0] = p2[0] || p1[1] = p2[1]) 
            |> Seq.map points 
            |> Seq.concat
            |> Seq.fold (fun m s -> m |> Map.change s incr) Map.empty

        marked.Values |> Seq.filter (fun v -> v > 1) |> Seq.length

    let part2 (input : string seq) =
        let lines = input |> Seq.map parseLine
 
        let marked =
            lines
            |> Seq.map points 
            |> Seq.concat
            |> Seq.fold (fun m s -> m |> Map.change s incr) Map.empty

        marked.Values |> Seq.filter (fun v -> v > 1) |> Seq.length
