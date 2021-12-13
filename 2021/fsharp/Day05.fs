namespace AdventOfCode.FSharp.Y2021

// Day 5: Hydrothermal Venture
module Day05 =    
    open AdventOfCode.FSharp.Util
    
    let parseLine text =
        match text with
        | Regex @"(\d+),(\d+) -> (\d+),(\d+)" [x1;y1;x2;y2] -> 
            ([int x1;int y1], [int x2;int y2])
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
        let lines = input |> splitLine |> Array.map parseLine
 
        lines
        |> Seq.filter (fun (p1, p2) -> p1[0] = p2[0] || p1[1] = p2[1]) 
        |> countPoints
        |> string

    let part2 (input : string) =
        let lines = input |> splitLine |> Array.map parseLine 
        lines |> countPoints |> string

