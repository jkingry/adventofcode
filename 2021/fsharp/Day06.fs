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
        // let mutable n = ints(input) |> Array.toList


        // for d=1 to 80 do
        //     let mutable nn : int list = []
        //     for i=0 to n.Length-1 do
        //         let nnn =
        //             match n[i] with 
        //             | 0 -> nn <- 8::nn ; 6
        //             | _ -> n[i] - 1
        //         nn <- nnn::nn
        //     n <- nn
        // Seq.length n
        -1


 

    let part2 (input : string) =
        let mutable m  = ints(input) |> Array.map (fun v -> int64 v) |> Array.toList |> List.groupBy id |> List.map (fun (k,v) -> (k,List.length v |> int64) ) |> Map.ofList



        for d=1 to 256 do
            let mutable mm : Map<int64, int64> = Map.empty
            for x in m do
                let k : int64 = x.Key
                let v : int64 = x.Value
                if k = 0 then   
                    mm <- mm |> Map.change 8 (fun vv -> match vv with | Some n -> Some (n + v) | _ -> Some v)         
                    mm <- mm |> Map.change 6 (fun vv -> match vv with | Some n -> Some (n + v) | _ -> Some v)            
                else
                    mm <- mm |> Map.change (k - 1L) (fun vv -> match vv with | Some n -> Some (n + v) | _ -> Some v)            
            m <- mm
        m.Values |> Seq.sum
