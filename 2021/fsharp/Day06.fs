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
        let mutable ages : int64[] = Array.zeroCreate 9
        let mutable ages' : int64[] = Array.zeroCreate 9

        let mutable m  = 
            ints(input) 
            |> Array.groupBy id 
            |> Array.iter (fun (k,v) -> ages[k] <- v.LongLength) 

        let day () =
            for i= 8 downto 0 do
                ages'[i] <- 0
            
            for i= 8 downto 0 do
                let v = ages[i]
                if i = 0 then
                    ages'[8] <- v
                    ages'[6] <- ages'[6] + v
                else
                    ages'[i-1] <- v
            let temp = ages
            ages <- ages'
            ages' <- temp

        for i=1 to 256 do
            day ()
        
        ages |> Array.sum
