namespace AdventOfCode.FSharp.Y2022

// Day 5: Supply Stacks
module Day05 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =    
        let parseLayout layout : List<char>[] =
            let lines = layout |> splitLine
            let stackCount = lines |> Array.last |> ints |> Array.max

            let text = 
                lines 
                |> Array.take (lines.Length - 1)
                |> array2D

            [0..(stackCount - 1)] 
                |> List.map (fun i -> 
                    text[*, 1 + (i * 4)] 
                        |> Array.filter (fun c -> c <> ' ') 
                        |> List.ofArray)
                |> Array.ofList
        
        let parseInstructions instructions =
            instructions
            |> splitLine
            |> Array.choose (fun line -> 
                match line with 
                | Regex @"move (\d+) from (\d+) to (\d+)" [amt; src; dest] -> Some (int amt, int src, int dest)
                | _ -> None)
                    
        let (layout, instructions) = 
            match input |> splitDoubleLine with
            | [| a; b |] -> (parseLayout a, parseInstructions b)
            | _ -> failwith "bad format"

        let topBox layout = layout |> Array.map List.head |> System.String 

        let CrateMover9000 (s: List<char>[]) (amt, src, dest) =
            for _ in 1..amt do
                let box = List.head s[src - 1] 
                s[src - 1] <- List.tail s[src - 1]
                s[dest - 1] <- box::s[dest - 1]

        let part1 = layout |> Array.copy
        instructions |> Array.iter (CrateMover9000 part1) 
        part1 |> topBox |> output 1

        let CrateMover9001 (s: List<char>[]) (amt, src, dest) =
            let mutable pick = []

            for _ in 1..amt do
                let box = List.head s[src - 1]
                pick <- box::pick
                s[src - 1] <- List.tail s[src - 1]

            for p in pick do
                s[dest - 1] <- p :: s[dest - 1]

        let part2 = layout |> Array.copy
        instructions |> Array.iter (CrateMover9001 part2) 
        part2 |> topBox |> output 2
