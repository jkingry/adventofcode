namespace AdventOfCode.FSharp.Y2022

// Day 5: ???
module Day05 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =    
        let parseLayoutLine stackCount stacks (line: string) =
            let mutable s = stacks
            for i in 1..stackCount do
                let box = line[1 + ((i-1) * 4)]
                if 'A' <= box && box <= 'Z' then 
                    s <- s |> Map.change i (fun stack -> Some (box::(stack |> Option.defaultValue []))) 
            s

        let parseLayout layout =
            let lines = layout |> splitLine
            let stackCount = lines |> Array.last |> ints |> Array.max
            lines 
                |> Array.take (lines.Length - 1)
                |> Array.fold (parseLayoutLine stackCount) Map.empty
                |> Map.map (fun _ v -> List.rev v)
        
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

        let topBox layout = layout |> Map.toSeq |> Seq.sortBy fst |> Seq.map (snd >> List.head) |> Seq.toArray 

        let CrateMover9000 (stacks: Map<int,List<char>>) (amt, src, dest) =
            let mutable s = stacks
            for _ in 1..amt do
                s <- s 
                    |> Map.add src (List.tail s[src]) 
                    |> Map.add dest ((List.head s[src])::s[dest])
            s  

        let part1 = instructions |> Array.fold CrateMover9000 layout |> topBox

        part1 |> System.String |> output 1

        let CrateMover9001 (stacks: Map<int, List<char>>) (amt, src, dest) =
            let mutable s = stacks

            let mutable pick = []
            for _ in 1..amt do
                pick <- (List.head s[src])::pick
                s <- s |> Map.add src (List.tail s[src]) 
            for p in pick do
                s <- s |> Map.add dest (p::s[dest])
            s 

        let part2 = instructions |> Array.fold CrateMover9001 layout |> topBox

        part2 |> System.String |> output 2
