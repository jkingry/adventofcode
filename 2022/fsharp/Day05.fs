namespace AdventOfCode.FSharp.Y2022

// Day 5: Supply Stacks
module Day05 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: byte array) (output: int -> string -> unit) =    
        let parseLayout layout : List<char>[] =
            let lines = layout |> splitLine
            let stackCount = lines |> Array.last |> ints |> Array.max
            let text = lines |> Array.take (lines.Length - 1) |> array2D

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
                | Regex @"move (\d+) from (\d+) to (\d+)" [amt; src; dst] -> Some (int amt, int src, int dst)
                | _ -> None)
                    
        let (layout, instructions) = 
            match input |> text |> splitDoubleLine with
            | [| a; b |] -> (parseLayout a, parseInstructions b)
            | _ -> failwith "bad format"

        let topBox layout = layout |> Array.map List.head |> System.String 

        let CrateMover9000 (s: List<char>[]) (amt, src, dst) =
            let (pick, srcList') = s[src - 1] |> List.splitAt amt 
            s[src - 1] <- srcList'
            s[dst - 1] <- List.append (List.rev pick) s[dst - 1]

        let CrateMover9001 (s: List<char>[]) (amt, src, dst) =
            let (pick, srcList') = s[src - 1] |> List.splitAt amt 
            s[src - 1] <- srcList'
            s[dst - 1] <- List.append pick s[dst - 1]

        let part1 = layout |> Array.copy
        let part2 = layout |> Array.copy

        for instr in instructions do
            CrateMover9000 part1 instr
            CrateMover9001 part2 instr

        part1 |> topBox |> output 1
        part2 |> topBox |> output 2
