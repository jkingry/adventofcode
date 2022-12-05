namespace AdventOfCode.FSharp.Y2022

// Day 5: ???
module Day05 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =    
        let parts = input |> splitDoubleLine
        let layoutLines = parts[0] |> splitLine
        let lc = layoutLines |> Array.last |> ints |> Array.max

        let lists : list<char>[] = Array.create lc [] 
        for q in layoutLines |> Array.take (layoutLines.Length - 1) do
            for i in 0..(lc - 1) do
                let c = q[1 + (i*4)]
                if c >= 'A' && c <= 'Z' then
                    lists[i] <- c::lists[i] 

        let lists = lists|> Array.map List.rev 

        printfn "%A" (List.head lists[0])

        let parse line =
            match line with
            | Regex @"move (\d+) from (\d+) to (\d+)" [m; src; dest] ->
                Some (int m, -1 + int src, -1 + int dest)
            | _ -> None        
        let b = 
            parts[1] 
            |> splitLine
            |> Seq.choose parse

        for (c, src, dest) in b do 
            let mutable pick = []
            for _ in 1..c do
                let h::sl = lists[src]
                lists[src] <- sl

                pick <- h::pick
            
            for p in pick do
                let dl = lists[dest]
                lists[dest] <- p::dl

        let part1 = lists |> Array.map (fun q -> List.head q) |> System.String


        // let part1 = 
        //     input
        //     |> splitLine
        //     |> Array.map(fun s -> 
        //         1
        //         )
        //     |> Array.sum
        
        part1 |> output 1
        0 |> string |> output 2
