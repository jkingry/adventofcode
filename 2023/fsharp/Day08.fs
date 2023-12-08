namespace AdventOfCode.FSharp.Y2023

// Day 8: Haunted Wasteland 
module Day08 =
    open AdventOfCode.FSharp.Util

    type Directions =
        | Left
        | Right

    let parseNode nodes line = 
        match line with
        | Regex "(.+) = \((.+), (.+)\)" [ node; left; right ]
            -> nodes |> Map.add node (left, right)
        | _ -> failwithf "Invalid line: %s" line
        
    let parse (input: byte[]) =
        let sections = input |> text |> splitDoubleLine
        let dirs = sections[0].ToCharArray() |> Array.map (function | 'L' -> Left | 'R' -> Right | c -> failwithf "Invalid: %c" c)

        let nodes = 
            sections[1]
            |> splitLine
            |> Array.fold parseNode Map.empty
        
        dirs, nodes

    let rec countSteps des (nodes: Map<string, string*string>) (dirs: Directions[]) src step =
        if src = des then step else
        let node = nodes[src]
        let next =
            match dirs[step % (Array.length dirs)] with
            | Left -> fst node
            | Right-> snd node

        countSteps des nodes dirs next (step + 1)

    let run (input: byte[]) (output: int -> string -> unit) =
        let dirs, nodes = parse input

        countSteps "ZZZ" nodes dirs "AAA" 0        
        |> string
        |> output 1
