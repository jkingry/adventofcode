namespace AdventOfCode.FSharp.Y2023

// Day 8: Haunted Wasteland
module Day08 =
    open AdventOfCode.FSharp.Util

    type Directions =
        | Left
        | Right

    let parseNode nodes line =
        match line with
        | Regex "(.+) = \((.+), (.+)\)" [ node; left; right ] -> nodes |> Map.add node (left, right)
        | _ -> failwithf "Invalid line: %s" line

    let parse (input: byte[]) =
        let sections = input |> text |> splitDoubleLine

        let dirs =
            sections[ 0 ].ToCharArray()
            |> Array.map (function
                | 'L' -> Left
                | 'R' -> Right
                | c -> failwithf "Invalid: %c" c)

        let nodes = sections[1] |> splitLine |> Array.fold parseNode Map.empty

        dirs, nodes

    let nextNode (nodes: Map<'a, 'a * 'a>) (dirs: Directions[]) src step =
        let node = nodes[src]

        match dirs[step % dirs.Length] with
        | Left -> fst node
        | Right -> snd node

    let rec countCamelSteps next goalFunc src step =
        if goalFunc (src) then
            step
        else
            countCamelSteps next goalFunc (next src step) (step + 1)

    let run (input: byte[]) (output: int -> string -> unit) =
        let dirs, nodes = parse input

        let next = nextNode nodes dirs

        countCamelSteps next (fun s -> s = "ZZZ") "AAA" 0 |> string |> output 1

        let steps =
            nodes
            |> Map.keys
            |> Seq.filter (fun s -> s[2] = 'A')
            |> Seq.map (fun s -> countCamelSteps next (fun s -> s[2] = 'Z') s 0)
            |> Seq.map int64
            |> Array.ofSeq

        let period = steps |> Array.reduce gcd

        steps |> Array.reduce (fun a b -> (a * b) / period) |> string |> output 2

    let nextIndex (nodes: (int * int)[]) (dirs: Directions[]) src step =
        let node = nodes[src]

        match dirs[step % dirs.Length] with
        | Left -> fst node
        | Right -> snd node

    let parseArray (input: byte[]) =
        let dirs, nodes = parse input

        let nodeToIndex =
            nodes |> Map.keys |> Seq.indexed |> Seq.map (fun (a, b) -> b, a) |> Map.ofSeq

        let nodeArray = Array.create nodes.Count (0, 0)

        for k, (lft, rgt) in nodes |> Map.toSeq do
            nodeArray[nodeToIndex[k]] <- (nodeToIndex[lft], nodeToIndex[rgt])

        dirs, nodeArray, nodeToIndex

    let runArray (input: byte[]) (output: int -> string -> unit) =
        let dirs, nodes, indexMap = parseArray input

        let next = nextIndex nodes dirs

        let zzzGoal = indexMap["ZZZ"]

        countCamelSteps next (fun s -> s = zzzGoal) indexMap["AAA"] 0
        |> string
        |> output 1

        let goalNodes =
            indexMap
            |> Map.toSeq
            |> Seq.choose (fun (k, v) -> if k[2] = 'Z' then Some v else None)
            |> Set.ofSeq

        let steps =
            indexMap
            |> Map.toSeq
            |> Seq.choose (fun (k, v) -> if k[2] = 'A' then Some v else None)
            |> Seq.map (fun s -> countCamelSteps next (goalNodes.Contains) s 0)
            |> Seq.map int64
            |> Array.ofSeq

        let period = steps |> Array.reduce gcd

        steps |> Array.reduce (fun a b -> (a * b) / period) |> string |> output 2
