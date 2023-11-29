namespace AdventOfCode.FSharp.Y2016

// Day 24: Air Duct Spelunking
module Day24 =
    open AdventOfCode.FSharp.Util

    let parseInput (input: byte array) =
        let lines = input |> text |> splitLine
        let size = lines |> Array.map String.length |> Array.sum
        let map = bits size
        let allWires = Array.zeroCreate 10
        let mutable maxWire = 0

        for row = 0 to lines.Length - 1 do
            let line = lines[row]

            for col = 0 to line.Length - 1 do

                let pos = (row * line.Length) + col

                match line[col] with
                | '#' -> map.Set(pos, true)
                | c when '0' <= c && c < '9' ->
                    let wire = c |> string |> int
                    allWires[wire] <- (row, col)
                    maxWire <- max maxWire wire
                | _ -> ()

        let size = lines.Length, lines[0].Length
        let wires = allWires |> Array.take (maxWire + 1)
        size, map, wires

    let intInfinity = System.Int32.MaxValue

    let run (input: byte array) (output: int -> string -> unit) =
        let (mx, my), map, wires = input |> parseInput

        let wireCosts = Array2D.zeroCreate wires.Length wires.Length

        let walls fx fy tx ty =
            let offset = (tx * my) + ty
            if map[offset] then None else Some 1

        for pos = 0 to wires.Length - 1 do
            wireCosts[pos, pos] <- Some 0
            let ix, iy = wires[pos]

            for dest = (pos + 1) to wires.Length - 1 do
                let dx, dy = wires[dest]

                let costs =
                    Dijkstra2D.init mx my intInfinity
                    |> Dijkstra2D.add ix iy 0
                    |> Dijkstra2D.run 4 0 (OrthoGrid.movesToBuffers walls) (fun x y -> x = dx && y = dy)
                    |> fst

                if costs[dx, dy] < intInfinity then
                    wireCosts[pos, dest] <- Some costs[dx, dy]
                    wireCosts[dest, pos] <- Some costs[dx, dy]

        let moves (pos, visited) =
            wireCosts[pos, *]
            |> Array.mapi (fun i o -> o |> Option.map (fun c -> (i, visited |> Set.add i), c))
            |> Array.choose id

        let goal1 (_, visited: Set<int>) = visited.Count = wires.Length

        DijkstraMap.empty
        |> DijkstraMap.add (0, Set [ 0 ]) 0
        |> DijkstraMap.run intInfinity moves goal1
        |> fst
        |> Map.toSeq
        |> Seq.filter (fst >> goal1)
        |> Seq.map snd
        |> Seq.min
        |> string
        |> output 1

        let goal2 (pos, visited: Set<int>) = pos = 0 && visited.Count = wires.Length

        DijkstraMap.empty
        |> DijkstraMap.add (0, Set [ 0 ]) 0
        |> DijkstraMap.run intInfinity moves goal2
        |> fst
        |> Map.toSeq
        |> Seq.filter (fst >> goal2)
        |> Seq.map snd
        |> Seq.min
        |> string
        |> output 2
