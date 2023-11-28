namespace AdventOfCode.FSharp.Y2016

// Day 22: Grid Computing
module Day22 =
    open AdventOfCode.FSharp.Util

    let parseInput (input: byte array) =
        input
        |> text
        |> splitLine
        |> Array.skip 2
        |> Array.map (function
            | Regex @"/dev/grid/node-x([0-9]+)-y([0-9]+)\s+[0-9]+T\s+([0-9]+)T\s+([0-9]+)T" [ x; y; used; avail ] ->
                ((int x), (int y)), (int used), (int avail)
            | line -> failwithf "Unknown format: %s" line)

    let run (input: byte array) (output: int -> string -> unit) =
        let data = parseInput input

        data
        |> Array.filter (fun (_, used, _) -> used > 0)
        |> Seq.collect (fun (a, used, _) ->
            data
            |> Array.filter (fun (b, _, avail) -> b <> a && avail >= used)
            |> Array.map (fun (b, _, _) -> a, b))
        |> Seq.length
        |> string
        |> output 1

        let (emptyNode, _, emptyAvail) = data |> Array.find (fun (_, used, _) -> used = 0)

        let blockedNodes =
            data
            |> Array.filter (fun (_, used, _) -> used > emptyAvail)
            |> Array.map (fun (n, _, _) -> n)
            |> Set.ofArray

        let maxY = data |> Array.map (fun (n, _, _) -> n) |> Array.map snd |> Array.max
        let maxX = data |> Array.map (fun (n, _, _) -> n) |> Array.map fst |> Array.max

        let goalNode = maxX, 0
        let bounds = maxX + 1, maxY + 1
        let initialState = emptyNode, goalNode

        let moves (emptyPos, goalPos) =
            OrthoGrid.movesToSeq bounds emptyPos
            |> Seq.filter (fun destination -> blockedNodes |> Seq.contains destination |> not)
            |> Seq.map (fun destination ->
                let goalPos' = if destination = goalPos then emptyPos else goalPos
                (destination, goalPos'), 1)

        let goal (_, goalNode) = goalNode = (0, 0)

        let h ((ex, ey), (gx, gy)) = (max (ex + ey - 1) 0) + gx + gy

        let costs =
            DijkstraMap.empty
            |> DijkstraMap.add initialState 0
            |> DijkstraMap.runAstar System.Int32.MaxValue moves goal h
            |> fst

        let goal = costs |> Map.findKey (fun k _ -> goal k)
        costs[goal] |> string |> output 2
