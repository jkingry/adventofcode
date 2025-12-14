namespace AdventOfCode.FSharp.Y2025

// Day 08
module Day08 =
    open AdventOfCode.FSharp.Util
    open FSharpx.Collections

    let inline manhattanDistance ((a, b, c), (d, e, f)) = (abs a - d) + (abs b - e) + (abs f - c)

    let inline squareDistance ((a, b, c), (d, e, f)) =
        let a = int64 a
        let b = int64 b
        let c = int64 c
        let d = int64 d
        let e = int64 e
        let f = int64 f
        (a - d) * (a - d) + (b - e) * (b - e) + (f - c) * (f - c)

    let run (input: byte array) (output: int -> string -> unit) =
        let points =
            input
            |> bsplit '\n'B
            |> Array.map parseInts
            |> Array.map (function
                | [| a; b; c |] -> a, b, c
                | _ -> failwith "invalid line")

        let mutable q =
            points
            |> Seq.map (fun a -> points |> Seq.takeWhile (fun b -> b <> a) |> Seq.map (fun b -> a, b))
            |> Seq.concat
            |> Seq.map (fun pair -> squareDistance pair, pair)
            |> Heap.ofSeq false

        let part1Index = if points.Length < 1000 then 10 else 1000

        let getTop3SizeMetric circuits =
            circuits
            |> List.map Set.count
            |> List.sortDescending
            |> List.take 3
            |> List.reduce (fun a b -> a * b)

        let mutable part1Top3SizeMetric = None
        let mutable part2LastPoints = None

        let mutable circuits = []
        let mutable index = 0

        while not q.IsEmpty
              && (circuits.Length <> 1 || circuits |> List.head |> Set.count <> points.Length) do
            let (_, (a, b)), nq = Heap.uncons q
            q <- nq
            index <- index + 1

            part2LastPoints <- Some(a, b)

            let overlap, remainder =
                circuits |> List.partition (fun c -> c |> Set.contains a || c |> Set.contains b)

            circuits <-
                match overlap with
                | [] -> Set [ a; b ] :: circuits
                | _ ->
                    let combined = Set.unionMany overlap |> Set.add a |> Set.add b
                    combined :: remainder

            if index = part1Index then
                part1Top3SizeMetric <- getTop3SizeMetric circuits |> Some


        part1Top3SizeMetric.Value |> string |> output 1

        match part2LastPoints with
        | Some((a, _, _), (b, _, _)) -> int64 a * int64 b
        | _ -> failwith "solution not found"
        |> string
        |> output 2
