namespace AdventOfCode.FSharp.Y2021

// Day 13: Transparent Origami
module Day13 =
    open AdventOfCode.FSharp.Util

    let parse text =
        let dots =
            (text |> splitDoubleLine).[0]
            |> splitLine
            |> Array.map ints
            |> Array.map
                (function
                | [| x; y |] -> (x, y)
                | _ -> failwith "Bad format")
            |> Set.ofArray

        let folds =
            (text |> splitDoubleLine).[1]
            |> splitLine
            |> Array.map
                (fun line ->
                    match line with
                    | Regex @"fold along (y|x)=(\d+)" [ axis; num ] -> (axis, int num)
                    | _ -> failwith "Bad format")

        dots, folds

    let foldy fold dots =
        dots
        |> Seq.filter (fun (_, y) -> y > fold)
        |> Seq.fold (fun a (x, y) -> a |> Set.add (x, 2 * fold - y)) dots
        |> Set.filter (fun (_, y) -> y <= fold)

    let foldx fold dots =
        dots
        |> Seq.filter (fun (x, _) -> x > fold)
        |> Seq.fold (fun a (x, y) -> a |> Set.add (2 * fold - x, y)) dots
        |> Set.filter (fun (x, _) -> x <= fold)

    let fold dots f =
        match f with
        | ("x", n) -> foldx n dots
        | ("y", n) -> foldy n dots
        | _ -> failwith "unreachable"

    let part1 text =
        let (dots, folds) = parse text

        let firstFold = folds |> Array.head
        fold dots firstFold |> Set.count |> string

    let part2 text =
        let (dots, folds) = parse text

        let foldedDots = folds |> Array.fold fold dots

        let (maxx, maxy) =
            foldedDots
            |> Seq.reduce (fun (mx, my) (x, y) -> max mx x, max my y)

        let mutable result = System.Environment.NewLine

        for y = 0 to maxy do
            let s =
                new System.Text.StringBuilder(String.replicate (maxx + 1) ".")

            let row =
                foldedDots |> Seq.filter (fun (_, yy) -> yy = y)

            for (x, _) in row do
                s.[x] <- '#'

            result <- result + (string s) + System.Environment.NewLine

        result
