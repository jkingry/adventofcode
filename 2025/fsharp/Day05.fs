namespace AdventOfCode.FSharp.Y2025

// Day 5: Cafeteria
module Day05 =
    open AdventOfCode.FSharp.Util

    let inline isFresh db n =
        db |> List.exists (fun (a, b) -> a <= n && n <= b)

    let condenseDb db (a, b) =
        let matches, remainder =
            db
            |> List.partition (fun (c, d) -> abs (a - d) = 1L || abs (c - b) = 1L || intersects a b c d)

        let merged =
            (a, b) :: matches |> List.reduce (fun (ax, ay) (bx, by) -> min ax bx, max ay by)

        merged :: remainder

    let run (input: byte array) (output: int -> string -> unit) =
        let sections = input |> text |> splitDoubleLine

        let db =
            sections[0]
            |> splitLine
            |> Seq.map (function
                | Regex "(\d+)-(\d+)" [ a; b ] -> int64 a, int64 b
                | s -> failwithf "invalid line: %s" s)
            |> Seq.fold condenseDb []

        let numbers = sections[1] |> splitLine |> Array.map int64

        numbers |> Array.filter (isFresh db) |> Array.length |> string |> output 1

        db |> List.map (fun (a, b) -> b - a + 1L) |> List.sum |> string |> output 2
