namespace AdventOfCode.FSharp.Y2021


// Day 10: Syntax Scoring
module Day10 =
    open AdventOfCode.FSharp.Util
    open Checked

    let errorMap = [ ')', 3L; ']', 57L; '}', 1197L; '>', 25137L ] |> Map.ofList

    let score = [ '(', 1L; '[', 2L; '{', 3L; '<', 4L ] |> Map.ofList

    let lineStack line =
        line
        |> Seq.fold
            (fun a c ->
                match a with
                | Error _ -> a
                | Ok s ->
                    match c with
                    | '('
                    | '['
                    | '{'
                    | '<' -> Ok(c :: s)
                    | ')'
                    | ']'
                    | '}'
                    | '>' ->
                        match s with
                        | [] -> Error errorMap.[c]
                        | x :: xs ->
                            // dirty ASCII tricks
                            if abs ((int c) - (int x)) <= 2 then
                                Ok xs
                            else
                                Error errorMap.[c]
                    | _ -> a)
            (Ok [])


    let part1 (text: string) =
        text
        |> splitLine
        |> Array.map lineStack
        |> Array.map (function
            | Error v -> v
            | _ -> 0L)
        |> Array.sum
        |> string

    let part2 (text: string) =
        let totals =
            text
            |> splitLine
            |> Array.map lineStack
            |> Array.choose (function
                | Error _ -> None
                | Ok s -> Some s)
            |> Array.map (fun s -> s |> List.fold (fun a v -> (a * 5L) + score.[v]) 0L)
            |> Array.sort

        totals.[totals.Length / 2] |> string
