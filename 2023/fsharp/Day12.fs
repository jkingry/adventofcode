namespace AdventOfCode.FSharp.Y2023

// Day 12: Hot Springs
module Day12 =
    open AdventOfCode.FSharp.Util

    let countMatches (pattern: char list) (expected: int list) =
        let mutable cache = Map.empty

        let rec countMatchesCached (pattern: char list) (expected: int list) curDmg =
            let cacheKey = pattern, expected, curDmg

            match cache |> Map.tryFind cacheKey with
            | Some n -> n
            | None ->
                let result = countMatchesActual pattern expected curDmg
                cache <- cache |> Map.add cacheKey result
                result

        and countMatchesActual (pattern: char list) (expected: int list) curDmg =
            match pattern, expected, curDmg with
            | '?' :: xs, fg :: ys, cd ->
                match cd with
                | 0 -> (countMatchesCached xs expected 1) + (countMatchesCached xs expected 0)
                | cd when cd < fg -> countMatchesCached xs expected (cd + 1)
                | cd when cd = fg -> countMatchesCached xs ys 0
                | _ -> 0L
            | '?' :: xs, [], 0 -> countMatchesCached xs [] 0

            | '#' :: xs, _, 0 -> countMatchesCached xs expected 1
            | '.' :: xs, _, 0 -> countMatchesCached xs expected 0

            | '#' :: xs, fg :: _, cd when cd < fg -> countMatchesCached xs expected (cd + 1)
            | '.' :: xs, fg :: ys, cd when cd = fg -> countMatchesCached xs ys 0

            | [], [ fg ], cd when fg = cd -> 1L
            | [], [], 0 -> 1L
            | _ -> 0L

        countMatchesCached pattern expected 0

    let parseLine (line: string) =
        match line.Split(' ') with
        | [| a; b |] -> (a.ToCharArray() |> Array.toList), (ints b |> Array.toList)
        | _ -> failwith "Invalid line"

    let run (input: byte[]) (output: int -> string -> unit) =
        let lines = input |> text |> splitLine |> Array.map parseLine

        lines
        |> Array.map (fun (pattern, conditions) -> countMatches pattern conditions)
        |> Array.sum
        |> string
        |> output 1

        lines
        |> Array.map (fun (pattern, conditions) ->
            let unfoldedPattern =
                List.replicate 5 ('?' :: pattern) |> List.concat |> List.skip 1

            let unfoldedConditions = List.replicate 5 conditions |> List.concat
            unfoldedPattern, unfoldedConditions)
        |> Array.map (fun (pattern, conditions) ->
            let matches = countMatches pattern conditions
            matches |> int64)
        |> Array.sum
        |> string
        |> output 2
