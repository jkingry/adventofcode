namespace AdventOfCode.FSharp.Y2015

// Day 8: Matchsticks
module Day08 =
    open AdventOfCode.FSharp.Util
    open System.Text.Json

    let isHex c =
        if '0' <= c && c <= '9' then true
        elif 'a' <= c && c <= 'f' then true
        else false

    let rec deserialize (x: string) (state: int) (pos: int) =
        let c = x[pos]
        let pos' = pos + 1

        match state, c with
        | 0, '\"' -> deserialize x 1 pos'
        | 1, '\"' -> 0
        | 1, '\\' -> deserialize x 2 pos'
        | 1, _ -> 1 + deserialize x 1 pos'
        | 2, '\\'
        | 2, '\"' -> 1 + deserialize x 1 pos'
        | 2, 'x' -> deserialize x 3 pos'
        | 3, n when isHex n -> deserialize x 4 pos'
        | 4, n when isHex n -> 1 + deserialize x 1 pos'
        | _, _ -> failwithf "invalid string: '%s' at position %d" x pos

    let rec serialize (x: string) =
        let mutable n = 2

        for c in x do
            match c with
            | '\"'
            | '\\' -> n <- n + 2
            | _ -> n <- n + 1

        n

    let measureString (x: string) =
        let codeLength = x.Length
        let deLength = deserialize x 0 0
        let seLength = serialize x
        codeLength - deLength, seLength - codeLength

    let run (input: byte array) (output: int -> string -> unit) =
        let (part1, part2) =
            input
            |> text
            |> splitLine
            |> Array.map measureString
            |> Array.fold (fun (p1, p2) (s1, s2) -> p1 + s1, p2 + s2) (0, 0)

        part1 |> string |> output 1
        part2 |> string |> output 2
