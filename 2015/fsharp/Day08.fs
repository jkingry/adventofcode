namespace AdventOfCode.FSharp.Y2015

// Day 8: Matchsticks
module Day08 =
    open AdventOfCode.FSharp.Util
    open System

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

    let rec measure2 (x: string) =
        let mutable part1 = 0
        let mutable part2 = 2
        let mutable state = 0

        for c in x do
            match c with
            | '\"'
            | '\\' -> part2 <- part2 + 2
            | _ -> part2 <- part2 + 1

            match state, c with
            | 0, '\"' -> state <- 1
            | 1, '\"' -> state <- -1
            | 1, '\\' -> state <- 2
            | 1, _ -> part1 <- part1 + 1
            | 2, '\\'
            | 2, '\"' ->
                part1 <- part1 + 1
                state <- 1
            | 2, 'x' -> state <- 3
            | 3, n when isHex n -> state <- 4
            | 4, n when isHex n ->
                part1 <- part1 + 1
                state <- 1
            | _, _ -> failwithf "invalid string: '%s'" x

        x.Length - part1, part2 - x.Length

    let runForLoop (input: byte array) (output: int -> string -> unit) =
        let part1, part2 =
            input
            |> text
            |> splitLine
            |> Array.map measure2
            |> Array.fold (fun (p1, p2) (s1, s2) -> p1 + s1, p2 + s2) (0, 0)

        part1 |> string |> output 1
        part2 |> string |> output 2


    let isHexByte c =
        if '0'B <= c && c <= '9'B then true
        elif 'a'B <= c && c <= 'f'B then true
        else false

    let rec deserializeSpan (x: ReadOnlySpan<byte>) (state: int) (pos: int) =
        let c = x[pos]
        let pos' = pos + 1

        match state, c with
        | 0, '\"'B -> deserializeSpan x 1 pos'
        | 1, '\"'B -> 0
        | 1, '\\'B -> deserializeSpan x 2 pos'
        | 1, _ -> 1 + deserializeSpan x 1 pos'
        | 2, '\\'B
        | 2, '\"'B -> 1 + deserializeSpan x 1 pos'
        | 2, 'x'B -> deserializeSpan x 3 pos'
        | 3, n when isHexByte n -> deserializeSpan x 4 pos'
        | 4, n when isHexByte n -> 1 + deserializeSpan x 1 pos'
        | _, _ -> failwithf "invalid string at position %d" pos

    let serializeSpan (x: ReadOnlySpan<byte>) =
        let mutable n = 2

        for c in x do
            match c with
            | '\"'B
            | '\\'B -> n <- n + 2
            | _ -> n <- n + 1

        n

    let runSpans (input: byte array) (output: int -> string -> unit) =
        let inputSpan = ReadOnlySpan<byte> input

        let mutable part1 = 0
        let mutable part2 = 0

        for r in inputSpan.Split '\n'B do
            let lineSpan = trySlice r inputSpan

            if lineSpan.Length > 0 then
                let deserializeLength = deserializeSpan lineSpan 0 0
                let serializeLength = serializeSpan lineSpan
                let codeLength = lineSpan.Length
                part1 <- part1 + (codeLength - deserializeLength)
                part2 <- part2 + (serializeLength - codeLength)

        part1 |> string |> output 1
        part2 |> string |> output 2
