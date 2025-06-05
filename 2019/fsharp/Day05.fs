namespace AdventOfCode.FSharp.Y2019

// Day 5: Sunny with a Chance of Asteroids
module Day05 =
    open AdventOfCode.FSharp.Util

    let inline getParameter1Mode op =
        match op / 100 % 10 with
        | 0 -> false
        | 1 -> true
        | x -> failwithf "Invalid parameter mode: %i" x

    let inline getParameter2Mode op =
        match op / 1000 with
        | 0 -> false
        | 1 -> true
        | x -> failwithf "Invalid parameter mode: %i" x

    let stepProgram (ops: int[]) (pos: int, io) =
        let op = ops[pos]

        match op % 100 with
        | 1 -> // add
            let p1 = ops[pos + 1]
            let p2 = ops[pos + 2]
            let r = ops[pos + 3]

            let p1v = if getParameter1Mode op then p1 else ops[p1]
            let p2v = if getParameter2Mode op then p2 else ops[p2]

            ops[r] <- p1v + p2v
            pos + 4, io
        | 2 -> // multiply
            let p1 = ops[pos + 1]
            let p2 = ops[pos + 2]
            let r = ops[pos + 3]

            let p1v = if getParameter1Mode op then p1 else ops[p1]
            let p2v = if getParameter2Mode op then p2 else ops[p2]

            ops[r] <- p1v * p2v
            pos + 4, io
        | 3 -> // input
            let r = ops[pos + 1]

            match io with
            | x :: xs, outputs ->
                ops[r] <- x
                pos + 2, (xs, outputs)
            | _ -> failwith "No inputs available"
        | 4 -> // output
            let p1 = ops[pos + 1]
            let p1v = if getParameter1Mode op then p1 else ops[p1]

            let inputs, outputs = io

            pos + 2, (inputs, p1v :: outputs)
        | 5 -> // jump-if-true
            let p1 = ops[pos + 1]
            let p2 = ops[pos + 2]

            let p1v = if getParameter1Mode op then p1 else ops[p1]
            let p2v = if getParameter2Mode op then p2 else ops[p2]

            if p1v <> 0 then p2v, io else pos + 3, io
        | 6 -> // jump-if-false
            let p1 = ops[pos + 1]
            let p2 = ops[pos + 2]

            let p1v = if getParameter1Mode op then p1 else ops[p1]
            let p2v = if getParameter2Mode op then p2 else ops[p2]

            if p1v = 0 then p2v, io else pos + 3, io
        | 7 -> // less than
            let p1 = ops[pos + 1]
            let p2 = ops[pos + 2]
            let r = ops[pos + 3]

            let p1v = if getParameter1Mode op then p1 else ops[p1]
            let p2v = if getParameter2Mode op then p2 else ops[p2]

            ops[r] <- if p1v < p2v then 1 else 0
            pos + 4, io
        | 8 -> // equal
            let p1 = ops[pos + 1]
            let p2 = ops[pos + 2]
            let r = ops[pos + 3]

            let p1v = if getParameter1Mode op then p1 else ops[p1]
            let p2v = if getParameter2Mode op then p2 else ops[p2]

            ops[r] <- if p1v = p2v then 1 else 0
            pos + 4, io
        | 99 -> -1, io
        | x -> failwithf "Unexpected op code: %i" x

    let runProgram program inputs =
        let program = program |> Array.copy

        let step = stepProgram program
        let mutable state = 0, (inputs, [])

        while fst state >= 0 do
            state <- state |> step

        state |> snd |> snd

    let run (input: byte array) output =
        let ops = parseInts input

        let result = runProgram ops [ 1 ]

        result |> List.head |> string |> output 1

        let result = runProgram ops [ 5 ]

        result |> List.head |> string |> output 2
