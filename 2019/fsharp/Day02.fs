namespace AdventOfCode.FSharp.Y2019

// Day 2: 1202 Program Alarm
module Day02 =
    open AdventOfCode.FSharp.Util

    let stepProgram (pos : int) (ops: int[]) = 
        let p1 = ops[pos + 1]
        let p2 = ops[pos + 2]
        let r = ops[pos + 3]

        match ops[pos] with 
        | 1 ->
            ops[r] <- ops[p1] + ops[p2]
            pos + 4
        | 2 ->
            ops[r] <- ops[p1] * ops[p2]
            pos + 4
        | 99 -> -1
        | x -> failwithf "Unexpected op code: %i" x

    let runProgram noun verb program =            
        let program = program |> Array.copy

        program[1] <- noun
        program[2] <- verb

        let mutable pos = 0

        while pos >= 0 do
            pos <- program |> stepProgram pos
        program[0]

    let run (input: byte array) output =
        let ops = parseInts input

        ops |> runProgram 12 2 |> string |> output 1

        let target = 19690720

        let mutable noun = 1
        let mutable verb = 1
        
        while (ops |> runProgram noun verb) <> target do
            if noun < 100 then
                noun <- noun + 1
            else
                noun <- 1
                verb <- verb + 1                

        (100 * noun + verb) |> string |> output 2
