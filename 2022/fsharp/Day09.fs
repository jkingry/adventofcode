namespace AdventOfCode.FSharp.Y2022

// Day 9: Rope Bridge
module Day09 =
    open Checked
    open AdventOfCode.FSharp.Util

    let follow (rx: int[], ry: int[]) (dir: byte, amt: int) =
        let ropeLength = Array.length rx

        seq {
            for _ in 1..amt do                
                match dir with 
                | 85uy -> ry[0] <- ry[0] + 1
                | 68uy -> ry[0] <- ry[0] - 1
                | 76uy -> rx[0] <- rx[0] - 1
                | 82uy -> rx[0] <- rx[0] + 1
                | c -> failwithf "Unexpected: %c" (char c)

                let mutable moved = false
                for i in 1..(ropeLength - 1) do                    
                    let dx = rx[i-1] - rx[i]
                    let dy = ry[i-1] - ry[i]

                    moved <- true
                    if dx = 0 && abs(dy) = 2 then
                        ry[i] <- ry[i] + sign(dy)
                    elif dy = 0 && abs(dx) = 2 then
                        rx[i] <- rx[i] + sign(dx)
                    elif (abs(dx) + abs(dy)) > 2 then
                        rx[i] <- rx[i] + sign(dx)
                        ry[i] <- ry[i] + sign(dy)
                    else moved <- false

                if moved then yield (Array.last rx),(Array.last ry)  
        }

    let createRope len = (Array.zeroCreate len), (Array.zeroCreate len)

    let trackMotionCount ropeLength instructions = 
        let rope = createRope ropeLength
    
        instructions
            |> Seq.map (follow rope)
            |> Seq.concat
            |> Set.ofSeq
            |> Set.add (0,0) // initial position
            |> Set.count
    
    let trackMotionCountHashSet ropeLength instructions = 
        let rope = createRope ropeLength
        let v = new System.Collections.Generic.HashSet<int*int>([(0,0)]);
        let o = instructions
                |> Seq.map (follow rope)
                |> Seq.concat
        for p in o do v.Add(p) |> ignore
        v.Count    

    let parseInstructions input =
        input
        |> text
        |> splitLine
        |> Array.map (fun s -> 
            let p = s.Split(' ')
            (byte (p[0][0])), (int p[1]))    

    let parseInstructionsFast (input: byte array) =
        let newline = byte '\n'
        seq {
            let mutable i = 0
            while i < (input.Length - 1) do
                let (ni, amt) = parseIntToDelim input (i + 2) newline
                yield (input[i], amt)
                i <- ni
        }

    let runFast (input: byte array) (output: int -> string -> unit) =
        let instructions = input |> parseInstructionsFast

        instructions |> trackMotionCountHashSet 2 |> string |> output 1 
        instructions |> trackMotionCountHashSet 10 |> string |> output 2 

    let run (input: byte array) (output: int -> string -> unit) =
        let instructions = input |> parseInstructions

        instructions |> trackMotionCount 2 |> string |> output 1
        instructions |> trackMotionCount 10 |> string |> output 2         
