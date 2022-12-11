namespace AdventOfCode.FSharp.Y2022

// Day 9: Rope Bridge
module Day09 =
    open Checked
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =

        let createRope len = (Array.zeroCreate len), (Array.zeroCreate len)

        let follow (rx: int[], ry: int[]) (dir: char, amt: int) =
            let ropeLength = Array.length rx

            seq {
                for _ in 1..amt do                
                    match dir with 
                    | 'U' -> ry[0] <- ry[0] + 1
                    | 'D' -> ry[0] <- ry[0] - 1
                    | 'L' -> rx[0] <- rx[0] - 1
                    | 'R' -> rx[0] <- rx[0] + 1
                    | c -> failwithf "Unexpected: %c" c

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

        let instructions = 
            input
            |> text
            |> splitLine
            |> Array.map (fun s -> 
                let p = s.Split(' ')
                p[0][0], (int p[1]))

        let trackMotionCount ropeLength = 
            let rope = createRope ropeLength
            let v = new System.Collections.Generic.HashSet<int*int>([(0,0)]);
            let o = instructions
                    |> Seq.map (follow rope)
                    |> Seq.concat
            for p in o do v.Add(p) |> ignore
            v.Count
            
            // // Sad functional process that is ~3.2x slower:
            // instructions
            //     |> Seq.map (follow rope)
            //     |> Seq.concat
            //     |> Set.ofSeq
            //     |> Set.add (0,0) // initial position
            //     |> Set.count
        
        trackMotionCount 2 |> string |> output 1 
        trackMotionCount 10 |> string |> output 2 
