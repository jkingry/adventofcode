namespace AdventOfCode.FSharp.Y2022

// Day 9
module Day09 =
    open Checked
    open AdventOfCode.FSharp.Util

    let run (input: string) (output: int -> string -> unit) =
        let mutable v = Set.empty
        
        let mutable hx = 0 
        let mutable hy = 0 
        let tx = Array.zeroCreate 9 
        let ty = Array.zeroCreate 9

        v <- v |> Set.add (0, 0)
        let r = 
            input 
            |> splitLine 
            |> Seq.iter (fun s -> 
                let p = s.Split(' ')
                let d = p[0][0]
                let a = int p[1]
                printfn "%s" s
                for aa in 1..a do 

                    match d with 
                    | 'U' -> 
                        hy <- hy + 1
                    | 'D' ->
                        hy <- hy - 1
                    | 'L' -> 
                        hx <- hx - 1
                    | 'R' -> 
                        hx <- hx + 1

                    let mutable px = hx
                    let mutable py = hy 
                    for i in 0..8 do
                        
                        let dx = px - tx[i]
                        let dy = py - ty[i]

                        if dx = 0 && abs(dy) = 2 then
                            ty[i] <- ty[i] + sign(dy)
                        elif dy = 0 && abs(dx) = 2 then
                            tx[i] <- tx[ i ] + sign(dx)
                        elif (abs(dx) + abs(dy)) > 2 then
                            tx[ i ] <- tx[ i ] + sign(dx)
                            ty[i] <- ty[ i ] + sign(dy)
                        else 
                            ()
                        px <- tx[i]
                        py <- ty[i]
                    v <- v |> Set.add (tx[8], ty[8])  
            )

        v |> Set.count |> string |> output 1
        1 |> string |> output 2
