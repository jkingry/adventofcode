namespace AdventOfCode.FSharp.Y2022

// Day 10
module Day10 =
    open Checked
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let lines =
            input
            |> text
            |> splitLine

        let mutable cc = 0
        let mutable x = 1
        let mutable t = 0
        let g = [20;60;100;140;180;220]
        let mutable cr = -1
        let mutable tt = "\n"
        for line in lines do
            let p = line.Split(' ')
            let op = p[0]

            let amt = if p.Length > 1 then int p[1] else 0
            let c = if op = "addx" then 2 else 1

            for _ = 1 to c do
                cc <- cc + 1
                cr <- cr + 1
                if cr % 40 = 0 then
                    cr <- 0
                    tt <- tt + "\n"
                if List.contains cc g then
                    printfn "%i x %i = %i" cc x (cc * x)
                    t <- t + (cc* x)
                if (x - 1) <= cr && cr <= (x + 1) then
                    tt <- tt + "#"
                else
                    tt <- tt + "."

            printfn "c = %i, x = %i" cr x
            x <- x + amt
        
        t |> string |> output 1 
        tt |> string |> output 2 
