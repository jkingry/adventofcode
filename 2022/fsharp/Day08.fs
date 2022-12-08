namespace AdventOfCode.FSharp.Y2022

// Day 8: Treetop Tree House
module Day08 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =
        let lines = input |> splitLine 
        let a = lines |> Array.map (
            fun s -> s.ToCharArray() |> Array.map (fun x -> (byte x) - (byte '0'))) |> array2D

        let mx = Array2D.length1 a
        let my = Array2D.length2 a

        let mutable visible = 0    

        let traverse x y v =
            if a[0 .. (x - 1), y] |> Array.forall (fun xx -> xx < v) 
                || a[(x + 1) .. (mx - 1), y] |> Array.forall (fun xx -> xx < v) 
                || a[x, 0 .. (y - 1)] |> Array.forall (fun xx -> xx < v) 
                || a[x, (y + 1) .. (my - 1)] |> Array.forall (fun xx -> xx < v) then
                visible <- visible + 1

        a |> Array2D.iteri traverse

        let countUntil v l =
            let mutable r = 0
            let mutable stop = false
            for e in l do
                if not stop then
                    r <- r + 1
                    if e >= v then stop <- true
            r

        let mutable maxScore = 0

        let traverse x y v =
            let top = a[0..(x-1), y] |> Array.rev |> countUntil v
            let bot = a[(x+1)..(mx-1), y] |> countUntil v
            let rgt = a[x, 0..(y-1)] |> Array.rev |> countUntil v            
            let lft = a[x, (y+1)..(my-1)] |> countUntil v            
            
            let score = top * bot * rgt * lft
            if score > maxScore then maxScore <- score

        a |> Array2D.iteri traverse

        visible |> string |> output 1
        maxScore |> string |> output 2
