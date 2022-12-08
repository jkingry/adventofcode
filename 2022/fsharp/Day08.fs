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

        let mutable maxScore = 0

        let countUntil v l =
            let mutable r = 0
            let mutable stop = false
            for e in l do
                if not stop then
                    r <- r + 1
                    if e >= v then stop <- true
            r        

        let traverse x y v =
            let top = a[0..(x-1), y] 
            let bot = a[(x+1)..(mx-1), y] 
            let rgt = a[x, 0..(y-1)]      
            let lft = a[x, (y+1)..(my-1)] 

            if top |> Array.forall (fun vv -> vv < v) 
                || bot |> Array.forall (fun vv -> vv < v) 
                || rgt |> Array.forall (fun vv -> vv < v) 
                || lft |> Array.forall (fun vv -> vv < v) then
                visible <- visible + 1

            let tops = top |> Array.rev |> countUntil v
            let bots = bot |> countUntil v
            let rgts = rgt |> Array.rev |> countUntil v            
            let lfts = lft |> countUntil v            
            
            let score = tops * bots * rgts * lfts
            if score > maxScore then maxScore <- score

        a |> Array2D.iteri traverse

        visible |> string |> output 1
        maxScore |> string |> output 2
