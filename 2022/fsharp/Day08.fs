namespace AdventOfCode.FSharp.Y2022

// Day 8
module Day08 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =
        let lines = input |> splitLine 
        let a = lines |> Array.map (
            fun s -> s.ToCharArray() |> Array.map (fun x -> (int x) - (int '0'))) |> array2D

        let mx = Array2D.length1 a
        let my = Array2D.length2 a

        let mutable fff = 0L       
        let ss x y v =
            let mutable f = false
            f <- false
            let mutable fl = 0

            for i in (y - 1) .. (-1) .. 0 do
                if (not f) && a[x, i] < v then
                    printfn "fl %i" a[x,i]
                    fl <- fl + 1

                if (not f) && a[x,i] >= v then
                    fl <- fl + 1
                    f <- true

            f <- false
            let mutable fr = 0

            for i in (y + 1) .. (my - 1) do
                if (not f) && a[x, i] < v then
                    printfn "fr %i" a[x,i]
                    fr <- fr + 1

                if (not f) && a[x, i] >= v then
                    f <- true
                    fr <- fr + 1

            f <- false
            let mutable fb = 0

            for i in (x + 1) .. (mx - 1) do
                if (not f) && a[i,y] < v then
                    printfn "fb %i" a[i, y]
                    fb <- fb + 1

                if (not f) && a[i, y] >= v then
                    fb <- fb + 1
                    f <- true
            f <- false

            let mutable ft = 0

            for i in (x - 1) .. (-1) .. 0 do
                if (not f) && a[i, y] < v then
                    printfn "ft %i" a[i, y]
                    ft <- ft + 1

                if (not f) && a[i, y] >= v then
                    ft <- ft + 1
                    f <- true

            let score = (int64 ft) * (int64 fb) * (int64 fr) * (int64 fl)
            printfn "score %i" score
            if score > fff then 
                fff <- score
            score
        

        let q = a |> Array2D.mapi ss

        printfn "%A" q

        fff |> string |> output 1
