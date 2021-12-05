namespace AdventOfCode.FSharp.Y2021

open System

// Day 5: 
module Day05 =    
    let ints (s : string) =
        s.Split(' ',',') 
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map (fun s -> s.Trim() |> Int32.Parse) 
        

    let part1 (input : string seq) =
        let n = 
            input
            |> Seq.map (fun s -> 
                let p = s.Split(' ')
                (ints(p[0]) , ints(p[p.Length - 1])))
            |> Seq.toList

        printfn "%A" n
        let mutable m = Map.empty
        for p in n do
            let (x1,x2) = p
            let p1 = x1
            let p2 = x2
            let d = [ 
                sign (p2[0] - p1[0])
                sign (p2[1] - p1[1])
            ]
            let mutable x = p1[0]
            let mutable y = p1[1]
            let mutable going = true
            while going do
                m <- m |> Map.change (x,y) (function | None -> Some 1 | Some x -> Some (x + 1))
                if x = p2[0] && y = p2[1] then going <- false
                x <- x + d[0]
                y <- y + d[1]
    
        m.Values |> Seq.filter (fun v -> v > 1) |> Seq.length        

    let part2 (input : string) =
        -1    