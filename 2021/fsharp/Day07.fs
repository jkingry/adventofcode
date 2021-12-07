namespace AdventOfCode.FSharp.Y2021

open AdventOfCode.FSharp.Util
open System
// Day 7
module Day07 =    

    let part1 (text : string) =
        let input = 
            text |> ints |> Array.groupBy id 
            |> Array.map (fun (k,v) -> (k, Array.length v))
            |> Map.ofArray
        
        let total = input |> Map.fold (fun a k v -> a + (k * v) ) 0
        let avg = total / (input.Values |> Seq.sum)

        let imin = input.Keys |> Seq.min
        let imax = input.Keys |> Seq.max

        printfn "min %d" imin
        printfn "max %d" imax

        let sint n = n * (n+1) / 2
        let cost x q =
            q |> Map.fold (fun a k v -> a + sint(abs(k - x)) * v) 0

        let (sol, solcost) = 
            [imin..imax] 
            |> List.fold (
                fun (a,c) i ->
                    let cc = cost i input
                    if cc < c then 
                        printfn "%d %d" i cc
                        (i,cc) 
                    else 
                        (a,c)) 
                (-1, Int32.MaxValue)

        solcost

    let part2 (input : string) =
        -1