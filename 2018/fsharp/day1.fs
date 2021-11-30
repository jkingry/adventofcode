namespace AdventOfCode.FSharp.Y2018

open System

module Day1 =

    let parse input =
        input
        |> Seq.map Int32.Parse
        |> Seq.toArray

    let part1 (input : string seq) =        
        parse input |> Seq.sum

    let part2 (input : string seq)  = 
        let x = parse input
        
        let mutable dupe : int option = None
        let mutable found = Set.empty
        let mutable sum = 0
        while dupe.IsNone do
            for y in x do
                sum <- sum + y
                if Set.contains sum found then
                    dupe <- Some sum
                found <- Set.add sum found
        dupe.Value


    let part3 (input : string seq)  = 
        let x = parse input

        let rec f i sum found =
            let ni = i % x.Length
            let nsum = x[ni] + sum
            if Set.contains nsum found then
                nsum
            else            
                f (ni + 1) nsum (Set.add nsum found)

        f 0 0 Set.empty
