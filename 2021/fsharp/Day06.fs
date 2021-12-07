namespace AdventOfCode.FSharp.Y2021

open AdventOfCode.FSharp.Util

// Day 6: Lanternfish
module Day06 =    
    let simulate days gestationTime (ages : int64 array) =
        for day = 0 to days - 1 do
            let offspringDay = day % ages.Length 
            let nextOffspringDay = (day + gestationTime) % ages.Length

            ages[nextOffspringDay] <- ages[nextOffspringDay] + ages[offspringDay]

    let part1 (input : string) =
        let ages : int64[] = Array.zeroCreate 9

        ints input 
        |> Array.groupBy id 
        |> Array.iter (fun (k,v) -> ages[k] <- v.LongLength) 

        simulate 80 7 ages
        
        Array.sum ages 

    let part2 (input : string) =
        let ages : int64[] = Array.zeroCreate 9

        ints input 
        |> Array.groupBy id 
        |> Array.iter (fun (k,v) -> ages[k] <- v.LongLength) 

        simulate 256 7 ages
        
        Array.sum ages
