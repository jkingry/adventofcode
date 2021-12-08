namespace AdventOfCode.FSharp.Y2021

// Day 6: Lanternfish
module Day06 =    
    open AdventOfCode.FSharp.Util
    
    let simulate days gestationTime ages =
        let maxAge = Array.length ages
        for day = 0 to days - 1 do
            let birthDay = day % maxAge
            let nextBirthDay = (day + gestationTime) % maxAge

            ages[nextBirthDay] <- ages[nextBirthDay] + ages[birthDay]

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
