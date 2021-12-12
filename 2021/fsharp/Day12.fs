namespace AdventOfCode.FSharp.Y2021

// Day 12
module Day12 =    
    open AdventOfCode.FSharp.Util
    open Checked

    let part1 (text : string) =   
        let caves : Map<string, Set<string>> = 
            text 
            |> splitLine 
            |> Array.fold (fun c line ->
                let p = line.Split('-')
                let a = p[0]
                let b = p[1]
                let ca = 
                    if a <> "end" && b <> "start"then
                        c |> Map.change a (fun o -> (defaultArg o Set.empty) |> Set.add b |> Some) 
                    else 
                        c
                if b <> "end" && a <> "start" then
                    ca |> Map.change b (fun o -> (defaultArg o Set.empty) |> Set.add a |> Some)
                else
                    ca) Map.empty 
                
        printfn "%A" caves

        let small (k : string) = 
            k.ToLowerInvariant() = k

        let mutable q = [(["start"], Set.empty, None)]

        let mutable ends = []

        while q.Length > 0 do    
            let (path, visited, dcave)::xs = q
            
            q <- xs
            
            let pathNode = List.head path

            if pathNode = "end" then
                ends <- path::ends
            else
                for nn in caves[pathNode] do
                    if (small nn) then
                        if visited |> Set.contains nn then 
                            match dcave with
                            | Some _ -> ()
                            | None -> q <- (nn::path, visited, Some nn)::q
                        else
                            q <- (nn::path, visited |> Set.add nn, dcave)::q
                    else
                        q <- (nn::path, visited, dcave)::q 

            //printfn "%A" (q.Length, ends.Length)
        printfn "%A" (ends |> (List.map List.rev) |> List.sort)

        ends.Length

    let part2 (text : string) =
        -1
