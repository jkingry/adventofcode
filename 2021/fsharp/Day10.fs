namespace AdventOfCode.FSharp.Y2021


// Day 10
module Day10 =    
    open AdventOfCode.FSharp.Util
    open Checked

    let part1 (text : string) =   
        let points = 
            [
                '(', 1L
                '[', 2L
                '{',3L
                '<',4L
            ] |> Map.ofList
        let mutable bad = 0
        let mutable totals : int64 list = []

        for line in text |> splitLine do
            let mutable s = []
            let mutable foundbad = false
            for c in line do                
                if not foundbad then
                    match c with
                    | '[' -> s <- c::s
                    | '<' -> s <- c::s
                    | '{' -> s <- c::s
                    | '(' -> s <- c::s

                    | ')' -> let h::xs = s in if h <> '(' then foundbad <- true else s <- xs
                    | '>' -> let h::xs = s in if h <> '<' then foundbad <- true else s <- xs 
                    | '}' -> let h::xs = s in if h <> '{' then foundbad <- true  else s <- xs
                    | ']' -> let h::xs = s in if h <> '[' then foundbad <- true  else s <- xs
                    | _ -> ()

            if not foundbad then
                let t : int64 = s |> List.map (fun cc -> points |> Map.find cc)  |> List.fold (fun a rr -> (a * 5L) + rr) 0
                printfn "%A %d" (s) t                
                totals <- t::totals
        totals <- totals |> List.sort
        totals[totals.Length / 2]

    let part2 (text : string) =
        -1