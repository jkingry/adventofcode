namespace AdventOfCode.FSharp.Y2020


// Day 24: Lobby Layout
module Day24 = 
    open AdventOfCode.FSharp.Util
    open System
    open System.Collections.Generic

    let swapTile k m =
        if Set.contains k m then
            Set.remove k m
        else
            Set.add k m

    let tupAdd (a,b) (c,d) = (a + c, b + d)


    let part1 (input : string) =    
        let mutable y = 0

        let delta (s : string) : (int * int)=
            seq {
                let mutable y = 0
                for c in s do
                    match c with
                    | 's' -> y <- -1
                    | 'n' -> y <- 1
                    | 'e' -> yield (2 - abs(y), y); y <- 0
                    | 'w' -> yield (-(2 - abs(y)), y); y <- 0
                    | _ -> failwith "Invalid character"
            } |> Seq.fold tupAdd (0,0)

        let m = 
            input 
            |> splitLine 
            |> Seq.map delta 
            |> Seq.fold(fun m c -> m |> swapTile c) Set.empty

        m.Count |> string

    let part2 (input : string) =
        let mutable y = 0

        let delta (s : string) : (int * int)=
            seq {
                let mutable y = 0
                for c in s do
                    match c with
                    | 's' -> y <- -1
                    | 'n' -> y <- 1
                    | 'e' -> yield (2 - abs(y), y); y <- 0
                    | 'w' -> yield (-(2 - abs(y)), y); y <- 0
                    | _ -> failwith "Invalid character"
            } |> Seq.fold tupAdd (0,0)

        let mutable m = 
            input 
            |> splitLine 
            |> Seq.map delta 
            |> Seq.fold(fun m c -> m |> swapTile c) Set.empty
        

        let n k =
            [
                (1,1)
                (2,0)
                (1,-1)
                (-1,-1)
                (-2,0)
                (-1,1)
            ] 
            |> Seq.map (fun d -> tupAdd k d)
        

        let nflip k m =
            n k |> Seq.filter (fun nn -> Seq.contains nn m ) |> Seq.length

        let doFlips m =        
            let flips = 
                m 
                |> Set.fold(fun u k -> u |> Set.union (Set.ofSeq (n k))) m 
                |> Seq.filter (fun k -> 
                    let black = nflip k m
                    if (Set.contains k m) then black = 0 || black > 2
                    else black = 2) 
            flips |> Seq.fold (fun m' k -> m' |> swapTile k) m

        for i = 1 to 100 do
            printfn "%d" i
            m <- doFlips m

        m.Count |> string

        


