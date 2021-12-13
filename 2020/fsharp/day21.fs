namespace AdventOfCode.FSharp.Y2020

// Allergen Assessment
module Day21 =
    open AdventOfCode.FSharp.Util
    open System
    open System.Collections.Generic
    
    type Deck = (Set<string> * Set<string>) list
    
    let possible (d : Deck) =
        seq {
            for (i, a) in d do
                for  ii in i do
                    for aa in a do
                        yield (ii, aa)  
        } |> Seq.distinct        

    let rec solve (d : Deck) (f : Map<string, string>) : Map<string, string> option =
        let check (i, a) =
            if Map.containsKey a f then            
                None
            else
                let nf = Map.add a i f
                let nd = 
                    d 
                    |> List.map (fun (oi, oa) -> 
                        (Set.remove i oi), (if Set.contains i oi then Set.remove a oa else oa))
                    |> List.filter (fun (oi, oa) -> oa.Count <> 0)
                let bad =
                    nd 
                    |> List.filter (fun (oi, oa) -> oi.Count = 0)
                if bad.Length > 0 then
                    None
                elif nd.Length = 0 then
                    Some nf
                else
                    solve nd nf

        d 
        |> possible
        |> Seq.tryPick check

    let parse (input : string) =
        input 
        |> splitLine
        |> Array.map (fun line ->
            let p = line.IndexOf "("
            let ingreds = line.Substring(0, p - 1).Split(' ') |> Set.ofArray
            let allergs = 
                line.Substring(p + 9).Trim(')').Split(',') 
                |> Array.map (fun s -> s.Trim()) 
                |> Set.ofArray
            (ingreds, allergs))
        |> Array.toList
    let solve2a (a : string) (d : Deck) =
        d
        |> Seq.filter (fun (_, sa) -> sa |> Set.contains a )
        |> Seq.map fst
        |> Set.intersectMany

    let solve2 (d : Deck) =
        let alla = 
            d
            |> Seq.collect (fun (_, a) -> a)
            |> Seq.fold (fun a i -> Set.add i a) Set.empty
        
        alla 
        |> Seq.map (fun a -> (a, solve2a a d))
 
        
    let part1 (input : string) =
        let d = parse input

        let alla = 
            d
            |> Seq.collect (fun (_, a) -> a)
            |> Seq.fold (fun a i -> Set.add i a) Set.empty
        printfn "%d = %A" alla.Count alla

        // let res = solve d Map.empty

        let alli = 
            d
            |> Seq.collect (fun (i, _) -> i)
            |> Seq.fold (fun a i -> Set.add i a) Set.empty

        let possi = solve2 d

        let noall = Set.difference alli (possi |> Seq.map snd |> Set.unionMany)

        // printfn "possi = %A" possi
        // printfn "noall = %A" noall

        d 
        |> Seq.map (fun (i, _) -> (Set.intersect noall i).Count)
        |> Seq.sum
        |> string

    let part2 (input : string) =
        let d = parse input

        let alla = 
            d
            |> Seq.collect (fun (_, a) -> a)
            |> Seq.fold (fun a i -> Set.add i a) Set.empty
        // printfn "%d = %A" alla.Count alla

        // let res = solve d Map.empty

        let alli = 
            d
            |> Seq.collect (fun (i, _) -> i)
            |> Seq.fold (fun a i -> Set.add i a) Set.empty

        let possi = solve2 d |> Seq.toList
        let noall = Set.difference alli (possi |> Seq.map snd |> Set.unionMany)


        let rec solve3 (p : (string * Set<string>) list) res : Map<string,string> option =
            match p with
            | [] -> Some res
            | (a, sa)::xs -> 
                sa |> 
                    Seq.tryPick (fun s -> 
                        let nxs = xs |> List.map (fun (b, bs) -> (b, Set.remove s bs))
                        if (nxs |> List.tryFind (fun (_, bs) -> bs.Count = 0)).IsSome then
                            None
                        else
                            solve3 nxs (Map.add a s res))
        
        let fres = (solve3 possi Map.empty).Value

        let danger = fres |> Map.toSeq |> Seq.sortBy fst |> Seq.map snd
        let dangerlist = System.String.Join (',', danger)
        
        dangerlist

