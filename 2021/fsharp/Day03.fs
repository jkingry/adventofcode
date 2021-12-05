namespace AdventOfCode.FSharp.Y2021

open System
open System.Text
open System.Text.RegularExpressions
open AdventOfCode.FSharp.Util

module Day03 =
    let fromBinary (s : string) = 
        let mutable n = 0
        for i = 0 to s.Length - 1 do
            if s[s.Length - i - 1] = '1' then n <- n + (1 <<< i)
        n

    let part1 (input : string seq) =        
        let mapIncr (i : int) (m : Map<int, int>) = m |> Map.change i (fun x -> Some (1 + Option.defaultValue 0 x))
        
        let countOnes (s : string) (counts : Map<int, int>) =
            s |> Seq.indexed |> Seq.fold (fun a (i, c) -> if c = '1' then (mapIncr i a) else a) counts

        let cache = input |> Seq.toList

        let counts = 
            cache
            |> Seq.fold (fun m s -> m |> countOnes s) Map.empty

        let n = cache.Length
        let gammaChars =
            counts 
            |> Map.map (fun _ v -> if v > (n/2) then '1' else '0') 
            |> Map.toSeq |> Seq.sortBy fst |> Seq.map snd |> Seq.toArray

        let gamma = new string(gammaChars) |> fromBinary 

        printfn "%d" gamma
        let digits = (List.head cache).Length
        let mask = (1 <<< digits) - 1
        let epsilon = (~~~gamma) &&& mask

        epsilon * gamma

    let part2 (input : string seq) : int =        
        let filterReport (dir: bool) (input : string list)  (p : int) = 
            let m = input |> List.groupBy (fun s -> s[p]) |> Map.ofList
            if (m['0'].Length > m['1'].Length) = dir then m['0'] else m['1']

        let foldReport (input : string list) (dir : bool) =
            let n = (List.head input).Length
            [0..n-1] |> Seq.scan (filterReport dir) input |> Seq.find (fun l -> l.Length = 1)
            |> List.head |> fromBinary     

        let cache = input |> List.ofSeq
        let oxygenGenerator = foldReport cache true
        let co2Scrubber  = foldReport cache false
        oxygenGenerator * co2Scrubber