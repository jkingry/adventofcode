namespace AdventOfCode.FSharp.Y2021

// Day 3: Binary Diagnostic
module Day03 =
    open AdventOfCode.FSharp.Util

    let fromBinary (s : string) = int ("0b" + s)

    let part1 (input : string) =        
        let mapIncr (i : int) (m : Map<int, int>) = m |> Map.change i (fun x -> Some (1 + Option.defaultValue 0 x))
        
        let countOnes (s : string) (counts : Map<int, int>) =
            s |> Seq.indexed |> Seq.fold (fun a (i, c) -> if c = '1' then (mapIncr i a) else a) counts

        let cache = input |> splitLine

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
        let digits = (Array.head cache).Length
        let mask = (1 <<< digits) - 1
        let epsilon = (~~~gamma) &&& mask

        epsilon * gamma |> string

    let part2 (input : string) =        
        let filterReport (dir: bool) (input : string list)  (p : int) = 
            let m = input |> List.groupBy (fun s -> s[p]) |> Map.ofList
            if (m['0'].Length > m['1'].Length) = dir then m['0'] else m['1']

        let foldReport (input : string list) (dir : bool) =
            let n = (List.head input).Length
            [0..n-1] |> Seq.scan (filterReport dir) input |> Seq.find (fun l -> l.Length = 1)
            |> List.head |> fromBinary     

        let cache = input |> splitLine |> List.ofArray
        let oxygenGenerator = foldReport cache true
        let co2Scrubber  = foldReport cache false
        oxygenGenerator * co2Scrubber |> string