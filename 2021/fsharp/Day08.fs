namespace AdventOfCode.FSharp.Y2021

// Day 8
module Day08 =    
    open AdventOfCode.FSharp.Util
    open Checked

    let charMap = 
        [
            "abcefg"
            "cf"
            "acdeg"
            "acdfg"
            "bcdf"
            "abdfg"
            "abdefg"
            "acf"
            "abcdefg"
            "abcdfg"
        ] |> List.indexed |> List.map (fun (a,b) -> (b,a.ToString()[0])) |> Map.ofList 

    let parse (input : string) =
        input
        |> splitLine
        |> Array.map (fun line -> 
            let parts = line |> split "|" |> Array.map splitSpace
            parts[0], parts[1])
    let charArrayToString (a : char[]) = new System.String(a)
    
    let translate cmap input =
        let segments : string =
                input
                |> Seq.map (fun c -> Map.find c cmap)
                |> Seq.sort
                |> Seq.toArray
                |> charArrayToString
            
        Map.tryFind segments charMap      

    let part1 (text : string) =
        let uniqueNumbers = set [ 2; 3; 4; 7 ]

        parse text
        |> Array.map (fun (_,e) ->
            e |> Array.filter (fun s -> uniqueNumbers |> Set.contains s.Length))
        |> Array.map Array.length
        |> Array.sum

    let part2 (text : string) =
        let stringToMap s =
            Seq.zip s "abcdefg" |> Map.ofSeq
        let success e cmap =
            e |> Seq.forall ((translate cmap) >> Option.isSome)

        parse text
        |> Array.map (fun (e,o) ->
            let cmap : Map<char,char> = 
                "abcdefg" 
                |> Seq.toList 
                |> permute
                |> List.map stringToMap 
                |> Seq.find (success e)
            o |> Seq.map (fun oo -> 
                (oo |> translate cmap).Value) 
                |> Seq.toArray 
                |> charArrayToString 
                |> int)
        |> Array.sum
