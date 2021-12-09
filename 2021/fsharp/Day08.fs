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

    let charArrayToString (a : char[]) = new System.String(a)

    let fastPermute (e : string seq) =
        let one = e |> Seq.find (fun s -> s |> String.length = 2) |> List.ofSeq
        let seven = e |> Seq.find (fun s -> s |> String.length = 3) |> List.ofSeq
        let four = e |> Seq.find (fun s -> s |> String.length = 4) |> List.ofSeq
        let all = "abcdefg" |> List.ofSeq
        let input : char list list =
            [
                seven|> List.except one  // a
                four |> List.except one  // b
                one                      // c
                four |> List.except one  // d
                all  |> List.except four // e
                one                      // f
                all  |> List.except four // g 
            ]

        input
        |> fromChoices
        |> List.map (fun l -> l |> List.toArray |> charArrayToString)

    let parse (input : string) =
        input
        |> splitLine
        |> Array.map (fun line -> 
            let parts = line |> split "|" |> Array.map splitSpace
            parts[0], parts[1])
    
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

        let mutable p = 0
        let res = 
            parse text
            |> Array.map (fun (e,o) ->
                let cmap : Map<char,char> = 
                    fastPermute e
                    |> List.map (fun s -> p <- p + 1; s)                
                    |> List.map stringToMap 
                    |> Seq.find (success e)

                o |> Seq.map (fun oo -> 
                    (oo |> translate cmap).Value) 
                    |> Seq.toArray 
                    |> charArrayToString 
                    |> int)
            |> Array.sum
        
        printfn "%i" p
        res
