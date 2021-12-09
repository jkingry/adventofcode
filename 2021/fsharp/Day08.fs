namespace AdventOfCode.FSharp.Y2021

// Day 8
module Day08 =    
    open AdventOfCode.FSharp.Util

    let (&&&) (a : Set<'a>) (b : Set<'a>) =
        Set.intersect a b

    let parse (input : string) =
        input
        |> splitLine
        |> Array.map (fun line -> 
            let parts = line |> split "|" |> Array.map splitSpace
            parts[0], parts[1])   

    let part1 (text : string) =
        let uniqueNumbers = set [ 2; 3; 4; 7 ]

        parse text
        |> Array.map (fun (_,e) ->
            e |> Array.filter (fun s -> uniqueNumbers |> Set.contains s.Length))
        |> Array.map Array.length
        |> Array.sum

    let solve (x : string seq, o : string seq) =
        let l = seq { for s in x -> s.Length, set s } |> Map.ofSeq

        let mutable n = ""

        for s in o |> Seq.map set do
            let c = 
                match s.Count, (s &&& l[4]).Count, (s &&& l[2]).Count with
                | 2,_,_ -> "1"
                | 3,_,_ -> "7"
                | 4,_,_ -> "4"
                | 7,_,_ -> "8"
                | 5,2,_ -> "2"
                | 5,3,1 -> "5"
                | 5,3,2 -> "3"
                | 6,3,1 -> "6"
                | 6,4,2 -> "9"
                | 6,3,2 -> "0"
                | _ -> failwith "Invalid"
            n <- n + c
        int n 

    let part2 (text : string) =
        parse text
        |> Array.map solve
        |> Array.sum
