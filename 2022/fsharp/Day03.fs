namespace AdventOfCode.FSharp.Y2022

// Day 3
module Day03 =
    open AdventOfCode.FSharp.Util

    let run (input: string) (output: int -> string -> unit) =    
        
        let findCommonItem item =
                item |> Seq.map Set.ofSeq |> Seq.reduce Set.intersect |> Seq.head

        let charScore c =
            (int c) -
                if System.Char.IsLower(c) then (int 'a') - 1 else (int 'A') - 27

        let lines = input |> splitLine

        // split each line in half
        let part1 = lines |> Seq.map(fun s ->
            let mid = s.Length / 2 
            [s[..mid - 1]; s[mid..]])

        part1 |> Seq.map (findCommonItem >> charScore) |> Seq.sum |> string |> output 1


        // group lines into sets of three
        let part2 = lines |> Seq.chunkBySize 3

        part2 |> Seq.map (findCommonItem >> charScore) |> Seq.sum |> string |> output 2
        