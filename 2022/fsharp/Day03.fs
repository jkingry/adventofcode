namespace AdventOfCode.FSharp.Y2022

// Day 3: Rucksack Reorganization
module Day03 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =    
        let toMask (s: char[]) =
            s |> Array.fold (fun a c -> a ||| (1UL <<< ((int c) - (int 'A')))) 0UL

        let findCommonItem item =
            let bit =
                 item 
                |> Array.map toMask 
                |> Array.reduce (fun a b -> a &&& b) 
                |> System.Numerics.BitOperations.TrailingZeroCount
            (int 'A') + bit |> char

        let charScore c =
            (int c) -
                if System.Char.IsLower(c) then (int 'a') - 1 else (int 'A') - 27

        let lines = input |> text |> splitLine

        // split each line in half
        let part1 = lines |> Array.map (fun s ->
            let (a,b) = s.ToCharArray() |> Array.splitAt (s.Length / 2)
            [| a; b |])

        part1 |> Array.map (findCommonItem >> charScore) |> Seq.sum |> string |> output 1

        // group lines into sets of three
        let part2 = lines |> Array.map (fun s -> s.ToCharArray()) |> Array.chunkBySize 3

        part2 |> Array.map (findCommonItem >> charScore) |> Seq.sum |> string |> output 2
