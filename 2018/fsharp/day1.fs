namespace AdventOfCode2018

open System

module Day1 =

    let parse input =
        input
        |> Seq.map Int32.Parse
        |> Seq.toArray

    let part1 (input : string seq) =        
        parse input |> Seq.sum

    let part2 (input : string seq)  = 
        let x = parse input

        let mutable notFound : int option = None
        let mutable s = Set.empty
        let mutable sum = 0
        while notFound.IsNone do
            for y in x do
                sum <- sum + y
                if Set.contains sum s then
                    notFound <- Some sum
                s <- Set.add sum s
        notFound.Value
