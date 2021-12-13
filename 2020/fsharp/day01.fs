namespace AdventOfCode.FSharp.Y2020

module Day01=
    open AdventOfCode.FSharp.Util
    open System
    open System.Collections.Generic
    
    let parse input =
        input
        |> splitLine
        |> Seq.map Int32.Parse
        |> Seq.toArray

    let list2tuple (l : 'a list list ) =
        l |> List.map (fun x -> x.[0], x.[1] )

    let part1 input =
        let x = 
            input
            |> parse

        Array.allPairs x x 
        |> Seq.filter (fun (a, b) -> a <> b && (a + b) = 2020)
        |> Seq.map (fun (a, b) -> a * b)
        |> Seq.head
        |> bigint
        |> string

    let list3tuple (l : 'a list list ) =
        l |> List.map (fun x -> x.[0], x.[1], x.[2] )

    let part2 input = 
        let x = 
            input
            |> parse

        Array.allPairs x x
        |> Array.allPairs x
        |> Seq.filter (fun (a, (b, c)) -> a <> b && a <> c && (a + b + c) = 2020)
        |> Seq.map (fun (a, (b, c)) -> a * b * c)
        |> Seq.head
        |> bigint
        |> string
