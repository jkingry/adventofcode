namespace AdventOfCode.FSharp.Y2017

open System

module Day1 =

    let parse (input: string seq) =
        input |> Seq.map Int32.Parse |> Seq.toArray


    let part1 (input: string seq) =
        let x = input |> parse

        Array.allPairs x x
        |> Seq.filter (fun (a, b) -> a <> b && (a + b) = 2020)
        |> Seq.map (fun (a, b) -> a * b)
        |> Seq.head
        |> bigint

    let list3tuple (l: 'a list list) =
        l |> List.map (fun x -> x.[0], x.[1], x.[2])

    let part2 (input: string seq) =
        let x = input |> parse

        Array.allPairs x x
        |> Array.allPairs x
        |> Seq.filter (fun (a, (b, c)) -> a <> b && a <> c && (a + b + c) = 2020)
        |> Seq.map (fun (a, (b, c)) -> a * b * c)
        |> Seq.head
        |> bigint
