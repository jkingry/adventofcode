namespace AdventOfCode.FSharp.Y2017

module Day01 =
    open AdventOfCode.FSharp.Util

    let parse (input: string) = input.ToCharArray() |> Array.map int


    let part1 (input: string) =
        let x = input |> parse

        Array.allPairs x x
        |> Seq.filter (fun (a, b) -> a <> b && (a + b) = 2020)
        |> Seq.map (fun (a, b) -> a * b)
        |> Seq.head
        |> bigint
        |> string

    let list3tuple (l: 'a list list) =
        l |> List.map (fun x -> x.[0], x.[1], x.[2])

    let part2 (input: string) =
        let x = input |> parse

        Array.allPairs x x
        |> Array.allPairs x
        |> Seq.filter (fun (a, (b, c)) -> a <> b && a <> c && (a + b + c) = 2020)
        |> Seq.map (fun (a, (b, c)) -> a * b * c)
        |> Seq.head
        |> bigint
        |> string

    let run (input: byte array) output =
        let textInput = text input
        part1 textInput |> output 1
        part2 textInput |> output 2
