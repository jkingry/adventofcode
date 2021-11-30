namespace AdventOfCode.FSharp.Y2020

open System
open AdventOfCode.FSharp.Util

module Day2 =
    type Pline = {
        min: int
        max: int
        c : char
        p : string
    }

    let parse line =
        match line with
        | Regex @"(\d+)\-(\d+) ([a-z])\: ([a-z]+)" [ mint; maxt; ct; p] ->
            Some { 
                min = Int32.Parse mint
                max = Int32.Parse maxt
                c = ct.[0]
                p = p
            }
        | _ -> None

    let part1 (input : string seq) : bigint =
        let valid p =
            let ccount = p.p |> Seq.where (fun x -> x = p.c) |> Seq.length
            ccount >= p.min && ccount <= p.max

        input 
        |> Seq.choose parse
        |> Seq.where valid
        |> Seq.length
        |> bigint

    let part2 input =
        let valid p =
            (p.p.[p.min - 1] = p.c) <> (p.p.[p.max - 1] = p.c)

        input
        |> Seq.choose parse 
        |> Seq.where valid
        |> Seq.length
        |> bigint
