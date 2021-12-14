namespace AdventOfCode.FSharp.Y2020

module Day02 =
    open AdventOfCode.FSharp.Util

    type Pline =
        { min: int
          max: int
          c: char
          input: string }

    let parse line =
        match line with
        | Regex @"(\d+)\-(\d+) ([a-z])\: ([a-z]+)" [ mint; maxt; ct; input ] ->
            Some
                { min = int mint
                  max = int maxt
                  c = ct.[0]
                  input = input }
        | _ -> None

    let part1 input =
        let valid p =
            let ccount =
                p.input |> Seq.where (fun x -> x = p.c) |> Seq.length

            p.min <= ccount && ccount <= p.max

        input
        |> splitLine
        |> Seq.choose parse
        |> Seq.where valid
        |> Seq.length
        |> string

    let part2 input =
        let valid p =
            (p.input.[p.min - 1] = p.c) <> (p.input.[p.max - 1] = p.c)

        input
        |> splitLine
        |> Seq.choose parse
        |> Seq.where valid
        |> Seq.length
        |> string
