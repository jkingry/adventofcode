namespace AdventOfCode.FSharp.Y2020

module Day02 =
    open AdventOfCode.FSharp.Util

    type Pline =
        { min: int
          max: int
          c: char
          p: string }

    let parse line =
        match line with
        | Regex @"(\d+)\-(\d+) ([a-z])\: ([a-z]+)" [ mint; maxt; ct; p ] ->
            Some
                { min = int mint
                  max = int maxt
                  c = ct.[0]
                  p = p }
        | _ -> None

    let part1 input =
        let valid p =
            let ccount =
                p.p |> Seq.where (fun x -> x = p.c) |> Seq.length

            ccount >= p.min && ccount <= p.max

        input
        |> splitLine
        |> Seq.choose parse
        |> Seq.where valid
        |> Seq.length
        |> string

    let part2 input =
        let valid p =
            (p.p.[p.min - 1] = p.c) <> (p.p.[p.max - 1] = p.c)

        input
        |> splitLine
        |> Seq.choose parse
        |> Seq.where valid
        |> Seq.length
        |> string
