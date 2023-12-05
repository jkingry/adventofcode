namespace AdventOfCode.FSharp.Y2023

// Day 5
module Day05 =
    open AdventOfCode.FSharp.Util

    let parse (input: byte[]) =
        let sections = input |> text |> splitDoubleLine

        let seeds = sections[ 0 ].Split(' ') |> Array.tail |> Array.map int64

        let maps =
            sections
            |> Array.tail
            |> Array.map (fun s ->
                s
                |> splitLine
                |> Array.tail
                |> Array.map (fun l ->
                    match l.Split(' ') with
                    | [| a; b; c |] -> (int64 a), (int64 b), (int64 c)
                    | x -> failwithf "Invalid line: %A" x))

        seeds, maps

    let getNextSection almanac value =
        almanac
        |> Array.tryPick (function
            | (a, b, c) when b <= value && value <= (b + c) - 1L -> a + (value - b) |> Some
            | _ -> None)
        |> Option.defaultValue value

    let rec followSection almanac level value =
        if level >= (Array.length almanac) then
            value
        else
            let value' = getNextSection almanac[level] value
            followSection almanac (level + 1) value'

    let run (input: byte[]) (output: int -> string -> unit) =
        let seeds, sections = input |> parse

        seeds |> Array.map (followSection sections 0) |> Array.min |> string |> output 1
