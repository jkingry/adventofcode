namespace AdventOfCode.FSharp.Y2023

// Day 9: Mirage Maintenance
module Day09 =
    open AdventOfCode.FSharp.Util

    let rec next (input: int[]) =
        let v = input[0]

        if input |> Array.forall (fun x -> x = v) then
            v, v
        else
            let first, last = input |> Array.pairwise |> Array.map (fun (a, b) -> b - a) |> next

            (Array.head input) - first, last + (Array.last input)

    let run (input: byte[]) (output: int -> string -> unit) =
        let parsed = input |> text |> splitLine |> Array.map ints

        let part2, part1 =
            parsed |> Array.map next |> Array.reduce (fun (a, b) (c, d) -> a + c, b + d)

        part1 |> string |> output 1
        part2 |> string |> output 2
