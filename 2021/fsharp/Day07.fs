namespace AdventOfCode.FSharp.Y2021

// Day 7: The Treachery of Whales
module Day07 =
    open AdventOfCode.FSharp.Util
    open Checked

    let part1 (text: string) =
        let input = text |> ints |> Array.map int64 |> Array.sort

        let median = input.[input.Length / 2]

        let cost x y = abs (x - y)

        input |> Array.map (cost median) |> Array.sum |> string

    let localMin f minlow maxhigh =
        let rec localMinUtil low high =
            let mid = low + (high - low) / 2L

            let midv = f mid
            let lowv = f (mid - 1L)
            let highv = f (mid + 1L)

            if (mid = minlow || lowv > midv) && (mid = maxhigh || midv < highv) then
                mid
            elif mid > minlow && lowv < midv then
                localMinUtil low (mid - 1L)
            else
                localMinUtil (mid + 1L) high

        localMinUtil minlow maxhigh

    let part2 (text: string) =
        let input = text |> ints |> Array.map int64 |> Array.sort

        let cost x y =
            let n = abs (x - y)
            n * (n + 1L) / 2L

        let totalCost p =
            input |> Array.map (cost p) |> Array.sum

        let min = localMin totalCost 0L (Array.max input)

        totalCost min |> string

    let runOld (input: byte array) (output: int -> string -> unit) =
        let inputText = input |> text
        inputText |> part1 |> output 1
        inputText |> part2 |> output 2

    let run (input: byte array) output =
        let input = input |> parseInts |> Array.map int64 |> Array.sort

        let median = input.[input.Length / 2]

        let cost1 x y = abs (x - y)

        input |> Array.map (cost1 median) |> Array.sum |> string |> output 1

        let cost2 x y =
            let n = abs (x - y)
            n * (n + 1L) / 2L

        let totalCost p =
            input |> Array.map (cost2 p) |> Array.sum

        let min = localMin totalCost 0L (Array.max input)

        totalCost min |> string |> output 2
