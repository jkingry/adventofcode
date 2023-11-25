namespace AdventOfCode.FSharp.Y2016

// Day 18: Like a Rogue
module Day18 =
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let inputLine1 = input |> bsplit '\n'B |> Array.head

        let row = bits inputLine1.Length

        inputLine1
        |> Array.iteri (fun i c ->
            match c with
            | '^'B -> row[i] <- true
            | _ -> ())

        let nextRow (row: bits) =
            let temp = (bits row).LeftShift 1
            row.RightShift 1 |> ignore
            row.Xor temp |> ignore

        let mutable total = 0

        for _ = 1 to 40 do
            total <- total + (row.Count - Bits.popCount row)
            nextRow row

        total |> string |> output 1

        for _ = 41 to 400000 do
            total <- total + (row.Count - Bits.popCount row)
            nextRow row

        total |> string |> output 2
