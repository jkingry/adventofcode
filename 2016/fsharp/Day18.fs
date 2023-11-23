namespace AdventOfCode.FSharp.Y2016

// Day 18: Like a Rogue
module Day18 =
    open AdventOfCode.FSharp.Util

    type bits = System.Collections.BitArray

    let run (input: byte array) (output: int -> string -> unit) =
        let toString (row: bits) =
            let mutable buf = System.Text.StringBuilder row.Count

            for i = 0 to row.Count - 1 do
                buf <- buf.Append(if row[i] then '^' else '.')

            buf.ToString()

        let popCount (row: bits) =
            let dest: int[] = Array.zeroCreate (((row.Count |> float) / 32.0) |> ceil |> int)
            row.CopyTo(dest, 0)
            dest |> Array.map (uint >> System.Numerics.BitOperations.PopCount) |> Array.sum

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
            total <- total + (row.Count - popCount row)
            nextRow row

        total |> string |> output 1

        for _ = 41 to 400000 do
            total <- total + (row.Count - popCount row)
            nextRow row

        total |> string |> output 2
