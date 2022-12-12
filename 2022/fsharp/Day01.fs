namespace AdventOfCode.FSharp.Y2022

// Day 1: Calorie Counting
module Day01 =
    open Checked
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let elfs =
            input
            |> text
            |> splitDoubleLine
            |> Array.map (fun s -> s |> splitLine |> Seq.map int |> Seq.sum)

        elfs |> Seq.max |> string |> output 1
        
        elfs |> Seq.sortDescending |> Seq.take 3 |> Seq.sum |> string |> output 2

    let runFast (input: byte array) output =
        let mutable sum = 0

        let top3 = Array.zeroCreate 3

        let handleElf () =
            let mutable maxDiff = 0
            let mutable maxIndex = -1
            for j in 0..2 do
                let diff = sum - top3[j] 
                if diff > maxDiff then
                    maxIndex <- j
                    maxDiff <- diff 
            if maxIndex >= 0 then
                top3[maxIndex] <- sum
            sum <- 0

        let newLine = byte '\n'
        let mutable i = 0
        while i < (input.Length - 1) do 
            if input[i] = newLine then
                handleElf ()
                i <- i + 1

            let (ni, n) = parseIntToDelim input i newLine
            sum <- sum + n

            i <- ni

        handleElf ()

        top3 |> Array.max |> string |> output 1
        (top3[0] + top3[1] + top3[2]) |> string |> output 2
