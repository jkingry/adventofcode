namespace AdventOfCode.FSharp.Y2022

// Day 1: Calorie Counting
module Day01 =
    open Checked
    open AdventOfCode.FSharp.Util
    open System

    let run (input: byte array) (output: int -> string -> unit) =
        let elfs =
            input
            |> text
            |> splitDoubleLine
            |> Array.map (fun s -> s |> splitLine |> Seq.map int |> Seq.sum)

        elfs |> Seq.max |> string |> output 1
        
        elfs |> Seq.sortDescending |> Seq.take 3 |> Seq.sum |> string |> output 2

    let zero = byte '0' 
    let newLine = byte '\n'

    let inline parseInt (s: ReadOnlySpan<byte>) =
        let mutable n = 0
        for c in s do
            n <- n * 10 + int (c - zero)  
        n

    let runFast input output =
        let s = new ReadOnlySpan<byte>(input)

        let mutable sum = 0
        let mutable p = 0

        let top3 = Array.zeroCreate 3

        let handleElf sum =
            let mutable maxDiff = 0
            let mutable maxIndex = -1
            for j in 0..2 do
                let diff = sum - top3[j] 
                if diff > maxDiff then
                    maxIndex <- j
                    maxDiff <- diff 
            if maxIndex >= 0 then
                top3[maxIndex] <- sum

        for i in 0 .. s.Length - 1 do
            if s[i] = newLine then
                if i > p then
                    let n = parseInt (s.Slice(p, i - p))
                    sum <- sum + n
                else
                    handleElf sum
                    sum <- 0
                p <- i + 1
        handleElf sum

        top3 |> Array.max |> string |> output 1
        (top3[0] + top3[1] + top3[2]) |> string |> output 2
