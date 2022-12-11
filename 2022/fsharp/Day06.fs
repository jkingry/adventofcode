namespace AdventOfCode.FSharp.Y2022

// Day 6: Tuning Trouble
module Day06 =
    open AdventOfCode.FSharp.Util
    open System.Numerics
    open Checked

    let run (input: byte array) (output: int -> string -> unit) =    
        let findUniqueWindowIndex windowSize (text: string) =
            let window = Array.zeroCreate windowSize

            seq { 0 .. (text.Length - 1) } 
            |> Seq.pick (fun i ->
                let c = text[i]
                let cbit = (1UL <<< ((int c) - (int 'A')))

                let  windowPos = i % windowSize
                
                for offset in 0..(windowSize-1) do
                    let wi = (windowPos + offset) % windowSize
                    window[wi] <- window[wi] ||| cbit
                
                if BitOperations.PopCount(window[windowPos]) = windowSize then
                    Some (i + 1)
                else
                    window[windowPos] <- 0UL
                    None)

        input |> text |> findUniqueWindowIndex 4 |> string |> output 1
        input |> text |> findUniqueWindowIndex 14 |> string |> output 2
