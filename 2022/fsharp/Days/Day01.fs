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
