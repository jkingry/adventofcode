namespace AdventOfCode.FSharp.Y2022

// Day 1: Calorie Counting
module Day01 =
    open Checked
    open AdventOfCode.FSharp.Util

    let run (input: string) (output: int -> string -> unit) =
        let elfs =
            input
            |> splitDoubleLine
            |> Array.map (fun s -> s |> splitLine |> Seq.map int |> Seq.sum)

        elfs |> Seq.max |> string |> output 1
        
        elfs |> Seq.sortDescending |> Seq.take 3 |> Seq.sum |> string |> output 2

