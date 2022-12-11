namespace AdventOfCode.FSharp.Y2022

// Day 11
module Day11 =
    open Checked
    open AdventOfCode.FSharp.Util

    let run (input: byte array) (output: int -> string -> unit) =
        let r =
            input
            |> text
            |> splitLine
            |> Seq.map (fun s ->
                1)
                
        1 |> string |> output 1 
        1 |> string |> output 2 
