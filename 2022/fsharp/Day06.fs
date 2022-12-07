namespace AdventOfCode.FSharp.Y2022

// Day 6: ????
module Day06 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =    
        let mutable mc = 0

        for (i, cw) in (Seq.windowed 14 input) |> Seq.indexed do
            let s = Set.ofSeq cw
            if s.Count = 14 && mc = 0 then
                mc <- i

        (mc + 14) |> string |> output 1
        0 |> string |> output 2
