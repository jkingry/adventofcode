namespace AdventOfCode.FSharp.Y2022

// Day 4: Rucksack Reorganization
module Day04 =
    open AdventOfCode.FSharp.Util
    open Checked

    let run (input: string) (output: int -> string -> unit) =    
        let lines = input |> splitLine

        let res = 
            lines 
            |> Seq.map(fun s -> 
                let a = s.Split(",")
                let x = a[0].Split("-") |> Array.map ints
                let y = a[1].Split("-") |> Array.map ints
                if (x[0] <= y[0] && y[0] <= x[1]) 
                    || (x[0] <= y[1] && y[1] <= x[1])
                    || (y[0] <= x[0] && x[0] <= y[1]) 
                    || (y[0] <= x[1] && x[1] <= y[1])  then 1 else 0)
        
        let res = res |> Seq.sum

        res |> string |> output 1
        2 |> string |> output 2

        