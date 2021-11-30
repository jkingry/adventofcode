namespace AdventOfCode.FSharp.Y2019

open System

// The Tyranny of the Rocket Equation
module Day1 =

    let part1 (input : string seq) =
        let f x = max ((x / 3) - 2) 0
        let q = input |> Seq.map Int32.Parse |> Seq.map f
        q |> Seq.sum

    let part2 (input : string seq) = 
        let f x  = max ((x / 3) - 2) 0
        let g x =
            let y = f x 
            if y > 0 then Some(y, y) else None
        let h x = (List.unfold g x) |> List.sum

        input |> Seq.map Int32.Parse |> Seq.map h |> Seq.sum
