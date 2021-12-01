namespace AdventOfCode.FSharp.Y2021

open System

module Day1 =

    let parse input =
        input
        |> Seq.map Int32.Parse
        |> Seq.toArray

    let part1 input =
        input 
        |> Seq.map Int32.Parse
        |> Seq.pairwise
        |> Seq.fold (fun t (a,b) -> t + if b > a then 1 else 0) 0

    let part2 input = 
        input
        |> Seq.map Int32.Parse
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.fold (fun t (a,b) -> t + if b > a then 1 else 0) 0
