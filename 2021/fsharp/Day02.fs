namespace AdventOfCode.FSharp.Y2021

open System

module Day02 =
    let increasing input =
        input
        |> Seq.pairwise
        |> Seq.filter (fun (a,b) -> (b > a))
        |> Seq.length

    let part1 input =
        input 
        |> Seq.map Int32.Parse
        |> increasing

    let part2 input = 
        input
        |> Seq.map Int32.Parse
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.filter (fun (a,b) -> (b > a))
        |> Seq.length