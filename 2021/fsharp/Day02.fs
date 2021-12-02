namespace AdventOfCode.FSharp.Y2021

open System
open System.Text
open System.Text.RegularExpressions
open AdventOfCode.FSharp.Util

module Day02 =
    let increasing input =
        input
        |> Seq.pairwise
        |> Seq.filter (fun (a,b) -> (b > a))
        |> Seq.length

    let part1 (input : string seq)=        
        let mutable depth = 0
        let mutable h = 0
        let mutable aim = 0

        input 
        |> Seq.iter (fun line -> 
            let p = line.Split(' ')
            let d = Int32.Parse(p[1])
            match p[0] with 
            | "down" ->
                 aim <- aim + d
            | "up" ->
                aim <- aim - d

            | "forward" -> 
                h <- h + d
                depth <- depth + (aim * d))

        h * depth

    let part2 input = 
        input
        |> Seq.map Int32.Parse
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.filter (fun (a,b) -> (b > a))
        |> Seq.length