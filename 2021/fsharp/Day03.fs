namespace AdventOfCode.FSharp.Y2021

open System
open System.Text
open System.Text.RegularExpressions
open AdventOfCode.FSharp.Util

type Pos = 
    {
        depth : int
        h : int
        aim : int
    }

module Day03 =
    let executeInput exec (input : string seq) =
        let { depth = depth; h = h } = 
            input
            |> Seq.map (fun line -> let p = line.Split(' ') in (p[0], Int32.Parse p[1]))
            |> Seq.fold exec { depth = 0; h = 0; aim = 0 }
        depth * h    

    let part1 (input : string seq) =
        let n = 5
        let a = Array.zeroCreate n
        let counts =
            input
            |> Seq.iter (fun s -> 
                s |> Seq.iteri (fun i c -> if c = '1' then a[i] <- a[i] + 1)) 
        
        Array.mapi (fun i aa -> if  )
        -1
    let part2 input = 
        -1
