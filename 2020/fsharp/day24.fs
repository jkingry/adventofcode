namespace AdventOfCode.FSharp.Y2020

// Day 24: Lobby Layout
module Day24 =
    open AdventOfCode.FSharp.Util
    open System
    open System.Collections.Generic

    let run input output =
        let parseTile position line =
            (1,2)
        let createGrid (lobby, (count), (offsetX, offsetY)) (x,y) =
            lobby, (count+1), (offsetX, offsetY)

        let (lobby, count, _) =
            input
            |> splitLine
            |> Array.map (fun line -> line |> Seq.fold parseTile (0,0))
            |> Array.fold createGrid ((Array2D.zeroCreate 10 10), 0, (0, 0))
        
        ()