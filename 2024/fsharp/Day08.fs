namespace AdventOfCode.FSharp.Y2024

// Day 8
module Day08 =
    open AdventOfCode.FSharp.Util

    let getAntinodes (nodes: (int*int) seq) =
        Seq.allPairs nodes nodes
        |> Seq.filter (fun (x,y) -> x < y)
        |> Seq.collect (fun ((a,b),(c,d)) ->
            let e = c - a
            let f = d - b
            [
                (a - e, b - f)
                (c + e, d + f)
            ])
    let getHarmonicAntinodes mx my (nodes: (int*int) seq) =
        Seq.allPairs nodes nodes
        |> Seq.filter (fun (x,y) -> x < y)
        |> Seq.collect (fun ((a,b),(c,d)) ->
            let dx = c - a
            let dy = d - b

            seq {
                let mutable (x,y) = a,b
                
                while x >= 0 && y >= 0 && x < mx && y < my do
                    yield (x,y)
                    x <- x - dx
                    y <- y - dy

                let mutable (x,y) = (a + dx), (b + dy)

                while x >= 0 && y >= 0 && x < mx && y < my do
                    yield (x,y)
                    x <- x + dx
                    y <- y + dy
            })            
 
    let run (input: byte array) (output: int -> string -> unit) =
        let antennaMap = 
            input 
            |> bsplit '\n'B
            |> array2D

        let mutable antennas = Map.empty

        antennaMap
        |> Array2D.iteri (fun x y c -> 
            if c <> '.'B then
                antennas <- antennas |> Map.change c (function 
                    | None -> [ x,y ] |> Some
                    | Some xs -> (x,y)::xs |> Some))

        antennas
        |> Map.values
        |> Seq.collect getAntinodes
        |> Seq.filter (fun (x,y) -> 
            OrthoGrid.checkBounds antennaMap x y)
        |> Seq.distinct
        |> Seq.length
        |> string
        |> output 1

        let mx = Array2D.length1 antennaMap
        let my = Array2D.length2 antennaMap

        antennas
        |> Map.values
        |> Seq.collect (getHarmonicAntinodes mx my)
        |> Seq.distinct
        |> Seq.length
        |> string
        |> output 2
