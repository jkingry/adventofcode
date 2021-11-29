namespace AdventOfCode2020

open Util
open System
open System.Text
open System.Text.RegularExpressions
open FParsec
open System.Collections.Generic

// Jurassic Jigsaw
type Tile = 
    { 
        id : int
        tile : char[,]
    }

module Day20 =

    let parseTile (e : IEnumerator<string>) =
        let takeWhile (x : IEnumerator<string>) = 
            seq {
                while x.MoveNext() && (String.IsNullOrEmpty(x.Current) = false) do
                    yield e.Current
            }
        match e.MoveNext() with
        | false -> None
        | true -> 
            let id = Int32.Parse(e.Current.Split(' ').[1].Trim(':'))
            let tile = e |> takeWhile |> array2D
            Some ({ id=id; tile=tile }, e)

    let ismatch (a : Tile) (b : Tile) =        
        let xlen = Array2D.length1 b.tile

        Array.forall2 (fun x y -> x = y) a.tile[*, 0] b.tile[*, xlen - 1]

    let part1 (input : string seq) =
        // let cache = input |> Seq.toList
        let e = input.GetEnumerator()
        let tiles = List.unfold parseTile e
        for t in tiles do
            for u in tiles do
                if t.id <> u.id && ismatch t u then
                    printfn "%d = %d" t.id u.id
        tiles.Length

    let part2 (input : string seq) =
        -1