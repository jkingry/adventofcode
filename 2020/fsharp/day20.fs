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
    
    let holes (puzzle : Map<int * int, Tile>) : (int * int) seq =
        let dir = [
            (1, 0)
            (-1, 0)
            (0, -1)
            (0, 1)
        ]
        puzzle.Keys |> Seq.collect (fun (x, y) -> dir |> List.filter (fun (dx, dy) -> (x + dx, y + dy) |> puzzle.ContainsKey |> not))

    let findtileindex (tiles : Tile list) (puzzle : Map<int * int, Tile>) (pos : int * int): int option =
        let xlen = (List.head tiles).tile |> Array2D.length1 
        let ylen = (List.head tiles).tile |> Array2D.length2 

        let x, y = pos
        let hborders = [
            puzzle |> Map.tryFind (x - 1, y) |> Option.bind (fun t -> Some t.tile[*, xlen - 1])
            puzzle |> Map.tryFind (x + 1, y) |> Option.bind (fun t -> Some t.tile[*, 0])
            puzzle |> Map.tryFind (x, y - 1) |> Option.bind (fun t -> Some t.tile[ylen - 1, *])
            puzzle |> Map.tryFind (x, y + 1) |> Option.bind (fun t -> Some t.tile[0, *])
        ]

        let ismatch t =
            let tborders = [
                t.tile[*, 0]
                t.tile[*, xlen - 1]
                t.tile[0, *]
                t.tile[ylen - 1, *]
            ]
            List.forall2
                (fun tba hb -> match hb with | None -> true; | Some hba -> Array.forall2 (fun x y -> x = y) hba tba)
                tborders
                hborders

        List.tryFindIndex ismatch tiles

    let rec solve (tiles : Tile list) (puzzle : Map<int * int, Tile>) =
        match tiles with
        | [] -> Some puzzle
        | t::ts -> 
            if puzzle.Count <> 0 then
                puzzle 
                |> holes
                |> Seq.tryPick (fun hole ->
                    printfn "%A" hole
                    let index = findtileindex tiles puzzle hole
                    match index with
                    | None -> None
                    | Some i -> 
                        let nt = tiles[i]
                        solve (List.removeAt i tiles) (Map.add hole nt puzzle)
                )
            else
                solve ts (Map.add (0, 0) t puzzle)

    let part1 (input : string seq) =
        // let cache = input |> Seq.toList
        let e = input.GetEnumerator()
        let tiles = List.unfold parseTile e
        let f = solve tiles Map.empty
        printfn $"{f}"

        tiles.Length |> bigint

    let part2 (input : string seq) =
        -1