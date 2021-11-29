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
        puzzle.Keys 
        |> Seq.collect (fun (x, y) -> dir |> Seq.map (fun (dx, dy) -> (x + dx, y + dy)))
            // let ph = (x + dx, y + dy)
            // printfn "%A + %A = %A" (x,y) (dx, dy) ph
            // ph))
        |> Seq.filter (puzzle.ContainsKey >> not)
        |> Seq.distinct
        
    let rotate (tile : Tile) : Tile =
        let n = Array2D.length1 tile.tile
        { tile with tile = tile.tile |> Array2D.mapi (fun x y v -> tile.tile[n - y - 1, x]) }

    let hflip (tile : Tile) : Tile =
        let n = Array2D.length1 tile.tile
        { tile with tile = tile.tile |> Array2D.mapi (fun x y v -> tile.tile[n - x - 1, y]) }

    let vflip (tile : Tile) : Tile =
        let n = Array2D.length1 tile.tile
        { tile with tile = tile.tile |> Array2D.mapi (fun x y v -> tile.tile[x, n - y- 1]) }

    let permute (tile : Tile) : Tile seq =
        seq {
            let mutable t = tile
            for r = 1 to 4 do            
                yield t
                yield hflip t
                yield vflip t
                t <- rotate t
        }        

        
    let findtiles (tiles : Tile list) (puzzle : Map<int * int, Tile>) (pos : int * int): Tile seq =
        let xlen = (Seq.head tiles).tile |> Array2D.length1 
        let ylen = (Seq.head tiles).tile |> Array2D.length2 

        let x, y = pos
        let hborders = [
            puzzle |> Map.tryFind (x - 1, y) |> Option.bind (fun t -> Some t.tile[*, xlen - 1])
            puzzle |> Map.tryFind (x + 1, y) |> Option.bind (fun t -> Some t.tile[*, 0])
            puzzle |> Map.tryFind (x, y - 1) |> Option.bind (fun t -> Some t.tile[0, *])
            puzzle |> Map.tryFind (x, y + 1) |> Option.bind (fun t -> Some t.tile[ylen - 1, *])
        ]      

        let ismatch t =
            permute t
            |> Seq.tryPick (fun tp ->
                let tborders = [
                    tp.tile[*, 0]
                    tp.tile[*, xlen - 1]
                    tp.tile[ylen - 1, *]
                    tp.tile[0, *]
                ]       

                let fits = 
                    List.forall2 (fun tba hb -> 
                        match hb with 
                        | None -> true
                        | Some hba -> 
                            Array.forall2 (fun x y -> x = y) hba tba) tborders hborders
                if fits then Some tp else None
            )
        tiles |> Seq.choose ismatch

    let rec solve (tiles : Tile list) (puzzle : Map<int * int, Tile>) (depth : int) =
        if puzzle.Count <> 0 then
            if tiles.Length = 0 then
                Some puzzle
            else
                puzzle 
                |> holes
                |> Seq.tryPick (fun hole ->
                    let matchtiles = findtiles tiles puzzle hole
                    printfn "%s %A" (new String (' ', depth)) hole
                    
                    matchtiles
                    |> Seq.tryPick (fun matchtile ->
                        printfn "%s %A = %d" (new String (' ', depth)) hole matchtile.id
                        let matchindex = tiles |> List.findIndex (fun t -> t.id = matchtile.id)
                        let ntiles = tiles |> List.removeAt matchindex
                        let npuzzle = Map.add hole matchtile puzzle
                        solve ntiles npuzzle (depth + 1)))
        else
            let t = List.head tiles
            let matchindex = tiles |> List.findIndex (fun t -> t.id = t.id)
            let ntiles = tiles |> List.removeAt matchindex
            printfn "%d" t.id
            solve ntiles (Map.add (0, 0) t puzzle) (depth + 1)

    let corners (p : Map<int * int, Tile>) = 
        let (ulx, uly) = p |> Map.keys |> Seq.min
        let (brx, bry) = p |> Map.keys |> Seq.max
        [
            p[(ulx, uly)]
            p[(ulx, bry)]
            p[(brx, bry)]
            p[(brx, uly)]]

    let part1 (input : string seq) =
        // let cache = input |> Seq.toList
        let e = input.GetEnumerator()
        let tiles = List.unfold parseTile e


        let solution = solve tiles Map.empty 0
        let result = 
            match solution with
            | None -> -1L
            | Some m -> 
                corners m |> List.fold (fun (a : int64) b ->
                    printfn "%d" b.id 
                    (int64 b.id) * a) 1L
        result |> bigint
            

    let part2 (input : string seq) =
        -1