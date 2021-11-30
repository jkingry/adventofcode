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
        id : int64
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
            let id = Int64.Parse(e.Current.Split(' ').[1].Trim(':'))
            let tile = e |> takeWhile |> array2D
            Some ({ id=id; tile=tile }, e)

    let holes (puzzle : Map<int * int, Tile>) : (int * int) seq =
        let dir = [
            (0, 1)
            (0, -1)
            (-1, 0)
            (1, 0)
        ]
        puzzle.Keys 
        |> Seq.collect (fun (r, c) -> dir |> Seq.map (fun (dr, dc) -> (r + dr, c + dc)))
        |> Seq.filter (puzzle.ContainsKey >> not)
        |> Seq.distinct
        
    let rotate (tile : char[,]) : char[,] =
        let n = Array2D.length1 tile
        tile |> Array2D.mapi (fun x y v -> tile[n - y - 1, x])

    let hflip (tile : char[,]) : char[,] =
        let n = Array2D.length1 tile
        tile |> Array2D.mapi (fun x y v -> tile[n - x - 1, y])

    let vflip (tile : char[,]) : char[,] =
        let n = Array2D.length1 tile
        tile |> Array2D.mapi (fun x y v -> tile[x, n - y- 1])

    let permute (tile : char[,]) : char[,] seq =
        seq {
            let mutable t = tile
            for r = 1 to 4 do            
                yield t
                yield hflip t
                yield vflip t
                t <- rotate t
        }        

        
    let findtiles (tiles : Tile seq) (puzzle : Map<int * int, Tile>) (pos : int * int): Tile seq =
        let n = (Seq.head tiles).tile |> Array2D.length1 

        let r, c = pos
        let hborders = [
            puzzle |> Map.tryFind (r, c - 1) |> Option.bind (fun t -> Some t.tile[*, n - 1])
            puzzle |> Map.tryFind (r, c + 1) |> Option.bind (fun t -> Some t.tile[*, 0])
            puzzle |> Map.tryFind (r - 1, c) |> Option.bind (fun t -> Some t.tile[n - 1, *])
            puzzle |> Map.tryFind (r + 1, c) |> Option.bind (fun t -> Some t.tile[0, *])
        ]      

        let ismatch t =
            permute t.tile
            |> Seq.map (fun tile -> { t with tile = tile })
            |> Seq.tryPick (fun tp ->
                let tborders = [
                    tp.tile[*, 0]
                    tp.tile[*, n - 1]
                    tp.tile[0, *]
                    tp.tile[n - 1, *]
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

    let rec solve (tiles : Map<int64, Tile>) (puzzle : Map<int * int, Tile>) =
        if puzzle.Count <> 0 then
            if tiles.Count = 0 then
                Some puzzle
            else
                puzzle 
                |> holes
                |> Seq.tryPick (fun hole ->
                    let matchtiles = findtiles tiles.Values puzzle hole

                    matchtiles
                    |> Seq.tryPick (fun mt ->
                        solve 
                            (tiles |> Map.remove mt.id) 
                            (puzzle |> Map.add hole mt)))
        else
            let mt = tiles |> Map.values |> Seq.head
            solve 
                (tiles |> Map.remove mt.id) 
                (puzzle |> Map.add (0, 0) mt)

    let corners (p : Map<int * int, Tile>) = 
        let (ulx, uly) = p |> Map.keys |> Seq.min
        let (brx, bry) = p |> Map.keys |> Seq.max
        [
            p[(ulx, uly)]
            p[(ulx, bry)]
            p[(brx, bry)]
            p[(brx, uly)]]

    let parse (input : string seq) =
        let e = input.GetEnumerator()
        List.unfold parseTile e
            |> List.map (fun t -> (t.id, t))
            |> Map.ofList

    let part1 (input : string seq) =
        let tiles = parse input 

        let solution = solve tiles Map.empty
        let result = 
            match solution with
            | None -> -1L
            | Some m -> 
                corners m |> List.fold (fun a  b -> b.id * a) 1L
        result |> bigint
            
    let monster = [
        "                  # "
        "#    ##    ##    ###"
        " #  #  #  #  #  #   "] |> array2D
    let monsterHeight = Array2D.length1 monster
    let monsterWidth = Array2D.length2 monster 

    let printMap markedMap =
        for r = 0 to (Array2D.length1 markedMap) - 1 do
            let row = markedMap[r, *]
            printfn "%s" (new string(row))     

    let markMonster (row : int) (col : int) (theMap : char[,]) =
        let mapSlice = theMap[row..row + monsterHeight - 1, col..col + monsterWidth - 1]
        let newMap = monster |> Array2D.mapi (fun r c v -> match v with '#' -> 'O'; | _ -> mapSlice[r, c])
        theMap[row..row + monsterHeight - 1, col..col + monsterWidth - 1] <- newMap

    let isMonster (row : int) (col : int) (theMap : char[,]) =
        let monsterCoords = seq {
            for r = 0 to monsterHeight - 1 do
                for c = 0 to monsterWidth - 1 do
                    if monster[r,c] = '#' then yield (r, c)
        }

        let mapSlice = theMap[row..row + monsterHeight - 1, col..col + monsterWidth - 1]

        monsterHeight = (Array2D.length1 mapSlice)  &&
            monsterWidth = (Array2D.length2 mapSlice) &&
            monsterCoords |> Seq.forall (fun (r, c) -> mapSlice[r, c] = '#')
        
    let markMap (theMap : char[,]) = 
        theMap |> Array2D.iteri (fun r c v -> 
            if isMonster r c theMap then
                markMonster r c theMap)                    

    let part2 (input : string seq) =
        let tiles = parse input 

        let p = (solve tiles Map.empty).Value

        let (ulr, ulc) = p |> Map.keys |> Seq.min
        let (brr, brc) = p |> Map.keys |> Seq.max

        for tr = ulr to brr do
            for tc = ulc to brc do
                printf "%d " p[(tr, tc)].id
            printfn ""  

        printfn "ul = %A" (ulr, ulc)
        printfn "br = %A" (brr, brc)
        let n = (tiles.Values |> Seq.head).tile |> Array2D.length1 
        let pn = n - 2

        let tw = (1 + brc - ulc)
        let th = (1 + brr - ulr)
        let w = pn * tw
        let h = pn * th

        let lookup r c =
            let tr = ulr + (r / pn)
            let tc = ulc + (c / pn)
            let nr = (r % pn) + 1
            let nc = (c % pn) + 1
            p[(tr, tc)].tile[nr, nc]

        let fp = Array2D.init h w lookup

        let countSymbol symbol input =
            let mutable count = 0
            input |> Array2D.iter (fun v -> if v = symbol then count <- count + 1)
            count

        let markedMap = fp |> permute |> Seq.pick (fun pfp ->
            markMap pfp
            if (pfp |> countSymbol 'O') > 0 then Some pfp else None)
        
        printMap markedMap

        markedMap |> countSymbol '#'

