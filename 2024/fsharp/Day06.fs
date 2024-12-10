namespace AdventOfCode.FSharp.Y2024

// Day 6
module Day06 =
    open Checked
    open AdventOfCode.FSharp.Util

    let inline (@+) (a: int * int) (b: int * int) =
        let ax, ay = a
        let bx, by = b
        (ax + bx), (ay + by)

    let inline isWall (lab: char[,]) (r, c) =
        if OrthoGrid.checkBounds lab r c then
            lab[r, c] = '#' |> Some
        else
            None

    let DIRECTIONS = [| -1, 0; 0, 1; 1, 0; 0, -1 |]

    let runRobotLoop (isWall: int * int -> bool option) startVisited startPos =
        let mutable visited = startVisited |> Set.add startPos

        let mutable pos = Some startPos

        let mutable exited = false

        while not exited do
            let p, dir = pos.Value
            let next = p @+ DIRECTIONS[dir]

            pos <-
                match next |> isWall with
                | None -> None
                | Some true ->
                    let dir = (dir + 1) % DIRECTIONS.Length
                    Some(p, dir)
                | Some false -> Some(next, dir)

            match pos with
            | None -> exited <- true
            | Some x ->
                if visited |> Set.contains x then
                    exited <- true
                else
                    visited <- visited |> Set.add x

        match pos with
        | None -> visited, true
        | _ -> visited, false


    let printMap (lab: char[,]) visited next =
        for rowIndex = 0 to (Array2D.length1 lab) - 1 do
            let row = lab[rowIndex, *]

            for colIndex = 0 to row.Length - 1 do
                let pos = (rowIndex, colIndex)

                let c =
                    if next = pos then 'O'
                    elif visited |> Set.contains pos then 'x'
                    else row[colIndex]

                printf "%c" c

            printfn ""

    let runRobotLoop2 lab (isWall: int * int -> bool option) startVisited startPos =
        let mutable visited = startVisited |> Set.add startPos

        let mutable pos = Some startPos

        let mutable exited = false
        let mutable found = Set.empty

        while not exited do
            let p, dir = pos.Value
            let next = p @+ DIRECTIONS[dir]

            match next |> isWall with
            | None -> pos <- None
            | Some true ->
                let dir' = (dir + 1) % DIRECTIONS.Length
                pos <- Some(p, dir')
            | Some false ->
                pos <- Some(next, dir)

                if found |> Set.contains next |> not then
                    let newIsWall x =
                        if x = next then Some true else isWall x

                    let (_, res) = runRobotLoop newIsWall Set.empty startPos

                    if not res then
                        found <- found |> Set.add next

            match pos with
            | None -> exited <- true
            | Some x -> visited <- visited |> Set.add x

        let part1 = visited |> Seq.map fst |> Seq.distinct |> Seq.length
        let part2 = found.Count
        part1,part2

    let xrun (input: byte array) (output: int -> string -> unit) =
        let lab =
            input 
            |> text 
            |> splitLine 
            |> Array.map (fun s -> s.ToCharArray()) 
            |> array2D

        let startPos = lab |> Array2D.findIndex (fun c -> c = '^')
        let startDir = 0

        let (part1,part2) = runRobotLoop2 lab (isWall lab) Set.empty (startPos, startDir)

        part1 
        |> string
        |> output 1

        part2
        |> string
        |> output 2


    type WallMap = 
        {
            Rows: (int*int) array array
            Cols: (int*int) array array
        }

    let mapToWallsList (lab: char[,]) : WallMap=
        let mutable rows = Array.zeroCreate (Array2D.length1 lab)
        for rowIndex = 0 to (Array2D.length1 lab) - 1 do            
            let mutable row = []
            let mutable lastWall = -1
            for colIndex = 0 to (Array2D.length2 lab) - 1 do
                let c = lab[rowIndex,colIndex]
                if c = '#' then
                    row <- (lastWall, colIndex - 1)::row
                    lastWall <- colIndex + 1
            row <- (lastWall, (Array2D.length2 lab))::row
            rows[rowIndex] <- row |> List.rev |> List.toArray
        
        let mutable cols = Array.zeroCreate (Array2D.length2 lab)
        for colIndex = 0 to (Array2D.length2 lab) - 1 do
            let mutable col = []
            let mutable lastWall = -1
            for rowIndex = 0 to (Array2D.length1 lab) - 1 do            
                let c = lab[rowIndex,colIndex]
                if c = '#' then
                    col <- (lastWall, rowIndex - 1)::col
                    lastWall <- rowIndex + 1
            col <- (lastWall, (Array2D.length1 lab))::col
            cols[colIndex] <- col |> List.rev |> List.toArray
        
        { Rows = rows; Cols = cols }

    let addWall (walls: WallMap) (wr,wc) =
        let origRow = walls.Rows[wr] |> Array.copy
        let rowIndex = walls.Rows[wr] |> Array.findIndex (fun (f,t) -> f <= wc && wc <= t)
        let (a,b) = walls.Rows[wr][rowIndex]
        walls.Rows[wr][rowIndex] <- (a, wc - 1)
        walls.Rows[wr] <- walls.Rows[wr] |> Array.insertAt (rowIndex + 1) (wc + 1,b)
        // printfn "%A" walls.Rows[wr]

        let origCol = walls.Cols[wc] |> Array.copy
        let colIndex = walls.Cols[wc] |> Array.findIndex (fun (f,t) -> f <= wr && wr <= t)
        let (a,b) = walls.Cols[wc][colIndex]
        walls.Cols[wc][colIndex] <- (a, wr - 1)
        walls.Cols[wc] <- walls.Cols[wc] |> Array.insertAt (colIndex + 1) (wr + 1,b)
        // printfn "%A" walls.Cols[wc]

        (wr,wc),origRow,origCol

    let undoAddWall (walls: WallMap) ((wr,wc),origRow,origCol) =
        walls.Rows[wr] <- origRow
        walls.Cols[wc] <- origCol
        
    let getIntermediatePoints (dest,_) (src, d) =
        seq {
            let dir = DIRECTIONS[d] 

            let mutable c = src

            while c <> dest do
                yield c
                c <- c @+ dir
        }
    
    let checkBounds (walls: WallMap) r c  =
        r >= 0 && c >= 0 && r < walls.Rows.Length && c < walls.Cols.Length

    let getMove ((r,c),d) (walls: WallMap) =
        let next =
            match d with
            | 1 -> 
                r, walls.Rows[r] |> Array.find (fun (f,t) -> f <= c && c <= t) |> snd
            | 3 -> 
                r, walls.Rows[r] |> Array.findBack (fun (f,t) -> f <= c && c <= t) |> fst
            | 0 -> 
                walls.Cols[c] |> Array.findBack (fun (f,t) -> f <= r && r <= t) |> fst, c
            | 2 -> 
                walls.Cols[c] |> Array.find (fun (f,t) -> f <= r && r <= t) |> snd, c
            | _ -> raise Unreachable
        next, (d + 1) % DIRECTIONS.Length

    let runRobotLoop4 (walls: WallMap) startPos =

        let mutable pos = startPos

        let mutable exited = false
        let mutable looped = false

        let mutable visited = Set.empty |> Set.add startPos

        while not exited do
            pos <- walls |> getMove pos

            if pos |> fst ||> checkBounds walls |> not then
                exited <- true
            
            if visited |> Set.contains pos then
                looped <- true
                exited <- true

            visited <- visited |> Set.add pos

        looped

    let runRobotLoop3 (walls: WallMap) startPos =

        let mutable pos = startPos

        let mutable exited = false

        let mutable visited = [ startPos ]
        let mutable tried = Set.empty
        let mutable worked = 0

        while not exited do
            let dest = walls |> getMove pos

            for p in getIntermediatePoints dest pos |> Seq.skip 1 do
                if tried |> Set.contains p |> not then
                    tried <- tried |> Set.add p
                    // printfn "%A" p
                    let undo = addWall walls p
                    if runRobotLoop4 walls pos then                 
                        worked <- worked + 1
                    undoAddWall walls undo       

            if dest |> fst ||> checkBounds walls |> not then
                exited <- true
            else
                let drc = fst dest
                if tried |> Set.contains drc |> not then
                    tried <- tried |> Set.add drc
                    let undo = addWall walls drc
                    if runRobotLoop4 walls pos then
                        worked <- worked + 1
                    undoAddWall walls undo     

            visited <- dest::visited
            pos <- dest

        let part1 =
            visited
            |> List.pairwise
            |> Seq.collect (fun pair -> pair ||> getIntermediatePoints) 
            |> Seq.distinct
            |> Seq.length

        part1,worked
    
    let run2 (input: byte array) (output: int -> string -> unit) =
        let lab =
            input 
            |> text 
            |> splitLine 
            |> Array.map (fun s -> s.ToCharArray()) 
            |> array2D

        let walls = mapToWallsList lab

        let startPos = lab |> Array2D.findIndex (fun c -> c = '^')
        let startDir = 0

        let (part1,part2) = runRobotLoop3 walls (startPos, startDir)

        part1 
        |> string
        |> output 1

        part2
        |> string
        |> output 2
