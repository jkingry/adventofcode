namespace AdventOfCode.FSharp.Y2024

// Day 6
module Day06 =
    open Checked
    open AdventOfCode.FSharp.Util

    let inline (@+) (a: int*int) (b: int*int) =
        let ax,ay = a
        let bx,by = b
        (ax + bx),(ay + by)

    let inline isWall (lab: char[,]) (r,c) =
        if OrthoGrid.checkBounds lab r c then
            lab[r,c] = '#' |> Some
        else
            None

    let DIRECTIONS = [|
        -1,0
        0,1
        1,0
        0,-1
    |]
    
    let runRobotLoop (isWall: int*int -> bool option) startVisited startPos =
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
                    Some (p, dir)
                | Some false ->
                    Some (next, dir)

            match pos  with
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
            let row = lab[rowIndex,*]
            for colIndex = 0 to row.Length - 1 do
                let pos = (rowIndex, colIndex)
                let c = 
                    if next = pos then
                        'O'
                    elif visited |> Set.contains pos then
                        'x'
                    else
                        row[colIndex]
                printf "%c" c
            printfn ""

    let runRobotLoop2 lab (isWall: int*int -> bool option) startVisited startPos =
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
                pos <- Some (p, dir')
            | Some false ->                                    
                pos <- Some (next, dir)
                let newIsWall x =
                    if x = next then Some true
                    else isWall x
                let wtf, res = runRobotLoop newIsWall Set.empty startPos
                if not res then
                    found <- found |> Set.add next

            match pos with
            | None -> exited <- true
            | Some x -> 
                visited <- visited |> Set.add x

        found.Count


    let run (input: byte array) (output: int -> string -> unit) =
        let lab = 
            input
            |> text
            |> splitLine
            |> Array.map (fun s -> s.ToCharArray())
            |> array2D
        printfn "%A" lab
        
        let startPos = lab |> Array2D.findIndex (fun c -> c = '^')
        let startDir = 0

        let (visited, _) = runRobotLoop (isWall lab) Set.empty (startPos, startDir)

        visited 
        |> Seq.map fst 
        |> Seq.distinct 
        |> Seq.length 
        |> string 
        |> output 1

        runRobotLoop2 lab (isWall lab) Set.empty (startPos, startDir)
        |> string
        |> output 2
