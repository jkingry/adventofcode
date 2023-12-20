namespace AdventOfCode.FSharp.Y2023

// Day 10: Pipe Maze
module Day10 =
    open AdventOfCode.FSharp.Util

    type Dir =
        | North = 0
        | East = 1
        | South = 2
        | West = 3

    let nextDir (d: Dir) pipe =
        match d, pipe with
        | Dir.North, '|'B -> Dir.North |> Some
        | Dir.South, '|'B -> Dir.South |> Some

        | Dir.East, '-'B -> Dir.East |> Some
        | Dir.West, '-'B -> Dir.West |> Some

        | Dir.North, 'F'B -> Dir.East |> Some
        | Dir.West, 'F'B -> Dir.South |> Some

        | Dir.North, '7'B -> Dir.West |> Some
        | Dir.East, '7'B -> Dir.South |> Some

        | Dir.South, 'J'B -> Dir.West |> Some
        | Dir.East, 'J'B -> Dir.North |> Some

        | Dir.South, 'L'B -> Dir.East |> Some
        | Dir.West, 'L'B -> Dir.North |> Some
        | _ -> None

    let leftSide (d: Dir) pipe =
        match d, pipe with
        | _, '|'B
        | _, '-'B -> [ ((4 + (d |> int) - 1) % 4) |> enum ]

        | Dir.North, 'F'B -> [ Dir.West; Dir.North ]
        | Dir.West, 'F'B -> []

        | Dir.North, '7'B -> []
        | Dir.East, '7'B -> [ Dir.North; Dir.East ]

        | Dir.South, 'J'B -> [ Dir.East; Dir.South ]
        | Dir.East, 'J'B -> []

        | Dir.South, 'L'B -> []
        | Dir.West, 'L'B -> [ Dir.South; Dir.West ]
        | _ -> failwith "Impossible"

    let rightSide (d: Dir) pipe =
        match d, pipe with
        | _, '|'B
        | _, '-'B -> [ (((d |> int) + 1) % 4) |> enum ]

        | Dir.North, 'F'B -> []
        | Dir.West, 'F'B -> [ Dir.West; Dir.North ]

        | Dir.North, '7'B -> [ Dir.North; Dir.East ]
        | Dir.East, '7'B -> []

        | Dir.South, 'J'B -> []
        | Dir.East, 'J'B -> [ Dir.East; Dir.South ]

        | Dir.South, 'L'B -> [ Dir.South; Dir.West ]
        | Dir.West, 'L'B -> []
        | _ -> failwith "Impossible"

    let move dir (x, y) =
        match dir with
        | Dir.North -> x - 1, y
        | Dir.East -> x, y + 1
        | Dir.South -> x + 1, y
        | Dir.West -> x, y - 1
        | c -> failwithf "Invalid: %A" c

    // Validate we could have moved this way
    let tryReverseDir pos (map: byte[,]) dir =
        let nextPos = move dir pos

        nextPos ||> Array2D.tryItem <| map
        |> Option.bind (fun pipe -> nextDir dir pipe |> Option.map (fun _ -> dir, nextPos))

    let getDirections pos (map: byte[,]) =
        [ 0..3 ]
        |> List.map enum
        |> List.choose (tryReverseDir pos map)
        |> function
            | [ a; b ] -> a, b
            | x -> failwithf "Invalid position: %A" x

    let nextStep (map: byte[,]) (dir: Dir) (x: int, y: int) =
        let pipe = map[x, y]
        let dir' = nextDir dir pipe |> Option.get
        let pos' = move dir' (x, y)
        dir', pos'

    let addBorder map pos borderSet borderDir =
        let (lx, ly) = move borderDir pos

        if OrthoGrid.checkBounds map lx ly then
            borderSet |> Set.add (lx, ly)
        else
            borderSet

    let addLeftBorder (map: byte[,]) (d, (x, y)) borderSet =
        leftSide d map[x, y] |> List.fold (addBorder map (x, y)) borderSet

    let addRightBorder (map: byte[,]) (d, (x, y)) borderSet =
        rightSide d map[x, y] |> List.fold (addBorder map (x, y)) borderSet


    let printMap (map: byte[,]) borderSet pipeSet isVisited =
        let textMap =
            map
            |> Array2D.mapi (fun x y v ->
                if borderSet |> Set.contains (x, y) then
                    '+'
                elif isVisited x y then
                    '.'
                elif pipeSet |> Set.contains (x, y) then
                    match v with
                    | '|'B -> '║'
                    | '-'B -> '═'
                    | 'F'B -> '╔'
                    | 'L'B -> '╚'
                    | 'J'B -> '╝'
                    | '7'B -> '╗'
                    | _ -> 'X'
                else
                    'O')

        for r = 0 to (Array2D.length1 textMap) - 1 do
            let row = textMap[r, *]
            row |> System.String |> printfn "%s"

    let run (input: byte[]) (output: int -> string -> unit) =
        let parsed = input |> bsplit '\n'B |> array2D

        let start = parsed |> Array2D.findIndex (fun p -> p = 'S'B)

        let mutable a, b = parsed |> getDirections start
        let mutable steps = 1
        let mutable pipes = Set.empty |> Set.add start |> Set.add (snd a) |> Set.add (snd b)
        let mutable border = Set.empty |> addRightBorder parsed a |> addLeftBorder parsed b

        while (snd a) <> (snd b) do
            a <- a ||> nextStep parsed
            b <- b ||> nextStep parsed
            steps <- steps + 1
            border <- border |> addRightBorder parsed a |> addLeftBorder parsed b
            pipes <- pipes |> Set.add (snd a) |> Set.add (snd b)

        steps |> string |> output 1

        let canMove fx fy tx ty =
            if (OrthoGrid.checkBounds parsed tx ty) && not (Set.contains (tx, ty) pipes) then
                Some 1
            else
                None

        let infiniteCost = System.Int32.MaxValue

        let borderVisitor =
            Dijkstra2D.init (Array2D.length1 parsed) (Array2D.length2 parsed) infiniteCost

        let visitedMap =
            (Set.difference border pipes)
            |> Seq.fold (fun t (x, y) -> t |> Dijkstra2D.add x y 0) borderVisitor
            |> Dijkstra2D.run 4 0 (OrthoGrid.movesToBuffers canMove) (fun _ _ -> false)
            |> fst

        let visited =
            visitedMap |> Array2D.fold (fun s v -> if v < infiniteCost then s + 1 else s) 0

        printMap parsed (border - pipes) pipes (fun x y -> visitedMap[x,y] < infiniteCost)

        (parsed.Length - (pipes.Count + visited)) |> string |> output 2
