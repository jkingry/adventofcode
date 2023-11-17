namespace AdventOfCode.FSharp.Y2016

// Day 11: Radioisotope Thermoelectric Generators
module Day11 =
    open AdventOfCode.FSharp.Util

    let parseInput (input: string) =
        let lines = input |> splitLine

        let floors = lines.Length

        let mutable rtgsMap = Map.empty
        let mutable chpsMap = Map.empty

        for floor = 0 to (floors - 1) do
            let words = lines[ floor ].Split(' ')

            let mutable i = 4

            while i < words.Length do
                match words[i] with
                | "nothing" -> i <- words.Length
                | "a" -> i <- i + 1
                | "and" -> i <- i + 1
                | c when c.EndsWith("-compatible") ->
                    let element = words[ i ].Split('-')[0]
                    chpsMap <- chpsMap |> Map.add element floor
                    i <- i + 2
                | c when words[ i + 1 ].StartsWith("generator") ->
                    let element = words[i]
                    rtgsMap <- rtgsMap |> Map.add element floor
                    i <- i + 2
                | c -> failwithf "Invalid layout: %s" c

        let rtgs = rtgsMap |> Map.toSeq |> Seq.sortBy fst |> Seq.map snd
        let chps = chpsMap |> Map.toSeq |> Seq.sortBy fst |> Seq.map snd
        let items = Seq.zip rtgs chps |> List.ofSeq

        floors, items

    type MoveType =
        | MoveRTG
        | MoveChip
        | MoveBoth

    let run (input: byte array) output =
        let print prefix (e, items) =
            let mitems =
                items
                |> Map.toSeq
                |> Seq.collect (fun (k, v) -> Seq.replicate v k)
                |> Seq.indexed
                |> List.ofSeq

            for i = 0 to 3 do
                let floor = 3 - i

                let floorItems =
                    mitems
                    |> List.collect (function
                        | j, (rf, cf) when rf = floor && cf = floor -> [ (sprintf "G%d" j); (sprintf "M%d" j) ]
                        | j, (rf, _) when rf = floor -> [ (sprintf "G%d" j); "." ]
                        | j, (_, cf) when cf = floor -> [ "."; (sprintf "M%d" j) ]
                        | _ -> [ "."; "." ])
                    |> List.map (sprintf "%3s")
                    |> String.concat " "

                let elevText = if e = floor then "E" else "."

                printfn "%sF%d %s %s" prefix (floor + 1) elevText floorItems

        let maxFloors, itemList = input |> text |> parseInput
        let topFloor = maxFloors - 1

        // Compute state, which is a map of pairs to counts
        // Each pair is (RTG floor, Chip floor)
        let items =
            itemList
            |> Seq.groupBy id
            |> Seq.map (fun (k, s) -> k, s |> Seq.length)
            |> Map.ofSeq

        let validItems items =
            let floors = Array.zeroCreate (maxFloors)

            items
            |> Map.keys
            |> Seq.forall (fun (rf, cf) ->
                (match floors[rf] with
                 | 0 ->
                     floors[rf] <- -1
                     true
                 | -1 -> true
                 | _ -> false)
                && (rf = cf
                    || (match floors[cf] with
                        | 0 ->
                            floors[cf] <- 1
                            true
                        | 1 -> true
                        | _ -> false)))

        let applyMove des items move =
            match move with
            | (MoveRTG, rf, cf) -> items |> mapDecrDel (rf, cf) |> mapIncr (des, cf)
            | (MoveChip, rf, cf) -> items |> mapDecrDel (rf, cf) |> mapIncr (rf, des)
            | (MoveBoth, rf, cf) -> items |> mapDecrDel (rf, cf) |> mapIncr (des, des)

        let applyMoves dest items moves =
            moves
            |> List.distinct
            |> List.map (fun move -> move |> List.fold (applyMove dest) items)
            |> List.filter validItems
            |> List.map (fun s -> dest, s)

        let moves (e, items) =
            let expandItem moveType rf cf =
                List.replicate (items |> Map.find (rf, cf)) (moveType, rf, cf)

            let itemMoves =
                items
                |> Map.keys
                |> Seq.collect (function
                    | (rf, cf) when rf = e && cf = e -> (expandItem MoveRTG rf cf) @ (expandItem MoveChip rf cf)
                    | (rf, cf) when rf = e -> (expandItem MoveRTG rf cf)
                    | (rf, cf) when cf = e -> (expandItem MoveChip rf cf)
                    | _ -> [])
                |> List.ofSeq

            let doubleMoves =
                comb 2 itemMoves
                // Condense paired devices as a single move
                // otherwise looking up (rf, cf) in `items` fails
                |> List.map (function
                    | [ (MoveRTG, a, b); (MoveChip, c, d) ] when a = b && c = d && a = c -> [ (MoveBoth, a, b) ]
                    | x -> x)

            let singleMoves = comb 1 itemMoves

            let minFloor =
                items |> Map.keys |> Seq.fold (fun a (rf, cf) -> a |> min rf |> min cf) e

            seq {
                if e < topFloor then
                    let validDoubleMoves = doubleMoves |> applyMoves (e + 1) items
                    yield! validDoubleMoves

                    if validDoubleMoves.Length = 0 then
                        yield! singleMoves |> applyMoves (e + 1) items

                if e > minFloor then
                    let validSingleMoves = singleMoves |> applyMoves (e - 1) items
                    yield! validSingleMoves

                    if validSingleMoves.Length = 0 then
                        yield! doubleMoves |> applyMoves (e - 1) items
            }

        let movesWithCost s = moves s |> Seq.map (fun s' -> s', 1)

        let intInfinity = System.Int32.MaxValue

        // Solve Part 1
        let goalState = (topFloor, Map [ (topFloor, topFloor), items.Values |> Seq.sum ])
        let goal s = s = goalState

        let costs, _ =
            DijkstraMap.empty
            |> DijkstraMap.add (0, items) 0
            |> DijkstraMap.run intInfinity movesWithCost goal

        costs
        |> Map.tryFind goalState
        |> Option.defaultValue intInfinity
        |> string
        |> output 1

        // Solve Part 2
        let items2 = items |> mapIncr (0, 0) |> mapIncr (0, 0)
        let goalState2 = (topFloor, Map [ (topFloor, topFloor), items2.Values |> Seq.sum ])
        let goal2 s = s = goalState

        let costs2, _ =
            DijkstraMap.empty
            |> DijkstraMap.add (0, items2) 0
            |> DijkstraMap.run intInfinity movesWithCost goal2

        costs2
        |> Map.tryFind goalState2
        |> Option.defaultValue intInfinity
        |> string
        |> output 2
