namespace AdventOfCode.FSharp.Y2016

// Day 11: Radioisotope Thermoelectric Generators 
module Day11 =
    open AdventOfCode.FSharp.Util

    type Device =
        | RTG of element : string
        | Chip of element : string

    let parseInput (expectedFloor: int) (line: string) =
        let words = line.Split(' ')

        let floor = 
            match words[1] with
            | "first" -> 0
            | "second" -> 1
            | "third" -> 2
            | "fourth" -> 3
            | w -> failwithf "Unsupported floor: %s" w

        if floor <> expectedFloor then failwithf "Expected floor %d but parsed '%s' as %d" expectedFloor words[1] floor

        if words[4] = "nothing" then
            Seq.empty
        else
            seq { 
                let mutable i = 4
                while i < words.Length do
                    match words[i] with
                    | "a" -> i <- i + 1
                    | "and" -> i <- i + 1
                    | c when c.EndsWith("-compatible") -> 
                        let element = words[i].Split('-')[0]
                        yield Chip element
                        i <- i + 2
                    | c when words[i+1].StartsWith("generator") ->
                        yield RTG words[i]
                        i <- i + 2
                    | c -> failwithf "Invalid layout: %s" c
            }
    let runFast (input: byte array) output =
        let items =         
            input
            |> text
            |> splitLine
            |> Array.mapi parseInput
            |> Array.indexed
            |> Array.fold (fun s (floor: int, items) -> 
                items |> Seq.fold (fun s' item -> 
                    match item with
                    | RTG e ->  s' |> Map.change e (function | None -> Some (floor, -1) | Some (_, x) -> Some (floor, x))
                    | Chip e -> s' |> Map.change e (function | None -> Some (-1, floor) | Some (x, _) -> Some (x, floor))) s) Map.empty
            |> Map.values
            |> Seq.groupBy id
            |> Seq.map (fun (k, s) -> k, s |> Seq.length)
            |> Map.ofSeq

        let maxFloor = 3

        let validItems items =
            let floors = Array.zeroCreate (maxFloor + 1)
            items 
            |> Map.keys
            |> Seq.forall (fun item -> 
                match item with
                | (rf, cf) when rf = cf -> 
                    match floors[rf] with
                    | 0 -> floors[rf] <- -1; true
                    | -1 -> true
                    | _ -> false                
                | (rf, cf) ->
                    (match floors[cf] with
                    | 0 -> floors[cf] <- 1; true
                    | 1 -> true
                    | _ -> 
                    false) &&
                    (match floors[rf] with
                    | 0 -> floors[rf] <- -1; true
                    | -1 -> true
                    | _ -> false))

        let applyMove des items move = 
            match move with
            | (0, rf, cf) -> 
                items |> mapDecrDel (rf, cf) |> mapIncr (des, cf)
            | (1, rf, cf) -> 
                items |> mapDecrDel (rf, cf) |> mapIncr (rf, des)
            | (2, rf, cf) -> 
                items |> mapDecrDel (rf, cf) |> mapIncr (des, des)
            | _ -> failwithf "Invalid move: %A" move

        let applyMoves dest items moves = 
            moves
            |> List.distinct 
            |> List.map (fun move -> move |> List.fold (applyMove dest) items)
            |> List.filter validItems
            |> List.map (fun s -> dest, s)

        let moves (e, items) =
            let itemMoves = 
                items 
                |> Map.keys 
                |> Seq.collect (function
                    // | (rf, cf) when rf = e && cf = e ->
                    // List.replicate (items |> Map.find (rf, cf)) (2, rf, cf)
                    | (rf, cf) when rf = e && cf = e ->
                        List.replicate (items |> Map.find (rf, cf)) (0, rf, cf)
                        @                    
                        List.replicate (items |> Map.find (rf, cf)) (1, rf, cf)                    
                    | (rf, cf) when rf = e ->
                        List.replicate (items |> Map.find (rf, cf)) (0, rf, cf)
                    | (rf, cf) when cf = e ->
                        List.replicate (items |> Map.find (rf, cf)) (1, rf, cf)
                    | _ -> [])
                |> List.ofSeq

            // let pairMoves, nonPairMoves = itemMoves |> List.partition (fun (ord, _, _) -> ord = 2)
            // let splitPairs = pairMoves |> List.collect (fun (_, rf, cf) -> [(0, rf, cf); (1, rf, cf)])

            let doubleMoves = 
                comb 2 itemMoves
                |> List.map (function 
                    | [ (0, a, b); (1, c, d)] when a = b && c = d && a = c -> 
                        [ (2, a, b) ]
                    | x -> x)

            let singleMoves = comb 1 itemMoves

            let minFloor = items |> Map.keys |> Seq.fold (fun a (rf, cf) -> a |> min rf |> min cf) e

            seq {
                if e < maxFloor then
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

        let movesWithCost s =
            moves s
            |> Seq.map (fun s' -> 
                s', 1)
        
        let goalState = (3, Map [ (3,3), items.Values |> Seq.sum ])

        let goal s = s = goalState
        
        let intInfinity = System.Int32.MaxValue

        let costs, _ =
            DijkstraMap.empty
            |> DijkstraMap.add (0, items) 0
            |> DijkstraMap.run intInfinity movesWithCost goal

        costs |> Map.tryFind goalState |> Option.defaultValue intInfinity |> string |> output 1        

        let items2 = items |> mapIncr (0, 0) |> mapIncr (0, 0)

        let goalState2 = (3, Map [ (3,3), items2.Values |> Seq.sum ])
        let goal2 s = s = goalState

        let costs2, _ =
            DijkstraMap.empty
            |> DijkstraMap.add (0, items2) 0
            |> DijkstraMap.run intInfinity movesWithCost goal2

        costs2 |> Map.tryFind goalState2 |> Option.defaultValue intInfinity  |> string |> output 2        

    let skipRun (input: byte array) output =
        let state =         
            input
            |> text
            |> splitLine
            |> Array.mapi parseInput
            |> Array.map Set.ofSeq

        let allDevices = 
            state |> Seq.concat |> Set.ofSeq
        
        let goalState = [|
            Set.empty
            Set.empty
            Set.empty
            allDevices
        |]

        let validFloor floor = 
            let rtgs, chips = 
                floor |> Seq.fold (fun (r,c) d ->
                    match d with
                    | RTG (e) -> (r |> Set.add e), c
                    | Chip (e) -> r, (c |> Set.add e)) (Set.empty, Set.empty)
            (Set.isEmpty rtgs) || (Set.isEmpty chips) || (Set.difference chips rtgs).Count = 0

        let goalFunc (e: int, s) = s = goalState

        let rec getValidFloorMoves (elevatorCount: int) (src: Set<Device>) (dest: Set<Device>) =
            seq {
                for item1 in src do
                    let src' = src |> Set.remove item1
                    let dest' = dest |> Set.add item1

                    if (validFloor src') && (validFloor dest') then
                        yield src', dest'

                    if elevatorCount < 2 then
                        yield! getValidFloorMoves 2 src' dest'
            }

        let moveFunc (e: int, s: Set<Device>[]) =
            seq {
                let floor = s[e]
                let maxFloor = s.Length - 1

                // move up
                if e < maxFloor then

                    for (oldFloor, newFloor) in getValidFloorMoves 1 floor s[e + 1] do
                        let s' = Array.copy s
                        s'[e] <- oldFloor
                        s'[e + 1] <- newFloor

                        yield (e + 1, s'), 1

                // move down
                if e > 0 then
                    for (oldFloor, newFloor) in getValidFloorMoves 1 floor s[e - 1] do
                        let s' = Array.copy s
                        s'[e] <- oldFloor
                        s'[e - 1] <- newFloor

                        yield (e - 1, s'), 1
            }
        
        let h (e: int, s) = 
            s |> Array.mapi (fun i s -> (3 - i) * (Set.count s)) |> Array.sum

        let costs, _ =
            DijkstraMap.empty
            |> DijkstraMap.add (0, state) 0
            |> DijkstraMap.runAstar System.Int32.MaxValue moveFunc goalFunc h

        costs[(3, goalState)] |> string |> output 1

        let extraItems = 
            [ 
                RTG "elerium"
                Chip "elerium"
                RTG "dilithium"
                Chip "dilithium"
            ] |> Set.ofList

        let state2 = Array.copy state
        state2[0] <- Set.union state2[0] extraItems

        let allDevices2 = 
            state2 |> Seq.concat |> Set.ofSeq
        
        let goalState2 = [|
            Set.empty
            Set.empty
            Set.empty
            allDevices2
        |]

        let goalFunc2 (e: int, s) = s = goalState2

        let costs2, _ =
            DijkstraMap.empty
            |> DijkstraMap.add (0, state2) 0
            |> DijkstraMap.runAstar System.Int32.MaxValue moveFunc goalFunc2 h

        costs2[(3, goalState2)] |> string |> output 2
