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
        let state =         
            input
            |> text
            |> splitLine
            |> Array.mapi parseInput
            |> Array.map Set.ofSeq

        0 |> string |> output 1
        1 |> string |> output 2

    let run (input: byte array) output =
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
