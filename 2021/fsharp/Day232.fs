namespace AdventOfCode.FSharp.Y2021

module Day232 =
    open Checked
    open FSharpx.Collections

    let PodTypeCount = 4

    let Amber = 0UL
    let Bronze = 1UL
    let Copper = 2UL
    let Desert = 3UL
    let Empty = 4UL
    
    let PodCosts = [|1; 10; 100; 1000; System.Int32.MaxValue |]

    let HallwayLength = 11
    let ValidHallwaySize = HallwayLength - PodTypeCount
    let ValidHallwayIndexes = [|1;1;0;1;0;1;0;1;0;1;1|]
    let DoorIndexes = [|8;6;4;2|]

    let LetterToPod =  Map [('A',Amber); ('B',Bronze);('C',Copper);('D',Desert);('.',Empty)]
    let PodToLetter = LetterToPod |> Map.toList |> List.map (fun (a,b) -> (b,a)) |> Map.ofList

    module btext =
        let print (t: string) =
            printf "#############\n#%c%s%c#\n" t[0] (t[1..5] |> Seq.map string |> String.concat ".") t[6] 
            for roomIndex in ValidHallwaySize .. 4 .. t.Length - 2 do
                if roomIndex = ValidHallwaySize then
                    printf "###%s###\n" (t[roomIndex..roomIndex+3]|> Seq.map string |> String.concat "#")
                else
                    printf "  #%s#\n" (t[roomIndex .. roomIndex+3] |> Seq.map string |> String.concat "#")
            printf "  #########\n"   
        let parse (input: string) =
            let mutable buf = new System.Text.StringBuilder()
            for i in 0..input.Length-1 do
                let c = input[i]                        
                if ('A' <= c && c <= 'D') || (c = '.') then buf <- buf.Append(c) 
            for i in DoorIndexes do
                buf <- buf.Remove(i, 1)
            buf.ToString()

    let bitcount (n: uint64) = System.Numerics.BitOperations.PopCount(n)

    module bs =
        // 0000000000111111111122222222223333333333444444444455555555556666
        // 0123456789012345678901234567890123456789012345678901234567890123
        // | HOME |S|  IS OCCUPIED BIT    ||        POD TYPE BITS         |
        // |      | |      |   |   |   |  ||       
        // 0000000010000000100011111111111111001001110010011111011

        let size (state: uint64) = if (state &&& (1UL <<< 8)) = 0UL then 15 else 23

        let podsPerHome (state: uint64) = if (state &&& (1UL <<< 8)) = 0UL then 2 else 4

        let getPod (position: int) (occupied: uint64) (pods: uint64) =
            let occupiedMask = occupied &&& ((1UL <<< position) - 1UL)
            let podIndex = bitcount (occupiedMask)
            (pods >>> (podIndex * 2)) &&& 3UL, podIndex

        let calculateHomes (size: int) (occupied: uint64) (pods: uint64) = 
            let mutable homes = 0UL

            for podType in 0..3 do
                let mutable homeCount = Some 0UL
                for homeRow in (if size = 15 then 1 else 3)..(-1)..1 do
                    homeCount <-
                        match homeCount with 
                            | Some count ->
                                let roomIndex = ValidHallwaySize + (homeRow * PodTypeCount) + podType
                                if (occupied &&& (1UL <<< roomIndex)) <> 0UL then
                                    let pod, _ = getPod roomIndex occupied pods
                                    if pod |> int = podType then
                                        Some (count + 1UL)
                                    else None
                                else Some count
                            | _ -> None

                if homeCount.IsSome then
                    homes <- homes ||| (homeCount.Value <<< (podType * 2))

            homes

        let fromString (value: string) =        
            let size = 
                match value.Length with
                | 15 -> 0UL
                | 23 -> 1UL
                | _ -> failwith "Invalid size"

            let mutable occupied = 0UL
            let mutable pods = 0UL

            let mutable occupiedIndex = 0

            for i in 0..value.Length-1 do
                let c = value[i]
                let pod = LetterToPod.[c]
                if pod <> Empty then
                    occupied <- occupied ||| (1UL <<< i)
                    pods <- pods ||| (pod <<< (occupiedIndex * 2))
                    
                    occupiedIndex <- occupiedIndex + 1

            let homes = calculateHomes value.Length occupied pods

            homes ||| (size <<< 8) ||| (occupied <<< 9) ||| (pods <<< 32)   

        let toString (state: uint64) =
            let size = size state

            seq {
                let mutable occupiedIndex = 0
                for roomIndex = 0 to size do
                    let occupied = (state &&& (1UL <<< (8 + 1 + roomIndex))) <> 0UL
                    if occupied then                
                        let pod = (state >>> (32 + (occupiedIndex * 2))) &&& 3UL
                        occupiedIndex <- occupiedIndex + 1
                        yield PodToLetter.[pod]
                    else
                        yield '.'
            } |> Seq.toArray |> System.String   

        let print (state: uint64) =
            state |> toString |> btext.print

        let movePod (state: uint64) (fromPos: int) (toPos: int) =     
            let size = size state
            let occupied = (state >>> 8 + 1) &&& ((1UL <<< size) - 1UL)
            let pods = state >>> 32

            let fromBit = 1UL <<< fromPos
            let toBit = 1UL <<< toPos

            let fromOccupied = (occupied &&& fromBit) <> 0UL
            let toOccupied = (occupied &&& toBit) <> 0UL
            
            if not fromOccupied then failwithf "No pod at source: %d" fromPos
            if toOccupied then failwithf "Pod at destination: %d" toPos

            let pod, fromIndex = getPod fromPos occupied pods
            
            let newOccupied = occupied &&& ~~~fromBit ||| toBit

            let _, toIndex = getPod toPos newOccupied pods

            let newPods =
                if fromIndex = toIndex then
                    pods
                else 
                    let lower = 
                        if fromIndex > 0 then   
                            let lowerMask = (1UL <<< ((fromIndex - 1) * 2)) - 1UL
                            pods &&& lowerMask
                        else 0UL

                    let upper =
                        if fromIndex < 15 then
                            let upperMask = (1UL <<< ((fromIndex + 1) * 2)) - 1UL
                            pods &&& ~~~upperMask            
                        else 0UL
                    let upperShifted = upper >>> 2
                   
                    let newPods = lower ||| upperShifted

                    let lower =
                        if toIndex > 0 then
                            let lowerMask = (1UL <<< (toIndex * 2)) - 1UL
                            newPods &&& lowerMask
                        else 0UL
                    let upper =
                        if toIndex < 15 then
                            let upperMask = (1UL <<< ((toIndex + 1) * 2)) - 1UL
                            newPods &&& ~~~upperMask
                        else 0UL            
                    let upperShifted = upper <<< 2
                    
                    lower ||| (pod <<< (2 * toIndex)) ||| upperShifted

            let homes = calculateHomes size newOccupied newPods
            let sizeBit = if size = 15 then 0UL else 1UL

            homes ||| (sizeBit <<< 8) ||| (newOccupied <<< 9) ||| (newPods <<< 32)                             

    let toBinary (input: uint64) =
        System.Convert.ToString(input |> int64, 2).ToCharArray() |> Array.rev |> System.String     

    let reconstructPath cameFrom current =
        let mutable path = [ current ]

        let mutable finished = false
        let mutable pos = current

        while not finished do
            match cameFrom |> Map.tryFind pos with
            | Some nextPos ->
                path <- nextPos :: path
                pos <- nextPos
            | _ -> finished <- true

        path 

    let dijkstra moves h start goal =
        let mutable gScore = Map [ (start, 0) ]
        let mutable cameFrom = Map.empty

        let mutable q = Heap.empty false |> Heap.insert ((h start), start)

        let mutable found = None

        while Option.isNone found && not (Heap.isEmpty q) do
            let ((_, current), nq) = Heap.uncons q

            if current = goal then
                found <- Some gScore[current]
            else
                q <- nq

                for (move, moveCost) in moves current do
                    let tentative_gScore = gScore[current] + moveCost

                    if tentative_gScore < (gScore |> Map.tryFind move |> Option.defaultValue System.Int32.MaxValue) then
                        cameFrom <- cameFrom |> Map.add move current
                        gScore <- gScore |> Map.add move tentative_gScore
                        q <- q |> Heap.insert (tentative_gScore + (h move), move)

        match found with
        | Some cost -> (cost, reconstructPath cameFrom goal)
        | _ -> failwith "INFINITY"
    
    let hall_moves = 
        [|
            //0 1.2.3.4.5 6
            [|3;2;2;4;6;8;9|]
            [|5;4;2;2;4;6;7|]
            [|7;6;4;2;2;4;5|]
            [|9;8;6;4;2;2;3|]
        |]

    let generateMoves state =
        let size = bs.size state
        let homes = state
        let occupied = state >>> 9
        let pods = state >>> 32

        let podsPerHome = if size = 15 then 2 else 4 

        seq {
            let mutable occupiedIndex = 0
            let mutable checkedHome = 0
            for roomIndex in 0..size do
                let occupiedBit = 1UL <<< roomIndex
                if occupied &&& occupiedBit <> 0UL then                    
                    let pod = (pods >>> (occupiedIndex * 2)) &&& 3UL |> int
                    occupiedIndex <- occupiedIndex + 1

                    let inHallway = roomIndex < ValidHallwaySize

                    if inHallway then
                        let homeFilled = (homes >>> (pod * 2)) &&& 3UL |> int

                        let targetDepth = podsPerHome - homeFilled - 1
                        let targetRoom = 7 + (podsPerHome * targetDepth) + pod

                        printfn "room=%i, pod=%i, homeFilled=%i, targetRoom=%i" roomIndex pod homeFilled targetRoom
                        // there is only one option

                        // get bit mask for path to door way
                        // if path to door way is clear
                            // determine which is the next home space based on homes bits
                            // get bit mask for path to home space
                            // if path to home space is clear                         
                                // OPTION
                        ()
                    else
                        let currentHome = (roomIndex - 7) % podsPerHome                        

                        if (checkedHome &&& (1 <<< (currentHome + 1))) <> 0 then () else

                        checkedHome <- checkedHome ||| (1 <<< (currentHome + 1))
                        
                        let homeFilled = (homes >>> (pod * 2)) &&& 3UL |> int
                        let homePosition = podsPerHome - ((roomIndex - 7) / podsPerHome)

                        if pod = currentHome && homePosition <= (homeFilled + 1) then () else

                        let mutable hallwayLft = currentHome + 1
                        while hallwayLft >= 0 && (occupied &&& (1UL <<< hallwayLft)) = 0UL do
                            let newState = bs.movePod state roomIndex hallwayLft
                            let hall_moves = hall_moves[currentHome][hallwayLft]
                            let home_moves = podsPerHome - homePosition
                            yield (newState, PodCosts[pod] * (hall_moves + home_moves))                            
                            hallwayLft <- hallwayLft - 1

                        let mutable hallwayRgt = pod + 2
                        while hallwayRgt < 7 && (occupied &&& (1UL <<< hallwayRgt)) = 0UL do
                            let newState = bs.movePod state roomIndex hallwayRgt
                            let hall_moves = hall_moves[currentHome][hallwayRgt]
                            let home_moves = podsPerHome - homePosition
                            yield (newState, PodCosts[pod] * (hall_moves + home_moves))                            
                            hallwayRgt <- hallwayRgt + 1
        }

    let run (input: string) (output: int -> string -> unit) =
        let state = input |> btext.parse |> bs.fromString
        
        printfn "%s" (toBinary state)
        state |> bs.print

        for (s,c) in generateMoves state do
            printfn "cost=%i" c
            s |> bs.print

        // let cost1, path1 = dijkstra generateMoves (fun _ -> 0) state ("...........AABBCCDD" |> bs.fromString)        

        1 |> string |> output 1
