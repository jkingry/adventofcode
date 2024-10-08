namespace AdventOfCode.FSharp.Y2021

// Day 23: Amphipod
module Day23 =
    open AdventOfCode.FSharp.Util

    let PodTypeCount = 4

    let Amber = 0UL
    let Bronze = 1UL
    let Copper = 2UL
    let Desert = 3UL
    let Empty = 4UL

    let PodCosts = [| 1; 10; 100; 1000; System.Int32.MaxValue |]

    let HallwayLength = 11
    let ValidHallwaySize = HallwayLength - PodTypeCount

    let LetterToPod =
        Map [ ('A', Amber); ('B', Bronze); ('C', Copper); ('D', Desert); ('.', Empty) ]

    let PodToLetter =
        LetterToPod |> Map.toList |> List.map (fun (a, b) -> (b, a)) |> Map.ofList

    module btext =
        let DoorIndexes = [| 8; 6; 4; 2 |]

        let print (t: string) =
            printf "#############\n#%c%s%c#\n" t[0] (t[1..5] |> Seq.map string |> String.concat ".") t[6]

            for roomIndex in ValidHallwaySize..4 .. t.Length - 2 do
                if roomIndex = ValidHallwaySize then
                    printf "###%s###\n" (t[roomIndex .. roomIndex + 3] |> Seq.map string |> String.concat "#")
                else
                    printf "  #%s#\n" (t[roomIndex .. roomIndex + 3] |> Seq.map string |> String.concat "#")

            printf "  #########\n"

        let parse (input: string) =
            let mutable buf = new System.Text.StringBuilder()

            for i in 0 .. input.Length - 1 do
                let c = input[i]

                if ('A' <= c && c <= 'D') || (c = '.') then
                    buf <- buf.Append(c)

            for i in DoorIndexes do
                buf <- buf.Remove(i, 1)

            buf.ToString()

    let bitcount (n: uint64) =
        System.Numerics.BitOperations.PopCount(n)

    let toBinary (input: uint64) =
        System.Convert.ToString(input |> int64, 2)

    module bs =
        (*
            0000000000111111111122222222223333333333444444444455555555556666
            0123456789012345678901234567890123456789012345678901234567890123
            | HOME |S|  IS OCCUPIED BIT    ||        POD TYPE BITS         |

            Home     - 8 bits
                - 2 bits per type storing how many pods are in correct place (0,1,2,3)
            Size     - 1 bit
                - small = 0, large = 1, saves passing in size as a seperate parameter
            Occupied - 15 or 23 bits
                - stores if a pod is in a _valid_ space in the burrow
                - hallway spaces are first, left to right
                - doors aren't valid spaces, so they are skipped
                - then borrows, one row/layer at time, left to right
                - offset map
                #############
                #01.2.3.4.56#
                ###7#8#9#..etc..
            Pods - 16 or 32 bits
                - always starting at offset 32
                - 2 bits per pod, indicating pod type
                - in the order the pods are in the occupied mask
                - 00 = Amber, 11 = Desert
        *)
        let size (state: uint64) =
            if (state &&& (1UL <<< 8)) = 0UL then 15 else 23

        let podsPerHome (state: uint64) =
            if (state &&& (1UL <<< 8)) = 0UL then 2 else 4

        let getPod (position: int) (occupied: uint64) (pods: uint64) =
            let occupiedMask = occupied &&& ((1UL <<< position) - 1UL)
            let podIndex = bitcount (occupiedMask)
            (pods >>> (podIndex * 2)) &&& 3UL, podIndex

        let calculateHomes (size: int) (occupied: uint64) (pods: uint64) =
            let mutable homes = 0UL

            for podType in 0..3 do
                let mutable homeCount = Some 0UL

                for homeRow in (if size = 15 then 1 else 3) .. (-1) .. 1 do
                    homeCount <-
                        match homeCount with
                        | Some count ->
                            let roomIndex = ValidHallwaySize + (homeRow * PodTypeCount) + podType

                            if (occupied &&& (1UL <<< roomIndex)) <> 0UL then
                                let pod, _ = getPod roomIndex occupied pods
                                if pod |> int = podType then Some(count + 1UL) else None
                            else
                                Some count
                        | _ -> None

                if homeCount.IsSome then
                    homes <- homes ||| (homeCount.Value <<< (podType * 2))

            homes

        let fromString (value: string) =
            let size =
                match value.Length with
                | 15 -> 0UL
                | 23 -> 1UL
                | s -> failwithf "Invalid size: %i" s

            let mutable occupied = 0UL
            let mutable pods = 0UL

            let mutable occupiedIndex = 0

            for i in 0 .. value.Length - 1 do
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

                for roomIndex = 0 to (size - 1) do
                    let occupied = (state &&& (1UL <<< (8 + 1 + roomIndex))) <> 0UL

                    if occupied then
                        let pod = (state >>> (32 + (occupiedIndex * 2))) &&& 3UL
                        occupiedIndex <- occupiedIndex + 1
                        yield PodToLetter.[pod]
                    else
                        yield '.'
            }
            |> Seq.toArray
            |> System.String

        let print (state: uint64) = state |> toString |> btext.print

        let movePod (state: uint64) (fromPos: int) (toPos: int) =
            let size = size state
            let occupied = (state >>> 8 + 1) &&& ((1UL <<< size) - 1UL)
            let pods = state >>> 32

            let fromBit = 1UL <<< fromPos
            let toBit = 1UL <<< toPos

            let fromOccupied = (occupied &&& fromBit) <> 0UL
            let toOccupied = (occupied &&& toBit) <> 0UL

            if not fromOccupied then
                failwithf "No pod at source: %d" fromPos

            if toOccupied then
                failwithf "Pod at destination: %d" toPos

            let pod, fromIndex = getPod fromPos occupied pods

            let newOccupied = occupied &&& ~~~fromBit ||| toBit

            let _, toIndex = getPod toPos newOccupied pods

            let newPods =
                if fromIndex = toIndex then
                    pods
                else
                    let lower =
                        if fromIndex > 0 then
                            let lowerMask = (1UL <<< (fromIndex * 2)) - 1UL
                            pods &&& lowerMask
                        else
                            0UL

                    let upper =
                        if fromIndex < 15 then
                            let upperMask = (1UL <<< ((fromIndex + 1) * 2)) - 1UL
                            pods &&& ~~~upperMask
                        else
                            0UL

                    let upperShifted = upper >>> 2

                    let newPods = lower ||| upperShifted

                    let lower =
                        if toIndex > 0 then
                            let lowerMask = (1UL <<< (toIndex * 2)) - 1UL
                            newPods &&& lowerMask
                        else
                            0UL

                    let upper =
                        if toIndex < 15 then
                            let upperMask = (1UL <<< (toIndex * 2)) - 1UL
                            newPods &&& ~~~upperMask
                        else
                            0UL

                    let upperShifted = upper <<< 2

                    lower ||| (pod <<< (2 * toIndex)) ||| upperShifted

            let homes = calculateHomes size newOccupied newPods
            let sizeBit = if size = 15 then 0UL else 1UL

            homes ||| (sizeBit <<< 8) ||| (newOccupied <<< 9) ||| (newPods <<< 32)

    let hall_moves =
        [|
           //0 1.2.3.4.5 6
           [| 3; 2; 2; 4; 6; 8; 9 |]
           [| 5; 4; 2; 2; 4; 6; 7 |]
           [| 7; 6; 4; 2; 2; 4; 5 |]
           [| 9; 8; 6; 4; 2; 2; 3 |] |]

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

                if occupied &&& occupiedBit = 0UL then
                    ()
                else

                    let pod = (pods >>> (occupiedIndex * 2)) &&& 3UL |> int
                    occupiedIndex <- occupiedIndex + 1

                    let inHallway = roomIndex < ValidHallwaySize

                    if inHallway then
                        let homeFilled = (homes >>> (pod * 2)) &&& 3UL |> int

                        let targetDepth = podsPerHome - homeFilled - 1
                        let targetRoom = 7 + (PodTypeCount * targetDepth) + pod

                        let mutable checkHomeBlocked = targetRoom

                        while checkHomeBlocked >= 7 && occupied &&& (1UL <<< checkHomeBlocked) = 0UL do
                            checkHomeBlocked <- checkHomeBlocked - PodTypeCount

                        if checkHomeBlocked < 7 then
                            let leftDoorIndex = pod + 1
                            let rightDoorIndex = pod + 2

                            let hallwayMask =
                                if roomIndex <= leftDoorIndex then
                                    let mask = (1UL <<< (leftDoorIndex + 1)) - 1UL
                                    mask &&& ~~~((1UL <<< (roomIndex + 1)) - 1UL)
                                else
                                    let mask = (1UL <<< roomIndex) - 1UL
                                    mask &&& ~~~((1UL <<< rightDoorIndex) - 1UL)

                            if (hallwayMask &&& occupied) = 0UL then
                                let newState = bs.movePod state roomIndex targetRoom
                                let hall_moves = hall_moves[pod][roomIndex]
                                let home_moves = targetDepth
                                yield (newState, PodCosts[pod] * (hall_moves + home_moves))
                    else
                        let currentHome = (roomIndex - 7) % PodTypeCount

                        if (checkedHome &&& (1 <<< (currentHome + 1))) <> 0 then
                            () // printfn "Already checked??"
                        else

                            checkedHome <- checkedHome ||| (1 <<< (currentHome + 1))

                            let homeFilled = (homes >>> (pod * 2)) &&& 3UL |> int
                            let homeDepth = podsPerHome - ((roomIndex - 7) / PodTypeCount)

                            if
                                pod = currentHome
                                && ((homeDepth <= homeFilled) || (homeFilled + 1 = podsPerHome))
                            then
                                () // printfn "All good? homeDepth=%i, homeFilled=%i" homeDepth homeFilled
                            else

                                let mutable hallwayLft = currentHome + 1

                                while hallwayLft >= 0 && (occupied &&& (1UL <<< hallwayLft)) = 0UL do
                                    let newState = bs.movePod state roomIndex hallwayLft
                                    let hall_moves = hall_moves[currentHome][hallwayLft]
                                    let home_moves = podsPerHome - homeDepth
                                    yield (newState, PodCosts[pod] * (hall_moves + home_moves))
                                    hallwayLft <- hallwayLft - 1

                                let mutable hallwayRgt = currentHome + 2

                                while hallwayRgt < 7 && (occupied &&& (1UL <<< hallwayRgt)) = 0UL do
                                    let newState = bs.movePod state roomIndex hallwayRgt
                                    let hall_moves = hall_moves[currentHome][hallwayRgt]
                                    let home_moves = podsPerHome - homeDepth
                                    yield (newState, PodCosts[pod] * (hall_moves + home_moves))
                                    hallwayRgt <- hallwayRgt + 1
        }

    let run input (output: int -> string -> unit) =
        let input = input |> text

        let state1 = input |> btext.parse |> bs.fromString

        let goal1 = ".......ABCDABCD" |> bs.fromString

        DijkstraMap.empty
        |> DijkstraMap.add state1 0
        |> DijkstraMap.run System.Int32.MaxValue generateMoves (fun s -> s = goal1)
        |> fst
        |> Map.find goal1
        |> string
        |> output 1

        let state2 = (state1 |> bs.toString).Insert(11, "DCBADBAC") |> bs.fromString

        let goal2 = ".......ABCDABCDABCDABCD" |> bs.fromString

        DijkstraMap.empty
        |> DijkstraMap.add state2 0
        |> DijkstraMap.run System.Int32.MaxValue generateMoves (fun s -> s = goal2)
        |> fst
        |> Map.find goal2
        |> string
        |> output 2
