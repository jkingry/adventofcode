namespace AdventOfCode.FSharp.Y2021

module Day232 =
    //open Checked
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

    module bs =
        // 0000000000111111111122222222223333333333444444444455555555556666
        // 0123456789012345678901234567890123456789012345678901234567890123
        // | LEN||  IS OCCUPIED BIT       ||        POD TYPE BITS         |

        let size (state: uint64) = state &&& 31UL |> int

        let fromString (value: string) =        
            let mutable occupiedIndex = 0

            let size = value.Length |> uint64
            let mutable occupied = 0UL
            let mutable pods = 0UL

            for i in 0..value.Length-1 do
                let c = value[i]
                let pod = LetterToPod.[c]
                if pod <> Empty then
                    occupied <- occupied ||| (1UL <<< i)
                    pods <- pods ||| (pod <<< (occupiedIndex * 2))
                    occupiedIndex <- occupiedIndex + 1
            size ||| (occupied <<< 5) ||| (pods <<< 32)   

        let toString (state: uint64) =
            let burrowSize = (state &&& 31UL) |> int
            seq {
                let mutable occupiedIndex = 0
                for roomIndex = 0 to burrowSize do
                    let occupied = (state &&& (1UL <<< (5 + roomIndex))) <> 0UL
                    if occupied then                
                        let pod = (state >>> (32 + (occupiedIndex * 2))) &&& 3UL
                        occupiedIndex <- occupiedIndex + 1
                        yield PodToLetter.[pod]
                    else
                        yield '.'
            } |> Seq.toArray |> System.String     
        let print (state: uint64) =
            state |> toString |> btext.print

        let bitcount (n : int) =
            let count2 = n - ((n >>> 1) &&& 0x55555555)
            let count4 = (count2 &&& 0x33333333) + ((count2 >>> 2) &&& 0x33333333)
            let count8 = (count4 + (count4 >>> 4)) &&& 0x0f0f0f0f
            (count8 * 0x01010101) >>> 24       

        let movePod (state: uint64) (fromPos: int) (toPos: int) =     
            let size = size state
            let occupied = (state >>> 5) &&& ((1UL <<< size) - 1UL)
            let pods = state >>> 32

            let fromBit = 1UL <<< fromPos
            let toBit = 1UL <<< toPos

            let fromOccupied = (occupied &&& fromBit) <> 0UL
            let toOccupied = (occupied &&& toBit) <> 0UL
            
            if not fromOccupied then failwithf "No pod at source: %d" fromPos
            if toOccupied then failwithf "Pod at destination: %d" toPos

            let fromMask = occupied &&& ((1UL <<< fromPos) - 1UL)
            let fromIndex = bitcount (fromMask |> int)

            let pod = (pods >>> (fromIndex * 2)) &&& 3UL        
            
            let newOccupied = occupied &&& ~~~fromBit ||| toBit

            let toMask = newOccupied &&& ((1UL <<< toPos) - 1UL)
            let toIndex = bitcount (toMask |> int)

            if fromIndex = toIndex then
                (size |> uint64) ||| (newOccupied <<< 5) ||| (pods <<< 32)
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
                let newPods = lower ||| (pod <<< (2 * toIndex)) ||| upperShifted

                (size |> uint64) ||| (newOccupied <<< 5) ||| (newPods <<< 32)            

    let toBinary (input: uint64) =
        System.Convert.ToString(input |> int64, 2)     

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

    let adj = Map [
        (0b1000000_0000_0000UL, 
            [
                // hallway to first room
                (0b0000000_1000_0000UL, 0b0100000_0000_0000UL);
                (0b0000000_0100_0000UL, 0b0110000_0000_0000UL);
                (0b0000000_0010_0000UL, 0b0111000_0000_0000UL);
                (0b0000000_0001_0000UL, 0b0111100_0000_0000UL);
                
                // hallway to second room
                (0b0000000_0000_1000UL, 0b0100000_1000_0000UL);
                (0b0000000_0000_0100UL, 0b0110000_0100_0000UL);
                (0b0000000_0000_0010UL, 0b0111000_0010_0000UL);
                (0b0000000_0000_0001UL, 0b0111100_0001_0000UL);
            ]
        );

        (0b0100000_0000_0000UL, 
            [
                // hallway to first room
                (0b0000000_1000_0000UL, 0b0000000_0000_0000UL);
                (0b0000000_0100_0000UL, 0b0010000_0000_0000UL);
                (0b0000000_0010_0000UL, 0b0011000_0000_0000UL);
                (0b0000000_0001_0000UL, 0b0011100_0000_0000UL);
                
                // hallway to second room
                (0b0000000_0000_1000UL, 0b0000000_1000_0000UL);
                (0b0000000_0000_0100UL, 0b0010000_0100_0000UL);
                (0b0000000_0000_0010UL, 0b0011000_0010_0000UL);
                (0b0000000_0000_0001UL, 0b0011100_0001_0000UL);
            ]
        );
    ]

    let generateMoves state =
        let size = bs.size state
        let occupied = state >>> 5
        let pods = state >>> 32

        seq {
            let mutable occupiedIndex = 0
            for i in 0..size do
                if occupied &&& (1UL <<< i) <> 0UL then                    
                    let pod = (pods >>> (occupiedIndex * 2)) &&& 3UL
                    occupiedIndex <- occupiedIndex + 1

                    // in hallway
                    let inHallway = occupiedIndex < ValidHallwaySize

                    if inHallway then
                        ignore
                    else

                        ignore

                    
                yield state, 0
        }

    let run (input: string) (output: int -> string -> unit) =
        let state = input |> btext.parse |> bs.fromString
        
        let cost1, path1 = dijkstra generateMoves (fun _ -> 0) state ("...........AABBCCDD" |> bs.fromString)        

        cost1 |> string |> output 1
