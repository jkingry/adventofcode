namespace AdventOfCode.FSharp.Y2021

module Day232 =
    //open Checked

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

    let LetterToPod =  Map [('A',Amber); ('B',Bronze);('C',Copper);('D',Desert);('.',Empty)]
    let PodToLetter = LetterToPod |> Map.toList |> List.map (fun (a,b) -> (b,a)) |> Map.ofList

    // 0000000000111111111122222222223333333333444444444455555555556666
    // 0123456789012345678901234567890123456789012345678901234567890123
    // | LEN||  IS OCCUPIED BIT       ||        POD TYPE BITS         |

    let printState (state: uint64) =
        let burrowSize = (state &&& 31UL) |> int
        let stateText =        
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
        // printfn "%s" stateText
        printf "#############\n#%c%c.%c.%c.%c.%c%c#\n" stateText[0] stateText[1] stateText[2] stateText[3] stateText[4] stateText[5] stateText[6] 
        for roomIndex in ValidHallwaySize .. 4 .. burrowSize - 1 do
            if roomIndex = ValidHallwaySize then
                printf "###%c#%c#%c#%c###\n" stateText.[roomIndex] stateText.[roomIndex+1] stateText.[roomIndex+2]  stateText.[roomIndex+3]
            else
                printf "  #%c#%c#%c#%c#\n" stateText.[roomIndex] stateText.[roomIndex+1] stateText.[roomIndex+2]  stateText.[roomIndex+3]
        printf "  #########\n"        

    let parseInput (input: string) =
        let mutable state = 0UL
        let mutable occupiedIndex = 0

        // hallway
        let mutable validHallwayIndex = 0
        for hallwayIndex = 0 to HallwayLength - 1 do
            let inputPos = hallwayIndex + 15
            let inputChar = input.[inputPos]
            let pod = LetterToPod.[inputChar] 

            if ValidHallwayIndexes.[hallwayIndex] = 1 then
                if pod <> Empty then
                    state <- state ||| (1UL <<< (5 + validHallwayIndex))
                    state <- state ||| (pod <<< (32 + (occupiedIndex * 2)))
                    occupiedIndex <- occupiedIndex + 1
                validHallwayIndex <- validHallwayIndex + 1
            else if pod <> Empty then
                failwithf "Invalid hallway at %d" hallwayIndex
    
        // homes
        let mutable inputPos = 31
        let mutable homeIndex = 0
        while inputPos < input.Length do
            let skip = 
                match input.[inputPos] with
                    | 'A' | 'B' | 'C' | 'D' | '.' as inputChar ->
                        let pod = LetterToPod.[inputChar]
                        if pod <> Empty then
                            state <- state ||| (1UL <<< (5 + ValidHallwaySize + homeIndex))
                            state <- state ||| (pod <<< (32 + (occupiedIndex * 2)))
                            occupiedIndex <- occupiedIndex + 1
                        homeIndex <- homeIndex + 1
                        2
                    | '#' -> 6
                    | '\n' -> 4
                    | _ -> failwithf "Unexpected input character: %A" input.[inputPos]
            inputPos <- inputPos + skip
        
        // length
        let burrowSize = ValidHallwaySize + homeIndex |> uint64
        // printfn "%i" burrowSize
        // printfn "%s" (System.Convert.ToString(state |> int64, 2))
        // printfn "%s" (System.Convert.ToString(burrowSize |> int64, 2))
        state <- state ||| burrowSize        
        // printfn "%s" (System.Convert.ToString(state |> int64, 2))
        state

    let bitcount (n : int) =
        let count2 = n - ((n >>> 1) &&& 0x55555555)
        let count4 = (count2 &&& 0x33333333) + ((count2 >>> 2) &&& 0x33333333)
        let count8 = (count4 + (count4 >>> 4)) &&& 0x0f0f0f0f
        (count8 * 0x01010101) >>> 24       

    let movePod (state: uint64) (fromPos: int) (toPos: int) =     
        let size = state &&& 31UL |> int
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

    let run (input: string) (output: int -> string -> unit) =
        let state = parseInput input
        printState state
        let newState = movePod state 22 1
        printState newState

        state |> string |> output 1 