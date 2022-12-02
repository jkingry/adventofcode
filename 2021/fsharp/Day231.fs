namespace AdventOfCode.FSharp.Y2021

module Day231 =
    open Checked
    open FSharpx.Collections

    let PodTypeCount = 4

    let Amber = 0
    let Bronze = 1
    let Copper = 2
    let Desert = 3
    let EmptyPod = 4
    let Main = 4
    
    let PodCosts = [|1; 10; 100; 1000; System.Int32.MaxValue |]
    let LetterToPod =  Map [('A',0); ('B',1);('C',2);('D',3);('.',4)]
    let PodToLetter = LetterToPod |> Map.toList |> List.map (fun (a,b) -> (b,a)) |> Map.ofList

    let HallwayLength = 11

    let toStateArray (state: int64) : int[] =
        let rec loop (state: int64) (pos: int) : seq<int> =
            if state = 0L then Seq.empty
            else
                let empty = (state &&& 1L) = 0L
                if empty then
                    Seq.cons EmptyPod (loop (state >>> 1) (pos + 1))
                else
                    let aval = (state >>> 1) &&& 0b11L |> int
                    Seq.cons aval (loop (state >>> 3) (pos + 1))
        loop state 0 |> Seq.toArray

    let getHomes (homeLength:int) =
        [0..PodTypeCount - 1] 
        |> List.map (fun pod -> HallwayLength + (pod * homeLength)) 
        |> List.toArray
    
    let doors =
        [0..PodTypeCount - 1] 
        |> List.map (fun pod -> 2 + (pod * 2)) 
        |> List.toArray

    let createEmpty (homeLength:int) =
        let nodeCount = HallwayLength + (PodTypeCount * homeLength)

        let homes = getHomes homeLength

        Array3D.init PodTypeCount nodeCount nodeCount (fun pod from dest -> 
            if from < HallwayLength then
                let homeStart = homes[pod]
                let homeFinish = homeStart + homeLength
                let door = doors[pod]
                // coming from the hall then, must only go to our home, no door
                if from <> door && dest >= homeStart && dest < homeFinish then
                    let hallDistance = abs (door - from)
                    let homeDistance = dest - homeStart
                    let distance = 1 + hallDistance + homeDistance
                    PodCosts[pod] * distance
                else 0
            else 
                // coming from a home then, can't go to a door
                let home = (from - HallwayLength) / homeLength
                let homeStart = homes[home]
                let door = doors[home]

                if dest < HallwayLength && not (Array.contains dest doors) then
                    let homeDistance = from - homeStart
                    let hallDistance = abs (door - dest)
                    let distance = 1 + homeDistance + hallDistance 
                    PodCosts[pod] * distance                    
                else 0)

    let checkAllowed (homeLength: int) (homes: int[]) (from :int) (dest: int) (state: int[]) =
        let pod = state[from]
        if pod = EmptyPod then true
        else 
            if from < HallwayLength then
                // getting out of hallway
                let homeStart = homes[pod]
                let door = doors[pod]
                let step = if from < door then 1 else -1 
                let path = [(from + step)..step..door]@[homeStart..dest]
                path |> List.forall (fun n -> state[n] = EmptyPod)
            else
                // getting into hallway
                let home = (from - HallwayLength) / homeLength
                let homeStart = homes[home]
                let door = doors[home]
                let step = if door < dest then 1 else -1 
                let path = [(from - 1)..(-1)..homeStart]@[door..step..dest]
                path |> List.forall (fun n -> state[n] = EmptyPod)

    let getMoves checkAllowed (adj: int[,,]) (pos: int) (state: int[]) : seq<int*int> =
        let nodes = Array3D.length3 adj
        let pod = state[pos]
        if pod = EmptyPod then []
        else 
            seq { 0..(nodes - 1) } |> Seq.choose (fun dest -> 
                let cost = adj[pod, pos, dest]
                if cost > 0 && (checkAllowed pos dest state) then Some (dest, cost) else None)

    let move (from: int) (dest: int) (state: int[]) =
        let newState = state |> Array.copy
        let pod = newState[from]
        newState.[from] <- EmptyPod
        newState.[dest] <- pod
        newState

    let reconstructPath cameFrom current =
        let mutable path = [current]

        let mutable finished = false
        let mutable pos = current

        while not finished do
            match cameFrom |> Map.tryFind pos with
            | Some nextPos ->
                path <- nextPos::path
                pos <- nextPos
            | _ -> finished <- true
        path         

    let dijkstra moves h start goal =
        let mutable gScore = Map [(start, 0)]
        let mutable cameFrom = Map.empty

        let mutable q =
            Heap.empty false |> Heap.insert ((h start), start)

        let mutable found = None

        while Option.isNone found && not (Heap.isEmpty q) do
            let ((_, current), nq) = Heap.uncons q

            if current = goal then found <- Some gScore[current] else
            
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

    let moveString (source: int) (dest: int) (burrow: string): string =
        let burrow' = burrow.ToCharArray()
        let c = burrow'[source]
        burrow'[source] <- '.'
        burrow'[dest] <- c
        new System.String(burrow')

    let printState (state: string) =
        printfn "#############"
        printfn "#%s#" state[0..HallwayLength - 1]
        printfn "###%c#%c#%c#%c###" state[11] state[11 + 2] state[11 + 4] state[11 + 6] 
        printfn "  #%c#%c#%c#%c#" state[12] state[12 + 2] state[12 + 4] state[12 + 6] 
        printfn "  #########"

    let run (input: string) (output: int -> string -> unit) =
        let homeLength = 2
        let adj = createEmpty homeLength
        let homes = getHomes homeLength
        let checkAllowed' = checkAllowed homeLength homes

        let graphCost (state: string) =
            state 
            |> Seq.map (fun c -> LetterToPod.[c])
            |> Seq.mapi (fun from pod -> 
                if pod = EmptyPod then 0 else
                let home = homes[pod] 
                if from < HallwayLength then
                    adj[pod, from, home] 
                elif from >= home && from < home + homeLength && state[home..from] = String.replicate (1 + (from - home)) (PodToLetter[pod] |> string) then 
                    0
                else
                    let door = doors[pod]
                    adj[pod, from, door + 1])
            |> Seq.sum

        let graphMove (state: string) =
            // printfn "%s" state
            let nstate = state |> Seq.map (fun c -> LetterToPod.[c]) |> Seq.toArray
            seq { 0..nstate.Length - 1}
            |> Seq.collect (fun from -> 
                getMoves checkAllowed' adj from nstate 
                |> Seq.map (fun (dest, cost) -> (moveString from dest state, cost)))

        let cost1, path1 = dijkstra graphMove graphCost "...........DBDACBCA" "...........AABBCCDD"
        let cost2, path2 = dijkstra graphMove (fun _ -> 0) "...........DBDACBCA" "...........AABBCCDD"
        //let cost1, path = dijkstra graphMove graphCost "...........DDDBDCBACBABCACA" "...........AAAABBBBCCCCDDDD"
        let m = [path1; path2] |> List.map List.length |> List.min
        for p1,p2 in List.zip (List.take m path1) (List.take m path2) do
            printfn "%s=%d %s=%d" p1 (graphCost p1) p2 (graphCost p2)

        cost1 |> string |> output 1 


//  01234567890
// #############
// #...........#
// ###B#C#B#D###
//   #A#D#C#A#
//   #########


// nodes

// main = 0
// ahome = 1
// bhome = 2

 

// let ahome = addHallway nodes 2
// let bhome = addHallway nodes 2

// nodesToState Node[] -> string[]
// stateToNodes string[] -> Node[]


// getEnergy Nodes -> int


// addHallway (hallway: int) (length: int) (Nodes) -> Nodes
// // connect hallway rooms to each other

// connectHome (Nodes) (nodeIndex:int) (nodeIndexint)
// // connect a home hallway 


// printNodes Nodes[] -> 


// connectHome nodes ahome[0] main[2]
// connectHome nodes bhome[0] main[4]

// let nodes =
// emptyNodes 
// |> addHallway main 10 
// |> addHallway ahome 2
// |> addHallway bhome 2 
// |> addHallway chome 2 
// |> addHallway dhome 2 
// |> connectHome ahome 2
// |> connectHome bhome 4
// |> connectHome chome 6
// |> connectHome dhome 8
// |> placePod Bronze bhome 1

// getOccupied (Nodes) -> int[] 
// // list of node indexes with pods

// getMoves (nodeIndex: int) (Nodes) -> (int: location, int: cost)[]
// // moves

// movePod (from:index) (to:index) (cost, Nodes) -> (cost, Nodes)






