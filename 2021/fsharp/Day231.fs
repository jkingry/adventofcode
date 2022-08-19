namespace AdventOfCode.FSharp.Y2021

module Day231 =
    open Checked
    open System.Collections.Generic

    let PodTypeCount = 4

    let Amber = 0
    let Bronze = 1
    let Copper = 2
    let Desert = 3
    let EmptyPod = 4
    let Main = 4
    
    let PodCosts = [|1; 10; 100; 1000; System.Int32.MaxValue |]
    let LetterToPod =  Map [('A',0); ('B',1);('C',2);('D',3)]
    let PodToLetter = LetterToPod |> Map.toList |> List.map (fun (a,b) -> (b,a)) |> Map.ofList

    let HallwayLength = 10

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
                    let distance = hallDistance + homeDistance
                    PodCosts[pod] * distance
                else 0
            else 
                // coming from a home then, can't go to a door
                let home = homes |> Array.findIndex (fun i -> i <= from)
                let homeStart = homes[home]
                let door = doors[home]

                if dest < HallwayLength && not (Array.contains dest doors) then
                    let homeDistance = from - homeStart
                    let hallDistance = abs (door - dest)
                    let distance = homeDistance + hallDistance 
                    PodCosts[pod] * distance                    
                else 0)

    let isBlocked (homes: int[]) (from :int) (dest: int) (state: int[]) =
        let pod = state[from]
        if pod = EmptyPod then true
        else 
            let homeStart = homes[pod]
            let door = doors[pod]
            if from < HallwayLength then
                // getting out of hallway
                let step = if from < door then 1 else -1 
                ([(from + step)..step..door]@[homeStart..dest]) |> List.forall (fun n -> state[n] = EmptyPod)
            else
                // getting into hallway
                let step = if door < dest then 1 else -1 
                ([from..(-1)..homeStart]@[door..step..dest]) |> List.forall (fun n -> state[n] = EmptyPod)


    let getMoves (homes: int[]) (adj: int[,,]) (pos: int) (state: int[]) =
        let nodes = Array3D.length3 adj
        let pod = state[pos]
        if pod = EmptyPod then []
        else 
            [0..nodes - 1] |> List.choose (fun dest -> 
                let cost = adj[pod, pos, dest]
                if cost > 0 && not (isBlocked homes pos dest state) then Some (dest, cost) else None)

    let move (from: int) (dest: int) (state: int[]) =
        let newState = state |> Array.copy
        let pod = newState[from]
        newState.[from] <- EmptyPod
        newState.[dest] <- pod
        newState

    let run (input: string) (output: int -> string -> unit) =
        let adj = createEmpty 2
        let homes = getHomes 2

        let getMoves' = getMoves homes adj
        let state = "...........BACDBCDA" |> Seq.map (fun c -> LetterToPod.[c]) |> Seq.toArray
        let moves = getMoves' 10 state

        printfn "%A" adj

        output 2 "WTF"
        



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






