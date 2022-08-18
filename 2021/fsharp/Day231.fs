namespace AdventOfCode.FSharp.Y2021

module Day231 =
    open Checked

    let PodTypeCount = 4

    let Amber = 0
    let Bronze = 1
    let Copper = 2
    let Desert = 3
    let EmptyPod = 4
    let Main = 4
    
    let PodCosts = [|1; 10; 100; 1000; System.Int32.MaxValue |];
    let PodLetter = [|'A';'B';'C';'D';'.'|];

    type NodeLayout = {
        goalEnergy: int[]
        validFor: bool[]
        exits: (int*int)[] // (nodeIndex, cost)[] = exit[pod-type]
    }
    type Node = {
        pod: int
        goalEnergy: int
        exits: (int*int)[] // (nodeIndex, cost)
    }

    type Layout = {
        nodes: NodeLayout[]
        hallways: Map<int, int>
    }

    let emptyLayout = {
        nodes = [||]
        hallways = Map.empty
    }

    let createExitLayout exitIndex weight =
        PodCosts |> Array.map (fun c -> (exitIndex, c * weight))

    let addHallway (length:int) (layout:Layout) (hallwayIndex:int) : Layout =
        let hallwayStart = layout.nodes.Length
        let hallway = [hallwayStart..(hallwayStart + length)]

        let hallwayExits = 
            hallway 
            |> List.pairwise
            |> List.fold (fun m (a, b) ->
                m 
                |> Map.change a (fun exitList -> (b, 1)::(defaultArg exitList []) |> Some)
                |> Map.change b (fun exitList -> (a, 1)::(defaultArg exitList []) |> Some))
                Map.empty

        let hallwayLayout = hallway |> List.map (fun index ->
            {
                goalEnergy = Array.zeroCreate PodTypeCount
                validFor = Array.replicate PodTypeCount true
                exits = hallwayExits.[index] |> List.toArray
            })

        { layout with 
            nodes = hallwayLayout |> List.toArray |> Array.append layout.nodes
            hallways = layout.hallways |> Map.add hallwayIndex hallwayStart
        }

    let connectHome hallway mainOffset layout =
        let mainIndex = layout.hallways.[Main] + mainOffset
        let mainNode = layout.nodes.[mainIndex]

        let hallwayIndex = layout.hallways[hallway]

        let newNodes = Array.copy layout.nodes

        // main node connects to nobody
        newNodes.[mainIndex] <- { mainNode with exits = Array.empty }

        // adjacent nodes 
        //  - remove link to mainNode
        //  - attach to other adjacent nodes
        //  - attach to hallwayNode
        mainNode.exits 
            |> Array.map fst
            |> Array.iter (fun adjIndex -> 
                let adj = layout.nodes.[adjIndex]

                let otherExits = 
                    mainNode.exits 
                    |> Array.filter (fun (otherIndex, _) -> otherIndex <> adjIndex)
                    |> Array.map (fun (index, weight) -> (index, weight + 1))

                let hallwayExit = [| (hallwayIndex, 2) |]

                let newExits = 
                    adj.exits 
                        |> Array.filter (fun (e, _) -> e <> mainIndex)
                        |> Array.append otherExits
                        |> Array.append hallwayExit

                newNodes.[adjIndex] <- { adj with exits = newExits }) 

        // connect hallway to adjacent nodes    
        let newHallwayExits = 
            mainNode.exits 
            |> Array.map (fun (index, weight) -> (index, weight + 1))

        let hallwayNode = layout.nodes[hallwayIndex]
        newNodes.[hallwayIndex] <- { hallwayNode with exits = hallwayNode.exits |> Array.append newHallwayExits }    

        { layout with nodes = newNodes }
        

    let run (input: string) (output: int -> string -> unit) =
        let mainLayout =
            (emptyLayout, Main)
            ||> addHallway 10

        let layout =
            [Amber; Bronze; Copper; Desert] 
            |> List.fold (addHallway 2) mainLayout 
            |> connectHome Amber 2
            |> connectHome Bronze 4
            |> connectHome Copper 6
            |> connectHome Desert 8

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






