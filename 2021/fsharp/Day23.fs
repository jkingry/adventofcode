namespace AdventOfCode.FSharp.Y2021

module Day23 =
    open AdventOfCode.FSharp.Util
     open FSharpx.Collections

    let cost = [| 1;10;100;1000|]

    // ...........
    // 01234567890
    //   1 3 5 7
    //   2 4 6 8

    let biomes = [| 'A';'B';'C';'D'|]
    let inline getId c = (int c) - 65
    let destinations = [| 12; 14; 16; 18 |]
    let doors = [| 2;4;6;8 |]
    
    let change (source: int) (dest: int) c (burrow: string): string =
        let burrow' = burrow.ToCharArray()
        burrow'[source] <- '.'
        burrow'[dest] <- c
        new System.String(burrow')

    let hallwayPath p (burrow: string) = 
        let c = burrow[p]
        if c = '.' then [] else
        let bid = getId c
        let dest = destinations[bid]
 
        let pathCost dest x =
            let door = doors[bid]
            let n = p + sign(door - p)
            let path = [(min n door)..(max n door)] 
            if path |> List.forall(fun i -> burrow[i] = '.') then
                [ (burrow |> change p dest c, cost[bid] * (path.Length + x)) ]
            else
                []

        match burrow[dest] with
        | '.' -> pathCost dest 2
        | c when burrow[dest-1] = '.' -> pathCost (dest - 1) 1
        | _ -> []

    let roomPath p (burrow: string) =
        let c = burrow[p]
        if c = '.' then [] else

        let door = (p - 10) + (p % 2)
        let bid = getId c

        let pathCost x dest =
            let path = [(min door dest)..(max door dest)]
            if path |> List.forall(fun i -> burrow[i] = '.') then
                Some (burrow |> change p dest c, cost[bid] * (path.Length + x))
            else
                None
        
        if (p % 2) = 0 then 
            if burrow[p - 1] <> '.' then [] else
                [0..10] |> List.except doors |> List.choose (pathCost 1)
        else
            [0..10] |> List.except doors |> List.choose (pathCost 0)

    let paths = 
        [|0..18|] |> Array.map (fun i -> if i <= 10 then hallwayPath i else roomPath i)          


    let moves (burrow: string) =
        paths |> Seq.map (fun f -> f burrow) |> Seq.concat

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

    let dijkstra start goal =
        let mutable costs = Map.empty
        let mutable visited = Set.empty
        let mutable cameFrom = Map.empty

        let mutable q =
            Heap.empty false |> Heap.insert (0, start)

        let mutable found = None

        while Option.isNone found && not (Heap.isEmpty q) do
            let ((cost, current), nq) = Heap.uncons q

            if current = goal then found <- Some cost else
            
            q <- nq

            if visited |> Set.contains current then () else

            visited <- visited |> Set.add current

            for (move, moveCost) in moves current do
                if visited |> Set.contains move then () else

                let newCost = cost + moveCost

                if newCost < (costs |> Map.tryFind move |> Option.defaultValue System.Int32.MaxValue) then
                    costs <- costs |> Map.add move newCost
                    cameFrom <- cameFrom |> Map.add move current
                    q <- q |> Heap.insert (newCost, move)

        match found with
        | Some cost -> (cost, reconstructPath cameFrom goal)
        | _ -> failwith "INFINITY"

    let run (input: string) (output: int -> string -> unit) =
        let (cost, path) = dijkstra "...........DBDACBCA" "...........AABBCCDD"
        
        //printfn "%A" path

        cost |> string |> output 1