namespace AdventOfCode.FSharp.Y2021

module Day19 =
    open AdventOfCode.FSharp.Util
    open Checked

    type point3D = int * int * int

    let list2tuple2 = function | [x;y] -> (x,y) | _ -> failwith "Invalid list item"
    let array2tuple3 = function | [|x;y;z|] -> (x,y,z) | _ -> failwith "Invalid list item"
    let list2tuple3 = function | [x;y;z] -> (x,y,z) | _ -> failwith "Invalid list item"

    let parseScan input =
        let lines = input |> splitLine
        let header = lines[0]
        let lines = Array.tail lines

        let scan =
            match header with
            | Regex "--- scanner (\d+) ---" [n] -> int n
            | _ -> failwith "Bad Input"
        let beacons =
            lines
            |> Array.map (ints >> array2tuple3) 
            |> Set.ofArray

        scan, beacons

    let distanceMapAdd dmap pair =
        let ((x,y,z),(a,b,c)) = pair
        let distance = (x-a)*(x-a) + (y-b)*(y-b) + (z-c)*(z-c)
        dmap |> Map.change distance (fun o -> 
            match o with
            | Some k -> Some (pair::k)
            | None -> Some [pair])
    
    let toDistanceMap (beacons: point3D seq) = 
        comb 2 (beacons |> Seq.toList)
        |> List.map list2tuple2
        |> List.fold distanceMapAdd Map.empty

    let inline delta (a,b,c) (d,e,f) = (a-d,b-e,c-f)

    let rotations = seq { 
        for (x,y,z) in permute [1;2;3] |> List.map list2tuple3 do
            for fx in [1;-1] do
                for fy in [1;-1] do
                    for fz in [1;-1] do
                        yield (x*fx,y*fy,z*fz)
    }

    let inline translate (tx,ty,tz) (x,y,z) = (x+tx,y+ty,z+tz)

    let inline rotate (rx,ry,rz) (x,y,z) =
        let c = [|x;y;z|]
        let tx = abs(rx) - 1 in let fx = sign(rx)
        let ty = abs(ry) - 1 in let fy = sign(ry)
        let tz = abs(rz) - 1 in let fz = sign(rz)
        (c.[tx]*fx,c.[ty]*fy,c.[tz]*fz)
    
    let inline flip (x,y,z) = (-x,-y,-z)

    let inline manhattan (a,b,c) (d,e,f) = abs(a-d)+abs(b-e)+abs(c-f)

    let matchScans admap aud bdmap bud =                
        let tryFindTransform d1 d2 =
            let a1x,a1y = admap |> Map.find d1 |> List.exactlyOne
            let pb1x,pb1y = bdmap |> Map.find d1 |> List.exactlyOne
            let a2x,a2y = admap |> Map.find d2 |> List.exactlyOne
            let pb2x,pb2y = bdmap |> Map.find d2 |> List.exactlyOne

            let p = [
                (pb1x,pb1y,pb2x,pb2y)
                (pb1x,pb1y,pb2y,pb2x)
                (pb1y,pb1x,pb2x,pb2y)
                (pb1y,pb1x,pb2y,pb2x)
            ]

            p 
            |> List.tryPick (fun (b1x,b1y,b2x,b2y) ->
                let a1delta = (delta a1x a1y)
                let b1delta = (delta b1x b1y)
                
                match rotations |> Seq.tryFind (fun r -> (rotate r b1delta) = a1delta) with
                | Some r1 -> 
                    let x1delta  = (delta a1x (rotate r1 b1x)) 

                    let a2delta = (delta a2x a2y)
                    let b2delta = (delta b2x b2y)

                    match rotations |> Seq.tryFind (fun r -> (rotate r b2delta) = a2delta) with
                    | Some r2 -> 
                        let x2delta  = (delta a2x (rotate r2 b2x)) 

                        if r1 = r2 && x1delta = x2delta then Some (r1,x1delta) else None
                    | _ -> None
                | _ ->
                    //printfn "First distance failed %d - (%A->%A) vs (%A->%A)" d1 a1x a1y b1x b1y
                    None)

        let samedistances = 
            Set.intersect aud bud
            |> Seq.toList     
        //printfn " Same Distances %d" (samedistances |> List.length)     

        if samedistances |> List.length < 12 then None else  

        comb 2 samedistances 
        |> List.map list2tuple2 
        |> List.tryPick (fun (d1,d2) -> tryFindTransform d1 d2)

    let run (input: string) (output: int -> string -> unit) =
        let mutable scans = 
            input
            |> splitDoubleLine
            |> Array.map parseScan
            |> Map.ofArray
        
        let mutable distances = scans |> Map.map (fun _ v -> toDistanceMap v)

        let toUniqueDistances m = 
            m 
            |> Map.toSeq
            |> Seq.choose (fun (k,v) -> if v |> List.length = 1 then Some k else None)
            |> Set.ofSeq

        let mutable uniquedistances = distances |> Map.map (fun _ v -> toUniqueDistances v)

        let mutable scanners = Map.empty |> Map.add 0 (0,0,0)

        while scans |> Map.count > 1 do
            let (an,bn,r,t) =
                distances 
                |> Map.keys 
                |> Seq.toList 
                |> comb 2 
                |> List.map list2tuple2
                |> List.pick (fun (an, bn) ->
                    // printfn "try %d to %d" an bn
                    match matchScans distances[an] uniquedistances[an] distances[bn] uniquedistances[bn] with
                    | Some (r,t) -> Some (an,bn,r,t)
                    | None -> None)
            
            // printfn "  found R=%A T=%A" r t
            
            let a_beacons = scans[an]
            let b_beacons = scans[bn]
            let b_beacons_transformed = b_beacons |> Set.map (rotate r >> translate t)
            let ab_beacons = a_beacons |> Set.union b_beacons_transformed
            let ab_new = Set.difference ab_beacons a_beacons
            // printfn "  - added %d points" (Set.count ab_new)
            
            let a_distmap = distances[an]
            let ab_distmap =  
                seq {
                    let mutable n = 0
                    for bp in ab_new do
                        n <- n + 1
                        for ap in (Seq.append ab_new a_beacons) |> Seq.skip n do
                            yield (bp, ap)
                }
                |> Seq.fold distanceMapAdd a_distmap

            scanners <- scanners |> Map.add bn t

            scans <- 
                scans 
                |> Map.add an ab_beacons
                |> Map.remove bn
            distances <-
                distances
                |> Map.add an ab_distmap
                |> Map.remove bn
            uniquedistances <-
                uniquedistances
                |> Map.add an (toUniqueDistances ab_distmap)
                |> Map.remove bn
            
        scans |> Map.values |> Seq.head |> Set.count |> string |> output 1

        scanners 
        |> Map.values
        |> Seq.toList
        |> comb 2
        |> List.map list2tuple2
        |> List.map (fun (a,b) -> manhattan a b)
        |> List.max
        |> string
        |> output 2
