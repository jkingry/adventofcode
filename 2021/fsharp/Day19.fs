namespace AdventOfCode.FSharp.Y2021

module Day19 =
    open AdventOfCode.FSharp.Util
    open Checked

    let parseScan input =
        let h::lines = 
            input
            |> splitLine
            |> Array.toList
        let scan =
            match h with
            | Regex "--- scanner (\d+) ---" [n] -> n
            | _ -> failwith "Bad Input"
        let coords =
            lines
            |> List.map (fun line -> 
                let c = line |> ints
                (c[0], c[1], c[2]))
        let dmap =
            comb 2 coords
            |> List.map (function | [a;b] -> (a,b) | _ -> failwith "impossible")
            |> List.fold (fun dmap pair -> 
                let ((x,y,z),(a,b,c)) = pair
                let distance = (x-a)*(x-a) + (y-b)*(y-b) + (z-c)*(z-c)
                dmap |> Map.change distance (fun o -> 
                    match o with
                    | Some k -> Some (pair::k)
                    | None -> Some [pair])) Map.empty
    
        scan, dmap


    let inline delta (a,b,c) (d,e,f) = (a-d,b-e,c-f)
    let list2tuple3 = function | [x;y;z] -> (x,y,z) | _ -> failwith "Invalid list item"

    let rotations = seq { 
        for (x,y,z) in permute [1;2;3] |> List.map list2tuple3 do
            for fx in [1;-1] do
                for fy in [1;-1] do
                    for fz in [1;-1] do
                        yield (x*fx,y*fy,z*fz)
    }

    let rotate (rx,ry,rz) (x,y,z) =
        let c = [|x;y;z|]
        let tx = abs(rx) - 1 in let fx = sign(rx)
        let ty = abs(ry) - 1 in let fy = sign(ry)
        let tz = abs(rz) - 1 in let fz = sign(rz)
        (c.[tx]*fx,c.[ty]*fy,c.[tz]*fz)
    
    let inline flip (x,y,z) = (-x,-y,-z)

    let matchScans a b =
        let an, admap = a
        let bn, bdmap = b

        printfn "%s to %s" an bn
        
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
                let r1 = rotations |> Seq.find (fun r -> (rotate r b1delta) = a1delta)
                let x1delta  = (delta a1x (rotate r1 b1x)) 

                // printfn "%7d A: %A - %A = %A -> %A" d1 a1x a1y (delta a1x a1y) r1
                // printfn "%7s B: %A - %A = %A -> %A" "" b1x b1y (delta b1x b1y) r1
                // printfn "%7s X: %A Y: %A" "" x1delta y1delta

                let a2delta = (delta a2x a2y)
                let b2delta = (delta b2x b2y)
                let r2 = rotations |> Seq.find (fun r -> (rotate r b2delta) = a2delta)
                let x2delta  = (delta a2x (rotate r2 b2x)) 

                // printfn "%7d A: %A - %A = %A -> %A" d2 a2x a2y (delta a2x a2y) r2
                // printfn "%7s B: %A - %A = %A -> %A" "" b2x b2y (delta b2x b2y) r2
                // printfn "%7s X: %A Y: %A" "" x2delta y2delta

                if r1 = r2 && x1delta = x2delta then Some (r1,x1delta) else None)

        let uniquedistances m = 
            m 
            |> Map.toSeq
            |> Seq.choose (fun (k,v) -> if v |> List.length = 1 then Some k else None)

        let samedistances = 
            Set.intersect (admap |> uniquedistances |> Set.ofSeq) (bdmap |> uniquedistances |> Set.ofSeq)
            |> Seq.toList            
        
        comb 2 samedistances 
        |> List.tryPick (fun [d1;d2] -> tryFindTransform d1 d2)

    let run (input: string) (output: int -> string -> unit) =
        let scans = 
            input
            |> splitDoubleLine
            |> Array.map parseScan

        for [a;b] in comb 2 (scans |> Array.toList) do
            match matchScans a b with
            | Some (r,t) -> printfn "R = %A, T = %A" r t
            | None -> printfn "No match"

        -1 |> string |> output 1